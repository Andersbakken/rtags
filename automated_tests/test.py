#!/usr/bin/env python

import argparse
import subprocess as sp
import json
import os
import unittest
import sys

binary_path = ""
debug = False
socket_file = "/var/tmp/rdm_dev"


def log(s):
    if debug:
        print s


def log_rc_output(out):
    lines = out.split("\n")
    for i in lines:
        log(">rc: " + i)


def log_rdm_output(out):
    log(">rdm:" + out.rstrip())


def run_rc(args):
    args = [os.path.join(binary_path, "rc"),
            "--socket-file=" + socket_file] + args
    # Do the query
    try:
        out = sp.check_output(args)
        log_rc_output(out)
    except sp.CalledProcessError as e:
        log("rc returncode: " + str(e.returncode))
        log("rc output: " + e.output)
        log("rc cmd: " + str(e.cmd))
        raise
    return out


def wait_for(p, match):
    while p.poll() is None:
        l = p.stdout.readline()  # This blocks until it receives a newline.
        log_rdm_output(l)
        if match in l:
            break


def readLocations(out):
    locations = []
    lines = out.split("\n")
    for line in lines:
        if len(line) > 0:
            loc = Location.fromStr(line)
            locations.append(loc)
    return locations


class Location:

    def __init__(self, file, line, col):
        self.file = str(file)
        self.line = int(line)
        self.col = int(col)

    @classmethod
    def fromStr(cls, s):
        tokens = s.split(":")
        return cls(tokens[0], tokens[1], tokens[2])

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        else:
            raise ValueError("Type error")

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        if isinstance(other, self.__class__):
            return self.__dict__ < other.__dict__
        else:
            raise ValueError("Type error")


def toStr(loc):
    return "%s:%s:%s" % (loc.file, str(loc.line), str(loc.col))


class TestFixture(unittest.TestCase):

    def setUp(self):
        log("#################################################################")
        log("Initializing test " + str(self.id))
        cwd = os.getcwd()
        # name should be defined in the derived class !
        self.test_wd = os.path.join(cwd, self.name)

        cdb = []
        for file in os.listdir(self.test_wd):
            if file.endswith(".cpp"):
                cdb.append({"directory": self.test_wd,
                            "command": "clang++ -I. -c " + file,
                            "file": file})
        cdb_path = os.path.join(self.test_wd, 'compile_commands.json')
        with open(cdb_path, 'w') as outfile:
            json.dump(cdb, outfile)
            outfile.write("\n")

        # Start rdm
        self.rdm = sp.Popen(
            [os.path.join(binary_path, "rdm"),
             "--socket-file=" + socket_file,
             "--data-dir=~/.rtags_dev",
             "--no-filesystem-watcher", "--no-startup-project",
             # verbose
             "-vvv",
             "--log-file", "logfile",
             # --clear-project-caches
             "-C"],
            stdout=sp.PIPE, stderr=sp.STDOUT)
        wait_for(self.rdm, "Includepaths")

        # Load project
        run_rc(["-J", self.test_wd])
        wait_for(self.rdm, "Jobs took")

        self.main_cpp = os.path.join(self.test_wd, "main.cpp")
        self.a_hpp = os.path.join(self.test_wd, "a.hpp")
        self.a_cpp = os.path.join(self.test_wd, "a.cpp")

    def tearDown(self):
        self.rdm.terminate()
        self.rdm.wait()


def compareLocationLists(a, b):
    return sorted(a) == sorted(b)


class OneTU(TestFixture):

    def __init__(self, a):
        self.name = 'OneTU'
        super(OneTU, self).__init__(a)

    def test_follow_location(self):
        out = run_rc(
            ["--follow-location", toStr(Location(self.main_cpp, 4, 5))])
        self.assertEqual(Location.fromStr(out), Location(self.main_cpp, 1, 6))

    def test_find_references(self):
        out = run_rc(
            ["--references", toStr(Location(self.main_cpp, 1, 6))])

        locations = readLocations(out)

        expected_locations = [Location(self.main_cpp, 4, 5),
                              Location(self.main_cpp, 5, 5)]
        self.assertTrue(compareLocationLists(locations, expected_locations))


class MultipleTU(TestFixture):

    def __init__(self, a):
        self.name = 'MultipleTU'
        super(MultipleTU, self).__init__(a)

    def test_follow_location(self):
        out = run_rc(
            ["--follow-location", toStr(Location(self.a_hpp, 1, 6))])
        self.assertEqual(Location.fromStr(out), Location(self.a_cpp, 3, 6))

    def test_find_references(self):
        out = run_rc(
            ["--references", toStr(Location(self.a_hpp, 1, 6))])

        locations = readLocations(out)

        expected_locations = [
            Location(self.a_cpp, 6, 5),
            Location(self.main_cpp, 4, 5),
            Location(self.main_cpp, 5, 5)]
        self.assertTrue(compareLocationLists(locations, expected_locations))


class ClassTemplates(TestFixture):

    def __init__(self, a):
        self.name = 'ClassTemplates'
        super(ClassTemplates, self).__init__(a)

    def test_follow_location(self):
        out = run_rc(
            ["--references",
             toStr(Location(self.main_cpp, 9, 8)),
             ])

        locations = readLocations(out)

        expected_locations = [
            Location(self.main_cpp, 16, 17),
            Location(self.main_cpp, 17, 17),
            Location(self.main_cpp, 20, 5)]
        self.assertTrue(compareLocationLists(locations, expected_locations))

    # This is not going to work, because of libclang's limitation.
    # We cannot recurse into the AST nodes of a template instantiation.
    # This means we can't connect a CXXDependentScopeMemberExpr in the base
    # template with the MemberExpr in the instantiation.
    # The only way to make it work is to use libtooling (and
    # RecursiveASTVisitor).
    def disabled_test_memberExpr(self):
        out = run_rc(
            ["--references",
             # t.foo();
             #   ^
             toStr(Location(self.main_cpp, 12, 11)),
             ])

        locations = readLocations(out)

        expected_locations = [
            Location(self.main_cpp, 2, 10),
            Location(self.main_cpp, 5, 9)]
        self.assertTrue(compareLocationLists(locations, expected_locations))


class FunctionTemplates(TestFixture):

    def __init__(self, a):
        self.name = 'FunctionTemplates'
        super(FunctionTemplates, self).__init__(a)

    def test_follow_location(self):
        out = run_rc(
            ["--references",
             toStr(Location(self.main_cpp, 2, 6)),
             ])

        locations = readLocations(out)

        expected_locations = [

            # Explicit instantiation will not match. With libclang, we currently
            # cannot visit this part of the AST.
            # template void foo<char>(char t);
            #               ^
            # Location(self.main_cpp, 6, 15),

            #     foo<int>(4);
            #     ^
            Location(self.main_cpp, 10, 5),
        ]
        self.assertTrue(compareLocationLists(locations, expected_locations))


class MetaPrograms(TestFixture):

    def __init__(self, a):
        self.name = 'MetaPrograms'
        super(MetaPrograms, self).__init__(a)

    def test_follow_location(self):
        out = run_rc(
            ["--references",
             toStr(Location(self.main_cpp, 2, 8)),
             ])

        locations = readLocations(out)

        # We'd expect to have several matches, one for each
        # instantiation/specialization.
        # But with libclang it is not possible to traverse AST nodes of
        # all the instantiations. I.e. factorial<1>, facatorial<2> and
        # factorial<3> is unreachable instantiation nodes.
        # This behaviour is particularly good in this case, because we don't
        # need to add special logic to handle meta programs like this.
        expected_locations = [
            Location(self.main_cpp, 3, 24),
            Location(self.main_cpp, 7, 8),
            Location(self.main_cpp, 11, 9),
        ]
        self.assertTrue(compareLocationLists(locations, expected_locations))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Function tests for rtags')
    parser.add_argument('--binary_path', '-b', required=True,
                        help='directory path to the binaries')
    parser.add_argument('--debug', '-d', action='store_true', default=False,
                        help='print debug output')
    parser.add_argument('unittest_args', nargs='*')

    args = parser.parse_args()
    binary_path = args.binary_path
    debug = args.debug

    sys.argv[1:] = args.unittest_args
    unittest.main()
