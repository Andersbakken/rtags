#!/usr/bin/env python

import argparse
import subprocess as sp
import json
import os
import unittest
import sys

binary_path = ""
debug = False


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
    args = [os.path.join(binary_path, "rc")] + args
    # Do the query
    try:
        out = sp.check_output(args)
        log_rc_output(out)
    except sp.CalledProcessError as e:
        log("rc err: " + e.output)
        log("rc cmd: " + str(e.cmd))
        raise
    return out


def wait_for(p, match):
    while p.poll() is None:
        l = p.stdout.readline()  # This blocks until it receives a newline.
        log_rdm_output(l)
        if match in l:
            break


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


class TestFixture(unittest.TestCase):

    def setUp(self):
        log("#################################################################")
        log("Initializing test " + str(self.id))
        cwd = os.getcwd()
        # name should be defined in the derived class !
        self.test_wd = os.path.join(cwd, self.name)

        cdb = [{"directory": self.test_wd,
                "command": "clang++ -c main.cpp",
                "file": "main.cpp"}]
        cdb_path = os.path.join(self.test_wd, 'compile_commands.json')
        with open(cdb_path, 'w') as outfile:
            json.dump(cdb, outfile)
            outfile.write("\n")

        # Start rdm
        self.rdm = sp.Popen(
            [os.path.join(binary_path, "rdm")],
            stdout=sp.PIPE, stderr=sp.STDOUT)
        wait_for(self.rdm, "Includepaths")

        # Clean projects
        run_rc(["-C"])
        wait_for(self.rdm, "rc -C")

        # Load project
        run_rc(["-J", self.test_wd])
        wait_for(self.rdm, "Jobs took")

    def tearDown(self):
        self.rdm.terminate()
        self.rdm.wait()


def compareLocationLists(a, b):
    return sorted(a) == sorted(b)


class FirstTest(TestFixture):

    def __init__(self, a):
        self.name = 'test_test'
        super(FirstTest, self).__init__(a)

    def test_follow_location(self):
        main_cpp = os.path.join(self.test_wd, "main.cpp")
        out = run_rc(
            ["--follow-location", main_cpp + ":4:5"])
        self.assertEqual(Location.fromStr(out), Location(main_cpp, 1, 6))

    def test_find_references(self):
        main_cpp = os.path.join(self.test_wd, "main.cpp")
        out = run_rc(
            ["--references", main_cpp + ":1:6"])

        locations = []
        lines = out.split("\n")
        for line in lines:
            if len(line) > 0:
                loc = Location.fromStr(line)
                locations.append(loc)

        expected_locations = [Location(main_cpp, 4, 5),
                              Location(main_cpp, 5, 5)]
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
