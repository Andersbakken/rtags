#!/usr/bin/env python
# coding=utf-8
#
# Python 2 and 3 compatible.
#
# Assuming that the RTags binaries are in build/bin, run with:
#
# $ RTAGS_BINARY_DIR=$(pwd)/build/bin:$PATH  nosetests --no-byte-compile
#
# Run nosetest with --nocapture flag to see print output
#

"""
Nose testing script for `automated_testing`.
"""

from __future__ import print_function
import os
import sys
import json
import time
import subprocess as sp
from hamcrest import assert_that, has_length, has_item, empty, is_not, equal_to

sys.dont_write_bytecode = True
os.environ["PYTHONDONTWRITEBYTECODE"] = "1"
SOCKET_FILE = "/var/tmp/rdm_dev"

try:
    RDM = os.path.join(os.environ['RTAGS_BINARY_DIR'], 'rdm')
    RC = os.path.join(os.environ['RTAGS_BINARY_DIR'], 'rc')
except KeyError:
    sys.stderr.write("You need to set RTAGS_BINARY_DIR environment variable.\n")
    sys.exit(1)


def create_compile_commands(test_dir, test_files):
    """
    Create dict of compile commands
    """
    return [dict(directory=os.path.abspath(test_dir), file=test_file,
                 command="clang++ -std=c++11 -I. -c %s" % os.path.join(test_dir, test_file))
            for test_file in (src_file for src_file in test_files
                              if src_file.endswith(('.cpp', '.c')))]

def read_locations(test_dir, lines):
    """
    Read location
    """
    lines = [line.split(":") for line in lines.split("\n") if len(line) > 0]
    return [Location(os.path.join(test_dir, line[0]), line[1], line[2]) for line in lines]


class Location:
    """
    Class representing location in file
    """
    def __init__(self, file, line, col):
        self.file, self.line, self.col = str(file), int(line), int(col)

    # pylint: disable=
    @classmethod
    def from_str(cls, string):
        """
        From string.
        """
        return cls(*string.split(":")[0:3])

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        else:
            raise ValueError("Type error")

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        return "%s:%d:%d" % (self.file, self.line, self.col)


def run_rc(args):
    """
    Run rc command.
    """
    args = [RC, "--socket-file=" + SOCKET_FILE] + args
    return sp.check_output(args).decode()

class TestType:
    location = 1
    parse = 2
    completion = 3

def run_location(test_dir, rc_command, expected):
    actual_locations = read_locations(test_dir,
                                      run_rc([c.format(test_dir) for c in rc_command]))
    # Compare that we have the same results in length and content
    assert_that(actual_locations, has_length(len(expected)))
    for expected_location_string in expected:
        expected_location = Location.from_str(expected_location_string.format(test_dir))
        assert_that(actual_locations, has_item(expected_location))

def run_parse(test_dir, rc_command, expected):
    output = run_rc(rc_command)
    assert_that(output, is_not(empty()))
    assert_that(output.split(" "), has_item(expected.format(test_dir + "/")))

def non_empty_split(s, delim=None):
    return [x for x in s.split(delim) if x]

def run_completion(test_dir, rc_command, expected):
    outputs = run_rc([c.format(test_dir) for c in rc_command]).split("\n")

    size = len(outputs)
    assert_that(size, equal_to(len(expected)))

    for i in xrange (0, size):
        assert_that(non_empty_split(outputs[i].strip(), " "),
                    equal_to(non_empty_split(expected[i].strip(), " ")))

def run(test_dir, rc_command, expected, test_type):
    """
    Run test
    """
    if test_type == TestType.location:
        run_location(test_dir, rc_command, expected)
    elif test_type == TestType.parse:
        run_parse(test_dir, rc_command, expected)
    elif test_type == TestType.completion:
        run_completion(test_dir, rc_command, expected)


def setup_rdm(test_dir, test_files):
    """
    Start rdm and parse the test files.
    """
    rdm = sp.Popen([RDM, "-n", SOCKET_FILE, "-d", "~/.rtags_dev", "-o", "-B",
                    "-C", "--log-flush"], stdout=sp.PIPE, stderr=sp.STDOUT)
    compile_commands = create_compile_commands(test_dir, test_files)
    # Wait for rdm
    for _ in range(10):
        try:
            run_rc(["-w"])
            break
        except sp.CalledProcessError:
            time.sleep(0.01)

    # Parse the test files
    for unit in compile_commands:
        run_rc(["--project-root", test_dir, "-c", unit["command"]])
        while True:
            try:
                run_rc(["--is-indexing"])
                break
            except sp.CalledProcessError:
                time.sleep(0.01)
    return rdm


def setup_module():
    """
    Nosetests module setup function.
    """
    for exe in [RDM, RC]:
        if not (os.path.isfile(exe) and os.access(exe, os.X_OK)):
            raise FileNotFoundError("{} does not exist or is not executable\n".format(exe))
        return 0

def get_type(test_dir):
    if "Parsing" in test_dir:
        return TestType.parse

    if "Completion" in test_dir:
        return TestType.completion

    return TestType.location

def test_generator():
    """
    Main nosetests entry point.
    """
    base_test_dir = os.path.dirname(os.path.abspath(__file__))
    for test_dir, _, test_files in tuple(os.walk(base_test_dir))[1:]:
        if "__pycache__" in test_dir:
            continue
        expectations = json.load(open(os.path.join(test_dir, "expectation.json"), 'r'))
        rdm = setup_rdm(test_dir, test_files)
        for exp in expectations:
            run.description = os.path.basename(test_dir) + ': ' + exp["name"]
            test_type = get_type(test_dir)
            yield run, test_dir, exp["rc-command"], exp["expectation"], test_type
        rdm.terminate()
        rdm.wait()
