#!/usr/bin/env python

import argparse
import subprocess as sp
import json
import os

parser = argparse.ArgumentParser(description='Function tests for rtags')
parser.add_argument('--binary_path', '-b', required=True,
                    help='directory path to the binaries')

args = parser.parse_args()
binary_path = args.binary_path


def log(s):
    print s


def run_rc(args):
    args = [os.path.join(binary_path, "rc")] + args
    # Do the query
    try:
        out = sp.check_output(args)
        log("rc output: " + out)
    except sp.CalledProcessError as e:
        log("rc err: " + e.output)
        log("rc cmd: " + str(e.cmd))
        raise
    return out


def wait_for(p, match):
    while p.poll() is None:
        l = p.stdout.readline()  # This blocks until it receives a newline.
        print l
        if match in l:
            break


def init_test(name):
    cwd = os.getcwd()
    test_wd = os.path.join(cwd, name)

    cdb = [{"directory": test_wd,
            "command": "clang++ -c main.cpp",
            "file": "main.cpp"}]
    cdb_path = os.path.join(test_wd, 'compile_commands.json')
    with open(cdb_path, 'w') as outfile:
        json.dump(cdb, outfile)
        outfile.write("\n")

    # Start rdm
    rdm = sp.Popen(
        [os.path.join(binary_path, "rdm")],
        stdout=sp.PIPE, stderr=sp.STDOUT)
    wait_for(rdm, "Includepaths")

    # Clean projects
    out = run_rc(["-C"])
    wait_for(rdm, "rc -C")

    # Load project
    out = run_rc(["-J", test_wd])
    wait_for(rdm, "Jobs took")

    # Do the query
    out = run_rc(
        ["--follow-location", os.path.join(test_wd, "main.cpp") + ":4:5"])

    # Finish rdm
    rdm.terminate()
    rdm.wait()

init_test('test_test')
# exit(0)
