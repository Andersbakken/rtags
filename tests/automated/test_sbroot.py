import glob
import json
import os
import os.path
import re
import shutil
import subprocess as ps

import pytest
from _pytest.fixtures import FixtureRequest
from _pytest.tmpdir import TempPathFactory

from . import utils

LOGFILE_COUNT = 1


@pytest.fixture
def setup(tmp_path_factory: TempPathFactory):
    tmp_directory = str(tmp_path_factory.getbasetemp())
    src_dir = os.path.join(os.path.dirname(__file__), 'sbroot_test')
    sbroot = os.path.join(tmp_directory, 'sbroot_test')
    shutil.copytree(src_dir, sbroot)
    yield sbroot


def construct_compile_re(makefile_dir):
    make_output = ps.check_output(['make', '-pnC', makefile_dir]).decode().split('\n')
    compilers = []

    for line in make_output:
        match = re.match(r'(?:CC|CXX) = (.+)', line)
        if match:
            compilers.append(match.group(1))

    compilers = [re.escape(c) for c in compilers]  # g++, clang++, ... need to be escaped
    return r'({}) -c'.format('|'.join(compilers))


def index_navigate(rtags: utils.RTags, sbroot: str):
    makefiles = glob.glob(os.path.join(sbroot, '*', 'Makefile'))
    compiler_re = construct_compile_re(os.path.dirname(makefiles[0]))

    for makefile in makefiles:
        makefile_dir = os.path.dirname(makefile)
        make_output = ps.check_output(['make', '-nkC', makefile_dir]).decode().split('\n')
        compile_commands = [
            compile_command for compile_command in make_output if re.match(compiler_re, compile_command)
        ]
        rtags.parse(makefile_dir, os.listdir(makefile_dir), sbroot, compile_commands)

    utils.navigate(rtags, sbroot, json.load(open(os.path.join(sbroot, 'expectation.json'), 'r')))


def rename_log(sbroot: str):
    # pylint: disable=global-statement
    global LOGFILE_COUNT
    log_file = os.path.join(sbroot, '.rtags', 'rdm.log')
    os.rename(log_file, log_file + str(LOGFILE_COUNT))
    LOGFILE_COUNT += 1


# pylint: disable=unused-variable,redefined-outer-name
def test_sbroot(request: FixtureRequest, setup: str):
    no_sandbox_root_check = request.config.getoption('--no-sandbox-root-check')
    sbroot = setup
    rtags = utils.RTags(sbroot)

    # Navigate code *WITHOUT* the --sandbox-root switch
    rtags.rdm()
    index_navigate(rtags, sbroot)
    rtags.rc('--clear')  # Clear projects
    rename_log(sbroot)

    # Navigate code with the --sandbox-root switch
    rtags.rdm(True)
    index_navigate(rtags, sbroot)
    rtags.rdm_stop()
    rename_log(sbroot)

    # Navigate code with the sandbox moved to a new location
    new_sbroot = sbroot + '_move'
    shutil.move(sbroot, new_sbroot)
    rtags.test_directory = new_sbroot
    rtags.rdm(True)
    index_navigate(rtags, new_sbroot)
