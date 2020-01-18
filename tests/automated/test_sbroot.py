import glob
import json
import os
import os.path
import shutil
import subprocess as ps

import pytest

from . import utils


@pytest.fixture
def setup(tmp_path_factory):
    base_tmp_dir = str(tmp_path_factory.getbasetemp())
    src_dir = os.path.join(os.path.dirname(__file__), 'sbroot_test')
    sbroot = os.path.join(base_tmp_dir, 'sbroot_test')
    rtags = utils.RTags(os.path.join(base_tmp_dir, '.socket'))

    shutil.copytree(src_dir, sbroot)
    yield (rtags, sbroot)


def index_navigate(rtags, sbroot):
    for makefile in glob.glob(os.path.join(sbroot, '*', 'Makefile')):
        rtags.parse(
            os.path.dirname(makefile),
            os.listdir(os.path.dirname(makefile)),
            sbroot,
            [
                compile_command for compile_command in
                ps.check_output(['make', '-nk', '-C', os.path.dirname(makefile)]).decode().split('\n')
                if compile_command.startswith('g++ -c')
            ],
        )

    navigate(rtags, sbroot)


def navigate(rtags, sbroot):
    expectations = json.load(open(os.path.join(sbroot, 'expectation.json'), 'r'))
    utils.navigate(rtags, sbroot, expectations)


# pylint: disable=unused-variable,redefined-outer-name
def test_sbroot(request, setup):
    no_sandbox_root_check = request.config.getoption('--no-sandbox-root-check')
    rtags, sbroot = setup

    # Navigate code *WITHOUT* the --sandbox-root switch
    rtags.rdm(sbroot)
    index_navigate(rtags, sbroot)
    rtags.rdm_stop()

    # Navigate code with the --sandbox-root switch
    shutil.rmtree(os.path.join(sbroot, '.rtags'))
    rtags.rdm(sbroot, True)
    index_navigate(rtags, sbroot)
    rtags.rdm_stop()

    # Navigate code with the sandbox moved to a new location
    new_dest_dir = sbroot + '_move'
    shutil.move(sbroot, new_dest_dir)
    sbroot = new_dest_dir
    rtags.rdm(sbroot, True)
    index_navigate(rtags, sbroot)
