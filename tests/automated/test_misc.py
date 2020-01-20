import enum
import glob
import json
import os
import os.path

from typing import List

import pytest
from _pytest.tmpdir import TempPathFactory

from . import utils

TESTS = None
TESTS_NAME = None


@pytest.yield_fixture(scope='module', autouse=True)
def rtags(tmp_path_factory: TempPathFactory):
    '''RTags session yield fixture.

    Start rdm at module scope, and return the RTags object. After all tests from this module have
    been run, the rdm process will be terminated.
    '''
    tmp_directory = str(tmp_path_factory.mktemp('misc_test'))
    _rtags = utils.RTags(tmp_directory)
    _rtags.rdm()
    yield _rtags


class TType(enum.IntEnum):
    '''Enum class representing the test types we support.'''
    LOCATION = 1
    PARSE = 2
    COMPLETION = 3
    OUTPUT = 4

    @classmethod
    def from_directory(cls, directory):
        '''Return test type derived from directory.'''
        if 'Parsing' in directory:
            return cls.PARSE

        if 'Completion' in directory:
            return cls.COMPLETION

        if 'Output' in directory:
            return cls.OUTPUT

        return cls.LOCATION


def collect_tests():
    '''Helper function to gather all tests, and to not pollute the global scope.'''
    # pylint: disable=global-statement
    global TESTS, TESTS_NAME

    TESTS = {t: [] for t in TType}
    TESTS_NAME = {t: [] for t in TType}
    os.chdir(os.path.dirname(__file__))
    pytest_files = glob.glob('test_*.py')

    for test_dir, _, test_files in tuple(os.walk(os.getcwd()))[1:]:
        if 'expectation.json' not in test_files:
            continue

        # Ignore any test for which a unique test file exists even though it contains a
        # expectation.json file.
        test = os.path.basename(test_dir).split('_')
        if len(test) == 2:
            test_file = '{1}_{0}.py'.format(*test)
            if any((f == test_file for f in pytest_files)):
                continue

        ttype = TType.from_directory(os.path.basename(test_dir))
        expectations = json.load(open(os.path.join(test_dir, 'expectation.json'), 'r'))
        TESTS[ttype].append([test_dir, test_files, expectations])
        TESTS_NAME[ttype].append(os.path.basename(test_dir))


###
# Collect all tests
###
collect_tests()


###
# Tests
###
# pylint: disable=redefined-outer-name
@pytest.mark.parametrize('directory,files,expectations', TESTS[TType.LOCATION], ids=TESTS_NAME[TType.LOCATION])
def test_location(directory: str, files: list, expectations: List[dict], rtags: utils.RTags):
    rtags.parse(directory, files)
    utils.navigate(rtags, directory, expectations)


@pytest.mark.parametrize('directory,files,expectations', TESTS[TType.PARSE], ids=TESTS_NAME[TType.PARSE])
def test_parse(directory: str, files: list, expectations: List[dict], rtags: utils.RTags):
    rtags.parse(directory, files)
    for exp in expectations:
        output = rtags.rc(exp['rc-command'])
        assert output
        assert exp['expectation'].format(directory + '/') in output.split()


@pytest.mark.parametrize('directory,files,expectations', TESTS[TType.COMPLETION], ids=TESTS_NAME[TType.COMPLETION])
def test_completion(directory: str, files: list, expectations: List[dict], rtags: utils.RTags):
    rtags.parse(directory, files)
    for exp in expectations:
        expected = exp['expectation']
        outputs = rtags.rc([c.format(directory) for c in exp['rc-command']]).split('\n')
        assert len(outputs) == len(expected)
        for output in outputs:
            assert output in expected


@pytest.mark.parametrize('directory,files,expectations', TESTS[TType.OUTPUT], ids=TESTS_NAME[TType.OUTPUT])
def test_output(directory: str, files: list, expectations: List[dict], rtags: utils.RTags):
    rtags.parse(directory, files)
    for exp in expectations:
        expected = exp['expectation']
        actual_outputs = [
            line for line in rtags.rc([c.format(directory) for c in exp['rc-command']]).split('\n') if len(line) > 0
        ]
        # Compare that we have the same results in length and content
        assert len(expected) == len(actual_outputs)
        for expected_output_string in expected:
            expected_output = expected_output_string.format(directory)
            assert expected_output in actual_outputs
