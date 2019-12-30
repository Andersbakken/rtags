import enum
import json
import os
import os.path
import subprocess as sp
import sys
import time

import pytest

TESTS = None
TESTS_NAME = None


def is_exe(path):
    if not (os.path.isfile(path) and os.access(path, os.X_OK)):
        ValueError(path, 'file does not exist or is not executable')


class RTags():
    '''RTags class for rdm/rc tests'''

    try:
        __rdm_exe = os.path.join(os.environ['RTAGS_BINARY_DIR'], 'rdm')
        __rc_exe = os.path.join(os.environ['RTAGS_BINARY_DIR'], 'rc')
        is_exe(__rdm_exe)
        is_exe(__rc_exe)
    except KeyError:
        print('You need to set RTAGS_BINARY_DIR environment variable.', file=sys.stderr)
        sys.exit(1)
    except ValueError as err:
        print(str(err), file=sys.stderr)
        sys.exit(1)

    __socket_file = '/var/tmp/rdm_dev'

    def __init__(self):
        self._rdm_p = None

    def rc(self, *args):
        '''Call rc with args.

        :params *args: Variable argument list, can also contain lists.
        '''
        rc_args = [self.__rc_exe, '--socket-file', self.__socket_file]

        for arg in args:
            if isinstance(arg, list):
                rc_args += arg
            else:
                rc_args.append(arg)

        return sp.check_output(rc_args).decode()

    def parse(self, directory, files):
        '''Parse files from directory.

        :param directory: The files location
        :param files: The files to parse
        '''
        compile_commands_g = (
            'clang++ -std=c++11 -I. -c ' + os.path.join(directory, test_file)
            for test_file in (src_file for src_file in files if src_file.endswith(('.cpp', '.c')))
        )

        for compile_command in compile_commands_g:
            self.rc('--project-root', directory, '-c', compile_command)

            # Wait until the file is indexed
            while True:
                try:
                    self.rc('--is-indexing')
                    break
                except sp.CalledProcessError:
                    time.sleep(0.01)

    def rdm(self, data_dir):
        '''Start rdm.

        :param data_dir: The rdm data directory
        '''
        if self._rdm_p:
            return

        self._rdm_p = sp.Popen(
            [self.__rdm_exe, '--socket-file', self.__socket_file, '-d', data_dir, '-o', '-B', '--log-flush'],
            stdout=sp.PIPE,
            stderr=sp.STDOUT,
        )

        # Wait until rdm is ready
        while True:
            try:
                self.rc('-w')
                break
            except sp.CalledProcessError:
                time.sleep(0.01)

    def rdm_quit(self):
        '''Quit rdm.'''
        self._rdm_p.terminate()
        self._rdm_p.wait()


@pytest.yield_fixture(scope='session', autouse=True)
def rtags(tmpdir_factory):
    '''RTags session yield fixture.

    Start rdm and return the RTags object.
    At the and of the session the rdm process will be stopped.

    :param tmpdir_factory: The tmpdir_factory fixture
    '''
    rtags = RTags()
    rtags.rdm(str(tmpdir_factory.mktemp('data_dir')))
    yield rtags
    rtags.rdm_quit()


class Location():
    '''Class representing location in file.'''

    def __init__(self, file, line, col):
        self.file, self.line, self.col = str(file), int(line), int(col)

    @classmethod
    def from_str(cls, string):
        '''From string.'''
        return cls(*string.split(':')[0:3])

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        else:
            raise ValueError('Type error')

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        return '%s:%d:%d' % (self.file, self.line, self.col)


class TType(enum.IntEnum):
    '''Enum class representing the test types we support.'''
    LOCATION = 1
    PARSE = 2
    COMPLETION = 3
    OUTPUT = 4

    @classmethod
    def get(cls, test_dir):
        '''Return test type derived from the directory name.'''
        if 'Parsing' in test_dir:
            return cls.PARSE

        if 'Completion' in test_dir:
            return cls.COMPLETION

        if 'Output' in test_dir:
            return cls.OUTPUT

        return cls.LOCATION


def collect_tests():
    '''Helper function to gather all tests and to not pollute the global scope.'''
    global TESTS, TESTS_NAME

    TESTS = {t: [] for t in TType}
    TESTS_NAME = {t: [] for t in TType}

    for test_dir, _, test_files in tuple(os.walk(os.path.dirname(os.path.abspath(__file__))))[1:]:
        if 'expectation.json' not in test_files:
            continue

        ttype = TType.get(os.path.basename(test_dir))
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
@pytest.mark.parametrize('directory,files,expectations', TESTS[TType.LOCATION], ids=TESTS_NAME[TType.LOCATION])
def test_location(directory, files, expectations, rtags):
    rtags.parse(directory, files)
    for exp in expectations:
        expected_locations = exp['expectation']
        actual_locations = [
            Location(os.path.join(directory, line.split(':')[0]), line.split(':')[1], line.split(':')[2])
            for line in
            rtags.rc([c.format(directory) for c in exp['rc-command']]).split('\n')
            if len(line) > 0
        ]
        # Compare that we have the same results in length and content
        assert len(actual_locations) == len(expected_locations)
        for expected_location_string in expected_locations:
            expected_location = Location.from_str(expected_location_string.format(directory))
            assert expected_location in actual_locations


@pytest.mark.parametrize('directory,files,expectations', TESTS[TType.PARSE], ids=TESTS_NAME[TType.PARSE])
def test_parse(directory, files, expectations, rtags):
    rtags.parse(directory, files)
    for exp in expectations:
        output = rtags.rc(exp['rc-command'])
        assert output
        assert exp['expectation'].format(directory + '/') in output.split()


@pytest.mark.parametrize('directory,files,expectations', TESTS[TType.COMPLETION], ids=TESTS_NAME[TType.COMPLETION])
def test_completion(directory, files, expectations, rtags):
    rtags.parse(directory, files)
    for exp in expectations:
        expected = exp['expectation']
        outputs = rtags.rc([c.format(directory) for c in exp['rc-command']]).split('\n')
        assert len(outputs) == len(expected)
        for output in outputs:
            assert output in expected


@pytest.mark.parametrize('directory,files,expectations', TESTS[TType.OUTPUT], ids=TESTS_NAME[TType.OUTPUT])
def test_output(directory, files, expectations, rtags):
    rtags.parse(directory, files)
    for exp in expectations:
        expected = exp['expectation']
        actual_outputs = [
            line
            for line in
            rtags.rc([c.format(directory) for c in exp['rc-command']]).split('\n')
            if len(line) > 0
        ]
        # Compare that we have the same results in length and content
        assert len(expected) == len(actual_outputs)
        for expected_output_string in expected:
            expected_output = expected_output_string.format(directory)
            assert expected_output in actual_outputs
