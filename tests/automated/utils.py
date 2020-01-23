import glob
import os.path
import re
import subprocess as sp
import sys
import time


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

    __sleep_time = 0.1  # Sleep time after command failed
    __max_retries = 100  # Maximal retries when command failed

    def __init__(self, test_directory: str):
        self._socket_file = None
        self._rdm_p = None
        self.test_directory = test_directory

    @property
    def test_directory(self):
        '''The test directory under which all generated data will be stored.'''
        return self._test_directory

    @test_directory.setter
    def test_directory(self, test_directory: str):
        self._test_directory = test_directory

        rtags_base_data_dir = os.path.join(self.test_directory, '.rtags')
        if not os.path.exists(rtags_base_data_dir):
            os.mkdir(rtags_base_data_dir)

        self._socket_file = os.path.join(self.test_directory, '.socket')

    def __del__(self):
        self.rdm_stop()

    def _add_args(self, sp_args, *args):
        '''Add ``*args`` to sp_args.

        Also add --socket-file argument right after the executable if not present and self._socket_file is set.
        :param sp_args: The subprocess argument list
        :param *args: Variable arguments
        '''
        for arg in args:
            if not isinstance(arg, str):
                # Unpack list/tuple
                self._add_args(sp_args, *arg)
            else:
                sp_args.append(arg)

        if not ('--socket-file' in sp_args or '-n' in sp_args):
            sp_args.insert(1, '--socket-file')
            sp_args.insert(2, self._socket_file)

    def _dump_log_files(self):
        for log_file in glob.glob(os.path.join(self.test_directory, '.rtags', 'rdm.log*')):
            print('-' * 120)
            print('-- {} '.format(log_file))
            print('-' * 120)
            with open(log_file) as f:
                while True:
                    substring = f.read(65536)
                    if not substring:
                        break
                    sys.stderr.write(substring)

    def _rc_error(self, tries, err):
        if tries >= self.__max_retries:
            sys.stderr.write(err.output.decode())
            sys.stderr.write(
                'To many retries({}): returncode({}) cmd({})'.format(tries, err.returncode, ' '.join(err.cmd))
            )
            self.rdm_stop()
            self._dump_log_files()
            sys.exit(err.returncode)

        time.sleep(self.__sleep_time)

    # pylint: disable=invalid-name
    def rc(self, *args):
        '''Call rc with args.

        :params *args: Variable arguments
        '''
        tries = 0
        rc_args = [self.__rc_exe]
        output = ''

        self._add_args(rc_args, args)

        while True:
            try:
                output = sp.check_output(rc_args, stderr=sp.STDOUT).decode()
                if output == '1\n':
                    raise sp.CalledProcessError(int(output.strip()), rc_args)
                break
            except sp.CalledProcessError as err:
                self._rc_error(tries, err)
                tries += 1

        return output

    def rdm(self, relative_sbroot=False):
        '''Start rdm.'''
        rdm_args = [
            self.__rdm_exe,
            '--no-rc',
            '--enable-compiler-manager',
            '--log-file={}/.rtags/rdm.log'.format(self.test_directory),
            '--log-file-log-level=debug',
            '--exclude-filter=/none',  # we want to index /tmp so add `--exclude-filter /none`
            '--watch-sources-only',
            '--data-dir={}/.rtags/db'.format(self.test_directory),
        ]

        self._add_args(rdm_args, ['--sandbox-root', self.test_directory] if relative_sbroot else [])
        self.rdm_stop()  # Quit rdm if rdm is running
        self._rdm_p = sp.Popen(rdm_args)
        self.rc('-w')  # Wait until rdm is ready

    def rdm_stop(self):
        '''Quit rdm.'''
        if self._rdm_p:
            self._rdm_p.terminate()
            self._rdm_p.wait()
            self._rdm_p = None

    def parse(self, directory, files, project_root=None, compile_commands=None):
        '''Parse files from directory.

        :param directory: The files location
        :param files: The files to parse
        :param project_root: The project root directory, defaults to ``directory``
        :param compile_commands: The compile_commands, if none where specified, they will be \
        generated for all src files (.cpp, .c).
        '''
        src_files = [os.path.join(directory, src_file) for src_file in files if src_file.endswith(('.cpp', '.c'))]

        if not compile_commands:
            compile_commands = ('clang++ -std=c++11 -I. -c ' + src_file for src_file in src_files)
        else:
            # The source file might be specified as relative path, that is doomed, so replace it
            # with the absolute variant.
            for i, _ in enumerate(compile_commands):
                compile_commands[i] = re.sub(r' [/\w]+\.(cpp|c)$', ' {}'.format(src_files[i]), compile_commands[i])

        if not project_root:
            project_root = directory

        os.chdir(directory)

        for i, compile_command in enumerate(compile_commands):
            self.rc('--project-root', project_root, '-c', compile_command)
            self.rc('--is-indexing', src_files[i])


def navigate(rtags: RTags, directory: str, expectations: dict):
    for exp in expectations:
        commands = exp['rc-command']
        for i, command in enumerate(commands):
            if '{0}' in command:
                commands[i] = command.format(directory)

        output = [line for line in rtags.rc(commands).split('\n') if len(line) > 0]
        # Compare that we have the same results in length and content
        assert len(output) == len(exp['expectation'])
        for line in output:
            if line not in exp['expectation']:
                sys.stderr.write('{} not in {}'.format(line, exp['expectation']))
            assert line in exp['expectation']
