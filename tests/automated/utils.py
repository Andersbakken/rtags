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

    def __init__(self, socket_file=None):
        '''Init object.

        :param socket_file: The socket file, if not specified the default socket file will be used.
        '''
        self._socket_file = socket_file
        self._rdm_p = None
        self._sbroot = None

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

        if not ('--socket-file' in sp_args or '-n' in sp_args) and self._socket_file:
            sp_args.insert(1, '--socket-file')
            sp_args.insert(2, self._socket_file)

    def rc(self, *args):
        '''Call rc with args.

        :params *args: Variable arguments
        '''
        rc_args = [self.__rc_exe]

        self._add_args(rc_args, args)
        try:
            output = sp.check_output(rc_args, stderr=sp.STDOUT).decode()
        except sp.CalledProcessError as err:
            sys.stderr.write(
                '{}: returncode({}) cmd({})'.format(err.output.decode(), err.returncode, ' '.join(err.cmd))
            )
            sys.exit(err.returncode)

        return output

    def _rc_call_wait(self, *args):
        tries = 0

        while True:
            try:
                output = self.rc(args)
                if '--is-indexing' in args and int(output) == 1:
                    raise sp.CalledProcessError(1, args, b'')
                break
            except sp.CalledProcessError as err:
                sys.stderr.write(err.output.decode())

                if tries >= self.__max_retries:
                    sys.stderr.write(
                        'To many retries({}): returncode({}) cmd({})\n'.format(
                            tries, err.returncode, ' '.join(err.cmd)
                        )
                    )
                    sys.exit(err.returncode)

                time.sleep(self.__sleep_time)
                tries += 1

    def rdm(self, sbroot, relative_sbroot=False):
        '''Start rdm.

        :params tmp_data_dir: The temporary directory under which the '.rtags/{db,rdm.log}'
        directories/files will be created.
        '''
        if self._rdm_p:
            return

        self._sbroot = sbroot

        rdm_args = [
            self.__rdm_exe,
            '--no-rc',
            '--enable-compiler-manager',
            '--log-file-log-level=debug',
            '--exclude-filter=/none',  # we want to index /tmp so add `--exclude-filter /none`
            '--watch-sources-only',
            '--data-dir={}/.rtags/db'.format(sbroot),
            '--log-file={}/.rtags/rdm.log'.format(sbroot),
        ]

        rtags_base_data_dir = os.path.join(sbroot, '.rtags')
        if not os.path.exists(rtags_base_data_dir):
            os.mkdir(rtags_base_data_dir)

        self._add_args(rdm_args, ['--sandbox-root', sbroot ] if relative_sbroot else [])
        self._rdm_p = sp.Popen(rdm_args)

        try:
            # Check if rdm printed an error message
            sys.stderr.write(self._rdm_p.communicate(timeout=0.2)[0].decode())
        except sp.TimeoutExpired:
            pass

        # Has rdm terminated
        returncode = self._rdm_p.poll()
        if returncode:
            sys.exit(returncode)

        self._rc_call_wait('-w')  # Wait until rdm is ready

    def rdm_stop(self):
        '''Quit rdm.'''
        if self._rdm_p:
            self._rdm_p.terminate()
            self._rdm_p.wait()
            self._rdm_p = self._sbroot = None

    def parse(self, directory, files, project_root=None, compile_commands=None):
        '''Parse files from directory.

        :param directory: The files location
        :param files: The files to parse
        :param project_root: The project root directory, defaults to ``directory``
        :param compile_commands: The compile_commands, if none where specified, they will be \
        generated for all src files (.cpp, .c).
        '''
        src_files = [
            os.path.join(directory, src_file)
            for src_file in files
            if src_file.endswith(('.cpp', '.c'))
        ]

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
            self._rc_call_wait('--is-indexing', src_files[i])


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

        raise ValueError('Type error')

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        return '%s:%d:%d' % (self.file, self.line, self.col)
