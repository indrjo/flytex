#!/usr/bin/env python3
import re
import sys
from subprocess import Popen, PIPE
try:  # argparse (>3.2) can be omitted with a simplified sys.argv logic below
    from argparse import ArgumentParser
    ARGPARSE_ENABLED = True
except Exception:
    ARGPARSE_ENABLED = False
try:  # If we cannot load shutil.which (>3.3), fallback to plain text
    from shutil import which
except Exception:
    def which(cmd: str) -> str:  # type: ignore
        return cmd


class Log:
    @staticmethod
    def err(app: str, msg: str) -> None:
        print('[{}-error] {}'.format(app, msg), file=sys.stderr)

    @staticmethod
    def info(app: str, msg: str) -> None:
        print('[{}] {}'.format(app, msg))


# --------------------------------------------------------------------------
# MAKE THE UNDERLYING SYSTEM DO THINGS
# --------------------------------------------------------------------------

# There ought be no surprise if we say this program intimately relies on
# the OS that hosts this program. Our ideal user, as we have already said,
# is a GNU/Linux user, or even a *nix one.

class Shell:
    class Result:  # equivalent to subprocess.CompletedProcess (>3.5)
        def __init__(self, returncode: int, stdout: bytes, stderr: bytes):
            self.returncode = returncode
            self.stdout = stdout
            self.stderr = stderr

        def findall(self, regex: re.Pattern) -> list:
            return [x.decode('utf8') for x in regex.findall(self.stdout)]

    @staticmethod
    def run(args: list, inp: bytes = b'') -> Result:
        Log.info('flytex', 'running: {} ...'.format(' '.join(args)))
        exe_fullpath = which(args[0])
        if exe_fullpath:
            args[0] = exe_fullpath
        else:
            Log.err('os', 'Could not find executable "{}"'.format(args[0]))
            exit(1)

        proc = Popen(args, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        out, err = proc.communicate(input=inp)
        return Shell.Result(proc.returncode, out, err)


# --------------------------------------------------------------------------
# INVOKING TLMGR
# --------------------------------------------------------------------------

# A minimal TeX Live has tlmgr who handles packages: not only you install
# packages with it, but you can search packages containing a given file!
# They are both interesting for our purpose.

class Tlmgr:
    ''' Helper class to interact with the tlmgr program. '''

    # `$ tlmgr search` has the following output format:
    # package:
    #   path1/package.sty
    #   path2/other.sty
    #   ...
    #
    # Thus we want to match all lines ending in colon
    RX_SEARCH = re.compile(rb'^(.+):$', re.MULTILINE)

    @staticmethod
    def search(fname: str) -> list:  # Optional[List[str]]
        '''
        Wrapper for `$ tlmgr search --global --file "/<pkg>"`

        Always search for "/package.sty" and not just "package.sty" !!!
        Otherwise, "caption" will also find "ccaption", "lwarp", etc.

        Return: `None` if error, `[]` if nothing found, else: list of packages
        '''
        ans = Shell.run(['tlmgr', 'search', '--global', '--file', '/' + fname])
        if ans.returncode == 0:
            return ans.findall(Tlmgr.RX_SEARCH)
        Log.err('tlmgr', ans.stderr.decode('utf8'))
        return None  # type: ignore[return-value]

    @staticmethod
    def install(package: str) -> bool:
        ''' A simple wrapper for `$ tlmgr install <pkg>` '''
        if Shell.run(['tlmgr', 'install', package]).returncode == 0:
            Log.info('tlmgr', 'installed "{}"'.format(package))
            return True
        Log.err('tlmgr', 'cannot install "{}"!'.format(package))
        return False

    @staticmethod
    def install_all(packages: list) -> bool:
        ''' Install all packages one by one until finished or one fails. '''
        # all() breaks on the first False and thus stops further installs
        return all(Tlmgr.install(pkg) for pkg in packages)

    @staticmethod
    def search_and_install(missing_file: str) -> bool:
        ''' Search and install all packages containing a given file. '''
        packages = Tlmgr.search(missing_file)
        Log.info('tlmgr', 'downloading {} missing packages: {}'.format(
            len(packages), packages))
        if packages and Tlmgr.install_all(packages):
            Log.info('tlmgr', 'all missing packages for "{}" installed'.format(
                missing_file))
            return True
        if packages == []:
            Log.err('tlmgr', 'nothing to install for "{}"!'.format(
                missing_file))
        return False

    @staticmethod
    def search_and_install_all(missing_files: list) -> bool:
        '''Process all missing files one by one until finished or one fails.'''
        return all(Tlmgr.search_and_install(fname) for fname in missing_files)


# --------------------------------------------------------------------------
# TEXLIVE ON THE FLY
# --------------------------------------------------------------------------

class FlyTex:
    RX_ERROR_LINES = re.compile(rb'^!.+$', re.MULTILINE)
    RX_MISSING = re.compile(rb'! (?:La)*TeX Error: File `([^\']+)\' not found')

    @staticmethod
    def run(texer: str, args: list) -> None:
        ''' Inspect output to get the names of the missing packages. '''
        # This trick gathers all missing files in one compilation.
        # Sending the return key '\n' so many times will load up to 100 errors
        ans = Shell.run([texer] + args, b'\n' * 99)
        missing = ans.findall(FlyTex.RX_MISSING)
        if not missing:
            if ans.returncode != 0:
                for error in ans.findall(FlyTex.RX_ERROR_LINES):
                    Log.err(texer, error)
            exit(1)
        Log.info('flytex', 'found {} missing files: {}'.format(
            len(missing), missing))
        Tlmgr.search_and_install_all(missing)
        Log.info('flytex', 'END!')


# -------------------------------------------------------------------------
# MAIN
# -------------------------------------------------------------------------

if __name__ == '__main__':
    if not ARGPARSE_ENABLED:  # if python < 3.2
        assert len(sys.argv) >= 3  # this_file, texer, tex_file
        FlyTex.run(sys.argv[1], sys.argv[2:])
        exit(0)

    ArgumentParser()
    parser = ArgumentParser(description='TeX Live on the fly')
    parser.add_argument('texer', metavar='TEXER',
                        help='Compiler to use. e.g., pdflatex or lualatex')
    parser.add_argument('args', metavar='arg', nargs='+',
                        help='Arguments passed to your latex compiler as is. '
                        'At least one of the args must point to a tex file.')
    res = parser.parse_args()
    FlyTex.run(res.texer, res.args)
