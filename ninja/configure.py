#!/usr/bin/env python
#
# Copyright 2001 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Script that generates the build.ninja for ninja itself.

Projects that use ninja themselves should either write a similar script
or use a meta-build system that supports Ninja output."""

from __future__ import print_function

from optparse import OptionParser
import os
import shlex
import string
import subprocess
import sys

sourcedir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.join(sourcedir, 'misc'))
import ninja_syntax


class Platform(object):
    """Represents a host/target platform and its specific build attributes."""
    def __init__(self, platform):
        self._platform = platform
        if self._platform is not None:
            return
        self._platform = sys.platform
        if self._platform.startswith('linux'):
            self._platform = 'linux'
        elif self._platform.startswith('freebsd'):
            self._platform = 'freebsd'
        elif self._platform.startswith('gnukfreebsd'):
            self._platform = 'freebsd'
        elif self._platform.startswith('openbsd'):
            self._platform = 'openbsd'
        elif self._platform.startswith('solaris') or self._platform == 'sunos5':
            self._platform = 'solaris'
        elif self._platform.startswith('mingw'):
            self._platform = 'mingw'
        elif self._platform.startswith('win'):
            self._platform = 'msvc'
        elif self._platform.startswith('bitrig'):
            self._platform = 'bitrig'
        elif self._platform.startswith('netbsd'):
            self._platform = 'netbsd'
        elif self._platform.startswith('aix'):
            self._platform = 'aix'
        elif self._platform.startswith('dragonfly'):
            self._platform = 'dragonfly'

    @staticmethod
    def known_platforms():
      return ['linux', 'darwin', 'freebsd', 'openbsd', 'solaris', 'sunos5',
              'mingw', 'msvc', 'gnukfreebsd', 'bitrig', 'netbsd', 'aix',
              'dragonfly']

    def platform(self):
        return self._platform

    def is_linux(self):
        return self._platform == 'linux'

    def is_mingw(self):
        return self._platform == 'mingw'

    def is_msvc(self):
        return self._platform == 'msvc'

    def msvc_needs_fs(self):
        popen = subprocess.Popen(['cl', '/nologo', '/?'],
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
        out, err = popen.communicate()
        return b'/FS' in out

    def is_windows(self):
        return self.is_mingw() or self.is_msvc()

    def is_solaris(self):
        return self._platform == 'solaris'

    def is_aix(self):
        return self._platform == 'aix'

    def uses_usr_local(self):
        return self._platform in ('freebsd', 'openbsd', 'bitrig', 'dragonfly', 'netbsd')

    def supports_ppoll(self):
        return self._platform in ('freebsd', 'linux', 'openbsd', 'bitrig',
                                  'dragonfly')

    def supports_ninja_browse(self):
        return False

    def can_rebuild_in_place(self):
        return not (self.is_windows() or self.is_aix())

class Bootstrap:
    """API shim for ninja_syntax.Writer that instead runs the commands.

    Used to bootstrap Ninja from scratch.  In --bootstrap mode this
    class is used to execute all the commands to build an executable.
    It also proxies all calls to an underlying ninja_syntax.Writer, to
    behave like non-bootstrap mode.
    """
    def __init__(self, writer, verbose=False):
        self.writer = writer
        self.verbose = verbose
        # Map of variable name => expanded variable value.
        self.vars = {}
        # Map of rule name => dict of rule attributes.
        self.rules = {
            'phony': {}
        }

    def comment(self, text):
        return self.writer.comment(text)

    def newline(self):
        return self.writer.newline()

    def variable(self, key, val):
        # In bootstrap mode, we have no ninja process to catch /showIncludes
        # output.
        self.vars[key] = self._expand(val).replace('/showIncludes', '')
        return self.writer.variable(key, val)

    def rule(self, name, **kwargs):
        self.rules[name] = kwargs
        return self.writer.rule(name, **kwargs)

    def build(self, outputs, rule, inputs=None, **kwargs):
        ruleattr = self.rules[rule]
        cmd = ruleattr.get('command')
        if cmd is None:  # A phony rule, for example.
            return

        # Implement just enough of Ninja variable expansion etc. to
        # make the bootstrap build work.
        local_vars = {
            'in': self._expand_paths(inputs),
            'out': self._expand_paths(outputs)
        }
        for key, val in kwargs.get('variables', []):
            local_vars[key] = ' '.join(ninja_syntax.as_list(val))

        self._run_command(self._expand(cmd, local_vars))

        return self.writer.build(outputs, rule, inputs, **kwargs)

    def default(self, paths):
        return self.writer.default(paths)

    def _expand_paths(self, paths):
        """Expand $vars in an array of paths, e.g. from a 'build' block."""
        paths = ninja_syntax.as_list(paths)
        return ' '.join(map(self._shell_escape, (map(self._expand, paths))))

    def _expand(self, str, local_vars={}):
        """Expand $vars in a string."""
        return ninja_syntax.expand(str, self.vars, local_vars)

    def _shell_escape(self, path):
        """Quote paths containing spaces."""
        return '"%s"' % path if ' ' in path else path

    def _run_command(self, cmdline):
        """Run a subcommand, quietly.  Prints the full command on error."""
        try:
            if self.verbose:
                print(cmdline)
            subprocess.check_call(cmdline, shell=True)
        except subprocess.CalledProcessError:
            print('when running: ', cmdline)
            raise


parser = OptionParser()
profilers = ['gmon', 'pprof']
parser.add_option('--bootstrap', action='store_true',
                  help='bootstrap a ninja binary from nothing')
parser.add_option('--verbose', action='store_true',
                  help='enable verbose build')
parser.add_option('--platform',
                  help='target platform (' +
                       '/'.join(Platform.known_platforms()) + ')',
                  choices=Platform.known_platforms())
parser.add_option('--host',
                  help='host platform (' +
                       '/'.join(Platform.known_platforms()) + ')',
                  choices=Platform.known_platforms())
parser.add_option('--debug', action='store_true',
                  help='enable debugging extras',)
parser.add_option('--profile', metavar='TYPE',
                  choices=profilers,
                  help='enable profiling (' + '/'.join(profilers) + ')',)
parser.add_option('--with-gtest', metavar='PATH', help='ignored')
parser.add_option('--with-python', metavar='EXE',
                  help='use EXE as the Python interpreter',
                  default=os.path.basename(sys.executable))
parser.add_option('--force-pselect', action='store_true',
                  help='ppoll() is used by default where available, '
                       'but some platforms may need to use pselect instead',)
(options, args) = parser.parse_args()
if args:
    print('ERROR: extra unparsed command-line arguments:', args)
    sys.exit(1)

platform = Platform(options.platform)
if options.host:
    host = Platform(options.host)
else:
    host = platform

BUILD_FILENAME = 'build.ninja'
ninja_writer = ninja_syntax.Writer(open(BUILD_FILENAME, 'w'))
n = ninja_writer

if options.bootstrap:
    # Make the build directory.
    try:
        os.mkdir('build')
    except OSError:
        pass
    # Wrap ninja_writer with the Bootstrapper, which also executes the
    # commands.
    print('bootstrapping ninja...')
    n = Bootstrap(n, verbose=options.verbose)

n.comment('This file is used to build ninja itself.')
n.comment('It is generated by ' + os.path.basename(__file__) + '.')
n.newline()

n.variable('ninja_required_version', '1.3')
n.newline()

n.comment('The arguments passed to configure.py, for rerunning it.')
configure_args = sys.argv[1:]
if '--bootstrap' in configure_args:
    configure_args.remove('--bootstrap')
n.variable('configure_args', ' '.join(configure_args))
env_keys = set(['CXX', 'AR', 'CFLAGS', 'CXXFLAGS', 'LDFLAGS'])
configure_env = dict((k, os.environ[k]) for k in os.environ if k in env_keys)
if configure_env:
    config_str = ' '.join([k + '=' + shlex.quote(configure_env[k])
                           for k in configure_env])
    n.variable('configure_env', config_str + '$ ')
n.newline()

CXX = configure_env.get('CXX', 'g++')
objext = '.o'
if platform.is_msvc():
    CXX = 'cl'
    objext = '.obj'

def src(filename):
    return os.path.join('$root', 'src', filename)
def built(filename):
    return os.path.join('$builddir', filename)
def doc(filename):
    return os.path.join('$root', 'doc', filename)
def cc(name, **kwargs):
    return n.build(built(name + objext), 'cxx', src(name + '.c'), **kwargs)
def cxx(name, **kwargs):
    return n.build(built(name + objext), 'cxx', src(name + '.cc'), **kwargs)
def binary(name):
    if platform.is_windows():
        exe = name + '.exe'
        n.build(name, 'phony', exe)
        return exe
    return name

root = sourcedir
if root == os.getcwd():
    # In the common case where we're building directly in the source
    # tree, simplify all the paths to just be cwd-relative.
    root = '.'
n.variable('root', root)
n.variable('builddir', 'build')
n.variable('cxx', CXX)
if platform.is_msvc():
    n.variable('ar', 'link')
else:
    n.variable('ar', configure_env.get('AR', 'ar'))

if platform.is_msvc():
    cflags = ['/showIncludes',
              '/nologo',  # Don't print startup banner.
              '/Zi',  # Create pdb with debug info.
              '/W4',  # Highest warning level.
              '/WX',  # Warnings as errors.
              '/wd4530', '/wd4100', '/wd4706', '/wd4244',
              '/wd4512', '/wd4800', '/wd4702', '/wd4819',
              # Disable warnings about constant conditional expressions.
              '/wd4127',
              # Disable warnings about passing "this" during initialization.
              '/wd4355',
              # Disable warnings about ignored typedef in DbgHelp.h
              '/wd4091',
              '/GR-',  # Disable RTTI.
              # Disable size_t -> int truncation warning.
              # We never have strings or arrays larger than 2**31.
              '/wd4267',
              '/DNOMINMAX', '/D_CRT_SECURE_NO_WARNINGS',
              '/D_HAS_EXCEPTIONS=0',
              '/DNINJA_PYTHON="%s"' % options.with_python]
    if platform.msvc_needs_fs():
        cflags.append('/FS')
    ldflags = ['/DEBUG', '/libpath:$builddir']
    if not options.debug:
        cflags += ['/Ox', '/DNDEBUG', '/GL']
        ldflags += ['/LTCG', '/OPT:REF', '/OPT:ICF']
else:
    cflags = ['-g', '-Wall', '-Wextra',
              '-Wno-deprecated',
              '-Wno-missing-field-initializers',
              '-Wno-unused-parameter',
              '-fno-rtti',
              '-fno-exceptions',
              '-fvisibility=hidden', '-pipe',
              '-DNINJA_PYTHON="%s"' % options.with_python]
    if options.debug:
        cflags += ['-D_GLIBCXX_DEBUG', '-D_GLIBCXX_DEBUG_PEDANTIC']
        cflags.remove('-fno-rtti')  # Needed for above pedanticness.
    else:
        cflags += ['-O2', '-DNDEBUG']
    try:
        proc = subprocess.Popen(
            [CXX, '-fdiagnostics-color', '-c', '-x', 'c++', '/dev/null',
             '-o', '/dev/null'],
            stdout=open(os.devnull, 'wb'), stderr=subprocess.STDOUT)
        if proc.wait() == 0:
            cflags += ['-fdiagnostics-color']
    except:
        pass
    if platform.is_mingw():
        cflags += ['-D_WIN32_WINNT=0x0501']
    ldflags = ['-L$builddir']
    if platform.uses_usr_local():
        cflags.append('-I/usr/local/include')
        ldflags.append('-L/usr/local/lib')
    if platform.is_aix():
        # printf formats for int64_t, uint64_t; large file support
        cflags.append('-D__STDC_FORMAT_MACROS')
        cflags.append('-D_LARGE_FILES')


libs = []

if platform.is_mingw():
    cflags.remove('-fvisibility=hidden');
    ldflags.append('-static')
elif platform.is_solaris():
    cflags.remove('-fvisibility=hidden')
elif platform.is_aix():
    cflags.remove('-fvisibility=hidden')
elif platform.is_msvc():
    pass
else:
    if options.profile == 'gmon':
        cflags.append('-pg')
        ldflags.append('-pg')
    elif options.profile == 'pprof':
        cflags.append('-fno-omit-frame-pointer')
        libs.extend(['-Wl,--no-as-needed', '-lprofiler'])

if platform.supports_ppoll() and not options.force_pselect:
    cflags.append('-DUSE_PPOLL')
if platform.supports_ninja_browse():
    cflags.append('-DNINJA_HAVE_BROWSE')

# Search for generated headers relative to build dir.
cflags.append('-I.')

def shell_escape(str):
    """Escape str such that it's interpreted as a single argument by
    the shell."""

    # This isn't complete, but it's just enough to make NINJA_PYTHON work.
    if platform.is_windows():
      return str
    if '"' in str:
        return "'%s'" % str.replace("'", "\\'")
    return str

if 'CFLAGS' in configure_env:
    cflags.append(configure_env['CFLAGS'])
    ldflags.append(configure_env['CFLAGS'])
if 'CXXFLAGS' in configure_env:
    cflags.append(configure_env['CXXFLAGS'])
    ldflags.append(configure_env['CXXFLAGS'])
n.variable('cflags', ' '.join(shell_escape(flag) for flag in cflags))
if 'LDFLAGS' in configure_env:
    ldflags.append(configure_env['LDFLAGS'])
n.variable('ldflags', ' '.join(shell_escape(flag) for flag in ldflags))
n.newline()

if platform.is_msvc():
    n.rule('cxx',
        command='$cxx $cflags -c $in /Fo$out /Fd' + built('$pdb'),
        description='CXX $out',
        # deps='msvc'  # /showIncludes is included in $cflags.
    )
else:
    n.rule('cxx',
        command='$cxx -MMD -MT $out -MF $out.d $cflags -c $in -o $out',
        depfile='$out.d',
        # deps='gcc',
        description='CXX $out')
n.newline()

if host.is_msvc():
    n.rule('ar',
           command='lib /nologo /ltcg /out:$out $in',
           description='LIB $out')
elif host.is_mingw():
    n.rule('ar',
           command='cmd /c $ar cqs $out.tmp $in && move /Y $out.tmp $out',
           description='AR $out')
else:
    n.rule('ar',
           command='rm -f $out && $ar crs $out $in',
           description='AR $out')
n.newline()

if platform.is_msvc():
    n.rule('link',
        command='$cxx $in $libs /nologo /link $ldflags /out:$out',
        description='LINK $out')
else:
    n.rule('link',
        command='$cxx $ldflags -o $out $in $libs',
        description='LINK $out')
n.newline()

objs = []

if platform.supports_ninja_browse():
    n.comment('browse_py.h is used to inline browse.py.')
    n.rule('inline',
           command='"%s"' % src('inline.sh') + ' $varname < $in > $out',
           description='INLINE $out')
    n.build(built('browse_py.h'), 'inline', src('browse.py'),
            implicit=src('inline.sh'),
            variables=[('varname', 'kBrowsePy')])
    n.newline()

    objs += cxx('browse', order_only=built('browse_py.h'))
    n.newline()

n.comment('the depfile parser and ninja lexers are generated using re2c.')
def has_re2c():
    try:
        proc = subprocess.Popen(['re2c', '-V'], stdout=subprocess.PIPE)
        return int(proc.communicate()[0], 10) >= 1103
    except OSError:
        return False
if has_re2c():
    n.rule('re2c',
           command='re2c -b -i --no-generation-date -o $out $in',
           description='RE2C $out')
    # Generate the .cc files in the source directory so we can check them in.
    n.build(src('depfile_parser.cc'), 're2c', src('depfile_parser.in.cc'))
    n.build(src('lexer.cc'), 're2c', src('lexer.in.cc'))
else:
    print("warning: A compatible version of re2c (>= 0.11.3) was not found; "
           "changes to src/*.in.cc will not affect your build.")
n.newline()

n.comment('Core source files all build into ninja library.')
cxxvariables = []
if platform.is_msvc():
    cxxvariables = [('pdb', 'ninja.pdb')]
for name in ['build',
             'build_log',
             'clean',
             'clparser',
             'debug_flags',
             'depfile_parser',
             'deps_log',
             'disk_interface',
             'dyndep',
             'dyndep_parser',
             'edit_distance',
             'eval_env',
             'graph',
             'graphviz',
             'lexer',
             'line_printer',
             'manifest_parser',
             'metrics',
             'parser',
             'state',
             'string_piece_util',
             'util',
             'version']:
    objs += cxx(name, variables=cxxvariables)
if platform.is_windows():
    for name in ['subprocess-win32',
                 'includes_normalize-win32',
                 'msvc_helper-win32',
                 'msvc_helper_main-win32']:
        objs += cxx(name, variables=cxxvariables)
    if platform.is_msvc():
        objs += cxx('minidump-win32', variables=cxxvariables)
    objs += cc('getopt')
else:
    objs += cxx('subprocess-posix')
if platform.is_aix():
    objs += cc('getopt')
if platform.is_msvc():
    ninja_lib = n.build(built('ninja.lib'), 'ar', objs)
else:
    ninja_lib = n.build(built('libninja.a'), 'ar', objs)
n.newline()

if platform.is_msvc():
    libs.append('ninja.lib')
else:
    libs.append('-lninja')

if platform.is_aix():
    libs.append('-lperfstat')

all_targets = []

n.comment('Main executable is library plus main() function.')
objs = cxx('ninja', variables=cxxvariables)
ninja = n.build(binary('ninja'), 'link', objs, implicit=ninja_lib,
                variables=[('libs', libs)])
n.newline()
all_targets += ninja

if options.bootstrap:
    # We've built the ninja binary.  Don't run any more commands
    # through the bootstrap executor, but continue writing the
    # build.ninja file.
    n = ninja_writer

n.comment('Tests all build into ninja_test executable.')

objs = []
if platform.is_msvc():
    cxxvariables = [('pdb', 'ninja_test.pdb')]

for name in ['build_log_test',
             'build_test',
             'clean_test',
             'clparser_test',
             'depfile_parser_test',
             'deps_log_test',
             'dyndep_parser_test',
             'disk_interface_test',
             'edit_distance_test',
             'graph_test',
             'lexer_test',
             'manifest_parser_test',
             'ninja_test',
             'state_test',
             'string_piece_util_test',
             'subprocess_test',
             'test',
             'util_test']:
    objs += cxx(name, variables=cxxvariables)
if platform.is_windows():
    for name in ['includes_normalize_test', 'msvc_helper_test']:
        objs += cxx(name, variables=cxxvariables)

ninja_test = n.build(binary('ninja_test'), 'link', objs, implicit=ninja_lib,
                     variables=[('libs', libs)])
n.newline()
all_targets += ninja_test


n.comment('Ancillary executables.')

for name in ['build_log_perftest',
             'canon_perftest',
             'depfile_parser_perftest',
             'hash_collision_bench',
             'manifest_parser_perftest',
             'clparser_perftest']:
  if platform.is_msvc():
    cxxvariables = [('pdb', name + '.pdb')]
  objs = cxx(name, variables=cxxvariables)
  all_targets += n.build(binary(name), 'link', objs,
                         implicit=ninja_lib, variables=[('libs', libs)])

n.newline()

n.comment('Generate a graph using the "graph" tool.')
n.rule('gendot',
       command='./ninja -t graph all > $out')
n.rule('gengraph',
       command='dot -Tpng $in > $out')
dot = n.build(built('graph.dot'), 'gendot', ['ninja', 'build.ninja'])
n.build('graph.png', 'gengraph', dot)
n.newline()

n.comment('Generate the manual using asciidoc.')
n.rule('asciidoc',
       command='asciidoc -b docbook -d book -o $out $in',
       description='ASCIIDOC $out')
n.rule('xsltproc',
       command='xsltproc --nonet doc/docbook.xsl $in > $out',
       description='XSLTPROC $out')
docbookxml = n.build(built('manual.xml'), 'asciidoc', doc('manual.asciidoc'))
manual = n.build(doc('manual.html'), 'xsltproc', docbookxml,
                 implicit=[doc('style.css'), doc('docbook.xsl')])
n.build('manual', 'phony',
        order_only=manual)
n.newline()

n.rule('dblatex',
       command='dblatex -q -o $out -p doc/dblatex.xsl $in',
       description='DBLATEX $out')
n.build(doc('manual.pdf'), 'dblatex', docbookxml,
        implicit=[doc('dblatex.xsl')])

n.comment('Generate Doxygen.')
n.rule('doxygen',
       command='doxygen $in',
       description='DOXYGEN $in')
n.variable('doxygen_mainpage_generator',
           src('gen_doxygen_mainpage.sh'))
n.rule('doxygen_mainpage',
       command='$doxygen_mainpage_generator $in > $out',
       description='DOXYGEN_MAINPAGE $out')
mainpage = n.build(built('doxygen_mainpage'), 'doxygen_mainpage',
                   ['README', 'COPYING'],
                   implicit=['$doxygen_mainpage_generator'])
n.build('doxygen', 'doxygen', doc('doxygen.config'),
        implicit=mainpage)
n.newline()

if not host.is_mingw():
    n.comment('Regenerate build files if build script changes.')
    n.rule('configure',
           command='${configure_env}%s $root/configure.py $configure_args' %
               options.with_python,
           generator=True)
    n.build('build.ninja', 'configure',
            implicit=['$root/configure.py',
                      os.path.normpath('$root/misc/ninja_syntax.py')])
    n.newline()

n.default(ninja)
n.newline()

if host.is_linux():
    n.comment('Packaging')
    n.rule('rpmbuild',
           command="misc/packaging/rpmbuild.sh",
           description='Building rpms..')
    n.build('rpm', 'rpmbuild')
    n.newline()

n.build('all', 'phony', all_targets)

n.close()
print('wrote %s.' % BUILD_FILENAME)

if options.bootstrap:
    print('bootstrap complete.  rebuilding...')

    rebuild_args = []

    if platform.can_rebuild_in_place():
        rebuild_args.append('./ninja')
    else:
        if platform.is_windows():
            bootstrap_exe = 'ninja.bootstrap.exe'
            final_exe = 'ninja.exe'
        else:
            bootstrap_exe = './ninja.bootstrap'
            final_exe = './ninja'

        if os.path.exists(bootstrap_exe):
            os.unlink(bootstrap_exe)
        os.rename(final_exe, bootstrap_exe)

        rebuild_args.append(bootstrap_exe)
    rebuild_args.append('-d')
    rebuild_args.append('explain')
    if options.verbose:
        rebuild_args.append('-v')

    subprocess.check_call(rebuild_args)
