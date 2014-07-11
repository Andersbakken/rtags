require 'formula'

class Clang < Formula
  homepage  'http://llvm.org/'
  url       'http://llvm.org/releases/3.4.2/cfe-3.4.2.src.tar.gz'
  sha1      'add5420b10c3c3a38c4dc2322f8b64ba0a5def97'

  head      'http://llvm.org/git/clang.git'
end

class CompilerRt < Formula
  homepage  'http://llvm.org/'
  url       'http://llvm.org/releases/3.4/compiler-rt-3.4.src.tar.gz'
  sha1      'd644b1e4f306f7ad35df0a134d14a1123cd9f082'

  head      'http://llvm.org/git/compiler-rt.git'
end

class LibCpp < Formula
  homepage 'http://llvm.org/'
  url       'http://llvm.org/releases/3.4.2/libcxx-3.4.2.src.tar.gz'
  sha1      '7daa55bd1e9d87c3657d08d14d6f83566e6a1c04'

  head      'http://llvm.org/git/libcxx.git'
end

class Llvm < Formula
  homepage  'http://llvm.org/'
  url       'http://llvm.org/releases/3.4.2/llvm-3.4.2.src.tar.gz'
  sha1      'c5287384d0b95ecb0fd7f024be2cdfb60cd94bc9'

  head      'http://llvm.org/git/llvm.git'

  option :universal
  option 'with-clang', 'Build Clang C/ObjC/C++ frontend'
  option 'with-libcxx', 'Build with libc++ support'
  option 'with-asan', 'Include support for -faddress-sanitizer (from compiler-rt)'
  option 'disable-shared', "Don't build LLVM as a shared library"
  option 'all-targets', 'Build all target backends'
  option 'rtti', 'Build with C++ RTTI'
  option 'disable-assertions', 'Speeds up LLVM, but provides less debug information'

  depends_on :python => :recommended

  env :std if build.universal?

  def install
    if build.with? 'python' and build.include? 'disable-shared'
      raise 'The Python bindings need the shared library.'
    end

    spec = build.head? ? :head : :stable
    path = Pathname.new(__FILE__).expand_path

    Clang.new("clang", path, spec).brew do
      clang_dir.install Dir['*']
    end if build.include? 'with-clang'

    CompilerRt.new("compiler-rt", path, spec).brew do
      (buildpath/'projects/compiler-rt').install Dir['*']
    end if build.include? 'with-asan'

    LibCpp.new("libcxx", path, spec).brew do
      (buildpath/'projects/libcxx').install Dir['*']
    end if build.include? 'with-libcxx'

    if build.universal?
      ENV['UNIVERSAL'] = '1'
      ENV['UNIVERSAL_ARCH'] = 'i386 x86_64'
    end

    ENV['REQUIRES_RTTI'] = '1' if build.include? 'rtti'

    args = [
      "--prefix=#{prefix}",
      "--enable-optimized",
      # As of LLVM 3.1, attempting to build ocaml bindings with Homebrew's
      # OCaml 3.12.1 results in errors.
      "--disable-bindings",
    ]

    if build.include? 'all-targets'
      args << "--enable-targets=all"
    else
      args << "--enable-targets=host"
    end
    args << "--enable-shared" unless build.include? 'disable-shared'

    args << "--disable-assertions" if build.include? 'disable-assertions'

    args << "--enable-libcpp" if build.include? 'with-libcxx'

    system "./configure", *args
    system "make install"

    # install llvm python bindings
    if build.with? "python"
      (lib+'python2.7/site-packages').install buildpath/'bindings/python/llvm'
      (lib+'python2.7/site-packages').install buildpath/'tools/clang/bindings/python/clang' if build.include? 'with-clang'
    end

    # install clang tools and bindings
    cd clang_dir do
      system 'make install'
      (share/'clang/tools').install 'tools/scan-build', 'tools/scan-view'
    end if build.include? 'with-clang'
  end

  def test
    system "#{bin}/llvm-config", "--version"
  end

  def caveats
    <<-EOS.undent
      Extra tools are installed in #{share}/llvm and #{share}/clang.

      If you already have LLVM installed, then "brew upgrade llvm" might not work.
      Instead, try:
          brew rm llvm && brew install llvm
    EOS
  end

  def clang_dir
    buildpath/'tools/clang'
  end
end
