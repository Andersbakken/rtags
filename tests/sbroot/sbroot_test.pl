#!/usr/bin/env perl
# File: sbroot_test.pl
# Abstract:
#   Used to exercise rtags, specifically focusing on the the sandbox root
#   handling which allows us to move or copy the tag index for a sandbox.
#
#   See sbroot_test_help.md
#

use strict;
use warnings;

use Cwd;
use File::Spec;
use File::Find;
use FindBin;
use File::Basename;
use File::Path;

my $gNumErrors      = 0;
my $gNumTotalErrors = 0;
my $gColor          = (-t STDOUT &&
                       exists($ENV{TERM}) && $ENV{TERM} ne "dumb" &&
                       (!exists($ENV{EMACS}) || $ENV{EMACS} ne "t")? 1: 0);
my $tBold           = ($gColor? "[1m": "");
my $tBoldUnderline  = ($gColor? "[1;4m": "");
my $tInfo           = ($gColor? "\033[30;46m": "");
my $tErr            = ($gColor? "[41;33;1m": "");
my $tNormal         = ($gColor? "[0m": "");
my $tStart          = ($gColor? "[42;37;1m": "");

Main();

#-------------#
# Subroutines #
#-------------#
sub ReportErr {
    my ($err) = @_;

    $gNumErrors++;
    $gNumTotalErrors++;
    print "${tErr}ERROR:$gNumErrors: $tNormal $err\n";
}

sub GetRTagsDir {
    my ($sbroot) = @_;
    return("$sbroot/.rtags");
}

sub GetSockFile {
    my ($sbroot) = @_;
    return GetRTagsDir($sbroot)."/rdm_socket";
}

sub Chdir {
    my ($dir) = @_;

    chdir($dir) || die "cd $dir: $!";
    my $cwd = Cwd::abs_path(".");
    # Note the rtags Path::pwd() function reads PWD. This seems like a mistake because doing a
    # chdir() from a program (such as this) doesn't alter PWD. Seem's like rtags should use
    # getcwd().
    $ENV{PWD}=$cwd;
    print "> pwd $cwd\n";
}


sub DispAction {
    my ($action) = @_;

    if ($action !~ /^([^\s:]+:) (.+)$/) {
        die "action must match 'ACTION_WORD: info'\n";
    }
    print "$tInfo$1$tNormal $2\n";
}



sub SetProjectRoot {
    my ($sbroot) = @_;
    my $config = "$sbroot/.rtags-config";
    open(my $fh, ">", $config) || die "create: $config: $!";
    print $fh "project: $sbroot\n";
    close($fh) || die "close $config: $!";
}



sub Usage {
    die("Usage: $0 [--no-cleanup] [--no-sandbox-root-check] [RTAGS_BIN]\n".
        "RTAGS_BIN is mandatory if RTAGS_BINARY_DIR environment is not set.\n".
        "See sbroot_test_help.md\n");
}


# Subroutine: RunCmd ===============================================================================
# Abstract:
#   Run $cmd, return output except when running in background (&), in which case undef is returned.
#
sub RunCmd {
    my $cmd = shift(@_);

    my %opts = @_;
    foreach my $key (keys(%opts)) {
        if ($key !~ /^(?:quiet|outQuiet|noDie)$/) {
            die "bad RunCmd() option, $key";
        }
    }

    if (!$opts{quiet}) {
        if ($cmd =~ /^(\S+\/bin\/(?:rc|rdm)) (.+)/) {
            my ($prog,$args) = ($1,$2); # Highlight RTags binaries.
            print "$tBoldUnderline>> $prog$tNormal $args\n";
        } else {
            print "> $cmd\n";
        }
    }

    if ($cmd =~ /&$/) {
        system($cmd); # e.g. "rdm ARGS > $log 2>&1 &"
        return undef;
    } else {
        my @out = `$cmd 2>&1`;
        my $stat=$?;
        if ($stat != 0) {
            my $msg = "'$cmd' failed, stat=$stat, out='".join('',@out)."'";
            if ($opts{noDie}) {
                ReportErr($msg);
            } else {
                die $msg;
            }
        }
        if (!$opts{quiet} && !$opts{outQuiet}) {
            print join('',@out);
        }
        if (wantarray) {
            return @out;
        } else {
            return join('',@out);
        }
    }

} # end RunCmd



# Subroutine: ProcessArgsAndSetupSandbox ===========================================================
# Abstract:
#    Assumes this program, resides one directory above the sandbox root (proj)
#    and the name of this program is the same as the sandbox root with .pl
#    appened to it, e.g.
#      sbroot_proj.pl
#      sbroot_proj/           # sbroot
#
#    This then copies recursively, sbroot_proj/ sbroot_proj_ARCH_tmp/
#    and leaves cwd, the sandbox root, at sbroot_proj_ARCH_tmp/
#
#   Return ($sbroot,$rdm,$rc), where
#    $sbroot is the sandbox root
#    $rdm is the tags server
#    $rc is the tags command interface
#
sub ProcessArgsAndSetupSandbox {

    my $swNoCleanup;
    my $swNoSandboxRootCheck;
    while (@ARGV) {
        if ($ARGV[0] =~ /^-?-no-cleanup$/) {
            shift(@ARGV);
            $swNoCleanup = 1;
        } elsif ($ARGV[0] =~ /^-?-no-sandbox-root-check$/) {
            shift(@ARGV);
            $swNoSandboxRootCheck = 1;
        } elsif ($ARGV[0] =~ /^-/) {
            Usage();
        } elsif ($ARGV[0] =~ /^--help$/) {
            Usage();
        } else {
            last;
        }
    }

    if (@ARGV != 1 && ! defined $ENV{RTAGS_BINARY_DIR}) {
        Usage();
    }
    my ($rtagsBin) = $ENV{RTAGS_BINARY_DIR} || @ARGV;
    if (! -d $rtagsBin) {
        die "Directory, $rtagsBin, doesn't exist\n";
    }
    $rtagsBin = Cwd::abs_path($rtagsBin);

    (my $me = $FindBin::Script) =~ s/\.pl$//;

    my @progs = ("$rtagsBin/rdm","$rtagsBin/rc");
    foreach my $p (@progs) {
        if (! -x $p) {
            die "$p doesn't exist or isn't executable";
        }
    }

    (my $tmp = $ENV{TMPDIR} || "/tmp") =~ s/\/+$//; # strip trailing slash(es) if present
    $tmp = Cwd::abs_path($tmp); # in case it's a symlink

    my $sbroot   = "$tmp/${me}_${$}_tmp";
    my $rtagsDir = GetRTagsDir($sbroot);

    (my $sbrootMove = $sbroot) =~ s/tmp$/move_tmp/;

    foreach my $d ($sbroot, $sbrootMove) {
        if (-e $d) {
            # If previous run was killed
            print "Cleaning up previous run state in: $d\n";
            StopRDMAndCleanup($progs[1],$d);
        }
    }

    RunCmd("cp -r $FindBin::Bin/$me $sbroot");
    if (! -d "$rtagsDir/rtags_db") {
        mkpath("$rtagsDir/rtags_db");
    }
    SetProjectRoot($sbroot);

    return($sbroot,$sbrootMove,@progs,$swNoCleanup,$swNoSandboxRootCheck);

} # end ProcessArgsAndSetupSandbox


# Subroutine: WaitForRDM ===========================================================================
# Abstract:
#    Wait for server to finish (or start)
#
sub WaitForRDM {
    my ($rc,$sbroot) = @_;

    my $sockFile = GetSockFile($sbroot);

    my $waitCmd = "$rc --socket-file=$sockFile --is-indexing";
    while (1) {
        sleep(1);
        my $out = `$waitCmd`;
        if ($out !~ /^\s*1\s*$/) {
            last;
        }
    }

} # end WaitForRDM


# Subroutine: StartRDM =============================================================================
# Abstract:
#   Launch rdm, the tags server using $sockFile.
#
#   Assumes current directory is the 'sandbox root'.
#
sub StartRDM {
    my ($rdm,$rc,$sbroot,$sandboxRootCheck) = @_;

    my $rtagsDir = GetRTagsDir($sbroot);
    my $sockFile = GetSockFile($sbroot);

    # Child, redirect stdout, stderr for rdm:
    my $log = "$rtagsDir/rdm_stdout_stderr.log";

    my $cmd = ("$rdm ".
               "--socket-file=$sockFile ".
               "--no-rc ".
               "--enable-compiler-manager ".
               "--data-dir=$rtagsDir/rtags_db ".
               "--log-file-log-level=debug ".
               "--log-file=$rtagsDir/rdm.log ".
               "--crash-dump-file=$rtagsDir/crash_dump.txt ".
               "--job-count=12 ".
               "--watch-sources-only ".
               "--exclude-filter /none ".   # we want to index /tmp
               ($sandboxRootCheck? "--sandbox-root=$sbroot ": "").
               ">$log ".
               "2>&1 ".
               "&");
    RunCmd($cmd);

    WaitForRDM($rc,$sbroot);

} # end StartRDM



# Subroutine: StopRDM ==============================================================================
# Abstract:
#   Shutdown the RDM server. Do not touch anything else.
#
sub StopRDM {
    my ($rc,$sbroot) = @_;

    my $sockFile = GetSockFile($sbroot);
    if (-e "$sockFile") {
        print "${tInfo}Stop:$tNormal Shutting down rdm for: $sbroot\n";
        my $rdmCmd = "$rc --socket-file=$sockFile --quit-rdm";
        my $out = `$rdmCmd 2>&1`;
        if ($? != 0 && $out !~ /Can't seem to connect to server/) {
            die "Failed to stop rdm, stat=$?, rdmCmdOut=\n$out";
        }
    }
} # end StopRDM



# Subroutine: StopRDMAndCleanup ====================================================================
# Abstract:
#   Shutdown the tags server at $sockFile.
#
sub StopRDMAndCleanup {
    my ($rc,$sbroot) = @_;

    StopRDM($rc,$sbroot);

    print "${tInfo}Cleanup:$tNormal removing tmp sandbox: $sbroot\n";
    # If in sandbox, move out of it.
    my $sandboxParentDir = dirname($sbroot);
    chdir($sandboxParentDir) || die "chdir($sandboxParentDir): $!";

    rmtree($sbroot); # delete tmp sandbox

} # end StopRDMAndCleanup



# Subroutine: GetCompilers =========================================================================
# Abstract:
#   Dump the make rules to get the C and C++ compilers
#
sub GetCompilers {
    my ($makefile) = @_;

    my $mod     = dirname($makefile);
    my $base    = basename($makefile);
    my @makeOut = RunCmd("make -p -n -C $mod -f $base", quiet=>1);

    my ($cc,$cxx);
    foreach my $line (@makeOut) {
        if ($line =~ /^CC = (.+)/) {
            $cc = $1;
        } elsif ($line =~ /^CXX = (.+)/) {
            $cxx = $1;
        }
    }

    if (!defined $cc || !defined $cxx) {
        die "unable to locate compilers in $makefile";
    }

    return($cc,$cxx);

} # end GetCompilers


# Subroutine: IndexAndWait =========================================================================
# Abstract:
#   Run make --dry-run, forcing all compiles to run and then parse for compile lines and send to the
#   tags server, rdm, via the tags command, rc.
#
sub IndexAndWait {
    my ($rc,$sbroot,$sandboxRootCheck) = @_;

    print "${tInfo}Indexing:$tNormal $sbroot\n";

    my $sockFile = GetSockFile($sbroot);
    my $nSources = 0;
    my @makefiles = glob("*/Makefile");
    if (@makefiles == 0) {
        die "no */Makefiles";
    }
    my ($cc,$cxx) = GetCompilers($makefiles[0]);

    foreach my $makefile (@makefiles) {
        my $mod = dirname($makefile);
        Chdir($mod);

        my @makeOut = RunCmd("make --always-make --dry-run",quiet=>1);
        foreach my $line (@makeOut) {
            if ($line =~ m!((?:\Q$cc\E|\Q$cxx\E).*\s-c\s.+)!) {
                $nSources++;
                (my $ccCmd = $1) =~ s/\s+$//;
                RunCmd("$rc --socket-file=$sockFile --compile $ccCmd");
            }
        }

        Chdir("..");
    }

    WaitForRDM($rc,$sbroot);

    print "$nSources sources indexed in ".scalar(@makefiles)." modules\n";

    # Verify no strings containing the sandbox root directory in the database files.
    if ($sandboxRootCheck) {
        my $sbrootBase = basename($sbroot);
        my $wanted = sub {
            if (-f $_) {
                my @strings = RunCmd("strings $_",quiet=>1);
                if (grep(/\Q$sbrootBase\E/,@strings)) {
                    ReportErr("non-relative path ($sbrootBase) found in $File::Find::name\n");
                }
            }
        };

        find($wanted,".rtags/rtags_db");
    }

} # end IndexAndWait



# Subroutine: FindFilesToNavigate ==================================================================
# Abstract:
#  Locate C++ files and *.org in $sbroot to navigate.
#
sub FindFilesToNavigate {

    my @filesToNavigate = ();
    my $wanted = sub {
        if ($_ =~ /\.(?:c|cpp|cxx|h|hpp|hxx|org)$/ && $_ !~ /^[\.#]/) {
            push(@filesToNavigate,$File::Find::name);
        }
    };

    find($wanted,".");
    return(@filesToNavigate);
} # end FindFilesToNavigate



# Subroutine: ReadFile =============================================================================
# Abstract:
#   Read a file, once.
#
my %gFileCache;
sub ReadFile {
    my ($file) = @_;
    my $absFile = Cwd::abs_path($file); # canonical location

    open(my $fh,$absFile) || die "open $absFile: $!";
    my @lines = <$fh>;
    close($fh) || die "close $absFile: $!";

    if (!exists($gFileCache{$absFile})) {
        $gFileCache{$absFile} = \@lines;
    }
    return $gFileCache{$absFile};

} # end ReadFile


# Subroutine: GetExpLineNum ========================================================================
# Abstract:
#    Given code and comment of form
#        return arg*2;  // --follow-location arg => sub.cpp:int sub(int @arg)
#    where $locFile is sub.cpp and $locLine is 'int sub(int arg)', where @ was
#    removed, return the actual line number in sub.cpp
#
sub GetExpLineNum {
    my ($srcFile,$srcLineNum,$expFile,$expLine) = @_;

    my $linesAPtr = ReadFile($expFile);

    my $expLineNum;
    my $nLocFileLines = scalar(@$linesAPtr);
    for (my $i=0; $i<$nLocFileLines; $i++) {
        if ($linesAPtr->[$i] =~ /^\Q$expLine\E/) {
            $expLineNum=$i+1;
            last;
        }
    }
    if (!$expLineNum) {
        ReportErr("in navigation comment at '$srcFile:$srcLineNum', unable ".
                  "to find '$expLine' in $expFile ".
                  "(match is sensitive to whitespace)\n");
    }
    return ($expLineNum);
} # end GetExpLineNum


# Subroutine: NavResultCmp =========================================================================
# Abstract:
#   Comparison subroutine to sort navigation result lines of form:
#     FILE:LINE_NUM:LINE_COL
#
sub NavResultCmp {

    if ($a =~ /^([^:]+):(\d+):(\d+)/) {
        my ($aFile,$aLineNum,$aLineCol) = ($1,$2,$3);
        if ($b =~ /^([^:]+):(\d+):(\d+)/) {
            my ($bFile,$bLineNum,$bLineCol) = ($1,$2,$3);
            if ($aFile eq $bFile) {
                if ($aLineNum == $bLineNum) {
                    return $aLineCol <=> $bLineCol;
                } else {
                    return $aLineNum <=> $bLineNum;
                }
            } else {
                return $aFile cmp $bFile;
            }
        }
    }
    return $a cmp $b;

} # end NavResultCmp



# Subroutine: DecodeExpectedResultInfo =============================================================
# Abstract:
#   The comments in the C++/navigation_cmds.org files specifying the expected
#   result information in the form:
#     CODE; // rc NAV_SWITCH ITEM => FILE_1.CPP: RESULT_LINE_1
#                                    ^^^^^^^^^^^^^^^^^^^^^^^^^
#                                    expected result
#  which needs to be decoded into what rc returns, e.g.
#     FILE:LINE_NUM:COL_NUM:   LINE
#
#  We also sort the expected result info to ensure comparison is dependent
#  on the order of the result info returned by rc.
#
sub DecodeExpectedResultInfo {
    my ($expectedResultInfo,$sbroot,$relSrcToSBRoot) = @_;

    my @expectedResult = ();

    foreach my $e (@$expectedResultInfo) {

        my ($expFile,$expLine,$srcLineNum) = @$e;

        my $expLineCol = index($expLine,'@');
        if ($expLineCol == -1) {
            die("Malformed test comment, missing '\@' in '$expLine' of ".
                "$relSrcToSBRoot:$srcLineNum\n");
        }
        $expLineCol += 1; # rc is 'one'-based.

        # remove the '@' marker so we can figure out the loc line number
        $expLine =~ s/\@//;

        my ($expLineNum)=GetExpLineNum($relSrcToSBRoot,$srcLineNum,
                                       $expFile,$expLine);
        if (!$expLineNum) {
            return;
        }

        # Results from rc are relative to the project root.
        my $absExpFile  = Cwd::abs_path($expFile);
        my $expFilePath = File::Spec->abs2rel($absExpFile,$sbroot);

        push(@expectedResult,"$expFilePath:$expLineNum:$expLineCol:\t$expLine");
    }

    @expectedResult = sort NavResultCmp @expectedResult;

    return @expectedResult;

} # end DecodeExpectedResultInfo



# Subroutine: DispChecking =========================================================================
# Abstract:
#   We've found a 'check' line within a source file, display what we
#   are about to validate.
#
sub DispChecking {
    my ($relSrcToSBRoot,$itemLineNum,$checkLinesAPtr) = @_;

    (my $lineCopy=$checkLinesAPtr->[0]) =~ s/^\s+//;
    print "----\n";
    print("${tStart}Checking:$tNormal $relSrcToSBRoot:$itemLineNum\n".$checkLinesAPtr->[0]);
    for (my $i=1; $i<@$checkLinesAPtr; $i++) {
        print $checkLinesAPtr->[$i];
    }
} # end DispChecking



# Subroutine: RunNavigation ========================================================================
# Abstract:
#   Run rc navigation, returning result lines. When
#   there is an error it is reported and an empty array of lines is returned.
#
sub RunNavigation {
    my ($rc,$sockFile,$navSwitch,$src,$item,$itemLineNum,$itemColNum) = @_;

    my $navSw;
    if ($itemColNum) { # --reference FILE:LINE_NUM:COL_NUM
        $navSw = "$navSwitch $src:$itemLineNum:$itemColNum";
    } else {
        # e.g. --references-name ITEM
        $navSw = "$navSwitch $item";
    }

    my @rcOut = RunCmd("$rc --socket-file=$sockFile --no-color $navSw",outQuiet=>1,noDie=>1);
    if (@rcOut == 0) {
        print ">>> rc result: <no-output>\n";
    } else {
        for (my $i=0; $i<@rcOut; $i++) {
            if ($i==0) {
                print ">>> rc result: $rcOut[$i]";
            } else {
                print ">>>          : $rcOut[$i]";
            }
        }
    }

    @rcOut = sort NavResultCmp @rcOut;
    return @rcOut;

} # end RunNavigation



# Subroutine: NavigateCode =========================================================================
# Abstract:
#   C++ file navigation is command is defined by a comment on a line followed
#   by what the result should. Given a C++ SOURCE file containing the comment
#
#     CODE; // rc NAV_SWITCH ITEM => FILE_1.CPP: RESULT_LINE_1
#           ...
#           //                    => FILE_N.CPP: RESULT_LINE_N
#
#   ITEM must be found in the CODE portion of the source file and RESULT_LINE's
#   must be found in the result FILE's. In addition the RESULT_LINE's must
#   contain an '@' character identifying the column number in the result.
#
#   From the comment information in the source, we can compute the:
#     1. ITEM for the navigation command's line number and character offset
#        column number required to use rc's navigation switches, and
#     2. The result lines info that is returned by rc.
#
#   For example, given main.cpp,
#
#        12345678901234567890
#     1: int main(void)
#     2: {
#     3:     int var;   // rc --references var => main.cpp:  @var = 1;
#     4:                //                     => main.cpp:  return (@var*2);
#     5:     var = 1;
#     6:     return (var*2);
#     7: }
#
#   we'll compute ITEM as 'main.cpp:3:9:':
#      rc -references main.cpp:3:9:
#   and compute the expected result lines:
#      main.cpp:5:5:    var = 1;
#      main.cpp:6:13:    return (var*2);
#
#   Next we run the rc command and verify the result matches the expected result
#   (after sorting both the expected result and actual result lines to thus
#   making the result independent of line ordering).
#
#   Note, NavigateCode() assumes the current directory is the sandbox root,
#   $sbroot.
#
sub NavigateCode {
    my ($rc,$sbroot,$relSrcToSBRoot) = @_;

    my $sockFile = GetSockFile($sbroot);

    my $srcDir=dirname($relSrcToSBRoot);
    Chdir($srcDir);
    my $src=basename($relSrcToSBRoot);

    my $linesAPtr = ReadFile($src);

    for (my $lineIdx=0; $lineIdx<scalar(@$linesAPtr); $lineIdx++) {
        my $line = $linesAPtr->[$lineIdx];
        if ($line =~ m!// \s* rc \s*
                       (  --follow-location
                       |  --references
                       |  --references-name
                       ) \s+
                       (\S+)                    # ITEM to navigate to
                       \s+ => \s+
                       ([^:]+):(.+)
                      !x) {
            my $navSwitch   = $1;
            my $item        = $2; # for $navSwitch
            my $resultFile  = $3;
            my $resultLine  = $4;
            my $itemLineNum = $lineIdx+1;
            my @expectedResultInfo = ([$resultFile,$resultLine,$itemLineNum]);
            my @checkLines = ($line);
            if ($lineIdx+1 < scalar(@$linesAPtr) &&
                $linesAPtr->[$lineIdx+1] =~ m!^[/\*:\s]*=>\s+([^:]+):(.+)!) {
                push(@expectedResultInfo,[$1,$2,$lineIdx+1]);
                push(@checkLines,$linesAPtr->[$lineIdx+1]);
                $lineIdx++;
            }
            my @expectedResult =
              DecodeExpectedResultInfo(\@expectedResultInfo,$sbroot,$relSrcToSBRoot);
            if (@expectedResult == 0) {
                next; # error was reported
            }

            my $itemCol;
            if ($navSwitch !~ /^(?:--references-name)$/) {
                # Compute navigation command,
                #  rc $navSwitch SOURCE:$itemLineNum:$itemLineCol
                $itemCol = index($line,$item);
                if ($itemCol == -1) {
                    die "Unable to find '$item' in $line\n";
                }
                $itemCol += 1; # rc is one-based
            }


            # Run the navigation command

            DispChecking($relSrcToSBRoot,$itemLineNum,\@checkLines);
            my @actualResult=RunNavigation($rc,$sockFile,$navSwitch,$src,
                                           $item,$itemLineNum,$itemCol);
            if (@actualResult == 0) {
                next; # error was reported
            }

            # Validate navigation actual result,@actualResult vs @expectedResult
            if (scalar(@expectedResult) != scalar(@actualResult)) {
                ReportErr("rc result failed to match result lengths:\n".
                          "  expectedResult:\n".
                          "    ".join("\n    ",@expectedResult)."\n".
                          "  actualResult:\n".
                          "    ".join("\n    ",@actualResult)."\n");
            }

            for (my $i=0; $i<scalar(@expectedResult); $i++) {
                my $expected = $expectedResult[$i];
                my $actual = $actualResult[$i];
                my ($expLoc,$expLine) = ($expected =~ /^([^\t]+)\t(.+)$/);

                if ($actual !~ /^\Q$expLoc\E\s*\Q$expLine\E/) {
                    ReportErr("rc result failed to match expected result, ".
                              "/\\Q$expLoc\\E\\s*\\Q$expLine\\E/");
                }
            }
        }
    }

    Chdir($sbroot);

} # end NavigateCode



# Subroutine: InstallCleanupHandler ================================================================
# Abstract:
#   On non-graceful exits delete the tmp $sbroot
#
sub InstallCleanupHandler {
    my ($rc,$sbroot,$swNoCleanup) = @_;

    my $cleanupHandler =
      ($swNoCleanup?
       sub { print("${tInfo}NoCleanup:$tNormal Not shutting down rdm (see $sbroot)\n"); }:
       sub { StopRDMAndCleanup($rc,$sbroot); });

    $SIG{QUIT}    = $cleanupHandler;
    $SIG{INT}     = $cleanupHandler;
    $SIG{TERM}    = $cleanupHandler;

    $SIG{__DIE__} = sub {
        ReportErr(join(" ",@_));
        &$cleanupHandler;
        print "FAILED, $gNumErrors error(s).\n";
        exit(1);
    };

} # end InstallCleanupHandler



# Subroutine: IndexAndNavigate =====================================================================
# Abstract:
#   Generate the index and navigate code. Leave rdm running.
#
sub IndexAndNavigate {
    my ($rdm,$rc,$sbroot,$sandboxRootCheck,$filesToNavigateAPtr) = @_;

    StartRDM($rdm,$rc,$sbroot,$sandboxRootCheck);

    #
    # Generate index, then validate basic navigation works
    #
    IndexAndWait($rc,$sbroot,$sandboxRootCheck);
    foreach my $f (@$filesToNavigateAPtr) {
        NavigateCode($rc,$sbroot,$f);
    }

} # end IndexAndNavigate



# Subroutine: Main =================================================================================
# Abstract:
#   Main entry for sbroot_test.pl:
#
#     1. Verify navigation works with no --sandbox-root specified (i.e. absolute paths)
#     2. Verify navigation works with --sandbox-root specified (i.e. relative paths)
#
sub Main {

    my ($sbroot,      # $sbroot == copy of ./sbroot_test, e.g. /tmp/sbroot_test_PID_tmp
        $sbrootMove,  # $sbrootMove == where to move $sbroot to, e.g. /tmp/sbroot_test_PID_move_tmp
        $rdm,$rc,     # paths to rtags binaries
        $swNoCleanup, # -no-cleanup?
        $swNoSandboxRootCheck # -no-sandbox-root-check?
       ) = ProcessArgsAndSetupSandbox();

    # Init
    Chdir($sbroot);
    InstallCleanupHandler($rc,$sbroot,$swNoCleanup);
    my @filesToNavigate = FindFilesToNavigate();

    #
    # Navigate code *WITHOUT* the --sandbox-root switch
    #
    my $noSandboxRootAction = "$sbroot *WITHOUT* --sandbox-root";
    DispAction("Navigate: $noSandboxRootAction");
    my $sandboxRootCheck = 0;
    IndexAndNavigate($rdm,$rc,$sbroot,$sandboxRootCheck,\@filesToNavigate);
    DispAction(($gNumErrors == 0? "Passed": "Failed")."Navigate: $noSandboxRootAction");
    $gNumErrors = 0;
    StopRDM($rc,$sbroot);

    if (!$swNoSandboxRootCheck) {
        #
        # Navigate code with the --sandbox-root switch (regenerate the index);
        #
        print "----\n";
        my $sandboxRootAction = "$sbroot with --sandbox-root";
        DispAction("Navigate: $sandboxRootAction");
        rmtree(GetRTagsDir($sbroot));
        mkpath(GetRTagsDir($sbroot)."/rtags_db");
        $sandboxRootCheck = 1;
        IndexAndNavigate($rdm,$rc,$sbroot,$sandboxRootCheck,\@filesToNavigate);
        DispAction(($gNumErrors == 0? "Passed": "Failed")."Navigate: $sandboxRootAction");
        $gNumErrors = 0;
        StopRDM($rc,$sbroot); # old sandbox


        #
        # Sandbox root check - move sandbox and verify navigation works.
        #
        print "----\n";
        DispAction("Move: Validating --sandbox-root via sandbox rename (mv $sbroot $sbrootMove)");
        rename($sbroot,$sbrootMove) || die "mv $sbroot $sbrootMove: $!";
        $sbroot = $sbrootMove;
        SetProjectRoot($sbroot);

        InstallCleanupHandler($rc,$sbroot,$swNoCleanup); # $sbroot changed!

        Chdir($sbroot);
        $sandboxRootCheck = 1;
        StartRDM($rdm,$rc,$sbroot,$sandboxRootCheck);

        my $reuseAction = "$sbroot *reusing* --sandbox-root index";
        DispAction("Navigating: $reuseAction");
        foreach my $f (@filesToNavigate) {
            NavigateCode($rc,$sbroot,$f);
        }
        DispAction(($gNumErrors == 0? "Passed": "Failed")."Navigate: $reuseAction");
    }

    #------#
    # EXIT #
    #------#
    &{$SIG{QUIT}}(); # run cleanup handler

    print "----\n";
    if ($gNumTotalErrors > 0) {
        print "FAILED, $gNumTotalErrors error(s).\n";
        exit(1);
    } else {
        print "PASSED\n";
        exit(0);
    }

} # end Main


# [eof] sbroot_test.pl
