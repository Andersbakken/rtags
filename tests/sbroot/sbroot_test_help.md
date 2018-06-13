# RTags Sandbox Root

Running the RTags server, rdm, with --sandbox-root=DIR instructs the RTags server to create the
index database using relative paths to DIR. This is very useful for 're-using' index
databases. Consider a large C++ project consisting of many modules, where each module consists of
many C++ source files. The term sandbox refers to the directory containing the project. For example,
we could have a sandbox root at DIR consisting of N modules and many C++ files:

    DIR/module1/FILES
    DIR/module2/FILES
    ...
    DIR/moduleN/FILES

Next we ask RTags to create a relative index, in DIR/.rtags/rtags_db:

    mkdir -p DIR/.rtags/rtags_db
    rdm --socket-file=DIR/.rtags/rdm_socket \
        --no-rc \
        --data-dir=DIR/.rtags/rtags_db \
        --log-file=DIR/.rtags/rtags.log \
        --crash-dump-file=DIR/.rtags/crash_dump.txt \
        --job-count=12 \
        --watch-sources-only \
        --sandbox-root=DIR
    # index C++ FILES by running rc --compile or rc --load-compilation-database, etc.

Suppose you use this as a master sandbox. Team members can then copy/clone the DIR sandbox using
rsync, btrfs, etc. e.g.

     rsync DIR/ DIR1
     or
     btrfs subvolume snapshot DIR DIR1

After the copy or clone operation, DIR1 is a 'deep copy' of DIR which can be immediately navigated
(after starting another instance of rdm for DIR1).

# Testing --sandbox-root=DIR

The `sbroot_test.pl` is a small perl program that runs on a 'mock' sandbox
project to validate the --sandbox-root handling in RTags. `sbroot_test.pl` asks
rdm to index the project. Then the project is moved to another directory
simulating the copy operation. The move operation ensures the navigation queries
in the moved sandbox don't refer to the original sandbox because the original
sandbox no longer exits.

## Usage

    sbroot_test.pl [--no-cleanup] [--no-sandbox-root-check] /path/to/rtags/install/bin

    or

    RTAGS_BINARY_DIR=/path/to/rtags/install/bin sbroot_test.pl [--no-cleanup] [--no-sandbox-root-check]

The exit status will be non-zero if the test fails.

The test driver `sbroot_test.pl` reads the C++ files in the 'mock' sandbox to determine
what to test. For example, suppose `sbroot_test/main/main.cpp` contains:

    ```C++
    #include "../sub1/export/sub1.hpp"
    #include <stdlib.h>

    int main(void)
    {
        int v=SUB1MOD::Csub1::sub1(1); // rc --follow-location sub1( => ../sub1/sub1.cpp:    int Csub1::@sub1(int arg)

        return SUB1MOD::fcn1(v); // rc --follow-location fcn1 => ../sub1/sub1.cpp:    SUB1_EXPORT_FCN int @fcn1(int p1)
    }
    ```

The --follow-location line is a navigation command which `sbroot_test.pl` runs
to navigate from the sub1() call to the function definition within sub1.cpp. The use of these
'navigation' comments simplifies writing tests because the `sbroot_test.pl` can decode these
lines to produce the expected result that RTags should produce.

## --follow-location ITEM => FILE:LINE

The syntax of the navigation --follow-location comment is:

    // --follow-location ITEM => FILE:LINE

where ITEM must match in the line before the comment, from which we get the character offset.
The FILE where we should navigate to is a relative path from the directory containing the
current file. The LINE is the exact line to match, where white space is sensitive. Within LINE
an extra '@' character is required which indicates which column RTags should be navigating to.

## --references ITEM => FILE:LINE

This is similar to --follow-location syntax, but the inverse, e.g. who's 'calling' a function.

## --references-name ITEM => FILE:LINE

Similar to --follow-location syntax.
If the reference is to multiple lines, make a multi-line comment, e.g.

    --references-name TWO => sub1/sub1.cpp:        return arg*@TWO*THREE;
                          => sub1/sub1.cpp:        return p1*@TWO;

# `sbroot_test.pl` switches

`sbroot_test.pl` copies the 'mock' sandbox, sbroot_test/... to a temporary location and runs the
test there. Specify `--no-cleanup` if you want to leave the tmp directory and rdm running.

If you want to just verify the navigation comments are correct and basic RTags is working
without the --sandbox-root switch, specify `--no-sandbox-root-check`.


[//]: # LocalWords:  RTags rdm rtags rc rsync btrfs stdlib Csub tmp
