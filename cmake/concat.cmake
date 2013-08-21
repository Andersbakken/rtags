# (c) 1997-2013 Netflix, Inc.  All content herein is protected by
# U.S. copyright and other applicable intellectual property laws and
# may not be copied without the express permission of Netflix, Inc.,
# which reserves all rights.  Reuse of any of this content for any
# purpose without the permission of Netflix, Inc. is strictly
# prohibited.


# Platform independent way to concatenate an arbitray number of files
# Invoke it with: cmake -P concat.cmake -DFILES=<files> -DTARGET=<absolute path to target file>

cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

separate_arguments(FILES UNIX_COMMAND "${FILES}")

set(TOTAL "")

foreach(FILE ${FILES})
    file(READ ${FILE} CONTENTS)
    set(TOTAL "${TOTAL}${CONTENTS}")
endforeach()

file(WRITE ${TARGET} "${TOTAL}")
