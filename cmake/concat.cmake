cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

separate_arguments(FILES UNIX_COMMAND "${FILES}")

set(TOTAL "")

foreach(FILE ${FILES})
    file(READ ${FILE} CONTENTS)
    set(TOTAL "${TOTAL}${CONTENTS}")
endforeach()

file(WRITE ${TARGET} "${TOTAL}")
