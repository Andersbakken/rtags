/* C++ code produced by gperf version 3.0.3 */
/* Command-line: gperf -I -C -l -L C++ --output-file gccopts_gperf.cpp -Z gccopts_gperf gccopts.gperf  */
/* Computed positions: -k'$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#include <string.h>

#define TOTAL_KEYWORDS 19
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 18
#define MIN_HASH_VALUE 2
#define MAX_HASH_VALUE 33
/* maximum key range = 32, duplicates = 0 */

class gccopts_gperf
{
private:
  static inline unsigned int hash (const char *str, unsigned int len);
public:
  static const char *in_word_set (const char *str, unsigned int len);
};

inline unsigned int
gccopts_gperf::hash (register const char *str, register unsigned int len)
{
  static const unsigned char asso_values[] =
    {
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34,  8, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 30, 34, 25, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34,  5, 34,
      34, 15, 34, 34,  0, 34, 34, 34, 34, 10,
      34, 20, 34, 34,  5, 20,  0, 34, 34, 34,
       0, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
      34, 34, 34, 34, 34, 34
    };
  return len + asso_values[(unsigned char)str[len - 1]];
}

const char *
gccopts_gperf::in_word_set (register const char *str, register unsigned int len)
{
  static const unsigned char lengthtable[] =
    {
       0,  0,  2,  0,  0,  5,  0,  2,  8,  9,  2,  0, 12,  8,
       0, 10, 11,  7,  8, 14,  0,  0,  2,  8,  0,  0,  0,  2,
       8,  0,  0,  0,  2, 18
    };
  static const char * const wordlist[] =
    {
      "", "",
      "-x",
      "", "",
      "-arch",
      "",
      "-b",
      "-iprefix",
      "-isysroot",
      "-G",
      "",
      "-iwithprefix",
      "-Xlinker",
      "",
      "-imultilib",
      "-Xassembler",
      "--param",
      "-isystem",
      "-Xpreprocessor",
      "", "",
      "-o",
      "-include",
      "", "", "",
      "-V",
      "-imacros",
      "", "", "",
      "-T",
      "-iwithprefixbefore"
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        if (len == lengthtable[key])
          {
            register const char *s = wordlist[key];

            if (*str == *s && !memcmp (str + 1, s + 1, len - 1))
              return s;
          }
    }
  return 0;
}
