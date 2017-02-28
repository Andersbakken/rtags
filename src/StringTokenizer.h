/* This file is part of RTags (http://rtags.net).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef StringTokenizer_h
#define StringTokenizer_h

#include "gtest/gtest.h"
#include <string>
#include <vector>
#include <cctype>
#include <algorithm>

using namespace std;

enum MatchResultType
{
  NO_MATCH,
  WORD_BOUNDARY_MATCH,
  PREFIX_MATCH_CASE_INSENSITIVE,
  PREFIX_MATCH_CASE_SENSITIVE,
  EXACT_MATCH_CASE_INSENSITIVE,
  EXACT_MATCH_CASE_SENSITIVE
};

static void lower(string &str)
{
  transform(str.begin(), str.end(), str.begin(), ::tolower);
}

static void upper(string &str)
{
  transform(str.begin(), str.end(), str.begin(), ::toupper);
}

class CompletionCandidate
{
  public:
    CompletionCandidate(string n): name(n), signature(""), kind(""), parent(""),
	brief_comment(""), annotation(""), priority(-1) {}

    string name;
    string signature;
    string kind;
    string parent;
    string brief_comment;
    string annotation;
    int priority;
};

class MatchResult
{
  public:
    MatchResult(MatchResultType t, CompletionCandidate *c): type(t), candidate(c) {}

    MatchResultType type;
    CompletionCandidate *candidate;
};

class NoMatchResult: public MatchResult
{
  public:
    NoMatchResult(CompletionCandidate *c): MatchResult(NO_MATCH, c) {}
};

class PrefixResult: public MatchResult
{
  public:
    PrefixResult(MatchResultType t, CompletionCandidate *c, unsigned l): MatchResult(t, c), prefix_length(l) {}

    unsigned prefix_length;
};

class WordBoundaryMatchResult: public MatchResult
{
  public:
    WordBoundaryMatchResult(CompletionCandidate *c, vector<unsigned> &i): MatchResult(WORD_BOUNDARY_MATCH, c), indices(i) {}

    vector<unsigned> indices;
};

struct MatchResultComparator
{
  bool operator()(MatchResult *a, MatchResult *b)
  {
    if (a->type != b->type)
      return a->type > b->type;

    if (a->type == WORD_BOUNDARY_MATCH)
    {
      WordBoundaryMatchResult *wba = static_cast<WordBoundaryMatchResult *> (a);
      WordBoundaryMatchResult *wbb = static_cast<WordBoundaryMatchResult *> (b);

      for (unsigned i = 0; i < wba->indices.size(); i++)
	if (wba->indices[i] != wbb->indices[i])
	  return wba->indices[i] > wbb->indices[i];
    }

    if (a->candidate->priority != b->candidate->priority)
      return a->candidate->priority < b->candidate->priority;

    return a < b;
  }
};

class StringTokenizer
{
  public:
    inline vector<string> break_parts_of_word(const string &str);
    inline unsigned common_prefix(const string &str1, const string &str2);
    inline MatchResult *find_match(CompletionCandidate *candidate, const string &query);
    inline bool is_boundary_match(const vector<string> &parts,
	const string &query, vector<unsigned> &indices);
    inline string find_identifier_prefix(const string &line, unsigned column, unsigned *start);
    inline vector<MatchResult *> find_and_sort_matches(vector<CompletionCandidate *> &candidates, const string &query);

private:
  inline bool is_boundary_match(const vector<string> &parts,
      const string &query,
      vector<unsigned> &indices,
      unsigned query_start,
      unsigned current_index);
};

vector<string>
StringTokenizer::break_parts_of_word(const string &str)
{
  vector<string> result;
  string buffer;

  for (string::const_iterator c = str.begin(); c != str.end(); c++)
  {
    if (*c == '_')
    {
      /* Underscore symbol always break */
      if (!buffer.empty())
      {
	result.push_back(buffer);
	buffer.clear();
      }
    }
    else if(islower(*c))
    {
      if (buffer.length() > 1 && isupper(buffer[buffer.length() - 1]))
      {
	/* Break: XML|Do.  */
	size_t l = buffer.length();
	result.push_back(buffer.substr(0, l - 1));
	buffer = buffer.substr (l - 1, 1);
      }
      else if(!buffer.empty() && isdigit(buffer[buffer.length() - 1]))
      {
	/* Break: 0|D.  */
	result.push_back(buffer);
	buffer.clear();
      }

      buffer += *c;
    }
    else if (isupper(*c))
    {
      /* Break: a|D or 0|D.  */
      if (!buffer.empty () && !isupper(buffer[buffer.length() - 1]))
      {
	result.push_back(buffer);
	buffer.clear();
      }

      buffer += *c;
    }
    else if(isdigit(*c))
    {
      /* Break: a|0 or A|0.  */
      if (!buffer.empty() && !isdigit(buffer[buffer.length() - 1]))
      {
	result.push_back(buffer);
	buffer.clear();
      }

      buffer += *c;
    }
  }

  if (!buffer.empty())
    result.push_back(buffer);

  /* Lower all parts of result.  */
  for(unsigned i = 0; i < result.size(); i++)
    lower(result[i]);

  return result;
}

unsigned StringTokenizer::common_prefix(const string &str1, const string &str2)
{
  unsigned l = std::min(str1.length(), str2.length());

  for (unsigned i = 0; i < l; i++)
    if (str1[i] != str2[i])
      return i;

  return l;
}

MatchResult *
StringTokenizer::find_match(CompletionCandidate *candidate, const string &query)
{
  string c = candidate->name;

  if (query.length() > c.length())
    return new NoMatchResult(candidate);

  string c_lower = c;
  lower(c_lower);
  string query_lower = query;
  lower(query_lower);

  bool are_equal = c.length() == query.length();
  if(equal(query.begin(), query.end(), c.begin()))
    return new PrefixResult(are_equal ? EXACT_MATCH_CASE_SENSITIVE : PREFIX_MATCH_CASE_SENSITIVE,
	candidate, query.length());

  if(equal(query_lower.begin(), query_lower.end(), c_lower.begin()))
    return new PrefixResult(are_equal ? EXACT_MATCH_CASE_INSENSITIVE : PREFIX_MATCH_CASE_INSENSITIVE,
	candidate, query.length());

  vector<string> words = StringTokenizer().break_parts_of_word(c);
  vector<unsigned> indices;
  bool r = is_boundary_match(words, query_lower, indices);
  if (r)
    return new WordBoundaryMatchResult(candidate, indices);

  return new NoMatchResult(candidate);
}

static bool isnotalnum(char c)
{
  return !isalnum(c);
}

bool StringTokenizer::is_boundary_match(const vector<string> &parts,
    const string &query,
    vector<unsigned> &indices)
{
  /* Strip non-alphanum characters from candidate.  */
  string stripped = query;
  stripped.erase(remove_if(stripped.begin(), stripped.end(), isnotalnum), stripped.end());

  indices.resize(parts.size());
  return is_boundary_match(parts, stripped, indices, 0, 0);
}

bool StringTokenizer::is_boundary_match(const vector<string> &parts,
    const string &query,
    vector<unsigned> &indices,
    unsigned query_start,
    unsigned current_index)
{
  if (query_start == query.length())
    return true;
  else if(current_index == parts.size())
    return false;

  string to_find = query.substr(query_start, query.length() - query_start);
  unsigned longest_prefix = common_prefix(parts[current_index], to_find);

  for (int i = longest_prefix; i >= 0; i--)
  {
    indices[current_index] = i;
    bool r = is_boundary_match(parts, query, indices, query_start + i, current_index + 1);
    if (r)
      return r;
  }

  return false;
}

vector<MatchResult *>
StringTokenizer::find_and_sort_matches(vector<CompletionCandidate *> &candidates, const string &query)
{
  vector<MatchResult *> results;

  for (vector<CompletionCandidate *>::const_iterator c = candidates.begin(); c != candidates.end(); c++)
  {
    MatchResult *r = find_match(*c, query);
    if (r->type != NO_MATCH)
      results.push_back(r);
    else
      delete r;
  }

  sort(results.begin(), results.end(), MatchResultComparator());

  return results;
}

#endif
