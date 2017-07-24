#include "StringTokenizer.h"

TEST (StringTokenizerTest, BreakIdentifierWithUnderscore)
{
    vector<string> result = StringTokenizer::break_parts_of_word("my_shiny_identifier");

    ASSERT_EQ (3, result.size());
    ASSERT_EQ ("my", result[0]);
    ASSERT_EQ ("shiny", result[1]);
    ASSERT_EQ ("identifier", result[2]);
}

TEST (StringTokenizerTest, BreakIdentifierWithCamelCase)
{
    vector<string> result = StringTokenizer::break_parts_of_word("MyShinyIdentifier");

    ASSERT_EQ (3, result.size());
    ASSERT_EQ ("my", result[0]);
    ASSERT_EQ ("shiny", result[1]);
    ASSERT_EQ ("identifier", result[2]);
}

TEST (StringTokenizerTest, BreakIdentifierWithUpperLetters)
{
    vector<string> result = StringTokenizer::break_parts_of_word("MyShinyXYZIdentifier");

    ASSERT_EQ (4, result.size());
    ASSERT_EQ ("my", result[0]);
    ASSERT_EQ ("shiny", result[1]);
    ASSERT_EQ ("xyz", result[2]);
    ASSERT_EQ ("identifier", result[3]);
}

TEST (StringTokenizerTest, BreakIdentifierWithDigits)
{
    vector<string> result = StringTokenizer::break_parts_of_word("foo12345bar");

    ASSERT_EQ (3, result.size());
    ASSERT_EQ ("foo", result[0]);
    ASSERT_EQ ("12345", result[1]);
    ASSERT_EQ ("bar", result[2]);
}

TEST (StringTokenizerTest, BreakIdentifierWithDigitsAtBeginning)
{
    vector<string> result = StringTokenizer::break_parts_of_word("12345FooBar");

    ASSERT_EQ (3, result.size());
    ASSERT_EQ ("12345", result[0]);
    ASSERT_EQ ("foo", result[1]);
    ASSERT_EQ ("bar", result[2]);
}

TEST (StringTokenizerTest, BreakVeryComplexIdentifier)
{
    vector<string> result = StringTokenizer::break_parts_of_word("XYZ12345XMLDocument");

    ASSERT_EQ (4, result.size());
    ASSERT_EQ ("xyz", result[0]);
    ASSERT_EQ ("12345", result[1]);
    ASSERT_EQ ("xml", result[2]);
    ASSERT_EQ ("document", result[3]);
}

TEST (StringTokenizerTest, BreakVeryComplexIdentifierWithUnderscore)
{
    vector<string> result = StringTokenizer::break_parts_of_word("XYZ12345XM_LDocument");

    ASSERT_EQ (5, result.size());
    ASSERT_EQ ("xyz", result[0]);
    ASSERT_EQ ("12345", result[1]);
    ASSERT_EQ ("xm", result[2]);
    ASSERT_EQ ("l", result[3]);
    ASSERT_EQ ("document", result[4]);
}

static bool test_word_boundary_match(const string &name, const string &candidate,
                                     vector<unsigned> &match_result)
{
    vector<string> words = StringTokenizer::break_parts_of_word(name);
    return StringTokenizer::is_boundary_match(words, candidate, match_result);
}

TEST (StringTokenizerTest, MatchSimpleSearchPattern)
{
    vector<unsigned> match_result;
    bool r = test_word_boundary_match ("foo_bar_text", "fb", match_result);
    ASSERT_TRUE(r);
    ASSERT_EQ(1, match_result[0]);
    ASSERT_EQ(1, match_result[1]);
}

TEST (StringTokenizerTest, CommonPrefix)
{
    ASSERT_EQ (3, StringTokenizer::common_prefix("sparta", "spa"));
    ASSERT_EQ (3, StringTokenizer::common_prefix("spa", "sparta"));
    ASSERT_EQ (0, StringTokenizer::common_prefix("", ""));
    ASSERT_EQ (0, StringTokenizer::common_prefix("xyz", "abc"));
}

TEST (StringTokenizerTest, MatchSimpleSearchPattern2)
{
    vector<unsigned> match_result;
    bool r = test_word_boundary_match ("foo_bar_text", "fbarte", match_result);
    ASSERT_TRUE(r);
    ASSERT_EQ(1, match_result[0]);
    ASSERT_EQ(3, match_result[1]);
    ASSERT_EQ(2, match_result[2]);
}

TEST (StringTokenizerTest, MatchSimpleSearchPatternSkipChunks)
{
    vector<unsigned> match_result;
    bool r = test_word_boundary_match ("foo_bar_text_sparta", "ftexts", match_result);
    ASSERT_TRUE(r);
    ASSERT_EQ(1, match_result[0]);
    ASSERT_EQ(0, match_result[1]);
    ASSERT_EQ(4, match_result[2]);
    ASSERT_EQ(1, match_result[3]);
}

TEST (StringTokenizerTest, MatchSimpleSearchPatternInvalid)
{
    vector<unsigned> match_result;
    bool r = test_word_boundary_match ("foo_bar_text", "fbx", match_result);
    ASSERT_FALSE(r);
}

TEST (StringTokenizerTest, MatchSimpleSearchPatternWithInvalidCandidateChars)
{
    vector<unsigned> match_result;
    bool r = test_word_boundary_match ("foo_bar_text", "f_^@ba", match_result);
    ASSERT_TRUE(r);
    ASSERT_EQ(1, match_result[0]);
    ASSERT_EQ(2, match_result[1]);
    ASSERT_EQ(0, match_result[2]);
}

TEST (StringTokenizerTest, MatchSimpleSearchPatternComplex)
{
    vector<unsigned> match_result;
    bool r = test_word_boundary_match ("ob_obsah_s", "obsas", match_result);
    ASSERT_TRUE(r);
    ASSERT_EQ(0, match_result[0]);
    ASSERT_EQ(4, match_result[1]);
    ASSERT_EQ(1, match_result[2]);
}

TEST (StringTokenizerTest, MatchSimpleSearchPatternComplex2)
{
    vector<unsigned> match_result;
    bool r = test_word_boundary_match ("spa_pax_paxo_paxon", "spaxon", match_result);
    ASSERT_TRUE(r);
    ASSERT_EQ(1, match_result[0]);
    ASSERT_EQ(0, match_result[1]);
    ASSERT_EQ(0, match_result[2]);
    ASSERT_EQ(5, match_result[3]);
}

TEST (StringTokenizerTest, FindMatchInvalid)
{
    MatchResult *r = StringTokenizer::find_match(new CompletionCandidate("foo_bar"), "xyz");
    ASSERT_EQ (NO_MATCH, r->type);
    delete r;
}

TEST (StringTokenizerTest, FindMatchInvalidSmaller)
{
    MatchResult *r = StringTokenizer::find_match(new CompletionCandidate("foo_bar"), "foo_bar_");
    ASSERT_EQ (NO_MATCH, r->type);
    delete r;
}

TEST (StringTokenizerTest, FindMatchExactCaseSensitive)
{
    MatchResult *r = StringTokenizer::find_match(new CompletionCandidate("FooBar"), "FooBar");
    ASSERT_EQ (EXACT_MATCH_CASE_SENSITIVE, r->type);
    delete r;
}

TEST (StringTokenizerTest, FindMatchExactCaseInsensitive)
{
    MatchResult *r = StringTokenizer::find_match(new CompletionCandidate("FooBar"), "Foobar");
    ASSERT_EQ (EXACT_MATCH_CASE_INSENSITIVE, r->type);
    delete r;
}

TEST (StringTokenizerTest, FindMatchPrefixCaseSensitive)
{
    MatchResult *r = StringTokenizer::find_match(new CompletionCandidate("FooBarBaz"), "FooBar");
    ASSERT_EQ (PREFIX_MATCH_CASE_SENSITIVE, r->type);
    delete r;
}

TEST (StringTokenizerTest, FindMatchPrefixCaseInsensitive)
{
    MatchResult *r = StringTokenizer::find_match(new CompletionCandidate("FooBarBaz"), "Foobar");
    ASSERT_EQ (PREFIX_MATCH_CASE_INSENSITIVE, r->type);
    delete r;
}

TEST (StringTokenizerTest, FindMatchWordBoundary)
{
    MatchResult *r = StringTokenizer::find_match(new CompletionCandidate("FooBarBaz"), "fbb");
    ASSERT_EQ (WORD_BOUNDARY_MATCH, r->type);

    WordBoundaryMatchResult *wbm = static_cast<WordBoundaryMatchResult *> (r);
    ASSERT_EQ (1, wbm->indices[0]);
    ASSERT_EQ (1, wbm->indices[1]);
    ASSERT_EQ (1, wbm->indices[2]);
    delete wbm;
}

static vector<MatchResult *> test_find_and_sort_matches(vector<string> &candidate_names, const string &query)
{
    vector<CompletionCandidate *> candidates;
    for(unsigned i = 0; i < candidate_names.size(); i++)
        candidates.push_back(new CompletionCandidate(candidate_names[i]));

    return StringTokenizer::find_and_sort_matches(candidates, query);
}

TEST (StringTokenizerTest, FindAndSortResultsSimple)
{
    vector<string> names = {"foo", "bar", "baz"};
    vector<MatchResult *> results = test_find_and_sort_matches(names, "fo");

    ASSERT_EQ(1, results.size());
    ASSERT_EQ(PREFIX_MATCH_CASE_SENSITIVE, results[0]->type);
    ASSERT_EQ("foo", results[0]->candidate->name);
}

TEST (StringTokenizerTest, FindAndSortResultsSimpleMultiple)
{
    vector<string> names = {"foo", "fredy", "baz", "f"};
    vector<MatchResult *> results = test_find_and_sort_matches(names, "f");

    ASSERT_EQ(3, results.size());
    ASSERT_EQ("f", results[0]->candidate->name);
    ASSERT_EQ(EXACT_MATCH_CASE_SENSITIVE, results[0]->type);
    ASSERT_EQ("foo", results[1]->candidate->name);
    ASSERT_EQ(PREFIX_MATCH_CASE_SENSITIVE, results[1]->type);
    ASSERT_EQ("fredy", results[2]->candidate->name);
    ASSERT_EQ(PREFIX_MATCH_CASE_SENSITIVE, results[2]->type);
}

TEST (StringTokenizerTest, FindAndSortResultsCaseSensitivity)
{
    vector<string> names = {"Fr", "Fredy", "Baz", "franko", "fr"};
    vector<MatchResult *> results = test_find_and_sort_matches(names, "Fr");

    ASSERT_EQ(4, results.size());
    ASSERT_EQ("Fr", results[0]->candidate->name);
    ASSERT_EQ(EXACT_MATCH_CASE_SENSITIVE, results[0]->type);
    ASSERT_EQ("fr", results[1]->candidate->name);
    ASSERT_EQ(EXACT_MATCH_CASE_INSENSITIVE, results[1]->type);
    ASSERT_EQ("Fredy", results[2]->candidate->name);
    ASSERT_EQ(PREFIX_MATCH_CASE_SENSITIVE, results[2]->type);
    ASSERT_EQ("franko", results[3]->candidate->name);
    ASSERT_EQ(PREFIX_MATCH_CASE_INSENSITIVE, results[3]->type);
}

TEST (StringTokenizerTest, FindAndSortResultsCaseMixture)
{
    vector<string> names = {"fbar", "f_call_bar", "from_bar_and_read", "gnome", "", "ffbar"};
    vector<MatchResult *> results = test_find_and_sort_matches(names, "fbar");

    ASSERT_EQ(3, results.size());
    ASSERT_EQ(EXACT_MATCH_CASE_SENSITIVE, results[0]->type);
    ASSERT_EQ("fbar", results[0]->candidate->name);
    ASSERT_EQ(WORD_BOUNDARY_MATCH, results[1]->type);
    ASSERT_EQ("from_bar_and_read", results[1]->candidate->name);
    ASSERT_EQ(WORD_BOUNDARY_MATCH, results[2]->type);
    ASSERT_EQ("f_call_bar", results[2]->candidate->name);
}

TEST (StringTokenizerTest, FindAndSortResultsLongerPrefixAtBeginning)
{
    vector<string> names = {"get_long_value_with_very_nice", "gloooveshark", "get_small_and_long", "gl_o"};
    vector<MatchResult *> results = test_find_and_sort_matches(names, "glo");

    ASSERT_EQ(4, results.size());
    ASSERT_EQ("gloooveshark", results[0]->candidate->name);
    ASSERT_EQ("gl_o", results[1]->candidate->name);
    ASSERT_EQ("get_long_value_with_very_nice", results[2]->candidate->name);
    ASSERT_EQ("get_small_and_long", results[3]->candidate->name);
}


int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS ();
}
