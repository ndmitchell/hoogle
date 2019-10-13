#include <stdlib.h>
#include <string.h>


int min_int(int a, int b){return a > b ? b : a;}
int max_int(int a, int b){return a < b ? b : a;}

int maximum_int(int xs[], int n)
{
    int mx = 0;
    for (int i = 0; i < n; i++)
        mx = max_int(mx, xs[i]);
    return mx;
}


// Given a list of words, separated by 0 and ending with two 0
// Find the character that occurs in most words, and return the count of it
int text_search_bound(char* xs)
{
    int counts[256];         // number of words I've seen each char in
    int poss[256];           // position I saw last char at
    int pos = 0;             // the current position I am at

    for (int i = 0; i < 256; i++)
    {
        counts[i] = 0;
        poss[i] = -1;
    }

    for (char* cs = xs; ; cs++)
    {
        unsigned char c = *cs;
        if (c == 0)
        {
            if (cs[1] == 0) break;
            pos++;
        }
        else if (poss[c] != pos)
        {
            poss[c] = pos;
            counts[c]++;
        }
    }
    return maximum_int(counts, 256);
}


int count_pointers(char** xs)
{
    int i;
    for (i = 0; xs[i] != NULL; i++)
        ;
    return i;
}

int compare_int(const void *a, const void *b)
{
  const int *da = (const int*) a;
  const int *db = (const int*) b;

  return (*da > *db) - (*da < *db);
}


// Taken from http://stackoverflow.com/questions/23999797/implementing-strnstr/24000056#24000056
char* memmem_(char* haystack, size_t hlen, char* needle, size_t nlen)
{
    if (nlen == 0) return haystack; // degenerate edge case
    if (hlen < nlen) return 0; // another degenerate edge case
    char *hlimit = haystack + hlen - nlen + 1;
    while ((haystack = memchr(haystack, needle[0], hlimit-haystack)))
    {
        if (!memcmp(haystack, needle, nlen)) return haystack;
        haystack++;
    }
    return 0;
}


// haystack is \0 (space if leading upper) (lowercase string), ends with two \0
// needles are a NULL terminated list of lowercase strings, with a leading space if upper
int text_search(char* haystack, char** needles, int exact, int* results)
{
    // information about the needles
    int n = count_pointers(needles);
    int uppers[n];        // is this needle uppercase prefix
    int lengths[n];       // length of this needle
    char* strings[n];     // actual text of this needle

    for (int i = 0; i < n; i++)
    {
        uppers[i] = needles[i][0] == ' ';
        strings[i] = uppers[i] ? &needles[i][1] : needles[i];
        lengths[i] = strlen(strings[i]);
    }

    int index = 0;          // Number of \n's I've already seen in haystack
    int found = 0;          // Number I've written to results

    for (index = 0; ; index++)
    {
        int upper = haystack[0] == ' ';
        if (upper) haystack++;
        int length = strlen(haystack);
        if (length == 0) break;
        char* string = haystack;
        haystack = &haystack[length+1];

        int score = 4;
        for (int i = 0; i < n; i++)
        {
            char* pos = strstr(string, strings[i]); // length, strings[i], lengths[i]);
            if (pos == NULL)
            {
                score = -1;
                break;
            }
            else if (pos == string)
            {
                int complete = lengths[i] == length;
                int cased = uppers[i] == upper;
                score = min_int(score, (!complete ? 2 : 0) + (!cased ? 1 : 0));
            }
        }
        if (score >= 0 && (!exact || score == 0))
            results[found++] = (score << 24) | index;
    }
    qsort(results, found, sizeof(int), compare_int);
    for (int i = 0; i < found; i++)
        results[i] = results[i] & 0xffffff;
    return found;
}
