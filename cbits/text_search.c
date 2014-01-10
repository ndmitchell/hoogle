
#define _CRT_SECURE_NO_WARNINGS 1
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <malloc.h>

typedef int (*callback)(int, int*, char**, int);

#define MAX_IDENTIFIER_LENGTH 1000
#define MAX_DEPTH 20

int str_prefix(char* x, char* in)
{
    for (;; x++, in++)
    {
        int d = tolower(x[0]) - tolower(in[0]);
        if (x[0] == '\0')
            return 1;
        else if (d != 0)
            return 0;
    }
}

int str_infix(char* x, char* in)
{
    for (;; in++)
    {
        if (in[0] == '\0')
            return 0;
        else if (str_prefix(x, in))
            return 1;
    }
}

int match(int count, char** matches, char* x)
{
    int i;
    for (i = 0; i < count; i++)
    {
        if (!str_infix(matches[i], x))
            return -1;
    }
    return 1;
}

int text_search(char* filename, int count, char** matches, callback call)
{
    char buffer[MAX_IDENTIFIER_LENGTH]; // max size of an identifier (way bigger than necessary)
    int depth = 0;
    int ids[MAX_DEPTH];
    char* names[MAX_DEPTH];
    FILE* h;

    ids[0] = 0;
    names[0] = "";

    h = fopen(filename, "r");
    if (h == NULL) return -1; // failure

    while (fgets(buffer, MAX_IDENTIFIER_LENGTH, h) != NULL)
    {
        char* s = buffer;
        char* k = strchr(s, '\n');
        int m;
        if (s[0] == '{' || s[0] == '}') s++;
        if (k != NULL) k[0] = '\0';

        m = match(count, matches, s);

        if (buffer[0] == '}' && buffer[1] != '}')
        {
            depth--;
            free(names[depth]);
        }
        else if (buffer[0] == '{' && buffer[1] != '{')
        {
            ids[depth]++;
            names[depth] = _strdup(s);
            if (m >= 0)
                call(depth+1, ids, names, m);
            depth++;
            ids[depth] = 0;
        }
        else
        {
            if (m >= 0)
            {
                names[depth] = s;
                call(depth+1, ids, names, m);
            }
            ids[depth]++;
        }
    }

    fclose(h);
    return 0;
}

#if 0

int output(int n, int* path, char** names, int quality)
{
    int i;
    for (i = 0; i < n; i++)
    {
        if (i != 0) putchar('.');
        printf("%i", path[i]);
    }
    putchar(' ');
    for (i = 0; i < n; i++)
    {
        if (i != 0) putchar('.');
        printf("%s", names[i]);
    }
    printf(" with quality %i\n", quality);
    return -1;
}

int main(int argc, char* argv[])
{
    return text_search("default.str", argc-1, &argv[1], output);
}

#endif
