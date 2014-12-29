#include <stdio.h>
#include <string>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string.h>

/*
1000 in 9 seconds

so each one takes ~0.01s - fine
10x faster than haskell
*/

std::string readFile(const char* file)
{
        std::ifstream f;
        std::stringstream ss;

        f.open(file, std::ios::in | std::ios::binary);

        if (f.fail()) printf("doh\n");
        ss << f.rdbuf();
        if (!f && !f.eof()) printf("doh\n");
        f.close();

        return ss.str();
}


int main()
{
    std::string x = readFile("output/bullet.ids");
    const char* s = x.c_str();
    std::cout << "Paused, press enter to continue...";
    std::cin.get();
    for (int i = 1; i <= 1000; i++)
    {
        char buf[3];
        buf[0] = '?';
        buf[1] = i;
        buf[2] = 0;

        char* c = strstr(s, buf);
        printf("%i %i\n", i, c == 0 ? -1 : c - s);
    }
    std::cout << "Done";
    return 0;
}
