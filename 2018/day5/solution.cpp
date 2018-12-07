#include <bits/stdc++.h>

using namespace std;

void part_1()
{
    ifstream input("input.txt");
    auto& f = std::use_facet<std::ctype<char>>(std::locale());
    deque<char> stack;
    for (char c; input.get(c) && !isspace(c); ) {
        if (!stack.empty() &&
            stack.back() != c &&
            f.tolower(stack.back()) == f.tolower(c)) {
            stack.pop_back();
        } else {
            stack.push_back(c);
        }
    }
    cout << "1) #units: " << stack.size() << endl;
}

void part_2()
{
    auto& f = std::use_facet<std::ctype<char>>(std::locale());

    ifstream input("input.txt");
    string s; getline(input, s);

    size_t min_length = s.size();
    for (char ch = 'a'; ch <= 'z'; ++ch) {
        deque<char> stack;
        for (char c : s) {
            if (f.tolower(c) == ch) continue;
            if (!stack.empty() &&
                stack.back() != c &&
                f.tolower(stack.back()) == f.tolower(c)) {
                stack.pop_back();
            } else {
                stack.push_back(c);
            }
        }
        min_length = min(min_length, stack.size());
    }

    cout << "2) #units: " << min_length << endl;
}

int main()
{
    part_1();
    part_2();
    return 0;
}
