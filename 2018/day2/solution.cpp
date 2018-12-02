#include <bits/stdc++.h>

using namespace std;

void each_line(const string& path, function<void(const string&)> fn)
{
    ifstream infile(path);
    for (string s; getline(infile, s); ) {
        fn(s);
    }
}

vector<size_t> histogram(const string& str)
{
    vector<size_t> hist(26);
    for (char c : str) {
        ++hist[c - 'a'];
    }
    return hist;
}

void part_1()
{
    size_t twos = 0,
        threes = 0;
    each_line("input.txt", [&twos, &threes](const string& line) {
        vector<size_t> hist = histogram(line);
        bool counted_2 = false,
            counted_3 = false;
        for (auto count : hist) {
            if (!counted_2 && count == 2) {
                ++twos;
                counted_2 = true;
            }
            if (!counted_3 && count == 3) {
                ++threes;
                counted_3 = true;
            }
        }
    });
    cout << "1) Checksum: " << twos * threes << endl;
}

// compare strings, and return the number of different chars
// `offset` is set to the smallest position where the two don't match
int diff(const string& id1, const string& id2, int& offset)
{
    offset = -1;
    if (id1.size() != id2.size()) return numeric_limits<int>::max();

    int d = 0;
    for (size_t i = 0; i < id1.size(); ++i) {
        if (id1[i] == id2[i]) continue;
        d++;
        // set offset if not set
        if (offset == -1) offset = i;
    }
    return d;
}

void part_2()
{
    vector<string> ids;
    each_line("input.txt", [&ids](const string& line) {
        ids.push_back(line);
    });

    for (size_t i = 0; i < ids.size(); ++i) {
        for (size_t j = i+1; j < ids.size(); ++j) {
            int offset;
            if (diff(ids[i], ids[j], offset) == 1) {
                cout << "2) Common letters: " << ids[i].substr(0, offset) + ids[i].substr(offset+1) << endl;
                return;
            }
        }
    }
}

int main()
{
    part_1();
    part_2();
    return 0;
}
