#include <bits/stdc++.h>

using namespace std;

struct pair_hash {
    template <class T1, class T2>
    std::size_t operator () (const std::pair<T1,T2> &p) const {
        auto h1 = std::hash<T1>{}(p.first);
        auto h2 = std::hash<T2>{}(p.second);

        // Mainly for demonstration purposes, i.e. works but is overly simple
        // In the real world, use sth. like boost.hash_combine
        return h1 ^ h2;
    }
};

using Position = pair<int, int>;
using Dimension = pair<int, int>;

void each_area(const string& path, function<void(int, Position, Dimension)> fn)
{
    ifstream infile(path);
    // #1 @ 37,526: 17x23
    regex pattern("#[0-9]+ @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)");
    smatch match_res;
    int i = 1;
    for (string s; getline(infile, s); ++i) {
        if (!std::regex_match(s, match_res, pattern)) {
            cerr << "Invalid input: " << s << endl;
            return;
        }
        Position offset = make_pair(stoi(match_res[1].str()), stoi(match_res[2].str()));
        Dimension dim = make_pair(stoi(match_res[3].str()), stoi(match_res[4].str()));
        fn(i, offset, dim);
    }
}

void part_1()
{
    unordered_map<Position, int, pair_hash> pos_map;
    each_area("input.txt", [&pos_map](int id, Position off, Dimension dim) {
        for (int i = 0; i < dim.first; ++i) {
            for (int j = 0; j < dim.second; ++j) {
                ++pos_map[{off.first+i, off.second+j}];
            }
        }
    });

    int total = 0;
    for (const auto& kv : pos_map) {
        if (kv.second > 1) ++total;
    }
    cout << "1) #overlapping squares: " << total << endl;
}

void part_2()
{
    // keep track of number of cells for each square
    unordered_map<int, int> cell_count;

    // keep track of all the squares that fill a cell
    unordered_map<Position, unordered_set<int>, pair_hash> pos_map;
    each_area("input.txt", [&cell_count, &pos_map](int id, Position off, Dimension dim) {
        for (int i = 0; i < dim.first; ++i) {
            for (int j = 0; j < dim.second; ++j) {
                pos_map[{off.first+i, off.second+j}].insert(id);
                ++cell_count[id];
            }
        }
    });

    // find overlaps between areas by id
    for (const auto& kv : pos_map) {
        if (kv.second.size() == 1) {
            --cell_count[*kv.second.begin()];
        }
    }

    // find the area whose cell count is now zero,
    // and that should be our unique area
    for (const auto& kv : cell_count) {
        if (kv.second == 0) {
            cout << "2) square ID: " << kv.first << endl;
            return;
        }
    }
}

int main()
{
    part_1();
    part_2();
    return 0;
}
