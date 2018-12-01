#include <bits/stdc++.h>

using namespace std;

void each_line(const string& path, function<bool(long)> fn)
{
    ifstream infile(path);
    for (string s; getline(infile, s); ) {
        long long n = stoull(s.substr(1));
        if (s[0] == '-') {
            n *= -1;
        }
        if (!fn(n)) return;
    }
}

void part_1()
{
    long total = 0;
    each_line("input.txt", [&total](long n) {
        total += n;
        return true;
    });
    cout << "Total: " << total << endl;
}

void part_2()
{
    long total = 0;
    bool run = true;
    unordered_set<long> value_set;
    while (run) {
        each_line("input.txt", [&total, &run, &value_set](long n) {
            total += n;

            if (value_set.count(total)) {
                cout << "The frequency reaches to " << total << " twice. \n";
                run = false;
                return false;
            } else {
                value_set.insert(total);
            }
            return true;
        });
    }
}

int main()
{
    part_1();
    part_2();
    return 0;
}
