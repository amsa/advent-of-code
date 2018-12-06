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

struct Log
{
    string timestamp;
    string event;
};

void each_event(const string& path, function<void(const Log&)> fn)
{
    ifstream infile(path);
    regex pattern("\\[([^\\]]+)\\] (.+)");
    smatch match_res;
    for (string s; getline(infile, s); ) {
        if (!std::regex_match(s, match_res, pattern)) {
            cerr << "Invalid input: " << s << endl;
            return;
        }
        Log e {match_res[1].str(), match_res[2].str()};
        fn(e);
    }
}

vector<Log> ordered_logs(const string& path)
{
    vector<Log> logs;
    each_event(path, [&logs](const Log& l) {
            logs.push_back(l);
        });
    // sort logs based on timestamp
    sort(logs.begin(), logs.end(), [](const Log& x, const Log& y) {
            return x.timestamp < y.timestamp;
        });
    return logs;
}

int get_id(const string& event)
{
    if (event.empty() || event[0] != 'G') return -1;

    const string& sub = event.substr(7);
    auto offset = sub.find_first_of(' ');
    return stoi(sub.substr(0, offset));
}

unordered_map<int, unordered_map<int, int>> get_sleep_summary(const string& path)
{
    vector<Log> logs = ordered_logs(path);
    unordered_map<int, unordered_map<int, int>> guard_sleep;
    int guard_id = 0, min_asleep = 0;
    for (const Log& l : logs) {
        if (l.event[0] == 'G') { // new guard
            guard_id = get_id(l.event);
        } else if (l.event[0] == 'f') { // falls asleep
            // start timer
            min_asleep = stoi(l.timestamp.substr(l.timestamp.find(':')+1));
            //cout << guard_id << " -> asleep " << min_asleep << endl;
        } else if (l.event[0] == 'w') { // wakes up
            // stop timer
            int min_awake = stoi(l.timestamp.substr(l.timestamp.find(':')+1));
            for (auto i = min_asleep; i < min_awake; ++i) {
                guard_sleep[guard_id][i]++;
            }
            //cout << guard_id << " sleeps at " << min_asleep << " and wakes up at " << min_awake << endl;
        }
    }

    return guard_sleep;
}

void part_1()
{
    unordered_map<int, unordered_map<int, int>> guard_sleep = get_sleep_summary("input.txt");

    // find the sleep guard
    int guard_id = 0, min_asleep = 0;
    size_t max_sleep = 0;
    for (const auto& kv : guard_sleep) {
        size_t total_min = 0;
        for (const auto& minute : kv.second)
            total_min += minute.second;
        if (total_min > max_sleep) {
            max_sleep = total_min;
            guard_id = kv.first;
        }
    }
    cout << "1) Guard id: " << guard_id << ", Max sleep: " << max_sleep;

    // find the answer
    max_sleep = 0;
    for (const auto& minute : guard_sleep[guard_id]) {
        if (minute.second > (int) max_sleep) {
            max_sleep = minute.second;
            min_asleep = minute.first;
        }
    }

    cout << ", Min: " << min_asleep
         << ". Answer = " << (guard_id * min_asleep)  << endl;
}

void part_2()
{
    unordered_map<int, unordered_map<int, int>> guard_sleep = get_sleep_summary("input.txt");
    int guard_id = 0, min_asleep = 0, max_sleep = 0;
    for (const auto& kv : guard_sleep) {
        for (const auto& minute : kv.second) {
            if (minute.second > max_sleep) {
                max_sleep = minute.second;
                min_asleep = minute.first;
                guard_id = kv.first;
            }
        }
    }

    cout << "2) Guard id: " << guard_id << ", Max sleep on minute: " << min_asleep
         << ". Answer = " << (guard_id * min_asleep) << endl;
}

int main()
{
    part_1();
    part_2();
    return 0;
}
