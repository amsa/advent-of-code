#include <bits/stdc++.h>

using namespace std;

using NodeSet = set<char>;
using Graph = map<char, set<char>>;

template <typename T>
void get_graphs(const string& path,
                Graph& children,
                Graph& parents,
                T& roots)
{
    ifstream infile(path);
    NodeSet keys, values;
    for (string s; getline(infile, s); ) {
        char p = s[5], m = s[36];
        keys.insert(p);
        values.insert(m);
        children[p].insert(m);
        parents[m].insert(p);
        parents[p];
    }

    // find the root nodes
    set_difference(keys.begin(), keys.end(),
                   values.begin(), values.end(),
                   inserter(roots, roots.begin()));
}

void part_1()
{
    // topological sort with an ordered set
    NodeSet roots;
    Graph children, parents;
    get_graphs("input.txt", children, parents, roots);
    stringstream res;
    while (!roots.empty()) {
        char n = *roots.begin();
        roots.erase(roots.begin());
        res << n;
        for (char m : children[n]) {
            parents[m].erase(n);
            if (parents[m].empty()) {
                roots.insert(m);
            }
        }
        children.erase(n);
    }

    cout << res.str() << endl;
}

struct Worker
{
    Worker(int id) : id(id), is_busy(false), job(0), remaining(0) {}
    void process(char task, int base_cost)
    {
        if (is_busy) {
            throw runtime_error("Cannot assign work to a busy worker.");
        }
        job = task;

        is_busy = true;
        remaining = base_cost;
        remaining += job - 'A' + 1;

        //printf("[%d] Processing %c with %d time required. \n", id, job, remaining);
    }

    void tick()
    {
        if (--remaining == 0) {
            is_busy = false;
        }
        //printf("[%d] tick. Remaining = %d \n", id, remaining);
    }

    int id;
    bool is_busy;
    char job;
    int remaining;
};

void part_2()
{
    // initialize the worker workers
    constexpr int NWORKERS = 5;
    constexpr int FIXED_COST = 60;
    deque<Worker> workers;
    for (size_t i = 0; i < NWORKERS; ++i) {
        workers.push_back(Worker(i));
    }

    NodeSet working, done;
    NodeSet roots;
    Graph children, parents;
    get_graphs("input.txt", children, parents, roots);

    auto next_job = [&done, &working, &parents]() {
        for (const auto& p : parents) {
            if (done.count(p.first) == 0 && working.count(p.first) == 0) {
                bool satisfied = true;
                for (auto c : p.second) {
                    satisfied &= done.count(c);
                }

                if (satisfied) return p.first;
            }
        }
        return (char) 0;
    };

    auto any_active_workers = [&workers]() {
        for (const auto& w : workers) {
            if (w.is_busy) return true;
        }
        return false;
    };

    int t = 0;
    while (done.size() < parents.size() || any_active_workers()) {
        // make some progress
        for (auto& w : workers) {
            if (w.is_busy) {
                w.tick();
            }
            if (!w.is_busy) {
                working.erase(w.job);
                done.insert(w.job);
            }
        }

        // assign the task
        for (auto& w : workers) {
            if (!w.is_busy) {
                char n = next_job();
                if (!n) break;
                w.process(n, FIXED_COST);
                working.insert(n);
            }
        }
        ++t;
    }

    cout << "Time = " << (t-1) << endl;
}

int main()
{
    part_1();
    part_2();
    return 0;
}
