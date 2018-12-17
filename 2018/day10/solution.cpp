#include <bits/stdc++.h>

using namespace std;

using Point = pair<long,long>;

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

template<typename T>
void printc(const T& l)
{
    copy(l.begin(), l.end(), ostream_iterator<int>(cout, ","));
    cout << endl;
}

void read_points(const string& path, vector<Point>& points, vector<Point>& v)
{
    ifstream in(path);
    regex pattern("position=< *(-?[0-9]+), *(-?[0-9]+)> velocity=< *(-?[0-9]+), *(-?[0-9]+)>");
    smatch match_res;
    for (string s; getline(in, s); ) {
        if (!std::regex_match(s, match_res, pattern)) {
            cerr << "Invalid input: " << s << endl;
            return;
        }

        int x = stol(match_res[1].str()),
            y = stol(match_res[2].str()),
            vx = stol(match_res[3].str()),
            vy = stol(match_res[4].str());

        //printf("(%d, %d) -> (%d, %d) \n", x, y, vx, vy);
        points.push_back({x, y});
        v.push_back({vx, vy});
    }
}

void get_min_max(const vector<Point>& points, Point& xrange, Point& yrange)
{
    long min_x = numeric_limits<long>::max(),
        max_x = numeric_limits<long>::min(),
        min_y = min_x,
        max_y = max_x;

    for (size_t i = 0; i < points.size(); ++i) {
        min_x  = min(points[i].first, min_x);
        max_x  = max(points[i].first, max_x);

        min_y = min(points[i].second, min_y);
        max_y = max(points[i].second, max_y);
    }

    xrange.first = min_x;
    xrange.second = max_x;
    yrange.first = min_y;
    yrange.second = max_y;
}

bool in_bound(const Point& xrange, const Point& yrange)
{
    constexpr int bound = 100;
    return xrange.second - xrange.first < bound &&
           yrange.second - yrange.first < bound;
}

void plot(const vector<Point>& points,
          const Point& xrange,
          const Point& yrange,
          vector<char>& out)
{
    unordered_set<Point, pair_hash> p_set(points.begin(), points.end());

    out.resize(0);
    for (auto j = yrange.first; j <= yrange.second; ++j) {
        for (auto i = xrange.first; i <= xrange.second; ++i) {
            if (p_set.count({i, j})) {
                out.push_back('#');
            } else {
                out.push_back('.');
            }
            cout << out.back();
        }
        out.push_back('\n');
        cout << out.back();
    }
}

void solve()
{
    vector<Point> points, vel;
    read_points("input.txt", points, vel);
    vector<char> output;
    size_t time = 0;
    bool found = false;
    for (size_t t = 0; t < 100000 ; ++t) {
        Point xrange, yrange;
        get_min_max(points, xrange, yrange);
        if (in_bound(xrange, yrange)) {
            size_t last_size = output.size();
            plot(points, xrange, yrange, output);
            found = true;
            if (last_size > 0 && last_size < output.size()) {
                // if the output size starts growing at t, t-1 must have been the
                // time the message appeared
                time = t-1;
                break;
            }
        } else if (found) break;

        // move the points
        for (size_t i = 0; i < points.size(); ++i) {
            points[i].first  += vel[i].first;
            points[i].second += vel[i].second;
        }
    }
    cout << "Time = " << time << endl;

}

int main()
{
    solve();
    return 0;
}
