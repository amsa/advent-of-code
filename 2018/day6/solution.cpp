#include <bits/stdc++.h>

using namespace std;

using Point = pair<int,int>;

vector<Point> get_points(const string& path)
{
    vector<Point> res;
    ifstream input(path);
    for (string s; getline(input, s); ) {
        const auto pos = s.find(',');
        res.push_back({stoi(s.substr(0, pos)),
                       stoi(s.substr(pos+2))});
    }
    return res;
}

int manhattan_distance(const Point& p1, const Point& p2)
{
    return abs(p1.first - p2.first) +
        abs(p1.second - p2.second);
}

void solve()
{
    auto points = get_points("input.txt");
    // find the width and height of the grid we want to construct
    int width = 0, height = 0;
    for (const auto& p : points) {
        width = max(width, p.first);
        height = max(height, p.second);
    }
    width++;
    height++;
    // create the grid
    Point grid[height][width];
    memset(grid, 0, sizeof(grid[0][0]) * width * height);

    // populate the grid
    for (size_t p = 0; p < points.size(); ++p) {
        grid[points[p].second][points[p].first] = {0, p+1};
    }
    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
            if (grid[i][j].second != 0) {
                continue;
            }

            deque<Point> min_dist;
            for (size_t p = 0; p < points.size(); ++p) {
                int d = manhattan_distance({j, i}, points[p]);
                while (!min_dist.empty() && min_dist.back().first > d)
                    min_dist.pop_back();

                if (min_dist.empty() || d == min_dist.back().first) {
                    min_dist.push_back({d, p+1});
                }
            }
            assert(!min_dist.empty());
            if (min_dist.size() == 1) {
                grid[i][j] = min_dist.back();
            } else {
                // equally close to more than one point
                grid[i][j] = {-1, -1};
            }
        }
    }

    // rule out the points with infinite close areas
    unordered_set<int> infinite; // set of point indices
    for (int i = 0; i < height; ++i) {
        infinite.insert(grid[i][0].second);
        infinite.insert(grid[i][width-1].second);
    }
    for (int i = 0; i < width; ++i) {
        infinite.insert(grid[0][i].second);
        infinite.insert(grid[height-1][i].second);
    }
    if (infinite.count(-1)) infinite.erase(-1);

    {
        // find the answer
        vector<int> counts(points.size()); // map of point index -> count
        for (int i = 0; i < height; ++i) {
            for (int j = 0; j < width; ++j) {
                int pointIdx = grid[i][j].second;
                if (pointIdx == -1 || infinite.count(pointIdx)) continue;
                ++counts[pointIdx-1];
            }
        }
        int res = 0;
        for (int c : counts) {
            res = max(res, c);
        }
        cout << "1) Size of the largest area: " << res << endl;
    }

    ////////////////////
    ///// part 2

    // size of the region
    size_t region = 0;
    constexpr int MAX_DIST = 10000;
    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
            size_t distance = 0;
            for (size_t p = 0; p < points.size(); ++p) {
                distance += manhattan_distance({j, i}, points[p]);
                if (distance >= MAX_DIST) break;
            }

            if (distance < MAX_DIST) ++region;
        }
    }

    cout << "2) Region size: " << region << endl;
}


int main()
{
    solve();
    return 0;
}
