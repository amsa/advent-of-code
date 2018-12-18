#include <bits/stdc++.h>

using namespace std;

int power_level(int sno, int x, int y)
{
    int rackID = 10 + x;
    long v = y * rackID;
    v += sno;
    v *= rackID;
    v = (v / 100) % 10;
    v -= 5;
    return v;
}

void test()
{
    assert(power_level(8, 3, 5) == 4);
    assert(power_level(57, 122, 79) == -5);
    assert(power_level(71, 101, 153) == 4);
}


void part_1(int sno, size_t N=300)
{
    int grid[N][N];
    for (size_t j = 0; j < N; ++j) {
        for (size_t i = 0; i < N; ++i) {
            grid[j][i] = power_level(sno, i, j);
        }
    }

    // find the largest square
    int max_val = 0;
    pair<int,int> pos;
    for (size_t j = 0; j < N-3; ++j) {
        for (size_t i = 0; i < N-3; ++i) {
            int sum = 0;
            for (int y = 0; y < 3; ++y) {
                for (int x = 0; x < 3; ++x) {
                    sum += grid[y+j][x+i];
                }
            }
            if (sum > max_val) {
                max_val = sum;
                pos = {i, j};
            }
        }
    }

    printf("1) Top-left position: (%d, %d) \n", pos.first, pos.second);
}

void part_2(int sno, size_t N=300)
{
    int grid[N][N];
    for (size_t j = 0; j < N; ++j) {
        for (size_t i = 0; i < N; ++i) {
            grid[j][i] = power_level(sno, i, j);
        }
    }

    // find the largest square with brute force
    int max_val = 0, max_size = 0;
    pair<int,int> pos;
    for (int sq = N-1; sq > 3; --sq) {
        for (size_t j = 0; j < N-sq; ++j) {
            for (size_t i = 0; i < N-sq; ++i) {
                int sum = 0;
                for (int y = 0; y < sq; ++y) {
                    for (int x = 0; x < sq; ++x) {
                        sum += grid[y+j][x+i];
                    }
                }
                if (sum > max_val) {
                    max_size = sq;
                    max_val = sum;
                    pos = {i, j};
                }
            }
        }
    }

    printf("2) Top-left position: (%d, %d) with square of size %d \n",
           pos.first, pos.second, max_size);
}

int main()
{
    test();
    part_1(4842);
    part_2(4842);
    return 0;
}
