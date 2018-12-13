#include <bits/stdc++.h>

using namespace std;

void clockwise(int count, list<int>& lst, list<int>::iterator& it)
{
    for (int i = 0; i < count; ++i) {
        if (it == lst.end()) it = lst.begin();
        ++it;
    }
}

void counter_clockwise(int count, list<int>& lst, list<int>::iterator& it)
{
    for (int i = 0; i < count; ++i) {
        if (it == lst.begin()) it = lst.end();
        --it;
    }
}

template<typename T>
void printc(const T& l)
{
    copy(l.begin(), l.end(), ostream_iterator<int>(cout, ","));
    cout << endl;
}

void simulate(int nplayers, int max_num)
{
    list<int> circle;
    circle.push_back(0);
    int player = 0; // player 1 starts first (0 index)
    vector<size_t> scores(nplayers);
    auto current = circle.begin();
    for (int i = 1; i <= max_num; ++i) {
        if (i % 23 == 0) {
            scores[player] += i;
            counter_clockwise(7, circle, current);
            scores[player] += *current;
            current = circle.erase(current);
        } else {
            clockwise(2, circle, current);
            current = circle.insert(current, i);
        }
        //printc(circle);
        player = (player + 1) % nplayers;
    }

    size_t winner = 0;
    for (auto s : scores) winner = max(winner, s);
    cout << "Max score is " << winner << endl;
}


int main()
{
    // input: 452 players; last marble is worth 71250 points
    simulate(452, 71250);
    simulate(452, 7125000);
    return 0;
}
