#include <bits/stdc++.h>

using namespace std;

struct TreeNode
{
    vector<TreeNode*> children;
    vector<int> metadata;
    int value;

    TreeNode() : children(), metadata(), value(-1) {}

    void add_children(TreeNode* n)
    {
        children.push_back(n);
    }

    void add_metadata(int d)
    {
        metadata.push_back(d);
    }

    int sum_metadata(TreeNode* node)
    {
        int s = 0;
        for (int md : node->metadata) {
            s += md;
        }
        for (TreeNode* c : node->children) {
            s += sum_metadata(c);
        }
        return s;
    }

    int get_value()
    {
        if (value > -1) return value;
        set_value();
        return value;
    }

private:
    void set_value()
    {
        if (children.empty()) {
            value = accumulate(metadata.begin(), metadata.end(), 0);
            return;
        }

        // sum up the values of all children
        int val = 0;
        for (int md : metadata) {
            size_t idx = md - 1;
            if (idx < 0 || idx >= children.size()) continue;
            val += children[idx]->get_value();
        }
        value = val;
    }
};

TreeNode* get_node(ifstream& in)
{
    TreeNode* node = new TreeNode();
    int ch, md;
    in >> ch >> md;
    for (int i = 0; i < ch; ++i) {
        node->add_children(get_node(in));
    }
    for (int i = 0; i < md; ++i) {
        int v;
        in >> v;
        node->add_metadata(v);
    }
    return node;
}

void part_1()
{
    ifstream in("input.txt");
    TreeNode* root = get_node(in);
    cout << "1) Metadata sum = " << root->sum_metadata(root) << endl;
}

void part_2()
{
    ifstream in("input.txt");
    TreeNode* root = get_node(in);
    cout << "2) Root value = " << root->get_value() << endl;
}

int main()
{
    part_1();
    part_2();
    return 0;
}
