int main() {
    int a = 1;
    {
        int a = 3;
        a = 4;
    }

    return a;
}