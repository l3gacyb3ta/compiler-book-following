int putchar(int char);
int getchar();
int print_number(int num) {
    if (num < 0xa) {
        putchar(48 + num);
    } else {
        print_number(num / 10);
        putchar((num % 10) + 48);
    }
}
int print(int num) {
    print_number(num);
    putchar(10);
}
int fib(int n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}
int main() {
    print(fib(20));
}
