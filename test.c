int counter = 0;

int increment_counter() {
    counter += 1;
}

int main() {
    increment_counter();
    increment_counter();
    increment_counter();
    return counter;
}