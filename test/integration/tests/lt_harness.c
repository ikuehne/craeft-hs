#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

bool unsigned_lt(uint64_t a, uint64_t b);
bool signed_lt(int64_t a, int64_t b);
bool float_lt(float a, float b);
bool double_lt(double a, double b);
bool ptr_lt(char *a, char *b);

int main(void) {
    printf("%d\n", unsigned_lt(3, 1));
    printf("%d\n", unsigned_lt(1, 3));

    printf("%d\n", signed_lt(3, 1));
    printf("%d\n", signed_lt(1, 3));

    printf("%d\n", float_lt(3, 1));
    printf("%d\n", float_lt(1, 3));

    printf("%d\n", double_lt(3, 1));
    printf("%d\n", double_lt(1, 3));

    char *test = "test";

    printf("%d\n", ptr_lt(test + 3, test + 1));
    printf("%d\n", ptr_lt(test + 1, test + 3));

    return 0;
}
