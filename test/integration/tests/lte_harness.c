#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

bool unsigned_lte(uint64_t a, uint64_t b);
bool signed_lte(int64_t a, int64_t b);
bool float_lte(float a, float b);
bool double_lte(double a, double b);
bool ptr_lte(char *a, char *b);

int main(void) {
    printf("%d\n", unsigned_lte(3, 1));
    printf("%d\n", unsigned_lte(1, 3));

    printf("%d\n", signed_lte(3, 1));
    printf("%d\n", signed_lte(1, 3));

    printf("%d\n", float_lte(3, 1));
    printf("%d\n", float_lte(1, 3));

    printf("%d\n", double_lte(3, 1));
    printf("%d\n", double_lte(1, 3));

    char *test = "test";

    printf("%d\n", ptr_lte(test + 3, test + 1));
    printf("%d\n", ptr_lte(test + 1, test + 3));

    return 0;
}
