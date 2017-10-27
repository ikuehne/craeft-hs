#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

bool unsigned_gte(uint64_t a, uint64_t b);
bool signed_gte(int64_t a, int64_t b);
bool float_gte(float a, float b);
bool double_gte(double a, double b);
bool ptr_gte(char *a, char *b);

int main(void) {
    printf("%d\n", unsigned_gte(3, 1));
    printf("%d\n", unsigned_gte(1, 3));

    printf("%d\n", signed_gte(3, 1));
    printf("%d\n", signed_gte(1, 3));

    printf("%d\n", float_gte(3, 1));
    printf("%d\n", float_gte(1, 3));

    printf("%d\n", double_gte(3, 1));
    printf("%d\n", double_gte(1, 3));

    char *test = "test";

    printf("%d\n", ptr_gte(test + 3, test + 1));
    printf("%d\n", ptr_gte(test + 1, test + 3));

    return 0;
}
