#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

bool unsigned_gt(uint64_t a, uint64_t b);
bool signed_gt(int64_t a, int64_t b);
bool float_gt(float a, float b);
bool double_gt(double a, double b);
bool ptr_gt(char *a, char *b);

int main(void) {
    printf("%d\n", unsigned_gt(3, 1));
    printf("%d\n", unsigned_gt(1, 3));

    printf("%d\n", signed_gt(3, 1));
    printf("%d\n", signed_gt(1, 3));

    printf("%d\n", float_gt(3, 1));
    printf("%d\n", float_gt(1, 3));

    printf("%d\n", double_gt(3, 1));
    printf("%d\n", double_gt(1, 3));

    char *test = "test";

    printf("%d\n", ptr_gt(test + 3, test + 1));
    printf("%d\n", ptr_gt(test + 1, test + 3));

    return 0;
}
