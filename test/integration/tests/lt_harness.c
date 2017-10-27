#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

bool unsigned_lt(uint64_t a, uint64_t b);
bool signed_lt(int64_t a, int64_t b);
bool float_lt(float a, float b);
bool double_lt(double a, double b);
bool *ptr_lt(char *a, char *b);

int main(void) {
    puts(unsigned_lt(3, 1)? "true": "false");
    puts(unsigned_lt(1, 3)? "true": "false");

    puts(signed_lt(3, 1)? "true": "false");
    puts(signed_lt(1, 3)? "true": "false");

    puts(float_lt(3, 1)? "true": "false");
    puts(float_lt(1, 3)? "true": "false");

    puts(double_lt(3, 1)? "true": "false");
    puts(double_lt(1, 3)? "true": "false");

    char *test = "test";

    puts(ptr_lt(test + 3, test + 1)? "true": "false");
    puts(ptr_lt(test + 1, test + 3)? "true": "false");

    return 0;
}
