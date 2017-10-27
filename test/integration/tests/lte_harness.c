#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

bool unsigned_lte(uint64_t a, uint64_t b);
bool signed_lte(int64_t a, int64_t b);
bool float_lte(float a, float b);
bool double_lte(double a, double b);
bool *ptr_lte(char *a, char *b);

int main(void) {
    puts(unsigned_lte(3, 1)? "true": "false");
    puts(unsigned_lte(1, 3)? "true": "false");

    puts(signed_lte(3, 1)? "true": "false");
    puts(signed_lte(1, 3)? "true": "false");

    puts(float_lte(3, 1)? "true": "false");
    puts(float_lte(1, 3)? "true": "false");

    puts(double_lte(3, 1)? "true": "false");
    puts(double_lte(1, 3)? "true": "false");

    char *test = "test";

    puts(ptr_lte(test + 3, test + 1)? "true": "false");
    puts(ptr_lte(test + 1, test + 3)? "true": "false");

    return 0;
}
