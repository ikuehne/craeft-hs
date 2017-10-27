#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

bool unsigned_gte(uint64_t a, uint64_t b);
bool signed_gte(int64_t a, int64_t b);
bool float_gte(float a, float b);
bool double_gte(double a, double b);
bool *ptr_gte(char *a, char *b);

int main(void) {
    puts(unsigned_gte(3, 1)? "true": "false");
    puts(unsigned_gte(1, 3)? "true": "false");

    puts(signed_gte(3, 1)? "true": "false");
    puts(signed_gte(1, 3)? "true": "false");

    puts(float_gte(3, 1)? "true": "false");
    puts(float_gte(1, 3)? "true": "false");

    puts(double_gte(3, 1)? "true": "false");
    puts(double_gte(1, 3)? "true": "false");

    char *test = "test";

    puts(ptr_gte(test + 3, test + 1)? "true": "false");
    puts(ptr_gte(test + 1, test + 3)? "true": "false");

    return 0;
}
