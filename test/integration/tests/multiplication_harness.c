#include <stdio.h>
#include <stdint.h>

uint64_t unsigned_mult(uint64_t a, uint64_t b);
int64_t signed_mult(int64_t a, int64_t b);
float float_mult(float a, float b);
double double_mult(double a, double b);
char *ptr_int_mult(char *a, int64_t b);
int64_t ptr_ptr_mult(char *a, char *b);

int main(void) {
    printf("%llu\n", unsigned_mult(2, 10));
    printf("%lld\n", signed_mult(2, 10));
    printf("%lld\n", signed_mult(-2, -10));
    printf("%lld\n", signed_mult(2, -10));
    printf("%g\n", float_mult(2, 10));
    printf("%g\n", double_mult(2, 10));
    return 0;
}
