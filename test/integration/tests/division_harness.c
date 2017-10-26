#include <stdio.h>
#include <stdint.h>

uint64_t unsigned_div(uint64_t a, uint64_t b);
int64_t signed_div(int64_t a, int64_t b);
float float_div(float a, float b);
double double_div(double a, double b);
char *ptr_int_div(char *a, int64_t b);
int64_t ptr_ptr_div(char *a, char *b);

int main(void) {
    printf("%llu\n", unsigned_div(200, 10));
    printf("%lld\n", signed_div(200, 10));
    printf("%lld\n", signed_div(-200, -10));
    printf("%lld\n", signed_div(200, -10));
    printf("%g\n", float_div(200, 10));
    printf("%g\n", double_div(200, 10));
    return 0;
}
