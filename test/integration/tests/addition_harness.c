#include <stdio.h>
#include <stdint.h>

int64_t signed_add(int64_t a, int64_t b);

int main(void) {
    printf("%lld\n", (long long)signed_add(10, 10));
}
