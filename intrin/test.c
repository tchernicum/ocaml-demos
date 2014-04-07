#include <stdio.h>
#include <stdlib.h>
#include "emmintrin.h"

int main()
{
  long long x, y, z, w;
  __m128i a;
  if (scanf("%lld %lld", &x, &y) != 2)
    return EXIT_FAILURE;

  z = x + y;
  w = x - y;
  a = _mm_set_epi64x(z, w);
  a = _mm_add_epi64(a, a);

  printf("%lld\n", _mm_cvtsi128_si64(a));

  return EXIT_SUCCESS;
}
