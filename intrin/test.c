#include <stdio.h>
#include "emmintrin.h"

void main()
{
  __m64 x, y;
  scanf("%ld %ld", (long int*) &x, (long int*) &y);
  __m128i z = _mm_set_epi64(x, y);

  printf("%ld\n", (long int) x);
}
