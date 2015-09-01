#include <immintrin.h>
#include <caml/mlvalues.h>

value string_index_stub(value vs, value vc)
{
  __m128i const* s = (__m128i const*) vs;
  const int c = Int_val(vc);
  int l = Wosize_val(vs) * 8, p = 0, r;
  __m128i cc, a;

  while (1)
  {
    cc = _mm_set1_epi64x(c);
    a = _mm_loadu_si128(s);
    r = _mm_cmpestri(cc, 1, a, l, 0);
    if (r < 16)
      return Val_int(p + r);
    else if (l < 16)
      return Val_int(-1);
    l -= 16;
    p += 16;
    s += 1;
  }
}
