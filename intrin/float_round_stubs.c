#include <immintrin.h>
#include <math.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

double floor_stub(double v)
{
  asm("roundsd	$1, %0, %0" : "=x" (v));
  return v;
}

double ceil_stub(double v)
{
  asm("roundsd	$2, %0, %0" : "=x" (v));
  return ceil(v);
}
