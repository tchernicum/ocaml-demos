#include <math.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

float float_min_stub(float x, float y)
{
  float z;
  asm("minsd	%2, %0" : "=x" (z) : "0" (x), "xm" (y));
  return z;
}
