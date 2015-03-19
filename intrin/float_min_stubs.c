#include <math.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value float_min_asm_stub(value a_v, value b_v)
{
  CAMLparam2(a_v, b_v);
  CAMLreturn(caml_copy_double(fmin(Double_val(a_v), Double_val(b_v))));
}
