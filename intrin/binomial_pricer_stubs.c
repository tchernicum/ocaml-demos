#include <errno.h>
#include <stdlib.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define Caml_white (0 << 8)

#define Make_header(wosize, tag, color)                                       \
      (/*Assert ((wosize) <= Max_wosize),*/                                   \
       ((header_t) (((header_t) (wosize) << 10)                               \
                    + (color)                                                 \
                    + (tag_t) (tag)))                                         \
      )

value caml_aligned_array_create(size_t alignment, value len)
{
  CAMLparam1 (len);

  void* bp;
  mlsize_t bosize;
  int result;

  bosize = (Int_val(len) + 1) * alignment;
  result = posix_memalign(&bp, alignment, bosize);
  if (result != 0)
  {
    if (result == EINVAL)
      caml_failwith(
        "The alignment was not a power of two, or was not a multiple of sizeof(void *)");
    else if (result == ENOMEM)
      caml_raise_out_of_memory();
    else
      caml_failwith("Unrecognized error");
  }

  /* Leave space for the header */
  bp += alignment;
  
  Hd_bp (bp) =
    Make_header (Wosize_bhsize(Bhsize_bosize(bosize - alignment)),
                 Double_array_tag, Caml_white);

  CAMLreturn (Val_bp(bp));
}

value caml_aligned_array_free(size_t alignment, value val)
{
  CAMLparam1 (val);

  void* bp = Bp_val(val);
  bp -= alignment;
  free(bp);

  CAMLreturn (Val_unit);
}

CAMLprim value caml_aligned_array_16_create(value len)
{
  return caml_aligned_array_create(16, len);
}

CAMLprim value caml_aligned_array_16_free(value len)
{
  return caml_aligned_array_free(16, len);
}

CAMLprim value caml_aligned_array_32_create(value len)
{
  return caml_aligned_array_create(32, len);
}

CAMLprim value caml_aligned_array_32_free(value len)
{
  return caml_aligned_array_free(32, len);
}
