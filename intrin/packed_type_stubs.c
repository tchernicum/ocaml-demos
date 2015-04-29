#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

struct t {
  long a;
  int b;
  short c;
  signed char d;
  long e;
  float f;
  double g;
};

value get_a(value t) { return ((struct t*) t)->a; }
value get_b(struct t *t) { return t->b; }
value get_c(struct t *t) { return t->c; }
value get_d(struct t *t) { return t->d; }
value get_e(struct t *t) { return t->e; }
value get_f(value t) {
  CAMLparam1(t);
  CAMLreturn(caml_copy_double(((struct t*) t)->f));
}
value get_g(value t) {
  CAMLparam1(t);
  CAMLreturn(caml_copy_double(((struct t*) t)->g));
}
void set_a(value t, value v) { ((struct t*) t)->a = v; }
void set_b(struct t *t, value v) { t->b = v; }
void set_c(struct t *t, value v) { t->c = v; }
void set_d(struct t *t, value v) { t->d = v; }
void set_e(struct t *t, value v) { t->e = v; }
void set_f(struct t *t, value v) { t->f = Double_val(v); }
void set_g(struct t *t, value v) { t->g = Double_val(v); }
