#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <signal.h>

CAMLprim value ocaml_progress_sigwinch (value unit)
{
  CAMLparam1 (unit);
  CAMLreturn (Val_int (SIGWINCH));
}
