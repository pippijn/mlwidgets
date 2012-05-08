#define _GNU_SOURCE 1
#include <caml/fail.h>
#include <caml/mlvalues.h>

#include <errno.h>
#include <locale.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <wchar.h>

CAMLprim value
ml_setlocale (value locale)
{
  setlocale (LC_ALL, String_val (locale));
  return Val_unit;
}

CAMLprim value
ml_wcwidth (value c)
{
  return Val_int (wcwidth (Int_val (c)));
}

CAMLprim value
ml_winrows (value unit)
{
  struct winsize size;

  if (ioctl (STDOUT_FILENO, TIOCGWINSZ, &size) == 0)
    return Val_int (size.ws_row);

  caml_failwith (strerror (errno));
  return Val_int (0);
}

CAMLprim value
ml_wincols (value unit)
{
  struct winsize size;

  if (ioctl (STDOUT_FILENO, TIOCGWINSZ, &size) == 0)
    return Val_int (size.ws_col);

  caml_failwith (strerror (errno));
  return Val_int (0);
}
