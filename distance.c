/* vim:set ft=c et ts=2 sw=2: */
#include <stddef.h>

ptrdiff_t distance_float(const float *a, const float *b)
{
  return b - a;
}

ptrdiff_t distance_double(const double *a, const double *b)
{
  return b - a;
}
