// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// code.cpp
double image_diff_convolution_max_value(cpp11::doubles_matrix<by_column> diff_matrix, int kernel_size);
extern "C" SEXP _shinytest2_image_diff_convolution_max_value(SEXP diff_matrix, SEXP kernel_size) {
  BEGIN_CPP11
    return cpp11::as_sexp(image_diff_convolution_max_value(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles_matrix<by_column>>>(diff_matrix), cpp11::as_cpp<cpp11::decay_t<int>>(kernel_size)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_shinytest2_image_diff_convolution_max_value", (DL_FUNC) &_shinytest2_image_diff_convolution_max_value, 2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_shinytest2(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
