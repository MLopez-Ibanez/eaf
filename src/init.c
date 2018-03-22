#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP compute_eaf_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP compute_eafdiff_area_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP compute_eafdiff_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP read_data_sets(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"compute_eaf_C",          (DL_FUNC) &compute_eaf_C,          5},
    {"compute_eafdiff_area_C", (DL_FUNC) &compute_eafdiff_area_C, 5},
    {"compute_eafdiff_C",      (DL_FUNC) &compute_eafdiff_C,      5},
    {"read_data_sets",         (DL_FUNC) &read_data_sets,         1},
    {NULL, NULL, 0}
};

void R_init_eaf(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
