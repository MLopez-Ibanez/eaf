#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h> // for NULL

/* .Call calls */
extern SEXP compute_eaf_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP compute_eafdiff_area_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP compute_eafdiff_rectangles_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP compute_eafdiff_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP read_data_sets(SEXP);
extern SEXP hypervolume_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT, SEXP REFERENCE);
extern SEXP hv_contributions_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT, SEXP REFERENCE);
extern SEXP normalise_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
                        SEXP RANGE, SEXP LBOUND, SEXP UBOUND, SEXP MAXIMISE);

extern SEXP is_nondominated_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT, SEXP MAXIMISE, SEXP KEEP_WEAKLY);
extern SEXP pareto_ranking_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT);
extern SEXP epsilon_mul_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
                          SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE);
extern SEXP epsilon_add_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
                          SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE);
extern SEXP igd_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
                  SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE);
extern SEXP igd_plus_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
                       SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE);

#define DECLARE_CALL_ENTRY(NAME, NARGS) \
    {#NAME, (DL_FUNC) &NAME, NARGS},

static const R_CallMethodDef CallEntries[] = {
    {"compute_eaf_C",          (DL_FUNC) &compute_eaf_C,          5},
    {"compute_eafdiff_area_C", (DL_FUNC) &compute_eafdiff_area_C, 5},
    DECLARE_CALL_ENTRY(compute_eafdiff_rectangles_C, 5)
    {"compute_eafdiff_C",      (DL_FUNC) &compute_eafdiff_C,      5},
    {"read_data_sets",         (DL_FUNC) &read_data_sets,         1},
    {"hypervolume_C",          (DL_FUNC) &hypervolume_C,          4},
    {"hv_contributions_C",     (DL_FUNC) &hv_contributions_C,     4},
    {"normalise_C",            (DL_FUNC) &normalise_C,            7},
    DECLARE_CALL_ENTRY(is_nondominated_C, 5)
    DECLARE_CALL_ENTRY(pareto_ranking_C,  3)
    {"epsilon_add_C",          (DL_FUNC) &epsilon_add_C,          6},
    {"epsilon_mul_C",          (DL_FUNC) &epsilon_mul_C,          6},
    {"igd_C",                  (DL_FUNC) &igd_C,                  6},
    {"igd_plus_C",             (DL_FUNC) &igd_plus_C,             6},
    {NULL, NULL, 0}
};

void R_init_eaf(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
