#include "Rcommon.h"
#include "nondominated.h"

extern SEXP
normalise_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
            SEXP RANGE, SEXP LBOUND, SEXP UBOUND, SEXP MAXIMISE);

SEXP
normalise_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
            SEXP RANGE, SEXP LBOUND, SEXP UBOUND, SEXP MAXIMISE)
{
    int nprotected = 0;
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NPOINT, npoint);
    SEXP_2_DOUBLE_VECTOR(RANGE, range, range_len);
    SEXP_2_DOUBLE_VECTOR(LBOUND, lbound, lbound_len);
    SEXP_2_DOUBLE_VECTOR(UBOUND, ubound, ubound_len);
    SEXP_2_LOGICAL_VECTOR(MAXIMISE, maximise, maximise_len);

    if (nobj != lbound_len)
        error("length of lbound (%d) is different from number of objectives (%d)",
              lbound_len, nobj);
    if (nobj != ubound_len)
        error("length of ubound (%d) is different from number of objectives (%d)",
              ubound_len, nobj);
    if (nobj != maximise_len)
        error("length of maximise (%d) is different from number of objectives (%d)",
              maximise_len, nobj);
    if (range_len != 2)
        error("length of range must be two (lower, upper)");

    signed char * minmax = malloc(sizeof(signed char) * nobj);
    for (int k = 0; k < nobj; k++) {
        minmax[k] = (maximise[k] == TRUE) ? AGREE_MAXIMISE : (maximise[k] == FALSE) ? AGREE_MINIMISE : AGREE_NONE;
    }

    // FIXME: Is this slower than pure R? Why is R so inefficient?
    new_real_matrix (out, nobj, npoint);
    double * data = REAL(DATA);
    for (int i = 0; i < nobj * npoint; i++)
        out[i] = data[i];

    normalise(out, nobj, npoint, minmax, AGREE_NONE, range[0], range[1],
              lbound, ubound);

    free (minmax);
    UNPROTECT(nprotected);
    return Rexp_out;
}

