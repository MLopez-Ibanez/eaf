#include "Rcommon.h"
#include "epsilon.h"
#include "nondominated.h"

SEXP
epsilon_mul_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
              SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE);
SEXP
epsilon_add_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
              SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE);

SEXP
epsilon_mul_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
              SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE)
{
    int nprotected = 0;
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NPOINT, npoint);
    double *ref = REAL(REFERENCE);
    SEXP_2_INT(REFERENCE_SIZE, ref_size);
    SEXP_2_LOGICAL_VECTOR(MAXIMISE, maximise, maximise_len);

    if (nobj != maximise_len)
        error("length of maximise (%d) is different from number of objectives (%d)",
              maximise_len, nobj);

    signed char * minmax = create_minmax(nobj, maximise);

    new_real_vector(epsilon, 1);
    double *data = REAL(DATA);
    epsilon[0] = epsilon_mult (nobj, minmax, data, npoint, ref, ref_size);

    free (minmax);
    UNPROTECT (nprotected);
    return Rexp(epsilon);
}

SEXP
epsilon_add_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
              SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE)
{
    int nprotected = 0;
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NPOINT, npoint);
    double *ref = REAL(REFERENCE);
    SEXP_2_INT(REFERENCE_SIZE, ref_size);
    SEXP_2_LOGICAL_VECTOR(MAXIMISE, maximise, maximise_len);

    if (nobj != maximise_len)
        error("length of maximise (%d) is different from number of objectives (%d)",
              maximise_len, nobj);

    signed char * minmax = create_minmax(nobj, maximise);

    new_real_vector(epsilon, 1);
    double *data = REAL(DATA);
    epsilon[0] = epsilon_additive (nobj, minmax, data, npoint, ref, ref_size);

    free (minmax);
    UNPROTECT (nprotected);
    return Rexp(epsilon);
}
