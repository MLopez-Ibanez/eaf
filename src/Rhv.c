#include "Rcommon.h"
#include "hv.h"

extern SEXP
hypervolume_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT, SEXP REFERENCE);

extern SEXP
hv_contributions_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT, SEXP REFERENCE);

SEXP
hypervolume_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT, SEXP REFERENCE)
{
    int nprotected = 0;
    double *data = REAL(DATA);
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NPOINT, npoint);
    SEXP_2_DOUBLE_VECTOR(REFERENCE, reference, reference_len);

    if (nobj != reference_len)
        Rf_error("length of reference point (%d) is different from number of objectives (%d)",
                 reference_len, nobj);

    new_real_vector(hv, 1);
    hv[0] = fpli_hv(data, nobj, npoint, reference);

    UNPROTECT (nprotected);
    return Rexp(hv);
}

SEXP
hv_contributions_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT, SEXP REFERENCE)
{
    int nprotected = 0;
    double *data = REAL(DATA);
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NPOINT, npoint);
    SEXP_2_DOUBLE_VECTOR(REFERENCE, reference, reference_len);

    if (nobj != reference_len)
        Rf_error("length of reference point (%d) is different from number of objectives (%d)",
                 reference_len, nobj);

    new_real_vector(hv, npoint);
    hv_contributions(hv, data, nobj, npoint, reference);
    UNPROTECT (nprotected);
    return Rexp(hv);
}
