#include "Rcommon.h"
#include "nondominated.h"

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
        Rf_error("length of lbound (%d) is different from number of objectives (%d)",
                 lbound_len, nobj);
    if (nobj != ubound_len)
        Rf_error("length of ubound (%d) is different from number of objectives (%d)",
                 ubound_len, nobj);
    if (nobj != maximise_len)
        Rf_error("length of maximise (%d) is different from number of objectives (%d)",
              maximise_len, nobj);
    if (range_len != 2)
        Rf_error("length of range must be two (lower, upper)");

    signed char * minmax = create_minmax(nobj, maximise);
    
    // FIXME: Is this slower than pure R? Why is R so inefficient?
    new_real_matrix (out, nobj, npoint);
    double * data = REAL(DATA);
    for (int i = 0; i < nobj * npoint; i++)
        out[i] = data[i];

    // We have to make the objectives agree before normalisation.
    // FIXME: Do normalisation and agree in one step.
    const signed char agree = AGREE_MINIMISE;
    agree_objectives (out, nobj, npoint, minmax, agree);
    normalise(out, nobj, npoint, minmax, agree, range[0], range[1],
              lbound, ubound);

    free (minmax);
    UNPROTECT(nprotected);
    return Rexp_out;
}

SEXP
is_nondominated_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT, SEXP MAXIMISE,
                  SEXP KEEP_WEAKLY)
{
    int nprotected = 0;
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NPOINT, npoint);
    SEXP_2_LOGICAL_VECTOR(MAXIMISE, maximise, maximise_len);
    SEXP_2_LOGICAL(KEEP_WEAKLY, keep_weakly);
    
    if (nobj != maximise_len)
        Rf_error("length of maximise (%d) is different from number of objectives (%d)",
                 maximise_len, nobj);

    signed char * minmax = create_minmax(nobj, maximise);
    bool * bool_is_nondom = nondom_init(npoint);
    double * data = REAL(DATA);

    if (keep_weakly) {
        find_weak_nondominated_set(data, nobj, npoint, minmax, bool_is_nondom);
    } else {
        find_nondominated_set(data, nobj, npoint, minmax, bool_is_nondom);
    }
    
    new_logical_vector (is_nondom, npoint);
    bool_2_logical_vector(is_nondom, bool_is_nondom, npoint);
    free (minmax);
    free (bool_is_nondom);
    UNPROTECT(nprotected);
    return Rexp_is_nondom;
}

SEXP
pareto_ranking_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT)
{
    int nprotected = 0;
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NPOINT, npoint);
    double * data = REAL(DATA);

    /* FIXME: How to assign directly? */
    new_int_vector (rank, npoint);
    int * rank2 = pareto_rank(data, nobj, npoint);
    for (int i = 0; i < npoint; i++) {
        rank[i] = rank2[i];
    }
    free (rank2);
    UNPROTECT(nprotected);
    return Rexp_rank;
}

