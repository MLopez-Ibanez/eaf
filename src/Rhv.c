#include "Rcommon.h"
#include "hv.h"

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

#include "whv.h"

SEXP
rect_weighted_hv2d_C(SEXP DATA, SEXP NPOINTS, SEXP RECTANGLES, SEXP RECTANGLES_NROW)
{
    int nprotected = 0;
    // It cannot be const because we need to sort it.
    double *data = REAL(DATA);
    SEXP_2_INT(NPOINTS, npoints);
    // It cannot be const because we need to sort it.
    double *rectangles = REAL(RECTANGLES);
    SEXP_2_INT(RECTANGLES_NROW, rectangles_nrow);
    
    new_real_vector(hv, 1);
    hv[0] = rect_weighted_hv2d(data, npoints, rectangles, rectangles_nrow); 
    UNPROTECT (nprotected);
    return Rexp(hv);
}

#include "whv_hype.h"

hype_sample_dist *
Sexp_to_dist(SEXP DIST, SEXP SEED)
{
    int nprotected = 0;
    hype_sample_dist * dist = NULL;
    SEXP_2_INT(SEED, seed);

    const char * dist_type = CHAR(STRING_ELT(VECTOR_ELT(DIST, 0), 0));
    if (0 == strcmp(dist_type, "uniform")) {
        dist = hype_dist_unif_new(seed);
    } else if (0 == strcmp(dist_type, "exponential")) {
        const double * mu = REAL(VECTOR_ELT(DIST, 1));
        dist = hype_dist_exp_new(mu[0], seed);
    } else if (0 == strcmp(dist_type, "point")) {
        const double * mu = REAL(VECTOR_ELT(DIST, 1));
        dist = hype_dist_gaussian_new(mu, seed);
    } else {
        Rf_error("unknown dist_type: %s", dist_type);
    }
    UNPROTECT (nprotected);
    return dist;
}

SEXP
whv_hype_C(SEXP DATA, SEXP NPOINTS, SEXP IDEAL, SEXP REFERENCE,
           SEXP DIST, SEXP SEED, SEXP NSAMPLES)
{
    int nprotected = 0;
    // It cannot be const because we need to sort it.
    double *data = REAL(DATA);
    SEXP_2_INT(NPOINTS, npoints);
    SEXP_2_INT(NSAMPLES, nsamples);
    SEXP_2_DOUBLE_VECTOR(REFERENCE, reference, reference_len);
    SEXP_2_DOUBLE_VECTOR(IDEAL, ideal, ideal_len);
    eaf_assert(reference_len == ideal_len);
    eaf_assert(reference_len == 2);
    hype_sample_dist * dist = Sexp_to_dist(DIST, SEED);
    new_real_vector(hv, 1);
    if (!dist) {
        Rf_error("Sexp_to_dist failed to create dist");
    } else {
        hv[0] = whv_hype_estimate(data, npoints, ideal, reference, dist, nsamples);
        hype_dist_free(dist);
    }
    UNPROTECT (nprotected);
    return Rexp(hv);
}
