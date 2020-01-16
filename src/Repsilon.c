#include "Rcommon.h"
#include "epsilon.h"
#include "igd.h"
#include "nondominated.h"


SEXP
epsilon_mul_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
              SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE);
SEXP
epsilon_add_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
              SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE);

enum unary_metric_t {
    EPSILON_ADD,
    EPSILON_MUL,
    INV_GD,
    INV_GDPLUS
}; 

static SEXP unary_metric_ref(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
                             SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE,
                             enum unary_metric_t metric)
{
    int nprotected = 0;
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NPOINT, npoint);
    double *ref = REAL(REFERENCE);
    SEXP_2_INT(REFERENCE_SIZE, ref_size);
    SEXP_2_LOGICAL_VECTOR(MAXIMISE, maximise, maximise_len);

    if (nobj != maximise_len)
        Rf_error("length of maximise (%d) is different from number of objectives (%d)",
                 maximise_len, nobj);

    signed char * minmax = create_minmax(nobj, maximise);

    new_real_vector(value, 1);
    double *data = REAL(DATA);
    
    switch(metric) {
      case EPSILON_ADD:
          value[0] = epsilon_additive (nobj, minmax, data, npoint, ref, ref_size);
          break;
      case EPSILON_MUL:
          value[0] = epsilon_mult (nobj, minmax, data, npoint, ref, ref_size);
          break;
      case INV_GD:
          value[0] = IGD (nobj, minmax, data, npoint, ref, ref_size);
          break;
      case INV_GDPLUS:
          value[0] = IGD_plus (nobj, minmax, data, npoint, ref, ref_size);
          break;
      default:
          Rf_error("unknown unary metric");
    }
    
    free (minmax);
    UNPROTECT (nprotected);
    return Rexp(value);
}

SEXP
epsilon_mul_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
              SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE)
{
    return(unary_metric_ref(DATA, NOBJ, NPOINT,
                            REFERENCE, REFERENCE_SIZE, MAXIMISE,
                            EPSILON_MUL));
}

SEXP
epsilon_add_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
              SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE)
{
    return(unary_metric_ref(DATA, NOBJ, NPOINT,
                            REFERENCE, REFERENCE_SIZE, MAXIMISE,
                            EPSILON_ADD));
}

SEXP
igd_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
      SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE)
{
    return(unary_metric_ref(DATA, NOBJ, NPOINT,
                            REFERENCE, REFERENCE_SIZE, MAXIMISE,
                            INV_GD));
}

SEXP
igd_plus_C(SEXP DATA, SEXP NOBJ, SEXP NPOINT,
          SEXP REFERENCE, SEXP REFERENCE_SIZE, SEXP MAXIMISE)
{
    return(unary_metric_ref(DATA, NOBJ, NPOINT,
                            REFERENCE, REFERENCE_SIZE, MAXIMISE,
                            INV_GDPLUS));
}
