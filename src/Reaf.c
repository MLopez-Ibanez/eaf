#include <R.h>
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <R_ext/Memory.h>
#include "eaf.h"

#ifndef DEBUG
#define DEBUG 0
#endif

#if DEBUG >= 1
#define DEBUG1(X) X;(void)0
#else  
#define DEBUG1(X) (void)0
#endif

#if DEBUG >= 2
#define DEBUG2(X) X;(void)0
#else  
#define DEBUG2(X) (void)0
#endif

#define CHECK_ARG_IS_INT_VECTOR(A)					\
    if (!isInteger(A) || !isVector(A))					\
	error("Argument '" #A "' is not an integer vector");

/* The C API of R is awfully ugly and unpractical (and poorly
   documented). These wrappers make it a little more bearable. */

#define Rexp(VAR) Rexp_##VAR

#define new_real_matrix(DOUBLEVAR, DIM1, DIM2)                                 \
    SEXP Rexp_##DOUBLEVAR; double *DOUBLEVAR;                                  \
    PROTECT(Rexp_##DOUBLEVAR = allocMatrix(REALSXP, (DIM1), (DIM2)));          \
    nprotected++; DOUBLEVAR = REAL(Rexp_##DOUBLEVAR)

#define new_real_vector(DOUBLEVAR, DIM)                                        \
    SEXP Rexp_##DOUBLEVAR; double *DOUBLEVAR;                                  \
    PROTECT(Rexp_##DOUBLEVAR = allocVector(REALSXP, (DIM)));                   \
    nprotected++; DOUBLEVAR = REAL(Rexp_##DOUBLEVAR)

#define new_string_vector(VAR, DIM)                                            \
    SEXP Rexp_##VAR; int Rexp_##VAR##_len = 0;                                 \
    PROTECT(Rexp_##VAR = allocVector(STRSXP, (DIM)));                          \
    nprotected++

#define string_vector_push_back(VAR, ELEMENT)                                  \
    SET_STRING_ELT(Rexp_##VAR, Rexp_##VAR##_len, mkChar(ELEMENT));             \
    Rexp_##VAR##_len++

#define new_list(LISTVAR, LENGTH)                                              \
    SEXP Rexp_##LISTVAR; int Rexp_##LISTVAR##_len = 0;                         \
    PROTECT(Rexp_##LISTVAR = allocVector(VECSXP, (LENGTH)));                   \
    ++nprotected

#define list_len(VAR) Rexp_##VAR##_len

#define list_push_back(LISTVAR, ELEMENT)                                       \
    SET_VECTOR_ELT(Rexp_##LISTVAR, Rexp_##LISTVAR##_len, Rexp_##ELEMENT);      \
    Rexp_##LISTVAR##_len++

#define set_names(VAR, NAMES)                                                  \
    setAttrib(Rexp_##VAR, R_NamesSymbol, Rexp_##NAMES)

#define set_attribute(VAR, ATTRIBUTE, VALUE)                                   \
    setAttrib(Rexp_##VAR, install(ATTRIBUTE), Rexp_##VALUE)

/*
 * Unpack an integer vector stored in SEXP S.
 */
#define SEXP_2_INT_VECTOR(S, I, N)               \
    CHECK_ARG_IS_INT_VECTOR(S);                  \
    int *I = INTEGER(S);                         \
    const R_len_t N = length(S);

#define SEXP_2_INT(S,var)                                               \
    int var = asInteger(S);                                             \
    if (var == NA_INTEGER)                                              \
        error ("Argument '" #S "' is not an integer");

#define SEXP_2_STRING(S,var)                                            \
    if (!isString(S) || length(S) != 1)                                 \
        error ("Argument '" #S "' is not a string");                    \
    const char * var = CHAR(STRING_ELT(S,0));


static eaf_t **
compute_eaf_helper (SEXP DATA, int nobj, SEXP CUMSIZES, int nruns, 
                    const int *percentile, int nlevels)
{
    int k;
    SEXP_2_INT_VECTOR(CUMSIZES, cumsizes, cumsizes_len);
    if (cumsizes_len < nruns)
        error("length of cumsizes (%d) is less than nruns (%d)",
              cumsizes_len, nruns);

    int *level;

    if (percentile != NULL) {
        level = malloc(sizeof(int) * nlevels);
        for (k = 0; k < nlevels; k++)
            level[k] = percentile2level(percentile[k], nruns);
    } else {
        eaf_assert (nlevels == nruns);
        level = malloc(sizeof(int) * nruns);
        for (k = 0; k < nruns; k++)
            level[k] = k + 1;
    }

    double *data = REAL(DATA);

    DEBUG2(
        Rprintf ("attsurf ({(%f, %f", data[0], data[1]);
        for (k = 2; k < nobj; k++) {
            Rprintf (", %f", data[k]);
        }
        Rprintf (")...}, %d, { %d", nobj, cumsizes[0]);
        for (k = 1; k < nruns; k++) {
            Rprintf (", %d", cumsizes[k]);
        }
        Rprintf ("}, %d, { %d", nruns, level[0]);
        for (k = 1; k < nlevels; k++) {
            Rprintf (", %d", level[k]);
        }
        Rprintf ("}, %d)\n", nlevels);
        );

    eaf_t **eaf = attsurf (data, nobj, cumsizes, nruns, level, nlevels);
    free (level);

    DEBUG2(
        Rprintf ("eaf computed\n");
        for (k = 0; k < nlevels; k++) {
            Rprintf ("eaf[%d] = %d\n", k, eaf[k]->size);
        });

    return eaf;
}

extern SEXP compute_eaf_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS,
                          SEXP PERCENTILES);

SEXP
compute_eaf_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS, SEXP PERCENTILE)
{
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NRUNS, nruns);
    SEXP_2_INT_VECTOR(PERCENTILE, percentile, nlevels);

    eaf_t **eaf = compute_eaf_helper(DATA, nobj, CUMSIZES, nruns, percentile, nlevels);

    int totalpoints = eaf_totalpoints (eaf, nlevels);

    SEXP mat;
    PROTECT(mat = allocMatrix(REALSXP, totalpoints, nobj + 1));
    double * rmat = REAL(mat);

    int pos = 0;
    int k;
    for (k = 0; k < nlevels; k++) {
        int npoints = eaf[k]->size;

        DEBUG2(
            int totalsize = npoints * nobj;
            Rprintf ("totalpoints eaf[%d] = %d\n", k, totalsize)
            );

        int i;
        for (i = 0; i < npoints; i++) {
            int j;
            for (j = 0; j < nobj; j++) {
                rmat[pos + j * totalpoints] = eaf[k]->data[j + i * nobj];
            }
            rmat[pos + nobj * totalpoints] = percentile[k];
            pos++;
        }
        eaf_delete (eaf[k]);
    }
    free(eaf);
    UNPROTECT (1);
    return mat;
}


SEXP compute_eafdiff_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS,
                       SEXP INTERVALS);

SEXP 
compute_eafdiff_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS,
                  SEXP INTERVALS)
{
    int k;
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NRUNS, nruns);
    SEXP_2_INT(INTERVALS, intervals);

    eaf_t **eaf = compute_eaf_helper(DATA, nobj, CUMSIZES, nruns, NULL, nruns);

    int nsets1 = nruns / 2;
    int nsets2 = nruns - nsets1;
    int totalpoints = eaf_totalpoints (eaf, nruns);

    SEXP mat;
    PROTECT(mat = allocMatrix(REALSXP, totalpoints, nobj + 1));
    double *rmat = REAL(mat);

    int pos = 0;
    for (k = 0; k < nruns; k++) {
        int npoints = eaf[k]->size;
        int i, j;

        DEBUG2(
            int totalsize = npoints * nobj;
            Rprintf ("totalpoints eaf[%d] = %d\n", k, totalsize)
            );

        // FIXME: Find the most efficient order of the loop.
        for (i = 0; i < npoints; i++) {
            for (j = 0; j < nobj; j++) {
                rmat[pos + j * totalpoints] = eaf[k]->data[j + i * nobj];
            }
            pos++;
        }
    }
    pos += (nobj - 1) * totalpoints;
    for (k = 0; k < nruns; k++) {
        int i;
        int npoints = eaf[k]->size;
        for (i = 0; i < npoints; i++) {
            int count_left;
            int count_right;
            attained_left_right (eaf[k]->attained + i * eaf[k]->nruns,
                                 nruns / 2, nruns, &count_left, &count_right);
            rmat[pos] = intervals * (double) ((count_left / (double) nsets1) - 
                                              (count_right / (double) nsets2));
            pos++;
        }
        eaf_delete (eaf[k]);
    }
    free(eaf);
    UNPROTECT (1);
    return mat;
}

static int polygon_len  (const double *src, int nobj)
{
    const double *src_orig = src;
    while (*src != objective_MIN)
        src += nobj;
    src += nobj;
    return (src - src_orig) / nobj;
}

static int polygon_copy (double *dest, int start, int nrows, const double *src)
{
    int len = start;
    while (*src != objective_MIN) {
        dest[len] = *src;
        dest[len + nrows] = *(src + 1);
        len++;
        src += 2;
    }
    dest[len] = NA_REAL;
    dest[len + nrows] = NA_REAL;
    len++;
    return len - start;
}

SEXP compute_eafdiff_area_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS,
                            SEXP INTERVALS);
SEXP 
compute_eafdiff_area_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS,
                       SEXP INTERVALS)
{
    int nprotected = 0;

    int k;
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NRUNS, nruns);
    SEXP_2_INT(INTERVALS, intervals);

    eaf_t **eaf = compute_eaf_helper(DATA, nobj, CUMSIZES, nruns, NULL, nruns);

    eaf_polygon_t *p = eaf_compute_area (eaf, nruns);

    for (k = 0; k < nruns; k++)
        eaf_delete (eaf[k]);
    free(eaf);

    const int division = nruns / 2;

    int ncol = vector_int_size(&p->col);

    DEBUG2(Rprintf ("ncol: %d\n", ncol));

    int left_ncol = 0, right_ncol = 0;
    int left_len = 0, right_len = 0;

    /* First compute the adjusted colors, and how much space we need
       on each side. */
    double * p_xy = vector_objective_begin(&p->xy);
    for (k = 0; k < ncol; k++) {
        double color = vector_int_at(&p->col, k);
        color = intervals * color / (double) division;
        int len = polygon_len (p_xy, nobj);
        p_xy += len * nobj;
        DEBUG2(Rprintf ("color: %d, len = %d\n", color, len));
        if (color >= 1) {
            left_len += len;
            left_ncol++;
        } else if (color <= 1) {
            right_len += len;
            right_ncol++;
        }
        vector_int_set(&p->col, k, color);
    }

    DEBUG2(Rprintf ("left_len: %d, right_len: %d, left_ncol: %d, right_ncol: %d\n", 
                    left_len, right_len, left_ncol, right_ncol));

    /* Now assign points to each side. */
    new_real_vector(left_col, left_ncol);
    new_real_vector(right_col, right_ncol);

    const int left_npoints = left_len;
    new_real_matrix (left, left_npoints, nobj);
    
    const int right_npoints = right_len;
    new_real_matrix (right, right_npoints, nobj);

    p_xy = vector_objective_begin(&p->xy);
    left_len = right_len = 0;
    left_ncol = right_ncol = 0;
    for (k = 0; k < ncol; k++) {
        int len;
        int color = vector_int_at(&p->col, k);
        if (color >= 1) {
            len = polygon_copy (left, left_len, left_npoints, p_xy);
            left_len += len;
            left_col[left_ncol++] = color + 1;
        } else if (color <= 1) {
            len = polygon_copy (right, right_len, right_npoints, p_xy);
            right_len += len;
            right_col[right_ncol++] = (-color) + 1;
        } else {
            len = polygon_len (p_xy, nobj);
        }
        p_xy += nobj * len;
    }
    vector_int_dtor (&p->col);
    vector_objective_dtor (&p->xy);
    free(p);

    set_attribute(left, "col", left_col);
    set_attribute(right, "col", right_col);

    new_list(poly, 2);

    list_push_back (poly, left);
    list_push_back (poly, right);

    new_string_vector (names, list_len (poly));
    string_vector_push_back (names, "left");
    string_vector_push_back (names, "right");
    set_names (poly, names);

    UNPROTECT (nprotected);
    return Rexp(poly);
}

SEXP read_data_sets (SEXP FILENAME);

SEXP
read_data_sets (SEXP FILENAME)
{
    SEXP_2_STRING(FILENAME, filename);
    objective_t *data = NULL;
    int* cumsizes = NULL;
    int nobj = 0, nruns = 0;

    /* Rprintf ("filename: %s\n", filename); */

    read_objective_t_data (filename, &data, &nobj, &cumsizes, &nruns);

    const int ntotal = cumsizes[nruns - 1];
    int * runtab = malloc (ntotal * sizeof(int));
    int k, j, i;
    for (k = 0, j = 0; k < ntotal; k++) {
        if (k == cumsizes[j])
            j++;
        runtab[k] = j + 1;
    }

    SEXP DATA;
    PROTECT(DATA = allocMatrix(REALSXP, cumsizes[nruns-1], nobj + 1));
    double *rdata = REAL(DATA);
    int pos = 0;
    for (j = 0; j < nobj; j++) {
        for (i = 0; i < ntotal; i++) {
            rdata[pos] = data[j + i * nobj];
            pos++;
        }
    }
    for (j = 0; j < ntotal; j++, pos++) {
        rdata[pos] = runtab[j];
    }
    free(data);
    free(cumsizes);
    free(runtab);
    UNPROTECT (1);
    return DATA;
}
