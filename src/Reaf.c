#include "Rcommon.h"
#include "eaf.h"

static eaf_t **
compute_eaf_helper (SEXP DATA, int nobj, SEXP CUMSIZES, int nruns, 
                    const double *percentile, int nlevels)
{
    int k;
    SEXP_2_INT_VECTOR(CUMSIZES, cumsizes, cumsizes_len);
    if (cumsizes_len < nruns)
        Rf_error("length of cumsizes (%d) is less than nruns (%d)",
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
            Rprintf ("eaf[%d] = %lu\n", k, (unsigned long) eaf[k]->size);
        });

    return eaf;
}

SEXP
compute_eaf_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS, SEXP PERCENTILE)
{
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NRUNS, nruns);
    SEXP_2_DOUBLE_VECTOR(PERCENTILE, percentile, nlevels);

    eaf_t **eaf = compute_eaf_helper(DATA, nobj, CUMSIZES, nruns, percentile, nlevels);
    int totalpoints = eaf_totalpoints (eaf, nlevels);

    SEXP mat;
    PROTECT(mat = Rf_allocMatrix(REALSXP, totalpoints, nobj + 1));
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

SEXP 
compute_eafdiff_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS,
                  SEXP INTERVALS)
{
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NRUNS, nruns);
    SEXP_2_INT(INTERVALS, intervals);
    int k;
    
    eaf_t **eaf = compute_eaf_helper(DATA, nobj, CUMSIZES, nruns, NULL, nruns);

    int nsets1 = nruns / 2;
    int nsets2 = nruns - nsets1;
    int totalpoints = eaf_totalpoints (eaf, nruns);

    SEXP mat;
    PROTECT(mat = Rf_allocMatrix(REALSXP, totalpoints, nobj + 1));
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
            /* bit_array_check(bit_array_offset(eaf[k]->bit_attained,i, eaf[k]->nruns), */
            /*                 eaf[k]->attained + i * eaf[k]->nruns, npoints, nruns); */
            attained_left_right (bit_array_offset(eaf[k]->bit_attained, i, eaf[k]->nruns),
                                 nsets1, nruns, &count_left, &count_right);
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

static int polygon_len(const double *src, int nobj)
{
    const double *src_orig = src;
    while (*src != objective_MIN)
        src += nobj;
    src += nobj;
    return (src - src_orig) / nobj;
}

static int polygon_copy(double *dest, int start, int nrows, const double *src)
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

SEXP compute_eafdiff_rectangles_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS,
                                  SEXP INTERVALS);
SEXP
compute_eafdiff_rectangles_C(SEXP DATA, SEXP NOBJ, SEXP CUMSIZES, SEXP NRUNS,
                             SEXP INTERVALS)
{
    SEXP_2_INT(NOBJ, nobj);
    SEXP_2_INT(NRUNS, nruns);
    SEXP_2_INT(INTERVALS, intervals);
    int nprotected = 0;
    int k;
    
    eaf_t **eaf = compute_eaf_helper(DATA, nobj, CUMSIZES, nruns, NULL, nruns);
    eaf_polygon_t * rects = eaf_compute_rectangles(eaf, nruns);
    for (k = 0; k < nruns; k++)
        eaf_delete (eaf[k]);
    free(eaf);

    const int division = nruns / 2;
    int nrow = vector_int_size(&rects->col);
    // Two points per row + color
    new_real_matrix (result, nrow, 2 * nobj + 1);
    double * p_xy = vector_objective_begin(&rects->xy);
    for (k = 0; k < nrow; ++k) {
        for (int i = 0; i < 2 * nobj; i++)
            result[k + nrow * i] = (double) *(p_xy++);
    }
    vector_objective_dtor (&rects->xy);

    for (k = 0; k < nrow; ++k) {
        double color = vector_int_at(&rects->col, k);
        // Each color is within [0, nruns / 2] or [-nruns / 2, 0]
        result[k + nrow * 2 * nobj] = intervals * color / (double) division;
    }
    // FIXME: This may return duplicated rows, remove them.
    vector_int_dtor (&rects->col);
    free(rects);

    const char* const colnames[] = {"xmin", "ymin", "xmax", "ymax", "diff"};
    set_colnames(Rexp(result), colnames, 5);
      
    UNPROTECT (nprotected);
    return Rexp(result);
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

    eaf_polygon_t *p = eaf_compute_area(eaf, nruns);

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
        // Truncate colors to interval
        int color = vector_int_at(&p->col, k) * intervals / (double) division;
        int len = polygon_len (p_xy, nobj);
        p_xy += len * nobj;
        DEBUG2(Rprintf ("color: %d, len = %d\n", color, len));
        // First interval (-1, 1) is white
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
            right_col[right_ncol++] = 1 - color;
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

SEXP
read_data_sets (SEXP FILENAME)
{
    SEXP_2_STRING(FILENAME, filename);
    /* Rprintf ("filename: %s\n", filename); */
    objective_t * data = NULL;
    int * cumsizes = NULL;
    int nobj = 0, nruns = 0;
    read_objective_t_data (filename, &data, &nobj, &cumsizes, &nruns);

    const int ntotal = cumsizes[nruns - 1];

    SEXP DATA;
    PROTECT(DATA = Rf_allocMatrix(REALSXP, cumsizes[nruns-1], nobj + 1));
    double *rdata = REAL(DATA);
    double_transpose (rdata, data, ntotal, nobj);

    int k, j;
    size_t pos = ntotal * nobj;
    for (k = 0, j = 0; k < ntotal; k++, pos++) {
        if (k == cumsizes[j]) j++;
        rdata[pos] = j + 1;
    }
    free(data);
    free(cumsizes);
    UNPROTECT (1);
    return DATA;
}
