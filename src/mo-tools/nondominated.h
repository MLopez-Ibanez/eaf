#ifndef NONDOMINATED_H
#define NONDOMINATED_H

#include "common.h"
#include <string.h> // memcpy

enum objs_agree_t { AGREE_MINIMISE = -1, AGREE_NONE = 0, AGREE_MAXIMISE = 1 };

/* Convert from int vector to minmax vector.  */
static inline signed char *
create_minmax(int nobj, const int * maximise)
{
    signed char * minmax = malloc(sizeof(signed char) * nobj);
    for (int k = 0; k < nobj; k++) {
        minmax[k] = (maximise[k] == TRUE)
            ? AGREE_MAXIMISE
            : (maximise[k] == FALSE) ? AGREE_MINIMISE : AGREE_NONE;
    }
    return minmax;
}

static inline bool *
nondom_init (size_t size)
{
    bool * nondom = malloc (sizeof(bool) * size);
    for (size_t n = 0; n <  size; n++)
        nondom[n] = true;
    return nondom;
}

/* When find_dominated_p == true, then stop as soon as one dominated point is
   found and return its position.

   When find_dominated_p == true, store which points are nondominated in nondom
   and return the number of nondominated points.

*/
static inline int
find_nondominated_set_ (const double *points, int dim, int size,
                        const signed char *minmax, const signed char agree,
                        bool *nondom, bool find_dominated_p, bool keep_weakly)
{
    int j, k, d;
    
    for (k = 0; k < size - 1; k++) {
        for (j = k + 1; j < size; j++) {

            if (!nondom[k]) break;
            if (!nondom[j]) continue;

            bool k_leq_j = true;
            bool j_leq_k = true;

            const double *pk = points + k * dim;
            const double *pj = points + j * dim;

            /* FIXME: As soon as j_leq_k and k_leq_j become false,
               neither k or j will be removed, so break.  */
            /* FIXME: Do not handle agree here, assume that objectives
               have been fixed already to agree on
               minimization/maximization.  */
            if (agree < 0) {
                for (d = 0; d < dim; d++) {
                    j_leq_k = j_leq_k && (pj[d] <= pk[d]);
                    k_leq_j = k_leq_j && (pk[d] <= pj[d]);
                }
            } else if (agree > 0) {
                for (d = 0; d < dim; d++) {
                    j_leq_k = j_leq_k && (pj[d] >= pk[d]);
                    k_leq_j = k_leq_j && (pk[d] >= pj[d]);
                }
            } else {
                for (d = 0; d < dim; d++) {
                    if (minmax[d] < 0) {
                        j_leq_k = j_leq_k && (pj[d] <= pk[d]);
                        k_leq_j = k_leq_j && (pk[d] <= pj[d]);
                    } else if (minmax[d] > 0) {
                        j_leq_k = j_leq_k && (pj[d] >= pk[d]);
                        k_leq_j = k_leq_j && (pk[d] >= pj[d]);
                    }
                }
            }

            // k is removed if it is weakly dominated by j (unless remove_weakly).
            nondom[k] = !j_leq_k || (keep_weakly && k_leq_j);
            // j is removed if it is dominated by k.
            nondom[j] = (!k_leq_j || j_leq_k);
            
            eaf_assert(nondom[k] || nondom[j]); /* both cannot be removed.  */

            if (find_dominated_p && (!nondom[k] || !nondom[j])) {
                return nondom[k] ? j : k;
            }
        }
    }

    if (find_dominated_p) return -1;

    int new_size = 0;
    for (k = 0; k < size; k++)
        if (nondom[k]) new_size++;
    return new_size;
}

static inline int
find_dominated_point (const double *points, int dim, int size,
                      const signed char *minmax)
{
    bool *nondom = nondom_init (size);

    int pos = find_nondominated_set_ (points, dim, size, minmax,
                                      AGREE_NONE, nondom, 
                                      /* find_dominated_p = */true,
                                      /* keep_weakly = */false);
    free (nondom);
    return pos;
}

static inline int
find_nondominated_set_agree (const double *points, int dim, int size,
                             const signed char *minmax, const signed char agree,
                             bool *nondom)
{
    return find_nondominated_set_ (points, dim, size, minmax, agree, nondom, 
                                   /* find_dominated_p = */false,
                                   /* keep_weakly = */false);
}

static inline int
find_nondominated_set (const double *points, int dim, int size,
                       const signed char *minmax, bool *nondom)
{
    return find_nondominated_set_ (points, dim, size, minmax, AGREE_NONE, nondom,
                                   /* find_dominated_p = */false,
                                   /* keep_weakly = */false);
}

static inline int
find_weak_nondominated_set (const double *points, int dim, int size,
                            const signed char *minmax, bool *nondom)
{
    return find_nondominated_set_ (points, dim, size, minmax, AGREE_NONE, nondom,
                                   /* find_dominated_p = */false,
                                   /* keep_weakly = */true);
}

static inline int
get_nondominated_set (double **pareto_set_p,
                      const double *points, int dim, int size,
                      const signed char *minmax)
{
    bool *nondom  = nondom_init(size);
    int new_size = find_nondominated_set (points, dim, size, minmax, nondom);

    DEBUG2 (
        fprintf (stderr, "# size\tnondom\tdom\n");
        fprintf (stderr, "  %d\t%d\t%d\n",
                 size, new_size, size - new_size);
        );

    if (new_size > size) {/* This can't happen.  */
        fatal_error ("%s:%d: a bug happened: new_size > old_size!\n",
                     __FILE__, __LINE__);
    }

    double *pareto_set = malloc (sizeof (double) * new_size * dim);
    int n, k;
    // FIXME: We could use new_size to stop earlier.
    for (n = 0, k = 0; n < size; n++) {
        if (!nondom[n]) continue;
        memcpy(&pareto_set[dim * k], &points[dim * n], sizeof(points[0]) * dim);
        k++;
    }
    eaf_assert (k == new_size);
    free (nondom);
    *pareto_set_p = pareto_set;
    return new_size;
}

static inline void
agree_objectives (double *points, int dim, int size,
                  const signed char *minmax, const signed char agree)
{
    for (int d = 0; d < dim; d++)
        if ((agree > 0 && minmax[d] < 0)
            || (agree < 0 && minmax[d] > 0))
            for (int k = 0; k < size; k++)
                points[k * dim + d] = -(points[k * dim + d]);
}


static inline void
normalise (double *points, int dim, int size,
           const signed char *minmax, signed char agree,
           const double lower_range, const double upper_range,
           const double *lbound, const double *ubound)
{
    int k, d;
    const double range = upper_range - lower_range;
    
    double *diff = malloc (dim * sizeof(double));
    for (d = 0; d < dim; d++) {
        diff[d] = ubound[d] - lbound[d];
        if (diff[d] == 0.0) // FIXME: Should we use approximate equality?
            diff[d] = 1; // FIXME: Do we need to handle agree?
    }

    for (k = 0; k < size; k++) {
        double *p = points + k * dim;
        for (d = 0; d < dim; d++)
            if ((agree > 0 && minmax[d] < 0)
                || (agree < 0 && minmax[d] > 0))
                p[d] = lower_range + range * (ubound[d] + p[d]) / diff[d];
            else
                p[d] = lower_range + range * (p[d] - lbound[d]) / diff[d];
    }

    free (diff);
}

int * pareto_rank (const double *points, int dim, int size);


#endif /* NONDOMINATED_H */
