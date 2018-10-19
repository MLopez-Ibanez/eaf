#include "hv.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <float.h>

/* It does not actually compute the contribution but HV_total - HV_i,
   where HV_total is the total HV and HV_i is the contribution of the
   point, that is, it actually computes the HV minus the point i. */
// FIXME: Old implementation: Remove after some testing
static double *
hv_contrib2 (const double *points, int dim, int size, const double * ref,
            const bool * uev)
{
    bool keep_uevs = uev != NULL;

    double * hv = malloc (sizeof(double) * size);

    // FIXME: Avoid duplicating the points! Play tricks with pointers?
    double * data = malloc (sizeof(double) * (size + 1) * dim);

    // FIXME: Avoid so many memcpy, remove points from the top and add
    // them to the end.
    for (int i = 0; i < size; i++) {
        int pos = 0;

        // FIXME: Check which points are dominated. Those contribute zero, so
        // we should return fpli_hv for total.
        for (int j = 0; j < size; j++) {
            const double * pointj = &points[j * dim];
            if (i == j) continue;
            memcpy (data + pos, pointj, sizeof(double) * dim);
            pos += dim;    
        }

        if (keep_uevs && uev[i]) {
            //assert (pointi[0] == ubound[0] || pointi[1] == ubound[1]);
            hv[i] = 0.0;
        } else 
            hv[i] = fpli_hv(data, dim, size - 1, ref);
    }
    free (data);
    return hv;
}

/* FIXME: Better than swapping, remove points by making them equal to the reference point. */
static void swap_points(double *a, double *b, double *tmp, size_t dim)
{
    memcpy (tmp, b, sizeof(double) * dim);
    memcpy (b, a, sizeof(double) * dim);
    memcpy (a, tmp, sizeof(double) * dim);
}

/* Given a list of points, compute the hypervolume of each set that can be
   obtained by removing just one point. */
double *
hv_contrib (double *hvc, double *points, int dim, int size, const double * ref,
            const bool * uev)
{
    bool keep_uevs = uev != NULL;

    if (hvc == NULL)
        hvc = malloc (sizeof(double) * size);

    double * tmp = malloc (sizeof(double) * dim);

    double * subset = points + dim;
    hvc[0] = (keep_uevs && uev[0])
        ? 0.0
        : fpli_hv(subset, dim, size - 1, ref);
    for (int i = 1; i < size; i++) {
        // data 
        swap_points(points, points + i * dim, tmp, dim);
        hvc[i] = (keep_uevs && uev[i])
            ? 0.0
            : fpli_hv(subset, dim, size - 1, ref);
    }
    free(tmp);
    return hvc;
}

void
hv_contributions (double *hvc, double *points, int dim, int size, const double * ref)
{
    const double tolerance = sqrt(DBL_EPSILON);

    double hv_total = fpli_hv(points, dim, size, ref);
    // FIXME: Find which points are dominated and simply return 0 for those.
    hv_contrib(hvc, points, dim, size, ref, NULL);
    double * hvc2 = hv_contrib2(points, dim, size, ref, NULL);
    for (int i = 0; i < size; i++) {
        assert(hvc[i] == hvc2[i]);
        hvc[i] = hv_total - hvc[i];
        // Handle very small values.
        hvc[i] = fabs(hvc[i]) >= tolerance ? hvc[i] : 0.0;
        assert(hvc[i] >= 0);
    }
}

