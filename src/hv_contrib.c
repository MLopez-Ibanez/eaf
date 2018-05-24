#include "hv.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static void swap_points(double *a, double *b, double *tmp, size_t dim)
{
    memcpy (tmp, b, sizeof(double) * dim);
    memcpy (b, a, sizeof(double) * dim);
    memcpy (a, tmp, sizeof(double) * dim);
}

/* Given a list of points, compute the hypervolume of each set that can be
   obtained by removing just one point. */
static double *
hv_contrib (double *hvc, double *points, int dim, int size, const double * ref,
            const bool * uev)
{
    bool keep_uevs = uev != NULL;

    if (hvc == NULL)
        hvc = malloc (sizeof(double) * size);

    double * tmp = malloc (sizeof(double) * dim);

    double * data = points + dim;
    hvc[0] = (keep_uevs && uev[0])
        ? 0.0
        : fpli_hv(data, dim, size - 1, ref);
    for (int i = 1; i < size; i++) {
        swap_points(points, &data[i * dim], tmp, dim);
        hvc[i] = (keep_uevs && uev[i])
            ? 0.0
            : fpli_hv(data, dim, size - 1, ref);
    }
    free(tmp);
    return hvc;
}

extern void
hv_contributions (double *hvc, double *points, int dim, int size, const double * ref);

void
hv_contributions (double *hvc, double *points, int dim, int size, const double * ref)
{
    double hv_total = fpli_hv(points, dim, size, ref);
    hv_contrib(hvc, points, dim, size, ref, NULL);
    for (int i = 0; i < size; i++)
        hvc[i] = hv_total - hvc[i];
}
