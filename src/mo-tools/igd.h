#ifndef IGD_H
#define IGD_H

/*************************************

 GD was first proposed in [1] with p=2. IGD seems to have been mentioned first
 in [2], however, some people also used the name D-metric for the same thing
 with p=1 and later papers have often used IGD/GD with p=1.  GD_p and IGD_p
 were proposed in [4] and they change how the numerator (psize) is
 computed. This has a significant effect for GD and less so for IGD given a
 constant reference set. IGD+ was proposed in [5] and changes how to compute
 the distances. In general, norm=2 (Euclidean distance), but other norms are
 possible [4]. See [6] for a comparison.

 [1] D. A. Van Veldhuizen and G. B. Lamont. Evolutionary Computation and
     Convergence to a Pareto Front. In J. R. Koza, editor, Late Breaking Papers
     at the Genetic Programming 1998 Conference, pages 221–228, Stanford
     University, California, July 1998. Stanford University Bookstore.
     Keywords: generational distance.

 [2] Coello Coello, C.A., Reyes-Sierra, M.: A study of the parallelization of a
     coevolutionary multi-objective evolutionary algorithm.  In: Monroy, R., et
     al. (eds.) Proceedings of MICAI, LNAI, vol. 2972, pp. 688–697. Springer,
     Heidelberg, Germany (2004).

 [3] Q. Zhang and H. Li. MOEA/D: A Multiobjective Evolutionary Algorithm Based
     on Decomposition. IEEE Transactions on Evolutionary Computation,
     11(6):712–731, 2007. doi:10.1109/TEVC.2007.892759.

 [4] Schutze, O., Esquivel, X., Lara, A., Coello Coello, C.A.: Using the
     averaged Hausdorff distance as a performance measure in evolutionary
     multiobjective optimization. IEEE Trans. Evol. Comput. 16(4), 504–522 (2012)

 [5] H. Ishibuchi, H. Masuda, Y. Tanigaki, and Y. Nojima.  Modified Distance
     Calculation in Generational Distance and Inverted Generational Distance.
     In A. Gaspar-Cunha, C. H. Antunes, and C. A. Coello Coello, editors,
     Evolutionary Multi-criterion Optimization, EMO 2015 Part I, volume 9018 of
     Lecture Notes in Computer Science, pages 110–125. Springer, Heidelberg,
     Germany, 2015.

 [6] Leonardo C. T. Bezerra, Manuel López-Ibáñez, and Thomas Stützle. An
     empirical assessment of the properties of inverted generational distance
     indicators on multi- and many-objective optimization. In H. Trautmann,
     G. Rudolph, K. Klamroth, O. Schütze, M. M. Wiecek, Y. Jin, and C. Grimme,
     editors, Evolutionary Multi-criterion Optimization, EMO 2017, Lecture
     Notes in Computer Science, pages 31–45. Springer, 2017.

*/

#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#ifndef INFINITY
#define INFINITY (HUGE_VAL)
#endif
#include "common.h"
#include "io.h"


static inline double
norm_distance (double a, double r, unsigned int p)
{
    return powl (fabs(r - a), p);
}

static inline double
plus_distance (double a, double r, unsigned int p)
{
    return powl (MAX(r - a, 0.0), p);
}

static inline double 
gd_common (int dim, const signed char *minmax,
           const double *points_a, int size_a,
           const double *points_r, int size_r,
           bool plus, bool psize, unsigned int p, unsigned int norm)
{
    int a, r, d;
    double gd = 0;
    for (a = 0; a < size_a; a++) {
        double min_dist = INFINITY;
        for (r = 0; r < size_r; r++) {
            double dist = 0.0;
            for (d = 0; d < dim; d++) {
                if (minmax[d] == 0) continue;
                double a_d = points_a[a * dim + d];
                double r_d = points_r[r * dim + d];
                if (!plus)
                    dist += norm_distance (a_d, r_d, norm);
                else if (minmax[d] < 0)
                    dist += plus_distance (a_d, r_d, norm);
                else /*(minmax[d] < 0)*/
                    dist += plus_distance (r_d, a_d, norm);
            }
            if (dist < min_dist) min_dist = dist;
        }
        min_dist = powl (min_dist, 1.0 / norm);
        gd += powl (min_dist, p);
    }
    
    if (psize)
        return powl (gd / (double) size_a, 1.0 / p);
    else 
        return powl (gd, 1.0 / p) / (double) size_a;
}

static inline double
GD (int dim, const signed char *minmax,
    const double *points_a, int size_a,
    const double *points_r, int size_r)
{
    return gd_common (dim, minmax,
                      points_a, size_a,
                      points_r, size_r,
                      /*plus=*/false, /*psize=*/false,
                      /*p=*/1, /*norm=*/2);
}

static inline double
IGD (int dim, const signed char *minmax,
     const double *points_a, int size_a,
     const double *points_r, int size_r)
{
    return gd_common (dim, minmax,
                      points_r, size_r,
                      points_a, size_a,
                      /*plus=*/false, /*psize=*/false,
                      /*p=*/1, /*norm=*/2);
}

static inline double
GD_p (int dim, const signed char *minmax,
      const double *points_a, int size_a,
      const double *points_r, int size_r, unsigned int p)
{
    return gd_common (dim, minmax,
                      points_a, size_a,
                      points_r, size_r,
                      /*plus=*/false, /*psize=*/true,
                      p, /*norm=*/2);
}

static inline double
IGD_p (int dim, const signed char *minmax,
       const double *points_a, int size_a,
       const double *points_r, int size_r, unsigned int p)
{
    return gd_common (dim, minmax,
                      points_r, size_r,
                      points_a, size_a,
                      /*plus=*/false, /*psize=*/true,
                      p, /*norm=*/2);
}

static inline double
IGD_plus (int dim, const signed char *minmax,
          const double *points_a, int size_a,
          const double *points_r, int size_r)
{
    return gd_common (dim, minmax,
                      points_r, size_r,
                      points_a, size_a,
                      /*plus=*/true, /*psize=*/true,
                      /*p=*/1, /*norm=*/2);

}

static inline double
avg_Hausdorff_dist (int dim, const signed char *minmax,
                    const double *points_a, int size_a,
                    const double *points_r, int size_r, unsigned int p)
{
    double gd_p = gd_common (dim, minmax,
                             points_a, size_a,
                             points_r, size_r,
                             /*plus=*/false, /*psize=*/true,
                             p, /*norm=*/2);

    double igd_p = gd_common (dim, minmax,
                              points_r, size_r,
                              points_a, size_a,
                              /*plus=*/false, /*psize=*/true,
                              p, /*norm=*/2);
    return MAX (gd_p, igd_p);
}
/* TODO: Implement p=INFINITY See [4] */

#endif /* IGD_H */
