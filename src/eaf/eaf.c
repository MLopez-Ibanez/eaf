/*************************************************************************

 eaf: Computes the empirical attainment function (EAF) from a number
 of approximation sets.

 ---------------------------------------------------------------------

    Copyright (c) 2005
            Carlos M. Fonseca <cmfonsec@ualg.pt>
    Copyright (c) 2006, 2007, 2008, 2015
            Carlos M. Fonseca <cmfonsec@dei.uc.pt>
            Manuel Lopez-Ibanez <manuel.lopez-ibanez@manchester.ac.uk>

 This program is free software (software libre); you can redistribute
 it and/or modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, you can obtain a copy of the GNU
 General Public License at:
                 http://www.gnu.org/copyleft/gpl.html
 or by writing to:
           Free Software Foundation, Inc., 59 Temple Place,
                 Suite 330, Boston, MA 02111-1307 USA

 ----------------------------------------------------------------------


*************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include "eaf.h"

#ifndef DEBUG
#define DEBUG 0
#endif

static int compare_x_asc (const void *p1, const void *p2)
{
    objective_t x1 = **(objective_t **)p1;
    objective_t x2 = **(objective_t **)p2;
	
    return (x1 < x2) ? -1 : ((x1 > x2) ? 1 : 0);
}

static int compare_y_desc (const void *p1, const void *p2)
{
    objective_t y1 = *(*(objective_t **)p1+1);
    objective_t y2 = *(*(objective_t **)p2+1);
	
    return (y1 > y2) ? -1 : ((y1 < y2) ? 1 : 0);
}

eaf_t * eaf_create (int nobj, int nruns, int npoints)
{
    eaf_t *eaf;
    EAF_MALLOC (eaf, 1, sizeof(eaf_t));
    eaf->nobj = nobj;
    eaf->nruns = nruns;
    eaf->size = 0;
    /* Maximum is npoints, but normally it will be smaller, so at most
       log2(2 * nruns) realloc will occur.  */
    eaf->maxsize = 256 + npoints / (2 * nruns); 

    EAF_MALLOC (eaf->data, nobj * eaf->maxsize, sizeof(objective_t));
    EAF_MALLOC (eaf->attained, nruns * eaf->maxsize, sizeof(bool));
    return eaf;
}

void eaf_delete (eaf_t * eaf)
{
    free (eaf->data);
    free (eaf->attained);
    free (eaf);
}

objective_t *
eaf_store_point_help (eaf_t * eaf, int nobj,
                      const int *save_attained)
{
    const int nruns = eaf->nruns;

    if (eaf->size == eaf->maxsize) {
        eaf_assert (eaf->size < INT_MAX / 2);
        eaf->maxsize = eaf->maxsize * 2;
        eaf->attained = realloc (eaf->attained, 
                                 sizeof(bool) * nruns * eaf->maxsize);
        eaf_assert(eaf->attained);
        eaf->data = realloc (eaf->data,
                             sizeof(objective_t) * nobj * eaf->maxsize);
        eaf_assert(eaf->data);
    }
    /* We convert from int to bool to hopefully save space. */
    for (int k = 0; k < nruns; k++) {
        eaf->attained[(nruns * eaf->size) + k] = (bool) save_attained[k];
    }
    return eaf->data + nobj * eaf->size;
}

static void
eaf_store_point_2d (eaf_t * eaf, objective_t x, objective_t y, 
                    const int *save_attained)
{
    const int nobj = 2;
    objective_t * pos = eaf_store_point_help (eaf, nobj, save_attained);
    pos[0] = x;
    pos[1] = y;
    eaf->size++;
}

static void
eaf_print_line (FILE *coord_file, FILE *indic_file, FILE *diff_file, 
                const objective_t *x, int nobj,
                const bool *attained, int nruns)
{
    int count1 = 0;
    int count2 = 0;
    int k;

    if (coord_file) {
        fprintf (coord_file, point_printf_format "\t" point_printf_format,
                 (double)x[0], (double)x[1]);
        for (k = 2; k < nobj; k++)
            fprintf (coord_file, "\t" point_printf_format, (double)x[k]);

        fprintf (coord_file, 
                 (coord_file == indic_file) || (coord_file == diff_file)
                 ? "\t" : "\n");
    }

    if (indic_file) {
        fprintf (indic_file, "%d", 
                 attained[0] ? (count1++,1) : 0);
        for (k = 1; k < nruns/2; k++) 
            fprintf (indic_file, "\t%d", 
                     attained[k] ? (count1++,1) : 0);
        for (k = nruns/2; k < nruns; k++)
            fprintf (indic_file, "\t%d", 
                     attained[k] ? (count2++,1) : 0);

        fprintf (indic_file, (indic_file == diff_file) ? "\t" : "\n");
    } else if (diff_file) {
        attained_left_right (attained, nruns/2, nruns, &count1, &count2);
    }

    if (diff_file)
        fprintf (diff_file,"%d\t%d\n", count1, count2);
}

/* Print one attainment surface of the EAF.  */
void
eaf_print_attsurf (eaf_t * eaf, FILE *coord_file,  FILE *indic_file, FILE *diff_file)
{
    int i;
    for (i = 0; i < eaf->size; i++) {
        const objective_t *p = eaf->data + i * eaf->nobj;
        eaf_print_line (coord_file, indic_file, diff_file,
                        p, eaf->nobj, eaf->attained + i * eaf->nruns, eaf->nruns);
    }
}

/* 
   eaf2d: compute attainment surfaces from points in objective space,
          using dimension sweeping.

   Input arguments:
        data : a pointer to the data matrix, stored as a linear array of 
               objective_t in row major order.
        cumsize : an array containing the cumulative number of rows in each 
                  non-dominated front (must be non-decreasing).
        nruns :	the number of independent non-dominated fronts.
        attlevel : an array containing the attainment levels to compute.
        nlevel : number of attainment levels to compute.
        coord_file  : stream to write the resulting attainment surfaces.
        indic_file  : stream to write the resulting attainment indices.
        diff_file   : stream to write the difference between the the 
                      first half and the second half of nruns.
*/

eaf_t **
eaf2d (const objective_t *data, const int *cumsize, int nruns,
       const int *attlevel, const int nlevels)
{
    const int nobj = 2;
    eaf_t **eaf;
    const objective_t **datax, **datay; /* used to access the data sorted
                                           according to x or y */
    
    const int ntotal = cumsize[nruns - 1]; /* total number of points in data */
    int *runtab;	
    int *attained, nattained, *save_attained;
    int k, j, l;

    /* Access to the data is made via two arrays of pointers: ix, iy
       These are sorted, to allow for dimension sweeping */

    datax = malloc (ntotal * sizeof(objective_t *));
    datay = malloc (ntotal * sizeof(objective_t *));

    for (k = 0; k < ntotal ; k++)
        datax[k] = datay[k] = data + nobj * k;

#if DEBUG > 1
    fprintf (stderr, "Original data:\n");
    fprint_set (stderr, datax, ntotal);
#endif

    qsort (datax, ntotal, sizeof(*datax), &compare_x_asc);
    qsort (datay, ntotal, sizeof(*datay), &compare_y_desc);

#if DEBUG > 1
    fprintf (stderr, "Sorted data (x):\n");
    fprint_set (stderr, datax, ntotal);
    fprintf (stderr, "Sorted data (y):\n");
    fprint_set (stderr, datay, ntotal);
#endif

    /* Setup a lookup table to go from a point to the approximation
       set (run) to which it belongs.  */

    runtab = malloc (ntotal * sizeof(int));
    for (k = 0, j = 0; k < ntotal; k++) {
        if (k == cumsize[j])
            j++;
        runtab[k] = j;
    }

#if DEBUG > 1
    fprintf (stderr, "Runtab:\n");
    for (k = 0; k < ntotal; k++) 
        fprintf (stderr, "%6d: %6d\n", k, runtab[k]);
#endif

    /* Setup tables to keep attainment statistics. In particular,
       save_attained is needed to cope with repeated values on the same
       axis. */

    attained = malloc (nruns * sizeof(int));
    save_attained = malloc (nruns * sizeof(int));
    eaf = malloc(nlevels * sizeof(eaf_t*));

    for (l = 0; l < nlevels; l++) {
        eaf[l] = eaf_create (nobj, nruns, ntotal);
        int level = attlevel[l];
        int x = 0;
        int y = 0;
        int run;

        nattained = 0;
        for (k = 0; k < nruns; k++) attained[k] = 0;

        /* Start at upper-left corner */
        run = runtab[(datax[x] - data) / nobj];
        attained[run]++;
        nattained++;

        do {
            /* Move right until desired attainment level is reached */
            while (x < ntotal - 1 && 
                   (nattained < level || datax[x][0] == datax[x+1][0])) {
                x++;
                if (datax[x][1] <= datay[y][1]) {
                    run = runtab[(datax[x] - data)/nobj];
                    if (!attained[run])
                        nattained++;
                    attained[run]++;
                }
            }
#if DEBUG > 1
            for (k = 0; k < nruns; k++)
                fprintf (stderr, "%d ", attained[k]);
            fprintf (stderr, "\n");
#endif

            if (nattained < level)
                continue;

            /* Now move down until desired attainment level is no
               longer reached.  */
            do {
                /* If there are repeated values along the y axis,
                   we need to remember where we are.  */
                /*save_nattained = nattained;*/
                memcpy (save_attained, attained, nruns * sizeof(*attained));

                do {
                    if (datay[y][0] <= datax[x][0]) {
                        run = runtab[(datay[y] - data)/nobj];
                        attained[run]--;
                        if (!attained[run])
                            nattained--;
                    }
#if DEBUG > 1
                    for (k = 0; k < nruns; k++)
                        fprintf (stderr, "%d ", attained[k]);
                    fprintf (stderr, "\n");
#endif
                    y++;
                } while (y < ntotal && datay[y][1] == datay[y - 1][1]);
            } while (nattained >= level && y < ntotal);

            eaf_assert (nattained < level);

            eaf_store_point_2d (eaf[l], datax[x][0], datay[y - 1][1],
                                save_attained);

        } while (x < ntotal - 1 && y < ntotal);
    }
    free(save_attained);
    free(attained);
    free(runtab);
    free(datay);
    free(datax);

    return eaf;
}


#define PRINT_POINT(X,Y) fprintf(stderr, point_printf_format "\t" point_printf_format "\n", X, Y)
#undef  PRINT_POINT
#define PRINT_POINT(X,Y) (void)0

static int
eaf_max_size(eaf_t * const * eaf, int nlevels)
{
    int max_size = 0;
    for (int a = 0; a < nlevels; a++) {
        if (max_size < eaf[a]->size)
            max_size = eaf[a]->size;
    }
    return max_size;
}

static int
eaf_diff_color(const eaf_t * eaf, size_t k, int nruns)
{
    const bool *attained = eaf->attained + k * nruns;
    int count_left, count_right;
    attained_left_right (attained, nruns/2, nruns, &count_left, &count_right);
    return count_left - count_right;
}

void
init_colors(int * color, const eaf_t * eaf, size_t eaf_size, int nruns)
{
    for (size_t k = 0; k < eaf_size; k++) {
        color[k] = eaf_diff_color(eaf, k, nruns);
    }
}
/* Produce a polygon suitable to be plotted by the polygon function in R.  */
eaf_polygon_t *
eaf_compute_polygon (eaf_t **eaf, int nlevels)
{
/* FIXME: Don't add anything if color_0 == 0 */

#define POLY_SIZE_CHECK()                                                      \
    do { _poly_size_check--; eaf_assert(_poly_size_check >= 4);                \
        eaf_assert(_poly_size_check % 2 == 0); _poly_size_check = 0; } while(0)
#define eaf_point(A,K) (eaf[(A)]->data + (K) * nobj)
#define push_point(X, Y)                                                       \
    do {  vector_objective_push_back (&polygon->xy, (X));                      \
        vector_objective_push_back (&polygon->xy, (Y));                        \
        _poly_size_check++; PRINT_POINT(X,Y);                                  \
    } while(0)
    
#define polygon_close()                                                        \
    do {                                                                       \
        vector_int_push_back (&polygon->col, color_0);                         \
        push_point(objective_MIN, objective_MIN); POLY_SIZE_CHECK(); } while(0)

    int _poly_size_check = 0;
    int nruns = eaf[0]->nruns;
    int nobj = eaf[0]->nobj;

    eaf_assert(nruns % 2 == 0);

    int max_size = eaf_max_size(eaf, nlevels);
    int *color;
    EAF_MALLOC (color, sizeof(int), max_size);
    eaf_polygon_t * polygon;
    EAF_MALLOC(polygon, sizeof(eaf_polygon_t), 1);
    vector_objective_ctor (&polygon->xy, max_size);
    vector_int_ctor (&polygon->col, max_size);
 
    for (int b = 1; b < nlevels; b++) {
        const int a = b - 1;
        const int eaf_a_size = eaf[a]->size;
        const int eaf_b_size = eaf[b]->size;
        init_colors(color, eaf[a], eaf_a_size, nruns);
        
        /* Find color transitions along the EAF level set.  */
        objective_t topleft_y = objective_MAX;
        int last_b = -1;
        int ka = 0;
        while (true) {
            const objective_t * pka = NULL;
            const objective_t * pkb = NULL;
            int kb = last_b + 1;
            while (ka < eaf_a_size && kb < eaf_b_size) {
                pka = eaf_point (a, ka);
                pkb = eaf_point (b, kb);
                if (pkb[0] != pka[0])
                    break;
                topleft_y = pkb[1];
                last_b = kb;
                if (pkb[1] == pka[1]) {
                    /* Ignore points that exactly overlap.  */
                    // FIXME: This should not happen, but it does. We should remove these points.
                    // eaf_assert(false);
                    ka++; kb++;
                } else {
                    /* b intersects a above pka. */
                    eaf_assert(pkb[1] > pka[1]);
                    kb++;
                    break;
                }
            }

            /* Everything in A was overlapping. */
            if (ka == eaf_a_size)
                break;

            objective_t prev_pka_y = topleft_y;
            int color_0 = color[ka];
            /* Print points and corners until we reach a different color. */
            do {
                pka = eaf_point (a, ka);
                /* Find the point in B not above the current point in A. */
                while (kb < eaf_b_size) {
                    pkb = eaf_point (b, kb);
                    eaf_assert(pkb[0] > pka[0]);
                    if (pkb[1] <= pka[1])
                        break;
                    kb++;
                }
                eaf_assert(pka[1] < prev_pka_y);
                push_point (pka[0], prev_pka_y);
                push_point (pka[0], pka[1]);
                prev_pka_y = pka[1];
                ka++;

                if (kb < eaf_b_size && ka < eaf_a_size) {
                    const objective_t * pka_next = eaf_point (a, ka);
                    eaf_assert (pkb[0] > pka[0]);
                    eaf_assert (pkb[1] <= pka[1]);
                    if (pkb[0] <= pka_next[0]) {
                        /* If B intersects with A, stop here.  */
                        eaf_assert(pkb[1] == pka[1] || pkb[0] == pka_next[0]);
                        eaf_assert (prev_pka_y >= pkb[1]);
                        break;
                    }
                }
            } while (ka < eaf_a_size && color_0 != color[ka]);

            /* pka is the point before changing color, but ka is the
               position after.  */
            if (ka == eaf_a_size) {/* We reached the end of eaf_a */
                /* We don't have to go down the other side since eaf_a
                   completely dominates eaf_b, so just start by the
                   end. */
                if (last_b == eaf_b_size - 1) {
                    /* There is nothing on the other side, just create
                       two points in the infinity. */
                    push_point (objective_MAX, pka[1]);
                    push_point (objective_MAX, topleft_y);
                    eaf_assert(topleft_y > pka[1]);
                } else {
                    kb = eaf_b_size - 1;
                    pkb = eaf_point(b, kb);
                    eaf_assert (pkb[1] >= pka[1]);
                    eaf_assert (pkb[0] > pka[0]);
                    if (pkb[1] > pka[1]) {
                        /* Create two points in the infinity.  */
                        push_point (objective_MAX, pka[1]);
                        push_point (objective_MAX, pkb[1]);
                        eaf_assert (pkb[1] <= topleft_y);
                    }
                    objective_t prev_pkb_x = pkb[0];
                    push_point (pkb[0], pkb[1]);
                    kb--;
                    while (kb > last_b) {
                        pkb = eaf_point(b, kb);
                        eaf_assert (pkb[0] > pka[0]);
                        push_point (prev_pkb_x, pkb[1]);
                        push_point (pkb[0], pkb[1]);
                        prev_pkb_x = pkb[0];
                        kb--;
                    }
                    push_point (pkb[0], topleft_y);
                    eaf_assert (topleft_y > pkb[1]);
                }
                /* last_b = eaf_b_size - 1; */
                polygon_close(); /* DONE */
                break; /* Really done! */
            } else {
                if (kb == eaf_b_size) { 
                    eaf_assert (pka[1] < topleft_y);
                    /* There is nothing on the other side, just create
                       two points in the infinity. */
                    push_point (objective_MAX, pka[1]);
                    push_point (objective_MAX, topleft_y);
                    last_b = eaf_b_size - 1;
                } else {
                    eaf_assert (kb < eaf_b_size);
                    pkb = eaf_point (b, kb);
                    eaf_assert (pkb[1] <= pka[1]);
                    eaf_assert (pkb[0] != pka[0]);
                    /* If pkb and pka are in the same horizontal, pkb
                       does not affect the next polygon. Otherwise, it
                       does. */
                    int save_last_b = (pkb[1] == pka[1]) ? kb : kb - 1;;
                    /* Now print in reverse.  */
                    objective_t prev_pkb_x = pkb[0];
                    push_point (pkb[0], pka[1]);
                    kb--;
                    while (kb > last_b) {
                        pkb = eaf_point (b, kb);
                        eaf_assert (pkb[0] != pka[0]);
                        push_point (prev_pkb_x, pkb[1]);
                        push_point (pkb[0], pkb[1]);
                        prev_pkb_x = pkb[0];
                        kb--;
                    }
                    eaf_assert (topleft_y > pkb[1]);
                    push_point (pkb[0], topleft_y);
                    last_b = save_last_b;
                }
                polygon_close(); /* DONE */
                eaf_assert(topleft_y >= pka[1]);
                topleft_y = pka[1];
            }
        }
    }

    free (color);
    return polygon;
}

/* FIXME: This version does not care about intersections, which is
   much simpler, but it produces artifacts when plotted with the
   polygon function in R.  */
eaf_polygon_t *
eaf_compute_polygon_old (eaf_t **eaf, int nlevels)
{
    int _poly_size_check = 0;
    eaf_polygon_t * polygon;

    int *color;
    int max_size = eaf_max_size(eaf, nlevels);
    int nruns = eaf[0]->nruns;
    int nobj = eaf[0]->nobj;

    eaf_assert(nruns % 2 == 0);

    EAF_MALLOC (color, sizeof(int), max_size);
    EAF_MALLOC(polygon, sizeof(eaf_polygon_t), 1);
    vector_objective_ctor (&polygon->xy, max_size);
    vector_int_ctor (&polygon->col, max_size);
 
    for (int b = 1; b < nlevels; b++) {
        const int a = b - 1;
        int eaf_a_size = eaf[a]->size;
        int eaf_b_size = eaf[a + 1]->size;
        init_colors(color, eaf[a], eaf_a_size, nruns);
        /* Find color transitions along the EAF level set.  */
        int last_b = -1;
        objective_t topleft_y = objective_MAX;
        int ka = 0;
        while (ka < eaf_a_size) {
            objective_t prev_pka_y = topleft_y;
            const objective_t * pka;

            /* Print points and corners until we reach a different color. */
            int color_0 = color[ka];
            do {
                pka = eaf_point (a, ka);
                push_point (pka[0], prev_pka_y);
                push_point (pka[0], pka[1]);
                prev_pka_y = pka[1];
                ka++;
            } while (ka < eaf_a_size && color_0 == color[ka]);

            /* pka is the point before changing color, but ka is the
               position after.  */
            if (ka == eaf_a_size) {/* We reached the end of eaf_a */
                /* We don't have to go down the other side since eaf_a
                   completely dominates eaf_b, so just start by the
                   end. */
                int kb = eaf_b_size - 1;

                if (last_b == kb) {
                    /* There is nothing on the other side, just create
                       two points in the infinity. */
                    push_point (objective_MAX, pka[1]);
                    push_point (objective_MAX, topleft_y);
                } else {
                    const objective_t * pkb = eaf_point(a + 1, kb);
                    eaf_assert (pkb[1] >= pka[1]);
                    if (pkb[1] > pka[1]) {
                        /* Create two points in the infinity.  */
                        push_point (objective_MAX, pka[1]);
                        push_point (objective_MAX, pkb[1]);
                    }
                    objective_t prev_pkb_x = pkb[0];
                    push_point (pkb[0], pkb[1]);
                    kb--;
                    while (kb > last_b) {
                        pkb = eaf_point(a + 1, kb);
                        push_point (prev_pkb_x, pkb[1]);
                        push_point (pkb[0], pkb[1]);
                        prev_pkb_x = pkb[0];
                        kb--;
                    }
                    push_point (pkb[0], topleft_y);
                    last_b = eaf_b_size - 1;
                }
                polygon_close(); /* DONE */
            } else {
                int kb = last_b + 1;
                /* Different color, go down by the other side until
                   reaching this point. */
                if (kb == eaf_b_size) { 
                    /* There is nothing on the other side, just create
                       two points in the infinity. */
                    push_point (objective_MAX, pka[1]);
                    push_point (objective_MAX, topleft_y);
                    polygon_close(); /* DONE */
                } else {
                    const objective_t * pkb;
                    do {
                        pkb = eaf_point (b, kb);
                        if (pkb[1] <= pka[1]) 
                            break;
                        kb++;
                    } while (kb < eaf_b_size);
                    int save_last_b = kb - 1;
                    if (kb == eaf_b_size) {
                        /* There is nothing on the other side, just create
                           two points in the infinity. */
                        push_point (objective_MAX, pka[1]);
                        push_point (objective_MAX, pkb[1]);
                    } else {/* pkb_y <= pka_y */
                        objective_t prev_pkb_x = pkb[0];
                        push_point (pkb[0], pka[1]);
                        /* Now print in reverse.  */
                        kb--;
                        while (kb > last_b) {
                            pkb = eaf_point (b, kb);
                            push_point (prev_pkb_x, pkb[1]);
                            push_point (pkb[0], pkb[1]);
                            prev_pkb_x = pkb[0];
                            kb--;
                        } 
                        push_point (pkb[0], topleft_y);
                    }
                    polygon_close(); /* DONE */
                    last_b = save_last_b;
                }
            }
            topleft_y = pka[1];
        }
    }

    free (color);
    return polygon;
}
#undef eaf_point
#undef push_point
#undef polygon_close
#undef PRINT_POINT
#undef POLY_SIZE_CHECK

void
eaf_print_polygon (FILE *stream, eaf_t **eaf, int nlevels)
{
    eaf_polygon_t *p = eaf_compute_area (eaf, nlevels);
    
    for(size_t i = 0; i < vector_objective_size(&p->xy); i += 2) {
        fprintf(stream, point_printf_format "\t" point_printf_format "\n", 
                vector_objective_at(&p->xy, i),
                vector_objective_at(&p->xy, i + 1));
    }

    fprintf (stream, "# col =");
    for (size_t k = 0; k < vector_int_size (&p->col); k++)
        fprintf (stream, " %d", vector_int_at(&p->col, k));
    fprintf (stream, "\n");

    vector_objective_dtor (&p->xy);
    vector_int_dtor (&p->col);
    free(p);
}

size_t
region_push (vector_objective *regions,
             objective_t lx, objective_t ly,
             objective_t ux, objective_t uy)
{
    vector_objective_push_back(regions, lx);
    vector_objective_push_back(regions, ly);
    vector_objective_push_back(regions, ux);
    vector_objective_push_back(regions, uy);
    return vector_objective_size(regions);
}

eaf_polygon_t *
eaf_compute_rectangles (eaf_t **eaf, int nlevels)
{
#define eaf_point(A,K) (eaf[(A)]->data + (K) * nobj)
#if 0
#define printf_points(ka,kb,pka,pkb)                                           \
    printf("%4d: pa[%d]=(" point_printf_format ", " point_printf_format "), pb[%d] = (" point_printf_format ", " point_printf_format ")\n", __LINE__, ka, pka[0], pka[1], kb, pkb[0], pkb[1])
#else
#define printf_points(ka,kb,pka,pkb)                                           
#endif    
    int nruns = eaf[0]->nruns;
    int nobj = eaf[0]->nobj;

    eaf_assert(nruns % 2 == 0);

    int max_size = eaf_max_size(eaf, nlevels);
    int *color;
    EAF_MALLOC (color, sizeof(int), max_size);
    eaf_polygon_t * regions;
    EAF_MALLOC(regions, sizeof(eaf_polygon_t), 1);
    vector_objective_ctor (&regions->xy, max_size);
    vector_int_ctor (&regions->col, max_size);
        
    for (int b = 1; b < nlevels; b++) {
        const int a = b - 1;
        const int eaf_a_size = eaf[a]->size;
        const int eaf_b_size = eaf[b]->size;
        if (eaf_a_size == 0 || eaf_b_size == 0) continue;

        init_colors(color, eaf[a], eaf_a_size, nruns);
        objective_t top = objective_MAX;
        int ka = 0, kb = 0;
        const objective_t * pka = eaf_point (a, ka);
        const objective_t * pkb = eaf_point (b, kb);
        printf_points(ka, kb, pka, pkb);
        /* printf("attained[ka] ="); */
        /* for (int k = 0; k < nruns; k++)  */
        /*     printf(" %d", (eaf[a]->attained + ka * nruns)[k]  ? 1 : 0); */
        /* printf("\n"); */
        /* printf("attained[kb] ="); */
        /* for (int k = 0; k < nruns; k++)  */
        /*     printf(" %d", (eaf[b]->attained + kb * nruns)[k]  ? 1 : 0); */
        /* printf("\n"); */
        /* printf("attained[%d][%d,%d] = [%d, %d]\n", ka, a, b, */
        /*        (eaf[a]->attained + ka * nruns)[a]  ? 1 : 0, */
        /*        (eaf[a]->attained + ka * nruns)[b]  ? 1 : 0); */
        /* printf("attained[%d][%d,%d] = [%d, %d]\n", kb, a, b, */
        /*        (eaf[b]->attained + kb * nruns)[a]  ? 1 : 0, */
        /*        (eaf[b]->attained + kb * nruns)[b]  ? 1 : 0); */
               
        eaf_assert(pka[0] <= pkb[0]);
        /* It is possible that pka does not dominate pkb if their intersection
           belongs to the next eaf level. */
        if (pka[1] >= pkb[1]) goto pka_above_equal_pkb;
                
    pka_below_pkb:
        eaf_assert(pka[1] < pkb[1]);
        /* If pka[0] >= pkb[0] then no rectangle is created. */
        if (pka[0] < pkb[0]) {
            region_push(&regions->xy, pka[0], pkb[1], pkb[0], top);
            vector_int_push_back(&regions->col, color[ka]);
        } 
        top = pkb[1];
        kb++;
        if (kb >= eaf_b_size) goto close_eaf;
        pkb = eaf_point (b, kb);
        printf_points(ka, kb, pka, pkb);
        if (pka[1] < pkb[1]) goto pka_below_pkb;

    pka_above_equal_pkb:
        if (pka[0] < pkb[0]) {                    
            region_push(&regions->xy, pka[0], pka[1], pkb[0], top);
            vector_int_push_back(&regions->col, color[ka]);
        } else {
            // Handle repeated points
            eaf_assert(pka[0] == pkb[0] && pka[1] == pkb[1]);
            top = pka[1];
            ka++, kb++;
            if (ka >= eaf_a_size) goto next_eaf;
            if (kb >= eaf_b_size) goto close_eaf;
            
            pka = eaf_point (a, ka);
            pkb = eaf_point (b, kb);
            printf_points(ka, kb, pka, pkb);
            eaf_assert(pka[0] <= pkb[0]);
            if (pka[1] >= pkb[1]) goto pka_above_equal_pkb;
            else goto pka_below_pkb;
        }
        top = pka[1];
        ka++;
        if (ka >= eaf_a_size) goto next_eaf;
        pka = eaf_point (a, ka);
        printf_points(ka, kb, pka, pkb);
        if (pka[1] >= pkb[1]) goto pka_above_equal_pkb;
        else goto pka_below_pkb;

    close_eaf:
        region_push(&regions->xy, pka[0], pka[1], objective_MAX, top);
        vector_int_push_back(&regions->col, color[ka]);
    next_eaf:
        continue;
    }
    return regions;
#undef eaf_point
}
