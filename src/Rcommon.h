#include <R.h>
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <R_ext/Memory.h>

#include "common.h"

#define CHECK_ARG_IS_NUMERIC_VECTOR(A)					\
    if (!isReal(A) || !isVector(A))					\
	error("Argument '" #A "' is not a numeric vector");

#define CHECK_ARG_IS_INT_VECTOR(A)					\
    if (!isInteger(A) || !isVector(A))					\
	error("Argument '" #A "' is not an integer vector");

#define CHECK_ARG_IS_LOGICAL_VECTOR(A)					\
    if (!isLogical(A) || !isVector(A))					\
	error("Argument '" #A "' is not a logical vector");

/* The C API of R is awfully ugly and unpractical (and poorly
   documented). These wrappers make it a little more bearable. */

#define Rexp(VAR) Rexp_##VAR

#define new_real_matrix(VAR, DIM1, DIM2)                                 \
    SEXP Rexp_##VAR; double *VAR;                                        \
    PROTECT(Rexp_##VAR = allocMatrix(REALSXP, (DIM1), (DIM2)));          \
    nprotected++; VAR = REAL(Rexp_##VAR)

#define new_real_vector(VAR, DIM)                                        \
    SEXP Rexp_##VAR; double *VAR;                                        \
    PROTECT(Rexp_##VAR = allocVector(REALSXP, (DIM)));                   \
    nprotected++; VAR = REAL(Rexp_##VAR)

#define new_int_vector(VAR, DIM)                                        \
    SEXP Rexp_##VAR; int *VAR;                                          \
    PROTECT(Rexp_##VAR = allocVector(INTSXP, (DIM)));                   \
    nprotected++; VAR = INTEGER(Rexp_##VAR)

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

#define new_logical_vector(VAR, DIM)                                           \
    SEXP Rexp_##VAR; int *VAR;                                                 \
    PROTECT(Rexp_##VAR = allocVector(LGLSXP, (DIM)));                          \
    nprotected++; VAR = LOGICAL(Rexp_##VAR)

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
#define SEXP_2_DOUBLE_VECTOR(S, I, N)                \
    CHECK_ARG_IS_NUMERIC_VECTOR(S);                  \
    double *I = REAL(S);                             \
    const R_len_t N = length(S);

#define SEXP_2_INT_VECTOR(S, I, N)               \
    CHECK_ARG_IS_INT_VECTOR(S);                  \
    int *I = INTEGER(S);                         \
    const R_len_t N = length(S);

#define SEXP_2_LOGICAL_VECTOR(S, I, N)               \
    CHECK_ARG_IS_LOGICAL_VECTOR(S);                  \
    int *I = LOGICAL(S);                             \
    const R_len_t N = length(S);

#define SEXP_2_INT(S,VAR)                                               \
    int VAR = asInteger(S);                                             \
    if (VAR == NA_INTEGER)                                              \
        error ("Argument '" #S "' is not an integer");

#define SEXP_2_LOGICAL(S,VAR)                                           \
    int VAR = asLogical(S);                                             \
    if (VAR == NA_LOGICAL)                                              \
        error ("Argument '" #S "' is not a logical");

#define SEXP_2_STRING(S,var)                                            \
    if (!isString(S) || length(S) != 1)                                 \
        error ("Argument '" #S "' is not a string");                    \
    const char * var = CHAR(STRING_ELT(S,0));

static inline void
bool_2_logical_vector(int *dst, const bool *src, size_t n)
{
    for (size_t i = 0; i < n; i++)
        dst[i] = src[i];
}

/* FIXME: Measure if this is faster than the R implementation of t()  */
static inline void
double_transpose(double *dst, const double *src,
                 const size_t nrows, const size_t ncols)
{
    size_t j, i, pos = 0;
    
    for (j = 0; j < ncols; j++) {
        for (i = 0; i < nrows; i++) {
            dst[pos] = src[j + i * ncols];
            pos++;
        }
    }
}
