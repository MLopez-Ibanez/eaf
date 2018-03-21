#ifndef EAF_INPUT_OUTPUT_H
#define EAF_INPUT_OUTPUT_H

#define _GNU_SOURCE
#include <errno.h> // for errno
/*const char *program_invocation_short_name = "eaf";*/
#define PROGRAM_NAME "eaf"

#define point_printf_format "% 17.16g"

/* Error codes for read_data.  */
enum ERROR_READ_DATA { READ_INPUT_FILE_EMPTY = -1,
                       READ_INPUT_WRONG_INITIAL_DIM = -2,
                       ERROR_FOPEN = -3,
                       ERROR_CONVERSION = -4,
                       ERROR_COLUMNS = -5,
};

int
read_int_data (const char *filename, int  **data_p, 
               int *nobjs_p, int **cumsizes_p, int *nsets_p);
int
read_double_data (const char *filename, double  **data_p, 
                  int *nobjs_p, int **cumsizes_p, int *nsets_p);

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
# define  __attribute__(x)  /* NOTHING */
#endif

#ifdef R_PACKAGE
#include <R.h>
#define errprintf error
#define warnprintf warning
#else
void errprintf(const char * template,...) 
/* enables the compiler to check the format string against the
   parameters */  __attribute__ ((format(printf, 1, 2)));

void warnprintf(const char *template,...)
/* enables the compiler to check the format string against the
   parameters */  __attribute__ ((format(printf, 1, 2)));
#endif

#endif
