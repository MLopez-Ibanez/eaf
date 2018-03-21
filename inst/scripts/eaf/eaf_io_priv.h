#include <stdlib.h>
#include <stdio.h>
#include <string.h> /* for strerror() */
#include <errno.h> /* for errno  */

#define PAGE_SIZE 4096
#define DATA_INC (PAGE_SIZE/sizeof(objective_t))

#ifndef DEBUG
#define DEBUG 0
#endif

/*
 * Read an array of objective values from a stream.  This function may
 * be called repeatedly to add data to an existing data set.
 *
 *  nobjs : number of objectives, also the number of columns.
 */

int
read_objective_t_data (const char *filename, objective_t **data_p, 
                       int *nobjs_p, int **cumsizes_p, int *nsets_p)
{
    FILE *instream;

    int nobjs = *nobjs_p;        /* number of objectives (and columns).  */
    int *cumsizes = *cumsizes_p; /* cumulative sizes of data sets.       */
    int nsets    = *nsets_p;     /* number of data sets.                 */
    objective_t *data = *data_p;

    int retval;			/* return value for fscanf */
    char newline[2];
    int ntotal;			/* the current element of (*datap) */

    int column = 0,
        line = 0;

    int datasize;
    int sizessize;

    int errorcode = 0;

    if (filename == NULL) {
        instream = stdin;
        filename = "<stdin>"; /* used to diagnose errors.  */
    } else if (NULL == (instream = fopen (filename,"rb"))) {
        errprintf ("%s: %s", filename, strerror (errno));
        return (ERROR_FOPEN);
    }

    if (nsets == 0) {
        ntotal = 0;
        sizessize = 0;
        datasize = 0;
    } else {
        ntotal = nobjs * cumsizes[nsets - 1];
        sizessize = ((nsets - 1) / DATA_INC + 1) * DATA_INC;
        datasize  = ((ntotal - 1) / DATA_INC + 1) * DATA_INC;
    }

    /* if size is equal to zero, this is equivalent to free().  
       That is, reinitialize the data structures.  */
    cumsizes = realloc (cumsizes, sizessize * sizeof(int));
    data = realloc (data, datasize * sizeof(objective_t));

    /* skip over leading whitespace, comments and empty lines.  */
    do { 
        line++;
        /* skip full lines starting with # */
        retval = skip_comment_line (instream);
    } while (retval == 1);

    if (retval == EOF) { /* faster than !feof() */
        warnprintf ("%s: file is empty.", filename);
        errorcode = READ_INPUT_FILE_EMPTY;
        goto read_data_finish;
    }

    do {
        /* beginning of data set */
	if (nsets == sizessize) {
            sizessize += DATA_INC;
            cumsizes = realloc(cumsizes, sizessize * sizeof(int));
        }
        
	cumsizes[nsets] = (nsets == 0) ? 0 : cumsizes[nsets - 1];
	
        do {
            /* beginning of row */
	    column = 0;		
            
	    do {
                /* new column */
                column++; 
                objective_t number;
                if (fscanf (instream, objective_t_scanf_format, &number) != 1) {
                    char buffer[64];
                    if (fscanf (instream, "%60[^ \t\r\n]", buffer) == EOF) {
                        errprintf ("%s: line %d column %d: "
                                   "read error or EOF", 
                                   filename, line, column);
                    } else {
                        errprintf ("%s: line %d column %d: "
                                   "could not convert string `%s' to double", 
                                   filename, line, column, buffer);
                    }
                    errorcode = ERROR_CONVERSION;
                    goto read_data_finish;
                }

                if (ntotal == datasize) {
                    datasize += DATA_INC;
                    data = realloc (data, datasize * sizeof(objective_t));
                }
                data[ntotal] = number;
                ntotal++;
#if DEBUG > 1
                fprintf(stderr, "%s:%d:%d(%d) %d (set %d) = %.15g\n", 
                        filename, line, column, nobjs, 
                        cumsizes[nsets], nsets, (double)number);
#endif
                /* skip possible trailing whitespace */
                ignore_unused_result (fscanf (instream, "%*[ \t\r]"));
                retval = fscanf (instream, "%1[\n]", newline);
	    } while (retval == 0);
            
	    if (!nobjs)
		nobjs = column;
            else if (column == nobjs)
                ; /* OK */
            else if (cumsizes[0] == 0) { /* just finished first row.  */
                errprintf ("%s: line %d: input has dimension %d"
                           " while previous data has dimension %d",
                           filename,line, column, nobjs);
                errorcode = READ_INPUT_WRONG_INITIAL_DIM;
                goto read_data_finish;
            } else {
                errprintf ("%s: line %d has different number of columns (%d)"
                           " from first row (%d)\n", 
                           filename, line, column, nobjs);
                errorcode = ERROR_COLUMNS;
                goto read_data_finish;
 	    }
	    cumsizes[nsets]++;

            /* look for an empty line */
            line++;
            retval = skip_comment_line (instream);
	} while (retval == 0);

	nsets++; /* new data set */

#if DEBUG > 1
	fprintf (stderr, "%s: set %d, read %d rows\n", 
                 filename, nsets, cumsizes[nsets - 1]);
#endif
        /* skip over successive empty lines */
        do { 
            line++;
            retval = skip_comment_line (instream);
        } while (retval == 1);

    } while (retval != EOF); /* faster than !feof() */

    /* adjust to real size (saves memory but probably slower).  */
    cumsizes = realloc (cumsizes, nsets * sizeof(int));
    data = realloc (data, ntotal * sizeof(objective_t));

read_data_finish:

    *nobjs_p = nobjs;
    *nsets_p = nsets;
    *cumsizes_p = cumsizes;
    *data_p = data;

    if (instream != stdin) 
        fclose(instream);

    return errorcode;
}

#undef PAGE_SIZE
#undef DATA_INC
