/*****************************************************************************

 eaf_input.c: read EAF data

 ---------------------------------------------------------------------

                 Copyright (c) 2005, 2006, 2007, 2008
                  Carlos Fonseca <cmfonsec@ualg.pt>
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

 TODO: things that may or may not improve reading performance.

  * different values for PAGE_SIZE.

  * reading whole lines and then sscanf the line.
 
  * mmaping the input file (or a big chunk of few megabytes), then
    read, then unmmap.

*****************************************************************************/

#include <stdarg.h>
#include <stdio.h>
#include "eaf_io.h"

#ifndef ignore_unused_result
#define ignore_unused_result(X)  do { if(X) {}} while(0);
#endif

/* skip full lines starting with # */
static inline int skip_comment_line (FILE * instream)
{
    char newline[2];
    if (!fscanf (instream, "%1[#]%*[^\n]", newline))
        /* and whitespace */
        ignore_unused_result (fscanf (instream, "%*[ \t\r]"));
    return fscanf (instream, "%1[\n]", newline);
}


#define objective_t int
#define objective_t_scanf_format "%d"
#define read_objective_t_data read_int_data
#include "eaf_io_priv.h"
#undef objective_t
#undef objective_t_scanf_format
#undef read_objective_t_data

#define objective_t double
#define objective_t_scanf_format "%lf"
#define read_objective_t_data read_double_data
#include "eaf_io_priv.h"
#undef objective_t
#undef objective_t_scanf_format
#undef read_objective_t_data

#ifndef R_PACKAGE
/* From:

   Edition 0.10, last updated 2001-07-06, of `The GNU C Library
   Reference Manual', for Version 2.3.x.

   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2001, 2002,
   2003 Free Software Foundation, Inc.
*/
void errprintf(const char *template,...)
{
    va_list ap;

    fprintf(stderr, "%s: error: ", PROGRAM_NAME);
    va_start(ap,template);
    vfprintf(stderr, template, ap);
    va_end(ap);
    fprintf(stderr, "\n");
}
/* End of copyright The GNU C Library Reference Manual */

void warnprintf(const char *template,...)
{
    va_list ap;

    fprintf(stderr, "%s: warning: ", PROGRAM_NAME);
    va_start(ap,template);
    vfprintf(stderr, template, ap);
    va_end(ap);
    fprintf(stderr, "\n");

}
#endif
