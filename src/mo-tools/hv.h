/*************************************************************************

 hv.h 

 ---------------------------------------------------------------------

                       Copyright (c) 2005, 2006
                  Carlos M. Fonseca <cmfonsec@dei.uc.pt>
             Manuel Lopez-Ibanez <manuel.lopez-ibanez@manchester.ac.uk>
                    Luis Paquete <paquete@dei.uc.pt>

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
#ifndef HV_H_
#define HV_H_

#ifdef __cplusplus
extern "C" {
#endif

double fpli_hv(const double *data, int d, int n, const double *ref);
void hv_contributions (double *hvc, double *points, int dim, int size, const double * ref);
#ifdef __cplusplus
}
#endif

#endif
