/****************************************************************************
 * File: tof_FEM.h
 * Author: Hiroyuki Sako (Univ. of Tsukuba)
 * Description: common header for tof FEM/DCM
 * Date: 07/31/98 H. Sako  First Version
 *       05/09/00 A.Kiyomichi FEM:864 -> 1136  DCM:855 -> 1144
 *****************************************************************************/

#ifndef INCLUDE_TOF_FEM_H

#define N_FEM_CRATE   8  /* No. of FEM cate for 10 panels in the East arm */
#define N_FEM_SLOT   16  /* No. of FEM slot in a FEM crate */
#define N_FEM_CH     16  /* No. of channels in a FEM */
#define N_FEM_WORD  1136 /* No. of FEM data word for 1 FEM crate */
#define N_DCM_WORD  855  /* No. of DCM data word for 1 DCM */
#define N_DCM_WORD407  855  /* No. of DCM data word for 1 DCM */
#define N_DCM_WORD607 1144  /* No. of DCM data word for 1 DCM */

#define INCLUDE_TOF_FEM_H
#endif
