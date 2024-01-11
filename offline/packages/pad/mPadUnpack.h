#ifndef __MPADUNPACK_H__
#define __MPADUNPACK_H__
/* Automatically generated.  Do not edit. */

#include "table_header.h"

#include "dPadDCM.h"

#include "dPadGeom.h"

#include "dPadNibbleGhit.h"

#include "dPadFEMPar.h"

#include "dPadRaw.h"

#include "dPadGhitRaw.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mpadunpack_(
       TABLE_HEAD_ST*, DPADDCM_ST*,
       TABLE_HEAD_ST*, DPADGEOM_ST*,
       TABLE_HEAD_ST*, DPADNIBBLEGHIT_ST*,
       TABLE_HEAD_ST*, DPADFEMPAR_ST*,
       TABLE_HEAD_ST*, DPADRAW_ST*,
       TABLE_HEAD_ST*, DPADGHITRAW_ST*
               );
#ifdef __cplusplus
}
#endif
#define mPadUnpack_ mpadunpack_

#endif /*__MPADUNPACK_H__*/
