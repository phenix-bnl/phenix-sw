#ifndef __MPADFEM_H__
#define __MPADFEM_H__
/* Automatically generated.  Do not edit. */

#include "table_header.h"

#include "dPadRaw.h"

#include "dPadGhitRaw.h"

#include "dPadGeom.h"

#include "dPadFEMPar.h"

#include "dPadFEM.h"

#include "dPadNibbleGhit.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mpadfem_(
       TABLE_HEAD_ST*, DPADRAW_ST*,
       TABLE_HEAD_ST*, DPADGHITRAW_ST*,
       TABLE_HEAD_ST*, DPADGEOM_ST*,
       TABLE_HEAD_ST*, DPADFEMPAR_ST*,
       TABLE_HEAD_ST*, DPADFEM_ST*,
       TABLE_HEAD_ST*, DPADNIBBLEGHIT_ST*
               );
#ifdef __cplusplus
}
#endif
#define mPadFEM_ mpadfem_

#endif /*__MPADFEM_H__*/
