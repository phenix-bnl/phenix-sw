#ifndef __MPADREC_H__
#define __MPADREC_H__
/* Automatically generated.  Do not edit. */

#include "table_header.h"

#include "dPadRecPar.h"

#include "dPadGeom.h"

#include "dPadRaw.h"

#include "dPadCluster.h"

#include "dPadRawClus.h"

#include "dPad23Par.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mpadrec_(
       TABLE_HEAD_ST*, DPADRECPAR_ST*,
       TABLE_HEAD_ST*, DPADGEOM_ST*,
       TABLE_HEAD_ST*, DPADRAW_ST*,
       TABLE_HEAD_ST*, DPADCLUSTER_ST*,
       TABLE_HEAD_ST*, DPADRAWCLUS_ST*,
       TABLE_HEAD_ST*, DPAD23PAR_ST*
               );
#ifdef __cplusplus
}
#endif
#define mPadRec_ mpadrec_

#endif /*__MPADREC_H__*/
