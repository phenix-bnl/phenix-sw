#ifndef __MPADEVALUATE_H__
#define __MPADEVALUATE_H__
/* Automatically generated.  Do not edit. */

#include "table_header.h"

#include "dPadEvalPar.h"

#include "dPadGeom.h"

#include "pcghit.h"

#include "dPadRaw.h"

#include "dPadCluster.h"

#include "dPadGhitRaw.h"

#include "dPadRawClus.h"

#include "dPadGhitClus.h"

#include "dPadEval.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mpadevaluate_(
       TABLE_HEAD_ST*, DPADEVALPAR_ST*,
       TABLE_HEAD_ST*, DPADGEOM_ST*,
       TABLE_HEAD_ST*, PCGHIT_ST*,
       TABLE_HEAD_ST*, DPADRAW_ST*,
       TABLE_HEAD_ST*, DPADCLUSTER_ST*,
       TABLE_HEAD_ST*, DPADGHITRAW_ST*,
       TABLE_HEAD_ST*, DPADRAWCLUS_ST*,
       TABLE_HEAD_ST*, DPADGHITCLUS_ST*,
       TABLE_HEAD_ST*, DPADEVAL_ST*
               );
#ifdef __cplusplus
}
#endif
#define mPadEvaluate_ mpadevaluate_

#endif /*__MPADEVALUATE_H__*/
