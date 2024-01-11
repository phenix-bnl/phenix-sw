#ifndef __MPADFASTSIM_H__
#define __MPADFASTSIM_H__
/* Automatically generated.  Do not edit. */

#include "table_header.h"

#include "pcghit.h"

#include "dPadFastSimPar.h"

#include "dPadCluster.h"

#include "dPadGhitClus.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mpadfastsim_(
       TABLE_HEAD_ST*, PCGHIT_ST*,
       TABLE_HEAD_ST*, DPADFASTSIMPAR_ST*,
       TABLE_HEAD_ST*, DPADCLUSTER_ST*,
       TABLE_HEAD_ST*, DPADGHITCLUS_ST*
               );
#ifdef __cplusplus
}
#endif
#define mPadFastSim_ mpadfastsim_

#endif /*__MPADFASTSIM_H__*/
