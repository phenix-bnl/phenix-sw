#ifndef __MEMCGEACLUSTEREVAL_H__
#define __MEMCGEACLUSTEREVAL_H__

#include "table_header.h"

#include "dEmcEvent.h"

#include "dEmcGeaTrack.h"

#include "dEmcGeaTowerTrack.h"

#include "dEmcClusterLocalExt.h"

#include "dEmcGeaTrackCluster.h"

#include "dEmcGeaClusterTrack.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcgeaclustereval_(
       TABLE_HEAD_ST*, DEMCEVENT_ST*,
       TABLE_HEAD_ST*, DEMCGEATRACK_ST*,
       TABLE_HEAD_ST*, DEMCGEATOWERTRACK_ST*,
       TABLE_HEAD_ST*, DEMCCLUSTERLOCALEXT_ST*,
       TABLE_HEAD_ST*, DEMCGEATRACKCLUSTER_ST*,
       TABLE_HEAD_ST*, DEMCGEACLUSTERTRACK_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcGeaClusterEval_ memcgeaclustereval_

#endif /*__MEMCGEACLUSTEREVAL_H__*/
