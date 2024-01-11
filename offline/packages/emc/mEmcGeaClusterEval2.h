#ifndef __MEMCGEACLUSTEREVAL2_H__
#define __MEMCGEACLUSTEREVAL2_H__

#include "table_header.h"

#include "dEmcEvent.h"

#include "dEmcGeaTrack.h"

#include "dEmcGeaTowerTrack.h"

#include "dEmcClusterExt.h"

#include "dEmcGeaTrackCluster.h"

#include "dEmcGeaClusterTrack.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcgeaclustereval2_(
       TABLE_HEAD_ST*, DEMCEVENT_ST*,
       TABLE_HEAD_ST*, DEMCGEATRACK_ST*,
       TABLE_HEAD_ST*, DEMCGEATOWERTRACK_ST*,
       TABLE_HEAD_ST*, DEMCCLUSTEREXT_ST*,
       TABLE_HEAD_ST*, DEMCGEATRACKCLUSTER_ST*,
       TABLE_HEAD_ST*, DEMCGEACLUSTERTRACK_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcGeaClusterEval2_ memcgeaclustereval2_

#endif /*__MEMCGEACLUSTEREVAL2_H__*/
