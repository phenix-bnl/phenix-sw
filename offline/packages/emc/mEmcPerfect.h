#ifndef __MEMCPERFECT_H__
#define __MEMCPERFECT_H__

#include "table_header.h"

#include "dEmcGeaHit.h"

#include "dEmcClusterLocal.h"

#include "dEmcGeaClusterTrack.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcperfect_(
       TABLE_HEAD_ST*, DEMCGEAHIT_ST*,
       TABLE_HEAD_ST*, DEMCCLUSTERLOCAL_ST*,
       TABLE_HEAD_ST*, DEMCGEACLUSTERTRACK_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcPerfect_ memcperfect_

#endif /*__MEMCPERFECT_H__*/
