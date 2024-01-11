#ifndef __MEMCGEATRACK_H__
#define __MEMCGEATRACK_H__

#include "table_header.h"

#include "dEmcGeaTrackTower.h"

#include "dEmcGeaTrack.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcgeatrack_(
       TABLE_HEAD_ST*, DEMCGEATRACKTOWER_ST*,
       TABLE_HEAD_ST*, DEMCGEATRACK_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcGeaTrack_ memcgeatrack_

#endif /*__MEMCGEATRACK_H__*/
