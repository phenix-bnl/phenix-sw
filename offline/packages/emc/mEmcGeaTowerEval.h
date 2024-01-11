#ifndef __MEMCGEATOWEREVAL_H__
#define __MEMCGEATOWEREVAL_H__

#include "table_header.h"

#include "dEmcGeometry.h"

#include "dEmcEvent.h"

#include "dEmcCalibTower.h"

#include "dEmcGeaTrack.h"

#include "dEmcGeaTowerTrack.h"

#include "dEmcGeaTowerEval.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcgeatowereval_(
       TABLE_HEAD_ST*, DEMCGEOMETRY_ST*,
       TABLE_HEAD_ST*, DEMCEVENT_ST*,
       TABLE_HEAD_ST*, DEMCCALIBTOWER_ST*,
       TABLE_HEAD_ST*, DEMCGEATRACK_ST*,
       TABLE_HEAD_ST*, DEMCGEATOWERTRACK_ST*,
       TABLE_HEAD_ST*, DEMCGEATOWEREVAL_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcGeaTowerEval_ memcgeatowereval_

#endif /*__MEMCGEATOWEREVAL_H__*/
