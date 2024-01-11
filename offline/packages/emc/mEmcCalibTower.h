#ifndef __MEMCCALIBTOWER_H__
#define __MEMCCALIBTOWER_H__

#include "table_header.h"

#include "dEmcRawData.h"

#include "dEmcGeometry.h"

#include "dEmcEvent.h"

#include "dEmcCalibTower.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memccalibtower_(
       TABLE_HEAD_ST*, DEMCRAWDATA_ST*,
       TABLE_HEAD_ST*, DEMCGEOMETRY_ST*,
       TABLE_HEAD_ST*, DEMCEVENT_ST*,
       TABLE_HEAD_ST*, DEMCCALIBTOWER_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcCalibTower_ memccalibtower_

#endif /*__MEMCCALIBTOWER_H__*/
