#ifndef __MEMCGEAMAKERAW_H__
#define __MEMCGEAMAKERAW_H__

#include "table_header.h"

#include "header.h"

#include "dEmcGeaHit.h"

#include "dEmcGeaParams.h"

#include "dEmcRespPar.h"

#include "dEmcGeometry.h"

#include "dEmcGeaTrackTower.h"

#include "dEmcGeaTowerTrack.h"

#include "dEmcRawData.h"
#include "EmcGeaRawDataSimMaker.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcgeamakeraw_(
       float r_lowgain_convfac, float r_highgain_convfac,
       TABLE_HEAD_ST*, HEADER_ST*,
       TABLE_HEAD_ST*, DEMCGEAHIT_ST*,
       TABLE_HEAD_ST*, DEMCGEAPARAMS_ST*,
       TABLE_HEAD_ST*, DEMCRESPPAR_ST*,
       TABLE_HEAD_ST*, DEMCGEOMETRY_ST*,
       TABLE_HEAD_ST*, DEMCGEATRACKTOWER_ST*,
       TABLE_HEAD_ST*, DEMCGEATOWERTRACK_ST*,
       TABLE_HEAD_ST*, DEMCRAWDATA_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcGeaMakeRaw_ memcgeamakeraw_

#endif /*__MEMCGEAMAKERAW_H__*/
