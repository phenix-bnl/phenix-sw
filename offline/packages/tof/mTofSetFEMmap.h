#ifndef __MTOFSETFEMMAP_H__
#define __MTOFSETFEMMAP_H__

#include "table_header.h"

#include "dTofGeo.h"

#include "dTofFEMmap.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mtofsetfemmap_(
       TABLE_HEAD_ST*, DTOFGEO_ST*,
       TABLE_HEAD_ST*, DTOFFEMMAP_ST*
               );
#ifdef __cplusplus
}
#endif
#define mTofSetFEMmap_ mtofsetfemmap_

#endif /*__MTOFSETFEMMAP_H__*/
