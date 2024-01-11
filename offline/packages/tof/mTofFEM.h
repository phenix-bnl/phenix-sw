#ifndef __MTOFFEM_H__
#define __MTOFFEM_H__

#include "table_header.h"

#include "dTofRaw.h"

#include "dTofGhitRaw.h"

#include "dTofFEMmap.h"

#include "dTofFEM.h"

#include "dTofFEMhitGhit.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mtoffem_(
       TABLE_HEAD_ST*, DTOFRAW_ST*,
       TABLE_HEAD_ST*, DTOFGHITRAW_ST*,
       TABLE_HEAD_ST*, DTOFFEMMAP_ST*,
       TABLE_HEAD_ST*, DTOFFEM_ST*,
       TABLE_HEAD_ST*, DTOFFEMHITGHIT_ST*
               );
#ifdef __cplusplus
}
#endif
#define mTofFEM_ mtoffem_

#endif /*__MTOFFEM_H__*/
