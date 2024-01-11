#ifndef __MEMCRAWTOFEM_H__
#define __MEMCRAWTOFEM_H__

#include "table_header.h"

#include "dEmcRawData.h"

#include "dEmcFEMData.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcrawtofem_(
       TABLE_HEAD_ST*, DEMCRAWDATA_ST*,
       TABLE_HEAD_ST*, DEMCFEMDATA_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcRawToFEM_ memcrawtofem_

#endif /*__MEMCRAWTOFEM_H__*/
