#ifndef __MEMCRAWTOLONG_H__
#define __MEMCRAWTOLONG_H__

#include "table_header.h"

#include "dEmcRawData.h"

#include "dEmcDCMLongData.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcrawtolong_(
       TABLE_HEAD_ST*, DEMCRAWDATA_ST*,
       TABLE_HEAD_ST*, DEMCDCMLONGDATA_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcRawToLong_ memcrawtolong_

#endif /*__MEMCRAWTOLONG_H__*/
