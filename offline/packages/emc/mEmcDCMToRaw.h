#ifndef __MEMCDCMTORAW_H__
#define __MEMCDCMTORAW_H__


#include "table_header.h"

#include "dEmcDCMData.h"

#include "dEmcRawData.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcdcmtoraw_(
       TABLE_HEAD_ST*, DEMCDCMDATA_ST*,
       TABLE_HEAD_ST*, DEMCRAWDATA_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcDCMToRaw_ memcdcmtoraw_

#endif /*__MEMCDCMTORAW_H__*/
