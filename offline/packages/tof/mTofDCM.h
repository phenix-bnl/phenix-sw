#ifndef __MTOFDCM_H__
#define __MTOFDCM_H__

#include "table_header.h"

#include "dTofFEM.h"

#include "dTofDCMPar.h"

#include "dTofDCM.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mtofdcm_(
       TABLE_HEAD_ST*, DTOFFEM_ST*,
       TABLE_HEAD_ST*, DTOFDCMPAR_ST*,
       TABLE_HEAD_ST*, DTOFDCM_ST*
               );
#ifdef __cplusplus
}
#endif
#define mTofDCM_ mtofdcm_

#endif /*__MTOFDCM_H__*/
