#ifndef __MTOFASSOCPID_H__
#define __MTOFASSOCPID_H__
/* Automatically generated.  Do not edit. */

#include "table_header.h"
#include "PAM.h"

#include "dTofPidPar.h"

#include "dTofAssociate.h"

#include "dTofPid.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mtofassocpid_(
       TABLE_HEAD_ST*, DTOFPIDPAR_ST*,
       TABLE_HEAD_ST*, DTOFASSOCIATE_ST*,
       TABLE_HEAD_ST*, DTOFPID_ST*
               );
#ifdef __cplusplus
}
#endif
#define mTofAssocPid_ mtofassocpid_

#endif /*__MTOFASSOCPID_H__*/
