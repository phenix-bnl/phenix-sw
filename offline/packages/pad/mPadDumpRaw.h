#ifndef __MPADDUMPRAW_H__
#define __MPADDUMPRAW_H__
/* Automatically generated.  Do not edit. */

#include "table_header.h"

#include "dPadRaw.h"

#include "dPadGhitRaw.h"

#include "dPadFEMPar.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mpaddumpraw_(
       TABLE_HEAD_ST*, DPADRAW_ST*,
       TABLE_HEAD_ST*, DPADGHITRAW_ST*,
       TABLE_HEAD_ST*, DPADFEMPAR_ST*
               );
#ifdef __cplusplus
}
#endif
#define mPadDumpRaw_ mpaddumpraw_

#endif /*__MPADDUMPRAW_H__*/
