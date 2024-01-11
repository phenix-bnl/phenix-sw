#ifndef __MBBCGHITRAW_H__
#define __MBBCGHITRAW_H__
/* Automatically generated.  Do not edit. */

#include <table_header.h>

#include "dBbcGhitRawPar.h"

#include "dBbcGeo.h"

#include "dBbcUcal.h"

#include "bbcghit.h"

#include "dBbcGhitRaw.h"

#include "dBbcRaw.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mbbcghitraw_(
       TABLE_HEAD_ST*, DBBCGHITRAWPAR_ST*,
       TABLE_HEAD_ST*, DBBCGEO_ST*,
       TABLE_HEAD_ST*, DBBCUCAL_ST*,
       TABLE_HEAD_ST*, BBCGHIT_ST*,
       TABLE_HEAD_ST*, DBBCGHITRAW_ST*,
       TABLE_HEAD_ST*, DBBCRAW_ST*
               );
#ifdef __cplusplus
}
#endif
#define mBbcGhitRaw_ mbbcghitraw_

#endif /*__MBBCGHITRAW_H__*/
