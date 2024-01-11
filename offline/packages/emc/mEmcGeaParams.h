#ifndef __MEMCGEAPARAMS_H__
#define __MEMCGEAPARAMS_H__

#include "table_header.h"

#include "emcpar.h"

#include "dEmcGeaParams.h"

#include "dEmcGeometry.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long memcgeaparams_(
       TABLE_HEAD_ST*, EMCPAR_ST*,
       TABLE_HEAD_ST*, DEMCGEAPARAMS_ST*,
       TABLE_HEAD_ST*, DEMCGEOMETRY_ST*
               );
#ifdef __cplusplus
}
#endif
#define mEmcGeaParams_ memcgeaparams_

#endif /*__MEMCGEAPARAMS_H__*/
