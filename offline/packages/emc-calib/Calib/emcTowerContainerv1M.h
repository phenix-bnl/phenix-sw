#ifndef __EMCTOWERCONTAINERV1M_H__
#define __EMCTOWERCONTAINERV1M_H__

#ifndef __EMCTOWERCONTAINERT_H__
#include "emcTowerContainerT.h"
#endif
#ifndef __EMCTOWERCONTENTV1M_H__
#include "emcTowerContentv1M.h"
#endif

/**@class emcTowerContainerv1M
(VERSION) Container of emcTowerContentv1M (merged towers).
Uses emcTowerContainerT as implementation base
@ingroup calibration
 */
typedef emcTowerContainerT<emcTowerContentv1M> emcTowerContainerv1M;

#endif
