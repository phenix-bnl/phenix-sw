#ifndef __EMCTOWERCONTAINERV1S_H__
#define __EMCTOWERCONTAINERV1S_H__

#ifndef __EMCTOWERCONTAINERT_H__
#include "emcTowerContainerT.h"
#endif
#ifndef __EMCTOWERCONTENTV1S_H__
#include "emcTowerContentv1S.h"
#endif

/**@class emcTowerContainerv1S
(VERSION) Container of emcTowerContentv1S (simulated towers).
Uses emcTowerContainerT as implementation base.
@ingroup calibration
*/

typedef emcTowerContainerT<emcTowerContentv1S> emcTowerContainerv1S;

#endif
