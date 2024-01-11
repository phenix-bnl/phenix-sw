#ifndef __MTOFPID_H__
#define __MTOFPID_H__
/* Automatically generated.  Do not edit. */

#include "table_header.h"
#include "PAM.h"

#include "dTofGeoPar.h"

#include "dTofAssocPar.h"

#include "dTofPidPar.h"

#include "dTofReconstructed.h"

#include "dBbcOut.h"

#include "dCglParticle.h"

#include "dCglTrack.h"

#include "dDchTracks.h"

#include "dPadCluster.h"

#include "dPadCluster.h"

#include "dPadCluster.h"

#include "dTecTrack.h"

#include "dTofPid.h"

#ifdef __cplusplus
extern "C"
{
#endif
  long mtofpid_(
       TABLE_HEAD_ST*, DTOFGEOPAR_ST*,
       TABLE_HEAD_ST*, DTOFASSOCPAR_ST*,
       TABLE_HEAD_ST*, DTOFPIDPAR_ST*,
       TABLE_HEAD_ST*, DTOFRECONSTRUCTED_ST*,
       TABLE_HEAD_ST*, DBBCOUT_ST*,
       TABLE_HEAD_ST*, DCGLPARTICLE_ST*,
       TABLE_HEAD_ST*, DCGLTRACK_ST*,
       TABLE_HEAD_ST*, DDCHTRACKS_ST*,
       TABLE_HEAD_ST*, DPADCLUSTER_ST*,
       TABLE_HEAD_ST*, DPADCLUSTER_ST*,
       TABLE_HEAD_ST*, DPADCLUSTER_ST*,
       TABLE_HEAD_ST*, DTECTRACK_ST*,
       TABLE_HEAD_ST*, DTOFPID_ST*
               );
#ifdef __cplusplus
}
#endif
#define mTofPid_ mtofpid_

#endif /*__MTOFPID_H__*/
