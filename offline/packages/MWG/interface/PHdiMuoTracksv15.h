// $Id: PHdiMuoTracksv15.h,v 1.1 2013/02/12 22:55:32 jinhuang Exp $
 
/*!
 * \file PHdiMuoTracksv15.h
 * \brief PHdiMuoTracksv15 is a container PHMuoTracksOut object for PHMuoTrackv15 and PHdiMuoTrackv5
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/02/12 22:55:32 $
 */



#ifndef PHDIMUOTRACKSV15_H_
#define PHDIMUOTRACKSV15_H_

#include "PHdiMuoTrackv5_Container.h"
#include "PHMuoTracksv15.h"

//! PHdiMuoTracksv15 is a container PHMuoTracksOut object for PHMuoTrackv15 and PHdiMuoTrackv5
typedef PHdiMuoTrackv5_Container<PHMuoTracksv15> PHdiMuoTracksv15;

#endif /* PHDIMUOTRACKSV15_H_ */
