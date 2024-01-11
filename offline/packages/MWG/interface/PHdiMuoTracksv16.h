// $Id: PHdiMuoTracksv16.h,v 1.1 2014/01/05 06:46:33 slash Exp $
 
/*!
 * \file PHdiMuoTracksv16.h
 * \brief PHdiMuoTracksv16 is a container PHMuoTracksOut object for PHMuoTrackv16 and PHdiMuoTrackv5
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2014/01/05 06:46:33 $
 */



#ifndef PHDIMUOTRACKSV16_H_
#define PHDIMUOTRACKSV16_H_

#include "PHdiMuoTrackv5_Container.h"
#include "PHMuoTracksv16.h"

//! PHdiMuoTracksv16 is a container PHMuoTracksOut object for PHMuoTrackv16 and PHdiMuoTrackv5
typedef PHdiMuoTrackv5_Container<PHMuoTracksv16> PHdiMuoTracksv16;

#endif /* PHDIMUOTRACKSV16_H_ */
