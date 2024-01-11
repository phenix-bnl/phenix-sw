// $Id: PHdiMuoTracksv12.h,v 1.1 2013/02/08 17:21:35 jinhuang Exp $                                                                                             
 
/*!
 * \file PHdiMuoTracksv12.h
 * \brief PHdiMuoTracksv12 is a container PHMuoTracksOut object for PHMuoTrackv12 and PHdiMuoTrackv5
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/02/08 17:21:35 $
 */



#ifndef PHDIMUOTRACKSV12_H_
#define PHDIMUOTRACKSV12_H_

#include "PHdiMuoTrackv5_Container.h"
#include "PHMuoTracksv12.h"

//! PHdiMuoTracksv12 is a container PHMuoTracksOut object for PHMuoTrackv12 and PHdiMuoTrackv5
typedef PHdiMuoTrackv5_Container<PHMuoTracksv12> PHdiMuoTracksv12;

#endif /* PHDIMUOTRACKSV12_H_ */
