// $Id: PHdiMuoTracksv14.h,v 1.1 2013/02/08 17:21:35 jinhuang Exp $                                                                                             
 
/*!
 * \file PHdiMuoTracksv14.h
 * \brief PHdiMuoTracksv14 is a container PHMuoTracksOut object for PHMuoTrackv14 and PHdiMuoTrackv5
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/02/08 17:21:35 $
 */



#ifndef PHDIMUOTRACKSV14_H_
#define PHDIMUOTRACKSV14_H_

#include "PHdiMuoTrackv5_Container.h"
#include "PHMuoTracksv14.h"

//! PHdiMuoTracksv14 is a container PHMuoTracksOut object for PHMuoTrackv14 and PHdiMuoTrackv5
typedef PHdiMuoTrackv5_Container<PHMuoTracksv14> PHdiMuoTracksv14;

#endif /* PHDIMUOTRACKSV14_H_ */
