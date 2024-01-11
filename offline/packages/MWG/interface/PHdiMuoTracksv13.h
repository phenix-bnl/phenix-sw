// $Id: PHdiMuoTracksv13.h,v 1.1 2013/02/08 17:21:35 jinhuang Exp $                                                                                             
 
/*!
 * \file PHdiMuoTracksv13.h
 * \brief PHdiMuoTracksv13 is a container PHMuoTracksOut object for PHMuoTrackv13 and PHdiMuoTrackv5
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/02/08 17:21:35 $
 */



#ifndef PHDIMUOTRACKSV13_H_
#define PHDIMUOTRACKSV13_H_

#include "PHdiMuoTrackv5_Container.h"
#include "PHMuoTracksv13.h"

//! PHdiMuoTracksv13 is a container PHMuoTracksOut object for PHMuoTrackv13 and PHdiMuoTrackv5
typedef PHdiMuoTrackv5_Container<PHMuoTracksv13> PHdiMuoTracksv13;

#endif /* PHDIMUOTRACKSV13_H_ */
