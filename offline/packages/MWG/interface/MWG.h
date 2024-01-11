
// $Id: MWG.h,v 1.4 2013/02/08 17:21:34 jinhuang Exp $
#ifndef __MWG_H__
#define __MWG_H__

/*!
	\file MWG.h	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
  \version $Revision: 1.4 $
  \date    $Date: 2013/02/08 17:21:34 $
*/

#include<string>
#include<iostream>

#include <PHMuoTracksOut.h>

//! widely used utility functions and enumerations
namespace MWG {

  //! default array sizes
  /*! it is used in the constructor of the PHMuoTracks and PHdiMuoTracks objects */
  enum
  {
    //! default single muon array size
    MU_ARRAY_SIZE = 0,
      
    //! default single muon array size
    DIMU_ARRAY_SIZE = 0
  
  };
  
  //! muon mass square
  const double MASS_MUON_SQUARE = 0.011163695;
 
  //! muon mass
  const float MUMASS=0.1056583568;
  
  //! returns pair mass assuming muons
  float get_mass( 
    float px1, float py1, float pz1,
    float px2, float py2, float pz2 );
  
  //! returns new instance of latest PHMuoTracks version
  PHMuoTracksOut* newPHMuoTracks( void );

  //! returns new instance of latest PHMuoTracks version for Fvtx
  PHMuoTracksOut* newPHMuoTracksFvtx( void );
  
  //! returns new instance of latest PHdiMuoTracks version
  PHMuoTracksOut* newPHdiMuoTracks( void );
  
  //! returns new instance of latest PHdiMuoTracks version for Fvtx
  PHMuoTracksOut* newPHdiMuoTracksFvtx( void );

};

#endif
