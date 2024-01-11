// $Id: PHMuoTrackPrivate.h,v 1.1 2009/07/04 18:32:21 hpereira Exp $

#ifndef PHMuoTrackPrivate_h
#define PHMuoTrackPrivate_h

/*!
  \file    PHMuoTrackPrivate.h
  \brief   stores track information that must be hidden to root
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2009/07/04 18:32:21 $
*/

#include <assert.h>

/* 
Contains variables needed as members of PHMuoTrack object to easier event mixing.
however they must not get written to the MWG output root file since they are redundant with information
stored elsewhere and would therefore artificially increase the file size, for nothing.

This class can be updated at will without breaking backward compatibility
*/
class PHMuoTrackPrivate
{

  public:
  
  //! constructor
  PHMuoTrackPrivate( void );
  
  enum {
 
    //! max number of associated primitives
    _lvl2dim = 5,
    
    //! max number of reaction plane angles stored
    _rpdim = 3
    
  };
  
  //!@name associate level2 primitive angles
  //@{
  
  //! number of muid primitives
  unsigned int _n_primitives;
  
  //! phi
  float _level2_phi[_lvl2dim];
  
  //! theta
  float _level2_theta[_lvl2dim];
  
  //! number of mutr primitives
  unsigned int _n_mutr_primitives;
  
  //! min level2 momentum
  float _level2_pmin_x[_lvl2dim];
  float _level2_pmin_y[_lvl2dim];
  float _level2_pmin_z[_lvl2dim];
  
  //! max level2 momentum
  float _level2_pmax_x[_lvl2dim];
  float _level2_pmax_y[_lvl2dim];
  float _level2_pmax_z[_lvl2dim];
  
  //@}
  
  //!@name event vertex z and error
  //@{
  
  //! event vertex z (cm)
  float _event_vertex_z;

  //! event vertex z error (cm)
  float _event_vertex_z_error;
  
  //@}
  
  //! event reaction plane angle (north/south average)
  float _event_rp_angle[_rpdim];

};

#endif
