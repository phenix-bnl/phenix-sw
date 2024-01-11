#ifndef __MWGVertex_h__
#define __MWGVertex_h__

// $Id: MWGVertex.h,v 1.2 2013/11/10 16:16:08 slash Exp $

/*!
  \file    MWGVertex.h
  \brief   vertex fit from NanoDST tracks and event vertex information
  \author  Hugo Pereira
  \version $Revision: 1.2 $
  \date    $Date: 2013/11/10 16:16:08 $
*/

#include "TMutVertexFit.h"

// forward declaration
class PHMuoTracksOut;
class TMutTrkPar;

//! nanoDST vertex fit
/*! 
it is an implementation of mutoo/base/TMutVertexFit that adds nodes 
constructed from the PHMuoTracksOut object. All the mathematics are 
performed in the base class
*/
class MWGVertex: public TMutVertexFit
{

  public:
  
  //! constructor
  MWGVertex( void )
  {}
  
  //! destructor
  virtual ~MWGVertex( void )
  {}
  
  //! add a track to the list of tracks entering the vertex
  /*! 
  for each track an internal node is created from the base class
  and used for the fit. This is the only method to implement
  on top of the base class methods.
  */
  void add_track( int imu, PHMuoTracksOut* muo);

  /*!
    add a track using the tracking parameter.
   */
  void add_track( TMutTrkPar* trk_par );
  
};

#endif
