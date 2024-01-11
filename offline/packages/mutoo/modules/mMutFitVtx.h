#ifndef __mMutFitVtx_H__
#define __mMutFitVtx_H__

// $Id: mMutFitVtx.h,v 1.14 2010/01/22 16:37:07 hpereira Exp $
/*!
  \file    mMutFitVtx.h
  \brief   Vertex Reco and Fit via linearized chisquare analytic minimisation
  \author  Hugo Pereira
  \version $Revision: 1.14 $
  \date    $Date: 2010/01/22 16:37:07 $
*/

#include <MUTOO.h>
#include <boost/array.hpp>
#include <PHGslMatrix.h>
#include <PHTimeServer.h>
#include <TMutTrkMap.h>
#include <TMutVtxMap.h>
#include <TTree.h>
#include <TMutVertexFit.h>

#include "mMutFitVtxPar.h"

class PHCompositeNode;

/*! \ingroup modules */
//! vertex fit module
/*!
<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutKalFitPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutTrkMap*</td>
<td> Track IOC</td>
<td> mutable </td>
</table>
*/

class mMutFitVtx
{

  public:

  //! constructor
  mMutFitVtx( void );

  //! destructor
  virtual ~mMutFitVtx( void )
  {}

  //! event method
  virtual PHBoolean event(PHCompositeNode*);

  //! Vertex fitter
  /*! the class is derive to add tracks to the vertex fit from TMutTrk objects */
  class Fitter: public TMutVertexFit
  {
    public:

    //! constructor
    Fitter( void )
    {}

    //! destructor
    virtual ~Fitter( void )
    {}

    //! add track to vertex fig
    void add_track( TMutTrkMap::const_pointer );

  };

  private:

  //! make local copy of needed nodes
  void set_interface_ptrs(PHCompositeNode*);

  //! module parameters
  mMutFitVtxPar* _mod_par;

  //! vertex map
  TMutVtxMap* _vtx_map;

  //! track map
  TMutTrkMap* _trk_map;

  //! Loop over  double track vertices
  void vertex_loop();

  //! double track vertex fit
  void fit_vertex( TMutVtxMap::pointer vtx_ptr );

  //! Loop over  double track vertices
  void track_loop();

  //! single track vertex fit
  void fit_track( TMutTrkMap::pointer );

  //! retrieve total chi_square from nodes
  double get_tot_chi_square( void );

  //! check if track are accepted for single track fit
  bool accept_track( TMutTrkMap::const_pointer trk_ptr ) const;

  //! Timer
  PHTimeServer::timer _timer;

};

#endif
