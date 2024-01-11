// $Id: mMutBPFit.h,v 1.12 2007/05/30 15:53:55 hpereira Exp $

/*!
  \file    mMutBPFit.h
  \brief   Bend Plane track fit module
  \author  S. Kelly
  \version $Revision: 1.12 $
  \date    $Date: 2007/05/30 15:53:55 $
*/

#ifndef __MMUTBPFIT_HH__
#define __MMUTBPFIT_HH__

#include<PHTimeServer.h>
#include<TMutTrkMap.h>
#include<mMutBPFitPar.h>
#include<PHPoint.h>
#include<boost/array.hpp>

class PHCompositeNode;

/*! \ingroup modules */
//! Bend Plane Fitter
/*!

MUTR Bend Plane Fitter

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutBPFitPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
</table>
*/

class mMutBPFit
{
 public: 

  mMutBPFit(); 
  virtual ~mMutBPFit(){}
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  
  
  // private methods
  
  //! track fit mode 
  enum trk_fit_mode 
  {
    //! use stations 2 and 3 only
    STA_23, 
    
    //! use station 1, 2 and 3  
    STA_123
  };

  //! get local pointer to needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! loop over tracks to perform fit
  void track_loop(void);
  
  //! fit using station 1, 2 and 3, and possibly vertex and muid
  void fit_track123(TMutTrkMap::pointer);
  
  //! fit using station 2 and 3
  void fit_track23(TMutTrkMap::pointer);
  
  //! fit using station 2 and 3 and possibly vertex
  void fit_track23_vertex(TMutTrkMap::pointer);

  //! check if a given track can be fitted with the proper mode
  bool chk_trk_fit_mode(TMutTrkMap::pointer, const trk_fit_mode& mode );
  
  //! module parameters
  const mMutBPFitPar* _mod_par;
  
  //! pointer to map of tracks
  TMutTrkMap* _trk_map;

  //! module timer
  PHTimeServer::timer _timer;
  
  //! data structure used for bent plane fit
  struct fit_data 
  {
    
    //! transformation vector (from track parameters to detector measurement)
    boost::array<double,5> cf;
    
    //! measurement z
    double z;
    
    //! measurement
    double w_meas;
    
    //! error on measurement
    double w_error;
    
  };

  //! true if external vertex is to be included in the fit
  bool _ext_vtx_flag;     
  
  //! external vertex position
  PHPoint _ext_vtx;
  
};

#endif /* __MMUTBPFIT_HH__ */









