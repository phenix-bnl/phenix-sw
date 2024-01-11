// $Id: mMutFindTrackMC.h,v 1.7 2006/01/14 04:29:42 pinkenbu Exp $
#ifndef __MMUTFINDTRACKMC_HH__
#define __MMUTFINDTRACKMC_HH__
/*!
  \file    mMutFindTrackMC.h
  \brief   Associate TMutCoord with TMutTrk using monte-carlo information (perfect pattern recognition)
  \author  S.Kelly
  \version $Revision: 1.7 $
  \date    $Date: 2006/01/14 04:29:42 $
*/

#include<PHTimeServer.h>
#include<vector>
#include<TMutTrkMap.h>
#include<boost/array.hpp>

class mMutFindTrackMCPar;
class PHCompositeNode;
class TMutMCTrkMap;
class TMutMCHitMap;
class TMutStubMap;
class TMutVtxMap;
class TMutCoordMap;
class TMutClusMap;
class TMutHitMap;

/*! \ingroup modules */

//! Perfect pattern recognition from monte-carlo data.

/*!  
  Initializes TMutTrk objects using monte-carlo data in TMutMCTr
  Associates TMutCoord with TMutTrk using TMutMCHit data.  This 
  module implements perfect pattern recognition using monte-carlo
  data. <br>

  <h2>Analysis Module Interface Specification</h2>
  <table>
  <tr>
  <td> Object </td>
  <td> Description </td>
  <td> Privilege </td>
  </tr>
  <tr>
  <td> const mMutFindTrackMCPar*</td>
  <td> Parameter Table </td>
  <td> immutable </td>
  </tr>
  <tr>
  <td> TMutMCTrk*</td>
  <td> IOC</td>
  <td> mutable </td>
  </tr>
  <tr>
  <td> TMutMCHit*</td>
  <td> IOC</td>
  <td> mutable </td>
  </tr>
  <tr>
  <td> TMutTrk*</td>
  <td> IOC</td>
  <td> mutable </td>
  </tr>
  </table>
*/


class mMutFindTrackMC
{
 public: 

  mMutFindTrackMC(); 
  virtual ~mMutFindTrackMC(){}
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  
  
  // private methods
  //
  void set_interface_ptrs(PHCompositeNode* top_node);

  void find_tracks();

  void find_stubs();

  void find_vtx();

  void promote_associations();  

  void set_reco_vtx_pars();

  double energy_loss(TMutTrkMap::pointer);

  double calc_p_vertex(const boost::array<double,4>& pars, double p_trk);
  
  // Interface pointers
  //
  const mMutFindTrackMCPar* _mod_par;        // parameter table
  TMutMCTrkMap* _mc_trk_map;                 // IOC
  TMutMCHitMap* _mc_hit_map;                 // IOC
  TMutTrkMap* _trk_map;			     // IOC
  TMutVtxMap* _vtx_map;			     // IOC
  TMutStubMap* _stub_map;                    // IOC
  TMutCoordMap* _coord_map;		     // IOC
  TMutClusMap* _clus_map;		     // IOC
  TMutHitMap* _hit_map;			     // IOC
  
  // Timer
  //
  PHTimeServer::timer _timer;
};

#endif /* __MMUTFINDTRACKMC_HH__ */














