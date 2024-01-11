// $Id: mFvtxFindTrackMC.h,v 1.5 2013/06/01 10:55:54 keyaaron Exp $
#ifndef __mFvtxFindTrackMC_HH__
#define __mFvtxFindTrackMC_HH__
/*!
  \file    mFvtxFindTrackMC.h
  \brief   Associate TFvtxCoord with TFvtxTrk using monte-carlo information (perfect pattern recognition)
  \author  Melynda Brooks
  \version $Revision: 1.5 $
  \date    $Date: 2013/06/01 10:55:54 $
*/

#include<PHTimeServer.h>
#include<vector>
#include<TFvtxTrkMap.h>
#include<boost/array.hpp>

#include <PHGslRng.h>

#include <mFvtxModuleBase.h>

class mFvtxFindTrackMCPar;
class PHCompositeNode;
class TFvtxClusMap;
class TFvtxCoordMap;
class TFvtxHitMap;
class TFvtxMCHitMap;
class TFvtxPisaHitMap;
class TFvtxTrkMap;
class TMutMCTrkMap;

/*! \ingroup modules */

//! Perfect pattern recognition from monte-carlo data.

/*!  
  Initializes TFvtxTrk objects using monte-carlo data in TFvtxMCTr
  Associates TFvtxCoord with TFvtxTrk using TFvtxMCHit data.  This 
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
  <td> const mFvtxFindTrackMCPar*</td>
  <td> Parameter Table </td>
  <td> imFvtxable </td>
  </tr>
  <tr>
  <td> TFvtxTrk*</td>
  <td> IOC</td>
  <td> Fvtxable </td>
  </tr>
  <tr>
  <td> TFvtxMCHit*</td>
  <td> IOC</td>
  <td> Fvtxable </td>
  </tr>
  <tr>
  <td> TFvtxTrk*</td>
  <td> IOC</td>
  <td> Fvtxable </td>
  </tr>
  </table>
*/


class mFvtxFindTrackMC : public mFvtxModuleBase
{
  public: 

  mFvtxFindTrackMC(); 
  virtual ~mFvtxFindTrackMC(){}
  virtual PHBoolean event(PHCompositeNode*);
  void init(PHCompositeNode*){};
  void init_run(PHCompositeNode*){};
  void end(PHCompositeNode*){};
  
  private:  
  
  // private methods
  //! retrieve pointer to needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! find tracks
  void find_tracks();

  //! associations between hits and tracks
  void promote_associations();  

  //!@name Interface pointers
  //@{

  //! module parameters
  const mFvtxFindTrackMCPar* _mod_par; 
  
  //! mc tracks
  TMutMCTrkMap* _mc_trk_map; 
  
  //! pisa barrel hits
  TFvtxPisaHitMap* _pisa_hit_map;
  
  //! MC hits
  TFvtxMCHitMap* _mc_hit_map; 
  
  //! tracks
  TFvtxTrkMap* _trk_map; 
  
  //! coordinates
  TFvtxCoordMap* _coord_map; 
  
  //! clusters
  TFvtxClusMap* _clus_map; 
  
  //! hits
  TFvtxHitMap* _hit_map; 
  //@}
  
  //! Random number generator
  PHGslRng _rng;
  
  //! module Timer
  PHTimeServer::timer _timer;

};

#endif /* __mFvtxFindTrackMC_HH__ */














