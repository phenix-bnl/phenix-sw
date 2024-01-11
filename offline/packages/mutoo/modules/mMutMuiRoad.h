// $Id: mMutMuiRoad.h,v 1.10 2007/05/21 16:57:11 hpereira Exp $

//////////////////////////////////////////////////////////////////
/*! 
  \file mMutMuiRoad.h
  \brief road to track association based on proximity at first gap
  \author	Chun Zhang
  \version $Revision: 1.10 $
  \date		$Date: 2007/05/21 16:57:11 $
*/
//////////////////////////////////////////////////////////////////

#ifndef __MMUTMUIROAD_H__
#define __MMUTMUIROAD_H__

// MUTOO Headers
#include<PHTimeServer.h>
#include<TMuiRoadMapO.h>
#include<TMutTrkMap.h>
#include<TMutTrkPar.hh>

// PHENIX Headers
#include<PHPoint.h>

// STL
#include <map>

class mMutMuiRoadPar;
class PHCompositeNode;

/*! 
  \class mMutMuiRoad
  \brief road to track association based on proximity at first gap
  \author	Chun Zhang
  \version $Revision: 1.10 $
  \date		$Date: 2007/05/21 16:57:11 $
*/
class mMutMuiRoad
{
 public:
  
  //! constructor
  mMutMuiRoad();
  
  //! destructor
  virtual ~mMutMuiRoad(){}
  
  //! event method
  virtual PHBoolean event(PHCompositeNode*);
  
 private:
  
  //! get local copy of external nodes
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! associate track with best matching road for each depth
  void associate_muioo();
  
  //! shortcut for track to road association map
  typedef std::map< double, TMuiRoadMapO::pointer > association_map;
  
  //! shortcut for track to road association map
  typedef std::pair< double, TMuiRoadMapO::pointer > association_pair;
  
  //! get best matching road for a given depth
  static TMuiRoadMapO::pointer select_roadO(
					    TMutTrkMap::pointer trk_ptr, 
					    const association_map& associations, 
					    int depth);
  
  //! calculate proximity from track parameters to muid point
  double get_proximity( TMutTrkMap::pointer trk_ptr , TMuiRoadMapO::pointer muid_road ) const;
  
  //! dump tracks to road associations
  void dump_associations( void ) const;
  
  //! check if track is accepted. Do nothing if not
  bool accept_track( TMutTrkMap::const_pointer trk_ptr ) const;

  //! Timer
  PHTimeServer::timer _timer;		

  //! pointer to mutr track map
  TMutTrkMap* _trk_map;		
  
  //! pointer to module parameters
  mMutMuiRoadPar* _mod_par;	 
  
  //! pointer to muid road map
  TMuiRoadMapO* _road_mapO;	 
  
};

#endif /* __MMUTMUIROAD_HH__ */
