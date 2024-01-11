#ifndef __MMUTFINDTRACK_HH__
#define __MMUTFINDTRACK_HH__

// $Id: mMutFindTrack.h,v 1.38 2011/12/24 04:48:29 slash Exp $
//////////////////////////////////////////////////////////////////
/*!
  \file    mMutFindTrack.cxx
  \brief   Associate TMutCoord with TMutTrk
  \author  S.Kelly, H. Pereira
  \version $Revision: 1.38 $
  \date    $Date: 2011/12/24 04:48:29 $
*/
//////////////////////////////////////////////////////////////////

#include<PHTimeServer.h>
#include<TMutStubFinder.h>
#include<TMutTrkMap.h>
#include<TMutStubMap.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMuiRoadMapO.h>
#include<MUTOO.h>
#include<boost/array.hpp>

#include"mMutFindTrackPar.h"

// forward declaration
class mMutFindClus;
class mMutFitClus;
class mMutMatchCoord;
class mMutBPFit;
class mMutStubFit;
class TNtuple;

/*! \ingroup modules */

//! Pattern recognition module
//! Pattern recognition module
/*!
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutFindTrackPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutGapCoordMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
<td> TMutCoordMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
<td> TMutTrkMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
</table>
*/
class mMutFindTrack
{
 public: 

  mMutFindTrack(); 
  virtual ~mMutFindTrack(); 
  virtual PHBoolean event(PHCompositeNode*);
  void finish_evaluation();
  
 private:  
  
  // private methods
  
  //! get local pointers to needed nodes	 
  void set_interface_ptrs(PHCompositeNode* top_node);
 
  //! create track interface from muid roads and station3 stubs
  void start_tracks_muid();
	
  //! create tracks from station3 stubs
  void start_tracks_no_muid();
	
  //! propagate/update tracks in given station
  void find_in_station( const unsigned short& station );
	
  //! define stub search window in given station
  void define_window_in_station( TMutTrkMap::const_pointer trk_ptr, const unsigned short& station);
  
  //! find stubs matching given tracks in given station
  void find_stubs( TMutTrkMap::const_pointer trk_ptr, const unsigned short& arm, const unsigned short& station, const unsigned short& octant);
  
  //! update track with stubs found in given station
  void add_stubs_to_trk(TMutTrkMap::pointer trk_ptr, const unsigned short& station);
  
  //! dump
  void print_summary();
  
  //! dump
  void print_stub_summary();

  //! returns true if stub passes cuts
  bool stub_cuts(TMutStubFinder::stub_list::const_iterator stub_iter);
  
  //! returns true if muid passes cuts
  bool passed_muid_cuts(TMuiRoadMapO::const_pointer road_ptr) const;
  
  //! returns true if given stub is in search window
  bool check_window(TMutStubMap::const_pointer stub_ptr) const;
  
  //! evaluation ntuples initialization [debug]
  bool initialize_track_ntuples();
	
  //! write evaluation ntuples [debug]
  void write_track_ntuple(unsigned short station);

  //! associate stubs to existing gap coordinates
  void associate_stub_gap_coords();
	
  //! associate track to stubs, coordinates, gap coordinates
  void associate_trk(unsigned short station);
    
  //! print track-coord associations
  static void print_associations( TMutTrkMap::const_pointer );

  //! create a new track from existing track, append stub to the created track (biffurcation)
  void clone_trk(TMutTrkMap::pointer in_trk_ptr,  TMutStubMap::pointer in_stub_ptr);

  //!@name local reconstruction modules
  //@{
  //! cluster finder
  mMutFindClus* _mMutFindClus_mod;

  //! cluster fit
  mMutFitClus* _mMutFitClus_mod;

  //! gap coordinates
  mMutMatchCoord* _mMutMatchCoord_mod;
 
  //! Stub fit module
  mMutStubFit* _mMutStubFit_mod;
  
  //! bent plane fit module
  mMutBPFit* _mMutBPFit_mod;
  //@}
  
  //! local stub finder algorithm
  TMutStubFinder _stub_finder;
	
  //! stub searching theta window
  TMutStubFinder::stub_window _theta_window;
  
  //! stub searching phi window
  TMutStubFinder::stub_window _phi_window;
	
  //! internal list of stub candidates
  std::list<TMutStubFinder::Stub> _master_list;

  //! main node
  PHCompositeNode* _top_node;

  //! parameter table
  const mMutFindTrackPar* _mod_par; 
  
  //! gap coordinate map
  TMutGapCoordMap* _gap_coord_map;
  
  //! coordinate map
  TMutCoordMap* _coord_map;
  
  //! stup map
  TMutStubMap* _stub_map;
  
  //! track map
  TMutTrkMap* _trk_map;
  
  //! road map
  TMuiRoadMapO* _road_mapO;

  //! Timer
  PHTimeServer::timer _timer;
  
  /*! \brief exception array
    when exception occurs in given arm, octant, the corresponding bit
    is set to true and the debug output is disabled.
  */
  boost::array<bool,MUTOO::NumberOfArms*MUTOO::NumberOfOctants> _exception_pattern;
  
  /*! \brief stores the number of tracks found in each arm. Used to throw exception
    when one arm limit on number of tracks is reached.
  */
  boost::array<size_t, MUTOO::NumberOfArms> _n_tracks;

  //! keep track of octants for which (clusters and) stubs has been looked after 
  /*! there is one mask per arm. Each mask is a bitwise or of fired octants */
  boost::array<unsigned int, MUTOO::NumberOfArms> _octant_mask;
  
  //! evaluation output filename
  std::string _filename;
  
  //! Ntuple Output
  TNtuple* _created_tracks;
  
};

#endif /* __MMUTFINDTRACK_HH__ */
