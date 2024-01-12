// $Id: MWGCuts.h,v 1.9 2008/04/21 20:19:15 hpereira Exp $

/*!
\file		MWGCuts.h
\brief	 handle cuts on nanoDSTs to fill picoDSTs
\author	Hugo Pereira
\version $Revision: 1.9 $
\date		$Date: 2008/04/21 20:19:15 $
*/

#ifndef __MWGCuts_h__
#define __MWGCuts_h__

#include <TObject.h>
#include <set>

class PHGlobal;
class PHMuoTracksOut;
class PHCompositeNode;

//! handle cuts on nanoDSTs to fill picoDSTs
class MWGCuts: public TObject
{
  
  public:
  
  //! constructor
  MWGCuts( void );
  
  //! destructor
  virtual ~MWGCuts( void )
  {}
  
  //! set all cuts to false
  virtual void reset( void )
  { _cuts.clear(); }
  
  //! initialize cuts using keyword
  virtual void init( const char* selection = "nocuts" );
  
  //! print all cuts
  void print( void ) const;
  
  //! returns true if current event pass selection cuts
  virtual bool pass_event_cuts(PHCompositeNode *topNode ) const;	
  
  //! returns true if current dimuon pass selection cuts	
  virtual bool pass_dimuon_cuts(PHGlobal* global, int idimu, PHMuoTracksOut* muo, const int& framework ) const; 
  
  //! returns true if current muon pass selection cuts
  virtual bool pass_single_muon_cuts(PHGlobal* global, int imu, PHMuoTracksOut* muo, const int& framework) const;	 
  
  //! cuts enumeration
  enum Cut
  {
    //! cut on BBC z
    BBC_Z,
      
    //! cut on centrality
    CENTRALITY,
    
    //! check bbc zd
    BBCZDC,
    
    //! cut on muid multiplicity vs BBC charge
    MUID_BBC,
    
    //! cut on dimuon road panels being different
    IDQUAD,
    
    //! cut on DCA vertex z wrt BBC z
    DIMU_VTX,
    
    //! cut on vertex mass
    DIMU_MASS,
    
    //! cut on vertex chisquare
    VTX_CHISQUARE,
    
    //! cut on dimuon rapidity in south arm
    Y_DIMU_SOUTH,
    
    //! cut on dimuon rapidity in south arm
    Y_DIMU_NORTH,
    
    /*!
    cut on L2 trigger decision.
    only dimuons in the arm which fires level2 are kept.
    NOTE: for an arm to fire the level2 requirement, at least one dimuon
    must fullfill the level2 requirement. As opposed to the LEVEL2_DIMU
    cut for which we require that _all_ dimuons fullfill the requirements.
    */
    LEVEL2,
    
    /*!
    cut on emulated L2 trigger decision for each dimuon
    only offline dimuons which pass the level2 emulator requirements are kept.
    NOTE: Level2 requirements are checked for the offline reconstructed
    dimuons, as opposet to the LEVEL2 cuts which is based on level2 
    fast_reconstructed roads
    */
    LEVEL2_DIMU,
    
    /*!
    cut on L2 trigger decision for each dimuon
    are kept only offline dimuons for which a valid L2 muid primitives are found 
    NOTES: 
    1/ Level2 requirements are checked on level2 primitives associated to the 
    pair muid roads.
    2/ This cuts is applied both at the single muon and dimuon level
    - at single muon level, only tracks for which a valid muid primitive is found (deep, 8hist, slope>12deg) are kept
    - at dimuon level, only pairs matching the single muon cuts + for which valid muid primitives with opening angle > 19.2 deg are kept
    */
    LEVEL2_PRIMITIVES,
    
    //! cut on track ghost flag
    GHOST_FLAG,
    
    //! cut on track chisquare
    TRK_CHISQUARE,
    
    //! cut on track pz in south arm
    TRK_PZ_SOUTH,
    
    //! cut on track pz in north arm
    TRK_PZ_NORTH,
    
    //! cut on track number of mutr hits
    MUTR_HITS,
    
    //! cut on track distance to vertex (at vertex z)
    MUTR_RV,
    
    //! cut on number of hits in station 3
    ST3_HITS,
    
    //! cut on track number of muid hits
    MUID_HITS,
    
    //! cut on road depth
    MUID_DEPTH,
    
    //! cut on road horizontal/vertical missing tubes
    MUID_TUBES,
    
    //! cut on missing tubes not being in the same plane
    MUID_CONT,
    
    //! cut on road distance to vertex (at vertex z)
    MUID_RV,
    
    //! cut on road distance to vertex (along the beam)
    MUID_DCA,
    
    //! cut on road to track distance at gap0
    DG0,
    
    //! cut on road to track angular difference
    DDG0,
    
    //! cut on road to track distance at station3
    DS3,
    
    //! cut on road to track distance at station3 assuming straight road/track from 0,0,0
    DS3_CTP
  };
  
  //! decides if a given cut is to be done or not
  virtual void set_do_cut( const Cut& cut, bool value );
  
  //! true if a given cut is to be done or not
  virtual bool do_cut( const Cut& cut ) const
  { return _cuts.find( cut ) != _cuts.end(); }
  
  //! get index of best muidoo road associated to new framework track
  int get_best_road_oo( int imu, PHMuoTracksOut* muo) const;
  
  //! max value for bbc z	
  virtual void set_bbc_z_cut( double value )
  { _bbc_z_cut = value; }
  
  //! centrality window 
  virtual void set_centrality_cut( double min, double max )
  { 
    _centrality_cut[0] = min;
    _centrality_cut[1] = max;
  }
  
  //! max value for dimuon dca vtx vs bbc
  virtual void set_dimu_vtx_cut( double value )
  { _dimu_vtx_cut = value; }
  
  //! minimum dimuon mass
  virtual void set_dimu_mass_cut( double value )
  { _dimu_mass_cut = value; }
  
  //! max fitted vertex chisquare
  virtual void set_vtx_chisquare_cut( double value )
  { _vtx_chisquare_cut = value; }
  
  //! south rapidity window
  virtual void set_y_dimu_south_cut( double min, double max )
  { 
    _y_dimu_south_cut[0] = min;
    _y_dimu_south_cut[1] = max;
  }
  
  //! north rapidity window
  virtual void set_y_dimu_north_cut( double min, double max )
  { 
    _y_dimu_north_cut[0] = min;
    _y_dimu_north_cut[1] = max;
  }
  
  //! max trk chisquare
  virtual void set_trk_chisquare_cut( double value )
  { _trk_chisquare_cut = value; }
  
  //! min number of mutr hits in trk
  virtual void set_mutr_hits_cut( int value )
  { _mutr_hits_cut = value; }
  
  //! max value on distance from trk to vertex (fixed z)
  virtual void set_mutr_rv_cut( double value )
  { _mutr_rv_cut = value; }
  
  //! min number of muid hits in trk
  virtual void set_muid_hits_cut( int value )
  { _muid_hits_cut = value; }
  
  //! min depth for muid road
  virtual void set_muid_depth_cut( int value )
  { _muid_depth_cut = value; }
  
  //! max distance from muid road to vertex (at fixed z)
  virtual void set_muid_rv_cut( double value )
  { _muid_rv_cut = value; }
  
  //! max distance from muid road to vertex (along the beam)
  virtual void set_muid_dca_cut( double value )
  { _muid_dca_cut = value; }
  
  //! max value for distance from road to trk at gap0
  virtual void set_dg0_cut( double value )
  { _dg0_cut = value; }
  
  //! max value for angular distance from road to trk at gap0
  virtual void set_ddg0_cut( double value )
  { _ddg0_cut = value; }
  
  //! max value for distance from road to trk at station3
  virtual void set_ds3_cut( double value )
  { _ds3_cut = value; }
  
  //! max value for distance from road to trk at station3 (assuming straight trk from 0,0,0)
  virtual void set_ds3_ctp_cut( double value )
  { _ds3_ctp_cut = value; }
  
  protected:
  
  //! returns true if bbc_z is OK
  virtual bool bbc_z_ok( PHGlobal* global ) const;
  
  //! check bbc and zdc 
  virtual bool bbczdc_ok( PHGlobal *global ) const;
  
  //! returns true if centrality is OK
  virtual bool centrality_ok( PHCompositeNode *topNode ) const;
  
  //! returns true if muid mult vs bbc charge is OK
  virtual bool muid_bbc_ok( PHGlobal *global ) const;
  
  //! returns true if dimuon roads belong to different panels
  virtual bool idquad_ok( int idimu, PHMuoTracksOut* muo ) const;
  
  //! returns true if dimuon dca vertex z is ok
  virtual bool dimu_vtx_ok( PHGlobal* global, int idimu, PHMuoTracksOut* muo ) const;
  
  //! returns true if dimuon mass is ok
  virtual bool dimu_mass_ok( int idimu, PHMuoTracksOut* muo ) const;
  
  //! returns true if dimuon fitted vertex chisquare is ok
  virtual bool vtx_chisquare_ok( int idimu, PHMuoTracksOut* muo ) const;
  
  //! returns true if north arm dimuons fall into rapidity window
  virtual bool y_dimu_south_ok( int idimu, PHMuoTracksOut* muo, const int& framework ) const; 
  
  //! returns true if north arm dimuons fall into rapidity window
  virtual bool y_dimu_north_ok( int idimu, PHMuoTracksOut* muo, const int& framework ) const; 
  
  //! returns true if dimuon arm matches level2 
  virtual bool level2_ok( int idimu, PHMuoTracksOut* muo, const int& framework ) const;
  
  //! returns true if trk ghost flag is false
  virtual bool ghost_flag_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! return true if trk chisquare is ok
  virtual bool trk_chisquare_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if trk number of mutr hits is ok
  virtual bool mutr_hits_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if trk number of mutr hits in station 3 is ok
  virtual bool st3_hits_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if trk distance to vertex (fixed z) is ok
  virtual bool mutr_rv_ok( PHGlobal* global, int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if number of trk muid hits is ok
  virtual bool muid_hits_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if number of trk muid hits is ok
  virtual bool muid_hits_oo_ok( int imu, int iroad, PHMuoTracksOut* muo ) const;
  
  //! returns true if muid road depth is ok
  virtual bool muid_depth_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if muid road depth is ok
  virtual bool muid_depth_oo_ok( int imu, int iroad, PHMuoTracksOut* muo ) const;
  
  //! returns true if muid horiz/vert tubes are ok
  virtual bool muid_tubes_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if muid cont (???) is ok
  virtual bool muid_cont_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if muid road distance to vertex (at fixed z) is ok
  virtual bool muid_rv_ok( PHGlobal* global, int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if muid road distance to vertex (along the beam) is ok
  virtual bool muid_dca_ok( PHGlobal* global, int imu, PHMuoTracksOut* muo ) const;
  
  //! return true if road to trk distance at gap0 is ok
  virtual bool dg0_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! return true if road to trk distance at gap0 is ok
  virtual bool dg0_oo_ok( int imu, int iroad, PHMuoTracksOut* muo ) const;
  
  //! return true if road to trk angular distance at gap0 is ok
  virtual bool ddg0_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! return true if road to trk angular distance at gap0 is ok
  virtual bool ddg0_oo_ok( int imu, int iroad, PHMuoTracksOut* muo ) const;
  
  //! returns true if road to trk distance at station3 is ok
  virtual bool ds3_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! returns true if road to trk distance at station3 is ok
  virtual bool ds3_oo_ok( int imu, int iroad, PHMuoTracksOut* muo ) const;
  
  //! retuns true if road to trk distance (assuming straight trk from 0,0,0) is ok
  virtual bool ds3_ctp_ok( int imu, PHMuoTracksOut* muo ) const;
  
  //! retuns true if road to trk distance (assuming straight trk from 0,0,0) is ok
  virtual bool ds3_ctp_oo_ok( int imu, int iroad, PHMuoTracksOut* muo ) const;
  
  private:
  
  //! set of cuts to be performed
  std::set< Cut > _cuts;
  
  //! max value for abs(BbcZ)
  double _bbc_z_cut; 
  
  //! centrality window
  double _centrality_cut[2];
  
  //! max value for dimuon dca vtx vs bbc
  double _dimu_vtx_cut;
  
  //! minimum dimuon mass
  double _dimu_mass_cut;
  
  //! max fitted vertex chisquare
  double _vtx_chisquare_cut;
  
  //! south rapidity window
  double _y_dimu_south_cut[2];
  
  //! north rapidity limits
  double _y_dimu_north_cut[2];
  
  //! max trk chisquare
  double _trk_chisquare_cut;
  
  //! min number of mutr hits in trk
  int _mutr_hits_cut;
  
  //! max value on distance from trk to vertex (fixed z)
  double _mutr_rv_cut;
  
  //! min number of muid hits in trk
  int _muid_hits_cut;
  
  //! min depth for muid road
  int _muid_depth_cut;
  
  //! max distance from muid road to vertex (at fixed z)
  double _muid_rv_cut;
  
  //! max distance from muid road to vertex (along the beam)
  double _muid_dca_cut;
  
  //! max value for distance from road to trk at gap0
  double _dg0_cut;
  
  //! max value for distance from road to trk at gap0
  double _ddg0_cut;
  
  //! max value for distance from road to trk at station3
  double _ds3_cut;
  
  //! max value for distance from road to trk at station3 (assuming straight trk from 0,0,0)
  double _ds3_ctp_cut;
  
  //! define class for CINT
  ClassDef(MWGCuts,1)
};

#endif
