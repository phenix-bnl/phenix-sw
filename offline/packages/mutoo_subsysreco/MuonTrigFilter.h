// $Id: MuonTrigFilter.h,v 1.55 2016/02/27 05:31:00 slash Exp $

/*!
  \file		MuonTrigFilter.h
  \ingroup supermodules
  \brief	 muid based offline trigger used to filter data on Deep Deep road pairs
  \author	Sean Kelly/Hugo Pereira
  \version $Revision: 1.55 $
  \date		$Date: 2016/02/27 05:31:00 $
*/

#ifndef __MUONTRIGFILTER_H__
#define __MUONTRIGFILTER_H__

#include <SubsysReco.h>


// Forward declerations
class PHCompositeNode;

#ifndef __CINT__
#include <TMutTrkMap.h>
#include <TMutMCTrkMap.h>
#include <TMutMCHitMap.h>
#include <PHTimeServer.h>
#include <boost/array.hpp>
#include <set>
#endif

#include<BbcOut.h>
#include<ZdcOut.h>
#include<PHGlobal.h>
#include<MUTOO.h>
#include <PHMuoTracksOut.h>

#define PI 3.1415926

/*!
  \class	 MuonTrigFilter
  \ingroup supermodules
  \brief	 muid based offline trigger used to filter data on Deep Deep road pairs
*/
class MuonTrigFilter: public SubsysReco
{
 public:

  //! Mode to decide which trigger data we want to select.
  enum MODE{

    //! to select events within a vertex cut but preserves UP events
    HI_EVENT,

    //! to select only trigger helper minimum bias events
    MINIBIAS,

    //! to select only trigger helper north deep deep events
    N2D,

    //! to select only trigger helper north deep shallow events
    N1D1S,

    //! to select only trigger helper north one deep events
    N1D,

    //! to select only trigger helper south deep deep events
    S2D,

    //! to select only trigger helper south deep shallow events
    S1D1S,

    //! to select only trigger helper south one deep events
    S1D,

    //! to select only trigger helper SG3: intermedian momentum muons
    SG3,

    //! to select events for which muioo find at least two deep tracks in one arm
    MUIOO_2D,

    //! to select events based on level2 trigger
    LEVEL2,

    //! to select events based on pseudoLL1 N2D trigger
    PSEUDOLL1_N2D,

    //! to select events based on pseudoLL1 S2D trigger
    PSEUDOLL1_S2D,

    //! to select events based on pseudoBLT 2D triggers (north or south)
    PSEUDOBLT_2D,

    //! to select events based on pseudoBLT N2D trigger
    PSEUDOBLT_N2D,

    //! to select events based on pseudoBLT S2D trigger
    PSEUDOBLT_S2D,

    //! to select events based on pseudoBLT 1D1S triggers (north or south)
    PSEUDOBLT_1D1S,

    //! to select events based on pseudoBLT N1D1S trigger
    PSEUDOBLT_N1D1S,

    //! to select events based on pseudoBLT S1D1S trigger
    PSEUDOBLT_S1D1S,

    //! to select events for FVTX analysis
    FVTX,

    //! to select cosmic events for projecting to FVTX
    RECO_COSMIC,

    //! to select two arms cosmic events for projecting to FVTX
    RECO_TWOARM_COSMIC,
    
    //! to select one arm plus MuID road in the other arm cosmic events for projecting to FVTX
    RECO_ONEARM_COSMIC,
    
    /*!
      \brief
      to select event on an MC basis. It keeps only events with 2 MC tracks with
      a mutr MC hit for each mutr gap, that falls in a "enabled" HV region
    */
    MC_HV,

    /*!
      \brief
      to select event on an MC basis. It keeps only events with 2 MC tracks with
      a mutr MC hit for each mutr gap and a muid MC hit in planes 0,1,2 at least (shallow roads)
    */
    MC_SHALLOW_SHALLOW,

    /*!
      \brief
      to select event on an MC basis. It keeps only events with 2 MC tracks with
      a mutr MC hit for each mutr gap and a muid MC hit in planes 0,1,2 at least (shallow roads)
    */
    MC_SHALLOW_SHALLOW_NORTH,

    /*!
      \brief
      to select event on an MC basis. It keeps only events with 2 MC tracks with
      a mutr MC hit for each mutr gap and a muid MC hit in planes 0,1,2 at least (shallow roads)
    */
    MC_SHALLOW_SHALLOW_SOUTH,

    /*! \brief
      to select event on an MC basis. It keeps only events with MC tracks with
      a mutr MC hit for each mutr gap and a muid MC hit in planes 0,1,2/0,1,2,3,4 at least (shallow/deep roads)
    */
    MC_DEEP_SHALLOW,

    /*!
      \brief
      to select event on an MC basis. It keeps only events with 3 MC tracks with
      a mutr MC hit for each mutr gap and a muid MC hit in planes 0,1,2,3,4 at least (deep roads)
    */
    MC_DEEP_DEEP,

    /*! \brief
      to select events for which the vertex falls into given range
      default range is +/- 40cm
    */
    Z_VERTEX,

    /*! \brief
      Remove events happening in large zvertex and number of MuTr hits larger than _max_mut_hit_cut.
      Helps to reduce reconstruction time and preserve ultraperipheral events
     */
    MUTHITS_ZVERTEX,

    //! obsolete version
    Z_BBC,
    /* \brief
      to reject events for which either one of the BBC/ZDC north/south
      has zero in multiplicity/charge
    */
    BBC_ZDC,

    //! cut on centrality : true if CentralityMin < centrality <= CentralityMax
    CENTRALITY,

    //! cut on BBC charge. True if BbcChargeMin < BbcCharge <= BbcChargeMax
    BBC_CHARGE,

    /*!
      \brief
      to remove events for which average hit in north station 1 is too big vs BBC charge.
      Removes pathological events in run4 [from Melynda Brooks studies]
    */
    BAD_MUTR_VS_BBC_EVENT,

    /*! \brief
      select events for which at least one vertex with charge 0 and mass in mass window is found
    */
    RECO_DIMUON,

    //! select events with at least one track
    TRK_MUON,

    //! select events with at least one track between minpt and maxpt
    TRK_HIPTMUON,

    //! select events with at least one road
    ROAD_MUON,

    //! nothing done. is default. prints a message at every event
    NONE
  };

  //! Mode to select what to do with rejected events
  enum ACTION {

    //! events are processed normaly but not written to the DST
    DISCARD_EVENT,

    //! event processing is aborted immediately (later modules are not processed)
    ABORT_EVENT,

    //! raise SKIP_MUTOO_RECO recoconst flag, to get caught by any other module
    SKIP_MUTOO,

    //! do nothing, stricly
    DO_NOTHING
  };

  //! constructor
  MuonTrigFilter( const char* name = "MUONTRIGFILTER", const MODE& mode = NONE, const ACTION& action = ABORT_EVENT );

  //! run initialization
  virtual int InitRun( PHCompositeNode *topNode );

  //! destructor
  virtual ~MuonTrigFilter() {}

  //! event method
  int process_event(PHCompositeNode *topNode);

  //! end method
  int End(PHCompositeNode *topNode);

  //! current event accepted flag
  const bool& event_accepted( void ) const
  { return _event_accepted; }

  //! cut on average charge/hit in north station1
  void set_mutr_hit_cut(UShort_t mutr_hit_cut)
  {_mutr_hit_cut = mutr_hit_cut;}

  //! cut on average charge/hit in north station1
  UShort_t get_mutr_hit_cut() const
  { return _mutr_hit_cut;}

  //! trigger mode
  void set_mode(MODE mode)
  { _mode = mode;}

  //! trigger mode
  MODE get_mode() const
  { return _mode;}

  //! action to be taken when event is not accepted
  void set_action(ACTION action)
  { _action = action;}

  //! action to be taken when event is not accepted
  ACTION get_action() const
  { return _action; }

  //! sets bbc_z_min
  void set_z_vertex_min( double value )
  { _z_vertex_min = value; }

  //! sets bbc_z_max
  void set_z_vertex_max( double value )
  { _z_vertex_max = value; }

  //! sets bbc_z window
  void set_z_vertex_window( const double& min, const double& max )
  {
    std::cout << "MuonTrigFilter::set_z_vertex_window - min: " << min << " max: " << max << std::endl;
    _z_vertex_min = min;
    _z_vertex_max = max;
  }

  //!@name obsolete versions of the vertex z filtering
  //@{

  //! sets bbc_z_min
  void set_z_bbc_min( double value )
  {
    std::cout << "MuonTrigFilter::set_z_bbc_min. WARNING: this method is obsolete. Use set_z_vertex_min instead" << std::endl;
    set_z_vertex_min( value );
  }

  //! sets bbc_z_max
  void set_z_bbc_max( double value )
  {
    std::cout << "MuonTrigFilter::set_z_bbc_max. WARNING: this method is obsolete. Use set_z_vertex_max instead" << std::endl;
    set_z_vertex_max( value );
  }

  //! sets bbc_z window
  void set_z_bbc_window( const double& min, const double& max )
  {
    std::cout << "MuonTrigFilter::set_z_bbc_window. WARNING: this method is obsolete. Use set_z_vertex_window instead" << std::endl;
    set_z_vertex_window( min, max );
  }

  //@}

  //! sets bbc_charge_min
  void set_bbc_charge_min( double value )
  { _bbc_charge_min = value; }

  //! sets bbc_charge_max
  void set_bbc_charge_max( double value )
  { _bbc_charge_max = value; }

  //! sets bbc_charge window
  void set_bbc_charge_window( const double& min, const double& max )
  {
    _bbc_charge_min = min;
    _bbc_charge_max = max;
  }

  //! sets centrality_min
  void set_centrality_min( double value )
  { _centrality_min = value; }

  //! sets centrality_max
  void set_centrality_max( double value )
  { _centrality_max = value; }

  //! sets centrality window
  void set_centrality_window( const double& min, const double& max )
  {
    _centrality_min = min;
    _centrality_max = max;
  }

  //! sets mass_min
  void set_mass_min( double value )
  { _mass_min = value; }

  //! sets mass_max
  void set_mass_max( double value )
  { _mass_max = value; }

  //! sets mass window
  void set_mass_window( const double& min, const double& max )
  {
    _mass_min = min;
    _mass_max = max;
  }

  void set_max_dimu_dcar( const double& max )
  {
    _max_dimu_dcar = max;
  }

  void set_max_dimu_dcaz( const double& max )
  {
    _max_dimu_dcaz = max;
  }

  void set_min_track_pz( const double& min )
  {
    _min_track_pz = min;
  }

  //! set maximum dimuon momentum asymmetry
  void set_max_asymmetry( const double& max )
  {
    _max_asymmetry = max;
  }

  void set_max_dimu_chi2( const double& max )
  {
    _max_dimu_chi2 = max;
  }

  //! sets minpt
  void set_minpt( double value )
  { _minpt = value; }

  //! sets maxpt
  void set_maxpt( double value )
  { _maxpt = value; }


  //! check mutr multiplicity is not too big with respect to BBC charge
  bool is_mutr_vs_bbc_accepted(PHCompositeNode* top_node) const;

  //! check muid multiplicity is not too big with respect to BBC charge
  bool is_mui_vs_bbc_accepted( PHCompositeNode* top_node ) const;

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit in each gap
  */
  bool is_mc_mutr_accepted(PHCompositeNode* top_node) const
  {
    return
      is_mc_mutr_accepted( top_node, MUTOO::North ) ||
      is_mc_mutr_accepted( top_node, MUTOO::South );
  }

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit in each gap
  */
  bool is_mc_mutr_accepted(PHCompositeNode* top_node, int arm) const;

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit matching the HV mask in each gap
  */
  bool is_hv_mutr_accepted(PHCompositeNode* top_node) const
  {
    return
      is_hv_mutr_accepted( top_node, MUTOO::North ) ||
      is_hv_mutr_accepted( top_node, MUTOO::South );
  }

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit matching the HV mask in each gap
  */
  bool is_hv_mutr_accepted(PHCompositeNode* top_node, int arm) const;

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit in each gap and a TMuiMCHit in muid gap 0 1 and 2
  */
  bool is_mc_shallow_shallow_accepted(PHCompositeNode* top_node) const
  {
    return
      is_mc_shallow_shallow_accepted( top_node, MUTOO::North ) ||
      is_mc_shallow_shallow_accepted( top_node, MUTOO::South );
  }

  /*! \brief
    returns true if mc events have at least two tracks in given arm
    with one TMutMCHit in each gap and a TMuiMCHit in muid gap 0 1 and 2
  */
  bool is_mc_shallow_shallow_accepted(PHCompositeNode* top_node, int arm ) const;

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit in each gap and a TMuiMCHit in muid gap 0,1,2/0,1,2,3,4
  */
  bool is_mc_deep_shallow_accepted(PHCompositeNode* top_node) const
  {
    return
      is_mc_deep_shallow_accepted( top_node, MUTOO::North ) ||
      is_mc_deep_shallow_accepted( top_node, MUTOO::South );
  }

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit in each gap and a TMuiMCHit in muid gap 0,1,2/0,1,2,3,4
  */
  bool is_mc_deep_shallow_accepted(PHCompositeNode* top_node, int arm) const;

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit in each gap and a TMuiMCHit in muid gap 0,1,2
  */
  bool is_mc_deep_deep_accepted(PHCompositeNode* top_node) const
  {
    return
        is_mc_deep_deep_accepted( top_node, MUTOO::North ) ||
        is_mc_deep_deep_accepted( top_node, MUTOO::South );
  }

  /*! \brief
    returns true if mc events have at least two tracks in same arm
    with one TMutMCHit in each gap and a TMuiMCHit in muid gap 0,1,2
  */
  bool is_mc_deep_deep_accepted(PHCompositeNode* top_node, int arm) const;

  //! returns true if events have one track
  bool has_track(PHCompositeNode* top_node, int arm) const;

  //! returns true if events have one track
  bool has_track(PHCompositeNode* top_node) const
  {
    return
        has_track( top_node, MUTOO::North ) ||
        has_track( top_node, MUTOO::South );
  }

  //! returns true if events have one track
  bool has_hipttrack(PHCompositeNode* top_node, int arm) const;


  //! returns true if events have one track
  bool has_hipttrack(PHCompositeNode* top_node) const
  {
    return
        has_hipttrack( top_node, MUTOO::North ) ||
        has_hipttrack( top_node, MUTOO::South );
  }

  //! returns true if events have one road
  bool has_road(PHCompositeNode* top_node, int arm) const;

  //! returns true if events have one road
  bool has_road(PHCompositeNode* top_node) const
  {
    return
        has_road( top_node, MUTOO::North ) ||
        has_road( top_node, MUTOO::South );
  }

  //! returns true if events have one good track: 
  //! nMuID hits>=5 && nMuTrhits>=10 && |Z0|<30cm
  bool has_good_track(PHCompositeNode* top_node) const;
  
  //! returns true if events have one good cosmic track: 
  bool has_good_cosmic_track(PHCompositeNode* top_node) const;

  //! returns true if events have two arm good cosmic track: 
  bool has_good_twoarm_cosmic_track(PHCompositeNode* top_node) const;

  //! returns true if events have one arm good cosmic track: 
  bool has_good_onearm_cosmic_track(PHCompositeNode* top_node) const;

  //! Calculate DG0 for track quality check
  float DG0( PHMuoTracksOut* &muo, int idx, int iroad) const;

  //! Calculate DDG0 for track quality check
  float DDG0( PHMuoTracksOut* &muo, int idx, int iroad) const;

  //! adds a level2 trigger to local set
  void add_level2_trigger( const char* trigger_name );

  //! check maximum number of MuTr hits
  bool is_mutnhits_accepted(PHCompositeNode* top_node) const;

  //! station masks
  enum mc_mask
  {
    mutr_mc_st1_mask = (1<<3) - 1,
    mutr_mc_st2_mask = (1<<6) - (1<<3),
    mutr_mc_st3_mask = (1<<8) - (1<<6),
    mutr_mc_mask = (1<<8) - 1,
    muid_mc_shallow_mask = (1<<3) - 1,
    muid_mc_sheep_mask = (1<<4) - 1,
    muid_mc_deep_mask = (1<<5) - 1
  };
  #ifndef __CINT__
  //!@name static MC utilities
  //@{

  //! returns bit pattern of mutr MC hits
  static int get_mc_mutr_pattern( const TMutMCTrkMap::value_type& trk );

  //! returns bit pattern of muid MC hits
  static int get_mc_muid_pattern( const TMutMCTrkMap::value_type& trk );

  //! returns bit pattern of mutr HV accepted MC hits
  static int get_hv_mutr_pattern( const TMutMCTrkMap::value_type& trk );

  //! returns true if associated TMutMCHit match the mask
  static bool is_mc_mutr_accepted( const TMutMCTrkMap::value_type& trk, const int& mask )
  { return (get_mc_mutr_pattern( trk )&mask) == mask; }

  //! returns true if associated TMuiMCHitO match the mask
  static bool is_mc_muid_accepted( const TMutMCTrkMap::value_type& trk, const int& mask )
  { return (get_mc_muid_pattern( trk )&mask) == mask; }

  //! returns true if TMutMCHits on TMutMCTracks are here and are HV accepted
  static bool is_hv_mutr_accepted( const TMutMCTrkMap::value_type& trk, const int& mask )
  { return (get_hv_mutr_pattern( trk )&mask) == mask; }

  //! returns true if TMutMCHit match a HV working anode wire
  static bool is_hv_mutr_accepted( const TMutMCHitMap::value_type& hit );

  //@}

  #endif

  protected:

  //! returns true when at least two deep roads are found by muioo
  bool is_muioo_2d(PHCompositeNode* top_node) const;

  //! returns true when at least one vertex is found
  bool is_reco_dimuon(PHCompositeNode* top_node) const;

  //! returns true if bbc z_vertex falls in specified range
  bool is_vertex_accepted(PHCompositeNode* top_node) const;

  //! returns true if all bbc/zdc north and south have non 0 values
  bool is_bbc_zdc_accepted(PHCompositeNode* top_node) const;

  //! returns true if CentralityMin < centrality <= CentralityMax
  bool is_centrality_accepted(PHCompositeNode* top_node) const;

  //! returns true if BbcChargeMin < BbcCharge <= BbcChargeMax
  bool is_bbc_charge_accepted(PHCompositeNode* top_node) const;

  //! returns true if at least one of the level2 triggers fires
  bool is_level2_accepted( PHCompositeNode* top_node ) const;

  //! returns true if the pseudoLL1 decision is N2D
  bool is_pseudoLL1_N2D( PHCompositeNode* top_node ) const;

  //! returns true if the pseudoLL1 decision is S2D
  bool is_pseudoLL1_S2D( PHCompositeNode* top_node ) const;

  //! returns true if the pseudoBLT decision is N2D
  bool is_pseudoBLT_N2D( PHCompositeNode* top_node ) const;

  //! returns true if the pseudoBLT decision is S2D
  bool is_pseudoBLT_S2D( PHCompositeNode* top_node ) const;

  //! returns true if the pseudoBLT decision is N1D1S
  bool is_pseudoBLT_N1D1S( PHCompositeNode* top_node ) const;

  //! returns true if the pseudoBLT decision is S1D1S
  bool is_pseudoBLT_S1D1S( PHCompositeNode* top_node ) const;

  //! lower limit on BBC z_vertex [cm]
  double _z_vertex_min;

  //! uper limit on BBC z_vertex [cm]
  double _z_vertex_max;

  //! lower limit on centrality
  double _centrality_min;

  //! uper limit on centrality
  double _centrality_max;

  //! lower limit on BBC multiplicity
  double _bbc_charge_min;

  //! upper limit on BBC multiplicity
  double _bbc_charge_max;

  //! lower limit on dimuon mass window [GeV]
  double _mass_min;

  //! upper limit on dimuon mass window [GeV]
  double _mass_max;

  //! upper limit on distance btw. vertex and dimuon Z
  double _max_dimu_dcaz;

  //! lower limit for the absolute track pz cut
  double _min_track_pz;

  //! upper dimuon momentum asymmetry 
  double _max_asymmetry;

  //! upper dimuon distance from beam line
  double _max_dimu_dcar;

  //! upper dimuon chi2
  double _max_dimu_chi2;
  
  #ifndef __CINT__
  /*!
    list of selected level2 triggers.
    Accept is _mode is LEVEL2 and one of the triggers fires
  */
  std::set< std::string > _level2_triggers;

  //! module timer
  PHTimeServer::timer _timer;

  #endif

  //! trigger mode
  MODE _mode;

  //! action to be taken for rejected events [local scope]
  ACTION _action;

  //! current event accepted flag
  bool _event_accepted;

  //! total number of processed events
  ULong_t _nevent;

  //! number of rejected events
  ULong_t _nrejected;

  //! used in Mutr hits vs. bbc cut
  UShort_t _mutr_hit_cut;

  //! BBC node
  BbcOut* _bbcout;

  //! ZDC node
  ZdcOut* _zdcout;

  //! lower limit on transverse momentum [GeV]
  double _minpt;

  //! upper limit on transverse momentum [GeV]
  double _maxpt;

  //! PHGlobal nord
  PHGlobal *_global;

  //! Maximum number of MuTr hits
  int _max_mut_nhits;


};

#endif /* __MUONTRIGFILTER_H__ */
