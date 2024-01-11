#ifndef __MUONMCEval_H__
#define __MUONMCEval_H__

/*!
  \file FvtxMCEval.h
  \ingroup supermodules
  \brief MC evaluation module
  \author Hugo Pereira
  \version $Revision: 1.43 $
  \date $Date: 2018/06/26 15:04:28 $
*/

#include <vector>
#include <map>

#include <Fun4AllServer.h>
#include <PHCompositeNode.h>
#include <SubsysReco.h>
#include <TTree.h>
#include <string>

#include <header.h>
#include <fkin.h>
#include <pythia.h>
#include <primary.h>
#include <PHTable.hh>

#ifndef __CINT__
#include <boost/array.hpp>
#include <mFvtxMatch.h>
#endif

#include <FvtxMCMatch.h>

// Forward declerations
class PHCompositeNode;
class PHTimer;
class TFvtxMCHitMap;
class TFvtxPisaHitMap;
class TMutMCHitMap;
class TMutMCTrkMap;
class TFvtxTrkMap;
class TFvtxHitMap;
class TFvtxClusMap;
class TFvtxCoordMap;
class TFvtxEvalMap;
class TMuiRoadMapO;

class mFvtxEval;

class fkinWrapper;
class headerWrapper;
class pythiaWrapper;
class primaryWrapper;
class TMutMCTrk;
class PHPythiaContainer;
class VtxOut;

/*!
  \class FvtxMCEval
  \ingroup supermodules
  \brief MC evaluation module
*/

class FvtxMCEval: public SubsysReco
{
  public:

  //! constructor
  FvtxMCEval( const char* name="FVTXMCEVAL", const char* file = "fvtx_mc_eval.root" );

  //! destructor
  virtual ~FvtxMCEval();

  //! full initialization
  int Init(PHCompositeNode *topNode);

  //! run initialization
  int InitRun(PHCompositeNode *topNode);

  //! event method
  int process_event(PHCompositeNode *topNode);

  //! full end
  int End(PHCompositeNode *topNode);

  //! defines mutoo evaluation filename
  void set_file_name( const char* file )
  { if( file ) _file_name = std::string( file ); }

  void set_save_file( const bool b )
  { _do_save_file = b; }

  // ! changes signal node name
  void SetSignalNodeName(std::string name)
  { _signalNodeName = name; }

  // ! changes fvtxoo node name
  void SetFvtxNodeName(std::string name)
  { _fvtxNodeName = name; }    

  //! do fkin eval or not
  void set_fkin_eval(bool val) { _do_fkin_eval = val; }

  //! do header eval or not
  void set_header_eval(bool val) { _do_header_eval = val; }

  //! do pythia eval or not
  void set_pythia_eval(bool val) { _do_pythia_eval = val; }

  //! do pythia eval or not
  //void set_primary_eval(bool val) { _do_primary_eval = val; }

  void do_match_mod(bool val) { _do_match_mod = val; }
  bool do_match_mod() { return _do_match_mod; }

  void set_fit_model(int val) { _fit_model = val; }

  protected:

  //! working node
  PHCompositeNode* _fvtx_node;

  //! set interface pointers
  void set_interface_pointers( PHCompositeNode* );

  void book_mc_radiograph_tree( void );
  void fill_mc_radiograph_tree( void );

  void book_mc_trk_eval_tree( void );
  void fill_mc_trk_eval_tree( void );

  void book_trk_eval_tree( void );
  void fill_trk_eval_tree( void );

  void book_mc_traj_eval_tree( void );
  void fill_mc_traj_eval_tree( void );

  void book_mc_cluster_tree( void );
  void fill_mc_cluster_tree( void );

  void book_mc_fvtx_hit_tree( void );
  void fill_mc_fvtx_hit_tree( void );

  void book_event_tree( void );
  void fill_event_tree( void );

  void book_fkin_eval_tree( void );
  void fill_fkin_eval_tree( void );

  void book_header_eval_tree( void );
  void fill_header_eval_tree( void );

  void book_pythia_eval_tree( void );
  void fill_pythia_eval_tree( void );

  void book_primary_eval_tree( void );
  void fill_primary_eval_tree( void );

  void book_coord_eval_tree( void );
  void fill_coord_eval_tree( void );

  void book_match_eval_tree( void );
  void fill_match_eval_tree( void );

  void book_straight_trk_eval_tree( void );
  void fill_straight_trk_eval_tree( void );

  void lookupCharmBottomAncestry(
    const int trackId, short int& primId, int& parent_pdgid,
    short int& hasAncB, short int& hasAncD);

  mFvtxEval* _mFvtxEvalMod;

  #ifndef __CINT__

  void add_muon_info( TFvtxTrk *ptr );

  mFvtxMatch _mFvtxMatchMod;

  #endif

  int _fit_model;

  //!@name radiograph tree
  //@{

  //! MC radiograph tree
  #ifndef __CINT__
  TTree *_mc_radiograph_tree;

  int _arm;
  int _cage;
  int _station;
  int _sector;
  int _plane;
  int _column;
  int _size;

  enum { max_size=50 };

  boost::array<int, max_size> _strip;
  boost::array<double, max_size> _q;
  boost::array<double, max_size> _strip_x_begin;
  boost::array<double, max_size> _strip_y_begin;
  boost::array<double, max_size> _strip_x_end;
  boost::array<double, max_size> _strip_y_end;

  //! distance from MC hit to strip
  boost::array<double, max_size> _d;

  boost::array<int,20> _hitstIds; // list of "nhitst" ids (the mchits associated with mctrk)
  boost::array<int,20> _hitsFoundIds; // list of found-hit ids
  boost::array<int,20> _coordFoundIds; // list of found-coord ids

  double _eloss;

  int _track_id;
  int _pid;
  int _parent_pid;
  int _parent_track_id;
  int _hit_id;
  int _event;
  double _x;
  double _y;
  double _z;
  #endif

  //! MC trk_eval tree
  #ifndef __CINT__
  TTree *_trk_eval_tree;

  double _x0mc;
  double _y0mc;
  double _z0mc;
  double _px0mc;
  double _py0mc;
  double _pz0mc;
  double _x0reco;
  double _y0reco;
  double _z0reco;
  double _px0reco;
  double _py0reco;
  double _pz0reco;
  float _chargemc;
  float _chargereco;
  double _chisqreco;
  double _x1mc;
  double _y1mc;
  double _z1mc;
  double _x2mc;
  double _y2mc;
  double _z2mc;
  double _x3mc;
  double _y3mc;
  double _z3mc;
  double _x4mc;
  double _y4mc;
  double _z4mc;
  int    _ittrk;
  int    _parent_pdgid;
  int    _idtrk;
  double _x1reco;
  double _y1reco;
  double _z1reco;
  double _x2reco;
  double _y2reco;
  double _z2reco;
  double _x3reco;
  double _y3reco;
  double _z3reco;
  double _x4reco;
  double _y4reco;
  double _z4reco;
  double _x0mcproj;
  double _y0mcproj;
  double _z0mcproj;
  int _nhitsb; // number of hits from the barrel
  double _x1mcb;
  double _y1mcb;
  double _z1mcb;
  double _x2mcb;
  double _y2mcb;
  double _z2mcb;
  double _x3mcb;
  double _y3mcb;
  double _z3mcb;
  double _x4mcb;
  double _y4mcb;
  double _z4mcb;
  int _nvtxf; // number of barrel hits associated with this track ("found")
  int _nvtxmc; // number of (true) barrel hits to go with this track
  int _nhitsf; // number of mc hits associated with this track
  double _x0recoerr;
  double _y0recoerr;
  int _ghost;
  double _chisqrecopdf;
  double _pxmutr;
  double _pymutr;
  double _pzmutr;
  double _chi2mutr;
  double _evtx0mc;
  double _evty0mc;
  double _evtz0mc;
  double _evtx0error;
  double _evty0error;
  double _evtz0error;
  double _evtx0smear;
  double _evty0smear;
  double _evtz0smear;
  double _mu_charge;
  double _mu_px;
  double _mu_py;
  double _mu_pz;
  double _mu_p;
  double _mu_pt;
  double _mu_chi2;
  double _mu_ghost;
  double _mu_muIDquad0;
  double _mu_muIDchis0;
  double _mu_muTRhits0;
  double _mu_muIDhits0;
  double _mu_dS30;
  double _mu_DG0;
  double _mu_DDG0;
  double _mu_dS3ctp0;
  double _mu_DS0;
  double _mu_lastGap;
  double _mu_mutr_nhits;
  double _mu_muid_nhits;
  double _mu_eta;

  boost::array<int,20> _coordIds;
  boost::array<double,20> _coordPhiStart;
  boost::array<double,20> _coordPhiEnd;
  boost::array<double,20> _coordR;
  boost::array<double,20> _coordZ;
  double _r_slopeFit;
  double _r_offsetFit;

  double _x0slreco;
  double _y0slreco;
  double _mxslreco;
  double _myslreco;
  double _x0slerr;
  double _y0slerr;
  double _mxslerr;
  double _myslerr;
  double _chi2slreco;
  int _ndfslreco;

  // Parameters for the combined FVTX+MUTR kalfit results
  double _x0mutreco;
  double _y0mutreco;
  double _z0mutreco;
  double _px0mutreco;
  double _py0mutreco;
  double _pz0mutreco;
  short _chargemutreco;
  float _chisqmutreco;
  float _chisqmutrecopdf;

  #endif
  //@}

  #ifndef __CINT__
  TTree *_mc_trk_eval_tree;

  int _nroadS;      // number of MuID roads in south arm
  int _nroadN;      // number of MuID roads in north arm
  int _nhitst;      // number of true FVTX MC hits formed.
  int _nhitst_mut;      // number of true MuT MC hits formed.
  int _nhitsmade_mut;      // number of MuT hits formed by the MC track.
  int _ncoordst_mut;      // number of MuT Coords formed by MC hits formed.
  int _foundmut;    // was a MuTr track found associated with this MC track or not?
  int _ntracks;     // number of reconstructed tracks associated with this MC track
  int _nhitsfound;  // number of correct MC hits found
  int _ncoordsShared; // number of coordinates shared by other tracks
  int _nstationst;  // number of stations hit by the MC track
  int _nhitsN;      // total hits in the north FVTX system
  int _nhitsS;      // total hits in the south FVTX system
  int _nhitsfoundt; // number of correct MC hits found
  int _ncoordsN;      // total coords in the north FVTX system
  int _ncoordsS;      // total coords in the south FVTX system
  int _nhitsfoundt1; // number of correct MC hits found at station 1
  int _nhitsfoundt2; // number of correct MC hits found at station 2
  int _nhitsfoundt3; // number of correct MC hits found at station 3
  int _nhitsfoundt4; // number of correct MC hits found at station 4
  int _recosuc;      // Was the track fit successful?
  int _recosuc_mut;  // Was the mutr track fit successful?
  int _coord1;      // Key for coord found in station 1
  int _coord2;      //  Key for coord found in station 2
  int _coord3;      //  Key for coord found in station 3
  int _coord4;      //  Key for coord found in station 4
  int _nhitsfound_mut;  //Number of MC hits picked up in track finding
  int _mutmatch;    // Was a mutr track associated to this fvtx track?
  int _muttrid;     // Mutr MC Track id for associated track
  short _hasAncB;   // has a B in its ancestry
  short _hasAncD;   // has a D in its ancestry
  short _primId;    // id in primary particle list, negative if not in primary node
  #endif
  //@}

  //!@name MC traj_eval tree
  //@{
  #ifndef __CINT__
  TTree *_mc_traj_eval_tree;

  double _xmc;
  double _ymc;
  double _zmc;
  double _xreco;
  double _yreco;
  double _zreco;
  int _itrack;
  #endif
  //@}

  //!@name MC cluster tree
  //@{
  #ifndef __CINT__
  TTree *_mc_cluster_tree;

  int _ncoords;
  int _ntrack_hits;
  double _w;
  double _xbegin;
  double _xend;
  double _ybegin;
  double _yend;
  double _xmcc;
  double _ymcc;
  double _res;
  #endif

  //@}
  //!@name fvtx hit tree
  //@{
  #ifndef __CINT__
  TTree *_mc_fvtx_hit_tree;

  int _fvtx_strip;
  double _zmcc;
  UShort_t _adc;
  double _qhit;

  #endif
  //@}
  //@}
  //!@name event tree
  //@{
  #ifndef __CINT__
  TTree *_event_tree;

  int _nhits_ev;
  int _narms;
  int _nstations;
  int _sector0;
  int _sector1;
  int _sector2;
  int _sector3;
  int _strip0;
  int _strip1;
  int _strip2;
  int _strip3;
  int _xbegin0;
  int _xbegin1;
  int _xbegin2;
  int _xbegin3;
  int _ybegin0;
  int _ybegin1;
  int _ybegin2;
  int _ybegin3;
  int _xend0;
  int _xend1;
  int _xend2;
  int _xend3;
  int _yend0;
  int _yend1;
  int _yend2;
  int _yend3;

  #endif
  //@}

  //!@name fkin evaluation tree
  //@{
  #ifndef __CINT__
  TTree* _fkin_eval_tree;
  FKIN_ST _fkin;     // Data struct with fkin info
  short _nstationsHit; // add in the number of stations hit by track

  #endif
  //@}

  //!@name header evaluation tree
  //@{
  #ifndef __CINT__
  TTree* _header_eval_tree;
  HEADER_ST _header;     // Data struct with header info
  int _multSta[8];       // multiplicity of tracks, conditional on hitting N stations (N=0-7, meaning 1-8)

  #endif
  //@}

  //!@name pythia evaluation tree
  //@{
  #ifndef __CINT__
  TTree* _pythia_eval_tree;

  PYTHIA_ST _pythia;     // Data struct with header info

  #endif
  //@}

  //!@name pythia evaluation tree
  //@{
  #ifndef __CINT__
  TTree* _primary_eval_tree;

  PRIMARY_ST _primary;     // Data struct with header info

  #endif
  //@}

  //!@name coord evaluation tree
  //@{
  #ifndef __CINT__
  TTree* _coord_eval_tree;

  int _index;
  std::vector<int> _stripv;
  double _phi_begin;
  double _phi_end;
  double _r;

  #endif
  //@}

  //!@name match evaluation tree
  //@{
  #ifndef __CINT__
  TTree* _match_eval_tree;

  FvtxMCMatch _mc_match;

  #endif
  //@}

  //!@name straight-line reconstruction evaluation tree
  //@{
  #ifndef __CINT__
  TTree* _straight_trk_eval_tree;
  int _track;
  int _nhits;
  float _rchi2;
  float _ndf;
  float _x_int;
  float _y_int;
  float _x_slope;
  float _y_slope;
  float _phi_rot;
  bool _foundVtxHit;
  boost::array<int,20> _halfWedgeId;
  boost::array<int,20> _nstrips;
  boost::array<float,20> _x_hit;
  boost::array<float,20> _y_hit;
  boost::array<float,20> _z_hit;
  boost::array<float,20> _r_hit;
  boost::array<float,20> _phi_hit;

  #endif
  //@}

  //! module timer
  PHTimer* _timer;

  //! root filename
  std::string _file_name;
	bool _do_save_file;

  //! top node
  PHCompositeNode *_top_node;

  // Nodes for input signal
  //! signal node for MC DST
  std::string _signalNodeName;
  std::string _fvtxNodeName;    
  PHCompositeNode* _signal_top_node;

  //! pointer to Forward vertex MC hit map
  TFvtxMCHitMap *_fvtx_mc_hit_map;
  TFvtxPisaHitMap* _vtx_mc_hit_map;
  TMutMCHitMap *_mut_mc_hit_map;
  TMutMCTrkMap *_mut_mc_trk_map;
  TFvtxTrkMap *_fvtx_trk_map;
  TFvtxHitMap *_fvtx_hit_map;
  TFvtxClusMap *_fvtx_clus_map;
  TFvtxCoordMap *_fvtx_coord_map;
  TFvtxEvalMap *_fvtx_eval_map;
  TMuiRoadMapO* _road_mapO;

  bool _do_fkin_eval;
  fkinWrapper* _fkin_ptr;      //!

  bool _do_header_eval;
  headerWrapper* _header_ptr;     //!

  bool _do_pythia_eval;
  pythiaWrapper* _pythia_ptr;     //!

  PHPythiaContainer* _phpythia_node; //!

  //bool _do_primary_eval;
  PHCompositeNode* _primary_node;  //!
  primaryWrapper* _primary_ptr;     //!
  std::map<int,int> _trackPriMap; //!

  VtxOut* _vtxout_node;

  std::vector<TTree*> _eval_trees; //!

  bool _do_match_mod;
};

#endif
