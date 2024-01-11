#ifndef __MUONMCEval_H__
#define __MUONMCEval_H__

/*!
  \file FvtxEval.h
  \ingroup supermodules
  \brief MC evaluation module
  \author Hugo Pereira
  \version $Revision: 1.10 $
  \date $Date: 2012/12/21 21:29:53 $
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
#include <PHTable.hh>

#ifndef __CINT__
#include <boost/array.hpp>
#endif


// Forward declerations
class PHCompositeNode;
class PHTimer;
class TFvtxTrkMap;
class TFvtxHitMap;
class TMutHitMap;
class TMutGapCoordMap;
class TFvtxClusMap;
class TFvtxSvxClusterMap;
class TFvtxCoordMap;
class TFvtxEvalMap;

class mFvtxEval;

class headerWrapper;
class VtxOut;

/*!
  \class FvtxEval
  \ingroup supermodules
  \brief MC evaluation module
*/

class FvtxEval: public SubsysReco
{
  public:

  //! constructor
  FvtxEval( const char* name="FvtxEval", const char* file = "fvtx_eval.root" );

  //! destructor
  virtual ~FvtxEval();

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

  // ! changes signal node name
  void SetSignalNodeName(std::string name)
  { _signalNodeName = name; }

  void do_match_mod(bool val) { _do_match_mod = val; }
  bool do_match_mod() { return _do_match_mod; }

  void set_fit_model(int val) { _fit_model = val; }
  void set_do_mutr_matching(bool flag) { _do_mutr_matching = flag; }
  

  protected:

  //! set interface pointers
  void set_interface_pointers( PHCompositeNode* );

  void book_trk_eval_tree( void );
  void fill_trk_eval_tree( void );

  void book_cluster_tree( void );
  void fill_cluster_tree( void );

  void book_svx_cluster_tree( void );
  void fill_svx_cluster_tree( void );

  void book_fvtx_hit_tree( void );
  void fill_fvtx_hit_tree( void );

  void book_event_tree( void );
  void fill_event_tree( void );

  void book_event_hit_tree( void );
  void fill_event_hit_tree( void );

  void book_event_coord_tree( void );
  void fill_event_coord_tree( void );

  void book_coord_eval_tree( void );
  void fill_coord_eval_tree( void );

  void book_straight_trk_eval_tree( void );
  void fill_straight_trk_eval_tree( void );

  mFvtxEval* _mFvtxEvalMod;


  int _fit_model;

  //!@name radiograph tree
  //@{

  //! MC radiograph tree
  #ifndef __CINT__

  int _arm;
  int _cage;
  int _station;
  int _sector;
  int _plane;
  int _column;
  int _size;
  int _mut_arm;
  int _mut_station;
  int _mut_octant;
  int _mut_half_octant;
  int _mut_gap;
  int _mut_cathode;
  int _mut_strip;

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

  double _x0reco;
  double _y0reco;
  double _z0reco;
  double _px0reco;
  double _py0reco;
  double _pz0reco;
  float _chargereco;
  double _chisqreco;
  int    _ittrk;
  int    _parent_pdgid;
  int    _idtrk;
  int    _size1;
  int    _size2;
  int    _size3;
  int    _size4;
  int    _sector4;
  int _nsvxclusters;
  double _q1;
  double _q2;
  double _q3;
  double _q4;
  double _w1;
  double _w2;
  double _w3;
  double _w4;
  double _r1meas;
  double _z1meas;
  double _phi1meas;
  double _x1reco;
  double _y1reco;
  double _z1reco;
  double _r2meas;
  double _z2meas;
  double _phi2meas;
  double _x2reco;
  double _y2reco;
  double _z2reco;
  double _r3meas;
  double _z3meas;
  double _phi3meas;
  double _x3reco;
  double _y3reco;
  double _z3reco;
  double _r4meas;
  double _z4meas;
  double _phi4meas;
  double _x4reco;
  double _y4reco;
  double _z4reco;
  double _r1meas_svx;
  double _phi1meas_svx;
  double _x1reco_svx;
  double _y1reco_svx;
  double _z1reco_svx;
  double _x1meas_svx;
  double _y1meas_svx;
  double _z1meas_svx;
  double _r2meas_svx;
  double _phi2meas_svx;
  double _x2reco_svx;
  double _y2reco_svx;
  double _z2reco_svx;
  double _x2meas_svx;
  double _y2meas_svx;
  double _z2meas_svx;
  double _r3meas_svx;
  double _phi3meas_svx;
  double _x3reco_svx;
  double _y3reco_svx;
  double _z3reco_svx;
  double _x3meas_svx;
  double _y3meas_svx;
  double _z3meas_svx;
  double _r4meas_svx;
  double _phi4meas_svx;
  double _x4reco_svx;
  double _y4reco_svx;
  double _z4reco_svx;
  double _x4meas_svx;
  double _y4meas_svx;
  double _z4meas_svx;
  int _nhitsb; // number of hits from the barrel
  int _nvtxf; // number of barrel hits associated with this track ("found")
  int _nhitsf; // number of mc hits associated with this track
  double _x0recoerr;
  double _y0recoerr;
  int _ghost;
  double _chisqrecopdf_w;
  double _chisqrecopdf_r;
  double _pxmutr;
  double _pymutr;
  double _pzmutr;
  double _chi2mutr;
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
  int _nseg;
  double _vtxx;
  double _vtxy;
  double _vtxz;
  double _vtxxp;
  double _vtxyp;
  double _vtxzp;
  double _extrapx;
  double _extrapy;
  double _extrapz;

  boost::array<int,100> _coordIds;
  boost::array<double,100> _coordPhiStart;
  boost::array<double,100> _coordPhiEnd;
  boost::array<double,100> _coordR;
  boost::array<double,100> _coordZ;
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

  double _xreco;
  double _yreco;
  double _zreco;
  int _itrack;

  //!@name MC cluster tree
  //@{
  #ifndef __CINT__
  TTree *_cluster_tree;

  int _ncoords;
  int _ntrack_hits;
  double _w;
  double _xbegin;
  double _xend;
  double _ybegin;
  double _yend;
  double _zbegin;
  double _zend;
  double _res;
  #endif

  //@}
  //!@name SVX cluster tree
  //@{
  #ifndef __CINT__
  TTree *_svx_cluster_tree;

  int _hitID;
  int _svxSection;
  int _layer;
  int _ladder;
  int _sensor;
  double _x_global;
  double _y_global;
  double _z_global;
  double _x_local;
  double _y_local;
  double _z_local;
  #endif

  //@}
  //!@name fvtx hit tree
  //@{
  #ifndef __CINT__
  TTree *_fvtx_hit_tree;

  UShort_t _fvtx_strip;
  UShort_t _adc;
  int _packet_id;
  int _fem_id;
  int _chip_id;
  int _chan_id;
  double _qhit;

  #endif
  //@}
  //!@name event tree
  //@{
  #ifndef __CINT__
  TTree *_event_tree;

  int _nhits_ev;
  int _nhits_mut;
  int _narms;
  int _nstations;
  int _nstations_mut;
  int _sector0;
  int _sector1;
  int _sector2;
  int _sector3;
  int _strip0;
  int _strip1;
  int _strip2;
  int _strip3;
  double _xbegin0;
  double _xbegin1;
  double _xbegin2;
  double _xbegin3;
  double _ybegin0;
  double _ybegin1;
  double _ybegin2;
  double _ybegin3;
  double _zbegin0;
  double _zbegin1;
  double _zbegin2;
  double _zbegin3;
  double _xend0;
  double _xend1;
  double _xend2;
  double _xend3;
  double _yend0;
  double _yend1;
  double _yend2;
  double _yend3;
  double _zend0;
  double _zend1;
  double _zend2;
  double _zend3;
  double _r0;
  double _r1;
  double _r2;
  double _r3;
  double _z0;
  double _z1;
  double _z2;
  double _z3;
  double _phi0;
  double _phi1;
  double _phi2;
  double _phi3;
  double _bbcz;

  #endif

  #ifndef __CINT__
  TTree *_event_hit_tree;

  double _rpos;
  double _zpos;
  double _phipos;

  #endif
  #ifndef __CINT__
  TTree *_event_coord_tree;

  int _gap;
  int _iarmf;
  int _iarmm;
  int _ncoordsf;
  int _ncoordsm;
  double _rposf1;
  double _zposf1;
  double _phiposf1;
  double _rposf2;
  double _zposf2;
  double _phiposf2;
  double _rposf3;
  double _zposf3;
  double _phiposf3;
  double _rposf4;
  double _zposf4;
  double _phiposf4;

  double _rposm1;
  double _zposm1;
  double _phiposm1;
  double _rposm2;
  double _zposm2;
  double _phiposm2;
  double _rposm3;
  double _zposm3;
  double _phiposm3;
  double _rposm4;
  double _zposm4;
  double _phiposm4;
  double _rposm5;
  double _zposm5;
  double _phiposm5;
  double _rposm6;
  double _zposm6;
  double _phiposm6;
  double _rposm7;
  double _zposm7;
  double _phiposm7;
  double _rposm8;
  double _zposm8;
  double _phiposm8;
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

  #endif
  //@}

  //!@name straight-line reconstruction evaluation tree
  //@{
  #ifndef __CINT__
  TTree* _straight_trk_eval_tree;
  int _track;
  int _nhits;
  int _column1;
  int _column2;
  int _column3;
  int _column4;
  float _rchi2;
  float _ndf;
  float _x_int;
  float _y_int;
  float _x_slope;
  float _y_slope;
  float _phi_rot;
  bool _foundVtxHit;
  int _recosuc;      // Was the track fit successful?
  int _coord1;      // Key for coord found in station 1
  int _coord2;      //  Key for coord found in station 2
  int _coord3;      //  Key for coord found in station 3
  int _coord4;      //  Key for coord found in station 4
  int _ncoordsShared; // number of coordinates shared by other tracks

  boost::array<int,100> _halfWedgeId;
  boost::array<int,100> _nstrips;
  boost::array<float,100> _x_hit;
  boost::array<float,100> _y_hit;
  boost::array<float,100> _z_hit;
  boost::array<float,100> _r_hit;
  boost::array<float,100> _phi_hit;

  #endif
  //@}

  //! module timer
  PHTimer* _timer;

  //! root filename
  std::string _file_name;

  //! top node
  PHCompositeNode *_top_node;

  // Nodes for input signal
  //! signal node for MC DST
  std::string _signalNodeName;
  PHCompositeNode* _signal_top_node;

  //! pointer to Forward vertex MC hit map
  TFvtxTrkMap *_fvtx_trk_map;
  TFvtxHitMap *_fvtx_hit_map;
  TMutHitMap *_mut_hit_map;
  TMutGapCoordMap *_mut_gap_coord_map;
  TFvtxClusMap *_fvtx_clus_map;
  TFvtxSvxClusterMap *_svx_clus_map;
  TFvtxCoordMap *_fvtx_coord_map;
  TFvtxEvalMap *_fvtx_eval_map;


  std::map<int,int> _trackPriMap; //!

  VtxOut* _vtxout_node;

  std::vector<TTree*> _eval_trees; //!

  bool _do_match_mod;
  bool _do_mutr_matching;

};

#endif
