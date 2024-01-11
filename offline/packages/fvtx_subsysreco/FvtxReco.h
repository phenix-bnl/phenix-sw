// $Id: FvtxReco.h,v 1.36 2017/12/14 02:02:19 shlim Exp $
#ifndef __FvtxReco_h__
#define __FvtxReco_h__

/*!
	\file		FvtxReco.h	
	\ingroup supermodules
	\brief	 RPC reconstruction module. 
	Reads TFvtxHits from DST, create clusters coordinates and tracks
	\author	H. Pereira Da Costa
	\version $Revision: 1.36 $
	\date		$Date: 2017/12/14 02:02:19 $
*/
#ifndef __CINT__
//#include <boost/shared_ptr.hpp>
//#include <boost/make_shared.hpp>
#include <PHTimeServer.h>
#endif //__CINT__

#include <iostream>
#include <string>
#include <utility>
#include <map>
#include <memory>
#include <MuonSubsysReco.h>
#include <TFvtxGlobalParCntrl.h>

// Forward declerations
class PHCompositeNode;
class mFvtxEmbedSili;
class mFvtxFindClus;
class mFvtxFindSvxClusters;
class mFvtxFindCoord;
class mFvtxFindTrackMC;
class mFvtxFindTrack;
class mFvtxFindTracks;
class mFvtxFindHoughTracks;
class mFvtxAddVtxHits;
class mFvtxKalFit;
class mFvtxStraightLineFit;
class mFvtxRejectTrack;
class mMutKalFitWithSiliReal;
class mFvtxModuleBase;

//! FVTX Reconstruction module
/*!
	\ingroup supermodules
	Forward vertex reconstruction module. 
	Reads TFvtxHits from DST, create clusters coordinates and tracks
*/
class FvtxReco: public MuonSubsysReco
{
  friend class FvtxRecoWithMut;  //! Allow FvtxRecoWithMut to retrieve module pointers

public:
  
  //! constructor
  FvtxReco();

  //! destructor
  ~FvtxReco();

  //! module initialization
  //!
  //! Message to module builders:
  //! DO NOT call FvtxGeom::get_arm() at this step.
  //!
  //! This is due to that at FvtxReco::Init, the runnumber is not nessisarily set and therefore
  //! FvtxGeom::get_arm() may not be able to determine which geometry to load.
  //! First first call of FvtxGeom::get_arm() should be at the step of FvtxReco::InitRun
  //! or later.
  int Init(PHCompositeNode *topNode);

  //! run initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! event processing
  int process_event(PHCompositeNode *topNode);
  
  //! end of process
  int End(PHCompositeNode *topNode);

  void
  set_finder(int val)
  {
    std::cout << "set finder = " << val << std::endl;
    _finder = val;
  }
  void
  set_do_mutr_matching(bool flag)
  {
    _do_mutr_matching = flag;
  }
  void
  set_finder_eval(bool flag)
  {
    obsolete_warning("set_finder_eval()", "FvtxEval module");
//    _do_finder_eval = flag;
  }
  void
  run_after_burner(bool ab)
  {
    obsolete_warning("run_after_burner()",
        "TFvtxGlobalParCntrl::set_bool_flag(\"use_svx\")");
//    _run_afterBurner = ab;
  }
  void
  set_use_HI_cuts(bool flag)
  {
    obsolete_warning("set_use_HI_cuts()",
        "TFvtxGlobalParCntrl::set_bool_flag(\"is_pp\")");
    TFvtxGlobalParCntrl::set_bool_par("is_pp", !flag);
//    _use_HI_cuts = flag;
  }
  void
  set_use_svx_cluster(bool flag)
  {
    obsolete_warning("set_use_svx_cluster()",
        "TFvtxGlobalParCntrl::set_bool_flag(\"use_svx\")");
    TFvtxGlobalParCntrl::set_bool_par("use_svx", flag);
//    _use_svx_cluster = flag;
  }
  void
  set_n_svx_layers(int nlay)
  {

    _n_svx_layers = nlay;
  }
  void
  set_rClusCut(double val)
  {
    obsolete_warning("set_n_svx_layers()", "TMutNode<mFvtxFindTrackPar>::find_node(se->topNode(),\"mFvtxFindTrackPar\")->set_rClusCut();");
    _rClusCut = val;
  }
  void
  set_alphaClusCut(double val)
  {
    obsolete_warning("set_n_svx_layers()", "TMutNode<mFvtxFindTrackPar>::find_node(se->topNode(),\"mFvtxFindTrackPar\")->set_alphaClusCut();");
    _alphaClusCut = val;
  }
	void 
  set_n_coord_cut(int max_cut)
  {
    _n_coord_cut = max_cut;
  }
	void 
	set_n_svxclus_cut(int max_cut)
	{
		_n_svxclus_cut = max_cut;
	}


//  //! proximity cut value for matching MuTr track to Fvtx track
//  const Double_t& get_fvtx_mutr_proximity_cut( void ) const
//  { return _fvtx_mutr_proximity_cut; }

  //! proximity cut value for matching MuTr track to Fvtx track
  //! track will be accepted if proximity smaller than this value;
  //! otherwise, it will be compared with a second cut as defined
  //! in mMutKalFitWithSiliReal::get_fvtx_mutr_match_cut();
  //!
  //! This type of function is obsolete. One can use in future
  //!  TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),"mMutKalFitWithSiliRealPar")->set_fvtx_mutr_proximity_cut(...);
  void set_fvtx_mutr_proximity_cut( const Double_t& value );

  //! make mMutKalFitWithSili evaluation ntuple
  //!
  //! This type of function is obsolete. One can use in future
  //!  TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),"mMutKalFitWithSiliRealPar")->set_do_evaluation(true);
  void set_do_mutkalfiteval(bool a) ;

  bool
  is_do_embedding() const
  {
    return _do_embedding;
  }

  void
  set_do_embedding(bool doembedding = true)
  {
    _do_embedding = doembedding;
  }

  //! @name dead channel map management - obsolete, use TFvtxDatabaseCtrl
  //@{

  // ! automatic load dead channel map according to run number
  Bool_t
  get_auto_load_dead_map() const
  {
    obsolete_warning("get_auto_load_dead_map()",
        "TFvtxGlobalParCntrl::get_bool_par(\"deadmap_auto_load\")");
    return TFvtxGlobalParCntrl::get_bool_par("deadmap_auto_load");
  }

  // ! automatic load dead channel map according to run number
  void
  set_auto_load_dead_map(Bool_t a = true)
  {
    obsolete_warning("set_auto_load_dead_map()",
        "TFvtxGlobalParCntrl::set_bool_par(\"deadmap_auto_load\")");
    TFvtxGlobalParCntrl::set_bool_par("deadmap_auto_load", a);
  }

  //! database name for dead channel map
  const std::string
  get_dead_map_name() const
  {
    obsolete_warning("get_dead_map_name()",
        "TFvtxGlobalParCntrl::get_string_par(\"deadmap_fvtxdb_use_custom_deadmap\")");
    return TFvtxGlobalParCntrl::get_string_par(
        std::string("deadmap_fvtxdb_use_custom_deadmap"));
  }

  //! database name for dead channel map
  void
  set_dead_map_name(std::string n)
  {
    obsolete_warning("set_dead_map_name()",
        "TFvtxGlobalParCntrl::set_string_par(\"deadmap_fvtxdb_use_custom_deadmap\")");
    TFvtxGlobalParCntrl::set_string_par(
        std::string("deadmap_fvtxdb_use_custom_deadmap"), n);
  }

  //@}

protected:


  static void obsolete_warning(const std::string & function, const std::string & suggestion);

  int _eventCount;

  int _finder;
//  bool _do_finder_eval;
//  bool _run_afterBurner;
//  bool _use_HI_cuts;
//  bool _use_svx_cluster;
  int _n_svx_layers;
  double _rClusCut;
  double _alphaClusCut;
	int _n_coord_cut;
	int _n_svxclus_cut;
  bool _do_mutr_matching;
  bool _do_mutkalfiteval;
  bool _do_embedding;

  //! proximity cut value for matching MuTr track to Fvtx track
  //! track will be accepted if proximity smaller than this value;
  //! otherwise, it will be compared with a second cut as defined
  //! in mMutKalFitWithSiliReal::get_fvtx_mutr_match_cut();
//  Double_t _fvtx_mutr_proximity_cut;

  //! create all new nodes
  int create_node_tree(PHCompositeNode *topNode);

  /* //! register modules with the bookeeping map */
  /* template<typename T> boost::shared_ptr<T>   */
  /* registerModule(const std::string& modName)     */
  /* { */
  /*   pBase ptr = pBase(new T()); // Store in a base type shared_ptr */
  /*   _mod_map.insert(std::make_pair(modName, ptr)); */
  /*   return boost::dynamic_pointer_cast<T>(ptr); // Cast back to derived for return */
  /* } */

  //! retrieve shared pointer to module by name lookup
  /* template<typename T> pBase<T> */
  /* getModule(const std::string& modName) const */
  /* { */
  /*   std::map<std::string,pBase>::const_iterator it = _mod_map.find(modName); */
  /*   if ( it != _mod_map.end() ) */
  /*     return it->second; */
  /*   else */
  /*     return NULL; // Return a null shared ptr if not found */
  /*  }  */

  //! retrive parameter table and print to cout
  //! one can also reset run specific parameters here
  int check_parameters(PHCompositeNode *topNode);
  
  //! working node
  PHCompositeNode* _fvtx_node;

  //! Map to track and retrieve the modules 
  typedef mFvtxModuleBase* pBase;
  std::map<std::string,pBase> _mod_map;

  //! cluster finder
  mFvtxEmbedSili* _mFvtxEmbedSili_mod;
  
  //! cluster finder
  mFvtxFindClus* _mFvtxFindClus_mod;

  //! coordinate finder
  mFvtxFindCoord* _mFvtxFindCoord_mod;
  
  //! Perfect track finder
  mFvtxFindTrackMC* _mFvtxFindTrackMC_mod;
  
  //! Real track finder
  mFvtxFindTrack* _mFvtxFindTrack_mod;
  
  //! Real track finder -- Nevis prototype
  mFvtxFindTracks* _mFvtxFindTracks_mod;

  //! Real track finder -- Hough pattern matching (UNM)
  mFvtxFindHoughTracks* _mFvtxFindHoughTracks_mod;

  //! Track AfterBurner
  mFvtxAddVtxHits* _mFvtxAddVtxHits_mod;

  //! Kalman Filter fitter
  mFvtxKalFit* _mFvtxKalFit_mod;

  //! Rejection of ghost tracks
  mFvtxRejectTrack* _mFvtxRejectTrack_mod;

  //! Match MuTr and FVTX tracks and re-fit.
  mMutKalFitWithSiliReal* _mMutKalFitWithSiliReal_mod;

  //!
  mFvtxStraightLineFit* _mFvtxStraightLineFit_mod;

  //!
  mFvtxFindSvxClusters* _mFvtxFindSvxClusters_mod;

//! module timer
#ifndef __CINT__
  PHTimeServer::timer _timer; 
#endif
};

#endif 
