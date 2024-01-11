// $Id: FvtxReco.cxx,v 1.53 2017/12/14 02:02:20 shlim Exp $

/*!
	\file		FvtxReco.cxx	
	\ingroup supermodules
	\brief	 Fvtx reconstruction module. 
	Reads TFvtxHits from DST, create clusters coordinates and tracks
	\author	H. Pereira Da Costa
	\version $Revision: 1.53 $
	\date		$Date: 2017/12/14 02:02:20 $
*/

#include <unistd.h> // for sleep
#include <sys/resource.h>
#include <Fun4AllReturnCodes.h>

#include <mFvtxEmbedPar.h>
#include <mFvtxEmbedSili.h>
#include <mFvtxFindClus.h>
#include <mFvtxFindCoord.h>
#include <mFvtxFindTrackMC.h>
#include <mFvtxFindTrackMCPar.h>
#include <mFvtxFindTrack.h>
#include <mFvtxFindTracks.h>
#include <mFvtxFindHoughTracks.h>
#include <mFvtxFindSvxClusters.h>
#include <mFvtxAddVtxHits.h>
#include <mFvtxFindTrackPar.h>
#include <mFvtxKalFit.h>
#include <mFvtxStraightLineFit.h>
#include <mFvtxKalFitPar.h>
#include <mFvtxRejectTrack.h>
#include <mFvtxRejectTrackPar.h>
#include <mMutKalFitWithSiliReal.h>
#include <mMutKalFitWithSiliRealPar.h>
#include <recoConsts.h>
#include <TFvtxStraightTrkParMap.h>
#include <TFvtxResidualMap.h>
#include <TFvtxDeadMap.h>
#include <TFvtxCompactCoordMap.h>
#include <RawDataCheck.h>

#include <FvtxReco.h>

using namespace std;

//______________________________________________________
FvtxReco::FvtxReco() :
  MuonSubsysReco("FVTXRECO"),
  _eventCount(0),
  _finder(3), // UNM Hough finder is now default
//  _run_afterBurner(false),
//  _use_HI_cuts(false),
//  _use_svx_cluster(true),
  _n_svx_layers(2),
  _rClusCut(0.003),// 30 microns (first pass)
  _alphaClusCut(0.0003),
	_n_coord_cut(-1),
	_n_svxclus_cut(-1),
  _do_mutr_matching(false),
  _do_mutkalfiteval(false),
  _do_embedding(false),
//  _fvtx_mutr_proximity_cut(0.0),   // in cm. disable this cut be default and mMutKalFitWithSiliReal::get_fvtx_mutr_match_cut will be used
//  _auto_load_dead_map(true),
//  _dead_map_name(""),
  _fvtx_node(NULL),
  _mFvtxEmbedSili_mod (NULL),
  _mFvtxFindClus_mod (NULL),
  _mFvtxFindCoord_mod (NULL),
  _mFvtxFindTrackMC_mod (NULL),
  _mFvtxFindTrack_mod (NULL),
  _mFvtxFindTracks_mod (NULL),
  _mFvtxFindHoughTracks_mod (NULL),
  _mFvtxAddVtxHits_mod (NULL),
  _mFvtxKalFit_mod (NULL),
  _mFvtxRejectTrack_mod (NULL),
  _mMutKalFitWithSiliReal_mod (NULL),
  _mFvtxStraightLineFit_mod (NULL),
  _mFvtxFindSvxClusters_mod (NULL),
  _timer(PHTimeServer::get()->insert_new("FvtxReco") )
{}

FvtxReco::~FvtxReco()
{
  delete _mFvtxFindSvxClusters_mod;
  delete _mFvtxEmbedSili_mod;
  delete _mFvtxFindClus_mod;
  delete _mFvtxFindCoord_mod;
  delete _mFvtxFindTrackMC_mod;
  delete _mFvtxFindTrack_mod;
  delete _mFvtxFindTracks_mod;
  delete _mFvtxFindHoughTracks_mod;
  delete _mFvtxAddVtxHits_mod;
  delete _mFvtxRejectTrack_mod;
  delete _mFvtxKalFit_mod;
  delete _mFvtxStraightLineFit_mod;
  delete _mMutKalFitWithSiliReal_mod;
}

int
FvtxReco::Init(PHCompositeNode *top_node)
{
  // Message to module builders:
  // DO NOT call FvtxGeom::get_arm() at this step.
  //
  // This is due to that at FvtxReco::Init, the runnumber is not nessisarily set and therefore
  // FvtxGeom::get_arm() may not be able to determine which geometry to load.
  // First first call of FvtxGeom::get_arm() should be at the step of FvtxReco::InitRun
  // or later.

  MuonSubsysReco::Init(top_node);

  create_node_tree(top_node);

  // Instantiate FVTXOO analysis modules
  _mFvtxFindSvxClusters_mod = new mFvtxFindSvxClusters();
  _mod_map.insert(make_pair("mFvtxFindSvxClusters",_mFvtxFindSvxClusters_mod));
  _mFvtxEmbedSili_mod = new mFvtxEmbedSili();
  _mod_map.insert(make_pair("mFvtxEmbedSili",_mFvtxEmbedSili_mod));
  _mFvtxFindClus_mod = new mFvtxFindClus();
  _mod_map.insert(make_pair("mFvtxFindClus",_mFvtxFindClus_mod));
  _mFvtxFindCoord_mod = new mFvtxFindCoord();
  _mod_map.insert(make_pair("mFvtxFindCoord",_mFvtxFindCoord_mod));
  _mFvtxFindTrackMC_mod = new mFvtxFindTrackMC();
  _mod_map.insert(make_pair("mFvtxFindTrackMC",_mFvtxFindTrackMC_mod));
  _mFvtxFindTrack_mod = new mFvtxFindTrack();
  _mod_map.insert(make_pair("mFvtxFindTrack",_mFvtxFindTrack_mod));
  _mFvtxFindTracks_mod = new mFvtxFindTracks();
  _mod_map.insert(make_pair("mFvtxFindTracks",_mFvtxFindTracks_mod));
  _mFvtxFindHoughTracks_mod = mFvtxFindHoughTracks::GetInstance();
  _mod_map.insert(make_pair("mFvtxFindHoughTracks",_mFvtxFindHoughTracks_mod));
  _mFvtxAddVtxHits_mod = new mFvtxAddVtxHits();
  _mod_map.insert(make_pair("mFvtxAddVtxHits",_mFvtxAddVtxHits_mod));
  _mFvtxRejectTrack_mod = new mFvtxRejectTrack();
  _mod_map.insert(make_pair("mFvtxRejectTrack",_mFvtxRejectTrack_mod));
  _mFvtxKalFit_mod = new mFvtxKalFit();
  _mod_map.insert(make_pair("mFvtxKalFit",_mFvtxKalFit_mod));
  _mFvtxStraightLineFit_mod = new mFvtxStraightLineFit();
  _mod_map.insert(make_pair("mFvtxStraightLineFit",_mFvtxStraightLineFit_mod));
  _mMutKalFitWithSiliReal_mod = new mMutKalFitWithSiliReal(); 
  _mod_map.insert(make_pair("mMutKalFitWithSiliReal",_mMutKalFitWithSiliReal_mod));

  
  _mFvtxFindSvxClusters_mod->init(top_node);
  _mFvtxEmbedSili_mod->init(top_node);
  _mFvtxFindTracks_mod->init(top_node);
  _mFvtxFindHoughTracks_mod->init(top_node);
  _mFvtxAddVtxHits_mod->init(top_node);
  _mFvtxStraightLineFit_mod->init(top_node);

  return EVENT_OK;
}

//______________________________________________________
int FvtxReco::InitRun(PHCompositeNode *top_node)
{
  TFvtxGlobalParCntrl::init_run();
  TFvtxGlobalParCntrl::print();
  TFvtxDeadMap::init_run();

  check_parameters(top_node);

  //Settings
  if(_finder == 1)
    {
      if (TFvtxGlobalParCntrl::get_bool_par("use_svx"))
	{
	  _mFvtxFindTracks_mod->set_use_svx_cluster(true);
	  _mFvtxFindTracks_mod->set_n_svx_layers(_n_svx_layers);
	}
      else
	{
	  _mFvtxFindTracks_mod->set_use_svx_cluster(false);
	}
    }
  else if(_finder == 3)
    {
      if (TFvtxGlobalParCntrl::get_bool_par("quick_muon_reco"))
	{
	  _mFvtxFindHoughTracks_mod->set_do_muon_quick_reco(true);
	}

      if (TFvtxGlobalParCntrl::get_bool_par("use_svx"))
	{
	  _mFvtxFindHoughTracks_mod->set_use_svx_cluster(true);
	  _mFvtxFindHoughTracks_mod->set_n_svx_layers(_n_svx_layers);
	}
      else{
	_mFvtxFindHoughTracks_mod->set_use_svx_cluster(false);
      }
    }

  if (TFvtxGlobalParCntrl::get_bool_par("use_svx"))
    {
      _mFvtxStraightLineFit_mod->set_use_svx_cluster(true);
    }

  _eventCount = 0;

  // Run-dependent initialization of modules
  //
  _mFvtxFindClus_mod->init_run(top_node);
  _mFvtxFindSvxClusters_mod->init_run(top_node);
  if (_do_embedding)
    _mFvtxEmbedSili_mod->init_run(top_node);
  _mFvtxFindTracks_mod->init_run(top_node);
  _mFvtxFindHoughTracks_mod->init_run(top_node);
  _mFvtxKalFit_mod->init_run(top_node);

  return 0;
}

//______________________________________________________
int FvtxReco::create_node_tree(PHCompositeNode *top_node)
{
  {
    // FVTX working space _fvtx_node
    PHNodeIterator nodeItr(top_node);
    _fvtx_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
    if(!_fvtx_node){
      _fvtx_node = new PHCompositeNode("FVTXOO");
      top_node->addNode(_fvtx_node);
    }
  }

  // PHCompositeNode *dst_node;
  PHCompositeNode* dst_node = 0;
  {
    PHNodeIterator nodeItr(top_node);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!dst_node) {
      dst_node = new PHCompositeNode("DST");
      top_node->addNode(dst_node);
    }
  }

  // interface object containers
  TFvtxClusMap* clus_map = TMutNode<TFvtxClusMap>::new_node( _fvtx_node, "TFvtxClusMap" );
  clus_map->make_persistant(dst_node,"TFvtxClus");

  TFvtxCoordMap* coord_map = TMutNode<TFvtxCoordMap>::new_node( _fvtx_node, "TFvtxCoordMap" );
  coord_map->make_persistant(dst_node,"TFvtxCoord");

  TFvtxCompactCoordMap* ccoord_map = TMutNode<TFvtxCompactCoordMap>::new_node( _fvtx_node, "TFvtxCompactCoordMap" );
  ccoord_map->make_persistant(dst_node,"TFvtxCompactCoord");

  TFvtxTrkMap* trk_map = TMutNode<TFvtxTrkMap>::new_node( _fvtx_node, "TFvtxTrkMap" );
  trk_map->make_persistant(dst_node,"TFvtxTrk");

  TFvtxCompactTrkMap* ctrk_map = TMutNode<TFvtxCompactTrkMap>::new_node( _fvtx_node, "TFvtxCompactTrkMap" );
  ctrk_map->make_persistant(dst_node,"TFvtxCompactTrk");

  TFvtxResidualMap* resid_map = TMutNode<TFvtxResidualMap>::new_node( _fvtx_node, "TFvtxResidualMap" );
  resid_map->make_persistant(dst_node,"TFvtxResidual");

  TFvtxStraightTrkParMap* trkpar_map =
    TMutNode<TFvtxStraightTrkParMap>::new_node( _fvtx_node, "TFvtxStraightTrkParMap" );
  trkpar_map->make_persistant(dst_node,"TFvtxStraightTrkPar");

  TFvtxSvxClusterMap* svxclus_map =
    TMutNode<TFvtxSvxClusterMap>::new_node( _fvtx_node, "TFvtxSvxClusterMap" );
//  svxclus_map->make_persistant(dst_node,"TFvtxSvxClusterMap"); // the convention should be TFvtxSvxCluster
  svxclus_map->make_persistant(dst_node,"TFvtxSvxCluster");

  // Module parameters tables
  mFvtxEmbedPar *mFvtxEmbed_par = TMutNode<mFvtxEmbedPar>::new_node( _fvtx_node, "mFvtxEmbedPar" );
  mFvtxEmbed_par->set_verbosity( FVTXOO::NONE );

  // Module parameters tables
  mFvtxFindClusPar *mFvtxFindClus_par = TMutNode<mFvtxFindClusPar>::new_node( _fvtx_node, "mFvtxFindClusPar" );
  mFvtxFindClus_par->set_verbosity( FVTXOO::NONE );
//  mFvtxFindClus_par->set_auto_load_dead_map(_auto_load_dead_map);
//  mFvtxFindClus_par->set_dead_map_name(_dead_map_name.c_str());

  mFvtxFindCoordPar *mFvtxFindCoord_par = TMutNode<mFvtxFindCoordPar>::new_node( _fvtx_node, "mFvtxFindCoordPar" );
  mFvtxFindCoord_par->set_verbosity( FVTXOO::NONE );

  mFvtxFindTrackMCPar *mFvtxFindTrackMC_par = TMutNode<mFvtxFindTrackMCPar>::new_node( _fvtx_node, "mFvtxFindTrackMCPar" );
  mFvtxFindTrackMC_par->set_verbosity( FVTXOO::NONE );

  mFvtxFindTrackPar *mFvtxFindTrack_par = TMutNode<mFvtxFindTrackPar>::new_node( _fvtx_node, "mFvtxFindTrackPar" );
  mFvtxFindTrack_par->set_verbosity( FVTXOO::NONE );
  mFvtxFindTrack_par->set_rClusCut(_rClusCut);
  mFvtxFindTrack_par->set_alphaClusCut(_alphaClusCut);
//  mFvtxFindTrack_par->set_useHICuts(_use_HI_cuts);

  mFvtxKalFitPar *mFvtxKalFit_par = TMutNode<mFvtxKalFitPar>::new_node( _fvtx_node, "mFvtxKalFitPar" );
  mFvtxKalFit_par->set_verbosity( FVTXOO::NONE );
//  if ( TFvtxGlobalParCntrl::get_bool_par("use_svx") ) mFvtxKalFit_par->set_use_vtx( true );

  mFvtxRejectTrackPar *mFvtxRejectTrack_par = TMutNode<mFvtxRejectTrackPar>::new_node( _fvtx_node, "mFvtxRejectTrackPar" );
  mFvtxRejectTrack_par->set_verbosity( FVTXOO::NONE );

  mMutKalFitWithSiliRealPar *mMutKalFitWithSiliReal_par = TMutNode<mMutKalFitWithSiliRealPar>::new_node( _fvtx_node, "mMutKalFitWithSiliRealPar" );
//  mMutKalFitWithSiliReal_par->set_verbosity( MUTOO::NONE );
  mMutKalFitWithSiliReal_par->set_do_evaluation(_do_mutkalfiteval );
//  mMutKalFitWithSiliReal_par->set_fvtx_mutr_proximity_cut(_fvtx_mutr_proximity_cut);

  mFvtxFindSvxClustersPar* mFvtxFindSvxClusters_par = TMutNode<mFvtxFindSvxClustersPar>::new_node( _fvtx_node, "mFvtxFindSvxClustersPar" );
  mFvtxFindSvxClusters_par->set_verbosity( FVTXOO::NONE );

  return 0;
}

void
FvtxReco::set_fvtx_mutr_proximity_cut(const Double_t& value)
{
  obsolete_warning("set_fvtx_mutr_proximity_cut","TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),\"mMutKalFitWithSiliRealPar\")");

  // This type of function is obsolete. One can use in future
  //  TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),"mMutKalFitWithSiliRealPar")
  //      ->set_fvtx_mutr_proximity_cut(...);

//  _fvtx_mutr_proximity_cut = value;
//
//  if (_fvtx_node)
//    {
//      try
//        {
//          mMutKalFitWithSiliRealPar *mMutKalFitWithSiliReal_par = TMutNode<
//              mMutKalFitWithSiliRealPar>::find_node(_fvtx_node,
//              "mMutKalFitWithSiliRealPar");
//
//          mMutKalFitWithSiliReal_par->set_fvtx_mutr_proximity_cut(
//              _fvtx_mutr_proximity_cut);
//        }
//      catch (...)
//        {
//        }
//    }
}

void
FvtxReco::set_do_mutkalfiteval(bool a)
{
  // This type of function is obsolete. One can use in future
//  TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),"mMutKalFitWithSiliRealPar")
//      ->set_do_evaluation(true);
  obsolete_warning("set_do_mutkalfiteval()", "TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),\"mMutKalFitWithSiliRealPar\")->set_do_evaluation();");

  _do_mutkalfiteval = a;

  if (_fvtx_node)
    {
      try
        {
          mMutKalFitWithSiliRealPar *mMutKalFitWithSiliReal_par = TMutNode<
              mMutKalFitWithSiliRealPar>::find_node(_fvtx_node,
              "mMutKalFitWithSiliRealPar");

          mMutKalFitWithSiliReal_par->set_do_evaluation(_do_mutkalfiteval);
        }
      catch (...)
        {
        }
    }
}


//______________________________________________________
int FvtxReco::check_parameters(PHCompositeNode */*top_node*/)
{
  // one can also reset run specific parameters here
  
  // Module parameters tables
  mFvtxFindClusPar *mFvtxFindClus_par = TMutNode<mFvtxFindClusPar>::find_node( _fvtx_node, "mFvtxFindClusPar" );

  mFvtxFindCoordPar *mFvtxFindCoord_par = TMutNode<mFvtxFindCoordPar>::find_node( _fvtx_node, "mFvtxFindCoordPar" );

  mFvtxFindTrackMCPar *mFvtxFindTrackMC_par = TMutNode<mFvtxFindTrackMCPar>::find_node( _fvtx_node, "mFvtxFindTrackMCPar" );

  mFvtxFindTrackPar *mFvtxFindTrack_par = TMutNode<mFvtxFindTrackPar>::find_node( _fvtx_node, "mFvtxFindTrackPar" );

  mFvtxKalFitPar *mFvtxKalFit_par = TMutNode<mFvtxKalFitPar>::find_node( _fvtx_node, "mFvtxKalFitPar" );
//  if ( _use_svx_cluster ) mFvtxKalFit_par->set_use_vtx( true ); // force to sync to seeting of FvtxReco

  mFvtxRejectTrackPar *mFvtxRejectTrack_par = TMutNode<mFvtxRejectTrackPar>::find_node( _fvtx_node, "mFvtxRejectTrackPar" );

  mMutKalFitWithSiliRealPar *mMutKalFitWithSiliReal_par = TMutNode<mMutKalFitWithSiliRealPar>::find_node( _fvtx_node, "mMutKalFitWithSiliRealPar" );

  mFvtxFindSvxClustersPar* mFvtxFindSvxClusters_par = TMutNode<mFvtxFindSvxClustersPar>::find_node( _fvtx_node, "mFvtxFindSvxClustersPar" );

  mFvtxEmbedPar* mFvtxEmbed_par = TMutNode<mFvtxEmbedPar>::find_node( _fvtx_node, "mFvtxEmbedPar" );

  if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) 
    {
      mFvtxFindSvxClusters_par->print();
      mFvtxEmbed_par->print();
      mFvtxFindClus_par->print();
      mFvtxFindCoord_par->print();
      mFvtxFindTrackMC_par->print();
      mFvtxFindTrack_par->print();
      mFvtxKalFit_par->print();
      mFvtxRejectTrack_par->print();
      mMutKalFitWithSiliReal_par->print();
    }

  return 0;
}

//______________________________________________________
int FvtxReco::process_event(PHCompositeNode *top_node)
{
  _eventCount++;


  //  std::cout << "FvtxReco::process_event: Processing event " << _eventCount << std::endl;  
  _timer.get()->restart();
  
  try {

      _mFvtxFindSvxClusters_mod->event(top_node);

      if (verbosity > 2)
        {
          struct rusage usage;
          getrusage(RUSAGE_SELF, &usage);

          cout << "FvtxReco::process_event::_mFvtxFindSvxClusters_mod current memory usage is " << usage.ru_maxrss
              << " kB" << endl;
        }

      if (_do_embedding)
        _mFvtxEmbedSili_mod->event(top_node);

      _mFvtxFindClus_mod->event(_fvtx_node);

      if (verbosity > 2)
        {
          struct rusage usage;
          getrusage(RUSAGE_SELF, &usage);

          cout << "FvtxReco::process_event::_mFvtxFindClus_mod current memory usage is " << usage.ru_maxrss
              << " kB" << endl;
        }
      _mFvtxFindCoord_mod->event(_fvtx_node);

			if ( _n_coord_cut>0 )
			{
				TFvtxCoordMap *coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
				int ncoord = coord_map->size();
				if ( ncoord>_n_coord_cut )
				{
					std::cout << "Skip the event of Num. Fvtx coord, " << ncoord << ", Cut:" << _n_coord_cut << std::endl;

					try
					{
						Event * event = TMutNode<Event>::find_node(top_node, "PRDF");
						RawDataCheck *raw = RawDataCheck::instance();
						raw->AddToList(event, "FVTXTOOMANYCOORD");
					}
					catch (const exception& e)
					{
						FVTXOO::TRACE(e.what());
					}

					return ABORTEVENT;
				}
			}//

			if ( _n_svxclus_cut>0 )
			{   
				TFvtxSvxClusterMap *svxclus_map = TMutNode<TFvtxSvxClusterMap>::find_node(top_node,"TFvtxSvxClusterMap");
				int nsvxclus = svxclus_map->size();
				if ( nsvxclus>_n_svxclus_cut )
				{   
					std::cout << "Skip the event of Num. SVX pixel cluster, " << nsvxclus << ", Cut:" << _n_svxclus_cut << std::endl;

					try 
					{   
						Event * event = TMutNode<Event>::find_node(top_node, "PRDF");
						RawDataCheck *raw = RawDataCheck::instance();
						raw->AddToList(event, "FVTXSVXTOOMANYCLUS");
					}   
					catch (const exception& e)
					{   
						FVTXOO::TRACE(e.what());
					}   

					return ABORTEVENT;
				}   
			}// 

      if (verbosity > 2)
        {
          struct rusage usage;
          getrusage(RUSAGE_SELF, &usage);

          cout << "FvtxReco::process_event::_mFvtxFindCoord_mod current memory usage is " << usage.ru_maxrss
              << " kB" << endl;
        }

      if (_finder == 0)
	{
	  _mFvtxFindTrack_mod->event(top_node);
	}
      else if (_finder == 1)
        {
          _mFvtxFindTracks_mod->event(top_node);
        }
      else if (_finder == 2)
        _mFvtxFindTrackMC_mod->event(top_node);
      else if (_finder == 3)
        {
          _mFvtxFindHoughTracks_mod->event(top_node);
        }
      else if (_finder < 0)
        {
          static bool once = true;

          if (once)
            {
              cout <<"FvtxReco::process_event - WARNING - tracking is disabled since set_finder = "<<_finder<<endl;

              once = false;
            }

        }
      else
        throw std::runtime_error(
            "FvtxReco::process_event: Invalid finder flag");

//     if ( _use_svx_cluster ) _mFvtxAddVtxHits_mod->set_use_svx_cluster(true);
//     else _mFvtxAddVtxHits_mod->set_use_svx_cluster(false);
//     if ( _run_afterBurner ) _mFvtxAddVtxHits_mod->event(top_node);

      if (verbosity > 2)
        {
          struct rusage usage;
          getrusage(RUSAGE_SELF, &usage);

          cout << "FvtxReco::process_event::_mFvtxFindTracks*_mod current memory usage is " << usage.ru_maxrss
              << " kB" << endl;
        }

      _mFvtxStraightLineFit_mod->event(top_node);

      if (verbosity > 2)
        {
          struct rusage usage;
          getrusage(RUSAGE_SELF, &usage);

          cout << "FvtxReco::process_event::_mFvtxStraightLineFit_mod current memory usage is " << usage.ru_maxrss
              << " kB" << endl;
        }
      _mFvtxKalFit_mod->event(_fvtx_node);

      if (verbosity > 2)
        {
          struct rusage usage;
          getrusage(RUSAGE_SELF, &usage);

          cout << "FvtxReco::process_event::_mFvtxKalFit_mod current memory usage is " << usage.ru_maxrss
              << " kB" << endl;
        }
      _mFvtxRejectTrack_mod->event(_fvtx_node);

      if (verbosity > 2)
        {
          struct rusage usage;
          getrusage(RUSAGE_SELF, &usage);

          cout << "FvtxReco::process_event::fill_compact_track current memory usage is " << usage.ru_maxrss
              << " kB" << endl;
        }
      _mFvtxKalFit_mod->fill_compact_coordinate();
      _mFvtxKalFit_mod->fill_compact_track();

      if (verbosity > 2)
        {
          struct rusage usage;
          getrusage(RUSAGE_SELF, &usage);

          cout << "FvtxReco::process_event::fill_compact_track current memory usage is " << usage.ru_maxrss
              << " kB" << endl;
        }
      if (_do_mutr_matching)
        _mMutKalFitWithSiliReal_mod->event(top_node);

    }
  catch (const exception& e)
    {
      FVTXOO::TRACE(e.what());
    }
  
  write_maps_if_needed();
  _timer.get()->stop();
  return 0;
}

//______________________________________________________
int FvtxReco::End(PHCompositeNode* top_node) 
{
  _mFvtxFindSvxClusters_mod->end(top_node);
  if (_do_embedding)
    _mFvtxEmbedSili_mod->end(top_node);
  _mFvtxFindClus_mod->end(top_node);
  _mFvtxFindCoord_mod->end(top_node);
  _mFvtxFindTrackMC_mod->end(top_node);
  _mFvtxFindTrack_mod->end(top_node);
  _mFvtxFindTracks_mod->end(top_node);
  _mFvtxFindHoughTracks_mod->end(top_node);
  _mFvtxRejectTrack_mod ->end(top_node);
  _mFvtxKalFit_mod->end(top_node);
  _mFvtxStraightLineFit_mod->end(top_node);
  _mFvtxAddVtxHits_mod->end(top_node);
  if (_do_mutr_matching) _mMutKalFitWithSiliReal_mod->finish_evaluation();
  _mMutKalFitWithSiliReal_mod->end(top_node);

  _timer.get()->print_stat();

  return 0;
}

void
FvtxReco::obsolete_warning(const std::string & function,
    const std::string & suggestion)
{
  std::cout << std::endl;
  std::cout << "FvtxReco - WARNING - FvtxReco::" << function << " is obsolete."
      << " Please use " << suggestion << " instead" << std::endl;
  std::cout << std::endl;
  sleep(5);
}
