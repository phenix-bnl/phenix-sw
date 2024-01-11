// $Id: FvtxRecoWithMut.cxx,v 1.14 2015/06/11 21:02:58 snowball Exp $

/*!
	\file		FvtxRecoWithMut.cxx	
	\ingroup supermodules
	\brief	 Reconstruct Muon tracks with silicon hits
	\author	 Melynda Brooks
	\version $Revision: 1.14 $
	\date		$Date: 2015/06/11 21:02:58 $
*/

#include <unistd.h> // for sleep
#include <mMutKalFitWithSili.h>
#include <mMutKalFitWithSiliReal.h>
#include <mMutKalFitWithSiliPar.h>
#include <mMutKalFitWithSiliRealPar.h>
#include <mFvtxFindHoughTracks.h>
#include <mFvtxFindTrackPar.h>
#include <PHTimer.h>
#include <PHTimeServer.h>
#include <recoConsts.h>
#include <Fun4AllServer.h>
#include <FvtxReco.h>

#include "FvtxRecoWithMut.h"

using namespace std;

//______________________________________________________
FvtxRecoWithMut::FvtxRecoWithMut() :
  MuonSubsysReco("FvtxRecoWithMut"),
  _do_mutkalfiteval(false),
  //fvtx_mutr_proximity_cut(0),
  _fvtx_node(NULL),
  _mMutKalFitWithSili_mod(NULL),
  _mMutKalFitWithSiliReal_mod(NULL),
  _mFvtxFindHoughTracks_mod(static_cast<mFvtxFindHoughTracks*>(NULL)),
  _track_mod_par(NULL),
  _timer( new PHTimer("FvtxRecoWithMut") )
{}

//______________________________________________________
FvtxRecoWithMut::~FvtxRecoWithMut()
{

  cout <<"FvtxRecoWithMut::destructor - Info - Clean up internal modules ..."<<endl;

  if (_mMutKalFitWithSili_mod)
    {
      delete _mMutKalFitWithSili_mod;
      _mMutKalFitWithSili_mod = NULL;
    }
  if (_mMutKalFitWithSiliReal_mod)
    {
      delete _mMutKalFitWithSiliReal_mod;
      _mMutKalFitWithSiliReal_mod = NULL;
    }
  if (_timer)
    {
      delete _timer;
      _timer = NULL;
    }

}

//______________________________________________________
int
FvtxRecoWithMut::Init(PHCompositeNode *top_node)
{
  MuonSubsysReco::Init(top_node);

  create_node_tree(top_node);

  // The track parameter node already exists, just grab it.
  _track_mod_par = TMutNode<mFvtxFindTrackPar>::find_node( top_node, "mFvtxFindTrackPar" );

  // Retrieve a pointer to the mFvtxFindHoughTracks module for secondary track finding
  _mFvtxFindHoughTracks_mod = mFvtxFindHoughTracks::GetInstance();

  return 0;
}

//______________________________________________________
int FvtxRecoWithMut::InitRun(PHCompositeNode *top_node)
{

  check_parameters(top_node);

  
  // Instantiate FVTXOO analysis modules
  if (!_mMutKalFitWithSili_mod)
    _mMutKalFitWithSili_mod = new mMutKalFitWithSili();
  if (!_mMutKalFitWithSiliReal_mod)
    _mMutKalFitWithSiliReal_mod = new mMutKalFitWithSiliReal();
  
  return 0;
}

//______________________________________________________
int FvtxRecoWithMut::create_node_tree(PHCompositeNode *top_node)
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
  TFvtxTrkMap* fvtx_trk_map = TMutNode<TFvtxTrkMap>::new_node( _fvtx_node, "TFvtxTrkMap" );
  fvtx_trk_map->make_persistant(dst_node,"TFvtxTrk");

  // Module parameters tables
  mMutKalFitWithSiliPar *mMutKalFitWithSili_par = TMutNode<mMutKalFitWithSiliPar>::new_node( _fvtx_node, "mMutKalFitWithSiliPar" );
//  mMutKalFitWithSili_par->set_verbosity( MUTOO::NONE );

  // Module parameters tables
  mMutKalFitWithSiliRealPar *mMutKalFitWithSiliReal_par =
    TMutNode<mMutKalFitWithSiliRealPar>::new_node( _fvtx_node, "mMutKalFitWithSiliRealPar" );
//  mMutKalFitWithSiliReal_par->set_verbosity( MUTOO::NONE );
  mMutKalFitWithSiliReal_par->set_do_evaluation(_do_mutkalfiteval );
//  mMutKalFitWithSiliReal_par->set_fvtx_mutr_proximity_cut(_fvtx_mutr_proximity_cut);

  if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
  {
    mMutKalFitWithSili_par->print();
  }

  return 0;
}


void
FvtxRecoWithMut::set_fvtx_mutr_proximity_cut(const Double_t& value)
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
FvtxRecoWithMut::set_do_mutkalfiteval(bool a)
{
  // This type of function is obsolete. One can use in future
//  TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),"mMutKalFitWithSiliRealPar")
//      ->set_do_evaluation(true);

  obsolete_warning("set_do_mutkalfiteval","TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),\"mMutKalFitWithSiliRealPar\")->set_do_evaluation()");

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
int FvtxRecoWithMut::check_parameters(PHCompositeNode */*top_node*/)
{
  // one can also reset run specific parameters here

  // Module parameters tables
  mMutKalFitWithSiliRealPar *mMutKalFitWithSiliReal_par =
    TMutNode<mMutKalFitWithSiliRealPar>::find_node( _fvtx_node, "mMutKalFitWithSiliRealPar" );

  if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
  {
      mMutKalFitWithSiliReal_par->print();
  }

  return 0;
}


//______________________________________________________
int FvtxRecoWithMut::process_event(PHCompositeNode *top_node)
{
	
  _timer->restart();
	
  try {
    if ( _track_mod_par->get_allowTwoHitTracks() )
      _mFvtxFindHoughTracks_mod->processTwoHitTracks();

    //_mMutKalFitWithSili_mod->event( top_node );
    _mMutKalFitWithSiliReal_mod->event( top_node );

  } catch (exception& e) {
    FVTXOO::TRACE(e.what());
  }	

  write_maps_if_needed();
  _timer->stop();
  return 0;
}

//______________________________________________________
int FvtxRecoWithMut::End(PHCompositeNode* top_node) 
{
	_timer->print_stat();
  _mMutKalFitWithSiliReal_mod->finish_evaluation();

	cout <<"FvtxRecoWithMut::End - Info - Clean up internal modules ..."<<endl;
  if (_mMutKalFitWithSili_mod)
    {
      delete _mMutKalFitWithSili_mod;
      _mMutKalFitWithSili_mod = NULL;
    }
  if (_mMutKalFitWithSiliReal_mod)
    {
      delete _mMutKalFitWithSiliReal_mod;
      _mMutKalFitWithSiliReal_mod = NULL;
    }
	return 0;
}


void
FvtxRecoWithMut::obsolete_warning(const std::string & function,
    const std::string & suggestion)
{
  std::cout << std::endl;
  std::cout << "FvtxRecoWithMut - WARNING - FvtxRecoWithMut::" << function << " is obsolete."
      << " Please use " << suggestion << " instead" << std::endl;
  std::cout << std::endl;
  sleep(5);
}

