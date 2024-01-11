// $Id: MuiooReco.cxx,v 1.47 2013/02/26 17:24:00 silvermy Exp $

/*!
  \file	MuiooReco.cxx
  \ingroup supermodules 
  \brief muioo muid reconstruction event loop, starting for TMuiHitMapO
  \author Sean Kelly
  \version $Revision: 1.47 $
  \date	$Date: 2013/02/26 17:24:00 $
*/

#include "recoConsts.h"

#include <TMuiClusterMapO.h>
#include <TMui1DRoadMapO.h>
#include <TMuiRoadMapO.h>
#include <TMuiPseudoBLTO.h>
#include <TMuiPseudoLL1Map.h>

#include <mMuiFastRoadFinder.h>
#include <mMuiClusterFinderPar.h>
#include <mMuiRoadFinder1Par.h>
#include <mMuiFastRoadFinderPar.h>
#include <mMuiFindRoadPar.h>

#include "Event.h"
#include "RawDataCheck.h"
#include "getClass.h"
#include "Fun4AllReturnCodes.h"

#include "MuiooReco.h"

using namespace std;

//________________________________________________________________
MuiooReco::MuiooReco() :
  MuonSubsysReco( "MUIOORECO" ),
  _flags( ROADORIG | LL1_EMULATOR_ENABLED ),
  _timer(PHTimeServer::get()->insert_new("MUIOORECO") ),
  _max_occupancy_per_arm(300), // same default as in mMuiRoadFinder1Par.h
  _asymm_cut_par( 100.0 ) // value > 80 => nothing will be cut; 12 was prev. default cut value
{
  _verbosity = MUTOO::SOME;
}

//________________________________________________________________
MuiooReco::~MuiooReco()
{}

//________________________________________________________________
int MuiooReco::Init(PHCompositeNode *top_node)
{ 

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );
  
  if(_verbosity>=MUTOO::ALOT) {
    MUTOO::PRINT( cout, "MuiooReco::Init" );
    cout << "flags: " << endl;
    cout << "ROADORIG             : " << (get_flag( ROADORIG ) ? "true":"false" ) << endl;
    cout << "ROADNEW              : " << (get_flag( ROADNEW ) ? "true":"false" ) << endl;  
    cout << "BLT_EMULATOR_ENABLED : " << (get_flag( BLT_EMULATOR_ENABLED ) ? "true":"false" ) << endl;  
    cout << "LL1_EMULATOR_ENABLED : " << (get_flag( LL1_EMULATOR_ENABLED ) ? "true":"false" ) << endl;  
    MUTOO::PRINT( cout, "**" ); }

  return 0; 
}

//________________________________________________________________
int MuiooReco::InitRun(PHCompositeNode *top_node)
{
  if(_verbosity>=MUTOO::ALOT) { MUTOO::PRINT( cout, "MuiooReco::InitRun" ); }
  CreateNodeTree(top_node);

  //We need to get timestamp from some PHENIX GLOBAL
  recoConsts* rc = recoConsts::instance();
  if( get_flag( BLT_EMULATOR_ENABLED ) ) 
  {
    int runNumber =	rc->get_IntFlag("RUNNUMBER");
    mMuiBLTEmulator_mod.initialize(runNumber);
  }
  
  if(_verbosity>=MUTOO::ALOT) {  MUTOO::PRINT(std::cout,"**"); }

  return 0;
}

//________________________________________________________________
int MuiooReco::CreateNodeTree(PHCompositeNode *top_node)
{

  // Instantiate nodes for muioo containers
  {
    PHNodeIterator nodeItr(top_node);
    muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
    if(!muioo_node){
      muioo_node = new PHCompositeNode("MUIOO");
      top_node->addNode(muioo_node);
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
  
  // Interface Object Containers (IOCS)
  // muid roads
  try{
    
    TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
  
  } catch(std::exception& e) {

    TMutNode<TMuiRoadMapO>::new_node(muioo_node,"TMuiRoadMapO")->make_persistant(dst_node,"TMuiRoadO");
    
  }

  // muid 1D roads
  try{
  
     TMutNode<TMui1DRoadMapO>::find_node(top_node,"TMui1DRoadMapO");
  
  } catch(std::exception& e){

    TMutNode<TMui1DRoadMapO>::new_node(muioo_node,"TMui1DRoadMapO")->make_persistant(dst_node,"TMui1DRoadO");
  
  }

  try{
  
    TMutNode<TMuiClusterMapO>::find_node(top_node,"TMuiClusterMapO");
  
  } catch(std::exception& e){

    TMutNode<TMuiClusterMapO>::new_node(muioo_node,"TMuiClusterMapO")->make_persistant(dst_node,"TMuiClusterO");
  
  }

  // blue logic trigger emulator
  if( get_flag( BLT_EMULATOR_ENABLED ) )
  {
    try{
      
      TMutNode<TMuiPseudoBLTMapO>::find_node(top_node,"TMuiPseudoBLTMapO");
      
    } catch(std::exception& e){

      TMutNode<TMuiPseudoBLTMapO>::new_node(muioo_node,"TMuiPseudoBLTMapO")->make_persistant(dst_node,"TMuiPseudoBLTO");
      
    }
  }

  // local level1 trigger emulator map
  if( get_flag( LL1_EMULATOR_ENABLED ) )
  {
    try{
      
      TMutNode<TMuiPseudoLL1Map>::find_node(top_node,"TMuiPseudoLL1Map");
      
    } catch(std::exception& e){
      
      TMutNode<TMuiPseudoLL1Map>::new_node(muioo_node,"TMuiPseudoLL1Map")->make_persistant(dst_node,"TMuiPseudoLL1");
      
    }
  }
  
  // Module parameter tables
  mMuiClusterFinderPar* mMuiClusterFinder_par = TMutNode<mMuiClusterFinderPar>::new_node(muioo_node,"mMuiClusterFinderPar");
  mMuiRoadFinder1Par* mMuiRoadFinder1_par = TMutNode<mMuiRoadFinder1Par>::new_node(muioo_node,"mMuiRoadFinder1Par");
  mMuiFastRoadFinderPar* mMuiFastRoadFinder_par = TMutNode<mMuiFastRoadFinderPar>::new_node(muioo_node,"mMuiFastRoadFinderPar");
  mMuiFindRoadPar* mMuiFindRoad_par = TMutNode<mMuiFindRoadPar>::new_node(muioo_node,"mMuiFindRoadPar");	
  mMuiBLTEmulatorPar* mMuiBLTEmulator_par = TMutNode<mMuiBLTEmulatorPar>::new_node(muioo_node,"mMuiBLTEmulatorPar");
  
  // MuidLL1 setting
  // needed only if muid LL1 emulator is to be run
  if( get_flag( LL1_EMULATOR_ENABLED ) )
  {

    mMuiFastRoadFinder_par->set_verbosity(MUIOO::NONE); 
    
  } else { cout << "MuiooReco::CreateNodeTree - ll1 emulator disabled." << endl; }
  
  // cluster finder
  mMuiClusterFinder_par->set_verbosity(MUIOO::NONE); 
  
  // road finder (ROADNEW)
  mMuiFindRoad_par->set_verbosity(MUIOO::NONE); 
  
  // road finder (ROADORIG)
  // JN & SK modifications to 1d Road Finder to accomodate low efficiency 1D1S trigger
  mMuiRoadFinder1_par->set_verbosity(MUIOO::NONE); 
  mMuiRoadFinder1_par->set_min_fired_gaps(1);
  mMuiRoadFinder1_par->set_min_last_gap_1d(1);
  mMuiRoadFinder1_par->set_max_del_last_gap(2);
  mMuiRoadFinder1_par->set_max_del_total_hits(2);
  mMuiRoadFinder1_par->set_max_del_total_hits(2);
  // also allow to change the maximum occupancy cut
  mMuiRoadFinder1_par->set_max_occupancy_per_arm(_max_occupancy_per_arm); 

  // BLT emulator
  if( get_flag( BLT_EMULATOR_ENABLED ) )
  {
    
    // debug flag inside BLT emulator.
    mMuiBLTEmulator_par->set_debug_flag(0);
    mMuiBLTEmulator_par->print();
    
  } else { cout << "MuiooReco::CreateNodeTree - blt emulator disabled." << endl; }

  if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) 
  {
    if( get_flag( BLT_EMULATOR_ENABLED ) ) mMuiBLTEmulator_par->print();
    if( get_flag( LL1_EMULATOR_ENABLED ) ) mMuiFastRoadFinder_par->print();
    mMuiClusterFinder_par->print();
    mMuiRoadFinder1_par->print();
  }
  return 0;
}

//________________________________________________________________
int MuiooReco::process_event(PHCompositeNode *top_node)
{

  _timer.get()->restart();
  
  try {
    
    // vertex loading
    MuonSubsysReco::load_vertex_if_needed( top_node );
    
    // cluster finder
    mMuiClusterFinder_mod.event(muioo_node);				

    // abort events with sparks
    if (check_for_sparks( top_node )) return ABORTEVENT;

    // trigger emulators 
    /*
      by default, BLT emulator is not run 
      and LL1 emulator does
    */
    if( get_flag( BLT_EMULATOR_ENABLED ) ) 
    { mMuiBLTEmulator_mod.event(muioo_node);}
    
    if( get_flag( LL1_EMULATOR_ENABLED ) ) 
    { mMuiFastRoadFinder_mod.event(top_node); }
    
    // pattern recognition
    if( get_flag( ROADORIG ) ) 
    {

      mMuiRoadFinder1_mod.event(top_node);
    
    } else if ( get_flag( ROADNEW ) ) {
    
      mMuiFindRoad_mod.event(top_node);
    
    } else MUIOO::TRACE("Unknown road-finding algorithm selected - do nothing..");
    
  } catch (std::exception& e) { MUIOO::TRACE(e.what()); }	

  /* 
  implemented in base class
  the maps are actually cleared only if 
  this module is the first registered module.
  */
  write_maps_if_needed();
  _timer.get()->stop();		
  
  return 0;
}

//________________________________________________________________
int MuiooReco::End(PHCompositeNode* top_node) 
{
  if( get_flag( BLT_EMULATOR_ENABLED ) ) mMuiBLTEmulator_mod.print_summary();

  if( get_flag( LL1_EMULATOR_ENABLED ) ) mMuiFastRoadFinder_mod.print();

//   _timer.get()->print_stat();

  return 0;
}

int MuiooReco::check_for_sparks(PHCompositeNode* top_node)
{
  // check the muid arms for too many hits in one vs the other of the two arms
  // (asymmetry between the number of hits in South vs North)
  TMuiHitMapO* mui_hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");

  int nhits[TMuiChannelId::kArmsTotal] = {0};
  for (short arm=0; arm<TMuiChannelId::kArmsTotal; arm++) 
    {
      TMuiHitMapO::iterator hit_iter = mui_hit_map->get(arm);
      nhits[arm] = hit_iter.count();
    }

  int nhitsTot = nhits[0] + nhits[1];

  int nhitsMin = (int)(_asymm_cut_par * _asymm_cut_par); // lower mult. would not be removed by cut

  if (nhitsTot>nhitsMin)
    { 
      double asymmCut = _asymm_cut_par/sqrt(nhitsTot); 

      double asymmRatio = (nhits[0] - nhits[1]) / (1.0*nhitsTot); // asymm-ratio = diff / sum
      if (asymmRatio<0) { asymmRatio = -asymmRatio; } // poor man's fabs..

      if (asymmRatio>asymmCut) 
	{
	  Event *evt = findNode::getClass<Event>(top_node, "PRDF");
	  RawDataCheck *raw = RawDataCheck::instance();
	  if (evt) {
	    raw->AddToList(evt, "MUIDTOOMANYCLUSTERS");
	  }
	  cout << PHWHERE << " nhitsSouth " << nhits[0]
	       << " nhitsNorth " << nhits[1]
	       << " asymmRatio " << asymmRatio
	       << ", above the threshold of " << asymmCut
	       << ", aborting the entire event." << endl;
	  return ABORTEVENT; 	       
	}
    }

  return 0;
}

