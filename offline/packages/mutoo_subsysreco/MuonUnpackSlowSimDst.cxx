
// $Id: MuonUnpackSlowSimDst.cxx,v 1.8 2011/07/14 22:27:10 pinkenbu Exp $

/*!
  \file MuonUnpackSlowSimDst.cxx	
  \ingroup supermodules
  \brief reads muid/mutr mc hit/track maps from a simulated DST 
  \author Sean Kelly
  \version $Revision: 1.8 $
  \date $Date: 2011/07/14 22:27:10 $
*/

#include "MuonUnpackSlowSimDst.h"
#include <recoConsts.h>

// MUTOO IOC includes
#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<TMCPrimaryMap.h>

// include pisa event header
#include <PISAEventHeader.h>

// MUIOO IOC includes
#include<TMuiMCHitMapO.h>

#include<PHTimer.h>

// ROOT include 
#include <TFile.h>
#include <TTree.h>

TFile* file;

using namespace std;

//______________________________________________________
MuonUnpackSlowSimDst::MuonUnpackSlowSimDst( const char* name ) :
  MuonSubsysReco(name),
  _mutoo_node(0),
  _muioo_node(0),
  _timer( PHTimeServer::get()->insert_new(name) ),
  _outfile("muonslowsimdst2tree.root")
{}

//______________________________________________________
int MuonUnpackSlowSimDst::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT(cout, "MuonUnpackSlowSimDst::InitRun");	
  CreateNodeTree(top_node);
  setup_output();

  MUTOO::PRINT(cout, "**");	
  return 0;
}

//_______________________________________________________________
void MuonUnpackSlowSimDst::setup_output()
{
  file = new TFile(_outfile.c_str(),"RECREATE");

  // mutr pisahit tree
  _mutr_pisahit_tree = new TTree("_mutr_pisahit_tree","_mutr_pisahit_tree");
  
  // leaves
  _mutr_pisahit_tree->Branch("zcoll", &zcoll, "zcoll/F");
  _mutr_pisahit_tree->Branch("b", &b, "b/F");
  _mutr_pisahit_tree->Branch("ncoll", &ncoll, "ncoll/I");
  _mutr_pisahit_tree->Branch("nhit", &nhit, "nhit/I");
  _mutr_pisahit_tree->Branch("arm", &arm, "arm/I");
  _mutr_pisahit_tree->Branch("sta", &station, "sta/I");
  _mutr_pisahit_tree->Branch("gap", &gap, "gap/I");
  _mutr_pisahit_tree->Branch("AptG", AptG, "AptG[nhit]/F");
  _mutr_pisahit_tree->Branch("ApzG", ApzG, "ApzG[nhit]/F");
  _mutr_pisahit_tree->Branch("Ar", Ar, "Ar[nhit]/F");
  _mutr_pisahit_tree->Branch("Az", Az, "Az[nhit]/F");
  _mutr_pisahit_tree->Branch("Aphi", Aphi, "Aphi[nhit]/F");
  _mutr_pisahit_tree->Branch("Arvtx", Arvtx, "Arvtx[nhit]/F");
  _mutr_pisahit_tree->Branch("Aphivtx", Aphivtx, "Aphivtx[nhit]/F");
  _mutr_pisahit_tree->Branch("Aidpart", Aidpart, "Aidpart[nhit]/I");
  _mutr_pisahit_tree->Branch("Agen", Agen, "Agen[nhit]/I");
  _mutr_pisahit_tree->Branch("Apripart", Apridpart, "Apridpart[nhit]/I");
  _mutr_pisahit_tree->Branch("Aprphi", Aprphi, "Aprphi[nhit]/F");
  _mutr_pisahit_tree->Branch("Aprthe", Aprthe, "Aprthe[nhit]/F");
  _mutr_pisahit_tree->Branch("Aprpt", Aprpt, "Aprpt[nhit]/F");
  _mutr_pisahit_tree->Branch("Aprpz", Aprpz, "Aprpz[nhit]/F");

  // primary tree
  _primary_tree = new TTree("_primary_tree","_primary_tree");
  
  // leaves
  _primary_tree->Branch("zcoll", &zcoll, "zcoll/F");
  _primary_tree->Branch("b", &b, "b/F");
  _primary_tree->Branch("ncoll", &ncoll, "ncoll/I");
  _primary_tree->Branch("ntrk", &ntrk, "ntrk/I");
  _primary_tree->Branch("Ppripart", Ppridpart, "Ppridpart[ntrk]/I");
  _primary_tree->Branch("Pprphi", Pprphi, "Pprphi[ntrk]/F");
  _primary_tree->Branch("Pprthe", Pprthe, "Pprthe[ntrk]/F");
  _primary_tree->Branch("Pprpt", Pprpt, "Pprpt[ntrk]/F");
  _primary_tree->Branch("Pprpz", Pprpz, "Pprpz[ntrk]/F");


}

//______________________________________________________
int MuonUnpackSlowSimDst::CreateNodeTree(PHCompositeNode *top_node)
{
  try {
    
    PHNodeIterator nodeItr(top_node);
    
    // Merged node -- MUTOO maps used for reconstruction 
    _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    if(!_mutoo_node){
      _mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(_mutoo_node);
    }
    
    // Merged node -- MUTOO maps used for reconstruction 
    _muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
    if(!_muioo_node){
      _muioo_node = new PHCompositeNode("MUIOO");
      top_node->addNode(_muioo_node);
    }
    
    // Monte-carlo hits and tracks are read from the input SIGNAL node
    TMutNode<TMutMCHitMap>::new_node(_mutoo_node, "TMutMCHitMap");
    TMutNode<TMuiMCHitMapO>::new_node(_muioo_node, "TMuiMCHitMapO");
    TMutNode<TMutMCTrkMap>::new_node(_mutoo_node,"TMutMCTrkMap");
    TMutNode<TMCPrimaryMap>::new_node( _mutoo_node, "TMCPrimaryMap");

  } catch(exception& e) {

    MUTOO::TRACE(e.what());

  }
  return 0;
}


//______________________________________________________
int MuonUnpackSlowSimDst::process_event(PHCompositeNode *top_node)
{

  _timer.get()->restart();

  // load vertex
  load_vertex_if_needed( top_node );
  
  // fill tree for pisa hit
  fill_mutr_pisahit_tree();
  
  // fill tree for primary
  fill_primary_tree();

  // write map anyway
  write_maps_if_needed();

  _timer.get()->stop();
  return 0;
}

//________________________________________________________________________________
void MuonUnpackSlowSimDst::reset_pisahit_tree() 
{
  zcoll	 = 0.;
  b			 = 0.;
  nhit		= 0;

  for(int index = 0; index < MAX_HIT; index++) {
    arm[index] = -1;
    station[index] = -1;
    gap[index] = -1;
    AptG[index] = 0.;
    ApzG[index] = 0.;
    Ar[index] = 0.;
    Az[index] = 0.;
    Aphi[index] = 0;
    Arvtx[index] = 0.;
    Azvtx[index] = 0.;
    Aphivtx[index] = 0.;
    Aidpart[index] = 0;
    Agen[index] = 0;
    Apridpart[index] = 0;
    Aprphi[index] = 0.;
    Aprthe[index] = 0.;
    Aprpt[index] = 0.; 
    Aprpz[index] = 0.;
  }
}

//_________________________________________________________________________________
void MuonUnpackSlowSimDst::fill_mutr_pisahit_tree()
{
  // reset the tree value fisrt.
  //
  reset_pisahit_tree();
  // get pisa event header for vertex and collision info 
  //
  PISAEventHeader *pisaEventHeader = PISAEventHeader::GetEventHeader();
  if(!pisaEventHeader) return;

  zcoll = pisaEventHeader->GetZvertex();
  b		 = pisaEventHeader->GetImpactParameter();
  ncoll = pisaEventHeader->GetBinaryCollisions();

  // get MCHit map
  //
  TMutMCHitMap* mc_hit_map = TMutNode<TMutMCHitMap>::find_node(_mutoo_node,"TMutMCHitMap");
  TMutMCHitMap::const_iterator mchit_iter = mc_hit_map->range();
  // set nhit to the number of MCHits
  //
  nhit = mchit_iter.count();
  // make an index for arries branch
  //
  int index = 0;
  // loop through all the MCHits 
  //
  while(TMutMCHitMap::const_pointer mchit_ptr = mchit_iter.next()) {
    // locators of the MCHit
    //
    arm[index] = mchit_ptr->get()->get_arm();
    station[index] = mchit_ptr->get()->get_station();
    gap[index] = mchit_ptr->get()->get_gap();
    // momentum of the associated track and the position
    //
    AptG[index] = sqrt(mchit_ptr->get()->get_px()*mchit_ptr->get()->get_px() + mchit_ptr->get()->get_py()*mchit_ptr->get()->get_py());
    ApzG[index] = mchit_ptr->get()->get_pz();
    Ar[index] = sqrt(mchit_ptr->get()->get_x()*mchit_ptr->get()->get_x() + mchit_ptr->get()->get_y()*mchit_ptr->get()->get_y());
    Az[index] = mchit_ptr->get()->get_z();
    Aphi[index] = atan2(mchit_ptr->get()->get_y(), mchit_ptr->get()->get_x());
    // associated track info. Should be only one
    //
    TMutMCTrkMap::const_key_iterator mctrk_iter = mchit_ptr->get()->get_associated<TMutMCTrk>();
    while(TMutMCTrkMap::const_pointer mctrk_ptr = mctrk_iter.next()) {
      Arvtx[index] = sqrt(mctrk_ptr->get()->get_x_orig()*mctrk_ptr->get()->get_x_orig()+
          mctrk_ptr->get()->get_y_orig()*mctrk_ptr->get()->get_y_orig());
      Azvtx[index] = mctrk_ptr->get()->get_z_orig();
      Aphivtx[index] = atan2(mctrk_ptr->get()->get_y_orig(),mctrk_ptr->get()->get_x_orig());	 
      Aidpart[index] = mctrk_ptr->get()->get_pid();
      Agen[index] = (mctrk_ptr->get()->get_parent_track_id() < 0) ? 0 : 1;
    }
    // associated primary track info., Should be only one again.
    //
    TMCPrimaryMap::const_key_iterator pri_iter = mchit_ptr->get()->get_associated<TMCPrimary>();
    while(TMCPrimaryMap::const_pointer pri_ptr = pri_iter.next()) {		
      Apridpart[index] = pri_ptr->get()->get_pid();
      Aprphi[index] = atan2(pri_ptr->get()->get_py_orig(),pri_ptr->get()->get_px_orig());
      Aprthe[index] = acos(pri_ptr->get()->get_pz_orig()/pri_ptr->get()->get_ptot_orig());
      Aprpt[index] = sqrt(pri_ptr->get()->get_px_orig()*pri_ptr->get()->get_px_orig()+
            pri_ptr->get()->get_py_orig()*pri_ptr->get()->get_py_orig());
      Aprpz[index] = pri_ptr->get()->get_pz_orig();
    }
    
    // increase index
    index++;
  }
  
  // at least entry of the arries is filled, we fill the tree
  if(index > 0) _mutr_pisahit_tree->Fill();
}

//________________________________________________________________________________
void MuonUnpackSlowSimDst::reset_primary_tree() 
{
  ntrk = 0;
  for(int index = 0; index < MAX_TRACK; index++) {
    Ppridpart[index] = 0;
    Pprphi[index] = 0;	 
    Pprthe[index] = 0.;
    Pprpt[index] = 0.;
    Pprpz[index] = 0.;

  }
}

//_________________________________________________________________________________
void MuonUnpackSlowSimDst::fill_primary_tree()
{
  // get TMCPrimary map
  //
  TMCPrimaryMap* pri_map = TMutNode<TMCPrimaryMap>::find_node(_mutoo_node,"TMCPrimaryMap");
  TMCPrimaryMap::const_iterator pri_iter = pri_map->range();
  // set ntrk to the number of primary tracks
  //
  ntrk = pri_iter.count();
  // make an index for arries branch
  //
  int index = 0;
  // loop through all the MCHits 
  //
  while(TMCPrimaryMap::const_pointer pri_ptr = pri_iter.next()) {
    // set value to the branchs
    //
    Ppridpart[index] = pri_ptr->get()->get_pid();
    Pprphi[index] = atan2(pri_ptr->get()->get_py_orig(),pri_ptr->get()->get_px_orig());
    Pprthe[index] = acos(pri_ptr->get()->get_pz_orig()/pri_ptr->get()->get_ptot_orig());
    Pprpt[index] = sqrt(pri_ptr->get()->get_px_orig()*pri_ptr->get()->get_px_orig()+
          pri_ptr->get()->get_py_orig()*pri_ptr->get()->get_py_orig());
    Pprpz[index] = pri_ptr->get()->get_pz_orig();
    // increase index
    //
    index++;
  }
  // at least entry of the arries is filled, we fill the tree
  //
  if(index > 0) _primary_tree->Fill();
}

//______________________________________________________
int MuonUnpackSlowSimDst::End(PHCompositeNode* top_node) 
{
  
  // print this module timer statistics
//   _timer.get()->print_stat();

  std::cout << " Write out the output trees." << std::endl;
  file->Write();
  file->Close();
  
  return 0;
}

