// $Id: MuonRotateGapCoord.cxx,v 1.5 2009/05/19 07:07:03 hpereira Exp $

/*!
  \file    MuonRotateGapCoord.cxx
  \ingroup supermodules 
  \brief   calculate reaction plane angle using Muon arm run4 AuAu DST  
  \author  Sean Kelly
  \version $Revision: 1.5 $
  \date    $Date: 2009/05/19 07:07:03 $
*/

#include <PHCompositeNode.h>
#include <recoConsts.h>

// Include root
#include <TTree.h>

// Include math
#include <math.h>

// MUTOO IOC includes
#include <TMutGapCoordMap.h>
#include <PHTFileServer.h>

// Include online runtime parameter.
#include <mMutFindClusPar.h>
#include <mMutFitClusPar.h>
#include <mMutMatchCoordPar.h>
#include <TMutErrorStats.h>

// pure static classes
#include <TMutClusterFitEval.h>
#include <TMutMathiesonPar.h>

// Event display includes
#include "MuonRotateGapCoord.h"

using namespace std;

//______________________________________________________
MuonRotateGapCoord::MuonRotateGapCoord( const char* name ) : 
  MuonSubsysReco( name ),
  _timer( PHTimeServer::get()->insert_new( name ) )
{ _out_file="muonrotategapcoord.root"; init_tree();}

//______________________________________________________
MuonRotateGapCoord::~MuonRotateGapCoord( void )
{}

//______________________________________________________
int MuonRotateGapCoord::Init(PHCompositeNode *top_node)
{
  
  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );  
  
  MUTOO::PRINT( cout, "MuonRotateGapCoord::Init" );
  setup_output();
  MUTOO::PRINT( cout, "**" );
  return 0;
  
}

//______________________________________________________
int MuonRotateGapCoord::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "MuonRotateGapCoord::InitRun" );
  CreateNodeTree(top_node);
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//______________________________________________________
int MuonRotateGapCoord::CreateNodeTree(PHCompositeNode *top_node)
{
  
  // Instantiate nodes for mutoo containers
  {
    PHNodeIterator nodeItr(top_node);
    mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    if(!mutoo_node){
      mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(mutoo_node);
    }
  }
  
  {
    PHNodeIterator nodeItr(top_node);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!dst_node) {
      dst_node = new PHCompositeNode("DST");
      top_node->addNode(dst_node);
    }
  }
  
  // Interface Object Containers (IOCS)
  recoConsts *rc = recoConsts::instance();
     
  TMutClusMap* clus_map = TMutNode<TMutClusMap>::new_node(mutoo_node, "TMutClusMap");    
  TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::new_node(mutoo_node, "TMutCoordMap");
  TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::new_node(mutoo_node, "TMutGapCoordMap");
  
  // Make IOCs persistent here
  clus_map->make_persistant(dst_node,"TMutClus");
  coord_map->make_persistant(dst_node,"TMutCoord");
  gap_coord_map->make_persistant(dst_node,"TMutGapCoord");
  
  // Module parameter tables
  mMutFindClusPar*    mMutFindClus_par = TMutNode<mMutFindClusPar>::new_node(mutoo_node,"mMutFindClusPar");
  mMutFitClusPar*     mMutFitClus_par = TMutNode<mMutFitClusPar>::new_node(mutoo_node,"mMutFitClusPar");
  mMutMatchCoordPar*  mMutMatchCoord_par = TMutNode<mMutMatchCoordPar>::new_node(mutoo_node,"mMutMatchCoordPar");
  
  // cluster finding
  mMutFindClus_par->set_verbosity( MUTOO::NONE );  
  mMutFindClus_par->set_max_cluster_width(25);
  
  // cluster fit
  mMutFitClus_par->set_verbosity( MUTOO::NONE );
  mMutFitClus_par->set_multi_track_fit(true);

  /* 
     coordinate matching 
     current set of cuts makes it work like the old
     mMutFindGapCoord module
  */
  mMutMatchCoord_par->set_verbosity( MUTOO::NONE );
  mMutMatchCoord_par->set_do_evaluation( false );
  mMutMatchCoord_par->set_max_combinations( 0 );
  mMutMatchCoord_par->set_do_refit( false );
  
  // dump parameters if needed.
  if( rc->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) {
    mMutFitClus_par->print();
    mMutMatchCoord_par->print();
  }
  
  return 0;
}

//______________________________________________________
int MuonRotateGapCoord::process_event(PHCompositeNode *top_node)
{
  
  _timer.get()->restart();

  recoConsts *rc = recoConsts::instance();

  // Call MUTOO modules for track momentum reconstruction and vertex finding
  try {
    
    rc->set_IntFlag("DATABASE", 1);

    load_vertex_if_needed( top_node );
    
    // cluster/gap coord finding/fitting
    _mMutFindClus_mod.event(mutoo_node);
    _mMutFitClus_mod.event(mutoo_node);
    // rotate south arm station 2.
    rotation_coords_by_half_octant(mutoo_node, 0, 1);
    _mMutMatchCoord_mod.event(mutoo_node);
    
  } catch (exception& e) {
    MUTOO::TRACE(e.what());
  }  

  //do_calculation(mutoo_node);

  write_maps_if_needed();
  _timer.get()->stop();

  return 0;
}

//______________________________________________________
int MuonRotateGapCoord::End(PHCompositeNode* top_node) 
{
  _mMutMatchCoord_mod.print_summary();
  PHTFileServer::get().write( _out_file );

  return 0;
}

//_______________________________________________________________
bool MuonRotateGapCoord::init_tree() {

  Tevt  = 0;
  Trun  = 0;
  Tcent = 0;
  Tzvtx = 0;
  Tarm  = 0;
  Tsta  = 0;
  Tgap  = 0;
  Tnhits= 0;
  Tphi1 = 0;
  Tphi2 = 0;

  return true;
}

//_______________________________________________________________
bool MuonRotateGapCoord::setup_output() {
  
  // TFile
  PHTFileServer::get().open( _out_file );

  // TTree
  _anaTree = new TTree("anaTree","anaTree");
  // leaves
  _anaTree->Branch("evt",   &Tevt,   "evt/i");
  _anaTree->Branch("run",   &Trun,   "run/i");
  _anaTree->Branch("cent",  &Tcent,  "cent/F");
  _anaTree->Branch("zvtx",  &Tzvtx,  "zvtx/F");
  _anaTree->Branch("arm",   &Tarm,   "arm/s");
  _anaTree->Branch("sta",   &Tsta,   "sta/s");
  _anaTree->Branch("gap",   &Tgap,   "gap/s");
  _anaTree->Branch("nhits", &Tnhits, "nhits/i");
  _anaTree->Branch("phi1",  &Tphi1,  "phi1/F");
  _anaTree->Branch("phi2",  &Tphi2,  "phi2/F");

  return true;
}
  
//_______________________________________________________________
int  MuonRotateGapCoord::rotation_coords_by_half_octant(PHCompositeNode* top_node,unsigned short arm, unsigned station)
{
  
  // Rotation is done by swapping half octant as following, 
  // for n = 1, 15
  // 0<->n, 
  // The coords in half_octant (0,0) will always store the coords of nth half octand after the swap.
  // Thus, it is equavolent to 0->1->2->3..->15.
  //


  // Put every thing in the try catch block 
  try {
    
    // get coord map.
    //
    TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");
    std::cout << " Before rotation." <<std::endl;
    coord_map->print();
    // Loop over all the half octant
    for(unsigned short igap = 0; igap < MUTOO::MAX_GAP; igap++) {
      for(unsigned short icat = 0; icat < MUTOO::MAX_CATHODE; icat++) {
	for(unsigned short ioct = 0; ioct < MUTOO::MAX_OCTANT; ioct++) {
	  for(unsigned short ihoc = 0; ihoc < MUTOO::MAX_HALF_OCTANT; ihoc++){
	    // do not swap with itself.
	    //
	    if(ioct+ihoc==0) continue;
	    // swap source and target.
	    //
	    swap_coord_in_half_octant(coord_map,
				      arm,
				      station,
				      igap,
				      icat,
				      0,
				      0,
				      ioct,
				      ihoc); 
	  }
	} 
      }
    }
    std::cout<<"After rotation." << std::endl;
    coord_map->print();
    
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }
  return 0;
}

// transfer coords to a different octant and half_octant
//
void transfer_coords_to_new_half_ocatant(TMutCoordMap* coord_map,
					 TMutCoordMap::iterator coord_iter,
					 unsigned short new_octant,
					 unsigned short new_half_octant) 
{

  // Loop through coords
  //
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()) {

    // get the rotation angle
    //
    float rotation_angle = M_PI/16.0*(new_octant*2
				      +new_half_octant
				      -coord_ptr->get()->get_octant()*2
				      -coord_ptr->get()->get_half_octant());

    // coord is a 2d line on the cathod plane, we need the begin point and end point of the line.
    PHPoint begin = coord_ptr->get()->get_coord_begin();
    PHPoint end = coord_ptr->get()->get_coord_end();
    
    // make two new PHPoint corresponding to the new coord after the rotation.
    //
    PHPoint begin_new(begin.getX()*cos(rotation_angle) - begin.getY()*sin(rotation_angle),
		      begin.getX()*sin(rotation_angle) + begin.getY()*cos(rotation_angle),
		      begin.getZ());
    
    PHPoint end_new(end.getX()*cos(rotation_angle) - end.getY()*sin(rotation_angle),
		    end.getX()*sin(rotation_angle) + end.getY()*cos(rotation_angle),
		    end.getZ());
    
    // make the new coord.
    PHLine new_coord(begin_new, end_new);
    // insert new coord
    TMutCoordMap::pointer new_ptr = (coord_map->insert_new(coord_ptr->get()->get_arm(),
							   coord_ptr->get()->get_station(),
							   new_octant,
							   new_half_octant,
							   coord_ptr->get()->get_gap(),
							   coord_ptr->get()->get_cathode())).current();
    // fill info for the new coord
    //
    new_ptr->get()->set_coord(new_coord);
    new_ptr->get()->set_q_peak(coord_ptr->get()->get_q_peak());
    new_ptr->get()->set_q_tot(coord_ptr->get()->get_q_tot());
    new_ptr->get()->set_peak_strip(coord_ptr->get()->get_peak_strip());
    new_ptr->get()->set_cos_theta_wire(coord_ptr->get()->get_cos_theta_wire());
    new_ptr->get()->set_error(coord_ptr->get()->get_error());
    new_ptr->get()->set_w(coord_ptr->get()->get_w());
    new_ptr->get()->set_status(coord_ptr->get()->get_status());
    new_ptr->get()->set_q_error(coord_ptr->get()->get_q_error());
    // redo the association with TMutClus,
    // 
    TMutClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
    
    while(TMutClusMap::pointer clus_ptr = clus_iter.next()) {
      // remove the association with the old coord
      // 
      PHKey::disassociate(clus_ptr,coord_ptr);
      // associate with the new coord
      //
      PHKey::associate(new_ptr,clus_ptr);
    }
    
    // erase the old coord from map.
    //
    coord_map->erase(coord_ptr->get()->get_key());
  }
}

// swap the coords between two half octant on a given cathod plane.
//
void MuonRotateGapCoord::swap_coord_in_half_octant(TMutCoordMap* coord_map,
						   unsigned short arm,
						   unsigned short station,
						   unsigned short gap,
						   unsigned short cathod,
						   unsigned short octant1,
						   unsigned short half_octant1,
						   unsigned short octant2,
						   unsigned short half_octant2) 
{
  // get the iterator first.
  //
  TMutCoordMap::iterator coord_iter1 = coord_map->get(arm,       
						      station,   
						      octant1,    
						      half_octant1,
						      gap,       
						      cathod);
  
  // get the iterator first.
  //
  TMutCoordMap::iterator coord_iter2 = coord_map->get(arm,       
						      station,   
						      octant2,    
						      half_octant2,
						      gap,       
						      cathod);

  // transfer all coords in a half octant to a new half octant.
  //
  transfer_coords_to_new_half_ocatant(coord_map, coord_iter1, octant2, half_octant2);
  transfer_coords_to_new_half_ocatant(coord_map, coord_iter2, octant1,half_octant1);

}


