// $Id: MWGReco.C,v 1.1 2009/07/04 18:33:51 hpereira Exp $

/*!
  \file    MWGReco.C
  \brief   muon nanoDST creation, using old framework input nodes
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2009/07/04 18:33:51 $
*/

#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include <PHGeometry.h>
#include <TMutNode.h>

// MUT
#include <dMuoTracksOut.h>
#include <PHTable.hh>
#include <dMuiRoadRawRelWrapper.h>
#include <dMuiRoadsWrapper.h>

#include <dMuiRawWrapper.h>
#include <dMuiRoadsWrapper.h>
#include <stdexcept>
#include <iostream>

#include "MWG.h"
#include "MWGInclusiveNanoCutsv2.h"
#include "MWGReco.h"
#include "rcp.h"

using namespace std;

//_______________________________________________________________
MWGReco::MWGReco(PHInclusiveNanoCuts *aCutter):
  _cutter( aCutter )
{
  ThisName = "MWG";
  MWG::PRINT( cout, "MWGReco::MWGReco" );
	cout << "Important Note: " << endl;
	cout << "MWGReco now only create nanoDST from old framework" << endl;
	cout << "(mut) nodes. To create nanoDST from new framework (mutoo)" << endl;
	cout << "nodes, use MWGOOReco." << endl;
	MWG::PRINT( cout, "**" );
	
	return;
}

//_______________________________________________________________
MWGReco::~MWGReco()
{ return; }

//_______________________________________________________________
int MWGReco::Init(PHCompositeNode *top_node)
{

  MWG::PRINT( cout, "MWGReco::Init" );
  PHNodeIterator iter(top_node);
  PHCompositeNode *dstNode =  static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  if( !dstNode ) {
    cout << "MWGReco::Init - cannot access DST node" << endl;
    return 1;
  }

  // single muon and dimuon branches selection
  PHMuoTracksOut *particle=0;
  bool dodimu( true );

  if (RCP::file_ok("MWG.rcp")) RCP::get<bool>("MWG.rcp","dodimu", dodimu);
  if (dodimu) particle = new PHdiMuoTracksv8();
  else particle = new PHMuoTracksv8();

	// add node to top node
	dstNode->addNode( new PHIODataNode<PHObject>(particle,"PHMuoTracks","PHObject") );
	
  MWG::PRINT( cout, "**" );
  return 0;
	
}

//_______________________________________________________________
int MWGReco::InitRun(PHCompositeNode *top_node)
{
  _cutter->Initialize(top_node);
  PHNodeIterator iter(top_node);
  PHCompositeNode *runNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","RUN"));
  if (!runNode) {
    cout << PHWHERE << "MWGReco:RUN Node missing doing nothing" << endl;
    return -1;
  } else {
		// create cut node
    PHIODataNode<PHObject>* CutsNode = new PHIODataNode<PHObject>(_cutter,"MWGCuts","PHObject");
    runNode->addNode(CutsNode);
  }
  return 0;
}

//_______________________________________________________________
int MWGReco::process_event(PHCompositeNode *top_node)
{
  
	//--------------------------------------------------------
	// check Global
	bool accepted = _cutter->GlobalOK(top_node);

  try {
 
    // grab pointer to ndstNode to get output particle objects 
    PHNodeIterator iter(top_node);
    PHCompositeNode *ndstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "NDST"));
    if(!ndstNode)  ndstNode = top_node;
  
    // grab pointers to the output particle...
    PHTypedNodeIterator<PHMuoTracksOut> particlenanoiter(ndstNode);
    
    // for old muon framework
    PHMuoTracksOut *particle=0;
    PHIODataNode<PHMuoTracksOut> *PHParticleNanoNode = particlenanoiter.find("PHMuoTracks");
    if (PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if (!particle) throw std::runtime_error( "MWGReco::process_event - PHMuoTracks Node not found" );
    
    // muon track loop
    do_muons( top_node, particle );
    
    // dimuon track loop
    if (_cutter->get_dodimu()) do_dimuons( particle );
    
    // check if filtering is required 
    if( _cutter->get_dofilter() ) {
      
      cout << "MWGReco::process_event - no filtering implemented" << endl;
  
//       // check filter cuts on old framework
//       accepted &= _cutter->diMuonFilterOK( particle );    
//       // dump accepted events
//       static int event = 0;
//       if( accepted )
//       cout << "MWGReco::process_event - Event " << event << " passed filter.\n";
//       event ++;
      
    }
  } catch( exception &e ) { cout << e.what() << endl; }
  
  return (accepted) ? EVENT_OK:DISCARDEVENT;
}

//_______________________________________________________________
int MWGReco::ResetEvent(PHCompositeNode *top_node)
{
  // We are resetting just prior to refilling...
  return 0;
}


//______________________________________________________________________________________
void MWGReco::do_muons( PHCompositeNode *top_node, PHMuoTracksOut *particle )
{

  // retrieve dst node
  PHNodeIterator iter(top_node);
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));    
  if(!dstNode)  throw runtime_error( DESCRIPTION( "dst node not found" ) );

  // retrieve dMuoTracksOut
  PHTypedNodeIterator<dMuoTracksOut> iMUO(dstNode);
  PHIODataNode<dMuoTracksOut> *MUO = iMUO.find("dMuoTracksOut");
  if(!MUO) throw runtime_error( DESCRIPTION( "dMuoTracksOut node not found" ) );
  dMuoTracksOut *d_muo  = MUO->getData();
  
  // retrueve road information
  PHIODataNode<PHTable> *Road = static_cast<PHIODataNode<PHTable>*>(iter.findFirst("PHIODataNode", "dMuiRoads"));
  
  dMuiRoadsWrapper *RoadWrapper( 0 );
  DMUIROADS_ST *dMuiRoad;
  if (Road){
    RoadWrapper = static_cast<dMuiRoadsWrapper*>(Road->getData());
    dMuiRoad    = RoadWrapper->TableData();
  }
  
  PHIODataNode<PHTable> *Raw = static_cast<PHIODataNode<PHTable>*>(iter.findFirst("PHIODataNode", "dMuiRaw"));
  PHIODataNode<PHTable> *RoadRaw = static_cast<PHIODataNode<PHTable>*>(iter.findFirst("PHIODataNode", "dMuiRoadRawRel"));

  dMuiRawWrapper        *RawWrapper( 0 ); 
  DMUIRAW_ST            *dMuiRaw( 0 );
  dMuiRoadRawRelWrapper *RoadRawRelWrapper( 0 );
  DMUIROADRAWREL_ST     *dMuiRoadRawRel( 0 );
  if (Raw && RoadRaw){
    RawWrapper        = static_cast<dMuiRawWrapper*>(Raw->getData());
    dMuiRaw           = RawWrapper->TableData();
    RoadRawRelWrapper = static_cast<dMuiRoadRawRelWrapper*>(RoadRaw->getData());
    dMuiRoadRawRel    = RoadRawRelWrapper->TableData();
  }
  
  //! check we have tracks
  if( !d_muo && particle ) return;

  int lastGood = 0;
  particle->Reset();
  particle->set_npart(d_muo->get_NumTrack()) ;
  particle->set_TClonesArraySize(d_muo->get_NumTrack());
  int itrk=0;
  
  // Loop over particles
  //====================
  for (int imuo=0; imuo<d_muo->get_NumTrack(); imuo++){ // loop over particles
	  particle->AddPHParticle(itrk);
	  lastGood = 0;
	
	  //---> store road hit information
	  int IDhits = 0;
	  if (Raw && RoadRaw){
	    for (unsigned int road = 0; road<RoadRawRelWrapper->RowCount(); road++)
	    if(dMuiRoadRawRel[road].dMuiRoadIndex == d_muo->get_muidtrack(imuo)){
	      
        for(unsigned int raw = 0; raw < RawWrapper->RowCount(); raw++)
	  	  if(dMuiRoadRawRel[road].dMuiRawIndex == RawWrapper->get_dMuiRawIndex(raw)){
	  	    // orientation = 0(hor), 1(ver); plane = 0,..,4
	  	    IDhits |= (1<<(RawWrapper->get_plane(raw)*2 
	  	  			     + RawWrapper->get_orientation(raw)));
	  	    break;
	  	  }
	      
	      // road position and direction at gap0
	      int roadidx=d_muo->get_muidtrack(imuo);
	      particle->set_muID_gap0(0,itrk,RoadWrapper->get_Gap0Position(0,roadidx)); 
	      particle->set_muID_gap0(1,itrk,RoadWrapper->get_Gap0Position(1,roadidx)); 
	      particle->set_muID_gap0(2,itrk,RoadWrapper->get_Gap0Position(2,roadidx)); 
	      float dx=RoadWrapper->get_RefDirection(0,roadidx);
	      float dy=RoadWrapper->get_RefDirection(1,roadidx);
	      float dz=RoadWrapper->get_RefDirection(2,roadidx);
	      particle->set_muID_gap0(3,itrk,dx/dz); 
	      particle->set_muID_gap0(4,itrk,dy/dz); 
	    }
	    
	    particle->set_muIDhits(itrk, IDhits);
	  }
	
	  //---> store track hit information 
	  // (Raphael) The following code builds an integer with bits set for hit plans
	  // From less to most significant bits = from upstream to downstream hits
	  int TRhits=0;
	  for (int i=0;i<26;i++)
    if (d_muo->get_xhits(i,imuo)>=0) 
	  TRhits |= (1<<(d_muo->get_station(i,imuo)*6 
	         + d_muo->get_gap(i,imuo)*2 
	         + d_muo->get_cath(i,imuo)));
	  
    particle->set_muTRhits(itrk, TRhits);
	
	  //-----> Particle filling
	  particle->set_nhits(itrk, d_muo->get_nhits(imuo));
	  particle->set_charge(itrk, d_muo->get_charge(imuo)); 
	  particle->set_ghostflag(itrk, d_muo->get_ghostflag(imuo));
	  particle->set_chisquare(itrk,d_muo->get_chisquare(imuo));
	  particle->set_PID(itrk,0);                                  //---> WARNING: NOT FILLED
	  particle->set_MuonConfidence(itrk,0);                       //---> WARNING: NOT FILLED
	  particle->set_PionConfidence(itrk,0);                       //---> WARNING: NOT FILLED
	
	  // Particle momentum and position at vertex, station 1, station 2 and station 3
	  for (int iarr=0; iarr<4; iarr++){
	    particle->set_px(iarr, itrk, d_muo->get_px(iarr, imuo)); 
	    particle->set_py(iarr, itrk, d_muo->get_py(iarr, imuo)); 
	    particle->set_pz(iarr, itrk, d_muo->get_pz(iarr, imuo)); 
	    particle->set_xpos(iarr, itrk, d_muo->get_x(iarr, imuo)); 
	    particle->set_ypos(iarr, itrk, d_muo->get_y(iarr, imuo)); 
	    particle->set_zpos(iarr, itrk, d_muo->get_z(iarr, imuo)); 
	  }
         
	  //===== Particle Bent Plane momentum at station 1
	  particle->set_st1_bp_P(0,itrk,d_muo->get_pxbp(1,imuo)); 
	  particle->set_st1_bp_P(1,itrk,d_muo->get_pybp(1,imuo)); 
	  particle->set_st1_bp_P(2,itrk,d_muo->get_pzbp(1,imuo)); 
	
	  //===== Particle Bent Plane position at station 1
	  particle->set_st1_bp_pos(0,itrk,d_muo->get_xbp(1,imuo)); 
	  particle->set_st1_bp_pos(1,itrk,d_muo->get_ybp(1,imuo)); 
	  particle->set_st1_bp_pos(2,itrk,d_muo->get_zbp(1,imuo)); 

	  //===== Kalman filter covariance matrix
	  for (int iarr1=0; iarr1<5; iarr1++) 
	  for (int iarr2=0; iarr2<5; iarr2++) 
	  particle->set_cov(iarr1, iarr2, itrk, d_muo->get_cov(iarr1, iarr2, imuo)); 
	
	  //===== Particle quality check
	  if (_cutter->MuonOK(particle, itrk)) { 
	    itrk++;  // Increment the track if particle OK...
	    lastGood = 1; // quality flag for the last particle
	  }      
  }
    
  //===== see if the last one made the cut, if not, remove it
  if (!lastGood && particle->get_npart()>0){
	  particle->RemovePHParticle(itrk);
	  particle->set_npart(itrk) ;
  }else particle->set_npart(itrk);
 
  return;
} 

//______________________________________________________________________________________
void MWGReco::do_dimuons(PHMuoTracksOut *particle )
{
  int np = particle->get_npart(); int nd = np*(np-1)/2; // number of expected dimuons
  particle->Set_DimuArraySize(nd); particle->set_ndimu(nd);
  int idimu=0; bool lastGood=0;
  
  // do it if at least one muon is expected.
  //========================================
  if (nd>0){
    for (int idx1=0; idx1<np-1; idx1++) 
    for (int idx2=idx1+1; idx2<np; idx2++) 
    if (idimu!=nd){
      particle->AddPHDimuon(idimu);
      lastGood = 0;
      
      //===== setting tracks indices
      particle->set_ditrkIndex(0,idimu,idx1); 
      particle->set_ditrkIndex(1,idimu,idx2);
      
      //===== computing dimuon charge
      //  0 ==> +- dimuon pair
      //  1 ==> ++ dimuon pair
      // -1 ==> -- dimuon pair
      // -2 ==> one of the muon charge = 0 
      
      if (particle->get_charge(idx1)==0 || particle->get_charge(idx2)==0)
	    particle->set_dicharge(idimu,-2);
      else{
	      if (particle->get_charge(idx1) == particle->get_charge(idx2))
	      particle->set_dicharge(idimu,particle->get_charge(idx1));
	      else particle->set_dicharge(idimu,0);
      }
      
      //===== dimuon px, py, pz
      float px = particle->get_px(0,idx1) + particle->get_px(0,idx2); 
      particle->set_dipx(idimu,px);
      float py = particle->get_py(0,idx1) + particle->get_py(0,idx2); 
      particle->set_dipy(idimu,py);
      float pz = particle->get_pz(0,idx1) + particle->get_pz(0,idx2); 
      particle->set_dipz(idimu,pz);
      
      //===== dimuon mass computed at vertex
      particle->set_dimass(idimu,
        MWG::get_mass( 
          particle->get_px(0,idx1), particle->get_py(0,idx1), particle->get_pz(0,idx1), 
          particle->get_px(0,idx2), particle->get_py(0,idx2), particle->get_pz(0,idx2) ) ); 
      
      //===== Dimuon quality check (so far, nothing)
      if (_cutter->diMuonOK(particle, idimu)){
	      idimu++;
	      lastGood=1;
      }
    }
    if (!lastGood){
      particle->RemovePHDimuon(idimu);
      particle->set_ndimu(idimu);
    }else{particle->set_ndimu(idimu);}
  }
}

