/*
  
  BbcMultipleVtxReco.cc
  
  Created: 2011/09/09
  Last Update: 2011/11/17
  Author: Hideyuki Oide (oide@icepp.s.u-tokyo.ac.jp)
  
  
  Description:
  
  Manually reconstruct multiple BBC verteces from BbcRaw node
  using clustering for several timing binsize.
  
  Reconstructed vertex candidates can have not only real verteces but also ghosts
  depending on the number of composed clusters in each BBC.
  
  Reconstructed vertex candidates are stored in "BbcMultipleVtx" DST node.
  BbcMultipleVtx has an array of several results of clustering,
  each corresponding to the tuning the binsize of clustering.
  Each of the result is packed into BbcMultipleVtxList.
  
 */



#include "BbcMultipleVtxReco.hh"
#include <cstring>
#include <iostream>
#include <iomanip>
#include <string>
#include <cstdlib>
#include "gsl/gsl_rng.h"
#include <gsl/gsl_const.h>

// General
#include "getClass.h"
#include "PHCompositeNode.h"
#include "Fun4AllReturnCodes.h"
#include <recoConsts.h>

// DST
#include "RunHeader.h"
#include "EventHeader.h"

// BBC Readout
#include "BbcRaw.h"
#include "BbcOut.h"
#include "BbcCalib.hh"
#include "BbcEvent.hh"

// BbcMultipleVtx
#include "BbcMultipleVtxList_v2.hh"
//#include "BbcMultipleVtxCluster.hh"
//#include "BbcMultipleVtxPoint.hh"
#include "BbcMultipleVtx_v2.hh"

// ROOT Classes
#include "TH1F.h"

using namespace std;

static const float C = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9;
static const float BbcTimingFullscale = 21.0;


//____________________________________________________________________________________________________
BbcMultipleVtxReco::BbcMultipleVtxReco(const string &name): 
  SubsysReco(name),
  fT0offset( 0 ),
  fBbcCalib(NULL),
  mvtx( new BbcMultipleVtx_v2(kNtimebin) )
{
  for(Int_t ibin = 0; ibin < kNtimebin; ibin++) {
    timebin[ibin] = 0.1 * (ibin+1);
  }
}

BbcMultipleVtxReco::~BbcMultipleVtxReco()
{
  delete fBbcCalib;
}


//____________________________________________________________________________________________________
int BbcMultipleVtxReco::Init(PHCompositeNode *topNode){
  
  fBbcCalib = new BbcCalib();
  
  
  // Prepare BbcMultipleVtx node for output
  for(Int_t ibin = 0; ibin < kNtimebin; ibin++) {
    if( verbosity>0 ) {
      cout << Form("<BbcMultipleVtxReco::Init()>:: adding list [%d] timebin %.1f", ibin, timebin[ibin]) << endl;
    }
    
    /*
    BbcMultipleVtxList *list = new BbcMultipleVtxList();
    list->set_binsize( timebin[ibin] );
    mvtx->add_vtxlist( list );
    */
    mvtx->get_vtxlist(ibin)->set_binsize( timebin[ibin] );
    
  }
  
  PHIODataNode<BbcMultipleVtx> *mvtx_node
    = new PHIODataNode<BbcMultipleVtx>(mvtx, "BbcMultipleVtx", "PHObject");
  
  PHNodeIterator it( topNode );
  PHCompositeNode *dst_node
    = dynamic_cast<PHCompositeNode*>(it.findFirst("PHCompositeNode","DST"));
  dst_node->addNode( mvtx_node );
  
  
  if(verbosity>0) cout << "<BbcMultipleVtxReco::Init()>:: end" << endl;
  
  return 0;
}



//____________________________________________________________________________________________________
int BbcMultipleVtxReco::InitRun(PHCompositeNode *topNode){
  if(verbosity>0) cout << "<BbcMultipleVtxReco::Init()>:: start" << endl;
  
  fRunHeader   = findNode::getClass<RunHeader>(topNode, "RunHeader");
  fEventHeader = findNode::getClass<EventHeader>(topNode,"EventHeader");
  fBbcRaw      = findNode::getClass<BbcRaw>(topNode, "BbcRaw");
  
  fRunNumber = fRunHeader->get_RunNumber();
  
  if( fBbcRaw ) SetupBbcRead();
  
  return 0;
}



//____________________________________________________________________________________________________
int BbcMultipleVtxReco::End(PHCompositeNode *topNode){
  return 0;
}



//____________________________________________________________________________________________________
int BbcMultipleVtxReco::process_event(PHCompositeNode *top_node){
  
  fEventNumber = fEventHeader->get_EvtSequence();
  if(verbosity > 0) cout << Form( "\nEvtNum = %5d\n", fEventNumber ) << endl;
  
  ReconstructBbcEvent( top_node );
    
  return EVENT_OK;
}


//____________________________________________________________________________________________________
void BbcMultipleVtxReco::SetupBbcRead() {
  cout << "<SetupBbcRead>:: " << Form("Run Number = %d", fRunNumber) << endl;
  cout << "bbcread::InitRun started..." << endl;

  recoConsts *rc = recoConsts::instance();
  int runnumber = rc->get_IntFlag("RUNNUMBER");

  int icalibversion = 4002; // after Run5 and Field_ON

  cout << "bbcread::InitRun - run number: " << runnumber << endl;
  cout << "bbcread::InitRun - calibration version:  " << icalibversion << endl;
  rc->set_IntFlag("BBCCALIBVERSION", icalibversion);

  if(fBbcCalib)
  {
    PHTimeStamp TimeStp = rc->get_TimeStamp();
    int BBCCALIBVERSION = rc->get_IntFlag("BBCCALIBVERSION");
    cout << "BbcReco::InitRun - restored constants are for " << TimeStp << endl;
    fBbcCalib->restore(TimeStp, BBCCALIBVERSION);
  }

  
  cout << "bbcread::InitRun ended." << endl;
  
  return;
}

  

//____________________________________________________________________________________________________
void BbcMultipleVtxReco::ReconstructBbcEvent(PHCompositeNode *top_node) {
  // BBC, imported from Hachiya-san's class bbcread
  
  if(verbosity>0) cout << "<BbcMultipleVtxReco::ReconstructBbcEvent()>:: Start" << endl;
  
  if(fBbcRaw==NULL){
    cout << "<BbcMultipleVtxReco::ReconstructBbcEvent()>:: No BbcRaw!" << endl;
    return;
  }
  
  //---------------------
  if( verbosity > 0 ) 
    cout << "<BbcMultipleVtxReco::ReconstructBbcEvent()>:: start BbcEvent calculation" << endl;
  
  BbcEvent bbcevent;
  bbcevent.Clear();
  bbcevent.setCalibDataAll(fBbcCalib);
  
  // set rawdata to calculation module(BbcEvent);
  for(int ipmt=0; ipmt<BBC_N_PMT; ipmt++){ // corresponding to south0 - 63 to north 0-63
    bbcevent.setAdc (fBbcRaw->get_Adc(ipmt), ipmt);
    bbcevent.setTdc0(fBbcRaw->get_Tdc0(ipmt), ipmt);
    bbcevent.setTdc1(fBbcRaw->get_Tdc1(ipmt), ipmt);
  }
  
  bbcevent.calculate();
  
  Int_t npmt_north = bbcevent.getnHitPmt(Bbc::North);
  Int_t npmt_south = bbcevent.getnHitPmt(Bbc::South);
  if( verbosity > 0 ) {
    cout << Form("DST:   npmt north %d, south %d, total %d", npmt_north, npmt_south, (npmt_north + npmt_south)) << endl;
  }
  
  fBbcOutBbcZ = bbcevent.getZVertex();
  fBbcOutT0 = bbcevent.getTimeZero();
  
  if( verbosity > 0 ) 
    cout << "<BbcMultipleVtxReco::ReconstructBbcEvent()>:: end BbcEvent calculation" << endl;
  
  
  
  // Count the number of hits in each side BBC
  // then fill the number of hit, which PMT hits
  // and the charge of the hit PMT
  
  fBbcNhit[Bbc::South] = fBbcNhit[Bbc::North] = 0;
  
  for(int ipmt=0; ipmt<BBC_N_PMT; ipmt++){ // corresponding to south0 - 63 to north 0-63
    int ihit = (bbcevent.isArmHitPmt(ipmt)); // if this pmt contributes to Zvtx calculation
    if( ihit ) {
      bbc_hitbit[ipmt] = kTRUE;
    } else {
      bbc_hitbit[ipmt] = kFALSE;
    }
    
    if( verbosity > 0 ) {
      if( BBC_N_PMT/2 == ipmt ) cout << "--------------------------------------------------" << endl;
    }
    
    if( ihit ) {
      if( verbosity > 0 ) {
	cout << ipmt << ": ";
	cout << bbcevent.getCharge(ipmt)<<" ";    // Nhit particles in this PMT
	cout << bbcevent.getHitTime0(ipmt)<<" ";  // Corrected Hittime of TDC0 in nano second 
	cout << bbcevent.getHitTime1(ipmt)<<endl; // Corrected Hittime of TDC1 in ns
      }
      
      if(ipmt < BBC_N_PMT/2)  // South
	fBbcNhit[Bbc::South]++;
      else                         // North
	fBbcNhit[Bbc::North]++;
      
    }
    
  }
  
  mvtx->set_bbcout_zvtx( fBbcOutBbcZ );
  mvtx->set_bbcout_t0( fBbcOutT0 );
  
  
  //
  // Clustering for each binwidth and threshold
  //
  const Int_t thr = 0;
  for(Int_t ibin = 0; ibin < kNtimebin; ibin++) {
    const Int_t nbins = (Int_t)( BbcTimingFullscale / timebin[ibin] );
    TH1 *hbbctime[2];
    hbbctime[Bbc::North] = new TH1F("hbbctime_north", "BBCTime North", nbins, 0.0, BbcTimingFullscale);
    hbbctime[Bbc::South] = new TH1F("hbbctime_south", "BBCTime South", nbins, 0.0, BbcTimingFullscale);
    
    is_cluster_made = kFALSE;
    
    ClearClusterVariables();
    
    // Perform clustering and vertex reconstruction only in the case that both BBC have hit
    if(fBbcNhit[Bbc::North]>0 && fBbcNhit[Bbc::South]>0) {
      
      MakeCluster(bbcevent, hbbctime, thr);
      ReconstructVertex();
      
      //if( verbosity > 0 && ibin==2 ) DumpClusterInfo(ibin, thr);
    }
    
    FillToDST( ibin );
    
    delete hbbctime[Bbc::North];
    delete hbbctime[Bbc::South];
  }
  
}



//____________________________________________________________________________________________________
void BbcMultipleVtxReco::MakeCluster(BbcEvent& bbcevent, TH1 **hbbctime, const int thr) {
  if(verbosity > 0 )
    cout << Form("\n<BbcMultipleVtxReco::MakeCluster()>:: size = %.1f thr = %d",
		 hbbctime[Bbc::North]->GetBinWidth(0), thr)
	 << endl;
  
  Int_t nhit[2] = { 0 };
  for(int ipmt=0; ipmt<BBC_N_PMT; ipmt++){ // corresponding to south0 - 63 to north 0-63
    int ihit = (bbcevent.isArmHitPmt(ipmt)); // if this pmt contributes to Zvtx calculation
    
    if( ihit ) {
      if( ipmt < BBC_N_PMT/2) {
	hbbctime[Bbc::South]->Fill(bbcevent.getHitTime0(ipmt));
	nhit[Bbc::South]++;
      } else {
	hbbctime[Bbc::North]->Fill(bbcevent.getHitTime0(ipmt));
	nhit[Bbc::North]++;
      }
    }
    
  }
  if(verbosity > 1) {
    cout << Form("nhit: South = %d, North = %d",
		 nhit[Bbc::South], nhit[Bbc::North])
	 << endl;
    hbbctime[Bbc::South]->Print();
    hbbctime[Bbc::North]->Print();
  }
  
  // Dump hbbctime if verbose level >1
  /*
  if( verbosity>1 ) {
    for(Int_t ibin = 0; ibin < hbbctime[Bbc::North]->GetNbinsX(); ibin++) {
      cout << Form("  North bin %3d: %7.2f ns, %5d",
		   ibin,
		   hbbctime[Bbc::North]->GetBinLowEdge(ibin+1),
		   hbbctime[Bbc::North]->GetBinContent(ibin+1) )
	   << endl;
    }
    for(Int_t ibin = 0; ibin < hbbctime[Bbc::South]->GetNbinsX(); ibin++) {
      cout << Form("  South bin %3d: %7.2f ns, %5d",
		   ibin,
		   hbbctime[Bbc::South]->GetBinLowEdge(ibin+1),
		   hbbctime[Bbc::South]->GetBinContent(ibin+1) )
	   << endl;
    }
  }
  */
  
  
  Float_t time_list[100];
  Int_t pmt_list[100];
  Float_t time_mean = 0.0;
  Float_t time_rms = 0.0;
  Bool_t pmt_bit[BBC_N_PMT] = { kFALSE };
    
  // BBC South
  {
    if( verbosity > 0 ) cout << "BbcSouth Loop -----------------------------" << endl;

    TH1 *hs = hbbctime[Bbc::South];
    Int_t nloop = 0;
    while( 1 ) {
      time_mean = 0.0;
      time_rms = 0.0;
    
      const Int_t maxbin = hs->GetMaximumBin();
      const Float_t mint = hs->GetBinLowEdge( maxbin-2 );
      const Float_t maxt = hs->GetBinLowEdge( maxbin+3 );
      if( verbosity > 0 ) {
	cout << Form("  nloop = %d :", nloop );
	if(verbosity > 1) {
	  cout << Form("  maxbin = %d: mint = %.3f, maxt = %.3f",
		       maxbin, mint, maxt);
	}
	cout << endl;
      }
      
      Int_t size = 0;
      for(Int_t ibin = maxbin-2; ibin < maxbin+3; ibin++) {
	hs->SetBinContent(ibin, 0);
      }
    
      Int_t nlist = 0;
      for(Int_t ipmt = 0; ipmt < BBC_N_PMT/2; ipmt++) {
	if(pmt_bit[ipmt]) continue;
	
	Float_t time = bbcevent.getHitTime0(ipmt);
	if(verbosity > 1 && time>-1.e3) cout << Form("  [%3d]: time = %7.2f", ipmt, time) << endl;
	
	if( time > mint && time < maxt) {
	  if( verbosity > 1) cout << Form("    ==> registering pmt [%3d]", ipmt) << endl;
	  time_mean += time;
	  time_list[nlist] = time;
	  pmt_list[nlist] = ipmt;
	  pmt_bit[ipmt] = kTRUE;
	  nlist++;
	  size++;
	}
      }
      time_mean /= (nlist*1.0);
    
      for(Int_t ilist = 0; ilist < nlist; ilist++) {
	time_rms += (time_list[ilist] - time_mean)*(time_list[ilist] - time_mean);
      }
      time_rms /= (nlist*1.0);
    
      if( size > thr ) {
	fBbcClusSize[Bbc::South][nloop] = size;
	fBbcClusPos[Bbc::South][nloop] = time_mean;
	fBbcClusPos_rms[Bbc::South][nloop] = time_rms;
	fBbcNclus[Bbc::South]++;
	
	if(verbosity > 0) {
	  cout << Form("   maxbin = %d: mint = %.3f, maxt = %.3f, size = %d",
		       maxbin, mint, maxt, size)
	       << endl;
	  cout << Form("   cluster ID %d: size = %d, time_mean = %.3f, time_rms = %.3f",
		       nloop, size, time_mean, time_rms)
	       << endl;
	  if(verbosity > 1) {
	    cout << Form("   cluster list: ") << endl;
	    for(Int_t ilist = 0; ilist < nlist; ilist++) {
	      cout << Form("     [%3d] (%7.2f)", pmt_list[ilist], time_list[ilist]) << endl;
	    }
	  }
	}
      }
      
      nloop++;
      if( 0 == size ) {
	if( verbosity > 0) cout << "   ==> size = 0; break." << endl;
	break;
      }
      if( nloop > kNMaxClus-1 ) {
	if( verbosity > 0) cout << "   ==> nloop > kNMaxClus-1; break." << endl;
	break;
      }
    }
  }
  

  // BBC North
  {
    if( verbosity > 0 ) cout << "BbcNorth Loop -----------------------------" << endl;
    
    TH1 *hn = hbbctime[Bbc::North];
    Int_t nloop = 0;
    while( 1 ) {
      
      time_mean = 0.0;
      time_rms = 0.0;
      
      const Int_t maxbin = hn->GetMaximumBin();
      const Float_t mint = hn->GetBinLowEdge( maxbin-2 );
      const Float_t maxt = hn->GetBinLowEdge( maxbin+3 );
      if( verbosity > 0 ) {
	cout << Form("  nloop = %d :", nloop );
	if(verbosity > 1) {
	  cout << Form("  maxbin = %d: mint = %.3f, maxt = %.3f",
		       maxbin, mint, maxt);
	}
	cout << endl;
      }
      
      Int_t size = 0;
      for(Int_t ibin = maxbin-2; ibin < maxbin+3; ibin++) {
	hn->SetBinContent(ibin, 0);
      }
      
      Int_t nlist = 0;
      for(Int_t ipmt = BBC_N_PMT/2; ipmt < BBC_N_PMT; ipmt++) {
	if(pmt_bit[ipmt]) continue;
	
	Float_t time = bbcevent.getHitTime0(ipmt);
	if(verbosity > 1 && time>-1.e3) cout << Form("  [%3d]: time = %7.2f", ipmt, time) << endl;
	
	if( time > mint && time < maxt) {
	  if( verbosity > 1) cout << Form("    ==> registering pmt [%3d]", ipmt) << endl;
	  time_mean += time;
	  time_list[nlist] = time;
	  pmt_list[nlist] = ipmt;
	  pmt_bit[ipmt] = kTRUE;
	  nlist++;
	  size++;
	}
      }
      time_mean /= (nlist*1.0);
    
      for(Int_t ilist = 0; ilist < nlist; ilist++) {
	time_rms += (time_list[ilist] - time_mean)*(time_list[ilist] - time_mean);
      }
      time_rms /= (nlist*1.0);
      
      if( size > thr ) {
	fBbcClusSize[Bbc::North][nloop] = size;
	fBbcClusPos[Bbc::North][nloop] = time_mean;
	fBbcClusPos_rms[Bbc::North][nloop] = time_rms;
	fBbcNclus[Bbc::North]++;
	
	if(verbosity > 0) {
	  cout << Form("   maxbin = %d: mint = %.3f, maxt = %.3f, size = %d",
		       maxbin, mint, maxt, size)
	       << endl;
	  cout << Form("   cluster ID %d: size = %d, time_mean = %.3f, time_rms = %.3f",
		       nloop, size, time_mean, time_rms)
	       << endl;
	  if(verbosity > 1) {
	    cout << Form("   cluster list: ") << endl;
	    for(Int_t ilist = 0; ilist < nlist; ilist++) {
	      cout << Form("     [%3d] (%7.2f)", pmt_list[ilist], time_list[ilist]) << endl;
	    }
	  }
	  
	}
      }
    
      nloop++;
      if( 0 == size ) {
	if( verbosity > 0) cout << "   ==> size = 0; break." << endl;
	break;
      }
      if( nloop > kNMaxClus-1 ) {
	if( verbosity > 0) cout << "   ==> nloop > kNMaxClus-1; break." << endl;
	break;
      }
    }
  }
    

  is_cluster_made = kTRUE;
  
  if( verbosity > 1 )
    cout << Form("<BbcMultipleVtxReco::MakeCluster()>:: end") << endl;
  
  return;
}



//____________________________________________________________________________________________________
void BbcMultipleVtxReco::ReconstructVertex() {
  if( verbosity > 0 )
    cout << Form("\n<BbcMultipleVtxReco::ReconstructVertex()>") << endl;
  
  if( !is_cluster_made ) {
    cerr << "<BbcMultipleVtxReco::ReconstructVertex()>:: cluster is not made yet!" << endl;
    return;
  }
  
  
  const float L = 288.7;        /* distance between BBC north and south [cm]*/
  const float TTT = L / C;        /* total transit time [ns] */
  
  for(Int_t iclusN = 0; iclusN < fBbcNclus[Bbc::North]; iclusN++) {
    for(Int_t iclusS = 0; iclusS < fBbcNclus[Bbc::South]; iclusS++) {
      
      if(verbosity>0) cout << Form("iclusN = %d, iclusS = %d : ", iclusN, iclusS);
      
      Float_t south_hittime = fBbcCalib->getArmHitTime(Bbc::South, fBbcClusPos[Bbc::South][iclusS]);
      Float_t north_hittime = fBbcCalib->getArmHitTime(Bbc::North, fBbcClusPos[Bbc::North][iclusN]);
      
      fBbcVtx_candidate[iclusN*kNMaxClus+iclusS]
	= (south_hittime - north_hittime) * C / 2.0;
      
      fBbcT0_candidate[iclusN*kNMaxClus+iclusS]
	= fBbcCalib->getCorrectedTzero(fBbcClusPos[Bbc::South][iclusS], fBbcClusPos[Bbc::North][iclusN]) - TTT / 2.0;
      
      if(verbosity>0)
	cout << Form("vtx = %7.2f, T0 = %7.2f",
		     fBbcVtx_candidate[iclusN*kNMaxClus+iclusS], fBbcT0_candidate[iclusN*kNMaxClus+iclusS] )
	     << endl;
    }
  }
  
  if( verbosity > 0) cout << "\n\n" << endl;
  
  if( verbosity > 1 )
    cout << Form("\n<BbcMultipleVtxReco::ReconstructVertex()>:: end") << endl;
  
  return;
}


//____________________________________________________________________________________________________
void BbcMultipleVtxReco::FillToDST(const int bin) {
  if( verbosity > 0 )
    cout << Form("\n<BbcMultipleVtxReco::FillToDST()>") << endl;
  
  if( verbosity > 1 ) mvtx->print();
  
  BbcMultipleVtxList *list = mvtx->get_vtxlist( bin );
  list->Reset();
  
  if( verbosity > 1 ) list->print();
  
  for(Int_t iclusN = 0; iclusN < fBbcNclus[Bbc::North]; iclusN++) {
    //BbcMultipleVtxCluster *cluster = new BbcMultipleVtxCluster();
    //cluster->set_order( iclusN );
    //cluster->set_size( fBbcClusSize[Bbc::North][iclusN] );
    //cluster->set_tof( fBbcClusPos[Bbc::North][iclusN] );
    //list->add_cluster( Bbc::North, cluster );
    list->add_cluster( Bbc::North, iclusN, fBbcClusSize[Bbc::North][iclusN], fBbcClusPos[Bbc::North][iclusN] );
  }
	
  for(Int_t iclusS = 0; iclusS < fBbcNclus[Bbc::South]; iclusS++) {
    //BbcMultipleVtxCluster *cluster = new BbcMultipleVtxCluster();
    //cluster->set_order( iclusS );
    //cluster->set_size( fBbcClusSize[Bbc::South][iclusS] );
    //cluster->set_tof( fBbcClusPos[Bbc::South][iclusS] );
    //list->add_cluster( Bbc::South, cluster );
    list->add_cluster( Bbc::South, iclusS, fBbcClusSize[Bbc::South][iclusS], fBbcClusPos[Bbc::South][iclusS] );
  }
	  
  for(Int_t iclusS = 0; iclusS < fBbcNclus[Bbc::South]; iclusS++) {
    for(Int_t iclusN = 0; iclusN < fBbcNclus[Bbc::North]; iclusN++) {
      //nBbcMultipleVtxPoint *vtxpoint = new BbcMultipleVtxPoint();
      //vtxpoint->set_vtxz( fBbcVtx_candidate[iclusN*kNMaxClus+iclusS] );
      //vtxpoint->set_t0( fBbcT0_candidate[iclusN*kNMaxClus+iclusS] );
      //vtxpoint->set_cluster_order( Bbc::North, iclusN );
      //vtxpoint->set_cluster_order( Bbc::South, iclusS );
      //list->add_vtx_point( vtxpoint );
      list->add_vtx_point( iclusS, iclusN,
			   fBbcVtx_candidate[iclusN*kNMaxClus+iclusS],
			   fBbcT0_candidate[iclusN*kNMaxClus+iclusS]   );
    }
  }
  
  if( verbosity > 1 )
    cout << Form("\n<BbcMultipleVtxReco::FillToDST()>") << endl;
}



//____________________________________________________________________________________________________
void BbcMultipleVtxReco::ClearClusterVariables() {
  fBbcNclus[Bbc::North] = 0;
  fBbcNclus[Bbc::South] = 0;
  
  for(Int_t iclus = 0; iclus < kNMaxClus; iclus++) {
    fBbcClusPos[Bbc::North][iclus] = -9999.0;
    fBbcClusPos[Bbc::South][iclus] = -9999.0;
    fBbcClusPos_rms[Bbc::North][iclus] = -9999.0;
    fBbcClusPos_rms[Bbc::South][iclus] = -9999.0;
    fBbcClusSize[Bbc::North][iclus] = -9999;
    fBbcClusSize[Bbc::South][iclus] = -9999;
  }
  
  for(Int_t iclusN = 0; iclusN < kNMaxClus; iclusN++) {
    for(Int_t iclusS = 0; iclusS < kNMaxClus; iclusS++) {
      fBbcT0_candidate[iclusN*kNMaxClus+iclusS] = 0.0;
      fBbcVtx_candidate[iclusN*kNMaxClus+iclusS] = 0.0;
    }
  }
  
  return;
}

//____________________________________________________________________________________________________
void BbcMultipleVtxReco::DumpClusterInfo(const int bin, const int thr) {
  cout << "------------------------------------" << endl;
  cout << "<BbcMultipleVtxReco::DumpClusterInfo>" << endl;
  cout << Form("  bin : %d (%.1f ns)", bin, timebin[bin]) << endl;
  cout << Form("  thr : %d\n", thr) << endl;
  cout << Form("  BBC North Nhit: %d", fBbcNhit[Bbc::North]) << endl;
  cout << Form("  BBC South Nhit: %d", fBbcNhit[Bbc::South]) << endl;
  cout << Form("  Bbc North Ncluster: %d", fBbcNclus[Bbc::North]) << endl;
  for(Int_t iclus=0; iclus<fBbcNclus[Bbc::North]; iclus++) {
    cout << Form("    [%d]: size = %2d, Time: %8.2f (%8.2f)",
		 iclus, fBbcClusSize[Bbc::North][iclus],
		 fBbcClusPos[Bbc::North][iclus], fBbcClusPos_rms[Bbc::North][iclus])
	 << endl;
  }
  cout << Form("  Bbc South Ncluster: %d", fBbcNclus[Bbc::South]) << endl;
  for(Int_t iclus=0; iclus<fBbcNclus[Bbc::South]; iclus++) {
    cout << Form("    [%d]: size = %2d, Time: %8.2f (%8.2f)",
		 iclus, fBbcClusSize[Bbc::South][iclus],
		 fBbcClusPos[Bbc::South][iclus], fBbcClusPos_rms[Bbc::South][iclus])
	 << endl;
  }
  cout << endl;
  
  cout << "  Vertex Candidates" << endl;
  Int_t nth = 0;
  for(Int_t iclusN = 0; iclusN<fBbcNclus[Bbc::North]; iclusN++) {
    for(Int_t iclusS = 0; iclusS<fBbcNclus[Bbc::South]; iclusS++) {
      if(iclusN < 3 && iclusS < 3) {
	cout << Form("    [%d]: North[%d] South[%d]: vtx = %9.2f, t0 = %9.2f",
		     nth, iclusN, iclusS,
		     fBbcVtx_candidate[iclusN*kNMaxClus+iclusS],
		     fBbcT0_candidate[iclusN*kNMaxClus+iclusS]  )
	     << endl;
	nth++;
      }
    }
  }
  cout << "------------------------------------" << endl;
}




//____________________________________________________________________________________________________
void BbcMultipleVtxReco::PrintBbcRaw(PHCompositeNode *top_node){
  if(!fBbcRaw) return;
  
  cout << "BBCRaw:" << endl;
  Int_t npmt = fBbcRaw->get_npmt();
  for(Int_t i=0; i<npmt; i++) {
    Short_t id = fBbcRaw->get_Pmt(i);
    Short_t adc = fBbcRaw->get_Adc(i);
    Short_t tdc0 = fBbcRaw->get_Tdc0(i);
    Short_t tdc1 = fBbcRaw->get_Tdc1(i);
    cout << Form("  PMT%3d: ADC = %5d, TDC0 = %5d, TDC1 = %5d", id, adc, tdc0, tdc1) << endl;
  }
}



//____________________________________________________________________________________________________
void BbcMultipleVtxReco::PrintBbcOut(PHCompositeNode *top_node){
  if(!fBbcOut) return;
  
  Float_t vtxPt = fBbcOut->get_VertexPoint();
  Float_t dvtxPt = fBbcOut->get_dVertexPoint();
  Float_t t0 = fBbcOut->get_TimeZero();
  Float_t dt0 = fBbcOut->get_dTimeZero();
  Int_t   npmt_north = fBbcOut->get_nPmt(Bbc::North);
  Int_t   npmt_south = fBbcOut->get_nPmt(Bbc::South);
  Float_t chsum_north = fBbcOut->get_ChargeSum(Bbc::North);
  Float_t chsum_south = fBbcOut->get_ChargeSum(Bbc::South);
  Float_t timing_north = fBbcOut->get_Timing(Bbc::North);
  Float_t timing_south = fBbcOut->get_Timing(Bbc::South);
  printf("BbcOut::\n");
  printf(" vtxPt = %7.2f, dvtxPt = %7.2f, t0 = %7.2f, dt0 = %7.2f,\n", vtxPt, dvtxPt, t0, dt0);
  printf(" North: npmt = %3d, chsum = %7.2f, timing = %7.2f,\n", npmt_north, chsum_north, timing_north);
  printf(" South: npmt = %3d, chsum = %7.2f, timing = %7.2f,\n", npmt_south, chsum_south, timing_south);
  
  return;
}


