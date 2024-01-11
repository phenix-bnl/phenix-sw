//
//  General PHENIX tools
//

#include "HbdGainAnalyzer.h"
#include "HbdFinalSimSupport.h"
#include "hbdDetectorGeo.hh"
#include "HbdCellList.h"
#include "HbdCell.h"
#include "HbdMiniCellList.h"
#include "HbdMiniCell.h"

#include <Fun4AllHistoManager.h>
#include <Fun4AllServer.h>
#include <PHCentralTrack.h>
#include <PHGlobal.h>
#include <SyncObject.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHDataNode.h>
#include <phool.h>
#include <getClass.h>

typedef PHIODataNode <PHCentralTrack> PHCentralTrackNode_t;
typedef PHIODataNode <PHGlobal>       PHGlobalNode_t;
typedef PHIODataNode <HbdCellList> HbdCellListNode_t;
typedef PHIODataNode <HbdMiniCellList> HbdMiniCellListNode_t;

#include <TString.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TLeaf.h>
#include <TH1F.h>
#include <TH2F.h>

using namespace std;
using namespace findNode;


//should not have 
HbdGainAnalyzer::HbdGainAnalyzer(const char *outfile):SubsysReco("gain analyzer")
{
   ThisName = "Hbd Gain Analyzer";
   hbdgeo = new hbdDetectorGeo();
   simsupport= new HbdFinalSimSupport();//needed for the pad coordinates
   sprintf(outfilename,"%s",outfile);

}

HbdGainAnalyzer::~HbdGainAnalyzer()
{
   delete hbdgeo;
}

int HbdGainAnalyzer::Init(PHCompositeNode *topNode)
{
  ncalls=0;

  //kWriteFullTree= kFALSE;
  //kUseFullCell= kFALSE;
  /**********
  Tree Construction
  **********/
  

  fHBDTree = new TTree("EventInfo","HBD Tree for Calibrations");
 
  
  fHBDTree->Branch("RunInfo",0,"run/I2:seg/I2:event/I2:zvtx/F:bbcS/F:bbcN/F:nDCtracks/I2",128000);//,128000
  fHBDTree->Branch("_HBDCells",0,"_HBDCells/I",128000);//ncells
  //full cell branch
  //fHBDTree->Branch("HBDCells",0,"charge[_HBDCells]/F:sector[_HBDCells]/I2:arm[_HBDCells]/I2:module[_HBDCells]/I2:padnum[_HBDCells]/I2:adcch[_HBDCells]/I2:trackflag[_HBDCells]/I2:locy[_HBDCells]/F:locz[_HBDCells]/F:globx[_HBDCells]/F:globy[_HBDCells]/F:globz[_HBDCells]/F:globphi[_HBDCells]/F",128000);
  //short cell branch
   fHBDTree->Branch("HBDCells",0,"charge[_HBDCells]/F:sector[_HBDCells]/I2:arm[_HBDCells]/I2:module[_HBDCells]/I2:padnum[_HBDCells]/I2:adcch[_HBDCells]/I2:trackflag[_HBDCells]/I2",128000);
  fHBDTree->Branch("_TrackProj",0,"_TrackProj/I2",128000);//number of good tracks
  fHBDTree->Branch("TrackProj",0,"x[_TrackProj]/F:y[_TrackProj]/F:z[_TrackProj]/F:phi[_TrackProj]/F",128000);
 
  //init the variables
  f_RunInfo_Run      = 0;
  f_RunInfo_Seg      = 0;
  f_RunInfo_Event    = 0;
  f_RunInfo_ZVtx     = 0.0;
  f_RunInfo_bbcS     = 0.0;
  f_RunInfo_bbcN     = 0.0;
  f_RunInfo_DCTracks =0;
  //f_HBDCell_ncells  = 0; //dynamic
  // f_TrackProj       = 0; //dynamic


  
  //************************** 
  //deepali's Histograams declaration
  //histograam per module
  //******************************
  Fun4AllServer *se = Fun4AllServer::instance();

  char name[500];
  
  for (Int_t i=0; i<Narms; i++)
    {
      for (Int_t j=0; j<Nsides; j++)
	{
	  for (Int_t k=0; k<Nsect; k++)
	    {
	      sprintf(name,"h_mod_arm_%d_side_%d_sect_%d",i,j,k);
	      h_mod_gain[i][j][k] = new TH1F(name,name,2048,0.5,2048.5);
	      se->registerHisto(name,h_mod_gain[i][j][k]);
	    }
	}
    }
   
  h_counter = new TH1F("h_counter","h_counter",10,0.5,10.5);
  se->registerHisto("h_counter",h_counter);
  
  return 0;
}


int HbdGainAnalyzer::InitRun(PHCompositeNode *topNode)
{
  SyncObject *sync = getClass<SyncObject>(topNode,"Sync");
  f_RunInfo_Run    = sync->RunNumber();
  f_RunInfo_Seg    = sync->SegmentNumber();
  
  hbdgeo->fetch(f_RunInfo_Run);

  hbdDetectorGeo t;
   t.fetchPad(f_RunInfo_Run);
  //when using HbdMiniCell
  for(int i=0;i<2304;i++){
      int seqsec,padid,arm,sec;
      t.getPadInfo(i,arm,sec,seqsec,padid);
      SeqSec[i] = seqsec;
      PadId[i] = padid-1; //pad numbers go from 0-191
  } 

  // fetching ADC calibrations
  //calib = new hbdAdcCalib();
  //calib->fetch(run);

  //put reco constants if needed
  //e.g average gain per module 

  cout << "This run number is: " << f_RunInfo_Run << " Segment: "<< f_RunInfo_Seg << endl;
  tmpf = new TFile(outfilename,"recreate");

  tmpf->cd();

  return 0;
}


int HbdGainAnalyzer::process_event(PHCompositeNode *topNode)
{
  if(++ncalls%10000==0) { cout << "Ncalls = " << ncalls << endl; }
  
  GetNodes(topNode);
  //cout<<"Got node"<<endl;
  fill_RunInfo(topNode);
  //cout<<"Fill run info"<<endl;

  fill_TrackProj(topNode);//Has to be run BEFORE fill_HBDCell();
  //cout<<"Fill track projection"<<endl;

  fill_HBDCell(topNode);//Has to be run AFTER fill_TrackProj(); Fills histograms as well
  //cout<<"Fill HBD cell"<<endl;

  fHBDTree->Fill();
  //cout<<"Fill tree"<<endl;
  
  //write tree at 250000 events 
  if(ncalls%200000==0) { cout << "Write a tree at Ncalls = " << ncalls << endl; 
  Int_t tcalls= int(ncalls/200000);
  //cout<<"tcalls = "<<tcalls<<endl;
 
  tmpf->cd();
  fHBDTree->Write();
  fHBDTree->Reset();
  tmpf->Close();
  //cout<<" file is = "<<outfilename<<endl;
  
  //Change File 
  //f_name = outfilename_number;
  sprintf(f_name,"%s_%d",outfilename,tcalls);
  tmpf = new TFile(f_name,"recreate");
  tmpf->cd();
  // cout<<outfilename<<endl;
  //cout<<f_name<<endl;
  }
  
  return 0;
}

int HbdGainAnalyzer::ResetEvent(PHCompositeNode *topNode)
{
  //Do I need this again?
  /*  
  f_RunInfo_Seg      = 0;
  f_RunInfo_Event    = 0;
  f_RunInfo_ZVtx     = 0.0;
  f_RunInfo_bbcS     = 0.0;
  f_RunInfo_bbcN     = 0.0;
  f_RunInfo_DCTracks = 0;

  //f_HBDCell_ncells  = 0; /dynamic
 
  //f_TrackProj       = 0; //dynamic
 
*/
  return 0;
}

void HbdGainAnalyzer::fill_RunInfo(PHCompositeNode *topNode)
{
  SyncObject *sync = getClass<SyncObject>(topNode,"Sync");
  f_RunInfo_Seg      = sync->SegmentNumber();
  f_RunInfo_Event    = sync->EventNumber();
  if(g){
    f_RunInfo_ZVtx     = g->getBbcZVertex();
    f_RunInfo_bbcS     = g->getBbcChargeS();
    f_RunInfo_bbcN     = g->getBbcChargeN();
  }
  else {//they are init, but just in case 
    f_RunInfo_ZVtx     = 0.0;
    f_RunInfo_bbcS     = 0.0;
    f_RunInfo_bbcN     = 0.0;
  }
  
  if (t) {f_RunInfo_DCTracks = t->get_npart();}
  else {f_RunInfo_DCTracks=-1; }
  
  TBranch *fBranch = (TBranch *) fHBDTree->GetBranch("RunInfo");
  ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(&f_RunInfo_Run);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(&f_RunInfo_Seg);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(&f_RunInfo_Event);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(&f_RunInfo_ZVtx);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(&f_RunInfo_bbcS);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(&f_RunInfo_bbcN);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(&f_RunInfo_DCTracks);

  
  return;
}

void HbdGainAnalyzer::fill_HBDCell(PHCompositeNode *topNode)
{
  //Deepali's event counter
  h_counter->Fill(5);

  Bool_t fZeroSuppress = !kTRUE;
  
  Short_t iCell = 0;

  //Get number of (mini)cells
  //for Mini cell list
  for(Int_t iThisCell=0 ; iThisCell<MiniCellList->get_nCells() ; iThisCell++) {
    if(MiniCellList->get_cell(iThisCell)->get_charge()<=0 && fZeroSuppress) {
      continue;
    }
    iCell++;
  }
  
  /*//else if cell list
    for(Int_t iThisCell=0 ; iThisCell<CellList->get_nCells() ; iThisCell++) {
      
      if(CellList->get_cell(iThisCell)->get_charge()<=0 && fZeroSuppress) {
	continue;
      }
      iCell++;
    }
  */
  f_HBDCell_ncells = iCell;//dynamic 
  iCell = 0;

  //mini cell list
  for(Int_t iThisCell=0 ; iThisCell<MiniCellList->get_nCells() ; iThisCell++) {
    if(MiniCellList->get_cell(iThisCell)->get_charge()<=0 && fZeroSuppress) {
      continue; }
    //cell list
    //for(Int_t iThisCell=0 ; iThisCell<CellList->get_nCells() ; iThisCell++) {
    //if(CellList->get_cell(iThisCell)->get_charge()<=0 && fZeroSuppress) {
    // continue; }
   
    //MiniCellList
    //charge3 is short
    f_HBDCell_charge[iCell] = float((MiniCellList->get_cell(iThisCell)->get_charge())/3);

    f_HBDCell_adcch[iCell] = MiniCellList->get_cell(iThisCell)->get_adcch();
    f_HBDCell_padnum[iCell]  = PadId[(int)f_HBDCell_adcch[iCell]];
    f_HBDCell_sector[iCell] = SeqSec[(int)f_HBDCell_adcch[iCell]];

    f_HBDCell_arm[iCell]    = 0;

    /*
    else{//FullCell
    //for MiniCell this is charge3
    f_HBDCell_charge[iCell] = CellList->get_cell(iThisCell)->get_charge();
    f_HBDCell_sector[iCell] = CellList->get_cell(iThisCell)->get_sector();
     f_HBDCell_arm[iCell]    = 0;
    if(f_HBDCell_sector[iCell]>=6){ f_HBDCell_arm[iCell] = 1; }
    f_HBDCell_padnum[iCell] = CellList->get_cell(iThisCell)->get_padnum();
    }
    */

    //setting the module/side sector(0-6) and arm
    Int_t c_side=0;
    if(f_HBDCell_sector[iCell]<6 && f_HBDCell_padnum[iCell]<=96)//East south
      {
	c_side = 0; f_HBDCell_arm[iCell]    = 0;
      }
    if(f_HBDCell_sector[iCell]>=6 && f_HBDCell_padnum[iCell]<=96)//West north
      {
	c_side = 1; f_HBDCell_arm[iCell] = 1;
	f_HBDCell_sector[iCell] = f_HBDCell_sector[iCell]-6;
      }
    if(f_HBDCell_sector[iCell]<6 && f_HBDCell_padnum[iCell]>96)//east north
      {
	c_side = 1; f_HBDCell_arm[iCell]    = 0;
      }
    if(f_HBDCell_sector[iCell]>=6 && f_HBDCell_padnum[iCell]>96)//west south
      {
	c_side = 0; f_HBDCell_arm[iCell] = 1;
	f_HBDCell_sector[iCell] = f_HBDCell_sector[iCell]-6;
      }
    
    f_HBDCell_module[iCell] = c_side;
    
    //Fill hitsograms  
    h_mod_gain[f_HBDCell_arm[iCell]][f_HBDCell_module[iCell]][f_HBDCell_sector[iCell]] ->Fill(f_HBDCell_charge[iCell]);
    //end fill histos    
   

 
    f_HBDCell_locy[iCell] = simsupport->get_pad_center(f_HBDCell_padnum[iCell],1);
    f_HBDCell_locz[iCell] = simsupport->get_pad_center(f_HBDCell_padnum[iCell],0);
    
    Double_t x_glob = 0.0;
    Double_t y_glob = 0.0;
    Double_t z_glob = 0.0;
    
    hbdgeo->LocToGlob(f_HBDCell_locy[iCell], f_HBDCell_locz[iCell],
		      x_glob,y_glob,z_glob,f_HBDCell_sector[iCell]);
    f_HBDCell_globx[iCell]  = x_glob;
    f_HBDCell_globy[iCell]  = y_glob;
    f_HBDCell_globz[iCell]  = z_glob;
    f_HBDCell_globphi[iCell]  = atan2(f_HBDCell_globy[iCell],
				      f_HBDCell_globx[iCell]);
    
    f_HBDCell_flag[iCell]   = 0;    
    
    for(Int_t iTrack=0 ; iTrack<f_TrackProj ; iTrack++) {
      if(fabs(f_TrackProj_phi[iTrack]-f_HBDCell_globphi[iCell])<0.03 &&
	 fabs(f_TrackProj_z[iTrack]-f_HBDCell_globz[iCell])<2.) {
	f_HBDCell_flag[iCell] = 1; 
	break; 
      }
    }
    

    iCell++;
  }

  TBranch *fBranch = fHBDTree->GetBranch("_HBDCells");
  ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(&f_HBDCell_ncells);
  
  /*
  //long branch
  fBranch = fHBDTree->GetBranch("HBDCells");
  ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(f_HBDCell_charge);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(f_HBDCell_sector);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(f_HBDCell_arm);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(f_HBDCell_module);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(f_HBDCell_padnum);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(f_HBDCell_adcch);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(f_HBDCell_flag);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(f_HBDCell_locy);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(8))->SetAddress(f_HBDCell_locz);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(9))->SetAddress(f_HBDCell_globx);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(10))->SetAddress(f_HBDCell_globy);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(11))->SetAddress(f_HBDCell_globz);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(12))->SetAddress(f_HBDCell_globphi);
  */
  
  //short branch
 fBranch = fHBDTree->GetBranch("HBDCells");
  ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(f_HBDCell_charge);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(f_HBDCell_sector);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(f_HBDCell_arm);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(f_HBDCell_module);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(f_HBDCell_padnum);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(f_HBDCell_adcch);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(f_HBDCell_flag);
 
  return;
}


void HbdGainAnalyzer::fill_TrackProj(PHCompositeNode *topNode)
{  
  Int_t iTrack = 0;
  if(t) {
    for(Int_t iThisTrack=0 ; iThisTrack<int(t->get_npart()) ; iThisTrack++) {
      if(t->get_charge(iThisTrack)==0) { continue; }
      if(t->get_quality(iThisTrack)!=63 &&
	 t->get_quality(iThisTrack)!=31) { continue; }
      if(t->get_n0(iThisTrack)<2) { continue; }
      if(t->get_ecore(iThisTrack)>=0.15) { continue; }
      iTrack++;
    }}
  
  f_TrackProj = iTrack;//dynamic variable for the tree
  
  iTrack = 0;
  if(t) {
    for(Int_t iThisTrack=0 ; iThisTrack<int(t->get_npart()) ; iThisTrack++) {
      if(t->get_charge(iThisTrack)==0) { continue; }
      if(t->get_quality(iThisTrack)!=63 && 
	 t->get_quality(iThisTrack)!=31) { continue; }
      if(t->get_n0(iThisTrack)<2) { continue; }
      if(t->get_ecore(iThisTrack)>=0.15) { continue; }
    
      f_TrackProj_x[iTrack]   = t->get_phbdx(iThisTrack);
      f_TrackProj_y[iTrack]   = t->get_phbdy(iThisTrack);
      f_TrackProj_z[iTrack]   = t->get_phbdz(iThisTrack);
      f_TrackProj_phi[iTrack] = atan2(f_TrackProj_y[iTrack],
				    f_TrackProj_x[iTrack]);
      
      iTrack++;
    }
  }
  else {
    f_TrackProj_x[iTrack]   = 0.0;
    f_TrackProj_y[iTrack]   = 0.0;
    f_TrackProj_z[iTrack]   = 0.0;
    f_TrackProj_phi[iTrack] = 0.0;
  }

  TBranch *fBranch = fHBDTree->GetBranch("_TrackProj");
  ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(&f_TrackProj); 
  fBranch = fHBDTree->GetBranch("TrackProj");
  ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(f_TrackProj_x);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(f_TrackProj_y);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(f_TrackProj_z);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(f_TrackProj_phi);
  
 
  return;
}

int HbdGainAnalyzer::End(PHCompositeNode *topNode)
{
  tmpf->cd();
  fHBDTree->Write();
  //fHBDTree->Write("0",2);//TObject::kWriteDelete 4,overwrite 2
  cout << "End of run Tree written with " << fHBDTree->GetEntries() << " entries."<< endl;
  tmpf->Close();
  
  return 0;
}

 void HbdGainAnalyzer::GetNodes(PHCompositeNode *topNode)
{
  //
  //Set all pointers to null...
  //
  
  t = 0;
  g = 0;
  
  //
  //Get the Cell List
  //
 
  //MiniCellList 
  MiniCellList = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
  if (!MiniCellList){
    cout << PHWHERE << "Hbd Calibrator:: No HbdMiniCellList. Something is wrong!" << endl;
    //return -1;
  }
  /* 
  // or use Full cell (in macro use MiniToFullCell
  CellList = findNode::getClass<HbdCellList>(topNode,"HbdCellList");
  if(!CellList) {
    //  cout << PHWHERE << "Hbd Calibrator:: No HbdCellList!" << endl; 
    //return;
  }
  */

  //
  // Search out the nodes from the node tree...
  // Tracks
  //
  PHTypedNodeIterator<PHCentralTrack> iCNT(topNode);
  PHCentralTrackNode_t *CNT = iCNT.find("PHCentralTrack");
  if(CNT) t = CNT->getData();
  //if(!t) cout << PHWHERE << "HbdGainReco:: PHCentralTrack not in Node Tree" << endl;

  //
  // Tracks
  //
  PHTypedNodeIterator<PHGlobal> iGBL(topNode);
  PHGlobalNode_t *GBL = iGBL.find("PHGlobal");
  if(GBL) g = GBL->getData();
  //if(!g) cout << PHWHERE << "HbdGainReco:: PHGlobal not in Node Tree" << endl;
  
  return;
}
