#include "TofCalibMakeTree.h"
#include "phool.h"
#include "RunHeader.h"
#include "PHGlobal.h"
#include "PhCglList.h"
#include "PhCglListv4_Run7a.h"
#include "PHCentralTrack.h"
#include "getClass.h"
#include "Fun4AllServer.h"
#include "recoConsts.h"
#include "PHCentralTrack.h"

#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"

#include <iostream>
#include <fstream>

typedef PHIODataNode <PHGlobal> PHGlobalNode_t;
using namespace std;

int nloop=0;

TofCalibMakeTree::TofCalibMakeTree(const char *OutFile){
  cout << "! Start TofCalibMakeTree" << endl;
  OutFileName = OutFile;
  cout << "OutFileName = " << OutFileName << endl;
  return;
}

int TofCalibMakeTree::Init(PHCompositeNode *topNode){
  hfile = new TFile(OutFileName.c_str(),"RECREATE");
  tree = new TTree("TofTree","tof tree for calibration");
  tree -> Branch("RunNumber"  ,&RunNumber  ,"RunNumber/i"  );
//  tree -> Branch("ZdcEnergyN" ,&ZdcEnergyN ,"ZdcEnergyN/F" );
//  tree -> Branch("ZdcEnergyS" ,&ZdcEnergyS ,"ZdcEnergyS/F" );
//  tree -> Branch("BbcMultN"   ,&BbcMultN   ,"bbcMultN/I"   );
//  tree -> Branch("BbcMultS"   ,&BbcMultS   ,"bbcMultS/I"   );
//  tree -> Branch("BbcChargeN" ,&BbcChargeN ,"BbcChargeN/F" );
//  tree -> Branch("BbcChargeS" ,&BbcChargeS ,"BbcChargeS/F" );
  tree -> Branch("BbcZVertex" ,&BbcZVertex ,"BbcZVertex/F" );
  tree -> Branch("BbcTimeZero",&BbcTimeZero,"BbcTimeZero/F");
//  tree -> Branch("BbcPercentile",&BbcPercentile,"BbcPercentile/F");
//  tree -> Branch("NumberTofHits",&NumberTofHits,"NumberTofHits/S");
  tree -> Branch("Centrality",&Centrality,"Centrality/F");
  
  tree -> Branch("ntrack"  , &ntrack  , "ntrack/I"         );
  tree -> Branch("charge"  , &charge  , "charge[ntrack]/S" );
//  tree -> Branch("quality" , &quality , "quality[ntrack]/S");
//  tree -> Branch("zed"     , &zed     , "zed[ntrack]/F"    );
//  tree -> Branch("alpha"   , &alpha   , "alpha[ntrack]/F"  );
//  tree -> Branch("phi"     , &phi     , "phi[ntrack]/F"    );
//  tree -> Branch("phi0"    , &phi0    , "phi0[ntrack]/F"   );
//  tree -> Branch("the0"    , &the0    , "the0[ntrack]/F"   );
  tree -> Branch("mom"     , &mom     , "mom[ntrack]/F"    );
//  tree -> Branch("ptofx"   , &ptofx   , "ptofx[ntrack]/F"  );
//  tree -> Branch("ptofy"   , &ptofy   , "ptofy[ntrack]/F"  );
//  tree -> Branch("ptofz"   , &ptofz   , "ptofz[ntrack]/F"  );
  tree -> Branch("pltof"   , &pltof   , "pltof[ntrack]/F"  );
  tree -> Branch("tofdphi" , &tofdphi , "tofdphi[ntrack]/F");
  tree -> Branch("tofdz"   , &tofdz   , "tofdz[ntrack]/F"  );
  tree -> Branch("tofsdphi", &tofsdphi, "tofsdphi[ntrack]/F");
  tree -> Branch("tofsdz"  , &tofsdz  , "tofsdz[ntrack]/F"  );
  tree -> Branch("slat"    , &slat    , "slat[ntrack]/I"   );
  tree -> Branch("ttof"    , &ttof    , "ttof[ntrack]/F"   );
  tree -> Branch("etof"    , &etof    , "etof[ntrack]/F"   );
  tree -> Branch("dcarm"   , &dcarm   , "dcarm[ntrack]/S"  );

//
  tree -> Branch("toftdc1" , &toftdc1 , "toftdc1[ntrack]/F"   );
  tree -> Branch("toftdc2" , &toftdc2 , "toftdc2[ntrack]/F"   );
  tree -> Branch("tofph1"  , &tofph1  , "tofph1[ntrack]/F"    );
  tree -> Branch("tofph2"  , &tofph2  , "tofph2[ntrack]/F"    );
//

/*
  if(RunNumber > 122223)
  {  
    tree -> Branch("tofph1" , &tofph1 , "tofph1[ntrack]/F" );
    tree -> Branch("tofph2" , &tofph2 , "tofph2[ntrack]/F" );
    tree -> Branch("toftdc1", &toftdc1, "toftdc1[ntrack]/F");
    tree -> Branch("toftdc2", &toftdc2, "toftdc2[ntrack]/F");
  }
*/

  return 0;
}

int TofCalibMakeTree::InitRun(PHCompositeNode* topNode){
  RunHeader *runheader = findNode::getClass<RunHeader>(topNode,"RunHeader");
  RunNumber = runheader->get_RunNumber();
  if     (IsRun4_200GeV())   cout << "This run is Run4 200GeV"   << endl;
  else if(IsRun4_62GeV())    cout << "This run is Run4 62GeV"    << endl;
  else if(IsRun5_200GeV())   cout << "This run is Run5 200GeV"   << endl;
  else if(IsRun5_62GeV())    cout << "This run is Run5 62GeV"    << endl; 
  else if(IsRun5_22_5GeV())  cout << "This run is Run5 22_5GeV"  << endl;
  else if(IsRun5_pp_200GeV())cout << "This run is Run5 pp 200GeV"<< endl;
  else if(IsRun6_pp_200GeV())cout << "This run is Run6 pp 200GeV"<< endl;
  else if(IsRun6_pp_62GeV()) cout << "This run is Run6 pp 62GeV" << endl;
  else if(IsRun7_AuAu_200GeV()) cout << "This run is Run7 AuAu 200GeV" << endl;
  else if(IsRun8_dAu_200GeV())  cout << "This run is Run8 dAu  200GeV"  << endl;
  else if(IsRun8_pp_200GeV())   cout << "This run is Run8 pp   200GeV"   << endl;
  else if(IsRun9_pp_500GeV())   cout << "This run is Run9 pp   500GeV"   << endl;
  else if(IsRun9_pp_200GeV())   cout << "This run is Run9 pp   200GeV"   << endl;
  else if(IsRun10_AuAu_200GeV())   cout << "This run is Run10 AuAu   200GeV"   << endl;
  else if(IsRun10_AuAu_62GeV())   cout << "This run is Run10 AuAu   62GeV"   << endl;
  else if(IsRun10_AuAu_39GeV())   cout << "This run is Run10 AuAu   39GeV"   << endl;
  else if(IsRun10_AuAu_7GeV())   cout << "This run is Run10 AuAu   7GeV"   << endl;
  else cout << "!! We can not use this run number !!" << endl;
  cout << "runnumber is " << RunNumber << endl;
  return 0;
}

int TofCalibMakeTree::End(PHCompositeNode *topNode){
  hfile -> Write();
  cout << "! End TofCalibMakeTree" << endl;
  return 0;
}

int TofCalibMakeTree::process_event(PHCompositeNode *topNode){
//  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode,"PhCglList");
  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode,"PHCentralTrack");

  PHGlobal *phglobal    = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if (!d_cnt){
    cout << PHWHERE << "Could not find the tracks !!" <<endl;
    return -1;
  }
  if (!phglobal){
    cout << PHWHERE << "Could not find the phglobal !!" <<endl;
    return -1;
  }
  
  if(nloop%1000==0) cout << "nloop=" << nloop << endl;
  nloop ++;

  ZdcEnergyN    = phglobal->getZdcEnergyN();
  ZdcEnergyS    = phglobal->getZdcEnergyS();
  BbcMultN      = phglobal->getBbcMultN();
  BbcMultS      = phglobal->getBbcMultS();
  BbcChargeN    = phglobal->getBbcChargeN();
  BbcChargeS    = phglobal->getBbcChargeS();
  BbcZVertex    = phglobal->getBbcZVertex();
  BbcTimeZero   = phglobal->getBbcTimeZero();
//  BbcPercentile = phglobal->getBbcPercentile();
  NumberTofHits = phglobal->getNumberTofHits();
  Centrality    = phglobal->getCentrality();

  bool isGoodEvent
    =  fabs(BbcZVertex) < 30.0
    && BbcMultN   > 0 && BbcMultS   > 0
    && BbcChargeN > 0 && BbcChargeS > 0;
  if(!isGoodEvent) return 1;

  ntrack = 0;
  //npart = d_cnt->get_npart();

  for(unsigned int i=0; i<d_cnt->get_npart(); i++){
    charge  [ntrack] = d_cnt->get_charge(i);
    quality [ntrack] = d_cnt->get_quality(i);
    zed     [ntrack] = d_cnt->get_zed(i);
    alpha   [ntrack] = d_cnt->get_alpha(i);
    phi     [ntrack] = d_cnt->get_phi(i);    
    phi0    [ntrack] = d_cnt->get_phi0(i);    
    the0    [ntrack] = d_cnt->get_the0(i);    
    mom     [ntrack] = d_cnt->get_mom(i);
    ptofx   [ntrack] = d_cnt->get_ptofx(i);
    ptofy   [ntrack] = d_cnt->get_ptofy(i);
    ptofz   [ntrack] = d_cnt->get_ptofz(i);
    pltof   [ntrack] = d_cnt->get_pltof(i);
    tofdphi [ntrack] = d_cnt->get_tofdphi(i);
    tofdz   [ntrack] = d_cnt->get_tofdz(i);
    tofsdphi[ntrack] = d_cnt->get_tofsdphi(i);
    tofsdz  [ntrack] = d_cnt->get_tofsdz(i);
    slat    [ntrack] = d_cnt->get_slat(i);
    ttof    [ntrack] = d_cnt->get_ttof(i);
    etof    [ntrack] = d_cnt->get_etof(i);
    dcarm   [ntrack] = d_cnt->get_dcarm(i); 
//
      tofph1 [ntrack] = d_cnt->get_tofph1(i);
      tofph2 [ntrack] = d_cnt->get_tofph2(i);
      toftdc1[ntrack] = d_cnt->get_toftdc1(i);
      toftdc2[ntrack] = d_cnt->get_toftdc2(i);
//




  if(RunNumber > 122223) 
    {
      tofph1 [ntrack] = d_cnt->get_tofph1(i);
      tofph2 [ntrack] = d_cnt->get_tofph2(i);
      toftdc1[ntrack] = d_cnt->get_toftdc1(i);
      toftdc2[ntrack] = d_cnt->get_toftdc2(i);
    }
    bool isGoodTrack 
      =  (quality[ntrack]==31 || quality[ntrack]==63)
      && fabs(zed[ntrack])<75.0
      && the0[ntrack]>-100
      && mom[ntrack]>0.0   && mom[ntrack]<16.0
      && pltof[ntrack]>0.0 && pltof[ntrack]<1000
      && slat[ntrack]>-1   && slat[ntrack]<960
      && etof[ntrack]>0.0;
    if(!isGoodTrack) continue;

    ntrack ++;
  
  }

  if( ntrack != 0 ) tree -> Fill();

  return 0;
}

bool TofCalibMakeTree::IsRun4_200GeV()
{
  if(RunNumber >= 107445 && RunNumber <= 122223) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun4_62GeV()
{
  if(RunNumber >= 122470 && RunNumber <= 123564) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun5_200GeV()
{
  if(RunNumber >= 149539 && RunNumber <= 160487) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun5_62GeV()
{
  if(RunNumber >= 161196 && RunNumber <= 163463) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun5_22_5GeV()
{
  if(RunNumber >= 163604 && RunNumber <= 163681) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun5_pp_200GeV()
{
  if(RunNumber >= 166416 && RunNumber <= 179846) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun6_pp_200GeV()
{
  if(RunNumber >= 188216 && RunNumber <= 204639) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun6_pp_62GeV()
{
  if(RunNumber >= 205153 && RunNumber <= 206495) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun7_AuAu_200GeV()
{
  if(RunNumber >= 227016 && RunNumber <= 240121) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun8_dAu_200GeV()
{
  if(RunNumber >= 246214 && RunNumber <= 253701) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun8_pp_200GeV()
{
  if(RunNumber >= 256450 && RunNumber <= 259575) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun9_pp_500GeV()
{
  if(RunNumber >= 276324 && RunNumber <= 280240) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun9_pp_200GeV()
{
  if(RunNumber >= 281911 && RunNumber <= 291515) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun10_AuAu_200GeV()
{
  if(RunNumber >= 300105 && RunNumber <= 310454) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun10_AuAu_62GeV()
{
  if(RunNumber >= 310656 && RunNumber <= 313322) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun10_AuAu_39GeV()
{
  if(RunNumber >= 313472 && RunNumber <= 314994) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun10_AuAu_7GeV()
{
  if(RunNumber >= 315422 && RunNumber <= 318946) return true;
  else return false;
}
bool TofCalibMakeTree::IsRun11_pp_500GeV()
{
  if(RunNumber >= 331130 && RunNumber <= 340515) return true;
  else return false;
}
