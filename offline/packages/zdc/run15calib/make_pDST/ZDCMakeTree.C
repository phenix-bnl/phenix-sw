#include "ZDCMakeTree.h"
#include "PHCompositeNode.h"
#include "getClass.h"
#include "phool.h"
#include "TrigLvl1.h"
#include "SyncObject.h"
#include "BbcOut.h"
#include "ZdcRaw.h"
#include "TOAD.h"

#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <fstream>
#include <math.h>

#include "TFile.h"
#include "TTree.h"
#include "TH1.h"



//modified on 2014.02.06
//cut branched I don't use, and removed calibration part
using namespace std;

//===========================================

ZDCMakeTree::ZDCMakeTree(const char *name){
  ThisName=name;
}

//===========================================

ZDCMakeTree::~ZDCMakeTree(void){
}

//===========================================

int ZDCMakeTree::Init(PHCompositeNode *topNode)
{
  nevt=0;

  return 0;
}

//===========================================

int ZDCMakeTree::InitRun(PHCompositeNode *topNode){

  fout = new TFile(outfilename.c_str(), "recreate");

  tree = new TTree("T", "zdc tree");
  // tree->Branch("runnumber",       &runnumber,       "runnumber/I");
  //  tree->Branch("eventnumber",     &eventnumber,     "eventnumber/I");
  tree->Branch("trig",            &trigscaled,      "trig/i");
  tree->Branch("cross",           &cross,           "cross/I");
  tree->Branch("zvtx",            &zvtx,            "zvtx/F");
  tree->Branch("zvtxerror",       &zvtxerror,       "zvtxerror/F");
  tree->Branch("zdc_adc",         zdc_adc,          "zdc_adc[40]/S");
  tree->Branch("zdc_tdc0",        zdc_tdc0,         "zdc_tdc0[40]/S");
  tree->Branch("zdc_tdc1",        zdc_tdc1,         "zdc_tdc1[40]/S");

  return 0;
}

//===========================================

int ZDCMakeTree::process_event(PHCompositeNode *topNode)
{
  nevt++;

  if( nevt%10000==0 )
    cout<<nevt<<endl;

  ReInit();
  
  // SyncObject *sync = findNode::getClass<SyncObject>( topNode, "SyncObject" );
  TrigLvl1 *lv1 = findNode::getClass<TrigLvl1>( topNode, "TrigLvl1" );
  BbcOut *bbcout = findNode::getClass<BbcOut>( topNode, "BbcOut" );
  ZdcRaw *zdcraw = findNode::getClass<ZdcRaw>( topNode, "ZdcRaw" );

  //if ( sync == 0 || lv1 == 0 || bbcout == 0 || zdcraw == 0 ) 
  if ( lv1 == 0 || bbcout == 0 || zdcraw == 0 ) 
    {
      cerr << "ZDCMakeTree::process_event getClass ERROR" << endl;
      return 0;
    }

  // SyncObject, TrigLvl1
  //  runnumber   = sync->RunNumber();
  //  eventnumber = sync->EventNumber();
  trigscaled  = lv1->get_lvl1_trigscaled();
  cross = lv1->get_lvl1_clock_cross();

  // BbdOut
  zvtx = bbcout->get_VertexPoint();
  zvtxerror = bbcout->get_dVertexPoint();

  // ZdcRaw
  for ( int i=0; i<40; i++ )
    zdc_tdc0[i] = zdcraw->get_Tdc0(i);
  for ( int i=0; i<40; i++ )
    zdc_tdc1[i] = zdcraw->get_Tdc1(i);
  for ( int i=0; i<40; i++ )
    zdc_adc[i]  = zdcraw->get_Adc(i);

  // Get MB triggers or PPG triggers only
  if ( trigscaled > 0x00000020 && trigscaled < 0x10000000 )
    return 0;
    
  tree->Fill();  
  return 0;
}


int ZDCMakeTree::End(PHCompositeNode *topNode)
{
  cout << "nevt = " << nevt << endl;
  fout->cd();
  fout->Write();
  fout->Close();

  delete fout;
  return 0;
}

int ZDCMakeTree::ReInit(void)
{
  //  runnumber=-9999;
  //  eventnumber=-9999;
  trigscaled=0;
  cross=9999;
  zvtx = -9999.0;
  zvtxerror = -9999.0;
  for ( int i=0; i<40; i++ )
    zdc_tdc0[i] = 9999;
  for ( int i=0; i<40; i++ )
    zdc_tdc1[i] = 9999;
  for ( int i=0; i<40; i++ )
    zdc_adc[i]  = 9999;

  return 0;
}
