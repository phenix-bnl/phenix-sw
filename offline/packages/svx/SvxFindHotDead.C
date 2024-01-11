// ===============
// FILE: SvxFindHotDead.C
// ===============

// ******************************************************
// Author:  Sasha Lebedev (lebedev@iastate.edu)
// Revisions: September 2010 - initial version
// ***************************************************************************

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <cstdio>
#include <phool.h>
#include <iostream>

#include <SvxCommon.h>
#include <SvxFindHotDead.h>
#include <SvxDeadMap.h>
#include <SvxRawhitv3.h>
#include <SvxRawhitListv3.h>

#include <svxAddress.hh>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>

#include <TFile.h>
#include <TH2F.h>

using namespace std;

// Helpers for scanning Node Tree...
typedef PHIODataNode <PHObject>             PHObjectNode_t;
typedef PHIODataNode <SvxRawhitList>        SvxRawhitListNode_t;

//----------------------------------------------------------------------------------------------------

SvxFindHotDead::SvxFindHotDead(const string &name): SubsysReco(name)
{
//  SvxAddressObject = svxAddress();
  theMap = new SvxDeadMap(0); 
  WriteToDatabase=false;
  OutputFileName="svxPixelDeadMap.txt";
  EventNumber=0;
}

//------------------------------------------------------------------------------------------------------

// Run-independent initialization
int SvxFindHotDead::Init(PHCompositeNode *topNode)
{
  if(verbosity>0) cout << "SvxFindHotDead::Init() Execution started..." << endl;

  //if(!SvxAddressObject.isInitialized()){
  //  SvxAddressObject.Initialize();
  //}
  theMap->Verbosity(verbosity);

  if(verbosity>0) cout << "SvxFindHotDead::Init() Execution completed." << endl;

  return EVENT_OK;

}

//----------------------------------------------------------------------------------------------------

// Run-dependent initialization
int SvxFindHotDead::InitRun(PHCompositeNode *topNode)
{

  if(verbosity>0) cout << "SvxFindHotDead::InitRun() Execution started..." << endl;

  ///////////////////////
  // added by T.Hachiya 2011.06.17
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  //svxAddress& SvxAddressObject = *address;

  ///////////////////////


  // each pixel sensor has 256 by 128 channels ("phi" by Z)
  int nbinspixz=128;
  int nbinspixphi=256;
  float startpixz = -0.5; float stoppixz = 128.-0.5;
  float startpixphi = -0.5; float stoppixphi = 256.-0.5;
  char h_name[100];

  int maxladd[SVXLAYERNUMBER];
  maxladd[0]=SVXLADDERSLAYER0*2; maxladd[1]=SVXLADDERSLAYER1*2; maxladd[2]=SVXLADDERSLAYER2*2; maxladd[3]=SVXLADDERSLAYER3*2;
  int maxsens[SVXLAYERNUMBER];
  maxsens[0]=SVXSENSORSLAYER0; maxsens[1]=SVXSENSORSLAYER1; maxsens[2]=SVXSENSORSLAYER2; maxsens[3]=SVXSENSORSLAYER3;
  h_hotdead_layer0 = new TH2F*[maxladd[0]*maxsens[0]];
  h_hotdead_layer1 = new TH2F*[maxladd[1]*maxsens[1]];
  h_hotdead_layer2 = new TH2F*[maxladd[2]*maxsens[2]];
  h_hotdead_layer3 = new TH2F*[maxladd[3]*maxsens[3]];

  int nbooked=0;
  int ilayer=0;
    for(int iladder=0; iladder<maxladd[ilayer]; iladder++) {
      for(int isensor=0; isensor<maxsens[ilayer]; isensor++) {
        sprintf(h_name, "h_hotdead_layer%d_ladder%d_sensor%d", ilayer, iladder, isensor);
        h_hotdead_layer0[iladder*maxsens[ilayer]+isensor] = new TH2F(h_name, "", nbinspixz, startpixz, stoppixz, nbinspixphi, startpixphi, stoppixphi);
        nbooked++;
      }
    }

  ilayer=1;
    for(int iladder=0; iladder<maxladd[ilayer]; iladder++) {
      for(int isensor=0; isensor<maxsens[ilayer]; isensor++) {
        sprintf(h_name, "h_hotdead_layer%d_ladder%d_sensor%d", ilayer, iladder, isensor);
        h_hotdead_layer1[iladder*maxsens[ilayer]+isensor] = new TH2F(h_name, "", nbinspixz, startpixz, stoppixz, nbinspixphi, startpixphi, stoppixphi);
        nbooked++;
      }
    }

  if(verbosity>0) cout << "SvxFindHotDead::InitRun() Execution finished. Booked " << nbooked << " histograms." << endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

int SvxFindHotDead::process_event(PHCompositeNode *topNode)
{

  if(verbosity>0) cout << "SvxFindHotDead::process_event() Execution started..." <<endl;

  if(verbosity>0 && EventNumber==0)  { cout << "SvxFindHotDead topNode:"<< endl; topNode->print(); }

  if(verbosity>0) cout << "SvxFindHotDead::process_event() Getting raw hits..." <<endl;
  SvxRawhitList* d_rawhit = NULL;
  PHTypedNodeIterator<SvxRawhitList> iRAWHIT(topNode);
  SvxRawhitListNode_t *RAWHIT = iRAWHIT.find("SvxRawhitList");
  if (RAWHIT) d_rawhit = RAWHIT->getData();
  if (!d_rawhit) {
    cerr << PHWHERE << "SvxFindHotDead ERROR: rawhit data not in the Node Tree" << endl;
    return EVENT_OK;
  }

  int nSvxRawhits = d_rawhit->get_nRawhits();
  if(verbosity>0) {std::cout << "SvxFindHotDead: Initial number of raw hits = " << nSvxRawhits << std::endl;}

  ////////////////////////
  // added by T.Hachiya 2011.06.17
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  svxAddress& SvxAddressObject = *address;
  ////////////////////////



  for(int i=0; i<nSvxRawhits; i++) {
    SvxRawhit* tmp = d_rawhit->get_Rawhit(i);
    int layer = tmp->get_layer();
    if(layer>1) continue; // pixels only for now
    int ladder = tmp->get_ladder();
    int sensor = tmp->get_sensor();
    int channel = tmp->get_channel();
    int roc = tmp->get_pixelROC();
    int module = tmp->get_pixelModule();
    // coordinates of a pixel in a sensor (pixel layer only)
    int ix = SvxAddressObject.getSensorIX0(layer, ladder, sensor, channel, roc, module);
    int iz = SvxAddressObject.getSensorIZ0(layer, ladder, sensor, channel, roc, module);
    if(layer==0) {
      int ihist = ladder*SVXSENSORSLAYER0+sensor;
      (h_hotdead_layer0[ihist])->Fill(iz,ix,1.);
    }
  }

  if(verbosity>0) {cout << "SvxFindHotDead::process_event() Event processed." <<endl;}
  EventNumber++;
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------------------

int SvxFindHotDead::End(PHCompositeNode *topNode)
{

  if(verbosity>0) {cout << "SvxFindHotDead::End() Started..." <<endl;}

/*
  int maxladd[SVXLAYERNUMBER];
  maxladd[0]=SVXLADDERSLAYER0*2; maxladd[1]=SVXLADDERSLAYER1*2; maxladd[2]=SVXLADDERSLAYER2*2; maxladd[3]=SVXLADDERSLAYER3*2;
  int maxsens[SVXLAYERNUMBER];
  maxsens[0]=SVXSENSORSLAYER0; maxsens[1]=SVXSENSORSLAYER1; maxsens[2]=SVXSENSORSLAYER2; maxsens[3]=SVXSENSORSLAYER3;

  int minstat=100;

  int ilayer=0; int ireadout=0;
    for(int iladder=0; iladder<maxladd[ilayer]; iladder++) {
      for(int isensor=0; isensor<maxsens[ilayer]; isensor++) {
        int nent = (h_hotdead_layer0[iladder*maxsens[ilayer]+isensor])->GetEntries();
        if(verbosity>0) {std::cout << "Layer,ladder,sensor: " << ilayer << " " << iladder << " " << isensor << "   # of entries = " << nent << std::endl;}
        int nhot=0; int ndead=0;
        if(nent<256*128*minstat) continue; // not enough statistics
        for(int iz=0; iz<128; iz++) {
          for(int ix=0; ix<256; ix++) {
            float tmp = (h_hotdead_layer0[iladder*maxsens[ilayer]+isensor])->GetBinContent(iz,ix);
            if(tmp==0) { // dead channel
               theMap->set_pixelChannelStatus(ilayer,iladder,isensor,iz,ix,SVX_DEAD); 
               ndead++;
            }
            if(tmp>minstat*3/2) { // hot channel
               theMap->set_pixelChannelStatus(ilayer,iladder,isensor,iz,ix,SVX_HOT); 
               nhot++;
            }
          }
        }
        if(verbosity>0) {std::cout << "     # of dead channels = " << ndead << ",  # of hot channels = " << nhot << std::endl;}
      }
    }

  ilayer=1; ireadout=0;
    for(int iladder=0; iladder<maxladd[ilayer]; iladder++) {
      for(int isensor=0; isensor<maxsens[ilayer]; isensor++) {
        int nent = (h_hotdead_layer1[iladder*maxsens[ilayer]+isensor])->GetEntries();
        if(verbosity>0) {std::cout << "Layer,ladder,sensor: " << ilayer << " " << iladder << " " << isensor << "   # of entries = " << nent << std::endl;}
        int nhot=0; int ndead=0;
        if(nent<256*128*minstat) continue; // not enough statistics
        for(int iz=0; iz<128; iz++) {
          for(int ix=0; ix<256; ix++) {
            float tmp = (h_hotdead_layer0[iladder*maxsens[ilayer]+isensor])->GetBinContent(iz,ix);
            if(tmp==0) { // dead channel
               theMap->set_pixelChannelStatus(ilayer,iladder,isensor,iz,ix,SVX_DEAD); // 0 = DEAD
               ndead++;
            }
            if(tmp>minstat*3/2) { // hot channel
               theMap->set_pixelChannelStatus(ilayer,iladder,isensor,iz,ix,SVX_HOT); // 2 = HOT
               nhot++;
            }
          }
        }
        if(verbosity>0) {std::cout << "     # of dead channels = " << ndead << ",  # of hot channels = " << nhot << std::endl;}
      }
    }

  if(verbosity>0) {theMap->print();}

  // Write out hot/dead map to an ascii file or database;
  //
  if(WriteToDatabase) {
  // PHTimeStamp* Tbeg = new PHTimeStamp();
  // Tbeg->setToSystemTime();
  // PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
  // Tbeg->print(); cout << endl; Tend->print(); cout << endl;
  // theMap->writeToDatabase(PHTimeStamp * Tbeg, PHTimeStamp * Tend);
  } else {
     theMap->writePixelsToFile(OutputFileName);
  }
*/

  if(verbosity>0) {cout << "SvxFindHotDead::End() Finished." <<endl;}
  return EVENT_OK;
}

