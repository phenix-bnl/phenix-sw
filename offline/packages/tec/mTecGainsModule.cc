#include "TecBasicObject.hh"
#include "mTecGainsModule.h"

#include "TecOutV1.hh"
#include "dPadClusterWrapper.h"
#include "BbcOut.h"
#include "EventHeader.h"
#include "RunHeader.h"

#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"
#include "PHCompositeNode.h"
#include "PHTimeStamp.h"

#include "TROOT.h"
#include "TFile.h"
#include "TH1.h"
#include "TF1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TProfile.h"

#include <cstdio>
#include <cmath>
#include <algorithm>
#include <vector>
#include <map>
#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<TecOutV1> TecOutNode_t;
typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;
typedef PHIODataNode <BbcOut> BbcOutNode_t;
typedef PHIODataNode<EventHeader> EventHeaderNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;

//======================================================================

mTecGainsModule::mTecGainsModule(){
  Verbose=0;
  RejectOverlaps=1;
  WriteHistograms=1;
  RunNumber=0;
  RunNumberS="0";
}

void 
mTecGainsModule::saveToFile() 
{
  TecGainsFile->Write();
}

PHBoolean 
mTecGainsModule::event(PHCompositeNode *root) 
{
  PHNodeIterator topIter(root);

  if(Verbose>0) cout << "mTecGainsModule: Started..." << endl;

  static int eventNumber = 0;
  static int first = 0;
  float nttrk[10];
  float ntpart[5];
  float ntpartrun[2];

//------------- Get Tables ---------------------------------------------
  int EventHeaderPresent=1;
  int RunHeaderPresent=1;

  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  TecOutV1* tecout;
  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
  }
  else {
    cerr << "mTecGainsModule ERROR: Can not find TecOutV1." << endl;
    return False;
  }

  dPadClusterNode_t* n8 = static_cast<dPadClusterNode_t*>(topIter.findFirst("PHIODataNode", "dPc1Cluster"));
  if (!n8) {
    if(Verbose>0) cerr 
      << "mTecGainsModule -> ERROR: dPc1Cluster table not found." << endl;
  }

  dPadClusterNode_t* n6 = static_cast<dPadClusterNode_t*>(topIter.findFirst("PHIODataNode", "dPc3Cluster"));
  if (!n6) {
    if(Verbose>0) 
      cerr << "mTecGainsModule -> ERROR: dPc3Cluster table not found." << endl;
  }

  PHTypedNodeIterator<BbcOut> bbciter(root);
  BbcOutNode_t *BbcOutNode = bbciter.find("BbcOut");
  if(!BbcOutNode) {
    if(Verbose>0) 
    cerr << "mTecGainsModule -> ERROR: BbcOut not found." << endl;
  }

  EventHeader* eventheader=0;
  PHTypedNodeIterator<EventHeader> evtiter(root);
  EventHeaderNode_t *EventHeaderNode = evtiter.find("EventHeader");
  if(EventHeaderNode) {
    eventheader = EventHeaderNode->getData();
  }
  else {
    if(Verbose>0)
    cerr << PHWHERE << " ERROR: EventHeader not found." << endl;
    EventHeaderPresent=0;
  }

  RunHeader* runheader=0;
  PHTypedNodeIterator<RunHeader> runiter(root);
  RunHeaderNode_t *RunHeaderNode = runiter.find("RunHeader");
  if(RunHeaderNode) {
    runheader = RunHeaderNode->getData();
  }
  else {
    if(Verbose>0)
    cerr << PHWHERE << " ERROR: RunHeader not found." << endl;
    RunHeaderPresent=0;
  }



  if(Verbose>0) {
    cout << "Number of Tracks: " << tecout->getNTracks() << endl;
    cout << "Number of Hits: " << tecout->getNHits() << endl;
  }

  // Get Time Stamp

    long tics;
    if(EventHeaderPresent) {
        tics = (long)eventheader->get_TimeStamp();
        if(Verbose>1) cout << "TimeStamp: " << tics << " " << (float)tics << endl;
    }
    else {
      tics = 0;
    }

// Get run number

    int runno=0;
    if(RunHeaderPresent) {
      runno = runheader->get_RunNumber();
    }

// Open output file and create ntuples

    char hfile[30];

    if(first==0) {
      first=1;

      if(runno!=0) {
        sprintf(hfile,"tecgains_run%d.root",runno);
      }
      else {
        if(RunNumber!=0) {
          sprintf(hfile,"tecgains_run%d.root",RunNumber);
        }
        else {
          sprintf(hfile,"tecgains_run%s.root",RunNumberS);
        }
      }

      TecGainsFile = new TFile(hfile,"RECREATE");
      tecgains = new TNtuple("tecgains","tecgains","mult:alpha:index:timebin:charge");

      tecgainsrun = new TNtuple("tecgainsrun","tecgainsrun","runno:time");

      tectrks = new TNtuple("tectrks","tectrks","mult:hitmult:xin:yin:zin:xout:yout:nhits:ntbins:alpha");

// Fill run/time ntuple only once

      if(RunNumber==0) {
        ntpartrun[0]=(float)runno;
      }
      else {
        ntpartrun[0]=(float)RunNumber;
      }
      ntpartrun[1]=(float)tics;
        tecgainsrun->Fill(ntpartrun);

    }


// Make a stl map of hits

    multimap<int, int, less<int> > HITMAP;

// Hits from several tracks (trkid=-2) are always rejected
// Pack amplitude and hit sector/plane/side/wire/bin into one integer

    for(int j=0; j<tecout->getNHits(); j++) {
        int trkid=tecout->getHitTrackID(j);
        if(trkid>-1) {
          int ampl = tecout->getHitADC(j);
          int glindex = tecout->getHitGlobalIndex(j);
          int amplindex = glindex + 10000000*ampl;
          HITMAP.insert(pair<int, int>(trkid,amplindex));
        }
    } 

// --- Loop over tracks ---

  if(Verbose>1) cout << "mTecGainsModule -> Start loop over tracks..." << endl;

  float Alpha;
  int nwtmp;

  for(int itrk=0; itrk<tecout->getNTracks(); itrk++) {

  Alpha = tecout->getTrackAlpha(itrk);
  //
  nttrk[0] = (float)tecout->getNTracks();
  nttrk[1] = (float)tecout->getNHits();
  nttrk[2] = tecout->getTrackXin(itrk);
  nttrk[3] = tecout->getTrackYin(itrk);
   float ztmp = 1.;
   if(tecout->getTrackSide(itrk)==0) ztmp = -1.;
  nttrk[4] = ztmp;
  nttrk[5] = tecout->getTrackXout(itrk);
  nttrk[6] = tecout->getTrackYout(itrk);
   nwtmp = 0;
   for(int i=0; i<TECMAXPLANE; i++) nwtmp += tecout->getTrackNwires(itrk,i);
  nttrk[7] = (float)nwtmp;
  nttrk[8] = tecout->getTrackNhits(itrk);
  nttrk[9] = Alpha;
  tectrks->Fill(nttrk);

// Loop over hits in the HITMAP and fill the ntuple

// Hits from several tracks are always rejected

      for(multimap<int, int, less<int> >::iterator it = HITMAP.begin();
          it != HITMAP.end(); ++it) {

           if((*it).first==itrk) {

          // unpack amplitude and hit sector/side/plane/wire/bin
             int amplindex = (*it).second;
             int myampl = amplindex/10000000; 
             int glindex = amplindex%10000000; 
             int ibin = (glindex%100000)%100;
             int iwire = (glindex%100000)/100;
             int iindex = glindex/100000;

		ntpart[0] = (float)tecout->getNTracks();
		ntpart[1] = Alpha;
		ntpart[2] = (float)(iwire + iindex*1000); 
                ntpart[3] = (float)ibin;
                ntpart[4] = (float)myampl;

		tecgains->Fill(ntpart);
	  } // if hit from this track 
      } // end loop over hits in HITMAP 
  } // end loop over tracks

  eventNumber++;
  
  if(Verbose>0) cout << "mTecGainsModule: Finished." << endl;
  return True;
}

