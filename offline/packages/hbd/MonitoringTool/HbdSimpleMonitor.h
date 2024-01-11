#ifndef __HBDSIMPLEMONITOR_H__
#define __HBDSIMPLEMONITOR_H__

#include <iostream>
#include "phool.h"
#include "SubsysReco.h"
#include "Fun4AllServer.h"

using namespace std;

class PHCompositeNode;
class TH1F;
class TH2F;
class Event;
class TNtuple;
class HbdClusterizer;
class HbdDcmRaw;

class HbdSimpleMonitor: public Fun4AllServer
{
   public:
   HbdSimpleMonitor(char *outfile="dumm.root");
   ~HbdSimpleMonitor();

   int process_event(PHCompositeNode* topNode, Event* evt)
      { return process_event(evt); };

   int process_event(Event* evt);

   int EndRun();

   private:
   HbdClusterizer *clus;
   HbdDcmRaw *dcmraw;

   TH1F *ADChist[240][32];
   TH1F *ADCCalhist[240][32];

   // Number of Hits in Pads
   TH1F *PadHits;

   // Ntuple for studying
   TNtuple *nt;

   // Z-phi coordinate center of cone distribution.
   TH2F *ConeHits;

   // Number of photo electrons per cone
   TH1F *PEInConeAll[40];
   TH1F *PEInConeSingle[40];
   TH1F *PEInConeMerged[40];

   // Number of photo electrons per cone
   TH1F *nHitsInConeAll[40];
   TH1F *nHitsInConeSingle[40];
   TH1F *nHitsInConeMerged[40];

   int date_first_flag;
   int evtcounter;
   int maxevt;

   char outfile[100];
};

#endif
