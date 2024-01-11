
#include <phool.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <PHDataNode.h>
#include <PHIODataNode.h>
#include <getClass.h>

//  Data classes I am using in analysis
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <EventHeader.h>
#include <RunHeader.h>

#include <SyncObject.h>
#include <utiCentrality.h>
#include <ErtOut.h>
#include <PHGlobal.h>

#include <MpcMap.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>

// analysis header file
#include "MpcPi0Cal.h"


//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TTree.h>
#include <TBranch.h>
#include <TFile.h>
#include <TLorentzVector.h>
#include <TVector3.h>
#include <cmath>
//#include "mynamespace.h"
//#define mixdepth 7
using namespace std;
using namespace findNode;



int MpcPi0Cal::passedMpcAsymmetry(const pi0base* ph1, const pi0base* ph2)
{
  float alpha=fabs((ph1->e - ph2->e)/(ph1->e + ph2->e));
  if(alpha>=mpcAssym) return 0;
  return 1;
}


int MpcPi0Cal::passedMpcPhotonCuts(pi0base *ph)
{
  if(ph->e < 1.0) return 0;
  return 1;

}

int MpcPi0Cal::passedMpcPionCuts(const pi0base *ph1, const pi0base *ph2)
{
  if((ph1->e + ph2->e )< mpcEmin) return 0;
  if((ph1->e + ph2->e) > mpcEmax) return 0;
  if(!passedMpcAsymmetry(ph1,ph2)) return 0;
  float pt = sqrt(pow((ph1->px+ph2->px),2) + pow((ph1->py+ph2->py),2));
  if(pt < mpcPtmin || pt > mpcPtmax) return 0;
  if(ph1->arm != ph2->arm) return 0;
  float radius = sqrt( (float)pow( (float)(ph1->ia - ph2->ia),2 ) + (float)pow( (float)(ph1->ib - ph2->ib),2 )); 
  if(radius < 1.5) return 0; //requires neighboring towers cannot have clusters (includes diagonal neighbors)
  //if(radius > mpcMaxRadiusCut) return 0;  //this may vary with energy
  float sep = sqrt( (float)pow( (float)(ph1->a - ph2->a),2 ) + (float)pow( (float)(ph1->b - ph2->b),2 )); 
  if(sep < mpcMinRadius) return 0;
  return 1;
}

