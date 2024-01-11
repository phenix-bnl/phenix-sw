//  GENERal PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
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
#include <THmulf.h>
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TTree.h>
#include <TBranch.h>
#include <TFile.h>
#include <TLorentzVector.h>
#include <TVector3.h>
#include <cmath>

//#define mixdepth 7

using namespace std;
using namespace findNode;


int MpcPi0Cal::SetMpcPhoton(pi0base *ph, mpcClusterContent* clus, float zvertex, int run, int event)
{

  ph->event      = event;
  ph->run        = run;
  ph->e          = clus->ecore();
  ph->z          = clus->z()-zvertex;
  ph->a          = clus->x();
  ph->b          = clus->y();
  ph->zvtx       = zvertex;
  ph->arm        = clus->arm();
  
  
  ph->ia         = (short)clus->ixpos();
  ph->ib         = (short)clus->iypos();
    
  TVector3 T3v1(ph->a,ph->b,ph->z);
  
  ph->px         = ph->e*T3v1(0)/T3v1.Mag();
  ph->py         = ph->e*T3v1(1)/T3v1.Mag();
  ph->pz         = ph->e*T3v1(2)/T3v1.Mag();
  ph->pt         = sqrt(ph->px*ph->px + ph->py*ph->py);
  ph->frozen = 0;

  
  return 1;
}

int MpcPi0Cal::SetMpcPion(pi0base *pi, pi0base *ph1, pi0base *ph2)
{
  
  if(ph1->arm == ph2->arm && ph1->event == ph2->event && ph1->run == ph2->run)
    {
      pi->arm     = ph1->arm;
      pi->event   = ph1->event;
      pi->run     = ph1->run;
      
    }
  else
    return 0;
  
  pi->e       = ph1->e + ph2->e;
  pi->px      = ph1->px + ph2->px;
  pi->py      = ph1->py + ph2->py;
  pi->pz      = ph1->pz + ph2->pz;
  pi->pt      = sqrt(pi->px*pi->px + pi->py*pi->py);
  
  TLorentzVector T4v(pi->px,pi->py,pi->pz,pi->e);
  float mass2 = T4v*T4v;
  if(mass2 < 0) return 0;
  pi->mass    =  sqrt(mass2);
  pi->theta = T4v.Theta();
  pi->phi = T4v.Phi();
  pi->zvtx       = ph1->zvtx;

  
  if(pi->phi < -TMath::Pi()/2.0) pi->phi += 2.0*TMath::Pi();  //take range from -180-180 to -90-270
  
  return 1;
}	  
  
