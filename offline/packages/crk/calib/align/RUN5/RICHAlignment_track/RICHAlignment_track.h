#ifndef __RICHALIGNMENT_TRACK_H__
#define __RICHALIGNMENT_TRACK_H__

//  General PHENIX tools
#include "Fun4AllServer.h"
#include "getClass.h"
#include "PHCompositeNode.h"
#include "phool.h"
//#include "PHNodeIOManager.h"
//#include "PHIODataNode.h"
#include "SubsysReco.h"

//  Data classes I am using in analysis
#include "PHCentralTrack.h"
#include "PHTrackOut.h"
#include "CglTrack.h"
#include "CrkHit.h"
#include "CrkGeometryObject.hh"

#include "PHGeometry.h"
//#include "PHSphereSection.h"
#include "PHPoint.h"
#include "PHLine.h"

#include "TROOT.h"
#include "TTree.h"
#include "TFile.h"

#include <string>
#include <iostream>
#include <fstream>
#include <stdlib.h>

using namespace PHGeometry;
using namespace std;
using namespace findNode;

class PHCompositeNode;
class PHCentralTrack;
class CrkHit;
class dCrkHitWrapper;

//typedef PHIODataNode <PHCentralTrack> PHPNode_t;
//typedef PHIODataNode <CrkHit> PHCNode_t;

class RICHAlignment_track: public SubsysReco
{
 public:
  RICHAlignment_track(const char* outfile = "EXAMPLE.root");
  virtual ~RICHAlignment_track() {}
  
  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);
  
 protected:
  int ntrk;
  int arm;
  int side;
  float cross_z;
  float cross_phi;
  float npe0;
  float mom;
  float ecore;
  float center_phi;
  float center_z;
  int n0;
  float chi2;
  float disp;
  
  int panel;
  int npmt;
  short pmt[1000];
  float npe[1000];
  double path;
  
  TTree *tree ;
  TFile *fout ; 
  char OutFileName[100];
  
  float v_ref[3];
  float b_ref[3];
  float ppc1pos[3];
  float ppc3pos[3];
  float posx[1000];
  float posy[1000];
  float posz[1000];
  float posr[1000];
  float posphi[1000];
  float start[3];
  float end[3];
  
  PHPoint cross_to_crk;
  PHPoint pstart, pend;
  PHPoint pmt_pos;
  PHLine ref;
  
  PHCentralTrack *d_cnt;
  CrkHit *d_crk;

  CrkGeometryObject *cgo;
  
  void GetNodes(PHCompositeNode *topNode); 
  void SetupCGO()
    {
      cgo = new CrkGeometryObject(); 
      cgo->UseSurvey(); // April 12 2006 
      // offline/packages/crk/CrkGeometryObject
      // offline/packages/CrkPID/CrkPID
      // offline/framework/preco/RingReco
      // use survey data in real data reconstruction
    }
};

#endif /* __RICHALIGNMENT_TRACK_H__ */
