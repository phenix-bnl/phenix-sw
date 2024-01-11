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
#include "PHGlobal.h"
#include "PHTrackOut.h"
#include "CglTrack.h"
#include "CrkHit.h"
#include "CrkGeometryObject.hh"

#include "SyncObject.h"

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
class PHGlobal;
class SyncObject;
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
  int run;
  int ntrk;
  int arm;
  int side;
  int side_swap;
  float cross_z;
  float cross_phi;
  float npe0;
  float mom;
  float ecore;
  float center_phi;
  float center_z;
  float pc2dphi;
  float pc2dz;
  float pc3dphi;
  float pc3dz;
  float emcdphi;
  float emcdz;
  float alpha;
  float beta;
  float zed;
  float bbcz;
  float phi;
  int flag;
  int charge;
  int n0;
  int sn0;
  float chi2;
  float disp;
  float bbcq;
  float zdce;
  
  int panel;
  int panel_swap;
  double path;
  double path_swap;
  
  TTree *tree ;
  TFile *fout ; 
  char OutFileName[100];
  
  float v_ref[3];
  float b_ref[3];
  float sv_ref[3];
  float sb_ref[3];
  float ppc1pos[3];
  float ppc2pos[3];
  float ppc3pos[3];
  float pemcpos[3];

  int npmt;
  short pmt[1000];
  float npe[1000];
  float tcrk[1000];
  float posx[1000];
  float posy[1000];
  float posz[1000];
  float posr[1000];
  float posphi[1000];
  float start[3];
  float end[3];
  
  int snpmt;
  short spmt[1000];
  float snpe[1000];
  float stcrk[1000];
  float sposx[1000];
  float sposy[1000];
  float sposz[1000];
  float sposr[1000];
  float sposphi[1000];
  

  PHPoint cross_to_crk;
  PHPoint cross_to_crk_swap;
  PHPoint pstart, pend;
  PHPoint pmt_pos;
  PHLine ref;
  PHLine ref_swap;
  
  //  PHCentralTrack *d_cnt;
  //  CrkHit *d_crk;

  CrkGeometryObject *cgo;
  


  //  void GetNodes(PHCompositeNode *topNode); 
  void SetupCGO()
    {
      cgo = new CrkGeometryObject(); 
      cgo->UseSurvey(); // April 12 2006 
      // offline/packages/crk/CrkGeometryObject
      // offline/packages/CrkPID/CrkPID
      // offline/framework/preco/RingReco
      // use survey data in real data reconstruction
    }
  float f_dphi[2][2][24];
  float f_dz[2][2][24];
  PHLine ReflectInZ(const PHLine &trk);


};

#endif /* __RICHALIGNMENT_TRACK_H__ */
