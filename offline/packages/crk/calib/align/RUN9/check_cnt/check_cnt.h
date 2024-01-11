#ifndef __check_cnt_H__
#define __check_cnt_H__
 
#include <string>
#include <iostream>
#include <fstream>
#include <stdlib.h>
 
//  General PHENIX tools
#include "Fun4AllServer.h"
#include "getClass.h"
#include "PHCompositeNode.h"
#include "phool.h"
#include "PHNodeIOManager.h"
#include "PHIODataNode.h"
#include "SubsysReco.h"
#include "Fun4AllHistoManager.h"

//  Data classes I am using in analysis
#include "PHCentralTrack.h"
#include "PHTrackOut.h"
#include "CglTrack.h"
#include "CrkHit.h"
#include "CrkGeometryObject.hh"

#include "PHGeometry.h"
#include "PHSphereSection.h"
#include "PHPoint.h"
#include "PHLine.h"

#include "TROOT.h"
#include "TTree.h"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"

using namespace PHGeometry;
using namespace std;
using namespace findNode;

class PHCompositeNode;
class PHCentralTrack;
class CrkHit;
class dCrkHitWrapper;

typedef PHIODataNode <PHCentralTrack> PHPNode_t;
typedef PHIODataNode <CrkHit> PHCNode_t;

class check_cnt : public SubsysReco
{
 public:
  
  check_cnt(const char* outfile = "output.root");
  virtual ~check_cnt() {};

  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

 protected:

  int ncalls;
  int n_event;

  int ntrk;
  int emcid;
  int pc1id;
  int pc2id;
  int pc3id;

  char hname[100];
  char htitle[100];
  char sname[100];

  short arm;
  short side;
  short sect;
  short quality;
  float pc1x;
  float pc1y;
  float pc1z;
  float pc1phi;
  int n0;
  int n1;
  float npe0;
  float npe1;
  float chi2;
  float chi2_npe0;
  float disp;
  int sn0;
  int sn1;
  float snpe0;
  float snpe1;
  float mom;
  float ecore;
  float ecore_mom;
  float pc2dz;
  float pc2dphi;
  float pc3dz;
  float pc3dphi;
  float pc2sdz;
  float pc2sdphi;
  float pc3sdz;
  float pc3sdphi;
  float emcdz_e;
  float emcdphi_e;
  float emcsdz_e;
  float emcsdphi_e;
  float spc2sdz;
  float spc2sdphi;
  float spc3sdz;
  float spc3sdphi;
  float semcsdz_e;
  float semcsdphi_e;
  float phi0;
  float the0;
  float zed;
  float phi;

  TH1F *hn0[4];
  TH1F *hn1[4];
  TH1F *hsn0[4];
  TH1F *hsn1[4];
  TH1F *hnpe0[4];
  TH1F *hnpe1[4];
  TH1F *hsnpe0[4];
  TH1F *hsnpe1[4];
  TH1F *hdisp[4];
  TH1F *hchi2[4];
  TH1F *hpc2dphi[4];
  TH1F *hpc2dz[4];
  TH1F *hpc2sdphi[4];
  TH1F *hpc2sdz[4];
  TH1F *hpc3dphi[4];
  TH1F *hpc3dz[4];
  TH1F *hpc3sdphi[4];
  TH1F *hpc3sdz[4];
  TH1F *hemcdphi[4];
  TH1F *hemcdz[4];
  TH1F *hemcsdphi[4];
  TH1F *hemcsdz[4];
  TH2F *hpc1hit;
  TFile *fout; 
  char OutFileName[100];

};

#endif /* __CHECK_CNT_H__ */
