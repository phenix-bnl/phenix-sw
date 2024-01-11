#ifndef __check_crk_H__
#define __check_crk_H__
 
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

using namespace PHGeometry;
using namespace std;
using namespace findNode;

class PHCompositeNode;
class PHCentralTrack;
class CrkHit;
class dCrkHitWrapper;

typedef PHIODataNode <PHCentralTrack> PHPNode_t;
typedef PHIODataNode <CrkHit> PHCNode_t;

class check_crk : public SubsysReco
{
 public:
  
  check_crk(const char* outfile = "crk_output.root");
  virtual ~check_crk() {};

  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

 protected:

  TTree *tree ;
  TH1F *hist_WN;
  TH1F *hist_WS;
  TH1F *hist_EN;
  TH1F *hist_ES;
  TFile *fout ; 
  char OutFileName[100];

  int npmt;
  short pmt[5120];
  float npe[5120];

  short PMT;
  float NPE;

  //  CrkHit *d_crk;

  //  void GetNodes(PHCompositeNode *topNode); 

};

#endif /* __CHECK_CRK_H__ */
