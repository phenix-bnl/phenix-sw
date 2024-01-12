#ifndef __SVXDAQQA_H__
#define __SVXDAQQA_H__

#include "SubsysReco.h"

class PHCompositeNode;
class RunHeader;
class EventHeader;
class VtxOut;
class PHGlobal;
class svxAddress;

 
class TFile;
class TH1D;
class TH3D;
class TH2D;
class TH1F;
class TH2F;
class TH3F;
class TH2I;
class TProfile;

class SvxDaqQA: public SubsysReco {

public:

  SvxDaqQA(std::string &filename, int BGFlag, int agsegnum=9000);
  virtual ~SvxDaqQA() {}

  int  Init(PHCompositeNode *topNode);
  void init();
  int  process_event(PHCompositeNode *topNode);
  int  End(PHCompositeNode *topNode);

  int  Reset(PHCompositeNode *topNode) 		{return 0;}
  int  ResetEvent(PHCompositeNode *topNode) 	{return 0;}
  int  InitRun(PHCompositeNode *topNode);
  void Print(const std::string &what) const {}
  int  GetNodes(PHCompositeNode *topNode);

  enum {N_LAYER=4, N_LADDER=24, N_EW=2};

  
 private:

  //
  // Data nodes for CM and PHENIX
  RunHeader*       d_runhead;
  EventHeader*     d_eventhead;
  PHGlobal*        d_global;
  //PHCentralTrack*  d_trk;
  VtxOut*          d_vtxout;
  //PreviousEvent*   d_peve;
  //
  // Data node for VTX
  //SvxCentralTrackMap* d_svxcnt;
  //SvxHitMap*          d_svxhit; 
  //SvxTrackMap*        d_svxtrk;
  //svxDetectorGeo*     d_svxgeometry;
  svxAddress*         d_svxadr;



  // Output histogram file
  TFile*           d_OutputFile;
  std::string      d_OutputFileName;

  //
  // variables
  //
  int  d_runnumber;
  int  d_EventNumber;
  int  d_EventSeqNumber;

  private:

  TH1D *d_hzbbc;
  TH1D *d_hcentrality;
  TH1D *d_hzvtxs;
  TH1D *d_hzvtxp;
  TH1D *d_hbbcq;
  TH2D *d_h2zvtxpcent;  
  TH2D *d_h2zvtxpzbbc;  
  TH3D *d_h3xyvtxpcent; 
  TH1D *d_hzbbcnozvtxpcut;
  TH1D *d_hcentralitynozvtxcut;

  TH2I *d_hpixelbadpacket[60];
  TH2I *d_hstripbadpacket[40];

  TH2F *d_h2pmodbadpkt; // pixel module(60) vs evt
  TH2F *d_h2smodbadpkt; // pixel module(40) vs evt
};

#endif

