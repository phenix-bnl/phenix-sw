#ifndef __Dst_hh
#define __Dst_hh
//=============================================================================

#include <stdio.h>
#include <stream.h>
#include <Rtypes.h>
#include <TObject.h>
#include <TROOT.h>
#include <TNamed.h>
#include <TTree.h>
#include <TFile.h>
#include <TDirectory.h>

#include <PHTable.hh>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeReset.h>

#include <dBbcOutWrapper.h>
#include <dBbcRawWrapper.h>
#include <dZdcOutWrapper.h>
#include <dZdcRawWrapper.h>
#include <dMvddNdEtaOutWrapper.h>
#include <dMvdMultOutWrapper.h>
#include <dMvbRawWrapper.h>
#include <dMvcRawWrapper.h>
#include <dMvdVertexOutWrapper.h>
#include <dDchHitWrapper.h>
#include <dDchTracksWrapper.h>
#include <dPadRawWrapper.h>
#include <dPadClusterWrapper.h>
#include <dCrkRawWrapper.h>
#include <dCrkHitWrapper.h>
#include <dCrkPidWrapper.h>
#include <dTecTrackWrapper.h>
#include <dTecCalibWrapper.h>
#include <dTecPIDWrapper.h>
#include <dTofRawWrapper.h>
#include <dTofReconstructedWrapper.h>
#include <dEmcCalibTowerWrapper.h>
#include <dEmcClusterLocalWrapper.h>
#include <dEmcClusterLocalExtWrapper.h>
#include <dCglParticleWrapper.h>
#include <dCglTrackWrapper.h>
#include <dCglPidWrapper.h>
#include <dPHTrackWrapper.h>
#include <dRunHeaderWrapper.h>
#include <dEventHeaderWrapper.h>
#include <fkinWrapper.h>

class Dst {
protected:
  int _eventnum;
  int _max_eventnum;
  PHString _name,_title;
  PHCompositeNode* _dstnode;
  PHAccessType _type;
  PHNodeIOManager* _ioman;
  TFile* _tfile;
  TTree* _tree;
public:
  Dst();
  ~Dst();
  Dst(char* name,char* title);
  // -- I/O of DST
  bool Open(char* fname,const PHAccessType type=PHReadOnly);
  Dst* Next();
  Dst* First(); // This is dummy!!! don't use !
  bool Reset();
  bool Close();
  bool AppendUnderNode(PHCompositeNode* topnode);
  bool cd(const char* path = 0);
  void Print(const char* opt = 0);
  int GetEvnum(){ return _eventnum; };
  int GetEntries(){ return _max_eventnum; };
protected:
  virtual void Init(PHCompositeNode*,const PHAccessType);
  void InitRead(PHCompositeNode*);
  void InitWrite(PHCompositeNode*);
  virtual int getData();
  int mountData();
public:
  dBbcOutWrapper* dBbcOut;
  dBbcRawWrapper* dBbcRaw;
  dZdcRawWrapper* dZdcRaw;
  dZdcOutWrapper* dZdcOut;
  dMvddNdEtaOutWrapper* dMvddNdEtaOut;
  dMvdMultOutWrapper* dMvdMultOut;
  dMvbRawWrapper* dMvbRaw;
  dMvcRawWrapper* dMvcRaw;
  dMvdVertexOutWrapper* dMvdVertexOut;
  dDchHitWrapper* dDchHit;
  dDchTracksWrapper* dDchTracks;
  dPadRawWrapper* dPc1Raw;
  dPadRawWrapper* dPc2Raw;
  dPadRawWrapper* dPc3Raw;
  dPadClusterWrapper* dPc1Cluster;
  dPadClusterWrapper* dPc2Cluster;
  dPadClusterWrapper* dPc3Cluster;
  dCrkRawWrapper* dCrkRaw;
  dCrkHitWrapper* dCrkHit;
  dCrkPidWrapper* dCrkPid;
  dTecTrackWrapper* dTecTrack;
  dTecCalibWrapper* dTecCalib;
  dTecPIDWrapper* dTecPID;
  dTofRawWrapper* dTofRaw;
  dTofReconstructedWrapper* dTofReconstructed;
  dEmcCalibTowerWrapper* dEmcCalibTower;
  dEmcClusterLocalWrapper* dEmcClusterLocal;
  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt;
  dCglTrackWrapper* dCglTrack;
  dCglParticleWrapper* dCglParticle;
  dCglPidWrapper* dCglPid;
  dPHTrackWrapper *dPHTrack;
  dRunHeaderWrapper* dRunHeader;
  dEventHeaderWrapper* dEventHeader;
protected:
  PHIODataNode<PHTable>* dBbcOutNode;
  PHIODataNode<PHTable>* dBbcRawNode;
  PHIODataNode<PHTable>* dZdcRawNode;
  PHIODataNode<PHTable>* dZdcOutNode;
  PHIODataNode<PHTable>* dMvddNdEtaOutNode;
  PHIODataNode<PHTable>* dMvdMultOutNode;
  PHIODataNode<PHTable>* dMvdVertexOutNode;
  PHIODataNode<PHTable>* dMvbRawNode;
  PHIODataNode<PHTable>* dMvcRawNode;
  PHIODataNode<PHTable>* dDchHitNode;
  PHIODataNode<PHTable>* dDchTracksNode;
  PHIODataNode<PHTable>* dPc1RawNode;
  PHIODataNode<PHTable>* dPc2RawNode;
  PHIODataNode<PHTable>* dPc3RawNode;
  PHIODataNode<PHTable>* dPc1ClusterNode;
  PHIODataNode<PHTable>* dPc2ClusterNode;
  PHIODataNode<PHTable>* dPc3ClusterNode;
  PHIODataNode<PHTable>* dCrkRawNode;
  PHIODataNode<PHTable>* dCrkHitNode;
  PHIODataNode<PHTable>* dCrkPidNode;
  PHIODataNode<PHTable>* dTecTrackNode;
  PHIODataNode<PHTable>* dTecCalibNode;
  PHIODataNode<PHTable>* dTecPIDNode;
  PHIODataNode<PHTable>* dTofRawNode;
  PHIODataNode<PHTable>* dTofReconstructedNode;
  PHIODataNode<PHTable>* dEmcCalibTowerNode;
  PHIODataNode<PHTable>* dEmcClusterLocalNode;
  PHIODataNode<PHTable>* dEmcClusterLocalExtNode;
  PHIODataNode<PHTable>* dCglTrackNode;
  PHIODataNode<PHTable>* dCglParticleNode;
  PHIODataNode<PHTable>* dCglPidNode;
  PHIODataNode<PHTable>* dPHTrackNode;
  PHIODataNode<PHTable>* dRunHeaderNode;
  PHIODataNode<PHTable>* dEventHeaderNode;

};
//=============================================================================
#endif
//=============================================================================

//=============================================================================
//=============================================================================
//=============================================================================
#ifdef TESTESTESTSETESTEST
{
  // My Libraty!!!
  gROOT->LoadMacro("mixDst.cc");
  gROOT->LoadMacro("mixPHTable.cc");
  gROOT->LoadMacro("mixEmcCalibTower.cc");
  gROOT->LoadMacro("mixEmcClusterLocal.cc");
  gROOT->LoadMacro("mixDstDraw.cc");
}
#endif
//=============================================================================
//=============================================================================
//=============================================================================

