#ifndef __Dst_cc
#define __Dst_cc
//==================================================================
//#include "mixPHTable.cc"
#include "Dst.hh"

//==================================================================
Dst::Dst(){
  _eventnum = 0;
  _max_eventnum = 0;
  _name = "";
  _title = "";
  _dstnode = 0;
  _type = PHReadOnly;
  _ioman = 0;
};

Dst::~Dst(){
  _tfile->Close();
  if( _dstnode ) delete _dstnode;
  if( _ioman ) delete _ioman;
};

Dst::Dst(char* name,char* title){
  _eventnum = 0;
  _max_eventnum = 0;
  _name = name;
  _title = title;
  _dstnode = new PHCompositeNode(name);
  _ioman = 0;
};
//==================================================================
bool Dst::Open(char* fname,const PHAccessType type){
  if( type == PHReadOnly ){
    TFile* f = new TFile(fname);
    if( f == 0 ){ cerr<<" Can't open the file "<<fname<<endl; return 0; }
    _max_eventnum = (int) ((TTree*)f->Get("T"))->GetEntries();
    f->Close();
  }
  _type = type;
  _ioman = new PHNodeIOManager(fname,_type);
  Init(_dstnode,_type);

  _tfile = (TFile*)(gROOT->GetListOfFiles()->FindObject(fname));
  _tree = (TTree*) _tfile->Get("T");
  cout<<" Dst::Open "<<fname<<" : tree Entries = "<<_tree->GetEntries()<<endl;
  bool status ;
  if( _tree!= 0 && _tfile != 0 )
    status = true;
  else
    status = false;
  return status;
}

Dst* Dst::Next(){
  if( _ioman == 0 ){ cerr<<" Error in Dst::Next() need Open() at first. "<<endl; return NULL; }
  if( _eventnum++ > -1 && _type == PHReadOnly ){
    if( _eventnum > _max_eventnum - 1 )
      return NULL;
    if( _ioman->read(_dstnode) == 0 ){ // End of File!!! Fix ME H.Torii
      cout<<"  Dst::Next() End Of File. "<<endl;
      return NULL;
    }
  }
  if(  _type == PHReadOnly ){
    int _readerr = getData();
  }
  return this;
};

Dst* Dst::First(){
  cout<<" Dst::First() "<<endl;
  if( _ioman == 0 ){ cerr<<" Error in Dst::Next() need Open() at first. "<<endl; return NULL; }
  _eventnum = -1;
  if(  _type == PHReadOnly ){
    int _readerr = getData();
    if( _readerr )
      cout<<" Dst::First() : Could not find "<<_readerr<<" data node"<<endl;
  }
  return this;
}

bool Dst::Reset(){
  if( _type == PHWrite ){
   _ioman->write(_dstnode);
   PHNodeIterator mainIter(_dstnode);
   PHNodeReset reset;
   mainIter.forEach(reset);
  }
  return kTRUE;
};
//==================================================================
bool Dst::Close(){
  delete _ioman;
  return kTRUE;
}
//==================================================================
void Dst::Print(const char* opt){
  cout<<" Dst::Print "<<opt<<"  "<<_name<<"    "<<_title<<"  :: "<<_tfile->GetName()<<endl;
  if( _dstnode )
    _dstnode->print(PHString("\t| "));
};
//==================================================================
bool Dst::AppendUnderNode(PHCompositeNode* topnode){
  topnode->addNode(_dstnode);
  return kTRUE;
}
//==================================================================
bool Dst::cd(const char* path){
  _tfile->cd();
}
//==================================================================
void Dst::Init(PHCompositeNode* dstNode,const PHAccessType type){
  if( type == PHReadOnly )
    Dst::InitRead(dstNode);
  if( type == PHWrite )
    Dst::InitWrite(dstNode);
};
//==================================================================
void Dst::InitWrite(PHCompositeNode* dstNode){
  int mr;
  mr =6000;
  dEmcClusterLocalExt = new dEmcClusterLocalExtWrapper("dEmcClusterLocalExt",mr);
  dEmcClusterLocalExtNode = new PHIODataNode<PHTable>(dEmcClusterLocalExt,"dEmcClusterLocalExt");
  dstNode->addNode(dEmcClusterLocalExtNode);

  mr=6000;
  dEmcClusterLocal = new dEmcClusterLocalWrapper("dEmcClusterLocal",mr);
  dEmcClusterLocalNode = new PHIODataNode<PHTable>(dEmcClusterLocal,"dEmcClusterLocal");
  dstNode->addNode(dEmcClusterLocalNode);

  mr=10000;
  dEmcCalibTower = new dEmcCalibTowerWrapper("dEmcCalibTower",mr);
  dEmcCalibTowerNode = new PHIODataNode<PHTable>(dEmcCalibTower,"dEmcCalibTower");
  dstNode->addNode(dEmcCalibTowerNode);
}
//==================================================================
void Dst::InitRead(PHCompositeNode* dstnode){
  _ioman->read(dstnode);
  _eventnum = -1;
  PHNodeIterator mainIter(dstnode);
  dBbcOutNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dBbcOut");
  dBbcRawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dBbcRaw");
  dZdcRawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dZdcRaw");
  dZdcOutNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dZdcOut");
  dMvddNdEtaOutNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dMvddNdEtaOut");
  dMvdMultOutNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dMvdMultOut");
  dMvdVertexOutNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dMvdVertexOut");
  dMvbRawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dMvbRaw");
  dMvcRawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dMvcRaw");
  dDchHitNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dDchHit");
  dDchTracksNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dDchTracks");
  dPc1RawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc1Raw");
  dPc2RawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc2Raw");
  dPc3RawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc3Raw");
  dPc1ClusterNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc1Cluster");
  dPc2ClusterNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc2Cluster");
  dPc3ClusterNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc3Cluster");
  dCrkRawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dCrkRaw");
  dCrkHitNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dCrkHit");
  dCrkPidNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dCrkPid");
  dTecTrackNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dTecTrack");
  dTecCalibNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dTecCalib");
  dTecPIDNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dTecPID");
  dTofRawNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dTofRaw");
  dTofReconstructedNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dTofReconstructed");
  dEmcCalibTowerNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcCalibTower");
  dEmcClusterLocalNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcClusterLocal");
  dEmcClusterLocalExtNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcClusterLocalExt");
  dCglTrackNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dCglTrack");
  dCglParticleNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dCglParticle");
  dCglPidNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dCglPid");
  dPHTrackNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPHTrack");
  dRunHeaderNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dRunHeader");
  dEventHeaderNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEventHeader");
  return;
};
//==================================================================
int Dst::getData(){
  return mountData();
}
//==================================================================
int Dst::mountData(){
    int _readerr = 0;
    if (!dBbcOutNode) {
      _readerr ++;
    } else {
      dBbcOut = (dBbcOutWrapper*)dBbcOutNode->getData();
    }
    if (!dBbcRawNode) {
      _readerr ++;
    } else {
      dBbcRaw = (dBbcRawWrapper*)dBbcRawNode->getData();
    }
    if (!dZdcRawNode) {
      _readerr ++;
    } else {
      dZdcRaw = (dZdcRawWrapper*)dZdcRawNode->getData();
    }
    if (!dZdcOutNode) {
      _readerr ++;
    } else {
      dZdcOut = (dZdcOutWrapper*)dZdcOutNode->getData();
    }
    if (!dMvddNdEtaOutNode) {
      _readerr ++;
    } else {
      dMvddNdEtaOut = (dMvddNdEtaOutWrapper*)dMvddNdEtaOutNode->getData();
    }
    if (!dMvdMultOutNode) {
      _readerr ++;
    } else {
      dMvdMultOut = (dMvdMultOutWrapper*)dMvdMultOutNode->getData();
    }
    if (!dMvbRawNode) {
      _readerr ++;
    } else {
      dMvbRaw = (dMvbRawWrapper*)dMvbRawNode->getData();
    }
    if (!dMvcRawNode) {
      _readerr ++;
    } else {
      dMvcRaw = (dMvcRawWrapper*)dMvcRawNode->getData();
    }
    if (!dMvdVertexOutNode) {
      _readerr ++;
    } else {
      dMvdVertexOut = (dMvdVertexOutWrapper*)dMvdVertexOutNode->getData();
    }
    if (!dDchHitNode) {
      _readerr ++;
    } else {
      dDchHit = (dDchHitWrapper*)dDchHitNode->getData();
    }
    if (!dDchTracksNode) {
      _readerr ++;
    } else {
      dDchTracks = (dDchTracksWrapper*)dDchTracksNode->getData();
    }
    if (!dPc1RawNode) {
      _readerr ++;
    } else {
      dPc1Raw = (dPadRawWrapper*)dPc1RawNode->getData();
    }
    if (!dPc2RawNode) {
      _readerr ++;
    } else {
      dPc2Raw = (dPadRawWrapper*)dPc2RawNode->getData();
    }
    if (!dPc3RawNode) {
      _readerr ++;
    } else {
      dPc3Raw = (dPadRawWrapper*)dPc3RawNode->getData();
    }
    if (!dPc1ClusterNode) {
      _readerr ++;
    } else {
      dPc1Cluster = (dPadClusterWrapper*)dPc1ClusterNode->getData();
    }
    if (!dPc2ClusterNode) {
      _readerr ++;
    } else {
      dPc2Cluster = (dPadClusterWrapper*)dPc2ClusterNode->getData();
    }
    if (!dPc3ClusterNode) {
      _readerr ++;
    } else {
      dPc3Cluster = (dPadClusterWrapper*)dPc3ClusterNode->getData();
    }
    if (!dCrkRawNode) {
      _readerr ++;
    } else {
      dCrkRaw = (dCrkRawWrapper*)dCrkRawNode->getData();
    }
    if (!dCrkHitNode) {
      _readerr ++;
    } else {
      dCrkHit = (dCrkHitWrapper*)dCrkHitNode->getData();
    }
    if (!dCrkPidNode) {
      _readerr ++;
    } else {
      dCrkPid = (dCrkPidWrapper*)dCrkPidNode->getData();
    }
    if (!dTecTrackNode) {
      _readerr ++;
    } else {
      dTecTrack = (dTecTrackWrapper*)dTecTrackNode->getData();
    }
    if (!dTecCalibNode) {
      _readerr ++;
    } else {
      dTecCalib = (dTecCalibWrapper*)dTecCalibNode->getData();
    }
    if (!dTecPIDNode) {
      _readerr ++;
    } else {
      dTecPID = (dTecPIDWrapper*)dTecPIDNode->getData();
    }
    if (!dTofRawNode) {
      _readerr ++;
    } else {
      dTofRaw = (dTofRawWrapper*)dTofRawNode->getData();
    }
    if (!dTofReconstructedNode) {
      _readerr ++;
    } else {
      dTofReconstructed = (dTofReconstructedWrapper*)dTofReconstructedNode->getData();
    }
    if (!dEmcCalibTowerNode) {
      _readerr ++;
    } else {
      dEmcCalibTower = (dEmcCalibTowerWrapper*)dEmcCalibTowerNode->getData();
    }
    if (!dEmcClusterLocalNode) {
      _readerr ++;
    } else {
      dEmcClusterLocal = (dEmcClusterLocalWrapper*)dEmcClusterLocalNode->getData();
    }
    if (!dEmcClusterLocalExtNode) {
      _readerr ++;
    } else {
      dEmcClusterLocalExt = (dEmcClusterLocalExtWrapper*)dEmcClusterLocalExtNode->getData();
    }
    if (!dCglTrackNode) {
      _readerr ++;
    } else {
      dCglTrack = (dCglTrackWrapper*)dCglTrackNode->getData();
    }
    if (!dCglParticleNode) {
      _readerr ++;
    } else {
      dCglParticle = (dCglParticleWrapper*)dCglParticleNode->getData();
    }
    if (!dCglPidNode) {
      _readerr ++;
    } else {
      dCglPid = (dCglPidWrapper*)dCglPidNode->getData();
    }
    if (!dPHTrackNode) {
      _readerr ++;
    } else {
      dPHTrack = (dPHTrackWrapper*)dPHTrackNode->getData();
    }
    if (!dRunHeaderNode) {
      _readerr ++;
    } else {
      dRunHeader = (dRunHeaderWrapper*)dRunHeaderNode->getData();
    }
    if (!dEventHeaderNode) {
      _readerr ++;
    } else {
      dEventHeader = (dEventHeaderWrapper*)dEventHeaderNode->getData();
    }
    return _readerr;
};

//==================================================================
//==================================================================
#endif
//==================================================================
//==================================================================

//==================================================================
//==================================================================
//==================================================================
#ifdef TESTESTESTESTESTESTST
//#include "/afs/rhic/phenix/software/new/include/PHIOManager.h"
//#include "/afs/rhic/phenix/software/new/include/PHNodeIOManager.h"
class PHNodeIOManagerWrapper : PHNodeIOManager {
public:
  PHNodeIOManagerWrapper(): PHNodeIOManager() { };
  PHNodeIOManagerWrapper(const PHString& s, const PHAccessType t= PHReadOnly)
    :PHNodeIOManager(s,t){ };
  PHNodeIOManagerWrapper(const PHString& s,const PHString& n, const PHAccessType t= PHReadOnly)
    :PHNodeIOManager(s,n,t){ };
  ~PHNodeIOManagerWrapper(){ closeFile(); };
public:
  virtual void closeFile() { PHNodeIOManager::closeFile(); };
  virtual PHBoolean write(PHCompositeNode * n){ return PHNodeIOManager::write(n); };
  virtual void print() const{ PHNodeIOManager::print(); };
  void Reset();
};
void PHNodeIOManagerWrapper::Reset(){
  this->eventNumber = 0;
};
#endif
//==================================================================
//==================================================================
