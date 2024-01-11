#ifndef Evt_HH
#define Evt_HH

#include <Rtypes.h>
#include <TObject.h>
#include <vector>
#include "Global.hh"
#include "Track.hh"
#include "Clust.hh"
#include "Ass.hh"
#include "nt_emc.hh"
#include "nt_trk.hh"
#include "nt_evt.hh"
#ifdef CLASSMICROEVENT_READING
#include "classMicroEvent.hh"
#endif
#ifdef DST_READING
#include "Dst.hh"
#endif

#define NUMOBJMAX 1000

class Evt : public TObject {
public:
  Global _glb;
  vector<Clust> _vec_clt;
  vector<Track> _vec_trk;
  vector<Ass> _vec_assclt;
  vector<Ass> _vec_asstrk;
  vector<Ass> _vec_assclt_s;
  vector<Ass> _vec_asstrk_s;
  int _current_run;
  int _current_seq;
  int _current_evn;
  float swapz(float inz);

private:
#ifndef __CINT__
  enum kMode { kMode_none, kMode_run1udst, kMode_run2tree, kMode_microDST, kMode_DST };
  kMode _mode;
  int _current_evtnum;
  // --- for run1udst
#ifdef CLASSMICROEVENT_READING
  classMicroEvent* _pt_microevent;
#endif
  TTree* _mdst_evt;
  TTree* _mdst_trk;
  TTree* _mdst_emc;
  int _current_emcnum;
  int _current_trknum;
  nt_evt* _pt_nt_evt;
  nt_emc* _pt_nt_emc;
  nt_trk* _pt_nt_trk;
  //
#ifdef DST_READING
  // --- for run1udst
  Dst* _dst;
#endif
  //
  // --- for run2tree
  TTree* _mdst_run2tree;
  mdst_run2tree* _pt_run2tree;

#endif  
  int MakeHealthClust();
  int MakeHealthTrack();

public:
  Evt();
  ~Evt();
  Evt(const Evt& evt);
  Evt& operator=(const Evt& evt);
  int Init_run1udst(TTree* in_nt_evt,TTree* in_nt_trk,TTree* in_nt_emc);
  int Init_run2tree(TTree* mdst);
  int Init_DST(TTree* mdst);
  int Reset();
  int Next();
  int Next_run1udst();
  int Next_run2tree();
  int Next_DST();
  int MakeAss();
  bool IsLastEvt();
  int GetRunNum(){return _current_run; };
  int GetSeqNum(){return _current_seq; };
  int GetEvtNum(){return _current_evn; };
  int GetEntriesNum(){return _current_evtnum; };
  int GetN(){return _current_evtnum; };
  int GetEntries();
  //
  int GetAssTrk(int cltid,int num);
  int GetAssClt(int trkid,int num);
  
  void Print();
  void Print_run1udst();
  void Print_run2tree();

  ClassDef(Evt,1)
};
//
#endif
//
