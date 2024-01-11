#ifndef EvtPair_HH
#define EvtPair_HH

#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <Rtypes.h>
#include <TObject.h>
#include <TSystem.h>
#include <TTree.h>
#include <TTreeFormula.h>
#include <vector.h>
#include "Evt.hh"
#include "ClustPair.hh"

class EvtPair : public TObject {
public:
  Global _glb0;
  Global _glb1;

  // Pair Information
  int _vec_size;
  int _vec_capacity;
  vector<ClustPair> _vec_cltpair;

public:
  EvtPair(int maxsize=20000);
  ~EvtPair();
  void Reset();
  void Fill(Evt& ev0,Evt& ev1,char* clust_selection="");
  int SetPtThreshold(float pt){ _pt_threshold = pt; };
  float GetPtThrshold(){return _pt_threshold; };
  int SetAsymCut(float asym){ _asym_cut = asym; };
  float GetAsymCut(){return _asym_cut; };

private:
  float _pt_threshold;
  float _asym_cut;
  long _time_all;
  long _time_calc;
  long _time_input;

  ClassDef(EvtPair,1)
};
//
#endif
//
