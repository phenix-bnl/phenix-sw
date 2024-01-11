#ifndef __MUTRGUNPACK_V1__
#define __MUTRGUNPACK_V1__

#include "MutrgUnpack.hh"

#include <TFile.h>
#include <TH2.h>
#include <TGraphAsymmErrors.h>

class TMutHitMap;

class MutrgUnpack_v1 : public MutrgUnpack{
public:
  MutrgUnpack_v1(bool init_flag=true);
  virtual ~MutrgUnpack_v1(void);

  void CreateObject(void);
  int Init(void);
  int End(void);

  int Unpack(Event *evt);

  int Associate(TMutHitMap *mut_hitmap);

  TFile *fpout;
  TH2 *hmrg_hit[3]; // station
};

#endif /* __MUTRGUNPACK_V1_ */
