//==================================================================
#ifndef _Pid_hh_
#define _Pid_hh_
//==================================================================

#include <TH1.h>
#include <TF1.h>
#include <TGraphErrors.h>
#include <TH2.h>
#include "Global.hh"
#include "Track.hh"
#include "Clust.hh"

#define  LIGHTSPEED 29.979
#define ELECTRON   0
#define POSITRON   1
#define PIONPLUS   2
#define PIONMINUS  3
#define KAONPLUS   4
#define KAONMINUS  5
#define PROTON     6
#define ANTIPROTON 7
#define NOPID      -2

class Pid: public TObject{
public:
  Pid();
  Pid(Global& glb,Track& trk,Clust& clt);
  ~Pid(){/*   */};

  int getid();
  int getid(Global& glb,Track& trk,Clust& clt);
  void set(Global& glb,Track& trk,Clust& clt);
  void reset();
public:
  float emc_pathl,emc_tof;
  float signedmom,beta;
  float invbeta,mass,mass2;
  float angle;

  int pid,epid;

public:
  
  ClassDef(Pid,1)
};
#endif


