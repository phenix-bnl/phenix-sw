#ifndef Clust_HH
#define Clust_HH

#include <TObject.h>
#include "nt_emc.hh"
#include "mdst_run2tree.hh"
#ifdef DST_READING
#include "Dst.hh"
#endif

class Clust : public TObject {
public:
  // EMCal Clustering information
  int arm,sector,twrhit,clustno,qa;
  float nsh,tof,e,ecent,ecore,ecorr,tofcorr,prob;
  float pos[3],disp[2],padisp[2];
  int ind[2];

  // For 2x2/4x4 trigger study(From DST only)
  float epart,e22,e44;

public:
  Clust();
  void Reset();
  Clust* operator=(nt_emc& r_nt_emc);
  Clust* Set(mdst_run2tree& mdst,int nemc);
#ifdef DST_READING
  Clust* Set(Dst* dst,int nemc);
#endif

  ClassDef(Clust,1)
};
//
#endif
//
