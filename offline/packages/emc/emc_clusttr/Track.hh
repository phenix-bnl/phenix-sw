#ifndef Track_HH
#define Track_HH

#include <Rtypes.h>
#include <TObject.h>
#include "nt_trk.hh"
#include "mdst_run2tree.hh"
#ifdef DST_READING
#include "Dst.hh"
#endif

class Track : public TObject {
public:
  // Tracking Information
  float inters,ptot,cglarm,quality;
  float proj[3],dir[3],pathl;
  float alpha,beta;

  // RICH information
  int crk_acc,crk_npmt0,crk_npmt1;
  float crk_npe0,crk_npe1,crk_chi2,crk_disp;

  // RICH information after Z-flip
  int crk_acc_s,crk_npmt0_s,crk_npmt1_s;
  float crk_npe0_s,crk_npe1_s,crk_chi2_s,crk_disp_s;

public:
  Track();
  void Reset();
  Track* operator=(nt_trk& r_nt_trk);
  Track* Set(mdst_run2tree& mdst, int ntrk);
#ifdef DST_READING
  Track* Set(Dst* dst,int ntrk);
#endif

  ClassDef(Track,1)
};
//
#endif
//
