#ifndef Global_HH
#define Global_HH

#include <Rtypes.h>
#include <TObject.h>
#include "nt_trk.hh"
#include "nt_emc.hh"
#include "nt_evt.hh"
#include "mdst_run2tree.hh"
#ifdef CLASSMICROEVENT_READING
#include "classMicroEvent.hh"
#endif
#ifdef MDST_MICRODST_READING
#include "mdst_microDST.hh"
#endif
#ifdef DST_READING
#include "Dst.hh"
#endif

class Global : public TObject {
public:
  // Global Information
  int run,seq,evn;
  unsigned int trig;
  float zdcz,zdct0,zdcch;
  float bbcz,bbct0,bbcch;
  int bbcnhit;
  int pc1nhit;
  int emcnhit;
  int emcnhitw;
  float etotw,etote;
  int centbin;
  float ncoll,npart;

public:
  Global();
  void Reset();
  Global* operator=(nt_evt& r_nt_evt);
  Global* operator=(mdst_run2tree& mdst);
#ifdef CLASSMICROEVENT_READING
  Global* operator=(classMicroEvent& r_microevent);
#endif
#ifdef MDST_MICRODST_READING
  Global* operator=(mdst_microDST& mdst);
#endif
#ifdef DST_READING
  Global* Set(Dst* dst);
#endif

  ClassDef(Global,2)
};
//
#endif
//
