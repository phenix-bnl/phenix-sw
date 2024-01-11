#ifndef Hist_eqa_HH
#define Hist_eqa_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TDirectory.h>
#include <TCanvas.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include "Hist.hh"
#include "Pid.hh"
#include "emcRejectClust.hh"
#include "CalibRunsTH1.hh"
#include "CalibRunsTH0.hh"

class Hist_eqa : public Hist {
public:

  TH1F* h_e_sect[8];                 //[8] Sector
  CalibRunsTH0* ene_sect[8];      //! Tower
  CalibRunsTH0* evn_sect[8];      //! Tower

public:
  Hist_eqa();
  Hist_eqa(char* pname,char* ptitle,char* opt="");
  //
  virtual bool Add(TFile* f);
  virtual int Write(const char* name = 0, Int_t option = 0, Int_t bufsize = 0);
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float=1.);

  ClassDef(Hist_eqa,1)
};
//
#endif
//
