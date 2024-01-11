#ifndef Hist_miptwr_HH
#define Hist_miptwr_HH

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

class Hist_miptwr : public Hist {
public:

  TH1F* h_sec_sect[8];               //[8] Sector: 0.4-2.0GeV/c
  CalibRunsTH1* h_sm_sect[8];        //! SM    : 0.4-2.0GeV/c
  CalibRunsTH1* h_twr_sect[8];       //! Tower : 0.4-2.0GeV/c

  TH1F* h_secpid_sect[8];            //[8] Sector: 0.6-1.0GeV/c, low-multi, pi+-
  CalibRunsTH1* h_smpid_sect[8];     //! SM    : 0.6-1.0GeV/c, low-multi, pi+-
  //CalibRunsTH1* h_twrpid_sect[8];    //[8] Tower : 0.6-1.0GeV/c, low-multi, pi+-

  TH1F* h_secpid10_sect[8];          //[8] Sector: 0.9-1.1GeV/c, low-multi, pi+-

  //  CalibRunsTH2* h_etof_sect[8];      //[8] Sector: 0.6-1.0GeV/c, low-multi
  //  CalibRunsTH2* h_etof_sect[8];      //[8] Sector: 0.9-1.1GeV/c, low-multi

public:
  Hist_miptwr();
  Hist_miptwr(char* pname,char* ptitle,char* opt="");
  //
  virtual bool Add(TFile* f);
  virtual int Write(const char* name = 0, Int_t option = 0, Int_t bufsize = 0);
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.);

  ClassDef(Hist_miptwr,1)
};
//
#endif
//
