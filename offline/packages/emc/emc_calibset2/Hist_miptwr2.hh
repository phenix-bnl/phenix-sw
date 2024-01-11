#ifndef Hist_miptwr2_HH
#define Hist_miptwr2_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TDirectory.h>
#include <TCanvas.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include "Hist.hh"
#include "CalibRunsTH1.hh"
#include "CalibRunsTH2.hh"

class Hist_miptwr2 : public Hist {
public:

  TH1F* h_sec_sect[6];               //[8] Sector: 0.4-2.0GeV/c
  CalibRunsTH1* h_sm_sect[6];        //! SM    : 0.4-2.0GeV/c
  CalibRunsTH1* h_twr_sect[6];       //! Tower : 0.4-2.0GeV/c

  CalibRunsTH2* h_sm_etof0_sect[6];   //! SM: 0.6-1.0GeV/c, low-multi
  CalibRunsTH2* h_sm_etof1_sect[6];   //! SM: 0.9-1.1GeV/c, low-multi
  CalibRunsTH2* h_twr_etof0_sect[6];  //! Tower: 0.6-1.0GeV/c, low-multi
#ifdef FINECALIB
  CalibRunsTH2* h_twr_etof1_sect[6];  //! Tower: 0.9-1.1GeV/c, low-multi
#endif

public:
  Hist_miptwr2();
  Hist_miptwr2(char* pname,char* ptitle,char* opt="");
  //
  virtual bool Add(TFile* f);
  virtual int Write(const char* name = 0, Int_t option = 0, Int_t bufsize = 0);
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.);

  ClassDef(Hist_miptwr2,1)
};
//
#endif
//
