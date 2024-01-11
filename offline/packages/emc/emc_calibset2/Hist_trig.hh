#ifndef Hist_trig_HH
#define Hist_trig_HH

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

class Hist_trig : public Hist {
public:
  // MIP Monitor Sector-by-Sector
  TH1F* h_e_sect[8];                       //[1]
  TH2F* h2_emom_sect[8];                   //[1]
  TH2F* h2_e22mom_sect[8];                 //[1]
  TH2F* h2_e44mom_sect[8];                 //[1]
  TH2F* h2_epartmom_sect[8];               //[1]

public:
  Hist_trig();
  Hist_trig(char* pname,char* ptitle,char* opt="");
  //
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid);

  ClassDef(Hist_trig,1)
};
//
#endif
//
