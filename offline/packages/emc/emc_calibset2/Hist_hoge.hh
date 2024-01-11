#ifndef Hist_hoge_HH
#define Hist_hoge_HH

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

class Hist_hoge : public Hist {
public:
  TH1F* h_sect[8];      //[8]
  TH2F* h2_sect[8];     //[8]
public:
  Hist_hoge();
  ~Hist_hoge();
  Hist_hoge(char* pname,char* ptitle,char* opt="");
  //
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid);

  ClassDef(Hist_hoge,1)
};
//
#endif
//
