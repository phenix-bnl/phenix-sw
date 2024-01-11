#ifndef Hist_mip_HH
#define Hist_mip_HH

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

class Hist_mip : public Hist {
public:
  // MIP Monitor Sector-by-Sector
  TH1F* h_e_sect[8];                       //[8]
  TH2F* h2_emom_sect[8];                   //[8]
  TH2F* h2_eangmom_sect[8];                //[8]
  TH2F* h2_eangmom_sectpid[8][8];          //[8][8]
  TH2F* h2_eangmom_lowmul_sectpid[8][8];   //[8][8]
  TH2F* h2_eangmom_sectmul[8][6];          //[8][6]

  TH2F* h2_eangsm_sect[8];                 //[8]  // pi 0.6 - 1.0GeV/c , multiplicity < 100
  TH2F* h2_ecentesm_sect[8];               //[8]
  TH2F* h2_angsm_sect[8];                  //[8]

public:
  Hist_mip();
  Hist_mip(char* pname,char* ptitle,char* opt="");
  //
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.);

  ClassDef(Hist_mip,1)
};
//
#endif
//
