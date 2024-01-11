#ifndef Hist_ass_HH
#define Hist_ass_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TDirectory.h>
#include <TCanvas.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include "Hist.hh"
#include "fitpeak.hh"
#include "emcRejectClust.hh"

class Hist_ass : public Hist {
public:
  // --------- Association
  TH1F* h_pdz_sect[8];                  //[8]
  TH1F* h_pdy_sect[8];                  //[8]
  TH2F* h2_pdzy_sect[8];                //[8]
  TH2F* h2_pdzmom_sectpid[8][8];        //[8][8]
  TH2F* h2_pdymom_sectpid[8][8];        //[8][8]
  TH2F* h2_pdzmom_ang_sectpid[8][8];    //[8][8]
  TH2F* h2_pdymom_ang_sectpid[8][8];    //[8][8]
  TH2F* h2_pdzmom_perp_sectpid[8][8];   //[8][8]
  TH2F* h2_pdymom_perp_sectpid[8][8];   //[8][8]
  TH2F* h2_posproj_sect[8];             //[8]

  // --------- Particle ID
  TH2F* h2_mmom_sect[8];                //[8]
  TH2F* h2_m2mom_sect[8];               //[8]

public:
  Hist_ass();
  Hist_ass(char* name,char* title,char* opt="");
  //
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.);
  virtual void Draw(TCanvas* c1,TPostScript* ps);

  ClassDef(Hist_ass,1)
};
//
#endif
//
