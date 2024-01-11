#ifndef Hist_ele_HH
#define Hist_ele_HH

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

class Hist_ele : public Hist {
public:
  // RICH Electron Calibration
  TH1F* h_ep_sect[8];                //[8]
  TH2F* h2_ep_sect[8];               //[8]
  TH2F* h2_ecorrp_sect[8];           //[8]
  TH2F* h2_ecorep_sect[8];           //[8]
  TH2F* h2_ecorepX12_sect[8];        //[8]
  //
  TH2F* h2_ecorrp_sectmul[8][6];     //[8][6]
  TH2F* h2_ecorrpX12_sectmul[8][6];  //[8][6]
  TH2F* h2_ecorep_sectmul[8][6];     //[8][6]
  TH2F* h2_ecorepX12_sectmul[8][6];  //[8][6]

  // Particle ID by RICH + EMC electron candidate
  TH2F* h2_pm_sectprob[8][2];        //[8][2]
  TH2F* h2_pm2_sectprob[8][2];       //[8][2]

  // RICH + EMC Electron Calibration // mass<0.1
  TH1F* h_epemce_sect[8];            //[8]
  TH2F* h2_epemce_sect[8];           //[8]
  TH2F* h2_ecorrpemce_sect[8];       //[8]
  TH2F* h2_ecorepemce_sect[8];       //[8]
  TH2F* h2_ecorepemceX12_sect[8];    //[8]
  //
  TH2F* h2_ecorrpemce_sectmul[8][6];    //[8][6]
  TH2F* h2_ecorrpemceX12_sectmul[8][6]; //[8][6]
  TH2F* h2_ecorepemce_sectmul[8][6];    //[8][6]
  TH2F* h2_ecorepemceX12_sectmul[8][6]; //[8][6]

public:
  Hist_ele();
  Hist_ele(char* pname,char* ptitle,char* opt="");
  //
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.);

  ClassDef(Hist_ele,1)
};
//
#endif
//
