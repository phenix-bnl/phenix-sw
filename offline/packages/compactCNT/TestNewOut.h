#ifndef __TestNewOut_H__
#define __TestNewOut_H__

#include <SubsysReco.h>
#include <PHLine.h>
#include <fstream>

class TH1F;
class TFile;

class TestNewOut: public SubsysReco
{
 public:
  TestNewOut(const std::string &name = "FILLCRKHITS");
  virtual ~TestNewOut() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void SetHistPath(const std::string &path);

  // RICH

  TH1F *hn0;
  TH1F *hn1;
  TH1F *hn2;
  TH1F *hn3;
  TH1F *hnpe0;
  TH1F *hnpe1;
  TH1F *hnpe2;
  TH1F *hnpe3;
  TH1F *hchi2;
  TH1F *hdisp;
  TH1F *hcross_phi;
  TH1F *hcenter_phi;
  TH1F *hcross_z;
  TH1F *hcenter_z;

  TH1F *hsn0;
  TH1F *hsn1;
  TH1F *hsn2;
  TH1F *hsn3;
  TH1F *hsnpe0;
  TH1F *hsnpe1;
  TH1F *hsnpe2;
  TH1F *hsnpe3;
  TH1F *hschi2;
  TH1F *hsdisp;

  // TEC

  TH1F *htc;
  TH1F *htnt;
  TH1F *htavgt;
  TH1F *htdphi;
  TH1F *htnplane;

  TH1F *hstc;
  TH1F *hstnt;
  TH1F *hstavgt;
  TH1F *hstdphi;
  TH1F *hstnplane;

  // Aerogel

  TH1F *hid;
  TH1F *ha1;
  TH1F *ha2;

  TH1F *hsid;
  TH1F *hsa1;
  TH1F *hsa2;

  TH1F *hhbdadcch;
  TH1F *hhbdcharge;
  TH1F *hhbdentries;

  TH1F *hecore;
  TH1F *hmom;
  TH1F *heoverp;

  TH1F *hnx1;
  TH1F *hnx2;
  TH1F *hqual;
  TH1F *hzed;
  TH1F *hphi;
  TH1F *hbeta;

  TH1F *hzvert;
  TH1F *hpc1hits;

  TH1F *hpc1assoc;

  TH1F *hemcpc3dphi;
  TH1F *hemcpc3dz;
  TH1F *hemcpc3neartrk;
  TH1F *hemctrk;
  TH1F *hemctrkdz;
  TH1F *hemctrkdphi;
  TH1F *hpemctrk;
  TH1F *hemctrkquality;

  TFile *histout;
  std::string histoutpath;

  int compactCNT;

 protected:

  std::ofstream dumpfile;
  int nevts;
  int got_lvl1_names;
  float Lvl1Scaled[32];

  const char *Lvl1Name[32];

};

#endif
