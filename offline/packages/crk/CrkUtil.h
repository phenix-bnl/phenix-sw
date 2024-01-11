#ifndef CRKUTILMODULE_H_INCLUDED
#define CRKUTILMODULE_H_INCLUDED
//
// Utility classes for RICH and electron ID analysis.
//
#include <vector>
class PHCompositeNode;
class TH1F;
class TH2F;

class Browser {
public:
  Browser(PHCompositeNode *top):ftop(top){}
  void browse(const char *nodename);
private:
  PHCompositeNode *ftop;
};



class CrkHist {
public:
  CrkHist();
  virtual ~CrkHist();
  void fill(PHCompositeNode *top);
  void write(const char *rootfile="crkhist.root");

private:
  TH1F *h1nhit;
  TH1F *h1npmt;
  TH1F *h1chi2;

  TH1F *h1dc_zvtx;
  TH1F *h1dc_r;
  TH1F *h1dc_z;
  TH1F *h1dc_ul;

  TH1F *h1cgv_zvtx;
  TH1F *h1cgv_r;
  TH1F *h1cgv_z;
  TH1F *h1cgv_ul;

  TH1F *h1cgp_zvtx;
  TH1F *h1cgp_r;
  TH1F *h1cgp_z;
  TH1F *h1cgp_ur;

  TH1F *h1pc1d;
  TH1F *h1pc2d;
  TH1F *h1pc3d;

  TH2F *h2pcxy;
  std::vector<TH1F*> vh;
};
#endif 

