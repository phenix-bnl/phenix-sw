#ifndef __VERNIER_H__
#define __VERNIER_H__

#include <SubsysReco.h>
#include <string>

class Fun4AllServer;
class Fun4AllHistoManager;
class PHCompositeNode;
class TH1;
class TFile;
class TTree;

class Vernier: public SubsysReco
{
 public:
  Vernier(const std::string &outfilename);
  virtual ~Vernier() {}
 
  int  Init         (PHCompositeNode *topNode); 
  int  process_event(PHCompositeNode *topNode);
  int  End          (PHCompositeNode *topNode);

 protected:
  
  bool trig_bbcnovtxcut(const int trig);
  bool trig_bbc(const int trig);
  bool trig_bbcnarrow(const int trig);
  bool trig_zdcwide(const int trig);
  bool trig_zdcnarrow(const int trig);
  
  std::string OutFileName;
  TFile *fout;
  TFile *finpol;
  TTree *Tout;

  int cross_id;
  int trigraw;
  int triglive;
  int trigscaled;
  int evtnumber;
  double bbc_z;
  double zdcll1_z; 

  static const int asize = 25000;
  static const int histosize = 25000;
  static const double historange;
  double bbc_a[120][asize];
  double bbcnarrow_a[120][asize];
  double clock_a[120][asize];
  double zdcnarrow_a[120][asize];
  double zdcwide_a[120][asize];

  TH1 *BBC_gl1p[120];
  TH1 *BBCNARROW_gl1p[120];
  TH1 *ZDCNARROW_gl1p[120];
  TH1 *ZDCWIDE_gl1p[120];
  TH1 *Clock_gl1p[120];

  TH1 *BBCgl1p;
  TH1 *BBCnarrowgl1p;
  TH1 *Zdcwidegl1p;
  TH1 *ZDCnarrowgl1p;
  TH1 *Clockgl1p;

  TH1 *BBCRATE[120];
  TH1 *BBCNARROWRATE[120];
  TH1 *ZDCNARROWRATE[120];
  TH1 *ZDCWIDERATE[120];
  TH1 *BBC_RATE;
  TH1 *BBCNARROW_RATE;
  TH1 *ZDCNARROW_RATE;
  TH1 *ZDCWIDE_RATE;

  TH1 *BBC_novtxcut_Z;
  TH1 *BBC_Z;
  TH1 *BBC_narrow_Z;
  TH1 *ZDC_wide_Z;
  TH1 *ZDC_narrow_Z;
  
  unsigned long int bbc_tot, bbcnarrow_tot, zdcnarrow_tot, zdcwide_tot, clock_tot;
  double bbc_zvtx, zdc_zvtx;
  int gl1clock, bbcll1, bbcnarrow, zdcnarrow, zdcwide, eventsequence;
  int dohh, double_dohh;
  char namestring[64];
  long int ncalls;
  int ievent, gl1crossingID; 
  unsigned int scaled_trig;
  
};

#endif




