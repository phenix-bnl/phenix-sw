#ifndef __CentralityReco_H__
#define __CentralityReco_H__


//
//  Hello Centrality Fan:
//    This is a simple Module whose sole purpose is to apply
//  return centrality related numbers. Make it via instance
//  with run number to make sure you get the right calibrations.
//
//                                                STJL 
//                                                2-21-2004
//
class TH1;

class CentralityReco
{
 public:
  static CentralityReco *instance(int runno);
  virtual ~CentralityReco();
  float getBBCPercentile(float vertex, float bbcqsum);
 
 private:
  CentralityReco(int runno);
  int Init();
  int Reset();
  int fetchBBChistos_DB(const int run);
  static CentralityReco * __instance;
  int currentrun; 
  TH1 * BBC_hcal[3][26];
  TH1 * BBC_h[25]; 
  float norms[25];
  int verbosity;
};

#endif /* __CENTRALITYRECO_H__ */
