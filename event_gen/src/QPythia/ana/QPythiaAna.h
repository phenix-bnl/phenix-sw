#ifndef __QPYTHIAANA_H__
#define __QPYTHIAANA_H__

#include <SubsysReco.h>
#include <string>
class PHCompositeNode;
class TFile;
class TTree;

class QPythiaAna : public SubsysReco {

 public:
  QPythiaAna(const std::string& jetNodeName, const std::string& outfilename);
  virtual ~QPythiaAna();

  int Init(PHCompositeNode* topNode);
  int process_event(PHCompositeNode* topNode);
  int End(PHCompositeNode* topNode);

 private:
  std::string _jetNodeName;
  std::string _outFileName;
  TFile *_outputFile;

  TTree *_outputTree;
  float _jetpt, _jeteta, _jetphi, _jetm;
  int _nconst;
  enum { NMAX=100 };
  float _constpt[NMAX], _consteta[NMAX], _constphi[NMAX], _constm[NMAX];
  float _constR[NMAX], _constjt[NMAX], _constz[NMAX];
  int _constpid[NMAX];

};

#endif /* __QPYTHIAANA_H__ */
