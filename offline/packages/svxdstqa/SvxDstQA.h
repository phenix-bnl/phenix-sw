#ifndef __SVXANAMAIN_H__
#define __SVXANAMAIN_H__

//#include <Rtypes.h>

#include <string>
#include <map>

#include <TFile.h>

#include <SubsysReco.h>

//#include "SvxFillHisto.h"

class PHCompositeNode;
class SvxFillHisto;


typedef std::map<std::string, SvxFillHisto*> SvxFillHistoArray;


class SvxDstQA: public SubsysReco {

public:

  SvxDstQA();
  SvxDstQA(std::string filename);
  virtual ~SvxDstQA();

  bool registerFillHisto(SvxFillHisto *hist);

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  int Reset(PHCompositeNode *topNode) 		{return 0;}
  int ResetEvent(PHCompositeNode *topNode) 	{return 0;}
  int InitRun(PHCompositeNode *topNode);
  void Print(const std::string&) const { }

protected:

  int CreateNodeTree(PHCompositeNode *topNode) 	{return 0;}

  std::string m_outputFileName;
  int         m_eventNumber;

  TFile*      m_outputFile;

private:
  SvxFillHisto*  m_histoObj;
  SvxFillHistoArray m_fillHistoAry;

  bool   m_firstevent_in_run;

  //--
//public:
//  ClassDef(SvxAnaMain, 1)
};

#endif

