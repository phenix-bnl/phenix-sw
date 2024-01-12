#ifndef MUONQAANALYZER_HISTO_QA_H
#define MUONQAANALYZER_HISTO_QA_H

// Package Includes
#include<HistoQABase.h>

// Forward declarations
class SubsystemQA;
class ObservableQABase;
class TFile;
class TH1;

class HistoQA :
    public HistoQABase
{
public:
#ifndef __CINT__
  typedef boost::shared_ptr<HistoQA> ptr;
  typedef boost::shared_ptr<SubsystemQA> SubsysPtr;
  typedef boost::shared_ptr<ObservableQABase> ObservablePtr;
#else
  class ptr;
  class SubsysPtr;
  class ObservablePtr;
#endif /* __CINT__ */  
  typedef std::vector<ptr> vector;

  HistoQA();
  HistoQA(const std::string& name);
  HistoQA(const std::string& name,
          const SubsysPtr& parent);
  virtual ~HistoQA() {}; 

  void processObservables();
  void saveSummaries();

  void setHistogram();
protected:  
};

#endif // __MUONQAANALYZER_HISTO_QA_H__










