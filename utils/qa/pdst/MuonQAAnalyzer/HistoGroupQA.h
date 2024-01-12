#ifndef MUONQAANALYZER_HISTOGROUP_QA_H
#define MUONQAANALYZER_HISTOGROUP_QA_H

// BOOST Includes
#ifndef __CINT__
#include<boost/regex.hpp>
#include<boost/shared_ptr.hpp>
#else
namespace boost {
class regex;
}
#endif /* __CINT__ */

// Package Includes
#include<HistoQABase.h>

// Forward declarations
class SubsystemQA;
class ObservableQABase;
class TFile;
class TH1;

class HistoGroupQA :
    public HistoQABase
{
public:
#ifndef __CINT__
  typedef boost::shared_ptr<HistoGroupQA> ptr;
  typedef boost::shared_ptr<SubsystemQA> SubsysPtr;
  typedef boost::shared_ptr<ObservableQABase> ObservablePtr;
#else
  class ptr;
  class SubsysPtr;
  class ObservablePtr;
#endif /* __CINT__ */  
  typedef std::vector<ptr> vector;

  HistoGroupQA();
  HistoGroupQA(const std::string& name);
  HistoGroupQA(const std::string& name,
               const std::string& regex);
  HistoGroupQA(const std::string& name,
               const SubsysPtr& parent);

  virtual ~HistoGroupQA() {}
  
  void processObservables();
  void saveSummaries();

  void setHistogram(const std::string& histName);

  virtual int getNHistos() const { return _histoNames.size(); }

protected:
  boost::regex _regex;
  std::vector<std::string> _histoNames;

  void populateHistoNames();
};

#endif // __MUONQAANALYZER_HISTO_QA_H__
