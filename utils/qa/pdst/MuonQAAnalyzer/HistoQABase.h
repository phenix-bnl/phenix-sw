#ifndef MUONQAANALYZER_HISTO_QA_BASE_H
#define MUONQAANALYZER_HISTO_QA_BASE_H

// STL Includes
#include<vector>
#include<string>
#include<set>
#include<map>

// BOOST Includes
#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#include<boost/enable_shared_from_this.hpp>
#include<boost/make_shared.hpp>
#endif /* __CINT__ */

// Package Includes
#include<MuonQAFlags.h>

// Forward declarations
class SubsystemQA;
class ObservableQABase;
class TFile;
class TH1;

#ifndef __CINT__
class HistoQABase : 
  public boost::enable_shared_from_this<HistoQABase>
#else
class HistoQABase
#endif /* __CINT__ */
{
public:
#ifndef __CINT__
  typedef boost::shared_ptr<HistoQABase> ptr;
  typedef boost::shared_ptr<SubsystemQA> SubsysPtr;
  typedef boost::shared_ptr<ObservableQABase> ObservablePtr;
#else
  class ptr;
  class SubsysPtr;
  class ObservablePtr;
#endif /* __CINT__ */  
  typedef std::vector<ptr> vector;
  typedef std::map<std::string, std::set<int> > SetMap;

  HistoQABase() :
      _name(),
      _observables(),
      _parentSubsys(),
      _file(NULL),
      _histo(NULL) {}
  
  HistoQABase(const std::string& name) :
      _name(name),
      _observables(),
      _parentSubsys(),
      _file(NULL),
      _histo(NULL) {}
  
  HistoQABase(const std::string& name,
              const SubsysPtr& parent) :
      _name(name),
      _observables(),
      _parentSubsys(parent),
      _file(NULL),
      _histo(NULL) {}
      
  virtual ~HistoQABase() {}
  
  virtual void addObservable(ObservableQABase* obs);
  virtual void addObservable(ObservableQABase* obs,
                             MuonQA::SummaryFlag sum);  
  virtual void addObservable(ObservablePtr obs);
  virtual void addObservable(ObservablePtr obs,
                             MuonQA::SummaryFlag sum);

  virtual void processObservables() = 0;
  virtual void saveSummaries() = 0;
  SetMap outputGoodRunList();

  void setParent(const SubsysPtr& parent) 
  { _parentSubsys = parent; }  
  SubsysPtr getParent() const
  { return _parentSubsys; }

  void setName(const std::string& name) { _name = name; }
  std::string getName() const { return _name; }

  void setFile(TFile* file) { _file = file; }

  virtual int getNHistos() const { return 1; }

protected:
  std::string _name; /**< Name of the histogram to run over */
  std::vector<ObservablePtr> _observables; /**< Storage for the observables to extract */
  SubsysPtr _parentSubsys; /**< Parent subsystem pointer */

  TFile * _file; /**< File currently being processed */
  TH1 *_histo; /**< Retrieved histogram from _file */
};

#endif // MUONQAANALYZER_HISTO_QA_BASE_H










