#ifndef MUONQAANALYZER_ENTRIES_QA_H
#define MUONQAANALYZER_ENTRIES_QA_H

// STL Includes
#include<vector>
#include<iostream>

// BOOST Includes
#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#include<boost/bind.hpp>
#endif /* __CINT__ */

// Package Includes
#include<ObservableQABase.h>

class TH1;
class HistoQA;

using boost::bind;

class EntriesQA : public ObservableQABase
{
public:  
#ifndef __CINT__
  typedef boost::shared_ptr<EntriesQA> ptr;
#else
  class ptr;
#endif /* __CINT__ */
  typedef std::vector<ptr> vector;
  typedef double value_type;
  typedef std::map<int,value_type> ValMap;
  typedef ValMap::iterator::value_type MVT;

  EntriesQA() :
    ObservableQABase(MuonQA::ENTRIES),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  EntriesQA(const std::string& name,
            const MuonQA::SummaryFlag sum = MuonQA::OVERALL) : 
    ObservableQABase(name, MuonQA::ENTRIES, sum),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  EntriesQA(const std::string& name, 
            const HistoQAPtr& parent,
            const MuonQA::SummaryFlag sum = MuonQA::OVERALL) :
    ObservableQABase(name, parent, MuonQA::ENTRIES, sum),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  virtual ~EntriesQA() {}

  //! Loop called from the higher level HistoQA, this just gets the  
  // entries in the histogram, can be made as complex as you like.
  void process(TH1* histo);
  void createSummary();

private:
  int _lastRun, _curRun;
  ValMap::iterator _lastIt;
  bool _outputHasBeenWritten;  //! Flag to avoid multiple output
  ValMap obsValByRun;
}; 
#endif // __MUONQAANALYZER_OBSERVABLE_QA_H__











