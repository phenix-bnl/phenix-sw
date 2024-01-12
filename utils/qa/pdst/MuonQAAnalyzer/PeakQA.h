#ifndef MUONQAANALYZER_PEAK_QA_H
#define MUONQAANALYZER_PEAK_QA_H

// STL Includes
#include<vector>
#include<iostream>
#include<map>

// BOOST Includes
#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#include<boost/bind.hpp>
#endif /* __CINT__ */

#include<ObservableQABase.h>

using boost::bind;

class TH1;
class HistoQA;

class PeakQA : public ObservableQABase
{
public:
#ifndef __CINT__
  typedef boost::shared_ptr<PeakQA> ptr;
#else
  class ptr;
#endif /* __CINT__ */
  typedef std::vector<ptr> vector;
  typedef double value_type;
  typedef std::map<int,value_type> ValMap;
  typedef ValMap::iterator::value_type MVT;
  typedef std::pair<ValMap::iterator, bool> InsertReturnType;

  PeakQA(int axis = 1) :
    ObservableQABase(MuonQA::PEAK),
    _axis(axis),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  PeakQA(const std::string& name,
         int axis = 1,
         const MuonQA::SummaryFlag sum = MuonQA::OVERALL) : 
    ObservableQABase(name, MuonQA::PEAK, sum),      
    _axis(axis),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  PeakQA(const std::string& name,
         int axis,
         const HistoQAPtr& parent,
         const MuonQA::SummaryFlag sum = MuonQA::OVERALL) : 
    ObservableQABase(name, parent, MuonQA::PEAK, sum),      
    _axis(axis),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  ~PeakQA() {};  

  //! Retrieve the mean coord size and accumulate it run by run
  virtual void process(TH1* histo);
  void createSummary();
 
private:
  int _axis; //! Axis along which to take the mean  
  int _lastRun, _curRun;
  ValMap::iterator _lastIt;
  bool _outputHasBeenWritten;  //! Flag to avoid multiple output
  ValMap obsValByRun;
};
#endif // __MUONQAANALYZER_COORDSIZE_QA_H__











