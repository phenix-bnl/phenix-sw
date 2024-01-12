#ifndef __MUONQAANALYZER_MEAN_QA_H__
#define __MUONQAANALYZER_MEAN_QA_H__

// STL Includes
#include<vector>
#include<map>
#include<iostream>

// BOOST Includes
#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#include<boost/bind.hpp>
#endif /* __CINT__ */

#include<ObservableQABase.h>

class TH1;
class HistoQA;

using boost::bind;

class MeanQA : public ObservableQABase
{
public:
#ifndef __CINT__
  typedef boost::shared_ptr<MeanQA> ptr;
#else
  class ptr;
#endif /* __CINT__ */
  typedef std::vector<ptr> vector;
  typedef double value_type;
  typedef std::map<int,value_type> ValMap;
  typedef ValMap::iterator::value_type MVT;
  typedef std::pair<ValMap::iterator, bool> InsertReturnType;

  MeanQA(int axis = 1) :
    ObservableQABase(MuonQA::MEAN),
    _axis(axis),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  MeanQA(const std::string& name,
         int axis = 1,
         const MuonQA::SummaryFlag sum = MuonQA::OVERALL) : 
    ObservableQABase(name, MuonQA::MEAN, sum),
    _axis(axis),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  MeanQA(const std::string& name,
         const HistoQAPtr& parent,
         int axis = 1,
         const MuonQA::SummaryFlag sum = MuonQA::OVERALL) :
    ObservableQABase(name, parent, MuonQA::MEAN, sum),
    _axis(axis),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  ~MeanQA() {};  

  //! Retrieve the mean coord size and accumulate it run by run
  virtual void process(TH1* histo);
  void createSummary();

  void setAxis(const int val) { _axis = val; }
  int getAxis() const { return _axis; }
    
private:
  int _axis; //! Axis along which to take the mean  
  int _lastRun, _curRun;
  ValMap::iterator _lastIt;
  bool _outputHasBeenWritten;  //! Flag to avoid multiple output
  ValMap obsValByRun;
}; 

#endif // __MUONQAANALYZER_COORDSIZE_QA_H__











