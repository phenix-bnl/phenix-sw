
#ifndef __MUONQAANALYZER_ACTIVEWEDGE_QA_H__
#define __MUONQAANALYZER_ACTIVEWEDGE_QA_H__

// STL Includes
#include<vector>
#include<map>
#include<set>
#include<string>

// BOOST Includes
#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#include<boost/bind.hpp>
#include<boost/tuple/tuple.hpp>
#include<boost/tuple/tuple_comparison.hpp>
#endif /* __CINT__ */

#include<ObservableQABase.h>

using boost::bind;

class TH1;
class HistoQA;

class ActiveWedgeQA : public ObservableQABase
{
public:
#ifndef __CINT__
  typedef boost::shared_ptr<ActiveWedgeQA> ptr;
  typedef std::set<boost::tuple<int,int,int,int> > TupleSet;
#else
  class ptr;
  class TupleSet;
#endif /* __CINT__ */
  typedef std::vector<ptr> vector;
  typedef int value_type;
  typedef std::map<int,TupleSet> ValMap;
  typedef ValMap::iterator::value_type MVT;
  typedef std::pair<ValMap::iterator, bool> InsertReturnType;

  ActiveWedgeQA() :
    ObservableQABase(MuonQA::ACTIVEWEDGE),
    _lastRun(0),
    _curRun(0),
    _lastIt(),
    _outputHasBeenWritten(false)
  {}

  ActiveWedgeQA(const std::string& name,
                const MuonQA::SummaryFlag sum = MuonQA::OVERALL) : 
    ObservableQABase(name, MuonQA::ACTIVEWEDGE, sum),
    _lastRun(0),
    _curRun(0),
    _lastIt(),    
    _outputHasBeenWritten(false)
  {}

  ActiveWedgeQA(const std::string& name, 
                const HistoQAPtr& parent,
                const MuonQA::SummaryFlag sum = MuonQA::OVERALL) : 
    ObservableQABase(name, parent, MuonQA::ACTIVEWEDGE, sum),
    _lastRun(0),
    _curRun(0),
    _lastIt(),    
    _outputHasBeenWritten(false)
  {}

  ~ActiveWedgeQA() {};  

  //! Loop called from the higher level HistoQA, this just gets the  
  // entries in the histogram, can be made as complex as you like.
  virtual void process(TH1* histo);
  void createSummary();
  
private:
  int _lastRun, _curRun;
  ValMap::iterator _lastIt;
  bool _outputHasBeenWritten;  //! Flag to avoid multiple output
  //! This maps run numbers to a set of pairs identifying wedges that were 
  //! not present for any given segment  
  ValMap obsValByRun;
}; 
#endif // __MUONQAANALYZER_ACTIVEWEDGE_QA_H__











