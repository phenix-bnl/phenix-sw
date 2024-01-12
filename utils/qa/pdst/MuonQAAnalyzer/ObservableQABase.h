#ifndef MUONQAANALYZER_OBSERVABLE_QA_BASE_H
#define MUONQAANALYZER_OBSERVABLE_QA_BASE_H

// STL Includes
#include<string>
#include<set>

// ROOT Includes
#include<TH1.h>
#include<TAxis.h>

// BOOST Includes
#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#include<boost/tuple/tuple.hpp>
#include<boost/tuple/tuple_comparison.hpp>
#endif /* __CINT__ */

#include<MuonQAFlags.h>

class HistoQABase;
#ifndef __CINT__
  typedef boost::shared_ptr<HistoQABase> HistoQAPtr;
#else
  class HistoQAPtr;
#endif /* __CINT__ */

class ObservableQABase
{
public:
#ifndef __CINT__
  typedef boost::shared_ptr<ObservableQABase> ptr;
  typedef std::set<boost::tuple<int, int, double, double> > TupleSet;
#else
  class ptr;
  class TupleSet;
#endif /* __CINT__ */
  typedef std::vector<ptr> vector;

  ObservableQABase(const MuonQA::ObsType obs = MuonQA::INVALID_TYPE,
                   const MuonQA::SummaryFlag sum = MuonQA::INVALID_FLAG) :
    _name(),
    _parentHisto(),
    _sumFlag(sum),
    _typeFlag(obs)
  {}

  ObservableQABase(const std::string& name,
                   const MuonQA::ObsType obs = MuonQA::INVALID_TYPE,
                   const MuonQA::SummaryFlag sum = MuonQA::INVALID_FLAG):
    _name(name),
    _parentHisto(),
    _sumFlag(sum),
    _typeFlag(obs)
  {}

  ObservableQABase(const std::string& name, 
                   const HistoQAPtr& parent,
                   const MuonQA::ObsType obs = MuonQA::INVALID_TYPE,
                   const MuonQA::SummaryFlag sum = MuonQA::INVALID_FLAG):
    _name(name),
    _parentHisto(parent),
    _sumFlag(sum),
    _typeFlag(obs)
  {}
  
  virtual ~ObservableQABase() {}
  
  void setParent(const HistoQAPtr& parent)
  { _parentHisto = parent; }
  HistoQAPtr getParent() const 
  { return _parentHisto; }

  void setName(const std::string& name) { _name = name; }
  std::string getName() const { return _name; }

  void setType(const MuonQA::ObsType flag) { _typeFlag = flag ; }
  MuonQA::ObsType getType() const { return _typeFlag; }
  
  void setSummaryFlag(const MuonQA::SummaryFlag flag) { _sumFlag = flag ; }
  MuonQA::SummaryFlag getSummaryFlag() const { return _sumFlag; }

  /** Set acceptable bounds for all runs */
  void setBounds(double lb, double ub);
  /** Set acceptable bounds for a subset of runs */
  void setBounds(int firstRun, int lastRun, double lb, double ub);
  /** Set acceptable bounds from given run to last run */
  void setBoundsFrom(int firstRun, double lb, double ub);
  /** Set acceptable bounds from first run to given run */
  void setBoundsTo(int lastRun,  double lb, double ub);

  /** Output a good run list based on the summary histogram and set bounds */
  std::set<int> getGoodRunList();

  TH1* getSummaryHisto() const { return _summary; }

  virtual void process(TH1* histo) = 0;
  virtual void createSummary() = 0;

  template<typename T>
  struct AutoSetBinContent {
    typedef void result_type;
    void operator()(TH1* hptr, int run, T val, double normFactor)
    {
      TAxis* xaxis = hptr->GetXaxis();
      int bin_center = 0;
      while ( bin_center < run )
        bin_center = xaxis->GetBinCenter(++ibin);
      
      if ( bin_center != run ) return;
      hptr->SetBinContent(ibin, val/normFactor);
    }
   private:
    int ibin; /**< Holds state from call to call */
  };
protected:
  std::string _name; //! Name of the observable
  HistoQAPtr _parentHisto; //! Ptr to owner of this observable
  MuonQA::SummaryFlag _sumFlag; //! What summaries to save
  MuonQA::ObsType _typeFlag; //! What kind of observable am I?
  TupleSet _validityBounds; //! Holds the good ranges for the observable
  TH1 *_summary; /**< Histogram that stores the run-by-run summary */
  
  const static int LAST_RUN = 999999; /**< Placeholder for last run number */
  const static int FIRST_RUN = -1; /**< Placeholder for first run number */  
};

#endif // __MUONQAANALYZER_OBSERVABLE_QA_BASE_H__
















