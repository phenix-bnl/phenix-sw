// STL Includes
#include<iostream>
#include<algorithm>
#include<iterator>
#include<stdexcept>

// BOOST Includes
#include<boost/bind.hpp>

// ROOT Includes
#include<TFile.h>
#include<TH1.h>

//#include<ObservableQA.h>
#include<ObservableQABase.h>
#include<HistoQA.h>
#include<SubsystemQA.h>

using boost::bind;

HistoQA::HistoQA() :
    HistoQABase()
{
}

HistoQA::HistoQA(const std::string& name) :
    HistoQABase(name)
{
}

HistoQA::HistoQA(const std::string& name,
                 const boost::shared_ptr<SubsystemQA>& parent) :
    HistoQABase(name, parent)
{
}

void 
HistoQA::processObservables()
{
  setHistogram();
  // First process all of the observables for this file
  std::for_each(_observables.begin(),_observables.end(),
                bind(&ObservableQABase::process,_1,_histo));
}

void
HistoQA::saveSummaries()
{
  std::for_each(_observables.begin(),_observables.end(),
                bind(&ObservableQABase::createSummary,_1)
                );
}

void
HistoQA::setHistogram()
{
  _histo = static_cast<TH1*>(_file->Get(_name.c_str()));
  if (!_histo)
    throw std::runtime_error(_name + " not found in " + _file->GetName());
}

