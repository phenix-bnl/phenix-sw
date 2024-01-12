// STL Includes
#include<iostream>
#include<algorithm>
#include<iterator>
#include<stdexcept>

// BOOST Includes
#include<boost/bind.hpp>
#include<boost/make_shared.hpp>

// ROOT Includes
#include<TFile.h>
#include<TH1.h>
#include<TKey.h>

//#include<ObservableQA.h>
#include<ObservableQABase.h>
#include<ObservableFactory.h>
#include<HistoQA.h>
#include<HistoGroupQA.h>
#include<SubsystemQA.h>

using boost::bind;

HistoGroupQA::HistoGroupQA() :
    HistoQABase()
{
}

HistoGroupQA::HistoGroupQA(const std::string& name) :
    HistoQABase(name)
{
}

HistoGroupQA::HistoGroupQA(const std::string& name,
                           const std::string& regex) :
    HistoQABase(name),
    _regex(regex)
{
}

HistoGroupQA::HistoGroupQA(const std::string& name,
                           const boost::shared_ptr<SubsystemQA>& parent) :
    HistoQABase(name, parent)
{
}

void 
HistoGroupQA::processObservables()
{
  if ( _histoNames.empty() ) populateHistoNames();
  for (std::vector<std::string>::iterator it = _histoNames.begin();
       it != _histoNames.end();
       ++it)
  {
    setHistogram((*it));
    // First process all of the observables for this file
    std::for_each(_observables.begin(),_observables.end(),
                  bind(&ObservableQABase::process,_1,_histo));
  }
}

void
HistoGroupQA::saveSummaries()
{
  std::for_each(_observables.begin(),_observables.end(),
                bind(&ObservableQABase::createSummary,_1)
                );
}

void
HistoGroupQA::setHistogram(const std::string& histName)
{
  _histo = static_cast<TH1*>(_file->Get(histName.c_str()));
  if (!_histo)
    throw std::runtime_error(_name + " not found in " + _file->GetName());
}

void
HistoGroupQA::populateHistoNames()
{
  std::cout << std::endl;

  // If histogram by histogram summaries have
  // been requested then we need to create new HistoQA's
  // with appropriate new ObservableQAs for each matched
  // name
  
  TIter nextkey(_file->GetListOfKeys());
  TKey *key;
  std::string name;
  while ( (key = static_cast<TKey*>(nextkey())) )
  {
    name = key->GetName();    
    if (boost::regex_match(name, _regex))
    {
      _histoNames.push_back(name);
      std::cout << "Adding " << name
                << " to HistoGroup " << _name
                << std::endl;
      for (std::vector<ObservablePtr>::iterator it = _observables.begin();
           it != _observables.end();
           ++it)      
        if ( (*it)->getSummaryFlag() & MuonQA::INDIVIDUAL )
        {
          boost::shared_ptr<HistoQA> newHist = boost::make_shared<HistoQA>(name, _parentSubsys);
          newHist->setFile(_file);
          _parentSubsys->addHistogram(newHist);

          std::cout << "\tAdded new HistoQA for individual histogram with name "
                    << name << std::endl;
          _parentSubsys->addObservable(name,
                                       ObservableFactory::makeObservable((*it)->getName(),
                                                                         (*it)->getType(),
                                                                         MuonQA::INDIVIDUAL
                                                                         )
                                       );

        }      
    }
  }

  std::cout << std::endl;
}
