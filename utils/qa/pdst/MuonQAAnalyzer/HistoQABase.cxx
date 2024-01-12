#include<iterator>
#include<functional>
#include<utility>

#include<boost/bind.hpp>

#include<ObservableQABase.h>
#include<HistoQABase.h>

using boost::bind;

void
HistoQABase::addObservable(ObservablePtr obs)
{
  _observables.push_back(obs);
  obs->setParent(shared_from_this());
}

void
HistoQABase::addObservable(ObservablePtr obs,
                       const MuonQA::SummaryFlag sum)
{
  addObservable(obs);
  obs->setSummaryFlag(sum);
}

void
HistoQABase::addObservable(ObservableQABase* obs)
{
  addObservable(ObservablePtr(obs));
}

void
HistoQABase::addObservable(ObservableQABase* obs,
                           const MuonQA::SummaryFlag sum)
{
  addObservable(ObservablePtr(obs),sum);
}

HistoQABase::SetMap
HistoQABase::outputGoodRunList()
{
  SetMap aggregate;  
  std::transform(_observables.begin(),
                 _observables.end(),
                 std::inserter(aggregate,
                               aggregate.end()),
                 bind(&std::make_pair<std::string, std::set<int> >,
                      bind(std::plus<std::string>(),
                           _name + "_",
                           bind(&ObservableQABase::getName,_1)
                           ),
                      bind(&ObservableQABase::getGoodRunList,_1)
                      )
                 );
  return aggregate;
}
