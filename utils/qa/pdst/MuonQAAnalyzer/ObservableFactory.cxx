#include<stdexcept>

#include<boost/make_shared.hpp>
#include<ObservableQABase.h>
#include<ObservableQA.h>

#include<ObservableFactory.h>

boost::shared_ptr<ObservableQABase>
ObservableFactory::makeObservable(const std::string& name,
                                  const MuonQA::ObsType obs,
                                  const MuonQA::SummaryFlag sum)
{
  switch (obs)
  {
    case MuonQA::ENTRIES:
      return boost::shared_ptr<ObservableQABase>(new EntriesQA(name, sum));
      break;
    case MuonQA::MEAN:
      return boost::shared_ptr<ObservableQABase>(new MeanQA(name, 1, sum));             
      break;
    case MuonQA::PEAK:
      return boost::shared_ptr<ObservableQABase>(new PeakQA(name, sum));             
      break;
    case MuonQA::ACTIVEWEDGE:
      return boost::shared_ptr<ObservableQABase>(new ActiveWedgeQA(name, sum));             
      break;
    default:
      throw std::runtime_error(std::string("Unknown observable type"));
      break;
  }  
}
