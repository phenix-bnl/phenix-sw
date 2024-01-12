#ifndef OBSERVABLEFACTORY_H
#define OBSERVABLEFACTORY_H

#include<string>

#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#endif /* __CINT__ */

#include<MuonQAFlags.h>

class ObservableQABase;

class ObservableFactory
{
 public:
  ObservableFactory() {};
  virtual ~ObservableFactory() {};

  typedef boost::shared_ptr<ObservableQABase> ObservablePtr;
  
  static ObservablePtr makeObservable(const std::string& name,
                               const MuonQA::ObsType obs,
                               const MuonQA::SummaryFlag sum);  
};

#endif /* OBSERVABLEFACTORY_H */
