#ifndef __MPCSAMPLECONTAINER_H__
#define __MPCSAMPLECONTAINER_H__

#include <PHObject.h>
#include <PHCompositeNode.h>
#include <iostream>

class TClonesArray;
class mpcSample;
class TH1;
class TH1F;
class TGraph;

class mpcSampleContainer : public PHObject
{
public:
  mpcSampleContainer();
  virtual ~mpcSampleContainer();

  
  virtual Short_t get_nsamples()  { return -9999; }
  virtual Short_t get_nchannels() { return -9999; }
  virtual Int_t get_nentries()  { return -9999; }

  /** Add a new adc sample (using copy ctor).
      s must be of compatible type, otherwise this will return 0.
  */
  virtual mpcSample* AddSample(const mpcSample&) {
    std::cout << "virtual AddSample" << std::endl;
    return NULL;
  }
  virtual mpcSample* GetSample(const UInt_t) const {
    std::cout << "virtual GetSample" << std::endl;
    return NULL;
  }

  /// the number of total entries
  virtual UInt_t size(void) const { return 0; }

  virtual void Reset();

  virtual void Clear(Option_t * = "") { Reset(); }

  virtual void print(std::ostream& os=std::cout) const {
    os << "mpcSampleContainer::print()" << std::endl;
  }

private:
  ClassDef(mpcSampleContainer,1)
};

#endif	// __MPCSAMPLECONTAINER_H__

