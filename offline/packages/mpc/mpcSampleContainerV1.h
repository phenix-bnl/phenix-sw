#ifndef __MPCSAMPLECONTAINERV1_H__
#define __MPCSAMPLECONTAINERV1_H__

#include <mpcSampleContainer.h>
#include <mpcSampleV1.h>
#include <TClonesArray.h>
#include <cassert>

class mpcSampleContainerV1 : public mpcSampleContainer
{
public:
  mpcSampleContainerV1();
  virtual ~mpcSampleContainerV1();

  Short_t get_nsamples() { return n_samples; }
  Short_t get_nchannels() { return n_channels; }
  Int_t get_nentries() { return n_entries; }

  mpcSample* AddSample(const mpcSample &s);

  mpcSample* GetSample(const UInt_t isamp) const
  {
    //assert( isamp <= (UInt_t)GetArray()->GetLast() );
    if ( isamp > (UInt_t)GetArray()->GetLast() ) return 0;
    return (mpcSampleV1*)GetArray()->At(isamp);
  }

  UInt_t size(void) const { return GetArray()->GetLast()+1; }

  void Reset();
  virtual void Clear(Option_t * /*option*/ ="") { Reset(); }

  void print(std::ostream& os=std::cout) const;

protected:
  Short_t n_samples;	// number of samples
  Short_t n_channels;	
  Int_t n_entries;

  TClonesArray *mpcsamples;
  
  TClonesArray *GetArray() const { return mpcsamples; }

private:
  ClassDef(mpcSampleContainerV1,1)
};

#endif	// __MPCSAMPLECONTAINERV1_H__

