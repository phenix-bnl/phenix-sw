#ifndef __MPCSAMPLE_H__
#define __MPCSAMPLE_H__

#include <PHObject.h>
#include <iostream>

class mpcSample : public PHObject
{
public:
  mpcSample();
  virtual ~mpcSample() {}

  virtual Short_t get_ch() const     { return -9999; }
  virtual Short_t get_adc() const    { return -9999; }
  virtual Char_t  get_sample() const { return -99; }

  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream&) { }

private:

  ClassDef(mpcSample,1)
};

#endif /* __MPCSAMPLE_H__ */

