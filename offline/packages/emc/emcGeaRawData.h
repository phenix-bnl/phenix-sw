#ifndef __EMC_GEARAWDATA_H__
#define __EMC_GEARAWDATA_H__





#include <PHObject.h>

#include <emcContentT.h>




class emcGeaRawData: public PHObject, public emcContentT<emcGeaRawData> {
public:


  emcGeaRawData * clone() const {
    return new emcGeaRawData(*this);
  }

  
public:
  long hwkey;         /// hardware key  --  to search hardware database
  long swkey;         /// software key  --  may be zero
  short type;         /// detector type: PbSc = 1, PbGl = 2
  short adclopre;     /// ADC low pre-sample
  short adclopost;    /// ADC low post-sample
  short adchipre;     /// ADC high pre-sample
  short adchipost;    /// ADC high post-sample
  short tdc;          /// TDC readout



public:
  const static bool __buildhash = false;
  typedef int key_type;
  key_type get_key() const { return 0; }
  virtual void copy(emcGeaRawData const * from);


  ClassDef(emcGeaRawData, 0)
};





#endif /* ! __EMC_GEARAWDATA_H__ */

