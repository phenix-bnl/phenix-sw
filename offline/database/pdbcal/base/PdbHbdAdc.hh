#ifndef __PDBHBDADC_HH__
#define __PDBHBDADC_HH__

#include "PdbCalChan.hh"

class PdbHbdAdc : public PdbCalChan 
{
public:
  PdbHbdAdc();
  PdbHbdAdc( const PdbHbdAdc &c);
  virtual ~PdbHbdAdc(){};

  PdbHbdAdc&  operator = (const PdbHbdAdc &c);

  virtual int  setPedestal(const int ADCch, const float pedestal, const float pederr);

  virtual int  setGain(const int ADCch, const float gain, const float gainerr);

  virtual int  getPedestal(const int ADCch, float& pedestal, float& pederr) const;

  virtual int  getGain(const int ADCch, float& gain, float& gainerr) const;

  virtual void print() const;

private:
  float  PedestalConst[2304];        // Pedestal Values
  float  PedestalErr[2304];        // Pedestal Error Values
  float  GainConst[2304];        // Gain Values
  float  GainErr[2304];        // Gain Error Values

  ClassDef(PdbHbdAdc,1);
};

#endif /* __PDBHBDADC_HH__ */
