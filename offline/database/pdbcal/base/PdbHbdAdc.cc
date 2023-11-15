
#include <iostream>
#include <string.h>

#include <PdbHbdAdc.hh>

PdbHbdAdc::PdbHbdAdc( const PdbHbdAdc &c)
{
  float factor, factorerr;
  for (int i = 0; i <2304; i++){

     // Set Gain and Pedestal
     c.getGain(i,factor,factorerr);
     setGain(i,factor,factorerr);
     c.getPedestal(i,factor,factorerr);
     setPedestal(i,factor,factorerr);
  }
}

PdbHbdAdc& PdbHbdAdc::operator= ( const PdbHbdAdc &c)
{
  float factor, factorerr;

  for (int i = 0; i <2304; i++){

     // Set Gain and Pedestal
     c.getGain(i,factor,factorerr);
     setGain(i,factor,factorerr);
     c.getPedestal(i,factor,factorerr);
     setPedestal(i,factor,factorerr);
  }
  return *this;
}

 
PdbHbdAdc::PdbHbdAdc()
{
  memset( PedestalConst,0, 2304 *sizeof(float));
  memset( GainConst,0, 2304 *sizeof(float));
  memset( PedestalErr,0, 2304 *sizeof(float));
  memset( GainErr,0, 2304 *sizeof(float));
}

int PdbHbdAdc::setGain(const int ADCch, const float gain, const float gainerr)
{
  if(ADCch>=2304) return 1;

  GainConst[ADCch]=gain;
  GainErr[ADCch]=gainerr;
  return 0;
}

int PdbHbdAdc::setPedestal(const int ADCch, const float pedestal, const float pederr)
{
  if(ADCch>=2304) return 1;

  PedestalConst[ADCch]=pedestal;
  PedestalErr[ADCch]=pederr;
  return 0;
}


int PdbHbdAdc::getGain(const int ADCch, float& gain, float& gainerr) const
{
  if(ADCch>=2304) return 1;

  gain = GainConst[ADCch];
  gainerr = GainErr[ADCch];
  return 0;
}

int PdbHbdAdc::getPedestal(const int ADCch, float& pedestal, float& pederr) const
{
  if(ADCch>=2304) return 1;

  pedestal = PedestalConst[ADCch];
  pederr = PedestalErr[ADCch];
  return 0;
}

void PdbHbdAdc::print() const
{
  std::cout << "Hello, This is PdbHbdAdc" << std::endl;
}


