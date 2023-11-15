#ifndef __PDBMUTCALIBSTRIP_v2_HH__
#define __PDBMUTCALIBSTRIP_v2_HH__
// $Id: PdbMutCalibStrip_v2.hh,v 1.1 2011/07/13 21:20:11 slash Exp $

//  Declaration of class PdbMutCalibStrip_v2
//  Purpose: Store MuTr calibration information; one object, one strip
//  Author: silvermy (silvermy@lanl.gov)Cesar da Silva (slash@bnl.gov)

#include <PdbMutCalibStrip.hh>
#include <iosfwd>
#include <string>
#include <vector>

// number of extra calibration parameters besides pedestal and gain
static const int NMUTCALIBPAR_PIECEWISE = 13;
static const int DAC_VALUES[13] = {0, 16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192};

class PdbMutCalibStrip_v2 : public PdbMutCalibStrip
{
public:
  
  PdbMutCalibStrip_v2( void );
  
  PdbMutCalibStrip_v2( const int iarm, const int istation, const int ioctant,
		       const int ihalfoctant, const int igap, const int iplane, const int istrip);
  
  PdbMutCalibStrip_v2( const PdbMutCalibStrip&  );
  
  //! assignment operator
  PdbMutCalibStrip_v2& operator = (const PdbMutCalibStrip& );

  //! destructor
  ~PdbMutCalibStrip_v2()
  {}
  void print() const;
  
  void write(std::ostream& os) const;
  
  void read(std::istream& is);
  
  void zero() ;
  
  // perhaps most important: conversion between DAC and ADC values
  // using the fit parameters
  int getAdc(const int dac) const;
  int getAdc(const float charge) const;
  int getDac(const int adc) const;
  float getCharge(const int adc) const;
  
  int get_ncalibpar ()  const
  { return NMUTCALIBPAR_PIECEWISE; }

  float get_calibpar (const int index) const
  { return calibpar[index]; }

  float get_calibpar_rms (const int index) const
  { return calibpar_rms[index]; }

  void set_calibpar (const int index, const float temp)
  { calibpar[index] = temp; }

  void set_calibpar_rms (const int index, const float temp)
  { calibpar_rms[index] = temp; }
  
  void set_calibparAll (const float temp[NMUTCALIBPAR_PIECEWISE]);
  
  void get_calibparAll (float temp[NMUTCALIBPAR_PIECEWISE]) const;
  
  protected:

  std::vector<float> calibpar;
  std::vector<float> calibpar_rms;

};

#endif
