#ifndef __PDBMUTCALIBSTRIP_HH__
#define __PDBMUTCALIBSTRIP_HH__
// $Id: PdbMutCalibStrip.hh,v 1.10 2010/09/28 23:03:21 hpereira Exp $

//  Declaration of class PdbMutCalibStrip
//  Purpose: Store MuTr calibration information; one object, one strip
//  Author: silvermy (silvermy@lanl.gov)

#include <PdbCalChan.hh>
#include <iosfwd>
#include <string>

// number of extra calibration parameters besides pedestal and gain
static const int NMUTCALIBPAR = 4;

static const int MAXMUTRDAC = 0xff;
static const int MAXMUTRADC = 1800;

class PdbMutCalibStrip : public PdbCalChan
{
public:

  //! constructor
  PdbMutCalibStrip();

  //! constructor
  PdbMutCalibStrip( const int iarm, const int istation, const int ioctant, const int ihalfoctant, const int igap, const int iplane, const int istrip);

  //! copy constructor
  PdbMutCalibStrip( const PdbMutCalibStrip& );


  //! assignment operator
  PdbMutCalibStrip& operator = (const PdbMutCalibStrip& );

  //! zero all calibration parameters
  void zero();

  //! destructor
  virtual ~PdbMutCalibStrip()
  {}

  virtual void print() const;

  virtual void write(std::ostream& os) const;

  virtual void read(std::istream& is);

  int getArm() const
  { return ArmNum; }

  int getStation() const
  { return StationNum; }

  int getOctant() const
  { return OctantNum; }

  int getHalfOctant() const
  { return HalfOctantNum; }

  int getGap() const
  { return GapNum; }

  int getPlane() const
  { return PlaneNum; }

  int getStrip() const
  { return StripNum; }

  int get_ncalibpar ()  const
  { return NMUTCALIBPAR; }

  float get_pedestal() const
  { return pedestal; }

  float get_gain() const
  { return gain; }

  float get_rms() const
  { return rms; }

  float get_calibpar (const int index) const
  { return calibpar[index]; }

  void setArm ( const int temp)
  { ArmNum = temp; }

  void setStation ( const int temp)
  { StationNum = temp; }

  void setOctant ( const int temp)
  { OctantNum = temp; }

  void setHalfOctant (const int temp)
  { HalfOctantNum = temp; }

  void setGap ( const int temp)
  { GapNum = temp; }

  void setPlane ( const int temp)
  { PlaneNum = temp; }

  void setStrip ( const int temp)
  { StripNum = temp; }

  void set_pedestal (const float temp)
  { pedestal = temp; }

  void set_gain ( const float temp)
  { gain = temp; }

  void set_rms ( const float temp)
  { rms = temp; }

  void set_calibpar (const int index, const float temp)
  { calibpar[index] = temp; }

  // set all routines
  void set_indices (const int iarm, const int istation, const int ioctant, const int ihalfoctant, const int igap, const int iplane, const int istrip);

  void set_values (const float fped, const float fgain, const float frms);

  void set_calibparAll (const float temp[NMUTCALIBPAR]);

  // get all
  void get_indices (
    int & iarm, int & istation, int & ioctant,
    int & ihalfoctant, int & igap, int & iplane,
    int & istrip) const;

  void get_values (float & fped, float & fgain, float & frms) const;

  void get_calibparAll (float temp[NMUTCALIBPAR]) const;

  // id routines
  int getUniqueId() const;

  //! strip name
  std::string getName( void ) const;

  // identifying the calibration parameters with their presently assigned names
  float getCenter() const
  { return calibpar[0]; }

  float getSqrLow() const
  { return calibpar[1]; }

  float getSqrHigh() const
  { return calibpar[2]; }

  float getLinSat() const
  { return calibpar[3]; }

  void setCenter(const float temp)
  { calibpar[0] = temp; }

  void setSqrLow(const float temp)
  { calibpar[1] = temp; }

  void setSqrHigh(const float temp)
  { calibpar[2] = temp; }

  void setLinSat(const float temp)
  { calibpar[3] = temp; }

  // fit info
  int getStatus() const
  { return Status; }

  int getStep() const
  { return Step; }

  int getSaturation() const
  { return Saturation; }

  void setStatus(const int temp)
  { Status = temp; }

  void setStep(const int temp)
  { Step = temp; }

  void setSaturation(const int temp)
  { Saturation = temp; }

  // Status > 1000 || Status < 0 means that the fit was not really succesfull
  int isValid() const
  { return ( (Status > 1000 || Status < 0) ? 0 : 1); }

  // perhaps most important: conversion between DAC and ADC values
  // using the fit parameters
  virtual int getAdc(const int dac) const;
  virtual int getAdc(const float charge) const;
  virtual int getDac(const int adc) const;
  virtual float getCharge(const int adc) const;

  // also simpler, linear approx. conversion routines
  int getLinAdc(const int dac) const;
  int getLinAdc(const float charge) const;
  int getLinDac(const int adc) const;
  float getLinCharge(const int adc) const;

  // help routine to calc. the non-linear contribution
  float calcNonLinContrib(

    const int nsteps = 0, const float stepfactor = 0,
    float *olddiff = 0, const int firststep = 0, const int sign = 1) const;

  // a hopefully faster sum version
  float sumNonLinContrib(const float dacval = 0) const;

  protected:

  int ArmNum;
  int StationNum;
  int OctantNum;
  int HalfOctantNum;
  int GapNum;
  int PlaneNum;
  int StripNum;

  float pedestal;
  float gain;
  float rms;
  float calibpar[NMUTCALIBPAR];

  int Status; // how did the fitting go?
  int Step; // step size used in fitting
  int Saturation; // where pre-amplifier changes response
                  // LinSat parameter kicks in

  ClassDef(PdbMutCalibStrip,1);
};
#endif /* __PDBMUTCALIBSTRIP_HH__ */
