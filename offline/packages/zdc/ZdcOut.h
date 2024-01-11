#ifndef __ZDCOUT_H
#define __ZDCOUT_H

#include <iostream>
#include "phool.h"
#include "PHObject.h"
#include "ZdcReturncodes.h"
#include "Zdc.hh"

class ZdcOut: public PHObject
{
 public:

  virtual ~ZdcOut() {}

  virtual void identify(std::ostream& os = std::cout) const 
    {
      std::cout << "virtual ZdcOut object" << std::endl;
      return;
    }

  virtual void Reset() 
    {
      std::cout << PHWHERE 
		<< "ERROR: Reset()not implemented by daughter class" 
		<< std::endl;
      return;
    }

  virtual int isValid() const 
    {
      std::cout << PHWHERE 
		<< "isValid() not implemented by daughter class" 
		<< std::endl;
      return 0;
    }

  virtual float get_Zvertex() const  {return INVALID_FLOAT;}
  virtual float get_ZvertexError() const {return INVALID_FLOAT;}
  virtual float get_TimeZero() const {return INVALID_FLOAT;}
  virtual float get_TimeZeroError() const {return INVALID_FLOAT;} 
  virtual short set_TimeVertex(float t0, float t0err, 
			       float vtx, float vtxerr) {return INVALID_SHORT;}
  virtual void set_TimeZero(float t0)
    {
      std::cout << PHWHERE 
		<< "ERROR: THIS IS A VIRTUAL FUNCTION WITH NO EFFECT" 
		<< std::endl;
      return;
    }

  virtual short get_npmt() const {return INVALID_SHORT;}

  virtual short AddZdcHit(float charge, 
			  float time0, float time1, 
			  short ipmt) {return INVALID_SHORT;}
  virtual float get_Charge(short iPmt) const {return INVALID_FLOAT;}
  virtual float get_Time0(short iPmt) const {return INVALID_FLOAT;}
  virtual float get_Time1(short iPmt) const {return INVALID_FLOAT;}

  virtual float get_DigitalSum(const int arm) const;
  virtual float get_AnalogSum(const int arm) const;

  virtual void AddZdcNS(float energy, float timing, short nzdc)
    {
      std::cout << PHWHERE 
		<< "ERROR: THIS IS A VIRTUAL FUNCTION WITH NO EFFECT" 
		<< std::endl;
      return;
    }
  virtual void AddZdcNS(float energy, short nzdc)
  {
      std::cout << PHWHERE 
		<< "ERROR: THIS IS A VIRTUAL FUNCTION WITH NO EFFECT" 
		<< std::endl;
      return;
    }
  virtual float get_Energy(short nzdc) const {return INVALID_FLOAT;}
  virtual float get_Timing(short nzdc) const {return INVALID_FLOAT;}


  virtual void FillFromClass(const ZdcOut& old);
  
  ClassDef(ZdcOut,1)

};

#endif
