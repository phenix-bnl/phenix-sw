//  Declaration of class PdbZdcLUT
//  Author: chiu

#ifndef __PDBZDCLUT_HH__
#define __PDBZDCLUT_HH__

#include "PdbCalChan.hh"

#define PDBZDCLUTSIZE 4096

class PdbZdcLUT : public PdbCalChan 
{
public:
  PdbZdcLUT();
  virtual ~PdbZdcLUT();

  float getlut(int ch) const { return lut[ch]; }
  void setlut(int ch, float val) { lut[ch] = val; return; }

  virtual void print() const;

private:
  void zero();	// zero out contents of lut

  float lut[PDBZDCLUTSIZE];

  ClassDef(PdbZdcLUT,1);
};

#endif /* __PDBZDCLUT_HH__ */
