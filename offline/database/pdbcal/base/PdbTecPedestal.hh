//  Declaration of class PdbADCChan
//  Purpose: Example of a calibration object
//  Author: Matthias Messer

#ifndef __PDBTECPEDESTAL_HH__
#define __PDBTECPEDESTAL_HH__

#include "PdbCalChan.hh"
#ifndef __CINT__
#include <cstddef>
#endif

class PdbTecPedestal : public PdbCalChan 
{
public:
   PdbTecPedestal();
   virtual ~PdbTecPedestal( );

   size_t getNDim() const { return nDim; }

   float getPedestal(int adc)     const {return ADCPedestal[adc];}
   
   void setPedestal(int adc, float val)     { ADCPedestal[adc] = val;}

   float getParameter(size_t) const;
   void setParameter(size_t, float);
   
   virtual void print() const;

private:
   void zero();
   
private:
   size_t nDim;
   float ADCPedestal[32];

  ClassDef(PdbTecPedestal,1);
};

#endif /* __PDBTECPEDESTAL_HH__ */
