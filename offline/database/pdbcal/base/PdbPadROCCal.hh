//  Declaration of class PdbPadROCCal
//  Purpose: Store Pad Chamber ROC calibration data
//  Author: silvermy

#ifndef __PDBPADROCCAL_HH__
#define __PDBPADROCCAL_HH__

#include "PdbCalChan.hh"

class PdbPadROCCal : public PdbCalChan 
{
public:
  PdbPadROCCal();
  virtual ~PdbPadROCCal(){}

  size_t getNDim() const { return nDim; }
  const char* getParName(const size_t) const;

  int getROCnumber()     const {return ROCCalParameter[0];}
  int getMeasindex()    const {return ROCCalParameter[1];}
  int getTGL1TP1()     const {return ROCCalParameter[2];}
  int getTGL1TP2()     const {return ROCCalParameter[3];}
  int getTGL1TP3()     const {return ROCCalParameter[4];}
  int getTGL2TP1()     const {return ROCCalParameter[5];}
  int getTGL2TP2()     const {return ROCCalParameter[6];}
  int getTGL2TP3()     const {return ROCCalParameter[7];}
  int getTGL3TP1()     const {return ROCCalParameter[8];}
  int getTGL3TP2()     const {return ROCCalParameter[9];}
  int getTGL3TP3()     const {return ROCCalParameter[10];}
   
  int getParameter(const size_t) const;
  
  void setROCnumber(const int val)     { ROCCalParameter[0] = val;}
  void setMeasindex(const int val)    { ROCCalParameter[1] = val;}
  void setTGL1TP1(const int val)     { ROCCalParameter[2] = val;}
  void setTGL1TP2(const int val)     { ROCCalParameter[3] = val;}
  void setTGL1TP3(const int val)     { ROCCalParameter[4] = val;}
  void setTGL2TP1(const int val)     { ROCCalParameter[5] = val;}
  void setTGL2TP2(const int val)     { ROCCalParameter[6] = val;}
  void setTGL2TP3(const int val)     { ROCCalParameter[7] = val;}
  void setTGL3TP1(const int val)     { ROCCalParameter[8] = val;}
  void setTGL3TP2(const int val)     { ROCCalParameter[9] = val;}
  void setTGL3TP3(const int val)     { ROCCalParameter[10] = val;}

  void setParameter(const size_t, const int);
  
  virtual void print() const;

private:
  void zero();
   
private:
  size_t nDim;
  int ROCCalParameter[11];

  ClassDef(PdbPadROCCal,1);

};

#endif /* __PDBPADROCCAL_HH__ */
