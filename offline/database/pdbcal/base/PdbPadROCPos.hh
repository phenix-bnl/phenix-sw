//  Declaration of class PdbPadROCPos
//  Purpose: Store info on where the Pad Chamber ROCs are placed
//  Author: silvermy

#ifndef __PDBPADROCPOS_HH__
#define __PDBPADROCPOS_HH__

#include "PdbCalChan.hh"

class PdbPadROCPos : public PdbCalChan 
{
public:
  PdbPadROCPos();
  virtual ~PdbPadROCPos(){}
  size_t getNDim() const { return nDim; }
  const char* getParName(const size_t) const;
  
  int getPacketid()     const {return ROCPosParameter[0];}
  int getGrouprowl()    const {return ROCPosParameter[1];}
  int getGroupcolumn()     const {return ROCPosParameter[2];}
  int getROCnumber()     const {return ROCPosParameter[3];}
   
  int getParameter(const size_t) const;
  
  void setPacketid(const int val)     { ROCPosParameter[0] = val;}
  void setGrouprow(const int val)    { ROCPosParameter[1] = val;}
  void setGroupcolumn(const int val)     { ROCPosParameter[2] = val;}
  void setROCnumber(const int val)     { ROCPosParameter[3] = val;}
  
  void setParameter(const size_t, const int);  

  virtual void print() const;

private:
  void zero();
   
private:
  size_t nDim;
  int ROCPosParameter[4];

  ClassDef(PdbPadROCPos,1);
};

#endif /* __PDBPADROCPOS_HH__ */
