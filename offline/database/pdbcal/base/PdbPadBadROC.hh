//  Declaration of class PdbPadBadROC
//  Purpose: Store info on malfunctioning Pad Chamber ROCs
//  Author: silvermy

#ifndef __PDBPADBADROC_HH__
#define __PDBPADBADROC_HH__

#include "PdbCalChan.hh"

class PdbPadBadROC : public PdbCalChan {
public:
  PdbPadBadROC();
  virtual ~PdbPadBadROC(){}

  size_t getNDim() const { return nDim; }
  const char* getParName(size_t) const;

  int getPacketid()     const {return BadROCParameter[0];}
  int getGrouprowl()    const {return BadROCParameter[1];}
  int getGroupcolumn()     const {return BadROCParameter[2];}
  int getROCtype()     const {return BadROCParameter[3];}
   
  int getParameter(const size_t) const;
   
  void setPacketid(const int val)     { BadROCParameter[0] = val;}
  void setGrouprow(const int val)    { BadROCParameter[1] = val;}
  void setGroupcolumn(const int val)     { BadROCParameter[2] = val;}
  void setROCtype(const int val)     { BadROCParameter[3] = val;}

  void setParameter(const size_t, const int);

  virtual void print() const;

private:
   void zero();
   
private:
   size_t nDim;
   int BadROCParameter[4];

  ClassDef(PdbPadBadROC,1);
};

#endif /* __PDBPADBADROC_HH__ */
