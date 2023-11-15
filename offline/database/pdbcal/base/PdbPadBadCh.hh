//  Declaration of class PdbPadBadCh
//  Purpose: Store info on inactive and hot channels for the Pad Chambers
//  Author: silvermy

#ifndef __PDBPADBADCH_HH__
#define __PDBPADBADCH_HH__

#include "PdbCalChan.hh"

class PdbPadBadCh : public PdbCalChan 
{
public:
  PdbPadBadCh();
  virtual ~PdbPadBadCh(){}

  size_t getNDim() const { return nDim; }
  const char* getParName(const size_t) const;

  int getPacketid()     const {return BadChParameter[0];}
  int getChannel()    const {return BadChParameter[1];}
  int getPadtype()     const {return BadChParameter[2];}
   
  int getParameter(const size_t) const;
   
  void setPacketid(const int val)     { BadChParameter[0] = val;}
  void setChannel(const int val)    { BadChParameter[1] = val;}
  void setPadtype(const int val)     { BadChParameter[2] = val;}

  void setParameter(const size_t, int);

  virtual void print() const;

private:
   void zero();
   
private:

   size_t nDim;
   int BadChParameter[3];

  ClassDef(PdbPadBadCh,1);
};

#endif /* __PDBPADBADCH_HH__ */
