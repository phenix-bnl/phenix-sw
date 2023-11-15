//  Declaration of class PdbEmcTrackMatch
//  Author: Cesar

#ifndef PDBEMCTRACKMATCH_HH__
#define PDBEMCTRACKMATCH_HH__

#include "PdbCalChan.hh"

#define NPAR 3
#define NMOMPAR 4

class PdbEmcTrackMatch : public PdbCalChan 
{
public:
  PdbEmcTrackMatch();
  virtual ~PdbEmcTrackMatch(){}

  float getParameter(const size_t, const size_t) const;
   
  void setParameter(const size_t, const size_t, const float);
   
   virtual void print() const;

private:
   void zero();
   
private:
  float match[NPAR][NMOMPAR];

  ClassDef(PdbEmcTrackMatch,1);
};

#endif /* __PDBEMCTRACKMATCH_HH__ */
