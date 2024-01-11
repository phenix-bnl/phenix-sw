#ifndef __BBCNORTHSOUTH_H
#define __BBCNORTHSOUTH_H

#include "PHObject.h"


class BbcNorthSouth : public TObject
{
 public:
  BbcNorthSouth() { }
  BbcNorthSouth(const short npmt, const float chargesum, const float timing);
  virtual ~BbcNorthSouth() { }
  void identify(std::ostream& os = std::cout) const;

  short get_nPmt() const {return nPmt;}
  float get_ChargeSum() const {return ChargeSum;}
  float get_Timing() const {return Timing;}

 protected:
  short nPmt;
  float ChargeSum;
  float Timing;

  ClassDef(BbcNorthSouth,1)
};

#endif
