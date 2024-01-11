#ifndef __ZDCOUTV2_H
#define __ZDCOUTV2_H

#include <iostream>
#include "TClonesArray.h"
#include "ZdcOut.h"
#include "ZdcReturncodes.h"

#define NPMTZDCV2 8
#define NZDC 2

class ZdcOutv2: public ZdcOut
{
 public:

  ZdcOutv2();
  virtual ~ZdcOutv2();

  // from PHObject
  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  float get_Zvertex() const {return ZdcVertex;}
  float get_ZvertexError() const {return ZdcVertexError;}
  float get_TimeZero() const {return ZdcTimeZero;}
  float get_TimeZeroError() const {return ZdcTimeZeroError;}

  short set_TimeVertex(float t0, float t0err, float vtx, float vtxerr);

  short get_npmt() const {return ZdcOutNpmt;}

  short AddZdcHit(float charge, float time0, float time1, short ipmt);
  float get_Charge(short iPmt) const;
  float get_Time0(short iPmt) const;
  float get_Time1(short iPmt) const;

  void AddZdcNS(float energy, float timing, short nzdc);
  float get_Energy(short nzdc) const;
  float get_Timing(short nzdc) const;

 protected:
  TClonesArray *GetZdcHits() const {return ZdcHits;}
  TClonesArray *GetZdcNS() const {return ZdcNS;}
  void Clear(Option_t *option = "");

  short ZdcOutNpmt;
  float ZdcVertex;
  float ZdcVertexError;
  float ZdcTimeZero;
  float ZdcTimeZeroError;
  TClonesArray *ZdcHits;
  TClonesArray *ZdcNS;

  ClassDef(ZdcOutv2,1)

};

#endif
