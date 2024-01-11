#ifndef __ZDCOUTV1_H
#define __ZDCOUTV1_H

#include <iostream>
#include "TClonesArray.h"
#include "ZdcOut.h"
#include "ZdcReturncodes.h"

#define NPMTZDCV1 8
#define NZDC 2

/* 
   The ZdcOutRun1 class is necessary since I screwed up on the year 1 Zdc 
   Output. The ZdcOut base class didnd't have an implementation and was not 
   known to root. It didn't seem to be possible to make ZdcOut known to root 
   and keep the year 1 dst's readable. Therefore I introduce an ZdcOutRun1 
   class which is not known to root and that seems to work fine for reading 
   the year 1 dst's and year 2 dst's where ZdcOut has a streamer. The class 
   version number was bumped to 2 to suppress the error message when reading 
   the test year 2 dst's
*/ 

class ZdcOutRun1: public ZdcOut
{
 public:

  virtual ~ZdcOutRun1() {}
  virtual void Reset() {return;}
  virtual void identify(std::ostream& os = std::cout) const {return;}
  virtual int isValid() const {return 0;}

 
};

class ZdcOutv1: public ZdcOutRun1
{
 public:

  ZdcOutv1();
  virtual ~ZdcOutv1();

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

  short ZdcOutNpmt;		// number of hit pmts
  float ZdcVertex;		// z-vertex
  float ZdcVertexError;		// error on z-vertex
  float ZdcTimeZero;		// time-zero
  float ZdcTimeZeroError;	// error on time-zero
  TClonesArray *ZdcHits;	// module information
  TClonesArray *ZdcNS;		// finished information

  ClassDef(ZdcOutv1,1)

};

#endif
