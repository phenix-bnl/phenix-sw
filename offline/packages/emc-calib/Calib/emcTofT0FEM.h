#ifndef __EMCTOFT0FEM_H__
#define __EMCTOFT0FEM_H__

#ifndef __EMCGAINFEM_H__
#include "emcGainFEM.h"
#endif

/** Stores T0 drift values for one FEM. 
    2 versions exit:
    Version 0 (up to Jan 2004) : 144 channels.
    Version 1 (starting Run4) : 145 channels (the last channel
    is used to keep track of BBC T0 simply).
    @ingroup calibration
*/

class emcTofT0FEM : public emcGainFEM
{

public:

  /// ctor.
  explicit emcTofT0FEM(int absPosition,int version=0);

  /// ctor.
  emcTofT0FEM(int absPosition,
	     const PHTimeStamp& tStart, const PHTimeStamp& tEnd,
	      int version=0);

  virtual emcTofT0FEM* clone(void) const
  {
    return new emcTofT0FEM(*this); 
  }

  virtual emcTofT0FEM* create(void) const
  {
    return new emcTofT0FEM(AbsolutePosition(),
			   GetStartValTime(),
			   GetEndValTime(),
			   Version());
  }

  /// Create default object with no drift at all.
  static emcTofT0FEM* Default(int absPosition,
			      const PHTimeStamp& tStart,
			      const PHTimeStamp& tEnd,
			      int version=0);

  /// Category = "TofT0s"
  virtual const char* GetCategory(void) const;

  void setBBCT0(float b);

  float getBBCT0() const { return fBBCT0; }

  using emcGainFEM::getValueFast;

  virtual float getValueFast(int /*ichannel*/) const { return getBBCT0(); }

  virtual void writeDataToFile(FILE* fp) const;

private:
  float fBBCT0;
};

#endif
