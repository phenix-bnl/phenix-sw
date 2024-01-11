#ifndef __EMCGAINFEM_H__
#define __EMCGAINFEM_H__

#ifndef __EMCTRACEDFEM_H__
#include "emcTracedFEM.h"
#endif

/** Stores gain values for one FEM. 
@ingroup calibration
*/

class emcGainFEM : public emcTracedFEM
{

public:
  
  /// ctor.
  explicit emcGainFEM(int absPosition);

  /// ctor.
  emcGainFEM(int absPosition,
	     const PHTimeStamp& tStart, const PHTimeStamp& tEnd);

  virtual emcGainFEM* clone(void) const 
    { return new emcGainFEM(*this); }
 
  virtual emcGainFEM* create(void) const
  { return new emcGainFEM(AbsolutePosition(),
			  GetStartValTime(),
			  GetEndValTime()); }

  ///
  virtual void AddNewItem(int ichannel, emcTracedValue* tv);

  /// Create default object with 144 channels with gain = 1.0.
  static emcGainFEM* Default(int absPosition, 
			     const PHTimeStamp& t1, 
			     const PHTimeStamp& t2);

  /** Compact items that are consistent within epsilon.
      WARNING: not thoroughly tested yet. Use with care.*/
  virtual float Compact(float epsilon);

  /// Category = "Gains"
  virtual const char* GetCategory(void) const { return "Gains"; }

protected:
  bool CompactOneChannelLines(int ichannel, float epsilon);
  bool CompactOneChannelConstants(int ichannel, float epsilon);
  virtual bool AreDifferent(float v1, float v2, float epsilon) const;
};

#endif
