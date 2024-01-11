#ifndef __EMCTACPEDFEM_H__
#define __EMCTACPEDFEM_H__

#ifndef __EMCGAINFEM_H__
#include "emcGainFEM.h"
#endif

/** Stores TAC Pedestal Drift information for one FEM. 
@ingroup calibration*/

class emcTacPedFEM : public emcGainFEM
{

public:

  /// ctor.
  emcTacPedFEM(int absPosition);

  /// ctor.
  emcTacPedFEM(int absPosition,
	     const PHTimeStamp& tStart, const PHTimeStamp& tEnd);

  virtual emcTacPedFEM* clone(void) const 
    { return new emcTacPedFEM(*this); }

  virtual emcTacPedFEM* create(void) const 
  { return new emcTacPedFEM(AbsolutePosition(),
			    GetStartValTime(),
			    GetEndValTime()); }
 
  ///
  virtual void AddNewItem(int ichannel, emcTracedValue* tv);

  /// Merge items that are consistent within epsilon.
  virtual float Compact(float epsilon);

  /// Tac Peds are not really lines but merely constants.
  virtual bool Constants(void) const { return true; }

  /// Default object with 144 channels and no drift at all.
  static emcTacPedFEM* Default(int absPosition,
			       const PHTimeStamp& tStart,
			       const PHTimeStamp& tEnd);

  /// Category = "TacPeds".
  virtual const char* GetCategory(void) const { return "TacPeds"; }

protected:
  virtual bool AreDifferent(float v1, float v2, float epsilon) const;
};

#endif
