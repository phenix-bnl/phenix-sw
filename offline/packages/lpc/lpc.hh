#ifndef PHENIX_LPC_HH
#define PHENIX_LPC_HH
#include "PHTimeStamp.h"
/**
 Just subsituate class for namespace.
 <p>
 This class is just subsituate class for namespace,
 which is too new to use the current compilers.
 So it should be replaced by namespace eventually

 @author Hiroaki Ohnishi
 @version June 23, 2000
*/
class lpc{
public:
  //  If you want to add any elements into these enum, 
  // all functions which have these enum as arguments,
  // should be checked how they act with the new elements.
  /// To specify ZDC arm (South North)
  enum ArmType  {South, North};
};
/// typedef. eventually should be PHTimeStamp
typedef PHTimeStamp lpcTime_t;
/// define the number of PMTs on ZDC
const int lpc_N_PMT = 4;
#endif /* PHENIX_LPC_HH */
