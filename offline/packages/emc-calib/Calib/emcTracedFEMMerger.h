#ifndef __EMCTRACEDFEMMERGER_H__
#define __EMCTRACEDFEMMERGER_H__

class emcTracedFEM;

/** Utility class to merge 2 emcTracedFEM objects. 
@ingroup calibration
*/

class emcTracedFEMMerger
{
 public:
  static emcTracedFEM* merge(const emcTracedFEM& t1, 
			     const emcTracedFEM& t2);
};

#endif
