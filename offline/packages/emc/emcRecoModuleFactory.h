#ifndef __emcRecoModuleFactory_h__
#define __emcRecoModuleFactory_h__

class SubsysReco;
class PHFlag;

/** Factory for emc SubsysReco objects. */

class emcRecoModuleFactory
{
public:

  /// Depending on the flags, returns the adequate emc SubsysReco module.
  static SubsysReco * create(const PHFlag&);
};

#endif
