#ifndef __emcRecoModuleSimFactory_h__
#define __emcRecoModuleSimFactory_h__

class SubsysReco;
class PHFlag;

/** Factory for emc SubsysReco objects processing simulated events . */

class emcRecoModuleSimFactory
{
public:

  /// Depending on the flags, returns the adequate emcRecoModule.
  static SubsysReco * create(const PHFlag&);
};

#endif
