#ifndef __emcRecoModuleRealFactory_h__
#define __emcRecoModuleRealFactory_h__

class SubsysReco;
class PHFlag;

/** Factory for emc SubsysReco objects processing real events . */

class emcRecoModuleRealFactory
{
public:

  /// Depending on the flags, returns the adequate emcRecoModule.
  static SubsysReco * create(const PHFlag&);
};

#endif
