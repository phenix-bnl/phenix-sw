#ifndef __emcObjectFillerManager_h__
#define __emcObjectFillerManager_h__

class PHObject;
class PHCompositeNode;

/** (OLD?) Manager for object fillers. 
@ingroup deprecated
*/

class emcObjectFillerManager
{
public:

  static bool fill(PHCompositeNode* topNode, PHObject& destination);

  static bool isFillerAvailable(PHObject& destination);

  static int verbose(void) { return fVerbose; }

  static void verbose(int verbose) { fVerbose=verbose; }

private:
  static int fVerbose;
};

#endif
