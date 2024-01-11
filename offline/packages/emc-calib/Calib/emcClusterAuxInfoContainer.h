#ifndef __EMCCLUSTERAUXINFOCONTAINER_H__
#define __EMCCLUSTERAUXINFOCONTAINER_H__


class emcClusterAuxInfo;

#include "PHObject.h"

#include <map>

class TClonesArray;

/** (VERSION) Container of emcClusterAuxInfo.
@ingroup calibration
*/

class emcClusterAuxInfoContainer : public PHObject
{
public:

  virtual ~emcClusterAuxInfoContainer() {};

  emcClusterAuxInfoContainer() {}

  virtual unsigned int capacity() const;

  virtual emcClusterAuxInfo* getInfo(unsigned int i) const { return 0;};

  virtual void identify(std::ostream& os=std::cout) const {} ;

  virtual void Reset() {} ;

  virtual unsigned int size(void) const {return 0;};

 protected:

  ClassDef(emcClusterAuxInfoContainer,1) // Array of emcClusterAuxInfo
};
#endif

