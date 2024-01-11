#ifndef __EMCCLUSTERAUXINFOCONTAINERV1_H__
#define __EMCCLUSTERAUXINFOCONTAINERV1_H__

//class emcClusterAuxInfo;

#include "emcClusterAuxInfoContainer.h"


class TClonesArray;

/** (VERSION) Container of emcClusterAuxInfo.
@ingroup calibration
*/

class emcClusterAuxInfoContainerV1 : public emcClusterAuxInfoContainer
{
public:

  virtual ~emcClusterAuxInfoContainerV1();

  emcClusterAuxInfoContainerV1();

  unsigned int capacity() const;

  virtual int addInfo(unsigned int i, const float c, const float e, const float x, const float y);

  emcClusterAuxInfo* getInfo(unsigned int i) const;

  void identify(std::ostream& os=std::cout) const;

  void Reset();

  unsigned int size(void) const;

 private:

  TClonesArray* fAuxInfo;

  ClassDef(emcClusterAuxInfoContainerV1,1) // Array of emcClusterAuxInfo

protected:

  void allocate(unsigned int thesize);
  bool expand(unsigned int);
  bool expand_for(unsigned int);

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;


};
#endif
