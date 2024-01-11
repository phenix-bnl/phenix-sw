#ifndef __MPCCLUSTERCONTAINERV1_H__
#define __MPCCLUSTERCONTAINERV1_H__

#include "mpcClusterContentV1.h"
#include "mpcClusterContainer.h"

class TClonesArray;

/** (VERSION) Container of mpcClusterContentV1. */

class mpcClusterContainerV1 : public mpcClusterContainer
{
public:

  mpcClusterContainerV1();
  mpcClusterContainerV1(const mpcClusterContainerV1&);
  mpcClusterContainerV1& operator=(const mpcClusterContainerV1&);

  unsigned int capacity(void) const;

  mpcClusterContainerV1* clone(void) const;

  mpcClusterContainerV1* create(void) const;

  virtual ~mpcClusterContainerV1();

  mpcClusterContentV1* addCluster(unsigned int i);
  
  mpcClusterContentV1* addCluster(unsigned int i, 
				  const mpcClusterContent& c);

  mpcClusterContent* findCluster(int clusterid) const;

  mpcClusterContent* getCluster(unsigned int i) const;

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  bool removeCluster(unsigned int i) ;

  void Reset();

  bool resize(unsigned int newsize);

  unsigned int size(void) const;

protected:

  TClonesArray* fMpcClusters;

private:
  void allocate(unsigned int thesize);
  void copy(mpcClusterContainerV1& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(mpcClusterContainerV1,1) // Array of mpcClusterContentV1
};
#endif
