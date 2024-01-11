#ifndef __MPCCLUSTERCONTAINERV2_H__
#define __MPCCLUSTERCONTAINERV2_H__

#include <mpcClusterContentV2.h>
#include <mpcClusterContainer.h>

class TClonesArray;

/** (VERSION) Container of mpcClusterContentV2. */

class mpcClusterContainerV2 : public mpcClusterContainer
{
public:

  mpcClusterContainerV2();
  mpcClusterContainerV2(const mpcClusterContainerV2&);
  mpcClusterContainerV2& operator=(const mpcClusterContainerV2&);

  unsigned int capacity(void) const;

  mpcClusterContainerV2* clone(void) const;

  mpcClusterContainerV2* create(void) const;

  virtual ~mpcClusterContainerV2();

  mpcClusterContentV2* addCluster(unsigned int i);
  
  mpcClusterContentV2* addCluster(unsigned int i, 
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
  void copy(mpcClusterContainerV2& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(mpcClusterContainerV2,1) // Array of mpcClusterContentV2
};
#endif
