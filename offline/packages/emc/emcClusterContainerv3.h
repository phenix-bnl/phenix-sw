#ifndef __emcClusterContainerv3_h__
#define __emcClusterContainerv3_h__

#include "emcClusterContentv3.h"
#include "emcClusterContainer.h"

class TClonesArray;

/** (VERSION) Container of emcClusterContentv3. */

class emcClusterContainerv3 : public emcClusterContainer
{
public:

  emcClusterContainerv3();
  emcClusterContainerv3(const emcClusterContainerv3&);
  emcClusterContainerv3& operator=(const emcClusterContainerv3&);

  unsigned int capacity(void) const;

  emcClusterContainerv3* clone(void) const;

  emcClusterContainerv3* create(void) const;

  virtual ~emcClusterContainerv3();

  emcClusterContentv3* addCluster(unsigned int i);
  
  emcClusterContentv3* addCluster(unsigned int i, 
				  const emcClusterContent& c);

  emcClusterContentv3* findCluster(int clusterid) const;

  emcClusterContentv3* getCluster(unsigned int i) const;

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  bool removeCluster(unsigned int i) ;

  void Reset();

  bool resize(unsigned int newsize);

  unsigned int size(void) const;

protected:

  TClonesArray* fEmcClusters;

private:
  void allocate(unsigned int thesize);
  void copy(emcClusterContainerv3& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(emcClusterContainerv3,1) // Array of emcClusterContentv3
};
#endif
