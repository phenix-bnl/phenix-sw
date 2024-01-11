#ifndef __emcClusterContainerv4_h__
#define __emcClusterContainerv4_h__

#include "emcClusterContentv4.h"
#include "emcClusterContainer.h"

class TClonesArray;

/** (VERSION) Container of emcClusterContentv4. */

class emcClusterContainerv4 : public emcClusterContainer
{
public:

  emcClusterContainerv4();
  emcClusterContainerv4(const emcClusterContainerv4&);
  emcClusterContainerv4& operator=(const emcClusterContainerv4&);

  unsigned int capacity(void) const;

  emcClusterContainerv4* clone(void) const;

  emcClusterContainerv4* create(void) const;

  virtual ~emcClusterContainerv4();

  emcClusterContentv4* addCluster(unsigned int i);
  
  emcClusterContentv4* addCluster(unsigned int i, 
				  const emcClusterContent& c);

  emcClusterContentv4* findCluster(int clusterid) const;

  emcClusterContentv4* getCluster(unsigned int i) const;

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
  void copy(emcClusterContainerv4& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(emcClusterContainerv4,1) // Array of emcClusterContentv4
};
#endif
