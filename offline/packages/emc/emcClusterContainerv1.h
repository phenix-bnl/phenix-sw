#ifndef __emcClusterContainerv1_h__
#define __emcClusterContainerv1_h__

#include "emcClusterContentv1.h"
#include "emcClusterContainer.h"

class TClonesArray;

/** (VERSION) Container of emcClusterContentv1.
 */

class emcClusterContainerv1 : public emcClusterContainer
{
public:

  emcClusterContainerv1();
  emcClusterContainerv1(const emcClusterContainerv1&);
  emcClusterContainerv1& operator=(const emcClusterContainerv1&);

  unsigned int capacity(void) const;

  emcClusterContainerv1* clone(void) const;

  emcClusterContainerv1* create(void) const;

  virtual ~emcClusterContainerv1();

  emcClusterContentv1* addCluster(unsigned int i);
  
  emcClusterContentv1* addCluster(unsigned int i, 
				  const emcClusterContent& c);

  emcClusterContentv1* findCluster(int clusterid) const;

  emcClusterContentv1* getCluster(unsigned int i) const;

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
  void copy(emcClusterContainerv1& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(emcClusterContainerv1,1) // Array of emcClusterContentv1
};
#endif
