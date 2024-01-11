#ifndef __emcClusterContainerv2_h__
#define __emcClusterContainerv2_h__

#include "emcClusterContentv2.h"
#include "emcClusterContainer.h"

class TClonesArray;

/** (VERSION) Container of emcClusterContentv2.
 */

class emcClusterContainerv2 : public emcClusterContainer
{
public:

  emcClusterContainerv2();
  emcClusterContainerv2(const emcClusterContainerv2&);
  emcClusterContainerv2& operator=(const emcClusterContainerv2&);

  unsigned int capacity(void) const;

  emcClusterContainerv2* clone(void) const;

  emcClusterContainerv2* create(void) const;

  virtual ~emcClusterContainerv2();

  emcClusterContentv2* addCluster(unsigned int i);
  
  emcClusterContentv2* addCluster(unsigned int i, 
				  const emcClusterContent& c);

  emcClusterContentv2* findCluster(int clusterid) const;

  emcClusterContentv2* getCluster(unsigned int i) const;

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
  void copy(emcClusterContainerv2& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(emcClusterContainerv2,1) // Array of emcClusterContentv2
};
#endif
