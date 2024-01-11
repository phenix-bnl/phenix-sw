#ifndef _PhPhotonList_h_
#define _PhPhotonList_h_

#include "PhPhotonSngl.h"
#include "emcClusterContainer.h"

class TClonesArray;

class PhPhotonList : public emcClusterContainer
{
public:

  PhPhotonList();
  PhPhotonList(const PhPhotonList&);
  PhPhotonList& operator=(const PhPhotonList&);

  unsigned int capacity(void) const;

  PhPhotonList* clone(void) const;

  PhPhotonList* create(void) const;

  virtual ~PhPhotonList();

  PhPhotonSngl* addCluster(unsigned int i);
  
  PhPhotonSngl* addCluster(unsigned int i, 
				  const emcClusterContent& c);

  PhPhotonSngl* findCluster(int clusterid) const;

  PhPhotonSngl* getCluster(unsigned int i) const;

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  bool removeCluster(unsigned int i) ;

  void Reset();

  bool resize(unsigned int newsize);

  unsigned int size(void) const;

protected:
  TClonesArray *GetTCArray() const {return fEmcClusters;}
  TClonesArray* fEmcClusters;

private:
  void allocate(unsigned int thesize);
  void copy(PhPhotonList& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(PhPhotonList,1)
};
#endif
