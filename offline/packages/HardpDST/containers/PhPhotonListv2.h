#ifndef _PhPhotonListv2_h_
#define _PhPhotonListv2_h_

#include "PhPhotonSnglv2.h"
#include "emcClusterContainer.h"

class TClonesArray;

class PhPhotonListv2 : public emcClusterContainer
{
public:

  PhPhotonListv2();
  PhPhotonListv2(const PhPhotonListv2&);
  PhPhotonListv2& operator=(const PhPhotonListv2&);

  unsigned int capacity(void) const;

  PhPhotonListv2* clone(void) const;

  PhPhotonListv2* create(void) const;

  virtual ~PhPhotonListv2();

  PhPhotonSnglv2* addCluster(unsigned int i);
  
  PhPhotonSnglv2* addCluster(unsigned int i, 
				  const emcClusterContent& c);

  PhPhotonSnglv2* findCluster(int clusterid) const;

  PhPhotonSnglv2* getCluster(unsigned int i) const;

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
  void copy(PhPhotonListv2& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(PhPhotonListv2,1)
};
#endif
