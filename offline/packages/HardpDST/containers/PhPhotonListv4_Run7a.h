#ifndef _PhPhotonListv4_Run7a_h_
#define _PhPhotonListv4_Run7a_h_

#include "PhPhotonSnglv2.h"
#include "emcClusterContainer.h"

class TClonesArray;

class PhPhotonListv4_Run7a : public emcClusterContainer
{
public:

  PhPhotonListv4_Run7a();
  PhPhotonListv4_Run7a(const PhPhotonListv4_Run7a&);
  PhPhotonListv4_Run7a& operator=(const PhPhotonListv4_Run7a&);

  unsigned int capacity(void) const;

  PhPhotonListv4_Run7a* clone(void) const;

  PhPhotonListv4_Run7a* create(void) const;

  virtual ~PhPhotonListv4_Run7a();

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

  void set_eventFilterType (const short trigtype) 
    {evtFilterType = trigtype; return;}
  unsigned short get_eventFilterType () 
    {return evtFilterType;}


protected:
  TClonesArray *GetTCArray() const {return fEmcClusters;}
  TClonesArray* fEmcClusters;

private:
  void allocate(unsigned int thesize);
  void copy(PhPhotonListv4_Run7a& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  unsigned short evtFilterType;

  ClassDef(PhPhotonListv4_Run7a,1)
};
#endif
