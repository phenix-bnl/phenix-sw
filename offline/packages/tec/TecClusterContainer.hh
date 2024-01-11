#ifndef TECCLUSTERCONTAINER_h
#define TECCLUSTERCONTAINER_h

#include <iostream>
#include "PHObject.h"
#include "TecCluster.hh"
#include "TClonesArray.h"

//class TClonesArray;

class TecClusterContainer: public PHObject {

 public:
///
  virtual ~TecClusterContainer() {}
///
  virtual void Reset();
///
  virtual int isValid() const;
///
  virtual TClonesArray *GetTecClusters() const {return 0;}
///
  virtual int AddTecCluster() {return -1;}
///
  virtual int AddTecCluster(TecCluster &source) {return -1;}
///
  virtual void AddTecCluster(const unsigned int itrk) {return;}
///
  virtual TecCluster* getTecCluster(const unsigned int i) const;
///
  virtual int getNClusters()  const {return -1;}
///
 virtual void identify(std::ostream& os = std::cout) const;

  virtual int set_TClonesArraySize (const unsigned int ntrk) {return -1;}

  ClassDef(TecClusterContainer,1)
};

#endif
