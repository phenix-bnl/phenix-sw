#ifndef TECCLSUTERCONTAINERV1_h
#define TECCLSUTERCONTAINERV1_h

#include <TecClusterContainer.hh>
#include <iostream>

#define TECMAXNUMCLUSTERS 100

class TecClusterV1;

class TecClusterContainerV1: public TecClusterContainer {

public:

///
  TecClusterContainerV1();
///
  virtual ~TecClusterContainerV1();

///
  void Reset();
///
  void Clear(Option_t *option = "");
///
  void identify(std::ostream& os = std::cout) const;
///
  int isValid () const;
///
  int AddTecCluster(TecClusterV1 &source);
///
    TClonesArray *GetTecClusters() const {return TecClusters;}
///
    int getNClusters() const {return NumberOfClusters;}

protected:

  int NumberOfClusters;
/// List of Tec Clusters
  TClonesArray *TecClusters;

  ClassDef(TecClusterContainerV1,1)

};

#endif


