#ifndef __KEvent__
#define __KEvent__

#include <cstdio>
#include "KCluster.h"
#include "Combination.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "mEmcGeometryModule.h"

#include <vector>
#include <algorithm>
using namespace std;

#define MAX_NCLUSTER 1024

typedef vector<Combination *> CombinationVector;

struct DeleteObject {

  template <typename T>
  void operator() (const T* ptr) const 
  {
    delete ptr;
  }
};


class KEvent {
 public:

  KEvent();
  virtual ~KEvent();

  vector <KCluster *> clusterVector;                // Cluster Array

  int      centralityBin;
  int      g_multiplicity;             // Number of clusters stored in the class as gamma
  int      c_multiplicity;             // Number of clusters in the event including rejected cluster
  
  void     add_multiplicity();         // Inclement the multiplicity
  void     clear();

  int      getEvent( emcClusterContainer *emccont, 
		     float bbcT0, float bbcVz, 
		     mEmcGeometryModule *EmcGeo, THmulf *tofc,
		     THmulf *toftower, THmulf *tower,
		     THmulf *hcluster,
		     THmulf *ecompactness, THmulf *padisp_ratio);


  float  getEtByMease();             // Calc total Et by mease and return
  
  KCluster * cluster_element(const unsigned int n);

 private:

};

#endif


