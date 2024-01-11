#ifndef HbdClusterList_h
#define HbdClusterList_h

#include <PHObject.h>
#include <TClonesArray.h>
#include "HbdCluster.h"
#include <memory>

const size_t CLUST_SIZE = 3000;

class HbdClusterList : public PHObject {
  public:
    HbdClusterList() :
      nClusters(0),
      clusters(new TClonesArray("HbdCluster", CLUST_SIZE)) {};

    ~HbdClusterList() {
      clusters->Delete();
    }

    virtual int set_TClonesArraySize(const unsigned int nclus) {
      if (nclus>CLUST_SIZE)
        clusters->Expand(nclus);
      return nclus;
    }

    void Reset() {
      clusters->Delete();
      if (nClusters>CLUST_SIZE)
        clusters->Expand(CLUST_SIZE);
      nClusters = 0;
    }

    int isValid() const {
      return nClusters>0;
    }

    void identify(std::ostream &os=std::cout) const {
      os << "identify yourself: HbdCluster Object\n"
        << "No of clusters: " << nClusters << std::endl;
    }

    const HbdCluster* addCluster(const HbdCluster& cluster) {
      return new((*clusters)[nClusters++]) HbdCluster(cluster);
    }

    const HbdCluster* getCluster(size_t i) const {
      return dynamic_cast<HbdCluster*>(clusters->UncheckedAt(i));
    }

    HbdClusterList* clone() const {
      return new HbdClusterList(*this);
    }

    HbdClusterList(const HbdClusterList& rhs) :
      nClusters(rhs.nClusters),
      clusters(new TClonesArray(*(rhs.clusters.get()))) {}

  private:
    size_t nClusters;
    std::auto_ptr<TClonesArray> clusters;

    ClassDef(HbdClusterList, 0);
};

#endif
