#ifndef HbdWisClusterList_h
#define HbdWisClusterList_h

#include <PHObject.h>
#include <TClonesArray.h>
#include "HbdCluster.h"
#include "HbdClusterList.h"
#include "HbdWisCluster.h"
#include <memory>


class HbdWisClusterList : public HbdClusterList {
  public:
    HbdWisClusterList() :
      nClusters(0),
      clusters(new TClonesArray("HbdWisCluster", CLUST_SIZE)) {};

    ~HbdWisClusterList() {
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
      os << "identify yourself: HbdWisCluster Object\n"
        << "No of clusters: " << nClusters << std::endl;
    }

    const HbdWisCluster* addCluster(const HbdWisCluster& cluster) {
      return new((*clusters)[nClusters++]) HbdWisCluster(cluster);
    }

    const HbdWisCluster* getCluster(size_t i) const {
      return dynamic_cast<HbdWisCluster*>(clusters->UncheckedAt(i));
    }

    HbdWisClusterList* clone() const {
      return new HbdWisClusterList(*this);
    }

    HbdWisClusterList(const HbdWisClusterList& rhs) :
      nClusters(rhs.nClusters),
      clusters(new TClonesArray(*(rhs.clusters.get()))) {}

  private:
    size_t nClusters;
    std::auto_ptr<TClonesArray> clusters;

    ClassDef(HbdWisClusterList, 0);
};

#endif
