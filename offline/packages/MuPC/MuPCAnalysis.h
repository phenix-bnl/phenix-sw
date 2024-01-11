#ifndef __MUPCANALYSIS_H__
#define __MUPCANALYSIS_H__


class PHCompositeNode;
class MuPCCluster;
class mupcghitWrapper;

class MuPCAnalysis
{
 public:
  MuPCAnalysis();
  virtual ~MuPCAnalysis() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode);

 protected:
  //DataNodes  -- ala Akiba...
  MuPCCluster     *mupc1cluster;
  MuPCCluster     *mupc2cluster;
  MuPCCluster     *mupc3cluster;

  mupcghitWrapper* mupc1ghit;
  mupcghitWrapper* mupc2ghit;
  mupcghitWrapper* mupc3ghit;

  // Utility to find "our" Nodes.
  int GetNodes(PHCompositeNode *topNode);

  void ancestor(int true_track, float& ptot, float& ptheta,
                           float& pphi, float& r_vertex, float& z_vertex,
                           int& idparent, int& idpart, int& idorigin,
                           float& ptotpri, float& pthetpri, float& pphipri,
                           float& z0vertex);
  void fillMuPCCluster(mupcghitWrapper* mupcghit, MuPCCluster* mupccluster);
  void fillAllCluster();

};

#endif /* __MUPCANALYSIS_H__ */
