#ifndef __SVXPRIVERTEXSEEDFINDER_H__
#define __SVXPRIVERTEXSEEDFINDER_H__

#include <SubsysReco.h>
#include <PHTimeServer.h>

class PHCompositeNode;
class TH1F;

///////////////////////////////
// modify : add flag in the constructor for resolution study 2011/Dec/12 T.Hachiya
// modify : add flag ncluster threshod and function of getting flag   2016/Mer/3 K.Nagashima

class SvxPriVertexSeedFinder : public SubsysReco
{
public:

  SvxPriVertexSeedFinder(const std::string &name = "SVXPRIVERTEXFINDER", const int flag = 0);
  virtual ~SvxPriVertexSeedFinder();
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  void set_UseSvxVtxOut(bool t) { m_use_SvxVtxOut = t; }
  void setNclstrThrshld(int index, int nclus); // index=0 for bbcq>200,  index=1 for bbcq<200

protected:

  int CreateNodeTree(PHCompositeNode *topNode);
  float  m_bbcz;
  int    m_nclscmb[2];
  int    m_nassestrk[2];
  //float  m_privtxz[2];//unused?
  float  m_mean[2];
  int    m_side; // 0: all, 1: west, 2: east
  TH1F  *m_h_vtxseed;
 
  int    getIndexThreshold(float bbcq); // 0: bbcq>200,  1: bbcq<200
  int    m_nclstr_thrshld[2]; // ncluster threshold

  bool   m_use_SvxVtxOut;

  PHTimeServer::timer _timer; ///< Timer
};
#endif
