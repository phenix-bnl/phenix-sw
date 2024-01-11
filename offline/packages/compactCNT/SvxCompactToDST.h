#ifndef __SVXCOMPACTTODST_H__
#define __SVXCOMPACTTODST_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class SvxCluster;
class SvxSegment;
class SvxCentralTrack;
class svxDetectorGeo;


class SvxCompactToDST: public SubsysReco 
{
public:
  SvxCompactToDST(const std::string& name="SvxCompactToDST");
  virtual ~SvxCompactToDST();
  
  // Standard Fun4All functions
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  inline int Reset(PHCompositeNode *topNode) 		{return 0;}
  inline int ResetEvent(PHCompositeNode *topNode) 	{return 0;}


  void setTest(const bool flag) { m_test  = flag; }
  void setTestSelectedCluster(const bool flag) { m_testSelClus  = flag; }
  void setPrint(const bool flag){ m_print = flag; }


  void skipRecoverCluster(const bool flag) { m_skipCluster = flag; }

protected:
  int CreateNodeTree(PHCompositeNode* topNode);
  int FillSvxCentralTrack(PHCompositeNode* topNode, int flag=0); // flag=0: normal, =1: for backlist
  int FillSvxTrack(PHCompositeNode* topNode);
  int FillSvxCluster(PHCompositeNode* topNode);

  bool CompareSvxTrackList(PHCompositeNode* topNode);
  bool CompareSvxTrack(SvxSegment *sorg, SvxSegment* snew);
  bool CompareSvxClusterList(PHCompositeNode* topNode);
  bool CompareSvxCluster(SvxCluster *sorg, SvxCluster* snew);
  bool CompareSvxCentralTrackList(PHCompositeNode* topNode, int flag=0);
  bool CompareSvxCentralTrack(SvxCentralTrack *sorg, SvxCentralTrack* snew);
  
  bool compareInt(int orgval, int newval, const char *err);
  bool compareFloat(float orgval, float newval, float judge, const char *err);

  // int get_Ladder(float gx,float gy,float gz,int layer);
  // int get_Sensor(float gx,float gy,float gz,int layer,int ladder);

private:
  bool fNewGeo;
  svxDetectorGeo* fGeo;

  int  m_event;

  bool m_skipCluster;

  bool m_test;
  bool m_testSelClus;
  bool m_print;
  int have_svx_data;

//  int  m_nBadCluster;

};
#endif
