#ifndef __SVXSELECTCLUSTERS_H__
#define __SVXSELECTCLUSTERS_H__

#include <SubsysReco.h>

#include <phool.h>

#include <vector>

class PHSnglCentralTrack;
class SvxCentralTrack;
class SvxClusterContainer;
class SvxClusterList;

class SvxSelectClusters : public SubsysReco
{

 public:

  SvxSelectClusters(const std::string &name = "SVXSELECTCLUSTERS");
  virtual ~SvxSelectClusters();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void setSelectMode(const int mode){ m_selectmode = mode; }

  void set_ZCut(const int icent, float a)   {if(0<=icent&&icent<3) ZCut[icent]  =a;}
  void set_PhiCut(float a)        {PhiCut           =a;}
  void set_Z_reduction(float a)   {Z_reduction      =a;}
  void set_Phi_reduction(float a) {Phi_reduction    =a;}
  void set_whichVertex(int a)     {m_vtxflag        =a;}
  void set_select_e(int a)        {m_select_e       =a;}
  void set_select_e_n0cut(int a)  {m_select_e_n0cut =a;}
  void set_select_e_eopcut(int a) {m_select_e_eopcut=a;}

 private:
  int   m_selectmode; // 0: default(nearby-cluster), 1: centrality (nearby for bbcq>=200 and all for bbcq<200)

  int   EventNumber;
  float ZCut[3];
  float PhiCut;
  float MomCut;
  float Z_reduction;
  float Phi_reduction;

  int fillSelectedClusters(PHCompositeNode *topNode);
  int fillAllClusters(PHCompositeNode *topNode);

  void calculate_dphidz(
                   float the0, float pt, float ux, float uy, int charge, // track info
                   float svxx, float svxy, float svxz,        // cluster position
                   float xvtx, float yvtx, float zvtx,        // prim. vertex position;
                   float bDir,                                // direction of B-field
                   float* dproj, float* magbend, float* zproj // "output"
                       );

  int   m_vtxflag;
  int   m_fieldScale;
  int   m_select_e;
  int   m_select_e_n0cut;
  float m_select_e_eopcut;

  void find_nearbyhit(PHSnglCentralTrack *track, SvxCentralTrack* cnttrk, 
                      SvxClusterContainer *clscont,
                      float phi, float the, 
                      float xvtx, float yvtx, float zvtx,
                      int icent, // 0,1,2
                      std::vector<int>& selectedclus, std::vector<int>& selectedclusdup);

  int check_associated_in_selected(
        SvxClusterList *d_svx,
        std::vector<int>& assocclus, std::vector<int>& selectedclus);

};
#endif 


