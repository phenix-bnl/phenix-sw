#ifndef __SVXEFFICIENCYQA_H__
#define __SVXEFFICIENCYQA_H__

#include <SubsysReco.h>
#include <SvxClusterStabilityQA.h>
#include <vector>
#include <PHTimeStamp.h>
#include <PHTimeServer.h>

class EventHeader;
class PreviousEvent;
class PHGlobal;
class VtxOut;
class TrigLvl1;
class SvxClusterList;
class SvxRawhitList;
class SvxRawhitClusterList;
class SvxClusterStabilityQA;
class svxDetectorGeo;
class SvxSensor;
class SvxCluster;
class svxAddress;
class PHCentralTrack;
class RunHeader;
class PHPoint;
class SvxComponentGeom;
struct SvxGeoTrack;

///class SvxProjection;


class TH1I;
class TH1D;
class TH1F;
class TH2I;
class TH2F;

struct SvxProjection
{
  SvxProjection();
  ~SvxProjection(){};
  
  int ladder[4];
  int sensor[4];
  int chipSS[4];
  int tile[4];
};

struct SvxChip
{
  float transvector_corr[3];
  float rotMatrix[3][3];
};

struct SvxTile
{
  float transvector_corr[3];
  float rotMatrix[3][3];
};




class SvxEfficiencyQA: public SubsysReco
{

public:

  SvxEfficiencyQA() ;
  virtual ~SvxEfficiencyQA() {}

  int  Init(PHCompositeNode *topNode);
  int  process_event(PHCompositeNode *topNode);
  int  End(PHCompositeNode *topNode);

  int  Reset(PHCompositeNode *topNode)    {
    return 0;
  }
  int  ResetEvent(PHCompositeNode *topNode)   {
    return 0;
  }
  int  InitRun(PHCompositeNode *topNode);
  int  GetNodes(PHCompositeNode *topNode);
  bool TickCut();
  virtual bool EventSelection(PHCompositeNode *topNode, float bbcz, float bbcq);
  virtual void Set_TickCut(bool is) {
    is_tickcut = is;
  }
  virtual void Set_bbczcut(float cut) {
    m_bbczcut = cut;
  }
  virtual void Set_outname(std::string name) {
    outname = name;
  }
  virtual void Set_UseModeler(bool is) {
    is_usemodeler = is;
  }
  virtual void Set_UseInternalProjection(bool is) {
    is_useinternalproj = is;
  }
  
  
  SvxProjection Search_CNTProjectedChip(float primx,float primy,float primz, 
                               float mom,float phi0,float the0, int charge,float bDir);
private:
  bool is_usemodeler;//flag for SvxComponentGeom
  bool is_useinternalproj;// flag for projection function implemented in this class

  void Create_chipGeometry();
  void Create_tileGeometry();
  void Print_chipGeometry();
  
  //pixel ROC 0-7
  //strip 0-1 (L-R)
  int ComponentToPixelROC(int ilr,int ild,int isn,int component);
  int ComponentToStripSS(int ilr,int ild,int isn,int component);

  SvxChip m_chip[4][24][6][4];
  SvxTile m_tile[2][20][4][4][16];

  void Set_maxevents(int nruns);
  void Init_Hists(); //initialize the TProfiles that need to be in memory during processevent

  int Get_AssociatedID(int index_svxcls);
  
  //get chip id or SS id from local position of cluster
  //pixel chip 0-3 , strip SS 0-1 (0:left)
  int Get_chipSS(int ilr,float local_z);
  
  //get tile id from local position of cluster
  //it should be same as svxcomponentgeom
  int Get_tileID(int ilr, float local_x,float local_z);
  float Get_chipSSlocalz(int ilr,int chipSS,float local_z);

  void calc_dphidz(
                   float phi0,float the0, float pt, int charge, // track info
                   float x, float y, float z,        // cluster position or center of chip
                   float xvtx, float yvtx, float zvtx,        // prim. vertex position;
                   float bDir,                                // direction of B-field
                   float* dproj, float* magbend, float* zproj // "output"
                   );
  bool CNTTrackSelection(PHCentralTrack *d_cnt,unsigned int itrk);
  //
  // Run nodes
  //
  RunHeader* d_runheader;
  //
  // Data nodes for CM and PHENIX
  EventHeader*     d_eventhead;
  VtxOut*          d_vtxout;
  PreviousEvent*   d_peve;
  PHCentralTrack*  d_cnt;
  PHGlobal*        d_global;
  //
  // Data node for VTX
  svxAddress*         d_svxadr;
  SvxRawhitList*      d_svxraw;
  SvxClusterList*     d_svxcls;
  TrigLvl1*           d_trg;
  SvxRawhitClusterList* d_svxrawcls;
  svxDetectorGeo*    d_svxgeo;

  SvxComponentGeom* m_svxCompGeom; // Detector model for tracking and dead-area checking
  
  std::string outname;

  // variables
  //
  int   m_fieldScale;
  int   m_EventNumber;
  int   m_EventSeqNumber;
  int   m_pticks[3];
  bool  is_tickcut;
  float m_bbczcut;
  float m_bbcqcut;
  int   m_pixelsizecut;
  int   m_stripsizecut;

  float m_pc3dphi_emcdphioffset;
  float m_pc3dphi_emcdphicut;
  float m_pc3dz_emcdzoffset;
  float m_pc3dz_emcdzcut;
  float m_dproj_cut;
  float m_zproj_cut;
  // saved histograms
  TH1D* m_hzbbc;
  TH1I* m_htrigL1;
  TH1I* m_htrignarrow;
  TH1I* m_htrignarrow_copyA;
  TH1I* m_htrignarrow_copyB;
  TH1I* m_htrignovtx;
  TH2F* m_hzbbc_vs_evtseq;
  TH2F* m_hzbbc_vs_evtseq_selection;

  static const int nbinevtseq = 50;
  static const int binwidth = 100000;
  //for numerator of the efficiency
//  TH2F* pixel_map[2][20][4][4];//layer ladder sensor chip(0-4)
  TH2F* pixel_map_cnt[2][20][4][4];//layer ladder sensor chip(0-4)
//  TH2F* strip_map[2][24][6][2];//layer ladder sensor LorR
  TH2F* strip_map_cnt[2][24][6][2];//layer ladder sensor LorR
  // these are tempolary 
  TH2I* pixel_bbcq_vs_nhit[2][20][4][4];
  TH2I* strip_bbcq_vs_nhit[2][24][6][2];

  //residual btw projection point and cluster in the search window
  TH1F* h1_dz_pixelchip[2][20][4][4]; 
  TH1F* h1_dphi_pixelchip[2][20][4][4]; 
  TH1F* h1_dz_stripss[2][24][6][2];
  TH1F* h1_dphi_stripss[2][24][6][2];
  //residual btw projection point and center of chip or SS
  TH1F* h1_dz_pixelchip_pro[2][20][4][4]; 
  TH1F* h1_dphi_pixelchip_pro[2][20][4][4]; 
  TH1F* h1_dz_stripss_pro[2][24][6][2];
  TH1F* h1_dphi_stripss_pro[2][24][6][2];

  TH1F* h1_dz_pixelchip_pro_modeler[2][20][4][4]; 
  TH1F* h1_dphi_pixelchip_pro_modeler[2][20][4][4]; 
  TH1F* h1_dz_stripss_pro_modeler[2][24][6][2];
  TH1F* h1_dphi_stripss_pro_modeler[2][24][6][2];

  TH1I* h1_noprojection_layer;

  //tile geometry, same as SvxComponentGeom (not yet implemented)
  TH1F* h1_dz_pixeltile[2][20][4][4][16];
  TH1F* h1_dphi_pixeltile[2][20][4][4][16];
  TH1F* h1_dz_striptile[2][24][6][2][16];
  TH1F* h1_dzphi_striptile[2][24][6][2][16];
  
  //QA plot for checking the acceptance
  TH2F* h2_cntcluster_zphi[4];
  TH2F* h2_cntzedphi0;
  TH2F* h2_cntzedphi;
  
  //for denominator of the efficiency
  TH1F* h1_projected_pixel;
  TH1F* h1_projected_strip;
  TH2F* h2_projected_zphi[4];

  //check modeler
  TH2F* h2_check_modeler_B0L0S1;
  TH2F* h2_check_modeler_B0L5S1;
  TH2F* h2_check_modeler_B2L3S2;
  TH2F* h2_check_modeler_B2L10S2;
  TH2F* h2_check_modeler_B3L3S2;
  TH2F* h2_check_modeler_B3L20S2;

  PHTimeServer::timer timer;
};

#endif
