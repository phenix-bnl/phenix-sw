#ifndef __SVXSTRIPCLUSTERQA_H__
#define __SVXSTRIPCLUSTERQA_H__

#include <SubsysReco.h>
#include <SvxParameters.h>
#include <PHTimeStamp.h>
#include <PHTimeServer.h>

//PHENIX classed
class PHCompositeNode;
class SvxClusterList; 
class SvxRawhitList;
class SvxRawhitClusterList ;
class SvxSegmentList; 
class RunHeader;
class SvxPixelHotDeadMap;
class SvxDeadMap; // strips

//ROOT classes
class TFile;
class TH1F;
class TH2F;
class TF1;
class TProfile;
class TGraphErrors;
class TTree;

class SvxStripClusterQA : public SubsysReco{

 public:
   SvxStripClusterQA(std::string filename = "SvxStripClusterQA");
   virtual ~SvxStripClusterQA();
    
   int Init(PHCompositeNode *topNode);
   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);
   int End(PHCompositeNode *topNode);
   inline int Reset(PHCompositeNode *topNode) 		{return 0;}
   inline int ResetEvent(PHCompositeNode *topNode) 	{return 0;}
  
  //    void set_ptcut(float pt) {ptcut=pt;}
  
 protected:
  
  //functions
   int CreateNodeTree(PHCompositeNode *topNode) {return 0;}
   bool init_histo();
   bool init_nodes(PHCompositeNode *topNode);
   void init_variables();
   void fill_histo();

   TFile* hstOutFile;
   std::string outfilename;

   //histo for QA
   //for HotDeadFlag
   TH1F* h1_rawhitflag[2];
   TH1F* h1_rawhitflagdiff[2];

   //for cluster QA
   TH2F* h2_cluster_zphi[2];
 
   //2X, 2U, 3X, 3U
   TH2F* h2_adc_barrel[2][2];
   TH2F* h2_size_barrel[2][2];
   TH2F* h2_cluster_hotsize_zphi_barrel[2][2];
 
   //DST data nodes

   RunHeader *run;
   SvxClusterList *svxcluslist;
   SvxSegmentList *svxtracks;
   SvxRawhitList  *svxrawhitlist;         ///< SVX raw hits
   SvxRawhitClusterList *svxrawhit2cluster; ///< SVX rawhit<->cluster relater
   SvxPixelHotDeadMap *m_pixelhotdead;   // Pixel HotDead Map
   SvxDeadMap         *m_striphotdead;   // Strip HotDead Map

   int nsvxrawhit;        ///< Number of SvxRawhit objects filled
   int nsvxtracks;
   int nsvxcluster;
   int runnumber;

   //variables for svxsegment
   int charge;
   //float mom,pt;
   //float p[3];// 3 momentum At innter most hit
   int nhit[4];
   float phi[4]; // atan2(y,x)
   float phib[4]; // atan2(y,-x);
   int   nseg;
   float eta;

   //vertex out info.
   float m_vtxout[3];
   float svxseed[3];//data from vtxout node
   float svxprim[3];//data from vtxout node

 private:
   
   //bool m_readbeamcenterFromDB;
   //float d_beamcenterFromDB_x;
   //float d_beamcenterFromDB_y;
   //int pixelchip_statusDB[480];


};
#endif //SVXSTRIPCLUSTERQA_H__
