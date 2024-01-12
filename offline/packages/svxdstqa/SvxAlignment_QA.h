#ifndef __SVXALIGNMENT_QA_H__
#define __SVXALIGNMENT_QA_H__

#include <SubsysReco.h>
#include <SvxParameters.h>
#include <PHTimeStamp.h>
#include <PHTimeServer.h>

//PHENIX classed
class Fun4AllHistoManager;
class PHCompositeNode;
class PHCentralTrack;
class SvxClusterList; 
class SvxSegmentList; 
class SvxBeamCenterPar;
class RunHeader;
class BbcOut;
class VtxOut;
class PHPoint;

//ROOT classes
class TFile;
class TH1F;
class TH2F;
class TF1;
class TProfile;
class TGraphErrors;
class TTree;


class SvxAlignment_QA : public SubsysReco{
 public:
   enum {N_LAYER=4, N_LADDER=24, N_EW=2};

 public:
   SvxAlignment_QA(std::string filename="svxalign.root");
   virtual ~SvxAlignment_QA();
    
   int Init(PHCompositeNode *topNode);
   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);
   int End(PHCompositeNode *topNode);
   inline int Reset(PHCompositeNode *topNode) 		{return 0;}
   inline int ResetEvent(PHCompositeNode *topNode) 	{return 0;}
  
   void set_WestBeamCenter(float xwest,float ywest); 
   void set_EastBeamCenter(float xeast,float yeast); 
  //    void set_ptcut(float pt) {ptcut=pt;}
  
 protected:
  
  //functions
   int CreateNodeTree(PHCompositeNode *topNode) {return 0;}
   bool init_histo();
   bool init_nodes(PHCompositeNode *topNode);
   void init_variables();
   void fill_histo();

   Fun4AllHistoManager *hm;
//   TFile* hstOutFile;
//   std::string outfilename;

   //histo for QA
   //external alignment
   TH1F* h1_dz_vtx_ladder_EW[N_LAYER][N_LADDER][N_EW]; //Fill(dz_vtx);
   TH1F* h1_dphi_ladder_EW[N_LAYER][N_LADDER][N_EW];    //Fill(dphi);
   TH2F* h2_dphi_vs_z_ladder[N_LAYER][N_LADDER];        // Fill(svxz,dphi);
   TH2F* h2_dphi_vs_phiv_ladder[N_LAYER][N_LADDER];     // Fill(gc_phiv,dphi);
   TH2F* h2_dz_zvtx_vs_dphi_ladder[N_LAYER][N_LADDER];
  
   //internal 
   TH1F* h1_dca0_EW[N_EW];//dca from beam center
   TH1F* h1_dca0_ladder[N_LAYER][N_LADDER];//dca from beam center
   TH1F* h1_dproj_ladder[N_LAYER][N_LADDER];//dca from beam center
   TH2F* h2_dca0_vs_phi_ladder[N_LAYER][N_LADDER];//dca from beam center
   TH2F* h2_dproj_vs_phi_ladder[N_LAYER][N_LADDER];//dca from beam center
   TH2F* h2_dca0_vs_phi_EW[N_EW];//dca from beam center
   
   TH1F* h1_dca0p_EW[N_EW];//dca from primary
   TH1F* h1_dcaz_EW[N_EW];//dcaz from primary
   TH1F* h1_dca0p_ladder[N_LAYER][N_LADDER];//dca from primary
   TH2F* h2_dca0p_vs_phi_ladder[N_LAYER][N_LADDER];//dca from primary
   TH2F* h2_dca0p_vs_phi_EW[N_EW];//dca from primary
   TH2F* h2_dcaz_vs_phi_EW[N_EW];//dca from primary


   //DST data nodes
   BbcOut *bbc; 
   SvxClusterList *svxcluslist;
   SvxSegmentList *svxtracks;
   PHCentralTrack  *trk;
   VtxOut *vtxout;          
   SvxBeamCenterPar *beamcenter;

   //variables for svxsegment
   int charge;
   float mom,pt;
   float p[3];// 3 momentum At innter most hit
   int nhit[4];
   int nsvxtracks;
   float bbcz;
   float bbcq;
   float phi[4]; // atan2(y,x)
   float phib[4]; // atan2(y,-x);
   float bphi; //track angle determined by L0 and L1 layer
   float bphib;
   float dca0; //DCA from beam center.it is calculated by hits on B0 and B1 layer
   float dca0p; //DCA from primary vertex.it is calculated by hits on B0 and B1 layer
   float dcaz ;//DCAz from primary vertex
   float dproj2; 
   float dproj3;
   float mag_bend2;
   float mag_bend3;

   float magnetic_bend;// takes into account beam center position
   float magnetic_bendp;// takes into account primary vertex position
   int ladder[4],sensor[4];

   //central info.
   static const int MAXTRACK =10000;
   float  gl_tr_mom[MAXTRACK];
   float  gl_tr_phi0[MAXTRACK];//phi angle at the vertex
   float  gl_tr_the0[MAXTRACK];//theta angle at the vertex
   float  gl_tr_pt[MAXTRACK];
   float  gl_tr_charge[MAXTRACK];
   float  gl_tr_emcdz[MAXTRACK];
   float  gl_tr_emcdphi[MAXTRACK];
   float  gl_tr_pc3dz[MAXTRACK];
   float  gl_tr_pc3dphi[MAXTRACK];
   float  gl_tr_pc2dz[MAXTRACK];
   float  gl_tr_pc2dphi[MAXTRACK];
   float  gl_tr_zed[MAXTRACK];
   int  gl_tr_trk_quality[MAXTRACK];
   float gl_tr_n0[MAXTRACK]; 
   float gl_tr_ecore[MAXTRACK]; 

   int    gl_ntrk;
   float  gl_mom;
   float  gl_phi0;
   float  gl_the0;
   float  gl_pt;
   float  gl_charge;
   float  gl_emcdz;
   float  gl_emcdphi;
   float  gl_pc3dz;
   float  gl_pc3dphi;
   float  gl_pc2dz;
   float  gl_pc2dphi;
   float  gl_zed;
   float  gl_itrack;
   int  gl_trk_quality;
   float  gl_n0;
   float  gl_ecore;

   int  gc_ilayer; 
   int  gc_iladder;
   float gc_zproj;
   float gc_dproj;
   float gc_mag_bend;
   float gc_svxx;//svx cluster's global position within search window
   float gc_svxy;//svx cluster's global position within search window
   float gc_svxz;//svx cluster's global position within search window
   float gc_phiv;
   float gc_zvtx;

   //per-event info
   float gc_vtxout[3];//data from vtxout node
   float svxprim[3];//data from vtxout node

 private:
   
   int m_fieldScale;
   float xbeamcenter_west,ybeamcenter_west;
   float xbeamcenter_east,ybeamcenter_east;
   float bbcqcut_up;
   float bbcqcut_low;
   float bbczcut;
   bool m_readbeamcenterFromDB;
   int runnumber;

   float pc3dphi_emcdphioffset;
   float pc3dphi_emcdphicut;
   float pc3dz_emcdzoffset;
   float pc3dz_emcdzcut;
   float dproj_cut;
   float zproj_cut;

};
#endif //SVXALIGNMENT_QA_H__
