#ifndef SVXDSTQA_PRODUCTIONHISTOS__
#define SVXDSTQA_PRODUCTIONHISTOS__

#include <SubsysReco.h>

//PHENIX classes
class Fun4AllHistoManager;
class PHCompositeNode;
class PHCentralTrack;
class SvxClusterList;
class SvxSegmentList;
class SvxBeamCenterPar;
class SvxPixelHotDeadMap;
class EventHeader;
class SvxCentralTrackList;
class RunHeader;
class BbcOut;
class VtxOut;
class PHPoint;
class TrigLvl1;
class SvxQAEventSelection;


//ROOT classes
class TFile;
class TH1F;
class TH2F;
class TH1I;
class TH2I;

class SvxQAForProduction : public SubsysReco{
 public:
  enum {N_PTBIN=12, N_HITBIN=6, N_BBCZBIN=10};

 public:
   SvxQAForProduction(std::string filename="svxqa.root");
   virtual ~SvxQAForProduction();

   int Init(PHCompositeNode *topNode);
   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);
   int End(PHCompositeNode *topNode);

   //bool EventSelection(PHCompositeNode *topNode,float bbcz);
   void set_BeamCenter(float xwest,float ywest);

 protected:

  //functions
   bool init_histo();
   bool init_nodes(PHCompositeNode *topNode);
   bool init_nodes_event(PHCompositeNode *topNode);
   void init_variables();
   void fill_histo();

   Fun4AllHistoManager *hm;

   //histo for QA
   //zvtx
   TH1F* h_zvtx;
   TH1F* h_bbcq;
   TH1F* h_bbcq_zcut;
   TH1F* h_seedz_when_primisnan;
   TH1F* h_bbcz_minus_seedz;
   TH1F* h_bbcz_minus_primz;
   TH1F* h_seedz_minus_primz;
   TH2F* h_seedz_vs_bbcz;
   TH2F* h_primz_vs_bbcz;
   TH2F* h_primz_vs_seedz;

   //beam center check
   TH1F* h_primx;
   TH1F* h_primy;
   TH1F* h_primx_minus_dbcenter;
   TH1F* h_primy_minus_dbcenter;
   TH2F* h_primxy2d_minus_dbcenter;
   TH1F* h_EWoffset_x;
   TH1F* h_EWoffset_y;
   TH1F* h_EWoffset_z;

   //for segment QA
   TH1F* h_nsegment_bbcqbin;
   TH1F* h_nprimary_bbcqbin;
   TH1F* h_nseed_bbcqbin;

   // zcut is applied.
   // to from here
   TH1F* h_nsegment_bbcqbin_zcut;
   TH1F* h_nprimary_bbcqbin_zcut;
   TH1F* h_nseed_bbcqbin_zcut;

   TH1F* h_seg_3hitprobability;
   TH1F* h_seg_4hitprobability;
   TH1F* h_seg_3hitprobability_pt[N_PTBIN];
   TH1F* h_seg_4hitprobability_pt[N_PTBIN];
   TH1F* h_seg_pt;
   TH1F* h_seg_pz;

   // to here
   //svxcnt
   TH1F* h_nsvxc_bbcqbin;
   TH2F* h_nsvxc_vs_ncnt;

   TH1F* h_svxc_chi2_E[N_HITBIN];//nhit
   TH1F* h_svxc_chi2_W[N_HITBIN];//nhit
   TH1F* h_svxc_chi2_pt_E[N_HITBIN][N_PTBIN];//nhit pt
   TH1F* h_svxc_chi2_pt_W[N_HITBIN][N_PTBIN];//nhit pt
   TH1F* h_svxc_dca2d_pt_E[N_PTBIN];
   TH1F* h_svxc_dca2d_pt_W[N_PTBIN];
   TH1F* h_svxc_dcaz_pt_E[N_PTBIN];
   TH1F* h_svxc_dcaz_pt_W[N_PTBIN];
   TH1F* h_svxc_dca2dp_pt_E[N_PTBIN];
   TH1F* h_svxc_dca2dp_pt_W[N_PTBIN];
   TH1F* h_svxc_dcazp_pt_E[N_PTBIN];
   TH1F* h_svxc_dcazp_pt_W[N_PTBIN];
   TH1F* h_svxc_nhit;


   //cnt check
   TH1F* m_hpt_goodcnt_west;
   TH1F* m_hpt_goodcnt_east;
   TH1I* m_hquality_cnt_west;
   TH1I* m_hquality_cnt_east;
   TH1F* m_hemcdphi_west;
   TH1F* m_hemcdphi_east;
   TH1F* m_hemcdz_west;
   TH1F* m_hemcdz_east;
   TH1F* m_hpc3dphi_west;
   TH1F* m_hpc3dphi_east;
   TH1F* m_hpc3dz_west;
   TH1F* m_hpc3dz_east;

   //cluster check
   TH2F* h2_pixelcluster[2];
   TH2F* h2_stripcluster[2];

   TH2I* pixel_bbcq_vs_nhit[2][20][4][4];//layer ladder sensor chip
   TH2I* strip_bbcq_vs_nhit[2][24][6][2];//layer ladder sensor sensorsection
   //DST data nodes

   RunHeader *run;
   EventHeader *event_header;
   BbcOut *bbc;
   SvxClusterList *svxcluslist;
   SvxSegmentList *svxtracks;
   PHCentralTrack  *trk;
   SvxCentralTrackList *svxcnttrklist;
   VtxOut *vtxout;
   SvxBeamCenterPar *beamcenter;

   int nsvxtracks;
   int nsvxcluster;
   int runnumber;

 private:

   float bbczcut;
   bool m_readbeamcenterFromDB;
   float d_beamcenterFromDB_x;
   float d_beamcenterFromDB_y;

   SvxQAEventSelection *d_eventselection;

};
#endif // SVXDSTQA_PRODUCTIONHISTOS__
