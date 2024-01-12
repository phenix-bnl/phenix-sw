#ifndef __SVXCLUSTERSTABILITYQA_H__
#define __SVXCLUSTERSTABILITYQA_H__

#include <SubsysReco.h>
#include <string>

class EventHeader;
class VtxOut;
class TrigLvl1;
class SvxClusterList;
class SvxRawhitList;
class SvxRawhitClusterList;
class SvxCluster;
class svxAddress;
class SvxQAEventSelection;
class TH1I;
class TH1D;
class TH2F;
class TH3D;

class SvxClusterStabilityQA: public SubsysReco {

public:
  static bool getRawhitIndex(const int nrawcls,
                             SvxCluster* cls,
                             int &pos, int& size,
                             bool isfirst=false);

public:
  SvxClusterStabilityQA() ;
  virtual ~SvxClusterStabilityQA() {}

  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  process_event(PHCompositeNode *topNode);
  int  End(PHCompositeNode *topNode);

  int  Reset(PHCompositeNode *topNode) 		{return 0;}
  int  ResetEvent(PHCompositeNode *topNode) 	{return 0;}
  void Print(const std::string& what) const {}

  virtual void Set_outname(const char * name ){ m_outname = name; }
  virtual void Set_TickCut(bool is){is_tickcut=is;}
  virtual void Set_bbczcut(float cut){m_bbczcut=cut;}


  virtual int  GetNodes(PHCompositeNode *topNode);

 private:
  void Set_maxevents(int runnumber);
  void Init_TProfiles(); //initialize the TProfiles that need to be in memory during processevent

  bool calcPixelChipChannel(double lx, double lz, int* chip, int* row, int* col, bool isdebug=false);
  bool calcStripChipChannel(double lx, double lz, int* senSec,
                            int* sensChipX, int* sensChipU, int* sensChipChanX, int* sensChipChanU,
                            int* iLx, int* iLz,
                            bool isdebug=false);

 private:
  //
  // Data nodes for CM and PHENIX
  EventHeader*     d_eventhead;
  VtxOut*          d_vtxout;

  //
  // Data node for VTX
  SvxRawhitList*        d_svxraw;
  SvxClusterList*       d_svxcls;
  SvxRawhitClusterList* d_svxrawcls;

  svxAddress*d_svxadr;

  std::string m_outname;

  //
  // variables
  //
  SvxQAEventSelection* d_eventselection;

  bool  is_tickcut;
  float m_bbczcut;

  int   m_EventNumber;
  int   m_EventSeqNumber;

  int   m_pticks[3];

  int   m_runnumber;

  // saved histograms
  TH1D *m_hzbbc;
  TH1I* m_htrigL1;
  TH1I* m_htrignarrow;
  TH1I* m_htrignarrow_copyA;
  TH1I* m_htrignarrow_copyB;
  TH1I* m_htrignovtx;
  TH2F* m_hzbbc_vs_evtseq;
  TH2F* m_hzbbc_vs_evtseq_selection;
  TH3D* m_h3_clsmap[4];
  TH3D* m_h3_clsmapw[4];
  TH3D* m_h3_clsmapd[4];

  // Tprofile info
  static const int NPIXEL   = 8192;
//  static const int NPIXEL   = 10; //for valgrind and insure
  static const int NCHIP_B0 =  160;
  static const int NCHIP_B1 =  320;

  static const int NSTRIPCHANNEL=128; //
  static const int NCHIP_B2     =960; // 16 ladders, 5 sensors/ladder, 12 chips/sensor, 128 channels/chip (122880 total channels)
  static const int NCHIP_B3     =1728;// 24 ladders, 6 sensors/ladder, 12 chips/sensor, 128 channels/chip (221184 total channels)

  static const int nbinevtseq=50; //modded by theok for run77171
  int MAXEVENTS;
  int binwidth;

  unsigned int nevents[nbinevtseq];
  unsigned int B0_chips_hit[NCHIP_B0][nbinevtseq][NPIXEL];//all chips in B0
  unsigned int B1_chips_hit[NCHIP_B1][nbinevtseq][NPIXEL];//all chips in B1
  unsigned int B2_chips_hit[NCHIP_B2][nbinevtseq][NSTRIPCHANNEL];//all chips in B1
  unsigned int B3_chips_hit[NCHIP_B3][nbinevtseq][NSTRIPCHANNEL];//all chips in B1

  float B0_chips_hitw[NCHIP_B0][nbinevtseq][NPIXEL];//all chips in B0
  float B1_chips_hitw[NCHIP_B1][nbinevtseq][NPIXEL];//all chips in B1
  float B2_chips_hitw[NCHIP_B2][nbinevtseq][NSTRIPCHANNEL];//all chips in B1
  float B3_chips_hitw[NCHIP_B3][nbinevtseq][NSTRIPCHANNEL];//all chips in B1
};

#endif
