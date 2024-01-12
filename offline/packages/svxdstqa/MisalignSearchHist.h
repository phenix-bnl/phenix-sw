#ifndef __MISALIGNSEARCHHIST_H__
#define __MISALIGNSEARCHHIST_H__

#include <SubsysReco.h>

class Fun4AllHistoManager;
class PHCompositeNode;
class RunHeader;
class BbcOut;
class EventHeader;
class PreviousEvent;
class VtxOut;
class SvxRawhitList;
class TrigLvl1;

class TProfile;
class TH1I;

class MisalignSearchHist: public SubsysReco
{

public:

  MisalignSearchHist() ;
  virtual ~MisalignSearchHist() {}

  int  Init(PHCompositeNode *topNode);
  int  process_event(PHCompositeNode *topNode);
  int  End(PHCompositeNode *topNode);

  int  InitRun(PHCompositeNode *topNode);
  void Print(const std::string &what) const {}
  int  GetNodes(PHCompositeNode *topNode);
  void Set_outname(const std::string &name) {outname = name;}
  void Set_FixRange(const bool fixrange) {is_fixrange = fixrange;}
  void Set_trigname(const std::string &name) {trigname = name;}
  void Set_clockname(const std::string &name) {clockname = name;}
	// if is_bbcq10percent is set true, the otr tprofile will be used to store
	// centrally selected events from all triggers
	void Set_bbcq10percent() {is_bbcq10percent = true;}

private:

  void Set_maxevents(const int runnumber);
  void Init_TProfiles(); //initialize the TProfiles that need to be in memory during processevent

  //
  // Run nodes
  //
  RunHeader *d_runheader;
  //
  // Data nodes for CM and PHENIX
  EventHeader     *d_eventhead;
  //
  // Data node for VTX
  SvxRawhitList   *d_svxraw;
  //
  // Data node for BBC
  BbcOut          *d_bbc;

  std::string outname;
	std::string trigname;
	std::string clockname;
	
  //
  // variables
  //
  int  m_EventNumber;
  int  m_EventSeqNumber;
  int  m_runnumber;
  bool is_fixrange; //true: fixes event range of TProfiles regardless of run length, false: event range of TProfiles determined by #of events in the run


  static const int NPIXELMODULE = 60;
  static const int NPIXELROC = 8;

  static const int NPIXEL = 8192;
  static const int NCHIP_B0 = 160; //10 ladders, 4 sensors/ladder, 4 chips/sensor, 8192 pixels/chip (1310720 total pixels)
  static const int NCHIP_B1 = 320; //20 ladders, 4 sensors/ladder, 4 chips/sensor, 8192 pixels/chip (2621440 total pixels)

  static const int NMAXLADDER = 24;
  static const int NMAXSENSOR = 6;


  //static const int nbinevtseq = 50;
	// Binning is decided in Set_maxevents() and is based on the number of 
	// events in the run
  int nbinevtseq;
  int MAXEVENTS;
  int binwidth;

	bool is_bbcq10percent;

	// Histograms to hold nevents
	// One for all triggers
	// one for clock triggers
	// one for user specified
	TH1I* h_nevents_all;
	TH1I* h_nevents_clk;
	TH1I* h_nevents_otr;

	// TProfiles for mean hits
  TProfile *B0_chip_profile_all[NCHIP_B0];
  TProfile *B0_chip_profile_clk[NCHIP_B0];
  TProfile *B0_chip_profile_otr[NCHIP_B0];

  TProfile *B1_chip_profile_all[NCHIP_B1];
  TProfile *B1_chip_profile_clk[NCHIP_B1];
  TProfile *B1_chip_profile_otr[NCHIP_B1];

};

#endif
