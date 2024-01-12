#ifndef __SVXSTABILITYQA_H__
#define __SVXSTABILITYQA_H__

#include <SubsysReco.h>
#include <vector>

class Fun4AllHistoManager;
class PHCompositeNode;
class RunHeader;
class BbcOut;
class EventHeader;
class PreviousEvent;
class VtxOut;
class SvxRawhitList;
class SvxClusterList;
class svxAddress;
class SvxStripThreshold;
class TrigLvl1;
class SvxQAEventSelection;

class vector;
class TH1D;
class TH1F;
class TH2F;
class TH1I;
class TH2I;
class TProfile;

class SvxStabilityQA: public SubsysReco
{

public:

    SvxStabilityQA() ;
    virtual ~SvxStabilityQA() {}

    int  Init(PHCompositeNode *topNode);
    int  process_event(PHCompositeNode *topNode);
    int  End(PHCompositeNode *topNode);

    int  InitRun(PHCompositeNode *topNode);
    void Print(const std::string &what) const {}
    int  GetNodes(PHCompositeNode *topNode);
    void Set_TickCut(const bool is) {is_tickcut = is;}
    void Set_bbczcut(const float cut) {m_bbczcut = cut;}
    void Set_outname(const std::string &name) {outname = name;}
    void Set_FixRange(const bool fixrange) {is_fixrange = fixrange;}
	void Set_bbcq10percent() {is_bbcq10percent = true;}

    void Set_run_pixels(bool b){run_pixels = b;}

private:

    void Set_maxevents(const int runnumber);
    void Init_TProfiles(); //initialize the TProfiles that need to be in memory during processevent

    //
    // Run nodes
    //
    RunHeader *d_runheader;
    SvxStripThreshold  *d_svxstripthreshold;
    //
    // Data nodes for CM and PHENIX
    EventHeader     *d_eventhead;
    //
    // Data node for VTX
    svxAddress         *d_svxadr;
    SvxRawhitList      *d_svxraw;
    SvxClusterList     *d_svxcls;

    std::string outname;

    //
    // variables
    //
    SvxQAEventSelection* d_eventselection;
    int  m_EventNumber;
    int  m_EventSeqNumber;
    bool is_tickcut;
    float m_bbczcut;
    int  m_runnumber;
    bool is_fixrange; //true: fixes event range of TProfiles regardless of run length, false: event range of TProfiles determined by #of events in the run
	bool is_bbcq10percent;

    bool run_pixels;

    static const int NPIXELMODULE = 60;
    static const int NPIXELROC = 8;
    static const int NSTRIPROC = 12;

    static const int NPIXEL = 8192;
    static const int NCHIP_B0 = 160; //10 ladders, 4 sensors/ladder, 4 chips/sensor, 8192 pixels/chip (1310720 total pixels)
    static const int NCHIP_B1 = 320; //20 ladders, 4 sensors/ladder, 4 chips/sensor, 8192 pixels/chip (2621440 total pixels)

    static const int NSTRIPCHANNEL = 128;
    static const int NCHIP_B2 = 960; //16 ladders, 5 sensors/ladder, 12 chips/sensor, 128 channels/chip (122880 total channels)
    static const int NCHIP_B3 = 1728; //24 ladders, 6 sensors/ladder, 12 chips/sensor, 128 channels/chip (221184 total channels)

    static const int NMAXLADDER = 24;
    static const int NMAXSENSOR = 6;


    static const int nbinevtseq = 50;
    int MAXEVENTS;
    int binwidth;

    unsigned int nevents[nbinevtseq+1];
    unsigned int B0_chips_hit[NCHIP_B0][nbinevtseq+1][NPIXEL];//all chips in B0
    unsigned int B1_chips_hit[NCHIP_B1][nbinevtseq+1][NPIXEL];//all chips in B1
    unsigned int B2_chips_hit[NCHIP_B2][nbinevtseq+1][NSTRIPCHANNEL];//all chips in B2
    unsigned int B3_chips_hit[NCHIP_B3][nbinevtseq+1][NSTRIPCHANNEL];//all chips in B3

    TProfile *B0_chip_profile[NCHIP_B0];
    TProfile *B1_chip_profile[NCHIP_B1];
    TProfile *B2_chip_profile[NCHIP_B2];
    TProfile *B3_chip_profile[NCHIP_B3];

    TProfile *B2_strip_adc_profile[NCHIP_B2][NSTRIPCHANNEL]; //average adc above threshold vs evt sequence for each strip
    TProfile *B3_strip_adc_profile[NCHIP_B3][NSTRIPCHANNEL]; //average adc above threshold vs evt sequence for each strip

};

#endif
