#include <TFile.h>
#include <TH1.h>
#include <TF1.h>

class BbcCalib;
class BbcEvent;
class ZdcCalib;
class ZdcEvent;

class ZdcTrigMap
{
public:
  ZdcTrigMap();
  virtual ~ZdcTrigMap();

  void InitCalibrations(const Event *evt);
  int ProcessEvent(Event *evt);
  int FindTdcMean();
  int SaveLUT(const char *fname = "zdclut");
  int ReadLUT(const char *fname = "zdclut");
  int UpdateLUT();
  void SetConversionTime(double t2ns) { trigtimestep = t2ns; }
  void SetTrigMask(const unsigned int t) { trigmask = t; }
  int CalculateMinMaxTimes();
  int ProcessVertexComparisonEvent(Event *evt);
  void CalculateVertexOffset();

  enum { ANY = 0, ZDCLL1wide, ZDCNS, BBCLL1, BBCLL1wide };
  enum { ANYmask = 0x1, ZDCLL1widemask = 0x2, ZDCNSmask = 0x4, BBCLL1mask = 0x8, BBCLL1widemask = 0x10 };

private:
  static const int MAX_ZDC_CHANNELS = 8;
  static const int MAX_TRIGGERS = 5;

  BbcCalib *bbccalib;
  BbcEvent *bbcevent;
  ZdcCalib *zdccalib;
  ZdcEvent *zdcevent;

  TFile *savefile;

  TH1F *ztdc1[MAX_ZDC_CHANNELS];	// time (ns) distribution for a channel
  TH1F *ztdc1minusbbc[MAX_ZDC_CHANNELS];// zdctime - bbctime for a channel
  TH1F *zdclvl1bbcvtx[MAX_TRIGGERS];
  TH1F *zdclvl1zdcvtx[MAX_TRIGGERS];
  TH1F *hzdcL1vtx[MAX_TRIGGERS];
  TH1F *hzdcvtx[MAX_TRIGGERS];
  TH1F *hbbcvtx[MAX_TRIGGERS];
  TF1 *peakfit;

  unsigned int trigmask;

  double overflow1cut[MAX_ZDC_CHANNELS];
  double underflow1cut[MAX_ZDC_CHANNELS];
  double tdc1ns[MAX_ZDC_CHANNELS];
  double tdc1t0[MAX_ZDC_CHANNELS];
  double dvtxmean;
  double trigtimestep;
  double mintime;
  double maxtime;

  unsigned int processed_event;
  int zdclut[MAX_ZDC_CHANNELS][2048];
};
