#ifndef __MPCPED_H__
#define __MPCPED_H__

class TFile;
class TH1;
class TNtuple;

class MpcPed
{
public:
  MpcPed(const char *outfname, const int fem);
  virtual ~MpcPed();

  void InitHistograms();

  void ProcessPRDF(const char *prdfname, const int nevents = 0);
  void CheckUnderOverflow(TH1 *th1, Float_t max_percentage = 0.05);
  TNtuple *GetSummaryNtuple(const char *name, const char *title);
  void FillSummaryNtuples(TNtuple *ntdc, TNtuple *nlopreped, TNtuple *nlopostped, TNtuple *nhipreped,
                                TNtuple *nhipostped, TNtuple *nloped, TNtuple *nhiped);
  void WriteAndClose();
  void SetTrigger(const unsigned int itrig) {
    fTrigger = itrig;
  };
  void SetVerbose(const unsigned int iverbose) {
    fVerbose = iverbose;
  };

private:
  int fPacketId[3];	// 21001-21004
  int fFEM;		// 0-3
  int fNpackets;	// 1 or 3, number of packets to loop over
  unsigned int fTrigger;	// trigger to select
  int fVerbose;

  TFile *fOutFile;

  // HiSTOGRAMS
  static const int NUM_CH = 144;
  static const int NUM_REFCH = 48;
  static const int NUM_AMU = 64;

  TH1 *htdc_all[144];
  TH1 *hlopre_all[144];
  TH1 *hlopost_all[144];
  TH1 *hhipre_all[144];
  TH1 *hhipost_all[144];
  TH1 *hlo_all[144];
  TH1 *hhi_all[144];
  
  TH1 *htdc[144][NUM_AMU];
  TH1 *hlopre[144][NUM_AMU];
  TH1 *hlopost[144][NUM_AMU];
  TH1 *hhipre[144][NUM_AMU];
  TH1 *hhipost[144][NUM_AMU];
  TH1 *hlo[144][NUM_AMU];
  TH1 *hhi[144][NUM_AMU];
  
  TH1 *refhtdc[48][NUM_AMU];
  TH1 *refhlopre[48][NUM_AMU];
  TH1 *refhlopost[48][NUM_AMU];
  TH1 *refhhipre[48][NUM_AMU];
  TH1 *refhhipost[48][NUM_AMU];
  TH1 *refhlo[48][NUM_AMU];
  TH1 *refhhi[48][NUM_AMU];

  TH1 *hamutdc;
  TH1 *hamupre;
  TH1 *hamupost;

  // Summary TNtuples
  TNtuple *fTdcPed;
  TNtuple *fLoPrePed;
  TNtuple *fLoPostPed;
  TNtuple *fHiPrePed;
  TNtuple *fHiPostPed;
  TNtuple *fLoPed;
  TNtuple *fHiPed;
};

#endif	// __MPCPED_H__

