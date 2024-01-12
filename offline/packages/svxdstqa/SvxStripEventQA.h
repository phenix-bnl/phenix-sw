#ifndef __SVXSTRIPEVENTQA_H__
#define __SVXSTRIPEVENTQA_H__

#include <SubsysReco.h>
#include <vector>
#include <SvxParameters.h>

class PHCompositeNode;
class Event;
class SvxQAInfo;
class TH1;
class TH2;
class TH3;
class svxAddress;

typedef std::pair<int, int> Pair;
inline bool sort_at_first( const Pair& b1, const Pair& b2 ){
  return b1.first < b2.first;
}

class SvxStripEventQA: public SubsysReco
{

 public:

  SvxStripEventQA(const std::string &name = "SvxStripEventQA");
  virtual ~SvxStripEventQA();

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  int EndRun(const int runno);

  int Reset(PHCompositeNode *topNode) 		{return 0;}
  int ResetEvent(PHCompositeNode *topNode) 	{return 0;}
  int InitRun(PHCompositeNode *topNode);
  void Print(const std::string&) const { }

  void Set_StuckCellThresh(int v1) { numsc = v1;}
  void Write_StripQAHisto(int v1) { qaflg = v1; }

  void SetFillCellIDHisto(int flag){m_cellidhist=flag;}
  void SetFillCellIDADC(int flag)  {m_cellidadc=flag;}

  TH1* h1_cellidcount[SVXNMODULESTRIP];   // tmp histogram cellid=0-256

 private:

  int CreateNodeTree(PHCompositeNode *topNode);
  int CreateQAHisto();

  void fillCellIDHistoByDecoder(Event* event);
  void fillAdcChip(Event* event);


  SvxQAInfo      *d_qainfo;  // SVX QA Information
  TH2* hSvxSCellvsEvt[SVXNMODULESTRIP];
  std::vector<Pair> evcel[SVXNMODULESTRIP];
  svxAddress *m_address;

  TH3* h3_cellidchipevt[SVXNMODULESTRIP]; // cellid=0-256, chip=0-11*6, 0-50M/1000
  TH3* h3_celliddiffchipevt[SVXNMODULESTRIP]; // cellid=0-256, chip=0-11*6, 0-50M/1000
  TH2* h2_cellidavgevt[SVXNMODULESTRIP];  // cellid=0-256, 0-50M/1000
  TH2* h2_cellidndiffevt[SVXNMODULESTRIP];// nchip diff from avg

  TH2* h2_adcchan[SVXNMODULESTRIP][6][2]; // adc:0-256, chan 0-11*128

  int numsc;
  int qaflg;
  int m_cellidhist;
  int m_cellidadc;

};

#endif
