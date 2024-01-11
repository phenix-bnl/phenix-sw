#ifndef __SVXEVENTKILLER_H__
#define __SVXEVENTKILLER_H__

#include <SubsysReco.h>

#include <string>
#include <vector>
#include <map>

class PHCompositeNode;
class TH1;
class svxAddress;
class SvxDaqErrorMap;
class EventHeader;

enum{FLAG_CELLID=1,FLAG_BIASQA=2,FLAG_DAQERR=4};

class SvxEventKiller : public SubsysReco
{
 public:

  SvxEventKiller(const std::string &name = "SvxEventKiller");
  virtual ~SvxEventKiller() {}

  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  process_event(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  End(PHCompositeNode *topNode);

  int  fetch(PHCompositeNode* topNode,const int runnumber);
  int  SetReturnCode(const char *action = "ABORT");
  int  SetLadderAllActive();
  int  SetLadderTag(const std::string& tag);
  int  PrintLadder() const;
  void SetMode(const int mode) { _mode=mode; }
  int  RemoveLadder(const std::string& ladder);
  void SetVerbosity(const int verbose) { _verbose=verbose; }

  void SetUseActiveLadderDatabase(bool flag) { m_activeladderDB = flag; }

  std::vector<std::string> GetBadLadderList() const;
  int GetLadderStatus(std::string& ladder);
  int GetStripLastGoodEvent() const { return _strip_last_good_event; }
  
 protected:
  int _mode;
  int _runnumber;
  int _strip_last_good_event;                // stuck bit strip problem
  std::map<std::string,int> _bad_bias_time;  // list of bad times for the bias system
  int _RetCode;
  int _calibration_status;
  unsigned int _nGood;
  unsigned int _nConsider;
  int _verbose;
  bool m_activeladderDB;

  std::map<std::string,int> _lad_pid; // ladder,packetid association map
  std::map<std::string,int> _reqlist; // list of required ladders

  int _event_status;//bitword summarizing the event
  TH1* h1status[2]; //event_status histogram with and without & with mode
  TH1* h1nevent;    //number of events total and passing QA
  TH1* h1packet;    //histogram to count #bad packets
  TH1* h1packet_mask;//histogram to count #bad packets after ladder mask.
  
  TH1* h1strip_last_good_event;   // the stuck bit time for the stripixel
  TH1* h1biastime;                // contains the bias failure time for each ladder

  
  std::map<std::string,int> ladder_status; //reset event-by-event, gives ladder-by-ladder status word

  // database interface
  svxAddress    * m_addr;
  SvxDaqErrorMap* m_daqerror;

 protected:
  void checkMask(EventHeader* evthead);
  void SetActiveLadderByDatabase();

};

#endif // __SVXEVENTKILLER_H__
