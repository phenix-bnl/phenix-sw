//-----------------------------------------------------------------------------
//  Declaration of class TofEvtHeader
//
//  Purpose: Event Header Class
//
//  Description: Event Header Class
//
//  Author: Akio Kiyomichi (Univ.of Tsukuba)
//-----------------------------------------------------------------------------
#ifndef __TOFEVTHEADER_HH__
#define __TOFEVTHEADER_HH__

class Event;
class PHCompositeNode;

class TofEvtHeader{
public:
  TofEvtHeader();
  virtual ~TofEvtHeader() {}

  PHBoolean event(PHCompositeNode *);
  PHBoolean event(Event *);
  PHBoolean setFromDst(PHCompositeNode *);

  void setDebugLevel(int debug) { iDebug = debug; }
  void print();

  int getRunNumber(){return run;}
  int getRunNumber(Event *);
  int getDate(){return date;}
  int getDate(Event *);
  int getTime(){return time;}
  int getTime(Event *);

  int getEvtSequence(){return evtseq;}
  int getEvtSequence(Event *);

  long getScaledTrig(){return scaledtrig;}
  long getScaledTrig(Event *);

  long getRawTrig(){return rawtrig;}
  long getRawTrig(Event *);
  long getLiveTrig(){return livetrig;}
  long getLiveTrig(Event *);

private:
  // Run info.
  int run;
  int date;
  int time;

  // Event sequence
  int evtseq;

  // GL1 Trigger info.
  long scaledtrig;
  long rawtrig;
  long livetrig;

  // Debug flag (0 = do not debug)
  int iDebug;
};

#endif /* __TOFEVTHEADER_HH__ */
