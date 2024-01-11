#ifndef __PHENIX_LPCEVENT_HH
#define __PHENIX_LPCEVENT_HH

class PHCompositeNode;
class PHTimeStamp;
class Event;
class lpcRaw;

class lpcEvent
{
public:
  lpcEvent();
  virtual ~lpcEvent(){}

  int setRawData (Event *);
  int setRawData (PHCompositeNode *topNode);


  void setlpcRaw(lpcRaw *lpc) {lpcraw = lpc;}
  void setCalibDataAll (const PHTimeStamp & time);

private:

  int packet_id[2];

  lpcRaw *lpcraw; // Raw Input Object (TDCs,ADCs)
  int lpc_pmt_map[2][2];
};
#endif

