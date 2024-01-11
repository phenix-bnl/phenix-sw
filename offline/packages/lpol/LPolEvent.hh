#ifndef __PHENIX_LPOLEVENT_HH
#define __PHENIX_LPOLEVENT_HH

class PHCompositeNode;
class PHTimeStamp;
class Event;
class LPolRaw;

class LPolEvent
{
public:
  LPolEvent();
  virtual ~LPolEvent(){}

  int setRawData (Event *);
  int setRawData (PHCompositeNode *topNode);


  void setLPolRaw(LPolRaw *lpol) {lpolraw = lpol;};
  void setCalibDataAll (const PHTimeStamp & time);

private:

  int packet_id;

  LPolRaw *lpolraw; // Raw Input Object (TDCs,ADCs)
  int lpol_pmt_map[4];
};
#endif
