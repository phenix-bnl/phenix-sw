
#ifndef __ACCEVENT_H__
#define __ACCEVENT_H__

class AccRaw;
class AccHit;
class AccGeometry;
class AccCalib;
class AerGeaHits;
class AccSim;
class PHCompositeNode;

class AccEvent
{
 public:
  AccEvent();
  virtual ~AccEvent();
  int Reset(PHCompositeNode* top);
  int process_event(PHCompositeNode* top);

  PHBoolean DcmToRaw(PHCompositeNode* top);
  PHBoolean RawToHit(PHCompositeNode* top, AccGeometry* geo, AccCalib* calib);

  PHBoolean GeaToRaw(PHCompositeNode* top);
  PHBoolean RawToRec(PHCompositeNode* top, AccGeometry* geo);

  AccRaw* get_AccRaw() const {return d_raw;}
  AccHit* get_AccHit() const {return d_hit;}

  void set_debug(const int dbg) {debug = dbg;}

 private:

  // Data Nodes
  AccRaw*     d_raw;  
  AccHit*     d_hit;
  AerGeaHits* aergea;
  AccSim*     d_sim;

  int debug;

};

#endif
