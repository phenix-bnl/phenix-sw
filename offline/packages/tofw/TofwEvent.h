#ifndef __TOFWEVENT_H__
#define __TOFWEVENT_H__

class TofwRaw;
class TofwHit;
class TofwGeometry;
class TofwCalib;
class PHCompositeNode;

class TofwEvent
{
 public:
  TofwEvent();
  virtual ~TofwEvent(){}
  
  //________________________________________________________________
  int Reset(PHCompositeNode* top);

  int process_event(PHCompositeNode* top, 
		    TofwGeometry* geom,
		    TofwCalib* calib);

  PHBoolean DcmToRaw(PHCompositeNode* top,
		     TofwCalib* calib);

  PHBoolean RawToHit(PHCompositeNode* top,
		     TofwGeometry* geom,
		     TofwCalib* calib);
  
  TofwRaw* get_TofwRaw() const {return d_raw;}
  TofwHit* get_TofwHit() const {return d_hit;}
  
  void set_debug(const int dbg) {debug = dbg;}

 private:
  // Find raw and hit Nodes.
  PHBoolean findNodes(PHCompositeNode* top);

  // Data Nodes
  TofwRaw*     d_raw;  
  TofwHit*     d_hit;
  int debug;

};

#endif
