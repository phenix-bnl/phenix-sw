#ifndef __TOFWSIMEVENT_H__
#define __TOFWSIMEVENT_H__

#include<gsl/gsl_rng.h>

class TofwRaw;
class TofwHit;
class TofwGeometry;
class PHCompositeNode;
class TfwPISAHit;

class TofwSimEvent
{
 public:
  
  //! constructor
  TofwSimEvent();
  
  //! destructor
  virtual ~TofwSimEvent();
  
  //________________________________________________________________
  int Reset(PHCompositeNode* top);
  
  int process_event(PHCompositeNode* top,TofwGeometry *geom);
  
  PHBoolean PisaToRaw(PHCompositeNode* top);
  
  PHBoolean RawToHit(PHCompositeNode* top, TofwGeometry* geom);
  
  TofwRaw* get_TofwRaw() const {return d_raw;}
  TofwHit* get_TofwHit() const {return d_hit;}
  
  int CalChamber(float glx, float gly, float glz);
  
  int CalStrip(float localz, float globalz);
  
  float CalElossDis(float eloss,float localz);
  
  void set_debug(const int dbg) {debug = dbg;}
  
 private:
  // Find raw and hit Nodes.
  PHBoolean findNodes(PHCompositeNode* top);
  
  // Data Nodes
  TofwRaw*     d_raw;  
  TofwHit*     d_hit;
 
  float z_strip_center[4];
  float sigmaGauss;
  int debug;
 
  //! gsl random number generator initialization structure
  gsl_rng* _rng;
  
};

#endif
