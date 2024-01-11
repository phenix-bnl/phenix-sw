#ifndef __DCHSNGLTRACKv1_H
#define __DCHSNGLTRACKv1_H

#include "PHObject.h"
#include "PHPoint.h"
#include "PHVector.h"

class DchSnglTrackv1 : public TObject
{
public:
  DchSnglTrackv1();
  DchSnglTrackv1(DchSnglTrackv1*track);
  virtual ~DchSnglTrackv1(){};

  void set_trackid(short val)      {trackid = val;}
  void set_arm(short val)          {arm = val;}
  void set_side(short val)         { side = val;}
  void set_quality(short val)      { quality = val;}
  void set_phi(float val)          { phi = val;}
  void set_alpha(float val)        {alpha = val;}
  void set_beta(float val)         { beta = val;}
  void set_betaNoVertex(float val) { betaNoVertex = val;} 
  void set_zed(float val) { zed = val;}
  void set_phi0(float val) {phi0 = val;}
  void set_theta0(float val) { theta0 = val;}
  void set_momentum(float val) { momentum = val;}
  void set_status(int val) { status = val;}
  void set_alpha1(float val) { alpha1 = val;}
  void set_alpha2(float val) { alpha2 = val;}
  void set_chi21(float val) { chi21 = val;}
  void set_chi22(float val) { chi22 = val;}
  void set_dist1(float val) { dist1 = val;}
  void set_dist2(float val) { dist2 = val;}

  void set_point(PHPoint val) 
       { x = val.getX();y = val.getY();z = val.getZ();}
  void set_direction(PHPoint val) 
       {dirx = val.getX();diry = val.getY();dirz = val.getZ();}

  void  set_hits(short plane, short hit_id) { hits[plane] = hit_id;}

  short get_trackid() {return trackid;}
  short get_arm() {return arm;}
  short get_side() {return side;}
  short get_quality() {return quality;}
  float get_phi() { return phi;}
  float get_alpha() { return alpha;}
  float get_beta() { return beta;}
  float get_betaNoVertex() {return betaNoVertex;}
  float get_zed() { return zed;}
  float get_phi0() { return phi0;}
  float get_theta0() { return theta0;}
  float get_momentum() { return momentum;}

  PHPoint get_point()          { PHPoint val(x,y,z); return val;}
  PHVector get_direction()     { PHVector val(dirx,diry,dirz);return val;}
  
  int    get_status() { return status;}
  float  get_alpha1() { return alpha1;}
  float  get_alpha2() { return alpha2;}
  float  get_chi21() { return chi21;}
  float  get_chi22() { return chi22;}
  float  get_dist1() { return dist1;}
  float  get_dist2() { return dist2;}
  short  get_nx1hits();
  short  get_nx2hits();
  short  get_hits(short plane) { return hits[plane];}

protected:

  void Init();

  short trackid;         
  short arm;        // EAST = 0, WEST = 1     
  short side;       // SOUTH = 0, NORTH = 1     
  short quality;
  short hits[40];
  int   status;
  float phi;
  float alpha;
  float beta;
  float betaNoVertex;
  float zed;
  float phi0;
  float theta0;
  float momentum;
  
  float  x,  y,  z;
  float  dirx,  diry,  dirz;
  float alpha1,alpha2;
  float chi21,chi22;
  float dist1,dist2;

  ClassDef(DchSnglTrackv1,1)
};

#endif /*__DCHSNGLTRACKv1_H*/
