#ifndef __DCHTRACKINFO_H__
#define __DCHTRACKINFO_H__

#include "phool.h"
#include "DchHitInfo.hh"
#include "PHPointerList.h"

class DchCandidate;

class DchTrackInfo {

public:
  
  DchTrackInfo();
  virtual ~DchTrackInfo();

  void setCandidate(DchCandidate* candi) { candidate = candi;}
  void setEvent(int val) { evt = val;}
  void setId(short val){ id = val;}
  void setArm(short val){ arm = val;}
  void setSide(short val){ side = val;}
  void setBasepoint(PHPoint val) { basepoint = val;}
  void setDirection(PHVector val) {direction = val;}
  void setErrorBasepoint(PHPoint val) { errorPoint = val;}
  void setErrorDirection(PHVector val) {errorDirection = val;}
  void appendHitInfo(DchHitInfo* info) {hitInfoList.append(info);}

  //tracks' parameter
  void setAlpha(float val){ alpha = val;}
  void setBeta(float val){ beta = val;}
  void setPhi(float val){ phi = val;}
  void setZed(float val){ zed = val;}

  float getAlpha() { return alpha;}
  float getBeta() { return beta;}
  float getPhi() { return phi;}
  float getZed() { return zed;}

  int   getEvent() { return evt;}
  short getId()   { return id;}
  short getArm()  { return arm;}
  short getSide() { return side;}
  DchCandidate* getCandidate() {return candidate;}
  PHPoint  getBasepoint() { return basepoint;}
  PHVector getDirection() { return direction; }
  PHPoint  getErrorBasepoint() { return errorPoint;}
  PHVector getErrorDirection() {return  errorDirection;}
  DchHitInfo* getHitInfo(size_t index) const { return hitInfoList[index];}
  PHPointerList<DchHitInfo>& getHitInfoList() { return hitInfoList;}

private:

  int evt;
  short id;
  short arm;
  short side;
  PHPoint basepoint;
  PHVector direction;
  PHPoint errorPoint;
  PHVector errorDirection;
  DchCandidate* candidate;  
  PHPointerList<DchHitInfo> hitInfoList;
 
  // Track parameters
  float alpha;
  float beta;
  float phi;
  float zed;
};

#endif /*   */

