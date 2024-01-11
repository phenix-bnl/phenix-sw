#ifndef __DCHHITINFO_H__
#define __DCHHITINFO_H__

#include "phool.h"
#include "PHPoint.h"
#include "PHVector.h"
#include "PHLine.h"
#include "DchRawInfo.hh"

class DchHitInfo {
public:
  DchHitInfo();
  virtual ~DchHitInfo();
 
  void  setId(short val)  {id = val;}
  void  setTrackId(short val) { trackId = val;}
  void  setArm(short val) {arm = val;}
  void  setSide(short val) {side = val;}
  void  setPlane(short val) {plane = val;}
  void  setCell(short val) {cell = val;}
  void  setDistance(float val) {distance = val;}
  void  setWidth(float val) {width = val;}
  void  setIdraw1(short val) {idraw1 = val;}
  void  setIdraw2(short val) {idraw2 = val;}
  void  setRaw1(DchRawInfo* val) { raw1 = val;} 
  void  setRaw2(DchRawInfo* val) { raw2 = val;} 
  void  setIdmirror(short val) {idmirror = val;}
  void  setMirror(DchHitInfo* val) {mirror = val;}
  void  setUsed(short val) {used = val;}
  
  void  setNorth(PHPoint val) { northP = val;}
  void  setSouth(PHPoint val) {southP = val;}
  void  setBasepoint(PHPoint val) {basepoint = val;}
  void  setDirection(PHVector val) {direction = val;}
  void  setTrackBasepoint(PHPoint val) {trackbasepoint = val;}
  void  setTrackDirection(PHVector val) {trackdirection = val;}
  void  setError(PHPoint val) { error = val;}
  
  short getId()  {return id;}
  short getTrackId()  {return trackId;}
  
  short getArm() {return arm;}
  short getSide() {return side;}
  short getPlane() {return plane;}
  short getCell() {return cell;}
  float getDistance() {return distance;}
  float getWidth() {return width;}
  short getIdraw1() {return idraw1;}
  short getIdraw2() {return idraw2;}
  short getIdmirror() {return idmirror;}
  short getUsed() {return used;}

  DchHitInfo* getMirror() { return mirror;}
  DchRawInfo* getRaw1() {return raw1;}
  DchRawInfo* getRaw2() {return raw2;}

  PHPoint getSouth() {return southP;}
  PHPoint getNorth() {return northP;}
  PHPoint getBasepoint() {return basepoint;}
  PHVector getDirection() {return direction;}
  PHPoint getTrackBasepoint() {return trackbasepoint;}
  PHVector getTrackDirection() {return trackdirection;}
  PHPoint getError() {return error;}

  PHLine  getLine();
  
  void   setResidual(float res){ residual = res;}
  float  getResidual(){ return residual;}

  void   setBlind(short val){ blind = val;}
  short  getBlind(){ return blind;}

  void   setOut(short val){ out = val;}
  short  getOut(){ return out;}
 
private:
  short id;
  short arm;
  short side;
  short plane;
  short cell;
  float distance;   // distance of hit from wire in cm
  float width;      // width of the hit in time bins 
  short idraw1;     // id of leading edge raw data hit
  short idraw2;     // id of trailing edge raw data hit
  short idmirror;   // id of hit if ambigous copy exists
  short used;       // hit used in track 
  PHPoint basepoint;
  PHVector direction;
  PHPoint error;

  DchRawInfo* raw1;
  DchRawInfo* raw2;
  DchHitInfo* mirror;

  short    blind;
  float    residual;
  short    out;
  short    trackId;
  PHPoint  trackbasepoint;
  PHVector trackdirection;

  PHPoint northP;
  PHPoint southP;
  
};

#endif /*__DCHHITINFO_H__ */

