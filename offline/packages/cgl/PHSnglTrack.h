#ifndef __PHSNGLTRACK_HH__
#define __PHSNGLTRACK_HH__

#include "PHObject.h"
#include "phool.h"

class PHSnglTrack: public PHObject
{
 public:

  PHSnglTrack(){}
  virtual ~PHSnglTrack(){}

  virtual unsigned short get_trackIndex()     const {return 0;}
  virtual void  set_trackIndex(const unsigned short val) {return;}

  virtual void set_projectionAcc(const short d0,const float v) {return;}
  virtual float get_projectionAcc(const short d0) const;
  virtual void set_projectionCrk(const short d0,const float v) {return;}
  virtual float get_projectionCrk(const short d0) const;
  virtual void set_projectionDch(const short d0, const float v) {return;}
  virtual float get_projectionDch(const short d0) const;
  virtual void  set_projectionHbd(const short d0, const float v) {return;}
  virtual float get_projectionHbd(const short d0) const;
  virtual void set_projectionMrpc(const short d0,const float v) {return;}
  virtual float get_projectionMrpc(const short d0) const;
  virtual void set_projectionTofw(const short d0,const float v) {return;}
  virtual float get_projectionTofw(const short d0) const;
  virtual void set_projectionPbGl(const short d0,const float v) {return;}
  virtual float get_projectionPbGl(const short d0) const;
  virtual void set_projectionPbSc(const short d0,const float v) {return;}
  virtual float get_projectionPbSc(const short d0) const;
  virtual void set_projectionPc1(const short d0, const float v) {return;}
  virtual float get_projectionPc1(const short d0) const;
  virtual void set_projectionPc2(const short d0,const float v) {return;}
  virtual float get_projectionPc2(const short d0) const;
  virtual void set_projectionPc3(const short d0, const float v) {return;}
  virtual float get_projectionPc3(const short d0) const;
  virtual void set_projectionPcr(const short d0, const float v) {return;}
  virtual float get_projectionPcr(const short d0) const;
  virtual void set_projectionTec(const short d0, const float v) {return;}
  virtual float get_projectionTec(const short d0) const;
  virtual void set_projectionTof(const short d0, const float v) {return;}
  virtual float get_projectionTof(const short d0) const;
  virtual void set_projectionTzr(const short d0, const float v) {return;}
  virtual float get_projectionTzr(const short d0) const;
  virtual void set_projectionSvx(const short ilayer, const short d0, const float v) {return;}
  virtual float get_projectionSvx(const short ilayer, const short d0) const;
  virtual void  set_projectionVtx(const short d0, const float v) {return;}
  virtual float get_projectionVtx(const short d0) const;

  virtual void set_directionAcc(const short d0,const float v) {return;}
  virtual float get_directionAcc(const short d0) const;
  virtual void set_directionCrk(const short d0, const float v) {return;}
  virtual float get_directionCrk(const short d0) const;
  virtual void set_directionDch(const short d0,const float v) {return;}
  virtual float get_directionDch(const short d0) const;
  virtual void set_directionHbd(const short d0, const float v) {return;}
  virtual float get_directionHbd(const short d0) const;
  virtual void set_directionMrpc(const short d0,const float v) {return;}
  virtual float get_directionMrpc(const short d0) const;
  virtual void set_directionTofw(const short d0,const float v) {return;}
  virtual float get_directionTofw(const short d0) const;
  virtual void set_directionPbGl(const short d0,const float v) {return;}
  virtual float get_directionPbGl(const short d0) const;
  virtual void set_directionPbSc(const short d0,const float v) {return;}
  virtual float get_directionPbSc(const short d0) const;
  virtual void set_directionPc1(const short d0,const float v) {return;}
  virtual float get_directionPc1(const short d0) const;
  virtual void set_directionPc2(const short d0, const float v) {return;}
  virtual float get_directionPc2(const short d0) const;
  virtual void set_directionPc3(const short d0,const float v) {return;}
  virtual float get_directionPc3(const short d0) const;
  virtual void set_directionPcr(const short d0,const float v) {return;}
  virtual float get_directionPcr(const short d0) const;
  virtual void set_directionSvx(const short ilayer, const short d0,const float v) {return;}
  virtual float get_directionSvx(const short ilayer, const short d0) const;
  virtual void set_directionTec(const short d0, const float v) {return;}
  virtual float get_directionTec(const short d0) const;
  virtual void set_directionTof(const short d0, const float v) {return;}
  virtual float get_directionTof(const short d0) const;
  virtual void set_directionTzr(const short d0, const float v) {return;}
  virtual float get_directionTzr(const short d0) const;
  virtual void set_directionVtx(const short d0, const float v) {return;}
  virtual float get_directionVtx(const short d0) const;

  virtual void set_crkPathLength(const float v)  {return;}
  virtual float get_crkPathLength() const;
  virtual void set_emcPathLength(const float v) {return;}
  virtual float  get_emcPathLength() const;
  virtual void set_mrpcPathLength(const float v) {return;}
  virtual float  get_mrpcPathLength() const;
  virtual void set_tofwPathLength(const float v) {return;}
  virtual float  get_tofwPathLength() const;
  virtual void set_tofPathLength(const float v) {return;}
  virtual float get_tofPathLength() const;
  virtual void set_tzrPathLength(const float v) {return;}
  virtual float get_tzrPathLength() const;
  void Reset();
  void ShutUp(const int i);

 protected:
  void Copy(const PHSnglTrack &src);
  void Copy(TObject&) const { PHOOL_VIRTUAL_WARNING; }

  ClassDef(PHSnglTrack, 1);
};
#endif
