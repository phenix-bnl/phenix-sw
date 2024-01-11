#ifndef __DPHTRACKOUT_H__
#define __DPHTRACKOUT_H__

#include <iostream>
#include <phool.h>
#include <PHObject.h>

// with the following we get the line number of the virtual function we called with PHWHERE
#define PHTRACK_VIRTUAL_WARNING std::cerr << PHWHERE << "using virtual function, doing nothing" << std::endl

class PHSnglTrack;
class TClonesArray;


class PHTrackOut : public PHObject
{
 public:
  virtual ~PHTrackOut() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os=std::cout) const;

  virtual unsigned int get_PHNTrack() const {virtual_warning("get_PHNTrack()"); return 0;}
  virtual void set_PHNTrack(const unsigned int ntrk) {virtual_warning("set_PHNTrack()"); return;}

  virtual int set_TClonesArraySize(const unsigned int ntrk);

  virtual void AddPHTrack(const unsigned int itrk) {virtual_warning("AddPHTrack()"); return;}

  virtual void FillFromClass(PHTrackOut *phtrackout) {virtual_warning("FillFromClass()"); return;}

  virtual unsigned short get_trackIndex(const unsigned int itrk)   const {virtual_warning("get_trackIndex()"); return 0;}
  virtual void  set_trackIndex(const unsigned int itrk, const unsigned short val) {virtual_warning("set_trackIndex()"); return;}


  virtual float get_projectionVtx(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionVtx()"); return -9999.9;}
  virtual void set_projectionVtx(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionVtx()"); return;}

  virtual float get_projectionDch(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionDch()"); return -9999.9;}
  virtual void set_projectionDch(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionDch()"); return;}

  virtual float get_projectionTec(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionTec()"); return -9999.9;}
  virtual void set_projectionTec(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionTec()"); return;}

  virtual float get_projectionPc1(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionPc1()"); return -9999.9;}
  virtual void set_projectionPc1(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionPc1()"); return;}

  virtual float get_projectionPc2(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionPc2()"); return -9999.9;}
  virtual void set_projectionPc2(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionPc2()"); return;}

  virtual float get_projectionPc3(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionPc3()"); return -9999.9;}
  virtual void set_projectionPc3(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionPc3()"); return;}

  virtual float get_projectionSvx(const unsigned int itrk, const short ilayer, const short ival) const {virtual_warning("get_projectionSvx()"); return -9999.9;}
  virtual void set_projectionSvx(const unsigned int itrk, const short ilayer, const short ival, const float rval) {virtual_warning("set_projectionSvx()"); return;}

  virtual float get_projectionTof(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionTof()"); return -9999.9;}
  virtual void set_projectionTof(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionTof()"); return;}

  virtual float get_projectionEmc(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionEmc()"); return -9999.9;}
  virtual void set_projectionEmc(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionEmc()"); return;}
  virtual float get_projectionPbGl(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionPbGl()"); return -9999.9;}
  virtual void set_projectionPbGl(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionPbGl()"); return;}
  virtual float get_projectionPbSc(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionPbSc()"); return -9999.9;}
  virtual void set_projectionPbSc(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionPbSc()"); return;}

  virtual float get_projectionCrk(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionCrk()"); return -9999.9;}
  virtual void set_projectionCrk(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionCrk()"); return;}

 virtual float get_projectionTzr(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionTzr()"); return -9999.9;}
  virtual void set_projectionTzr(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionTzr()"); return;}

 virtual float get_projectionPcr(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionPcr()"); return -9999.9;}
  virtual void set_projectionPcr(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionPcr()"); return;}

 virtual float get_projectionAcc(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionAcc()"); return -9999.9;}
  virtual void set_projectionAcc(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionAcc()"); return;}

 virtual float get_projectionTofw(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionTofw()"); return -9999.9;}
  virtual void set_projectionTofw(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionTofw()"); return;}

  virtual float get_directionVtx(const unsigned int itrk, const short ival) const {virtual_warning("get_directionVtx()"); return -9999.9;}
  virtual void set_directionVtx(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionVtx()"); return;}

 virtual float get_projectionHbd(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionHbd()"); return -9999.9;}
  virtual void set_projectionHbd(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionHbd()"); return;}

  virtual float get_directionDch(const unsigned int itrk, const short ival) const {virtual_warning("get_directionDch()"); return -9999.9;}
  virtual void set_directionDch(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionDch()"); return;}

  virtual float get_directionTec(const unsigned int itrk, const short ival) const {virtual_warning("get_directionTec()"); return -9999.9;}
  virtual void set_directionTec(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionTec()"); return;}

  virtual float get_directionPc1(const unsigned int itrk, const short ival) const {virtual_warning("get_directionPc1()"); return -9999.9;}
  virtual void set_directionPc1(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionPc1()"); return;}

  virtual float get_directionPc2(const unsigned int itrk, const short ival) const {virtual_warning("get_directionPc2()"); return -9999.9;}
  virtual void set_directionPc2(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionPc2()"); return;}

  virtual float get_directionPc3(const unsigned int itrk, const short ival) const {virtual_warning("get_directionPc3()"); return -9999.9;}
  virtual void set_directionPc3(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionPc3()"); return;}

  virtual float get_directionSvx(const unsigned int itrk, const short ilayer, const short ival) const {virtual_warning("get_directionSvx()"); return -9999.9;}
  virtual void set_directionSvx(const unsigned int itrk, const short ilayer, const short ival, const float rval) {virtual_warning("set_directionSvx()"); return;}

  virtual float get_directionTof(const unsigned int itrk, const short ival) const {virtual_warning("get_directionTof()"); return -9999.9;}
  virtual void set_directionTof(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionTof()"); return;}
  virtual float get_directionEmc(const unsigned int itrk, const short ival) const {virtual_warning("get_directionEmc()"); return -9999.9;}
  virtual void set_directionPbEmc(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionPbEmc()"); return;}
  virtual float get_directionPbGl(const unsigned int itrk, const short ival) const {virtual_warning("get_directionPbGl()"); return -9999.9;}
  virtual void set_directionPbGl(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionPbGl()"); return;}
  virtual float get_directionPbSc(const unsigned int itrk, const short ival) const {virtual_warning("get_directionPbSc()"); return -9999.9;}
  virtual void set_directionPbSc(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionPbSc()"); return;}

  virtual float get_directionCrk(const unsigned int itrk, const short ival) const {virtual_warning("get_directionCrk(()"); return -9999.9;}
  virtual void set_directionCrk(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionCrk()"); return;}

  virtual float get_directionTzr(const unsigned int itrk, const short ival) const {virtual_warning("get_directionTzr()"); return -9999.9;}
  virtual void set_directionTzr(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionTzr()"); return;}

  virtual float get_directionPcr(const unsigned int itrk, const short ival) const {virtual_warning("get_directionPcr()"); return -9999.9;}
  virtual void set_directionPcr(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionPcr()"); return;}

  virtual float get_directionAcc(const unsigned int itrk, const short ival) const {virtual_warning("get_directionAcc()"); return -9999.9;}
  virtual void set_directionAcc(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionAcc()"); return;}

  virtual float get_directionTofw(const unsigned int itrk, const short ival) const {virtual_warning("get_directionTofw()"); return -9999.9;}
  virtual void set_directionTofw(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionTofw(()"); return;}

  virtual float get_directionHbd(const unsigned int itrk, const short ival) const {virtual_warning("get_directionHbd()"); return -9999.9;}
  virtual void set_directionHbd(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionHbd(()"); return;}

  virtual float get_tofPathLength(const unsigned int itrk) const {virtual_warning("get_tofPathLength()"); return -9999.9;}
  virtual void set_tofPathLength(const unsigned int itrk, const float rval) {virtual_warning("set_tofPathLength()"); return;}
  virtual float get_emcPathLength(const unsigned int itrk) const {virtual_warning("get_emcPathLength()"); return -9999.9;}
  virtual void set_emcPathLength(const unsigned int itrk, const float rval) {virtual_warning("set_emcPathLength()"); return;}
  virtual float get_crkPathLength(const unsigned int itrk) const {virtual_warning("get_crkPathLength()"); return -9999.9;}
  virtual void set_crkPathLength(const unsigned int itrk, const float  rval) {virtual_warning("set_crkPathLength()"); return;}
  virtual float get_tzrPathLength(const unsigned int itrk) const {virtual_warning("get_tzrPathLength()"); return -9999.9;}
  virtual void set_tzrPathLength(const unsigned int itrk, const float  rval) {virtual_warning("set_tzrPathLength()"); return;}
  virtual float get_tofwPathLength(const unsigned int itrk) const {virtual_warning("get_tofwPathLength()"); return -9999.9;}
  virtual void set_tofwPathLength(const unsigned int itrk, const float  rval) {virtual_warning("set_tofwPathLength()"); return;}


  virtual short get_ifIntersectVtx(const unsigned int itrk) const {virtual_warning("get_ifIntersectVtx()"); return 0;}
  virtual short get_ifIntersectDch(const unsigned int itrk) const {virtual_warning("get_ifIntersectDch()"); return 0;}
  virtual short get_ifIntersectPc1(const unsigned int itrk) const {virtual_warning("get_ifIntersectPc1()"); return 0;}
  virtual short get_ifIntersectPc2(const unsigned int itrk) const {virtual_warning("get_ifIntersectPc2()"); return 0;}
  virtual short get_ifIntersectPc3(const unsigned int itrk) const {virtual_warning("get_ifIntersectPc3()"); return 0;}
  virtual short get_ifIntersectSvx(const unsigned int itrk, const short ilayer) const {virtual_warning("get_ifIntersectSvx()"); return 0;}
  virtual short get_ifIntersectCrk(const unsigned int itrk) const {virtual_warning("get_ifIntersectCrk()"); return 0;}
  virtual short get_ifIntersectTec(const unsigned int itrk) const {virtual_warning("get_ifIntersectTec()"); return 0;}
  virtual short get_ifIntersectTof(const unsigned int itrk) const {virtual_warning("get_ifIntersectTof()"); return 0;}
  virtual short get_ifIntersectEmc(const unsigned int itrk) const {virtual_warning("get_ifIntersectEmc()"); return 0;}
  virtual short get_ifIntersectPbgl(const unsigned int itrk) const {virtual_warning("get_ifIntersectPbgl()"); return 0;}
  virtual short get_ifIntersectPbsc(const unsigned int itrk) const {virtual_warning("get_ifIntersectPbsc()"); return 0;}
  virtual short get_ifIntersectTzr(const unsigned int itrk) const {virtual_warning("get_ifIntersectTzr()"); return 0;}
  virtual short get_ifIntersectPcr(const unsigned int itrk) const {virtual_warning("get_ifIntersectPcr()"); return 0;}
  virtual short get_ifIntersectAcc(const unsigned int itrk) const {virtual_warning("get_ifIntersectAcc()"); return 0;}
  virtual short get_ifIntersectTofw(const unsigned int itrk) const {virtual_warning("get_ifIntersectTofw()"); return 0;}
  virtual short get_ifIntersectHbd(const unsigned int itrk) const {virtual_warning("get_ifIntersectHbd()"); return 0;}

  virtual short ifIntersectVtx(const unsigned int itrk) const {virtual_warning("ifIntersectVtx()"); return 0;}
  virtual short ifIntersectDch(const unsigned int itrk) const {virtual_warning("ifIntersectDch()"); return 0;}
  virtual short ifIntersectPc1(const unsigned int itrk) const {virtual_warning("ifIntersectPc1()"); return 0;}
  virtual short ifIntersectPc2(const unsigned int itrk) const {virtual_warning("ifIntersectPc2()"); return 0;}
  virtual short ifIntersectPc3(const unsigned int itrk) const {virtual_warning("ifIntersectPc3()"); return 0;}
  virtual short ifIntersectSvx(const unsigned int itrk, const short ilayer) const {virtual_warning("ifIntersectSvx()"); return 0;}
  virtual short ifIntersectCrk(const unsigned int itrk) const {virtual_warning("ifIntersectCrk()"); return 0;}
  virtual short ifIntersectTec(const unsigned int itrk) const {virtual_warning("ifIntersectTec()"); return 0;}
  virtual short ifIntersectTof(const unsigned int itrk) const {virtual_warning("ifIntersectTof()"); return 0;}
  virtual short ifIntersectEmc(const unsigned int itrk) const {virtual_warning("ifIntersectEmc()"); return 0;}
  virtual short ifIntersectPbgl(const unsigned int itrk) const {virtual_warning("ifIntersectPbgl()"); return 0;}
  virtual short ifIntersectPbsc(const unsigned int itrk) const {virtual_warning("ifIntersectPbsc()"); return 0;}
  virtual short ifIntersectTzr(const unsigned int itrk) const {virtual_warning("ifIntersectTzr()"); return 0;}
  virtual short ifIntersectPcr(const unsigned int itrk) const {virtual_warning("ifIntersectPcr()"); return 0;}
  virtual short ifIntersectAcc(const unsigned int itrk) const {virtual_warning("ifIntersectAcc()"); return 0;}
  virtual short ifIntersectTofw(const unsigned int itrk) const {virtual_warning("ifIntersectTofw()"); return 0;}
  virtual short ifIntersectHbd(const unsigned int itrk) const {virtual_warning("ifIntersectHbd()"); return 0;}

  virtual float get_projectionMrpc(const unsigned int itrk, const short ival) const {virtual_warning("get_projectionMrpc()"); return -9999.9;}
virtual void set_projectionMrpc(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_projectionMrpc()"); return;}
  virtual float get_directionMrpc(const unsigned int itrk, const short ival) const {virtual_warning("get_directionMrpc()"); return -9999.9;}
  virtual void set_directionMrpc(const unsigned int itrk, const short ival, const float rval) {virtual_warning("set_directionMrpc(()"); return;}
  virtual float get_mrpcPathLength(const unsigned int itrk) const {virtual_warning("get_mrpcPathLength()"); return -9999.9;}
  virtual void set_mrpcPathLength(const unsigned int itrk, const float  rval) {virtual_warning("set_mrpcPathLength()"); return;}
  virtual short get_ifIntersectMrpc(const unsigned int itrk) const {virtual_warning("get_ifIntersectMrpc()"); return 0;}
  virtual short ifIntersectMrpc(const unsigned int itrk) const {virtual_warning("ifIntersectMrpc()"); return 0;}


  virtual void ShutUp(const int i = 1);
  virtual PHSnglTrack* get_track(const unsigned int itrk) const;

 protected:

  virtual TClonesArray *GetPHTrk() const;


 private:
  void  virtual_warning(const char *funcname) const;

  ClassDef(PHTrackOut,1)
};

#endif /*__DPHTRACKOUT_H__*/
