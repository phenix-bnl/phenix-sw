#ifndef  __PHTRACKOUTV4_H
#define  __PHTRACKOUTV4_H

#include "PHTrackOut.h"
#include <iostream>

class TClonesArray;

class PHTrackOutv4:public PHTrackOut
{
 public:
  PHTrackOutv4();
  virtual ~PHTrackOutv4();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os=std::cout) const;

  unsigned int get_PHNTrack() const;
  void set_PHNTrack(const unsigned int ntrk);
  void AddPHTrack(const unsigned int itrk);

  unsigned short get_trackIndex(const unsigned int itrk)   const ;
  void  set_trackIndex(const unsigned int itrk, const unsigned short val);

  float get_projectionVtx(const unsigned int itrk, const short ival) const ;
  void set_projectionVtx(const unsigned int itrk, const short ival, const float rval);
  
  float get_projectionDch(const unsigned int itrk, const short ival) const;
  void set_projectionDch(const unsigned int itrk, const short ival, const float rval);
  
  float get_projectionTec(const unsigned int itrk, const short ival) const; 
  void set_projectionTec(const unsigned int itrk, const short ival, const float rval);
  
  float get_projectionPc1(const unsigned int itrk, const short ival) const;
  void set_projectionPc1(const unsigned int itrk, const short ival, const float rval);
 
  float get_projectionPc2(const unsigned int itrk, const short ival) const;
  void set_projectionPc2(const unsigned int itrk, const short ival, const float rval);
  
  float get_projectionPc3(const unsigned int itrk, const short ival) const;
  void set_projectionPc3(const unsigned int itrk, const short ival, const float rval);
   
  float get_projectionTof(const unsigned int itrk, const short ival) const;
  void set_projectionTof(const unsigned int itrk, const short ival, const float rval);
  
  float get_projectionCrk(const unsigned int itrk, const short ival) const;
  void set_projectionCrk(const unsigned int itrk, const short ival, const float rval);
  float get_projectionPbGl(const unsigned int itrk, const short ival) const;
  void set_projectionPbGl(const unsigned int itrk, const short ival, const float rval);
  float get_projectionPbSc(const unsigned int itrk, const short ival) const;
  void set_projectionPbSc(const unsigned int itrk, const short ival, const float rval);

  float get_projectionAcc(const unsigned int itrk, const short ival) const;
  void set_projectionAcc(const unsigned int itrk, const short ival, const float rval);

  float get_projectionEmc(const unsigned int itrk, const short ival) const;

  float get_directionVtx(const unsigned int itrk, const short ival) const ;
  void set_directionVtx(const unsigned int itrk, const short ival, const float rval);
  
  float get_directionDch(const unsigned int itrk, const short ival) const;
  void set_directionDch(const unsigned int itrk, const short ival, const float rval);
  
  float get_directionTec(const unsigned int itrk, const short ival) const; 
  void set_directionTec(const unsigned int itrk, const short ival, const float rval);
  
  float get_directionPc1(const unsigned int itrk, const short ival) const;
  void set_directionPc1(const unsigned int itrk, const short ival, const float rval);
 
  float get_directionPc2(const unsigned int itrk, const short ival) const;
  void set_directionPc2(const unsigned int itrk, const short ival, const float rval);
  
  float get_directionPc3(const unsigned int itrk, const short ival) const;
  void set_directionPc3(const unsigned int itrk, const short ival, const float rval);
  
  float get_directionTof(const unsigned int itrk, const short ival) const;
  void set_directionTof(const unsigned int itrk, const short ival, const float rval);
  
  float get_directionCrk(const unsigned int itrk, const short ival) const;
  void set_directionCrk(const unsigned int itrk, const short ival, const float rval);
  float get_directionPbGl(const unsigned int itrk, const short ival) const;
  void set_directionPbGl(const unsigned int itrk, const short ival, const float rval);
  float get_directionPbSc(const unsigned int itrk, const short ival) const;
  void set_directionPbSc(const unsigned int itrk, const short ival, const float rval);
  float get_directionAcc(const unsigned int itrk, const short ival) const;
  void set_directionAcc(const unsigned int itrk, const short ival, const float rval);
   
  short ifIntersectVtx(const unsigned int itrk) const;
  short ifIntersectDch(const unsigned int itrk) const;
  short ifIntersectPc1(const unsigned int itrk) const;
  short ifIntersectPc2(const unsigned int itrk) const;
  short ifIntersectPc3(const unsigned int itrk) const;
  short ifIntersectCrk(const unsigned int itrk) const;
  short ifIntersectEmc(const unsigned int itrk) const;
  short ifIntersectTec(const unsigned int itrk) const;
  short ifIntersectTof(const unsigned int itrk) const;
  short ifIntersectPbgl(const unsigned int itrk) const;
  short ifIntersectPbsc(const unsigned int itrk) const;
  short ifIntersectAcc(const unsigned int itrk) const;
   
  short get_ifIntersectVtx(const unsigned int itrk) const {return ifIntersectVtx(itrk);}
  short get_ifIntersectDch(const unsigned int itrk) const {return ifIntersectDch(itrk);}
  short get_ifIntersectPc1(const unsigned int itrk) const {return ifIntersectPc1(itrk);}
  short get_ifIntersectPc2(const unsigned int itrk) const {return ifIntersectPc2(itrk);}
  short get_ifIntersectPc3(const unsigned int itrk) const {return ifIntersectPc3(itrk);}
  short get_ifIntersectCrk(const unsigned int itrk) const {return ifIntersectCrk(itrk);}
  short get_ifIntersectEmc(const unsigned int itrk) const {return ifIntersectEmc(itrk);}
  short get_ifIntersectTec(const unsigned int itrk) const {return ifIntersectTec(itrk);}
  short get_ifIntersectTof(const unsigned int itrk) const {return ifIntersectTof(itrk);}
  short get_ifIntersectPbgl(const unsigned int itrk) const {return ifIntersectPbgl(itrk);}
  short get_ifIntersectPbsc(const unsigned int itrk) const {return ifIntersectPbsc(itrk);}
  short get_ifIntersectAcc(const unsigned int itrk) const {return ifIntersectAcc(itrk);}
   
  float get_tofPathLength(const unsigned int itrk) const ;
  void set_tofPathLength(const unsigned int itrk, const float rval);
  float get_crkPathLength(const unsigned int itrk) const;
  void set_crkPathLength(const unsigned int itrk, const float rval);
  float get_emcPathLength(const unsigned int itrk) const;
  void set_emcPathLength(const unsigned int itrk, const float rval);
  
 protected:
  TClonesArray *GetPHTrk() const {return PHTrk;}
  unsigned int PHNTrack;
  TClonesArray *PHTrk;
  ClassDef(PHTrackOutv4,1)
};
#endif
