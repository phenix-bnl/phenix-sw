#ifndef  __PHTRACKOUTV7_H
#define  __PHTRACKOUTV7_H

#include <PHTrackOut.h>

#include <iostream>

class PHTrackOutv7:public PHTrackOut
{
 public:
  PHTrackOutv7();
  virtual ~PHTrackOutv7();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;

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

  float get_projectionHbd(const unsigned int itrk, const short ival) const;
  void set_projectionHbd(const unsigned int itrk, const short ival, const float rval);

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
  
  float get_directionHbd(const unsigned int itrk, const short ival) const;
  void set_directionHbd(const unsigned int itrk, const short ival, const float rval);
  

// Only add ifIntersectHbd here, and inherit other ifIntersect functions from v5.
  short ifIntersectHbd(const unsigned int itrk) const;
   
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

  ClassDef(PHTrackOutv7,1)
};
#endif
