#ifndef __PHDCHTRACKOUTV1_H__
#define __PHDCHTRACKOUTV1_H__

#include "PHDchTrackOut.h"
#include <iostream>


class dPHDchTrackWrapper;
class TClonesArray;

class PHDchTrackOutv1 : public PHDchTrackOut
{
 public:
  PHDchTrackOutv1();
  virtual ~PHDchTrackOutv1();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;

  unsigned int get_PHDchNTrack() const {return PHDchNTrack;}
  void set_PHDchNTrack(const unsigned int ntrk) {PHDchNTrack = ntrk; return;}

  int set_TClonesArraySize(const unsigned int ntrk);
  void AddPHDchTrack(const unsigned int itrk);

  void FillFromWrapper(dPHDchTrackWrapper *wrap);

  short get_ErrorCode(const unsigned int itrk)   const;
  void  set_ErrorCode(const unsigned int itrk, const short val);

  short get_numberOfSuccessfulIterations(const unsigned int itrk)   const;
  void  set_numberOfSuccessfulIterations(const unsigned int itrk, const short val);

  short get_numberOfX1X2hitsFitted(const unsigned int itrk)   const;
  void  set_numberOfX1X2hitsFitted(const unsigned int itrk, const short val);



  float get_chi2(const unsigned int itrk) const;
  void set_chi2(const unsigned int itrk, const float rval);

  float get_fittedAlpha(const unsigned int itrk) const;
  void set_fittedAlpha(const unsigned int itrk, const float rval);

  float get_fittedBeta(const unsigned int itrk) const;
  void set_fittedBeta(const unsigned int itrk, const float rval);

  float get_fittedPhi(const unsigned int itrk) const;
  void set_fittedPhi(const unsigned int itrk, const float rval);

  float get_fittedPhi0(const unsigned int itrk) const;
  void set_fittedPhi0(const unsigned int itrk, const float rval);

  float get_fittedTheta0(const unsigned int itrk) const;
  void set_fittedTheta0(const unsigned int itrk, const float rval);

  float get_momentum(const unsigned int itrk) const;
  void set_momentum(const unsigned int itrk, const float rval);


 protected:
  TClonesArray *GetPHDchTrk() const {return PHDchTrk;}
  unsigned int PHDchNTrack;
  TClonesArray *PHDchTrk;

  ClassDef(PHDchTrackOutv1,1)
};

#endif /*__PHDCHTRACKOUTV1_H__*/
