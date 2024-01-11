#ifndef __PHDCHTRACKOUT_H__
#define __PHDCHTRACKOUT_H__

#include <iostream>
#include "phool.h"
#include "PHObject.h"


class dPHDchTrackWrapper;

// with the following we get the line number of the virtual function we called with PHWHERE
#define PHDCH_VIRTUAL_WARNING std::cerr << PHWHERE << "using virtual function, doing nothing" << std::endl


class PHDchTrackOut : public PHObject
{
 public:
  virtual ~PHDchTrackOut() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;


  virtual void FillFromWrapper(dPHDchTrackWrapper *wrap) {PHDCH_VIRTUAL_WARNING; return;}

  virtual unsigned int get_PHDchNTrack() const {PHDCH_VIRTUAL_WARNING; return 0;}
  virtual void set_PHDchNTrack(const unsigned int ntrk) {PHDCH_VIRTUAL_WARNING; return;}

  virtual int set_TClonesArraySize(const unsigned int ntrk) {PHDCH_VIRTUAL_WARNING; return 0;}
  virtual void AddPHDchTrack(const unsigned int itrk) {PHDCH_VIRTUAL_WARNING; return;}

  virtual short get_ErrorCode(const unsigned int itrk)   const {PHDCH_VIRTUAL_WARNING; return -999;}
  virtual void  set_ErrorCode(const unsigned int itrk, const short val) {PHDCH_VIRTUAL_WARNING; return;}

  virtual short get_numberOfSuccessfulIterations(const unsigned int itrk)   const {PHDCH_VIRTUAL_WARNING; return -999;}
  virtual void  set_numberOfSuccessfulIterations(const unsigned int itrk, const short val) {PHDCH_VIRTUAL_WARNING; return;}

  virtual short get_numberOfX1X2hitsFitted(const unsigned int itrk)   const {PHDCH_VIRTUAL_WARNING; return -999;}
  virtual void  set_numberOfX1X2hitsFitted(const unsigned int itrk, const short val) {PHDCH_VIRTUAL_WARNING; return;}

  virtual float get_chi2(const unsigned int itrk) const {PHDCH_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_chi2(const unsigned int itrk, const float rval) {PHDCH_VIRTUAL_WARNING; return;}

  virtual float get_fittedAlpha(const unsigned int itrk) const {PHDCH_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_fittedAlpha(const unsigned int itrk, const float rval) {PHDCH_VIRTUAL_WARNING; return;}

  virtual float get_fittedBeta(const unsigned int itrk) const {PHDCH_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_fittedBeta(const unsigned int itrk, const float rval) {PHDCH_VIRTUAL_WARNING; return;}

  virtual float get_fittedPhi(const unsigned int itrk) const {PHDCH_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_fittedPhi(const unsigned int itrk, const float rval) {PHDCH_VIRTUAL_WARNING; return;}

  virtual float get_fittedPhi0(const unsigned int itrk) const {PHDCH_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_fittedPhi0(const unsigned int itrk, const float rval) {PHDCH_VIRTUAL_WARNING; return;}

  virtual float get_fittedTheta0(const unsigned int itrk) const {PHDCH_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_fittedTheta0(const unsigned int itrk, const float rval) {PHDCH_VIRTUAL_WARNING; return;}

  virtual float get_momentum(const unsigned int itrk) const {PHDCH_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_momentum(const unsigned int itrk, const float rval) {PHDCH_VIRTUAL_WARNING; return;}

  ClassDef(PHDchTrackOut,1)
};

#undef PHDCH_VIRTUAL_WARNING

#endif /*__PHDCHTRACKOUT_H__*/
