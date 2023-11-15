#ifndef __PDBEMCTOWERREJECT_HH__
#define __PDBEMCTOWERREJECT_HH__

#include "PdbCalChan.hh"

/**
   Store the so-called EMCAL Reject List. 
   Tower-based. For each tower, has 4 flags : 2 for amplitude 
   (1 error, 1 warning) and 2 for timing (1 error, 1 warning).
   A flag of 0 means "ok=no warning/error".
   fTowerId ranges from 0 to 24768.
   (see e.g. offline/packages/emc-calib/EmcIndexer.h for more information).
*/

class PdbEmcTowerReject : public PdbCalChan
{
public:
  PdbEmcTowerReject();
  virtual ~PdbEmcTowerReject() {}

  int AmplitudeError() const { return fAmplitudeError; }
  int AmplitudeWarning() const { return fAmplitudeWarning; }
  int TimingError() const { return fTimingError; }
  int TimingWarning() const { return fTimingWarning; }
  int TowerId() const { return fTowerId; }

  virtual void print() const;

  /** WARNING: this is a low level object. No check made on towerid.
      Mind your steps. */
  void set(int towerid, int amp_error, int amp_warning, 
	   int timing_error, int timing_warning);

private:
  int fTowerId;
  int fAmplitudeError;
  int fAmplitudeWarning;
  int fTimingError;
  int fTimingWarning;

  ClassDef(PdbEmcTowerReject,1);
};
#endif
