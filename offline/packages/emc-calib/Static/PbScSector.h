#ifndef __PBSCSECTOR_H__
#define __PBSCSECTOR_H__

#include <Rtypes.h>
#ifndef __EMCSECTOR_H__
#include "EmcSector.h"
#endif

#include <vector>

class PHTimeStamp;

/** PbSc implementation of EmcSector.
       
Its Basic function is to store Enrgy Calibration data ordered in 
the sequence of towers within one Sector (numbering starts at 0 in the 
leftmost bottom corner of the Sector viewed from the readout side and 
grows left to right, bottom to top).

Any class which needs direct access to calibration data must either 
be declared  as a friend below or to use indirect access using public 
access functions provided by class.

@author:  E.Kistenev

Created: 03/01/99

Last modification: June-16-2001 by L. Aphecetche

*/

class PbScSector: public EmcSector
{
 public:

  PbScSector(int &, PHTimeStamp* ts = 0 );

  virtual ~PbScSector();

  ///
  virtual EmcSuperModule * getSuperModule(int SMNumber) {
    return SM[SMNumber];
  }

  /** Probably the most important to off-line - this is the approved 
      access path to initial energy calibration for PbSc Calorimeter */
  virtual void GetEnergyCalibration(int, float&, float&, float&);  

  void CorrectEnergyCalibration(const char * fname);

  static size_t GetSize(void) { return fgSize; }
  static size_t size(void) { return GetSize(); }
  static size_t Size(void) { return GetSize(); }

  static size_t NumberOfSuperModules(void) { 
    return fgNumberOfSuperModules; 
  }

 private:

  void BuildFromFiles(void);
  void BuildFromDB(PHTimeStamp*);
  void Reset(void);

 protected:

  std::vector<EmcSuperModule*> SM;
  std::vector<float> fECalib; //  Energy calibration (GeV/ADC count) at t=0
  std::vector<float> fNorm0;  //  Normalization at t=0 (Laser/Ref*TestPulse)

private:
  static size_t fgSize;
  static size_t fgNumberOfSuperModules;
};

#endif
