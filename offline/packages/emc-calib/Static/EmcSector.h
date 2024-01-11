#ifndef __EMCSECTOR_H__
#define __EMCSECTOR_H__

#include <string>

class EmcSuperModule;

/** ABC of an EMCAL Sector.
    It provides just the basic framework for anything we like 
    to call PbSc and PbGl Sectors. 
*/

class EmcSector
{
 public:

  EmcSector();
  virtual  ~EmcSector();

 public:

  ///
  virtual EmcSuperModule * getSuperModule(int SMnumber) = 0;
  ///
  virtual void GetEnergyCalibration(int, float&, float&, float&) = 0;  

  /// Sets Sector Identifier
  virtual void setSectorId(const char *EmcSector);

  ///
  virtual void CorrectEnergyCalibration(const char* /*fname*/) { }

  /// Is this Sector object useable (i.e. was it properly created ?)
  virtual bool IsOK(void) const { return fIsOK; }
  virtual bool getStatus(void) const { return IsOK(); }

protected:

  std::string SectorId;
  int SectorN;
  bool fIsOK;
};

#endif
