#ifndef __PDBEMCTOFSECTOROFFSET_HH__
#define __PDBEMCTOFSECTOROFFSET_HH__

#include "PdbCalChan.hh"

/** Information related to global timing offset for one emcal sector.
 */

class PdbEmcTofSectorOffset : public PdbCalChan
{
 public:
  PdbEmcTofSectorOffset();

  virtual ~PdbEmcTofSectorOffset();

  virtual void print() const;

  void setProcessInfo(int runnumber, int numberOfEvents);

  void setPeak(float peak, float width);

  void setGausPeak(float peak, float width);

  void setBBC(float t0, float rms);
  void setTOF(float t0, float rms);

  float peak() const { return fPeak; }
  float width() const { return fWidth; }

  float gausPeak() const { return fGausPeak; }
  float gausWidth() const { return fGausWidth; }

  float BBCT0() const { return fBBCT0; }
  float BBCT0rms() const { return fBBCT0rms; }

  float TOFT0() const { return fTOFT0; }
  float TOFT0rms() const { return fTOFT0rms; }

  int runnumber() const { return fRunNumber; }
  int numberOfEvents() const { return fNevents; }

 private:

  int fRunNumber;
  int fNevents;
  float fPeak;
  float fWidth;
  float fGausPeak;
  float fGausWidth;
  float fBBCT0;
  float fBBCT0rms;
  float fTOFT0;
  float fTOFT0rms;

  ClassDef(PdbEmcTofSectorOffset,1)
};

#endif
