#ifndef __PHCENTRALTRACKV21_H
#define __PHCENTRALTRACKV21_H

#include <iostream>
#include "PHCentralTrack.h"

class PHSnglCentralTrack;
class TClonesArray;

class PHCentralTrackv21 : public PHCentralTrack
{
 public:
  PHCentralTrackv21();
  PHCentralTrackv21(const PHCentralTrackv21&);
  PHCentralTrackv21& operator=(const PHCentralTrackv21&);
  virtual ~PHCentralTrackv21();

  PHCentralTrackv21* clone() const;

  // The "standard response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Actual implementations of the "set" methods...
  // First the methods from PHParticle:
  void set_npart      (const unsigned int NTRACK) {nCentral = NTRACK; return;}


  // Actual implementations of the "get" methods...
  unsigned int get_npart() const {return nCentral;}

  // Routines to manipulate the particle array
  int set_TClonesArraySize(const unsigned int ntrk);
  void AddPHParticle      (const unsigned int itrk);
  void RemovePHParticle   (const unsigned int itrk);

 protected:
  TClonesArray *GetCentral() const {return Central;}
  unsigned int nCentral;
  TClonesArray *Central;

private:
  // Copy this to dest.
  void copyto(PHCentralTrackv21& dest) const;

  ClassDef(PHCentralTrackv21,1)
};

#endif /* __PHCENTRALTRACKV21_H */
