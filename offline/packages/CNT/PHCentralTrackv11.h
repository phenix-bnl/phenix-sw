#ifndef __PHCENTRALTRACKV11_H
#define __PHCENTRALTRACKV11_H

#include "PHCentralTrack.h"
#include <iostream>

//
//  This class is a so-called "versioned" object.  We are
//  inheriting from the PHCentralTrack.  That virtual base 
//  class has fields which are specific to central arm tracks.
//  However, since the PHCentralTrack inherits from PHParticle,
//  we get our 4-vector from there.
//
//  Following the PHENIX Schema Evolution system we choose to
//  make meaningful implementations of all those fields which we
//  want to keep in this particular version of a PHCentralTrack.
//
//  We also chose to *not* override functions that we don't want.
//  As examples of these, I conclude that since we don't make final
//  decisions on the PID in this version of track, we should not 
//  implement either the PID function (int ID number of particle),
//  nor should we implement the Emass (Energy of the particle based
//  upon the known mass).  For that reason, I have commented out the 
//  lines which would have implemented these methods.
//
//  The name of this file is not really chosen by a particle type.
//  The name refers to the fact that the CentralTrack working group
//  will be making nDSTs containing PHCentralTracks.  
//                                 TKH 3-7-2002
//
//  This is the "v4" version which is the candidate for CNT tracks
//  that we will keep in the QM2002 analysis.
//                                 TKH 4-17-2002
//
//  This is the "v6" version which is the candidate for CNT tracks
//  that we will keep in the QM2002 analysis of pp data.
//                                 TKH 6-21-2002
//  This is the "v9" version that is made available to OnCal online
//  calibrations.
//                                 TKH 1-9-2003
//
//  This is the "v11" version that is the first candidate for offline
//  production from the dAu data.  Initially, the developing version indeed
//  contains the same fields as the v9 version.  The major changes are to the
//  infrastructure that allows for copying PHSnglCentralTracks from another
//  container to the local one without the user being sensitive to the
//  underlying version number.  I do anticipate, however, that these tracks
//  will additionally evolve before they are settled and therefore I
//  begin the infrastructure changes with a version upgrade at the same time.
//                                 TKH 1-9-2003
//
//  This is the v11 version that is the final choice for Run3 data.  It removes the
//  so-called "swap" variables (since these only work correctly in events with
//  uniform particle density) and also removes the redundancy of the so-called 
//  "absolutes".  However, at popular request, I will retain the dz and dphi
//  even though these are simple 1-1 mappings for the sdz and sdphi.
//                                 TKH 8-3-2003
//

class TClonesArray;

class PHCentralTrackv11 : public PHCentralTrack
{
 public:
  PHCentralTrackv11(int fetch=0);
  PHCentralTrackv11(const PHCentralTrackv11&);
  PHCentralTrackv11& operator=(const PHCentralTrackv11&);
  virtual ~PHCentralTrackv11();

  PHCentralTrackv11* clone() const;

  // The "standard response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os = std::cout) const;

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
  void copyto(PHCentralTrackv11& dest) const;

  ClassDef(PHCentralTrackv11,1)
};

#endif /* __PHCENTRALTRACKV11_H */
