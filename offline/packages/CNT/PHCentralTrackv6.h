#ifndef __PHCENTRALTRACKV6_H
#define __PHCENTRALTRACKV6_H

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
//

class TClonesArray;

class PHCentralTrackv6 : public PHCentralTrack
{
 public:
  PHCentralTrackv6(int fetch=0);
  virtual ~PHCentralTrackv6();

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

  float sigmaAlpha;
  float sigmaMultScatt;
  float sigmaTOF;
  float sigmaEMC;
  float k1;

  ClassDef(PHCentralTrackv6,1)
};

#endif /* __PHCENTRALTRACKV6_H */






