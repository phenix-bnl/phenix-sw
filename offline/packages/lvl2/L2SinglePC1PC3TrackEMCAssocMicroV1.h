//////////////////////////////////////////////////////////////////
//
// Root object that stores a single Level-2 track-emc association
//   Used in the code to write the Level-2 primitive L2EMCAssociator
//   to the ()DST.
//
// Author: Cole
// Date: 3-Feb-2003
//
// $Log: L2SinglePC1PC3TrackEMCAssocMicroV1.h,v $
// Revision 1.3  2007/06/08 00:48:50  phnxlvl2
// Changed all includes from quote form to bracket form to avoid include path problems when compiling
//
// Revision 1.2  2004/07/28 19:56:04  pinkenbu
// use proper TObject include file
//
// Revision 1.1  2003/02/25 21:13:07  cole
// Adding L2PC1PC3EMCAssoc classes for storing PC1PC3 track associations with EMCAl clusters
//
//
/////////////////////////////////////////////////////////////////

#ifndef __L2SinglePC1PC3TrackEMCAssocMicroV1_H__
#define __L2SinglePC1PC3TrackEMCAssocMicroV1_H__

#include <TObject.h>

class L2SinglePC1PC3TrackEMCAssocMicroV1 : public TObject 
{
 private:
  
  unsigned int _trackIndex;
  unsigned int _clusterIndex;
  float _energy;
  float _deltarphi;
  float _deltaz;

 public:

  L2SinglePC1PC3TrackEMCAssocMicroV1(int trackIndex, int clusterIndex, 
				     float energy, float deltarphi, float deltaz) :
    _trackIndex(trackIndex),
    _clusterIndex(clusterIndex),
    _energy(energy),
    _deltarphi(deltarphi),
    _deltaz(deltaz)
  {}

  ~L2SinglePC1PC3TrackEMCAssocMicroV1(){}

  unsigned int get_trackIndex() const {return _trackIndex;}
  unsigned int get_clusterIndex() const {return _clusterIndex;}
  float get_energy() const {return _energy;}
  float get_deltarphi() const {return _deltarphi;}
  float get_deltaz() const {return _deltaz;}

  ClassDef (L2SinglePC1PC3TrackEMCAssocMicroV1, 1)
};
#endif	// __L2SinglePC1PC3TrackEMCAssocMicroV1_H__








