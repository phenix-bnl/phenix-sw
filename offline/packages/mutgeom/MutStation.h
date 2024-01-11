#ifndef __MUTSTATION_HH__
#define __MUTSTATION_HH__

// $Id: MutStation.h,v 1.25 2009/08/22 05:07:04 pinkenbu Exp $

/*!
  \file MutStation.h
  \brief Describes an Station of the muon tracker system. 
  \author Douglas Fields, Nicki Bruner
*/

#include <iostream>
#include <iosfwd>
#include <stdexcept>
#include "MutGeomObject.h"
#include "MutArm.h"
#include "MUTGEOM.h"

// forward declarations ...
class MutOctant;
class MutStrip;
class MutWire;

//! Describes an Station of the muon tracker system. 
class MutStation : public MutGeomObject
{
  public:

  //! Real constructor.
  MutStation(const MutArm* Arm,const MUTGEOM::StationNumber StationNum);

  //! Copy constructor.
  MutStation(const MutStation& rhs)
  { throw std::logic_error( "MutStation::MutStation - use of copy constructor is forbidden" ); }

  //! Destructor.
  ~MutStation();

  //! Arm identifier
  MUTGEOM::ArmNumber getArm() const
  {return f_pArm->getArm();}

  //! Station identifier
  MUTGEOM::StationNumber getStation() const 
  {return fStationNum;}

  //! Spacing of the anode wires
  double getWireSpacing() const 
  {return wireSpacing;}

  //! number of gaps in the half-octants - defined in MutHalfOctant.C
  int getNumberOfGaps() const 
  {return NumberOfGaps;}

  /*!
    fill a file with the x-y regions covered by each DCM
    also, fill a file with the  begin position and vector of first 
    strip in each DCM region
  */
  void getDCMRegions(const char *regionFile="DCMRegions.dat", const char *angleFile="DCMRegionStripAngles.dat");

private:

  //! parent arm 
  const MutArm* f_pArm;
  
  //! station id
  MUTGEOM::StationNumber fStationNum;
  
  //! spacing between anode wires
  double wireSpacing;
  
  //! number of gaps in station
  int NumberOfGaps;
  
  public:

  //! Pointers to this station's octants
  MutOctant *f_pMutOctants[MUTGEOM::NumberOfOctants];  

  //! Refresh an octant of the station.
  void RefreshOctant(const MUTGEOM::OctantNumber& OctantNum);

  //! Get position and orientation of object from a file.
  void GetGlobalGeom();

  /*! 
    Write the strip positions for each plane configuration to the database.
    (Basically, copy the various .dat files for station1.
    And copy the .dat files for station3.)
  */
  void updateStripPositions(PHTimeStamp &Tstart, PHTimeStamp &Tstop);
  
  //! write the strip position for each plane in station3
  void updateSt3HalfOctFrame(PHTimeStamp &Tstart, PHTimeStamp &Tstop, const char *file = "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/St3OctantSurveys.dat");

  //! print DCM channel map to file
  void printDCMChannelMap( std::string tag = "" ) const;
  
  //! write DCM to strip map to database
  void updateDCMChannelMap( PHTimeStamp &Tstart, PHTimeStamp &Tstop, std::string description );

  //! Translate position of Station and its Octants.
  void translate(const PHPoint &translation);

  //! Rotate the position and orientation of the station and its elements.
  void rotate(float angle, char axisLabel);

  //! Expand strips and wires within the half-octant planes.
  void XYExpansion(float expansionPercentage);

  //! Draw the strips and wires for all planes and write to a file.
  void Draw(const char *filename = "MutStationGeom.root", PHBoolean DrawDisabledOnly = False);

  //! Draw only the disabled strips or wires for all planes and write to a file.
  void DrawDisabledChannels(const char *filename = "MutDisabledChannels.root");

  /*! 
    For Monte Carlo reconstruction, return strip channel and distance 
    of hit from strip center given the simulated track position.
    \param PisaCoordinate is the (x,y,z) position of the hit in the plane.
    \param stripIP is the impact parameter of the hit to the midpoint of
    the closest strip.
    \param ptrStrip is a pointer to the strip object closest to the pisa hit.
    \param ptrWire is a pointer to the wire object closest to the pisa hit.
    \param wireIP is the impact parameter of the hit to the midpoint of
    the closest wire.
  */
  int convertPisaHitToChamber(PHPoint PisaCoordinate, MutWire *ptrWire[1],
             double &wireIP, MutStrip *ptrStrip[2], 
             double stripIP[2]);
};

#endif   /* __MutStation_HH__ */

