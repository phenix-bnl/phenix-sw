// $Id: MutPlane.h,v 1.28 2008/12/05 22:18:01 hpereira Exp $

#ifndef __MUTPLANE_HH__
#define __MUTPLANE_HH__

/*!
   \file MutPlane.h
   \brief Muon tracker Plane (cathode/anode)
   \author Douglas Fields, Nicki Bruner, Hugo Pereira
   \version $Revision: 1.28 $
   \date $Date: 2008/12/05 22:18:01 $
*/

#include <iosfwd>
#include <string>
#include <stdexcept>
#include <set>
#include <vector>

#include "MutGeomObject.h"
#include "MutArm.h"
#include "MutStation.h"
#include "MutOctant.h"
#include "MutHalfOctant.h"
#include "MutGap.h"

// forward declarations ...
class MutStrip;
class MutWire;

//! Describes a Plane of the muon tracker system. 
class MutPlane : public MutGeomObject
{
  public:

  //! Real constructor.
  MutPlane(const MutArm* Arm,
         const MutStation* Station,
         const MutOctant* Octant,
         const MutHalfOctant* HalfOctant,
         const MutGap* Gap,
         const MUTGEOM::PlaneNumber& PlaneNum);

  //! Copy constructor.
  MutPlane(const MutPlane& rhs)
  { throw std::logic_error( "MutPlane::MutPlane - use of copy constructor is forbidden" ); }
  
  //! Destructor.
  ~MutPlane( void );

  //!@name identifier and parent object accessors
  //@{
  
  //! Arm identifier
  MUTGEOM::ArmNumber getArm() const 
  {return f_pArm->getArm();}

  //! Station identifier
  MUTGEOM::StationNumber getStation() const 
  {return f_pStation->getStation();}

  //! Octant identifier
  MUTGEOM::OctantNumber getOctant() const
  {return f_pOctant->getOctant();}

  //! HalfOctant identifier
  MUTGEOM::HalfOctantNumber getHalfOctant() const
  {return f_pHalfOctant->getHalfOctant();}

  //! Gap identifier
  MUTGEOM::GapNumber getGap() const 
  {return f_pGap->getGap();}

  //! Plane identifier
  MUTGEOM::PlaneNumber getPlane() const 
  {return fPlaneNum;}

  //@}
  
  //! number of strips/wires
  int getNumElements() const 
  {return NumElements;}
  
  //! strip spacing
  double getStripSpacing() const 
  {return stripPerpSpacing;}
  
  //! plane offset allong z
  PHVector getplaneOffset() const
  {return planeOffset;}

  //! disabled anode cards
  const std::set<int>& getDisabledAnodeCards() const
  { return _disabled_anode_cards; }
  
  //! disabled anode cards
  void disableAnodeCard( const int& card_id )
  { _disabled_anode_cards.insert( card_id ); }
  
  //! disable anode cards
  void clearDisabledAnodeCards( void )
  { _disabled_anode_cards.clear(); }
  
  private:

  //! parent arm
  const MutArm* f_pArm;
  
  //! parent station
  const MutStation* f_pStation;
  
  //! parent octant
  const MutOctant* f_pOctant;
  
  //! parent half octant
  const MutHalfOctant* f_pHalfOctant;
  
  //! parent gap
  const MutGap* f_pGap;
  
  //! plane id
  MUTGEOM::PlaneNumber fPlaneNum;
  
  //! number of strips/wires in the plane
  int NumElements;
  
  /*!  
    difference between the Plane's global position 
    and the gap's global position
  */
  PHVector planeOffset;  
  
  //! the perpendicular distance between strips.
  double stripPerpSpacing; 

  //! put strip in array
  /*! check against array overflow and multiple assignment */
  void AddStrip(const int& StripNum, MutStrip* strip);  
  
  //! put strip in array
  /*! check against array overflow and multiple assignment */
  void AddWire(const int& WireNum, MutWire* wire);
  
  //! station1 wires
  void createSt1Wires( void );
  
  //! station2 wires
  void createSt2Wires( void );
  
  //! station3 wires 
  void createSt3Wires( void ); 
  
  //! station1 strips
  void createSt1Strips( void );
  
  //! station2 strips
  void createSt2Strips( void );
  
  //! station3 strips
  void createSt3Strips( void );

  //! station1 strip geometry from database
  void fetchSt1Strips( void );

  //! name of survey alignment file for station 1
  std::string getSt1AutocadFile();

  //! name of survey alignment file for station 3
  std::string getSt3HalfOctSrvyFile();
  
  //! reads station 3 survey file 
  bool fileReadSt3StripSrvyData(double surveyData[13]);

  //! reads station 3 survey file 
  bool fetchSt3StripSrvyData(double surveyData[13]);
  
  //! survey points 
  // void getSt3HalfOctFramePoints(PHPoint surveyPin[8]);
  std::vector<PHPoint> getSt3HalfOctFramePoints( void );
  
  /*! 
  get the rotation and translation that accounts for the station3 survey
  and converts the half octant frame into the octant frame
  */
  //void getSt3SurveyFrame( PHVector& translation, PHMatrix& rotation );
  void getSt3SurveyFrame( PHFrame& half_octant_frame, PHFrame& octant_frame );

  //! DCM channels
  void getDCMChannels( std::string tag="" );
  
  //! DCM channels
  void fetchDCMChannels( void );
  
  //! strip spacing
  void setStripSpacing( void );

  //! list of disabled anode cards
  std::set<int> _disabled_anode_cards;

  public:

  //! Pointers to this Planes' Strips 
  std::vector<MutStrip *> f_pMutStrips;  

  //! Pointers to this Planes' Wires 
  std::vector<MutWire *> f_pMutWires;    
 
  //! print object
  virtual void print() const;
  
  //! print object
  virtual void printStripSpacing() const;

  //! translate position of plane and its strips and wires.
  void translate(const PHPoint &translation);

  //! Rotate the position and orientation of the plane and its elements.
  void rotate(float rotAngle, char axisLabel);

  //! Expand strips and wires within the half-octant planes.
  void XYExpansion(float expansionPercentage);

  //! frame transformation
  void transformToNewFrame(PHFrame oldFrame, PHFrame newFrame);

  /*!
    Rotate and translate the member elements based on fetched database
    calibration info.  Currently, the plane position/vector are fetched 
    and filled prior to updating the strips/wires. So the old position 
    and orientation are passed as parameters.
  */
  void updateElements(const PHPoint oldPlanePosition, const PHVector oldPlaneVector);

  /*!
    Write station 1 strip positions in the plane to the database.
    (Basically a copy of the various .dat files.)
  */  
  void updateSt1Strips(PHTimeStamp &Tstart, PHTimeStamp &Tstop);
  
  //! write station 3 strip survey position
  void updateSt3StripSrvyData(PHTimeStamp &Tstart, PHTimeStamp &Tstop);

};

#endif   /* __MutPlane_HH__ */
