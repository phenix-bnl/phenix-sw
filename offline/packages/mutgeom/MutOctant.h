#ifndef __MUTOCTANT_HH__
#define __MUTOCTANT_HH__

// $Id: MutOctant.h,v 1.22 2009/08/22 05:07:04 pinkenbu Exp $

/*!
  \file MutOctant.h
  \brief Describes an Octant of the muon tracker system. 
  \author Douglas Fields, Nicki Bruner
  \version $Revision: 1.22 $
  \date $Date: 2009/08/22 05:07:04 $

*/

#include <iostream>
#include <iosfwd>
#include <stdexcept>
#include <MutGeomObject.h>
#include <MutArm.h>
#include <MutStation.h>
#include <PHFrame.h>

// forward declarations ...
class MutArm;
class MutStation;
class MutHalfOctant;
class MutPlane;
class MutGap;
class MutStrip;


//! Describes an Octant of the muon tracker system. 
class MutOctant : public MutGeomObject 
{
public:

  //! Real constructor.
  MutOctant(
    const MutArm* Arm,
    const MutStation* Station,
    const MUTGEOM::OctantNumber& OctantNum);

  //! Copy constructor.
  MutOctant(const MutOctant& rhs)
  { throw std::logic_error( "MutOctant::MutOctant - use of copy constructor is forbidden" );}

  //! Destructor.
  virtual ~MutOctant();

  //! Arm identifier
  MUTGEOM::ArmNumber getArm() const 
  {return f_pArm->getArm();}

  //! Station identifier
  MUTGEOM::StationNumber getStation() const 
  {return f_pStation->getStation();}

  //! Octant identifier
  MUTGEOM::OctantNumber getOctant() const 
  {return fOctantNum;}

  //! phi coordinate of octant near strips # 0
  double getBeginPhi() const 
  {return beginPhi;}

  //! phi coordinate of octant at other side
  double getEndPhi() const 
  {return endPhi;}

  //! theta coordinate of octant at inner radius
  double getThetaInner() const 
  {return thetaInner;}

  //! theta coordinate of octant at outer radius
  double getThetaOuter() const 
  {return thetaOuter;}

  //! inner radius
  double getInnerRadius() const 
  {return innerRadius;}
  
  //! outer radius
  double getOuterRadius() const 
  {return outerRadius;}
  
  //! make MutStation::Draw a friend to access octant private members.
  friend void MutStation::Draw(const char *filename, PHBoolean DrawDisabledOnly);

private:
  
  //! parent arm
  const MutArm* f_pArm;

  //! parent station
  const MutStation* f_pStation;
  
  //! octant id
  MUTGEOM::OctantNumber fOctantNum;
  
  //! octant phi window
  double beginPhi;

  //! octant phi window
  double endPhi;
  
  //! octant theta window
  double thetaInner;

  //! octant theta window
  double thetaOuter; 
  
  //! octant radius
  double innerRadius;
  
  //! octant radius
  double outerRadius;

  // PRIVATE FUNCTIONS
  //! Get geometry from pisa 
  void getPisaGeom();

  //! Fill beingPhi, endPhi, and Theta
  void fillCylCoordParams();

public:

  //! reference frame defined by octant's survey points
  PHFrame octFrame;  

  //! Pointers to this Octants' HalfOctants
  MutHalfOctant *f_pMutHalfOctants[MUTGEOM::NumberOfHalfOctants]; 

  //! Refresh a HalfOctant of the Octant.
  void RefreshHalfOctant(const MUTGEOM::HalfOctantNumber& HalfOctantNum);

  //! Get position and orientation of object from file or database.
  void GetGlobalGeom(const std::string& file = std::string() );
  
  //! translate position of octant and its half-octants and gaps.
  void translate(const PHPoint &translation);

  //! Rotate the position and orientation of the octant and its elements.
  void rotate(float angle, char axisLabel);

  //! Expand strips and wires within the half-octant planes.
  void XYExpansion(float expansionPercentage);

  //! return a refernce frame centered on the octant.  used for alignment.
  PHFrame getBodyCenteredFrame();

  //! frame transformation
  void transformToNewFrame(PHFrame oldFrame, PHFrame newFrame);

  //! This function is called by Draw().
  void stripDraw(const MUTGEOM::GapNumber Gap, const MUTGEOM::PlaneNumber Plane, const PHBoolean DrawDisabledOnly = False);

  //! draw octant, save to a TFile
  void Draw(const char *filename = "MutOctantGeom.root");
 
};


#endif
