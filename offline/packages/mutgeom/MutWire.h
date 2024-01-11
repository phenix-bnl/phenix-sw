#ifndef __MUTWIRE_HH__
#define __MUTWIRE_HH__
// $Id: MutWire.h,v 1.11 2008/06/24 13:02:18 hpereira Exp $

/*!
  \file MutWire.h
  \brief Describes a Wire of the muon tracker system. 
  \author Douglas Fields, Nicki Bruner
*/

#include <stdexcept>

#include "PHPoint.h"
#include "MutGeomObject.h"
#include "MutArm.h"
#include "MutStation.h"
#include "MutOctant.h"
#include "MutHalfOctant.h"
#include "MutGap.h"
#include "MutPlane.h"

//!  Describes a Wire of the muon tracker system. 
class MutWire : public MutGeomObject 
{
  public:

  //! Default constructor.
  MutWire();

  //! Real constructor.
  MutWire(
    const MutArm* Arm,
    const MutStation* Station,
    const MutOctant* Octant,
    const MutHalfOctant* HalfOctant,
    const MutGap* Gap,
    const MutPlane* Plane,
    const int& WireNum);

  //! Destructor.
  ~MutWire();

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
  {return f_pPlane->getPlane();}

  //! Wire identifier
  int getWire() const 
  {return fWireNum;}

  //! global position 
  PHPoint getGlobalPositionBegin() const 
  {return fGlobalPositionBegin;}
  
  //! global position    
  PHPoint getGlobalPositionEnd() const 
  {return fGlobalPositionEnd;}

  //! wire angle
  double getAngle();

  //! dead wires
  PHBoolean ChannelIsDead() const 
    {return ChannelDead;}

  //! print
  void print() const;

  //! Set position and orientation of object in global coords.
  void SetGlobalGeom( 
      const double& a,const double& b,const double& c,
      const double& d,const double& e,const double& f)
  {
    fGlobalPositionBegin = PHPoint( a, b, c );
    fGlobalPositionEnd = PHPoint( d, e, f ); 
  }
  
  //! global wire geometry
  void SetGlobalGeom( const PHPoint& begin, const PHPoint& end)
  {
    fGlobalPositionBegin = begin;
    fGlobalPositionEnd = end;
  }

  //! Translate position of wire.
  void translate(const PHPoint &translation);
  
  //! wire is dead
  void setChannelToDead() 
  {ChannelDead = True;}
  
  //! wire is active
  void setChannelToActive() 
  {ChannelDead = False;}

  private:

  //! Copy constructor.
  MutWire(const MutWire& rhs):
    f_pArm(0),
    f_pStation(0),
    f_pOctant(0),
    f_pHalfOctant(0),
    f_pGap(0),
    f_pPlane(0),
    fWireNum(0)
  { throw std::logic_error( "MutWire::MutWire - use of copy constructor is forbidden" ); }

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
  
  //! parent (wire) plane
  const MutPlane* f_pPlane;

  //! wire id
  const int fWireNum;
  
  //! wire begin point (in global coordinates)
  PHPoint fGlobalPositionBegin;

  //! wire end point (in global coordinates)
  PHPoint fGlobalPositionEnd;
  
  //! true if wire is dead
  PHBoolean ChannelDead;
  
};


#endif   /* __MutWire_HH__ */
