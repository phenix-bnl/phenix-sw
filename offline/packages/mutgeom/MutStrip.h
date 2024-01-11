#ifndef __MUTSTRIP_HH__
#define __MUTSTRIP_HH__

// $Id: MutStrip.h,v 1.16 2008/06/24 13:02:18 hpereira Exp $

/*!
  \file MutStrip.h
  \brief Describes a Strip of the muon tracker system. 
  \author Douglas Fields, Nicki Bruner
*/

#include <iosfwd>
#include <stdexcept>
#include <PHPoint.h>

#include "MutGeomObject.h"
#include "MutArm.h"
#include "MutStation.h"
#include "MutOctant.h"
#include "MutHalfOctant.h"
#include "MutGap.h"
#include "MutPlane.h"

//! Describes a Strip of the muon tracker system. 
class MutStrip : public MutGeomObject 
{
public:

  //! constructor.
  MutStrip(
    const MutArm* Arm = 0,
    const MutStation* Station = 0,
    const MutOctant* Octant = 0,
    const MutHalfOctant* HalfOctant = 0,
    const MutGap* Gap = 0,
    const MutPlane* Plane = 0,
    const int& StripNum = 0);

  //! Destructor.
  ~MutStrip();

  //! print location
  void printLocation( void ) const;
  
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

  //! get only the Cathode plane
  MUTGEOM::CathodePlaneNumber getCathode() const
  {return getPlane()==MUTGEOM::Cathode1 ? MUTGEOM::Cathode_1 : MUTGEOM::Cathode_2;}
 
  //! Strip identifier
  int getStrip() const 
  {return fStripNum;}

  //! Number of strips in a plane
  int getNumberOfStrips() const 
  {return f_pPlane->getNumElements();}

  //! Strip global position 
  PHPoint getGlobalPositionBegin() const 
  {return fGlobalPositionBegin;}

  //! strip global position
  PHPoint getGlobalPositionEnd() const 
  {return fGlobalPositionEnd;}

  //! strip status
  PHBoolean ChannelIsDead() const 
  {return ChannelDead;}
  
  //! strip attenuated
  PHBoolean UseAttenuation() const 
  {return IsStripAttenuated;}

  //! dcm channel
  int getDCMChannel() const 
  {return DCMChannel;}
  
  //! packet id
  int getPacket_ID() const 
  {return packet_ID;}
  
  //! strip angle
  double getAngle() const;
  
  //! strip spacing (pitch)
  double getStripSpacing() const 
  {return f_pPlane->getStripSpacing();}

  //! print object
  virtual void print() const;

  //! global begin/end coordinates
  virtual void SetGlobalGeom(
      const double& b_x, const double& b_y, const double& b_z,
      const double& e_x, const double& e_y, const double& e_z)
  { 
    fGlobalPositionBegin = PHPoint( b_x, b_y, b_z );
    fGlobalPositionEnd   = PHPoint( e_x, e_y, e_z );
  }

  //! global begin/end coordinates
  void SetGlobalGeom( const PHPoint& begin, const PHPoint& end)
  {
    fGlobalPositionBegin = begin;
    fGlobalPositionEnd = end;
  }

  //! Translate position of strip.
  void translate(const PHPoint &translation);

  //! dead channel
  void setChannelToDead() 
  {ChannelDead = True;}
  
  //! active channel
  void setChannelToActive() 
  {ChannelDead = False;}
  
  //! attenuated channel
  void setAttenuationFlag(PHBoolean flag = True) 
  {IsStripAttenuated = flag;}
  
  //! DCM channel
  void setDCMChannel(int channel) 
  {DCMChannel=channel;}
  
  //! packet ID
  void setPacket_ID(int pkt_id) 
  {packet_ID=pkt_id;}

  private:

  //! Copy constructor
  /*! it is set private to abort usage at compilation time */
  MutStrip(const MutStrip& rhs) :
    f_pArm(0),
    f_pStation(0),
    f_pOctant(0),
    f_pHalfOctant(0),
    f_pGap(0),
    f_pPlane(0),
    fStripNum(0)
  { throw std::logic_error( "MutStrip::MutStrip - use of copy constructor is forbidden" ); }

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
  
  //! parent plane
  const MutPlane* f_pPlane;
  
  //! strip id
  const int fStripNum;
  
  //! strip begin position (in global frame)
  PHPoint fGlobalPositionBegin;

  //! strip end position (in global frame)
  PHPoint fGlobalPositionEnd;
  
  //! true if strip is dead
  PHBoolean ChannelDead;
  
  //! true if strip is attenuated
  PHBoolean IsStripAttenuated;
  
  //! associated DCM channel id
  int DCMChannel;
  
  //! associated DCM packet id
  int packet_ID;
  
};

#endif   /* __MutStrip_HH__ */
