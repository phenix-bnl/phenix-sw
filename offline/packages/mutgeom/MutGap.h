#ifndef __MUTGAP_HH__
#define __MUTGAP_HH__

// $Id: MutGap.h,v 1.11 2010/05/07 18:24:38 hpereira Exp $

/*! 
  \file MutGap.h
  \brief Describes a Gap of the muon tracker system. 
  \author <a href="mailto:fields@unm.edu">Douglas Fields </a>, <a href="mailto:bruner@glueball.phys.unm.edu"> Nicki Bruner</a>
  \version $Revision: 1.11 $
  \date $Date: 2010/05/07 18:24:38 $
*/

#include <iosfwd>
#include <vector>
#include <stdexcept>
#include "MutGeomObject.h"
#include "MutArm.h"
#include "MutStation.h"
#include "MutOctant.h"
#include "MutHalfOctant.h"

class MutPlane;

//! Describes a Gap of the muon tracker system.  
class MutGap : public MutGeomObject
{
public:

  //! Default constructor.
  MutGap();

  //! Real constructor.
  MutGap(const MutArm* Arm,
       const MutStation* Station,
       const MutOctant* Octant,
       const MutHalfOctant* HalfOctant,
       const MUTGEOM::GapNumber& GapNum);

  //! Copy constructor
  /*! 
    copy constructor should never be called due to the handling and
    of pointers to objects which gets deleted in the destructor
  */
  MutGap(const MutGap& rhs)
  {
    throw std::logic_error( "MutGap::MutGap - use of copy constructor is forbidden" );
  }
  
  //! Destructor.
  ~MutGap();

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
  {return fGapNum;}
  //@}
  
  //! gap offset along z
  PHVector getGapOffset() const 
  {return gapOffset;}
  
  //! gap thickness along z
  double getGapThickness() const 
  {return gapThickness;}

  private:

  //! parent arm
  const MutArm* f_pArm;
  
  //! parent station
  const MutStation* f_pStation;
  
  //! parent octant
  const MutOctant* f_pOctant;
  
  //! parent half octant
  const MutHalfOctant* f_pHalfOctant;
  
  //! gap index
  MUTGEOM::GapNumber fGapNum;
  
  //! gap position with respect to station
  PHVector gapOffset;
  
  //! gap thickness
  double gapThickness;
  
  //! landau parameters
  /*! 
  these are used in the simulations to decide the charge deposited in the chamber
  by a particle. They are adjusted on a year by year basis to match real-data cluster 
  charge distributions
  */
  double f_landauOffset;

  //! landau parameters
  /*! 
  these are used in the simulations to decide the charge deposited in the chamber
  by a particle. They are adjusted on a year by year basis to match real-data cluster 
  charge distributions
  */
  double f_landauScale;
  
  public:

  //! Pointers to this Gaps' Planes
  std::vector<MutPlane*> f_pMutPlanes;  

  //! Refresh a plane of the Gap.
  void RefreshPlane(const MUTGEOM::PlaneNumber& PlaneNum);

  //! Translate position of gap and its planes.
  void translate(const PHPoint &translation);

  //! Rotate the position and orientation of the gap and its elements.
  void rotate(float angle, char axisLabel);

  //! Expand strips and wires within each plane.
  void XYExpansion(float expansionPercentage);

  //! to new frame
  void transformToNewFrame(PHFrame oldFrame, PHFrame newFrame);
  
  //!@name landau parameters
  //@{
  void setLandauOffset( const double& value )
  { f_landauOffset = value; }
  
  const double& getLandauOffset( void ) const
  { return f_landauOffset; }

  void setLandauScale( const double& value )
  { f_landauScale = value; }
  
  const double& getLandauScale( void ) const
  { return f_landauScale; }
  //@}
  
};


#endif   /* __MutGap_HH__ */
