#ifndef __MUTHALFOCTANT_HH__
#define __MUTHALFOCTANT_HH__

// $Id: MutHalfOctant.h,v 1.13 2008/07/29 16:26:12 hpereira Exp $

/*! 
  \file MutHalfOctant.h
  \brief Describes a Half octant of the muon tracker system. 
  \author <a href="mailto:fields@unm.edu">Douglas Fields </a>, <a href="mailto:bruner@glueball.phys.unm.edu"> Nicki Bruner</a>
  \version $Revision: 1.13 $
  \date $Date: 2008/07/29 16:26:12 $
*/

#include <iosfwd>
#include <vector>
#include <stdexcept>
#include <PHFrame.h>

#include "MutGeomObject.h"
#include "MutArm.h"
#include "MutStation.h"
#include "MutOctant.h"

class MutGap;

//! Describes a HalfOctant of the muon tracker system. 
class MutHalfOctant : public MutGeomObject
{
public:

  //! Real constructor.
  MutHalfOctant(
    const MutArm* Arm,
    const MutStation* Station,
    const MutOctant* Octant,
    const MUTGEOM::HalfOctantNumber& HalfOctantNum);
  
  //! Destructor.
  ~MutHalfOctant();

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
  {return fHalfOctantNum;}

  //! return number of gaps for this half octant 
  int getNumberOfGaps() const 
  {return f_pMutGaps.size();}

private:

  //! Copy constructor.
  /*! 
    copy constructor should never be called due to the handling and
    of pointers to objects which gets deleted in the destructor. 
    It is therefore private.
  */
  MutHalfOctant(const MutHalfOctant& rhs)
  { throw std::logic_error( "MutHalfOctant::MutHalfOctant - use of copy constructor is forbidden" );}

  //! parent arm
  const MutArm* f_pArm;
  
  //! parent station
  const MutStation* f_pStation;
  
  //! parent station
  const MutOctant* f_pOctant;
  
  //! half octant id
  MUTGEOM::HalfOctantNumber fHalfOctantNum;
  
public:
    
  //! Pointers to this HalfOctants' Gaps
  std::vector<MutGap *> f_pMutGaps;  

  //! Refresh a gap of the halfoctant.
  void RefreshGap(const MUTGEOM::GapNumber& GapNum);

  //! translate position of halfoctant and its gaps.
  void translate(const PHPoint &translation);

  //! Rotate the position and orientation of the half-octant and its elements.
  void rotate(float angle, char axisLabel);

  //! Expand strips and wires within each plane.
  void XYExpansion(float expansionPercentage);

  /*! 
    return a reference frame that is centered on the half-octant and
    has its y-axis parallel to the straight strips.  This frame is used 
    to make alignment corrections.
  */
  PHFrame getBodyCenteredFrame();

  //! frame transformation
  void transformToNewFrame(PHFrame oldFrame, PHFrame newFrame);
};

#endif   /* __MutHalfOctant_HH__ */
