#ifndef __TMUIPANELGEO_HH__
#define __TMUIPANELGEO_HH__
// $Id: TMuiPanelGeo.hh,v 1.9 2006/12/21 13:41:44 hpereira Exp $

/*!
  \file    TMuiPanelGeo.hh
  \brief   single panel of (both horizontal and vertical) Iarocci tubes in the muon identifier system. 
  \author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>  
  \author  A.M. Glenn (aglenn@bnl.gov)
  \version $Revision: 1.9 $
  \date    $Date: 2006/12/21 13:41:44 $
*/

#include <iosfwd>
#include <vector>
#include "PHMatrix.h"
#include "PHVector.h"
#include "PHPanel.h"

class PdbMuiPanelGeo;
class PdbCoordinate;

//! single panel of (both horizontal and vertical) Iarocci tubes in the muon identifier system. 
/*!
 single panel of (both horizontal and vertical) Iarocci tubes in the muon identifier system. 
 
@author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>  
*/

class TMuiPanelGeo 
{
  
  public:

  //! Real constructor (construct two-packs externally)
  TMuiPanelGeo(
    const short& arm,
    const short& gap,
    const short& panel,
    const short& num_twopacks_horiz,
    const short& num_twopacks_vert,
    const float& xSize,
    const float& ySize,
    const float& zSize,
    const float& xTarget1Global,
    const float& yTarget1Global,
    const float& zTarget1Global,
    const float& xTarget2Global,
    const float& yTarget2Global,
    const float& zTarget2Global,
    const float& dxTarget1ToFiducial,
    const float& dyTarget1ToFiducial,
    const float& dxFiducialToCenter,
    const float& dyFiducialToCenter,
    const float& dzCenterToCloseHTubes,
    const float& dzCenterToFarHTubes,
    const float& dzCenterToCloseVTubes,
    const float& dzCenterToFarVTubes);
  
  //! Real constructor (construct two-packs externally)
  TMuiPanelGeo(
    const PdbMuiPanelGeo& geo,
    const short& num_twopacks_horiz,
    const short& num_twopacks_vert,
    const float& xSize,
    const float& ySize,
    const float& zSize);
  
  //! Destructor.
  ~TMuiPanelGeo();

  //! Arm identifier (0 or 1)
  short getArm() const
  {return _id.Arm();}
  
  //! Gap identifier (0-4)
  short getPlane() const
  {return _id.Plane();}
  
  //! Panel identifier (0-5)
  short getPanel() const
  {return _id.Panel();}
  
  //! Return channel identifier ("software" address) for this panel.
  TMuiChannelId Channel() const
  {return _id;}
  
  //! How many (horizontal or vertical) two-packs in this panel?
  short getTwoPackCount(const EOrient_t& orient) const
  { return (orient == kHORIZ) ? fTwoPacksH:fTwoPacksV; }

  //! Point to a two-pack within this panel.
  TMuiTwoPackGeo* TwoPackPointer(const EOrient_t& orient, const short& tube) const
  {
    if (orient == kHORIZ) return ((unsigned int)tube >= _twopacks_h.size()) ? 0:_twopacks_h[tube];
    else return ((unsigned int)tube >= _twopacks_v.size()) ? 0:_twopacks_v[tube];
  }

  //! Panel center position (global system)
  void CenterPos(float& x, float& y, float& z) const
  {
    x = _center.getX();
    y = _center.getY();
    z = _center.getZ();
    return;
  }
  
  //! Dimensions of the panel.
  void Size(float& dx, float& dy, float& dz) const
  {
    dx = _x_size;
    dy = _y_size;
    dz = _z_size;
    return;
  }
  
  //! Z displacement of "near" horizontal tubes from the panel center.
  float DzCenterToNearHTubes() const;
  
  //! Z displacement of "near" vertical   tubes from the panel center.
  float DzCenterToNearVTubes() const;
  
  //! Z displacement of "far" horizontal tubes from the panel center.
  float DzCenterToFarHTubes() const;
  
  //! Z displacement of "far" vertical   tubes from the panel center.
  float DzCenterToFarVTubes() const;
  
  //! get the panel-to-global rotation matrix
  PHMatrix GetRotationMatrix( void ) const
  { return _rotation; }
  
  //! get the panel-to-global translation vector
  PHVector GetTranslationVector( void ) const
  { return _translation; }
  
  //! reset the panel-to-global rotation matrix and corresponding frame/center)
  void Rotate( const PHMatrix& rotation );
  
  //! reset the panel-to-global translation vector (and corresponding frame/center)
  void Translate( const PHVector& translation );
          
  //! Get the rotation angles of the panel in the global coordinate system.
  void RotationMatrix(
    float& thetaX, float& phiX,
    float& thetaY, float& phiY,
    float& thetaZ, float& phiZ) const;

  //! Given a position and an orientation, estimate the two-pack number.
  short GuessTwoPack(
    const int& orient,
    const float& qx, const float& qy, const float& qz) const;

  //! Find the intersection of the given line with the panel.
  PHPoint ProjectToPanel(const PHPoint& p, const PHVector& v) const;

  //! Transform from PHENIX global coords to panel coords (no translation).
  PHVector RotateToPanel(const PHVector& GVect) const;
  
  //! Transform from panel coords to PHENIX global coords (no translation).
  PHVector RotateToGlobal(const PHVector& PVect) const;

  //! Transform from PHENIX global coordinates to panel coordinates.
  PHPoint TransformToPanel(const PHPoint& GVect) const;
  
  //! Transform from panel coordinates to PHENIX global coordinates.
  PHPoint TransformToGlobal(const PHPoint& PVect) const;

  //! Is the point (given in panel coords) within the panel boundaries?
  bool IsInPanel(const float& x, const float& y, const float& z) const
  {
    return (   
      ( (x > -0.5 * _x_size) && (x < 0.5 * _x_size) )
      && ( (y > -0.5 * _y_size) && (y < 0.5 * _y_size) ) );
  }

  //! Set the channel ID of this panel.
  void SetID(const short& arm, const short& plane, const short& panel);

  //! Add a two-pack to the panel.
  TMuiTwoPackGeo* AddTwoPack(const short& orient, const short& twopack);

  //! Tell the panel whether or not it registered a signal in this event.
  void SetHitStatus(const bool hit);

  private:

  //! panel id
  TMuiChannelId _id;
  
  // Break H and V orientations out separately because the number of
  // H tubes does not equal the number of V tubes.
  //  (slight preference -- may change)
  
  //! pointers to horizontal tubes
  std::vector<TMuiTwoPackGeo *> _twopacks_h;
  
  //! Pointers to vertical tubes
  std::vector<TMuiTwoPackGeo *> _twopacks_v;  

  //! Position of panel center (global coords)
  PHPoint _center;       
  
  //! rotation from panel to global frame
  PHMatrix _rotation;          
  
  //! translation from panel to global frame
  PHVector _translation; 
  
  //! panel geometry
  PHPanel  fGeomPanel;
  
  //! z distrince from center to nearest horizontal tube
  float f_dzCenterToNearHTubes; 

  //! z distrince from center to farest horizontal tube
  float f_dzCenterToFarHTubes;   
  
  //! z distrince from center to nearest vertical tube
  float f_dzCenterToNearVTubes; 
  
  //! z distrince from center to farest vertical tube
  float f_dzCenterToFarVTubes;
  
  //! panel dimension along x
  float _x_size;         

  //! panel dimension along y
  float _y_size;
  
  //! panel dimension along z
  float _z_size;
  
  //! number of horizontal twopacks
  short fTwoPacksH;     

  //! number of vertical twopacks
  short fTwoPacksV;     

  //! streamer
  friend std::ostream& operator << (std::ostream& s, const TMuiPanelGeo& g);
  
};

#endif   /* __TMUIPANELGEO_HH__ */
