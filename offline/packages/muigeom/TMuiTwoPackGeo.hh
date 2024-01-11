#ifndef __TMUITWOPACKGEO_HH__
#define __TMUITWOPACKGEO_HH__
// $Id: TMuiTwoPackGeo.hh,v 1.6 2009/05/12 02:13:36 shoji Exp $ 
/*!
  \file    TMuiTwoPackGeo.hh
  \brief   single Iarocci tube in the muon identifier system.
  \author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>    
  \version $Revision: 1.6 $
  \date    $Date: 2009/05/12 02:13:36 $
*/



#include <iosfwd>
#include "PHVector.h"

class TMuiPanelGeo;    
class PdbMuiTubeGeo;
class PdbCoordinate;
class PHPoint;
class PHVect;

//! single Iarocci tube in the muon identifier system.
/*!
Describes a single Iarocci tube in the muon identifier system.
@author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>     
*/
class TMuiTubeGeo
{
  
  public:

  //! Constructor.
  TMuiTubeGeo(const bool& front,
        const float& xmin, const float& xmax,
        const float& ymin, const float& ymax,
        const float& zmin, const float& zmax);

  //! Constructor (dummy edge positions).
  TMuiTubeGeo(const bool& front);

  /// Destructor.
  ~TMuiTubeGeo()
  {}

  //! Is this the front tube of the two-pack?
  bool IsFront() const
  {return _front;}
 
  //! Return position of  low-X edge (in the panel coordinate system)
  float XMin() const
  {return _x_min;}
  
  //! Return position of high-X edge (in the panel coordinate system)
  float XMax() const
  {return _x_max;}
  
  /// Return position of  low-Y edge (in the panel coordinate system)
  float YMin() const
  {return _y_min;}
  
  /// Return position of high-Y edge (in the panel coordinate system)
  float YMax() const
  {return _y_max;}
  
  /// Return position of  low-Z edge (in the panel coordinate system)
  float ZMin() const
  {return _z_min;}
  
  /// Return position of high-Z edge (in the panel coordinate system)
  float ZMax() const
  {return _z_max;}
  
  /// Return position of center (in the panel coordinate system)
  void CenterPos(float& x, float& y, float& z) const;

  /// Is the point within the boundaries of this tube?
  bool IsInTube(const float& x, const float& y, const float& z) const;
  
  /// Is the point within the boundaries of this tube?
  bool IsInTube(const PHPoint& p, const PHVector& v) const;
  
  bool IsInTube(const PHPoint& p, const PHVector& v, float& dz) const;
  
  //! Set the edge positions  (in the panel coordinate system)
  void SetEdges(
    const float& xmin, const float& xmax,
    const float& ymin, const float& ymax,
    const float& zmin, const float& zmax);

  private:
  
  //! true for front tube of two pack, false for back tube
  bool  _front;  
  
  //! Position of  low-X edge in the panel coordinate system
  float _x_min;   
  
  //! Position of high-X edge in the panel coordinate system
  float _x_max;   
  
  //! Position of  low-Y edge in the panel coordinate system
  float _y_min;  
  
  //! Position of high-Y edge in the panel coordinate system
  float _y_max;   
  
  //! Position of  low-Z edge in the panel coordinate system
  float _z_min;   

  //! Position of high-Z edge in the panel coordinate system
  float _z_max;  
   
};


/*! \brief
 Describes a single two-pack in the muon identifier system.
 The two-pack is the unit of readout in the MuID; it consists of
 two Iarocci tubes.
 @author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>    
*/
class TMuiTwoPackGeo
{
  public:

  //! Constructor (dummy tube positions and sizes).
  TMuiTwoPackGeo(const short& arm, const short& plane,
     const short& panel, const EOrient_t& orient,
     const short& tube,
     TMuiPanelGeo* const pointer);

  //! Destructor.
  ~TMuiTwoPackGeo();

  //! Return the channel identifier ("software" address) of this two-pack.
  TMuiChannelId Channel() const
  {return _id;}
  
  //! Return the object for the front tube of this two-pack.
  const TMuiTubeGeo& FrontTube() const
  { return _front_tube; }

  //! Return the object for the back  tube of this two-pack.
  const TMuiTubeGeo& BackTube() const
  { return _back_tube; }

  //! Return pointer to the panel that contains this two-pack.
  TMuiPanelGeo* Panel() const
  { return _panel; }

  /*! \brief
  Pointer to the adjacent two-pack on the -X or -Y side of this one.
  (returns 0 if no neighbor in this panel).
  */
  TMuiTwoPackGeo* LeftNeighbor() const
  { return _left_neighbor; }

  /*! \brief
  Pointer to the adjacent two-pack on the +X or +Y side of this one.
  (returns 0 if no neighbor in this panel).
  */
  TMuiTwoPackGeo* RightNeighbor() const
  { return _right_neighbor; }
  
  //! Position of this two-pack (in the panel coordinate system).
  void CenterPos(float& x, float& y, float& z) const
  {
    x = _center_pos.getX();
    y = _center_pos.getY();
    z = _center_pos.getZ();
    return;
  }

  //! Uncertainty in the position of this two-pack (panel coordinate system).
  void CenterSigma(float& sx, float& sy, float& sz) const
  {
    sx = _center_sigma.getX();
    sy = _center_sigma.getY();
    sz = _center_sigma.getZ();
    return;
  }

  //! Is the point within the boundaries of this two-pack?
  bool IsInTwoPack(const float& x, const float& y, const float& z) const;

  //! Is the point within the boundaries of this two-pack?
  bool IsInTwoPack(const PHPoint& p, const PHVector& v) const;

  //! Get the two-pack hit status.
  bool HitStatus() const;

  /*! \brief
   * Set the edge positions of a tube  (in the panel coordinate system)
   * @param layer  layer number of the tube in the two-pack
   * @param x1     low-x edge pos for horiz tubes, center x pos for vert
   * @param x2     high-x edge pos for horiz tubes, center x pos for vert
   * @param y1     low-y edge pos for vert tubes, center y pos for horiz
   * @param y2     high-y edge pos for vert tubes, center y pos for horiz
   * @param length length of tube ("x" size for horiz, "y" for vert)
   * @param width  width of tube ("y" size for horiz, "x" for vert)
   * @param thick  thickness ("z" size) of tube 
   */
  void SetTubeEdges(
    const short& layer,
    const float& x1, const float& x2,
    const float& y1, const float& y2,
    const float& length,
    const float& width,
    const float& thick);

  //! Set the edge positions of a tube  (in the panel coordinate system).
  void SetTubeEdges(
    const PdbMuiTubeGeo& geo,
    const float& length,
    const float& width,
    const float& thick);
  
  /*! \brief
  Set pointer to the adjacent two-pack on -X or -Y side of this one.
  (0 if no neighbor in this panel).
  */
  void SetLeftNeighbor(TMuiTwoPackGeo* ptr)
  { if (ptr) _left_neighbor = ptr; }

  /*! \brief
  Set pointer to the adjacent two-pack on +X or +Y side of this one.
  (0 if no neighbor in this panel).
  */
  void SetRightNeighbor(TMuiTwoPackGeo* ptr)
  { if (ptr) _right_neighbor = ptr; }

  //! Tell the two-pack whether or not it registered a signal in this event.
  void SetHitStatus(const bool hit);
  
  //! length
  const float& GetLength( void ) const 
  { return _length; }

  //! width
  const float& GetWidth( void ) const 
  { return _width; }

  //! thickness
  const float& GetThickness( void ) const 
  { return _thickness; }

  private:
    
  //! Channel identifier of this two-pack
  TMuiChannelId _id;          

  //! Pointer to the left neighbor two-pack
  TMuiTwoPackGeo* _left_neighbor;   
  
  //! Pointer to the right neighbor two-pack
  TMuiTwoPackGeo* _right_neighbor;
  
  //! Object for front tube of two-pack  
  TMuiTubeGeo _front_tube;    
  
  //! Object for back tube of two-pack
  TMuiTubeGeo _back_tube;     
  
  //! Pointer to panel containing two-pack
  TMuiPanelGeo* _panel;  
  
  //! Position of center (panel coordinate system)  
  PHVector _center_pos;     
  
  //! Uncert. in center position (panel coords)
  PHVector _center_sigma;   
  
  //! length 
  float _length;
    
  //! width
  float _width;
  
  //! thickness
  float _thickness;
  
  //! If true, this two-pack was hit
  bool _hit_status;           

  //! Print TMuiTwoPackGeo information to a stream.
  friend std::ostream& operator << (std::ostream& s, const TMuiTwoPackGeo& g);

};


#endif /* __TMUITWOPACKGEO_HH__ */
