// $Id: FvtxStrip.h,v 1.16 2015/09/14 15:28:52 snowball Exp $
#ifndef _FvtxStrip_h_
#define _FvtxStrip_h_

/*!
 \file FvtxStrip.h
 \brief Forward vertex Strip geometry
 Initialize and provide access to FVTX planes
 \author Hugo Pereira da costa
 \version $Revision: 1.16 $
 \date $Date: 2015/09/14 15:28:52 $
 */

#include <iostream>
#include <PHPoint.h>
#include <PHVector.h>

#include "FvtxGeomObject.h"

//! Forward vertex Strip geometry
/*! Initialize and provide access to FVTX planes */
class FvtxStrip : public FvtxGeomObject
{

public:

  //! constructor
  FvtxStrip(const TFvtxIndex& index, const unsigned int& strip_index) :
      FvtxGeomObject(index), _strip_index(strip_index), _width(-9999), _thickness(-9999), 
	_position_begin(-9999, -9999, -9999), _position_end(-9999, -9999, -9999),
	_position_begin_local_column(-9999, -9999, -9999), _position_end_local_column(-9999, -9999, -9999),
	_position_begin_local_strip(-9999, -9999, -9999), _position_end_local_strip(-9999, -9999, -9999),
	_dcm_channel(0), _packet_id(0), _fem_id(0), _chip_id(0)
  {
  }

  //! destructor
  virtual
  ~FvtxStrip()
  {
  }

  //! @name accessors
  //@{

  //! strip_index
  virtual const unsigned int&
  get_strip_index(void) const
  {
    return _strip_index;
  }

  //! width
  virtual const double
  get_width() const
  {
//    return 2*((TGeoTrd1*)(_phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()))->GetDz();
    return _width;
  }

  //! thickness
  virtual const double
  get_thickness() const
  {
//    return 2*((TGeoTrd1*)(_phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()))->GetDy();
    return _thickness;
  }

  //! get position of center
  virtual PHPoint
  get_center() const
  {
    return (get_position_begin() + get_position_end()) * 0.5;
  }

  //! begin position
  virtual const PHPoint
  get_position_begin(void) const
  {
    return is_swap_begin_end()?_position_end:_position_begin;
  }

  //! end position
  virtual const PHPoint
  get_position_end(void) const
  {
    return is_swap_begin_end()?_position_begin:_position_end;
  }

  //! get position of center
  virtual PHPoint
  get_center_local_column() const
  {
    return (get_position_begin_local_column() + get_position_end_local_column()) * 0.5;
  }

  //! get position of center
  virtual PHPoint
  get_center_local_strip() const
  {
    return (get_position_begin_local_strip() + get_position_end_local_strip()) * 0.5;
  }

  //! begin position
  virtual const PHPoint
  get_position_begin_local_column(void) const
  {
    return is_swap_begin_end()?_position_end_local_column:_position_begin_local_column;
  }

  //! end position
  virtual const PHPoint
  get_position_end_local_column(void) const
  {
    return is_swap_begin_end()?_position_begin_local_column:_position_end_local_column;
  }

  //! begin position
  virtual const PHPoint
  get_position_begin_local_strip(void) const
  {
    return is_swap_begin_end()?_position_end_local_strip:_position_begin_local_strip;
  }

  //! end position
  virtual const PHPoint
  get_position_end_local_strip(void) const
  {
    return is_swap_begin_end()?_position_begin_local_strip:_position_end_local_strip;
  }

//  //! width
//  virtual void set_width( const double& width )
//  { _width = width; }
//
//  //! begin position
//  virtual void set_position_begin( const PHPoint& point )
//  { _position_begin = point; }
//
//  //! end position
//  virtual void set_position_end( const PHPoint& point )
//  { _position_end = point; }

  virtual bool
  contains(const PHPoint& point) const
  {
    check_phy_node("FvtxStrip::contains");
    PHPoint local = global_to_local(point);
    return contains_local(local);
  }

  //! returns true if point (local coord) is in it
  virtual bool
  contains_local(const PHPoint& point) const
  {
    check_phy_node("FvtxStrip::contains_local");
    Double_t local[3] =
      { point.getX(), point.getY(), point.getZ() };
    return _phy_node->GetVolume()->GetNode(_strip_index)->GetVolume()->GetShape()->Contains(
        local);
  }

  //! transform global point into local coordinate
  //  here local is Column coordinate since phy_node is column's
  virtual PHPoint
  global_to_local(const PHPoint& point) const
  {
    check_phy_node("FvtxStrip::global_to_local");
    Double_t column_local[3] =
      { 0.0, 0.0, 0.0 };
    Double_t global[3] =
      { point.getX(), point.getY(), point.getZ() };
    _phy_node->GetMatrix(-1 * _phy_node->GetLevel())->MasterToLocal(global,
        &column_local[0]);

    Double_t local[3] =
      { 0.0, 0.0, 0.0 };
    _phy_node->GetVolume()->GetNode(_strip_index)->MasterToLocal(column_local,
        &local[0]);
    return PHPoint(local[0], local[1], local[2]);
  }


  //! transform column to strip
  //  here local is Column coordinate since phy_node is column's
  virtual PHPoint
  column_to_strip(const PHPoint& point) const
  {
    check_phy_node("FvtxStrip::global_to_local");
    Double_t column_local[3] = { point.getX(), point.getY(), point.getZ() };
    Double_t local[3] =  { 0.0, 0.0, 0.0 };
    _phy_node->GetVolume()->GetNode(_strip_index)->MasterToLocal(column_local, &local[0]);
    return PHPoint(local[0], local[1], local[2]);
  }

  virtual PHPoint
  strip_to_column(const PHPoint& point) const
  {
    check_phy_node("FvtxStrip::local_to_global");
    Double_t column_local[3] = { 0.0, 0.0, 0.0 };
    Double_t local[3] =  { point.getX(), point.getY(), point.getZ() };
    _phy_node->GetVolume()->GetNode(_strip_index)->LocalToMaster(local, &column_local[0]);

    return PHPoint(column_local[0], column_local[1], column_local[2]);

  }


  //! transform local point into global coordinate
  virtual PHPoint
  local_to_global(const PHPoint& point) const
  {
    check_phy_node("FvtxStrip::local_to_global");
    Double_t column_local[3] =
      { 0.0, 0.0, 0.0 };
    Double_t local[3] =
      { point.getX(), point.getY(), point.getZ() };
    _phy_node->GetVolume()->GetNode(_strip_index)->LocalToMaster(local,
        &column_local[0]);

    Double_t global[3] =
      { 0.0, 0.0, 0.0 };
    _phy_node->GetMatrix(-1 * _phy_node->GetLevel())->LocalToMaster(
        column_local, &global[0]);

    return PHPoint(global[0], global[1], global[2]);
  }

  //@}

  //!@name I/O
  //@{

  virtual GeoDataID_t
  unique_index() const
  {
    return unique_index_encoder(index().arm(), index().cage(),
        index().station(), index().sector(), index().column(),
        get_strip_index());
  }

  //! unique id for a particular strip type
  virtual GeoDataID_t
  local_strip_index() const
  {
    return unique_index_encoder(-1, -1, index().station() >= 1 ? 1 : 0, -1,
        index().column(), get_strip_index());
  }

  //! set TGeoPhysicalNode with this GeomObject
  virtual void
  set_phy_node(TGeoPhysicalNode *node);

  //! GeoData_t -> FvtxGeomObject
  virtual const GeoDataRec_t &
  apply_geometry_data(const GeoData_t & data);

  //! FvtxGeomObject -> GeoData_t
  virtual GeoDataRec_t &
  save_geometry_data(GeoData_t & data) const;

  //! FvtxGeomObject -> GeoData_t
  virtual GeoDataRec_t &
  save_geometry_data_and_check(GeoData_t & data) const;

  //@}

  //!@name utilities
  //@{

  //! path length
  /*!
   retrieve "relative" (between 0 and 1) path length in strip
   for a particle traversing the detector between
   point_in and point_out
   */
  double
  get_path_length(const PHPoint& point_in, const PHPoint& point_out);

  //! strip angle
  double
  get_angle() const;

  //! DCM channel
  void
  set_dcm_channel(int channel)
  {
    _dcm_channel = channel;
  }

  //! packet ID
  void
  set_packet_id(int pkt_id)
  {
    _packet_id = pkt_id;
  }

  //! fem ID
  void
  set_fem_id(int fem_id)
  {
    _fem_id = fem_id;
  }

  //! chip ID
  void
  set_chip_id(int chip_id)
  {
    _chip_id = chip_id;
  }

  //! dcm channel
  int
  get_dcm_channel() const
  {
    return _dcm_channel;
  }

  //! packet id
  int
  get_packet_id() const
  {
    return _packet_id;
  }

  //! fem id
  int
  get_fem_id() const
  {
    return _fem_id;
  }

  //! chip id
  int
  get_chip_id() const
  {
    return _chip_id;
  }

  //! get sign of w_absolute relative to the r direction
  int
  get_w_sign() const;

  //! print
  virtual void
  print(std::ostream& out = std::cout) const
  {
    out << "FvtxStrip::print - index: " << index() << std::endl;
    out << "FvtxStrip::print - strip_index: " << get_strip_index() << std::endl;
    out << "FvtxStrip::print - width: " << get_width() << " cm" << std::endl;
    out << "FvtxStrip::print - position_begin: (" << get_position_begin().getX()
        << "," << get_position_begin().getY() << ","
        << get_position_begin().getZ() << ") cm" << std::endl;
    out << "FvtxStrip::print - position_end  : (" << get_position_end().getX()
        << "," << get_position_end().getY() << "," << get_position_end().getZ()
        << ") cm" << std::endl;
  }

  //@}

private:

  bool is_swap_begin_end() const
  {
    return (index().arm() != (index().sector() % 2));
  }

  //! strip index
  unsigned int _strip_index;

  //! strip width
  double _width;

  //! thickness
  double _thickness;

  //! strip begin position
  PHPoint _position_begin;

  //! strip end position
  PHPoint _position_end;

  //! column begin position
  PHPoint _position_begin_local_column;

  //! column end position
  PHPoint _position_end_local_column;

  //! strip begin position
  PHPoint _position_begin_local_strip;

  //! strip end position
  PHPoint _position_end_local_strip;

  //! associated DCM channel id
  int _dcm_channel;

  //! associated DCM packet id
  int _packet_id;

  //! associated fem id
  int _fem_id;

  //! associated chip id
  int _chip_id;
};

#endif
