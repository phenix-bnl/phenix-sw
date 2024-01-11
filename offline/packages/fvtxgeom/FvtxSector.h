// $Id: FvtxSector.h,v 1.18 2013/11/05 00:04:47 jinhuang Exp $
#ifndef _FvtxSector_h_
#define _FvtxSector_h_

/*!
 \file FvtxSector.h
 \brief Forward vertex Sector geometry
 Initialize and provide access to FVTX columns
 \author Hugo Pereira da costa
 \version $Revision: 1.18 $
 \date $Date: 2013/11/05 00:04:47 $
 */

#include <iostream>
#include <stdexcept>
#include <vector>
#include <cmath>

#include "FVTXGEOM.h"
#include "FvtxGeomObject.h"
#include "FvtxColumn.h"

//! Forward vertex Sector geometry
/*! Initialize and provide access to FVTX columns */
class FvtxSector : public FvtxGeomObject
{

public:

  //! constructor
  FvtxSector(const TFvtxIndex& index) :
      FvtxGeomObject(index, true), _overlap(0), _columns(
          FVTXGEOM::NumberOfColumns, 0)
  {
  }

  //! destructor
  virtual
  ~FvtxSector()
  {
    // delete columns
    for (std::vector<FvtxColumn*>::iterator iter = _columns.begin();
        iter != _columns.end(); iter++)
      if (*iter)
        {
          delete *iter;
          *iter = 0;
        }
  }

  //! @name accessors
  //@{

  //! frame thickness
  virtual const double
  get_delta_z(void) const
  {
    return _shape.fDy;
  }

  //! inner radius
  virtual const double
  get_inner_radius(void) const
  {
    return get_r() - _shape.fDz;
  }

  //! outer radius
  virtual const double
  get_outer_radius(void) const
  {
    return get_r() + _shape.fDz;
  }

  //! overlap
  virtual const double
  get_overlap(void) const
  {
    return _overlap;
  }

  //! phi begin , since sectors declared "MANY" in pisa, add a shift of 0.02221 to phi to avoid overlap
  virtual const double
  get_phi_begin(void) const
  {
//    Double_t local[3] =
//      { 0.0, 0.0, 0.0 };
//    Double_t master[3] =
//      { 0.0, 0.0, 0.0 };
//    local[0] = -1.0 * ((TGeoTrd1*) _phy_node->GetShape())->GetDx2();
//    if (index().sector() % 2 == 0)
//      local[0] *= -1;
//    local[2] = 1.0 * ((TGeoTrd1*) _phy_node->GetShape())->GetDz();
//    _phy_node->GetMatrix(-1 * _phy_node->GetLevel())->LocalToMaster(local,
//        &master[0]);
//
//    _phi_begin = TVector3(master[0], master[1], master[2]).Phi();

    int sign = 1;
    if (index().sector() % 2 == 0)
      sign *= -1;
    PHPoint local(-sign * _shape.fDx2, 0, _shape.fDz);
    PHPoint global = local_to_global(local);

    return atan2(global.getY(), global.getX());

  }

  //! phi end
  virtual const double
  get_phi_end(void) const
  {
//    Double_t local[3] =
//      { 0.0, 0.0, 0.0 };
//    Double_t master[3] =
//      { 0.0, 0.0, 0.0 };
//    local[0] = 1.0 * ((TGeoTrd1*) _phy_node->GetShape())->GetDx2();
//    if (index().sector() % 2 == 0)
//      local[0] *= -1;
//    local[2] = 1.0 * ((TGeoTrd1*) _phy_node->GetShape())->GetDz();
//    _phy_node->GetMatrix(-1 * _phy_node->GetLevel())->LocalToMaster(local,
//        &master[0]);
//    _phi_end = TVector3(master[0], master[1], master[2]).Phi();

    int sign = 1;
    if (index().sector() % 2 == 0)
      sign *= -1;

    PHPoint local(+sign * _shape.fDx2, 0, _shape.fDz);
    PHPoint global = local_to_global(local);

    return atan2(global.getY(), global.getX());
  }

  //! phi window
#ifndef __CINT__
  FVTXGEOM::Window
  get_phi_window(void) const
  {
    return FVTXGEOM::Window(get_phi_begin(), get_phi_end());
  }
#endif

  //! column from index
  virtual FvtxColumn*
  get_column(unsigned int index) const
  {
    if (_columns.size() <= index)
      throw std::runtime_error("FvtxSector::get_column - invalid index");
    if (!_columns[index])
      throw std::runtime_error("FvtxSector::get_column - not initialized");
    return _columns[index];
  }

  //! overlap width
  virtual void
  set_overlap(const double& overlap)
  {
    _overlap = overlap;
  }

  //! set column
  void
  set_column(unsigned int index, FvtxColumn* column)
  {
    if (_columns.size() <= index)
      throw std::runtime_error("FvtxRadius::set_column - invalid index");
    if (_columns[index])
      throw std::runtime_error("FvtxRadius::set_column - already initialized");
    _columns[index] = column;
  }

  //@}
  //! @name dumper
  //@{
  //! print
  virtual void
  print(std::ostream& out = std::cout) const
  {
    out << "FvtxSector::print - index: " << index() << std::endl;
    out << "FvtxSector::print - z: " << get_z() << " delta_z: " << get_delta_z()
        << std::endl;
    out << "FvtxSector::print - inner_radius: " << get_inner_radius()
        << " outer_radius: " << get_outer_radius() << std::endl;
    out << "FvtxSector::print - phi_begin: " << get_phi_begin() << " phi_end: "
        << get_phi_end() << std::endl;
    out << "FvtxSector::print - overlap: " << get_overlap() << std::endl;
  }
  //@}

  //! returns true if point is in active area
  //! this function use internal geometry data, therefore do not depend on loading stages of TGeo
  virtual bool
  contains(const PHPoint& point) const;

  //! set active status, for dead channel map
  virtual void
  set_active(bool b)
  {
    FvtxGeomObject::set_active(b);

    for (std::vector<FvtxColumn*>::iterator iter = _columns.begin();
        iter < _columns.end(); iter++)
      {
        (*iter)->set_active(b);
      }
  }

  //! Ratio of strips that are activated (not masked in dead map)
  virtual double
  get_active_ratio()
  {

    if (!_is_active)
      return 0;

    int cnt = 0;
    double sum_activity = 0;

    for (std::vector<FvtxColumn*>::iterator iter = _columns.begin();
        iter < _columns.end(); iter++)
      {
        sum_activity += (*iter)->get_active_ratio();
        cnt++;
      }

    return sum_activity / cnt;
  }

  virtual GeoDataID_t
  unique_index() const
  {
    return unique_index_encoder(index().arm(), index().cage(),
        index().station(), index().sector());
  }

  //! set TGeoPhysicalNode with this GeomObject
  virtual void
  set_phy_node(TGeoPhysicalNode *node)
  {
    FvtxGeomObject::set_phy_node(node);

    _shape.CopyFrom(*((TGeoTrd1*) (_phy_node->GetShape())));

    return;
  }

  //! GeoData_t -> FvtxGeomObject
  virtual const GeoDataRec_t &
  apply_geometry_data(const GeoData_t & data)
  {
    const GeoDataRec_t & rec = FvtxGeomObject::apply_geometry_data(data);

    GeoDataRec_t::const_iterator i;

    apply_geometry_data_to_Trd1("_shape", _shape, rec);

    for (std::vector<FvtxColumn*>::iterator iter = _columns.begin();
        iter < _columns.end(); iter++)
      {
        (*iter)->apply_geometry_data(data);
      }

    return rec;
  }

  //! FvtxGeomObject -> GeoData_t
  virtual GeoDataRec_t &
  save_geometry_data(GeoData_t & data) const
  {
    GeoDataRec_t & rec = FvtxGeomObject::save_geometry_data(data);

    save_geometry_data_from_Trd1("_shape", _shape, rec);

    for (std::vector<FvtxColumn*>::const_iterator iter = _columns.begin();
        iter < _columns.end(); iter++)
      {
        (*iter)->save_geometry_data(data);
      }

    return rec;
  }

private:

  //! overlap width
  double _overlap;

  //! shape parameters
  Trd1 _shape;

  //! pointer to stations
  std::vector<FvtxColumn*> _columns;

};

#endif
