// $Id: FvtxColumn.h,v 1.24 2013/11/05 00:04:46 jinhuang Exp $
#ifndef _FvtxColumn_h_
#define _FvtxColumn_h_

/*!
 \file FvtxColumn.h
 \brief Forward vertex Column geometry
 Initialize and provide access to FVTX strips
 \author Hugo Pereira da costa
 \version $Revision: 1.24 $
 \date $Date: 2013/11/05 00:04:46 $
 */

#include <iostream>
#include <stdexcept>
#include <vector>
#include <list>
#include <sstream>

#include "FVTXGEOM.h"
#include "FvtxGeomObject.h"
#include "FvtxStrip.h"

//! Forward vertex Column geometry
/*! Initialize and provide access to FVTX strips */
class FvtxColumn : public FvtxGeomObject
{

public:

  //! constructor
  FvtxColumn(const TFvtxIndex& index, unsigned int n_strips) :
      FvtxGeomObject(index, true), _overlap(0), _strip_tilt(0), _strips(
          n_strips, 0)
  {
  }

  //! destructor
  virtual
  ~FvtxColumn()
  {
    // delete strips
    for (std::vector<FvtxStrip*>::iterator iter = _strips.begin();
        iter != _strips.end(); iter++)
      if (*iter)
        {
          delete *iter;
          *iter = 0;
        }
  }

  //! @name accessors
  //@{

  //! detector thickness [cm]
  const double
  get_delta_z(void) const
  {
//      return ((TGeoTrd1*)_phy_node->GetShape())->GetDy();
    return _shape.fDy;
  }

  //! inner radius [cm]
  const double
  get_inner_radius(void) const
  {
//      return get_r()-((TGeoTrd1*)_phy_node->GetShape())->GetDz();
    return get_r() - _shape.fDz;
  }

  //! outer radius [cm]
  const double
  get_outer_radius(void) const
  {
//      return get_r()+((TGeoTrd1*)_phy_node->GetShape())->GetDz();
    return get_r() + _shape.fDz;
  }

  //! overlap [cm]
  const double
  get_overlap(void) const
  {
    return _overlap;
  }

  //! phi begin [rad]
  const double
  get_phi_begin(void) const
  {
//        Double_t local[3]  = {0.0, 0.0, 0.0};
//        Double_t master[3] = {0.0, 0.0, 0.0};
//
//        // point on top angle of the trapezoid
//        local[0] = -1.0*((TGeoTrd1*)_phy_node->GetShape())->GetDx2();
//
//        if( index().sector()%2 == 0 ) local[0] *= -1;
//
//        local[2] =  1.0*((TGeoTrd1*)_phy_node->GetShape())->GetDz();
//        _phy_node->GetMatrix(-1*_phy_node->GetLevel())->LocalToMaster(local, &master[0]);
//
//        return TVector3(master[0], master[1], master[2]).Phi();

    int sign = 1;
    if (index().sector() % 2 == 0)
      sign *= -1;

    PHPoint local(-sign * _shape.fDx2, 0, _shape.fDz);
    PHPoint global = local_to_global(local);

    return atan2(global.getY(), global.getX());
  }

  //! phi end [rad]
  const double
  get_phi_end(void) const
  {
//        Double_t local[3]  = {0.0, 0.0, 0.0};
//        Double_t master[3] = {0.0, 0.0, 0.0};
//
//        // point on top angle of the trapezoid
//        local[0] = 1.0*((TGeoTrd1*)_phy_node->GetShape())->GetDx2();
//        if( index().sector()%2 == 0 ) local[0] *= -1;
//        local[2] =  1.0*((TGeoTrd1*)_phy_node->GetShape())->GetDz();
//        _phy_node->GetMatrix(-1*_phy_node->GetLevel())->LocalToMaster(local, &master[0]);
//        return TVector3(master[0], master[1], master[2]).Phi();

    int sign = 1;
    if (index().sector() % 2 == 0)
      sign *= -1;

    PHPoint local(+sign * _shape.fDx2, 0, _shape.fDz);
    PHPoint global = local_to_global(local);

    return atan2(global.getY(), global.getX());
  }

#ifndef __CINT__
  //! phi window [rad]
  FVTXGEOM::Window
  get_phi_window(void) const
  {
    return FVTXGEOM::Window(get_phi_begin(), get_phi_end());
  }
#endif

  //! strip tilt angle [rad]
  const double
  get_strip_tilt(void) const
  {
    return _strip_tilt;
  }

  //! number of strips
  virtual unsigned int
  get_n_strips(void) const
  {
    return _strips.size();
  }

  //! strip from index
  FvtxStrip*
  get_strip(const unsigned int& index) const
  {
    if (_strips.size() <= index)
      {
        std::ostringstream s;
        int max_index = _strips.size() - 1;
        s << "FvtxColumn::get_strip - invalid index " << index
            << ", max index = " << max_index;
        throw std::out_of_range(s.str());
      }
    if (!_strips[index])
      throw std::runtime_error("FvtxColumn::get_strip - not initialized");
    return _strips[index];
  }

//    //! average position along the beam [cm]
//    void set_z( const double& z )
//    { _z = z; }

//    //! inner radius [cm]
//    void set_inner_radius( const double& inner_radius )
//    { _inner_radius = inner_radius; }
//
//    //! outer radius [cm]
//    void set_outer_radius( const double& outer_radius )
//    { _outer_radius = outer_radius; }

  //! overlap width [cm]
  void
  set_overlap(const double& overlap)
  {
    _overlap = overlap;
  }

  /*! 0 means strips are perpendicular to the column mediane */
  void
  set_strip_tilt(const double& tilt)
  {
    _strip_tilt = tilt;
  }

  //! set strip
  void
  set_strip(unsigned int index, FvtxStrip* strip)
  {
    if (_strips.size() <= index)
      throw std::runtime_error("FvtxColumn::set_strip - invalid index");
    if (_strips[index])
      throw std::runtime_error("FvtxColumn::set_strip - already initialized");
    _strips[index] = strip;
  }

  //@}

  //! @name utility functions
  //@{

  //! returns true if point is in active area
  //! this function use internal geometry data, therefore do not depend on loading stages of TGeo
  virtual bool
  contains(const PHPoint& point) const;

  //! returns strip pointed to by point
  /*!
   returns 0 if no strip is found .
   The method assumes that contains( point ) is true
   and that the z matches the FvtxColumn z
   */
  virtual FvtxStrip*
  find_strip(const PHPoint& point) const;

  //! returns strips hit by the track segment
  /*!
   returns an empty list if no strip is found .
   The method assumes that contains( point ) is true
   and that the z matches the FvtxColumn z
   */
  virtual std::list<FvtxStrip*>
  find_strips(const PHPoint& point_in, const PHPoint& point_out) const;

  //@}

  //! @name dumper
  //@{

  //! print
  virtual void
  print(std::ostream& out = std::cout) const
  {
    out << "FvtxColumn::print - index: " << index() << std::endl;
    out << "FvtxColumn::print - z: " << get_z() << " delta_z: " << get_delta_z()
        << " (cm)" << std::endl;
    out << "FvtxColumn::print - inner_radius: " << get_inner_radius()
        << " outer_radius: " << get_outer_radius() << " (cm)" << std::endl;
    out << "FvtxColumn::print - phi_begin: " << get_phi_begin() << " phi_end: "
        << get_phi_end() << " (rad)" << std::endl;
    out << "FvtxColumn::print - overlap: " << _overlap << std::endl;
    out << "FvtxColumn::print - strip_tilt: " << _strip_tilt << " (rad)"
        << std::endl;
    out << "FvtxColumn::print - n_strips: " << get_n_strips() << std::endl;
  }
  //@}

  //! set active status, for dead channel map
  virtual void
  set_active(bool b)
  {
    FvtxGeomObject::set_active(b);

    for (std::vector<FvtxStrip*>::iterator iter = _strips.begin();
        iter < _strips.end(); iter++)
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

    for (std::vector<FvtxStrip*>::iterator iter = _strips.begin();
        iter < _strips.end(); iter++)
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
        index().station(), index().sector(), index().column());
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

    for (std::vector<FvtxStrip*>::iterator iter = _strips.begin();
        iter < _strips.end(); iter++)
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

    for (std::vector<FvtxStrip*>::const_iterator iter = _strips.begin();
        iter < _strips.end(); iter++)
      {
        (*iter)->save_geometry_data(data);
      }

    return rec;
  }

private:

////    //! average position along the beam
//    double _z;

//    //! inner radius
//    double _inner_radius;
//
//    //! outer radius
//    double _outer_radius;

  //! overlap width
  double _overlap;

  //! strip tilt angle
  double _strip_tilt;

  //! shape parameters
  Trd1 _shape;

  //! pointer to stations
  std::vector<FvtxStrip*> _strips;

};

#endif
