// $Id: FvtxCage.h,v 1.7 2014/01/26 15:40:11 bbannier Exp $
#ifndef _FvtxCage_h_
#define _FvtxCage_h_

/*!
 \file FvtxCage.h
 \brief Forward vertex Cage geometry
 \Initialize and provide access to FVTX statoinss
 \author Zhengyun You
 \version $Revision: 1.7 $
 \date $Date: 2014/01/26 15:40:11 $
 */

#include <iostream>
#include <stdexcept>
#include <vector>
#include <list>

#include "FVTXGEOM.h"
#include "FvtxGeomObject.h"
#include "FvtxStation.h"

//! Forward vertex cage geometry
/*! Initialize and provide access to FVTX stations */
class FvtxCage : public FvtxGeomObject
{

public:
 //! constructor
 FvtxCage(const TFvtxIndex& index)
     : FvtxGeomObject(index, true),
       _inner_radius(-9999),
       _outer_radius(-9999),
       _phi0(0),
       _stations(FVTXGEOM::NumberOfStations, 0) {}

  //! destructor
  virtual
  ~FvtxCage()
  {
    // delete stations
    for (std::vector<FvtxStation*>::iterator iter = _stations.begin();
        iter != _stations.end(); iter++)
      if (*iter)
        {
          delete *iter;
          *iter = 0;
        }
  }

  //! @name accessors
  //@{

  //! inner radius
  virtual const double
  get_inner_radius(void) const
  {
    return _inner_radius;
  }

  //! outer radius
  virtual const double
  get_outer_radius(void) const
  {
    return _outer_radius;
  }

  //! phi0 (azimuth angle of station station 0)
  virtual const double
  get_phi0(void) const
  {
    return _stations[0]->get_phi_begin();
  }

  //! station from index
  virtual FvtxStation*
  get_station(const unsigned int& index) const
  {
    if (_stations.size() <= index)
      throw std::runtime_error("FvtxCage::get_station - invalid index");
    if (!_stations[index])
      throw std::runtime_error("FvtxCage::get_station - not initialized");
    return _stations[index];
  }

  //! phi0 (azimuth angle of station station 0)
  virtual void
  set_phi0(const double& phi0)
  {
    _phi0 = phi0;
  }

  //! set station
  virtual void
  set_station(unsigned int index, FvtxStation* station)
  {
    if (_stations.size() <= index)
      throw std::runtime_error("FvtxCage::set_station - invalid index");
    if (_stations[index])
      throw std::runtime_error("FvtxCage::set_station - already initialized");
    _stations[index] = station;
  }

  //@}

  //! @name dumper
  //@{

  // Feb 2012, Jin Huang <jhuang@bnl.gov>
  // Redesign the mappings
  //! DCM channels
//    void getDCMChannels();

  //! print
  virtual void
  print(std::ostream& out = std::cout) const
  {
    out << "FvtxCage::print - index: " << index() << std::endl;
    out << "FvtxCage::print - z: " << get_z() << " inner_radius: "
        << get_inner_radius() << " outer_radius: " << get_outer_radius()
        << " phi0: " << get_phi0() << std::endl;
  }

  //@}

  //! set active status, for dead channel map
  virtual void
  set_active(bool b)
  {
    FvtxGeomObject::set_active(b);

    for (std::vector<FvtxStation*>::iterator iter = _stations.begin();
        iter < _stations.end(); iter++)
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

    for (std::vector<FvtxStation*>::iterator iter = _stations.begin();
        iter < _stations.end(); iter++)
      {
        sum_activity += (*iter)->get_active_ratio();
        cnt++;
      }

    return sum_activity / cnt;
  }

  virtual GeoDataID_t
  unique_index() const
  {
    return unique_index_encoder(index().arm(), index().cage());
  }

  //! set TGeoPhysicalNode with this GeomObject
  virtual void
  set_phy_node(TGeoPhysicalNode *node)
  {
    FvtxGeomObject::set_phy_node(node);

    const int nz = ((TGeoPcon*) _phy_node->GetShape())->GetNz();

    _inner_radius = ((TGeoPcon*) _phy_node->GetShape())->GetRmin(nz-1);

    _outer_radius = ((TGeoPcon*) _phy_node->GetShape())->GetRmax(nz-1);

    return;
  }

  //! GeoData_t -> FvtxGeomObject
  virtual const GeoDataRec_t &
  apply_geometry_data(const GeoData_t & data)
  {
    const GeoDataRec_t & rec = FvtxGeomObject::apply_geometry_data(data);


    GeoDataRec_t::const_iterator i;

    i = rec.find("_inner_radius");
    if (i == rec.end())
      {
        throw std::runtime_error(
            "FvtxCage::apply_geometry_data - Missing _inner_radius");
      }
    _inner_radius = (*i).second;

    i = rec.find("_outer_radius");
    if (i == rec.end())
      {
        throw std::runtime_error(
            "FvtxCage::apply_geometry_data - Missing _outer_radius");
      }
    _outer_radius = (*i).second;

//    std::cout << "FvtxCage::apply_geometry_data "<<unique_index()<<" - _inner_radius = "<<_inner_radius << std::endl;
//    std::cout << "FvtxCage::apply_geometry_data "<<unique_index()<<" - _outer_radius = "<<_outer_radius << std::endl;
//    std::cout << "FvtxCage::apply_geometry_data "<<unique_index()<<" - i = "<<i->first <<" -> "<<i->second<< std::endl;

    for (std::vector<FvtxStation*>::iterator iter = _stations.begin();
        iter < _stations.end(); iter++)
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

    rec["_inner_radius"] = _inner_radius;
    rec["_outer_radius"] = _outer_radius;

//    std::cout << "FvtxCage::save_geometry_data "<<unique_index()<<" - _inner_radius = "<<_inner_radius <<" -> "<<rec["_inner_radius"] << std::endl;
//    std::cout << "FvtxCage::save_geometry_data "<<unique_index()<<" - _outer_radius = "<<_outer_radius <<" -> "<<rec["_outer_radius"] << std::endl;

    for (std::vector<FvtxStation*>::const_iterator iter = _stations.begin();
        iter < _stations.end(); iter++)
      {
        (*iter)->save_geometry_data(data);
      }

    return rec;
  }

private:

  //! inner radius
  double _inner_radius;

  //! outer radius
  double _outer_radius;

  //! azimut angle of the first station in station
  double _phi0;

  //! pointer to stations
  std::vector<FvtxStation*> _stations;

};

#endif
