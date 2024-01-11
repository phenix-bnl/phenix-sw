// $Id: FvtxStation.h,v 1.13 2013/11/05 00:04:47 jinhuang Exp $
#ifndef _FvtxStation_h_
#define _FvtxStation_h_

/*!
 \file FvtxStation.h
 \brief Forward vertex Station geometry
 Initialize and provide access to FVTX sectors
 \author Hugo Pereira da costa
 \version $Revision: 1.13 $
 \date $Date: 2013/11/05 00:04:47 $
 */

#include <iostream>
#include <stdexcept>
#include <vector>
#include <list>

#include "FVTXGEOM.h"
#include "FvtxGeomObject.h"
#include "FvtxSector.h"

//! Forward vertex Station geometry
/*! Initialize and provide access to FVTX sectors */
class FvtxStation : public FvtxGeomObject
{

public:

  //! constructor
  FvtxStation(const TFvtxIndex& index) :
      FvtxGeomObject(index, true), _inner_radius(-9999), _outer_radius(-9999), _phi_begin(
          0), _phi_end(0), _phi0(0), _sectors(FVTXGEOM::NumberOfSectors, 0)
  {
  }

  //! destructor
  virtual
  ~FvtxStation()
  {
    // delete sectors
    for (std::vector<FvtxSector*>::iterator iter = _sectors.begin();
        iter != _sectors.end(); iter++)
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

  //! phi0 (azimuth angle of station sector 0)
  virtual const double
  get_phi0(void) const
  {
    return _sectors[0]->get_phi_begin();
  }

  //! phi begin
  double
  get_phi_begin(void) const
  {
    return _phi_begin;
  }

  //! phi end
  double
  get_phi_end(void) const
  {
    return _phi_end;
  }

  //! sector from index
  virtual FvtxSector*
  get_sector(const unsigned int& index) const
  {
    if (_sectors.size() <= index)
      throw std::runtime_error("FvtxStation::get_sector - invalid index");
    if (!_sectors[index])
      throw std::runtime_error("FvtxStation::get_sector - not initialized");
    return _sectors[index];
  }

  //! phi0 (azimuth angle of station sector 0)
  virtual void
  set_phi0(const double& phi0)
  {
    _phi0 = phi0;
  }

  //! set sector
  virtual void
  set_sector(unsigned int index, FvtxSector* sector)
  {
    if (_sectors.size() <= index)
      throw std::runtime_error("FvtxStation::set_sector - invalid index");
    if (_sectors[index])
      throw std::runtime_error("FvtxStation::set_sector - already initialized");
    _sectors[index] = sector;
  }

  //@}

  //! @name utility functions
  //@{

  //! retrieve sectors possibly hit by a given point
  /*! due to overlap either 1 or two sectors are possibly returned */
  virtual std::list<FvtxSector*>
  find_sectors(const PHPoint& point) const;

  //! @name dumper
  //@{

  //! print
  virtual void
  print(std::ostream& out = std::cout) const
  {
    out << "FvtxStation::print - index: " << index() << std::endl;
    out << "FvtxStation::print - z: " << get_z() << " inner_radius: "
        << get_inner_radius() << " outer_radius: " << get_outer_radius()
        << " phi0: " << get_phi0() << std::endl;
  }

  //@}

  //! set active status, for dead channel map
  virtual void
  set_active(bool b)
  {
    FvtxGeomObject::set_active(b);

    for (std::vector<FvtxSector*>::iterator iter = _sectors.begin();
        iter < _sectors.end(); iter++)
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

    for (std::vector<FvtxSector*>::iterator iter = _sectors.begin();
        iter < _sectors.end(); iter++)
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
        index().station());
  }

  //! set TGeoPhysicalNode with this GeomObject
  virtual void
  set_phy_node(TGeoPhysicalNode *node)
  {
    FvtxGeomObject::set_phy_node(node);

//    {
//      Double_t global[3] =
//        { 0.0, 0.0, 0.0 };
//      Double_t local[3] =
//        { 0.0, 0.0, 0.0 };
//      _matrix->LocalToMaster(local, &global[0]);
//      std::cout << "FvtxStation::set_phy_node " << unique_index()
//          << " - get z =" << get_z() << ", " << _is_store_matrix << " - "
//          << _matrix->GetTranslation()[2] << " - " << get_center().getZ() << " - "
//          << global[2] << std::endl;
//      _matrix->Print();
//    }

    _inner_radius = ((TGeoPcon*) _phy_node->GetShape())->GetRmin(0);

    _outer_radius = ((TGeoPcon*) _phy_node->GetShape())->GetRmax(0);

      {

        Double_t local[3] =
          { 0.0, 0.0, 0.0 };
        Double_t master[3] =
          { 0.0, 0.0, 0.0 };

        const double phi_local =
            ((TGeoPcon*) (_phy_node->GetShape()))->GetPhi1() / 180.
                * TMath::Pi();

        local[0] = _outer_radius * cos(phi_local);
        local[0] = _outer_radius * sin(phi_local);

        _phy_node->GetMatrix(-1 * _phy_node->GetLevel())->LocalToMaster(local,
            &master[0]);
        _phi_begin = TVector3(master[0], master[1], master[2]).Phi();

//        std::cout << "FvtxStation::set_phy_node " << unique_index()
//            << " phi_local = " << phi_local << " _phi_begin = " << _phi_begin
//            << std::endl;

      }

      {

        Double_t local[3] =
          { 0.0, 0.0, 0.0 };
        Double_t master[3] =
          { 0.0, 0.0, 0.0 };

        const double phi_local =
            (((TGeoPcon*) (_phy_node->GetShape()))->GetPhi1()
                + ((TGeoPcon*) (_phy_node->GetShape()))->GetDphi()) / 180.
                * TMath::Pi();

        local[0] = _outer_radius * cos(phi_local);
        local[0] = _outer_radius * sin(phi_local);

        _phy_node->GetMatrix(-1 * _phy_node->GetLevel())->LocalToMaster(local,
            &master[0]);
        _phi_end = TVector3(master[0], master[1], master[2]).Phi();

//        std::cout << "FvtxStation::set_phy_node " << unique_index()
//            << " phi_local = " << phi_local << " _phi_end = " << _phi_end
//            << std::endl;
      }

    return;
  }

  //! GeoData_t -> FvtxGeomObject
  virtual const GeoDataRec_t &
  apply_geometry_data(const GeoData_t & data)
  {
    const GeoDataRec_t & rec = FvtxGeomObject::apply_geometry_data(data);

//    {
//      Double_t global[3] =
//        { 0.0, 0.0, 0.0 };
//      Double_t local[3] =
//        { 0.0, 0.0, 0.0 };
//      _matrix->LocalToMaster(local, &global[0]);
//      std::cout << "FvtxStation::apply_geometry_data " << unique_index()
//          << " - get z =" << get_z() << ", " << _is_store_matrix << " - "
//          << _matrix->GetTranslation()[2] << " - " << get_center().getZ() << " - "
//          << global[2] << std::endl;
//      _matrix->Print();
//    }

    GeoDataRec_t::const_iterator i;

    i = rec.find("_inner_radius");
    if (i == rec.end())
      {
        throw std::runtime_error(
            "FvtxStation::apply_geometry_data - Missing _inner_radius");
      }
    _inner_radius = (*i).second;

    i = rec.find("_outer_radius");
    if (i == rec.end())
      {
        throw std::runtime_error(
            "FvtxStation::apply_geometry_data - Missing _outer_radius");
      }
    _outer_radius = (*i).second;

    i = rec.find("_phi_begin");
    if (i == rec.end())
      {
        throw std::runtime_error(
            "FvtxStation::apply_geometry_data - Missing _phi_begin");
      }
    _phi_begin = (*i).second;

    i = rec.find("_phi_end");
    if (i == rec.end())
      {
        throw std::runtime_error(
            "FvtxStation::apply_geometry_data - Missing _phi_end");
      }
    _phi_end = (*i).second;

    for (std::vector<FvtxSector*>::iterator iter = _sectors.begin();
        iter < _sectors.end(); iter++)
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
    rec["_phi_begin"] = _phi_begin;
    rec["_phi_end"] = _phi_end;

    for (std::vector<FvtxSector*>::const_iterator iter = _sectors.begin();
        iter < _sectors.end(); iter++)
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

  //! phi begin
  double _phi_begin;

  //! phi end
  double _phi_end;

  //! azimut angle of the first sector in station
  double _phi0;

  //! pointer to stations
  std::vector<FvtxSector*> _sectors;

};

#endif
