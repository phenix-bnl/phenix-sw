// $Id: FvtxGeomObject.h,v 1.11 2013/11/05 00:04:47 jinhuang Exp $

#ifndef _FvtxGeomObject_h_
#define _FvtxGeomObject_h_

/*!
 \file FvtxGeomObject.h
 \brief Forward vertex base geometry object
 \author Hugo Pereira da costa
 \version $Revision: 1.11 $
 \date $Date: 2013/11/05 00:04:47 $
 */

#include <cmath>
#include <iostream>
#include <stdexcept>
#include <PHPoint.h>
#include <vector>
#include <utility>
#include <map>

#include <TMath.h>
#include <TVector3.h>
#include <TGeoTrd1.h>
#include <TGeoPcon.h>
#include <TGeoTube.h>
#include <TGeoShape.h>
#include <TGeoVolume.h>
#include <TGeoNode.h>
#include <TGeoMatrix.h>
#include <TGeoPhysicalNode.h>

#include "FVTXGEOM.h"
#include "TFvtxIndex.h"

//! Forward vertex base geometry object
class FvtxGeomObject
{
public:

  //! constructor
  FvtxGeomObject(const TFvtxIndex& index, const bool use_trans_matrix = false);

  //! destructor
  virtual
  ~FvtxGeomObject()
  {
    if (_matrix)
      delete _matrix;
  }

  //! index
  virtual TFvtxIndex
  index() const
  {
    return _index;
  }


  //! returns true if point (global coord) is in it
  virtual bool
  contains(const PHPoint& point) const
  {
    check_phy_node("FvtxGeomObject::contains");
    PHPoint local = global_to_local(point);
    return contains_local(local);
  }

  //! returns true if point (local coord) is in it
  virtual bool
  contains_local(const PHPoint& point) const
  {
    check_phy_node("FvtxGeomObject::contains_local");
    Double_t local[3] =
      { point.getX(), point.getY(), point.getZ() };
    return _phy_node->GetShape()->Contains(local);
  }

  //! transform global point into local coordinate
  virtual PHPoint
  global_to_local(const PHPoint& point) const;

  //! transform local point into global coordinate
  virtual PHPoint
  local_to_global(const PHPoint& point) const;

  //! get position of center of this GeomObject
  virtual PHPoint
  get_center() const
  {
    return local_to_global(PHPoint(0.0, 0.0, 0.0));
  }

  //! get position Z of center of this GeomObject
  virtual const double
  get_z() const
  {
    return get_center().getZ();
  }

  //! get position r of center of this GeomObject
  virtual const double
  get_r() const
  {
    PHPoint center = get_center();
    return sqrt(center.getX() * center.getX() + center.getY() * center.getY());
  }

  //! get phi of center of this GeomObject
  virtual const double
  get_phi() const
  {
    PHPoint center = get_center();
    return TVector3(center.getX(), center.getY(), center.getZ()).Phi();
  }

  //! set active status, for dead channel map
  virtual void
  set_active(bool b)
  {
    _is_active = b;
  }

  //! whether it is active, for dead channel map
  virtual const bool
  is_active() const
  {
    return _is_active;
  }

  //! Ratio of strips that are activated (not masked in dead map)
  virtual double
  get_active_ratio()
  {
    // should be reloaded in derivative classes
    return _is_active ? 1 : 0;
  }

private:

  //! object index
  TFvtxIndex _index;

protected:

  void
  check_phy_node(const std::string & src = "") const;

  //! TGeoPhysicalNode to this sector
  TGeoPhysicalNode *_phy_node;

  //! whether it is active, for dead channel map
  bool _is_active;

  //! @name Geometry Data
  //@{

protected:
  //!  translation matrix
  TGeoHMatrix * _matrix;

  //!  store translation matrix
  bool _is_store_matrix;

  //! simplified TGeoTrd1 to get independence of TGeo
  struct Trd1
  {
    void
    CopyFrom(const TGeoTrd1& trd)
    {
      fDx1 = trd.GetDx1();
      fDx2 = trd.GetDx2();
      fDy = trd.GetDy();
      fDz = trd.GetDz();
    }

    //! From TGeoTrd1::Contains
    bool
    Contains(const PHPoint & point) const
    {
      // test if point is inside this shape
      // check Z range
      if (TMath::Abs(point.getZ()) > fDz)
        return kFALSE;
      // then y
      if (TMath::Abs(point.getY()) > fDy)
        return kFALSE;
      // then x
      Double_t dx = 0.5
          * (fDx2 * (point.getZ() + fDz) + fDx1 * (fDz - point.getZ())) / fDz;
      if (TMath::Abs(point.getX()) > dx)
        return kFALSE;
      return kTRUE;
    }

    double fDx1; //! half length in X at lower Z surface (-dz)
    double fDx2; //!  half length in X at higher Z surface (+dz)
    double fDy; //! half length in Y
    double fDz; //!half length in Z
  };

  //@}

  //! @name Input/Output
  //@{

public:

  //! print
  virtual void
  print(std::ostream& out = std::cout) const
  {
    out << "FvtxGeomObject::print - index: " << index() << std::endl;
  }

  // these typedefs should be consistent with the counterpart in PdbFvtxAlignmentNumeric
  //! ID for geometry data
  typedef unsigned int GeoDataID_t;

  //! single record for geometry data
  typedef std::map<std::string, double> GeoDataRec_t;

  //! data set for geometry data
  typedef std::map<GeoDataID_t, GeoDataRec_t> GeoData_t;

  //! index which is unique for all FVTX components parts
  virtual GeoDataID_t
  unique_index() const=0;

  //! encode index to an unsgined long, unique for all FVTX components parts
  static GeoDataID_t
  unique_index_encoder( //
      int arm = -1, //
      int cage = -1, //
      int station = -1, //
      int sector = -1, //
      int side = -1, //
      int strip = -1 //
      )
  {
    GeoDataID_t record = 0;

    record += arm < 0 ? FVTXGEOM::NumberOfArms : arm;

    record *= 10;
    record += cage < 0 ? FVTXGEOM::NumberOfCages : cage;

    record *= 10;
    record += station < 0 ? FVTXGEOM::NumberOfStations : station;

    record *= 100;
    record += sector < 0 ? FVTXGEOM::NumberOfSectors : sector;

    record *= 10;
    record += side < 0 ? FVTXGEOM::NumberOfColumns : side;

    record *= 10000;
    record += strip < 0 ? FVTXGEOM::NumberOfStripsSt2 : strip;

    return record;
  }

  //! GeoData_t -> FvtxGeomObject
  virtual const GeoDataRec_t &
  apply_geometry_data(const GeoData_t & data);

  //! FvtxGeomObject -> GeoData_t
  virtual GeoDataRec_t &
  save_geometry_data(GeoData_t & data) const;

  //! GeoDataRec_t -> TGeoTrd1
  static void
  apply_geometry_data_to_Trd1(const std::string & name, Trd1 & trd,
      const GeoDataRec_t & rec);

  //! TGeoTrd1 -> GeoDataRec_t
  static void
  save_geometry_data_from_Trd1(const std::string & name, const Trd1 & trd,
      GeoDataRec_t & rec);

  //! GeoDataRec_t -> TGeoHMatrix
  static void
  apply_geometry_data_to_TGeoHMatrix(const std::string & name,
      TGeoHMatrix & mat, const GeoDataRec_t & rec);

  //! TGeoHMatrix -> GeoDataRec_t
  static void
  save_geometry_data_from_TGeoHMatrix(const std::string & name,
      const TGeoHMatrix & mat, GeoDataRec_t & rec);

  //! GeoDataRec_t -> PHPoint
  static void
  apply_geometry_data_to_PHPoint(const std::string & name, PHPoint & p,
      const GeoDataRec_t & rec);

  //! PHPoint -> GeoDataRec_t
  static void
  save_geometry_data_from_PHPoint(const std::string & name, const PHPoint & p,
      GeoDataRec_t & rec);

  //! set TGeoPhysicalNode with this GeomObject
  virtual void
  set_phy_node(TGeoPhysicalNode *node);

  //! access to root based physical node with this GeomObject
  virtual const TGeoPhysicalNode*
  get_phy_node() const
  {
    check_phy_node("FvtxGeomObject::get_phy_node");

    return _phy_node;
  }

protected:

  //@}

};

#endif
