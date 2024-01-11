// $Id: FvtxGeomObject.cxx,v 1.1 2013/11/05 00:04:47 jinhuang Exp $                                                                                             

/*!
 * \file FvtxGeomObject.cxx
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/11/05 00:04:47 $
 */

#include "FvtxGeomObject.h"

#include <iostream>
#include <stdexcept>
#include <vector>
#include <sstream>
#include <limits>
#include <string.h>
#include <cassert>
#include <boost/lexical_cast.hpp>
#include "TMatrixD.h"

#include "FVTXGEOM.h"

using namespace std;

//! constructor
FvtxGeomObject::FvtxGeomObject(const TFvtxIndex& index,
    const bool use_trans_matrix) :
    _index(index), _phy_node(NULL), _is_active(true), _matrix(NULL), _is_store_matrix(
        use_trans_matrix)
{
  if (_is_store_matrix)
    _matrix = new TGeoHMatrix();
}

void
FvtxGeomObject::check_phy_node(const std::string & src) const
{
  if (!_phy_node)
    {
      std::cout << "FvtxGeomObject::check_phy_node from " << src
          << " - Fatal Error - Physics node is not initialized!"
          << " Please load the TGeo type of geometry, instead of the numerical geometry table."
          << " TGeo type of geometry can be retrieved from database (FvtxGeom::load_pdb_geometry(type = BANK_ID_TYPE_TGEO))"
          << " or from ROOT files (FvtxGeom::load_root_geometry())."
          << std::endl;
      print();
      throw std::runtime_error(
          "FvtxGeomObject::check_phy_node - Fatal Error - TGeo Physics node is not initialized!");
    }
}

const FvtxGeomObject::GeoDataRec_t &
FvtxGeomObject::apply_geometry_data(const FvtxGeomObject::GeoData_t & data)
{
  GeoData_t::const_iterator iter = data.find(unique_index());

  if (iter == data.end())
    {
      cout
          << "FvtxGeomObject::apply_geometry_data - ERROR - missing geometry data record for index = "
          << index() << " and unique index = " << unique_index() << endl;
    }

  const GeoDataRec_t & rec = (*iter).second;

  GeoDataRec_t::const_iterator i;

  if (_is_store_matrix)
    {
      if (!_matrix)
        throw std::runtime_error(
            "FvtxGeomObject::apply_geometry_data - Missing _matrix");

      apply_geometry_data_to_TGeoHMatrix("_matrix", *_matrix, rec);
    }

  return rec;
}

FvtxGeomObject::GeoDataRec_t &
FvtxGeomObject::save_geometry_data(FvtxGeomObject::GeoData_t & data) const
{
  GeoData_t::iterator iter = data.find(unique_index());

  if (iter != data.end())
    {
      cout
          << "FvtxGeomObject::save_geometry_data - WARNING - over write geometry data record forindex = "
          << index() << " and unique index = " << unique_index()
          << ". It original contains " << data.size()
          << " records. Matching record contains " << (*iter).second.size()
          << " data. Overwriting ... " << endl;
    }

  GeoDataRec_t & rec = data[unique_index()];

  if (_is_store_matrix)
    {
      if (!_matrix)
        throw std::runtime_error(
            "FvtxGeomObject::save_geometry_data - Missing _matrix");

      save_geometry_data_from_TGeoHMatrix("_matrix", *_matrix, rec);
    }

  return rec;
}

//! GeoDataRec_t -> TGeoTrd1
void
FvtxGeomObject::apply_geometry_data_to_Trd1(const std::string & name,
    FvtxGeomObject::Trd1 & trd, const FvtxGeomObject::GeoDataRec_t & rec)
{

  string s;
  GeoDataRec_t::const_iterator it;

  s = name + ".fDx1";
  it = rec.find(s);
  if (it == rec.end())
    {
      throw std::runtime_error(
          "FvtxGeomObject::apply_geometry_data_to_Trd1 - Missing " + s);
    }
  trd.fDx1 = ((*it).second);

  s = name + ".fDx2";
  it = rec.find(s);
  if (it == rec.end())
    {
      throw std::runtime_error(
          "FvtxGeomObject::apply_geometry_data_to_Trd1 - Missing " + s);
    }
  trd.fDx2 = ((*it).second);

  s = name + ".fDy";
  it = rec.find(s);
  if (it == rec.end())
    {
      throw std::runtime_error(
          "FvtxGeomObject::apply_geometry_data_to_Trd1 - Missing " + s);
    }
  trd.fDy = ((*it).second);

  s = name + ".fDz";
  it = rec.find(s);
  if (it == rec.end())
    {
      throw std::runtime_error(
          "FvtxGeomObject::apply_geometry_data_to_Trd1 - Missing " + s);
    }
  trd.fDz = ((*it).second);

}

//! TGeoTrd1 -> GeoDataRec_t
void
FvtxGeomObject::save_geometry_data_from_Trd1(const std::string & name,
    const FvtxGeomObject::Trd1 & trd, FvtxGeomObject::GeoDataRec_t & rec)
{

  rec[name + ".fDx1"] = trd.fDx1;
  rec[name + ".fDx2"] = trd.fDx2;
  rec[name + ".fDy"] = trd.fDy;
  rec[name + ".fDz"] = trd.fDz;

}

//! GeoDataRec_t -> TGeoHMatrix
void
FvtxGeomObject::apply_geometry_data_to_TGeoHMatrix(const std::string & name,
    TGeoHMatrix & mat, const FvtxGeomObject::GeoDataRec_t & rec)
{

  GeoDataRec_t::const_iterator it;


  mat.Clear();

  string name_base = name + ".GetTranslation.";
  for (int i = 0; i < 3; i++)
    {
      stringstream s;
      s << (name_base);

      s << i;

      it = rec.find(s.str());
      if (it == rec.end())
        {
          continue;
        }

      mat.SetBit(TGeoHMatrix::kGeoTranslation);
      (mat.GetTranslation())[i] = ((*it).second);

    }

  //  Double_t              fRotationMatrix[9]; // rotation matrix
  name_base = name + ".GetRotationMatrix.";
  for (int i = 0; i < 9; i++)
    {
      stringstream s;
      s << (name_base);

      s << i;
      it = rec.find(s.str());
      if (it == rec.end())
        {
          continue;
        }
      mat.SetBit(TGeoHMatrix::kGeoRotation);
      (mat.GetRotationMatrix())[i] = ((*it).second);
    }

  //  Double_t              fScale[3];          // scale component
  name_base = name + ".GetScale.";
  for (int i = 0; i < 3; i++)
    {
      stringstream s;
      s << (name_base);

      s << i;
      it = rec.find(s.str());
      if (it == rec.end())
        {
          continue;
        }
      mat.SetBit(TGeoHMatrix::kGeoScale);
      (mat.GetScale())[i] = ((*it).second);
    }

}

//! TGeoHMatrix -> GeoDataRec_t
void
FvtxGeomObject::save_geometry_data_from_TGeoHMatrix(const std::string & name,
    const TGeoHMatrix & mat, FvtxGeomObject::GeoDataRec_t & rec)
{

  //! this stream assumes a translation and rotation matrix only
  if(mat.TestBit(TGeoHMatrix::kGeoReflection))
    {
      throw runtime_error("FvtxGeomObject::save_geometry_data_from_TGeoHMatrix - do not support reflection");
    }

  string name_base;

  //  Double_t              fTranslation[3];    // translation component
  if(mat.IsTranslation())
    {
      name_base = name + ".GetTranslation.";
      for (int i = 0; i < 3; i++)
        {
          stringstream s;
          s << (name_base);

          s << i;

          rec[s.str()] = (mat.GetTranslation())[i];
        }
    }

  //  Double_t              fRotationMatrix[9]; // rotation matrix
  if(mat.IsRotation())
    {
      name_base = name + ".GetRotationMatrix.";
      for (int i = 0; i < 9; i++)
        {
          stringstream s;
          s << (name_base);

          s << i;

          rec[s.str()] = (mat.GetRotationMatrix())[i];
        }
    }

  //  Double_t              fScale[3];          // scale component

  if(mat.IsScale())
    {
      name_base = name + ".GetScale.";
      for (int i = 0; i < 3; i++)
        {
          stringstream s;
          s << (name_base);

          s << i;

          rec[s.str()] = (mat.GetScale())[i];
        }
    }


}

//! GeoDataRec_t -> PHPoint
void
FvtxGeomObject::apply_geometry_data_to_PHPoint(const std::string & name,
    PHPoint & p, const FvtxGeomObject::GeoDataRec_t & rec)
{

  string s;
  GeoDataRec_t::const_iterator it;

  s = name + ".getX";
  it = rec.find(s);
  if (it == rec.end())
    {
      throw std::runtime_error(
          "FvtxGeomObject::apply_geometry_data_to_PHPoint - Missing " + s);
    }
  p.setX((*it).second);

  s = name + ".getY";
  it = rec.find(s);
  if (it == rec.end())
    {
      throw std::runtime_error(
          "FvtxGeomObject::apply_geometry_data_to_PHPoint - Missing " + s);
    }
  p.setY((*it).second);

  s = name + ".getZ";
  it = rec.find(s);
  if (it == rec.end())
    {
      throw std::runtime_error(
          "FvtxGeomObject::apply_geometry_data_to_PHPoint - Missing " + s);
    }
  p.setZ((*it).second);

}

//! PHPoint -> GeoDataRec_t
void
FvtxGeomObject::save_geometry_data_from_PHPoint(const std::string & name,
    const PHPoint & p, FvtxGeomObject::GeoDataRec_t & rec)
{

  rec[name + ".getX"] = p.getX();
  rec[name + ".getY"] = p.getY();
  rec[name + ".getZ"] = p.getZ();

}

//! transform global point into local coordinate
PHPoint
FvtxGeomObject::global_to_local(const PHPoint& point) const
{

  Double_t local[3] =
    { 0.0, 0.0, 0.0 };
  Double_t global[3] =
    { point.getX(), point.getY(), point.getZ() };

  if (_is_store_matrix)
    {
      if (!_matrix)
        throw std::runtime_error(
            "FvtxGeomObject::global_to_local - Missing _matrix");

      _matrix->MasterToLocal(global, &local[0]);
    }
  else
    {
      throw std::runtime_error(
          "FvtxGeomObject::global_to_local - this object is not configured for global_to_local");
    }

  return PHPoint(local[0], local[1], local[2]);
}

//! transform local point into global coordinate
PHPoint
FvtxGeomObject::local_to_global(const PHPoint& point) const
{

  Double_t global[3] =
    { 0.0, 0.0, 0.0 };
  Double_t local[3] =
    { point.getX(), point.getY(), point.getZ() };

  if (_is_store_matrix)
    {
      if (!_matrix)
        throw std::runtime_error(
            "FvtxGeomObject::local_to_global - Missing _matrix");

      _matrix->LocalToMaster(local, &global[0]);
    }
  else
    {
      throw std::runtime_error(
          "FvtxGeomObject::local_to_global - this object is not configured for local_to_global");
    }

  return PHPoint(global[0], global[1], global[2]);
}

//! set TGeoPhysicalNode with this GeomObject
void
FvtxGeomObject::set_phy_node(TGeoPhysicalNode *node)
{
  _phy_node = node;

  if (_is_store_matrix)
    {
      if (!_matrix)
        throw std::runtime_error(
            "FvtxGeomObject::set_phy_node - Missing _matrix");

      _matrix->CopyFrom(_phy_node->GetMatrix(-1 * _phy_node->GetLevel()));

      //! Fix the precision of the rotation matrix
      if (_matrix->IsRotation())
        {
          Double_t *rot = _matrix->GetRotationMatrix();

          TMatrixD m(3,3,rot);
          TMatrixD m_inv(3,3,rot);
          m_inv.InvertFast();
          m_inv.Transpose(m_inv);

          m = (m + m_inv)*.5;

          m.GetMatrix2Array(rot);
        }

    }
}

