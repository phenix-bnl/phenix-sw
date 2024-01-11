// $Id: FvtxArm.h,v 1.10 2015/03/26 15:50:05 pinkenbu Exp $

#ifndef _FvtxArm_h_
#define _FvtxArm_h_

/*!
 \file FvtxArm.h
 \brief Forward vertex Arm geometry
 Initialize and provide access to FVTX cages
 \author Hugo Pereira da costa
 \version $Revision: 1.10 $
 \date $Date: 2015/03/26 15:50:05 $
 */

#include "FvtxGeomObject.h"
#include "FvtxCage.h"

#include <iostream>
#include <stdexcept>
#include <vector>


//! fvtx Arm geometry.
/*! 
 Initialize and provide access to FVTX cages
 */
class FvtxArm : public FvtxGeomObject
{

public:

  //! constructor
  FvtxArm(const TFvtxIndex& index) :
      FvtxGeomObject(index, true), _cages(FVTXGEOM::NumberOfCages, 0)
  {
  }

  //! destructor
  virtual
  ~FvtxArm()
  {
    // delete cages
    for (std::vector<FvtxCage*>::iterator iter = _cages.begin();
        iter != _cages.end(); iter++)
      if (*iter)
        {
          delete *iter;
          *iter = 0;
        }
  }

  //! @name accessors
  //@{

  //! cage from index
  FvtxCage*
  get_cage(const unsigned int& index) const
  {
    if (_cages.size() <= index)
      throw std::runtime_error("FvtxArm::get_cage - invalid index");
    if (!_cages[index])
      throw std::runtime_error("FvtxArm::get_cage - not initialized");
    return _cages[index];
  }

  //! set cage
  void
  set_cage(unsigned int index, FvtxCage* cage)
  {
    if (_cages[index])
      throw std::runtime_error("FvtxArm::set_cage - already initialized");
    _cages[index] = cage;
  }

  //@}

  //! @name dumper
  //@{

  //! print
  virtual void
  print(std::ostream& out = std::cout) const
  {
    out << "FvtxArm::print - index: " << index() << std::endl;
  }

  //@}

  //! set active status, for dead channel map
  virtual void
  set_active(bool b)
  {
    FvtxGeomObject::set_active(b);

    for (std::vector<FvtxCage*>::iterator iter = _cages.begin();
        iter < _cages.end(); iter++)
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

    for (std::vector<FvtxCage*>::iterator iter = _cages.begin();
        iter < _cages.end(); iter++)
      {
        sum_activity += (*iter)->get_active_ratio();
        cnt++;
      }

    return sum_activity / cnt;
  }


  virtual GeoDataID_t
  unique_index() const
  {
    return unique_index_encoder(index().arm());
  }

  //! GeoData_t -> FvtxGeomObject
  virtual const GeoDataRec_t &
  apply_geometry_data(const GeoData_t & data)
  {
    const GeoDataRec_t & rec = FvtxGeomObject::apply_geometry_data(data);

    for (std::vector<FvtxCage*>::iterator iter = _cages.begin();
        iter < _cages.end(); iter++)
      {
        (*iter)->apply_geometry_data(data);
      }

//    std::cout <<"FvtxArm::apply_geometry_data - get z ="<<get_z()<<", "<<_is_store_matrix<<std::endl;

    return rec;
  }

  //! FvtxGeomObject -> GeoData_t
  virtual GeoDataRec_t &
  save_geometry_data(GeoData_t & data) const
  {
    GeoDataRec_t & rec = FvtxGeomObject::save_geometry_data(data);

    for (std::vector<FvtxCage*>::const_iterator iter = _cages.begin();
        iter < _cages.end(); iter++)
      {
        (*iter)->save_geometry_data(data);
      }

    return rec;
  }
private:

  //! pointer to cages
  std::vector<FvtxCage*> _cages;

};

#endif
