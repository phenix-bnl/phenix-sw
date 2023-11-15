// $Id: PdbFvtxAlignmentNumeric.hh,v 1.1 2013/11/05 00:02:32 jinhuang Exp $

/*!
 * \file PdbFvtxAlignmentNumeric.hh
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/11/05 00:02:32 $
 */

#ifndef PdbFvtxAlignmentNumeric_HH_
#define PdbFvtxAlignmentNumeric_HH_

#include "PdbCalChan.hh"

#include <vector>
#include <utility>
#include <map>

/*!
 * \brief PdbFvtxAlignmentNumeric
 *
 * Save FVTX geometry to database in numeric format under STL containers
 *
 */
class PdbFvtxAlignmentNumeric : public PdbCalChan
{
public:
  PdbFvtxAlignmentNumeric();
  virtual
  ~PdbFvtxAlignmentNumeric();

  virtual void
  print() const
  {
    print(0);
  }

  virtual void
  print(unsigned int depth) const;

  // these typedefs should be consistent with the counterpart in FvtxGeomObject
  //! ID for geometry data
  typedef unsigned int GeoDataID_t;

  //! single record for geometry data
  typedef std::map<std::string, double> GeoDataRec_t;

  //! data set for geometry data
  typedef std::map<GeoDataID_t, GeoDataRec_t> GeoData_t;

  const GeoData_t & get_data() const
    {
      return _data;
    }

  GeoData_t & get_data()
    {
      return _data;
    }

  void
  set_data(const GeoData_t & data)
  {
    _data = data;
  }

private:

  GeoData_t _data;

ClassDef(PdbFvtxAlignmentNumeric,1)

};

#endif /* PdbFvtxAlignmentNumeric_HH_ */
