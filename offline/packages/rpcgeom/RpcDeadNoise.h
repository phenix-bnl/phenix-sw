#ifndef __RPCDEADNOISE_H__
#define __RPCDEADNOISE_H__

/*!
  \file RpcDeadNoise.h
  \brief Class to store and retrieve information from the database.
*/

#include <PHTimeStamp.h>
#include <PHString.h>

#include <iostream>
#include <vector>
#include <set>
#include <PdbRpcDeadNoise.hh>

//#include "MUTGEOM.h"

#ifndef __CINT__
#include <boost/array.hpp>
#endif

//! RPC Geometry ordering (based on the unique id)
class RpcDeadNoiseOrder
{
 public:
  bool operator() (const PdbRpcDeadNoise p1, const PdbRpcDeadNoise p2) const
  { return ( p1.getUniqueId() < p2.getUniqueId()); }  
};

//! RPC Geometry interface
class RpcDeadNoise
{

 public:

  //! constructor
  RpcDeadNoise();

  //! destructor
  ~RpcDeadNoise();
  
  //! read calibration objects from database
  int dbGetAll(PHTimeStamp tsearch);
  
  //! write calibration objects to database
  int dbPutAll(PHTimeStamp start, PHTimeStamp stop, PHString descriptor) const;

  //! read calibration from ASCII text
  int txtGetAll( const char* infile);

  //! write calibration to ASCII text
  int txtPutAll( const char* outfile) const;

  //! number of Calibration objects to store
  int getNumberOfSets() const
  { return (int) _strips.size(); }

  //! reset
  void reset( void )
  { _strips.clear(); }

  //! access to the individual calib objects
  /*! changing these afterwards does not change the contents in the set */
  const PdbRpcDeadNoise* getPdbRpcDeadNoise(
    const int& arm,
    const int& station,
    const int& octant,
    const int& halfoctant,
    const int& radsegment
    ) const;

  /*!
  put in other strip values,
  overwriting if strip is already existing in set
  */
  void putPdbRpcDeadNoise(const PdbRpcDeadNoise* );

  //! print
  void print( std::ostream& out = std::cout ) const;

  protected:

  //! set of geometry entries
  typedef std::set<PdbRpcDeadNoise, RpcDeadNoiseOrder> StripSet;
  
  StripSet _strips;
};
#endif /* __RPCGEOMETRY_H__ */
