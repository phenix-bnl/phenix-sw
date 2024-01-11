//////////////////////////////////////////////////////////////////
/*!
  \file TMutCoordFill.h
  \author: M. Brooks
  \version $Revision: 1.3 $
  \date    $Date: 2005/08/26 16:54:52 $
  \brief 
    Takes centroid values from a Cluster object and creates corresponding
    Coord objects.
    
*/
//////////////////////////////////////////////////////////////////

#ifndef __TMUTCOORDFILL_H__
#define __TMUTCOORDFILL_H__

#include <list>
#include "TMutClusMap.h"
#include "TMutCoordMap.h"
#include "MUTOO.h"

/*!
  \brief 
    Takes centroid values from a Cluster object and creates corresponding
    Coord objects.
*/
class TMutCoordFill
{
  public: 
  
  //! make coordinates from cluster centroids, put the in map, return list of created coordinates
  static std::list<TMutCoordMap::pointer> create_coords( TMutClusMap::pointer, TMutCoordMap* coord_map );

  //! set gsl_fiter verbosity
  static void set_verbosity( MUTOO::Verbosity value )
  { _verbosity = value; }
  
  private:
  
  //! verbosity level
  static MUTOO::Verbosity _verbosity;
   
};
#endif
