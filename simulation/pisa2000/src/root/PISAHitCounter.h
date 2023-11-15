#ifndef PISAHitCounter_h
#define PISAHitCounter_h

// $Id: PISAHitCounter.h,v 1.3 2010/12/14 14:57:21 bbannier Exp $

/*!
  \file   PISAHitCounter.h
  \brief  keeps track of how many hits are recorded per event for a given subsystem
  \author Hugo Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2010/12/14 14:57:21 $
*/

#include <cstdio>
#include <iostream>

//! keeps track of how many hits are recorded per event for a given subsystem
class PISAHitCounter
{
  
  public:
  
  //! constructor
  PISAHitCounter( void ):
    _accumulated_size(0),
    _ncycle(0)
  {}
  
  //! add counts
  /*! 
  the file argument is used when several files
  are merged, in which case, _ncycle is incremented 
  only when the file is equal to 0
  */
  void add_count( size_t counts, int file = 0 )
  {
    _accumulated_size += counts;
    if( file == 0 ) _ncycle++;
  }
  
  //! accumulated size
  const size_t& accumulated_size() const
  { return _accumulated_size; }
  
  //! cycles
  const size_t& cycles( void ) const
  { return _ncycle; }
  
  //! size per cycle
  double size_per_cycle( void ) const
  { return double( _accumulated_size ) / _ncycle; }
  
  private:
  
  //! total number of objects stored in this map (updated at every clear())
  size_t _accumulated_size;

  //! total number of cycles (clear())
  size_t _ncycle;

  //! streamer
  friend std::ostream& operator << ( std::ostream& out, const PISAHitCounter& counter )
  {
    
    char str[512];
    sprintf( str, "accumulated: %-7lu, cycles: %-10lu, per cycle %-6g",
      static_cast<unsigned long>(counter.accumulated_size()),
      static_cast<unsigned long>(counter.cycles()),
      counter.size_per_cycle() );
    out << str;
    
    return out;
    
  }
  
};

#endif
