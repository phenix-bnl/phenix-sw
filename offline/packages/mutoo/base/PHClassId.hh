#ifndef PHClassId_hh
#define PHClassId_hh

// $Id: PHClassId.hh,v 1.3 2011/12/24 04:48:18 slash Exp $

/*!
   \file PHClassId.hh
   \brief  map class names into unique ID
   \author H.Pereira
   \version $Revision: 1.3 $
   \date $Date: 2011/12/24 04:48:18 $
*/

#include <map>
#include <string>
#include <TDataType.h>
#include <iostream>

#include "MUTOO.h"

//! map class names to unique ID
class PHClassId
{
  
  public:
  
  //! class id type
  typedef unsigned short id_type;
  
  //! get class ID from name
  static id_type get( const std::string& name )
  {
    
    IdMap::const_iterator iter = _map.find( name );
    if( iter != _map.end() ) return iter->second;
    else
    {
      _id++;
      _map[name] = _id;
      return _id;
    }
    
  }
  
  //! get name from class ID
  static std::string get_name( const id_type& id )
  {
    for( IdMap::const_iterator iter = _map.begin(); iter != _map.end(); iter++ )
    { if( iter->second == id ) return iter->first; }
    return "unknown";
  }
  
  //! print map
  static void print( std::ostream& out = std::cout )
  {
    
    MUTOO::PRINT( out, "PHClassId::print" );
    for( IdMap::const_iterator iter = _map.begin(); iter != _map.end(); iter++ )
    { out << iter->first << ": " << iter->second << std::endl; }
    MUTOO::PRINT( out, "**" );
    
  }
  
  private:
  
  //! map names to id
  typedef std::map<std::string, id_type> IdMap;
  
  //! map names to id
  static IdMap _map;
  
  //! static id of last implemented class
  static id_type _id;
  
};

#endif
