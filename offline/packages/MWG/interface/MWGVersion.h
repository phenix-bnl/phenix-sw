#ifndef MWGVersion_h
#define MWGVersion_h

// $Id: MWGVersion.h,v 1.1 2009/07/04 18:55:50 hpereira Exp $

/*!
  \file MWGVersion.h
  \brief map object class name to version
  \author Hugo Pereira
  \version $Revision: 1.1 $
  \date $Date: 2009/07/04 18:55:50 $
*/

#include <string>
#include <map>

class MWGVersion
{
  
  public:
  
  //! get version matching class name
  static unsigned int get( std::string class_name );
  
  //! true if class name is of type PHMuoTracks
  static bool is_PHMuoTracks( std::string class_name )
  { 
    static std::string match( "PHMuoTrack" );
    return class_name.substr( 0, match.size() ) == match;
  }
  
  //! true if class name is of type PHMuoTracks
  static bool is_PHdiMuoTrack( std::string class_name )
  { 
    static std::string match( "PHdiMuoTrack" );
    return class_name.substr( 0, match.size() ) == match;
  }

  private:
  
  //! get version from class name
  static unsigned int _get( std::string class_name );
  
  //! match class name and version
  static std::map<std::string, unsigned int> _versions;
  
};

#endif
