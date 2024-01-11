
// $Id: MWGVersion.C,v 1.2 2013/02/08 17:21:35 jinhuang Exp $

/*!
  \file MWGVersion.C
  \brief map object class name to version
  \author Hugo Pereira
  \version $Revision: 1.2 $
  \date $Date: 2013/02/08 17:21:35 $
*/

#include <boost/regex.hpp>
#include <iostream>
#include <sstream>

#include "MWGVersion.h"

using namespace std;

//___________________________________________
map<string,unsigned int> MWGVersion::_versions;

//___________________________________________
unsigned int MWGVersion::get( string class_name )
{
  
  // try get version from map
  map<string, unsigned int>::const_iterator iter;
  if( ( iter = _versions.find( class_name ) ) != _versions.end() )
  { return iter->second; }
  
  // if not found, parse class name, insert in map
  unsigned int version( _get( class_name ) );
  _versions.insert( make_pair( class_name, version ) );
  
  // return version
  return version;
  
}

//___________________________________________
unsigned int MWGVersion::_get( string class_name )
{
  boost::regex reg_exp( "(\\d+)[\\>]?$" );
  boost::smatch what;
  if( boost::regex_search(class_name, what, reg_exp) )
  {  
   
    string captured( what[0] );
    istringstream in( captured );
    unsigned int version;
    in >> version;
    return version;
    
  } else {
    
    cout << "MWGVersion::_get - no version number found in class name " << class_name << endl;
    return 0;
    
  }

}
