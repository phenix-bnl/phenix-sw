// $Id: rcp.h,v 1.1 2009/07/04 18:32:23 hpereira Exp $

//////////////////////////////////////////////////////////////////
/*!
   \file    rcp.h
   \brief   nanodst runtime parameter manager
   \version $Revision: 1.1 $
   \date    $Date: 2009/07/04 18:32:23 $
*/
//////////////////////////////////////////////////////////////////

#ifndef _RCP_H
#define _RCP_H

#include <string>
#include <fstream>
#include <sstream>
#include <iostream>

/*
  \class RCP
  \brief pure static class to access rcp filename and read options
*/
class RCP {
  
  public:
  
  //! check a given file is accessible
  static bool file_ok(const char* filename);
  
  /* \brief 
    look vor variable varname in RCP filename
    assign it to value if variable type matches T
    returns true if found with correct type, false otherwise.
    must be inlined due to template
  */
  template < typename T >
  static bool get( const char* filename, const char* varname, T& value )
  {
    // open/check input file
    std::ifstream from(filename); 
    if( !from ){
      std::cout << "RCP::get - ERROR: cannot open "<<filename<<std::endl;
      return false;
    }
    
    // check variable name
    if( !( varname && std::string( varname ).size() ) ) {
      std::cout << "RCP::get - ERROR: incorrect varname.\n";
      return false;
    }
  
    // search string
    std::string current_string( "" ); 
    
    while( !from.eof()) {
      from >> current_string;
      if( current_string != varname ) continue;
  
      // look for the equal sign
      char equal;
      from >> equal;
  	  if (equal != '=') {
        std::cout << "RCP::get - ERROR: improper line format (missing '=' sign) " << std::endl;
        continue;
      }
      
      // try get correct type value
      T tmp_value;
      from >> tmp_value;
      if( from.rdstate() & std::ios::failbit ) {
        std::cout << "RCPget - ERROR: wrong type for " << varname << std::endl;
        continue;
      }
      
      value = tmp_value;
      return true;
      
    }
        
    std::cout << "RCP::get - WARNING: could not find " << varname << std::endl;
    return false;
  }
};
#endif
