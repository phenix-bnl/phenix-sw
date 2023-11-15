// $Id: PdbPisaInterface.hh,v 1.2 2007/03/27 14:09:14 hpereira Exp $

#ifndef PdbPisaInterface_h
#define PdbPisaInterface_h

/*!
   \file PdbPisaInterface.hh
   \brief basic container for pisa configuration files
   \author Hugo Pereira
   \version $Revision: 1.2 $
   \date $Date: 2007/03/27 14:09:14 $
*/

#include <iostream>
#include <string>

#include "PdbCalChan.hh"

//! basic container for pisa configuration files
class PdbPisaInterface : public PdbCalChan
{
  public:
  
  //! constructor
  PdbPisaInterface()
  {}

  //! destructor
  virtual ~PdbPisaInterface()
  {}

  //! print
  virtual void print( void ) const
  {
    std::cout << "PdbPisaInterface::print" << std::endl;
    std::cout << "PdbPisaInterface::_comments: " << _comments << std::endl;
    std::cout << "PdbPisaInterface::_contents: " << _contents << std::endl;
  }
    
  //! comments
  virtual const std::string& get_comments( void ) const
  { return _comments; }
  
  //! comments
  virtual void set_comments( const std::string& value ) 
  { _comments = value; }

  //! contents
  virtual const std::string& get_contents( void ) const
  { return _contents; }
  
  //! contents
  virtual void set_contents( const std::string& value ) 
  { _contents = value; }

  private:
  
  /* comments associated to commit */
  std::string _comments;

  /* contents of committed file */
  std::string _contents;

  ClassDef(PdbPisaInterface,1);
  
};

#endif 
