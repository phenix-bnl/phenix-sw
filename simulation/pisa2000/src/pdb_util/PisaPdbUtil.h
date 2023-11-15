// $Id: PisaPdbUtil.h,v 1.2 2007/03/28 11:55:30 hpereira Exp $
#ifndef PisaPdbUtil_h
#define PisaPdbUtil_h

/*!
   \file PisaPdbUtil.h
   \brief basic interface between pisa phnx.par file and database
   \author Hugo Pereira
   \version $Revision: 1.2 $
   \date $Date: 2007/03/28 11:55:30 $
*/

#include <TObject.h>
#include <string>
#include <PdbPisaInterface.hh>
#include <PHTimeStamp.h>

//! basic interface between pisa phnx.par file and database
class PisaPdbUtil: public TObject
{
  public:
  
  //! constructor
  PisaPdbUtil( void )
  {}
  
  //! destructor
  ~PisaPdbUtil()
  {}
  
  //! read phnx.par file
  void read( const char* file );
  
  //! read object from DB
  void read( const int& rhic_run, const int& rhic_subrun = 0);
  
  //! read object from DB
  void read( PHTimeStamp );
  
  //! write to file
  void write( const char* file ) const;
  
  //! write to db
  void write( PHTimeStamp start, PHTimeStamp stop, const char* comments = "no comments" ) const;
  
  //! retrieve interface
  PdbPisaInterface& interface( void )
  { return _interface; }
    
  private:
  
  //! current database interface object
  PdbPisaInterface _interface;
  
  //! root dictionary
  ClassDef( PisaPdbUtil, 0 );
  
};

#endif
