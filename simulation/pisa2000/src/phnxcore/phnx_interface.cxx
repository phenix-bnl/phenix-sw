// $Id: phnx_interface.cxx,v 1.5 2007/03/28 13:34:06 pinkenbu Exp $

/*!
  \file phnx_interface.cxx
  \brief initialize fortran logical unit from file phnx.par or from database
  \author Hugo Pereira
  \version $Revision: 1.5 $
  \date $Date: 2007/03/28 13:34:06 $
*/

#include <iostream>
#include <fstream>
#include <string>
#include <unistd.h>

#include <PHTimeStamp.h>
#include <PdbPisaInterface.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbBankManager.hh>
#include <PdbCalBank.hh>
#include <PisaPdbUtil.h>

#include <RunToTime.hh>

/* postgres specific database access */
#include <PgPostApplication.hh>
#include <PgPostBankManager.hh>
#include <RunToTimePg.hh>


using namespace std;

//! initialize geometry scrath file from character string
/*! this method is from fortran phnx_par.f */
extern "C" {void init_phnx_par__( char*, int );}

//! initialize geometry 
/*! 
  \param file filename
  \param rhic_run phenix run number (2, 3, etc)
  \param rhic_subrun phenix sub run number (not used)
  \param length length of the filename (to retrieve the fortran string properly)
*/
extern "C" {void init_phnx_interface__( char* file, int* rhic_run, int* rhic_subrun ); }

//_____________________________________________________________________
void init_phnx_interface__( char* tmp, int* rhic_run, int* rhic_subrun )
{
  cout << "phnx_interface::init_phnx_interface__ " << endl;

  // copy character string into stl
  string file( tmp ); 
  
  // need to remove trailing spaces from the fortran string
  while( file.length() && file[file.length()-1] == ' ' ) file = file.substr( 0, file.length()-1 );
  if( file.empty() ) {
    cout << "phnx_interface::init_phnx_interface_ - no valid file provided. Using default phnx.par." << endl;
    file = "phnx.par";
  }
  
  cout 
    << "phnx_interface::init_phnx_interface__ -"
    << " file: " << file
    << " rhic_run: " << *rhic_run 
    << " subrun: " << *rhic_subrun 
    << endl;
  
  
  // check for local file
  PisaPdbUtil pdb_util;
  if( access( file.c_str(), R_OK ) == 0 ) pdb_util.read( file.c_str() );
  else 
  {
    
    // initialize access to Pdb calibration database
    PgPostApplication::Register();
    PgPostBankManager::Register();
    RunToTimePg::Register();
    
    // read
    pdb_util.read( *rhic_run, *rhic_subrun );

  }
    
  // initialize from string
  string contents( pdb_util.interface().get_contents() );
  init_phnx_par__( const_cast<char*>( contents.c_str() ), contents.size() );
  
  return;
  
}
