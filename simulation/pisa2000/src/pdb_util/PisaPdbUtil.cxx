// $Id: PisaPdbUtil.cxx,v 1.7 2009/08/20 04:15:15 pinkenbu Exp $

/*!
   \file PisaPdbUtil.cxx
   \brief basic interface between pisa phnx.par file and database
   \author Hugo Pereira
   \version $Revision: 1.7 $
   \date $Date: 2009/08/20 04:15:15 $
*/

#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbBankManager.hh>
#include <PdbCalBank.hh>
#include <RunToTime.hh>

#include <PisaPdbUtil.h>
#include <RunNumberRanges.h>

#include <cstdlib>
#include <iostream>
#include <fstream>

using namespace std;

ClassImp( PisaPdbUtil );

//_______________________________________________________________
void PisaPdbUtil::read( const char* file )
{
  
  // open and test stream
  ifstream in( file );
  if( !in ) 
  { 
    cout << "PisaPdbUtil::read - unable to read from file " << file << endl; 
    return;
  } 
  
  cout << "PisaPdbUtil::read - loading file: " << file << endl;
  
  // store file contents into a string
  string contents;
  while( !(in.rdstate() & ios::failbit ) )
  {
    char c = 0;
    in.get(c);
    
    // add character to string. Skip null characters.
    if( c && !(in.rdstate() & ios::failbit ) ) contents.push_back( c );
    
  }   
  
  // update interface object
  _interface.set_contents( contents );
  
}
 
//_______________________________________________________________
void PisaPdbUtil::read(  const int& rhic_run, const int& rhic_subrun )
{ 
  
  // retrieve/check runToTime instance
  RunToTime *runTime = RunToTime::instance();
  if( !runTime ) 
  {
    cout << "PisaPdbUtil::read - no RunToTime instance." << endl;
    return;
  }
  
  // map run number to timestamp
  PHTimeStamp time_stamp;
  switch( rhic_run )
  {
    case 1:
      case 2:
      time_stamp = PHTimeStamp( 2001, 05, 01, 00, 00, 00 );
    break;
    
    case 3:
      time_stamp = *runTime->getBeginTime( BEGIN_OF_RUN3 );
    break;
    
    case 4:
      time_stamp = *runTime->getBeginTime( BEGIN_OF_RUN4 );
    break;
    
    case 5:
      time_stamp = *runTime->getBeginTime( BEGIN_OF_RUN5 );
    break;
    
    case 6:
      time_stamp = *runTime->getBeginTime( BEGIN_OF_RUN6 );
    break;
     
    case 7:
      time_stamp = *runTime->getBeginTime( BEGIN_OF_RUN7 );
    break;
    
    default:
    cout 
      << "PisaPdbUtil::read -"
      << " rhic_run " << rhic_run 
      << " subrun " << rhic_subrun 
      << " not supported." 
      << endl;
    exit(0);
    break;
    
  }
  
  cout 
    << "PisaPdbUtil::read -"
    << " rhic_run: " << rhic_run 
    << " subrun: " << rhic_subrun 
    << " timestamp: " << time_stamp
    << endl;  
  
  read( time_stamp );
  
}

//_______________________________________________________________
void PisaPdbUtil::read( PHTimeStamp stamp )
{
  static const std::string bank_name = "pisa.phnx.par";
  static const std::string class_name = "PdbPisaInterfaceBank";

  PdbBankID bank_id = 0;
  PdbBankManager *bank_manager = PdbBankManager::instance();
  
  if( !bank_manager ) 
  {
    cout << "PisaPdbUtil::read - no bank_manager instance." << endl;
    return;
  }

  PdbApplication *application = bank_manager->getApplication();  
  
  if(!application->startRead()) {
    cout<<"PisaPdbUtil::read - Database not readable." << endl;
    application->abort();
    return;
  }  
    
  // read bank
  PdbCalBank *bank = bank_manager->fetchBank( class_name.c_str(), bank_id, bank_name.c_str(), stamp );
  if( !bank ) {
    cout<<"PisaPdbUtil::read - unable to fetch bank named " << bank_name << "." << endl;
    return;
  }

  cout<<"PisaPdbUtil::read - reading " << bank_name << " for timestamp " << stamp << endl;

  // read interface
  PdbPisaInterface* interface = (PdbPisaInterface*) &bank->getEntry(0);
  _interface = *interface;
  
  // close application and delete bank
  application->commit();
  delete bank;
  
  return; 

}

//_______________________________________________________________
void PisaPdbUtil::write( const char* file ) const
{
  const string& contents( _interface.get_contents() );
  if( contents.empty() )
  { 
    cout << "PisaPdbUtil::write - no contents to be written. Canceled." << endl; 
    return;
  }
    
  ofstream out( file );
  if( !out )
  {
    cout << "PisaPdbUtil::write - Unable to write to file " << file << endl; 
    return;
  }
    
  cout << "PisaPdbUtil::write - Writing to file " << file << endl; 
  out << contents;
  out.close();
  
}

//_______________________________________________________________
void PisaPdbUtil::write( PHTimeStamp start, PHTimeStamp stop, const char* comments ) const
{ 

  static const std::string bank_name = "pisa.phnx.par";
  static const std::string class_name = "PdbPisaInterfaceBank";
  
  PdbBankID bank_id = 0;
  
  PdbBankManager *bank_manager = PdbBankManager::instance();
  if( !bank_manager ) 
  {
    cout << "PisaPdbUtil::write - no bank_manager instance." << endl;
    return;
  }
  cout<<"PisaPdbUtil::write - retrieved bank_manager." << endl;

  
  PdbApplication *application = bank_manager->getApplication();  
  
  if(!application->startUpdate()) {
    cout<<"PisaPdbUtil::write - Database not writable." << endl;
    application->abort();
    return;
  }  

  PdbCalBank *bank = bank_manager->createBank( class_name.c_str(),bank_id, "", start, stop, bank_name.c_str() );
  if( !bank ) {
    cout << "PisaPdbUtil::write - unable to create bank named " << bank_name << "." << endl;
    return;
  }

  // adjust bank length
  bank->setLength(1);
  
  // copy _interface to the database
  PdbPisaInterface *interface = (PdbPisaInterface*) &bank->getEntry(0);
  *interface = _interface;
  interface->set_comments( comments );
  
  // output
  cout << "PisaPdbUtil::write - writing to database." << endl;
  
  // try commit modifications
  application->commit( bank );
  delete bank;
  
  return; 

}
