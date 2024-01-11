
//////////////////////////////////////////////////////////////
/*! 
  \file PHResources.cxx
  \brief muon code resources evaluation
  \author  Hugo Pereira
  \version $Revision: 1.4 $
*/
//////////////////////////////////////////////////////////////

#include "MUTOO.h"
#include "PHResources.h"

using namespace std;

//______________________________________________________
bool PHResources::update( const char* tag )
{
  
  try{

    _resources_current.update();
    _resources_diff = _resources_current - _resources_previous;
    _resources_previous = _resources_current;

    if( _verbosity >= MUTOO::SOME )
    {
      if( tag ) {
        cout << "PHResources::update - tag: " << tag << " current  : " << _resources_current << endl;
        cout << "PHResources::update - tag: " << tag << " increment: " << _resources_diff << endl;
      } else {
        cout << "PHResources::update - current  : " << _resources_current << endl;
        cout << "PHResources::update - increment: " << _resources_diff << endl;
      }
    }
    
  } catch( exception& e ) { cout << e.what() << endl; }
  
  return true;
}

//_____________________________________________________________
bool PHResources::Resources::update( void )
{

  _valid = true;

  
  // Get proc fileName 
  char filename[80];
  sprintf( filename, "/proc/%i/status", _pid );
  
  FILE *fid;
  fid = fopen( filename, "r" );
  if( fid ) {
    int   bufsize = 1024;
    char* buffer = new char[bufsize];
    int   status = fread( buffer, 1, bufsize, fid );
    fclose( fid );
    
    if( status > 0 ) 
    {
      char* cursor = strstr( buffer, "VmSize:" );
      if( cursor ) {
	      sscanf( cursor, 
      		"VmSize: %i kB\n"
      		"VmLck:  %i kB\n"
      		"VmRSS:  %i kB\n"
      		"VmData: %i kB\n"
      		"VmStk:  %i kB\n"
      		"VmExe:  %i kB\n"
      		"VmLib:  %i kB\n",
      		&_vmSize,  &_vmLock, &_vmResident, &_vmData,
      		&_vmStack, &_vmExec, &_vmLib );
      } else _valid = false;
    }   else _valid = false;
    
    if( buffer ) delete [] buffer;
    
  } else {
    std::cout << "PHResources::Resources::Update - ERROR: cannot open \"" << filename << "\"\n";
    _valid = false;
  }

  return _valid;
}

//_____________________________________________________________
ostream& operator << ( ostream& out, const PHResources::Resources& resources ) 
{
  out 
    << "pid: " << resources._pid
    << " valid: " << (resources._valid ? "true":"false" )
    << " total: " << resources._vmSize << " kb"
    << " lock: " << resources._vmLock << " kb"
    << " resident: " << resources._vmResident << " kb"
    << " data: " << resources._vmData << " kb"
    << " stack: " << resources._vmStack << " kb"
    << " exec: " << resources._vmExec << " kb"
    << " lib: " << resources._vmLib << " kb";
  return out;
}
