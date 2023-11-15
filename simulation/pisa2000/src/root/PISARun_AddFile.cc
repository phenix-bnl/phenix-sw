// $Id: PISARun_AddFile.cc,v 1.1 2007/11/13 22:29:23 hpereira Exp $

/*!
  \file   PISARun_AddFile.cc
  \brief  interface to pisa
  \author H. Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2007/11/13 22:29:23 $
*/

#include "PISARun.h"

using namespace std;

//__________________________________________________________________________________
bool PISARun::AddFile( const string& filename )
{
  
  cout << "PISARun::AddFile - adding " << filename << endl;
  
  // open TFile and check existence
  PISAInput::TFile_pointer tfile_pointer( new TFile( filename.c_str() ) );
  if( !tfile_pointer->IsOpen() )
  {
    cout << "PISARun::AddFile - cannot open file " << filename << endl;
    return false;
  }
  
  // try load tree
  TTree *ttree = (TTree*) tfile_pointer->Get("T");
  if( !ttree )
  {
    cout << "PISARun::AddFile - cannot find tree \"T\" in " << filename << endl;
    return false;
  }
  
  // check branch
  if( !ttree->GetBranch( "pisaevent" ) )
  {
    cout << "PISARun::AddFile - cannot find branch \"pisaevent\" in " << filename << endl;
    return false;
  }
  
  // update _max_events
  if( _inputs.empty() ) _max_events = ttree->GetEntries();
  else _max_events = min( _max_events, static_cast<int>(ttree->GetEntries()) );
  
  // create pisa event and set branch address
  PISAEvent* pisa_event( new PISAEvent() );
  ttree->GetBranch("pisaevent")->SetAddress( &pisa_event );
  
  // create input structure
  PISAInput input = 
  {
    filename, 
    tfile_pointer,
    ttree,
    PISAEvent::pointer( pisa_event )
  };
  
  // store structure
  _inputs.push_back( input );
  
  return true;

}
