// $Id: pisaRootRead.cc,v 1.28 2007/11/13 22:34:10 hpereira Exp $

/*!
  \file   pisarRootRead.cc
  \brief  reads several input files and fill pisa evaluation ntuples for all subsystems
  \author Charlie Maguire
  \version $Revision: 1.28 $
  \date $Date: 2007/11/13 22:34:10 $
*/


#include <iostream>
#include <cstdlib>

#include <TROOT.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <boost/weak_ptr.hpp>
#include <sstream>
#include <vector>

#include "PISARun.h"
#include "PISAEvent.h"

using namespace std;

//________________________________________
//! main
int main(int argc, char** argv)
{

  try {
    
    cout << "usage: pisaRootRead [n_events] [n_files]" << endl;
    cout << endl;
        
    // Initialize the ROOT system
    TROOT simple("", "");
    
    // variable to control how many events are read
    // set event limit from first argument, if there is one
    int events = 0;            
    if(argc > 1) events = atoi(argv[1]);  
    
    // number of files
    int files = 1;
    if(argc > 2) files = atoi(argv[2]);   
  
    if(files < 1 ) 
    {
      cout << "pisaRootRead - illegal number of files: " << files << endl;
      exit(1);
    }
  
    // load TFiles
    // the use of shared_ptr makes sure that files are closed and deleted at the end of the job.
    // the use of vector allows to add as many TFiles as needed without caring about the array size.
    cout << "pisaRootRead - adding TFiles" << endl;
    PISARun run;
    for( int i = 0; i < files; i++ )
    {
      
      // generate rootfile name
      ostringstream file_name;
      if( i == 0 ) file_name << "PISAEvent.root";
      else file_name << "PISAEvent" << i << ".root";
      
      // add file to pisa interface
      run.AddFile( file_name.str() );
            
    }    
    
    // check that at least one file is found
    if( !run.GetNInputs() )
    { 
      cout << "pisaRootRead - no valid file opened." << endl;
      exit(0);
    }
   
    // update number of events
    events = (events == 0) ? run.GetMaxEvents() : min( run.GetMaxEvents(), events );
    cout << "pisaRootRead - reading " << events << " events" << endl;
    
    for( int event = 0; event < events; event++ )
    {
      
      cout << "pisaRootRead - event " << event << endl;
      run.GetOneEvent( event );
      run.AncAll(0);
      run.HitsClear();
  
    }
  
    // run all anc modules with flag 1, to close ntuples
    run.AncAll(1);
    run.PrintCounters();
    
  } catch (exception &e ) { cout << "pisaRootRead - " << e.what() << endl; }
  
  cout << "pisaRootRead - successfully completed" << endl;
  
  return 0;
}
