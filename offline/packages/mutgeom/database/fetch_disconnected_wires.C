// $Id: fetch_disconnected_wires.C,v 1.3 2009/07/02 21:01:01 hpereira Exp $
//! read disabled wires from database for a given run
void fetch_disconnected_wires()
{
  
  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");
  
  // use one arm (here south) only because it reads disabled wires for both arms
  MutArm *south = new MutArm( 0 );
  MutArm *north = new MutArm( 1 );
 
  // define run number
  //int run_number = 231429;
   
  // run 9 run number
  int run_number = 278937;
  
  // define output file name
  char out_filename[512];

  // set verbosity to maximum
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::DISCONNECTED_WIRES, TMutDatabaseCntrl::MAX );
  
  // fetch database (retrieves the number of disabled channels).
  south->fetchDisconnectedWires( run_number, 0 );
  north->fetchDisconnectedWires( run_number, 0 );
    
}
