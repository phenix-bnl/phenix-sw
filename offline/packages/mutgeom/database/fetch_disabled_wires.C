// $Id: fetch_disabled_wires.C,v 1.5 2009/01/23 15:39:22 hpereira Exp $
//! read disabled wires from database for a given run
void fetch_disabled_wires()
{
  
  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");
  
  // use one arm (here south) only because it reads disabled wires for both arms
  MutArm south( 0 );
  
  // define run number
  int run_number = 231429;
   
  // define output file name
  char out_filename[512];
  sprintf( out_filename, "mut.disabledAnodes.dat_run%i", run_number );

  // set verbosity to maximum
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::DISABLED_WIRES, TMutDatabaseCntrl::MAX );
  
  // fetch database (retrieves the number of disabled channels).
  int num_channels = south.fetchDisabledWires( run_number, 0, out_filename );
    
}
