// $Id: update_disabled_wires.C,v 1.6 2014/08/07 14:15:42 slash Exp $
//! write disabled wires from file to database
/*! 
  takes a list of runs as input.
  user must provide the correct path to the matching files that are to be committed to the DB
*/
void update_disabled_wires( const char* file = "run_list.txt" )
{
    
  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");
  
  // use one arm (here south) only because it reads disabled wires for both arms
  MutArm *south = new MutArm( 0 );
    
  // path
  
  // run4 path
  // const char* path = "/phenix/WWW/p/draft/mjkwn/run4pp/HVstatus/mutr/HVConfig";
  // const char* path = "/phenix/WWW/p/draft/mjkwn/run4AuAu/HVstatus/mutr/HVConfig";
  // const char* path = "/phenix/WWW/p/draft/mjkwn/run4AuAu63GeV/HVstatus/mutr/HVConfig";
  
  // run5 path
  // const char* path = "/phenix/WWW/p/draft/djkim/simulation/HV/run5CuCu_200GeV/HVstatus/mutr/HVConfig";
  // const char* path = "/phenix/WWW/p/draft/djkim/simulation/HV/run5CuCu_62GeV/HVstatus/mutr/HVConfig";
  // const char* path = "/phenix/WWW/p/draft/djkim/simulation/HV/run5pp_200GeV/HVstatus/mutr/HVConfig";  
  
  // run6 path
  // const char* path = "/phenix/u/hpereira/afs/public/www/database/run6_pp/";

  // run7 path
  // const char* path = "/phenix/u/hpereira/afs/public/www/database/run7_auau/";

  // run8 path
  // const char* path = "/phenix/u/hpereira/afs/public/www/database/run8_dau/";
  
  // run9 path
  //  const char* path = "/phenix/WWW/p/draft/kempel/Run9MuonQA/disabledAnodes_500GeV";

  // run12 path
  const char* path = "phenix/WWW/slash/Run12CuAu/disabledAnodes";

  // open run list
  ifstream in( file );
  while(!in.eof()) 
  {

    char line[512];
    in.getline(line,512);
    
    // test if line is not empty and not commented
    if( !strlen( line ) ) continue;
    if( strncmp( line, "//", 2 ) == 0 ) continue;
   
    // read run number
    int run_number;
    if( sscanf( line, "%i", &run_number ) != 1 ) continue;    
  
    // create filename
    char filename[512];
    sprintf( filename, "%s/mut.disabledAnodes.dat_run%i", path, run_number );

    // create description
    char description[512];
    sprintf( description, "Run12 Cu+Au 200 GeV - run %i", run_number );
    
    // write file to the database
    south->updateDisabledWires( run_number, 0, description, filename );
    
  }
  
}
