// $Id: readTubeEff.C,v 1.1 2007/04/05 08:30:16 hpereira Exp $
//! writes tube efficiency database to local tube efficiency files
/*! 
  the default filenames are: 
    tube_eff_north_default.txt  
    tube_eff_south_default.txt
  the bank name is calib.mui.mui_tubeeff
  the container class is PdbMuiTubeEffBank
*/

void readTubeEff( void )
{
  
  // load libraries
  gSystem->Load("libfun4all.so");
  gSystem->Load("libmuigeom.so");
    
  // database entry start of validity
  PHTimeStamp time_stamp=PHTimeStamp(2004,1,1,1,0,0);

  // input files
  const char* south_file = "tube_eff_south_default.txt_clone";
  const char* north_file = "tube_eff_north_default.txt_clone";
    
  // initialize geometry
  /* 
    apart from the muid efficiencies all other configurations (geometry)
    are retrieved from the database, using start timestamp
  */
  mMuiInitModule* init = new mMuiInitModule();
  init->EnableDatabaseMode();
  init->SetSearchTimeStamp(time_stamp);
  PHCompositeNode *topnode = new PHCompositeNode("TOPNODE");
  init->event(topnode); 
  
  // configure HV mask
  TMuiHVMask::set_mode( TMuiHVMask::FROM_DATABASE );
  TMuiHVMask::set_search_timestamp( time_stamp );
  TMuiHVMask::initialize();
  
  // update files
  TMuiHVMask::set_filename_south( south_file );
  TMuiHVMask::set_filename_north( north_file );
  TMuiHVMask::update_files();
}
