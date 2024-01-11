// $Id: writeTubeEff.C,v 1.1 2007/04/05 08:30:16 hpereira Exp $
//! writes local tube efficiency files to the database using specified timestamps for validity
/*! 
  the default filenames are: 
    tube_eff_north_default.txt  
    tube_eff_south_default.txt
  the bank name is calib.mui.mui_tubeeff
  the container class is PdbMuiTubeEffBank
*/
void writeTubeEff( void )
{
  
  // load libraries
  gSystem->Load("libfun4all.so");
  gSystem->Load("libmuigeom.so");
    
  // database entry start of validity
  
//   // run4 Au+Au 200GeV (also covers Au+Au 63GeV and p+p 200GeV )
//   PHTimeStamp start=PHTimeStamp(2003,12,23,21,27,55); // begin of run 105287
//   PHTimeStamp stop=PHTimeStamp(2004,5,14,19,53,38);   // end of run 130553
//   const char* south_file = "/afs/rhic.bnl.gov/phenix/users/hpereira/muons/work/muideff_results/muideff_run4_AuAu_200GeV_twopack_south.dat";
//   const char* north_file = "/afs/rhic.bnl.gov/phenix/users/hpereira/muons/work/muideff_results/muideff_run4_AuAu_200GeV_twopack_north.dat";
//   const char* comments = "run4 Au+Au tube efficiencies";
  
//   // run5 Cu+Cu 200GeV
//   PHTimeStamp start=PHTimeStamp(2005,1,18,4,41,03); // begin of run 149539
//   PHTimeStamp stop=PHTimeStamp(2005,3,6,19,55,52);  // end of run 160487
//   const char* south_file = "/afs/rhic.bnl.gov/phenix/users/hpereira/muons/work/muideff_results/muideff_run5_CuCu_200GeV_twopack_south.dat";
//   const char* north_file = "/afs/rhic.bnl.gov/phenix/users/hpereira/muons/work/muideff_results/muideff_run5_CuCu_200GeV_twopack_north.dat";
//   const char* comments = "run5 Cu+Cu 200 GeV tube efficiencies";
 
  // run5 p+p 200GeV
  PHTimeStamp start=PHTimeStamp(2005,4,8,17,01,28); // begin of run 166400
  PHTimeStamp stop=PHTimeStamp(2005,6,24,07,38,36);  // end of run 179846
  const char* south_file = "/afs/rhic.bnl.gov/phenix/users/hpereira/muons/work/muideff_results/muideff_run5_PP_200GeV_twopack_south.dat";
  const char* north_file = "/afs/rhic.bnl.gov/phenix/users/hpereira/muons/work/muideff_results/muideff_run5_PP_200GeV_twopack_north.dat";
  const char* comments = "run5 P+P 200 GeV tube efficiencies";
  
  // initialize geometry
  /* 
    apart from the muid efficiencies all other configurations (geometry)
    are retrieved from the database, using start timestamp
  */
  mMuiInitModule* init = new mMuiInitModule();
  init->EnableDatabaseMode();
  init->SetSearchTimeStamp(start);
  PHCompositeNode *topnode = new PHCompositeNode("TOPNODE");
  init->event(topnode); 
  
  // configure HV mask
  TMuiHVMask::set_mode( TMuiHVMask::FROM_FILE );
  TMuiHVMask::set_filename_south( south_file );
  TMuiHVMask::set_filename_north( north_file );
  TMuiHVMask::initialize();
    
  // update database
  TMuiHVMask::update_database( start, stop, comments );
  
}
 
