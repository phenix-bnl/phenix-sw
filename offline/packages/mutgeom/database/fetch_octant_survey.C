// $Id: fetch_octant_survey.C,v 1.2 2009/05/26 16:39:51 hpereira Exp $
/*! 
  this method dumps muon tracker octant survey to a text file
  for a given timestamp
*/
void fetch_octant_survey()
{

  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");
  
  // define run number
  int run_number = 231429;
   
  // get time stamp
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp *tsearch = runTime->getBeginTime( abs(run_number) );
  cout << "fetch_octant_survey - search time: " << *tsearch << endl;
  
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::OCTANT_SURVEY, TMutDatabaseCntrl::MAX );
  TMutDatabaseCntrl::set_database_access( "use_local_octant_survey_file", true );
  TMutDatabaseCntrl::set_filename( "use_local_octant_survey_file", "octant_survey-v0.txt" );

  // South arm	
  cout << "fetch_octant_survey - south arm:" << endl;
  MutArm *south = new MutArm( 0, *tsearch );
  
  cout << "fetch_octant_survey - north arm:" << endl;
  MutArm *south = new MutArm( 1, *tsearch );
  
  cout << "fetch_octant_survey - done." << endl;
}
