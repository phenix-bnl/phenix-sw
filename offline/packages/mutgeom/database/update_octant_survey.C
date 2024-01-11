// $Id: update_octant_survey.C,v 1.1 2009/05/26 16:33:51 hpereira Exp $
//! write octant survey from file to database
/*! 
  takes a list of disabled channels as input.
  user must provide the validity range for the database insertion, namely 
  the time interval between which the channels are to be disabled
*/
//#include <RunNumberRanges.h>
void update_octant_survey( const char* file = "DeadChannels.dat" )
{
    
  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");
  
  
  PHTimeStamp *begin_time = new PHTimeStamp( 2010,1,1,0,0,0 );
  PHTimeStamp *end_time = new PHTimeStamp( 2011,1,1,0,0,0 );
  const char* filename = "octant_survey-v0.txt";
  const char* comments = "test octant survey file (copied from 'official' bank entry)";
  
  cout << "update_octant_survey - validity range: [" << *begin_time << "," << *end_time << "]" << endl;
  cout << "update_octant_survey - filename: " << filename << endl;
   
  if( true )
  {
    cout << "update_octant_survey - processing south arm." << endl;
    MutArm *south = new MutArm( 0 );  
    south->updateOctantSurvey( *begin_time, *end_time, 0, comments, file ); 
    
    cout << "update_octant_survey - processing north arm." << endl;
    MutArm *north = new MutArm( 1 );  
    north->updateOctantSurvey( *begin_time, *end_time, 0, comments, file ); 
  
  }
  
  return;
  
}
