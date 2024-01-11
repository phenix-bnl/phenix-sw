// $Id: update_dead_channels.C,v 1.3 2009/01/23 15:39:23 hpereira Exp $
//! write dead channels from file to database
/*! 
  takes a of disabled channels as input.
  user must provide the validity range for the database insertion, namely 
  the time interval between which the channels are to be disabled
*/
//#include <RunNumberRanges.h>
void update_dead_channels( const char* file = "DeadChannels.dat" )
{
    
  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");
  
  // check file
  cout << "update_dead_channels - file: " << file << endl;
  ifstream in( file );
  if( !in ) 
  { 
    cout << "update_dead_channels - cannot read file " << file << " - aborting." << endl;
    return;
  }
  
  
  const char* comments = "Default list of dead channels (empty), starting from Run6";
//   // define the time range from begin/end run and get the associated timeStamps
//   int first_run = 188000;
//   int last_run = 0;
//   RunToTime *runTime = RunToTime::instance();
//   
//   // begin time
//   PHTimeStamp* begin_time = runTime->getBeginTime( first_run );
//   if( !begin_time )
//   {
//     cout << "update_dead_channels - unable to get begin time for run " << first_run << " - aborting." << endl;
//     return;
//   }
//   
//   // end time
//   PHTimeStamp* end_time;
//   if( last_run > 0 ) end_time = runTime->getEndTime( last_run );
//   else {
//     
//     end_time = new PHTimeStamp();
//     end_time->setToFarFuture();
//       
//   }
//   
//   if( !end_time ) 
//   {
//     cout << "update_dead_channels - unable to get end time for run " << last_run << " - aborting." << endl;
//     return;
//   }
    
  //PHTimeStamp *begin_time = new PHTimeStamp( 2006,03,03,13,59,01 );
  //PHTimeStamp *end_time = new PHTimeStamp( 2007,11,26,17,08,58 );  

  PHTimeStamp *begin_time = new PHTimeStamp( 2008,03,09,19,36,00 );
  PHTimeStamp *end_time = new PHTimeStamp();
  end_time->setToFarFuture();
  
  cout << "update_dead_channels - validity range: [" << *begin_time << "," << *end_time << "]" << endl;
 
  if( 1 )
  {
  
    // need to create both arms, because each arm will read only its own channels from the input file
    cout << "update_dead_channels - processing south arm." << endl;
    MutArm *south = new MutArm( 0 );  
    south->updateDeadChannels( *begin_time, *end_time, 0, comments, file ); 
    
//     cout << endl;
//     cout << "update_dead_channels - processing north arm." << endl;
//     MutArm *north = new MutArm( 1 );
//     north->updateDeadChannels( *begin_time, *end_time, 0, comments, file ); 
  
  }
  
  return;
  
}
