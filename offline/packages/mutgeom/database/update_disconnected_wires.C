// $Id: update_disconnected_wires.C,v 1.3 2009/08/22 05:13:23 hpereira Exp $
//! write disconnected wires from file to database
/*! user must provide list of disconnected wires and the validity range */
void update_disconnected_wires( void )
{
  
  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

//   // run6
//   PHTimeStamp start( 2006, 1, 1, 0, 0, 0 );  // 1st January 2006
//   PHTimeStamp stop( 2006, 10, 31, 23, 59, 59 ); // 31 october 2006
//   const char* file = "files/mut.disabledWires.dat.run6";
//   const char* tag = "Run6 disconnected wires";
  
//   // run7 
//   PHTimeStamp start( 2006, 11, 1, 0, 0, 0 );  // 1st november 2006
//   PHTimeStamp stop( 2007, 10, 31, 23, 59, 59 ); // 31 october 2007 
//   const char* file = "files/mut.disabledWires.dat.run7";
//   const char* tag = "Run7 disconnected wires, updated with bug-fixed number of wires per channel (Hugo)";
  
//   // run8
//   PHTimeStamp start( 2007, 11, 1, 0, 0, 0 );  // 1st november 2007
//   PHTimeStamp stop( 2009, 02, 01, 23, 59, 59 );  // February 1st 2009
//   const char* file = "files/mut.disabledWires.dat.run8";
//   const char* tag = "Run8 disconnected wires, updated with bug-fixed number of wires per channel (Hugo)";
  
  // run9
  PHTimeStamp start( 2009, 02, 02, 0, 0, 0 );  // February 2nd 2009
  PHTimeStamp stop; stop.setToFarFuture(); // forever
  const char* tag = "Run9 disconnected wires, updated with bug-fixed number of wires per channel (Hugo)";
  
  // both arm need to be called because they are writting the
  // relevant wires to different banks.
  for( unsigned int i_arm = 0; i_arm < 2; i_arm++ )
  {
    MutArm *arm = new MutArm( i_arm );
    arm->updateDisconnectedWires( start, stop, 0, tag, file );
  }
  
}
