// $Id: writeDB.C,v 1.2 2006/12/20 17:05:30 hpereira Exp $
//! writes local files to the database using specified timestamps for validity
/*! 
  this method writes local files to the database using specified timestamps for validity
  for a given timestamp. The required local files are:
<ul>
 <li> mui-panel-geom.dat
 <li> mui-panel-size.dat
 <li> mui-tube-geom.dat
 <li> mui-tube-size.dat
 <li> mui-fem-config.dat
</ul> 

*/
void writeDB( void )
{
  // load libraries
  gSystem->Load("libfun4all.so");
  gSystem->Load("libmuigeom.so");

  mMuiAddDBEntry* addentry = new mMuiAddDBEntry();
  
  // database entry start of validity
  PHTimeStamp start=PHTimeStamp(2010,1,1,1,0,0);

  // database entry end of validity
  PHTimeStamp stop=PHTimeStamp(2012,4,6,12,0,0);
  stop->setToFarFuture();
  
  // write to database
  PHCompositeNode *topnode = new PHCompositeNode("TOPNODE");
  addentry->SetTimeStamps(start,stop);
  addentry->SetDescription( "test commit, to be removed" );
  addentry->event(topnode);
}
