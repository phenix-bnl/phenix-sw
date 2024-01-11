/*!
  \file RpcDBInfo.cxx
  \brief provides an interface to the database/text file
  \author Richard Hollis, UCR (rhollis@ucr.edu)
*/
#include "RpcDBInfo.h"
#include "RpcStrip.h"

#include <recoConsts.h>

//DB Interfacing classes
#include <RunToTime.hh>
#include "RpcDeadNoise.h"
#include <PdbRpcDeadNoise.hh>

#include <TString.h>

#include <cmath>
#include <fstream>

using namespace::std;

ClassImp( RpcDBInfo )

bool RpcDBInfo::instantiated   = false;
RpcDBInfo* RpcDBInfo::dbinstance = NULL;

RpcDBInfo::RpcDBInfo()
{
}

//_______________________________________________
RpcDBInfo *RpcDBInfo::getInstance()
{
  if(instantiated) { return dbinstance; }
  
  //Create an instance of the recoConsts
  recoConsts *myrc = recoConsts::instance();
  
  //Grab the DC Map type flag, set to DB if not set
  TString flag( myrc->get_CharFlag("RpcDCMap","DB") );
  
  if(flag.Contains("db") || flag.Contains("database") ||
     flag.Contains("DB")) {
    //-------------------------
    // Let's use the database
    //-------------------------
    const int run( myrc->get_IntFlag("RUNNUMBER",0) );
    dbinstance = new RpcDBInfo();
    dbinstance->readDB( run );
    instantiated=true;
    return dbinstance; }
  else if(flag.Contains("NULL") || flag.Contains("Null") || flag.Contains("null")) {
    //-------------------------
    // Use all strips
    //-------------------------
    dbinstance = new RpcDBInfo();
    dbinstance->reset();
    instantiated=true;
    return dbinstance; }
  else {
    //-------------------------
    //Must be a file, let's try to read it
    //- will return null arrays if file does not exist...
    //-------------------------
    dbinstance = new RpcDBInfo();
    dbinstance->readFile(flag.Data());
    instantiated=true;
    return dbinstance; }
}

//_______________________________________________
void RpcDBInfo::readFile(const std::string& fFile)
{
  std::ifstream inFile(fFile.c_str());
  if(!inFile.is_open()) {
    cout << "Could not open file " << fFile << "! ... "
	 << "resetting the arrays." << endl;
    reset();//safety feature
    return; }
  
  int arm, station,octant,ho,radseg,strip;
  int isdead;
  double noiserate;
  int counter = 0;
  int larm=-1, lstation=-1,loctant=-1,lho=-1,lradseg=-1,lstrip=-1;
  while(!inFile.eof() && counter<10000) {
    inFile >> arm >> station >> octant >> ho
	   >> radseg >> strip;
    inFile >> isdead >> noiserate;
    
    if(larm==arm && lstation==station && loctant==octant &&
       lho==ho && lradseg==radseg && lstrip==strip) { break; }
    larm=arm; lstation=station; loctant=octant; lho=ho;
    lradseg=radseg; lstrip=strip;
    
    DeadChannel[arm][station][octant][ho][radseg][strip] = isdead;
    NoiseInChannel[arm][station][octant][ho][radseg][strip] = noiserate;

    counter++; } 
}

//_______________________________________________
void RpcDBInfo::readDB( const int run )
{
  //Safety feature - reset all channels to be valid
  reset();
  
  if(run==0) {
    cout << "RpcDBInfo::readDB WARNING: \"RUNNUMBER\" is not set!!;" << endl;
    cout << "No dead channels are set!!" << endl;
    return; }
  
  //RunToTime converts the run number into a start time
  RunToTime *r2t = RunToTime::instance();
  //Make a PHTimeStamp object for the start time of the run
  PHTimeStamp *start = r2t->getBeginTime(run);
  
  //Create a new dead channel object
  RpcDeadNoise *dbdead = new RpcDeadNoise();
  dbdead->dbGetAll(*start);

  for(int arm=0 ; arm<RPCFULLGEOM::NumberOfArms ; arm++) {
    for(int sta=0 ; sta<RPCFULLGEOM::NumberOfStations ; sta++) {
      for(int oct=0 ; oct<8 ; oct++) {
	for(int hoct=0 ; hoct<2 ; hoct++) {
	  for(int rseg=0 ; rseg<3 ; rseg++) {
	    //do we delete this??
	    const PdbRpcDeadNoise *mydb = dbdead->getPdbRpcDeadNoise(arm,sta,oct,hoct,rseg);
	    if(mydb == NULL) { continue; }
	    for(int str=0 ; str<64 ; str++) {
	      DeadChannel[arm][sta][oct][hoct][rseg][str] = mydb->get_Dead(str);
	      NoiseInChannel[arm][sta][oct][hoct][rseg][str] = mydb->get_Noise(str);
	      //Also: mydb->get_ClusterSize(str);
	    } } } } } }  
  
  delete dbdead;
  delete start;
}

//_______________________________________________
void RpcDBInfo::reset()
{
  //Reset the arrays to be 0
  // i.e. nothing dead and no noise...
  for(int arm=0 ; arm<RPCFULLGEOM::NumberOfArms ; arm++) {
    for(int sta=0 ; sta<RPCFULLGEOM::NumberOfStations ; sta++) {
      for(int oct=0 ; oct<8 ; oct++) {
	for(int hoct=0 ; hoct<2 ; hoct++) {
	  for(int rseg=0 ; rseg<3 ; rseg++) {
	    for(int str=0 ; str<64 ; str++) {
	      DeadChannel[arm][sta][oct][hoct][rseg][str] = 0;
	      NoiseInChannel[arm][sta][oct][hoct][rseg][str] = 0;
	    } } } } } }  
}

//_______________________________________________
void RpcDBInfo::killall()
{
  //Set all the channels to be dead and 100% noise
  // for testing only, of course ;)
  for(int arm=0 ; arm<RPCFULLGEOM::NumberOfArms ; arm++) {
    for(int sta=0 ; sta<RPCFULLGEOM::NumberOfStations ; sta++) {
      for(int oct=0 ; oct<8 ; oct++) {
	for(int hoct=0 ; hoct<2 ; hoct++) {
	  for(int rseg=0 ; rseg<3 ; rseg++) {
	    for(int str=0 ; str<64 ; str++) {
	      DeadChannel[arm][sta][oct][hoct][rseg][str] = 1;
	      NoiseInChannel[arm][sta][oct][hoct][rseg][str] = 1;
	    } } } } } }  
}

//_______________________________________________
void RpcDBInfo::flatnoise(double rate)
{
  //Set a flat noise rate for the detector
  // e.g. 0.1 = 10% noise, or fires noise 1 in every 10 events
  for(int arm=0 ; arm<RPCFULLGEOM::NumberOfArms ; arm++) {
    for(int sta=0 ; sta<RPCFULLGEOM::NumberOfStations ; sta++) {
      for(int oct=0 ; oct<8 ; oct++) {
	for(int hoct=0 ; hoct<2 ; hoct++) {
	  for(int rseg=0 ; rseg<3 ; rseg++) {
	    for(int str=0 ; str<64 ; str++) {
	      DeadChannel[arm][sta][oct][hoct][rseg][str] = 0;
	      NoiseInChannel[arm][sta][oct][hoct][rseg][str] = rate;
	    } } } } } }  
}

//_______________________________________________
bool RpcDBInfo::isDead(RpcStrip *strip)
{  
  return DeadChannel[strip->GetArm()][strip->GetStation()][strip->GetOctant()][strip->GetHalfOctant()][strip->GetRSeg()][strip->GetStripId()];
}

//_______________________________________________
double RpcDBInfo::getNoise(RpcStrip *strip)
{    
  return NoiseInChannel[strip->GetArm()][strip->GetStation()][strip->GetOctant()][strip->GetHalfOctant()][strip->GetRSeg()][strip->GetStripId()];
}
