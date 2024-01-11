/*!
  \file RpcDeadNoise.cxx
  \brief Class to store and retrieve information from the database.
*/

#include <cassert>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <iostream>
#include <fstream>

#include "RpcDeadNoise.h"
//#include "MUTGEOM.h"

using namespace std;

//________________________________________________
RpcDeadNoise::RpcDeadNoise()
{
  //The default constructor.
}

//________________________________________________
RpcDeadNoise::~RpcDeadNoise()
{
  //The default destructor.
}

//________________________________________________
int RpcDeadNoise::dbGetAll(PHTimeStamp tsearch)
{
  //Grab a database set for the RPC dead, noise, and cluster sizes
  
  assert( _strips.empty() );
  
  // start out fresh
  // Access the database, pull data into the internal structure
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHString calibname="rpcdeadnoise";
  PHString classname="PdbRpcDeadNoiseBank";
  PdbBankID bankid(0);

  // Open the Objectivity database for reading and pull all the values for
  // a given bank (i.e. a strip) and put it into our set
  if(application->startRead()) {
    PdbCalBank *rpcBank =
      bankManager->fetchBank(classname.getString(),
			     bankid, calibname.getString(), tsearch);
    if(rpcBank) {
      cout << "RpcDeadNoise::dbGetAll"
	   << " - start validity : " << rpcBank->getStartValTime()
	   << " (" << rpcBank->getStartValTime().getTics() << ")" << endl;
      cout << "RpcDeadNoise::dbGetAll"
	   << " - end validity   : " << rpcBank->getEndValTime()
	   << " (" << rpcBank->getEndValTime().getTics() << ")" << endl;
      cout << "RpcDeadNoise::dbGetAll"
	   << " - insertion      : " << rpcBank->getInsertTime()
	   << " (" << rpcBank->getInsertTime().getTics() << ")" << endl;
      cout << "RpcDeadNoise::dbGetAll"
	   << " - description    : " << rpcBank->getDescription() << endl;
      
      int length = rpcBank->getLength();
      
      for(int i=0 ; i<length ; i++) {
        PdbRpcDeadNoise strip = (PdbRpcDeadNoise&)(rpcBank->getEntry(i));
	
        // add the read strip to the set
        StripSet::const_iterator iter(_strips.find(strip));
        if(iter==_strips.end())  { //No set is found, add this
	  _strips.insert( strip ); }
        else {
          cout << "RpcDeadNoise::dbGetAll"
	       << " - a set with the same indices has already been inserted"
	       << endl;
          strip.print(); } }
      
      delete rpcBank; }
    else {
      cout << "RpcDeadNoise::dbGetAll"
	   << " - bankManager returned zero-pointer " << endl; } }
  else {
    application->abort();
    cout << "RpcDeadNoise::dbGetAll"
	 << " - Transaction aborted." <<endl; }
  
  return 0;
}

//_____________________________________________________
int RpcDeadNoise::dbPutAll( PHTimeStamp start, PHTimeStamp stop, PHString descriptor) const
{
  //Add a new entry into the database
  
  cout << "RpcDeadNoise::dbPutAll"
       << " - startvaltime: " << start << endl;
  cout << "RpcDeadNoise::dbPutAll"
       << " - endvaltime: "   << stop << endl;
  cout << "RpcDeadNoise::dbPutAll"
       << " - description: "  << descriptor << endl;
  
  // do we have something to put into the database?
  int length = _strips.size();
  if(length<1) {
    cout << "RpcDeadNoise::dbPutAll"
	 << " - Nothing to put into DB (length=" << length << ")" << endl;
    return -1; }
  
  //timestamp values: sanity check
  if(start>stop) {
    cout << "RpcDeadNoise::dbPutAll"
	 << " - invalid start and stop:" << start << stop << endl;
    cout << "ignored" << endl;
    return -1; }
  
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  PHString calibname="rpcdeadnoise";
  PHString classname="PdbRpcDeadNoiseBank";
  PdbBankID bankid(0);

  if(application->startUpdate()) {
    PdbCalBank *rpcBank =
      bankManager->createBank( classname.getString(),
			       bankid, descriptor.getString(),
			       start, stop, calibname.getString());
    cout << "RpcDeadNoise::dbPutAll"
	 << " - new bank created" << endl;
    cout << "Setting length to: " << length << endl;
    rpcBank->setLength(length);
    rpcBank->print();

    int i=0;
    for(StripSet::const_iterator iter=_strips.begin() ; iter!=_strips.end() ; iter++) {
      PdbRpcDeadNoise *strip_pointer = (PdbRpcDeadNoise*)&(rpcBank->getEntry(i));
      *strip_pointer = (*iter);
      i++; }

    //commit new bank to the db and delete
    application->commit( rpcBank );
    delete rpcBank; }
  else {
    cout << "RpcDeadNoise::dbPutAll"
	 << " - failed to start application for update" << endl; }
  
  cout << "RpcDeadNoise::dbPutAll"
       << " - done." << endl;
  cout << endl;

  return 0;
}

//____________________________________________________________
int RpcDeadNoise::txtGetAll( const char* infile )
{
  //Read the dead, noise, and cluster information from a text file.
  assert( _strips.empty() );

  ifstream fin(infile);
  if(!fin) {
    cout << "RpcDeadNoise::txtGetAll"
	 << " - cannot open file " << infile << " for input" << endl;
    return 1; }
  else { 
    cout << "RpcDeadNoise::txtGetAll - reading calibrations from file "
	 << infile << endl; }
  
  //restart the set
  _strips.clear();
  
  // read from the file as long as we can
  // add strips to set as we go along
  while(fin.good()) {
    PdbRpcDeadNoise strip;
    strip.read(fin);

    if(!fin.good()) { break; }
    
    // add the read strip to the set
    StripSet::const_iterator iter(_strips.find(strip));
    if(iter==_strips.end()) { //No set is found, add this
      _strips.insert( strip ); }
    else {
      cout << "RpcDeadNoise::txtGetAll"
	   << " - A set with the same indices has already been inserted"
	   << endl;
      strip.print();
    } }
  
  cout << "RpcDeadNoise::txtGetAll"
       << " - " << _strips.size() << " dead, noise, and cluster objects in the set" << endl;
  
  fin.close();
  return 0;
}

//___________________________________________________________________
int RpcDeadNoise::txtPutAll( const char* outfile ) const
{
  //Write the dead, noise, and cluster into a text file.
  ofstream fout(outfile);
  if(!fout) {
    cout << "RpcDeadNoise::txtPutAll"
	 << " - cannot open " << outfile << " for output" << endl;
    return 1; }
  
  // go thru the set and print all to fout
  for(StripSet::const_iterator iter=_strips.begin() ; iter!=_strips.end() ; iter++) {
    (*iter).write(fout); }
  
  fout.close();
  return 0;
}

//________________________________________________________________________
const PdbRpcDeadNoise* RpcDeadNoise::getPdbRpcDeadNoise(
  const int& arm, const int& station, const int& octant, const int& halfoctant,
  const int& radsegment) const
{
  // find and return the dead, noise, and cluster object in the set
  PdbRpcDeadNoise strip(arm,station,octant,halfoctant,radsegment);
  
  StripSet::iterator iter = _strips.find( strip );
  
  if(iter==_strips.end())  {
    cout << "RpcDeadNoise::getPdbRpcDeadNoise - "
	 << " arm " << arm
	 << " sta " << station
	 << " oct " << octant
	 << " hoct " << halfoctant
	 << " gap " << radsegment
	 << " - is not found in database " << endl;
    return NULL; }
  
  return &(*iter);
}

//______________________________________________________________________
void RpcDeadNoise::putPdbRpcDeadNoise(const PdbRpcDeadNoise *new_strip)
{
  //Add new dead, noise, and cluster information into the set.
  _strips.erase( *new_strip );
  _strips.insert( *new_strip );
}

//______________________________________________________________________
void RpcDeadNoise::print( ostream& out ) const
{
  //Print out some information.
  //  MUTGEOM::PRINT( out, "RpcDeadNoise::print" );

  for(StripSet::const_iterator iter=_strips.begin() ; iter!=_strips.end() ; iter++) {
    out << "[" << iter->getArm()
	<< "," << iter->getStation()
	<< "," << iter->getOctant()
	<< "," << iter->getHalfOctant()
	<< "," << iter->getRadSegment()
	<< "]" << endl;
    for(int istrip=0 ; istrip<PdbRpcDeadNoise::get_MaxNumStrips() ; istrip++) {
      out << "Strip " << istrip
	  << "(" << iter->get_Dead(istrip) << ","
	               << iter->get_Noise(istrip) << ","
	  << iter->get_ClusterSize(istrip) << ")" << endl; } }

  //  MUTGEOM::PRINT( out, "**" );
}
