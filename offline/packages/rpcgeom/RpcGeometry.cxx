/*!
  \file RpcGeometry.cxx
  \brief Class to store and retrieve information from the database.
*/

#include <cassert>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <iostream>
#include <fstream>

#include "RpcGeometry.h"
//#include "MUTGEOM.h"

using namespace std;

//________________________________________________
RpcGeometry::RpcGeometry()
{
  //The default constructor.
}

//________________________________________________
RpcGeometry::~RpcGeometry()
{
  //The default destructor.
}

//________________________________________________
int RpcGeometry::dbGetAll(PHTimeStamp tsearch)
{
  //Grab a database set for the RPC geometry
  assert( _strips.empty() );
  
  // start out fresh
  // Access the database, pull data into the internal structure
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHString calibname="rpcgeom";
  PHString classname="PdbRpcGeometryBank";
  PdbBankID bankid(0);

  // Open the Objectivity database for reading and pull all the values for
  // a given bank (i.e. a strip) and put it into our set
  if(application->startRead()) {
    PdbCalBank *rpcBank =
      bankManager->fetchBank(classname.getString(),
			     bankid, calibname.getString(), tsearch);
    if(rpcBank) {
      cout << "RpcGeometry::dbGetAll"
	   << " - start validity : " << rpcBank->getStartValTime()
	   << " (" << rpcBank->getStartValTime().getTics() << ")" << endl;
      cout << "RpcGeometry::dbGetAll"
	   << " - end validity   : " << rpcBank->getEndValTime()
	   << " (" << rpcBank->getEndValTime().getTics() << ")" << endl;
      cout << "RpcGeometry::dbGetAll"
	   << " - insertion      : " << rpcBank->getInsertTime()
	   << " (" << rpcBank->getInsertTime().getTics() << ")" << endl;
      cout << "RpcGeometry::dbGetAll"
	   << " - description    : " << rpcBank->getDescription() << endl;
      
      int length = rpcBank->getLength();
      
      for(int i=0 ; i<length ; i++) {
        PdbRpcGeometry strip = (PdbRpcGeometry&)(rpcBank->getEntry(i));
	
        // add the read strip to the set
        StripSet::const_iterator iter(_strips.find(strip));
        if(iter==_strips.end())  { //No set is found, add this
	  _strips.insert( strip ); }
        else {
          cout << "RpcGeometry::dbGetAll"
	       << " - a set with the same indices has already been inserted"
	       << endl;
          strip.print(); } }
      
      delete rpcBank; }
    else {
      cout << "RpcGeometry::dbGetAll"
	   << " - bankManager returned zero-pointer " << endl; } }
  else {
    application->abort();
    cout << "RpcGeometry::dbGetAll"
	 << " - Transaction aborted." <<endl; }
  
  return 0;
}

//_____________________________________________________
int RpcGeometry::dbPutAll( PHTimeStamp start, PHTimeStamp stop, PHString descriptor) const
{
  //Add a new entry into the database
  cout << "RpcGeometry::dbPutAll"
       << " - startvaltime: " << start << endl;
  cout << "RpcGeometry::dbPutAll"
       << " - endvaltime: "   << stop << endl;
  cout << "RpcGeometry::dbPutAll"
       << " - description: "  << descriptor << endl;
  
  // do we have something to put into the database?
  int length = _strips.size();
  if(length<1) {
    cout << "RpcGeometry::dbPutAll"
	 << " - Nothing to put into DB (length=" << length << ")" << endl;
    return -1; }
  
  //timestamp values: sanity check
  if(start>stop) {
    cout << "RpcGeometry::dbPutAll"
	 << " - invalid start and stop:" << start << stop << endl;
    cout << "ignored" << endl;
    return -1; }
  
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  PHString calibname="rpcgeom";
  PHString classname="PdbRpcGeometryBank";
  PdbBankID bankid(0);

  if(application->startUpdate()) {
    PdbCalBank *rpcBank =
      bankManager->createBank( classname.getString(),
			       bankid, descriptor.getString(),
			       start, stop, calibname.getString());
    cout << "RpcGeometry::dbPutAll"
	 << " - new bank created" << endl;
    cout << "Setting length to: " << length << endl;
    rpcBank->setLength(length);
    rpcBank->print();

    int i=0;
    for(StripSet::const_iterator iter=_strips.begin() ; iter!=_strips.end() ; iter++) {
      PdbRpcGeometry *strip_pointer = (PdbRpcGeometry*)&(rpcBank->getEntry(i));
      *strip_pointer = (*iter);
      i++; }

    //commit new bank to the db and delete
    application->commit( rpcBank );
    delete rpcBank; }
  else {
    cout << "RpcGeometry::dbPutAll"
	 << " - failed to start application for update" << endl; }
  
  cout << "RpcGeometry::dbPutAll"
       << " - done." << endl;
  cout << endl;

  return 0;
}

//____________________________________________________________
int RpcGeometry::txtGetAll( const char* infile )
{
  //Read the geometry from a text file.
  assert( _strips.empty() );

  ifstream fin(infile);
  if(!fin) {
    cout << "RpcGeometry::txtGetAll"
	 << " - cannot open file " << infile << " for input" << endl;
    return 1; }
  else { 
    cout << "RpcGeometry::txtGetAll - reading calibrations from file "
	 << infile << endl; }
  
  //restart the set
  _strips.clear();
  
  // read from the file as long as we can
  // add strips to set as we go along
  while(fin.good()) {
    PdbRpcGeometry strip;
    strip.read(fin);

    if(!fin.good()) { break; }
    
    // add the read strip to the set
    StripSet::const_iterator iter(_strips.find(strip));
    if(iter==_strips.end()) { //No set is found, add this
      _strips.insert( strip ); }
    else {
      cout << "RpcGeometry::txtGetAll"
	   << " - A set with the same indices has already been inserted"
	   << endl;
      strip.print();
    } }
  
  cout << "RpcGeometry::txtGetAll"
       << " - " << _strips.size() << " geometry objects in the set" << endl;
  
  fin.close();
  return 0;
}

//___________________________________________________________________
int RpcGeometry::txtPutAll( const char* outfile ) const
{
  //Write the geometry into a text file.
  ofstream fout(outfile);
  if(!fout) {
    cout << "RpcGeometry::txtPutAll"
	 << " - cannot open " << outfile << " for output" << endl;
    return 1; }
  
  // go thru the set and print all to fout
  for(StripSet::const_iterator iter=_strips.begin() ; iter!=_strips.end() ; iter++) {
    (*iter).write(fout); }
  
  fout.close();
  return 0;
}

//________________________________________________________________________
const PdbRpcGeometry* RpcGeometry::getPdbRpcGeometry(
  const int& arm, const int& station, const int& octant, const int& halfoctant,
  const int& radsegment) const
{
  // find and return the geometry object in the set
  PdbRpcGeometry strip(arm,station,octant,halfoctant,radsegment);
  
  StripSet::iterator iter = _strips.find( strip );
  
  if(iter==_strips.end())  {
    cout << "RpcGeometry::getPdbRpcGeometry - "
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
void RpcGeometry::putPdbRpcGeometry(const PdbRpcGeometry *new_strip)
{
  //Add new geometry information into the set.
  _strips.erase( *new_strip );
  _strips.insert( *new_strip );
}

//______________________________________________________________________
void RpcGeometry::print( ostream& out ) const
{
  //Print out some information.
  //  MUTGEOM::PRINT( out, "RpcGeometry::print" );

  for(StripSet::const_iterator iter=_strips.begin() ; iter!=_strips.end() ; iter++) {
    out << "[" << iter->getArm()
	<< "," << iter->getStation()
	<< "," << iter->getOctant()
	<< "," << iter->getHalfOctant()
	<< "," << iter->getRadSegment()
	<< "] Packet Offset: " << iter->get_PacketOffset() << endl;
    for(int istrip=0 ; istrip<PdbRpcGeometry::get_MaxNumStrips() ; istrip++) {
      out << "Strip " << istrip
	  << " from (" << iter->get_PosXBegin(istrip) << ","
	               << iter->get_PosYBegin(istrip) << ","
	               << iter->get_PosZBegin(istrip) << ")"
	  << " to ("   << iter->get_PosXEnd(istrip) << ","
	               << iter->get_PosYEnd(istrip) << ","
	               << iter->get_PosZEnd(istrip) << ")"
	  << " Area "  << iter->get_StripArea(istrip) << endl; } }
  
  //  MUTGEOM::PRINT( out, "**" );
}
