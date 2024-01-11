/*!
  \file MutChamberPar.cc
  \brief declare calibration data class that will store the
  calibration data and be able to retrieve it from the database.
*/
#include <cassert>
#include <RunToTime.hh>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbMutChamberPar.hh>
#include <TMutDatabaseInit.h>
#include <iostream>
#include <fstream>
#include <cstdlib>

#include "MutChamberPar.hh"

using namespace std;

//________________________________________________
MutChamberPar::MutChamberPar():
  _initialized( false )
{}

//________________________________________________
MutChamberPar::~MutChamberPar()
{}

//______________________________________________
MutChamberPar& MutChamberPar::get( void )
{
  static MutChamberPar singleton;
  return singleton;
}

void MutChamberPar::initialize()
{
  assert( !initialized() );
  
  PHTimeStamp timeStamp = TMutDatabaseInit::get_time_stamp();
  std::cout << "MutChamberPar::initialize - time stamp: " << timeStamp << " (" << timeStamp.getTics() << ")" << std::endl;
  
  if( !timeStamp.getTics() )
    {
      std::cout << "MutChamberPar::initialize - time stamp is invalid. No point continuing" << std::endl;
      std::cout << "MutChamberPar::initialize - You should either do the initialization by calling " << std::endl;
      std::cout << "MutChamberPar::initialize - TMutDatabaseInit::initialize( PHCompositeNode *);, " << std::endl;
      std::cout << "MutChamberPar::initialize - in which case run number will be used to set the time stamp; " << std::endl;
      std::cout << "MutChamberPar::initialize - or set a valid timestamp manually, using " << std::endl;
      std::cout << "MutChamberPar::initialize - TMutDatabaseInit::setTimeStamp( PHTimeStamp ); in your macro. " << std::endl;
      exit(0);
    }
  dbGetAll( timeStamp );
  set_initialized( true );
}


//_____________________________________________________
int MutChamberPar::dbGetAll( int runnumber)
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(runnumber));
  PHTimeStamp Tstart = *ts;
  delete ts;

  return dbGetAll( Tstart);  
}

//________________________________________________
int MutChamberPar::dbGetAll(PHTimeStamp tsearch)
{

  assert( _chambers.empty() );

  // start out fresh
  // Access the database, pull data into the internal structure
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHString calibname = "mut.chamber.par";
  PHString classname = "PdbMutChamberParBank";
  PdbBankID bankid(2);
  
  // Open the Objectivity database for reading and pull all the values for
  // a given bank (i.e. a chamber) and put it into our set
  if ( application->startRead() )
    {

      PdbCalBank *mutBank =
        bankManager->fetchBank(classname.getString(),
                               bankid, calibname.getString(), tsearch);

      if (mutBank)
        {

	  //          printf("MutChamberPar::dbGetAll - start validity : %s (%u)\n", mutBank->getStartValTime().formatTimeString(), (unsigned int)mutBank->getStartValTime().getTics());
	  //          printf("MutChamberPar::dbGetAll - end validity   : %s (%u)\n", mutBank->getEndValTime().formatTimeString(), (unsigned int)mutBank->getEndValTime().getTics());
	  //          printf("MutChamberPar::dbGetAll - insertion      : %s (%u)\n", mutBank->getInsertTime().formatTimeString(), (unsigned int) mutBank->getInsertTime().getTics());
	  //          printf("MutChamberPar::dbGetAll - description    : %s\n", mutBank->getDescription().getString());

          //mutBank->print();
          int length = mutBank->getLength();

          for (int i = 0; i < length; i++)
            {
	      PdbMutChamberPar chamber = (PdbMutChamberPar&)(mutBank->getEntry(i));
	      int chamber_ID = chamber.get_unique_ID();
	      _chambers.insert( pair<int,PdbMutChamberPar>(chamber_ID, chamber) );		  
	    }
          delete mutBank;
        }
      else
        {
          printf("MutChamberPar::dbGetAll - bankManager returned zero-pointer\n");
        }
    }
  else
    {
      application->abort();
      printf("MutChamberPar::dbGetAll - Transaction aborted.\n");

    }

  return 0;
}

//_____________________________________________________
int MutChamberPar::dbPutAll( int beginrun, int endrun, PHString descriptor) const
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0) {
    ts = runTime->getEndTime(endrun);
    Tstop = *ts;
    delete ts;
  }
  else {
    Tstop.setToFarFuture();
  }

  return dbPutAll( Tstart, Tstop, descriptor);  
}

//_____________________________________________________
int MutChamberPar::dbPutAll( PHTimeStamp start, PHTimeStamp stop, PHString descriptor) const
{

  cout << "MutChamberPar::dbPutAll - startvaltime: " << start << endl;
  cout << "MutChamberPar::dbPutAll - endvaltime: " << stop << endl;
  cout << "MutChamberPar::dbPutAll - description: " << descriptor << endl;

  // do we have something to put into the database?
  int length = _chambers.size();
  if (length < 1)
  {
    cout << "MutChamberPar::dbPutAll - Nothing to put into DB " << endl;
    cout << "length = " << length << endl;
    return -1;
  }

  //timestamp values: sanity check
  if (start>stop)
  {
    cout << "MutChamberPar::dbPutAll - invalid start and stop:" << start << stop << endl;
    cout << "ignored" << endl;
    return -1;
  }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHString calibname="response.mut.par";
  PHString classname="PdbMutChamberParBank";
  PdbBankID bankid(2);

  if (application->startUpdate())
  {

    PdbCalBank *mutBank =
      bankManager->createBank( classname.getString(),
      bankid, descriptor.getString(),
      start, stop, calibname.getString());

    cout << "MutChamberPar::dbPutAll - new bank created" << endl;
    mutBank->setLength(length);
    mutBank->print();

    for(size_t i=0; i<_chambers.size(); i++)
    {
      PdbMutChamberPar *chamber_pointer = (PdbMutChamberPar*)&(mutBank->getEntry(i));
      *chamber_pointer = (_chambers.find( i )->second);
    }

    // commit new bank to the db and delete
    application->commit( mutBank );
    delete mutBank;

  } else cout << "MutChamberPar::dbPutAll - failed to start application for update" << endl;

  cout << "MutChamberPar::dbPutAll - done." << endl;
  cout << endl;

  return 0;
}

//____________________________________________________________
int MutChamberPar::txtGetAll( const char* infile )
{

  assert( _chambers.empty() );

  ifstream fin(infile);
  if (!fin)
  {

    cout << "MutChamberPar::txtGetAll - can't open " << infile << "for input" << endl;
    return 1;

  } else cout << "MutChamberPar::txtGetAll - reading calibrations from file " << infile << endl;

  //setup
  _chambers.clear();

  // read from the file as long as we can
  // add chamber to map as we go along
  while ( fin.good() )
  {
    PdbMutChamberPar chamber;
    chamber.read(fin);
    if (! fin.good() ) break;
    {
      int chamber_ID = chamber.get_unique_ID();
      _chambers.insert( pair<int,PdbMutChamberPar>(chamber_ID, chamber) );
    }
  }
  cout << "MutChamberPar::txtGetAll - Read " << _chambers.size() << " chambers" << endl;

  fin.close();
  return 0;
}

//___________________________________________________________________
int MutChamberPar::txtPutAll( const char* outfile ) const
{
  ofstream fout(outfile);
  if (!fout)
  {
    cout << "MutChamberPar::txtPutAll - can't open " << outfile << "for output" << endl;
    return 1;
  }

  // go thru the set and print all to fout
  for(size_t i=0; i<_chambers.size(); i++)
  { 
    const PdbMutChamberPar* chamber = &_chambers.find( i )->second;
    chamber->write(fout); 
  }

  fout.close();
  return 0;
}

//________________________________________________________________________
const PdbMutChamberPar* 
MutChamberPar::getPdbMutChamberPar( const int& arm,
				     const int& station, const int& octant, const int& halfoctant,
				     const int& gap, const int& plane )
{
  if ( !initialized() ) initialize();

  // find object in the set and return pointer to it (NULL, if not found)
  int chamber_ID = PdbMutChamberPar::get_unique_ID(arm, station, octant, halfoctant, gap, plane);
  const PdbMutChamberPar* chamber_found = &_chambers.find( chamber_ID )->second;
  if (chamber_found) return chamber_found;

  cout
    << "MutChamberPar::getPdbMutChamberPar - "
    << " arm " << arm
    << " sta " << station
    << " oct " << octant
    << " hoct " << halfoctant
    << " gap " << gap
    << " pla " << plane
    << " chamber " << chamber_ID
    << " - not found in database " << endl;
  return NULL;
}

//___________________________________________________________________________________
void MutChamberPar::putPdbMutChamberPar(const PdbMutChamberPar *new_chamber)
{
  int chamber_ID = new_chamber->get_unique_ID();
  _chambers.erase( _chambers.find( chamber_ID ) );
  _chambers.insert( pair<int,PdbMutChamberPar>(chamber_ID, *new_chamber) );
}

//______________________________________________________________________
void MutChamberPar::print( ostream& out ) const
{

  out << "MutChamberPar::print \n";

  out << "Arm Station Octant HalfOct. Gap Plane Chamber pedestal gain rms" << endl;

  for(size_t i=0; i<_chambers.size(); i++)
    {
      const PdbMutChamberPar* chamber = &_chambers.find( i )->second;
      chamber->print();
    }

  out << "**" << endl;
  
}

//____________________________________________________________
void MutChamberPar::load_time_offset( string filename )
{
  ifstream file(filename.c_str());
  int arm,station,gap,cathode;
  float time_offset, time_offset_rms;
  while ( file >> arm >> station >> gap >> cathode >> time_offset >> time_offset_rms )
    {
      for (int ioct=0; ioct<8; ioct++)
	for (int ihalfoct=0; ihalfoct<2; ihalfoct++)
	  {
	    int chamber_ID = PdbMutChamberPar::get_unique_ID(arm, station, ioct, ihalfoct, gap, cathode);
	    PdbMutChamberPar chamber;
	    if ( _chambers.find( chamber_ID ) != _chambers.end() )
	      chamber = _chambers.find( chamber_ID )->second;
	    else
	      {
		chamber = PdbMutChamberPar(arm, station, ioct, ihalfoct, gap, cathode);
		_chambers.insert( pair<int,PdbMutChamberPar>(chamber_ID, chamber) );
	      }
	    chamber.set_time_offset( time_offset );
	    chamber.set_time_offset_rms( time_offset_rms );
	  }
    }
  file.close();
}

//____________________________________________________________
void MutChamberPar::load_pedestal( string filename )
{
  ifstream file(filename.c_str());
  int arm,station,gap,cathode;
  float pedestal;
  while ( file >> arm >> station >> gap >> cathode >> pedestal )
    {
      for (int ioct=0; ioct<8; ioct++)
	for (int ihalfoct=0; ihalfoct<2; ihalfoct++)
	  {
	    int chamber_ID = PdbMutChamberPar::get_unique_ID(arm, station, ioct, ihalfoct, gap, cathode);
	    PdbMutChamberPar chamber;
	    if ( _chambers.find( chamber_ID ) != _chambers.end() )
	      chamber = _chambers.find( chamber_ID )->second;
	    else
	      {
		chamber = PdbMutChamberPar(arm, station, ioct, ihalfoct, gap, cathode);
		_chambers.insert( pair<int,PdbMutChamberPar>(chamber_ID, chamber) );
	      }
	    chamber.set_pedestal( pedestal );
	  }
    }
  file.close();
}

//____________________________________________________________
void MutChamberPar::load_Landau_parameters( string filename )
{
  ifstream file(filename.c_str());
  int arm,station,gap,cathode, octant, halfoctant;
  float Landau_offset, Landau_scale;
  while ( file >> arm >> station >> octant >> halfoctant >> gap >> cathode >> Landau_offset >> Landau_scale )
    {
      int chamber_ID = PdbMutChamberPar::get_unique_ID(arm, station, octant, halfoctant, gap, cathode);
      PdbMutChamberPar chamber;
      if ( _chambers.find( chamber_ID ) != _chambers.end() )
	chamber = _chambers.find( chamber_ID )->second;
      else
	{
	  chamber = PdbMutChamberPar(arm, station, octant, halfoctant, gap, cathode);
	  _chambers.insert( pair<int,PdbMutChamberPar>(chamber_ID, chamber) );
	}
      chamber.set_Landau_offset( Landau_offset );
      chamber.set_Landau_scale( Landau_scale );
    }
  file.close();
}

//____________________________________________________________
void MutChamberPar::load_Mathieson_parameters( string filename )
{
  ifstream file(filename.c_str());
  int arm,station,gap, octant;
  float Mathieson_cathode_coupling, Mathieson_anode_coupling;
  while ( file >> arm >> station >> octant >> gap >> Mathieson_cathode_coupling >> Mathieson_anode_coupling )
    {
      for (int ihalfoct=0; ihalfoct<2; ihalfoct++)
	for (int icath=0; icath<2; icath++)
	  {
	    int chamber_ID = PdbMutChamberPar::get_unique_ID(arm, station, octant, ihalfoct, gap, icath);
	    PdbMutChamberPar chamber;
	    if ( _chambers.find( chamber_ID ) != _chambers.end() )
	      chamber = _chambers.find( chamber_ID )->second;
	    else
	      {
		chamber = PdbMutChamberPar(arm, station, octant, ihalfoct, gap, icath);
		_chambers.insert( pair<int,PdbMutChamberPar>(chamber_ID, chamber) );
	      }
	    chamber.set_Mathieson_cathode_coupling( Mathieson_cathode_coupling );
	    chamber.set_Mathieson_anode_coupling( Mathieson_anode_coupling );
	  }
    }
  file.close();
}

//____________________________________________________________
void MutChamberPar::load_cathode_error( string filename )
{
  ifstream file(filename.c_str());
  int arm,station,gap,cathode;
  float cathode_error;
  while ( file >> arm >> station >> gap >> cathode >> cathode_error )
    {
      for (int ioct=0; ioct<8; ioct++)
	for (int ihalfoct=0; ihalfoct<2; ihalfoct++)
	  {
	    int chamber_ID = PdbMutChamberPar::get_unique_ID(arm, station, ioct, ihalfoct, gap, cathode);
	    PdbMutChamberPar chamber;
	    if ( _chambers.find( chamber_ID ) != _chambers.end() )
	      chamber = _chambers.find( chamber_ID )->second;
	    else
	      {
		chamber = PdbMutChamberPar(arm, station, ioct, ihalfoct, gap, cathode);
		_chambers.insert( pair<int,PdbMutChamberPar>(chamber_ID, chamber) );
	      }
	    chamber.set_cathode_error( cathode_error );
	  }
    }
  file.close();
}

