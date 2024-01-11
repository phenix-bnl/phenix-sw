/*!
  \file MutArm.C
  \brief Describes an Arm of the muon tracker system.
  \author Douglas Fields, Nicki Bruner, Hugo Pereira
  \version $Revision: 1.117 $
  \date $Date: 2010/09/14 03:12:22 $
*/


#include "MutAnodeMap.h"
#include "MutCalib.h"
#include "MutStrip.h"
#include "MutWire.h"
#include "TMutDatabaseCntrl.h"

#include <PHGeometry.h>
#include <PdbMutGeom.hh>
#include <PdbMutCalibStrip.hh>
#include <PdbCoordinate.hh>
#include <PdbCalBank.hh>
#include <PdbMutHVDisabled.hh>
#include <RunToTime.hh>
#include <RunNumberRanges.h>

#include <cassert>
#include <fstream>
#include <iostream>
#include <list>
#include <sstream>

using namespace std;
using namespace PHGeometry;
using namespace MUTGEOM;

//!nose cone min z (closest to the vertex) (cm)
double _noseConeZmin = 40.0;

//!nose cone max z (farest from the vertex) (cm)
double _noseConeZmax = 60.0;

//_________________________________________
// Real Constructor without DB access.
MutArm::MutArm(ArmNumber ArmNum):
  f_pMutStations( MUTGEOM::NumberOfStations, 0 ),
  _arm(ArmNum),
  _verbosity(MUTGEOM::NONE)
{

  cout << "MutArm::MutArm - arm: " << getArm() << endl;

  PISAPar = 0;
  switch (getArm())
  {

    case South:
    SetGlobalGeom(0,0,0,0,0,1);
    name = "MuTr Arm South";
    noseConeZmin =-(_noseConeZmin);
    noseConeZmax =-(_noseConeZmax);
    break;

    case North:
    SetGlobalGeom(0,0,0,0,0,-1);
    name = "MuTr Arm North";
    noseConeZmin =_noseConeZmin;
    noseConeZmax =_noseConeZmax;
    break;

  }

  readFromDB = false;

  // Create Stations and store pointers to them
  AddStations();

}

//_________________________________________
// Real Constructor using database.
MutArm::MutArm(ArmNumber ArmNum, PHTimeStamp &time_stamp):
  f_pMutStations( MUTGEOM::NumberOfStations, 0 ),
  _arm(ArmNum),
  _verbosity( MUTGEOM::NONE )

{
  PISAPar = 0;
  switch (getArm()) {
  case South:
    SetGlobalGeom(0,0,0,0,0,1);
    name = "MuTr Arm South";
    noseConeZmin =-(_noseConeZmin);
    noseConeZmax =-(_noseConeZmax);
    break;
  case North:
    SetGlobalGeom(0,0,0,0,0,-1);
    name = "MuTr Arm North";
    noseConeZmin = _noseConeZmin;
    noseConeZmax = _noseConeZmax;
    break;
  }

  _db_time = time_stamp;
  readFromDB = true;

  // Create Stations and store pointers to them
  AddStations();
}


//_________________________________________
// Real constructor for pisa response/reco.
MutArm::MutArm(ArmNumber ArmNum, MutPISAPara *PISAPara):
  f_pMutStations( MUTGEOM::NumberOfStations, 0 ),
  _arm(ArmNum)
{
  PISAPar = PISAPara;
  switch (getArm()) {
  case South:
    SetGlobalGeom(0,0,0,0,0,1);
    name = "MuTr Arm South (PISA)";
    noseConeZmin =-(_noseConeZmin);
    noseConeZmax =-(_noseConeZmax);
    break;
  case North:
    SetGlobalGeom(0,0,0,0,0,-1);
    name = "MuTr Arm North (PISA)";
    noseConeZmin = _noseConeZmin;
    noseConeZmax = _noseConeZmax;
    break;
  }

  readFromDB = false;

  AddStations();
}

//_________________________________________
MutArm::~MutArm()
{

  for(unsigned int i=0; i< f_pMutStations.size(); i++)
  if (f_pMutStations[i]) delete f_pMutStations[i];

}


//_________________________________________
void MutArm::AddStations()
{
  for (int j=0; j<NumberOfStations; j++) {
    f_pMutStations[j] = 0L;
    StationNumber k = StationNumber(j);
    MutStation* p = new MutStation(this,k);
    f_pMutStations[j] = p;
  }
}

//_________________________________________
void MutArm::RefreshStation(const StationNumber& StationNum)
{
  int j = int(StationNum);
  if (f_pMutStations[j]) {
    // The station object exists - delete it and create a new one.
    delete f_pMutStations[j];
    f_pMutStations[j] = new MutStation(this,StationNum);
  } else cout << "Cannot refresh non-existant station." << endl;
}


//_________________________________________
bool MutArm::updateOctantSurvey(
  PHTimeStamp &time_start,
  PHTimeStamp &time_stop,
  PdbBankID bank_id,
  const char *descrip, const char *file)
{
  // check input file
  ifstream in(file);
  if (!in.good()){
    cout << "MutArm::updateOctantSurvey - error opening file " << file << endl;
    return false;
  }

  // open database
  const string bank_name = (getArm() == South) ? "survey.mut.SouthOctants" : "survey.mut.NorthOctants";
  const string class_name = "PdbCoordinateBank";

  if(!update( class_name.c_str(), time_start, time_stop, bank_name.c_str(), bank_id, descrip ) )
  {
    cout<<"MutArm::updateOctantSurvey - failed to open database."<<endl;
    return false;
  }

  //bank length = number of stations * number of octants * 3 coordinates
  geometryBank->setLength(72);

  // parse file
  static const int bufsize = 256;
  char linebuf[bufsize];
  while( in.getline(linebuf,bufsize,'\n') )
  {

    int arm,station, octant;
    double x1, y1, z1; // x,y,z position of pin1
    double x2, y2, z2; // x,y,z position of pin2
    double x3, y3, z3; // x,y,z position of pin3

    istringstream stringbuf(linebuf);
    stringbuf
      >> arm >> station >> octant
      >> x1 >> y1 >> z1
      >> x2 >> y2 >> z2
      >> x3 >> y3 >> z3;


    // check stream
    if( stringbuf.rdstate() & ios::failbit )
    {
      cout << "MutArm::updateOctantSurvey - unable to parse line " << linebuf;
      continue;
    }

    if (arm != getArm())  continue;

    int count = station * 24 + octant * 3;
    static_cast<PdbCoordinate*>(&geometryBank->getEntry(count))->setAllParameters(x1, y1, z1);
    count++;

    static_cast<PdbCoordinate*>(&geometryBank->getEntry(count))->setAllParameters(x2, y2, z2);
    count++;

    static_cast<PdbCoordinate*>(&geometryBank->getEntry(count))->setAllParameters(x3, y3, z3);

  }

  if(!commit())
  {
    cout<<"MutArm::updateOctantSurvey - commit failed." << endl;
    return false;
  }

  return true;
}

//__________________________________________________________________
bool MutArm::updateGlobalAligConsts(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file)
{

  // check input file
  ifstream in(file);
  if (!in.good()){
    cout << "MutArm::updateGlobalAligConsts - error opening file " << file << endl;
    return false;
  }

  // parse the file
  list< PdbMutGeom > geom_objects;
  char line[512];
  while( !( in.rdstate() & ios::failbit ) )
  {

    // try parse the line
    int arm(0), station(0), octant(0);
    double ds(0), dr(0), dz(0);
    double pitch(0), roll(0), yaw(0), expansion(0);

    in.getline( line, 512, '\n' );
    istringstream line_stream( line );
    line_stream
        >> arm >> station >> octant
        >> ds >> dr >> dz
        >> roll >> pitch >> yaw
        >> expansion;

    // check stream
    if( line_stream.rdstate() & ios::failbit )
    {
      cout << "MutArm::updateGlobalAligConsts - unable to parse line " << line;
      continue;
    }


    // create objects
    {
      PdbMutGeom geom_object;
      geom_object.setArmStationOctant(arm,station,octant);
      geom_object.setGlobalPosition(ds,dr,dz);
      geom_object.setGlobalVector(roll,pitch,yaw);
      geom_objects.push_back( geom_object );
    }

    {
      PdbMutGeom geom_object;
      geom_object.setArmStationOctant(arm, station, octant);
      geom_object.setGlobalPosition(expansion,0.0,0.0);
      geom_objects.push_back( geom_object );
    }
  }

  cout<<"MutArm::updateGlobalAligConsts - created " << geom_objects.size() << " objects." << endl;

  const string bank_name = "mut.geom.globalalign";
  const string class_name = "PdbMutGeomBank";

  if(!update( class_name.c_str(), time_start, time_stop, bank_name.c_str(), bank_id, descrip ) )
  {
    cout<<"MutArm::updateGlobalAligConsts - failed to open database."<<endl;
    return false;
  }

  geometryBank->setLength( geom_objects.size() );
  cout<<"MutArm::updateGlobalAligConsts - database opened."<<endl;

  // update bank entries with stored objects
  unsigned int count = 0;
  for( list<PdbMutGeom>::const_iterator iter = geom_objects.begin(); iter != geom_objects.end(); iter++, count++ )
  {
    cout << "MutArm::updateGlobalAligConsts - writing entry " << count << endl;
     PdbMutGeom* entry = static_cast<PdbMutGeom*>( &geometryBank->getEntry(count) );
     *entry = *iter;
  }

  // commit
  if(!commit()) {
    cout<<"MutArm::updateGlobalAligConsts - commit failed." << endl;
    return false;
  }

  return true;
}

//_____________________________________________________________
bool MutArm::updateInternalAligConsts(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file)
{

  // check input file
  ifstream in(file);
  if (!in.good()){
    cout << "MutArm::updateInternalAligConsts - error opening file " << file << endl;
    return false;
  }

  // parse the file
  list< PdbMutGeom > geom_objects;
  char line[512];
  while( !( in.rdstate() & ios::failbit ) )
  {
    // read a line
    in.getline( line, 512, '\n' );
    if( !strlen( line ) ) continue;
    if( strncmp( line, "//", 2 ) == 0 ) continue;

    // try parse the line
    istringstream line_stream( line );
    InternalAligConst alig;
    line_stream >> alig;

    // check stream
    if( line_stream.rdstate() & ios::failbit )
    {
      cout << "MutArm::updateInternalAligConsts - unable to parse line " << line;
      continue;
    }

    // create objects
    {
      PdbMutGeom geom_object;
      geom_object.setArmStationOctant( alig.arm, alig.station, alig.octant );
      geom_object.setGlobalPosition( alig.ds, alig.dr, alig.dz );
      geom_object.setGlobalVector( alig.roll, alig.pitch, alig.yaw);
      geom_objects.push_back( geom_object );
    }

    {
      PdbMutGeom geom_object;
      geom_object.setArmStationOctant( alig.half, alig.gap, alig.plane);
      geom_object.setGlobalPosition( alig.dExpansion, 0.0,0.0);
      geom_objects.push_back( geom_object );
    }

  }

  cout<<"MutArm::updateInternalAligConsts - created " << geom_objects.size() << " objects." << endl;

  // try create bank.
  const string bank_name = "mut.geom.internalalign";
  const string class_name = "PdbMutGeomBank";

  if(!update( class_name.c_str(), time_start, time_stop, bank_name.c_str(), bank_id, descrip ) )
  {
    cout<<"MutArm::updateInternalAligConsts - failed to open database."<<endl;
    return false;
  }

  geometryBank->setLength( geom_objects.size() );
  cout<<"MutArm::updateInternalAligConsts - database opened."<<endl;

  // update bank entries with stored objects
  unsigned int count = 0;
  for( list<PdbMutGeom>::const_iterator iter = geom_objects.begin(); iter != geom_objects.end(); iter++, count++ )
  {
    cout << "MutArm::updateInternalAligConsts - writing entry " << count << endl;
     PdbMutGeom* entry = static_cast<PdbMutGeom*>( &geometryBank->getEntry(count) );
     *entry = *iter;
  }

  // commit
  if(!commit()) {
    cout<<"MutArm::updateInternalAligConsts - commit failed." << endl;
    return false;
  }

  return true;
}

//_________________________________________
void MutArm::printDCMChannelMap( string tag ) const
{

  cout << "MutArm::PrintDCMChannelMap - arm: " << _arm << endl;
  for( int i_station = 0; i_station < MUTGEOM::NumberOfStations; i_station++ )
  {
    assert( f_pMutStations[i_station] );
    f_pMutStations[i_station]->printDCMChannelMap( tag );
  }

}

//_________________________________________
void MutArm::updateDCMChannelMap( PHTimeStamp& begin, PHTimeStamp& end, string description, int station )
{

  cout << "MutArm::PrintDCMChannelMap - arm: " << _arm << endl;
  for( int i_station = 0; i_station < MUTGEOM::NumberOfStations; i_station++ )
  {

    // skip if station is set and do not match
    if( station >= 0 && station != i_station ) continue;

    assert( f_pMutStations[i_station] );
    f_pMutStations[i_station]->updateDCMChannelMap( begin, end, description );
  }

}

//_________________________________________
void MutArm::fetchLandauParameters( const char* file )
{

  ifstream s(file);
  if (!s.good()){
    cout << "MutArm::fetchLandauParameters - cannot access file " << file << endl;
    return;
  }

  cout << "MutArm::fetchLandauParameters - " << name << ": reading landau parameters using file " << file << endl;

  // create array
  LandauParametersArray array;

  // parse file
  static const int bufsize = 256;
  char linebuf[bufsize];
  while(s.getline(linebuf,bufsize,'\n'))
  {

    // skip empty or commented lines
    if( !strlen( linebuf ) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    istringstream stringbuf(linebuf);
    LandauParameters par;
    stringbuf >> par;
    if( stringbuf.rdstate() & ios::failbit )
    {
      cout << "MutArm::fetchLandauParameters - unable to parse line " << linebuf;
      continue;
    }

    array.push_back( par );

  }

  // pass parameters to relevant MutGap objects
  loadLandauParameters( array );

}

//_________________________________________
void MutArm::fetchLandauParameters( int run_number )
{

  cout << "MutArm::fetchLandauParameters - run: " << run_number << endl;
  if( run_number < static_cast<int>(BEGIN_OF_RUN8) ) { fetchLandauParameters_default(); }
  else if( run_number < static_cast<int>(BEGIN_OF_RUN9) ) { fetchLandauParameters_run8(); }
  else fetchLandauParameters_run9();

}

//_________________________________________
void MutArm::fetchLandauParameters_default( void )
{

  cout << "MutArm::fetchLandauParameters_default" << endl;
  LandauParametersArray array;

  // these are the landau parameters adjusted from run4 data, without rescaling of the calibration gains
  for( int octant=0; octant<MUTGEOM::NumberOfOctants; octant++ )
  {

    // indexes are arm,station,octant,gap
    array.push_back( LandauParameters( 0,0,octant,0, 13.2, 3.4 ));
    array.push_back( LandauParameters( 0,0,octant,1, 12.9, 3.5 ));
    array.push_back( LandauParameters( 0,0,octant,2, 12.8, 3.6 ));

    array.push_back( LandauParameters( 0,1,octant,0, 12.8, 3.2 ));
    array.push_back( LandauParameters( 0,1,octant,1, 13.3, 3.2 ));
    array.push_back( LandauParameters( 0,1,octant,2, 12.5, 3.1 ));

    array.push_back( LandauParameters( 0,2,octant,0, 12.4, 3.2 ));
    array.push_back( LandauParameters( 0,2,octant,1, 12.6, 3.4 ));

    array.push_back( LandauParameters( 1,0,octant,0, 10.2, 3.4 ));
    array.push_back( LandauParameters( 1,0,octant,1,  9.5, 2.9 ));
    array.push_back( LandauParameters( 1,0,octant,2,  6.9, 2.2 ));

    array.push_back( LandauParameters( 1,1,octant,0, 20.0, 6.0 ));
    array.push_back( LandauParameters( 1,1,octant,1, 21.0, 5.4 ));
    array.push_back( LandauParameters( 1,1,octant,2, 20.9, 5.2 ));

    array.push_back( LandauParameters( 1,2,octant,0, 17.5, 4.3 ));
    array.push_back( LandauParameters( 1,2,octant,1, 16.2, 6.1 ));
  }

  // pass parameters to relevant MutGap objects
  loadLandauParameters( array );

}

//_________________________________________
void MutArm::fetchLandauParameters_run8( void )
{
  // for now use the same set as the default parameters
  cout << "MutArm::fetchLandauParameters_run8" << endl;

  if( MutCalibSingleton::get().getUseGainCorrections() )
  {

    // these are the landau parameters adjusted from run9 data, after rescaling of the calibration gains
    // as evaluated by Hugo
    // indexes are arm,station,octant,gap

    cout << "MutArm::fetchLandauParameters_run8 - rescaled" << endl;

    LandauParametersArray array;
    array.push_back( LandauParameters(0, 0, 0, 0, 12.789706, 3.641622 ));
    array.push_back( LandauParameters(0, 0, 0, 1, 15.652680, 4.232113 ));
    array.push_back( LandauParameters(0, 0, 0, 2, 15.625954, 4.303080 ));
    array.push_back( LandauParameters(0, 1, 0, 0, 10.071286, 2.592143 ));
    array.push_back( LandauParameters(0, 1, 0, 1, 9.929152, 2.825432 ));
    array.push_back( LandauParameters(0, 1, 0, 2, 11.586411, 3.106742 ));
    array.push_back( LandauParameters(0, 2, 0, 0, 11.118484, 3.059683 ));
    array.push_back( LandauParameters(0, 2, 0, 1, 10.469812, 2.848764 ));
    array.push_back( LandauParameters(0, 0, 1, 0, 13.138512, 3.680331 ));
    array.push_back( LandauParameters(0, 0, 1, 1, 13.989047, 3.675235 ));
    array.push_back( LandauParameters(0, 0, 1, 2, 13.741720, 3.774732 ));
    array.push_back( LandauParameters(0, 1, 1, 0, 11.392157, 2.945833 ));
    array.push_back( LandauParameters(0, 1, 1, 1, 9.792877, 2.479409 ));
    array.push_back( LandauParameters(0, 1, 1, 2, 10.766302, 2.912740 ));
    array.push_back( LandauParameters(0, 2, 1, 0, 10.333333, 2.920081 ));
    array.push_back( LandauParameters(0, 2, 1, 1, 9.826630, 2.706575 ));
    array.push_back( LandauParameters(0, 0, 2, 0, 14.304601, 3.951008 ));
    array.push_back( LandauParameters(0, 0, 2, 1, 13.891431, 3.450077 ));
    array.push_back( LandauParameters(0, 0, 2, 2, 14.173246, 3.593388 ));
    array.push_back( LandauParameters(0, 1, 2, 0, 10.087159, 2.680538 ));
    array.push_back( LandauParameters(0, 1, 2, 1, 11.764186, 3.043069 ));
    array.push_back( LandauParameters(0, 1, 2, 2, 10.048232, 2.827745 ));
    array.push_back( LandauParameters(0, 2, 2, 0, 10.654569, 2.847482 ));
    array.push_back( LandauParameters(0, 2, 2, 1, 12.157234, 3.272377 ));
    array.push_back( LandauParameters(0, 0, 3, 0, 13.765773, 3.768482 ));
    array.push_back( LandauParameters(0, 0, 3, 1, 13.472163, 3.756211 ));
    array.push_back( LandauParameters(0, 0, 3, 2, 13.267823, 3.677710 ));
    array.push_back( LandauParameters(0, 1, 3, 0, 11.521567, 3.031021 ));
    array.push_back( LandauParameters(0, 1, 3, 1, 10.281942, 2.796958 ));
    array.push_back( LandauParameters(0, 1, 3, 2, 11.363533, 2.871833 ));
    array.push_back( LandauParameters(0, 2, 3, 0, 13.799397, 3.543155 ));
    array.push_back( LandauParameters(0, 2, 3, 1, 12.610845, 3.231601 ));
    array.push_back( LandauParameters(0, 0, 4, 0, 16.321887, 4.469156 ));
    array.push_back( LandauParameters(0, 0, 4, 1, 12.536565, 3.383669 ));
    array.push_back( LandauParameters(0, 0, 4, 2, 14.477018, 3.779170 ));
    array.push_back( LandauParameters(0, 1, 4, 0, 10.755851, 2.839674 ));
    array.push_back( LandauParameters(0, 1, 4, 1, 11.710942, 3.093162 ));
    array.push_back( LandauParameters(0, 1, 4, 2, 9.681517, 2.590825 ));
    array.push_back( LandauParameters(0, 2, 4, 0, 12.944173, 3.259120 ));
    array.push_back( LandauParameters(0, 2, 4, 1, 13.818668, 3.506094 ));
    array.push_back( LandauParameters(0, 0, 5, 0, 14.334582, 3.710859 ));
    array.push_back( LandauParameters(0, 0, 5, 1, 12.858723, 3.641623 ));
    array.push_back( LandauParameters(0, 0, 5, 2, 12.599665, 3.694771 ));
    array.push_back( LandauParameters(0, 1, 5, 0, 11.053827, 2.655756 ));
    array.push_back( LandauParameters(0, 1, 5, 1, 9.409667, 2.492309 ));
    array.push_back( LandauParameters(0, 1, 5, 2, 10.093262, 2.758253 ));
    array.push_back( LandauParameters(0, 2, 5, 0, 12.100158, 3.024603 ));
    array.push_back( LandauParameters(0, 2, 5, 1, 11.584289, 3.113810 ));
    array.push_back( LandauParameters(0, 0, 6, 0, 13.989127, 3.606432 ));
    array.push_back( LandauParameters(0, 0, 6, 1, 14.764455, 3.805051 ));
    array.push_back( LandauParameters(0, 0, 6, 2, 13.277182, 3.530796 ));
    array.push_back( LandauParameters(0, 1, 6, 0, 12.228676, 3.187854 ));
    array.push_back( LandauParameters(0, 1, 6, 1, 11.618765, 3.000103 ));
    array.push_back( LandauParameters(0, 1, 6, 2, 11.134370, 2.891117 ));
    array.push_back( LandauParameters(0, 2, 6, 0, 11.711814, 3.040727 ));
    array.push_back( LandauParameters(0, 2, 6, 1, 11.580563, 3.031843 ));
    array.push_back( LandauParameters(0, 0, 7, 0, 13.406732, 3.625313 ));
    array.push_back( LandauParameters(0, 0, 7, 1, 16.234788, 4.228739 ));
    array.push_back( LandauParameters(0, 0, 7, 2, 14.058055, 3.809524 ));
    array.push_back( LandauParameters(0, 1, 7, 0, 10.809898, 2.925072 ));
    array.push_back( LandauParameters(0, 1, 7, 1, 9.983036, 2.636827 ));
    array.push_back( LandauParameters(0, 1, 7, 2, 11.359815, 2.855380 ));
    array.push_back( LandauParameters(0, 2, 7, 0, 12.397025, 3.179713 ));
    array.push_back( LandauParameters(0, 2, 7, 1, 8.956752, 2.432533 ));
    array.push_back( LandauParameters(1, 0, 0, 0, 6.482776, 1.854124 ));
    array.push_back( LandauParameters(1, 0, 0, 1, 5.987269, 1.650917 ));
    array.push_back( LandauParameters(1, 0, 0, 2, 5.246510, 1.378792 ));
    array.push_back( LandauParameters(1, 1, 0, 0, 16.330130, 4.786521 ));
    array.push_back( LandauParameters(1, 1, 0, 1, 14.569876, 4.167117 ));
    array.push_back( LandauParameters(1, 1, 0, 2, 14.906318, 4.267262 ));
    array.push_back( LandauParameters(1, 2, 0, 0, 7.721394, 2.126870 ));
    array.push_back( LandauParameters(1, 2, 0, 1, 7.150834, 1.889136 ));
    array.push_back( LandauParameters(1, 0, 1, 0, 6.688568, 1.823985 ));
    array.push_back( LandauParameters(1, 0, 1, 1, 5.822613, 1.616535 ));
    array.push_back( LandauParameters(1, 0, 1, 2, 5.672569, 1.581823 ));
    array.push_back( LandauParameters(1, 1, 1, 0, 20.223265, 5.221296 ));
    array.push_back( LandauParameters(1, 1, 1, 1, 17.350353, 5.089011 ));
    array.push_back( LandauParameters(1, 1, 1, 2, 18.882073, 4.692294 ));
    array.push_back( LandauParameters(1, 2, 1, 0, 6.511652, 1.798245 ));
    array.push_back( LandauParameters(1, 2, 1, 1, 6.566920, 1.780788 ));
    array.push_back( LandauParameters(1, 0, 2, 0, 5.815147, 1.754323 ));
    array.push_back( LandauParameters(1, 0, 2, 1, 5.699202, 1.657219 ));
    array.push_back( LandauParameters(1, 0, 2, 2, 4.927832, 1.343700 ));
    array.push_back( LandauParameters(1, 1, 2, 0, 16.820433, 4.367861 ));
    array.push_back( LandauParameters(1, 1, 2, 1, 15.944241, 4.306254 ));
    array.push_back( LandauParameters(1, 1, 2, 2, 16.430947, 4.583032 ));
    array.push_back( LandauParameters(1, 2, 2, 0, 6.469047, 1.633627 ));
    array.push_back( LandauParameters(1, 2, 2, 1, 9.480115, 2.886850 ));
    array.push_back( LandauParameters(1, 0, 3, 0, 6.574961, 1.844698 ));
    array.push_back( LandauParameters(1, 0, 3, 1, 4.849685, 1.313709 ));
    array.push_back( LandauParameters(1, 0, 3, 2, 5.230522, 1.392123 ));
    array.push_back( LandauParameters(1, 1, 3, 0, 20.874866, 6.200461 ));
    array.push_back( LandauParameters(1, 1, 3, 1, 16.742540, 4.341499 ));
    array.push_back( LandauParameters(1, 1, 3, 2, 19.831292, 5.086072 ));
    array.push_back( LandauParameters(1, 2, 3, 0, 6.917160, 1.716560 ));
    array.push_back( LandauParameters(1, 2, 3, 1, 7.153486, 1.880638 ));
    array.push_back( LandauParameters(1, 0, 4, 0, 5.375381, 1.412670 ));
    array.push_back( LandauParameters(1, 0, 4, 1, 4.401653, 1.165014 ));
    array.push_back( LandauParameters(1, 0, 4, 2, 4.535923, 1.056240 ));
    array.push_back( LandauParameters(1, 1, 4, 0, 20.444254, 5.501307 ));
    array.push_back( LandauParameters(1, 1, 4, 1, 16.435912, 4.674313 ));
    array.push_back( LandauParameters(1, 1, 4, 2, 16.471219, 4.637516 ));
    array.push_back( LandauParameters(1, 2, 4, 0, 6.720095, 1.703719 ));
    array.push_back( LandauParameters(1, 2, 4, 1, 8.047010, 2.160347 ));
    array.push_back( LandauParameters(1, 0, 5, 0, 6.097525, 1.684778 ));
    array.push_back( LandauParameters(1, 0, 5, 1, 5.431365, 1.538813 ));
    array.push_back( LandauParameters(1, 0, 5, 2, 5.206251, 1.484340 ));
    array.push_back( LandauParameters(1, 1, 5, 0, 19.282498, 4.709798 ));
    array.push_back( LandauParameters(1, 1, 5, 1, 16.499576, 4.326056 ));
    array.push_back( LandauParameters(1, 1, 5, 2, 22.278362, 5.832343 ));
    array.push_back( LandauParameters(1, 2, 5, 0, 6.999188, 1.743460 ));
    array.push_back( LandauParameters(1, 2, 5, 1, 6.742415, 1.926113 ));
    array.push_back( LandauParameters(1, 0, 6, 0, 5.670006, 1.551314 ));
    array.push_back( LandauParameters(1, 0, 6, 1, 6.634217, 1.829688 ));
    array.push_back( LandauParameters(1, 0, 6, 2, 5.150253, 1.436725 ));
    array.push_back( LandauParameters(1, 1, 6, 0, 17.357796, 4.734400 ));
    array.push_back( LandauParameters(1, 1, 6, 1, 16.848524, 4.533510 ));
    array.push_back( LandauParameters(1, 1, 6, 2, 16.236036, 3.895569 ));
    array.push_back( LandauParameters(1, 2, 6, 0, 6.289828, 1.663063 ));
    array.push_back( LandauParameters(1, 2, 6, 1, 6.102017, 1.774422 ));
    array.push_back( LandauParameters(1, 0, 7, 0, 5.960207, 1.699218 ));
    array.push_back( LandauParameters(1, 0, 7, 1, 5.872898, 1.613406 ));
    array.push_back( LandauParameters(1, 0, 7, 2, 5.543638, 1.605043 ));
    array.push_back( LandauParameters(1, 1, 7, 0, 18.826719, 5.553550 ));
    array.push_back( LandauParameters(1, 1, 7, 1, 16.363806, 3.982330 ));
    array.push_back( LandauParameters(1, 1, 7, 2, 18.173913, 4.922471 ));
    array.push_back( LandauParameters(1, 2, 7, 0, 6.304194, 1.719292 ));
    array.push_back( LandauParameters(1, 2, 7, 1, 7.097450, 1.880754 ));
    loadLandauParameters( array );

  } else {

    // in unscaled mode, we use the default, established for Run4
    cout << "MutArm::fetchLandauParameters_run8 - default" << endl;
    fetchLandauParameters_default();

  }
}

//_________________________________________
void MutArm::fetchLandauParameters_run9( void )
{

  LandauParametersArray array;
  if( MutCalibSingleton::get().getUseGainCorrections() )
  {

    // these are the landau parameters adjusted from run9 data, after rescaling of the calibration gains
    // as evaluated by Hugo
    // indexes are arm,station,octant,gap
    cout << "MutArm::fetchLandauParameters_run9 - rescaled" << endl;

    array.push_back( LandauParameters( 0, 0, 0, 0,  15.8154, 4.42254 ));
    array.push_back( LandauParameters( 0, 0, 0, 1,  16.7534, 4.75557 ));
    array.push_back( LandauParameters( 0, 0, 0, 2,  15.7152, 4.34127 ));
    array.push_back( LandauParameters( 0, 0, 1, 0,  14.6017, 4.05053 ));
    array.push_back( LandauParameters( 0, 0, 1, 1,  16.9175, 4.21648 ));
    array.push_back( LandauParameters( 0, 0, 1, 2,  27.2072, 6.99487 ));
    array.push_back( LandauParameters( 0, 0, 2, 0,  24.3196, 6.79225 ));
    array.push_back( LandauParameters( 0, 0, 2, 1,  22.6728, 5.81333 ));
    array.push_back( LandauParameters( 0, 0, 2, 2,  16.766, 4.17223 ));
    array.push_back( LandauParameters( 0, 0, 3, 0,  15.8018, 4.2732 ));
    array.push_back( LandauParameters( 0, 0, 3, 1,  15.7772, 4.24515 ));
    array.push_back( LandauParameters( 0, 0, 3, 2,  23.06, 6.60686 ));
    array.push_back( LandauParameters( 0, 0, 4, 0,  18.1119, 4.57967 ));
    array.push_back( LandauParameters( 0, 0, 4, 1,  13.2347, 3.40346 ));
    array.push_back( LandauParameters( 0, 0, 4, 2,  16.6652, 4.22174 ));
    array.push_back( LandauParameters( 0, 0, 5, 0,  23.4675, 7.12197 ));
    array.push_back( LandauParameters( 0, 0, 5, 1,  23.5713, 6.8529 ));
    array.push_back( LandauParameters( 0, 0, 5, 2,  22.618, 6.21299 ));
    array.push_back( LandauParameters( 0, 0, 6, 0,  14.5851, 4.36369 ));
    array.push_back( LandauParameters( 0, 0, 6, 1,  16.3401, 4.73431 ));
    array.push_back( LandauParameters( 0, 0, 6, 2,  24.2803, 6.32048 ));
    array.push_back( LandauParameters( 0, 0, 7, 0,  13.5042, 3.74764 ));
    array.push_back( LandauParameters( 0, 0, 7, 1,  16.1735, 4.22764 ));
    array.push_back( LandauParameters( 0, 0, 7, 2,  16.7344, 4.15638 ));
    array.push_back( LandauParameters( 0, 1, 0, 0,  17.4928, 5.64509 ));
    array.push_back( LandauParameters( 0, 1, 0, 1,  19.5613, 6.15632 ));
    array.push_back( LandauParameters( 0, 1, 0, 2,  12.4801, 3.56267 ));
    array.push_back( LandauParameters( 0, 1, 1, 0,  12.4432, 3.61204 ));
    array.push_back( LandauParameters( 0, 1, 1, 1,  11.3266, 3.20808 ));
    array.push_back( LandauParameters( 0, 1, 1, 2,  12.2847, 3.53138 ));
    array.push_back( LandauParameters( 0, 1, 2, 0,  16.1489, 4.85554 ));
    array.push_back( LandauParameters( 0, 1, 2, 1,  22.9038, 7.16278 ));
    array.push_back( LandauParameters( 0, 1, 2, 2,  16.9147, 5.20632 ));
    array.push_back( LandauParameters( 0, 1, 3, 0,  20.7415, 6.00111 ));
    array.push_back( LandauParameters( 0, 1, 3, 1,  18.6341, 5.44181 ));
    array.push_back( LandauParameters( 0, 1, 3, 2,  19.4439, 5.66316 ));
    array.push_back( LandauParameters( 0, 1, 4, 0,  10.9556, 3.3708 ));
    array.push_back( LandauParameters( 0, 1, 4, 1,  14.2859, 3.94712 ));
    array.push_back( LandauParameters( 0, 1, 4, 2,  10.6254, 3.12785 ));
    array.push_back( LandauParameters( 0, 1, 5, 0,  11.819, 3.28046 ));
    array.push_back( LandauParameters( 0, 1, 5, 1,  17.6785, 5.14893 ));
    array.push_back( LandauParameters( 0, 1, 5, 2,  18.6014, 5.19953 ));
    array.push_back( LandauParameters( 0, 1, 6, 0,  13.9201, 3.36034 ));
    array.push_back( LandauParameters( 0, 1, 6, 1,  14.6557, 4.01976 ));
    array.push_back( LandauParameters( 0, 1, 6, 2,  12.1832, 3.23963 ));
    array.push_back( LandauParameters( 0, 1, 7, 0,  11.4035, 3.31399 ));
    array.push_back( LandauParameters( 0, 1, 7, 1,  11.4628, 3.20777 ));
    array.push_back( LandauParameters( 0, 1, 7, 2,  13.1279, 3.59887 ));
    array.push_back( LandauParameters( 0, 2, 0, 0,  16.3196, 4.32745 ));
    array.push_back( LandauParameters( 0, 2, 0, 1,  15.5892, 4.3847 ));
    array.push_back( LandauParameters( 0, 2, 1, 0,  12.6736, 3.42982 ));
    array.push_back( LandauParameters( 0, 2, 1, 1,  12.4799, 3.43772 ));
    array.push_back( LandauParameters( 0, 2, 2, 0,  14.291, 3.91123 ));
    array.push_back( LandauParameters( 0, 2, 2, 1,  16.406, 4.5286 ));
    array.push_back( LandauParameters( 0, 2, 3, 0,  15.3476, 4.10656 ));
    array.push_back( LandauParameters( 0, 2, 3, 1,  13.1351, 3.58738 ));
    array.push_back( LandauParameters( 0, 2, 4, 0,  13.4911, 3.38784 ));
    array.push_back( LandauParameters( 0, 2, 4, 1,  14.8087, 3.95976 ));
    array.push_back( LandauParameters( 0, 2, 5, 0,  11.5936, 3.16543 ));
    array.push_back( LandauParameters( 0, 2, 5, 1,  12.4185, 3.36805 ));
    array.push_back( LandauParameters( 0, 2, 6, 0,  12.9897, 3.68734 ));
    array.push_back( LandauParameters( 0, 2, 6, 1,  12.1975, 3.52206 ));
    array.push_back( LandauParameters( 0, 2, 7, 0,  12.2604, 3.32858 ));
    array.push_back( LandauParameters( 0, 2, 7, 1,  8.9923, 2.52969 ));
    array.push_back( LandauParameters( 1, 0, 0, 0,  7.00194, 2.07097 ));
    array.push_back( LandauParameters( 1, 0, 0, 1,  6.88421, 2.09409 ));
    array.push_back( LandauParameters( 1, 0, 0, 2,  9.33565, 2.76089 ));
    array.push_back( LandauParameters( 1, 0, 1, 0,  7.43533, 2.09963 ));
    array.push_back( LandauParameters( 1, 0, 1, 1,  7.18595, 2.0675 ));
    array.push_back( LandauParameters( 1, 0, 1, 2,  6.3248, 1.89322 ));
    array.push_back( LandauParameters( 1, 0, 2, 0,  6.50942, 1.94778 ));
    array.push_back( LandauParameters( 1, 0, 2, 1,  8.61995, 2.57949 ));
    array.push_back( LandauParameters( 1, 0, 2, 2,  10.7783, 3.26803 ));
    array.push_back( LandauParameters( 1, 0, 3, 0,  7.32543, 2.03833 ));
    array.push_back( LandauParameters( 1, 0, 3, 1,  9.35085, 2.62464 ));
    array.push_back( LandauParameters( 1, 0, 3, 2,  10.1337, 2.77395 ));
    array.push_back( LandauParameters( 1, 0, 4, 0,  10.0133, 2.91669 ));
    array.push_back( LandauParameters( 1, 0, 4, 1,  10.4694, 2.94222 ));
    array.push_back( LandauParameters( 1, 0, 4, 2,  9.83315, 2.8343 ));
    array.push_back( LandauParameters( 1, 0, 5, 0,  7.10062, 2.0782 ));
    array.push_back( LandauParameters( 1, 0, 5, 1,  10.4934, 2.95369 ));
    array.push_back( LandauParameters( 1, 0, 5, 2,  10.3093, 2.88924 ));
    array.push_back( LandauParameters( 1, 0, 6, 0,  6.31964, 1.82079 ));
    array.push_back( LandauParameters( 1, 0, 6, 1,  7.47046, 2.08994 ));
    array.push_back( LandauParameters( 1, 0, 6, 2,  10.5612, 3.00055 ));
    array.push_back( LandauParameters( 1, 0, 7, 0,  7.03719, 2.14926 ));
    array.push_back( LandauParameters( 1, 0, 7, 1,  9.73703, 2.67179 ));
    array.push_back( LandauParameters( 1, 0, 7, 2,  10.1145, 3.03664 ));
    array.push_back( LandauParameters( 1, 1, 0, 0,  26.7559, 9.42752 ));
    array.push_back( LandauParameters( 1, 1, 0, 1,  24.9203, 7.51858 ));
    array.push_back( LandauParameters( 1, 1, 0, 2,  22.373, 7.49984 ));
    array.push_back( LandauParameters( 1, 1, 1, 0,  33.7452, 9.87459 ));
    array.push_back( LandauParameters( 1, 1, 1, 1,  18.3195, 5.1956 ));
    array.push_back( LandauParameters( 1, 1, 1, 2,  18.7276, 5.05043 ));
    array.push_back( LandauParameters( 1, 1, 2, 0,  25.026, 7.24899 ));
    array.push_back( LandauParameters( 1, 1, 2, 1,  24.6411, 7.83146 ));
    array.push_back( LandauParameters( 1, 1, 2, 2,  25.7401, 8.47552 ));
    array.push_back( LandauParameters( 1, 1, 3, 0,  32.9466, 12.2466 ));
    array.push_back( LandauParameters( 1, 1, 3, 1,  17.1678, 5.25139 ));
    array.push_back( LandauParameters( 1, 1, 3, 2,  18.9509, 5.51411 ));
    array.push_back( LandauParameters( 1, 1, 4, 0,  33.85, 10.0585 ));
    array.push_back( LandauParameters( 1, 1, 4, 1,  24.6481, 8.21269 ));
    array.push_back( LandauParameters( 1, 1, 4, 2,  24.4362, 6.36962 ));
    array.push_back( LandauParameters( 1, 1, 5, 0,  29.6954, 8.77253 ));
    array.push_back( LandauParameters( 1, 1, 5, 1,  26.8875, 7.53009 ));
    array.push_back( LandauParameters( 1, 1, 5, 2,  36.5894, 10.4662 ));
    array.push_back( LandauParameters( 1, 1, 6, 0,  18.3871, 5.48031 ));
    array.push_back( LandauParameters( 1, 1, 6, 1,  17.7445, 4.97066 ));
    array.push_back( LandauParameters( 1, 1, 6, 2,  25.1817, 6.88355 ));
    array.push_back( LandauParameters( 1, 1, 7, 0,  35.1081, 12.7015 ));
    array.push_back( LandauParameters( 1, 1, 7, 1,  17.0684, 4.99706 ));
    array.push_back( LandauParameters( 1, 1, 7, 2,  18.9981, 5.80854 ));
    array.push_back( LandauParameters( 1, 2, 0, 0,  28.0726, 8.17725 ));
    array.push_back( LandauParameters( 1, 2, 0, 1,  28.4284, 7.54236 ));
    array.push_back( LandauParameters( 1, 2, 1, 0,  16.8288, 4.59307 ));
    array.push_back( LandauParameters( 1, 2, 1, 1,  19.2598, 5.29199 ));
    array.push_back( LandauParameters( 1, 2, 2, 0,  26.7949, 7.02587 ));
    array.push_back( LandauParameters( 1, 2, 2, 1,  32.956, 8.53942 ));
    array.push_back( LandauParameters( 1, 2, 3, 0,  20.501, 5.51731 ));
    array.push_back( LandauParameters( 1, 2, 3, 1,  19.7658, 5.59146 ));
    array.push_back( LandauParameters( 1, 2, 4, 0,  26.3219, 7.13387 ));
    array.push_back( LandauParameters( 1, 2, 4, 1,  31.1137, 8.2486 ));
    array.push_back( LandauParameters( 1, 2, 5, 0,  27.7034, 7.66248 ));
    array.push_back( LandauParameters( 1, 2, 5, 1,  30.6418, 8.66427 ));
    array.push_back( LandauParameters( 1, 2, 6, 0,  20.3772, 5.42036 ));
    array.push_back( LandauParameters( 1, 2, 6, 1,  19.2113, 5.34676 ));
    array.push_back( LandauParameters( 1, 2, 7, 0,  16.0681, 4.74042 ));
    array.push_back( LandauParameters( 1, 2, 7, 1,  17.9634, 4.81617 ));

  } else {

    cout << "MutArm::fetchLandauParameters_run9 - raw" << endl;
    // these are the landau parameters adjusted from run9 data, without rescaling of the calibration gains
    // as evaluated by Todd Kempel
    // indexes are arm,station,octant,gap
    // south station 0
    array.push_back( LandauParameters( 0,0,0,0, 14.5, 3.8 ));
    array.push_back( LandauParameters( 0,0,1,0, 16.4, 4.3 ));
    array.push_back( LandauParameters( 0,0,2,0, 23.1, 5.9 ));
    array.push_back( LandauParameters( 0,0,3,0, 17.0, 4.2 ));
    array.push_back( LandauParameters( 0,0,4,0, 16.7, 4.1 ));
    array.push_back( LandauParameters( 0,0,5,0, 23.6, 6.1*0.8 ));
    array.push_back( LandauParameters( 0,0,6,0, 15.4, 4.4 ));
    array.push_back( LandauParameters( 0,0,7,0, 12.8*1.2, 3.7 ));

    array.push_back( LandauParameters( 0,0,0,1, 15.7*1.2, 4.1*1.2 ));
    array.push_back( LandauParameters( 0,0,1,1, 16.2, 4.6*0.8 ));
    array.push_back( LandauParameters( 0,0,2,1, 22.3, 6.4*0.8 ));
    array.push_back( LandauParameters( 0,0,3,1, 17.5*0.8, 4.1 ));
    array.push_back( LandauParameters( 0,0,4,1, 16.9*0.8, 4.4*0.6 ));
    array.push_back( LandauParameters( 0,0,5,1, 23.8, 6.4*0.8 ));
    array.push_back( LandauParameters( 0,0,6,1, 13.9*1.2, 3.8*1.2 ));
    array.push_back( LandauParameters( 0,0,7,1, 13.8*1.2, 3.2*1.2 ));

    array.push_back( LandauParameters( 0,0,0,2, 16.7, 3.7 ));
    array.push_back( LandauParameters( 0,0,1,2, 17.2*1.6, 4.8*1.4 ));
    array.push_back( LandauParameters( 0,0,2,2, 27.3*0.6, 7.7*0.6 ));
    array.push_back( LandauParameters( 0,0,3,2, 13.4*1.8, 4.1*1.8 ));
    array.push_back( LandauParameters( 0,0,4,2, 15.1*1.2, 3.6*1.4 ));
    array.push_back( LandauParameters( 0,0,5,2, 21.0*1.2, 6.3 ));
    array.push_back( LandauParameters( 0,0,6,2, 15.1*1.8, 3.7*1.8 ));
    array.push_back( LandauParameters( 0,0,7,2, 17.6, 4.5 ));

    // south station 1
    array.push_back( LandauParameters( 0,1,0,0, 14.4*1.2, 3.6*1.2 ));
    array.push_back( LandauParameters( 0,1,1,0, 14.8*0.8, 3.8*0.8 ));
    array.push_back( LandauParameters( 0,1,2,0, 31.4*0.8, 6.5 ));
    array.push_back( LandauParameters( 0,1,3,0, 12.5*1.6, 3.2*1.8 ));
    array.push_back( LandauParameters( 0,1,4,0, 12.2*0.8, 2.9 ));
    array.push_back( LandauParameters( 0,1,5,0, 18.6*0.6, 4.2*0.8 ));
    array.push_back( LandauParameters( 0,1,6,0, 15.0, 4.0 ));
    array.push_back( LandauParameters( 0,1,7,0, 15.0*0.8, 3.8*0.8 ));

    array.push_back( LandauParameters( 0,1,0,1, 14.4*1.4, 3.8*1.4 ));
    array.push_back( LandauParameters( 0,1,1,1, 26.5*0.4, 7.3*0.4 ));
    array.push_back( LandauParameters( 0,1,2,1, 16.8*1.4, 4.2*1.4 ));
    array.push_back( LandauParameters( 0,1,3,1, 22.6, 5.8 ));
    array.push_back( LandauParameters( 0,1,4,1, 14.4, 3.9 ));
    array.push_back( LandauParameters( 0,1,5,1, 25.2*0.8, 6.8*0.8 ));
    array.push_back( LandauParameters( 0,1,6,1, 22.7*0.6, 6.4*0.6 ));
    array.push_back( LandauParameters( 0,1,7,1, 15.4*0.8, 3.8 ));

    array.push_back( LandauParameters( 0,1,0,2, 13.3, 3.6*0.8 ));
    array.push_back( LandauParameters( 0,1,1,2, 23.8*0.6, 6.5*0.6 ));
    array.push_back( LandauParameters( 0,1,2,2, 14.4*1.2, 3.6*1.4 ));
    array.push_back( LandauParameters( 0,1,3,2, 19.0, 4.8*1.2 ));
    array.push_back( LandauParameters( 0,1,4,2, 15.1*0.8, 4.1 ));
    array.push_back( LandauParameters( 0,1,5,2, 22.6, 5.7 ));
    array.push_back( LandauParameters( 0,1,6,2, 23.0*0.6, 7.6*0.4 ));
    array.push_back( LandauParameters( 0,1,7,2, 14.1, 3.8 ));

    // south station 2
    array.push_back( LandauParameters( 0,2,0,0, 14.9, 4.2 ));
    array.push_back( LandauParameters( 0,2,1,0,  9.9*1.2, 2.2*1.4 ));
    array.push_back( LandauParameters( 0,2,2,0, 12.0*1.2, 3.6 ));
    array.push_back( LandauParameters( 0,2,3,0, 19.9*0.8, 4.6*0.6 ));
    array.push_back( LandauParameters( 0,2,4,0, 12.0*1.4, 2.7*1.4 ));
    array.push_back( LandauParameters( 0,2,5,0, 10.1*1.2, 2.3*1.4 ));
    array.push_back( LandauParameters( 0,2,6,0, 12.6, 3.3 ));
    array.push_back( LandauParameters( 0,2,7,0, 11.5, 3.0 ));

    array.push_back( LandauParameters( 0,2,0,1, 14.9, 3.4 ));
    array.push_back( LandauParameters( 0,2,1,1,  9.4*1.2, 2.4*1.2 ));
    array.push_back( LandauParameters( 0,2,2,1, 13.9*1.2, 3.2*1.4 ));
    array.push_back( LandauParameters( 0,2,3,1, 16.8*0.8, 4.7*0.8 ));
    array.push_back( LandauParameters( 0,2,4,1, 10.1*1.6, 2.5*1.4 ));
    array.push_back( LandauParameters( 0,2,5,1, 10.1*1.2, 2.5*1.4 ));
    array.push_back( LandauParameters( 0,2,6,1, 12.6, 2.9*1.2 ));
    array.push_back( LandauParameters( 0,2,7,1, 10.3*0.8, 2.2 ));

    // north station 0
    array.push_back( LandauParameters( 1,0,0,0, 13.6, 3.4*1.2 ));
    array.push_back( LandauParameters( 1,0,1,0,  8.0, 2.9*0.8 ));
    array.push_back( LandauParameters( 1,0,2,0, 17.8, 4.9*0.8 ));
    array.push_back( LandauParameters( 1,0,3,0, 17.4, 3.9 ));
    array.push_back( LandauParameters( 1,0,4,0, 17.2, 3.0*1.2 ));
    array.push_back( LandauParameters( 1,0,5,0, 14.0, 2.6 ));
    array.push_back( LandauParameters( 1,0,6,0, 12.0*0.8, 3.2 ));
    array.push_back( LandauParameters( 1,0,7,0, 10.8, 2.7*1.2 ));

    array.push_back( LandauParameters( 1,0,0,1, 12.6, 3.0 ));
    array.push_back( LandauParameters( 1,0,1,1, 11.2*1.2, 2.9*1.2 ));
    array.push_back( LandauParameters( 1,0,2,1, 19.4, 4.9 ));
    array.push_back( LandauParameters( 1,0,3,1, 19.2, 4.9*1.4 ));
    array.push_back( LandauParameters( 1,0,4,1, 15.5, 3.1 ));
    array.push_back( LandauParameters( 1,0,5,1, 16.8, 3.6*1.2 ));
    array.push_back( LandauParameters( 1,0,6,1, 12.0, 2.4*1.4 ));
    array.push_back( LandauParameters( 1,0,7,1,  9.2, 2.4*0.8 ));

    array.push_back( LandauParameters( 1,0,0,2, 17.6, 3.5 ));
    array.push_back( LandauParameters( 1,0,1,2, 11.0, 2.8 ));
    array.push_back( LandauParameters( 1,0,2,2,  9.6*1.2, 3.1*1.2 ));
    array.push_back( LandauParameters( 1,0,3,2,  9.7, 2.4*0.8 ));
    array.push_back( LandauParameters( 1,0,4,2, 16.7, 4.0*1.2 ));
    array.push_back( LandauParameters( 1,0,5,2,  9.6, 4.0 ));
    array.push_back( LandauParameters( 1,0,6,2, 16.7, 4.0 ));
    array.push_back( LandauParameters( 1,0,7,2, 14.5, 4.0*0.8 ));

    // north station 1
    array.push_back( LandauParameters( 1,1,0,0, 15.5*1.6, 6.0 ));
    array.push_back( LandauParameters( 1,1,1,0, 18.6*1.8, 8.1 ));
    array.push_back( LandauParameters( 1,1,2,0, 18.2*1.4, 3.0*1.6 ));
    array.push_back( LandauParameters( 1,1,3,0,  6.3*0.4,13.3*1.6 ));
    array.push_back( LandauParameters( 1,1,4,0, 47.1*1.4,16.6*0.8 ));
    array.push_back( LandauParameters( 1,1,5,0, 19.0*1.6, 4.6*1.6 ));
    array.push_back( LandauParameters( 1,1,6,0, 11.8*1.6, 2.3*1.8 ));
    array.push_back( LandauParameters( 1,1,7,0, 19.1*1.6, 4.7*1.6 ));

    array.push_back( LandauParameters( 1,1,0,1, 24.1*1.2, 6.6 ));
    array.push_back( LandauParameters( 1,1,1,1, 16.9*1.2, 4.4*0.8 ));
    array.push_back( LandauParameters( 1,1,2,1, 28.2, 6.2 ));
    array.push_back( LandauParameters( 1,1,3,1, 13.5*1.2, 4.2*1.2 ));
    array.push_back( LandauParameters( 1,1,4,1, 29.1, 8.2 ));
    array.push_back( LandauParameters( 1,1,5,1, 24.1, 6.9 ));
    array.push_back( LandauParameters( 1,1,6,1, 20.2, 4.3*0.8 ));
    array.push_back( LandauParameters( 1,1,7,1, 22.3*0.8, 5.3*0.8 ));

    array.push_back( LandauParameters( 1,1,0,2, 20.5*1.2, 6.2 ));
    array.push_back( LandauParameters( 1,1,1,2, 18.2, 4.8*0.8 ));
    array.push_back( LandauParameters( 1,1,2,2, 22.6*1.2, 6.3 ));
    array.push_back( LandauParameters( 1,1,3,2, 18.6, 4.1*1.2 ));
    array.push_back( LandauParameters( 1,1,4,2, 32.5*0.8, 7.2*0.8 ));
    array.push_back( LandauParameters( 1,1,5,2, 34.4, 8.2 ));
    array.push_back( LandauParameters( 1,1,6,2, 26.6, 5.1*1.2 ));
    array.push_back( LandauParameters( 1,1,7,2, 14.7, 5.8*0.8 ));

    // north station 2
    array.push_back( LandauParameters( 1,2,0,0, 32.9, 6.6*1.2 ));
    array.push_back( LandauParameters( 1,2,1,0, 23.8, 5.9*0.8 ));
    array.push_back( LandauParameters( 1,2,2,0, 34.7, 5.9*1.2 ));
    array.push_back( LandauParameters( 1,2,3,0, 21.4*1.2, 9.5*0.6 ));
    array.push_back( LandauParameters( 1,2,4,0, 30.2, 7.6 ));
    array.push_back( LandauParameters( 1,2,5,0, 34.3, 7.8 ));
    array.push_back( LandauParameters( 1,2,6,0, 26.5, 5.8 ));
    array.push_back( LandauParameters( 1,2,7,0, 18.4, 4.0*1.4 ));

    array.push_back( LandauParameters( 1,2,0,1, 31.8*1.2, 7.0*1.2 ));
    array.push_back( LandauParameters( 1,2,1,1, 22.0, 5.2 ));
    array.push_back( LandauParameters( 1,2,2,1, 38.0, 7.9 ));
    array.push_back( LandauParameters( 1,2,3,1,  0.1, 5.5*1.8 ));
    array.push_back( LandauParameters( 1,2,4,1, 39.5, 8.4 ));
    array.push_back( LandauParameters( 1,2,5,1, 25.5, 5.6*0.8 ));
    array.push_back( LandauParameters( 1,2,6,1, 21.2, 5.0*1.2 ));
    array.push_back( LandauParameters( 1,2,7,1, 20.2, 3.8 ));


  }

  // pass parameters to relevant MutGap objects
  loadLandauParameters( array );

}

//_________________________________________
void MutArm::loadLandauParameters( const MutArm::LandauParametersArray& array )
{

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::LANDAU_PARAMETERS ) >= TMutDatabaseCntrl::MAX )
  {
    cout << "MutArm::loadLandauParameters (" << (_arm == MUTGEOM::South ? "south":"north") << ")" << endl;
    cout << "MutArm::loadLandauParameters. Format: arm, station, octant, gap, offset, scale" << endl;
  }

  for( LandauParametersArray::const_iterator iter = array.begin(); iter != array.end(); iter++ )
  {

    if( iter->_arm != _arm ) continue;
    if( iter->_station == MUTGEOM::Station3 && iter->_gap == MUTGEOM::Gap3 ) continue;

    {
      // first half octant
      MutGap* gap =
        f_pMutStations[iter->_station]->
        f_pMutOctants[iter->_octant]->
        f_pMutHalfOctants[MUTGEOM::HalfOctant1]->
        f_pMutGaps[iter->_gap];
      gap->setLandauOffset( iter->_offset );
      gap->setLandauScale( iter->_scale );
    }

    {
      // second half octant
      MutGap* gap =
        f_pMutStations[iter->_station]->
        f_pMutOctants[iter->_octant]->
        f_pMutHalfOctants[MUTGEOM::HalfOctant2]->
        f_pMutGaps[iter->_gap];
      gap->setLandauOffset( iter->_offset );
      gap->setLandauScale( iter->_scale );
    }

    if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::LANDAU_PARAMETERS ) >= TMutDatabaseCntrl::MAX )
    { cout << *iter; }

  }

}

//_________________________________________
void MutArm::fetchGlobalAligConsts(const char *file)
{

  // open file
  ifstream s(file);
  if (!s.good()){
    cout << "MutArm::fetchGlobalAligConsts - cannot access file " << file << endl;
    return;
  }

  cout << "MutArm::fetchGlobalAligConsts - " << name << ": reading global alignment using file " << file << endl;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN ) >= TMutDatabaseCntrl::MAX ) {
    MUTGEOM::PRINT( cout, "MutArm::fetchGlobalAligConsts" );
    cout << "arm station octant ds dr dz roll pitch yaw dExpansion" << endl;
  }

  // parse file
  _globalAligConsts.clear();
  static const int bufsize = 256;
  char linebuf[bufsize];
  while(s.getline(linebuf,bufsize,'\n'))
  {

    // skip empty or commented lines
    if( !strlen( linebuf ) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    istringstream stringbuf(linebuf);

    // try read
    GlobalAligConst alig;
    stringbuf >> alig;

    // check stream
    if( stringbuf.rdstate() & ios::failbit )
    {
      cout << "MutArm::fetchGlobalAligConsts - unable to parse line " << linebuf;
      continue;
    }

    // apply correction
    setGlobalAligConst( alig );

    // dump
    if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN ) >= TMutDatabaseCntrl::MAX ) cout << alig;

  }

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN ) >= TMutDatabaseCntrl::MAX )
  { MUTGEOM::PRINT( cout, "**" ); }

}

//____________________________________________________________________________________________________
void MutArm::fetchGlobalAligConsts(const int& run_number, PdbBankID bank_id, const char* output_file)
{

  // retrieve timestamp from run number
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp* time_stamp = runTime->getBeginTime( run_number );
  if( !time_stamp ) {
    cout << "MutArm::fetchGlobalAligConsts - could not find time stamp for run number " << run_number << endl;
    return;
  }

  // read based on timestamp
  cout << "MutArm::fetchGlobalAligConsts - run number: " << run_number << " timestamp: " << *time_stamp << endl;
  fetchGlobalAligConsts( *time_stamp, bank_id, output_file );

}

//____________________________________________________________________________________________________
void MutArm::fetchGlobalAligConsts(PHTimeStamp &time_stamp, PdbBankID bank_id, const char* output_file)
{
  const string bank_name="mut.geom.globalalign";
  const string class_name="PdbMutGeomBank";

  if(!fetch( class_name.c_str(), time_stamp, bank_name.c_str(), bank_id)){
    cout << "MutArm::fetchGlobalAligConsts - fetch failed." << endl;
    cout << "MutArm::fetchGlobalAligConsts - could not find "<<bank_name<<" with bank ID "<<bank_id.getInternalValue()<<" in database." << endl;
    return;
  }

  cout<<"MutArm::fetchGlobalAligConsts - updating "<<name<<" global alignment from the database. " << endl;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN ) >= TMutDatabaseCntrl::MAX ) {
    MUTGEOM::PRINT( cout, "MutArm::fetchGlobalAligConsts" );
    cout << "arm station octant ds dr dz roll pitch yaw dExpansion" << endl;
  }

  // prepare output file
  ofstream out( output_file ? output_file:"/dev/null" );

  _globalAligConsts.clear();
  unsigned int i=0;
  while(i<getBankLength()) {

    PdbMutGeom *alignCorrections  = (PdbMutGeom*)&geometryBank->getEntry(i);
    GlobalAligConst alig;

    // location
    alig.arm = alignCorrections->getArm();
    alig.station = alignCorrections->getStation();
    alig.octant = alignCorrections->getOctant();

    // translations
    alig.ds = alignCorrections->getGlobalPosition().getX();
    alig.dr = alignCorrections->getGlobalPosition().getY();
    alig.dz = alignCorrections->getGlobalPosition().getZ();

    // body-centered rotations
    alig.roll = alignCorrections->getGlobalVector().getX();
    alig.pitch = alignCorrections->getGlobalVector().getY();
    alig.yaw = alignCorrections->getGlobalVector().getZ();
    i++;

    // expansion
    alignCorrections = (PdbMutGeom*)&geometryBank->getEntry(i);
    alig.dExpansion = alignCorrections->getGlobalPosition().getX();
    i++;

    // apply correction
    setGlobalAligConst( alig );

    // dump
    if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN ) >= TMutDatabaseCntrl::MAX ) cout << alig;
    if( output_file && out ) out << alig;

  }

  if(!commit()) cout << "MutArm::fetchGlobalAligConsts - commit failed." << endl;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN ) >= TMutDatabaseCntrl::MAX )
  { MUTGEOM::PRINT( cout, "**" ); }

  if( output_file && out ) {
    out.close();
    cout << "MutArm::fetchGlobalAligConsts - alignment written to file " << output_file << endl;
  }

}

//______________________________________________________________________________
void MutArm::setGlobalAligConst( const MutArm::GlobalAligConst& alig )
{

  // check arm
  if( alig.arm != getArm() ) return;

  _globalAligConsts.push_back( alig );

  // retrieve octant and frame
  MutOctant *pOct = f_pMutStations[ alig.station ]->f_pMutOctants[ alig.octant ];
  PHFrame OctCenterFrame = pOct->getBodyCenteredFrame();
  PHFrame globalFrame;

  // translation
  if(  alig.dr!=0.0 ||  alig.ds!=0.0 ||  alig.dz!=0.0)
  {

    //convert body-centered translation into global coordinate translation.
    PHPoint BodyCenteredTrans( alig.ds, alig.dr, alig.dz );
    PHPoint translation = rotateAndTranslate(OctCenterFrame,BodyCenteredTrans,globalFrame) - OctCenterFrame.getOrigin();
    pOct->translate(translation);
  }

  // rotation
  if( alig.roll!=0.0 ||  alig.pitch!=0.0 ||  alig.yaw!=0.0 )
  {

    //transform to body-centered coordinates.
    pOct->transformToNewFrame(globalFrame, OctCenterFrame);

    //rotate in body-centered frame
    if( alig.pitch != 0.0 ) pOct->rotate( alig.pitch, 'x' );
    if( alig.roll != 0.0 ) pOct->rotate( alig.roll, 'z' );
    if( alig.yaw != 0.0 ) pOct->rotate( alig.yaw, 'y' );

    //transform back to global coords
    pOct->transformToNewFrame(OctCenterFrame, globalFrame);
  }

  if( alig.dExpansion != 0.0 ) pOct->XYExpansion( alig.dExpansion );

}
//_________________________________________
void MutArm::fetchInternalAligConsts(const char *file)
{
  ifstream s(file);
  if (!s.good()){
    cout << "MutArm::fetchInternalAligConsts - cannot access file " << file << endl;
    return;
  } else cout << "MutArm::fetchInternalAligConsts - " << name << ": reading internal alignment using file " << file << endl;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN ) >= TMutDatabaseCntrl::MAX )
  {
    MUTGEOM::PRINT( cout, "MutArm::fetchInternalAligConsts" );
    cout << "arm station octant half gap plane ds dr dz roll pitch yaw dExpansion" << endl;
  }

  static const int bufsize = 256;
  char linebuf[bufsize];

  _internalAligConsts.clear();
  while(s.getline(linebuf,bufsize,'\n'))
  {

    // skip empty or commented lines
    if( !strlen( linebuf ) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    istringstream stringbuf(linebuf);

    // dump to alignement constant
    InternalAligConst alig;
    stringbuf >> alig;

    // apply alignment constants
    setInternalAligConst( alig );

    // dump
    if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN ) >= TMutDatabaseCntrl::MAX ) cout << alig;

  }

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN ) >= TMutDatabaseCntrl::MAX )
  { MUTGEOM::PRINT( cout, "**" ); }

}

//_________________________________________________________
void MutArm::fetchInternalAligConsts( const int& run_number, PdbBankID bank_id, const char* output_file )
{
  // retrieve timestamp from run number
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp* time_stamp = runTime->getBeginTime( run_number );
  if( !time_stamp ) {
    cout << "MutArm::fetchInternalAligConsts - could not find time stamp for run number " << run_number << endl;
    return;
  }

  // read based on timestamp
  cout << "MutArm::fetchInternalAligConsts - run number: " << run_number << " timestamp: " << *time_stamp << endl;
  fetchInternalAligConsts( *time_stamp, bank_id, output_file );

}

//_________________________________________________________
void MutArm::fetchInternalAligConsts( PHTimeStamp &time_stamp, PdbBankID bank_id, const char* output_file )
{

  const string bank_name="mut.geom.internalalign";
  const string class_name="PdbMutGeomBank";

  if(!fetch( class_name.c_str(), time_stamp, bank_name.c_str(), bank_id)){
    cout << "MutArm::fetchInternalAligConsts - fetch failed." << endl;
    cout << "MutArm::fetchInternalAligConsts - could not find "<<bank_name<<" with bank ID "<<bank_id.getInternalValue()<<" in database." << endl;
    cout << "MutArm::fetchInternalAligConsts - MuTr Geometry has not been aligned!" << endl;
    return;
  }

  cout<<"MutArm::fetchInternalAligConsts - updating " << name << " internal alignment from the database." << endl;
  print_bank_summary();

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN ) >= TMutDatabaseCntrl::MAX )
  {
    MUTGEOM::PRINT( cout, "MutArm::fetchInternalAligConsts" );
    cout << "arm station octant half gap plane ds dr dz roll pitch yaw dExpansion" << endl;
  }

  // prepare output file
  ofstream out( output_file ? output_file:"/dev/null" );

  unsigned int i=0;
  _internalAligConsts.clear();
  while(i<getBankLength())
  {
    PdbMutGeom *alignCorrections = (PdbMutGeom*)&geometryBank->getEntry(i);
    InternalAligConst alig;

    // location
    alig.arm = alignCorrections->getArm();
    alig.station = alignCorrections->getStation();
    alig.octant = alignCorrections->getOctant();

    // translations
    alig.ds = alignCorrections->getGlobalPosition().getX();
    alig.dr = alignCorrections->getGlobalPosition().getY();
    alig.dz = alignCorrections->getGlobalPosition().getZ();

    // rotations
    alig.roll = alignCorrections->getGlobalVector().getX();
    alig.pitch = alignCorrections->getGlobalVector().getY();
    alig.yaw = alignCorrections->getGlobalVector().getZ();

    i++;
    alignCorrections = (PdbMutGeom*)&geometryBank->getEntry(i);

    // locations
    alig.half  = alignCorrections->getArm();
    alig.gap = alignCorrections->getStation();
    alig.plane = alignCorrections->getOctant();
    alig.dExpansion = alignCorrections->getGlobalPosition().getX();
    i++;

    // apply correction
    setInternalAligConst( alig );

    // dump
    if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN ) >= TMutDatabaseCntrl::MAX ) cout << alig;
    if( output_file && out ) out << alig;

  }

  if(!commit()) cout<<"MutArm::fetchInternalAligConsts - commit failed." << endl;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN ) >= TMutDatabaseCntrl::MAX )
  { MUTGEOM::PRINT( cout, "**" ); }

  if( output_file && out ) {
    out.close();
    cout << "MutArm::fetchInternalAligConsts - alignment written to file " << output_file << endl;
  }

}

//______________________________________________________________________________
void MutArm::setInternalAligConst( const MutArm::InternalAligConst& alig )
{

  // check arm
  if( alig.arm != getArm() ) return;

  _internalAligConsts.push_back( alig );

  // retrieve half octant
  MutHalfOctant *pHalfOct = f_pMutStations[ alig.station ]->f_pMutOctants[ alig.octant ]->f_pMutHalfOctants[ alig.half ];

  // retrieve half octant frame
  PHFrame halfOctFrame = pHalfOct->getBodyCenteredFrame();

  // retrieve place
  MutPlane *pPlane = pHalfOct->f_pMutGaps[ alig.gap ]->f_pMutPlanes[ alig.plane ];

  // translation
  PHFrame globalFrame;
  if( alig.dr!=0.0 || alig.ds!=0.0 || alig.dz!=0.0 )
  {
    //convert body-centered translation into global coordinate translation.
    PHPoint BodyCenteredTrans( alig.ds, alig.dr, alig.dz );
    PHPoint translation = rotateAndTranslate(halfOctFrame,BodyCenteredTrans,globalFrame) - halfOctFrame.getOrigin();
    pPlane->translate(translation);
  }

  // rotation
  if( alig.roll!=0.0 || alig.pitch!=0.0 || alig.yaw!=0.0 )
  {

    //transform to body-centered coordinates.
    pPlane->transformToNewFrame(globalFrame, halfOctFrame);

    //rotate in body-centered frame
    if( alig.pitch != 0.0 ) pPlane->rotate( alig.pitch, 'x' );
    if( alig.roll != 0.0 ) pPlane->rotate( alig.roll, 'z' );
    if( alig.yaw != 0.0 ) pPlane->rotate( alig.yaw, 'y' );

    //transform back to global coords
    pPlane->transformToNewFrame(halfOctFrame, globalFrame);

  }

  // expension
  if( alig.dExpansion!=0.0 ) pHalfOct->XYExpansion( alig.dExpansion );

}


//_________________________________________________________
void MutArm::translate(const PHPoint &translation)
{
  //Translate position of Arm and its Stations.

  fGlobalPosition = fGlobalPosition + translation;
  for (int j=0; j<NumberOfStations; j++)
  if(f_pMutStations[j]) f_pMutStations[j]->translate(translation);
}

//_________________________________________________________
void MutArm::rotate(float angle, char axisLabel)
{
  rotateThis(angle, axisLabel);
  for (int j=0; j<NumberOfStations; j++)
  if(f_pMutStations[j]) f_pMutStations[j]->rotate(angle, axisLabel);
}

//_________________________________________________________
unsigned int MutArm::fetchDisabledWires(const char *file)
{

  ifstream s(file);
  if (!s){
    cout << "MutArm::fetchDisabledWires - unable to open file " << file << endl;
    s.close();
    return 0;
  } else
  cout << "MutArm::fetchDisabledWires - " << name << ": Reading disabled HV channels from " << file << endl;

  set< string > dead_channels;
  const int bufsize = 126;
  char linebuf[bufsize];
  while(s.getline(linebuf,bufsize))
  {

    if( !strlen( linebuf ) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    string dead_channel;
    istringstream str( linebuf );
    str >> dead_channel;
    dead_channels.insert( dead_channel );
  }

  s.close();

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DISABLED_WIRES ) >= TMutDatabaseCntrl::MAX )
  {
    MUTGEOM::PRINT( cout, "MutArm::fetchDisabledWires" );
    for( set<string>::const_iterator iter = dead_channels.begin(); iter != dead_channels.end(); iter++ )
    { cout << *iter << endl; }
    MUTGEOM::PRINT( cout, "**" );
  }
  disable_anodes(dead_channels);
  return dead_channels.size();

}

//_________________________________________________________
unsigned int MutArm::fetchDisabledWires(const int& run_number, PdbBankID bank_id, const char* output_file )
{

  // retrieve timestamp from run number
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp* time_stamp = runTime->getBeginTime( run_number );
  if( !time_stamp ) {
    cout << "MutArm::fetchDisabledWires - could not find time stamp for run number " << run_number << endl;
    return 0;
  }

  // read based on timestamp
  cout << "MutArm::fetchDisabledWires - run number: " << run_number << " timestamp: " << *time_stamp << endl;
  return fetchDisabledWires( *time_stamp, bank_id, output_file );

}

//_________________________________________________________
unsigned int MutArm::fetchDisabledWires(PHTimeStamp &time_stamp, PdbBankID bank_id, const char* output_file )
{

  cout<< "MutArm::fetchDisabledWires - " << name << ": Reading disabled HV channels from database. " << endl;
  const string bank_name="mut.hv.disabled";
  const string class_name="PdbMutHVDisabledBank";

  if(!fetch( class_name.c_str(), time_stamp, bank_name.c_str(), bank_id))
  {
    cout << "MutArm::fetchDisabledWires - fetch failed." << endl;
    cout << "MutArm::fetchDisabledWires - could not find " << bank_name << " with bank ID " << bank_id.getInternalValue() << " in database." << endl;
    cout << "MutArm::fetchDisabledWires - this is harmless if you are running on real data" << endl;
    return 0;
  }

  set< string > dead_channels;
  for( unsigned int i=0; i < getBankLength(); i++ )
  {

    PdbMutHVDisabled* disabled_hv = (PdbMutHVDisabled*)&geometryBank->getEntry(i);
    if( !disabled_hv )
    {
      cout << "MutArm::fetchDisabledWires - unable to cast entry " << i << endl;
      continue;
    }

    const vector<string> channels = disabled_hv->getChanStringVec();
    for( vector<string>::const_iterator iter = channels.begin(); iter != channels.end(); iter++ )
    { dead_channels.insert( *iter ); }

  }

  // print
  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DISABLED_WIRES ) >= TMutDatabaseCntrl::MAX )
  {
    MUTGEOM::PRINT( cout, "MutArm::fetchDisabledWires" );
    for( set<string>::const_iterator iter = dead_channels.begin(); iter != dead_channels.end(); iter++ )
    { cout << *iter << endl; }
    MUTGEOM::PRINT( cout, "**" );
  }

  // print to file
  if( output_file )
  {
    ofstream out( output_file );
    if( !out ) cout << "MutArm::fetchDisabledWires - cannot write to file " << output_file << endl;
    else {
      for( set<string>::const_iterator iter = dead_channels.begin(); iter != dead_channels.end(); iter++ )
      { out << *iter << endl; }
      cout << "MutArm::fetchDisabledWires - disabled channels written to file " << output_file << endl;
      out.close();
    }
  }

  // disable found channels
  disable_anodes(dead_channels);

  if(!commit()) cout<<"MutArm::fetchDisabledWires - commit failed." << endl;
  return dead_channels.size();

}

//_________________________________________________________
bool MutArm::updateDisabledWires( const int& run_number, PdbBankID bank_id, const char *descrip, const char *file )
{

  // retrieve timestamp from run number
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp time_start, time_stop;
  PHTimeStamp* time_stamp = runTime->getBeginTime( run_number );
  if( !time_stamp )
  {
    cout << "MutArm::fetchDisabledWires - could not find time stamp for run number " << run_number << endl;
    return 0;
  } else time_start = *time_stamp;


  time_stamp = runTime->getEndTime( run_number );
  if( !time_stamp )
  {
    cout << "MutArm::fetchDisabledWires - could not find end time stamp for run number " << run_number << endl;
    cout << "MutArm::fetchDisabledWires - will use start time + 1 minute " << endl;
    time_stop = time_start;
    time_stop += 60;
  } else time_stop = *time_stamp;

  cout << "MutArm::updateDisabledWires - run_number: " << run_number << " start: " << time_start << " stop: " << time_stop << endl;

  // update database using found timestamps
  return updateDisabledWires( time_start, time_stop, bank_id, descrip, file );

}

//_________________________________________________________
bool MutArm::updateDisabledWires(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file )
{

  // read disabled channels from file
  ifstream s(file);
  if (!s){
    cout << "MutArm::updateDisabledWires - unable to open file " << file << endl;
    return false;
  } else
  cout<< "MutArm::updateDisabledWires - " << name << ": Reading disabled HV channels from " << file << endl;

  set< string > dead_channels;
  const int bufsize = 126;
  char linebuf[bufsize];
  while(s.getline(linebuf,bufsize))
  {

    if( !strlen( linebuf ) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    string dead_channel;
    istringstream str( linebuf );
    str >> dead_channel;
    dead_channels.insert( dead_channel );
  }

  // try create bank
  const string bank_name="mut.hv.disabled";
  const string class_name="PdbMutHVDisabledBank";

  // try create bank
  if(!update( class_name.c_str(), time_start, time_stop, bank_name.c_str(), bank_id, descrip))
  {
    cout << "MutArm::updateDisabledWires - failed to open database ."<<endl;
    return false;
  }

  // only one entry
  geometryBank->setLength( 1 );
  PdbMutHVDisabled* interface = static_cast<PdbMutHVDisabled*>( &geometryBank->getEntry(0) );
  if( !interface )
  {
    cout << "MutArm::updateDisabledWires - unable to cast database entry."<<endl;
    return false;
  }

  // fill channels
  for( set<string>::const_iterator iter = dead_channels.begin(); iter != dead_channels.end(); iter++ )
  { interface->addDeadHVCharString( *iter ); }

  // commit
  if(!commit()) {
    cout<<"MutArm::updateDisabledWires - commit failed." << endl;
    return false;
  }

  cout<< "MutArm::updateDisabledWires - " << name << ": write " << dead_channels.size() << " disabled HV channels to database. " << endl;
  return true;

}

//_________________________________________________________
void MutArm::resetDisabledWires()
{
  for (int j=0; j<NumberOfStations; j++)
  for (int k=0; k<NumberOfOctants; k++)
  for (int l=0; l<NumberOfHalfOctants; l++)
  {

    if( !f_pMutStations[j]->f_pMutOctants[k]->f_pMutHalfOctants[l]) continue;

    int NumberOfGaps = f_pMutStations[j]->f_pMutOctants[k]->f_pMutHalfOctants[l]->getNumberOfGaps();
    for (int m=0; m<NumberOfGaps; m++) {
      MutGap *pGap = f_pMutStations[j]->f_pMutOctants[k]->f_pMutHalfOctants[l]->f_pMutGaps[m];

      if(!pGap->f_pMutPlanes[Wire]) continue;

      MutPlane *plane = pGap->f_pMutPlanes[Wire];
      int numChannels=plane->getNumElements();
      for (int channel=0; channel<numChannels; channel++)
      plane->f_pMutWires[channel]->setChannelToActive();
    }
  }
}

//_________________________________________________________
void MutArm::disable_anodes(const set<string>& dead_channels)
{
  /*
    For Run2, there are 2 mappings that need to be performed:
    1) The HV channels must be mapped to their corresponding anode card,
       since design mapping differs from reality in some cases

    2) The anode cards need to be mapped to their corresponding wires,
       which differ by station and card number (startWire[][], endWire[][])

    There are 2 reasons a channel will be disabled:
    1) Some of the HV channels were not connected to anything due to
       wiring problems

    2) HV channels tripped during a run (deadHVchnls[][])

    Run3 and beyond are mapped correctly.
  */

  PHTimeStamp beginRun2(2001,5,5,0,0,0,0);
  PHTimeStamp endRun2(2002,2,2,0,0,0,0);
  bool is_run2( TMutDatabaseCntrl::get_database_access("Run02") || (_db_time>beginRun2 && _db_time < endRun2) );

  if(is_run2)
  { cout << "MutArm::disable_anodes - using Run2 wire to HV boards mapping." << endl; }

  // get the set of disabled anodes from MutAnodeMap
  MutAnodeMap::CardSet disabled_cards( MutAnodeMap::find_cards( dead_channels, is_run2 ) );

  // for run2, also fetch the non connected boards
  if( is_run2 )
  {
    MutAnodeMap::CardSet disconnected_cards( MutAnodeMap::find_disconnected_cards() );
    disabled_cards.insert( disconnected_cards.begin(), disconnected_cards.end() );
  }

  // loop over found cards, make sure that the arm match, and disable
  unsigned int n_anodes(0);
  for( MutAnodeMap::CardSet::const_iterator iter = disabled_cards.begin(); iter != disabled_cards.end(); iter++ )
  {
    if( iter->arm() == getArm() )
    {
      disable_anode_cards(iter->station(), iter->octant(), iter->gap(), iter->card() );
      n_anodes++;
    }
  }

  cout << "MutArm::disable_anodes - there are " << n_anodes << " disabled anodes." << endl;
  return;

}

//___________________________________________________
void MutArm::disable_anode_cards( const int& station, const int& octant, const int& gap, const int& card)
{

  // If card exists, disable its associated wires from MutAnodeMap
  MutOctant *octantPtr = f_pMutStations[station]->f_pMutOctants[octant];
  for(int half=0; half<NumberOfHalfOctants; half++)
  {

    // check if half octant and gap exists
    if(f_pMutStations[station]->f_pMutOctants[octant]->f_pMutHalfOctants[half]->f_pMutGaps[gap])
    {

      MutPlane *planePtr = octantPtr->f_pMutHalfOctants[half]->f_pMutGaps[gap]->f_pMutPlanes[MUTGEOM::Wire];
      planePtr->disableAnodeCard( card );

      //number of wires differs between station 2 front and rear
      //octants - truncate if necessary
      unsigned int start_wire( min( MutAnodeMap::get_start_wire( getArm(), station, octant, card ), planePtr->getNumElements() ) );
      unsigned int end_wire( min( MutAnodeMap::get_end_wire( getArm(), station, octant, card ), planePtr->getNumElements() ) );

      for( unsigned int wire_id = start_wire; wire_id<end_wire; wire_id++)
      {
        MutWire *wire = planePtr->f_pMutWires[wire_id];
        if(wire) wire->setChannelToDead();
      }

    }
  }
}

//___________________________________________________
unsigned int MutArm::fetchDisconnectedWires(const char *file)
{

  ifstream s(file);
  if (!s)
  {
    cout << "MutArm::fetchDisconnectedWires - cannot access file " << file << endl;
    return 0;
  }

  cout
    << "MutArm::fetchDisconnectedWires - "
    << " fetching "<< name <<" individual dead anode wires from file "
    << file << endl;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DISCONNECTED_WIRES ) >= TMutDatabaseCntrl::MAX )
  {
    MUTGEOM::PRINT( cout, "MutArm::fetchDisconnectedWires" );
    cout << "MutArm::fetchDisconnectedWires - arm station octant half gap channel" << endl;
  }

  const int bufsize = 126;
  char linebuf[bufsize];
  unsigned int n_wires(0);
  while(s.getline(linebuf,bufsize))
  {
    // skip empty lines and commented lines
    if( !strlen(linebuf) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    int arm,station,octant,half,gap,channel;
    istringstream stringbuf(linebuf);
    stringbuf >> arm >> station >> octant >> half  >> gap >> channel;

    if( stringbuf.rdstate() & ios::failbit )
    {
      cout << "MutArm::fetchDisconnectedWires - problem parsing line " << linebuf << endl;
      continue;
    }

    if (arm != getArm()) continue;

    // dump
    if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DISCONNECTED_WIRES ) >= TMutDatabaseCntrl::MAX )
    cout << "MutArm::fetchDisconnectedWires - "
      << arm << " " << station << " " << octant << " "
      << half  << " " << gap << " " << channel << endl;

    // retrieve/check plane
    MutPlane *plane =  f_pMutStations[station]->f_pMutOctants[octant]->
      f_pMutHalfOctants[half]->
      f_pMutGaps[gap]->f_pMutPlanes[MUTGEOM::Wire];
    if( !plane )
    {
      cout << "MutArm::fetchDisconnectedWires - invalid plane: "
        << arm << " " << station << " " << octant << " "
        << half  << " " << gap << endl;
      continue;
    }

    // check num elements
    MutWire *wire(0);
    if( channel >= plane->getNumElements() || !( wire = plane->f_pMutWires[channel] ) )
    {
      cout << "MutArm::fetchDisconnectedWires - invalid channel: "
        << arm << " " << station << " " << octant << " "
        << half  << " " << gap << " " << channel << " maximum is: " << plane->getNumElements() << endl;
      continue;
    }

    wire->setChannelToDead();
    n_wires++;

  }

  cout << "MutArm::fetchDisconnectedWires - there are " << n_wires << " disconnected wires." << endl;
  return n_wires;

}


//_________________________________________________________
unsigned int MutArm::fetchDisconnectedWires(const int& run_number, PdbBankID bank_id )
{

  // retrieve timestamp from run number
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp* time_stamp = runTime->getBeginTime( run_number );
  if( !time_stamp ) {
    cout << "MutArm::fetchDisconnectedWires - could not find time stamp for run number " << run_number << endl;
    return 0;
  }

  // read based on timestamp
  cout << "MutArm::fetchDisconnectedWires - run number: " << run_number << " timestamp: " << *time_stamp << endl;
  return fetchDisconnectedWires( *time_stamp, bank_id );

}

//___________________________________________________
unsigned int MutArm::fetchDisconnectedWires(PHTimeStamp &time_stamp, PdbBankID bank_id)
{
  const string bank_name = (getArm() == South ) ? "mut.south.disconnectedwires":"mut.north.disconnectedwires";
  const string class_name = "PdbMutCalibStripBank";

  if(!fetch( class_name.c_str(), time_stamp, bank_name.c_str(), bank_id ) )
  {
    cout << "MutArm::fetchDisconnectedWires - fetch failed." << endl;
    cout << "MutArm::fetchDisconnectedWires - " << name <<" dead anode wires have not been disabled!" << endl;
    return 0;
  }

  cout
    << "MutArm::fetchDisconnectedWires -"
    <<" fetching "<< name <<" individual dead anode wires from database" << endl;

  PdbMutCalibStrip *deadChannel;
  int arm, station, octant, half, gap, channel;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DISCONNECTED_WIRES ) >= TMutDatabaseCntrl::MAX )
  {
    MUTGEOM::PRINT( cout, "MutArm::fetchDisconnectedWires" );
    cout << "MutArm::fetchDisconnectedWires - arm station octant half gap channel" << endl;
  }

  unsigned int i=0;
  unsigned int n_wires(0);
  while(i<getBankLength())
  {

    deadChannel = (PdbMutCalibStrip*)&geometryBank->getEntry(i);
    arm = deadChannel->getArm();
    station = deadChannel->getStation();
    octant = deadChannel->getOctant();
    half  = deadChannel->getHalfOctant();
    gap = deadChannel->getGap();
    channel = deadChannel->getStrip();
    i++;

    if( arm != getArm() ) continue;

    // dump
    if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DISCONNECTED_WIRES ) >= TMutDatabaseCntrl::MAX )
    cout << "MutArm::fetchDisconnectedWires - "
      << arm << " " << station << " " << octant << " "
      << half  << " " << gap << " " << channel << endl;

    // retrieve/check plane
    MutPlane *plane =  f_pMutStations[station]->f_pMutOctants[octant]->
      f_pMutHalfOctants[half]->
      f_pMutGaps[gap]->f_pMutPlanes[Wire];
    if( !plane )
    {
      cout << "MutArm::fetchDisconnectedWires - invalid plane: "
        << arm << " " << station << " " << octant << " "
        << half  << " " << gap << endl;
      continue;
    }

    // check num elements
    MutWire *wire(0);
    if( channel >= plane->getNumElements() || !( wire = plane->f_pMutWires[channel] ) )
    {
      cout << "MutArm::fetchDisconnectedWires - invalid channel: "
        << arm << " " << station << " " << octant << " "
        << half  << " " << gap << " " << channel << " maximum is: " << plane->getNumElements() << endl;
      continue;
    }

    // disable channel
    wire->setChannelToDead();
    n_wires++;

  }

  if(!commit()) cout << "MutArm::fetchDisconnectedWires - commit failed." << endl;

  cout << "MutArm::fetchDisconnectedWires - there are " << n_wires << " disconnected wires." << endl;
  return n_wires;

}

//________________________________________________________________
bool MutArm::updateDisconnectedWires(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file)
{

  // check input file
  ifstream in(file);
  if (!in.good()){
    cout << "MutArm::updateDisconnectedWires - error opening file "<< file << endl;
    return false;
  }

  // parse the file
  list< PdbMutCalibStrip > geom_objects;
  char line[512];
  while( !( in.rdstate() & ios::failbit ) )
  {
    in.getline( line, 512, '\n' );

    int arm(0);
    int station(0);
    int octant(0);
    int half(0);
    int gap(0);
    int plane( MUTGEOM::Wire );
    int channel(0);
    istringstream line_stream( line );

    // parse line
    line_stream >> arm >> station >> octant >> half >> gap >> channel;

    // check stream
    if( line_stream.rdstate() & ios::failbit )
    {
      cout << "MutArm::updateDisconnectedWires - unable to parse line " << line << endl;
      continue;
    }

    // skip if arm does not match
    if( arm != getArm() ) continue;

    // book new object
    PdbMutCalibStrip deadWire;
    deadWire.setArm(arm);
    deadWire.setStation(station);
    deadWire.setOctant(octant);
    deadWire.setHalfOctant(half);
    deadWire.setGap(gap);
    deadWire.setPlane(plane);
    deadWire.setStrip(channel);
    geom_objects.push_back( deadWire );
  }

  cout<<"MutArm::updateDisconnectedWires - created " << geom_objects.size() << " objects." << endl;

  const string bank_name = (getArm() == MUTGEOM::South ) ? "mut.south.disconnectedwires" : "mut.north.disconnectedwires";
  cout << "MutArm::updateDisconnectedWires - arm: " << getArm() << " bankname: " << bank_name << endl;

  const string class_name = "PdbMutCalibStripBank";
  if(!update( class_name.c_str(), time_start, time_stop, bank_name.c_str(), bank_id, descrip)){
    cout << "MutArm::updateDisconnectedWires - failed to open database ."<<endl;
    return false;
  }

  geometryBank->setLength( geom_objects.size() );
  cout<<"MutArm::updateDisconnectedWires - database opened."<<endl;

  // update bank entries with stored objects
  unsigned int count = 0;
  for( list<PdbMutCalibStrip>::const_iterator iter = geom_objects.begin(); iter != geom_objects.end(); iter++, count++ )
  {
    cout << "MutArm::updateDisconnectedWires - writing entry " << count << endl;
     PdbMutCalibStrip* entry = static_cast<PdbMutCalibStrip*>( &geometryBank->getEntry(count) );
     *entry = *iter;
  }

  // commit
  if(!commit()) {
    cout<<"MutArm::updateDisconnectedWires - commit failed." << endl;
    return false;
  }

  return true;
}

//________________________________________________________________
void MutArm::fetchDeadChannels(const char *file)
{
  const int bufsize = 256;
  char linebuf[bufsize];
  int arm, station, octant, half, gap, plane, channel;

  ifstream s(file);
  if (!s){
    cout << "MutArm::fetchDeadChannels - cannot access file " << file << endl;
    return;
  } else {
    cout << "MutArm::fetchDeadChannels - " << name << "; reading dead DCM channels from " << file << endl;
  }

  int counts(0);
  while(s.getline(linebuf,bufsize,'\n'))
  {

    // skip empty lines or comments
    if( !strlen( linebuf ) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    istringstream stringbuf(linebuf);
    stringbuf >> arm >> station >> octant >> half  >> gap >> plane >> channel;

    // check if parsing went well
    if( stringbuf.rdstate() & ios::failbit )
    {
      cout << "MutArm::fetchDeadChannels - problem parsing line " << linebuf << endl;
      continue;
    }

    if (arm==getArm())
    {
      if(plane==Cathode1 || plane == Cathode2)
      {

        MutStrip *strip =
          f_pMutStations[station]->
          f_pMutOctants[octant]->
          f_pMutHalfOctants[half]->
          f_pMutGaps[gap]->
          f_pMutPlanes[plane]->
          f_pMutStrips[channel];

        if(strip) {

          strip->setChannelToDead();
          counts++;

        } else cout << "MutArm::fetchDeadChannels - invalid strip at line " << linebuf << endl;
      } else cout << "MutArm::fetchDeadChannels - invalid plane at line " << linebuf << endl;
    }
  }

  s.close();
  cout<<"MutArm::fetchDeadChannels - " << name << ": read " << counts << " dead channels." << endl;
}

//________________________________________________________________
void MutArm::fetchDeadChannels(PHTimeStamp &time_stamp, PdbBankID bank_id)
{
  const string bank_name = ( getArm() == South ) ? "mut.south.deadchannels":"mut.north.deadchannels";
  const string class_name = "PdbMutCalibStripBank";

  if(!fetch( class_name.c_str(), time_stamp, bank_name.c_str(), bank_id))
  {
    cout << "MutArm::fetchDeadChannels - cannot access database" << endl;
    cout << "MutArm::fetchDeadChannels - this is harmless. This means that there is no stored dead channels for this run" << endl;
    cout << "MutArm::fetchDeadChannels - (apart from specific cases, dead channels are handled by the calibrations)." << endl;
    return;
  }

  cout << "MutArm::fetchDeadChannels - " << name << ": fetching dead FEM channels from database. " << endl;
  cout << "MutArm::fetchDeadChannels - " << name << " - time: " << time_stamp << " (" << time_stamp.getTics() << ")" << endl;
  print_bank_summary();

  PdbMutCalibStrip *deadChannel;
  int arm, station, octant, half, gap, plane, channel;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DEAD_CHANNELS ) >= TMutDatabaseCntrl::MAX ) {
    MUTGEOM::PRINT( cout, "MutArm::fetchDeadChannels" );
    cout << "MutArm::fetchDeadChannels - arm station octant half gap plane channel" << endl;
  }

  unsigned int i=0;
  unsigned int counts = 0;
  while(i<getBankLength())
  {

    deadChannel = (PdbMutCalibStrip*)&geometryBank->getEntry(i);
    arm = deadChannel->getArm();
    station = deadChannel->getStation();
    octant = deadChannel->getOctant();
    half  = deadChannel->getHalfOctant();
    gap = deadChannel->getGap();
    plane = deadChannel->getPlane();
    channel = deadChannel->getStrip();

    i++;
    if( arm!=getArm() ) continue;
    if(plane==Cathode1 || plane == Cathode2) {

      // dump
      if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DEAD_CHANNELS ) >= TMutDatabaseCntrl::MAX )
      cout << "MutArm::fetchDeadChannels - "
        << arm << " " << station << " " << octant << " " << half  << " "
        << gap << " " << plane << " " << channel << endl;

      MutStrip *strip =
        f_pMutStations[station]->
        f_pMutOctants[octant]->
        f_pMutHalfOctants[half]->
        f_pMutGaps[gap]->
        f_pMutPlanes[plane]->
        f_pMutStrips[channel];

      if(strip) {
        counts++;
        strip->setChannelToDead();
      }
    }

  }

  if(!commit()) cout << "MutArm::fetchDeadChannels - commit failed." << endl;
  cout<<"MutArm::fetchDeadChannels - " << name << ": read " << counts << " dead channels." << endl;

}

//________________________________________________________________
bool MutArm::updateDeadChannels(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file)
{

  cout << "MutArm::updateDeadChannels - arm: " << getArm() << endl;

  // check input file
  ifstream in(file);
  if (!in.good()){
    cout << "MutArm::updateDeadChannels - error opening file " << file << endl;
    return false;
  }

  // parse the file
  list< PdbMutCalibStrip > geom_objects;
  char line[512];
  while( !( in.rdstate() & ios::failbit ) )
  {
    in.getline( line, 512, '\n' );

    // skip empty lines or comments
    if( ( !strlen( line ) ) || strncmp( line, "//", 2 ) == 0 ) continue;

    int arm(0), station(0), octant(0), half(0), gap(0), plane(0), channel(0);
    istringstream line_stream( line );
    line_stream >> arm >> station >> octant >> half >> gap >> plane >> channel;

    // check stream
    if( line_stream.rdstate() & ios::failbit )
    {
      cout << "MutArm::updateDeadChannels - unable to parse line " << line;
      continue;
    }

    // skip if arm does not match
    if( arm != getArm() ) continue;

    // book new object
    PdbMutCalibStrip deadChannel;
    deadChannel.setArm(arm);
    deadChannel.setStation(station);
    deadChannel.setOctant(octant);
    deadChannel.setHalfOctant(half);
    deadChannel.setGap(gap);
    deadChannel.setPlane(plane);
    deadChannel.setStrip(channel);
    geom_objects.push_back( deadChannel );
  }

  cout<<"MutArm::updateDeadChannels - created " << geom_objects.size() << " objects." << endl;
  const string bank_name = ( getArm() == South ) ? "mut.south.deadchannels":"mut.north.deadchannels";
  const string class_name = "PdbMutCalibStripBank";

  if(!update( class_name.c_str(), time_start, time_stop, bank_name.c_str(), bank_id, descrip))
  {
    cout<<"MutArm::updateDeadChannels - failed to open database ."<<endl;
    return false;
  }

  geometryBank->setLength( geom_objects.size() );
  cout<<"MutArm::updateDeadChannels - database opened."<<endl;

  // update bank entries with stored objects
  unsigned int count = 0;
  for( list<PdbMutCalibStrip>::const_iterator iter = geom_objects.begin(); iter != geom_objects.end(); iter++, count++ )
  {
    cout << "MutArm::updateDeadChannels - writing entry " << count << endl;
     PdbMutCalibStrip* entry = static_cast<PdbMutCalibStrip*>( &geometryBank->getEntry(count) );
     *entry = *iter;
  }

  // commit
  if(!commit()) {
    cout<<"MutArm::updateDeadChannels - commit failed." << endl;
    return false;
  }

  return true;
}

//________________________________________________________________
void MutArm::fetchAttenuatedChannels(const char *file)
{
  const int bufsize = 256;
  char linebuf[bufsize];
  int arm, station, octant, half, gap, plane, channel;

  ifstream s(file);
  if (!s) {
    cout << "MutArm::fetchAttenuatedChannels - cannot access file " << file << endl;
    return;
  }

  cout<<"MutArm::fetchAttenuatedChannels - " << name << ": reading attenuated strips from "<<file<< endl;

  while(s.getline(linebuf,bufsize,'\n'))
  {

    // skip empty lines or comments
    if( !strlen( linebuf ) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    istringstream stringbuf(linebuf);
    stringbuf >> arm >> station >> octant >> half  >> gap >> plane >> channel;

    // check if parsing went well
    if( stringbuf.rdstate() & ios::failbit )
    {
      cout << "MutArm::fetchAttenuatedChannels - problem parsing line " << linebuf << endl;
      continue;
    }

    if (arm==getArm())
    {
      if(plane==Cathode_2) plane=Cathode2;
      if(plane==Cathode1 || plane == Cathode2)
      {
        MutStrip *strip =
          f_pMutStations[station]->
          f_pMutOctants[octant]->
          f_pMutHalfOctants[half]->
           f_pMutGaps[gap]->
          f_pMutPlanes[plane]->
          f_pMutStrips[channel];

        if(strip) strip->setAttenuationFlag(true);
      } else {
        cout<< PHWHERE<<"..Skipping wire plane: "<<endl;
      }
    }
  }
}

//________________________________________________________________
void MutArm::fetchAttenuatedChannels(PHTimeStamp &time_stamp, PdbBankID bank_id)
{
  const string bank_name = ( getArm() == South ) ? "mut.south.attenuatedstrips" :  "mut.north.attenuatedstrips";
  const string class_name = "PdbMutCalibStripBank";

  if( !fetch( class_name.c_str(), time_stamp, bank_name.c_str(), bank_id ) )
  {
    cout << "MutArm::fetchAttenuatedChannels - fetch failed." << endl;
    cout << "MutArm::fetchAttenuatedChannels - there are no attenuated strips for "<< name<<" in the database." << endl;
    return;
  }

  cout<<"MutArm::fetchAttenuatedChannels - " << name << ": fetching attenuated strips from database. " << endl;

  PdbMutCalibStrip *badStrip;
  int arm, station, octant, half, gap, plane, channel;

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::ATTENUATED_CHANNELS ) >= TMutDatabaseCntrl::MAX ) {
    MUTGEOM::PRINT( cout, "MutArm::fetchAttenuatedChannels" );
    cout << "MutArm::fetchAttenuatedChannels - arm station octant half gap plane channel" << endl;
  }

  unsigned int i=0;
  while(i<getBankLength()) {
    badStrip = (PdbMutCalibStrip*)&geometryBank->getEntry(i);
    arm = badStrip->getArm();
    station = badStrip->getStation();
    octant = badStrip->getOctant();
    half  = badStrip->getHalfOctant();
    gap = badStrip->getGap();
    plane = badStrip->getPlane();
    channel = badStrip->getStrip();

    i++;
    if (arm!=getArm()) continue;

    if(plane==Cathode_2) plane=Cathode2;
    if(plane==Cathode1 || plane == Cathode2)
    {

      // dump
      if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::ATTENUATED_CHANNELS ) >= TMutDatabaseCntrl::MAX )
      cout << "MutArm::fetchAttenuatedChannels - "
        << arm << " " << station << " " << octant << " " << half  << " "
        << gap << " " << plane << " " << channel << endl;


      MutStrip *strip =
        f_pMutStations[station]->f_pMutOctants[octant]->f_pMutHalfOctants[half]->
        f_pMutGaps[gap]->f_pMutPlanes[plane]->f_pMutStrips[channel];

      if(strip) strip->setAttenuationFlag(true);
    }

  }

  cout<<"MutArm::fetchAttenuatedChannels - there are "<<i<<" attenuated channels. " << endl;
  if(!commit()) cout << "MutArm::fetchAttenuatedChannels - commit failed. " << endl;
}

//________________________________________________________________
bool MutArm::updateAttenuatedChannels(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file)
{

  // check input file
  ifstream in(file);
  if (!in.good()){
    cout << "MutArm::updateAttenuatedChannels - error opening file " << file << endl;
    return false;
  }

  // parse the file
  list< PdbMutCalibStrip > geom_objects;
  char line[512];
  while( !( in.rdstate() & ios::failbit ) )
  {


    in.getline( line, 512, '\n' );

    // skip empty lines or comments
    if( !strlen( line ) ) continue;
    if( !strncmp( line, "//", 2 ) == 0 ) continue;

    int arm(0), station(0), octant(0), half(0), gap(0), plane(0), channel(0);
    istringstream line_stream( line );
    line_stream >> arm >> station >> octant >> half >> gap >> plane >> channel;

    // check stream
    if( line_stream.rdstate() & ios::failbit )
    {
      cout << "MutArm::updateAttenuatedChannels - unable to parse line " << line;
      continue;
    }

    // skip if arm does not match
    if( arm != getArm() ) continue;

    // book new object
    PdbMutCalibStrip badstrip;
    badstrip.setArm(arm);
    badstrip.setStation(station);
    badstrip.setOctant(octant);
    badstrip.setHalfOctant(half);
    badstrip.setGap(gap);
    badstrip.setPlane(plane);
    badstrip.setStrip(channel);
    geom_objects.push_back( badstrip );
  }

  cout<<"MutArm::updateAttenuatedChannels - created " << geom_objects.size() << " objects." << endl;

  const string bank_name = (getArm() == South ) ? "mut.south.attenuatedstrips":"mut.north.attenuatedstrips";
  const string class_name = "PdbMutCalibStripBank";

  if(!update( class_name.c_str(), time_start, time_stop, bank_name.c_str(), bank_id, descrip ) )
  {
    cout<<"MutArm::updateAttenuatedChannels - failed to open database ."<<endl;
    return false;
  }
  geometryBank->setLength( geom_objects.size() );
  cout<<"MutArm::updateAttenuatedChannels - database opened."<<endl;

  // update bank entries with stored objects
  unsigned int count = 0;
  for( list<PdbMutCalibStrip>::const_iterator iter = geom_objects.begin(); iter != geom_objects.end(); iter++, count++ )
  {
    cout << "MutArm::updateAttenuatedChannels - writing entry " << count << endl;
     PdbMutCalibStrip* entry = static_cast<PdbMutCalibStrip*>( &geometryBank->getEntry(count) );
     *entry = *iter;
  }

  // commit
  if(!commit()) {
    cout<<"MutArm::updateAttenuatedChannels - commit failed." << endl;
    return false;
  }

  return true;
}

//________________________________________________________________
bool MutArm::updatePisaGeom(const char* PisaOutput)
{
  ofstream s(PisaOutput, ios::out | ios::app);
  if (!s){
    cout << "Error opening file " << PisaOutput << endl;
    s.close();
    return false;
  }

  s << name <<endl;

  /*
    The survey positions are used to correct the geometry used
    in simulation.   The survey points are used to determine the origin
    and rotation of our geometry objects in PISA.
    For an explanation of the rotation matrix angles see
    http://wwwinfo.cern.ch/asdoc/geant_html3/node118.html#SECTION049000000000000000000000
  */

  /*
    These are used to calculate offsets of octant survey points from
    pisa origins.    Stations 2 and 3 have different numbers for north
    and south. Station 2 has different number for front and rear
    octants, as well.
    Station1 Octants 2,4,6,8 (fOctantNums 1,3,5,7) need sign
    flipped for x-axis (b).
    !!!In these arrays, it is hardwired that South == 0!
  */
  double a[2][6] = {
      {-3.878, -3.878, 213.6408, 222.733, 317.7564, 317.7564},
      {-3.878, -3.878, 252.2139, 261.0331, 421.0856, 421.0856}
    };
  double b[2][6] = {
      {115.008, -115.008, 0.0, 0.0, 76.1151, 76.1151},
      {115.008, -115.008, 0.0, 0.0, 101.9532, 101.9532}
    };

  MutOctant *p_octant;
  PHPoint octPisaPosition;
  PHFrame globalFrame;
  PHMatrix rotation;
  PHVector translation;
  double theta1, phi1, theta2, phi2, theta3, phi3;

  for (int i=0; i<NumberOfStations; i++) {

    s << f_pMutStations[i]->name <<"\t GSROTM info"<< endl;
    s << "Reminder: Octant 2 angles are used to define quadrant 1!, etc."
      << endl;
    for (int j=0; j<NumberOfOctants; j++)
    {
      p_octant = f_pMutStations[i]->f_pMutOctants[j];
      if(p_octant)
      {
        //take an octant frame defined by its survey points and find the
        //rotation matrix to transform from octant frame to globalframe.
        frames2MatrixAndVector(p_octant->octFrame,globalFrame, rotation,translation);
        /*
          The angles from the rotation matrix are used as pisa input
          to rotate the pisa octants.  Of course the logical survey
          coordinate systems are different than the logical pisa
          coordinate systems, so the angles must then be adjusted.
        */
        theta1=acos(rotation.getA02())*180/Pi;
        phi1 = acos(rotation.getA00())*180/Pi;
        theta2=acos(rotation.getA12())*180/Pi;
        phi2 = acos(rotation.getA10())*180/Pi;
        theta3=acos(rotation.getA22())*180/Pi;
        phi3 = acos(rotation.getA20())*180/Pi;

        if(i!=Station1) {
          phi1 = phi1 + 112.5;
          phi2 = phi2 + 112.5;
        }
        if(phi1>360) phi1 = phi1 - 360;
        if(phi2>360) phi2 = phi2 - 360;

        int armIndx = getArm();
        octPisaPosition = p_octant->getGlobalPosition() +
          p_octant->octFrame.getV() * a[armIndx][2*i+getOctant()%2] +
          p_octant->octFrame.getU() * b[armIndx][2*i+getOctant()%2];

        s << p_octant->name <<"\t x \t\t y \t\t z " << endl;
        s << "\t \t" << octPisaPosition.getX() << "\t"
          << octPisaPosition.getY() << "\t"
          << octPisaPosition.getZ() << endl;
        s << theta1 << " "<< phi1 << " "<< theta2
          << " "<< phi2 << " "<< theta3
          << " " <<phi3 << endl;
      }
    }
  }
  s.close();

  return true;
}

//________________________________________________________________
int MutArm::convertPisaHitToChamber(PHPoint PisaCoordinate, MutWire *ptrWire[1], double &wireIP, MutStrip *ptrStrip[2], double stripIP[2])
{
  //initialize pointers

  bool foundStation = false;
  ptrStrip[0] = 0;
  ptrStrip[1] = 0;
  ptrWire[0] = 0;
  wireIP = 0;

  int j=-1;
  while(!foundStation && j<2) {

    //Locate the Station that contains this hit.

    j++;
    PHPoint stationPosition = f_pMutStations[j]->getGlobalPosition();
    float zDiff = abs(PisaCoordinate.getZ()-stationPosition.getZ());
    if(zDiff<100.00) foundStation = true;

  }
  if(!foundStation) return 5;
  return f_pMutStations[j]->convertPisaHitToChamber(PisaCoordinate, ptrWire, wireIP, ptrStrip, stripIP);

}

//________________________________________________________________
int MutArm::getAnode(PHPoint hitCoordinate, int &station, int &octant, int &gap)
{
  MutWire *ptrWire[1];
  double wireIP;
  MutStrip *ptrStrip[2];
  double stripIP[2];

  if(convertPisaHitToChamber(hitCoordinate, ptrWire, wireIP, ptrStrip, stripIP)!=0)
  return -1;

  int wireNum = ptrWire[0]->getWire();
  int anode = -1;
  int HVchannel = -1;

  if(getArm()==South){
    if(ptrWire[0]->getStation()==Station1){
      if(wireNum<2) HVchannel = 0;
      else if(wireNum<6) HVchannel = 1;
      else HVchannel = wireNum/16 + 2;
      anode = HVchannel;
    }
    else if(ptrWire[0]->getStation()==Station2){
      if(wireNum<32) HVchannel = 0;
      else HVchannel = wireNum/32 - 1;
      anode = wireNum/32;
    }
    else { //Station 3
      HVchannel = wireNum/64;
      anode = wireNum/32;
    }
  }
  else {
      // North Arm has not been implemented for this function.
    return -1;
  }

  station = ptrWire[0]->getStation();
  octant = ptrWire[0]->getOctant();
  gap = ptrWire[0]->getGap();
  return anode;
}

//________________________________________________________________
void MutArm::dumpDCMChannels(const char *outFile)
{
  ofstream outfile(outFile);
  if (!outfile){
    cout<<"Error creating file. " << endl;
    return;
  }

  MutPlane *pPlane;
  MutStrip *pStrip;
  int packet, channel;

  for(int station=0; station<NumberOfStations; station++)
  for(int octant=0; octant<NumberOfOctants; octant++)
  for(int half=0; half<NumberOfHalfOctants; half++)
  for(int gap=0; gap<f_pMutStations[station]->getNumberOfGaps(); gap++)
  for(int plane=0; plane<NumberOfPlanes; plane+=2) {

    pPlane=f_pMutStations[station]->f_pMutOctants[octant]->f_pMutHalfOctants[half]->f_pMutGaps[gap]->f_pMutPlanes[plane];
    for(int strip=0; strip<pPlane->getNumElements(); strip++) {
      pStrip = pPlane->f_pMutStrips[strip];
      if(pStrip){
        packet = pStrip->getPacket_ID();
        channel = pStrip->getDCMChannel();
        outfile
          << packet << "\t" << channel << "\t" << getArm() << "\t" << station
          <<"\t"<< octant << "\t" << half << "\t" << gap << "\t" << plane << "\t"
          << strip << "\t" << endl;
      }
    }
  }

  outfile<<"Packet Chan Arm station Octant Half Gap Plane Strip"<<endl;

}

//______________________________________________________________________________________________
istream& operator >> (istream& in, MutArm::LandauParameters& par )
{
  in >> par._arm >> par._station >> par._octant >> par._gap;
  in >> par._offset >> par._scale;
  return in;
}

//______________________________________________________________________________________________
ostream& operator << (ostream& out, const MutArm::LandauParameters& par )
{
  out << par._arm << " " << par._station << " " << par._octant << " " << par._gap;

  char formated_out[512];
  sprintf( formated_out, "%8.5f %8.5f", par._offset, par._scale );
  out << "    " << formated_out << endl;
  return out;
}

//______________________________________________________________________________________________
istream& operator >> (istream& in, MutArm::GlobalAligConst& alig )
{
  in
    >> alig.arm >> alig.station >> alig.octant
    >> alig.ds >> alig.dr >> alig.dz
    >> alig.roll >> alig.pitch >> alig.yaw
    >> alig.dExpansion;
  return in;
}

//______________________________________________________________________________________________
ostream& operator << (ostream& out, const MutArm::GlobalAligConst& alig )
{
  out << alig.arm << " " << alig.station << " " << alig.octant << "  ";

  char formated_out[512];
  sprintf( formated_out, "%8.5f    %8.5f   %5.2f", alig.ds, alig.dr, alig.dz );
  out << "    " << formated_out;

  // dump rotations
  sprintf( formated_out, "%8.5f    %8.5f   %8.5f", alig.roll, alig.pitch, alig.yaw );
  out << "    " << formated_out;

  // and expension
  out << "    " << alig.dExpansion << endl;

  return out;
}

//______________________________________________________________________________________________
istream& operator >> (istream& in, MutArm::InternalAligConst& alig )
{
  in
    >> alig.arm >> alig.station >> alig.octant >> alig.half  >> alig.gap >> alig.plane
    >> alig.ds >> alig.dr >> alig.dz
    >> alig.roll >> alig.pitch >> alig.yaw
    >> alig.dExpansion;
  return in;
}

//______________________________________________________________________________________________
ostream& operator << (ostream& out, const MutArm::InternalAligConst& alig )
{
  out << alig.arm << " " << alig.station << " " << alig.octant << "  ";
  out << alig.half << " " << alig.gap << " " << alig.plane << "  ";

  char formated_out[512];
  sprintf( formated_out, "%8.5f    %8.5f   %5.2f", alig.ds, alig.dr, alig.dz );
  out << "    " << formated_out;

  // dump rotations
  sprintf( formated_out, "%8.5f    %8.5f   %8.5f", alig.roll, alig.pitch, alig.yaw );
  out << "    " << formated_out;

  // and expension
  out << "    " << alig.dExpansion << endl;

  return out;
}
