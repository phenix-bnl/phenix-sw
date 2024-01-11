#include "MuiGeomClasses.hh"
#include "MuiMapInit.h"
#include "TMuiAddressTable.hh"
#include "PHIODataNode.h"
#include "PHTable.hh"
#include "mMuiInitModule.h"

#include <fstream>
#include <iostream>

using namespace std;

//_____________________________________________________________
mMuiInitModule::mMuiInitModule(): 
  fSearchTime(2000,4,2,0,0,0), 
  fIsUsingDatabase(true)
{}

//_____________________________________________________________
void mMuiInitModule::SetSearchTimeStamp(const PHTimeStamp& t_search)
{
  
  fSearchTime = t_search;
  cout << "mMuiInitModule::SetSearchTimeStamp - " << fSearchTime << endl;
  return;
  
}

//_____________________________________________________________
void mMuiInitModule::reset( void ) const
{
  
  // clear geometry and address table
  TMuiGeometry::Geom()->Clear();
  TMuiAddressTable::Table()->Clear();
  
}

//_____________________________________________________________
void mMuiInitModule::initialize( void )
{

  if (fIsUsingDatabase) 
  {
    
    PdbBankID bankID(42);
    string calibname1( "map.mui.mui_address" );
    string calibname2( "geom.mui." );
  
    TMuiGeometry::Geom()->fetch(fSearchTime, calibname2.c_str(), bankID);
    TMuiAddressTable::Table()->fetch(fSearchTime, calibname1.c_str(), bankID);
  
  }else{
  
    TMuiGeometry::Geom()->Init();
    TMuiAddressTable::Table()->Init();
  
  }

  return;
}

//_____________________________________________________________
bool mMuiInitModule::event( PHCompositeNode* )
{ 
  initialize(); 
  return true;
}
  
//_____________________________________________________________
bool mMuiInitModule::dumpmaps()
{
  ofstream foutG("TMuiGeom.dat");
  foutG << *TMuiGeometry::Geom() << endl;
  foutG.close();

  ofstream foutA("TMuiAddressTable.dat");
  foutA << *TMuiAddressTable::Table() << endl;
  foutA.close();

  // Variable declaration of the array to hold the mapping of
  // arm/gap/panel/orientation/twopack into logical tube number.
  // Declared to here to hold value in ouer scope.
  // ===========================================================
  hashVector<TMuiChannelId, int> map(TMuiChannelId::kTwoPacksMaxTotal, &TwoPackHash);

  // Initialize array to map arm/gap/panel/orientation/twopack 
  // into logical tube number.
  // =========================================================
  MuiMapInit(map);

  return True;
}
