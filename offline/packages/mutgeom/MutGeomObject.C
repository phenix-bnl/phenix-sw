
/*!
  \file MutGeomObject.C
  \brief Describes a GeomObject of the muon tracker system. 
  \author Douglas Fields, Nicki Bruner, Hugo Pereira
  \version $Revision: 1.26 $
  \date $Date: 2009/05/27 00:43:23 $
*/

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <iostream>

#include "MutGeomObject.h"

using namespace std;

//___________________________________________________
MutGeomObject::MutGeomObject():
  fGlobalPosition( PHPoint( 0,0,0 ) ),
  fGlobalVector( PHVector( 0,0,0 ) ),
  dbState( UNINITIALIZED ),
  bankManager( PdbBankManager::instance() ),
  application( bankManager->getApplication() ),
  geometryBank( 0 ),
  name( "generic Mut Geometry Object" )
{}


//___________________________________________________
MutGeomObject::MutGeomObject(const MutGeomObject& rhs):
  fGlobalPosition( rhs.fGlobalPosition ),
  fGlobalVector( rhs.fGlobalVector ),
  dbState( UNINITIALIZED ),
  bankManager( PdbBankManager::instance() ),
  application( bankManager->getApplication() ),
  geometryBank( 0 ),
  name( rhs.name )
  {}


//___________________________________________________
MutGeomObject::~MutGeomObject()
{ if( dbState != UNINITIALIZED ) commit(); }

//___________________________________________________
bool MutGeomObject::fetch(
  const char* className, 
  PHTimeStamp &Tsearch, 
  const char *calibname,
  const PdbBankID& bankID)
{
  // check database current state
  if( dbState == UNINITIALIZED ) { //from PHGeometryObject                   

    if(!application->startRead()) {
      cout<<"Aborting geometry database fetch ... Database not readable\n";
      application->abort();
      return false;
    } else dbState = FETCH_OPEN;

  } else if( dbState == UPDATE_OPEN ) {
    cout<<"Aborting geometry database fetch ... Database open for update.\n";
    return false;
  }
  
  //the class name isn't actually used in fetch
  geometryBank = bankManager->fetchBank( className, bankID, calibname, Tsearch );
  return (geometryBank != 0 );
}

//___________________________________________________
bool MutGeomObject::update( 
  const char* className, PHTimeStamp &Tstart, PHTimeStamp &Tstop,
  const char* bankName, const PdbBankID& bankID, 
  const char* descrip)
{
  if( dbState == UNINITIALIZED) {              
    
    if(!application->startUpdate()) {
      cout<<"MutGeomObject::update - Database not writable.\n";
      application->abort();
      return false;
    } else dbState = UPDATE_OPEN;
    
  } else if( dbState == FETCH_OPEN ) {
    cout<<"MutGeomObject::update - Database open for fetch.\n";
    return true;
  }    
 
  geometryBank = bankManager->createBank(className,bankID,descrip, Tstart,Tstop,bankName);
  return (geometryBank != 0);
  
}

//___________________________________________________
bool MutGeomObject::commit()
{

  if( dbState == UPDATE_OPEN && geometryBank ) 
  {
  
    application->commit( geometryBank );
    delete geometryBank;
    geometryBank = 0;
    dbState = UNINITIALIZED;
    return true;
    
  } else if( dbState == FETCH_OPEN && geometryBank ) {
    
    delete geometryBank;
    geometryBank = 0;
    dbState = UNINITIALIZED;  
    return true;
  
  } else return false;
  
  return true;
}

//________________________________________________
void MutGeomObject::print_bank_summary( void ) const
{
  
  cout << "MutGeomObject::print_bank_summary - validity start: " << geometryBank->getStartValTime() << endl;
  cout << "MutGeomObject::print_bank_summary - validity end: " << geometryBank->getEndValTime() << endl;
  cout << "MutGeomObject::print_bank_summary - insert time: " << geometryBank->getInsertTime() << endl; 
  cout << "MutGeomObject::print_bank_summary - description: " << geometryBank->getDescription() << endl; 

}

//________________________________________________
unsigned int MutGeomObject::getBankLength() const
{ return (geometryBank) ? geometryBank->getLength():0; }

//___________________________________________________
void MutGeomObject::print() const
{
  cout << name <<"\n";
  cout << " position = ";
  fGlobalPosition.print();
  cout << " orientation vector = "; 
  fGlobalVector.print();
}

//___________________________________________________
void MutGeomObject::rotateThis(float rotAngle, char axisLabel)
{
  PHVector axis(0,0,0);

  switch(axisLabel){
    
    case 'x':
    axis.setX(1);
    break;
    
    case 'y':
    axis.setY(1);
    break;
    
    case 'z':
    axis.setZ(1);
    break;
    
    default:
    cout<< axis <<" does not specify either x, y, or z.\n";
    cout<<"Rotation not performed.\n";
    return;
  }

  PHMatrix rotation = PHGeometry::rotationMatrix(rotAngle, axis);

  fGlobalVector = rotation * fGlobalVector;
  PHVector tempV = fGlobalPosition;
  tempV = rotation * tempV;
  fGlobalPosition = tempV;
}

