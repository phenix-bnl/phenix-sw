// Created by: Federica Messer at Tue Feb 16 13:11:37 1999

#include "PdbCoordinate.hh"
#include "PHGeometryObject.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

PHGeometryObject::PHGeometryObject()
{
  address = 0;
  committed=1;

  bankManager = PdbBankManager::instance();  
  application = bankManager->getApplication();  
}

PHGeometryObject::PHGeometryObject(PHAddressObject *add)
{
  address = add;
  committed=1;
  bankManager = PdbBankManager::instance();
  application = bankManager->getApplication();  
}

PHGeometryObject::~PHGeometryObject()
{
  commit();
}

PHBoolean
PHGeometryObject::commit()
{
  if (committed == 0) 
    {
      application->commit();
      committed = 1;
    }

  return True;
}

void 
PHGeometryObject::print()
{
  if (geometryBank) 
    {
      geometryBank->printHeader();
    }
}

PHBoolean 
PHGeometryObject::validate(PHTimeStamp& Tsearch)
{
  if (Tsearch >= getStartValTime()  &&
      Tsearch < getEndValTime()) 
    {
      return True;       
    }
  
  return False;
}

const PHTimeStamp
PHGeometryObject::getStartValTime() const
{
  return start;
}

const PHTimeStamp
PHGeometryObject::getEndValTime()   const
{
  return stop;
}

const PdbCalChan * 
PHGeometryObject::get(int index) const
{
  return (PdbCalChan *) &geometryBank->getEntry(index);
} 

int
PHGeometryObject::getBankLength()
{
  return geometryBank->getLength();
}
