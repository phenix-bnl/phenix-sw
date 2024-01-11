// Created by: Federica Messer at Mon Dec 27 13:11:37 1999

#include "PHNoiseObject.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

PHNoiseObject::PHNoiseObject()
{
  address = 0;
  bankManager = PdbBankManager::instance();
  application = bankManager->getApplication();  
}

PHNoiseObject::PHNoiseObject(PHAddressObject *add)
{
  address = add; 
  bankManager = PdbBankManager::instance();
  application = bankManager->getApplication();  
}

PHNoiseObject::~PHNoiseObject()
{
  commit();
}

PHBoolean
PHNoiseObject::commit()
{
  if (committed == 0) 
    {
      application->commit();
      committed = 1;
    }

  return True;
}

void
PHNoiseObject::print()
{
   noiseBank->printHeader();
}

PHBoolean
PHNoiseObject::validate(PHTimeStamp& Tsearch)
{
  if (Tsearch >= getStartValTime()  &&
      Tsearch < getEndValTime()) 
    {
      return True;       
    }

  return False;
}

const PHTimeStamp
PHNoiseObject::getStartValTime() const
{
  return start;
}

const PHTimeStamp
PHNoiseObject::getEndValTime() const
{
  return stop;
}

const PdbCalChan *
PHNoiseObject::get(int index) const
{
  return (PdbCalChan*)&noiseBank->getEntry(index);
}

int
PHNoiseObject::getBankLength()
{
  return noiseBank->getLength();
}
