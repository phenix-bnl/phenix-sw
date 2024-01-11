// Created by: Federica Ceretto at Wed Feb 17 12:55:46 1999

#include "PHAddressObject.h" 
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

PHAddressObject::PHAddressObject()
{
  committed   = 1; 
  addressBank = 0;
  bankManager = PdbBankManager::instance();
  application = bankManager->getApplication();  
}

PHAddressObject::~PHAddressObject()
{
 commit();
}

PHBoolean
PHAddressObject::commit()
{
  if (committed == 0) 
    {
      application->commit();
      committed = 1;
    }

  return True;
}

void 
PHAddressObject::print()
{
  if (addressBank) 
    {
      addressBank->printHeader();
    }
}

PHBoolean 
PHAddressObject::validate(PHTimeStamp &Tsearch)
{
  if (Tsearch >= getStartValTime() &&
      Tsearch < getEndValTime()) 
    {
      return True;       
    }

  return False;
}

const PHTimeStamp 
PHAddressObject::getStartValTime() const
{
   return start;
}

const PHTimeStamp 
PHAddressObject::getEndValTime() const
{
  return stop; 
}


const PdbIndex *  
PHAddressObject::get(int value) const
{
  return (PdbIndex *)& addressBank->getEntry(value);
} 

int 
PHAddressObject::getBankLength() const
{
  return addressBank->getLength();
}







