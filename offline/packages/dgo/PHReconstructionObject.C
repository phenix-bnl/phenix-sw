// Created by: Federica Messer at Mon Dec 27 13:11:37 1999

#include "PHReconstructionObject.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

PHReconstructionObject::PHReconstructionObject()
{
  bankManager = PdbBankManager::instance();
  application = bankManager->getApplication();  
}

PHReconstructionObject::~PHReconstructionObject()
{
  commit();
}

PHBoolean
PHReconstructionObject::commit()
{
  if (committed == 0) 
    {
      application->commit();
      committed = 1;
    }

  return True;
}

void
PHReconstructionObject::print()
{
  recoBank->printHeader();
}

PHBoolean
PHReconstructionObject::validate(PHTimeStamp& Tsearch)
{
  if (Tsearch > getStartValTime()  &&
      Tsearch < getEndValTime()) 
    {
      PHMessage("PHReconstructionObject::validate",PHHullo,"Database still valid ");
      return True;       
    }
  PHMessage("PHReconstructionObject::validate",PHHullo,"Fetch new database");
  return False;
}

const PHTimeStamp
PHReconstructionObject::getStartValTime() const
{
  if (recoBank) 
    {
      return recoBank->getStartValTime();
    }
  
  return start;
}
    
const PHTimeStamp
PHReconstructionObject::getEndValTime()   const
{
  if (recoBank) 
    {
      return recoBank->getEndValTime();
    }
  
  return stop;
}
