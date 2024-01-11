// Created by: Federica Ceretto at Tue Feb 16 13:11:37 1999

//INCLUDECHECKER: Removed this line: #include "PdbParameter.hh"
#include "PHCalibrationObject.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

PHCalibrationObject::PHCalibrationObject(PHAddressObject *add)
{
  address = add;
  committed=1;
  bankManager = PdbBankManager::instance();
  application = bankManager->getApplication();  
  calibrationBank = 0;
}

PHCalibrationObject::~PHCalibrationObject()
{
  commit();
}

PHBoolean
PHCalibrationObject::commit()
{
  if (committed == 0) 
    {
      application->commit();
      committed = 1;
    }
  
  return True;
}

void
PHCalibrationObject::print()
{
  if (calibrationBank) 
    {
      calibrationBank->printHeader();
    }
}

PHBoolean
PHCalibrationObject::validate(PHTimeStamp& Tsearch)
{
  if (Tsearch >= getStartValTime() &&
      Tsearch < getEndValTime() ) 
    {
      return True;       
    }

  return False;
}

const PHTimeStamp
PHCalibrationObject::getStartValTime() const
{
  return start;
}

const PHTimeStamp
PHCalibrationObject::getEndValTime() const
{
  return stop;
}

const PdbParameter *
PHCalibrationObject::get(int value) const
{
  return (PdbParameter *) &calibrationBank->getEntry(value);
} 

int
PHCalibrationObject::getBankLength() const
{
  return calibrationBank->getLength();
}
