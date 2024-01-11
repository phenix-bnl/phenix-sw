#ifndef __PHENIX_ZDCCALIBPAR_HH__
#define __PHENIX_ZDCCALIBPAR_HH__

#include <Zdc.hh>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PHTimeStamp.h>
#include <PHString.h>

#include <PdbPmtPeak.hh>
#include <PdbPmtFitPar.hh>
#include <PdbZdcLUT.hh>

#include <msg_profile.h>
#include <msg_control.h>

#include <iostream>
#include <ctime>
#include <fstream>
#include <string>

// eventually we will have two boards
static const int MAX_ENTRIES = 40;

template <class T>
class ZdcCalibPar
{
public:

    ZdcCalibPar();

    int store(const PHTimeStamp& time, const char* type, const char *description = "", const char *name = "");
    int restore(const PHTimeStamp& time, const char* type);
    int restore(const char* filename, const char* type);
    int restore(const char* type);

    int storelut(const PHTimeStamp& time, const char* type, const char *description = "", const char *name = "");
    int restorelut(const PHTimeStamp& time, const char* type);
    int restorelut(const char* filename, const char* type);

    void setPrevious();

    T* getCalibPar(int ipmt) { return &calibpar[ipmt]; }
    void setCalibPar(int ipmt, T value) { calibpar[ipmt] = value; }
    T* getPrevCalib(int ipmt) { return &prevcalib[ipmt]; }
    void setPrevCalib(int ipmt, T value) { prevcalib[ipmt] = value; }
    PHTimeStamp* getStartTime() { return &StartTime; }
    PHTimeStamp* getEndTime() { return &EndTime; }
    PdbZdcLUT* getCalibParLUT(int ipmt) { return &caliblut[ipmt]; }

    void print();
    void dump(const char *type);

private:
    int send_message(const int severity, const std::string &err_msg) const;
    int CalibCondition;
    PHTimeStamp StartTime;
    PHTimeStamp EndTime;
    char User[10];
    PdbBankID bankID;
    int num_entries;	// the number of entries in the database
    T calibpar[MAX_ENTRIES];
    T prevcalib[MAX_ENTRIES];
    PdbZdcLUT caliblut[8];
};

// Because of a limitation of g++, one cannot separate compilation of
// a template class. All memeber function should be defined in the
// header file.
template <class T>
ZdcCalibPar<T>::ZdcCalibPar()
{
  num_entries = 8;
}

// What is this for?
template <class T>
void
ZdcCalibPar<T>::setPrevious()
{
  for (int ipmt = 0;ipmt < num_entries; ipmt++)
    {
      prevcalib[ipmt] = calibpar[ipmt];
    }
}

// Restore calibrations from database
template <class T>
int
ZdcCalibPar<T>::restore(const PHTimeStamp& time, const char* type)
{
  PHString classname;

  if (strcmp(type, "pedestal") == 0 || 
      strcmp(type, "overflow0") == 0 ||
      strcmp(type, "overflow1") == 0 || 
      strcmp(type, "pmtgain") == 0 || 
      strcmp(type, "tzero") == 0 ||
      strcmp(type, "zvtx") == 0 ||
      strcmp(type, "smdoffset") == 0)
    {
      classname = "PdbPmtPeakBank";
    }
  else if (strcmp(type, "adc") == 0 ||
	   strcmp(type, "tdc0") == 0 || 
	   strcmp(type, "tdc1") == 0 ||
	   strcmp(type, "slewpar0") == 0 || 
	   strcmp(type, "slewpar1") == 0)
    {
      classname = "PdbPmtFitParBank";
    }
  else if (strcmp(type, "tdc0lut") == 0 || 
	   strcmp(type, "tdc1lut") == 0)
    {
      classname = "PdbZdcLUT";
    }
  else
    {
      std::cout << "ZdcCalibPar: unknown type " << type << std::endl
	   << "             failed database lookup" << std::endl;
      return -1;
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  if ( bankManager==0 )
    {
      std::cout << "ZdcCalibPar: Failed to get bankManager" << std::endl;
      return -1;
    }

  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  if ( application==0 )
    {
      std::cout << "ZdcCalibPar: Failed to get PdbApplication" << std::endl;
      return -1;
    }

  if ( application->startRead() )
    {
      PHTimeStamp tSearch = time;
      PdbBankID bankID;
      bankID.setInternalValue(0);

      PHString TmpName("calib.zdc.");
      PHString TypeName(type);
      PHString calibname = TmpName + TypeName;

      std::cout << calibname.getString() << std::endl;
      PdbCalBank *zdcBank = bankManager->fetchBank(classname.getString(), 
						   bankID, 
						   calibname.getString(), 
						   tSearch);
      if (zdcBank)
        {
          zdcBank->printHeader();
	  num_entries = zdcBank->getLength();
          StartTime = zdcBank->getStartValTime();
          EndTime = zdcBank->getEndValTime();
          for (unsigned int i = 0; i < zdcBank->getLength(); i++)
            {
              calibpar[i] = (T &)zdcBank->getEntry(i);
            }

	  delete zdcBank;
        }
      else
        {
          std::cout << "main()" << std::endl
	       << "\tError:" << std::endl
	       << "\tbankManager returned zero-pointer" << std::endl;
          return 1;
        }
      application->commit();
    }
  else
    {
      application->abort();
      std::cout << "Transaction aborted." << std::endl;
      return 1;
    }

  return 0;
}

/// Store calibrations into database
template <class T>
int
ZdcCalibPar<T>::store(const PHTimeStamp& time, 
		      const char* type, 
		      const char *description, 
		      const char *name)
{
  PHString classname;

  if (strcmp(type, "pedestal") == 0 || 
      strcmp(type, "overflow0") == 0 ||
      strcmp(type, "overflow1") == 0 || 
      strcmp(type, "pmtgain") == 0 || 
      strcmp(type, "tzero") == 0 ||
      strcmp(type, "zvtx") == 0 ||
      strcmp(type, "smdoffset") == 0)
    {
      classname = "PdbPmtPeakBank";
    }
  else if (strcmp(type, "adc") == 0 ||
	   strcmp(type, "tdc0") == 0 || 
	   strcmp(type, "tdc1") == 0 ||
	   strcmp(type, "slewpar0") == 0 || 
	   strcmp(type, "slewpar1") == 0)
    {
      classname = "PdbPmtFitParBank";
    }
  else if (strcmp(type, "tdc0lut") == 0 || 
	   strcmp(type, "tdc1lut") == 0)
    {
      classname = "PdbZdcLUT";
    }
  else
    {
      std::cout << "ZdcCalibPar: unknown type " << type << std::endl
	   << "             failed database storage" << std::endl;
      return -1;
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  if ( bankManager==0 )
    {
      std::cout << "ZdcCalibPar: Failed to get bankManager" << std::endl;
      return -1;
    }
  PdbApplication *application = bankManager->getApplication();
  if ( application==0 )
    {
      std::cout << "ZdcCalibPar: Failed to get PdbApplication" << std::endl;
      return -1;
    }

  std::cout << "Opening FD in update mode.." << std::endl;
  if (application->startUpdate())
    {
      PHTimeStamp tStart = time;
      PHTimeStamp tStop = PHTimeStamp(2020, 1, 1, 0, 0, 0);
      PdbBankID bankID;

      PHString TmpName("calib.zdc.");
      PHString TypeName(type);
      PHString calibname = TmpName + TypeName;

      PdbCalBank *prevBank = bankManager->fetchBank(classname.getString(), bankID, calibname.getString(), tStart);

      if (prevBank)
        {
          std::cout << " overlapping bank found. Changing the EndValTime of it " << std::endl;
          tStop = prevBank->getEndValTime();
          prevBank->setEndValTime(time);
          std::cout << " and setting current EndValTime to " << tStop << std::endl;
	  delete prevBank;
        }

      // Inputs for calibration bunk header informations.
      PHString Description;
      if ( strlen(description) == 0 )
        {
          char tempdescription[240];
          std::cout << "Please input description for this calibration parameter:" << std::endl;
          std::cin.getline(tempdescription, 240);
	  Description = tempdescription;
        }
      else
        {
          Description = description;
	}

      PHString UserName;
      if ( strlen(name) == 0 )
        {
          char tempname[20];
	  std::cout << "Please enter your name:" << std::endl;
          std::cin.getline(tempname, 20);
          UserName = tempname;
        }
      else
	{
	  UserName = name;
	}

      // why do we always use internal value 0?
      bankID.setInternalValue(0);

      PdbCalBank *zdcBank = bankManager->createBank(classname.getString(),
						    bankID,
						    Description.getString(),
						    tStart,
						    tStop,
						    calibname.getString());

      zdcBank->setLength(num_entries);
      zdcBank->setUserName(UserName);

      for (unsigned int i = 0; i < zdcBank->getLength(); i++)
        {
          T* entry = (T*) & (zdcBank->getEntry(i));
          *entry = calibpar[i];
        }
      application->commit();
    }
  else
    {
      std::cout << "failed to start application for update" << std::endl;
      return 1;
    }
  return 0;
}

/// Print out the calibration info
template <class T>
void
ZdcCalibPar<T>::print()
{
  for (int ipmt = 0;ipmt < num_entries; ipmt++)
    {
      std::cout << "*** channel " << ipmt << " ***" << std::endl;
      calibpar[ipmt].print();
    }
}

template <class T>
int
ZdcCalibPar<T>::send_message(const int severity, const std::string &err_message) const
{
  msg_control *Message = new msg_control(MSG_TYPE_OFFLINE,
                                         MSG_SOURCE_ZDC,
                                         severity, "ZdcCalibPar");
  std::cout << *Message << err_message << std::endl;
  delete Message;
  return 0;
}

#endif /*  __PHENIX_ZDCCALIBPAR_HH__ */


