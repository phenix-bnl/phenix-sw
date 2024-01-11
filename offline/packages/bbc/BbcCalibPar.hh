#ifndef PHENIX_BBCCALIBPAR_HH
#define PHENIX_BBCCALIBPAR_HH

#include <Bbc.hh>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PHTimeStamp.h>
#include <PHString.h>

#include <PdbBbcConf.hh>
#include <PdbPmtPeak.hh>
#include <PdbPmtFitPar.hh>

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>

template < class T > 
class BbcCalibPar
{
public:

  BbcCalibPar ();

  int store (const PHTimeStamp & time, const char *type);

  int restore (const PHTimeStamp & time, const char *type, int version=0);
  int restore (const char *filename, const char *type);
  int restore (const char *type);

  void setPrevious ();

  T *getCalibPar (int ipmt)
  {
    return &calibpar[ipmt];
  }
  void setCalibPar (int ipmt, T value)
  {
    calibpar[ipmt] = value;
  }

  T *getPrevCalib (int ipmt)
  {
    return &prevcalib[ipmt];
  }
  void setPrevCalib (int ipmt, T value)
  {
    prevcalib[ipmt] = value;
  }

  PHTimeStamp *getStartTime ()
  {
    return &StartTime;
  }
  PHTimeStamp *getEndTime ()
  {
    return &EndTime;
  }

  void print ()
  {
  }

private:
  int CalibCondition;
  PHTimeStamp StartTime;
  PHTimeStamp EndTime;
  char User[10];
  PdbBankID bankID;
  T calibpar[BBC_N_PMT];
  T prevcalib[BBC_N_PMT];
};

// Because of a limitation of g++, one can not separate compilation of
// a template class. All member function should be defined in the
// header file.

template < class T > BbcCalibPar < T >::BbcCalibPar ()
{
}

template < class T > 
void BbcCalibPar < T >::setPrevious ()
{
  int i;

  for (i = 0; i < BBC_N_PMT; i++)
    {
      prevcalib[i] = calibpar[i];
    }
}

template < class T >
int BbcCalibPar < T >::restore (const PHTimeStamp & time, const char *type, int version)
{

  std::cout << "BbcCalib." << type << ":restore" << std::endl;

  PHString classname;

  if (strcmp (type, "pedestal")  == 0 || strcmp (type, "overflow0") == 0 ||
      strcmp (type, "overflow1") == 0 || strcmp (type, "pmtgain")   == 0 ||
      strcmp (type, "offset")    == 0 || strcmp (type, "tzero")     == 0 ||
      strcmp (type, "fakeped")   == 0 || strcmp (type, "threshold") == 0 ||
      strcmp (type, "timereso")  == 0 )
    {
      classname = "PdbPmtPeakBank";
    }
  else
    if (strcmp (type, "adc")      == 0 ||
	strcmp (type, "tdc0")     == 0 || strcmp (type, "tdc1")     == 0 ||
	strcmp (type, "slewpar0") == 0 || strcmp (type, "slewpar1") == 0   )
    {
      classname = "PdbPmtFitParBank";
    }
  else if (strcmp (type, "config") == 0)
    {
      classname = "PdbBbcConfBank";
    }

  //
  // Select Objy implementation.
  //
  PdbBankManager *bankManager = PdbBankManager::instance();
  //
  // Get application manager class.
  //
  PdbApplication *application = bankManager->getApplication ();

  std::cout << "Now opening FD in readonly mode.." << std::endl;
  if (application->startRead ())
    {

      PHTimeStamp tSearch = time;
      PdbBankID bankID;
      bankID.setInternalValue (version);

      PHString TmpName ("calib.bbc.");
      PHString TypeName (type);
      PHString calibname = TmpName + TypeName;

      PdbCalBank *bbcBank
	= bankManager->fetchBank (classname.getString (), bankID,
				  calibname.getString (), tSearch);

      if (bbcBank)
	{
	  bbcBank->printHeader ();
	  StartTime = bbcBank->getStartValTime ();
	  EndTime = bbcBank->getEndValTime ();

	  for (unsigned int i = 0; i < bbcBank->getLength (); i++)
	    {
	      calibpar[i] = (T &) bbcBank->getEntry (i);
	    }
	  delete bbcBank;
	}
      else
	{
	  std::cout << "main()" << std::endl;
	  std::cout << "\tError:" << std::endl;
	  std::cout << "\tbankManager returned zero-pointer" << std::endl;
	  return 1;
	}
      application->commit ();
    }
  else
    {
      application->abort ();
      std::cout << "Transaction aborted." << std::endl;
      return 1;
    }
  return 0;
}

template < class T >
int 
BbcCalibPar < T >::store (const PHTimeStamp & time, const char *type)
{
  std::cout << "BbcCalib." << type << ":stored " << std::endl;

  PHString classname;

  if (strcmp (type, "pedestal")  == 0 || strcmp (type, "overflow0") == 0 ||
      strcmp (type, "overflow1") == 0 || strcmp (type, "pmtgain")   == 0 ||
      strcmp (type, "offset")    == 0 || strcmp (type, "tzero")     == 0 ||
      strcmp (type, "fakeped")   == 0 || strcmp (type, "threshold") == 0 ||
      strcmp (type, "timereso")  == 0 )
    {
      classname = "PdbPmtPeakBank";
    }
  else
    if (strcmp (type, "adc")      == 0 ||
	strcmp (type, "tdc0")     == 0 || strcmp (type, "tdc1")     == 0 ||
	strcmp (type, "slewpar0") == 0 || strcmp (type, "slewpar1") == 0   )
    {
      classname = "PdbPmtFitParBank";
    }
  else if (strcmp (type, "config") == 0)
    {
      classname = "PdbBbcConfBank";
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication ();

  std::cout << "Opening FD in update mode.." << std::endl;
  if (application->startUpdate ())
    {

      PHTimeStamp tStart = time;
      PHTimeStamp tStop = PHTimeStamp (2020,12,31, 0, 0, 0);
      PdbBankID bankID;
      bankID.setInternalValue (1);

      PHString TmpName ("calib.bbc.");
      PHString TypeName (type);
      PHString calibname = TmpName + TypeName;

      PdbCalBank *prevBank
	= bankManager->fetchBank (classname.getString (), bankID,
				  calibname.getString (), tStart);

      if (prevBank)
	{
	  std::cout << " overlapping bank found. Change the EndValTime of it " <<
	    std::endl;
	  prevBank->setEndValTime (time);
	}

      // Inputs for calibration bank header.
      std::string descrip;
      std::cout << " " << std::endl;
      std::cout << "Please input description for this calibration parameter"
		<< std::endl;
      std::getline(std::cin,descrip);

      std::string name;
      std::cout << " " << std::endl;
      std::cout << "Please enter your name" << std::endl;
      std::getline(std::cin,name);

      std::string tmpid;
      std::cout << " " << std::endl;
      std::cout << "Please input Internal Bank ID (version Number,etc.)" << std::endl;
      std::getline(std::cin,tmpid);
      int id = atoi (tmpid.c_str());
      bankID.setInternalValue (id);

      PdbCalBank *bbcBank
	= bankManager->createBank (classname.getString (), bankID, descrip.c_str(),
				   tStart, tStop, calibname.getString ());

      if (strcmp (type, "offset") == 0)
	{
	  bbcBank->setLength (2);
	}
      else if (strcmp (type, "tzero") == 0)
	{
	  bbcBank->setLength (1);
	}
      else
	{
	  bbcBank->setLength (BBC_N_PMT);
	}

      bbcBank->setUserName (name.c_str());

      for (unsigned int i = 0; i < bbcBank->getLength (); i++)
	{
	  T *entry = (T *) & (bbcBank->getEntry (i));
	  *entry = calibpar[i];
	}
      application->commit ();
    }
  else
    {
      std::cout << "failed to start application for update" << std::endl;
      return 1;
    }
  return 0;
}

#endif /*  PHENIX_BBCCALIBPAR_HH */
