
#include <cstring>
#include <fstream>
#include <iostream>

#include <MpcNoise.h>
#include <RunToTime.hh>
#include <recoConsts.h>
//#include "phool.h"

// Database Includes

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PHString.h>
#include <PdbMpcNoise.hh>

using namespace std;

MpcNoise::MpcNoise()
{
  status = 0;
  Reset();
}

MpcNoise::MpcNoise(const int runnumber)
{
  status = 0;
  Reset();
  Download_Noise(runnumber);
}

MpcNoise::MpcNoise(const string& dbase_location)
{
  status = 0;
  Reset();
  Download_Noise(dbase_location);
  
  
  //For now, just use full pathname to download MpcCalib.noise files
/*
  // get database location
  const char *dbase_directory = getenv("MPC_DATABASE");

  if ( dbase_directory != NULL )
    {
      Download_All( dbase_directory );
    }

*/
}

int MpcNoise::Download_Noise(const int& runnumber)
{
  RunToTime *runtotime = RunToTime::instance();
  PHTimeStamp *timestamp = runtotime->getBeginTime( runnumber );
  Download_Noise( *timestamp, "NOISE"  );
  delete timestamp;
  return 1;
}

int MpcNoise::Download_Noise(const string& dbase_location)
{
  ifstream infile( dbase_location.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_location << endl;
      status = -3;
      return status;
    }
  
  int fee576ch, N_thr1, N_thr2, N_thr3, N_thr4;
  
  int ich = 0;
  while ( infile >> fee576ch >> N_thr1 >> N_thr2 >> N_thr3 >> N_thr4 )
    {
      if(ich < MAXCH+2)
	{
	  mpcPdbNoise[ich].set_fee576( fee576ch );
	  mpcPdbNoise[ich].set( ich, N_thr1, N_thr2, N_thr3, N_thr4 );
	}
      ich++;
    }
  
  return 1;
}

int MpcNoise::Download_Noise(const PHTimeStamp& tstamp, const string& what)
{
  string classname = "PdbMpcNoiseBank";
  string calibname;
  PdbMpcNoise *data = 0;
  if ( what == "NOISE" )
    {
      calibname = "mpcnoise";
      data = mpcPdbNoise;
    }
 
 

  // Download from the database
  PdbBankManager *bankManager = PdbBankManager::instance();
  if ( bankManager==0 )
    {
      cout << PHWHERE << " Failed to get bankManager" << endl;
      return -1;
    }

  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  if ( application==0 )
    {
      cout << PHWHERE << " Failed to get PdbApplication" << endl;
      return -1;
    }

  if ( application->startRead() )
    {
      PHTimeStamp tSearch = tstamp;
      PdbBankID bankID;
      bankID.setInternalValue(0);

      cout << calibname << endl;
      PdbCalBank *mpcBank = bankManager->fetchBank(classname.c_str(),
                                                  bankID,
                                                  calibname.c_str(),
                                                  tSearch);
      if (mpcBank)
        {
          mpcBank->printHeader();
          StartTime = mpcBank->getStartValTime();
          EndTime = mpcBank->getEndValTime();
          for (unsigned int ich = 0; ich < mpcBank->getLength(); ich++)
            {
              *(data+ich) = (PdbMpcNoise &)mpcBank->getEntry(ich);
            }

          delete mpcBank;
        }
      else
        {
          return 1;
        }
      application->commit();
    }
  else
    {
      application->abort();
      cout << PHWHERE << " Transaction aborted." << endl;
      return 1;
    }

  return 0;
}

int MpcNoise::StoreInDatabase(const int& runnumber, const string& what, const string& username, const string& description)
{
  RunToTime *runtotime = RunToTime::instance();
  PHTimeStamp *timestamp = runtotime->getBeginTime( runnumber );
  StoreInDatabase(*timestamp, what, username, description);
  return 1;
}

int MpcNoise::StoreInDatabase(PHTimeStamp& tStart, const string& what, const string& username, const string& description)
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  if ( bankManager==0 )
    {
      cout << PHWHERE << " Failed to get bankManager" << endl;
      return -1;
    }
  PdbApplication *application = bankManager->getApplication();
  if ( application==0 )
    {
      cout << PHWHERE << " Failed to get PdbApplication" << endl;
      return -1;
    }

  string classname = "PdbMpcNoiseBank";
  string calibname;
  PdbMpcNoise *data = 0;
  if ( what == "NOISE" )
    {
      calibname = "mpcnoise";
      data = mpcPdbNoise;
    }
  

  cout << "Opening FD in update mode.." << endl;
  if (application->startUpdate())
    {
      PHTimeStamp tStop = PHTimeStamp(2020, 1, 1, 0, 0, 0);     // canonical end of run time
      PdbBankID bankID;

      PdbCalBank *prevBank = bankManager->fetchBank(classname.c_str(), bankID, calibname.c_str(), tStart);

      if (prevBank)
        {
          cout << " overlapping bank found. Changing the EndValTime of it " << endl;
          tStop = prevBank->getEndValTime();
          prevBank->setEndValTime(tStart);
          cout << " and setting current EndValTime to " << tStop << endl;
          delete prevBank;
        }

      // Inputs for calibration bunk header informations.
      PHString Description;
      if ( description.size() == 0 )
        {
          char tempdescription[240];
          cout << "Please input description for this calibration parameter:" << endl;
          cin.getline(tempdescription, 240);
          Description = tempdescription;
        }
      else
        {
          Description = description.c_str();
        }

      PHString UserName;
      if ( username.size() == 0 )
        {
          char tempname[20];
          cout << "Please enter your name:" << endl;
          cin.getline(tempname, 20);
          UserName = tempname;
        }
      else
        {
          UserName = username.c_str();
        }

      // why do we always use internal value 0?
      bankID.setInternalValue(0);

      PdbCalBank *mpcmapBank = bankManager->createBank(classname.c_str(),
                                                    bankID,
                                                    Description.getString(),
                                                    tStart,
                                                    tStop,
                                                    calibname.c_str());

      mpcmapBank->setLength(MAXCH+2);
      mpcmapBank->setUserName(UserName);

      for (unsigned int ich = 0; ich < mpcmapBank->getLength(); ich++)
        {
          PdbMpcNoise *entry = (PdbMpcNoise*) &(mpcmapBank->getEntry(ich));
          *entry = *(data+ich);
        }
      application->commit();
    }
  else
    {
      cout << PHWHERE << "failed to start dbase application for update" << endl;
      return 1;
    }

  return 1;
}


void MpcNoise::Reset()
{
  // Set all initial values
  for (int ifee576ch=0; ifee576ch<MAXCH+2; ifee576ch++)
    {
      mpcPdbNoise[ifee576ch].Reset();
    }
}

void MpcNoise::Dump(const std::string &what)
{
  // make timerange string
  string timerange = ".";
  timerange.append( getStartTime()->formatTimeString() );
  timerange.append("-");
  timerange.append( getEndTime()->formatTimeString() );
  string runstr = ".";
  int runnum = getRun();
  char run_cstr[1024];
  sprintf(run_cstr,"%d",runnum);
  runstr.append(run_cstr);

  if ( what == "NOISE" )
    {
      string full_outfname = "MpcCal.noise";
      full_outfname.append( runstr );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH+2; ifeech++)
        {
          outfile << ifeech;
	  for(int ithr=0; ithr<MAXTHR; ithr++)
	    outfile << "\t" << getN(ifeech,ithr);
	  outfile << endl;
        }

      outfile.close();
    }
}

int MpcNoise::IsValid(const int verbosity) const
{
  if ( verbosity>0 && status<=0 )
    {
      if ( status == -1 )
        {
          cout << PHWHERE << "Please set environment variable MPC_DATABASE" << endl;
        }
      else if ( status == -2 )
        {
          cout << PHWHERE << "Database not yet implemented" << endl;
        }
      else
        {
          cout << PHWHERE << "Unknown status " << status << endl;
        }
    }

  return status;
}

void MpcNoise::Print()
{
  cout << "Printing MpcNoise entry for Run " << getRun()
       << " which contains " << getNtot() << " entries." << endl;
  cout << "The Thresholds are: " << endl;
  
  
  for(int ithr=0; ithr<MAXTHR; ithr++)
    cout << "\t" << getThresh(ithr);
  cout << endl;
  
  cout << "Chan\tN_thr1\tN_thr2\tN_thr3\tN_thr4\n";
    
  for (int ifeech=0; ifeech<MAXCH+2; ifeech++)
    {
      cout << ifeech;
      for(int ithr=0; ithr<MAXTHR; ithr++)
	cout << "\t" << getN(ifeech,ithr);
      cout << endl;
    }
} 
