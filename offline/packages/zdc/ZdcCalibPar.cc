#include <ZdcCalibPar.hh>
#include <msg_profile.h>
#include <msg_control.h>

#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace std;

template <>
int
ZdcCalibPar<PdbPmtPeak>::restore(const char* filename, const char* type)
{
  printf("restore Calibration Parameter from file %s\n", filename);

  fstream DataFile;
  DataFile.open(filename, ios::in);
  if (!DataFile)
    {
      ostringstream msg;
      msg << "Fail to open Ascii database File " << filename ;
      send_message(MSG_SEV_FATAL, msg.str());
      exit( -1);
    }
  int ientry = 0;
  float peak;
  float devi;
  int stat;
  while ( DataFile >> peak >> devi >> stat )
    {
      calibpar[ientry].setPeakChannel( peak );
      calibpar[ientry].setDeviation( devi );
      calibpar[ientry].setStatus( stat );
      ientry++;
    }
  DataFile.close();

  num_entries = ientry;
  return 0;
}

template <>
int
ZdcCalibPar<PdbPmtFitPar>::restore(const char* filename, const char* type)
{
  printf("restore Calibration Parameter from file %s\n", filename) ;
  PHString InFile = filename;
  fstream DataFile;
  DataFile.open(InFile.getString(), ios::in);
  if (!DataFile)
    {
      ostringstream msg;
      msg << "Fail to open Ascii database File " << InFile ;
      send_message(MSG_SEV_FATAL, msg.str());
      exit( -1);
    }

  int ientry = 0;
  float par0, par1, par2, par3, par4;
  float chi2;
  int stat;
  while ( DataFile >> par0 >> par1 >> par2 >> par3 >> par4 >> chi2 >> stat )
    {
      calibpar[ientry].setPar0( par0 );
      calibpar[ientry].setPar1( par1 );
      calibpar[ientry].setPar2( par2 );
      calibpar[ientry].setPar3( par3 );
      calibpar[ientry].setPar4( par4 );
      calibpar[ientry].setChi2( chi2 );
      calibpar[ientry].setStatus( stat );
      ientry++;
    }
  DataFile.close();

  num_entries = ientry;

  return 0;
}

/// the lookup table type
template <>
int
ZdcCalibPar<PdbZdcLUT>::restore(const char* filename, const char* type)
{
  printf("ZdcCalibPar: restore Calibration Parameter from file %s\n", filename) ;
  fstream DataFile;
  DataFile.open(filename, ios::in);
  if (!DataFile)
    {
      ostringstream msg;
      msg << "Fail to open Ascii database File " << filename ;
      send_message(MSG_SEV_FATAL, msg.str());
      exit( -1);
    }

  float lutval[8];
  for ( int ch = 0; ch < 4096; ch++)
    {
      DataFile >> lutval[0] >> lutval[1]
	       >> lutval[2] >> lutval[3]
	       >> lutval[4] >> lutval[5]
	       >> lutval[6] >> lutval[7];

      for (int tdc = 0; tdc < 8; tdc++)
        {
          calibpar[tdc].setlut( ch, lutval[tdc] );
        }
    }
  DataFile.close();
  return 0;
}

template <>
int
ZdcCalibPar<PdbPmtPeak>::restore(const char* type)
{

  if ( strcmp(type, "pedestal") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPeakChannel( 0.0 );
          calibpar[i].setDeviation( 2.0 );
        }
    }
  else if ( strcmp(type, "overflow0") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPeakChannel( 4095.0 );
          calibpar[i].setDeviation( 2.0 );
        }
    }
  else if ( strcmp(type, "overflow1") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPeakChannel( 4095.0 );
          calibpar[i].setDeviation( 2.0 );
        }
    }
  else if ( strcmp(type, "pmtgain") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPeakChannel( 1.0 );
          calibpar[i].setDeviation( 0.0 );
        }
    }
  else if ( strcmp(type, "tzero") == 0 )
    {
      for (int i = 0; i < 1; i++)
        {
          calibpar[i].setPeakChannel( 0.0 );
          calibpar[i].setDeviation( 0.0 );
          calibpar[i].setStatus( 0 );
        }
    }
  else if ( strcmp(type, "zvtx") == 0 )
    {
      for (int i = 0; i < 1; i++)
        {
          calibpar[i].setPeakChannel( 0.0 );
          calibpar[i].setDeviation( 0.0 );
          calibpar[i].setStatus( 0 );
        }
    }
  else if ( strcmp(type, "smdoffset") == 0 )
    {
      for (int i = 0; i < 4; i++)
        {
          calibpar[i].setPeakChannel( 0.0 );
          calibpar[i].setDeviation( 0.0 );
          calibpar[i].setStatus( 0 );
        }
    }
  return 0;
}

template <>
int
ZdcCalibPar<PdbPmtFitPar>::restore(const char* type)
{
  if ( strcmp(type, "adc") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPar0(0.0);
          calibpar[i].setPar1(1.0);
          calibpar[i].setPar2(0.0);
          calibpar[i].setPar3(0.0);
          calibpar[i].setPar4(0.0);
          calibpar[i].setChi2(1.0);
        }
    }
  else if ( strcmp(type, "tdc0") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPar0(0.0);
          calibpar[i].setPar1(0.007);
          calibpar[i].setPar2(0.0);
          calibpar[i].setPar3(0.0);
          calibpar[i].setPar4(0.0);
          calibpar[i].setChi2(1.0);
        }
    }
  else if ( strcmp(type, "tdc1") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPar0(0.0);
          calibpar[i].setPar1(0.007);
          calibpar[i].setPar2(0.0);
          calibpar[i].setPar3(0.0);
          calibpar[i].setPar4(0.0);
          calibpar[i].setChi2(1.0);
        }
    }
  else if ( strcmp(type, "slewpar0") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPar0(0.0);
          calibpar[i].setPar1(0.0);
          calibpar[i].setPar2(0.0);
          calibpar[i].setPar3(0.0);
          calibpar[i].setPar4(0.0);
          calibpar[i].setChi2(1.0);
        }
    }
  else if ( strcmp(type, "slewpar1") == 0 )
    {
      for (int i = 0; i < MAX_ENTRIES; i++)
        {
          calibpar[i].setStatus( 0 );
          calibpar[i].setPar0(0.0);
          calibpar[i].setPar1(0.0);
          calibpar[i].setPar2(0.0);
          calibpar[i].setPar3(0.0);
          calibpar[i].setPar4(0.0);
          calibpar[i].setChi2(1.0);
        }
    }
  return 0;
}

template <>
int
ZdcCalibPar<PdbZdcLUT>::restore(const char* type)
{
  return 0;
}

template <>
int
ZdcCalibPar<PdbPmtPeak>::restorelut(const PHTimeStamp& time, const char* type)
{
  PHString classname;

  classname = "PdbZdcLUTBank";

  //
  // Get Bank Manager
  //
  PdbBankManager *bankManager = PdbBankManager::instance();

  if ( bankManager == 0 )
    {
      ostringstream msg;
      msg << "Failed to get bankManager" ;
      send_message(MSG_SEV_ERROR, msg.str());
      return -1;
    }

  //
  // Get application manager class.
  //
  PdbApplication *application = bankManager->getApplication();
  if ( application == 0 )
    {
      ostringstream msg;
      msg << "Failed to get PdbApplication" << endl;
      send_message(MSG_SEV_ERROR, msg.str());
      return -1;
    }

  if (application->startRead())
    {

      PHTimeStamp tSearch = time;
      PdbBankID bankID;
      bankID.setInternalValue(0);

      PHString TmpName("calib.zdc.");
      PHString TypeName(type);
      PHString calibname = TmpName + TypeName;
      printf("%s\n", calibname.getString());
      PdbCalBank *zdcBank
	= bankManager->fetchBank(classname.getString(), bankID, calibname.getString(), tSearch);

      if (zdcBank)
        {
          zdcBank->printHeader();
          StartTime = zdcBank->getStartValTime();
          EndTime = zdcBank->getEndValTime();


          for ( unsigned int i = 0; i < zdcBank->getLength(); i++ )
            {
              caliblut[i] = (PdbZdcLUT &)zdcBank->getEntry(i);
            }
          delete zdcBank;
        }
      else
        {
          ostringstream msg;
          msg << "bankManager returned zero-pointer" ;
          send_message(MSG_SEV_ERROR, msg.str());
          return (1);
        }
      application->commit();
    }
  else
    {
      application->abort();
      ostringstream msg;
      msg << "Transaction aborted." ;
      send_message(MSG_SEV_ERROR, msg.str());
      return (1);
    }
  return (0);
}

/// store calibrations into database
template <>
int
ZdcCalibPar<PdbPmtPeak>::storelut(const PHTimeStamp& time, const char* type, const char *description, const char *name)
{
  cout << "ZdcCalib" << type << ":stored " << endl;

  PHString classname;

  classname = "PdbZdcLUTBank";

  PdbBankManager *bankManager = PdbBankManager::instance();

  if ( bankManager == 0 )
    {
      cout << "ZdcCalibPar: Failed to get bankManager" << endl;
      return -1;
    }
  PdbApplication *application = bankManager->getApplication();
  if ( application == 0 )
    {
      cout << "ZdcCalibPar: Failed to get PdbApplication" << endl;
      return -1;
    }

  cout << "Opening FD in update mode.." << endl;
  if (application->startUpdate())
    {

      PHTimeStamp tStart = time;
      PHTimeStamp tStop = PHTimeStamp(2025, 1, 1, 0, 0, 0);
      PdbBankID bankID;

      PHString TmpName("calib.zdc.");
      PHString TypeName(type);
      PHString calibname = TmpName + TypeName;

      PdbCalBank *prevBank
	= bankManager->fetchBank(classname.getString(), bankID, calibname.getString(), tStart);

      if ( prevBank )
        {
          cout << " overlapping bank found. Change the EndValTime of it " << endl;
          tStop = prevBank->getEndValTime();
          prevBank->setEndValTime( time );
          std::cout << " and setting current EndValTime to " << tStop << endl;
        }


      //=======================================================================
      // Inputs for calibration bunk header informations.
      //=======================================================================
      PHString Description;
      if ( strlen(description) == 0 )
        {
          char tempdescription[240];
          cout << "Please input description for this calibration parameter:" << endl;
          cin.getline(tempdescription, 240);
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
          cout << "Please enter your name:" << endl;
          cin.getline(tempname, 20);
          UserName = tempname;
        }
      else
        {
          UserName = name;
        }

      bankID.setInternalValue( 0 );

      //=======================================================================

      PdbCalBank *zdcBank = bankManager->createBank(classname.getString(),
						    bankID, Description.getString(),
						    tStart, tStop, calibname.getString());

      zdcBank->setLength(8);
      zdcBank->setUserName( UserName );

      for (unsigned int i = 0; i < zdcBank->getLength(); i++)
        {
          PdbZdcLUT* entry = (PdbZdcLUT*) & (zdcBank->getEntry(i));
          *entry = caliblut[i];
        }
      application->commit();
    }
  else
    {
      cout << "failed to start application for update" << endl;
      return (1);
    }
  return (0);
}

/// the lookup table type
template <>
int
ZdcCalibPar<PdbPmtPeak>::restorelut(const char *filename, const char *type)
{
  printf("ZdcCalibPar: restore Calibration Parameter from file %s\n", filename) ;
  fstream DataFile;
  DataFile.open(filename, ios::in);
  if (!DataFile)
    {
      ostringstream msg;
      msg << "Fail to open Ascii database File " << filename ;
      send_message(MSG_SEV_FATAL, msg.str());
      exit( -1);
    }

  float lutval[16];
  int channel;

  for ( int ch = 0; ch < 4096; ch++)
    {
      DataFile >> channel
	       >> lutval[0] >> lutval[1]
	       >> lutval[2] >> lutval[3]
	       >> lutval[4] >> lutval[5]
	       >> lutval[6] >> lutval[7];

      for (int tdc = 0; tdc < 8; tdc++)
        {
          caliblut[tdc].setlut( ch, lutval[tdc] );
        }
    }
  DataFile.close();
  return 0;
}

template <>
int
ZdcCalibPar<PdbPmtFitPar>::storelut(PHTimeStamp const &a, const char *b, const char *c, const char *d)
{
  return 0;
}

template <>
int
ZdcCalibPar<PdbPmtFitPar>::restorelut(const char *a, const char *b)
{
  return 0;
}

template <>
int
ZdcCalibPar<PdbPmtFitPar>::restorelut(const PHTimeStamp& time, const char* type)
{
  return 0;
}

// dump calibrations out to file
template <>
void
ZdcCalibPar<PdbPmtPeak>::dump(const char *type)
{
  PHString outfname;
  if ( strcmp(type, "pedestal") == 0 )
    {
      outfname = "ZdcCalib.pedestal.";
    }
  else if ( strcmp(type, "overflow0") == 0 )
    {
      outfname = "ZdcCalib.overflow0.";
    }
  else if ( strcmp(type, "overflow1") == 0 )
    {
      outfname = "ZdcCalib.overflow1.";
    }
  else if ( strcmp(type, "pmtgain") == 0 )
    {
      outfname = "ZdcCalib.pmtgain.";
    }
  else if ( strcmp(type, "tdc0lut") == 0 )
    {
      outfname = "ZdcCalib.tdc0lut.";
    }
  else if ( strcmp(type, "tdc1lut") == 0 )
    {
      outfname = "ZdcCalib.tdc1lut.";
    }
  else if ( strcmp(type, "tzero") == 0 )
    {
      outfname = "ZdcCalib.tzero.";
    }
  else if ( strcmp(type, "zvtx") == 0 )
    {
      outfname = "ZdcCalib.zvtx.";
    }
  else if ( strcmp(type, "smdoffset") == 0 )
    {
      outfname = "ZdcCalib.smdoffset.";
    }
  else
    {
      cout << "ZdcCalibPar: unknown type " << type << endl;
      return ;
    }

  outfname += getStartTime()->formatTimeString();
  outfname += "-";
  outfname += getEndTime()->formatTimeString();
  ofstream outfile(outfname.getString());

  if ( strcmp(type, "tdc0lut") != 0 && strcmp(type, "tdc1lut") != 0 )
    {
      for (int ich = 0; ich < num_entries; ich++)
        {
          outfile << setiosflags(ios::left)
		  << setw(10) << setprecision(4) << calibpar[ich].getPeakChannel() << " "
		  << setw(10) << setprecision(2) << calibpar[ich].getDeviation() << " "
		  << setw(10) << setprecision(2) << calibpar[ich].getStatus() << endl;
        }
    }
  else
    {
      for (int iadc = 0; iadc < 4096; iadc++)
        {
          outfile << iadc << "\t";
          for (int ich = 0; ich < 8; ich++)
            {
              outfile << setw(10) << setiosflags(ios::fixed | ios::right)
		      << setprecision(3) << caliblut[ich].getlut(iadc) << " ";
            }
          outfile << endl;
        }
    }

  outfile.close();
}

template <>
void
ZdcCalibPar<PdbPmtFitPar>::dump(const char *type)
{
  PHString outfname;

  if ( strcmp(type, "adc") == 0 )
    {
      outfname = "ZdcCalib.adc.";
    }
  else if ( strcmp(type, "tdc0") == 0 )
    {
      outfname = "ZdcCalib.tdc0.";
    }
  else if ( strcmp(type, "tdc1") == 0 )
    {
      outfname = "ZdcCalib.tdc1.";
    }
  else if ( strcmp(type, "slewpar0") == 0 )
    {
      outfname = "ZdcCalib.slewpar0.";
    }
  else if ( strcmp(type, "slewpar1") == 0 )
    {
      outfname = "ZdcCalib.slewpar1.";
    }
  else
    {
      cout << "ZdcCalibPar: unknown type " << type << endl;
      return ;
    }

  outfname += getStartTime()->formatTimeString();
  outfname += "-";
  outfname += getEndTime()->formatTimeString();
  ofstream outfile(outfname.getString());

  for (int ich = 0; ich < num_entries; ich++)
    {
      outfile << setiosflags( ios::left )
	      << setw(10) << setprecision(6) << calibpar[ich].getPar0() << " "
	      << setw(10) << setprecision(6) << calibpar[ich].getPar1() << " "
	      << setw(10) << setprecision(6) << calibpar[ich].getPar2() << " "
	      << setw(10) << setprecision(6) << calibpar[ich].getPar3() << " "
	      << setw(10) << setprecision(6) << calibpar[ich].getPar4() << " "
	      << setw(6) << setprecision(2) << calibpar[ich].getChi2() << " "
	      << setw(6) << calibpar[ich].getStatus() << endl;
    }

  outfile.close();
}
