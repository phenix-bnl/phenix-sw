#include <BbcCalibPar.hh>

#include <string>
#include <fstream>
#include <sstream>

using namespace std;

template <>
int 
BbcCalibPar < PdbPmtPeak >::restore(const char *filename, const char *type)
{
  float peak, devi;
  int i, stat, nentries;
  printf("restore Calibration Parameter from file %s\n",filename);

  PHString InFile = filename;
  ifstream DataFile;
  DataFile.open (InFile.getString ());

  if (!DataFile)
    {
      printf("Fail to open Ascii database File %s\n",filename);
      return 1;
    }
  if (strcmp (type, "offset") == 0)
    {
      nentries = 2;
    }
  else if (strcmp (type, "tzero") == 0)
    {
      nentries = 1;
    }
  else
    {
      nentries = BBC_N_PMT;
    }
  for (i = 0; i < nentries; i++)
    {
      DataFile >> peak >> devi >> stat;
      calibpar[i].setStatus (stat);
      calibpar[i].setPeakChannel (peak);
      calibpar[i].setDeviation (devi);
    }
  DataFile.close ();

  return 0;
}

template <>
int 
BbcCalibPar < PdbPmtFitPar >::restore (const char *filename, const char *type)
{
  float par0, par1, par2, par3, par4, chi2;
  int i, stat;

  printf("restore Calibration Parameter from file %s\n",filename);

  PHString InFile = filename;
  ifstream DataFile;
  DataFile.open (InFile.getString ());
  if (!DataFile)
    {
      printf("Fail to open Ascii database File %s\n",InFile.getString());
      return 1;
    }
  
  for (i = 0; i < BBC_N_PMT; i++)
    {
      DataFile >> par0 >> par1 >> par2 >> par3 >> par4 >> chi2 >> stat;
      calibpar[i].setPar0 (par0);
      calibpar[i].setPar1 (par1);
      calibpar[i].setPar2 (par2);
      calibpar[i].setPar3 (par3);
      calibpar[i].setPar4 (par4);
      calibpar[i].setChi2 (chi2);
      calibpar[i].setStatus (stat);
    }
  DataFile.close ();
  
  return 0;
}

template <>
int 
BbcCalibPar < PdbBbcConf >::restore (const char *filename, const char *type)
{
  int i, id, FemID, FemCh, HVID, HV, Fiber, IDinPISA;
  string tmp1, tmp2;
  printf("BbcCalibPar: restore Calibration Parameter from file %s\n",filename);
  PHString InFile = filename;
  ifstream DataFile;
  DataFile.open(InFile.getString ());
  if (!DataFile)
    {
      printf("Fail to open Ascii database File %s\n",filename);
      return 1;
    }

  for (i = 0; i < BBC_N_PMT; i++)
    {
      DataFile >> tmp1 >> tmp2 >> id >> FemID >> FemCh >> HVID >> HV >> Fiber >> IDinPISA;
      PHString arm (tmp1.c_str());
      PHString serial (tmp2.c_str());
      calibpar[i].setArmID (arm);
      calibpar[i].setPmtSerialID (serial);
      calibpar[i].setPmtID (id);
      calibpar[i].setFemID (FemID);
      calibpar[i].setFemChanID (FemCh);
      calibpar[i].setHVgroupID (HVID);
      calibpar[i].setHV (HV);
      calibpar[i].setFiberID (Fiber);
      calibpar[i].setPmtIDinPIAS (IDinPISA);
    }
  DataFile.close ();

  return 0;
}

template <>
int
BbcCalibPar < PdbPmtPeak >::restore (const char *type)
{
  int i;

  if (strcmp (type, "pedestal") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (890.0);
	  calibpar[i].setDeviation (0.0);
	}
    }
  else if (strcmp (type, "overflow0") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (4096.0);
	  calibpar[i].setDeviation (0.0);
	}
    }
  else if (strcmp (type, "overflow1") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (4096.0);
	  calibpar[i].setDeviation (0.0);
	}
    }
  else if (strcmp (type, "pmtgain") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (40.0);
	  calibpar[i].setDeviation (0.0);
	}
    }
  else if (strcmp (type, "offset") == 0)
    {
      for (i = 0; i < 2; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (0.0);
	  calibpar[i].setDeviation (0.0);
	}
    }
  else if (strcmp (type, "tzero") == 0)
    {
      for (i = 0; i < 1; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (0.0);
	  calibpar[i].setDeviation (0.0);
	}
    }
  else if (strcmp (type, "fakeped") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (890.0);
	  calibpar[i].setDeviation (0.0);
	}
    }
  else if (strcmp (type, "threshold") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (950.0);
	  calibpar[i].setDeviation (0.0);
	}
    }
  else if (strcmp (type, "timereso") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPeakChannel (0.05);
	  calibpar[i].setDeviation (0.0);
	}
    }

  return 0;
}

template <>
int
BbcCalibPar < PdbPmtFitPar >::restore (const char *type)
{
  int i;

  if (strcmp (type, "adc") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPar0 (0.0);
	  calibpar[i].setPar1 (0.07);
	  calibpar[i].setPar2 (0.0);
	  calibpar[i].setPar3 (0.0);
	  calibpar[i].setPar4 (0.0);
	  calibpar[i].setChi2 (1.0);
	}
    }
  else if (strcmp (type, "tdc0") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPar0 (0.0);
	  calibpar[i].setPar1 (0.007);
	  calibpar[i].setPar2 (0.0);
	  calibpar[i].setPar3 (0.0);
	  calibpar[i].setPar4 (0.0);
	  calibpar[i].setChi2 (1.0);
	}
    }
  else if (strcmp (type, "tdc1") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPar0 (0.0);
	  calibpar[i].setPar1 (0.007);
	  calibpar[i].setPar2 (0.0);
	  calibpar[i].setPar3 (0.0);
	  calibpar[i].setPar4 (0.0);
	  calibpar[i].setChi2 (1.0);
	}
    }
  else if (strcmp (type, "slewpar0") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPar0 (0.0);
	  calibpar[i].setPar1 (0.0);
	  calibpar[i].setPar2 (0.0);
	  calibpar[i].setPar3 (0.0);
	  calibpar[i].setPar4 (0.0);
	  calibpar[i].setChi2 (1.0);
	}
    }
  else if (strcmp (type, "slewpar1") == 0)
    {
      for (i = 0; i < BBC_N_PMT; i++)
	{
	  calibpar[i].setStatus (0);
	  calibpar[i].setPar0 (0.0);
	  calibpar[i].setPar1 (0.0);
	  calibpar[i].setPar2 (0.0);
	  calibpar[i].setPar3 (0.0);
	  calibpar[i].setPar4 (0.0);
	  calibpar[i].setChi2 (1.0);
	}
    }

  return 0;
}

template <>
int
BbcCalibPar < PdbBbcConf >::restore (const char *type)
{
  int i;

  for (i = 0; i < 64; i++)
    {
      PHString tmp1 ("N");
      PHString tmp2 ("N/A");

      calibpar[i].setPmtID (i + 1);
      calibpar[i].setFemID (i / 8 + 1);
      calibpar[i].setFemChanID (i % 8 + 1);
      calibpar[i].setHVgroupID (0);
      calibpar[i].setHV (0);
      calibpar[i].setFiberID (0);
      calibpar[i].setPmtIDinPIAS (i + 1);
      calibpar[i].setArmID (tmp1);
      calibpar[i].setPmtSerialID (tmp2);
    }
  for (i = 64; i < 128; i++)
    {
      PHString tmp1 ("S");
      PHString tmp2 ("N/A");

      calibpar[i].setPmtID (i - 64 + 1);
      calibpar[i].setFemID (i / 8 + 1);
      calibpar[i].setFemChanID (i % 8 + 1);
      calibpar[i].setHVgroupID (0);
      calibpar[i].setHV (0);
      calibpar[i].setFiberID (0);
      calibpar[i].setPmtIDinPIAS (i - 64 + 1);
      calibpar[i].setArmID (tmp1);
      calibpar[i].setPmtSerialID (tmp2);
    }

  return 0;
}
