// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 01/24/00
// Description: Implementation of TecCalibrationObject class

#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbTecPedestal.hh"
#include "TecCalibrationObject.hh"
#include <iostream>
#include <fstream>

using namespace std;

/// Constructor
TecCalibrationObject::TecCalibrationObject()
{

  Debug = 0;
  iFlag = 1;
  const char* location;
  location = "calib.tec.tecgain_run00";
  setCalibName(location);
  setBankNumber(TECCAL2BANK);
  setBankID(BankNumber);
  setDescription("TEC Calibration Constants");
  Tsearch.setToSystemTime();

  int i, j;
  for (i = 0; i < TECMAXINDEX; i++)
    {
      for (j = 0; j < TECMAXWIRE; j++)
        {
          RelativeGain[i][j] = 1.0;
        }
    }

  for (i = 0; i < TECMAXINDEX; i++)
    {
      AbsoluteGain[i] = 1.0;
    }

  for (i = 0; i < TECMAXINDEX; i++)
    {
      FirstTimeBin[i] = 0;
      LastTimeBin[i] = TECMAXTIMEBIN - 1;
      for (j = 0; j < TECMAXTIMEBIN; j++)
        {
          TimingGain[i][j] = 1.0;
        }
    }

  for (i = 0; i < TECMAXNUMHOT; i++)
    {
      HotDeadList[i] = -1;
    }

}

//---------------------------------------------------------------------------------
// member functions
//---------------------------------------------------------------------------------

///
void TecCalibrationObject::UseSimulationDatabase()
{
  const char* location;
  location = "calib.tec.tecgain_geant0";
  setCalibName(location);
}
///
void TecCalibrationObject::UseRealDatabase()
{
  const char* location;
  location = "calib.tec.tecgain_run00";
  setCalibName(location);
}

/// Get relative gain
float TecCalibrationObject::getRelativeGain(TecAddressObject* TAO)
{

  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int iwire = TAO->getWire();
  int index = iside + iplane * TECMAXSIDE + isector * TECMAXSIDE * TECMAXPLANE;

  if (iFlag != 0)
    {
      cerr << "TecCalibrationObject ERROR: TCO not initialized." << endl
	   << "    Use Fetch() or FetchFromFile() methods first." << endl;
      return 0.0;
    }
  return RelativeGain[index][iwire];
}

//---------------------------------------------------------------------------------

/// Set relative gain
void TecCalibrationObject::setRelativeGain(TecAddressObject* TAO, float gain)
{

  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int iwire = TAO->getWire();
  int index = iside + iplane * TECMAXSIDE + isector * TECMAXSIDE * TECMAXPLANE;

  RelativeGain[index][iwire] = gain;
}

//---------------------------------------------------------------------------------

/// Get relative gain
float TecCalibrationObject::getRelativeGain(int index, int iwire)
{
  return RelativeGain[index][iwire];
}

//---------------------------------------------------------------------------------

/// Set relative gain
void TecCalibrationObject::setRelativeGain(int index, int iwire, float gain)
{
  RelativeGain[index][iwire] = gain;
}

//---------------------------------------------------------------------------------

/// Get absolute gain
float TecCalibrationObject::getAbsoluteGain(TecAddressObject* TAO)
{
  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int index = iside + iplane * TECMAXSIDE + isector * TECMAXSIDE * TECMAXPLANE;

  if (iFlag != 0)
    {
      cerr << "TecCalibrationObject ERROR: TCO not initialized." << endl
	   << "    Use Fetch() or FetchFromFile() methods first." << endl;
      return 0.0;
    }
  return AbsoluteGain[index];
}

/// Set absolute gain
void TecCalibrationObject::setAbsoluteGain(TecAddressObject* TAO, float gain)
{
  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int index = iside + iplane * TECMAXSIDE + isector * TECMAXSIDE * TECMAXPLANE;

  AbsoluteGain[index] = gain;
}

//---------------------------------------------------------------------------------

/// Get absolute gain
float TecCalibrationObject::getAbsoluteGain(int index)
{
  return AbsoluteGain[index];
}

//---------------------------------------------------------------------------------

/// Set absolute gain
void TecCalibrationObject::setAbsoluteGain(int index, float gain)
{
  if (index >= 0 && index < TECMAXINDEX)
    {
      AbsoluteGain[index] = gain;
    }
  else
    {
      cerr << "TecCalibrationObject::setAbsoluteGain ERROR: wrong index = " << index << endl;
    }
}

//---------------------------------------------------------------------------------

/// Get timing calibration constant
float TecCalibrationObject::getTimingGain(TecAddressObject* TAO, int timebin)
{
  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int index = iside + iplane * TECMAXSIDE + isector * TECMAXSIDE * TECMAXPLANE;

  if (iFlag != 0)
    {
      cerr << "TecCalibrationObject ERROR: TCO not initialized." << endl
	   << "    Use Fetch() or FetchFromFile() methods first." << endl;
      return 0.0;
    }
  return TimingGain[index][timebin];
}

//---------------------------------------------------------------------------------

/// Set timing calibration constant
void TecCalibrationObject::setTimingGain(TecAddressObject* TAO, int timebin, float gain)
{
  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int index = iside + iplane * TECMAXSIDE + isector * TECMAXSIDE * TECMAXPLANE;

  TimingGain[index][timebin] = gain;
}

//------------------------------------------------------------------------

/// Get timing calibration constant
float TecCalibrationObject::getTimingGain(int index, int timebin)
{
  return TimingGain[index][timebin];
}

/// Set timing calibration constant
void TecCalibrationObject::setTimingGain(int index, int timebin, float gain)
{
  TimingGain[index][timebin] = gain;
}

/// Fetch timing calibration from an ASCII file "filename"
PHBoolean TecCalibrationObject::FetchTimingGainFromFile(const char* tecdb)
{

  ifstream file;
  file.open(tecdb);

  int totnent = TECMAXINDEX * TECMAXTIMEBIN + TECMAXINDEX * 2;
  float buff[3936], tmp;
  int i, index;
  int ibuf = 0;

  // Read gains from ASCII file

  if (!file)
    {
      cerr << "TecCalibrationObject::FetchTimingGainFromFile ERROR: "
	   << "Can not open " << tecdb << endl;
      return False;
    }
  else
    {
      // last 96 entries in this file are first and last time bins to use
      if (Debug > 0)
        cout << "TecCalibrationObject::FetchTimingGainFromFile: reading " <<
	  totnent << " entries from file." << endl;
      for (i = 0; i <= totnent; i++)
        {
          file >> tmp;

          if (file.eof())
            break;

          buff[ibuf] = tmp;
          ibuf++;
        }

      if (Debug > 0)
        cout << "TecCalibrationObject::FetchTimingGainFromFile: "
	     << "TEC absolute gains read from " << tecdb << endl
	     << "Total " << i << " " << ibuf << " rows." << endl;
    }

  // Fill gains array and first and last bins

  int firstplane = 0;
  int lastplane = TECMAXPLANE - 1;
  int isect, iplane, izsign;

  int count = 0;

  for (isect = 0; isect < TECMAXSECT; isect++)
    {
      for (iplane = firstplane; iplane < lastplane + 1; iplane++)
        {
          for (izsign = 0; izsign < TECMAXSIDE; izsign++)
            {

              index = isect * TECMAXPLANE * TECMAXSIDE + iplane * TECMAXSIDE + izsign;

              FirstTimeBin[index] = (int)buff[TECMAXTIMEBIN * TECMAXINDEX + index];
              LastTimeBin[index] = (int)buff[TECMAXTIMEBIN * TECMAXINDEX + TECMAXINDEX + index];

              for (i = 0; i < TECMAXTIMEBIN; i++)
                {
                  TimingGain[index][i] = buff[count];
                  count++;
                }

            }
        }
    }

  iFlag = 0;
  return True;
}

//--------------------------------------------------------------------

/// Fetch absolute gains from an ASCII file "filename"
PHBoolean TecCalibrationObject::FetchAbsGainFromFile(const char* tecdb)
{

  ifstream file;
  file.open(tecdb);

  int totnent = TECMAXSECT * TECMAXPLANE * TECMAXSIDE;
  float buff[48], tmp;
  int i, index;

  // Read gains from ASCII file

  if (!file)
    {
      cerr << "TecCalibrationObject::FetchAbsGainFromFile ERROR: "
	   << "Can not open " << tecdb << endl;
      return False;
    }
  else
    {
      int ibuf = 0;
      for (i = 0; i <= totnent; i++)
        {
          file >> tmp;

          if (file.eof())
            break;

          buff[ibuf] = tmp;
          ibuf++;
        }

      if (Debug > 0)
        cout << "TecCalibrationObject::FetchAbsGainFromFile: "
	     << "TEC absolute gains read from " << tecdb << endl
	     << "Total " << i << " " << ibuf << " rows." << endl;
    }

  // Fill gains array

  int firstplane = 0;
  int lastplane = TECMAXPLANE - 1;
  int isect, iplane, izsign;

  int count = 0;

  for (isect = 0; isect < TECMAXSECT; isect++)
    {
      for (iplane = firstplane; iplane < lastplane + 1; iplane++)
        {
          for (izsign = 0; izsign < TECMAXSIDE; izsign++)
            {

              index = isect * TECMAXPLANE * TECMAXSIDE + iplane * TECMAXSIDE + izsign;

              AbsoluteGain[index] = buff[count];
              count++;

            }
        }
    }

  iFlag = 0;
  return True;
}

//----------------------------------------------------------------------

int TecCalibrationObject::getHotDeadList(int* hotlist)
{

  int nhotdead = 0;

  for (int i = 0; i < TECMAXNUMHOT; i++)
    {
      hotlist[i] = HotDeadList[i];
      if (HotDeadList[i] > -1)
        {
          nhotdead++;
        }
    }

  return nhotdead;
}

//----------------------------------------------------------------------

/// Fetch a list of hot and dead wires from an ASCII file "filename"
PHBoolean TecCalibrationObject::FetchHotDeadFromFile(const char* tecdb)
{

  ifstream file;
  file.open(tecdb);
  int nhotdead = 0;
  int tmp;

  if (!file)
    {
      cerr << "TecCalibrationObject::FetchHotDeadFromFile ERROR: "
	   << "Can not open " << tecdb << endl;
      return False;
    }
  else
    {
      for (int i = 0; i < 99999; i++)
        {
          file >> tmp;
          if (file.eof())
            break;
          if (nhotdead >= TECMAXNUMHOT)
            {
              nhotdead = TECMAXNUMHOT - 1;
              cerr << "TecCalibrationObject::FetchHotDeadFromFile ERROR: "
		   << "Too many hot/dead wires." << endl;
            }
          HotDeadList[nhotdead] = tmp;
          nhotdead++;
        }
      if (Debug > 0)
        cout << "TecCalibrationObject::FetchHotDeadFromFile: "
	     << "List of Hot and Dead wires read from " << tecdb << endl
	     << "Total " << nhotdead << " rows." << endl;
      if (Debug > 1)
        {
          for (int i = 0; i < nhotdead; i++)
            {
              cout << i << " " << HotDeadList[i] << endl;
            }
        }
    }

  return True;
}

//--------------------------------------------------------------------

/// Fetch relative gains from an ASCII file "filename"
PHBoolean TecCalibrationObject::FetchRelGainFromFile(const char* tecdb)
{

  ifstream file;
  file.open(tecdb);

  int totnent =
    TecGeometryObject::get_NumWires(0) * TECMAXSECT * TECMAXSIDE +
    TecGeometryObject::get_NumWires(1) * TECMAXSECT * TECMAXSIDE +
    TecGeometryObject::get_NumWires(2) * TECMAXSECT * TECMAXSIDE +
    TecGeometryObject::get_NumWires(3) * TECMAXSECT * TECMAXSIDE +
    TecGeometryObject::get_NumWires(4) * TECMAXSECT * TECMAXSIDE +
    TecGeometryObject::get_NumWires(5) * TECMAXSECT * TECMAXSIDE;
  float buff[25000], tmp;
  int i, index;

  // Read ASCII file

  if (!file)
    {
      cerr << "TecCalibrationObject::FetchRelGainFromFile ERROR: "
	   << "Can not open " << tecdb << endl;
      return False;
    }
  else
    {
      int ibuf = 0;
      for (i = 0; i <= totnent; i++)
        {
          file >> tmp;

          if (file.eof())
            break;

          buff[ibuf] = tmp;
          ibuf++;
        }

      if (Debug > 0)
        cout << "TecCalibrationObject::FetchRelGainFromFile: "
	     << "TEC relative gains read from " << tecdb << endl
	     << "Total " << i << " " << ibuf << " rows." << endl;
    }

  // Fill gains array

  int firstplane = 0;
  int lastplane = TECMAXPLANE - 1;
  int isect, iplane, izsign, iwire;

  int count = 0;

  for (isect = 0; isect < TECMAXSECT; isect++)
    {
      for (iplane = firstplane; iplane < lastplane + 1; iplane++)
        {
          for (izsign = 0; izsign < TECMAXSIDE; izsign++)
            {

              index = isect * TECMAXPLANE * TECMAXSIDE + iplane * TECMAXSIDE + izsign;

              for (iwire = 0; iwire < TecGeometryObject::get_NumWires(iplane); iwire++)
                {

                  RelativeGain[index][iwire] = buff[count];
                  count++;

                }
            }
        }
    }

  iFlag = 0;

  return True;
}

//--------------------------------------------------------------------------------

/// Fetch calibration constants from default ASCII file
PHBoolean TecCalibrationObject::FetchFromFile()
{

  int i, index, isect, iplane, izsign, iwire;
  int firstplane, lastplane;

  firstplane = 0;
  lastplane = TECMAXPLANE - 1;

  for (isect = 0; isect < TECMAXSECT; isect++) /* 0 = bottom, 3 = top */
    {
      for (iplane = firstplane; iplane < lastplane + 1; iplane++)
        {
          for (izsign = 0; izsign < TECMAXSIDE; izsign++) /* 0=north, 1=south */
            {

              index = isect * TECMAXPLANE * TECMAXSIDE + iplane * TECMAXSIDE + izsign;

              AbsoluteGain[index] = 1.0;

              for (i = 0; i < TECMAXTIMEBIN; i++)
                TimingGain[index][i] = 1.0;
              FirstTimeBin[index] = 10;
              LastTimeBin[index] = 70;

              for (iwire = 0; iwire < TecGeometryObject::get_NumWires(iplane); iwire++)
                {
                  RelativeGain[index][iwire] = 1.0;
                }

            } // izsign
        } // iplane
    } // isect

  iFlag = 0;
  return True;

}

//---------------------------------------------------------------------------------

/// Fetch calibration constants from a Database
PHBoolean TecCalibrationObject::Fetch()
{

  setDescription("TEC Relative Gains");
  PHBoolean status = FetchRelGain();
  if (!status)
    {
      cerr << "TecCalibrationObject::Fetch() ERROR reading Relative Gains." << endl;
      return False;
    }

  setDescription("TEC Absolute Gains");
  status = FetchAbsGain();
  if (!status)
    {
      cerr << "TecCalibrationObject::Fetch() ERROR reading Absolute Gains." << endl;
      return False;
    }

  setDescription("TEC Timing Constants");
  status = FetchTimingGain();
  if (!status)
    {
      cerr << "TecCalibrationObject::Fetch() ERROR reading Timing Gains." << endl;
      return False;
    }

  setDescription("TEC Hot/Dead Channels");
  status = FetchHotDead();
  if (!status)
    {
      cerr << "TecCalibrationObject::Fetch() WARNING: "
	"No Hot/Dead Channels read from the Database." << endl;
    }

  iFlag = 0;
  return True;

}

//---------------------------------------------------------------------------------

/// Fetch calibration constants from a Database
PHBoolean TecCalibrationObject::Fetch(int runnumber)
{

  setDescription("TEC Relative Gains");
  PHBoolean status = FetchRelGain(runnumber);
  if (!status)
    {
      cerr << "TecCalibrationObject::Fetch() ERROR reading Relative Gains." << endl;
      return False;
    }

  setDescription("TEC Absolute Gains");
  status = FetchAbsGain(runnumber);
  if (!status)
    {
      cerr << "TecCalibrationObject::Fetch() ERROR reading Absolute Gains." << endl;
      return False;
    }

  setDescription("TEC Timing Constants");
  status = FetchTimingGain(runnumber);
  if (!status)
    {
      cerr << "TecCalibrationObject::Fetch() ERROR reading Timing Gains." << endl;
      return False;
    }

  setDescription("TEC Hot/Dead Channels");
  status = FetchHotDead(runnumber);
  if (!status)
    {
      cerr << "TecCalibrationObject::Fetch() WARNING: "
	"No Hot/Dead Channels read from the Database." << endl;
    }

  iFlag = 0;
  return True;

}

//-----------------------------------------------------------------------------------

/// Fetch Absolute gains from a Database
PHBoolean TecCalibrationObject::FetchAbsGain()
{

  PdbADCChan* achan = 0;
  int bankid = 0;
  float G1, G2;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  if (application->startRead())
    {

      // Fetch corresponding bank
      PHTimeStamp tSearch = Tsearch;
      PdbBankID bankID;
      const char* calibname = CalibNameA2;
      bankid = TECCAL2BANK;
      bankID.setInternalValue(bankid);

      if (Debug > 0)
        {
          cout << "TeccalibrationObject::FetchAbsGain: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::FetchAbsGain: bankid = " << bankid << endl;
          cout << "TecCalibrationObject::FetchAbsGain: Search Time = ";
          tSearch.print();
          cout << endl;
        }

      tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, tSearch);

      if (tecBank)
        {
          if (Debug > 1)
            {
              tecBank->print();
            }
          if (Debug > 1)
            {
              cout << "Number of Channels = " << tecBank->getLength() << endl;
            }
          for (int i = 0; i < (int)tecBank->getLength(); i++)
            {
              achan = (PdbADCChan*) & (tecBank->getEntry(i));
              G1 = achan->getParameter(0);
              G2 = achan->getParameter(1);

              AbsoluteGain[i] = G1;

              if (Debug > 2)
                {
                  cout << "  Gains: " << G1 << " " << G2 << endl;
                }
            }
          delete tecBank;
        }
      else
        {
          cerr << "TecCalibrationObject::FetchAbsGain() ERROR: bankManager returned zero-pointer." << endl;
          return False;
        }

      application->commit();
    }
  else
    {
      application->abort();
      cerr << "TecCalibrationObject::FetchAbsGain() ERROR: Transaction aborted." << endl;
      return False;
    }

  iFlag = 0;
  return True;
}

//-----------------------------------------------------------------------------------

/// Fetch Absolute gains from a Database
PHBoolean TecCalibrationObject::FetchAbsGain(int runnumber)
{

  int bankid = 0;
  float G1, G2;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan* achan = 0;

  if (application->startRead())
    {

      // Fetch corresponding bank
      PdbBankID bankID;
      const char* calibname = CalibNameA2;
      bankid = TECCAL2BANK;
      bankID.setInternalValue(bankid);

      if (Debug > 0)
        {
          cout << "TeccalibrationObject::FetchAbsGain: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::FetchAbsGain: bankid = " << bankid << endl;
          cout << "TecCalibrationObject::FetchAbsGain: Search run # " << runnumber << endl;
        }

      tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, runnumber);

      if (tecBank)
        {
          if (Debug > 1)
            tecBank->print();
          if (Debug > 1)
            cout << "Number of Channels = " << tecBank->getLength() << endl;
          for (int i = 0; i < (int)tecBank->getLength(); i++)
            {
              achan = (PdbADCChan*) & (tecBank->getEntry(i));
              G1 = achan->getParameter(0);
              G2 = achan->getParameter(1);

              AbsoluteGain[i] = G1;

              if (Debug > 2)
                cout << "  Gains: " << G1 << " " << G2 << endl;
            }
        }
      else
        {
          cerr << "TecCalibrationObject::FetchAbsGain() ERROR: bankManager returned zero-pointer." << endl;
          return False;
        }

      application->commit();
    }
  else
    {
      application->abort();
      cerr << "TecCalibrationObject::FetchAbsGain() ERROR: Transaction aborted." << endl;
      return False;
    }

  iFlag = 0;
  if (tecBank)
    delete tecBank;
  return True;
}

//----------------------------------------------------------------------------------

/// Fetch Timing calibration constants from a Database
PHBoolean TecCalibrationObject::FetchTimingGain()
{

  PdbADCChan* achan = 0;
  int bankid = 0, irc, irow, icol;
  float G1, G2, G3;

  irow = 0;
  icol = 0;
  irc = 0;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  if (application->startRead())
    {

      // Fetch corresponding bank
      PHTimeStamp tSearch = Tsearch;
      PdbBankID bankID;
      const char* calibname = CalibNameT;

      bankid = TECCALTBANK;
      bankID.setInternalValue(bankid);

      if (Debug > 0)
        {
          cout << "TecCalibrationObject::FetchTimingGain: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::FetchTimingGain: bankid = " << bankid << endl;
          cout << "TecCalibrationObject::FetchTimingGain: Search Time = ";
          tSearch.print();
          cout << endl;
        }

      tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, tSearch);

      if (tecBank)
        {
          if (Debug > 1)
            tecBank->print();
          if (Debug > 1)
            cout << "Number of Channels = "
		 << tecBank->getLength() << endl;
          for (int i = 0; i < (int)tecBank->getLength(); i++)
            {
              achan = (PdbADCChan*) & (tecBank->getEntry(i));
              G1 = achan->getParameter(0);
              G2 = achan->getParameter(1);
              G3 = achan->getParameter(2);

              TimingGain[irow][icol] = G1;
              if (icol == 0)
                {
                  FirstTimeBin[irow] = (int)G2;
                  LastTimeBin[irow] = (int)G3;
                }
              icol++;
              irc++;
              if (icol == TECMAXTIMEBIN)
                {
                  icol = 0;
                  irow++;
                }

              if (Debug > 2)
                cout << "  Gains: " << G1 << " " << G2 << " " << G3 << endl;
            }
        }
      else
        {
          cerr << "TecCalibrationObject::FetchTimingGain() ERROR: bankManager returned zero-pointer." << endl;
          return False;
        }

      application->commit();
    }
  else
    {
      application->abort();
      cerr << "TecCalibrationObject::FetchTimingGain() ERROR: Transaction aborted." << endl;
      return False;
    }

  iFlag = 0;
  if (tecBank)
    delete tecBank;
  return True;
}


//----------------------------------------------------------------------------------

/// Fetch Timing calibration constants from a Database
PHBoolean TecCalibrationObject::FetchTimingGain(int runnumber)
{

  int irc, irow, icol;
  float G1, G2, G3;

  irow = 0;
  icol = 0;
  irc = 0;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan* achan = 0;

  if (application->startRead())
    {

      // Fetch corresponding bank
      PdbBankID bankID;
      const char* calibname = CalibNameT;

      int bankid = TECCALTBANK;
      bankID.setInternalValue(bankid);

      if (Debug > 0)
        {
          cout << "TecCalibrationObject::FetchTimingGain: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::FetchTimingGain: bankid = " << bankid << endl;
          cout << "TecCalibrationObject::FetchTimingGain: Search for run # " << runnumber << endl;
        }

      tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, runnumber);

      if (tecBank)
        {
          if (Debug > 1)
            tecBank->print();
          if (Debug > 1)
            cout << "Number of Channels = "
		 << tecBank->getLength() << endl;
          for (int i = 0; i < (int)tecBank->getLength(); i++)
            {
              achan = (PdbADCChan*) & (tecBank->getEntry(i));
              G1 = achan->getParameter(0);
              G2 = achan->getParameter(1);
              G3 = achan->getParameter(2);

              TimingGain[irow][icol] = G1;
              if (icol == 0)
                {
                  FirstTimeBin[irow] = (int)G2;
                  LastTimeBin[irow] = (int)G3;
                }
              icol++;
              irc++;
              if (icol == TECMAXTIMEBIN)
                {
                  icol = 0;
                  irow++;
                }

              if (Debug > 2)
                cout << "  Gains: " << G1 << " " << G2 << " " << G3 << endl;
            }
        }
      else
        {
          cerr << "TecCalibrationObject::FetchTimingGain() ERROR: bankManager returned zero-pointer." << endl;
          return False;
        }

      application->commit();
    }
  else
    {
      application->abort();
      cerr << "TecCalibrationObject::FetchTimingGain() ERROR: Transaction aborted." << endl;
      return False;
    }

  iFlag = 0;
  if (tecBank)
    delete tecBank;
  return True;
}

//----------------------------------------------------------------------------------

/// Fetch a list of Hot/Dead Channels from a Database
PHBoolean TecCalibrationObject::FetchHotDead()
{

  float G1;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan* achan = 0;

  if (application->startRead())
    {

      // Fetch corresponding bank
      PHTimeStamp tSearch = Tsearch;
      PdbBankID bankID;
      const char* calibname = CalibNameA1;

      int bankid = TECCAL1BANK + TECMAXINDEX;
      bankID.setInternalValue(bankid);

      if (Debug > 0)
        {
          cout << "TecCalibrationObject::FetchHotDead: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::FetchHotDead: bankid = " << bankid << endl;
          cout << "TecCalibrationObject::FetchHotDead: Search Time = ";
          tSearch.print();
          cout << endl;
        }

      tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, tSearch);

      if (tecBank)
        {
          if (Debug > 1)
            tecBank->print();
          if (Debug > 1)
            cout << "Number of Channels = "
		 << tecBank->getLength() << endl;
          for (int i = 0; i < (int)tecBank->getLength(); i++)
            {
              achan = (PdbADCChan*) & (tecBank->getEntry(i));
              G1 = achan->getParameter(0);

              HotDeadList[i] = (int)G1;

              if (Debug > 1)
                cout << "  Hot/Dead channel: " << G1 << endl;
            }
        }
      else
        {
          cerr << "TecCalibrationObject::FetchHotDead() ERROR: "
	       << "bankManager returned zero-pointer." << endl;
          return False;
        }

      application->commit();
    }
  else
    {
      application->abort();
      cerr << "TecCalibrationObject::FetchHotDead() ERROR: "
	   << "Transaction aborted." << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;

}

//----------------------------------------------------------------------------------

PHBoolean TecCalibrationObject::FetchHotDead(int runnumber)
{

  int bankid = 0;
  float G1;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan* achan = 0;

  if (application->startRead())
    {

      // Fetch corresponding bank
      PdbBankID bankID;
      const char* calibname = CalibNameA1;

      bankid = TECCAL1BANK + TECMAXINDEX;
      bankID.setInternalValue(bankid);

      if (Debug > 0)
        {
          cout << "TecCalibrationObject::FetchHotDead: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::FetchHotDead: bankid = " << bankid << endl;
          cout << "TecCalibrationObject::FetchHotDead: Search for run # " << runnumber << endl;
        }

      tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, runnumber);
      if (tecBank)
        {
          if (Debug > 1)
            tecBank->print();
          if (Debug > 1)
            cout << "Number of Channels = "
		 << tecBank->getLength() << endl;
          for (int i = 0; i < (int)tecBank->getLength(); i++)
            {
              achan = (PdbADCChan*) & (tecBank->getEntry(i));
              G1 = achan->getParameter(0);

              HotDeadList[i] = (int)G1;

              if (Debug > 1)
                cout << "  Hot/Dead channel: " << G1 << endl;
            }
        }
      else
        {
          cerr << "TecCalibrationObject::FetchHotDead() ERROR: "
	       << "bankManager returned zero-pointer." << endl;
          return False;
        }

      application->commit();
    }
  else
    {
      application->abort();
      cerr << "TecCalibrationObject::FetchHotDead() ERROR: "
	   << "Transaction aborted." << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;

}

//----------------------------------------------------------------------------------

/// Fetch Relative gains from a Database
PHBoolean TecCalibrationObject::FetchRelGain()
{

  PdbADCChan* achan = 0;
  int bankid = 0;
  float G1, G2;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  // Loop over chambers
  for (int index = 0; index < TECMAXINDEX; index++)
    {

      if (application->startRead())
        {

          // Fetch corresponding bank
          PHTimeStamp tSearch = Tsearch;
          PdbBankID bankID;
          const char* calibname = CalibNameA1;

          bankid = TECCAL1BANK + index;
          bankID.setInternalValue(bankid);

          if (Debug > 0)
            {
              cout << "TecCalibrationObject::FetchRelGain: calibname = " << calibname << endl;
              cout << "TecCalibrationObject::FetchRelGain: bankid = " << bankid << endl;
              cout << "TecCalibrationObject::FetchRelGain: Search Time = ";
              tSearch.print();
              cout << endl;
            }

          tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, tSearch);

          if (tecBank)
            {
              if (Debug > 1)
                {
                  tecBank->print();
                }
              if (Debug > 1)
                {
                  cout << "Number of Channels = "
		       << tecBank->getLength() << endl;
                }
              for (int i = 0; i < (int)tecBank->getLength(); i++)
                {
                  achan = (PdbADCChan*) & (tecBank->getEntry(i));
                  G1 = achan->getParameter(0);
                  G2 = achan->getParameter(1);

                  RelativeGain[index][i] = G1;

                  if (Debug > 2)
                    {
                      cout << "  Gains: " << G1 << " " << G2 << endl;
                    }
                }
              delete tecBank;
            }
          else
            {
              cerr << "TecCalibrationObject::FetchRelGain() ERROR: bankManager returned zero-pointer." << endl;
              return False;
            }

          application->commit();
        }
      else
        {
          application->abort();
          cerr << "TecCalibrationObject::FetchRelGain() ERROR: Transaction aborted." << endl;
          return False;
        }

    } // end loop over indices

  iFlag = 0;
  return True;
}

//---------------------------------------------------------------------
/// Fetch Relative gains from a Database
PHBoolean TecCalibrationObject::FetchRelGain(int runnumber)
{

  float G1, G2;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan* achan = 0;

  // Loop over chambers
  for (int index = 0; index < TECMAXINDEX; index++)
    {

      if (application->startRead())
        {

          // Fetch corresponding bank
          PdbBankID bankID;
          const char* calibname = CalibNameA1;

          int bankid = TECCAL1BANK + index;
          bankID.setInternalValue(bankid);

          if (Debug > 0)
            {
              cout << "TecCalibrationObject::FetchRelGain: calibname = " << calibname << endl;
              cout << "TecCalibrationObject::FetchRelGain: bankid = " << bankid << endl;
              cout << "TecCalibrationObject::FetchRelGain: Search for run # " << runnumber << endl;
            }

          tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, runnumber);

          if (tecBank)
            {
              if (Debug > 1)
                tecBank->print();
              if (Debug > 1)
                cout << "Number of Channels = "
		     << tecBank->getLength() << endl;
              for (int i = 0; i < (int)tecBank->getLength(); i++)
                {
                  achan = (PdbADCChan*) & (tecBank->getEntry(i));
                  G1 = achan->getParameter(0);
                  G2 = achan->getParameter(1);

                  RelativeGain[index][i] = G1;

                  if (Debug > 2)
                    cout << "  Gains: " << G1 << " " << G2 << endl;
                }
            }
          else
            {
              cerr << "TecCalibrationObject::FetchRelGain() ERROR: bankManager returned zero-pointer." << endl;
              return False;
            }

          application->commit();
        }
      else
        {
          application->abort();
          cerr << "TecCalibrationObject::FetchRelGain() ERROR: Transaction aborted." << endl;
          return False;
        }

    } // end loop over indices

  iFlag = 0;
  if (tecBank)
    delete tecBank;
  return True;
}

//---------------------------------------------------------------------

PHBoolean TecCalibrationObject::UpdateTimeCalib(int runnumber)
{

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan *achan = 0;

  if (Debug > 0)
    cout << "TecCalibrationObject::UpdateTimeCalib: Opening FD in update mode..." << endl;
  if (application->startUpdate())
    {
      PdbBankID bankID;
      const char *descrip = Description;
      const char *calibname = CalibNameT;
      int bankid = TECCALTBANK;
      bankID.setInternalValue(bankid);
      if (Debug > 0)
        {
          cout << "TecCalibrationObject::UpdateTimeCalib: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::UpdateTimeCalib: bankid = " << bankid << endl;
          cout << "Valid for run # " << runnumber << endl;
        }

      tecBank = bankManager->createBank(runnumber, runnumber, "PdbADCChanBank", bankID, descrip, calibname);

      tecBank->setLength(TECMAXINDEX*TECMAXTIMEBIN);
      if (Debug > 1)
        tecBank->print();

      for (int i = 0; i < TECMAXINDEX; i++)
        {
          for (int j = 0; j < TECMAXTIMEBIN; j++)
            {

              int ij = j + i * TECMAXTIMEBIN;

              float myTimeCal = TimingGain[i][j];
              float firstbin = FirstTimeBin[i];
              float lastbin = LastTimeBin[i];

              achan = (PdbADCChan*) & (tecBank->getEntry(ij));
              achan->setParameter(0, myTimeCal);
              achan->setParameter(1, firstbin);
              achan->setParameter(2, lastbin);
              achan->setParError(0, 0.0);
              achan->setParError(1, 0.0);
              achan->setParError(2, 0.0);
            } // end i loop
        } // end j loop

      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateTimeCalib ERROR: " <<
	"failed to start application for update." << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;

}

//-------------------------------------------------------------------------------------

PHBoolean TecCalibrationObject::UpdateTimeCalib(PHTimeStamp* Tbeg, PHTimeStamp* Tend)
{

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  if (Debug > 0)
    cout << "TecCalibrationObject::UpdateTimeCalib: Opening FD in update mode..." << endl;
  if (application->startUpdate())
    {
      PHTimeStamp tStart = *Tbeg;
      PHTimeStamp tStop = *Tend;
      PdbBankID bankID;
      const char *descrip = Description;
      const char *calibname = CalibNameT;
      int bankid = TECCALTBANK;
      bankID.setInternalValue(bankid);
      if (Debug > 0)
        {
          cout << "TecCalibrationObject::UpdateTimeCalib: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::UpdateTimeCalib: bankid = " << bankid << endl;
          cout << "Start validity = ";
          tStart.print();
          cout << endl;
          cout << "End validity = ";
          tStop.print();
          cout << endl;
        }

      tecBank = bankManager->createBank("PdbADCChanBank", bankID, descrip, tStart, tStop, calibname);

      int itmp = TECMAXINDEX * TECMAXTIMEBIN;
      tecBank->setLength(itmp);

      if (Debug > 1)
        tecBank->print();

      PdbADCChan *achan;

      for (int i = 0; i < TECMAXINDEX; i++)
        {
          for (int j = 0; j < TECMAXTIMEBIN; j++)
            {

              int ij = j + i * TECMAXTIMEBIN;

              float myTimeCal = TimingGain[i][j];
              float firstbin = FirstTimeBin[i];
              float lastbin = LastTimeBin[i];

              achan = (PdbADCChan*) & (tecBank->getEntry(ij));
              achan->setParameter(0, myTimeCal);
              achan->setParameter(1, firstbin);
              achan->setParameter(2, lastbin);
              achan->setParError(0, 0.0);
              achan->setParError(1, 0.0);
              achan->setParError(2, 0.0);
            } // end i loop
        } // end j loop

      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateTimeCalib ERROR: failed to start application for update." << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;

}

//---------------------------------------------------------------------

PHBoolean
TecCalibrationObject::UpdateHotDead(int runnumber)
{
  // Count number of hot/dead wires and return if there are none
  int nhotdead = 0;

  for (int i = 0; i < TECMAXNUMHOT; i++)
    {
      if (HotDeadList[i] > -1)
        {
          nhotdead++;
        }
    }
  if (nhotdead == 0)
    {
      return True;
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan *achan = 0;

  if (Debug > 0)
    {
      cout << "TecCalibrationObject::UpdateHotDead: " << "opening FD in update mode..." << endl;
    }

  if (application->startUpdate())
    {

      PdbBankID bankID;
      const char *descrip = "List of Hot and Dead Wires";
      const char *calibname = CalibNameA1;
      int bankid = TECCAL1BANK + TECMAXINDEX;
      bankID.setInternalValue(bankid);

      tecBank = bankManager->createBank(runnumber, runnumber, "PdbADCChanBank", bankID, descrip, calibname);

      tecBank->setLength(nhotdead);
      if (Debug > 1)
        {
          tecBank->print();
        }

      for (int i = 0; i < (int)tecBank->getLength(); i++)
        {

          float hd = HotDeadList[i];

          achan = (PdbADCChan*) & (tecBank->getEntry(i));
          achan->setParameter(0, hd);
          achan->setParameter(1, 0.0);
          achan->setParameter(2, 0.0);
          achan->setParError(0, 0.0);
          achan->setParError(1, 0.0);
          achan->setParError(2, 0.0);
        }
      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateHotDead ERROR: "
	   << "Failed to start application for update." << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;
}

//---------------------------------------------------------------------

PHBoolean
TecCalibrationObject::UpdateHotDead(PHTimeStamp* Tbeg, PHTimeStamp* Tend)
{
  // Count number of hot/dead wires and return if there are none
  int nhotdead = 0;

  for (int i = 0; i < TECMAXNUMHOT; i++)
    {
      if (HotDeadList[i] > -1)
        {
          nhotdead++;
        }
    }
  if (nhotdead == 0)
    {
      return True;
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan *achan = 0;

  if (Debug > 0)
    {
      cout << "TecCalibrationObject::UpdateHotDead: " << "opening FD in update mode..." << endl;
    }

  if (application->startUpdate())
    {

      PHTimeStamp tStart = *Tbeg;
      PHTimeStamp tStop = *Tend;
      PdbBankID bankID;
      const char *descrip = "List of Hot and Dead Wires";
      const char *calibname = CalibNameA1;
      int bankid = TECCAL1BANK + TECMAXINDEX;
      bankID.setInternalValue(bankid);

      tecBank = bankManager->createBank("PdbADCChanBank", bankID, descrip, tStart, tStop, calibname);

      tecBank->setLength(nhotdead);
      if (Debug > 1)
        {
          tecBank->print();
        }

      for (int i = 0; i < (int)tecBank->getLength(); i++)
        {
          float hd = HotDeadList[i];

          achan = (PdbADCChan*) & (tecBank->getEntry(i));
          achan->setParameter(0, hd);
          achan->setParameter(1, 0.0);
          achan->setParameter(2, 0.0);
          achan->setParError(0, 0.0);
          achan->setParError(1, 0.0);
          achan->setParError(2, 0.0);
        }
      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateHotDead ERROR: "
	   << "Failed to start application for update." << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;
}

//---------------------------------------------------------------------

PHBoolean TecCalibrationObject::UpdateRelGain(int runnumber)
{

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbADCChan *achan = 0;
  PdbCalBank *tecBank = 0;

  if (Debug > 0)
    cout << "TecCalibrationObject::UpdateRelGain: opening FD in update mode..." << endl;
  if (application->startUpdate())
    {
      PdbBankID bankID;
      const char *descrip = Description;
      const char *calibname = CalibNameA1;

      for (int is = 0; is < TECMAXSECT; is++)
        {

          for (int ip = 0; ip < TECMAXPLANE; ip++)
            {

              for (int iz = 0; iz < TECMAXSIDE; iz++)
                {
                  int index = is * TECMAXPLANE * TECMAXSIDE + ip * TECMAXSIDE + iz;
                  int bankid = TECCAL1BANK + index;

                  bankID.setInternalValue(bankid);

                  if (Debug > 0)
                    cout << "Filling database for chamber # " << index << endl;

                  tecBank = bankManager->createBank(runnumber, runnumber, "PdbADCChanBank", bankID, descrip, calibname);

                  if (tecBank)
                    {

                      int itmp = TecGeometryObject::get_NumWires(ip);
                      tecBank->setLength(itmp);

                      if (Debug > 1)
                        tecBank->print();

                      for (int i = 0; i < (int)tecBank->getLength(); i++)
                        {

                          float myGain = RelativeGain[index][i];

                          achan = (PdbADCChan*) & (tecBank->getEntry(i));
                          achan->setParameter(0, myGain);
                          achan->setParameter(1, 0.0);
                          achan->setParameter(2, 0.0);
                          achan->setParError(0, 0.0);
                          achan->setParError(1, 0.0);
                          achan->setParError(2, 0.0);
                        } // end i loop over wires

                      delete tecBank;
                    } // tecBank!=0

                } // end iz loop over z-sides
            } // end ip loop over planes
        } // end is loop over sectors

      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateRelGain ERROR: failed to start application for update." << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;
}

//---------------------------------------------------------------------

PHBoolean TecCalibrationObject::UpdateRelGain(PHTimeStamp* Tbeg, PHTimeStamp* Tend)
{

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbADCChan *achan = 0;
  PdbCalBank *tecBank = 0;


  if (Debug > 0)
    cout << "TecCalibrationObject::UpdateRelGain: opening FD in update mode..." << endl;
  if (application->startUpdate())
    {
      PHTimeStamp tStart = *Tbeg;
      PHTimeStamp tStop = *Tend;
      PdbBankID bankID;
      const char *descrip = Description;
      const char *calibname = CalibNameA1;

      for (int is = 0; is < TECMAXSECT; is++)
        {

          for (int ip = 0; ip < TECMAXPLANE; ip++)
            {

              for (int iz = 0; iz < TECMAXSIDE; iz++)
                {
                  int index = is * TECMAXPLANE * TECMAXSIDE + ip * TECMAXSIDE + iz;
                  int bankid = TECCAL1BANK + index;

                  bankID.setInternalValue(bankid);

                  if (Debug > 0)
                    cout << "Filling database for chamber # " << index << endl;

                  tecBank = bankManager->createBank("PdbADCChanBank", bankID, descrip, tStart, tStop, calibname);
                  if (tecBank)
                    {

                      int itmp = TecGeometryObject::get_NumWires(ip);
                      tecBank->setLength(itmp);

                      if (Debug > 1)
                        tecBank->print();

                      for (int i = 0; i < (int)tecBank->getLength(); i++)
                        {

                          float myGain = RelativeGain[index][i];

                          achan = (PdbADCChan*) & (tecBank->getEntry(i));
                          achan->setParameter(0, myGain);
                          achan->setParameter(1, 0.0);
                          achan->setParameter(2, 0.0);
                          achan->setParError(0, 0.0);
                          achan->setParError(1, 0.0);
                          achan->setParError(2, 0.0);
                        } // end i loop over wires

                      delete tecBank;
                    } // tecBank!=0

                } // end iz loop over z-sides
            } // end ip loop over planes
        } // end is loop over sectors

      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateRelGain ERROR: failed to start application for update." << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;
}

//---------------------------------------------------------------------

PHBoolean TecCalibrationObject::UpdateAbsGain(int RunNumber)
{

  int totnent = TECMAXSECT * TECMAXPLANE * TECMAXSIDE;
  PdbADCChan *achan = 0;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  if (Debug > 0)
    cout << "TecCalibrationObject::UpdateAbsGain: opening FD in update mode..." << endl;
  if (application->startUpdate())
    {

      PdbBankID bankID;
      const char *descrip = Description;
      const char *calibname = CalibNameA2;
      int bankid = TECCAL2BANK;
      bankID.setInternalValue(bankid);
      if (Debug > 0)
        {
          cout << "TecCalibrationObject::UpdateAbsGain: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::UpdateAbsGain: bankid = " << bankid << endl;
          cout << "Validity: Run #  " << RunNumber << endl;
        }

      tecBank = bankManager->createBank(RunNumber, RunNumber,
                                        "PdbADCChanBank",
                                        bankID,
                                        descrip,
                                        calibname);
      tecBank->setLength(totnent);
      if (Debug > 1)
        tecBank->print();

      for (int i = 0; i < (int)tecBank->getLength(); i++)
        {
          float myGain = AbsoluteGain[i];
          achan = (PdbADCChan*) & (tecBank->getEntry(i));
          achan->setParameter(0, myGain);
          achan->setParameter(1, 0.0);
          achan->setParameter(2, 0.0);
          achan->setParError(0, 0.0);
          achan->setParError(1, 0.0);
          achan->setParError(2, 0.0);
        } // end i loop over wires

      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateAbsGain ERROR: failed to start application for update" << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;
}



//---------------------------------------------------------------------

PHBoolean TecCalibrationObject::UpdateAbsGain(PHTimeStamp* Tbeg, PHTimeStamp* Tend)
{

  int totnent = TECMAXSECT * TECMAXPLANE * TECMAXSIDE;
  PdbADCChan *achan = 0;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  if (Debug > 0)
    cout << "TecCalibrationObject::UpdateAbsGain: opening FD in update mode..." << endl;
  if (application->startUpdate())
    {
      PHTimeStamp tStart = *Tbeg;
      PHTimeStamp tStop = *Tend;
      PdbBankID bankID;
      const char *descrip = Description;
      const char *calibname = CalibNameA2;
      int bankid = TECCAL2BANK;

      if (Debug > 0)
        {
          cout << "TecCalibrationObject::UpdateAbsGain: calibname = " << calibname << endl;
          cout << "TecCalibrationObject::UpdateAbsGain: bankid = " << bankid << endl;
          cout << "Start validity = ";
          tStart.print();
          cout << endl;
          cout << "End validity = ";
          tStop.print();
          cout << endl;
        }

      bankID.setInternalValue(bankid);

      tecBank = bankManager->createBank("PdbADCChanBank", bankID, descrip, tStart, tStop, calibname);

      int itmp = totnent;
      tecBank->setLength(itmp);

      if (Debug > 1)
        tecBank->print();

      for (unsigned int i = 0; i < tecBank->getLength(); i++)
        {

          float myGain = AbsoluteGain[i];

          achan = (PdbADCChan*) & (tecBank->getEntry(i));
          achan->setParameter(0, myGain);
          achan->setParameter(1, 0.0);
          achan->setParameter(2, 0.0);
          achan->setParError(0, 0.0);
          achan->setParError(1, 0.0);
          achan->setParError(2, 0.0);
        } // end i loop over wires
      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateAbsGain ERROR: failed to start application for update" << endl;
      return False;
    }

  if (tecBank)
    delete tecBank;
  return True;
}

/// Set Noise
void TecCalibrationObject::setNoise(int glindex, int adc, float noise)
{

  int iwire = glindex % 1000;
  int index = glindex / 1000;

  int itmp = TecGeometryObject::get_NumWires((index % 12) / 2);
  if (iwire < itmp)
    Noise[index][iwire][adc] = noise;
}

void TecCalibrationObject::setNoise(TecAddressObject* TAO, int adc, float noise)
{

  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int iwire = TAO->getWire();
  int index = iside + iplane * TECMAXSIDE + isector * TECMAXSIDE * TECMAXPLANE;

  Noise[index][iwire][adc] = noise;
}

/// Get Noise
float TecCalibrationObject::getNoise(int index, int iwire, int adc)
{
  return Noise[index][iwire][adc];
}

// Update Noise
int TecCalibrationObject::UpdateNoise(int runnumber)
{

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbTecPedestal *achan = 0;
  PdbCalBank *tecBank = 0;

  if (Debug > 0)
    cout << "TecCalibrationObject::UpdateNoise: opening FD in update mode..." << endl;
  if (application->startUpdate())
    {
      PdbBankID bankID;

      const char *description = "Wire/adc noise";
      const char *calibname = "calibtecnoise";

      for (int is = 0; is < TECMAXSECT; is++)
        {

          for (int ip = 0; ip < TECMAXPLANE; ip++)
            {

              for (int iz = 0; iz < TECMAXSIDE; iz++)
                {
                  int index = is * TECMAXPLANE * TECMAXSIDE + ip * TECMAXSIDE + iz;
                  int bankid = TECCAL1BANK + index;

                  bankID.setInternalValue(bankid);

                  if (Debug > 0)
                    cout << "Filling database for chamber # " << index << endl;

                  tecBank = bankManager->createBank(runnumber, runnumber, "PdbTecPedestalBank", bankID, description, calibname);

                  if (tecBank)
                    {

                      int itmp = TecGeometryObject::get_NumWires(ip);
                      tecBank->setLength(itmp);

                      if (Debug > 1)
                        tecBank->print();

                      for (int i = 0; i < (int)tecBank->getLength(); i++)
                        {

                          achan = (PdbTecPedestal*) & (tecBank->getEntry(i));

                          for (int iadc = 0; iadc < 32; iadc++)
                            {
                              achan->setParameter(iadc, Noise[index][i][iadc]);
                            } // end of loop over adc values
                        } // end i loop over wires

                    } // tecBank!=0
                  application->commit();
                  delete tecBank;
                } // end iz loop over z-sides
            } // end ip loop over planes
        } // end is loop over sectors
      if (Debug > 0)
        cout << "Comitting .." << endl;
      //      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateRelGain ERROR: failed to start application for update." << endl;
      return False;
    }

  //  if(tecBank) delete tecBank;

  return True;
}

int TecCalibrationObject::UpdateNoise(PHTimeStamp* Tbeg, PHTimeStamp* Tend)
{

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbTecPedestal *achan = 0;
  PdbCalBank *tecBank = 0;

  if (Debug > 0)
    cout << "TecCalibrationObject::UpdateNoise: opening FD in update mode..." << endl;
  if (application->startUpdate())
    {
      PHTimeStamp tStart = *Tbeg;
      PHTimeStamp tStop = *Tend;
      PdbBankID bankID;

      const char *description = "Wire/adc noise";
      const char *calibname = "calibtecnoise";

      for (int is = 0; is < TECMAXSECT; is++)
        {

          for (int ip = 0; ip < TECMAXPLANE; ip++)
            {

              for (int iz = 0; iz < TECMAXSIDE; iz++)
                {
                  int index = is * TECMAXPLANE * TECMAXSIDE + ip * TECMAXSIDE + iz;
                  int bankid = TECCAL1BANK + index;

                  bankID.setInternalValue(bankid);

                  if (Debug > 0)
                    {
                      cout << "Filling database for chamber # " << index << endl;
                      cout << "Start validity = ";
                      tStart.print();
                      cout << endl;
                      cout << "End validity = ";
                      tStop.print();
                      cout << endl;
                    }
                  tecBank = bankManager->createBank("PdbTecPedestalBank", bankID, description, tStart, tStop, calibname);

                  if (tecBank)
                    {

                      int itmp = TecGeometryObject::get_NumWires(ip);
                      tecBank->setLength(itmp);

                      if (Debug > 1)
                        tecBank->print();

                      for (int i = 0; i < (int)tecBank->getLength(); i++)
                        {

                          achan = (PdbTecPedestal*) & (tecBank->getEntry(i));

                          for (int iadc = 0; iadc < 32; iadc++)
                            {
                              achan->setParameter(iadc, Noise[index][i][iadc]);
                            } // end of loop over adc values
                        } // end i loop over wires

                    } // tecBank!=0
                  application->commit();
                  delete tecBank;
                } // end iz loop over z-sides
            } // end ip loop over planes
        } // end is loop over sectors
      if (Debug > 0)
        cout << "Comitting .." << endl;
      //      application->commit();
    }
  else
    {
      cerr << "TecCalibrationObject::UpdateRelGain ERROR: failed to start application for update." << endl;
      return False;
    }

  //  if(tecBank) delete tecBank;

  return True;
}

/// Fetch Noise
PHBoolean TecCalibrationObject::FetchNoise()
{

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbTecPedestal* achan = 0;

  // Loop over chambers
  PHTimeStamp tSearch = Tsearch;
  const char* calibname = "calibtecnoise"; //CalibNameA1;
  for (int index = 0; index < TECMAXINDEX; index++)
    {

      if (application->startRead())
        {

          // Fetch corresponding bank
          PdbBankID bankID;

          int bankid = TECCAL1BANK + index;
          bankID.setInternalValue(bankid);

          if (Debug > 0)
            {
              cout << "TecCalibrationObject::FetchNoise: calibname = " << calibname << endl;
              cout << "TecCalibrationObject::FetchNoise: bankid = " << bankid << endl;
            }

          tecBank = bankManager->fetchBank("PdbTecPedestalBank", bankID, calibname, tSearch);

          if (tecBank)
            {
              if (Debug > 1)
                tecBank->print();
              if (Debug > 1)
                cout << "Number of Channels = "
                << tecBank->getLength() << endl;
              for (int i = 0; i < (int)tecBank->getLength(); i++)
                {
                  achan = (PdbTecPedestal*) & (tecBank->getEntry(i));

                  for (int iadc = 0; iadc < 32 ; iadc++)
                    {
                      Noise[index][i][iadc] = achan->getParameter(iadc);
                      ;
                      if (Debug > 2)
                        cout << "  Noise[" << index << "][" << i << "]["
                        << iadc << "] : " << Noise[index][i][iadc] << endl;
                    }
                }
            }
          else
            {
              cerr << "TecCalibrationObject::FetchNoise() ERROR: bankManager returned zero-pointer." << endl;
              return False;
            }
          application->commit();
          delete tecBank;
        }
      else
        {
          application->abort();
          cerr << "TecCalibrationObject::FetchNoise() ERROR: Transaction aborted." << endl;
          return False;
        }

    } // end loop over indices

  iFlag = 0;
  //  if(tecBank) delete tecBank;
  return True;
}
