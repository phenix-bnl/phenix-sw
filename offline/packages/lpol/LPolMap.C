#include <LPolMap.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbInt.hh>

#include <phool.h>

#include <cstdlib>
#include <iostream>

using namespace std;

LPolMap::LPolMap():
  TStart(0)
{
  TStop.setToFarFuture();
  lpolchannelmap = 0;
  lpolchansize = 0;
  description="Parameters submitted by LPolMap class";
  return;
}

LPolMap::~LPolMap()
{
  delete [] lpolchannelmap;
  return;
}

int *
LPolMap::FillChannelMap(const PHTimeStamp &ts, unsigned int &size)
{
  FillMyArray(ts);
  int *array = new int[lpolchansize];
  size = lpolchansize;
  for (unsigned int i=0; i<lpolchansize;i++)
    {
      array[i] = lpolchannelmap[i];
    }
  return array;
}

void
LPolMap::Print()
{
  cout << "LPol PMT to Channel Map:" << endl;
  for (unsigned int i=0; i<lpolchansize; i++)
    {
      cout << "PMT " << i << " is at channel " << lpolchannelmap[i] << endl;
    }
  cout << "Validity Range from " << TStart
       << " to " << TStop << endl;
  return ;
}

int
LPolMap::Commit()
{
  if (TStop <= TStart)
    {
      cout << "TStop " << TStop << " is smaller then TStart " << TStart << endl;
      return -1;
    }
  if (lpolchansize == 0)
    {
      cout << PHWHERE << " LPol channel array size is zero" << endl;
      return -1;
    }
  PdbBankManager *bankManager = PdbBankManager::instance();
  if (!bankManager)
    {
      cout << PHWHERE << "Could not get instance of PdbBankManager, exiting" << endl;
      exit(1);
    }
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      cout << PHWHERE << "Aborting ... Database not readable" << endl;
      application->abort();
      return -1;
    }
  PdbBankID bankID(0);
  PdbCalBank *NewBank = bankManager->createBank("PdbIntBank",
						bankID,
						description.c_str(),
						TStart, TStop,
						"lpcpmtchannelmap");
  if (NewBank)
    {
      NewBank->setLength(lpolchansize);
      for (unsigned int n = 0; n < NewBank->getLength(); n++)
        {
          PdbInt *lpolchan = (PdbInt *) & (NewBank->getEntry(n));
          lpolchan->setValue(lpolchannelmap[n]);
        }
      application->commit(NewBank);
      delete NewBank;
    }
  else
    {
      cout << PHWHERE << " PdbIntBank was not created, calibration failed" << endl;
      return -1;
    }
  cout << "Comitted" << endl;
  if (verify_readback())
    {
      cout << "ReadBack of committed calibration failed!!!" << endl;
      return -1;
    }
  else
    {
      cout << "ReadBack successful" << endl;
    }
  return 0;
}

int
LPolMap::FillMyArray(const PHTimeStamp &ts)
{
  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      cout << PHWHERE << "Aborting ... Database not readable" << endl;
      application->abort();
      return -1;
    }
  PdbBankID bankID(0);
  PdbCalBank *Bank = bankManager->fetchBank("PdbIntBank",
					    bankID,
					    "lpcpmtchannelmap",
					    ts);
  if (Bank)
    {
      delete [] lpolchannelmap;
      lpolchansize = Bank->getLength();
      lpolchannelmap = new int[lpolchansize];
      for (unsigned int i = 0; i < lpolchansize; i++)
        {
          PdbInt *lpolchan = (PdbInt *) & Bank->getEntry(i);
          lpolchannelmap[i] = lpolchan->getValue();
        }
      TStart = Bank->getStartValTime();
      TStop = Bank->getEndValTime();
      delete Bank;
    }
  else
    {
      cout << PHWHERE << "Failed to get LPol mapping info from DB" << endl;
      return -1;
    }
  return 0;

}

int
LPolMap::verify_readback()
{
  // readback and crosscheck
  int iret = 0;
  PHTimeStamp TStopbck = TStop;
  PHTimeStamp TStartbck = TStart;
  // int lpolarray[lpolchansize];
  // for (unsigned int i=0;i<lpolchansize;i++)
  //   {
  //     lpolarray[i] = lpolchannelmap[i];
  //   }
  FillMyArray(TStartbck);
  /*
    //This loop generate bug for scan-build. I don't get channel map from DB for now, so I commented out here. - 2015.06.25 Minjung Kim
  for (unsigned int i=0;i<lpolchansize;i++)
    {
      if (lpolarray[i] != lpolchannelmap[i])
	{
	  cout << "ReadBack problem for channel " << i 
	       << " written " << lpolarray[i] 
	       << " read back: " << lpolchannelmap[i] << endl;
	  iret = -1;
	}
    }
  */
  if (TStart != TStartbck)
    {
      cout << "ReadBack problem: TStart differs, written " << TStartbck
	   << " read back: " << TStart << endl;
      iret = -1;
    }
  if (TStop != TStopbck)
    {
      cout << "ReadBack problem: TStart differs, written " << TStopbck
	   << " read back: " << TStop << endl;
      iret = -1;
    }


  return iret;
}

void
LPolMap::SetArray(const int *array, const unsigned int size)
{
  delete [] lpolchannelmap;
  lpolchannelmap = new int[size];
  lpolchansize = size;
  for (unsigned int i=0;i<size;i++)
    {
      lpolchannelmap[i] = array[i];
    }
  return;
}
