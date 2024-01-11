#include <lpcMap.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbInt.hh>

#include <phool.h>

#include <cstdlib>
#include <iostream>

using namespace std;

lpcMap::lpcMap():
  TStart(0)
{
  TStop.setToFarFuture();
  lpcchannelmap = 0;
  lpcchansize = 0;
  description="Parameters submitted by lpcMap class";
  return;
}

lpcMap::~lpcMap()
{
  delete [] lpcchannelmap;
  return;
}

int *
lpcMap::FillChannelMap(const PHTimeStamp &ts, unsigned int &size)
{
  FillMyArray(ts);
  int *array = new int[lpcchansize];
  size = lpcchansize;
  for (unsigned int i=0; i<lpcchansize;i++)
    {
      array[i] = lpcchannelmap[i];
    }
  return array;
}

void
lpcMap::Print()
{
  cout << "lpc PMT to Channel Map:" << endl;
  for (unsigned int i=0; i<lpcchansize; i++)
    {
      cout << "PMT " << i << " is at channel " << lpcchannelmap[i] << endl;
    }
  cout << "Validity Range from " << TStart
       << " to " << TStop << endl;
  return ;
}

int
lpcMap::Commit()
{
  if (TStop <= TStart)
    {
      cout << "TStop " << TStop << " is smaller then TStart " << TStart << endl;
      return -1;
    }
  if (lpcchansize == 0)
    {
      cout << PHWHERE << " lpc channel array size is zero" << endl;
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
      NewBank->setLength(lpcchansize);
      for (unsigned int n = 0; n < NewBank->getLength(); n++)
        {
          PdbInt *lpcchan = (PdbInt *) & (NewBank->getEntry(n));
          lpcchan->setValue(lpcchannelmap[n]);
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
lpcMap::FillMyArray(const PHTimeStamp &ts)
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
      delete [] lpcchannelmap;
      lpcchansize = Bank->getLength();
      lpcchannelmap = new int[lpcchansize];
      for (unsigned int i = 0; i < lpcchansize; i++)
        {
          PdbInt *lpcchan = (PdbInt *) & Bank->getEntry(i);
          lpcchannelmap[i] = lpcchan->getValue();
        }
      TStart = Bank->getStartValTime();
      TStop = Bank->getEndValTime();
      delete Bank;
    }
  else
    {
      cout << PHWHERE << "Failed to get lpc mapping info from DB" << endl;
      return -1;
    }
  return 0;

}

int
lpcMap::verify_readback()
{
  // readback and crosscheck
  int iret = 0;
  PHTimeStamp TStopbck = TStop;
  PHTimeStamp TStartbck = TStart;
  int lpcarray[lpcchansize];
  for (unsigned int i=0;i<lpcchansize;i++)
    {
      lpcarray[i] = lpcchannelmap[i];
    }
  FillMyArray(TStartbck);
  for (unsigned int i=0;i<lpcchansize;i++)
    {
      if (lpcarray[i] != lpcchannelmap[i])
	{
	  cout << "ReadBack problem for channel " << i 
	       << " written " << lpcarray[i] 
	       << " read back: " << lpcchannelmap[i] << endl;
	  iret = -1;
	}
    }
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
lpcMap::SetArray(const int *array, const unsigned int size)
{
  delete [] lpcchannelmap;
  lpcchannelmap = new int[size];
  lpcchansize = size;
  for (unsigned int i=0;i<size;i++)
    {
      lpcchannelmap[i] = array[i];
    }
  return;
}
