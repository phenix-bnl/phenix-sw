#include <ZdcMap.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbInt.hh>

#include <phool.h>

#include <cstdlib>

using namespace std;

ZdcMap::ZdcMap():
  description("Parameters submitted by ZdcMap class"),
  TStart(0),
  zdcchannelmap(NULL),
  zdcchansize(0)
{
  TStop.setToFarFuture();
  return;
}

ZdcMap::~ZdcMap()
{
  delete [] zdcchannelmap;
  return;
}

int *
ZdcMap::FillChannelMap(const PHTimeStamp &ts, unsigned int &size)
{
  FillMyArray(ts);
  int *array = new int[zdcchansize];
  size = zdcchansize;
  for (unsigned int i=0; i<zdcchansize;i++)
    {
      array[i] = zdcchannelmap[i];
    }
  return array;
}

void
ZdcMap::Print()
{
  cout << "Zdc PMT to Channel Map:" << endl;
  for (unsigned int i=0; i<zdcchansize; i++)
    {
      cout << "PMT " << i << " is at channel " << zdcchannelmap[i] << endl;
    }
  cout << "Validity Range from " << TStart
       << " to " << TStop << endl;
  return ;
}

int
ZdcMap::Commit()
{
  if (TStop <= TStart)
    {
      cout << "TStop " << TStop << " is smaller then TStart " << TStart << endl;
      return -1;
    }
  if (zdcchansize == 0)
    {
      cout << PHWHERE << " zdc channel array size is zero" << endl;
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
  //  Make a bank ID...
  PdbBankID bankID(0); // lets start at zero
  PdbCalBank *NewBank = bankManager->createBank("PdbIntBank",
						bankID,
						description.c_str(),
						TStart, TStop,
						"calibzdcpmtchannelmap");
  if (NewBank)
    {
      NewBank->setLength(zdcchansize);
      for (unsigned int n = 0; n < NewBank->getLength(); n++)
        {
          PdbInt *zdcchan = (PdbInt *) & (NewBank->getEntry(n));
          zdcchan->setValue(zdcchannelmap[n]);
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
ZdcMap::FillMyArray(const PHTimeStamp &ts)
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
					    "calibzdcpmtchannelmap",
					    ts);
  if (Bank)
    {
      delete [] zdcchannelmap;
      zdcchansize = Bank->getLength();
      zdcchannelmap = new int[zdcchansize];
      for (unsigned int i = 0; i < zdcchansize; i++)
        {
          PdbInt *zdcchan = (PdbInt *) & Bank->getEntry(i);
          zdcchannelmap[i] = zdcchan->getValue();
        }
      TStart = Bank->getStartValTime();
      TStop = Bank->getEndValTime();
      delete Bank;
    }
  else
    {
      cout << PHWHERE << "Failed to get zdc channel map info from DB" << endl;
      return -1;
    }
  return 0;

}

int
ZdcMap::verify_readback()
{
  // readback and crosscheck
  int iret = 0;
  unsigned int save_zdcchansize = zdcchansize;
  PHTimeStamp TStopbck = TStop;
  PHTimeStamp TStartbck = TStart;
  int *zdcarray = new int[zdcchansize];
  for (unsigned int i=0;i<zdcchansize;i++)
    {
      zdcarray[i] = zdcchannelmap[i];
    }
  // this updates zdcchansize by what's in the DB
  if (FillMyArray(TStartbck))
    {
      cout << PHWHERE << " Cannot read back zdc channel map" << endl;
      delete [] zdcarray;
      return -1;
    }
  if (zdcchansize != save_zdcchansize)
    {
      cout << PHWHERE << "Readback Problem - number of zdc channels changed from "
	   <<  save_zdcchansize << " to " << zdcchansize << endl;
      delete [] zdcarray;
      return -1;
    }
  for (unsigned int i=0;i<zdcchansize;i++)
    {
      if (zdcarray[i] != zdcchannelmap[i])
	{
	  cout << "ReadBack problem for channel " << i 
	       << " written " << zdcarray[i] 
	       << " read back: " << zdcchannelmap[i] << endl;
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
  delete [] zdcarray;
  return iret;
}

void
ZdcMap::SetArray(const int *array, const unsigned int size)
{
  delete [] zdcchannelmap;
  zdcchannelmap = new int[size];
  zdcchansize = size;
  for (unsigned int i=0;i<size;i++)
    {
      zdcchannelmap[i] = array[i];
    }
  return;
}
