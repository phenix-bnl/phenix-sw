#include "SvxDaqErrorMap.h"

#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <RunToTime.hh>
#include "PdbSvxDaqErrorMap.hh"


#include <iostream>
#include <fstream>

using namespace std;

static const unsigned int BANKID_VALUE = 0;
static const char *CALIB_TABLE    = "calibsvxdaqerror";
static const char *CALIB_BANKNAME = "PdbSvxDaqErrorMapBank";

SvxDaqErrorMap::SvxDaqErrorMap()
  : m_verbose(0)
{
  clearStatus();
}

SvxDaqErrorMap::~SvxDaqErrorMap()
{
}

void SvxDaqErrorMap::clearStatus()
{
  for(int imod=0; imod<SVXNMODULEPIXEL; imod++)
    {
      m_pixelStatus[imod] = 0;
    }

  for(int imod=0; imod<SVXNMODULESTRIP; imod++)
    {
      m_stripStatus[imod] = 0;
    }
}


bool SvxDaqErrorMap::readFromFile(const char* filename)
{
  ifstream fin(filename);

  if(!fin) 
    {
      cerr<< PHWHERE <<" failed to open file : "<<filename<<endl;
      return false;
    }

  ///////
  clearStatus();

  ///////
  int detector, module;
  char byte[256];
  while(fin>>detector>>module>>byte)
    {
      if(m_verbose>0) cout<<detector<<" "<<module<<" "<<byte<<endl;
      unsigned int status;
      sscanf(byte, "0x%x", &status);

      if(detector==0) // pixel
        {
           if(0<=module&&module<SVXNMODULEPIXEL)
             {
               m_pixelStatus[module] = status;
             }
        }
      else if(detector==1) // strip
        {
           if(0<=module&&module<SVXNMODULESTRIP)
             {
               m_stripStatus[module] = status;
             }
        }
      else 
        {
          cerr<< PHWHERE <<" Out of range detector: "<<detector<<endl;
        }
    }

  fin.close();

  return true;
}

bool SvxDaqErrorMap::writeToFile (const char* filename)
{
  // need to implement
  return true;
}

bool SvxDaqErrorMap::readFromDatabase(const int runnumber)
{
  if (runnumber<0) 
    {
      cerr << PHWHERE <<" Strange run = "<<runnumber<<endl;
      return false;
    }

  ////////////////////////
  // get starting time of the run
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(runnumber);

  if (Tsearch == NULL) 
    {
      cerr << PHWHERE <<" Error Tsearch==null : run=" << runnumber << endl;
      return false;
    }
  

  ////////////////////////
  // initialize I/F
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startRead()) 
    {
      application->abort();
      std::cerr << PHWHERE
                << " ERROR -> Transaction aborted. Database NOT available."
                << std::endl;
      return false;
    }


  /////////////////////////
  // pre-defined variables
  PdbBankID bankID(BANKID_VALUE);
  const char *tableName = CALIB_TABLE;
  const char *bankName  = CALIB_BANKNAME;
  
  if (m_verbose>90) 
    {
      cout << "tableName = " << tableName << endl;
      cout << "bankID = " << bankID.getInternalValue() << endl;
      cout << "Tsearch: " << *Tsearch << endl;
    }

  // reset first
  clearStatus();

  // read from database
  bool success = true;

  PdbCalBank *svxBank = bankManager->fetchBank(bankName, bankID,
                                               tableName, *Tsearch);

  if (svxBank) 
    {
      unsigned numRecords = (unsigned)svxBank->getLength();

      if (m_verbose>90) cout << __FUNCTION__<<" numRecords = " << numRecords << endl;
      
      for (unsigned int idx=0; idx<numRecords; idx++) 
        {
          PdbSvxDaqErrorMap *rec = (PdbSvxDaqErrorMap*) & (svxBank->getEntry(idx));
          setStatus(rec->getDetector(), rec->getModule(), rec->getStatus());
        }
    }
  else 
    {
      cerr << PHWHERE <<" ERROR -> bankManager returned null pointer."<<endl;
      success = false;
    }


  if (success) 
    {
      application->commit();
      if (m_verbose>90) cout << __FUNCTION__ << " commit()" << endl;
    }
  else 
    {
      application->abort();
      if (m_verbose>90) cout << __FUNCTION__ << " abort()" << endl;
    }
  
  if (svxBank!=NULL) delete svxBank;
  if (Tsearch!=NULL) delete Tsearch;
  
  return success;
}

bool SvxDaqErrorMap::writeToDatabase(const int runnumber)
{
  if (runnumber <0)
    {
      cerr << PHWHERE << " Strange run = " << runnumber <<endl;
      return false;
    }

  ///////////
  // check run-time
  RunToTime   *rt     = RunToTime::instance();
  PHTimeStamp *Tstart = rt->getBeginTime(runnumber);
  PHTimeStamp *Tstop  = rt->getEndTime(runnumber);

  char description[100];
  sprintf(description, "svxdaqerrormap_%010d", runnumber) ;

  bool success = writeToDatabase(Tstart, Tstop, description);

  if(Tstart!=NULL) delete Tstart;
  if(Tstop !=NULL) delete Tstop;

  return success;

/*
  if (Tstart == NULL) {
    cerr << PHWHERE << " Warning Tstart==NULL, taking current time" << endl;
    return false;
  }
  if (Tstop == NULL) {
    cerr << PHWHERE << " Warning Tstop==NULL, taking current time" << endl;
    return false;
  }


  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if ( !application->startUpdate() ) 
    {
      cerr << PHWHERE << "ERROR: Database not writable, aborting." << endl;
      return false;
  
    }


  ///////////////////
  // pre-definition

  const char *tableName = CALIB_TABLE;
  const char *bankName  = CALIB_BANKNAME;

  PdbBankID bankID(BANKID_VALUE);

  cout << "tableName   = " << tableName << endl;
  cout << "description = " << description << endl;
  cout << "bankID      = " << bankID.getInternalValue() << endl;
  cout << "Validity range: " << *Tstart << " - " << *Tstop << endl;

  ///////////////////
  // writring to database
  
  //here we have correspondence between PdbSvxPixelHotDeadMap structure and bank record length

  bool success = true;

  PdbCalBank *svxBank = bankManager->createBank(bankName,
                                                bankID, description,
                                                *Tstart, *Tstop, tableName);

  if (svxBank) 
    {
      // The number of records is same as the number of data in the map array
      unsigned int numRecords = getPixelNBad()+getStripNBad();
      svxBank->setLength(numRecords);
      
      // And now this loop writes out the data in the map
      unsigned int recordNumber = 0;
      
      for(int idet=0; idet<2; idet++)
        {
          int nmodule = (idet==0) ? SVXNMODULEPIXEL : SVXNMODULESTRIP;
          for (int imod=0; imod<nmodule; imod++) 
            {
              unsigned int status = getStatus(idet, imod);

              if ((status!=0) && (status!=ERRCODE))
                {
                  PdbSvxDaqErrorMap *rec =
                    (PdbSvxDaqErrorMap *) & (svxBank->getEntry(recordNumber));

                  rec->setStatus(idet, imod, status);
                  recordNumber++;
                }
            }
        }
    }
  else 
    {
      cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
      success = false;
    }

  if (m_verbose>90)  cout << __FUNCTION__ <<" success = " << success << endl; 

  if (success) 
    {
      application->commit();
      if (m_verbose>90) cout << PHWHERE << " commit()" << endl;
    }
  else 
    {
      application->abort();
      if (m_verbose>90) cout << PHWHERE << " abort()" << endl;
    }

  if(svxBank!=NULL) delete svxBank;
  if(Tstart !=NULL) delete Tstart;
  if(Tstop  !=NULL) delete Tstop;

  return success;
*/
}

bool SvxDaqErrorMap::writeToDatabase (PHTimeStamp* Tstart, PHTimeStamp *Tstop, const char *desc)
{
  if (Tstart == NULL) {
    cerr << PHWHERE << " Warning Tstart==NULL" << endl;
    return false;
  }
  if (Tstop == NULL) {
    cerr << PHWHERE << " Warning Tstop==NULL" << endl;
    return false;
  }
  if(desc==NULL){
    cerr << PHWHERE << " Warning desc==NULL" << endl;
    return false;
  }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if ( !application->startUpdate() ) 
    {
      cerr << PHWHERE << "ERROR: Database not writable, aborting." << endl;
      return false;
  
    }


  ///////////////////
  // pre-definition
  const char *tableName = CALIB_TABLE;
  const char *bankName  = CALIB_BANKNAME;

  PdbBankID bankID(BANKID_VALUE);

  cout << "tableName   = " << tableName << endl;
  cout << "description = " << desc << endl;
  cout << "bankID      = " << bankID.getInternalValue() << endl;
  cout << "Validity range: " << *Tstart << " - " << *Tstop << endl;

  ///////////////////
  // writring to database
  
  //here we have correspondence between PdbSvxPixelHotDeadMap structure and bank record length

  bool success = true;

  PdbCalBank *svxBank = bankManager->createBank(bankName,
                                                bankID, desc,
                                                *Tstart, *Tstop, tableName);

  if (svxBank) 
    {
      // The number of records is same as the number of data in the map array
      unsigned int numRecords = getPixelNBad()+getStripNBad();
      svxBank->setLength(numRecords);
      
      // And now this loop writes out the data in the map
      unsigned int recordNumber = 0;
      
      for(int idet=0; idet<2; idet++)
        {
          int nmodule = (idet==0) ? SVXNMODULEPIXEL : SVXNMODULESTRIP;
          for (int imod=0; imod<nmodule; imod++) 
            {
              unsigned int status = getStatus(idet, imod);

              if ((status!=0) && (status!=ERRCODE))
                {
                  PdbSvxDaqErrorMap *rec =
                    (PdbSvxDaqErrorMap *) & (svxBank->getEntry(recordNumber));

                  rec->setStatus(idet, imod, status);
                  recordNumber++;
                }
            }
        }
    }
  else 
    {
      cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
      success = false;
    }

  if (m_verbose>90)  cout << __FUNCTION__ <<" success = " << success << endl; 

  if (success) 
    {
      application->commit();
      if (m_verbose>90) cout << PHWHERE << " commit()" << endl;
    }
  else 
    {
      application->abort();
      if (m_verbose>90) cout << PHWHERE << " abort()" << endl;
    }

  if(svxBank!=NULL) delete svxBank;

  return success;
}


void SvxDaqErrorMap::printStatus()
{

  cout<<"Pixel"<<endl;
  for(int imod=0; imod<SVXNMODULEPIXEL; imod++)
    {
      cout<<imod<<" 0x"<<hex<<m_pixelStatus[imod]<<dec<<endl;
    }

  cout<<"Strip"<<endl;
  for(int imod=0; imod<SVXNMODULESTRIP; imod++)
    {
      cout<<imod<<" 0x"<<hex<<m_stripStatus[imod]<<dec<<endl;
    }
}

void SvxDaqErrorMap::printMaskModule()
{

  cout<<"Pixel: ";
  for(int imod=0; imod<SVXNMODULEPIXEL; imod++)
    {
      if(m_pixelStatus[imod]>0) cout<<imod<<" ";
    }
  cout<<endl;

  cout<<"Strip: ";
  for(int imod=0; imod<SVXNMODULESTRIP; imod++)
    {
      if(m_stripStatus[imod]>0) cout<<imod<<" ";
    }
  cout<<endl;
}

unsigned int SvxDaqErrorMap::getPixelStatus(const int module)
{
  if(0<=module&&module<SVXNMODULEPIXEL)
    {
      return m_pixelStatus[module];
    }
  
  return ERRCODE;
}

unsigned int SvxDaqErrorMap::getStripStatus(const int module)
{
  if(0<=module&&module<SVXNMODULESTRIP)
    {
      return m_stripStatus[module];
    }
  
  return ERRCODE;
}

unsigned int SvxDaqErrorMap::getStatus(const int detector, const int module)
{
  if(detector==PdbSvxDaqErrorMap::SVX_PIXEL)
    {
      return getPixelStatus(module);
    }
  else if(detector==PdbSvxDaqErrorMap::SVX_STRIP)
    {
      return getStripStatus(module);
    }
  else 
    {
      cerr<< PHWHERE <<" Out of range detector = "<<detector<<endl;
    }

  return ERRCODE;
}


void SvxDaqErrorMap::setPixelStatus(const int module, const unsigned int status)
{
  if(0<=module&&module<SVXNMODULEPIXEL)
    {
      m_pixelStatus[module] = status;
    }
  else 
    {
      cerr<< PHWHERE <<" Out of range module :"<<module<<endl;
    }
}

void SvxDaqErrorMap::setStripStatus(const int module, const unsigned int status)
{
  if(0<=module&&module<SVXNMODULESTRIP)
    {
      m_stripStatus[module] = status;
    }
}

void SvxDaqErrorMap::setStatus(const int detector, const int module, const unsigned int status)
{
  if(detector==PdbSvxDaqErrorMap::SVX_PIXEL)
    {
      setPixelStatus(module, status);
    }
  else if(detector==PdbSvxDaqErrorMap::SVX_STRIP)
    {
      setStripStatus(module, status);
    }
  else 
    {
      cerr<< PHWHERE <<" Out of range detector = "<<detector<<endl;
    }
}


int SvxDaqErrorMap::getPixelNBad()
{
  return getNBad(0);
}

int SvxDaqErrorMap::getStripNBad()
{
  return getNBad(1);
}

int SvxDaqErrorMap::getNBad(const int detector)
{
  int nmodule=0;
  unsigned int* status;
  if(detector==0) // pixel
    {
      nmodule = SVXNMODULEPIXEL;
      status  = m_pixelStatus;
    }
  else if(detector==1) // strip
    {
      nmodule = SVXNMODULESTRIP;
      status  = m_stripStatus;
    }


  int nbad=0;
  for(int imod=0; imod<nmodule; imod++)
    {
      if( status[imod] > 0 ) nbad++;
//      cout<<imod<<" 0x"<<hex<<status[imod]<<dec<<endl;
    }

  return nbad;
}

void SvxDaqErrorMap::test(){
  cout<<"NpixelBad : "<<getPixelNBad()<<endl;
  cout<<"NstripBad : "<<getStripNBad()<<endl;
}

bool SvxDaqErrorMap::compare(SvxDaqErrorMap& map)
{
  if(m_verbose>0)
    {
      cout<<"SvxDaqErrorMap::"<<__FUNCTION__<<endl;
      cout<<"idet : imod : status,status2 : result"<<endl;
    }

  int nfail[2]={0,0};
  for(int idet=0; idet<2; idet++)
    {
      int nmodule = (idet==0) ? SVXNMODULEPIXEL : SVXNMODULESTRIP;
      for (int imod=0; imod<nmodule; imod++) 
        {
          unsigned int status  = getStatus(idet, imod);
          unsigned int status2 = map.getStatus(idet, imod);

          if(m_verbose>0)
            {
              cout<<idet<<" "<<imod<<" ";
              cout<<" 0x"<<hex<<status<<dec;
              cout<<" 0x"<<hex<<status2<<dec;
            }

          if(status==status2) 
            {
              if(m_verbose>0) cout<<" OK";
            }
          else 
            {
              if(m_verbose>0) cout<<" FAIL";
              nfail[idet]++;
            }

          if(m_verbose>0) cout<<endl;
        }
    }

  bool result=(nfail[0]==0&&nfail[1]==0);

  cout<<"NFail : "<<nfail[0]<<" "<<nfail[1]<<endl;
  if(result) cout<<"result is OK"<<endl;
  else       cout<<"result is Failed"<<endl;

  return result;
}
