//----------------------------------------------------------------------------- 
//                                                                
// Created by: David Silvermyr
// Modified by Henrik Tydesjo and Rickard du Rietz 6/2-03
// Modified by Henrik Tydesjo 2/6-03
// Description: Implementation of PadCalibrationObject class
//                                                                
//-----------------------------------------------------------------------------

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <recoConsts.h>
#include <PdbPadBadCh.hh>
#include <PdbPadBadROC.hh>
#include <PHTimeStamp.h>
#include <recoConsts.h>
#include <PadCalibrationObject.hh> 
#include <stdlib.h>
#include <stdio.h>

using namespace std;

//
// This is the maximum length (in number of characters) in a line
// from the data file
//
#define MAXLENGTH 128

// Should we use memset to initialize arrays? (This can break on some 
// architecture)
//
#define USE_MEMSET         1

//
// Basic constructor
//


PadCalibrationObject::PadCalibrationObject()
{  
  //
  // Create and initialize a PadAddressObject
  //
  Debug        = 0;
  iFlag        = -1;
  numberBadCh  = 0;
  numberBadROC = 0;
  
  addressObj = new PadAddressObject;

  Tsearch.setToSystemTime();
  TUpdateStart.setToSystemTime();
  TUpdateStop.setToFarFuture();

  badroc     = new std::map<int, Roc*>;
  PCOnew     = 0;
  
}

//
// Destructor
//
PadCalibrationObject::~PadCalibrationObject() 
{ 
  delete addressObj;
  
  std::map<int,Roc*>::iterator it;
  
  for (it = badroc->begin(); it != badroc->end(); ++it) {
    delete it->second;
  }
  delete badroc;
}


//-----------------------------------------------------------------------------
//                Methods to FETCH stuff from database
//-----------------------------------------------------------------------------

//
// Fetch calibration constants from default ASCII files
//
PHBoolean
PadCalibrationObject::FetchFromFile()
{
  const char *filebadch="padbadch.txt";
  const char *filebadroc="padbadroc.txt";
  return PadCalibrationObject::FetchFromFile(filebadch,filebadroc);
}

//
// Fetch calibration constants from ASCII files
//
PHBoolean
PadCalibrationObject::FetchFromFile(const char* filebadch,
                                    const char* filebadroc)
{
  PHBoolean status = FetchBadChFromFile(filebadch);
  
  if (!status) {
    cerr << "PadCalibrationObject::FetchFromFile() ERROR reading Bad Channels."
         << endl;
    return False;
  }

  status = FetchBadROCFromFile(filebadroc);
  
  if (!status) {
    cerr << "PadCalibrationObject::FetchFromFile() ERROR reading Bad ROCs."
         << endl;
    return False;
  }

  iFlag = 0;
 
  return True;  
}

//
// Fetch bad channel info from default ASCII file
//
PHBoolean
PadCalibrationObject::FetchBadChFromFile()
{
  const char * filebadch = "padbadch.txt";
  return PadCalibrationObject::FetchBadChFromFile(filebadch);
}

//
// Fetch bad channel info from an ASCII file "filebadch"
//
PHBoolean
PadCalibrationObject::FetchBadChFromFile(const char * filebadch)
{

  FILE *fin;
  if((fin = fopen(filebadch,"r")) == NULL){ 
    cerr << "PadCalibrationObject::FetchBadChFromFile() Cannot open file.\n";
    return False;
  }

  //
  // Now, read from the file and put the bad channel data into memory.
  // The first line tells us how many bad channels have been found.
  //
  fscanf(fin,"%d\n", &numberBadCh); 
  
  if (numberBadCh > MAXBADCH) {
    cerr << "PadCalibrationObject::FetchBadChFromFile() "
         <<"Too many bad channels.\n";
    
    cerr << "Number of bad channels= " << numberBadCh
         << " Maximum allowed number= " << MAXBADCH
         << " (this is the number of channels we will store)\n";
    numberBadCh=MAXBADCH;
  }
  
  for (int i = 0; i < numberBadCh; i++) {
    fscanf(fin,"%d %d %d\n",
           &badch[i].packetid, &badch[i].channelid, &badch[i].padtype); 
  }
  
  fclose(fin);  
  
  iFlag = 0;
  
  return True;
}

//
// Fetch bad ROC info from default ASCII file
//
PHBoolean
PadCalibrationObject::FetchBadROCFromFile()
{
  const char * filebadroc="padbadroc.txt";
  return PadCalibrationObject::FetchBadROCFromFile(filebadroc);
}

//
// Fetch bad ROC info from an ASCII file "filebadroc"
//
PHBoolean
PadCalibrationObject::FetchBadROCFromFile(const char * filebadroc)
{
  //
  // Before reading new data, remove previously stored bad rocs from memory
  //
  ClearBadRocList();
  
  FILE *fin;
  if ((fin = fopen(filebadroc,"r")) == NULL){ 
    cerr << "PadCalibrationObject::FetchBadROCFromFile() Cannot open file.\n";
    return False;
  }
  //
  // Now, read from the file and put the bad channel data into memory.
  // The first line tells us how many bad channels have been found.
  //
  
  int packetid,grow,gcol,roctype;
  int nbadrocs;
  
  fscanf(fin,"%d\n", &nbadrocs);

  for (int i=0; i< nbadrocs; i++) {
    fscanf(fin,"%d %d %d %d\n",&packetid,&grow,&gcol,&roctype); 
    this->AddBadRoc(packetid,grow,gcol,roctype);
  }

  fclose(fin);  

  iFlag=0;
   _get_hot_channels(); 
  return True;
}

//
// Fetch calibration constants from Objectivity (Objy) Database
//
PHBoolean PadCalibrationObject::Fetch() {

  PHBoolean status = FetchBadChObjy();
  if(!status) {
    cerr << "PadCalibrationObject::Fetch() ERROR reading Bad Channels." << endl;
    return False;
  }
  
  status = FetchBadROCObjy();
  if(!status) {
    cerr << "PadCalibrationObject::Fetch() ERROR reading Bad ROCs." << endl;
    return False;
  }
  
  iFlag=0;
  return True;
}



//
// Fetch bad channels from Database
//
PHBoolean PadCalibrationObject::FetchBadChObjy()
{

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (application->startRead())
    {
      // Fetch corresponding bank
      setCalibName("calib.pad.badch");
      setBankNumber(4100);
      setBankID(BankNumber); // yes, slightly unnecessary, but it doesn't hurt to be explicit

      PdbCalBank *padBank = bankManager->fetchBank("PdbPadBadChBank", BankID, "calib.pad.badch", Tsearch);

      //
      // Test access to the header
      //
      if (Debug == 1)
        {
          cout << "Information directly to the header:" << endl;
          cout << "startTime = " << padBank->getStartValTime() << endl;
          cout << "endTime   = " << padBank->getEndValTime() << endl;
          cout << "bankID    = " << padBank->getBankID().getInternalValue() << endl;
          cout << "---------------------------------------------" << endl;
        }

      if (padBank)
        {
          if (Debug == 1)
            {
              padBank->print();
              cout << "Number of stored Bad Channels = " << padBank->getLength() << endl;
              for (unsigned int i = 0; i < padBank->getLength(); i++)
                {
                  padBank->printEntry(i);
                }
            }
          // fill the badch struct
          numberBadCh = padBank->getLength();
          if (numberBadCh > MAXBADCH)
            {
              cerr << "PadCalibrationObject::FetchBadChObjy() Too many bad channels.\n";
              cerr << "Number of bad channels= " << numberBadCh << " Maximum allowed number= " << MAXBADCH << " (this is the number of channels we will store)\n";
              numberBadCh = MAXBADCH;
            }
          PdbPadBadCh *badchobj;
          for (int i = 0; i < numberBadCh; i++)
            {
              badchobj = (PdbPadBadCh*) & (padBank->getEntry(i));
              badch[i].packetid = badchobj->getPacketid();
              badch[i].channelid = badchobj->getChannel();
              badch[i].padtype = badchobj->getPadtype();
            }
        }
      else
        {
	  cout << PHWHERE << "Could not find calibration for time " 
	       << Tsearch << endl;
	  cout << "Provide a calibration for this run" << endl;
	  exit(1);
        }

      application->commit();

      if (padBank)
        {
          delete padBank;
        }

    }
  else
    {
      application->abort();
      cerr << "PadCalibrationObject::FetchBadChObjy() ERROR: Transaction aborted." << endl;
      return False;
    }

  iFlag = 0;
  return True;
}



//
// Fetch bad ROCs from Objy Database
//

PHBoolean PadCalibrationObject::FetchBadROCObjy()
{
  // remove previously stored bad rocs from memory
  ClearBadRocList();
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (application->startRead())
    {

      // Fetch corresponding bank

      setCalibName("calib.pad.badroc");
      setBankNumber(4101);
      setBankID(BankNumber); // yes, slightly unnecessary, but it doesn't hurt to be explicit

      PdbCalBank *padBank = bankManager->fetchBank("PdbPadBadROCBank", BankID, "calib.pad.badroc", Tsearch);

      //
      // Test access to the header
      //
      if (Debug == 2)
        {
          cout << "Information directly to the header:" << endl;
          cout << "startTime = " << padBank->getStartValTime() << endl;
          cout << "endTime   = " << padBank->getEndValTime() << endl;
          cout << "bankID    = " << padBank->getBankID().getInternalValue() << endl;
          cout << "---------------------------------------------" << endl;
        }

      if (padBank)
        {
          if (Debug == 2)
            {
              padBank->print();
              cout << "Number of stored Bad ROCs = " << padBank->getLength() << endl;
              for (unsigned int i = 0; i < padBank->getLength(); i++)
                {
                  padBank->printEntry(i);
                }
            }
          // fill the badroc struct
          //      numberBadROC=padBank->getLength();
          numberBadROC = 0; //this variable is handled in AddBadRoc method
          PdbPadBadROC *badrocobj;
          for (unsigned int i = 0; i < padBank->getLength(); i++)
            {
              badrocobj = (PdbPadBadROC*) & (padBank->getEntry(i));
              this->AddBadRoc(badrocobj->getPacketid(),
                              badrocobj->getGrouprowl(),
                              // oops, need to remove that extra l.. or just say it is l as in local.. (remember that we have grouprow=0..4 not 0..9 for PC2/3..
                              badrocobj->getGroupcolumn(),
                              badrocobj->getROCtype());
            }
        }
      else
        {
	  cout << endl << endl;
          cout << PHWHERE << "Could not find calibration for time "
	       << Tsearch << " in table calibpadbadroc" << endl;
          cout << "Provide a calibration for this run" << endl;
          exit(1);
        }

      application->commit();

      if (padBank)
        {
          delete padBank;
        }

    }
  else
    {
      application->abort();
      cerr << "PadCalibrationObject::FetchBadROCObjy() ERROR: Transaction aborted." << endl;
      return False;
    }

  iFlag = 0;
  _get_hot_channels();
  return True;

}


//-----------------------------------------------------------------------------
//                Methods to WRITE stuff into database
//-----------------------------------------------------------------------------

//
// Put calibration constants to default ASCII files
//
PHBoolean
PadCalibrationObject::PutToFile()
{
  const char* filebadch  = "newbadch.txt";
  const char* filebadroc = "newbadroc.txt";
  return PadCalibrationObject::PutToFile(filebadch,filebadroc);
}

//
// Put calibration constants to ASCII files
//
PHBoolean
PadCalibrationObject::PutToFile(const char* filebadch,
                                const char* filebadroc)
{
  PHBoolean status = PutBadChToFile(filebadch);
  if(!status) {
    cerr << "PadCalibrationObject::PutToFile() ERROR writing Bad Channels."
         << endl;
    return False;
  }

  status = PutBadROCToFile(filebadroc);
  if(!status) {
    cerr << "PadCalibrationObject::PutToFile() ERROR writing Bad ROCs." << endl;
    return False;
  }

  return True;  
}

//
// Put bad channel info to default ASCII file
//
PHBoolean
PadCalibrationObject::PutBadChToFile()
{
  const char* filebadch = "padnewbadch.txt";
  return PadCalibrationObject::PutBadChToFile(filebadch);
}

//
// Put bad channel info to an ASCII file "filebadch"
//
PHBoolean
PadCalibrationObject::PutBadChToFile(const char* filebadch)
{

  FILE *fout;
  if ((fout = fopen(filebadch,"w")) == NULL){ 
    cerr << "PadCalibrationObject::PutBadChToFile() Cannot open file.\n";
    return False;
  }
  //
  // The first line tells us how many bad channels have been found.
  //
  fprintf(fout,"%d\n",numberBadCh); 
  
  if (numberBadCh>MAXBADCH) {
    cerr << "PadCalibrationObject::PutBadChToFile() Too many bad channels.\n";
    cerr << "Number of bad channels= " << numberBadCh
    << " Maximum allowed number= " << MAXBADCH
    << " (this is the number of channels we will store)\n";
    numberBadCh=MAXBADCH;
  }
  for (int i=0; i<numberBadCh; i++) {
    fprintf(
      fout,"%d %d %d\n",badch[i].packetid,badch[i].channelid,badch[i].padtype
    ); 
  }
  
  fclose(fout);
  return True;
}

//
// Put bad ROC info to default ASCII file
//
PHBoolean
PadCalibrationObject::PutBadROCToFile()
{
  const char* filebadroc = "padnewbadroc.txt";
  return PadCalibrationObject::PutBadROCToFile(filebadroc);
}

//
// Put bad ROC info to an ASCII file "filebadroc"
//
PHBoolean
PadCalibrationObject::PutBadROCToFile(const char* filebadroc)
{

  FILE *fout;
  if ((fout = fopen(filebadroc,"w")) == NULL){ 
    cerr << "PadCalibrationObject::PutBadROCToFile() Cannot open file.\n";
    return False;
  }
  
  //
  // The first line tells us how many bad ROCs have been found.
  //
  fprintf(fout,"%d\n",numberBadROC); 

  std::map<int,Roc*>::iterator it;
  for (it = badroc->begin(); it!=badroc->end(); ++it) {
    fprintf(fout,"%d %d %d %d\n",
      it->second->packetid,
      it->second->grow,
      it->second->gcol,
      it->second->roctype
    ); 
  }

  fclose(fout);
  return True;
}


//
// Put calibration constants to Objectivity (Objy) Database
//
PHBoolean
PadCalibrationObject::Put()
{

  PHBoolean status = PutBadChObjy();
  if (!status) {
    cerr << "PadCalibrationObject::Put() ERROR storing Bad Channels." << endl;
    return False;
  }

  status = PutBadROCObjy();
  if (!status) {
    cerr << "PadCalibrationObject::Put() ERROR storing Bad ROCs." << endl;
    return False;
  }
  
  return True;
}

//
// Put bad channels to Objy Database
//
PHBoolean
PadCalibrationObject::PutBadChObjy()
{
  //
  // Open database in UPDATE mode
  //
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (application->startUpdate()) {   
    //
    // Put corresponding bank    
    //
    setCalibName("calib.pad.badch");
    setBankNumber(4100);    
    setBankID(BankNumber); // yes, slightly unnecessary, but it
                           // doesn't hurt to be explicit    
    const char* descrip = "Bad Ch info";

    PdbCalBank *padBank = bankManager->createBank(
                                         "PdbPadBadChBank", BankID, descrip,
                                         TUpdateStart, TUpdateStop,
                                         "calib.pad.badch"
                                       );
    
    if (numberBadCh>MAXBADCH) {
      cerr << "PadCalibrationObject::PutBadChObjy() Too many bad channels.\n";
      cerr << "Number of bad channels= " << numberBadCh
           << " Maximum allowed number= " << MAXBADCH
           << " (this is the number of channels we will store)\n";
      numberBadCh=MAXBADCH;
    }
    padBank->setLength(numberBadCh);
    
    if (Debug==1)  {
      padBank->print();
    }
    
    PdbPadBadCh *badchobj;
    
    for (int i=0; i < numberBadCh; i++) {
      badchobj = (PdbPadBadCh*)&(padBank->getEntry(i));
      badchobj->setParameter(0, badch[i].packetid);
      badchobj->setParameter(1, badch[i].channelid);
      badchobj->setParameter(2, badch[i].padtype);
      if (Debug==1) {
        padBank->printEntry(i);
      }
    }
    
    application->commit();

    if (padBank) {
      delete padBank;
    }
  
  } else {
    application->abort();
    cerr << "PadCalibrationObject::PutBadChObjy() ERROR: Transaction aborted."
         << " Failed to start application." << endl;
    return False;
  }

  return True;
}

//
// Put bad ROCs to Objy Database
//
PHBoolean
PadCalibrationObject::PutBadROCObjy()
{
  //
  // Open database in UPDATE mode.
  //
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (application->startUpdate()) {   
    //
    // Put corresponding bank    
    //
    setCalibName("calib.pad.badroc");
    setBankNumber(4101);    
    setBankID(BankNumber); // yes, slightly unnecessary, but it
                           // doesn't hurt to be explicit    
    const char* descrip = "Bad ROC info";

    PdbCalBank *padBank = bankManager->createBank(
                                         "PdbPadBadROCBank", BankID, descrip,
                                         TUpdateStart, TUpdateStop,
                                         "calib.pad.badroc"
                                       );

    padBank->setLength(numberBadROC);

    if (Debug == 2) {
      padBank->print();
    }
    
    PdbPadBadROC *badrocobj;

    int i = 0;
    std::map<int,Roc*>::iterator it;
    
    for (it=badroc->begin(); it!=badroc->end(); ++it) {
      badrocobj = (PdbPadBadROC*)&(padBank->getEntry(i));
      badrocobj->setParameter(0, it->second->packetid);
      badrocobj->setParameter(1, it->second->grow);
      badrocobj->setParameter(2, it->second->gcol);
      badrocobj->setParameter(3, it->second->roctype);
      if (Debug == 2) {
        padBank->printEntry(i);
      }
      i++;
    }
    application->commit();

    if(padBank) {
      delete padBank;
    }
  
  } else {
    application->abort();
    cerr << "PadCalibrationObject::PutBadROCObjy() ERROR: Transaction aborted."
         << " Failed to start application." << endl;
    return False;
  }

  return True; 
}


// Compare badroc list with list in other padcalib-object
// Returns number of differing rocs

int
PadCalibrationObject::CompareBadRocList(PadCalibrationObject* that)
{
  int diff=0;

  std::map<int,Roc*>::iterator thisiter;
  std::map<int,Roc*>::iterator thatiter;

  thisiter = badroc->begin();
  thatiter = that->badroc->begin();

  while (thisiter != badroc->end() && thatiter != that->badroc->end()) {

    if (thisiter->first != thatiter->first
          || thisiter->second->roctype != thatiter->second->roctype) {
      diff++;
    }
    if (thisiter->first < thatiter->first) {
      thisiter++;
    
    } else if (thisiter->first > thatiter->first) {
      thatiter++;
    
    } else {
      thisiter++;
      thatiter++;
    }
  }
  return diff;
}


//  Print the list of bad rocs.

void PadCalibrationObject::PrintBadRocList(PadCalibrationObject* that)
{
  std::map<int,Roc*>::iterator it;
  for (it = badroc->begin(); it != badroc->end(); ++it) {
    cout << "pid from method =  " << it->first << endl; 
  }
}


// This method reads data entries from the file named filename. This file
// should obey the following syntax:
// <nentries>
// <packet_id> <grow> <gcolumn> <roctype>
//     .
//     .
//
// For each packet id, the total number of 'hot rocs' in its roctype field
// is stored in the array this->_pid_to_hits
// as this->_pid_to_hits[packet_id].
//


void
PadCalibrationObject::_get_hot_channels()
{

  int size_array = LARGEST_PACKET_ID - SMALLEST_PACKET_ID + 1;
#if USE_MEMSET
  //
  // Use memset to initialize the array. This may break on some (exotic!?)
  // architectures hence the USE_MEMSET symbol...
  //
  memset(this->_pid_to_hits, 0x00, size_array * sizeof(int));
#else
  for (int i = 0; i < size_array; i++) {
    this->_pid_to_hits[i] = 0;
  }
#endif

  //
  // Now, just fill the array with the values from the map badroc
  //
  std::map<int,Roc*>::iterator it;

  for (it = badroc->begin(); it != badroc->end(); ++it) {
    Roc* roc     = it->second;
    int  roctype = roc->roctype;
    //
    // Compute the number of excited channels, i.e. the number of "twos"
    // in the decimal representation of roctype...
    //
    int nhot = 0;
    while (roctype != 0) {
     if ((roctype % 10) == 2) {
       nhot++;
     }
     roctype /= 10;
    }
    _pid_to_hits[roc->packetid - SMALLEST_PACKET_ID] += nhot;
  }
  return;
}

// Given the packet with ID pid, returns the total number of hot channels

int
PadCalibrationObject::GetNumberofPadHits(int packetid)
{
  return this->_pid_to_hits[packetid - SMALLEST_PACKET_ID];
}

// Compare hot roc list with list in other padcalib-object

int
PadCalibrationObject::CompareHotRocList(PadCalibrationObject* that)
{
  int diff=0;

  std::map<int,Roc*>::iterator thisiter;
  std::map<int,Roc*>::iterator thatiter;

  map<int,int> thishotmap, thathotmap;
  
  // first fill the hot rocs from each calib object into
  // its own map
  
  for (thisiter = badroc->begin(); thisiter != badroc->end(); thisiter++) {
    int roc = thisiter->second->roctype;
    int hot = 0;
    if (roc / 100 == 2) {
      hot += 200;
    }
    roc = roc % 100;
    if (roc / 10 == 2) {
      hot += 20;
    }
    roc = roc % 10;
    if (roc == 2) {
      hot += 2;
    }
    if (hot) {
      thishotmap[thisiter->first] = hot;
    }
  }

  for (thatiter = that->badroc->begin(); thatiter != that->badroc->end();
       thatiter++) {
    int roc = thatiter->second->roctype;
    int hot = 0;
    if (roc / 100 == 2) {
      hot += 200;
    }
    roc = roc % 100;
    if (roc / 10 == 2) {
      hot += 20;
    }
    roc = roc % 10;
    if (roc == 2) {
      hot += 2;
    }
    if (hot) {
      thathotmap[thatiter->first] = hot;
    }
  }

  map<int,int>::const_iterator iterthis;
  map<int,int>::const_iterator iterthat;

  iterthis = thishotmap.begin();
  iterthat = thathotmap.begin();

  //
  // Now loop over both maps and find the differences just like above...
  //
  while (iterthis != thishotmap.end() && iterthat != thathotmap.end()) {

    if (iterthis->first!=iterthat->first
          || iterthis->second != iterthat->second ){
      diff++;
    }
    if (iterthis->first < iterthat->first) {
      iterthis++;
    
    } else if (iterthis->first > iterthat->first) {
      iterthat++;
    
    } else {
      iterthis++;
      iterthat++;
    }
  }
  return diff;
}

// Merge badroc list with list of rocs with known (and correct!) values.

void
PadCalibrationObject::MergeBadRocList(PadCalibrationObject* that)
{
  std::map<int, Roc*>::iterator it;
  for (it = that->badroc->begin(); it != that->badroc->end(); ++it) {
    this->AddBadRoc(
      it->second->packetid,
      it->second->grow,
      it->second->gcol,
      it->second->roctype
    );
  }
}

// Add a bad roc to the list of bad rocs.

int
PadCalibrationObject::AddBadRoc(int packet, int row, int col, int type)
{
  iFlag = 0;
  
  int key = (packet - 4001) * 45 + row * 9 + col;
  
  if (badroc->find(key) != badroc->end()) {
    this->RemoveBadRoc(packet, row, col);
  }
  Roc *tmp = new Roc(packet, row, col, type);

  badroc->insert(std::make_pair(key, tmp));

  numberBadROC++;

  return 1;
}


// Add a bad roc to the list of bad rocs

int
PadCalibrationObject::RemoveBadRoc(int packet, int row, int col)
{
  int key = (packet - 4001) * 45 + row * 9 + col;
  
  if (badroc->find(key) != badroc->end()) {
    Roc *tmp = badroc->find(key)->second;
    badroc->erase(key);
    delete tmp;
    numberBadROC--;
    return 1;
  
  } else {
    return 0;
  }
}


// Clear the list of bad rocs.

void
PadCalibrationObject::ClearBadRocList()
{
  std::map<int,Roc*>::iterator it;
  
  for (it = badroc->begin(); it != badroc->end(); ++it) {
    badroc->erase(it->first);
    delete it->second;
    numberBadROC--;
  }
}

//-----------------------------------------------------------------------------
//              Accessors to get information about bad channels
//-----------------------------------------------------------------------------


// Returns the number of bad channels.

int PadCalibrationObject::getNumberBadCh()
{
  
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getNumberBadCh: PCO not initialized."
         << endl;
    return -1;
  }
  return numberBadCh;
} 

//
// Returns packet id of bad channel ibadch
//
int
PadCalibrationObject::getPacketidBadCh(int ibadch)
{
  
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getPacketidBadCh: PCO not initialized." 
         << endl;
    return -1;
  }
  int tmppacketid = -1;
  
  if ( (ibadch >= 0) && (ibadch < numberBadCh) ) {
    tmppacketid=badch[ibadch].packetid;
  }
  return tmppacketid;
} 

//
// Returns channelid of bad channel ibadch
//
int
PadCalibrationObject::getChannelidBadCh(int ibadch)
{
  if( iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getChannelidBadCh: PCO not initialized." 
         << endl;
    return -1;
  }
  
  int tmpchannelid=-1;
  if ( (ibadch >= 0) && (ibadch < numberBadCh) ) {
    tmpchannelid=badch[ibadch].channelid;
  }
  return tmpchannelid;
  
} 

//
// Returns padtype of bad channel ibadch
//
int
PadCalibrationObject::getPadtypeBadCh(int ibadch)
{
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getPadtypeBadCh: PCO not initialized." 
         << endl;
    return -1;
  }
  
  int tmppadtype = -1;
  if ( (ibadch >= 0) && (ibadch < numberBadCh) ) {
    tmppadtype=badch[ibadch].padtype;
  }
  return tmppadtype;
  
} 

//
// Returns detector number of where the bad channel is placed
//
int PadCalibrationObject::getDetBadCh(int ibadch)
{
  
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getDetBadCh: PCO not initialized."
         << endl;
    return -1;
  }
  
  int tmpdet = -1;
  if ( (ibadch >= 0) && (ibadch < numberBadCh) ) {
    tmpdet = addressObj->getDet(badch[ibadch].packetid);
  }
  return tmpdet;  
} 

//
// Returns arm number of where the bad channel is placed.
//
int PadCalibrationObject::getArmBadCh(int ibadch)
{  
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getArmBadCh: PCO not initialized."
         << endl;
    return -1;
  }
  
  int tmparm = -1;
  
  if ( (ibadch >= 0) && (ibadch < numberBadCh) ) {
    tmparm = addressObj->getArm(badch[ibadch].packetid);
  }
  return tmparm;
  
}

//
// Returns side number of where the bad channel is placed..
// 
int
PadCalibrationObject::getSideBadCh(int ibadch)
{
  
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getSideBadCh: PCO not initialized."
         << endl;
    return -1;
  }
  int tmpside = -1;
  if ( (ibadch >= 0) && (ibadch < numberBadCh) ) {
    tmpside = addressObj->getSide(badch[ibadch].packetid);
  }
  return tmpside;
  
} 

//
// Returns sector number of where the bad channel is placed.
//
int
PadCalibrationObject::getSectorBadCh(int ibadch)
{
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getSectorBadCh: PCO not initialized."
         << endl;
    return -1;
  }
  
  int tmpsector = -1;
  
  if ( (ibadch >= 0) && (ibadch < numberBadCh) ) {
    tmpsector = addressObj->getSector(badch[ibadch].packetid);
  }
  return tmpsector;
  
} 

//**********************************************************************
int PadCalibrationObject::getPadzBadCh(int ibadch)
{
  // Get padz number of where the bad channel is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getPadzBadCh: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmppadz=-1;
    if ( (ibadch>=0) && (ibadch<numberBadCh) ) 
      tmppadz = addressObj->getPadz(badch[ibadch].channelid);
    return tmppadz;
  }
} 
/* end getPadzBadCh() */
//**********************************************************************
int PadCalibrationObject::getPadxBadCh(int ibadch)
{
  // Get padx number of where the bad channel is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getPadxBadCh: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmppadx=-1;
    if ( (ibadch>=0) && (ibadch<numberBadCh) ) 
      tmppadx = addressObj->getPadx(badch[ibadch].packetid,badch[ibadch].channelid);
    return tmppadx;
  }
} 
/* end getPadxBadCh() */
//**********************************************************************
int PadCalibrationObject::getNumberBadROC()
{
  // Get number of Bad ROCs
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getNumberBadROC: PCO not initialized." << endl;
    return -1;
  }
  else
    return numberBadROC;
} 
/* end getNumberBadROC() */
//**********************************************************************
int PadCalibrationObject::getPacketidBadROC(int ibadroc)
{
  // Get packetid number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getPacketidBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmppacketid=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      tmppacketid = it->second->packetid;
    }
    return tmppacketid;
  }
} 
/* end getPacketidBadROC() */
//**********************************************************************
int PadCalibrationObject::getGrowBadROC(int ibadroc)
{
  // Get grow number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getGrowBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmpgrow=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      tmpgrow = it->second->grow;
    }
    return tmpgrow;
  }
} 
/* end getGrowBadROC() */
//**********************************************************************
int PadCalibrationObject::getGcolBadROC(int ibadroc)
{
  // Get gcol number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getGcolBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmpgcol=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      tmpgcol = it->second->gcol;
    }
    return tmpgcol;
  }
} 
/* end getGcolBadROC() */
//**********************************************************************
int PadCalibrationObject::getROCtypeBadROC(int ibadroc)
{
  // Get roctype number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getROCtypeBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmproctype=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      tmproctype = it->second->roctype;
    }
    return tmproctype;
  }
} 
/* end getROCtypeBadROC() */
//**********************************************************************
int PadCalibrationObject::getDetBadROC(int ibadroc)
{
  // Get det number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getDetBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmpdet=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      tmpdet = addressObj->getDet(it->second->packetid);
    }
    return tmpdet;
  }
} 
/* end getDetBadROC() */
//**********************************************************************
int PadCalibrationObject::getArmBadROC(int ibadroc)
{
  // Get arm number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getArmBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmparm=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      tmparm = addressObj->getArm(it->second->packetid);
    }
    return tmparm;
  }
} 
/* end getArmBadROC() */
//**********************************************************************
int PadCalibrationObject::getSideBadROC(int ibadroc)
{
  // Get side number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getSideBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmpside=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      tmpside = addressObj->getSide(it->second->packetid);
    }
    return tmpside;
  }
} 
/* end getSideBadROC() */
//**********************************************************************
int PadCalibrationObject::getSectorBadROC(int ibadroc)
{
  // Get sector number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getSectorBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmpsector=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      tmpsector = addressObj->getSector(it->second->packetid);
    }
    return tmpsector;
  }
} 
/* end getSectorBadROC() */
//**********************************************************************
int PadCalibrationObject::getMaxPadzBadROC(int ibadroc)
{
  // Get maxpadz number of where the bad ROC is placed..
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR getMaxPadzBadroc: PCO not initialized." << endl;
    return -1;
  }
  else {
    int tmpmaxpadz=-1;
    if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
      std::map<int,Roc*>::iterator it = badroc->begin();
      for (int i=0;i<ibadroc;i++) {it++;}
      // find out which tgls are bad..
      int badtgl[3],roctype;
      for (int tgl=0; tgl<3; tgl++) badtgl[tgl]=0;
      if (it->second->roctype==-1) {
        for (int tgl=0; tgl<3; tgl++) badtgl[tgl]=-1;
      }
      else {
        roctype=it->second->roctype;
        if ( (roctype >= 0) && (roctype <= 222) ) { // allowed error index range 
    // this decoding just reflects how the encoding to fill the database is done..
          badtgl[0]=roctype/100;
        badtgl[1]=(roctype%100)/10;
        badtgl[2]=roctype%10;
      }
    }
      // The maximum padz coordinate is found at the highest (4) channelvalue(s)
      // of the highest bad tglvalue
    for (int tgl=0; tgl<3; tgl++) {
      if (badtgl[tgl]!=0) {
        tmpmaxpadz=(tgl*4)+3; // tgl1: padcolumns 0-3 tgl2: 4-7 tgl3: 8-11
      }
    }
    tmpmaxpadz+=it->second->gcol*12; // add 12 columns for each groupcolumn
  }
  return tmpmaxpadz;
}
} 
/* end getMaxPadzBadROC() */
//
// Returns minpadz number of where the bad ROC is placed.
//
int
PadCalibrationObject::getMinPadzBadROC(int ibadroc)
{
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getMinPadzBadroc: PCO not initialized." 
         << endl;
    return -1;
  }
  
  int tmpminpadz = -1;
  
  if ( (ibadroc>=0) && (ibadroc<numberBadROC) ) {
    std::map<int,Roc*>::iterator it = badroc->begin();
  
    for (int i = 0; i < ibadroc; i++) {
      it++;
    }
    
    int badtgl[3],roctype;
    for (int tgl = 0; tgl < 3; tgl++) {
      badtgl[tgl]=0;
    }
    if (it->second->roctype == -1) {
      for (int tgl = 0; tgl < 3; tgl++) {
        badtgl[tgl]=-1;
      }
    } else {
      roctype=it->second->roctype;
      if ((roctype >= 0) && (roctype <= 222)) { // allowed error index range 
        //
        // This decoding just reflects how the encoding to fill the
        //database is done..
        //
        badtgl[0] = roctype / 100;
        badtgl[1] = (roctype % 100) / 10;
        badtgl[2] = roctype % 10;
      }
    }
    //
    // The minimum padz coordinate is found at the lowest (4) channelvalue(s)
    // of the lowest bad tglvalue
    //
    for (int tgl = 2; tgl >= 0; tgl--) {
      if (badtgl[tgl] != 0) {
        tmpminpadz = (tgl * 4); // tgl1: padcolumns 0-3 tgl2: 4-7 tgl3: 8-11
      }
    }
    tmpminpadz += it->second->gcol * 12; // add 12 columns for each groupcolumn
  }
  return tmpminpadz;
} 

//
// Returns maxpadx number of where the bad ROC is placed.
//
int
PadCalibrationObject::getMaxPadxBadROC(int ibadroc)
{
  
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getMaxPadxBadroc: PCO not initialized." 
         << endl;
    return -1;
  }
  
  int tmpmaxpadx = -1;
  
  if ( (ibadroc >= 0) && (ibadroc < numberBadROC)) {
    std::map<int,Roc*>::iterator it = badroc->begin();
  
    for (int i = 0; i < ibadroc; i++) {
      it++;
    }
    //
    // We don't need to find out which tgls are bad.
    // At least one is bad and they share the same padx interval anyway.
    // The maximum padx coordinate is found at the highest (4) channelvalue(s)
    //
    // Per tgl: padrows 0-3, 3 is the highest 
    //
    
    tmpmaxpadx = 3 + it->second->grow * 4; // add 4 rows for each grouprow
    
    //
    // Add 20 if necessary.. use addressObj and look at channel 0 in our packet
    // to figure out if we should add an offset.
    //
    int offset = addressObj->getPadx(it->second->packetid,0);
    //
    // The offset is either 0 or 20 depending on which FEM the packet is coming 
    // from.
    //
    if ((offset == 0) || (offset == 20)) {
      tmpmaxpadx += offset;
    }
  }
  return tmpmaxpadx;  
} 

//
// Returns minpadx number of where the bad ROC is placed.
//
int
PadCalibrationObject::getMinPadxBadROC(int ibadroc)
{
  
  if (iFlag != 0) {
    cerr << "PadCalibrationObject ERROR getMinPadxBadroc: PCO not initialized." 
         << endl;
    return -1;
  }

  int tmpminpadx = -1;
  
  if ((ibadroc >= 0) && (ibadroc < numberBadROC)) {
    std::map<int,Roc*>::iterator it = badroc->begin();
    
    for (int i = 0; i < ibadroc; i++) {
      it++;
    }
    //
    // We don't need to find out which tgls are bad.
    // At least one is bad and they share the same padx interval anyway.
    // The minimum padx coordinate is found at the lowest (4) channelvalue(s)
    //
    
    tmpminpadx = it->second->grow * 4; // add 4 rows for each grouprow
    
    //
    // Add 20 if necessary. Use addressObj and look at channel 0 in our packet
    // to figure out if we should add an offset.
    //
    int offset = addressObj->getPadx(it->second->packetid, 0);
    //
    // The offset is either 0 or 20 depending on which FEM the packet
    // is coming from.
    //
    if ((offset == 0) || (offset == 20)) {
      tmpminpadx += offset;
    }
  }
  return tmpminpadx;
} 

//----------------------------------------------------------------------------
//                      Debug / print utilities.
//----------------------------------------------------------------------------

//
// Prints information about this PadCalibrationObject.
//
void PadCalibrationObject::print()
{
  // Print the parameter information
  if(iFlag!=0) {
    cerr << "PadCalibrationObject ERROR print: PCO not initialized." << endl;
  }
  cout << "PadCalibrationObject Parameter information" << endl;
  cout << "numberBadCh= " << numberBadCh << endl; 
  if (Debug==1) {
    for (int i=0; i<numberBadCh; i++) {
      cout << "bad channel= " << i << endl;
      cout << "packetid= " << badch[i].packetid << " channelid " << badch[i].channelid << endl;
      addressObj->setHard(badch[i].packetid,badch[i].channelid);
      addressObj->print();
    }
  }
  cout << "numberBadROC= " << numberBadROC << endl; 
  if (Debug==2) {
    int badtgl[3],roctype,chid,startchid;
    cout << "packetid grouprow groupcolumn roctype" << endl; 
    int i=0;
    for (std::map<int,Roc*>::iterator it = badroc->begin(); it!=badroc->end(); ++it) {
      cout << i << " " << it->second->packetid << " " << it->second->grow << " " << it->second->gcol << " " << it->second->roctype << endl;
      for (int tgl=0; tgl<3; tgl++) badtgl[tgl]=0;
      if (it->second->roctype==-1) {
	cout << "ROC was out of synch with the others" << endl; 
	for (int tgl=0; tgl<3; tgl++) badtgl[tgl]=-1;
      }
      else {
	roctype=it->second->roctype;
	if ( (roctype >= 0) && (roctype<222) ) { // allowed error index range 
	  badtgl[0]=roctype/100;
	  badtgl[1]=(roctype%100)/10;
	  badtgl[2]=roctype%10;
	}
      }
      for (int tgl=1; tgl<=3; tgl++) {
	if (badtgl[tgl-1]!=0) {
	  cout << " tgl " << tgl << " was bad. That corresponds to:" << endl;
	  startchid=it->second->gcol*240+((tgl-1)*4)*20+it->second->grow*4;
	  cout << " startchid= " << startchid << endl;

	  for (int channel=1; channel<=16; channel++) {
	    chid= startchid + ((channel-1)/4)*20 + (channel-1)%4;
	    cout << " channel, chid= " << channel << ", " << chid << endl;
	    addressObj->setHard(it->second->packetid,chid);
	    addressObj->print();
	  }
	}
      }
      i++;
    }
  }
  
  cout << endl;
  cout << "Debug= " << Debug;
  cout << " (should be 1 or 2 when debugging Info for Bad Channel and Bad ROCs respectively) " << endl;
  
} 
/* end print() */
