// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 01/24/00
// Description: Implementation of TecHVObject class

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <TecHVObject.hh> 
#include <iostream>
#include <fstream>

using namespace std;

/// Constructor
TecHVObject::TecHVObject()
{

  Debug=0;
  iFlag=1;
  CalibName = "hv.tec.run00";
  setBankNumber(TECHVBANK);
  setBankID(BankNumber);
  setDescription("TEC High Voltage Values");
  Tsearch.setToSystemTime();

  int i,j;
  for (i = 0; i < TECMAXINDEX; i++)
    {
      for (j = 0; j < TECMAXHVTYP; j++)
	{
	  HighVolt[i][j] = 0.0;
      }
  }
}

/// Destructor
TecHVObject::~TecHVObject() { }

///
void TecHVObject::UseSimulationDatabase() {
  CalibName = "hv.tec.geant00";
}
///
void TecHVObject::UseRealDatabase() {
  CalibName = "hv.tec.run00";
}

/// Get high voltage value
float TecHVObject::getHVVal(TecAddressObject* TAO, int j)
{
  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int index = iside + iplane*TECMAXSIDE + isector*TECMAXSIDE*TECMAXPLANE;

  if(iFlag!=0) {
    cerr << "TecHVObject ERROR:THVO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    return 0.0;
  }
  return HighVolt[index][j];
}

/// Set high voltage value
void TecHVObject::setHVVal(TecAddressObject* TAO, int j, float hvval)
{
  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int index = iside + iplane*TECMAXSIDE + isector*TECMAXSIDE*TECMAXPLANE;

  HighVolt[index][j] = hvval;
}

/// Get high voltage value
float TecHVObject::getHVVal(int index, int j) {
return HighVolt[index][j];
}

/// Set high voltage value
void TecHVObject::setHVVal(int index, int j, float hvval) {
  if(index>=0 && index<TECMAXINDEX && j<TECMAXHVTYP) {
    HighVolt[index][j] = hvval;
  }
  else {
    cerr << "TecHVObject::setHVVal ERROR: wrong index = " << index << endl
         << "     or wrong hv type = " << j << endl;
  }
}

/// Fetch high voltage values from an ASCII file "filename"
PHBoolean TecHVObject::FetchHVValFromFile(const char* filename) {

const char* tecdb = filename;
ifstream file;
file.open(tecdb);

int totnentgeo = TECMAXSECT*TECMAXPLANE*TECMAXSIDE;
int totnent = totnentgeo*TECMAXHVTYP;
float buff[totnent],tmp;
int i,index;

// Read hv values from ASCII file

if(!file) {
  cerr << "TecHVObject::FetchHVValFromFile ERROR: "
       << "Can not open " << tecdb << endl;
  return False;
}
else {
  int ibuf=0;
  for(i=0; i<=totnent; i++) {
    file >> tmp;

    if (file.eof()) break;

      buff[ibuf] = tmp;
      ibuf++;
  } 

  if(Debug>0) cout << "TecHVObject::FetchHVValFromFile: "
       << "TEC high voltage values read from " << tecdb << endl
       << "Total " << i << " " << ibuf << " rows." << endl;
}

// Fill hv values array

  int firstplane=0; int lastplane=TECMAXPLANE-1;
  int isect,iplane,izsign;
  int j;

  int count=0;

  for(isect=0; isect<TECMAXSECT; isect++) {
    for (iplane=firstplane; iplane<lastplane+1; iplane++) {
      for (izsign=0; izsign<TECMAXSIDE; izsign++) {

        index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+izsign;

	for (j=0; j<TECMAXHVTYP;j++){
          HighVolt[index][j]=buff[count+j*totnentgeo];
	}
          count++;

      }
    }
  }

  iFlag=0;
  return True;
}

/// Fetch high voltage values from default ASCII file
PHBoolean TecHVObject::FetchFromFile() {

  int j,index,isect,iplane,izsign;
  int firstplane,lastplane;
  
  firstplane=0; lastplane=TECMAXPLANE-1;

  for (j=0; j<TECMAXHVTYP; j++)
  {
    for (isect=0; isect<TECMAXSECT; isect++) /* 0 = bottom, 3 = top */
    {
      for (iplane=firstplane; iplane<lastplane+1; iplane++)
      {
        for (izsign=0; izsign<TECMAXSIDE; izsign++) /* 0=north, 1=south */
        {

          index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+izsign;

          HighVolt[index][j]=0.0;

        } // izsign 
      } // iplane 
    } // isect 
  } // hvtyp

  iFlag=0;
  return True;

}

/// Fetch high voltage values from a Database
PHBoolean TecHVObject::Fetch() {

  setDescription("TEC High Voltage Values");
  PHBoolean status = FetchHVVal();
  if(!status) {
    cerr << "TecHVObject::Fetch() ERROR reading High Voltage Values." << endl;
    return False;
  }

  iFlag=0;
  return True;

}

/// Fetch high voltage constants from a Database
PHBoolean TecHVObject::Fetch(int runnumber) {

  setDescription("TEC High Voltage Values");
  PHBoolean status = FetchHVVal(runnumber);
  if(!status) {
    cerr << "TecHVObject::Fetch() ERROR reading High Voltage Values." << endl;
    return False;
  }

  iFlag=0;
  return True;

}

/// Fetch High Voltage Values from a Database
PHBoolean TecHVObject::FetchHVVal() {

PdbADCChan* achan=0;
int bankid=0;
float G1,G2;

// Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

// Loop over HV types
for(int j=0; j<TECMAXHVTYP; j++){
 
  if (application->startRead()) {

// Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID;
    const char* calibname = CalibName;
    bankid = TECHVBANK+j;
    bankID.setInternalValue(bankid);
   
    if(Debug>0) {
      cout << "TecHVObject::FetchHVVal: calibname = " << calibname << endl;
      cout << "TecHVObject::FetchHVVal: bankid = " << bankid << endl;
      cout << "TecHVObject::FetchHVVal: Search Time = "; tSearch.print(); cout << endl;
    }

      tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, tSearch);

      if (tecBank) {
         if(Debug>1) tecBank->print();
         if(Debug>1) cout << "Number of Channels = " 
                          << tecBank->getLength() << endl;
         for(int i=0; i < (int)tecBank->getLength(); i++) {
           achan = (PdbADCChan*)&(tecBank->getEntry(i));
           G1 = achan->getParameter(0);
           G2 = achan->getParameter(1);

	   HighVolt[i][j] = G1;

           if(Debug>2) cout << "  Gains: " << G1 << " " << G2 << endl;
         }
      }
      else {
        cerr << "TecHVObject::FetchHVVal() ERROR: bankManager returned zero-pointer." << endl;
        return False;
      }

      application->commit();
  }
  else {
    application->abort();
    cerr << "TecHVObject::FetchHVVal() ERROR: Transaction aborted." << endl;
    return False;
  }

} // end loop over hv types

  iFlag=0;
  if(tecBank) delete tecBank;
  return True;
}

/// Fetch High Voltage Values from a Database
PHBoolean TecHVObject::FetchHVVal(int runnumber) {

int bankid=0;
float G1,G2;

// Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  PdbADCChan* achan=0;

// Loop over hv types
for(int j=0; j<TECMAXHVTYP; j++) {

  if (application->startRead()) {

// Fetch corresponding bank
    PdbBankID bankID;
    const char* calibname = CalibName;
    bankid = TECHVBANK+j;
    bankID.setInternalValue(bankid);

    if(Debug>0) {
      cout << "TecHVObject::FetchHVVal: calibname = " << calibname << endl;
      cout << "TecHVObject::FetchHVVal: bankid = " << bankid << endl;
      cout << "TecHVObject::FetchHVVal: Search run # " << runnumber << endl;
    }

      tecBank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, runnumber);

      if (tecBank) {
         if(Debug>1) tecBank->print();
         if(Debug>1) cout << "Number of Channels = " 
                          << tecBank->getLength() << endl;
         for(int i=0; i < (int)tecBank->getLength(); i++) {
           achan = (PdbADCChan*)&(tecBank->getEntry(i));
           G1 = achan->getParameter(0);
           G2 = achan->getParameter(1);

           HighVolt[i][j] = G1;

           if(Debug>2) cout << "  Gains: " << G1 << " " << G2 << endl;
         }
      }
      else {
        cerr << "TecHVObject::FetchHVVal() ERROR: bankManager returned zero-pointer." << endl;
        return False;
      }

      application->commit();
  }
  else {
    application->abort();
    cerr << "TecHVObject::FetchHVVal() ERROR: Transaction aborted." << endl;
    return False;
  }

} // end of loop over hv types

  iFlag=0;
  if(tecBank) delete tecBank;
  return True;
}

PHBoolean TecHVObject::UpdateHVVal(int RunNumber) {

  int totnent = TECMAXSECT*TECMAXPLANE*TECMAXSIDE;
  PdbADCChan *achan=0;

   PdbBankManager *bankManager = PdbBankManager::instance();
   PdbApplication *application = bankManager->getApplication();
   PdbCalBank *tecBank = 0;

   if(Debug>0) cout << "TecHVObject::UpdateHVVal: opening FD in update mode..." << endl;
   if (application->startUpdate()) {

      PdbBankID bankID;
      const char *descrip   = Description;
      const char *calibname = CalibName;

      for(int j=0; j<TECMAXHVTYP; j++){

	int bankid = TECHVBANK+j;
	bankID.setInternalValue(bankid);

	if(Debug>0) {
        cout << "TecHVObject::UpdateHVVal: calibname = " << calibname << endl;
	cout << "TecHVObject::UpdateHVVal: bankid = " << bankid << endl;
	cout << "Validity: Run #  " << RunNumber << endl;
	}

	tecBank = bankManager->createBank(RunNumber, 
					  "PdbADCChanBank",
					  bankID,
					  descrip,
					  calibname);
	tecBank->setLength(totnent);
	if(Debug>1) tecBank->print();

              for(int i=0; i<(int)tecBank->getLength(); i++) {
                float myGain = HighVolt[i][j];
                achan = (PdbADCChan*)&(tecBank->getEntry(i));
                achan->setParameter(0, myGain);
                achan->setParameter(1, 0.0);
                achan->setParameter(2, 0.0);
                achan->setParError(0, 0.0);
                achan->setParError(1, 0.0);
                achan->setParError(2, 0.0);
              } // end i loop over wires

      } // end j loop over hv types

      application->commit();
   }
   else {
     cerr << "TecHVObject::UpdateHVVal ERROR: failed to start application for update" << endl;
     return False;
   }

   if(tecBank) delete tecBank;
  return True;
}

PHBoolean TecHVObject::UpdateHVVal(PHTimeStamp* Tbeg, PHTimeStamp* Tend) {

   int totnent = TECMAXSECT*TECMAXPLANE*TECMAXSIDE;
   PdbADCChan *achan=0;

   PdbBankManager *bankManager = PdbBankManager::instance();
   PdbApplication *application = bankManager->getApplication();
   PdbCalBank *tecBank = 0;

   if(Debug>0) cout << "TecHVObject::UpdateHVVal: opening FD in update mode..." << endl;
   if (application->startUpdate()) {
      PHTimeStamp tStart = *Tbeg;
      PHTimeStamp tStop  = *Tend;
      PdbBankID bankID;
      const char *descrip   = Description;
      const char *calibname = CalibName;

      for(int j=0; j<TECMAXHVTYP; j++){

        int bankid = TECHVBANK+j;

        if(Debug>0) {
        cout << "TecHVObject::UpdateHVVal: calibname = " << calibname << endl;
        cout << "TecHVObject::UpdateHVVal: bankid = " << bankid << endl;
        cout << "Start validity = "; tStart.print(); cout << endl;
        cout << "End validity = "; tStop.print(); cout << endl;
	}

	bankID.setInternalValue(bankid);

        tecBank = bankManager->createBank("PdbADCChanBank", 
                                          bankID, 
                                          descrip, 
                                          tStart, tStop, 
                                          calibname);
        tecBank->setLength(totnent);
        if(Debug>1) tecBank->print();

              for(unsigned int i=0; i < tecBank->getLength(); i++) {
                float myGain = HighVolt[i][j];
                achan = (PdbADCChan*)&(tecBank->getEntry(i));
                achan->setParameter(0, myGain);
                achan->setParameter(1, 0.0);
                achan->setParameter(2, 0.0);
                achan->setParError(0, 0.0);
                achan->setParError(1, 0.0);
                achan->setParError(2, 0.0);
              } // end i loop over wires

      } // end j loop over hv types

      application->commit();
   }
   else {
     cerr << "TecHVObject::UpdateHVVal ERROR: failed to start application for update" << endl;
     return False;
   }

   if(tecBank) delete tecBank;
  return True;
}

