//--------------------------------------------------------------- 
//                                                                
// Created by: David Silvermyr
//                                                                
// Description: Implementation of PadAncObjy class
//                                                                
//----------------------------------------------------------------
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbPadHV.hh>
#include <PdbPadROCCal.hh>
#include <PdbPadROCPos.hh>

#include <PadAncObjy.hh> 

#include <sstream>
using namespace std;

// Constructor
PadAncObjy::PadAncObjy()
{  
  // initialize and create an addressobject
  Debug=0; // no Debug is default..
  Tsearch.setToSystemTime();
  iFlag=-1;

  HVFlag=-1;
  ROCCalFlag=-1;
  ROCPosFlag=-1;

  for (int i=0; i<MAXPCPLANES; i++) 
    PlaneInfoAvailable[i] = False; // ain't seen nothin' yet
  numberHVPlanes=0; // number of planes with HV active

  numberROCCal=0;
  numberROCPos=0;

  addressObj = new PadAddressObject;

  TUpdateStart.setToSystemTime();
  TUpdateStop.setToFarFuture();
}

// Destructor
PadAncObjy::~PadAncObjy() 
{ 
  delete addressObj;
}

// member functions
//**********************************************************************
// ********* Routines to fetch stuff from files *************
//**********************************************************************
// Fetch calibration constants from default ASCII files
PHBoolean PadAncObjy::FetchFromFile() {
  const char* filehv="padhv.txt";
  const char* fileroccal="padroccal.txt";
  const char* filerocpos="padrocpos.txt";
  return PadAncObjy::FetchFromFile(filehv,fileroccal,filerocpos);
}
//**********************************************************************
// Fetch calibration constants from ASCII files
PHBoolean PadAncObjy::FetchFromFile(const char* filehv, const char* fileroccal, const char* filerocpos) {
  
  PHBoolean status = FetchHVFromFile(filehv);
  if(!status) {
    cerr << "PadAncObjy::FetchFromFile() ERROR reading HV." << endl;
    return False;
  }
  
  status = FetchROCCalFromFile(fileroccal);
  if(!status) {
    cerr << "PadAncObjy::FetchFromFile() ERROR reading ROC Cal." << endl;
    return False;
  }

  status = FetchROCPosFromFile(filerocpos);
  if(!status) {
    cerr << "PadAncObjy::FetchFromFile() ERROR reading ROC Pos." << endl;
    return False;
  }

  iFlag=0;
  return True;  
}
//**********************************************************************
// Fetch hv info from default ASCII file
PHBoolean PadAncObjy::FetchHVFromFile() {
  const char* filehv="padhv.txt";
  return PadAncObjy::FetchHVFromFile(filehv);
}
//**********************************************************************
// Fetch hv info from an ASCII file "filehv"
PHBoolean PadAncObjy::FetchHVFromFile(const char* filehv) {
  
  // Open the file
  FILE *fin;
  if((fin = fopen(filehv,"r"))==NULL){ 
    cerr << "PadAncObjy::FetchHVFromFile() Cannot open file.\n";
    return False;
  }
  
  if (Debug>0) cout << "PadAncObjy::FetchHVFromFile() Opened file \n";
  // first line tells us how many planes are active
  fscanf(fin,"%d\n",&numberHVPlanes); 
  if (numberHVPlanes>MAXPCPLANES) {
    cerr << "PadAncObjy::FetchHVFromFile() Too many planes\n";
    cerr << "Number of planes= " << numberHVPlanes << " Maximum allowed number= " << MAXPCPLANES << " (this is the number we will store)\n";
    numberHVPlanes=MAXPCPLANES;
  }
  if (Debug>0) cout << "PadAncObjy::FetchHVFromFile() numberHVPlanes= " << numberHVPlanes << endl;

  int status,ipc,iarm,pcplane;
  for (int i=0; i<numberHVPlanes; i++) {
    fscanf(fin,"%d %d\n",&ipc,&iarm);
    pcplane=ipc*2+iarm; 
    if (Debug>0) {
      cout << "PadAncObjy::FetchHVFromFile() ipc= " << ipc << endl;
      cout << "PadAncObjy::FetchHVFromFile() iarm= " << iarm << endl;
      cout << "PadAncObjy::FetchHVFromFile() pcplane= " << pcplane << endl;
    }
    if ( (pcplane>=0) && (pcplane<MAXPCPLANES) ) 
      PlaneInfoAvailable[pcplane] = True; 
    for (int j=0; j<MAXHVSECT; j++) {
      if (j<(MAXHVSECT-1)) fscanf(fin,"%d ",&status); 
      else fscanf(fin,"%d\n",&status); 
      if (Debug>0) {
	cout << "PadAncObjy::FetchHVFromFile() j= " << j << endl;
	cout << "PadAncObjy::FetchHVFromFile() stat= " << status << endl;
      }
      if ( (pcplane>=0) && (pcplane<MAXPCPLANES) ) {
	if (status==1) HVOk[pcplane][j] = True; // on = ok
	else HVOk[pcplane][j] = False; // not ok
      }
    }
  }
  // ok, that was it.. we did it..
  fclose(fin);

  iFlag=0;  HVFlag=0;
  return True;
}
//**********************************************************************
// Fetch ROC calibration info from default ASCII file
PHBoolean PadAncObjy::FetchROCCalFromFile() {
  const char* fileroccal="padroccal.txt";
  return PadAncObjy::FetchROCCalFromFile(fileroccal);
}
//**********************************************************************
// Fetch ROC calibration info from an ASCII file "fileroccal"
PHBoolean PadAncObjy::FetchROCCalFromFile(const char* fileroccal) {
  
  // Open the file
  FILE *fin;
  if((fin = fopen(fileroccal,"r"))==NULL){ 
    cerr << "PadAncObjy::FetchROCCalFromFile() Cannot open file.\n";
    return False;
  }

  // first line tells us how many ROCs have been found 
  fscanf(fin,"%d\n",&numberROCCal); 
  if (numberROCCal>MAXCALROC) {
    cerr << "PadAncObjy::FetchROCCalFromFile() Too many ROCs.\n";
    cerr << "Number of ROCs= " << numberROCCal << " Maximum allowed number= " << MAXCALROC << " (this is the number of ROCs we will store)\n";
    numberROCCal=MAXCALROC;
  }
  int rocnr,meas,tgl1_1,tgl1_2,tgl1_3,tgl2_1,tgl2_2,tgl2_3,tgl3_1,tgl3_2,tgl3_3;  
  int ncols;                                                                   
  for (int i=0; i<numberROCCal; i++) {   
      ncols = fscanf(fin,"%d %d %d %d %d %d %d %d %d %d %d\n",&rocnr,&meas,&tgl1_1,&tgl1_2,&tgl1_3,&tgl2_1,&tgl2_2,&tgl2_3,&tgl3_1,&tgl3_2,&tgl3_3); 
      if (ncols < 0) break;
      roccalpar[i][0] = rocnr; 
      roccalpar[i][1] = meas;
      roccalpar[i][2] = tgl1_1;
      roccalpar[i][3] = tgl1_2;
      roccalpar[i][4] = tgl1_3;
      roccalpar[i][5] = tgl2_1;
      roccalpar[i][6] = tgl2_2;
      roccalpar[i][7] = tgl2_3;
      roccalpar[i][8] = tgl3_1;
      roccalpar[i][9] = tgl3_2;
      roccalpar[i][10] = tgl3_3;
  }
  // ok, that was it.. we did it..
  fclose(fin);

  iFlag=0; ROCCalFlag=0;
  return True;
}
//**********************************************************************
// Fetch ROC position info from default ASCII file
PHBoolean PadAncObjy::FetchROCPosFromFile() {
  const char* filerocpos="padrocpos.txt";
  return PadAncObjy::FetchROCPosFromFile(filerocpos);
}
//**********************************************************************
// Fetch ROC position info from an ASCII file "filerocpos"
PHBoolean PadAncObjy::FetchROCPosFromFile(const char* filerocpos) {
  
  // Open the file
  FILE *fin;
  if((fin = fopen(filerocpos,"r"))==NULL){ 
    cerr << "PadAncObjy::FetchROCPosFromFile() Cannot open file.\n";
    return False;
  }

  // first line tells us how many ROCs have been found 
  fscanf(fin,"%d\n",&numberROCPos); 
  if (numberROCPos>MAXINSTALLROC) {
    cerr << "PadAncObjy::FetchROCPosFromFile() Too many ROCs.\n";
    cerr << "Number of ROCs= " << numberROCPos << " Maximum allowed number= " << MAXINSTALLROC << " (this is the number of ROCs we will store)\n";
    numberROCPos=MAXINSTALLROC;
  }
  int packetid,grouprow,groupcolumn,rocnr;
  int ncols;
  for (int i=0; i<numberROCPos; i++) {   
      ncols = fscanf(fin,"%d %d %d %d\n",&packetid,&grouprow,&groupcolumn,&rocnr);
      if (ncols < 0) break;
      rocpospar[i][0] = packetid; 
      rocpospar[i][1] = grouprow; 
      rocpospar[i][2] = groupcolumn; 
      rocpospar[i][3] = rocnr; 
  }
  // ok, that was it.. we did it..
  fclose(fin);

  iFlag=0; ROCPosFlag=0;
  return True;
}
//**********************************************************************
// Fetch calibration constants from Objectivity (Objy) Database
PHBoolean PadAncObjy::Fetch() {
  
  PHBoolean status = FetchHVObjy();
  if(!status) {
    cerr << "PadAncObjy::Fetch() ERROR reading HV status." << endl;
    return False;
  }
  
  status = FetchROCCalObjy();
  if(!status) {
    cerr << "PadAncObjy::Fetch() ERROR reading ROC Calibrations." << endl;
    return False;
  }

  status = FetchROCCalObjy();
  if(!status) {
    cerr << "PadAncObjy::Fetch() ERROR reading ROC Positions." << endl;
    return False;
  }
  
  iFlag=0;
  return True;
}
//**********************************************************************
// Fetch HV status from Objy Database
PHBoolean PadAncObjy::FetchHVObjy() {
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {    
    // Fetch corresponding bank    
    setCalibName("calib.pad.hv");
    setBankNumber(4106);
    setBankID(BankNumber); // yes, slightly unnecessary, but it doesn't hurt to be explicit    

    PdbCalBank *padBank = bankManager->fetchBank("PdbPadHVBank", BankID,"calib.pad.hv", Tsearch);
    
    //
    // Test access to the header
    //
    if (Debug>0) {
      cout << "Information directly to the header:" << endl;
      cout << "startTime = " << padBank->getStartValTime() << endl;
      cout << "endTime   = " << padBank->getEndValTime() << endl;
      cout << "bankID    = " << padBank->getBankID().getInternalValue() << endl;
      cout << "---------------------------------------------" << endl;
    }
    
    if (padBank) {
      if (Debug>0) {
	padBank->print();
	cout << "Number of stored planes = " << padBank->getLength() << endl;
	for(unsigned int i=0; i < padBank->getLength(); i++) {
	  padBank->printEntry(i);
	}
      }
      // fill the hv struct
      numberHVPlanes=padBank->getLength();
      if (numberHVPlanes>MAXPCPLANES) {
	cerr << "PadAncObjy::FetchHVObjy() Too many panels.\n";
	cerr << "Number of planes= " << numberHVPlanes << " Maximum allowed number= " << MAXPCPLANES << " (this is the number of channels we will store)\n";
	numberHVPlanes=MAXPCPLANES;
      }
      PdbPadHV *hvobj;
      int ipc,iarm,pcplane;
      for(int i=0; i < numberHVPlanes; i++) {
	hvobj = (PdbPadHV*)&(padBank->getEntry(i));
	ipc=hvobj->get_PC();
	iarm=hvobj->get_Arm();
	pcplane=ipc*2+iarm; 
	
	if ( (pcplane>=0) && (pcplane<MAXPCPLANES) ) {
	  PlaneInfoAvailable[pcplane] = True; 
	  for(int j=0; j<MAXHVSECT; j++) 
	    HVOk[pcplane][j]=hvobj->get_HVStatusSect(j);
	}
      }
    }
    else {
      cout << "PadAncObjy::FetchHVObjy()" << endl;
      cout << "Error:" << endl;
      cout << "bankManager returned zero-pointer" << endl;
    }
    
    application->commit();
    
  }
  else {
    application->abort();
    cerr << "PadAncObjy::FetchHVObjy() ERROR: Transaction aborted." << endl;
    return False;
  }

  iFlag=0; HVFlag=0;
  return True;
}
//**********************************************************************
// Fetch ROC calibrations from Objy Database
PHBoolean PadAncObjy::FetchROCCalObjy() {

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    
    setCalibName("calib.pad.roccal");
    setBankNumber(4102);
    setBankID(BankNumber); // yes, slightly unnecessary, but it doesn't hurt to be explicit
    
    PdbCalBank *padBank = bankManager->fetchBank("PdbPadROCCalBank",BankID,"calib.pad.roccal",Tsearch);
    
    //
    // Test access to the header
    //
    if (Debug>0) {
      cout << "Information directly to the header:" << endl;
      cout << "startTime = " << padBank->getStartValTime() << endl;
      cout << "endTime   = " << padBank->getEndValTime() << endl;
      cout << "bankID    = " << padBank->getBankID().getInternalValue() << endl;
      cout << "---------------------------------------------" << endl;
    }
    
    if (padBank) {
      if (Debug>0) {
	padBank->print();
	cout << "Number of stored ROC calibrations = " << padBank->getLength() << endl;
	for(unsigned int i=0; i < padBank->getLength(); i++) {
	  padBank->printEntry(i);
	}
      }
      // fill the roccal struct
      numberROCCal=padBank->getLength();
      if (numberROCCal>MAXCALROC) {
	cerr << "PadAncObjy::FetchROCCalObjy() Too many calibrations.\n";
	cerr << "Number of ROC calibrations= " << numberROCCal << " Maximum allowed number= " << MAXCALROC << " (this is the number of ROCs we will store)\n";
	numberROCCal=MAXCALROC;
      }
      PdbPadROCCal *roccalobj;
      for(int i=0; i < numberROCCal; i++) {
	roccalobj = (PdbPadROCCal*)&(padBank->getEntry(i));
	for(int j=0; j<11; j++) 
	  roccalpar[i][j] = roccalobj->getParameter(j); 
      }
    }
    else {
      cout << "PadAncObjy::FetchROCCalObjy()" << endl;
      cout << "Error:" << endl;
      cout << "bankManager returned zero-pointer" << endl;
    }
    
    application->commit();
    
  }
  else {
    application->abort();
    cerr << "PadAncObjy::FetchROCCalObjy() ERROR: Transaction aborted." << endl;
    return False;
  }

  iFlag=0; ROCCalFlag=0;
  return True;
 
}
//**********************************************************************
// Fetch ROC positions from Objy Database
PHBoolean PadAncObjy::FetchROCPosObjy() {

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    
    setCalibName("calib.pad.rocpos");
    setBankNumber(4103);
    setBankID(BankNumber); // yes, slightly unnecessary, but it doesn't hurt to be explicit
    
    PdbCalBank *padBank = bankManager->fetchBank("PdbPadROCPosBank",BankID,"calib.pad.rocpos",Tsearch);
    
    //
    // Test access to the header
    //
    if (Debug>0) {
      cout << "Information directly to the header:" << endl;
      cout << "startTime = " << padBank->getStartValTime() << endl;
      cout << "endTime   = " << padBank->getEndValTime() << endl;
      cout << "bankID    = " << padBank->getBankID().getInternalValue() << endl;
      cout << "---------------------------------------------" << endl;
    }
    
    if (padBank) {
      if (Debug>0) {
	padBank->print();
	cout << "Number of stored ROC positions = " << padBank->getLength() << endl;
	for(unsigned int i=0; i < padBank->getLength(); i++) {
	  padBank->printEntry(i);
	}
      }
      // fill the rocpos struct
      numberROCPos=padBank->getLength();
      if (numberROCPos>MAXINSTALLROC) {
	cerr << "PadAncObjy::FetchROCPosObjy() Too many posibrations.\n";
	cerr << "Number of ROC posibrations= " << numberROCPos << " Maximum allowed number= " << MAXINSTALLROC << " (this is the number of ROCs we will store)\n";
	numberROCPos=MAXINSTALLROC;
      }
      PdbPadROCPos *rocposobj;
      for(int i=0; i < numberROCPos; i++) {
	rocposobj = (PdbPadROCPos*)&(padBank->getEntry(i));
	for(int j=0; j<4; j++) 
	  rocpospar[i][j] = rocposobj->getParameter(j); 
      }
    }
    else {
      cout << "PadAncObjy::FetchROCPosObjy()" << endl;
      cout << "Error:" << endl;
      cout << "bankManager returned zero-pointer" << endl;
    }
    
    application->commit();
    
  }
  else {
    application->abort();
    cerr << "PadAncObjy::FetchROCPosObjy() ERROR: Transaction aborted." << endl;
    return False;
  }

  iFlag=0; ROCPosFlag=0;
  return True;
 
}
// ********* Routines to put stuff into database *************
//**********************************************************************
// Put calibration constants to default ASCII files
PHBoolean PadAncObjy::PutToFile() {
  const char* filehv="padnewhv.txt";
  const char* fileroccal="padnewroccal.txt";
  const char* filerocpos="padnewrocpos.txt";
  return PadAncObjy::PutToFile(filehv,fileroccal,filerocpos);
}
//**********************************************************************
// Put calibration constants to ASCII files
PHBoolean PadAncObjy::PutToFile(const char* filehv, const char* fileroccal, const char* filerocpos) {
  
  PHBoolean status = PutHVToFile(filehv);
  if(!status) {
    cerr << "PadAncObjy::PutToFile() ERROR writing HV status." << endl;
    return False;
  }
  
  status = PutROCCalToFile(fileroccal);
  if(!status) {
    cerr << "PadAncObjy::PutToFile() ERROR writing ROC calibrations." << endl;
    return False;
  }

  status = PutROCPosToFile(filerocpos);
  if(!status) {
    cerr << "PadAncObjy::PutToFile() ERROR writing ROC positions." << endl;
    return False;
  }

  return True;  
}
//**********************************************************************
// Put HV info to default ASCII file
PHBoolean PadAncObjy::PutHVToFile() {
  const char* filehv="padnewhv.txt";
  return PadAncObjy::PutHVToFile(filehv);
}
//**********************************************************************
// Put HV info to an ASCII file "filehv"
PHBoolean PadAncObjy::PutHVToFile(const char* filehv) {
  
  // Open the file
  FILE *fout;
  if((fout = fopen(filehv,"w"))==NULL){ 
    cerr << "PadAncObjy::PutHVToFile() Cannot open file.\n";
    return False;
  }

  // first line tells us how many planes are active
  fprintf(fout,"%d\n",numberHVPlanes); 
  if (numberHVPlanes>MAXPCPLANES) {
    cerr << "PadAncObjy::PutHVToFile() Too many bad planes.\n";
    cerr << "Number of planes= " << numberHVPlanes << " Maximum allowed number= " << MAXPCPLANES << " (this is the number of channels we will store)\n";
    numberHVPlanes=MAXPCPLANES;
  }
  int status,ipc,iarm;
  for (int i=0; i<MAXPCPLANES; i++) {
    if (PlaneInfoAvailable[i]) {
      ipc=i/2;
      iarm=i%2;
      fprintf(fout,"%d %d\n",ipc,iarm); 
      for (int j=0; j<MAXHVSECT; j++) {
	if (HVOk[i][j]) status=1; // ok
	else status=-1; // not ok

	if (j<(MAXHVSECT-1)) fprintf(fout,"%d ",status); 
	else fprintf(fout,"%d\n",status); 
      }
    }
  }
  // ok, that was it.. we did it..
  
  fclose(fout);
  return True;
}
//**********************************************************************
// Put ROC calibration info to default ASCII file
PHBoolean PadAncObjy::PutROCCalToFile() {
  const char* fileroccal="padnewroccal.txt";
  return PadAncObjy::PutROCCalToFile(fileroccal);
}
//**********************************************************************
// Put ROC calibration info to an ASCII file "fileroccal"
PHBoolean PadAncObjy::PutROCCalToFile(const char* fileroccal) {
  
  // Open the file
  FILE *fout;
  if((fout = fopen(fileroccal,"w"))==NULL){ 
    cerr << "PadAncObjy::PutROCCalToFile() Cannot open file.\n";
    return False;
  }

  fprintf(fout,"%d\n",numberROCCal); 
  if (numberROCCal>MAXCALROC) {
    cerr << "PadAncObjy::PutROCCalToFile() Too many ROCs.\n";
    cerr << "Number of ROCs= " << numberROCCal << " Maximum allowed number= " << MAXCALROC << " (this is the number of ROCs we will store)\n";
    numberROCCal=MAXCALROC;
  }
  for (int i=0; i<numberROCCal; i++) {
    fprintf(fout,"%d %d %d %d %d %d %d %d %d %d %d\n",roccalpar[i][0],roccalpar[i][1],roccalpar[i][2],roccalpar[i][3],roccalpar[i][4],roccalpar[i][5],roccalpar[i][6],roccalpar[i][7],roccalpar[i][8],roccalpar[i][9],roccalpar[i][10]);
  }

  fclose(fout);
  return True;
}
//**********************************************************************
// Put ROC position info to default ASCII file
PHBoolean PadAncObjy::PutROCPosToFile() {
  const char* filerocpos="padnewrocpos.txt";
  return PadAncObjy::PutROCPosToFile(filerocpos);
}
//**********************************************************************
// Put ROC position info to an ASCII file "filerocpos"
PHBoolean PadAncObjy::PutROCPosToFile(const char* filerocpos) {
  
  // Open the file
  FILE *fout;
  if((fout = fopen(filerocpos,"w"))==NULL){ 
    cerr << "PadAncObjy::PutROCPosToFile() Cannot open file.\n";
    return False;
  }

  fprintf(fout,"%d\n",numberROCPos); 
  if (numberROCPos>MAXINSTALLROC) {
    cerr << "PadAncObjy::PutROCPosToFile() Too many ROCs.\n";
    cerr << "Number of ROCs= " << numberROCPos << " Maximum allowed number= " << MAXINSTALLROC << " (this is the number of ROCs we will store)\n";
    numberROCPos=MAXINSTALLROC;
  }
  for (int i=0; i<numberROCPos; i++) {
    fprintf(fout,"%d %d %d %d\n",rocpospar[i][0],rocpospar[i][1],rocpospar[i][2],rocpospar[i][3]);
  }

  fclose(fout);
  return True;
}
//**********************************************************************
// Put calibration constants to Objectivity (Objy) Database
PHBoolean PadAncObjy::Put() {
  
  PHBoolean status = PutHVObjy();
  if(!status) {
    cerr << "PadAncObjy::Put() ERROR storing HV info." << endl;
    return False;
  }
  
  status = PutROCCalObjy();
  if(!status) {
    cerr << "PadAncObjy::Put() ERROR storing ROC calibrations." << endl;
    return False;
  }

  status = PutROCPosObjy();
  if(!status) {
    cerr << "PadAncObjy::Put() ERROR storing ROC positions." << endl;
    return False;
  }
  
  return True;
}
//**********************************************************************
// Put HV info to Objy Database
PHBoolean PadAncObjy::PutHVObjy() {
  
  // Open database in update mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startUpdate()) {   

    // Put corresponding bank    
    setCalibName("calib.pad.hv");
    setBankNumber(4106);    
    setBankID(BankNumber); // yes, slightly unnecessary, but it doesn't hurt to be explicit    
    const char *descrip   = "HV info";

    PdbCalBank *padBank = bankManager->createBank("PdbPadHVBank", BankID, descrip, TUpdateStart, TUpdateStop,"calib.pad.hv");
    //
    if (numberHVPlanes>MAXPCPLANES) {
      cerr << "PadAncObjy::PutHVObjy() Too many planes.\n";
      cerr << "Number of planes= " << numberHVPlanes << " Maximum allowed number= " << MAXPCPLANES << " (this is the number of channels we will store)\n";
      numberHVPlanes=MAXPCPLANES;
    }    
    padBank->setLength(numberHVPlanes);
    if (Debug>0)  
      padBank->print();

    short ipc,iarm,okplanes=0;
    PdbPadHV *hvobj;
    for(int i=0; i < MAXPCPLANES; i++) {
      if (PlaneInfoAvailable[i]) {
	ipc=i/2;
	iarm=i%2;
	hvobj = (PdbPadHV*)&(padBank->getEntry(okplanes));
	hvobj->set_PC(ipc);
	hvobj->set_Arm(iarm);
	for(int j=0; j<MAXHVSECT; j++) 
	  hvobj->set_HVStatusSect(j,HVOk[i][j]);
	if (Debug>0)  
	  padBank->printEntry(okplanes);
	okplanes++;
      }
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "PadAncObjy::PutHVObjy() ERROR: Transaction aborted. Failed to start application." << endl;
    return False;
  }

  return True;
}
//**********************************************************************
// Put ROC calibrations to Objy Database
PHBoolean PadAncObjy::PutROCCalObjy() {

  // Open database in update mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startUpdate()) {   

    // Put corresponding bank    
    setCalibName("calib.pad.roccal");
    setBankNumber(4102);    
    setBankID(BankNumber); // yes, slightly unnecessary, but it doesn't hurt to be explicit    
    const char *descrip   = "ROC calibration info";

    PdbCalBank *padBank = bankManager->createBank("PdbPadROCCalBank", BankID, descrip, TUpdateStart, TUpdateStop,"calib.pad.roccal");
    //
    if (numberROCCal>MAXCALROC) {
      cerr << "PadAncObjy::PutROCCalObjy() Too many ROC calibrations.\n";
      cerr << "Number of ROC calibrations= " << numberROCCal << " Maximum allowed number= " << MAXCALROC << " (this is the number of ROCs we will store)\n";
      numberROCCal=MAXCALROC;
    }    
    padBank->setLength(numberROCCal);
    if (Debug>0)  
      padBank->print();

    PdbPadROCCal *roccalobj;
    for(int i=0; i < numberROCCal; i++) {
      roccalobj = (PdbPadROCCal*)&(padBank->getEntry(i));
      for(int j=0; j<11; j++) 
	roccalobj->setParameter(j,roccalpar[i][j]); 
      if (Debug>0)  
	padBank->printEntry(i);
    }    
    application->commit();
    
  }
  else {
    application->abort();
    cerr << "PadAncObjy::PutROCCalObjy() ERROR: Transaction aborted. Failed to start application." << endl;
    return False;
  }

  return True; 
}
//**********************************************************************
// Put ROC positions to Objy Database
PHBoolean PadAncObjy::PutROCPosObjy() {

  // Open database in update mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startUpdate()) {   

    // Put corresponding bank    
    setCalibName("calib.pad.rocpos");
    setBankNumber(4103);    
    setBankID(BankNumber); // yes, slightly unnecessary, but it doesn't hurt to be explicit    
    const char *descrip   = "ROC position info";

    PdbCalBank *padBank = bankManager->createBank("PdbPadROCPosBank", BankID, descrip, TUpdateStart, TUpdateStop,"calib.pad.rocpos");
    //
    if (numberROCPos>MAXINSTALLROC) {
      cerr << "PadAncObjy::PutROCPosObjy() Too many ROC positions.\n";
      cerr << "Number of ROC positions= " << numberROCPos << " Maximum allowed number= " << MAXINSTALLROC << " (this is the number of ROCs we will store)\n";
      numberROCPos=MAXINSTALLROC;
    }    
    padBank->setLength(numberROCPos);
    if (Debug>0)  
      padBank->print();

    PdbPadROCPos *rocposobj;
    for(int i=0; i < numberROCPos; i++) {
      rocposobj = (PdbPadROCPos*)&(padBank->getEntry(i));
      for(int j=0; j<4; j++) 
	rocposobj->setParameter(j,rocpospar[i][j]); 
      if (Debug>0)  
	padBank->printEntry(i);
    }    
    application->commit();
    
  }
  else {
    application->abort();
    cerr << "PadAncObjy::PutROCPosObjy() ERROR: Transaction aborted. Failed to start application." << endl;
    return False;
  }

  return True; 
}
// SIMPLE HV ROUTINES HERE 
//**********************************************************************
PHBoolean PadAncObjy::calcHVSummaryInfo()
{
  if(HVFlag!=0) {
    cerr << "PadAncObjy ERROR calcHVSummaryInfo: HV not initialized." << endl;
    return False;
  }
  else {
    numberOkHVTot=0;
    numberBadHVTot=0;

    for (int pcplane=0; pcplane<MAXPCPLANES; pcplane++) {
      if (PlaneInfoAvailable[pcplane]) {
	for (int ihvsectid=0; ihvsectid<MAXHVSECT; ihvsectid++) {
	  if (HVOk[pcplane][ihvsectid]) numberOkHVSect[pcplane]++;
	  else numberBadHVSect[pcplane]++;
	}
	numberOkHVTot+=numberOkHVSect[pcplane];
	numberBadHVTot+=numberBadHVSect[pcplane];
      }
    }
    return True;    
  }
} 
/* end calcHVSummaryInfo() */
//**********************************************************************
PHBoolean PadAncObjy::getHVStatus(short ipc, short iarm, short ihvsectid) 
{
  if(HVFlag!=0) {
    cerr << "PadAncObjy ERROR getHVStatus: HV not initialized." << endl;
    return False;
  }
  else if ( (ipc<0) || (ipc>2) || (iarm<0) || (iarm>1) || (ihvsectid<0) || (ihvsectid>=MAXHVSECT) ) {
    cerr << "PadAncObjy ERROR getHVStatus: arguments out of bounds." << endl;
    cerr << "ipc = " << ipc << " iarm = " << iarm << " ihvsectid = " << ihvsectid << endl;
    return False;
  }
  else {
    short pcplane = ipc*2+iarm;
    PHBoolean status = False; // default is not ok
    if ( (pcplane>=0) && (pcplane<MAXPCPLANES) ) {
      if (PlaneInfoAvailable[pcplane]) {
	if ( (ihvsectid>=0) && (ihvsectid<MAXHVSECT) ) {
	  if (HVOk[pcplane][ihvsectid]) status=True; // ok
	}
      }
    }
    return status;
  }
} 
/* end getHVStatus() */
//**********************************************************************
short PadAncObjy::getHVChamber(short ipc, short ihvsectid) 
{
  if(HVFlag!=0) {
    cerr << "PadAncObjy ERROR getHVChamber: HV not initialized." << endl;
    return -1;
  }
  else if ( (ipc<0) || (ipc>2) || (ihvsectid<0) || (ihvsectid>=MAXHVSECT) ) {
    cerr << "PadAncObjy ERROR getHVChamber: arguments out of bounds." << endl;
    cerr << "ipc = " << ipc << " ihvsectid = " << ihvsectid << endl;
    return False;
  }
  else {
    short chamber = -1; // default is not ok
    if (ipc==0) chamber = ihvsectid/4;
    else chamber = (ihvsectid%16)/4;

    return chamber;
  }
} 
/* end getHVChamber() */
//**********************************************************************
short PadAncObjy::getHVSide(short ipc, short iarm, short ihvsectid) 
{
  if(HVFlag!=0) {
    cerr << "PadAncObjy ERROR getHVSide: HV not initialized." << endl;
    return -1;
  }
  else if ( (ipc<0) || (ipc>2) || (iarm<0) || (iarm>1) || (ihvsectid<0) || (ihvsectid>=MAXHVSECT) ) {
    cerr << "PadAncObjy ERROR getHVSide: arguments out of bounds." << endl;
    cerr << "ipc = " << ipc << " iarm = " << iarm << " ihvsectid = " << ihvsectid << endl;
    return False;
  }

  else {
    short side = -1;
    if (ipc==0) {
      if (iarm==0) side = 1-(ihvsectid%2); // 0 and 2 is N on East
      else side = ihvsectid%2; // 0 and 2 is S on West
    }
    else side = ihvsectid/16; // 0 to 15 is S, 16 to 31 N 

    return side;
  }
} 
/* end getHVSide() */
//**********************************************************************
short PadAncObjy::getHVNwires(short ipc, short ihvsectid) 
{
  if(HVFlag!=0) {
    cerr << "PadAncObjy ERROR getHVNwires: HV not initialized." << endl;
    return -1;
  }
  else if ( (ipc<0) || (ipc>2) || (ihvsectid<0) || (ihvsectid>=MAXHVSECT) ) {
    cerr << "PadAncObjy ERROR getHVNwires: arguments out of bounds." << endl;
    cerr << "ipc = " << ipc << " ihvsectid = " << ihvsectid << endl;
    return False;
  }
  else {
    short nwires = -1;
    short hvsectmod = ihvsectid%4;
    if (ipc==0) { // PC1
      if ((hvsectmod==0) && (hvsectmod==0)) nwires = 9; // outer HV sectors
      else nwires = 20; // inner
    }
    else if (ipc==1) {  // PC2
      if ((hvsectmod==0) && (hvsectmod==0)) nwires = 24; // outer HV sectors
      else nwires = 34; // outer
    }
    else if (ipc==2)  // PC3
      nwires = 29; // all the same
    
    return nwires;
  }
} 
/* end getHVNwires() */
//**********************************************************************
PHBoolean PadAncObjy::getHVHardwareNotation(short ipc, short iarm, short ihvsectid, string &hvhwname) 
{
  if(HVFlag!=0) {
    cerr << "PadAncObjy ERROR getHVHardwareNotation: HV not initialized." << endl;
    return False;
  }
  else if ( (ipc<0) || (ipc>2) || (iarm<0) || (iarm>1) || (ihvsectid<0) || (ihvsectid>=MAXHVSECT) ) {
    cerr << "PadAncObjy ERROR getHVHardwareNotation: arguments out of bounds." << endl;
    cerr << "ipc = " << ipc << " iarm = " << iarm << " ihvsectid = " << ihvsectid << endl;
    return False;
  }

  else {    
    short chamber = -1;
    short side = -1;
    short hwindex = -1;
 
    if (ipc==0) {
      chamber = ihvsectid/4;
      if (iarm==0) {
	side = 1-(ihvsectid%2); // 0 and 2 is N on East
	// on this side, wire increases downward so we need to shift 
	// hwindex order
	hwindex = (3-(ihvsectid%4))/2;
      }
      else {
	side = ihvsectid%2; // 0 and 2 is S on West
	hwindex = (ihvsectid%4)/2;
      }
    }
    else {
      chamber = (ihvsectid%16)/4;
      side = ihvsectid/16; // 0 to 15 is S, 16 to 31 N 
      if (iarm==0) 	
	// on this side, wire increases downward so we need to shift 
	// hwindex order
	hwindex = 3-ihvsectid%4;
      else
	hwindex = ihvsectid%4;
    }
    char charm[5],chside[5];

    if (iarm==1) sprintf(charm,"W"); // West arm
    else sprintf(charm,"E"); // East arm
    
    if (side==0) sprintf(chside,"S"); // South side
    else sprintf(chside,"N"); // North side 

    // syntax ex. for hvname HV_PC1_W_S_0_0
    ostringstream hardwarehv;
    hardwarehv << "HV_PC" << ipc+1 << "_" << charm << "_" << chside
	       << "_" << chamber << "_" << hwindex;
    hvhwname = hardwarehv.str();

    return True;
  }
} 
/* end getHVHardwareNotation() */
//**********************************************************************
//*********** And finally, just a print utilty.. *********************
//**********************************************************************
void PadAncObjy::print()
{
  // Print the parameter information
  if(iFlag!=0) {
    cerr << "PadAncObjy ERROR print: PAO not initialized." << endl;
  }
  cout << "PadAncObjy Parameter information" << endl;
  cout << "numberHVPlanes= " << numberHVPlanes << endl; 
  cout << "numberROCCal= " << numberROCCal << endl; 
  cout << "numberROCPos= " << numberROCPos << endl; 
  
  cout << endl;
  cout << "Debug= " << Debug;
  cout << " (should be >0 when debugging) " << endl;
  
} 
/* end print() */


