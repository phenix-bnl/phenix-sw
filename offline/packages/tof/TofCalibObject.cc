//-----------------------------------------------------------------------------
//  Implementation of class TofCalibObject
//
//  Author: Tatsuya Chujo (Univ. of Tsukuba)
//
//  History: 07/08/00  T.Chujo  First Version
//           08/06/00  T.Chujo  DB accsess update
//           08/23/00  A.Kiyomichi add Mip Peak info
//           01/09/04  H.Masui  DB access update
//-----------------------------------------------------------------------------

#include "PdbPmtFitPar.hh"
#include "PdbCalBank.hh"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"

#include "TofAddressObject.hh"
#include "TofCalibObject.hh"

#include <string>
#include <fstream> 
#include <iostream> 

using namespace std;

//========================
// member functions
//========================
// Constructor
TofCalibObject::TofCalibObject(){
  // initialize  
  setDebugLevel(0);
  setCalibName("calib.tof"); 
  setBankNumber(7300); 
  setBankID(BankNumber); 
  Tsearch.setToSystemTime(); 
  
  // Initialize
  for(int islat = 0; islat < TOF_NSLAT; islat++){
    for(int lu = 0; lu <2; lu++){
      TvcPede[islat][lu] = -9999.0;
      QvcPede[islat][lu] = -9999.0;
      TvcConv[islat][lu] = -9999.0;
      QvcConv[islat][lu] = -9999.0;
      SlewPar_a[islat][lu] = -9999.0;
      SlewPar_b[islat][lu] = -9999.0;
      MipPeak[islat][lu]   = -9999.0;
      MipSigma[islat][lu]  = -9999.0;
    }
    Toffset[islat]   = -9999.0;
    Yoffset[islat]   = -9999.0;
    Velocity[islat]  = -9999.0;
    ElossConv[islat] = -9999.0;
  }
  GlobalT[0] = -9999.0;
}

// Destructor
TofCalibObject::~TofCalibObject(){
}
//====================================================================
// Get and Set Calibration parameters
//====================================================================
//
// Get and Set TvcPede -----------------------------------------------
float TofCalibObject::getTvcPede(int lu, int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT || lu <0 || lu >=2) return -9999;
  return TvcPede[slatid][lu];
}
void TofCalibObject::setTvcPede(int lu, int slatid, float tvcpede){
  TvcPede[slatid][lu] = tvcpede;
}
//
// Get and Set QvcPede -----------------------------------------------
float TofCalibObject::getQvcPede(int lu, int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT || lu <0 || lu >=2) return -9999;
  return QvcPede[slatid][lu];
}
void TofCalibObject::setQvcPede(int lu, int slatid, float qvcpede){
  QvcPede[slatid][lu] = qvcpede;
}
//
// Get and Set TvcConv -----------------------------------------------
float TofCalibObject::getTvcConv(int lu, int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT || lu <0 || lu >=2) return -9999;
  return TvcConv[slatid][lu];
}
void TofCalibObject::setTvcConv(int lu, int slatid, float tvcconv){
  TvcConv[slatid][lu] = tvcconv;
}
//
// Get and Set QvcConv -----------------------------------------------
float TofCalibObject::getQvcConv(int lu, int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT || lu <0 || lu >=2) return -9999;
  return QvcConv[slatid][lu];
}
void TofCalibObject::setQvcConv(int lu, int slatid, float qvcconv){
  QvcConv[slatid][lu] = qvcconv;
}
//
// Get and Set SlewPar_a ---------------------------------------------
float TofCalibObject::getSlewPar_a(int lu, int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT || lu <0 || lu >=2) return -9999;
  return SlewPar_a[slatid][lu];
}
void TofCalibObject::setSlewPar_a(int lu, int slatid, float slewpar_a){
  SlewPar_a[slatid][lu] = slewpar_a;
}
//
// Get and Set SlewPar_b ---------------------------------------------
float TofCalibObject::getSlewPar_b(int lu, int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT || lu <0 || lu >=2) return -9999;
  return SlewPar_b[slatid][lu];
}
void TofCalibObject::setSlewPar_b(int lu, int slatid, float slewpar_b){
  SlewPar_b[slatid][lu] = slewpar_b;
}
//
// Get and Set Toffset -----------------------------------------------
float TofCalibObject::getToffset(int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT) return -9999;
  return Toffset[slatid];
}
void TofCalibObject::setToffset(int slatid, float toffset){
  Toffset[slatid] = toffset;
}
//
// Get and Set Yoffset -----------------------------------------------
float TofCalibObject::getYoffset(int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT) return -9999;
  return Yoffset[slatid];
}
void TofCalibObject::setYoffset(int slatid, float yoffset){
  Yoffset[slatid] = yoffset;
}
//
// Get and Set Velocity -----------------------------------------------
float TofCalibObject::getVelocity(int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT) return -9999;
  return Velocity[slatid];
}
void TofCalibObject::setVelocity(int slatid, float velocity){
  Velocity[slatid] = velocity;
}
//
// Get and Set ElossConv -----------------------------------------------
float TofCalibObject::getElossConv(int slatid){
  if(slatid < 0 || slatid >= TOF_NSLAT) return -9999;
  return ElossConv[slatid];
}
void TofCalibObject::setElossConv(int slatid, float elossconv){
  ElossConv[slatid] = elossconv;
}
//
// Get and Set GlobalT -----------------------------------------------
float TofCalibObject::getGlobalT(){
  return GlobalT[0];
}
void TofCalibObject::setGlobalT(float globalt){
  GlobalT[0] = globalt;
}
///
// Get and Set MipPeak -----------------------------------------------
float TofCalibObject::getMipPeak(int lu, int slatid){
  return MipPeak[slatid][lu];
}
float TofCalibObject::getMipSigma(int lu, int slatid){
  return MipSigma[slatid][lu];
}
void TofCalibObject::setMipPeak(int lu, int slatid, float mippeak, float mipsigma){
  MipPeak[slatid][lu]  = mippeak;
  MipSigma[slatid][lu] = mipsigma;
}

//====================================================================
// Fetch Calibration Parameters from ASCII file "filename"
//====================================================================
// Fetch TVC/QVC pedestal from an ASCII file "filename"
PHBoolean TofCalibObject::fetchPedestalFromFile(const char* filename,
						TofAddressObject *address){
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchPedestalFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int crate, slot, ch, slatid, pmt;
    float tvcpede, qvcpede;
    for(int i=0; i < TOF_NPMTS; i++) {
      file >> crate >> slot >> ch >> tvcpede >> qvcpede;
      
      if (file.eof()) break;
      slatid = address->getSlatID(crate,slot,ch);
      pmt    = address->getPmt(crate,slot,ch);
      TvcPede[slatid][pmt] = tvcpede;
      QvcPede[slatid][pmt] = qvcpede;
    }
  }
  file.close();

  iFlag=0;
  return True;
}
//
// Fetch TVC conversion factor from an ASCII file "filename"
PHBoolean TofCalibObject::fetchTvcConvFromFile(const char* filename,
					       TofAddressObject *address){
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchTvcConvFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int crate, slot, ch, slatid, pmt;
    float value;
    for(int i=0; i < TOF_NPMTS; i++) {
      file >> crate >> slot >> ch >> value;
      
      if (file.eof()) break;
      slatid = address->getSlatID(crate,slot,ch);
      pmt    = address->getPmt(crate,slot,ch);
      TvcConv[slatid][pmt] = value;
    }
  }
  file.close();

  iFlag=0;
  return True;
}
//
// Fetch QVC conversion factor from an ASCII file "filename"
PHBoolean TofCalibObject::fetchQvcConvFromFile(const char* filename,
					       TofAddressObject *address){
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchQvcConvFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int crate, slot, ch, slatid, pmt;
    float value;
    for(int i=0; i < TOF_NPMTS; i++) {
      file >> crate >> slot >> ch >> value;
      
      if (file.eof()) break;
      slatid = address->getSlatID(crate,slot,ch);
      pmt    = address->getPmt(crate,slot,ch);
      QvcConv[slatid][pmt] = value;
    }
  }
  file.close();

  iFlag=0;
  return True;
}
//
// Fetch Slewing parameter from an ASCII file "filename"
PHBoolean TofCalibObject::fetchSlewParFromFile(const char* filename) {
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchSlewParFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int slatid;
    float slew_a_low, slew_b_low, slew_a_up, slew_b_up;
    for(int i=0; i < TOF_NSLAT; i++) {
      file >> slatid >> slew_a_low >> slew_b_low 
	   >> slew_a_up >> slew_b_up;
      
      if (file.eof()) break;
      SlewPar_a[slatid][0] = slew_a_low;
      SlewPar_b[slatid][0] = slew_b_low;
      SlewPar_a[slatid][1] = slew_a_up;
      SlewPar_b[slatid][1] = slew_b_up;
    }
  }
  file.close();
  
  iFlag=0;
  return True;
}
//
// Fetch Time offset from an ASCII file "filename"
PHBoolean TofCalibObject::fetchToffsetFromFile(const char* filename) {
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchToffsetFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int slatid;
    float value;
    for(int i=0; i < TOF_NSLAT; i++) {
      file >> slatid >> value;
      if (file.eof()) break;
      Toffset[slatid] = value;
    }
  }
  file.close();

  iFlag=0;
  return True;
}
//
// Fetch Y position offset from an ASCII file "filename"
PHBoolean TofCalibObject::fetchYoffsetFromFile(const char* filename) {
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchYoffsetFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int slatid;
    float value;
    for(int i=0; i < TOF_NSLAT; i++) {
      file >> slatid >> value;
      if (file.eof()) break;
      Yoffset[slatid] = value;
    }
  }
  file.close();

  iFlag=0;
  return True;
}
//
// Fetch light velocity in scintillator from an ASCII file "filename"
PHBoolean TofCalibObject::fetchVelocityFromFile(const char* filename) {
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchVelocityFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int slatid;
    float value;
    for(int i=0; i < TOF_NSLAT; i++) {
      file >> slatid >> value;
      if (file.eof()) break;
      Velocity[slatid] = value;
    }
  }
  file.close();

  iFlag=0;
  return True;
}
//
// Fetch energy loss conversion factor from an ASCII file "filename"
PHBoolean TofCalibObject::fetchElossConvFromFile(const char* filename) {
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchElossConvFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int slatid;
    float value;
    for(int i=0; i < TOF_NSLAT; i++) {
      file >> slatid >> value;
      if (file.eof()) break;   
      ElossConv[slatid] = value;
    }
  }
  file.close();

  iFlag=0;
  return True;
}
//
// Fetch Global Time offset from an ASCII file "filename"
PHBoolean TofCalibObject::fetchGlobalTFromFile(const char* filename) {
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchGlobalTFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    float value;
    file >> value;
    GlobalT[0] = value;
  }
  file.close();

  iFlag=0;
  return True;
}
//
// Fetch Mip. Peak info from an ASCII file "filename"
PHBoolean TofCalibObject::fetchMipPeakFromFile(const char* filename) {
  
  ifstream file(filename);
  
  if(!file) {
    cerr << "TofCalibObject::fetchMipPeakFromFile ERROR:" << endl;
    cerr << "Can not open " << filename << endl;
    cerr << endl;
    return False;
  }
  else {
    int slatid;
    float peak0, peak1;
    for(int i=0; i < TOF_NSLAT; i++) {
      file >> slatid >> peak0 >> peak1;
      if (file.eof()) break;
      MipPeak[slatid][0] = peak0;
      MipPeak[slatid][1] = peak1;
    }
  }
  file.close();

  iFlag=0;
  return True;
}

//====================================================================
// Fetch Calibration Parameters from a Database
//====================================================================
//--------------------------------------
// Fetch All Parameters from a Database
//--------------------------------------

PHBoolean TofCalibObject::fetch() {
  TofCalibObject::fetchTvcPede();
  TofCalibObject::fetchQvcPede();
  TofCalibObject::fetchTvcConv();
  TofCalibObject::fetchQvcConv();
  TofCalibObject::fetchSlewPar();
  TofCalibObject::fetchToffset();
  TofCalibObject::fetchYoffset();
  TofCalibObject::fetchVelocity();
  TofCalibObject::fetchElossConv();
  TofCalibObject::fetchGlobalT();

  return True;
}

PHBoolean TofCalibObject::fetch(const int run) 
{

  Tsearch = getTimeStamp(run);

  TofCalibObject::fetchTvcPede();
  TofCalibObject::fetchQvcPede();
  TofCalibObject::fetchTvcConv();
  TofCalibObject::fetchQvcConv();
  TofCalibObject::fetchSlewPar();
  TofCalibObject::fetchToffset();
  TofCalibObject::fetchYoffset();
  TofCalibObject::fetchVelocity();
  TofCalibObject::fetchElossConv();
  TofCalibObject::fetchGlobalT();

  return True;
}

PHBoolean TofCalibObject::fetchTvcPede(const int run)
{
  Tsearch = getTimeStamp(run);

  return fetchTvcPede();
}

PHBoolean TofCalibObject::fetchQvcPede(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchQvcPede();
}

PHBoolean TofCalibObject::fetchTvcConv(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchTvcConv();
}

PHBoolean TofCalibObject::fetchQvcConv(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchQvcConv();
}

PHBoolean TofCalibObject::fetchSlewPar(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchSlewPar();
}

PHBoolean TofCalibObject::fetchToffset(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchToffset();
}

PHBoolean TofCalibObject::fetchYoffset(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchYoffset();
}

PHBoolean TofCalibObject::fetchVelocity(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchVelocity();
}

PHBoolean TofCalibObject::fetchElossConv(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchElossConv();
}

PHBoolean TofCalibObject::fetchGlobalT(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchGlobalT();
}

PHBoolean TofCalibObject::fetchMipPeak(const int run)
{
  Tsearch = getTimeStamp(run);
  return fetchMipPeak();
}


//------------------------------------
// Fetch TVC pedestal from a Database
// -----------------------------------
PHBoolean TofCalibObject::fetchTvcPede() {
  
  PdbPmtFitPar* fitpar;
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.tvcpede0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitPar", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
	fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	TvcPede[i][0] = fitpar->getPar0();
	TvcPede[i][1] = fitpar->getPar1();
	
	if(Debug>2) cout << "TvcPede (lower/upper): " << TvcPede[i][0] 
			 << " " << TvcPede[i][1] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchTvcPede() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchTvcPede() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//------------------------------------
// Fetch QVC pedestal from a Database
//------------------------------------
PHBoolean TofCalibObject::fetchQvcPede() {
  
  
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.qvcpede0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
	PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	QvcPede[i][0] = fitpar->getPar0();
	QvcPede[i][1] = fitpar->getPar1();
	
	if(Debug>2) cout << "QvcPede (lower/upper): " << QvcPede[i][0] 
			 << " " << QvcPede[i][1] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchQvcPede() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchQvcPede() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//---------------------------------------------
// Fetch TVC conversion factor from a Database
//---------------------------------------------
PHBoolean TofCalibObject::fetchTvcConv() {
  
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.tvcconv0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
        PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	TvcConv[i][0] = fitpar->getPar0();
	TvcConv[i][1] = fitpar->getPar1();
	
	if(Debug>2) cout << "TvcConv (lower/upper): " << TvcConv[i][0] 
			 << " " << TvcConv[i][1] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchTvcConv() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchTvcConv() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//---------------------------------------------
// Fetch QVC conversion factor from a Database
//---------------------------------------------
PHBoolean TofCalibObject::fetchQvcConv() {
  
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.qvcconv0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
        PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	QvcConv[i][0] = fitpar->getPar0();
	QvcConv[i][1] = fitpar->getPar1();
	
	if(Debug>2) cout << "QvcConv (lower/upper): " << QvcConv[i][0] 
			  << " " << QvcConv[i][1] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchQvcConv() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchQvcConv() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//-----------------------------------------
// Fetch Slewing parameter from a Database
//-----------------------------------------
PHBoolean TofCalibObject::fetchSlewPar() {
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.slewpar0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
        PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	SlewPar_a[i][0] = fitpar->getPar0();
	SlewPar_b[i][0] = fitpar->getPar1();
	SlewPar_a[i][1] = fitpar->getPar2();
	SlewPar_b[i][1] = fitpar->getPar3();
	
	if(Debug>2) cout << "Slewing Par A and B (lower/upper): " 
			 << SlewPar_a[i][0] << " " 
			 << SlewPar_b[i][0] << " "
			 << SlewPar_a[i][1] << " "
			 << SlewPar_b[i][1] <<endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchSlewPar() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchSlewPar() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//-----------------------------------
// Fetch Time offset from a Database
//-----------------------------------
PHBoolean TofCalibObject::fetchToffset() {
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.toffset0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
        PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	Toffset[i] = fitpar->getPar0();
	
	if(Debug>2) cout << "Toffset: " << Toffset[i] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchToffset() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchToffset() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//-----------------------------------------
// Fetch Y position offset from a Database
//-----------------------------------------
PHBoolean TofCalibObject::fetchYoffset() {
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.yoffset0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
        PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	Yoffset[i] = fitpar->getPar0();
	
	if(Debug>2) cout << "Yoffset: " << Yoffset[i] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchYoffset() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchYoffset() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//------------------------------------------------
// Fetch Light velocity in Scint. from a Database
//------------------------------------------------
PHBoolean TofCalibObject::fetchVelocity() {
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.velocity0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
        PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	Velocity[i] = fitpar->getPar0();
	
	if(Debug>2) cout << "Velocity: " << Velocity[i] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchVelocity() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchVelocity() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//-----------------------------------------------------
// Fetch Energy loss conversion factor from a Database
//-----------------------------------------------------
PHBoolean TofCalibObject::fetchElossConv() {
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.elossconv0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
        PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	ElossConv[i] = fitpar->getPar0();
	
	if(Debug>2) cout << "ElossConv: " << ElossConv[i] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchElossConv() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchElossConv() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}
//------------------------------------------
// Fetch Global time offset from a Database
//------------------------------------------
PHBoolean TofCalibObject::fetchGlobalT() {
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.globalt0";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      
      PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(0));
      GlobalT[0] = fitpar->getPar0();
      
      if(Debug>2) cout << "GlobalT: " << GlobalT[0] << endl;
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchGlobalT() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchGlobalT() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}

PHBoolean TofCalibObject::fetchMipPeak() {
  
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) {
    
    // Fetch corresponding bank
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(TOFCALIBBANK);
    const char* calibname = "calib.tof.mippeak";
    
    PdbCalBank *tofBank = bankManager->fetchBank("PdbPmtFitParBank", bankID, calibname, tSearch);
    if(Debug>0) tofBank->printHeader();
    
    if (tofBank) {
      if(Debug>1) tofBank->print();
      if(Debug>1) cout << "Number of Channels = " << tofBank->getLength() << endl;
      for(unsigned int i=0; i < tofBank->getLength(); i++) {
        PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
	MipPeak[i][0] = fitpar->getPar0();
	MipPeak[i][1] = fitpar->getPar1();
	
	if(Debug>2) cout << "MipPeak: " << MipPeak[i][0] << " " << MipPeak[i][1] << endl;
      }
      delete tofBank;
    }
    else {
      cerr << "TofCalibObject::fetchMipPeak() ERROR:" << endl;
      cerr << "bankManager returned zero-pointer." << endl;
      return False;
    }
    
    application->commit();
  }
  else {
    application->abort();
    cerr << "TofCalibObject::fetchMipPeak() ERROR:" << endl;
    cerr << "Transaction aborted." << endl;
    return False;
  }
  iFlag=0;
  return True;
}

//====================================================================
// Update calibration parameters from current TofCalib 
// object in memory 
//=================================================================== 
//----------------
// TvcPede update
//----------------
PHBoolean TofCalibObject::updateTvcPede(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateTvcPede:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Tvc Pedestal";
    const char *calibname = "calib.tof.tvcpede0";

    if(Debug>0) {
      cout << "TofCalibObject::updateTvcPede: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateTvcPede: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myTvcPede_l = TvcPede[i][0];
      float myTvcPede_u = TvcPede[i][1];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myTvcPede_l);
      fitpar->setPar1(myTvcPede_u);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(0.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateTvcPede ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}
//----------------
// QvcPede update
//----------------
PHBoolean TofCalibObject::updateQvcPede(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateQvcPede:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Qvc Pedestal";
    const char *calibname = "calib.tof.qvcpede0";
    if(Debug>0) {
      cout << "TofCalibObject::updateQvcPede: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateQvcPede: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myQvcPede_l = QvcPede[i][0];
      float myQvcPede_u = QvcPede[i][1];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myQvcPede_l);
      fitpar->setPar1(myQvcPede_u);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(1.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateQvcPede ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
} 
//----------------
// TvcConv update
//----------------
PHBoolean TofCalibObject::updateTvcConv(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateTvcConv:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Tvc Conversion";
    const char *calibname = "calib.tof.tvcconv0";
    if(Debug>0) {
      cout << "TofCalibObject::updateTvcConv: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateTvcConv: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);

    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myTvcConv_l = TvcConv[i][0];
      float myTvcConv_u = TvcConv[i][1];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myTvcConv_l);
      fitpar->setPar1(myTvcConv_u);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(0.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateTvcConv ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}
//----------------
// QvcConv update
//----------------
PHBoolean TofCalibObject::updateQvcConv(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateQvcConv:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Qvc Conversion";
    const char *calibname = "calib.tof.qvcconv0";
     if(Debug>0) {
      cout << "TofCalibObject::updateQvcConv: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateQvcConv: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myQvcConv_l = QvcConv[i][0];
      float myQvcConv_u = QvcConv[i][1];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myQvcConv_l);
      fitpar->setPar1(myQvcConv_u);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(1.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateQvcConv ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}
//----------------
// SlewPar update
//----------------
PHBoolean TofCalibObject::updateSlewPar(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateSlewPar:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Slewing Parameter";
    const char *calibname = "calib.tof.slewpar0";
    if(Debug>0) {
      cout << "TofCalibObject::updateSlewPar: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateSlewPar: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float mySlewPar_a_l = SlewPar_a[i][0];
      float mySlewPar_b_l = SlewPar_b[i][0];
      float mySlewPar_a_u = SlewPar_a[i][1];
      float mySlewPar_b_u = SlewPar_b[i][1];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(mySlewPar_a_l);
      fitpar->setPar1(mySlewPar_b_l);
      fitpar->setPar2(mySlewPar_a_u);
      fitpar->setPar3(mySlewPar_b_u);
      fitpar->setPar4(0.0);
      fitpar->setChi2(1.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateSlewPar ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}
//----------------
// Toffset update
//----------------
PHBoolean TofCalibObject::updateToffset(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateToffset:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Toffset";
    const char *calibname = "calib.tof.toffset0";
    if(Debug>0) {
      cout << "TofCalibObject::updateToffset: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateToffset: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myToffset = Toffset[i];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myToffset);
      fitpar->setPar1(0.0);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(1.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateToffset ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}
//----------------
// Yoffset update
//----------------
PHBoolean TofCalibObject::updateYoffset(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateYoffset:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Yoffset";
    const char *calibname = "calib.tof.yoffset0";
    if(Debug>0) {
      cout << "TofCalibObject::updateYoffset: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateYoffset: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myYoffset = Yoffset[i];

      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myYoffset);
      fitpar->setPar1(0.0);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(1.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateYoffset ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}
//-----------------
// Velocity update
//-----------------
PHBoolean TofCalibObject::updateVelocity(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateVelocity:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Light Velocity in Scint.";
    const char *calibname = "calib.tof.velocity0";
    if(Debug>0) {
      cout << "TofCalibObject::updateVelocity: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateVelocity: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myVelocity = Velocity[i];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myVelocity);
      fitpar->setPar1(0.0);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(1.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateVelocity ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}
//------------------
// ElossConv update
//------------------
PHBoolean TofCalibObject::updateElossConv(PHTimeStamp TStart, PHTimeStamp TStop) {
  
   
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateElossConv:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Energy Loss Conversion";
    const char *calibname = "calib.tof.elossconv0";
    if(Debug>0) {
      cout << "TofCalibObject::updateElossConv: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateElossConv: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myElossConv = ElossConv[i];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myElossConv);
      fitpar->setPar1(0.0);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(1.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateElossConv ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}
//----------------
// GlobalT update
//----------------
PHBoolean TofCalibObject::updateGlobalT(PHTimeStamp TStart, PHTimeStamp TStop) {
  
  
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateGlobalT:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration Global Time offset";
    const char *calibname = "calib.tof.globalt0";
    if(Debug>0) {
      cout << "TofCalibObject::updateGlobalT: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateGlobalT: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(1);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    float myGlobalT = GlobalT[0];
    
    PdbPmtFitPar* fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(0));
    fitpar->setPar0(myGlobalT);
    fitpar->setPar1(0.0);
    fitpar->setPar2(0.0);
    fitpar->setPar3(0.0);
    fitpar->setPar4(0.0);
    fitpar->setChi2(1.0);
    fitpar->setStatus(-1);
    
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateGlobalT ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }
  return True;
}

PHBoolean TofCalibObject::updateMipPeak(PHTimeStamp TStart, PHTimeStamp TStop)
{

  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  
  // Open the federation in update mode
  if(Debug>0){
    cout << "TofCalibObject::updateMipPeak:" << endl;
    cout << "Opening FD in update mode..." << endl;
  }
  if (application->startUpdate()) {
    PdbBankID bankID(TOFCALIBBANK);
    const char *descrip = "TOF Calibration MIP peak";
    const char *calibname = "calib.tof.mippeak";
    if(Debug>0) {
      cout << "TofCalibObject::updateMipPeak: calibname = " << calibname << endl;
      cout << "TofCalibObject::updateMipPeak: bankid = " << TOFCALIBBANK << endl;
    }
    
    PdbCalBank *tofBank = 
      bankManager->createBank("PdbPmtFitParBank", bankID, descrip, 
			      TStart, TStop, calibname);
    
    tofBank->setLength(TOF_NSLAT);
    tofBank->setUserName(UserName);
    
    if(Debug>1) tofBank->print();
    
    for(int i=0; i<TOF_NSLAT; i++) {
      
      float myMipPeak0 = MipPeak[i][0];
      float myMipPeak1 = MipPeak[i][1];
      
      PdbPmtFitPar *fitpar = (PdbPmtFitPar*)&(tofBank->getEntry(i));
      fitpar->setPar0(myMipPeak0);
      fitpar->setPar1(myMipPeak1);
      fitpar->setPar2(0.0);
      fitpar->setPar3(0.0);
      fitpar->setPar4(0.0);
      fitpar->setChi2(1.0);
      fitpar->setStatus(-1);
    }     
    application->commit();
  }
  else {
    cerr << "TofCalibObject::updateMipPeak ERROR:" << endl;
    cerr << " failed to start application for update." << endl;
    return False;
  }


  return True;
}

PHBoolean TofCalibObject::updateTvcPede(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateTvcPede(start, stop);
}

PHBoolean TofCalibObject::updateQvcPede(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateQvcPede(start, stop);
}

PHBoolean TofCalibObject::updateTvcConv(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateTvcConv(start, stop);
}

PHBoolean TofCalibObject::updateQvcConv(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateQvcConv(start, stop);
}

PHBoolean TofCalibObject::updateSlewPar(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateSlewPar(start, stop);
}

PHBoolean TofCalibObject::updateToffset(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateToffset(start, stop);
}

PHBoolean TofCalibObject::updateYoffset(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateYoffset(start, stop);
}

PHBoolean TofCalibObject::updateVelocity(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateVelocity(start, stop);
}

PHBoolean TofCalibObject::updateElossConv(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateElossConv(start, stop);
}

PHBoolean TofCalibObject::updateGlobalT(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateGlobalT(start, stop);
}

PHBoolean TofCalibObject::updateMipPeak(const int beginrun, const int endrun)
{
  PHTimeStamp start;
  PHTimeStamp stop;
  start = getTimeStamp(beginrun);
  stop = getTimeStamp(endrun);

  return updateMipPeak(start, stop);
}

//====================================================================
//  Write
//====================================================================
// Write Pedestal to ASCII file
PHBoolean TofCalibObject::writePedestalToFile(const char* filename, 
					      TofAddressObject *address){
  ofstream file(filename);
  
  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
    int crate  = address->getCrate(slatid);
    int slot   = address->getSlot(slatid);
    int ch_low = address->getChannel(0,slatid);
    int ch_up  = address->getChannel(1,slatid);
	
    file<<"   "<< crate <<"   "<< slot <<"   "<< ch_low 
	<<"\t"<< getTvcPede(0,slatid) 
	<<"\t \t"<< getQvcPede(0,slatid) << endl;
    
    file<<"   "<< crate <<"   "<< slot <<"   "<< ch_up 
	<<"\t"<< getTvcPede(1,slatid) 
	<<"\t \t"<< getQvcPede(1,slatid) << endl;
      }
  cout<<"Write Pedestal to "<<filename <<"  [ASCII file]"<<endl;

  file.close();
  
  return True;
}
// 
// Write TvcConv to ASCII file
PHBoolean TofCalibObject::writeTvcConvToFile(const char* filename, 
					     TofAddressObject *address){
  ofstream file(filename);
  
  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
    int crate  = address->getCrate(slatid);
    int slot   = address->getSlot(slatid);
    int ch_low = address->getChannel(0,slatid);
    int ch_up  = address->getChannel(1,slatid);
	
    file<<"   "<< crate <<"   "<< slot <<"   "<< ch_low 
	<<"\t"<< getTvcConv(0,slatid) << endl;
    
    file<<"   "<< crate <<"   "<< slot <<"   "<< ch_up 
	<<"\t"<< getTvcConv(1,slatid) << endl;
  }
  cout<<"Write TvcConv to "<<filename <<"  [ASCII file]"<<endl;

  file.close();

  return True;
}
// 
// Write QvcConv to ASCII file
PHBoolean TofCalibObject::writeQvcConvToFile(const char* filename, 
					     TofAddressObject *address){
  ofstream file(filename);
  
  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
    int crate  = address->getCrate(slatid);
    int slot   = address->getSlot(slatid);
    int ch_low = address->getChannel(0,slatid);
    int ch_up  = address->getChannel(1,slatid);
    
    file<<"   "<< crate <<"   "<< slot <<"   "<< ch_low 
	<<"\t"<< getQvcConv(0,slatid) << endl; 
    
    file<<"   "<< crate <<"   "<< slot <<"   "<< ch_up 
	<<"\t"<< getQvcConv(1,slatid) << endl;
  }
  cout<<"Write QvcConv to "<<filename <<"  [ASCII file]"<<endl;

  file.close();

  return True;
}
// 
// Write SlewPar to ASCII file
PHBoolean TofCalibObject::writeSlewParToFile(const char* filename){
  ofstream file(filename);

  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
	
    file<<"   "<< slatid <<"\t\t"<< 
      getSlewPar_a(0,slatid) <<"\t\t" <<
      getSlewPar_b(0,slatid) <<"\t\t" <<
      getSlewPar_a(1,slatid) <<"\t\t" <<
      getSlewPar_b(1,slatid) << endl;
  }
  cout<<"Write SlewPar to "<<filename <<"  [ASCII file]"<<endl;
  
  file.close();

  return True;
}
// 
// Write Toffset to ASCII file
PHBoolean TofCalibObject::writeToffsetToFile(const char* filename){
  ofstream file(filename);

  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
	
    file<<"   "<< slatid <<"\t"<< getToffset(slatid) << endl;
  }
  cout<<"Write Toffset to "<<filename <<"  [ASCII file]"<<endl;

  file.close();

  return True;
}
// 
// Write Yoffset to ASCII file
PHBoolean TofCalibObject::writeYoffsetToFile(const char* filename){
  ofstream file(filename);

  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
	
    file<<"   "<< slatid <<"\t"<< getYoffset(slatid) << endl;
  }
  cout<<"Write Yoffset to "<<filename <<"  [ASCII file]"<<endl;

  file.close();

  return True;
}
// 
// Write Velocity to ASCII file
PHBoolean TofCalibObject::writeVelocityToFile(const char* filename){
  ofstream file(filename);

  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
	
    file<<"   "<< slatid <<"\t"<< getVelocity(slatid) << endl;
  }
  cout<<"Write Velocity to "<<filename <<"  [ASCII file]"<<endl;

  file.close();

  return True;
}
// 
// Write ElossConv to ASCII file
PHBoolean TofCalibObject::writeElossConvToFile(const char* filename){
  ofstream file(filename);

  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
	
    file<<"   "<< slatid <<"\t"<< getElossConv(slatid) << endl;
  }
  cout<<"Write ElossConv to "<<filename <<"  [ASCII file]"<<endl;

  file.close();

  return True;
}
// 
// Write GlobalT to ASCII file
PHBoolean TofCalibObject::writeGlobalTToFile(const char* filename){
  ofstream file(filename);
  
  file<< getGlobalT() << endl;
  
  cout<<"Write GlobalT to "<<filename <<"  [ASCII file]"<<endl;

  file.close();
 
  return True;
}
// 
// Write Mip. Peak info to ASCII file
PHBoolean TofCalibObject::writeMipPeakToFile(const char* filename){
  ofstream file(filename);

  for(int slatid = 0; slatid < TOF_NSLAT; slatid++){
	
    file<<"   "<< slatid <<"\t"<< MipPeak[slatid][0]
	<<"\t\t"<< MipPeak[slatid][1] << endl;
  }
  cout<<"Write MipPeak to "<<filename <<"  [ASCII file]"<<endl;

  file.close();

  return True;
}
//====================================================================
//  Print
//====================================================================
void TofCalibObject::print(int slatid){
  if(iFlag!=0) {
    cerr << "TofCalibObject ERROR print: Tof Calib not initialized." << endl;
  }
  cout<<" "<<endl;
  cout<<"##### TofCalibObject #####"<<endl;
  cout<<" TVCPEDE(low/up)  = "<< getTvcPede(0,slatid) <<"\t"<< getTvcPede(1,slatid) <<endl;
  cout<<" QVCPEDE(low/up)  = "<< getQvcPede(0,slatid) <<"\t"<< getQvcPede(1,slatid) <<endl;
  cout<<" TVCCONV(low/up)  = "<< getTvcConv(0,slatid) <<"\t"<< getTvcConv(1,slatid) <<endl;
  cout<<" QVCCONV(low/up)  = "<< getQvcConv(0,slatid) <<"\t"<< getQvcConv(1,slatid) <<endl;
  cout<<" SLEWPAR low(A/B) = "<< getSlewPar_a(0,slatid) <<"\t"<< getSlewPar_b(0,slatid) <<endl;
  cout<<" SLEWPAR up (A/B) = "<< getSlewPar_a(1,slatid) <<"\t"<< getSlewPar_b(1,slatid) <<endl;
  cout<<" TOFFSET   = "<< getToffset(slatid)       <<endl;
  cout<<" YOFFSET   = "<< getYoffset(slatid)       <<endl;
  cout<<" VELOCITY  = "<< getVelocity(slatid)      <<endl;
  cout<<" ELOSSCONV = "<< getElossConv(slatid)     <<endl;
  cout<<" GLOBALT   = "<< getGlobalT()             <<endl;
  cout<<" "<<endl;
  cout<<" PdbCal::"<<endl;
  cout<<"    CalibName  = "<< CalibName <<endl;
  cout<<"    BankNumber = "<< BankNumber <<endl;
  cout<<"    TimeStamp  = "<< Tsearch<<endl;
}
