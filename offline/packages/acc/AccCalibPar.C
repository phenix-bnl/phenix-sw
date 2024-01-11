
#include <iostream>
#include <fstream>

//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "AccCalibPar.h"

//INCLUDECHECKER: Removed this line: #include "PHString.h"
#include "PdbApplication.hh"
#include "PdbBankManager.hh"
#include "PdbCalBank.hh"
#include "PdbAccCalib.hh"

using namespace std;

//___________________________________________________________________
AccCalibPar::AccCalibPar() 
  : calibName(""),calibrationOK(0),verbosity(0)
{
  // default constructor
  // Initialization of Calibration container
  for(int ich=0;ich<ACC::ACC_NCH;ich++){
    Status.push_back(0);
    Par0.push_back(0);
    Par1.push_back(0);
    Par2.push_back(0);
    Par3.push_back(0);
  }

}

//___________________________________________________________________
AccCalibPar::AccCalibPar(const char* name) 
  : calibName(name),calibrationOK(0), verbosity(0)
{ 
  // Initialization of Calibration container
  for(int ich=0;ich<ACC::ACC_NCH;ich++){
    Status.push_back(0);
    Par0.push_back(0);
    Par1.push_back(0);
    Par2.push_back(0);
    Par3.push_back(0);
  }
}

//___________________________________________________________________
AccCalibPar::~AccCalibPar() { }

//___________________________________________________________________
int AccCalibPar::get_Status(const int ich)
{
  if(ich<0 || ich>=ACC::ACC_NCH){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return 0;
  }

  return Status[ich];
}

//___________________________________________________________________
float AccCalibPar::get_CalibPar(const int ich, const int seq)
{
  if(seq<0 || seq>=NPAR){
    cout << PHWHERE << " Invalid parameter id, seq= " << seq << endl;
    return 0;
  }

  if(seq==0) return get_CalibPar0(ich);
  if(seq==1) return get_CalibPar1(ich);
  if(seq==2) return get_CalibPar2(ich);
  if(seq==3) return get_CalibPar3(ich);

  return 0;
}

//___________________________________________________________________
float AccCalibPar::get_CalibPar0(const int ich)
{
  if(ich<0 || ich>=(int)Par0.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return 0;
  }

  return Par0[ich];
}

//___________________________________________________________________
float AccCalibPar::get_CalibPar1(const int ich)
{
  if(ich<0 || ich>=(int)Par1.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return 0;
  }

  return Par1[ich];
}

//___________________________________________________________________
float AccCalibPar::get_CalibPar2(const int ich)
{
  if(ich<0 || ich>=(int)Par2.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return 0;
  }

  return Par2[ich];
}

//___________________________________________________________________
float AccCalibPar::get_CalibPar3(const int ich)
{
  if(ich<0 || ich>=(int)Par3.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return 0;
  }

  return Par3[ich];
}

//___________________________________________________________________
void AccCalibPar::set_Status(const int ich, const int val)
{
  if(ich<0 || ich>=(int)Status.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  Status[ich] = val;
}

//___________________________________________________________________
void AccCalibPar::set_Par(const int ich, const int seq, const float val)
{
  if(seq<0 || seq>=NPAR){
    cout << PHWHERE << " Invalid seq number, seq= " << seq << endl;
    return;
  }

  if(seq==0) set_Par0(ich, val);
  if(seq==1) set_Par1(ich, val);
  if(seq==2) set_Par2(ich, val);
  if(seq==3) set_Par3(ich, val);

  return;
}

//___________________________________________________________________
void AccCalibPar::set_Par0(const int ich, const float val)
{
  if(ich<0 || ich>=(int)Par0.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  Par0[ich] = val;
}

//___________________________________________________________________
void AccCalibPar::set_Par1(const int ich, const float val)
{
  if(ich<0 || ich>=(int)Par1.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  Par1[ich] = val;
}

//___________________________________________________________________
void AccCalibPar::set_Par2(const int ich, const float val)
{
  if(ich<0 || ich>=(int)Par2.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  Par2[ich] = val;
}

//___________________________________________________________________
void AccCalibPar::set_Par3(const int ich, const float val)
{
  if(ich<0 || ich>=(int)Par3.size()){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  Par3[ich] = val;
}

//___________________________________________________________________
int AccCalibPar::fetch(const int run)
{
  cout << GetName() << "." << calibName.c_str() << " fetch" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  // open the federation in readonly mode
  cout << "NOW opening FD in readonly mode" << endl;
  if( application->startRead() ){

    PdbBankID bankID(ACC::ACC_BANKID_CALIB);

    string classname ("PdbAccCalibBank");
    string head ("acc.calib.");
    string calibname = head + calibName; 

    const char* ClassName = classname.c_str();
    const char* CalibName = calibname.c_str();

    PdbCalBank* accBank = bankManager->fetchBank(ClassName, bankID, CalibName, run);

    if( accBank ){
      accBank->printHeader();  
      startTime = accBank->getStartValTime();
      endTime   = accBank->getEndValTime();

      PdbAccCalib* entry = 0;
      for(unsigned int ich=0;ich< accBank->getLength(); ich++){
        entry = (PdbAccCalib*) & (accBank->getEntry (ich));

        set_Status(ich, entry->get_status());
        for(int ipar=0;ipar<NPAR;ipar++){
          set_Par(ich, ipar, entry->get_calibpar(ipar));
        }
      }
      if(verbosity>0) Print();

      delete accBank;
    }
    else{
      cout << PHWHERE << " Error: bankManager returned zero pointer" << endl;
      return 0;
    }
    application->commit();
  }
  else{
    application->abort();
    cout << PHWHERE << " Transaction aborted" << endl;
    return 0;
  }

  return 1;
}


//___________________________________________________________________
int AccCalibPar::fetch(const PHTimeStamp& time)
{
  // fetch calibration parameter from FD

  cout << GetName() << "." << calibName.c_str() << " fetch" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  // open the federation in readonly mode
  cout << "NOW opening FD in readonly mode" << endl;
  if( application->startRead() ){

    PHTimeStamp tStart = time;

    PdbBankID bankID(ACC::ACC_BANKID_CALIB);

    string classname ("PdbAccCalibBank");
    string head ("acc.calib.");
    string calibname = head + calibName; 

    PdbCalBank* accBank = bankManager->fetchBank(classname.c_str(), bankID, calibname.c_str(), tStart);

    if( accBank ){
      accBank->printHeader();  
      startTime = accBank->getStartValTime();
      endTime   = accBank->getEndValTime();

      for(unsigned int ich=0;ich< accBank->getLength();ich++){
        PdbAccCalib* entry = (PdbAccCalib*) & (accBank->getEntry (ich));

        set_Status(ich, entry->get_status());
        for(int ipar=0;ipar<NPAR;ipar++){
          set_Par(ich, ipar, entry->get_calibpar(ipar));
        }
      }
      if(verbosity>0) Print();

      delete accBank;
    }
    else{
      cout << PHWHERE << " Error: bankManager returned zero pointer" << endl;
      return 0;
    }
    application->commit();
  }
  else{
    application->abort();
    cout << PHWHERE << " Transaction aborted" << endl;
    return 0;
  }

  return 1;
}

//___________________________________________________________________
int AccCalibPar::fetch(const char* filename)
{
  // fetch calibration parameter from file

  ifstream fin(filename);
  if(!fin){
    cout << PHWHERE << " can't open " << filename << endl;
    return 0;
  }

  int status;
  float par[4];

  for(int ich=0;ich<ACC::ACC_NCH;ich++){
    fin >> status;
    Status[ich] = status;

    fin >> par[0] >> par[1] >> par[2] >> par[3];
    Par0[ich] = par[0];
    Par1[ich] = par[1];
    Par2[ich] = par[2];
    Par3[ich] = par[3];
  }
  fin.close();

  return 1;
}

//___________________________________________________________________
int AccCalibPar::fetch(string filename)
{
  // fetch calibration parameter from file

  return AccCalibPar::fetch(filename.c_str());
}

//___________________________________________________________________
int AccCalibPar::write(const char* filename)
{
  // write calibration parameters to file

  ofstream fout(filename);
  cout << " write parameters to " << filename << endl;

  for(int ich=0;ich<ACC::ACC_NCH;ich++){
    fout << Status[ich] << " "
         << Par0[ich] << " " << Par1[ich] << " " << Par2[ich] << " " << Par3[ich] << endl;
  }
  fout.close();

  return 1;
}

//___________________________________________________________________
int AccCalibPar::write(string filename)
{
  // write calibration parameters to file

  return AccCalibPar::write(filename.c_str());
}

//___________________________________________________________________
int AccCalibPar::update(const int beginrun, const int endrun)
{
  // update calibration parameter from beginrun to endrun

  cout << GetName() << "." << calibName.c_str() << " update" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  // open the federation in update mode
  cout << "NOW opening FD in update mode" << endl;
  if( application->startUpdate() ){

    PdbBankID bankID(ACC::ACC_BANKID_CALIB);

    string classname ("PdbAccCalibBank");
    string head ("acc.calib.");
    string calibname = head + calibName; 

//    PdbCalBank* prevBank = bankManager->fetchBank(classname.c_str(), bankID, calibname.c_str(), beginrun);
//    if( prevBank ){
//      cout << PHWHERE << " overlapping bank found. Check the run number " << endl;
//      return 1;
//    }

    PdbCalBank *accBank = bankManager->createBank(beginrun, endrun, classname.c_str(), bankID, "", calibname.c_str());
    accBank->setLength(ACC::ACC_NCH);

    for(int ich=0;ich<ACC::ACC_NCH;ich++){
      PdbAccCalib *entry = (PdbAccCalib*) & (accBank->getEntry (ich));
      entry->set_status( Status[ich] );

      entry->set_calibpar(0, Par0[ich]);
      entry->set_calibpar(1, Par1[ich]);
      entry->set_calibpar(2, Par2[ich]);
      entry->set_calibpar(3, Par3[ich]);
    }
    if(verbosity>0) Print();

    application->commit(accBank);

  }
  else{
    cout << PHWHERE << " failed to start application for update" << endl;
    return 1;
  }

  return 0;
}

//___________________________________________________________________
int AccCalibPar::update(const PHTimeStamp& start, const PHTimeStamp& stop)
{
  // update calibration parameter from Tstart to Tstop

  cout << GetName() << "." << calibName.c_str() << " update" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  // open the federation in update mode
  cout << "NOW opening FD in update mode" << endl;
  if( application->startUpdate() ){

    PHTimeStamp tStart = start;
    PHTimeStamp tStop  = stop;

    PdbBankID bankID(ACC::ACC_BANKID_CALIB);

    string classname ("PdbAccCalibBank");
    string head ("acc.calib.");
    string calibname = head + calibName; 

    PdbCalBank* prevBank = bankManager->fetchBank(classname.c_str(), 
                                                  bankID, 
                                                  calibname.c_str(),
                                                  tStart);
    if( prevBank ){
      cout << " overlapping bank found. change the EndValTime " << endl;
      prevBank->setEndValTime(start);
    }

    PdbCalBank *accBank = bankManager->createBank(classname.c_str(), bankID, "", tStart, tStop, calibname.c_str());
    accBank->setLength(ACC::ACC_NCH);

    for(int ich=0;ich<ACC::ACC_NCH;ich++){
      PdbAccCalib *entry = (PdbAccCalib*) & (accBank->getEntry (ich));

      entry->set_status( Status[ich] );
      entry->set_calibpar(0, Par0[ich]);
      entry->set_calibpar(1, Par1[ich]);
      entry->set_calibpar(2, Par2[ich]);
      entry->set_calibpar(3, Par3[ich]);
    }

    application->commit(accBank);
  }
  else{
    cout << PHWHERE << " failed to start application for update" << endl;
    return 1;
  }
  return 0;
}


void AccCalibPar::Print()
{

  for(int ich=0;ich<ACC::ACC_NCH;ich++){
    cout << Status[ich] << " "
         << Par0[ich] << " " << Par1[ich] << " "
         << Par2[ich] << " " << Par3[ich] << endl;
  }

}
