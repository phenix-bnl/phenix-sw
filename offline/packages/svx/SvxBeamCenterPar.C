// ===================
// FILE: SvxBeamCenterPar.C
// ===================


#include <SvxBeamCenterPar.h>

#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <RunToTime.hh>

#include <PHTimeStamp.h>

#include <string>
#include <iostream>

using namespace std;

SvxBeamCenterPar::SvxBeamCenterPar()
{
  Verbosity(0);
  setParameters(0.0, 0.0, 0, -1);
  
}

SvxBeamCenterPar::~SvxBeamCenterPar(){ }

bool SvxBeamCenterPar::fetchFromDB(const int run)
{
  if ((run<0)||(run>99999999)) {
    cerr << "SvxBeamCenter::fetch run= "  << run << endl;
    return false;
  }

  RunToTime   *rt      = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(run);
  if(m_verbosity>5){
     cout<<"RunNumber : "<<run<<endl;
     cout<<"Time      : "<<flush;
     Tsearch->print();
     cout<<endl;
  }

  bool result = fetchFromDB(Tsearch);

  delete Tsearch;

  return result;
}

bool SvxBeamCenterPar::fetchFromDB(const PHTimeStamp* Tsearch)
{
  PdbCalBank *svxBank   = NULL;
  const char *tableName = "calibsvxbeamcenter";
  PdbBankID bankID;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startRead()) {
    application->abort();
    std::cerr << PHWHERE
	      << " ERROR -> Transaction aborted. Database NOT available."
	      << std::endl;
    return false;
  }
  
  int keyvalue=0;
  bankID.setInternalValue(keyvalue);//important!!

  if (Tsearch == NULL) {
    cerr << "SvxBeamCenterPar::fetch Error Tsearch==null" <<endl;
    return false;
  }
  
  const char *bankName = "PdbSvxBeamCenterBank";
  

  if (m_verbosity>90) {
    cout << "tableName = " << tableName << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Tsearch: " << *Tsearch << endl;
  }


  svxBank = bankManager->fetchBank(bankName, bankID, tableName, *Tsearch);

  bool success = true;
  if (svxBank) {
    unsigned numRecords = (unsigned)svxBank->getLength();

    if (m_verbosity>90) cout << "fetch numRecords = " << numRecords << endl;
    
    for (unsigned recordNumber=0; recordNumber<numRecords; recordNumber++) {
      m_beamcenter = (PdbSvxBeamCenter &) (svxBank->getEntry(recordNumber));
    }
  }
  else {
    cerr << PHWHERE << " ERROR -> bankManager returned null pointer."<<endl;
    success = false;
  }

  if(success) {
    application->commit();
    if (m_verbosity>90) cout << "fetch commit()" << endl; 
  }
  else {
    application->abort();
    if (m_verbosity>90) cout << "fetch abort()" << endl;
  }
  
  if(svxBank) delete svxBank;
  
  return success;
}




bool SvxBeamCenterPar::updateToDB(const int beginRun, const int endRun, const char *desc)
{
  if ((beginRun<0)||(beginRun>99999999)||
      (endRun<0)  ||(endRun>99999999)) 
  {
    cerr << "SvxBeamCenter::updateToDB Strange run" << endl;
    cerr << "beginRun = " << beginRun << ", endRun = " << endRun << endl;
    return false;
  }

  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *Tbegin = rt->getBeginTime(beginRun);
  PHTimeStamp *Tend  = rt->getEndTime(endRun);

  bool result  = updateToDB(Tbegin, Tend, desc);

  delete Tbegin;
  delete Tend;

  return result;
}

bool SvxBeamCenterPar::updateToDB(const PHTimeStamp* Tbegin, const PHTimeStamp* Tend, const char *desc)
{
  bool success = true;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  const char *tableName = "calibsvxbeamcenter";

  PdbCalBank *svxBank = NULL;

  if (!application->startUpdate()) {
    application->abort();
    cerr << PHWHERE << "ERROR: Database not writable, aborting." << endl;
    return false;
  }

  //
  if (Tbegin == NULL) {
    cerr << "SvxBeamCenterPar::updateToDB> Error Tbegin==NULL" << endl;
    return false;
  }
  if (Tend == NULL) {
    cerr << "SvxBeamCenterPar::updateToDB> Error Tend==NULL"<< endl;
    return false;
  }

  // description
  string s_desc;
  if(desc==NULL){
    s_desc = string("BeamOffset Period : ") + Tbegin->formatTimeString() + " - " + Tend->formatTimeString();
  }
  else {
    s_desc = string(desc);
  }
  if(s_desc.size()>256){
    s_desc = s_desc.substr(0, 256);
    cout<<"Description should be less than 256. Longer part is cut."<<endl;
    if(m_verbosity>5) cout<<"Description : "<<endl<<s_desc.c_str()<<endl;
  }

  const char *bankName = "PdbSvxBeamCenterBank";
  
  int keyvalue = 0;
  PdbBankID bankID;
  bankID.setInternalValue(keyvalue);

  cout << "tableName = " << tableName << endl;
  cout << "description = " << s_desc.c_str() << endl;
  cout << "bankID = " << bankID.getInternalValue() << endl;
  cout << "Validity range: " << *Tbegin << " - " << *Tend << endl;

  
  //here we have correspondence between PdbSvxPixelHotDeadMap structure and bank record length
  svxBank = bankManager->createBank(bankName, bankID, s_desc.c_str(), 
                              (PHTimeStamp&)(*Tbegin), (PHTimeStamp&)(*Tend), tableName);



  if (svxBank) {
    // The number of records is same as the number of data in the map array
    svxBank->setLength(1);
    
    // And now write the value to DB
    unsigned recordNumber = 0;
    PdbSvxBeamCenter *rec = dynamic_cast<PdbSvxBeamCenter*> (&(svxBank->getEntry(recordNumber)));
    if(rec!=NULL){
      *rec = m_beamcenter;
    }
    else {
      cerr << PHWHERE << " ERROR: can not get BeamCenterObject." << endl;
      success = false;
    }
  }
  else {
    cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
    success = false;
  }
  

  if (m_verbosity>90) cout << "updateToDB success = " << success << endl;

  if (success) {
    application->commit();
    if (m_verbosity>90) cout << "updateToDB commit()" << endl;
    
  }
  else {
    application->abort();
    if (m_verbosity>90) cout << "updateToDB abort()" << endl;
  }

  if (svxBank!=NULL) delete svxBank;

  return success;
}

void SvxBeamCenterPar::print() const 
{
  cout << endl;
  m_beamcenter.print();
}
