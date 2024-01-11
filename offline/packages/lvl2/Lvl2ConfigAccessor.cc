#include <Lvl2ConfigAccessor.h>
#include <cstdio>
#include <iostream>
#include <phool.h>

using namespace std;

ooRef(Lvl2TrigConfigObjyBase) Lvl2ConfigAccessor::restoreLvl2TrigSeq(int runNum) 
{

  ooRef(ooFDObj)  fdRef;
  ooRef(ooDBObj)  dbRef;
  cout << "Requested Run number " << runNum << endl;
  char *dbname = "L2TestConfig";

  if ( fdRef.open(bootFile, oocRead ) != oocSuccess )
    {
      cerr << "ERROR: Cannot open existing Federated Database.\n";
      //      return 1;
    }else{
      cout << "FD opened " << bootFile << endl; 
    }
  
  if (dbRef.exist(oovTopFD, dbname) ){
    if (dbRef.open(oovTopFD, dbname, oocRead) != oocSuccess){ 
      cout << "database open error, could not find " << dbname << endl;
      //      return 2;
    }
  }
  else{
    cout << "database " << dbname << " does not exist" << endl;
    //    return 3;
  }

  ooRef(Lvl2TrigConfigObjyBase) Lvl2TrigObjyRef = NULL;

  //  Lvl2TrigObjyRef = findCurrentLvl2Configuration(dbRef);
   
  ooItr(Lvl2TrigConfigObjyBase) L2Itr;
  
  if(L2Itr.scan(dbRef) != oocSuccess){
    cout << "scan error" << endl;
    //    return 4;
  }
    
  cout << "Requested Run number " << runNum << endl;

  while (L2Itr.next()) {
    if( L2Itr->getRunNumber() == runNum) {
      Lvl2TrigObjyRef = L2Itr;
      return Lvl2TrigObjyRef;
    }
  }
  cout << "###############################################" <<endl; 
  cout << "###############################################" <<endl <<endl; 
  cout << "Could not find DB entry for run number " << runNum << endl <<endl;
  cout << " returning NULL object " << endl;
  cout << "###############################################" <<endl; 
  cout << "###############################################" <<endl; 
  return Lvl2TrigObjyRef;
}

void Lvl2ConfigAccessor::printLvl2Mode(Lvl2OperationMode &t) {
  
  switch(t) {
  case EvBLvl2disabled:
    cout << "Lvl2disabled" << endl;
    break;
  case EvBLvl2enabledNoReject:
    cout << "EvBLvl2enabledNoReject" << endl;
    break;
  case EvBLvl2enabledReject:
    cout << "EvBLvl2enabledReject" << endl;
    break;
  default:
    break;
  }
}
