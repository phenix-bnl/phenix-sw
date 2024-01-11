// Implementation of class file: PHDchReconstructionObject.h
// Created by: Federica Messer at Mon Dec 27 13:11:37 1999

#include <fstream>
//INCLUDECHECKER: Removed this line: #include <cstdio>
//INCLUDECHECKER: Removed this line: #include <cstring>
#include "phool.h"
#include "PHDchReconstructionObject.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

using namespace std;

PHDchReconstructionObject::PHDchReconstructionObject()
  : PHReconstructionObject()
{
  initialize();
}

void 
PHDchReconstructionObject::initialize()
{
  committed = 1;
  recoParameters = 3;
}

PHDchReconstructionObject::~PHDchReconstructionObject()
{
  commit();
}

PHBoolean 
PHDchReconstructionObject::update(PHTimeStamp &Tstart, 
				  PHTimeStamp &Tstop,
				  const char *calibname,
				  PdbBankID bankID,
				  char *descrip )
{
  if(committed == 1) {
    if(!application->startUpdate()) {
      PHMessage("PHDchReconstructionObject", PHError, 
		"Aborting ... Database not writable");
      application->abort();
    }else{
      committed = 0;
    }
  }
  recoBank = bankManager->createBank("PdbParameterBank", bankID, 
				     descrip, Tstart,Tstop,calibname);

  int  length = recoParameters;
  recoBank->setLength(length);
  print();

  return True;
}
 
PHBoolean 
PHDchReconstructionObject::fetch(PHTimeStamp &Tsearch,
				 const char *calibname,
				 PdbBankID bankID )
{
  if(committed == 1) {
    if(!application->startRead()) {
      PHMessage("PHDchReconstructionObject",PHError, "Aborting ... Database not readable");
      application->abort();
    }else{
      committed =  0;
    }
  }
  recoBank = bankManager->fetchBank("PdbParameterBank",bankID,calibname,Tsearch);
  
  return True;
}   

PHBoolean 
PHDchReconstructionObject::fetchReconstructionFromFile()
{
  const char *dchRecoFile="reco.txt";
    
  if (!dchRecoFile) {
    cerr << " no file name given !! Fatal !!" << endl;
    return False;
  }
  ifstream file;
  file.open(dchRecoFile);
  if (!file) {
    cerr << " could not open input file " << dchRecoFile << " !!" << endl;
    return False;
  }

  return True;
}



