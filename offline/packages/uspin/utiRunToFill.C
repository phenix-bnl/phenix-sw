///////////////////////////////////////////////////////////////////////
//
// utiRunToFill class
//
// Author:  F.Messer, BNL
// 
// Description: see .hh file  
//
//////////////////////////////////////////////////////////////////////


#include <cmath>
#include <iostream>
#include "utiRunToFill.hh"
#include <fstream>
  
using namespace std;

utiRunToFill::utiRunToFill(const char* file)
{
  verbose = 0;
  readFromFile = 1;
  readFromDB   = 0;
  filename = file;
  
}

void utiRunToFill::set_readFromFile(const char* file)
{
  readFromFile = 1; 
  readFromDB   = 0; 
  filename     = file;
}

void utiRunToFill::set_readFromDB() 
{ 
  readFromDB   = 1; 
  readFromFile = 0; 
  cout << "DB not ready "<< endl;

}


int utiRunToFill::getFillNumber(int run)
{
  

  int fillNumber = -1;

  if (readFromFile) { //-------------------------------------------------------------
    ifstream file;
    //file.open("/phenix/workarea/federica/outgoing/fillVsRun_year02.txt"); // copy of the file
    file.open(filename);
    if (!file) {
      cout << " could not open input file " << filename  << " !!!! "<< endl;
      return fillNumber;
    }else {
      if (verbose) cout << "Fill vs Run file found  !! for utiRunvsFill" << endl;
    }
    
    int tmpfill, tmprun1, tmprun2;
    while (!file.eof()) {
      file >> tmpfill >> tmprun1 >> tmprun2 ;
      if (run <= tmprun2 && run >= tmprun1) {
	fillNumber = tmpfill;
	if (verbose) cout << "Run Number: " << run << " Fill: " << fillNumber << endl;
	return fillNumber;
      }
    }
  }else if (readFromDB) { //---------------------------------------------------------
    cout << "Reading from DB not implemented yet !!"<< endl;
    cout << "Reading from DB not implemented yet !!"<< endl;
    cout << "Reading from DB not implemented yet !!"<< endl;
    cout << "Reading from DB not implemented yet !!"<< endl;
  }else {                 //----------------------------------------------------------
    cout << "Not this option :: Choose between readFromFile or readFromDB "<< endl;
  }
  return fillNumber;

}

























