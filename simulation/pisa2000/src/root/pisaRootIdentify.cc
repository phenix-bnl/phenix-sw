// $Id: pisaRootIdentify.cc,v 1.8 2007/03/02 10:11:33 hpereira Exp $

/*!
  \file   pisarRootIdentify.cc
  \brief  read pisa file informations
  \author Charlie Maguire
  \version $Revision: 1.8 $
  \date $Date: 2007/03/02 10:11:33 $
*/
#include <iostream>
#include <cstdlib>

#include <TROOT.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>

#include "PISARun.h"
#include "PISAEvent.h"

#include <cmath>

using namespace std;

//________________________________________
//! main
int main(int argc, char** argv)
{
  try {

 	 // Initialize the ROOT system
	  TROOT simple("simple", "Example of creation of a tree");
	
	  //  Connect to input file
	  TFile f("PISAEvent.root");
	
	  //   Read Tree named "T" in memory. Tree pointer is assigned the same name
	  TTree* T = (TTree*)f.Get("T");  
	
	  //   Start main loop on all events
	  if(T == 0) {
	    cerr << "\n Bad TTree " << endl;
	    exit(1);
	  }
	  
	  int nevent = Int_t(T->GetEntries());
	
	  PISAEvent *pisaevent = new PISAEvent();
	  T->SetBranchAddress("pisaevent",&pisaevent);
	
    // retrieve first event and read header
	  Int_t kevent = 0;
	  T->GetEntry(kevent);
	
	  PISAEventHeader *EventHeader = pisaevent->GetHeader();
	  Int_t inputRunNumber = EventHeader->GetInputRunNumber();
	  Int_t outputRunNumber = EventHeader->GetOutputRunNumber();
	  Int_t projectNumber = EventHeader->GetProjectNumber();
	  Int_t versionNumber = EventHeader->GetVersionNumber();
	
	  cout << "pisaRootIdentify - PISA file contains event file inputRunNumber = " <<  inputRunNumber << endl;
	  cout << "pisaRootIdentify - hits file outputRunNumber = " <<  outputRunNumber << endl;
	  cout << "pisaRootIdentify - simulation projectNumber = " <<  projectNumber << endl;
	  cout << "pisaRootIdentify - software versionNumber = " <<  versionNumber << endl << endl;
	  cout << "pisaRootIdentify - Number of events present in the file = " << nevent << endl;
		  
	} catch (exception &e ) { cout << "pisaRootIdentify - " << e.what() << endl; }
  
  cout << "pisaRootIdentify - successfully completed" << endl;

  return 0;
  
} 
