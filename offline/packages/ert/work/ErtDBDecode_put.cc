/*
#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include "PdbObjyBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankID.hh"
#include "PdbEmcT0Sector.hh"
#include "PdbErtDecode.hh"
#include "PdbCalBank.hh"
#include "PdbBankList.hh"
#include "PdbBankListIterator.hh"
#include "PHTimeStamp.h"



int update(PdbErtDecode *ertdbdecode, FILE *fp) 
{
  int word4x4ACoord_ROC, word4x4ACoord_WORD;
  int word4x4BCoord_ROC, word4x4BCoord_WORD;
  int word4x4CCoord_ROC, word4x4CCoord_WORD;
  int word2x2Coord_ROC, word2x2Coord_WORD;
  int word4x5CoordRICH_ROC, word4x5CoordRICH_WORD;
  int SMCoordModeAPBGL; 
  int SMCoordModeBPBGL;
  int SMCoordModeAPBSC;
  int SMCoordModeBPBSC;
  int SMCoordModeARICH;
  int SMCoordModeBRICH;

    //... 4x4A ..
  for(int k = 0; k<2; k++)
     for(int j = 0; j<4; j++) {
        fscanf(fp, "%d%d", &word4x4ACoord_ROC, &word4x4ACoord_WORD); 
	ertdbdecode->set_word4x4ACoord_ROC_WORD(j, k, word4x4ACoord_ROC, word4x4ACoord_WORD);
     }

    //... 4x4B ..
  for(int k = 0; k<2; k++)
     for(int j = 0; j<4; j++) {
	fscanf(fp, "%d%d", &word4x4BCoord_ROC, &word4x4BCoord_WORD); 
	ertdbdecode->set_word4x4BCoord_ROC_WORD(j, k, word4x4BCoord_ROC, word4x4BCoord_WORD);
     }

    //... 4x4C ..
  for(int k = 0; k<2; k++)
     for(int j = 0; j<4; j++) {
	fscanf(fp, "%d%d", &word4x4CCoord_ROC, &word4x4CCoord_WORD); 
	ertdbdecode->set_word4x4CCoord_ROC_WORD(j, k, word4x4CCoord_ROC, word4x4CCoord_WORD);
     }
    //..  2x2 ....
  for(int k = 0; k<2; k++)
     for(int j = 0; j<4; j++) {
        fscanf(fp, "%d%d", &word2x2Coord_ROC, &word2x2Coord_WORD); 
	ertdbdecode->set_word2x2Coord_ROC_WORD(j, k, word2x2Coord_ROC, word2x2Coord_WORD);
     }

   //..  RICH ....
  for(int k = 0; k<2; k++)
     for(int j = 0; j<4; j++) {
        fscanf(fp, "%d%d", &word4x5CoordRICH_ROC, &word4x5CoordRICH_WORD); 
	ertdbdecode->set_word4x5Coord_ROC_WORD(j, k, word4x5CoordRICH_ROC, word4x5CoordRICH_WORD);
     }

  for(int i = 0; i<16; i++) {
    fscanf(fp, "%d", &SMCoordModeAPBGL);
    ertdbdecode->set_SMCoordModeAPBGL(i, SMCoordModeAPBGL);
  }
  for(int i = 0; i<16; i++) {
    fscanf(fp, "%d", &SMCoordModeBPBGL);
    ertdbdecode->set_SMCoordModeBPBGL(i, SMCoordModeBPBGL);
  }
  for(int i = 0; i<16; i++) {
    fscanf(fp, "%d", &SMCoordModeAPBSC);
    ertdbdecode->set_SMCoordModeAPBSC(i, SMCoordModeAPBSC);
  }
  for(int i = 0; i<16; i++) {
    fscanf(fp, "%d", &SMCoordModeBPBSC);
    ertdbdecode->set_SMCoordModeBPBSC(i, SMCoordModeBPBSC);
  }
  for(int i = 0; i<16; i++) {
    fscanf(fp, "%d", &SMCoordModeARICH);
    ertdbdecode->set_SMCoordModeARICH(i, SMCoordModeARICH);
  }
  for(int i = 0; i<16; i++) {
    fscanf(fp, "%d", &SMCoordModeBRICH);
    ertdbdecode->set_SMCoordModeBRICH(i, SMCoordModeBRICH);
  }

  fclose(fp);
  return 0;
}

//_______________________________________________
int main()
{

   //
   // Select Objy implementation.
   //
   PdbBankManager *bankManager = PdbObjyBankManager::instance();  

   //
   // Get application manager class.
   //
   PdbApplication *application = bankManager->getApplication();  

   //
   // Open the federation in update mode
   //
   cout << "Opening FD in update mode.." << endl;
   if (application->startUpdate()) {
	PHTimeStamp tStart = PHTimeStamp(1999,1,1,0,0,0);
	PHTimeStamp tStop  = PHTimeStamp(2038,1,1,0,0,0);
	//
	// The string argument for the constructor of PdbBankID
	// currently has no effect. You can still use the bank-id
	// by setting its internal integer value, as done below.
	//
	PdbBankID bankID("");
	bankID.setInternalValue(0);
	//
	// 
	//
	char *descrip   = "ERT packets ROC/WORD mapping ";
	//
	// This resolves into the path name for the actual database file.
	// The given example would create a database as calib/emc/calib.emc.laser.pdb.,
	// calib.emc.laser being the Objy database name. This may look strange, but
	// it is necessary to prevent overwriting.
	//
	char *calibname = "calib.ErtBLT.map";
    
	PdbCalBank *ertBank = bankManager->createBank("PdbErtDecodeBank", bankID, descrip, tStart, tStop, calibname);
	ertBank->setLength(1);
	
	ertBank->print();

	FILE* fp;
	if((fp=fopen("/afs/rhic/phenix/software/calibration/run2001/SM_Word_map.dat", "r"))==NULL) {
	     std::cout<<"** can not find file SM_Word_map.dat, exit now !! **"<<endl;
	     return -1;
	}


        PdbErtDecode *ertdbdecode;
	for(int i=0; i < ertBank->getLength(); i++) {
	    ertdbdecode = (PdbErtDecode*)&(ertBank->getEntry(i));
	    update (ertdbdecode, fp);
	}
  
	application->commit();
   }
   else {
	cout << "failed to start application for update" << endl;
   }
  

   return 0;
}
*/
