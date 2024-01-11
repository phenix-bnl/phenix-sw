#include <iostream.h>
#include <fstream.h>
#include "PdbObjyBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankID.hh"
#include "PdbErtDecode.hh"
#include "PdbCalBank.hh"
#include "PdbBankList.hh"
#include "PdbBankListIterator.hh"
#include "PHTimeStamp.h"
#include <map.h>


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


   PdbCalBank *ertBank;

   if (application->startRead()) {
	PHTimeStamp tStart = PHTimeStamp(2001,1,3,0,0,0);
	PHTimeStamp tStop  = PHTimeStamp(2002,28,2,0,0,0);
	
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
	char *descrip   = "ERT packets ROC/WORD mapping";
	//
	// This resolves into the path name for the actual database file.
	// The given example would create a database as calib/emc/calib.emc.laser.pdb.,
	// calib.emc.laser being the Objy database name. This may look strange, but
	// it is necessary to prevent overwriting.
	//
	char *calibname = "calib.ErtBLT.map";

	int run, arm, sector;
	float tofcorr, tofcorr_error;
	int tofcorr_status;

        PdbErtDecode *ertdbdecode;

	PdbBankList bankList;
       	bankManager->fetchAllBanks(bankList, "PdbErtDecodeBank", bankID, calibname, 40925);
	
	PdbBankListIterator iter(bankList);
        while (ertBank = iter()) {
	  ertBank->printHeader();
	  
	  ertBank->print();
	  ertdbdecode = (PdbErtDecode*)&(ertBank->getEntry(0));
	  
            //... 4x4A ..
          for(int k = 0; k<2; k++)
             for(int j = 0; j<4; j++) {
	        cout<<ertdbdecode->get_word4x4ACoord_ROC(j, k)<<" "<<ertdbdecode->get_word4x4ACoord_WORD(j, k)<<" ";
             }

          cout<<endl;
            //... 4x4B ..
          for(int k = 0; k<2; k++)
             for(int j = 0; j<4; j++) {
	        cout<<ertdbdecode->get_word4x4BCoord_ROC(j, k)<<" "<<ertdbdecode->get_word4x4BCoord_WORD(j, k)<<" ";
             }

          cout<<endl;
            //... 4x4C ..
          for(int k = 0; k<2; k++)
             for(int j = 0; j<4; j++) {
	        cout<<ertdbdecode->get_word4x4CCoord_ROC(j, k)<<" "<<ertdbdecode->get_word4x4CCoord_WORD(j, k)<<" ";
             }
          cout<<endl;
            //..  2x2 ....
          for(int k = 0; k<2; k++)
             for(int j = 0; j<4; j++) {
	        cout<<ertdbdecode->get_word2x2Coord_ROC(j, k)<<" "<<ertdbdecode->get_word2x2Coord_WORD(j, k)<<" ";
             }

          cout<<endl;
           //..  RICH ....
          for(int k = 0; k<2; k++)
             for(int j = 0; j<4; j++) {
	        cout<<ertdbdecode->get_word4x5Coord_ROC(j, k)<<" "<<ertdbdecode->get_word4x5Coord_WORD(j, k)<<" ";
             }

          cout<<endl;
          for(int i = 0; i<16; i++) {
            cout<<ertdbdecode->get_SMCoordModeAPBGL(i)<<" ";
          }
          cout<<endl;
          for(int i = 0; i<16; i++) {
            cout<<ertdbdecode->get_SMCoordModeBPBGL(i)<<" ";
          }
          cout<<endl;
          for(int i = 0; i<16; i++) {
            cout<<ertdbdecode->get_SMCoordModeAPBSC(i)<<" ";
          }
          cout<<endl;
          for(int i = 0; i<16; i++) {
            cout<<ertdbdecode->get_SMCoordModeBPBSC(i)<<" ";
          }
          cout<<endl;
          for(int i = 0; i<16; i++) {
            cout<<ertdbdecode->get_SMCoordModeARICH(i)<<" ";
          }
          cout<<endl;
          for(int i = 0; i<16; i++) {
            cout<<ertdbdecode->get_SMCoordModeBRICH(i)<<" ";
          }

        }

	if (ertBank) {
	  cout << "Information directly from the header:" << endl;
	  cout << "startTime = " << ertBank->getStartValTime() << endl;
	  cout << "endTime   = " << ertBank->getEndValTime() << endl;
	  cout << "bankID    = " << ertBank->getBankID().getInternalValue() << endl;
	  cout << "---------------------------------------------" << endl;
	
	  ertBank->print();
	  ertBank->printEntry(0);	 
	}
	else {
	  cout << "main()" << endl;
	  cout << "\tError:" << endl;
	  cout << "\tbankManager returned zero-pointer" << endl;
	  } 
  
	application->abort();
   }
   else {
	cout << "failed to start application for update" << endl;
   }
  

   return 0;
}
