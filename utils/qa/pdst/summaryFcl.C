#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "summaryQA.h"
#include "FclIndexer.h"
#include <fstream>
#include <iostream>

#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PdbParameterError.hh"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"

#include "QaBankID.h"  // enumerates unique bank ID's

using namespace std;
int QASummary::processFcl()
{
  cout << "Fcl..." << endl;
fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }

  int fclstats=0;
  FclIndexer* indexer = FclIndexer::Instance();
  
  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- FCL QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  
  Int_t fclstatus = 0;

int numSouthDead = 0;
int numNorthDead = 0;

  TH2F *fclADCNorth = (TH2F *) qafile->Get ("fclADCNorth");  
  TH2F *fclADCSouth = (TH2F *) qafile->Get ("fclADCSouth");
  TProfile *fclADCNorthP = (TProfile *) qafile->Get ("fclADCNorthP");  
  TProfile *fclADCSouthP = (TProfile *) qafile->Get ("fclADCSouthP");


  // check that the histograms exist
  if (fclADCNorth == NULL || fclADCSouth == NULL ||
      fclADCNorthP == NULL || fclADCSouthP == NULL)
  {
    textFile << " FCL ERROR: could not extract histograms" << endl;
    fclstatus +=1;
  }
  else
  {

    for (int iChan = 0; iChan<144; iChan++){
      if (indexer->getColumnSouth(iChan)>0 &&
	  indexer->getColumnSouth(iChan)<8){ // skip the last couple columns on the south
	if (fclADCSouthP->GetBinContent(iChan)-
	    fclADCSouthP->GetBinError(iChan) < 0.0) numSouthDead++;
      }
	
      if (indexer->getColumnNorth(iChan)>0 &&
	  indexer->getColumnNorth(iChan)<7){ // skip the last few columns on the north
	if (fclADCNorthP->GetBinContent(iChan)-
	    fclADCNorthP->GetBinError(iChan) < 0.0) numNorthDead++;
      }
    }
    
    
    textFile << "FCAL Low ADC North Channels: " << numNorthDead << endl;
    textFile << "FCAL Low ADC South Channels: " << numSouthDead << endl;
    textFile << "More FCAL Summary stuff here:" << endl;

    if (numNorthDead>15) fclstats +=2;
    if (numNorthDead>15) fclstats +=4;

  }
textFile << "FCAL status:" << fclstatus << endl;
statusFile << fclstatus << " " ;

 if (CommitToDatabase==false)
   {
     cout << "    ...done." << endl;
     return 0;
   }

  PdbParameterError *achan=0;
  int Debug = 0;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *summaryBank = 0;
  
  if(Debug>0) cout << "QA Fcal::Update summary: opening FD in update mode..." << endl;

  if (application->startUpdate())
    {
      PdbBankID bankID;
      const char *calibname = "qasummary";
      const char *descrip   = "FCal";
      int bankid = QaBankID::FCal;  // unique ID

      if(Debug>0) {
	cout << "QAFCal:: summary database:: calibname = " << calibname << endl;
	cout << "QAFCal:: summary database:: bankid = " << bankid << endl;
      }

      bankID.setInternalValue(bankid);

      summaryBank =
	bankManager->createBank(runNumber,"PdbParameterErrorBank", bankID, descrip, calibname);
 
      int totentr = 2;
      summaryBank->setLength(totentr);

      if(Debug>1) summaryBank->print();
      if(Debug>1) summaryBank->printHeader();

      achan = (PdbParameterError*)&(summaryBank->getEntry(0));
      achan->setParameter((float)numNorthDead);
      achan->setParameterError(0.0);
      achan->setName("Low ADC North Channels");

      achan = (PdbParameterError*)&(summaryBank->getEntry(1));
      achan->setParameter((float)numSouthDead);
      achan->setParameterError(0.0);
      achan->setName("Low ADC South Channels");
      application->commit(summaryBank);
    }
  else {
    cerr << "QA FCal::Update summary ERROR: failed to start application for update" << endl;
    return 0;
  }
  cout << "    ...done." << endl;
  return 0;
}
