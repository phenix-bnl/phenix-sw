#ifndef __CINT__
#include<iostream>
#include <PdbBankManagerFactory.hh>
//#include <PHPointerList.h>
#include <PdbApplication.hh>
#include <PHTimeStamp.h>
#include <PdbCalBank.hh>
#include <PdbBankList.hh>
#include <PdbFclGain.hh>
#include <FclConsts.h>
#include <TNtuple.h>
#include <TFile.h>
#include <TH2.h>
#include <stdio.h>
#endif

// This macro reads the output histogram of extractCbCZdcXtalkProd.C
// including the Zdc-Fcl Xtalk fit paramters and commits them to the database.
void commitCbCCosmicProd(const char* fname, int run1, int run2)
{
  // Macro to commit cosmic calibration constants to the database
  // intput root file should be the dumphistos from the FCAL OnCal
  // module...

  const int ncols = 9;
  const int nrows = 10;

  //  gSystem->Load("libPgCal.so");

  TFile tf(fname);

  // We'll create a bank named "calib.fcl.cosmic"

  PdbBankManager *bankManager = PdbBankManagerFactory::instance().create("Pg");
  PdbApplication *application = bankManager->getApplication();
  if(application->startUpdate()){
    PdbBankID bankID("");
    bankID.setInternalValue(0);
    PHString username("rjnewby");
    PHTimeStamp tStart;
    tStart.setTics(0);
    PHTimeStamp tStop;
    tStop.setToFarFuture();
    PHString bankname = "calib.fcl.cosmic";
    PHString bankdesc = "Cosmic Reconstruction Calibration";
    PHString bankclass = "PdbFclGainBank";

    PHTimeStamp tlh;
    tlh.setTics(74569);

    PdbCalBank *fclLowHighBank = bankManager->fetchBank("PdbFclGainBank",bankID,"calib.fcl.lowhigh",tlh);
    PdbFclGain* fclLowHigh = (PdbFclGain*)&(fclLowHighBank->getEntry(0));


    PdbCalBank *fclBank = bankManager->createBank(run1,run2,bankclass.getString(),bankID,bankdesc.getString(),bankname.getString());
    fclBank->setUserName(username);

//PdbCalBank *fclBank = bankManager->createBank(bankclass.getString(),bankID,bankdesc.getString(),tStart,tStop,bankname.getString());

    fclBank->setLength(1);
    fclBank->print();
    PdbFclGain *pdb_cosmic = (PdbFclGain*)&(fclBank->getEntry(0));
    char buff[255];

    float mean, gain, rms, nentries, lowhigh, error;

    for(int whichSide = 0; whichSide < 2; whichSide++)
    for(int icol = 0; icol < ncols; icol++)
      for(int irow = 0; irow < nrows; irow++)
      {
	sprintf(buff,"FCAL/High_Gain_%d_%d_%d",whichSide,icol,irow);
	TH1* thi = (TH1*)(tf.Get(buff));
	if(!thi)
	{
	  application->abort();
	  return;
	}
	
	mean = thi->GetMean();
	rms = thi->GetRMS();
	nentries = thi->Integral();
	lowhigh = (whichSide==FCALSOUTH)? fclLowHigh->getSouthGain(irow,icol):fclLowHigh->getNorthGain(irow,icol);
	gain = mean*lowhigh*10.0; // 10.0 -> 0.1GeV/cosmic 
	error = rms/sqrt(nentries);
	if(whichSide==FCALNORTH)
	{
	  pdb_cosmic->setNorthGain(irow,icol,gain,error);
	}else{
	  pdb_cosmic->setSouthGain(irow,icol,gain,error);
	}
	printf("%d %d %d %f %f %f %f\n",whichSide,icol, irow, gain, mean, lowhigh, error);
      }
    pdb_cosmic->print();
    //application->abort();
    application->commit(fclBank);
  }


}
