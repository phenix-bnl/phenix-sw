#ifndef __CINT__
#include<iostream>
#include <PdbBankManagerFactory.hh>
//#include <PHPointerList.h>
#include <PdbApplication.hh>
#include <PHTimeStamp.h>
#include <PdbCalBank.hh>
#include <PdbBankList.hh>
#include <PdbFclGain.hh>
#include <TNtuple.h>
#include <TFile.h>
#include <TH2.h>
#include <stdio.h>
#endif

// This macro reads the output histogram of extractCbCZdcXtalkProd.C
// including the Zdc-Fcl Xtalk fit paramters and commits them to the database.
void commitCbCZdcXtalkProd(const char* fname, int run1, int run2)
{

  const int ncols = 9;
  const int nrows = 10;

  //  gSystem->Load("libPgCal.so");

  TFile tf(fname);
  TH2* hSlopes = (TH2*)(tf.Get("hSlopes2"));
  TH2* hIntercepts = (TH2*)(tf.Get("hIntercepts2"));
  if(!(hSlopes&&hIntercepts))
    {
      cout<<"No Input histograms found!\n";
      return;
    }
  double slope, slope_err, intercept, intercept_err; 
  
  // We'll create a bank described by "calib.fcl.zdcxtalk"
  // The first entry will contain the intercept (and errors)
  // The second entry will contain the slopes (and errors)

  PdbBankManager *bankManager = PdbBankManagerFactory::instance().create("Pg");
  PdbApplication *application = bankManager->getApplication();
  if(application->startUpdate()){
    PdbBankID bankID("");
    bankID.setInternalValue(1);
    PHTimeStamp tStart;
    tStart.setTics(0);
    PHTimeStamp tStop;
    tStop.setToFarFuture();
    PHString bankname = "calib.fcl.zdcxtalk";
    PHString bankdesc = "Fcal ZdcXtalk Parameters";
    PHString bankclass = "PdbFclGainBank";
    PdbCalBank *fclBank = bankManager->createBank(run1,run2,bankclass.getString(),bankID,bankdesc.getString(),bankname.getString());
    //PdbCalBank *fclBank = bankManager->createBank(bankclass.getString(),bankID,bankdesc.getString(),tStart,tStop,bankname.getString());

    fclBank->setLength(2);
    fclBank->print();
    PdbFclGain *pdb_intercepts = (PdbFclGain*)&(fclBank->getEntry(0));
    PdbFclGain *pdb_slopes = (PdbFclGain*)&(fclBank->getEntry(1));

    for(int icol = 0; icol < ncols; icol++)
      for(int irow = 0; irow < nrows; irow++)
      {
	slope = hSlopes->GetBinContent(hSlopes->FindBin(icol,irow));
	slope_err = hSlopes->GetBinError(hSlopes->FindBin(icol,irow));
	intercept = hIntercepts->GetBinContent(hIntercepts->FindBin(icol,irow));
	intercept_err = hIntercepts->GetBinError(hIntercepts->FindBin(icol,irow));
	printf("%d %d %f %f %f %f\n",icol, irow, slope, slope_err, intercept, intercept_err);
	pdb_intercepts->setSouthGain(irow,icol,intercept,intercept_err);
	pdb_slopes->setSouthGain(irow,icol,slope,slope_err);
      }
    pdb_slopes->print();
    pdb_intercepts->print();
    //application->abort();
    application->commit(fclBank);
  }


}
