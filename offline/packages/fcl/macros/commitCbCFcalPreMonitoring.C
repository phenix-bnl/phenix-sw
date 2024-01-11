#ifndef __CINT__
#include<iostream>
#include <PdbBankManagerFactory.hh>
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

// This macro reads the output histograms fcalMonAnal.C
// including the pre ADC maxbin, bottom 1% and top 1% parameters and commits them to the database.

#define NCOLS 9
#define NROWS 10
#define NDET 2

TH2* hPreMax[NDET];
TH2* hPreBottom1[NDET];
TH2* hPreTop1[NDET];

void commitCbCFcalPreMonitoring(const char* filename = "FcalPreAnalysis_18Feb2005.hist.root") {

  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");

  TFile f(filename);

  char* compass[] = {"north","south"};
  
  ostringstream tmp;
  for(int idet = 0; idet < NDET; idet++) {
    tmp << "fcal_" << compass[idet] << "_pre_TOP1_summary" << ends;
    hPreTop1[idet] = (TH2*)(f.Get(tmp.str().c_str()));
    tmp.str("");
    tmp << "fcal_" << compass[idet] << "_pre_BOTTOM1_summary" << ends;
    hPreBottom1[idet] = (TH2*)(f.Get(tmp.str().c_str()));
    tmp.str("");
    tmp << "fcal_" << compass[idet] << "_pre_MAX_summary" << ends;
    hPreMax[idet] = (TH2*)(f.Get(tmp.str().c_str()));
    tmp.str("");
    if(!(hPreMax[idet]&&hPreBottom1[idet]&&hPreTop1[idet]))
      {
	cout << "No Input histograms found!\n";
	return;
      }
  }

  float max, max_err; 
  float top1, top1_err;
  float bottom1, bottom1_err;
 
  // We'll create 3 entries in "calib.fcl.premonitor"
  // The table entries will contain the maxbin values of the pre per channel (and errors)
  // the bottom 1% and the top 1% of the pre per channel (and errors)

  PdbBankManager *bankManager = PdbBankManagerFactory::instance().create("Pg");
  PdbApplication *application = bankManager->getApplication();
  if(application->startUpdate()){
    PdbBankID bankID("");
    bankID.setInternalValue(1);
    PHString username("jlklay");
    PHTimeStamp tStart = PHTimeStamp(2005,2,18,0,0,0);
    PHTimeStamp tStop;
    tStop.setToFarFuture();

    PHString bankname = "calib.fcl.premonitor";
    PHString bankdesc = "Fcal Pre Monitoring Parameters";
    PHString bankclass = "PdbFclGainBank";
    PdbCalBank *fclBank = bankManager->createBank(bankclass.getString(),bankID,bankdesc.getString(),tStart,tStop,bankname.getString());

    fclBank->setUserName(username);
    fclBank->setLength(3);
    fclBank->print();
    PdbFclGain *pdb_max = (PdbFclGain*)&(fclBank->getEntry(0));
    PdbFclGain *pdb_bot = (PdbFclGain*)&(fclBank->getEntry(1));
    PdbFclGain *pdb_top = (PdbFclGain*)&(fclBank->getEntry(2));


    for(int idet = 0; idet < NDET; idet++) {
      for(int icol = 0; icol < NCOLS; icol++) {
        for(int irow = 0; irow < NROWS; irow++) {
	  max = hPreMax[idet]->GetBinContent(hPreMax[idet]->FindBin(icol,irow));
	  max_err = hPreMax[idet]->GetBinError(hPreMax[idet]->FindBin(icol,irow));

	  top1 = hPreTop1[idet]->GetBinContent(hPreTop1[idet]->FindBin(icol,irow));
	  top1_err = hPreTop1[idet]->GetBinError(hPreTop1[idet]->FindBin(icol,irow));

	  bottom1 = hPreBottom1[idet]->GetBinContent(hPreBottom1[idet]->FindBin(icol,irow));
	  bottom1_err = hPreBottom1[idet]->GetBinError(hPreBottom1[idet]->FindBin(icol,irow));
	  printf("%d %d %f %f %f %f %f %f\n",icol, irow, max, max_err, bottom1, bottom1_err, top1, top1_err);

	  if(idet == 1) {
	    pdb_max->setSouthGain(irow,icol,max,max_err);
	    pdb_bot->setSouthGain(irow,icol,bottom1,bottom1_err);
	    pdb_top->setSouthGain(irow,icol,top1,top1_err);
	  }
	  if(idet == 0) {
	    pdb_max->setNorthGain(irow,icol,max,max_err);
	    pdb_bot->setNorthGain(irow,icol,bottom1,bottom1_err);
	    pdb_top->setNorthGain(irow,icol,top1,top1_err);
	  }	  
	} 
      }
    }
    pdb_max->print();
    pdb_bot->print();
    pdb_top->print();
    //application->abort();
    //application->commit(fclBank);
  }
}
