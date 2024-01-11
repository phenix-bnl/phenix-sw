void extractFclGreySPercentiles(const char *fname = "FclSummary_ana70076.root",const char *hname = "hFclGreyS")
{
  //This macro reads a 1D histo of the FclGreyS distribution and
  //extracts the fraction of the distribution above the low edge of
  //the bin.
  // The first two entries are the lowedge of the largest Fcl energy
  // bin and the binwidth.  The following entries are the values
  // fraction of the distribution above the lowbinedge.

  TFile infile(fname);
  TH1 *hin = (TH1*)(infile.Get(hname));

  hin->Sumw2();
  hin->Scale(1.0/hin->Integral("width"));
  double hpercent;
  cout<<"Nbins: "<<hin->GetNbinsX()<<endl;

  int ibin = hin->GetNbinsX();


  gSystem->Load("libPgCal.so");
  PdbBankManager *bankManager = PdbBankManagerFactory::instance().create("Pg");
  PdbApplication *application = bankManager->getApplication();
  if(application->startUpdate()){
    PdbBankID bankID("");
    bankID.setInternalValue(0);
    PHTimeStamp tStart;
    tStart.setTics(0);
    PHTimeStamp tStop;
    tStop.setToFarFuture();
    PHString bankname = "calib.fcl.centrality";
    PHString bankdesc = "Fcal dAu Centrality Percentiles";
    PHString bankclass = "PdbDoubleBank";
    PdbCalBank *fclBank = bankManager->createBank(bankclass.getString(),bankID,bankdesc.getString(),tStart,tStop,bankname.getString()); 
    
    fclBank->setLength(ibin+2);
    fclBank->print();
    


  double lowedge = hin->GetBinLowEdge(ibin);
  double binwidth = lowedge - hin->GetBinLowEdge(ibin-1);
  cout<<lowedge<<endl;
  cout<<binwidth<<endl;

  //Set the first two entries
  int calentry = 0;
  ((PdbDouble*)&(fclBank->getEntry(calentry++)))->setValue(lowedge);
  ((PdbDouble*)&(fclBank->getEntry(calentry++)))->setValue(binwidth);
  while(ibin>0)
  {
    hpercent = hin->Integral(ibin,hin->GetNbinsX(),"width");
    printf("%d %f %f\n",ibin, hin->GetBinLowEdge(ibin),hpercent*88.5);
    ((PdbDouble*)&(fclBank->getEntry(calentry++)))->setValue(hpercent*88.5);
    ibin--;
  }
  fclBank->print();
  //application->abort();
  application->commit(fclBank);
  }
  return;
}
