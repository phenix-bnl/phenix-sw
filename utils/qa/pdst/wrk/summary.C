void summary(const char *histIFile = "qaout.root",
	     const char *tag = "",
             const char *outfilename = "dstqasummary.txt",
             const char *statusfilename = "dstqastatus.txt",
             Int_t runNumber=1234)
{
  gSystem->Load("libTHmul.so");
  gSystem->Load("libqasummary.so");

  cout << "library loaded." << endl;
                                                                                                                              
  QASummary* qa = new QASummary();
  qa->setInputFileName(histIFile);
  qa->setOutputTextFileName(outfilename);
  qa->setOutputStatusFileName(statusfilename);
  qa->setRunNumber(runNumber);
  qa->setCommitToDatabase(1);
  qa->setTagEntry(tag);
  qa->setSegmentNumber(-1);   // -1 means all segments of a run number

  qa->Init();
  //  qa->processTec();
  qa->processZdc();
  qa->processBbc();
  qa->processMut();
  qa->processMui();
  qa->processPad();
  qa->processDch();
  qa->processEmc();
  //  qa->processFcl();
  //  qa->processNtcp();
  qa->processTof();
  //  qa->processMvd();
  qa->processCrk();
  
  qa->processElectron();
  //  qa->processElectron(0.2,0.4); // different momentum ranges
  //  qa->processElectron(0.4,0.8);
  //  qa->processElectron(0.8,4.8);
  qa->processErt();
  qa->processPi0();
  qa->End();
  
  // if everything went OK then we move the summary and status files to their definitely place
  TString final_summary_file(outfilename);
  final_summary_file.ReplaceAll("tmp/","");
  gSystem->Exec(Form("mv %s %s",outfilename,final_summary_file.Data()));

  TString final_status_file(statusfilename);
  final_status_file.ReplaceAll("tmp/","");
  gSystem->Exec(Form("mv %s %s",statusfilename,final_status_file.Data()));
}
