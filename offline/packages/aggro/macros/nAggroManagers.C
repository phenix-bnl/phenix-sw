// This macro handle setting all of the output nodes for the various types 
// of output files.
// First version to run with CRS production
// MAR 03/13/03
// JUL 07/12/03 : MWG added by Frederic and DongJo 
// JUL 07/13/03 : muon->SetDestinationFile is put into IOManager.C  by Frederic and DongJo
// AUG 08/01/03 : Trigger dicing Dimitri & DongJo

#include <stdio.h> 
#include <time.h> 

void CNT_IOManager(const char *cntfile = "CNT_nanoDST.root")
{
  
  cout << "Opening output aggregate file named: " << cntfile << endl;

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllIOManager *cntManager  = new Fun4AllIOManager("CNT",  cntfile);

  cntManager->AddNode("PHCentralTrack");
  cntManager->AddNode("PHGlobal");
  cntManager->AddNode("TrigLvl1");
  cntManager->AddNode("ErtOut");
  cntManager->AddNode("EventHeader");
  cntManager->AddNode("Sync");

  se->registerIOManager(cntManager);
}


void EWG_IOManager(const char *ewgfile = "EWG_nanoDST.root" )
{
  
  cout << "Opening output aggregate file named: " << ewgfile << endl;

  Fun4AllServer *se = Fun4AllServer::instance();  
  Fun4AllIOManager *ewgManager  = new Fun4AllIOManager("EWG",  ewgfile);

  ewgManager->AddNode("EWGCentralTrack");
  ewgManager->AddNode("PHGlobal");
  ewgManager->AddNode("TrigLvl1");
  ewgManager->AddNode("ErtOut");
  ewgManager->AddNode("EventHeader");
  ewgManager->AddNode("Sync");

  se->registerIOManager(ewgManager);
}

void MWG_IOManager(const char *mwgfile = "MWG_nanoDST.root")
{

  cout << "Opening output aggregate file named: " << mwgfile << endl;

  Fun4AllServer *se = Fun4AllServer::instance();  
  Fun4AllIOManager *mwgManager  = new Fun4AllIOManager("MWG",  mwgfile);

  mwgManager->AddNode("dMuiPseudoTriggerOut");
  mwgManager->AddNode("PHGlobal");
  mwgManager->AddNode("PHMuoTracks");
  mwgManager->AddNode("PHMuoTracksOO");
  mwgManager->AddNode("Sync");
  mwgManager->AddNode("TrigLvl1");

  se->registerIOManager(mwgManager);
}


void HWG_IOManager(const char *hwgfile = "HWG_nanoDST.root")
{
  
  cout << "Opening output aggregate file named: " << hwgfile << endl;

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllIOManager *hwgManager  = new Fun4AllIOManager("HWG",  hwgfile);

  hwgManager->AddNode("HWGCentralTrack");
  hwgManager->AddNode("PHGlobal");
  hwgManager->AddNode("TrigLvl1");
  hwgManager->AddNode("ErtOut");
  hwgManager->AddNode("EventHeader");
  hwgManager->AddNode("Sync");

  se->registerIOManager(hwgManager);
}


void PWG_IOManager(const char *pwgfile = "PWG_nanoDST.root")
{

  cout << "Opening output aggregate file named: " << pwgfile << endl;
  
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllIOManager *pwgManager  = new Fun4AllIOManager("PWG_",  pwgfile);

  pwgManager->AddEventSelector("");
  pwgManager->AddNode("emcClusterContainer");
  pwgManager->AddNode("PHGlobal");
  pwgManager->AddNode("TrigLvl1");
  pwgManager->AddNode("ErtOut");
  pwgManager->AddNode("EventHeader");
  pwgManager->AddNode("Sync");


  se->registerIOManager(pwgManager);
}


int setProcObject()
{
  const char *cvstag             = gSystem->Getenv("CVSTAG");
  const char *desc               = gSystem->Getenv("DESCRIPTION");
  const char *inFile             = gSystem->Getenv("PRDFNAME");
  const char *dst_File           = gSystem->Getenv("DST_NAME");
  const char *udst_File          = gSystem->Getenv("UDST_NAME");
  const char *cnt_minbias_File   = gSystem->Getenv("CNT_MINBIAS_NAME");
  const char *cnt_muon_File      = gSystem->Getenv("CNT_MUON_NAME");
  const char *cnt_electron_File  = gSystem->Getenv("CNT_ELECTRON_NAME");
  const char *cnt_photon_File    = gSystem->Getenv("CNT_PHOTON_NAME");
  const char *ewg_minbias_File   = gSystem->Getenv("EWG_MINBIAS_NAME");
  const char *ewg_muon_File      = gSystem->Getenv("EWG_MUON_NAME");
  const char *ewg_electron_File  = gSystem->Getenv("EWG_ELECTRON_NAME");
  const char *ewg_photon_File    = gSystem->Getenv("EWG_PHOTON_NAME");
  const char *mwg_minbias_File   = gSystem->Getenv("MWG_MINBIAS_NAME");
  const char *mwg_muon_File      = gSystem->Getenv("MWG_MUON_NAME");
  const char *mwg_electron_File  = gSystem->Getenv("MWG_ELECTRON_NAME");
  const char *mwg_photon_File    = gSystem->Getenv("MWG_PHOTON_NAME");
  const char *hwg_minbias_File   = gSystem->Getenv("HWG_MINBIAS_NAME");
  const char *hwg_muon_File      = gSystem->Getenv("HWG_MUON_NAME");
  const char *hwg_electron_File  = gSystem->Getenv("HWG_ELECTRON_NAME");
  const char *hwg_photon_File    = gSystem->Getenv("HWG_PHOTON_NAME");
  const char *pwg_minbias_File   = gSystem->Getenv("PWG_MINBIAS_NAME");
  const char *pwg_muon_File      = gSystem->Getenv("PWG_MUON_NAME");
  const char *pwg_electron_File  = gSystem->Getenv("PWG_ELECTRON_NAME");
  const char *pwg_photon_File    = gSystem->Getenv("PWG_PHOTON_NAME");

  // need add after DST Slicing
  if (cvstag && desc && inFile &&  
      dst_File &&  udst_File &&
      cnt_minbias_File && cnt_electron_File && cnt_muon_File && cnt_photon_File &&
      ewg_minbias_File && ewg_electron_File && ewg_muon_File && ewg_photon_File &&
      mwg_minbias_File && mwg_electron_File && mwg_muon_File && mwg_photon_File &&
      hwg_minbias_File && hwg_electron_File && hwg_muon_File && hwg_photon_File &&
      pwg_minbias_File && pwg_electron_File && pwg_muon_File && pwg_photon_File)
    {
      FILE *f;
      char catfilename[100]; 
      sprintf(catfilename,"%s.txt",inFile);
      cout << "Saving process object: "<< catfilename << endl;
      cout << "cvstag      : " << cvstag   << endl;
      cout << "description : " << desc     << endl;
      cout << "input file  : " << inFile   << endl;
      cout << "dst file : " << dst_File  << endl;
      cout << "udst file : " << udst_File  << endl;
      cout << "cnt_minbias file : " << cnt_minbias_File  << endl;
      cout << "cnt_electron file : " << cnt_electron_File  << endl;
      cout << "cnt_muon file : " << cnt_muon_File  << endl;
      cout << "cnt_photon file : " << cnt_photon_File  << endl;
      cout << "ewg_minbias file : " << ewg_minbias_File  << endl;
      cout << "ewg_electron file : " << ewg_electron_File  << endl;
      cout << "ewg_muon file : " << ewg_muon_File  << endl;
      cout << "ewg_photon file : " << ewg_photon_File  << endl;
      cout << "mwg_minbias file : " << mwg_minbias_File  << endl;
      cout << "mwg_electron file : " << mwg_electron_File  << endl;
      cout << "mwg_muon file : " << mwg_muon_File  << endl;
      cout << "mwg_photon file : " << mwg_photon_File  << endl;
      cout << "hwg_minbias file : " << hwg_minbias_File  << endl;
      cout << "hwg_electron file : " << hwg_electron_File  << endl;
      cout << "hwg_muon file : " << hwg_muon_File  << endl;
      cout << "hwg_photon file : " << hwg_photon_File  << endl;
      cout << "pwg_minbias file : " << pwg_minbias_File  << endl;
      cout << "pwg_electron file : " << pwg_electron_File  << endl;
      cout << "pwg_muon file : " << pwg_muon_File  << endl;
      cout << "pwg_photon file : " << pwg_photon_File  << endl;


      time_t rawtime; 
      struct tm * timeinfo; 
      time ( &rawtime ); 
      timeinfo = localtime( &rawtime ); 

      f = fopen(catfilename,"a");
      fprintf(f,"%s",asctime(timeinfo) ); 
      fprintf(f,"%s\n",cvstag);
      fprintf(f,"%s\n",desc);
      fprintf(f,"%s\n",inFile);
      fprintf(f,"%s\n",dst_File);
      fprintf(f,"%s\n",udst_File);
      fprintf(f,"%s\n",cnt_minbias_File);
      fprintf(f,"%s\n",cnt_electron_File);
      fprintf(f,"%s\n",cnt_muon_File);
      fprintf(f,"%s\n",cnt_photon_File);
      fprintf(f,"%s\n",ewg_minbias_File);
      fprintf(f,"%s\n",ewg_electron_File);
      fprintf(f,"%s\n",ewg_muon_File);
      fprintf(f,"%s\n",ewg_photon_File);
      fprintf(f,"%s\n",mwg_minbias_File);
      fprintf(f,"%s\n",mwg_electron_File);
      fprintf(f,"%s\n",mwg_muon_File);
      fprintf(f,"%s\n",mwg_photon_File);
      fprintf(f,"%s\n",hwg_minbias_File);
      fprintf(f,"%s\n",hwg_electron_File);
      fprintf(f,"%s\n",hwg_muon_File);
      fprintf(f,"%s\n",hwg_photon_File); 
      fprintf(f,"%s\n",pwg_minbias_File);
      fprintf(f,"%s\n",pwg_electron_File);
      fprintf(f,"%s\n",pwg_muon_File);
      fprintf(f,"%s\n",pwg_photon_File); 

      fclose(f);
      return 0;
    }
  return -1;
}

