
void QAInit()
{
  gSystem->Load("libTHmul.so");
  gSystem->Load("libpi0qa.so");

  SubsysReco* qabbc = new QABbc();
  SubsysReco* qazdc = new QAZdc();
  SubsysReco* qapad = new QAPad();
  SubsysReco* qacrk = new QACrk();
  SubsysReco* qatec = new QATec();
  SubsysReco* qaemc = new QAEmc();
  SubsysReco* qadch = new QADch();
  SubsysReco* qatof = new QATof();
  SubsysReco* qaelectron = new QAElectron();
  SubsysReco* qaert = new QAErt();
  SubsysReco* qamutr = new QAMut();
  SubsysReco* qamuid = new QAMui();
	//   SubsysReco* qatzr = new QATzr();
	//   SubsysReco* qafcl = new QAFcl();

//  SubsysReco* qapi0 = new QA_pi0();
//	SubsysReco* qaspin = new QASpin();

  Fun4AllServer *se = Fun4AllServer::instance();
  se->registerSubsystem(qabbc);
  se->registerSubsystem(qazdc);
  se->registerSubsystem(qapad);
  se->registerSubsystem(qacrk);
  se->registerSubsystem(qatec);
  se->registerSubsystem(qaemc);
  se->registerSubsystem(qadch);
  se->registerSubsystem(qatof);
  se->registerSubsystem(qaelectron);
  se->registerSubsystem(qaert);
  se->registerSubsystem(qamutr);
  se->registerSubsystem(qamuid);
	//   se->registerSubsystem(qatzr);
	//   se->registerSubsystem(qafcl);
	
//  se->registerSubsystem(qapi0);
//	se->registerSubsystem(qaspin);
}

void QAEnd(const char* QAOUT="qaOut.root")
{
	// Here all QA histograms are picked up from subdirectories to qaout file
	TDirectory *tmpdir = gDirectory;
	TFile* qaout = TFile::Open(QAOUT, "RECREATE");
	TIterator *titer = tmpdir->GetList()->MakeIterator();
	while ((obj = titer->Next()))
		{
			TString objname = obj->GetName();
			if (objname.Contains("QA"))     // look for subdirectories starting with QA 
				{
					TDirectory *tmpsubdir = (TDirectory*)obj;
					TIterator *tsubiter = tmpsubdir->GetList()->MakeIterator();
					TObject *obj2;
					while ((obj2 = tsubiter->Next()))
						obj2->Write();
				}
		}
	qaout->Close();
}
