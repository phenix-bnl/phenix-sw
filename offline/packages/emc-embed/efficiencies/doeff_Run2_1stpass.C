
//_____________________________________________________________________________
// First iteration over the embed+eval files to determine the set of Hagedorn weights

void doeff_Run2_1stpass(int sequence=644)
{
  gSystem->Load("libemcEmbedRun2.so");
  gSystem->Load("libemcEfficiency.so");

  char listoffiles[200];
  sprintf(listoffiles,"/afs/rhic/phenix/users/enterria/efficiencies/list_%d.txt",sequence);
  ifstream in(listoffiles);

  emcEfficiency eff;
  eff.setVerbose(1);

  char name[200];
  sprintf(name,"/phenix/data06/enterria/efficiency_%d.root",sequence);
  eff.openEfficiencyFile(name);

  TStopwatch timer;
  timer.Start();

  int n=0;
	
  char s[200];
  while ( in.getline(s,199,'\n') )
    {
      eff.openEmbedFile(s);
      eff.run();
      eff.closeEmbedFile();
      ++n;
      if ( n % 10 == 0 ) 
       {
	 cout << "Checkpointing output file " << name << endl;
         eff.writeEfficiencyFile();
	 cout << "Done." << endl;
       }      
    }
  eff.close();

  eff.openEfficiencyFile(name,"UPDATE");
  cout << "Fitting correction" << endl;
  eff.fitFluctuation();
  eff.close();

  timer.Stop();
  timer.Print();
}
 
//_____________________________________________________________________________
// Second iteration over the embed+eval files with the new set of Hagedorn weights

void doeff_Run2_2ndpass(int sequence=644)
{

  char listoffiles[200];
  sprintf(listoffiles,"list_%d.txt",sequence);
  ifstream in(listoffiles);

  emcEfficiency eff;
  eff.setVerbose(1);

  char input[200];
  sprintf(input,"/common/b2/phoncs/enterria/efficiencies/efficiency_%d.root",sequence);

  TFile f(input);
  TH1* h1 = (TH1*)(f.Get("hCorrectionFunction"));
  h1->SetDirectory(0);
  eff.setCorrectionFunction(h1); 
  f.Close();

  char name[200];
  sprintf(name,"/common/b2/phoncs/enterria/efficiencies/efficiency_2nd_%d.root",sequence);
  eff.openEfficiencyFile(name);

  TStopwatch timer;
  timer.Start();

  char s[200];
  while ( in.getline(s,199,'\n') )
    {
      eff.openEmbedFile(s);
      eff.run();
      eff.closeEmbedFile();      
    }
  eff.close();

  timer.Stop();
  timer.Print();
}
 
