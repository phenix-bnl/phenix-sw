{
textFile << " ----------------------------------------------------" << endl;
textFile << " -- PAD QA Summary --" << endl;
textFile << " ----------------------------------------------------" << endl;
    
textFile << endl << "*** Detailed HV info ***" << endl;

  pc1EastActivity = (TProfile *) qafile->Get ("pc1EastActivity");
  pc1WestActivity = (TProfile *) qafile->Get ("pc1WestActivity");
  pc1EastRatio = (TH1F *) qafile->Get ("pc1EastRatio");
  pc1WestRatio = (TH1F *) qafile->Get ("pc1WestRatio");

  pc2WestActivity = (TProfile *) qafile->Get ("pc2WestActivity");
  pc2WestRatio = (TH1F *) qafile->Get ("pc2WestRatio");

  pc3EastActivity = (TProfile *) qafile->Get ("pc3EastActivity");
  pc3WestActivity = (TProfile *) qafile->Get ("pc3WestActivity");
  pc3EastRatio = (TH1F *) qafile->Get ("pc3EastRatio");
  pc3WestRatio = (TH1F *) qafile->Get ("pc3WestRatio");
  // add here analysis and printout results to textFile
  
  Float_t padxn[32];
  Float_t ratio;
  Int_t pc,arm;
  Int_t pc1eastactstat = 0, pc3eastactstat = 0;
  Int_t pc1westactstat = 0, pc2westactstat = 0, pc3westactstat = 0;
  Int_t pc1eastratiostat = 0, pc3eastratiostat = 0;
  Int_t pc1westratiostat = 0, pc2westratiostat = 0, pc3westratiostat = 0;
  Int_t pc1eastlow = 0, pc3eastlow = 0;
  Int_t pc1westlow = 0, pc2westlow = 0, pc3westlow = 0;
  Float_t activitycut = 0.01;	   // min clusters/event/wire (default gold)
  Float_t gold_activitycut = 0.01; // min clusters/event/wire (gold)
  Float_t pp_activitycut = 0.001;   // min clusters/event/wire (pp)
  Int_t maximumlow = 1;	           // 1 bad is still almost ok
  Int_t runchange = 35000;
  Int_t ratiocut_semibad = 1;
  Float_t ratiocut_realbad = 0.5;

Int_t PC1E_bad_low=32010, PC1E_bad_high=32441;
Int_t PC2W_bad_low=33708, PC2W_bad_high=35000;

  if(runNumber<=runchange) {
    activitycut = gold_activitycut;  //gold data
    // cout << "AuAu ";
  }
  if(runNumber>runchange) {
    activitycut = pp_activitycut; //pp data
    // cout << "pp ";
  }

// cout << activitycut <<endl;

  // PC1 E
  ratio = 0;
  pc = 0; // PC1
  arm = 0; // East arm
  textFile << pc << " " << arm << endl;
  for (Int_t ic = 0; ic < 32; ic++)
    {
      padxn[ic] = pc1EastActivity->GetBinContent (ic + 1);
      
      if(runNumber>=PC1E_bad_low && runNumber<=PC1E_bad_high){
        padxn[ic]=0;
      }

       if (padxn[ic] < activitycut)
	{
	  // bad HV
	  pc1eastlow++;
	  textFile << "-1 "; // -1 means not ok
	}
      else 
	{
	  textFile << "1 "; // 1 means ok
	}

      ratio += pc1EastRatio->GetBinContent(ic+1);
    }
  textFile << endl;

  ratio /= 32;
  if (ratio <= ratiocut_semibad)
    {
      pc1eastratiostat = 2;
    
      if (ratio <= ratiocut_realbad)
	{
	  pc1eastratiostat = 1;
	}
    }
  else
    {
      pc1eastratiostat = 0;
    }

  // PC1 W
  ratio = 0;
  pc = 0; // PC1
  arm = 1; // West arm
  textFile << pc << " " << arm << endl;
  for (Int_t ic = 0; ic < 32; ic++)
    {
      padxn[ic] = pc1WestActivity->GetBinContent (ic + 1);
      if (padxn[ic] < activitycut)
	{
	  // bad HV
	  pc1westlow++;
	  textFile << "-1 "; // -1 means not ok
	}      
      else 
	{
	  textFile << "1 "; // 1 means ok
	}

      ratio += pc1WestRatio->GetBinContent(ic+1);
    }
  textFile << endl;

  ratio /= 32;
  if (ratio <= ratiocut_semibad)
    {
      pc1westratiostat = 2;
    
      if (ratio <= ratiocut_realbad)
	{
	  pc1westratiostat = 1;
	}
    }
  else
    {
      pc1westratiostat = 0;
    }

  // PC2 W
  ratio = 0;
  pc = 1; // PC2
  arm = 1; // West arm
  textFile << pc << " " << arm << endl;
  for (Int_t ic = 0; ic < 32; ic++)
    {
      padxn[ic] = pc2WestActivity->GetBinContent (ic + 1);
      
      if(runNumber>=PC2W_bad_low &&  runNumber<=PC2W_bad_high && ic>27){
        padxn[ic]=1;
      }
      
      if (padxn[ic] < activitycut)
	{
	  // bad HV
	  pc2westlow++;
	  textFile << "-1 "; // -1 means not ok
	}
      else 
	{
	  textFile << "1 "; // 1 means ok
	}

      ratio += pc2WestRatio->GetBinContent(ic+1);
    }
  textFile << endl;

  ratio /= 32;
  if (ratio <= ratiocut_semibad)
    {
      pc2westratiostat = 2;
    
      if (ratio <= ratiocut_realbad)
	{
	  pc2westratiostat = 1;
	}
    }
  else
    {
      pc2westratiostat = 0;
    }

  // PC3 E
  ratio = 0;
  pc = 2; // PC3
  arm = 0; // East arm
  textFile << pc << " " << arm << endl;
  for (Int_t ic = 0; ic < 32; ic++)
    {
      padxn[ic] = pc3EastActivity->GetBinContent (ic + 1);
      if (padxn[ic] < activitycut)
	{
	  // bad HV
	  pc3eastlow++;
	  textFile << "-1 "; // -1 means not ok
	}
      else 
	{
	  textFile << "1 "; // 1 means ok
	}

      ratio += pc3EastRatio->GetBinContent(ic+1);
    }
  textFile << endl;

  ratio /= 32;
  if (ratio <= ratiocut_semibad)
    {
      pc3eastratiostat = 2;
    
      if (ratio <= ratiocut_realbad)
	{
	  pc3eastratiostat = 1;
	}
    }
  else
    {
      pc3eastratiostat = 0;
    }

  // PC3 W
  ratio = 0;
  pc = 2; // PC3
  arm = 1; // West arm
  textFile << pc << " " << arm << endl;
  for (Int_t ic = 0; ic < 32; ic++)
    {
      padxn[ic] = pc3WestActivity->GetBinContent (ic + 1);
      if (padxn[ic] < activitycut)
	{
	  // bad HV
	  pc3westlow++;
	  textFile << "-1 "; // -1 means not ok
	}
      else 
	{
	  textFile << "1 "; // 1 means ok
	}

      ratio += pc3WestRatio->GetBinContent(ic+1);
    }
  textFile << endl;

  ratio /= 32;
  if (ratio <= ratiocut_semibad)
    {
      pc3westratiostat = 2;
    
      if (ratio <= ratiocut_realbad)
	{
	  pc3westratiostat = 1;
	}
    }
  else
    {
      pc3westratiostat = 0;
    }
  
  // is anything wrong? Use at your own risk..
  if (pc1eastlow > 0)
    pc1eastactstat = 2;
  if (pc3eastlow > 0)
    pc3eastactstat = 2;
  if (pc1westlow > 0)
    pc1westactstat = 2;
  if (pc2westlow > 0)
    pc2westactstat = 2;
  if (pc3westlow > 0)
    pc3westactstat = 2;
  
  // is something really wrong? Flag as bad.. Could still be useable 
  if (pc1eastlow > maximumlow)
    pc1eastactstat = 1;
  if (pc3eastlow > maximumlow)
    pc3eastactstat = 1;
  if (pc1westlow > maximumlow)
    pc1westactstat = 1;
  if (pc2westlow > maximumlow)
    pc2westactstat = 1;
  if (pc3westlow > maximumlow)
    pc3westactstat = 1;
  
  
  textFile << endl << "*** Cuts ***" << endl;
  //  textFile << " ----------------------------------------------------" << endl;
  textFile << "Req. activity (clusters/event/wire)= " << activitycut << endl;
  textFile << "Max. allowed sectors with low activity (per pc and arm)= " <<
    maximumlow << endl;
  textFile << "Req. ratios (#2-cellclusters to #1-cellclusters) > " << ratiocut_semibad << endl;
  textFile << "Allowed ratios (#2-cellclusters to #1-cellclusters) > " << ratiocut_realbad << endl;

  textFile << endl << "*** Status ***" << endl;
  //  textFile << " ----------------------------------------------------" << endl;
  textFile << "Chamber\tPC1W\tPC1E\tPC2W\tPC3W\tPC3E" << endl;
  textFile << "Active\t" 
	   << pc1westactstat << "\t"
	   << pc1eastactstat << "\t"
	   << pc2westactstat << "\t"
	   << pc3westactstat << "\t"
	   << pc3eastactstat << endl;

  textFile << "Ratio\t" 
	   << pc1westratiostat << "\t"
	   << pc1eastratiostat << "\t"
	   << pc2westratiostat << "\t"
	   << pc3westratiostat << "\t"
	   << pc3eastratiostat << endl;

  Int_t pc1easttotstat = 0, pc3easttotstat = 0;
  Int_t pc1westtotstat = 0, pc2westtotstat = 0, pc3westtotstat = 0;

  if (pc1westratiostat == 1 || pc1westactstat == 1)
    {
      pc1westtotstat = 1;
    }
  else if (pc1westratiostat == 2 || pc1westactstat == 2)
    {
      pc1westtotstat = 2;
    }

  if (pc2westratiostat == 1 || pc2westactstat == 1)
    {
      pc2westtotstat = 1;
    }
  else if (pc2westratiostat == 2 || pc2westactstat == 2)
    {
      pc2westtotstat = 2;
    }

  if (pc3westratiostat == 1 || pc3westactstat == 1)
    {
      pc3westtotstat = 1;
    }
  else if (pc3westratiostat == 2 || pc3westactstat == 2)
    {
      pc3westtotstat = 2;
    }

  if (pc1eastratiostat == 1 || pc1eastactstat == 1)
    {
      pc1easttotstat = 1;
    }
  else if (pc1eastratiostat == 2 || pc1eastactstat == 2)
    {
      pc1easttotstat = 2;
    }

  if (pc3eastratiostat == 1 || pc3eastactstat == 1)
    {
      pc3easttotstat = 1;
    }
  else if (pc3eastratiostat == 2 || pc3eastactstat == 2)
    {
      pc3easttotstat = 2;
    }

  textFile << "Total\t" 
	   << pc1westtotstat << "\t"
	   << pc1easttotstat << "\t"
	   << pc2westtotstat << "\t"
	   << pc3westtotstat << "\t"
	   << pc3easttotstat << endl;

   statusFile << pc1easttotstat << " " << pc1westtotstat << " ";
   statusFile << pc2westtotstat << " ";
   statusFile << pc3easttotstat << " ";
   statusFile << pc3westtotstat << " ";
}





