#include "TFile.h"
#include "TH1.h"
#include "TProfile.h"
#include "summaryQA.h"
#include <fstream>
#include <iostream>

using namespace std;
int QASummary::processPad()
{
  cout << "Pad..." << endl;
  fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
  fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- PAD QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
    
  textFile << endl << "*** Detailed HV info ***" << endl;

  TProfile *pc1EastActivity = (TProfile *) qafile->Get ("pc1EastActivity");
  TProfile *pc1WestActivity = (TProfile *) qafile->Get ("pc1WestActivity");
  TH1F *pc1EastRatio = (TH1F *) qafile->Get ("pc1EastRatio");
  TH1F *pc1WestRatio = (TH1F *) qafile->Get ("pc1WestRatio");

  TProfile *pc2WestActivity = (TProfile *) qafile->Get ("pc2WestActivity");
  TH1F *pc2WestRatio = (TH1F *) qafile->Get ("pc2WestRatio");

  TProfile *pc3EastActivity = (TProfile *) qafile->Get ("pc3EastActivity");
  TProfile *pc3WestActivity = (TProfile *) qafile->Get ("pc3WestActivity");
  TH1F *pc3EastRatio = (TH1F *) qafile->Get ("pc3EastRatio");
  TH1F *pc3WestRatio = (TH1F *) qafile->Get ("pc3WestRatio");
  // add here analysis and printout results to textFile
  
  float padxn[32];
  float ratio;
  int pc,arm;
  int pc1eastactstat = 0, pc3eastactstat = 0;
  int pc1westactstat = 0, pc2westactstat = 0, pc3westactstat = 0;
  int pc1eastratiostat = 0, pc3eastratiostat = 0;
  int pc1westratiostat = 0, pc2westratiostat = 0, pc3westratiostat = 0;
  int pc1eastlow = 0, pc3eastlow = 0;
  int pc1westlow = 0, pc2westlow = 0, pc3westlow = 0;
  float activitycut = 0.001;	   // min clusters/event/wire (default gold)
  int maximumlow = 1;	           // 1 bad is still almost ok
  int ratiocut_semibad = 1;
  float ratiocut_realbad = 0.5;

int PC1E_bad_low=32010, PC1E_bad_high=32441;
int PC2W_bad_low=33708, PC2W_bad_high=35000;

// *** Henrik: we no longer need to differentiate between AuAu and pp ***
//  if(runNumber<=runchange) {
//    activitycut = gold_activitycut;  //gold data
//    cout << "AuAu ";
//  }
//  if(runNumber>runchange) {
//    activitycut = pp_activitycut; //pp data
//    cout << "pp ";
//  }

// cout << activitycut <<endl;

  // PC1 E
  ratio = 0;
  pc = 0; // PC1
  arm = 0; // East arm
  textFile << pc << " " << arm << endl;
  for (int ic = 0; ic < 32; ic++)
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
  float ratio_pc1e = (float) ratio;

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
  for (int ic = 0; ic < 32; ic++)
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

  float ratio_pc1w = (float)ratio;

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
  for (int ic = 0; ic < 32; ic++)
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

  float ratio_pc2w = (float)ratio;

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
  for (int ic = 0; ic < 32; ic++)
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

  float ratio_pc3e = (float) ratio;

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
  for (int ic = 0; ic < 32; ic++)
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

  float ratio_pc3w = (float)ratio;

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

  int pc1easttotstat = 0, pc3easttotstat = 0;
  int pc1westtotstat = 0, pc2westtotstat = 0, pc3westtotstat = 0;

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

   CommitToQADatabase("Pad", "PC1 East Ratio", ratio_pc1e, 0.0);
   CommitToQADatabase("Pad", "PC1 West Ratio", ratio_pc1w, 0.0);
   CommitToQADatabase("Pad", "PC2 West Ratio", ratio_pc2w, 0.0);
   CommitToQADatabase("Pad", "PC3 East Ratio", ratio_pc3e, 0.0);
   CommitToQADatabase("Pad", "PC3 West Ratio", ratio_pc3w, 0.0);
   CommitToQADatabase("Pad", "PC1 East Low Activity", (float)pc1eastlow, 0.0);
   CommitToQADatabase("Pad", "PC1 West Low Activity", (float)pc1westlow, 0.0);
   CommitToQADatabase("Pad", "PC2 West Low Activity", (float)pc2westlow, 0.0);
   CommitToQADatabase("Pad", "PC3 East Low Activity", (float)pc3eastlow, 0.0);
   CommitToQADatabase("Pad", "PC3 West Low Activity", (float)pc3westlow, 0.0);

  cout << "    ...done." << endl;
  return 0;
}





