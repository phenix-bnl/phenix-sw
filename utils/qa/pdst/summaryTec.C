#include <TFile.h>
#include <TH1.h>
#include <fstream>
#include <iostream>
#include <cmath>

#include <summaryQA.h>

float meantrackmulti, meantimebinstrack;
int ineff;

#include <QaBankID.h>  // enumerates unique bank ID's

using namespace std;
int QASummary::processTec()
{
  cout << "Tec..." << endl;
  fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
  fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }

  const int nSector  = 4;
  const int nPlane   = 6; 
  const int nSide    = 2; // North = 1 South = 0
	//RUN 3 sector 0 is mostly off considered inactive - skipping it for qa
	//RUN 3 most plane 5 are  highly inefficient (fem off bacause of noise) - skipping them
  //RUN 3 sector 2 plane 2N and sector 3 plane 1S are dead - skipping them
  //RUN 4 only sectors 1S,2S,1N,2N worked.   Cesar
  const int active[nSide][nSector][nPlane] =
  { 
     {{0, 0, 0, 0, 0, 0},  // E 0 South  0 - 5
     {1, 1, 1, 1, 1, 1},  // E 1 South  0 - 5
     {1, 1, 1, 1, 1, 1},  // E 2 South  0 - 5
     {0, 0, 0, 0, 0, 0}},  // E 3 South  0 - 5 
     {{0, 0, 0, 0, 0, 0},  // E 0 North  0 - 5 
     {1, 1, 1, 1, 1, 1},  // E 1 North  0 - 5
     {1, 1, 0, 1, 1, 1},  // E 2 North  0 - 5
     {0, 0, 0, 0, 0, 0}}  // E 3 North  0 - 5
  };

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- TEC QA Summary for run # " << runNumber << endl;
  textFile << " ----------------------------------------------------" << endl;

  TH1F* techist0 = (TH1F *) qafile->Get ("tecTrkMult");
  TH1F* techist1 = (TH1F *) qafile->Get ("tecTrkNhits");
  TH1F* techist2 = (TH1F *) qafile->Get ("tecNhitProf");
  float xentries[2], xmean[2], xrms[2];

  if (!techist0 || !techist1 || !techist2)
    {
      textFile << "One of the histograms wasn't founded." << endl;
      textFile << "Aborting TEC QA summary" << endl;
      return 0;
    }

  xentries[0] = techist0->GetEntries ();
  xmean[0] = techist0->GetMean ();
  xrms[0] = techist0->GetRMS ();
  xentries[1] = techist1->GetEntries ();
  xmean[1] = techist1->GetMean ();
  xrms[1] = techist1->GetRMS ();
 
  float xn[48];
  
  for (int ic = 0; ic < 48; ic++)
    {
      xn[ic] = techist2->GetBinContent (ic + 1);
    }
  
  int tecstatus;
  tecstatus = 0;
  ineff = 0;
  
  for (int isec = 0; isec<nSector; isec++)
    {
      for (int iside = 0; iside<nSide; iside++)
	{
	  for (int ipl = 0; ipl<nPlane; ipl++)
	    {
	      int ind=(isec*2+iside)*nPlane+ipl;
	      if(active[iside][isec][ipl]==1)
		{
		  //cout<<isec<<" "<<" "<<ipl<<" "<<iside<<" ind "<<ind<<" "<<xn[ind]<<endl;
		  if(xn[ind] < 15.)ineff = ineff + 1;
		}
	    }
	}
    }


  if (ineff > 0)
    tecstatus = 2;

  if (ineff > 2)
    tecstatus = 1;
  
  meantrackmulti = xmean[0];
  meantimebinstrack = xmean[1];

  textFile << " TEC number of inefficient sector side " << ineff << endl;
  textFile << " Average TEC track multiplicity " << xmean[0] << endl;
  textFile << " Average number of time bins/track " << xmean[1] << endl;
  textFile << " TEC Status = " << tecstatus << endl;

  statusFile << tecstatus << " ";

  textFile.close();
  statusFile.close();

  CommitToQADatabase("Tec", "Inefficient planes", (float)ineff, 0.0);
  CommitToQADatabase("Tec", "Track Multiplicity", meantrackmulti, xrms[0]/sqrt(xentries[0]));
  CommitToQADatabase("Tec", "timebins / track", meantimebinstrack, xrms[1]/sqrt(xentries[1]));

  cout << "    ...done." << endl;
  return 0;
}
