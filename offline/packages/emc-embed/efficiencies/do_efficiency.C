#include "emcEfficiency.h"
#include "TSystem.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TH2.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TString.h"
#include "TLegend.h"
#include "TStopwatch.h"
#include "EventSorting.h"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>

const char *location = "/home/enterria/";
//const char *location = "/phenix/u/enterria/wa/";

//_____________________________________________________________________________
// 
/*

gSystem->Load("libCrkPID.so");
gSystem->Load("/afs/rhic/phenix/users/enterria/install8/lib/libemc.so");
gSystem->Load("/afs/rhic/phenix/users/enterria/install8/lib/libemcEfficiency.so");
gSystem->Load("~/afs/offline/packages/emc-embed/efficiencies/do_efficiency_C.so");

// or ...

gSystem->Load("libCrkPID.so"); 
gSystem->Load("/afs/rhic/phenix/users/enterria/install8/lib/libemc.so");
gSystem->Load("/afs/rhic/phenix/users/enterria/install8/lib/libemcEfficiency.so");
.includepath /afs/rhic/phenix/users/enterria/install8/include
.includepath $OFFLINE_MAIN/include
.L ~/afs/offline/packages/emc-embed/efficiencies/do_efficiency.C++

*/

//
// 1) First iteration over the embed+eval files to determine the set of Hagedorn weights
//    (*and* the fluctuation correction histogram in the case of Run-2 PPG014 pi0).
// 
// 2) Second run over the embed+eval files with the new set of weights.

void loop_on_merged(char *partic="eta", 
		    int sequence = 659, int smearing = 7, int pass = 1, 
		    int verbose = 1 )
{

  cout << " OK. I'll loop on " << sequence << " embed+eval files with " 
       << smearing << "% energy smearing. This is PASS = " << pass << endl;

  //_____________________________________________________________________________

  int typeOfPartic = 0;

  if (!strcmp(partic,"pi0")) typeOfPartic = 7;
  else typeOfPartic = 17;

  emcEfficiency eff(2,typeOfPartic); // Run-2 pi0 or eta

  eff.setEmbedObjectsType(0);// old STAF tables
  //eff.setEmbedObjectsType(1);// new Justin's objects

  eff.setVerbose(verbose);

  //eff.computeEfficiencyFor("PbGl");

  //eff.setPTrange(8,12);// for high-pT pions

  //_____________________________________________________________________________

  char name[400];
  //sprintf(name,"%s/efficiency_%s_%dpercent_smear_%d_%dpass_3dHagWeigh.root",location,partic,smearing,sequence,pass);
  //sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_nofixedwindows.root",
  sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass.root",
	  location,partic,partic,smearing,sequence,pass);

  // backup copy
  cout << endl;
  char backup[600];
  //sprintf(backup,"cp -vf %s %s/efficiency_%s_%dpercent_smear_%d_%dpass_3dHagWeigh_bckup_firstprod.root",
  sprintf(backup,"cp -vf %s %s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_bckup_firstprod.root",
	  name,location,partic,partic,smearing,sequence,pass);
  gSystem->Exec(backup);
  cout << endl;
  
  // read list of input files
  char listoffiles[400];
  //sprintf(listoffiles,"list_%dpercent_smear_%d_reduced.txt",partic,smearing,sequence);
  sprintf(listoffiles,"list_%s_%dpercent_smear_%d.txt",partic,smearing,sequence);
  ifstream in(listoffiles);

  if ( !in )
    {
      cout << "<E> Could not read " << listoffiles << endl;
      return;
    }
  cout << "<I> Reading " << listoffiles << endl;

  //_____________________________________________________________________________

  TStopwatch timer;
  timer.Start();

//   // Needed only for PPG014 pi0 production ...
//   if ( !strcmp(partic,"pi0") && pass == 2 ) // Export fluctuation correction first
//     {
//       char name1[400];
//       //sprintf(name1,"%s/efficiency_%s_%dpercent_smear_%d_1pass_3dHagWeigh.root",location,partic,smearing,sequence);
//       sprintf(name1,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_1pass.root",location,partic,partic,smearing,sequence);
//       // Recover the fluctuation function from the first pass
//       bool ok = eff.setRun2ExodusFluctCorrectionFromFile(name1);
//       if ( !ok ) return;
//     }

  bool ok = eff.openEfficiencyFile(name,"RECREATE");
  if ( !ok ) return;

  //_____________________________________________________________________________
  
  int n=0;
  
  char s[400];
  while ( in.getline(s,199,'\n') )
    {
      eff.openEmbedFile(s);
      eff.run();
      eff.closeEmbedFile();
      ++n;
      if ( n % 10 == 0 ) 
	{
	  cout << endl << " ************** Now checkpointing output file # : " 
	       << n << " ************** " << endl;
	  eff.writeEfficiencyFile(); // security saving every 10 files ...
	  cout << "Done." << endl;
	}      
    }
  eff.close();

//   if ( pass == 1 && !strcmp(partic,"pi0")) // Fit fluctuation correction (only relevant for pi0 PPG014 Run-2 ...)
//     {
//       eff.openEfficiencyFile(name,"UPDATE");
//       cout << "Fitting correction" << endl;
//       eff.fitFluctuation();
//       eff.close();
//     }

  timer.Stop();
  timer.Print();

}

//_____________________________________________________________________________
// in case the fitting correction above didn't work ...
// function only relevant for pi0 Run-2 (PPG014)

void fitfluct(int sequence = 659, 
	      int smearing = 7, int pass = 1, 
	      int verbose = 1 )
{

  emcEfficiency eff;
  eff.setVerbose(verbose);

  char name[400];
  //sprintf(name,"%s/efficiency_pi0_%dpercent_smear_%d_%dpass_3dHagWeigh.root",location,smearing,sequence,pass);
  sprintf(name,"%s/efficiency_pi0_%dpercent_smear_%d_%dpass.root",location,smearing,sequence,pass);

  eff.openEfficiencyFile(name,"UPDATE");

  eff.fitFluctuation();
  eff.close();

} 

//_____________________________________________________________________________
// Obtain the efficiency histograms and peak and width positions

void do_efficiency(char *partic="eta", 
		   int sequence = 659, int smearing = 7, int pass=2, 
		   int verbose = 2 )
{

  int typeOfPartic = 0;

  if (!strcmp(partic,"pi0")) typeOfPartic = 7;
  else typeOfPartic = 17;

  emcEfficiency eff(2,typeOfPartic); // Run-2 pi0 or eta

  eff.setEmbedObjectsType(0);// old STAF tables
  //eff.setEmbedObjectsType(1);// new Justin's objects

  eff.setVerbose(verbose);

  eff.useFastMCDeadWarnEfficLoss(true); // correction for different fidu&dead maps in fastMC and in embedding
  //eff.useFastMCDeadWarnEfficLoss(false); // no correction for different fidu&dead maps in fastMC and in embedding

  //eff.setFixedExtractionWindows(false); 

  //eff.computeEfficiencyFor("PbGl");

  //eff.setPTrange(8,12);// for high-pT pions

  char name[400];
  //sprintf(name,"%s/efficiency_%s_%dpercent_smear_%d_%dpass.root",location,partic,smearing,sequence,pass);
  //sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_nofixedwindows.root",
  sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass.root",
  //sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_newextraloss.root",
	  location,partic,partic,smearing,sequence,pass);

  cout << endl;
  char backup[400];
  //sprintf(backup,"cp -vf %s %s/efficiency_%s_%dpercent_smear_%d_%dpass_3dHagWeigh_bckup_before_effcalc.root",
  //sprintf(backup,"cp -vf %s %s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_nofixedwindows_bckup_before_effcalc.root",
  sprintf(backup,"cp -vf %s %s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_bckup_before_effcalc.root",
	  name,location,partic,partic,smearing,sequence,pass);
  gSystem->Exec(backup);
  cout << endl;

  //_____________________________________________________________________________

  cout << "Computing efficiencies from file " << name << endl;

  eff.openEfficiencyFile(name,"UPDATE");

  eff.printCuts();

  eff.efficiency_all_cuts();

//   // message only relevant for pi0 Run-2 (PPG014) with "fluctuation" problems
//   if (!strcmp(partic,"pi0"))
//     {
//       cout << " <I> If your efficiency calculation didn't work for many cuts/centralities"
// 	   << " something wrong may have happened with your fluctuation correction ... " << endl
// 	   << " Try fitfluct() again and redo do_efficiency() afterwards ..." << endl;
//     }

  //std::ofstream fileout("my_correction_factors.h");
  //eff.outputFinalEfficiencyFactors("mycut","counts",fileout);
  //eff.outputFinalEfficiencyFactors_all_cuts("counts",fileout);

}

//_____________________________________________________________________________
// Fit the efficiency histograms and output the correction factors

void output_eff_factors(char *partic="eta", 
			int sequence = 659, int smearing = 7, int pass = 2, 
			int verbose = 1 )
{

  int typeOfPartic = 0;

  double ptfitmin = 0.;
  double ptfitmax = 0.;

  if (!strcmp(partic,"pi0"))
    {
      typeOfPartic = 7;
      ptfitmin = 0.7; // "standard" pi0 values
      ptfitmax = 19.; // "standard" pi0 values
      //ptfitmin = 9.;// for high-pT pi0
      //ptfitmax = 11.; // for high-pT pi0
    }
  else
    {
      typeOfPartic = 17;
      //ptfitmin = 2.0;
      //ptfitmax = 10.;
      ptfitmin = 1.0;
      ptfitmax = 19.;
    }

  emcEfficiency eff(2,typeOfPartic); // Run-2 pi0 or eta

  eff.setEmbedObjectsType(0);// old STAF tables

  eff.setVerbose(verbose);

  //eff.setPTrange(8,12);// for high-pT pions

  char name[400];
  //sprintf(name,"%s/efficiency_%s_%dpercent_smear_%d_%dpass_3dHagWeigh.root",location,partic,smearing,sequence,pass);
  //sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_nofixedwindows.root",
  sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass.root",
  //sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_newextraloss.root",
	  location,partic,partic,smearing,sequence,pass);

  char outfile[400];
  //sprintf(outfile,"emcEfficiency_%s_%dpercentsmear_%dpass_counts_nofixedwindows.h",partic,smearing,pass);
  sprintf(outfile,"emcEfficiency_%s_%dpercentsmear_%dpass_counts.h",partic,smearing,pass);

  //_____________________________________________________________________________

  eff.readEfficiencyFile(name);

  eff.printCuts();

  cout << "<I> Outputing efficiency factors" << endl;

  std::ofstream fileout(outfile);
  eff.outputFinalEfficiencyFactors_all_cuts("counts",ptfitmin,ptfitmax,fileout);

  fileout.close();

  cout << endl << "<I> "<< outfile << " produced." << endl;
}

//_____________________________________________________________________________
// Fit the efficiency histograms and output the correction factors
// for a given cut

void outputEfficiencyTableForCut(char *partic="eta",
				 int sequence = 659, int smearing = 7, int pass=2, 
				 char *cut = "BasicCutsAsym1ChiSq1ToF1Cut",
				 char *format = "latex", int verbose = 1 )
{

  int typeOfPartic = 0;

  if (!strcmp(partic,"pi0")) typeOfPartic = 7;
  else typeOfPartic = 17;

  emcEfficiency eff(2,typeOfPartic); // Run-2 pi0 or eta

  eff.setEmbedObjectsType(0);// old STAF tables

  eff.setVerbose(verbose);

  char name[400];
  //sprintf(name,"%s/efficiency_%s_%dpercent_smear_%d_%dpass_3dHagWeigh.root",location,partic,smearing,sequence,pass);
  sprintf(name,"%s/efficiency_%s_%dpercent_smear_%d_%dpass.root",location,partic,smearing,sequence,pass);

  char outfile[400];
  sprintf(outfile,"emcEfficiencyTable_%s_%dpercent_%d_counts.tex",partic,smearing,pass);
  eff.readEfficiencyFile(name);

  double ptfitmin = 0.8; // "standard" values
  double ptfitmax = 10.; // "standard" values

  cout << endl << " <I> Outputing efficiency factors" << endl << endl;

  std::ofstream fileout(outfile);
  eff.outputFinalEfficiencyFactors( cut, "counts", ptfitmin, ptfitmax, true,fileout, format);

  fileout.close();

}

//_____________________________________________________________________________
// plot the peak and width positions

void plot_peak_and_width_positions(char *partic="eta",
				   int sequence = 659, int smearing = 7, int pass = 2, 
				   std::string cut = "BasicCuts3x3Asym1ChiSq2ToF2Cut", 
				   int verbose = 1 )
{

  char file_location[100];
  int typeOfPartic = 0;

  if (!strcmp(partic,"pi0"))
    {
      sprintf(file_location,"/phenix/u/saskia/pi0_rescan"); // PPG014 peaks&widths
      sprintf(file_location,"/phenix/u/saskia/year2/dec03_scan"); // Dec.03 scan: peaks&widths
      typeOfPartic = 7;
    }
  else
    {
      sprintf(file_location,"/phenix/workarea/saskia/eta_peaks"); // Jul'03 peaks&widths
      sprintf(file_location,"/phenix/workarea/saskia/dec03_scan"); // Dec.03 scan: peaks&widths
      sprintf(file_location,"/phenix/u/enterria/afs/run2_eta/saskia_eta_peaks_dec03"); // Dec.03 scan: peaks&widths
      typeOfPartic = 17;
    }

  emcEfficiency eff(2,typeOfPartic); // Run-2 pi0 or eta

  eff.setEmbedObjectsType(0);// old STAF tables

  eff.setVerbose(verbose);

  char name[400];
  //sprintf(name,"%s/efficiency_%s_%dpercent_smear_%d_%dpass_3dHagWeigh.root",location,partic,smearing,sequence,pass);
  sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass.root",
	  location,partic,partic,smearing,sequence,pass);

  eff.readEfficiencyFile(name);

  //_____________________________________________________________________________

  EventSorting* eventSorting = (EventSorting*)eff.getEventSorting();
  const size_t ncent = eventSorting->getNCentralities(); // last one is min.bias

  size_t icent = 0 ;
  int iminbias = 10000 ;
  int centMin = -1;
  int centMax = -1;

  std::string khRecoParticleMinvPeak = "hRecoParticleMinvPeak";
  std::string khRecoParticleMinvSigma = "hRecoParticleMinvSigma";

  TH1* peak_embed[ncent];
  TH1* width_embed[ncent];

  //_____________________________________________________________________________

  char FixedPeakWidthFiles[300];
  char RealPeakWidthFiles[300];

  const int NMAX = 30;
  double pT[NMAX];
  double meanfix[NMAX];
  double sigfix[NMAX];

  double pTreal[NMAX];
  double meanreal[NMAX];
  double sigreal[NMAX];
  double emeanreal[NMAX];
  double esigreal[NMAX];

  TGraphErrors *peaks_real[ncent];
  TGraphErrors *widths_real[ncent];

  TGraph *peaks_fixed_extraction[ncent];
  TGraph *widths_fixed_extraction[ncent];
  //gStyle->SetEndErrorSize(0);

  //_____________________________________________________________________________

  TCanvas *peak_canvas[ncent];
  TCanvas *width_canvas[ncent];

  // Default values for pi0
  double ptmin = 1., ptmax = 20.;
  double massmin = 0.12, massmax = 0.16; // GeV/c2
  double wmin = 0., wmax = 25; // MeV/c2
  double GeVtoMeV = 1000;

  // eta
  if (typeOfPartic == 17)
    {
      ptmin = 1.;
      ptmax = 10.;
      massmin = 0.4;
      massmax = 0.7; // GeV/c2
      wmin = 0.;
      wmax = 100; // MeV/c2
    }

  TLegend *pleg[ncent];
  TLegend *wleg[ncent];

  char title[400];

  //_____________________________________________________________________________
  
  for ( icent = 0 ; icent < ncent; icent++)
    {

      //_____________________________________________________________________________
      // embed 

      peak_embed[icent] = eff.getHistogram<TH1>(cut,khRecoParticleMinvPeak,icent);
      width_embed[icent] = eff.getHistogram<TH1>(cut,khRecoParticleMinvSigma,icent);
      width_embed[icent]->Scale(GeVtoMeV);

      //_____________________________________________________________________________
      // fixed & real

      eventSorting->getCentralityClassLimits(icent,centMin,centMax);

      if (typeOfPartic == 7)
	{
	  centMin = ( centMin >= 60 ) ? 60 : centMin;
	  centMax = ( centMin >= 60 ) ? centMin+20 : centMax; // last file is 60_80.txt (we use it for all most periph. classes)
	  if ( centMax == iminbias ) { centMin = 0 ; centMax = 100 ; }
	  sprintf(FixedPeakWidthFiles,"%s/peaksfix_%d_%d.txt",file_location,centMin,centMax);
	}
      else
	{
	  //if ( centMax != iminbias ) continue;
	  if ( centMax == iminbias ) { centMin = 0 ; centMax = 92; }
	  sprintf(FixedPeakWidthFiles,"%s/peaksfix_%d_%d.txt",file_location,centMin,centMax);
	  sprintf(RealPeakWidthFiles,"%s/pkposwidth_%d_%d.txt",file_location,centMin,centMax);
	}

      //_____________________________________________________________________________
      // read fixed peak and width from files

      ifstream ifiledata(FixedPeakWidthFiles);
      if (!ifiledata)
	{
	  cerr << "<E> Can't open file: " << FixedPeakWidthFiles << endl;
	  return;
	}
      else if ( verbose > 0 )
	{
	  cout << "<I> Reading " << FixedPeakWidthFiles << endl;
	}

      int i = 0;
      while ( ifiledata >>  pT[i] >>  meanfix[i] >> sigfix[i] ) { sigfix[i]*=GeVtoMeV; i++;  }
      ifiledata.close();

      // fixed up to 20 GeV/c
      pT[i] = 20;
      meanfix[i] = meanfix[i-1];
      sigfix[i] = sigfix[i-1];
      i++;

      //_____________________________________________________________________________
      // read real peak and width from files (for eta)

      int j = 0;
      if (typeOfPartic == 17)
	{
	  ifstream rfiledata(RealPeakWidthFiles);
	  if (!rfiledata)
	    {
	      cerr << "<E> Can't open file: " << RealPeakWidthFiles << endl;
	    }
	  else if ( verbose > 0 )
	    {
	      cout << "<I> Reading " << RealPeakWidthFiles << endl;
	      while ( rfiledata >> pTreal[j] >> meanreal[j] >> emeanreal[j] >> sigreal[j] >> esigreal[j] ) 
		{ sigreal[j]*=GeVtoMeV; esigreal[j]*=GeVtoMeV; j++;  }
	    }
	  rfiledata.close();
	}

      //_____________________________________________________________________________
      // plot PEAKS

      sprintf(title,"%s_peaks_cent%d_%d_%s_%dsmear_%dpass",partic,centMin,centMax,cut.c_str(),smearing,pass);
      peak_canvas[icent] = new TCanvas(title,title,650,500);
      peak_canvas[icent]->Range(-0.25,0.35,10.,0.75);
      peak_canvas[icent]->SetLeftMargin(0.12);
      peak_canvas[icent]->SetRightMargin(0.02);
      peak_canvas[icent]->SetTopMargin(0.08);
      peak_canvas[icent]->SetBottomMargin(0.12);

      // fixed peaks
      peaks_fixed_extraction[icent] = new TGraph(i,pT,meanfix);
      sprintf(title,"%s_%dsmear_%dpass",cut.c_str(),smearing,pass);
      peaks_fixed_extraction[icent]->SetTitle(title);
      peaks_fixed_extraction[icent]->SetMaximum(massmax);
      peaks_fixed_extraction[icent]->SetMinimum(massmin);
      peaks_fixed_extraction[icent]->SetMarkerStyle(21);
      peaks_fixed_extraction[icent]->SetMarkerColor(4);
      peaks_fixed_extraction[icent]->SetMarkerSize(1.4);
      peaks_fixed_extraction[icent]->Draw("AP");
      sprintf(title,"%s peaks (%d-%d%%) [GeV/c2]",partic,centMin,centMax);
      peaks_fixed_extraction[icent]->GetHistogram()->SetYTitle(title);
      peaks_fixed_extraction[icent]->GetHistogram()->SetXTitle("p_{T} (GeV/c)");
      peaks_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetRangeUser(ptmin,ptmax);
//       double iptmin = ptmin/peaks_fixed_extraction[icent]->GetHistogram()->GetXaxis()->GetBinWidth(0);
//       double iptmax = ptmax/peaks_fixed_extraction[icent]->GetHistogram()->GetXaxis()->GetBinWidth(0);
      //peaks_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetRange(iptmin,iptmax);
      //peaks_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetRange(ptmin,ptmax);
      peaks_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetTitleSize(0.055);
      peaks_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetTitleOffset(1.0);
      peaks_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetLabelSize(0.05);
      peaks_fixed_extraction[icent]->GetHistogram()->GetYaxis()->SetTitleSize(0.055);
      peaks_fixed_extraction[icent]->GetHistogram()->GetYaxis()->SetTitleOffset(1.15);
      peaks_fixed_extraction[icent]->GetHistogram()->GetYaxis()->SetLabelSize(0.05);
      peak_canvas[icent]->Update();

      // real peaks
      peaks_real[icent] = new TGraphErrors(j,pTreal,meanreal,0,emeanreal);
      peaks_real[icent]->SetMarkerStyle(30);
      peaks_real[icent]->SetMarkerColor(4);
      peaks_real[icent]->SetMarkerSize(2.1);
      peaks_real[icent]->SetLineColor(4);
      peaks_real[icent]->Draw("P");
      peak_canvas[icent]->Update();

      // embed peaks
      peak_embed[icent]->SetMarkerStyle(24);
      peak_embed[icent]->SetMarkerColor(4);
      peak_embed[icent]->SetMarkerSize(1.4);
      peak_embed[icent]->Draw("same");
      peak_canvas[icent]->Update();

      // legends peaks
      sprintf(title,"%s peaks (%d-%d%%)",partic,centMin,centMax);
      pleg[icent] = new TLegend(0.379257,0.741525,0.955108,0.891949,title,"brNDC");
      pleg[icent]->SetTextSize(0.034);
      pleg[icent]->SetLineColor(1);
      pleg[icent]->SetLineStyle(1);
      pleg[icent]->SetLineWidth(1);
      pleg[icent]->SetFillColor(0);
      peak_canvas[icent]->cd();
      sprintf(title,"measured in real %s",partic);
      pleg[icent]->AddEntry(peaks_real[icent],title, "P");
      sprintf(title,"fixed peak used in extraction");
      pleg[icent]->AddEntry(peaks_fixed_extraction[icent],title, "P");
      sprintf(title,"measured in embedded %s",partic);
      pleg[icent]->AddEntry(peak_embed[icent],title, "P");
      pleg[icent]->Draw();
      peak_canvas[icent]->Update();

      //_____________________________________________________________________________
      // plot WIDTHS

      sprintf(title,"%s_widths_cent%d_%d_%s_%dsmear_%dpass",partic,centMin,centMax,cut.c_str(),smearing,pass);
      width_canvas[icent] = new TCanvas(title,title,650,500);
      width_canvas[icent]->Range(0,-12.,10.,110.);
      width_canvas[icent]->SetLeftMargin(0.12);
      width_canvas[icent]->SetRightMargin(0.02);
      width_canvas[icent]->SetTopMargin(0.08);
      width_canvas[icent]->SetBottomMargin(0.12);

      // fixed widths
      widths_fixed_extraction[icent] = new TGraph(i,pT,sigfix);
      sprintf(title,"%s_%dsmear_%dpass",cut.c_str(),smearing,pass);
      widths_fixed_extraction[icent]->SetTitle(title);
      widths_fixed_extraction[icent]->SetMaximum(wmax);
      widths_fixed_extraction[icent]->SetMinimum(wmin);
      widths_fixed_extraction[icent]->SetMarkerStyle(20);
      widths_fixed_extraction[icent]->SetMarkerColor(2);
      widths_fixed_extraction[icent]->SetMarkerSize(1.4);
      widths_fixed_extraction[icent]->Draw("AP");
      sprintf(title,"%s widths (%d-%d%%) [MeV/c2]",partic,centMin,centMax);
      widths_fixed_extraction[icent]->GetHistogram()->SetYTitle(title);
      widths_fixed_extraction[icent]->GetHistogram()->SetXTitle("p_{T} (GeV/c)");

//       widths_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetRangeUser(ptmin,ptmax);
//       iptmin = ptmin/widths_fixed_extraction[icent]->GetHistogram()->GetXaxis()->GetBinWidth(0);
//       iptmax = ptmax/widths_fixed_extraction[icent]->GetHistogram()->GetXaxis()->GetBinWidth(0);
//       widths_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetRange(iptmin,iptmax);
      //widths_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetRange(ptmin,ptmax);

      widths_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetTitleSize(0.055);
      widths_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetTitleOffset(1.0);
      widths_fixed_extraction[icent]->GetHistogram()->GetXaxis()->SetLabelSize(0.05);
      widths_fixed_extraction[icent]->GetHistogram()->GetYaxis()->SetTitleSize(0.055);
      widths_fixed_extraction[icent]->GetHistogram()->GetYaxis()->SetTitleOffset(1.15);
      widths_fixed_extraction[icent]->GetHistogram()->GetYaxis()->SetLabelSize(0.05);      
      width_canvas[icent]->Update();

      // real peaks
      widths_real[icent] = new TGraphErrors(j,pTreal,sigreal,0,esigreal);
      widths_real[icent]->SetMarkerStyle(29);
      widths_real[icent]->SetMarkerColor(2);
      widths_real[icent]->SetMarkerSize(2.1);
      widths_real[icent]->SetLineColor(2);
      widths_real[icent]->Draw("P");
      width_canvas[icent]->Update();

      // embed widths
      width_embed[icent]->SetMarkerStyle(24);
      width_embed[icent]->SetMarkerColor(2);
      width_embed[icent]->SetMarkerSize(1.4);
      width_embed[icent]->Draw("same");
      width_canvas[icent]->Update();

      // legends widths
      sprintf(title,"%s widths (%d-%d%%)",partic,centMin,centMax);
      wleg[icent] = new TLegend(0.379257,0.741525,0.955108,0.891949,title,"brNDC");
      wleg[icent]->SetTextSize(0.034);
      wleg[icent]->SetLineColor(1);
      wleg[icent]->SetLineStyle(1);
      wleg[icent]->SetLineWidth(1);
      wleg[icent]->SetFillColor(0);
      width_canvas[icent]->cd();
      sprintf(title,"measured in real %s",partic);
      wleg[icent]->AddEntry(widths_real[icent],title, "P");
      sprintf(title,"fixed width used in extraction");
      wleg[icent]->AddEntry(widths_fixed_extraction[icent],title, "P");
      sprintf(title,"measured in embedded %s",partic);
      wleg[icent]->AddEntry(width_embed[icent],title, "P");
      wleg[icent]->Draw();
      width_canvas[icent]->Update();
    }

}

//_____________________________________________________________________________
// should yield the same results as output_eff_factors()

void plot_efficiencies(char *partic="eta",
		       int sequence = 659, int smearing = 7, int pass = 2, 
		       const char* cut = "BasicCuts3x3Asym1ChiSq2ToF2Cut", 
		       const char* type = "counts", 
		       double effmax = 1.0,
		       int verbose = 1)
{

  gROOT->SetStyle("Plain") ;
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);
  gStyle->SetFitFormat("5.4g");
  gStyle->SetMarkerStyle(20);
  gROOT->ForceStyle();
  
  int typeOfPartic = 0;

  if (!strcmp(partic,"pi0")) typeOfPartic = 7;
  else typeOfPartic = 17;

  emcEfficiency eff(2,typeOfPartic); // Run-2 pi0 or eta
  eff.setEmbedObjectsType(0);// old STAF tables
  eff.setVerbose(verbose);

  char name[400];
  //sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_nofixedwindows.root",
  sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass.root",
  //sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass_newextraloss.root",
	  location,partic,partic,smearing,sequence,pass);
  eff.readEfficiencyFile(name);

  //_____________________________________________________________________________

  EventSorting* eventSorting = (EventSorting*)eff.getEventSorting();
  const size_t ncent = eventSorting->getNCentralities(); // last one is min.bias

  size_t icent = 0 ;
  int iminbias = 10000 ;
  int centMin = -1;
  int centMax = -1;
  
  std::string khEfficiency = "hEfficiency_";
  khEfficiency += type; // "counts" or "fit"

  TH1* effic[ncent];

  TCanvas *effic_canvas[ncent];
  TLegend *leg[ncent];

  // Default values for pi0
  double ptmin = 1., ptmax = 20.;
  double effmin = 0.;
  int icolor[10]={2,5,3,50,4,6,9,18,28,1};

  // eta
  if (typeOfPartic == 17)
    {
      ptmin = 2.;
      ptmax = 20.;
      effmin = 0.; 
      // effmax = 0.4; 
      // only 4 centrality classes
      icolor[0]= 2;
      icolor[1]= 9;
      icolor[2]= 3;
      icolor[3]= 1;
    }


  char title[400];

  float y_offset = 0;

  //_____________________________________________________________________________
  // global plot

  //sprintf(title,"%s_effic_allcent_%s_%dsmear_%dpass_nofixedwindows",partic,cut,smearing,pass);
  sprintf(title,"%s_effic_allcent_%s_%dsmear_%dpass_%s",partic,cut,smearing,pass,type);
  TCanvas *effic_canvas_allcent = new TCanvas(title,title,650,500);
  effic_canvas_allcent->Range(-1.16015,-0.0746341,7.26071,0.616098);
  effic_canvas_allcent->SetLeftMargin(0.137771);
  effic_canvas_allcent->SetRightMargin(0.0309598);
  effic_canvas_allcent->SetTopMargin(0.07);
  TLegend *leg_allcent = new TLegend(0.380805,0.786017,0.930341,0.913136,NULL,"brNDC");
  leg_allcent->SetTextSize(0.034);
  leg_allcent->SetLineColor(1);
  leg_allcent->SetLineStyle(1);
  leg_allcent->SetLineWidth(1);
  leg_allcent->SetFillColor(0);

  //_____________________________________________________________________________
  // loop over centralities 

  for ( icent=0 ; icent < ncent; icent++ )
    {

      eventSorting->getCentralityClassLimits(icent,centMin,centMax);

      if (typeOfPartic == 7)
	{
	  centMin = ( centMin >= 60 ) ? 60 : centMin;
	  centMax = ( centMin >= 60 ) ? centMin+20 : centMax; // last file is 60_80.txt (we use it for all most periph. classes)
	  if ( centMax == iminbias ) { centMin = 0 ; centMax = 100 ; }
	}
      else
	{
	  if ( centMax == iminbias ) { centMin = 0 ; centMax = 92; }
	}

      y_offset = 0;
      const std::string cutstr = cut;
      effic[icent] = eff.getHistogram<TH1>(cutstr,khEfficiency,icent);

      // efficiency already scaled on file ...
//       double extraEffLoss3x3 = 0.80/0.69;
//       TString cutstr = cut; 
//       //if ( cutstr.Contains("5x5",TString::kExact) ) { effic[icent]->Scale(extraEffLoss5x5); }
//       if ( cutstr.Contains("3x3",TString::kExact) ) 
// 	{
// 	  cout << "scaling efficiency by extraEffLoss3x3=" << extraEffLoss3x3 << endl;
// 	  effic[icent]->Scale(extraEffLoss3x3); 
// 	}

      //sprintf(title,"%s_effic_cent%d_%d_%s_%dsmear_%dpass_nofixedwindows",
      //partic,centMin,centMax,cut,smearing,pass);

      sprintf(title,"%s_effic_cent%d_%d_%s_%dsmear_%dpass_%s",
	      partic,centMin,centMax,cut,smearing,pass,type);
      effic[icent]->SetTitle(title);
      effic[icent]->SetMinimum(effmin);
      effic[icent]->SetMaximum(effmax+y_offset);//0.6);
      //effic[icent]->Print("all");
      effic[icent]->GetXaxis()->SetRangeUser(ptmin,ptmax);
      effic[icent]->GetXaxis()->SetRange(ptmin/effic[icent]->GetXaxis()->GetBinWidth(0),
      				 ptmax/effic[icent]->GetXaxis()->GetBinWidth(0));
      effic[icent]->GetXaxis()->SetTitle("p_{T}(GeV/c)");
      sprintf(title,"Efficiency (%d-%d%%) = N_{meas.cut} / N_{true}",centMin,centMax);
      effic[icent]->GetYaxis()->SetTitle(title);
      effic[icent]->GetXaxis()->SetTitleSize(0.05);
      effic[icent]->GetYaxis()->SetTitleSize(0.05);
      effic[icent]->GetXaxis()->SetTitleOffset(0.9);
      effic[icent]->GetYaxis()->SetTitleOffset(1.2);
      effic[icent]->SetMarkerStyle(20);
      effic[icent]->SetMarkerSize(1.4);
      effic[icent]->SetMarkerColor(icolor[icent]);
      
      //sprintf(title,"%s_effic_cent%d_%d_%s_%dsmear_%dpass_nofixedwindows",partic,centMin,centMax,cut,smearing,pass);
      sprintf(title,"%s_effic_cent%d_%d_%s_%dsmear_%dpass",partic,centMin,centMax,cut,smearing,pass);
      effic_canvas[icent] = new TCanvas(title,title,650,500);
      effic_canvas[icent]->Range(-1.16015,-0.0746341,7.26071,0.616098);
      effic_canvas[icent]->SetLeftMargin(0.137771);
      effic_canvas[icent]->SetRightMargin(0.0309598);
      effic_canvas[icent]->SetTopMargin(0.07);      
      effic_canvas[icent]->cd();
            
      cout << " plotting " << effic[icent]->GetTitle() << endl;
      effic[icent]->Draw("");
      //effic[icent]->Print("all");

      effic_canvas_allcent->cd();
      if (icent==0) effic[icent]->Draw("");
      else effic[icent]->Draw("same");

      leg[icent] = new TLegend(0.380805,0.786017,0.930341,0.913136,NULL,"brNDC");
      leg[icent]->SetTextSize(0.034);
      leg[icent]->SetLineColor(1);
      leg[icent]->SetLineStyle(1);
      leg[icent]->SetLineWidth(1);
      leg[icent]->SetFillColor(0);
      sprintf(title,"Efficiency #%s (%d-%d%%) -- %d%% smearing",partic,centMin,centMax,smearing);
      leg[icent]->AddEntry(effic[icent],title, "P");
      leg_allcent->AddEntry(effic[icent],title, "P");
      effic_canvas[icent]->cd();
      leg[icent]->Draw();
      effic_canvas[icent]->Update();
      
// 	  fconv->cd(cutcent_7percentSmear);
	  
// 	  hEffic_7percentSmear[icent] = static_cast<TH1F*>hEfficiency_counts->Clone() ;//(gDirectory->Get("hEfficiency_counts"));
// 	  hEffic_7percentSmear[icent]->SetDirectory(gROOT);
// 	  hEffic_7percentSmear[icent]->SetTitle(cutcent_7percentSmear);
// 	  hEffic_7percentSmear[icent]->SetMarkerStyle(21);
// 	  hEffic_7percentSmear[icent]->SetMarkerSize(1.4);
// 	  hEffic_7percentSmear[icent]->SetMarkerColor(4);

// 	  effic_canvas[icent]->cd();
// 	  hEffic_7percentSmear[icent]->Draw("same");
// 	  cout << " plotting " << hEffic_7percentSmear[icent]->GetTitle() << endl;
// 	  leg[icent]->AddEntry(hEffic_7percentSmear[icent],"Effic. conversion 7percentSmear", "P");

// 	  effic_canvas[icent]->Update();

// 	  //_____________________________________________________________________________
// 	  // ratio 5percentSmear/7percentSmear
	  
// 	  ratio_effics_5_7percentSmear[icent] = new TCanvas(title2,title2,650,500);
// 	  ratio_effics_5_7percentSmear[icent]->Range(-1.16015,-0.0746341,7.26071,0.616098);
// 	  ratio_effics_5_7percentSmear[icent]->SetFillColor(0);
// 	  ratio_effics_5_7percentSmear[icent]->SetBorderMode(0);
// 	  ratio_effics_5_7percentSmear[icent]->SetBorderSize(2);
// 	  ratio_effics_5_7percentSmear[icent]->SetLeftMargin(0.137771);
// 	  ratio_effics_5_7percentSmear[icent]->SetRightMargin(0.0309598);
// 	  ratio_effics_5_7percentSmear[icent]->SetTopMargin(0.07);
// 	  ratio_effics_5_7percentSmear[icent]->SetFrameFillColor(0);
// 	  ratio_effics_5_7percentSmear[icent]->SetFrameBorderMode(0);
	  
// 	  hratio_effics_5_7percentSmear[icent] = (TH1F*)effic[icent]->Clone();
// 	  hratio_effics_5_7percentSmear[icent]->SetDirectory(gROOT);
// 	  hratio_effics_5_7percentSmear[icent]->SetTitle(title2);
// 	  hratio_effics_5_7percentSmear[icent]->Divide(hEffic_7percentSmear[icent]);
// 	  hratio_effics_5_7percentSmear[icent]->SetMarkerStyle(21);
// 	  hratio_effics_5_7percentSmear[icent]->SetMarkerSize(1.4);
// 	  hratio_effics_5_7percentSmear[icent]->SetMarkerColor(1);
// 	  hratio_effics_5_7percentSmear[icent]->GetXaxis()->SetTitle("#pi^{0} p_{T}(GeV/c)");
// 	  hratio_effics_5_7percentSmear[icent]->GetYaxis()->SetTitle("Efficiency conversion 5percentSmear / 7percentSmear");
// 	  hratio_effics_5_7percentSmear[icent]->GetXaxis()->SetTitleSize(0.05);
// 	  hratio_effics_5_7percentSmear[icent]->GetYaxis()->SetTitleSize(0.05);
// 	  hratio_effics_5_7percentSmear[icent]->GetXaxis()->SetTitleOffset(0.9);
// 	  hratio_effics_5_7percentSmear[icent]->GetYaxis()->SetTitleOffset(1.2);
// 	  ratio_effics_5_7percentSmear[icent]->cd();
// 	  cout << " plotting " << hratio_effics_5_7percentSmear[icent]->GetTitle() << endl;
// 	  hratio_effics_5_7percentSmear[icent]->Draw("");
// 	  hratio_effics_5_7percentSmear[icent]->Fit("pol0");

// 	  ratio_effics_5_7percentSmear[icent]->Update();

	}

  effic_canvas_allcent->cd();
  leg_allcent->Draw();
  effic_canvas_allcent->Update();

}

// //_____________________________________________________________________________
// void plot_efficiencies(char *partic="eta",
// 		       int sequence = 659, 
// 		       //int smearing = 7, 
// 		       int pass = 2, 
// 		       char *cut = "BasicCuts3x3Asym1ChiSq1ToF1Cut", 
// 		       int verbose = 1)
// {
//   std::string cut = cut;

//   gROOT->SetStyle("Plain") ;
//   gStyle->SetOptStat(0);
//   gStyle->SetOptFit(1111);
//   gStyle->SetFitFormat("5.4g");
//   gStyle->SetMarkerStyle(20);
//   gROOT->ForceStyle();
  
//   int typeOfPartic = 0;

//   if (!strcmp(partic,"pi0")) typeOfPartic = 7;
//   else typeOfPartic = 17;

//   emcEfficiency eff1(2,typeOfPartic); // Run-2 pi0 or eta
//   eff1.setEmbedObjectsType(0);// old STAF tables
//   eff1.setVerbose(verbose);
//   char name1[400];
//   int smearing = 5;
//   sprintf(name1,"%s/efficiency_%s_%dpercent_smear_%d_%dpass.root",location,partic,smearing,sequence,pass);
//   eff1.readEfficiencyFile(name1);

//   emcEfficiency eff2(2,typeOfPartic); // Run-2 pi0 or eta
//   eff2.setEmbedObjectsType(0);// old STAF tables
//   eff2.setVerbose(verbose);
//   char name2[400];
//   int smearing = 7;
//   sprintf(name2,"%s/efficiency_%s_%dpercent_smear_%d_%dpass.root",location,partic,smearing,sequence,pass);
//   eff2.readEfficiencyFile(name2);

//   //_____________________________________________________________________________

//   EventSorting* eventSorting = (EventSorting*)eff1.getEventSorting();
//   const size_t ncent = eventSorting->getNCentralities(); // last one is min.bias

//   size_t icent = 0 ;
//   int iminbias = 10000 ;
//   int centMin = -1;
//   int centMax = -1;

//   const int ncuts = 5;
  
//   TCanvas *effic[ncent][ncuts];
//   TLegend *leg[ncent][ncuts];

//   TH1F* hEffic_5percentSmear[ncent][ncuts];
//   TH1F* hEffic_7percentSmear[ncent][ncuts];

//   TCanvas *ratio_effics_5_7percentSmear[ncent][ncuts];
//   TH1F* hratio_effics_5_7percentSmear[ncent][ncuts];

//   char cutcent_5percentSmear[400];
//   char cutcent_7percentSmear[400];
//   char cut[100];
//   char cent[10];
//   char title1[400];
//   char title2[400];

//   // To select a given centrality or cut:
//   //const int icent = 0;
//   //const int icut = 2;

//   float y_offset = 0;

//   //_____________________________________________________________________________
//   // loops: 
//   //
//   // [icent][0]: no cuts
//   // [icent][1]: basic cuts
//   // [icent][2]: "favorite" cuts

//   for ( icent=0 ; icent < ncent; icent++ )
//     {

//       for (int icut=0; icut<ncuts; icut++)
//       	{

// 	  y_offset = 0;

// 	  if (icent == 0) // periph
// 	    {
// 	      sprintf(cent,"C8");
// 	      sprintf(title1,"periph_effics");
// 	      sprintf(title2,"ratio_conv_on_off_%s",title1);
// 	    }
// 	  if (icent == 1)// central
// 	    {
// 	      sprintf(cent,"C0");
// 	      sprintf(title1,"central_effics");
// 	      sprintf(title2,"ratio_conv_on_off_%s",title1);
// 	    }
	  
// 	  if (icut == 0) // No cut
// 	    {
// 	      sprintf(cutcent_5percentSmear,"NoCut/%s",cent);
// 	      sprintf(cutcent_7percentSmear,"ConversionOffCut/%s",cent);
// 	      sprintf(title1,"%s_nocut",title1);
// 	      sprintf(title2,"%s_nocut",title2);
// 	      y_offset = 0.4;
// 	    }
// 	  if (icut == 1) // Basic cuts
// 	    {
// 	      sprintf(cutcent_5percentSmear,"BasicCuts_FiduNoW3DeadWarnEnergyCosCut/%s",cent);
// 	      sprintf(cutcent_7percentSmear,"BasicCutsConvOffCut/%s",cent);
// 	      sprintf(title1,"%s_basiccuts",title1);
// 	      sprintf(title2,"%s_basiccuts",title2);
// 	    }
// 	  if (icut == 2) // Full cuts
// 	    {
// 	      sprintf(cutcent_5percentSmear,"BasicCutsAsym1ChiSq1ToF1Cut/%s",cent);
// 	      sprintf(cutcent_7percentSmear,"BasicCutsAsym1ChiSq1ToF1ConvOffCut/%s",cent);
// 	      sprintf(title1,"%s_fullcuts",title1);
// 	      sprintf(title2,"%s_fullcuts",title2);
// 	    }
// 	  if (icut == 3) // Conversion only
// 	    {
// 	      sprintf(cutcent_7percentSmear,"NoCut/%s",cent);
// 	      sprintf(cutcent_5percentSmear,"ElectronEvtsCut/%s",cent);
// 	      sprintf(title1,"%s_electevts_nocut",title1);
// 	      sprintf(title2,"%s_electevts_nocut",title2);
// 	    }
// 	  if (icut == 4) // Conversion only (full cuts)
// 	    {
// 	      sprintf(cutcent_7percentSmear,"BasicCutsAsym1ChiSq1ToF1Cut/%s",cent);
// 	      sprintf(cutcent_5percentSmear,"BasicCutsAsym1ChiSq1ToF1ElectronEvtsCut/%s",cent);
// 	      sprintf(title1,"%s_electevts_fullcuts",title1);
// 	      sprintf(title2,"%s_electevts_fullcuts",title2);
// 	    }
	  	  
// 	  effic[icent][icut] = new TCanvas(title1,title1,650,500);
// 	  effic[icent][icut]->Range(-1.16015,-0.0746341,7.26071,0.616098);
// 	  effic[icent][icut]->SetLeftMargin(0.137771);
// 	  effic[icent][icut]->SetRightMargin(0.0309598);
// 	  effic[icent][icut]->SetTopMargin(0.07);
	  
// 	  leg[icent][icut] = new TLegend(0.459752,0.779661,0.939628,0.970339,NULL,"brNDC");
// 	  leg[icent][icut]->SetTextSize(0.034);
// 	  leg[icent][icut]->SetLineColor(1);
// 	  leg[icent][icut]->SetLineStyle(1);
// 	  leg[icent][icut]->SetLineWidth(1);
// 	  leg[icent][icut]->SetFillColor(0);
// 	  effic[icent][icut]->cd();
// 	  leg[icent][icut]->Draw();

// 	  //if (icut<3)
// 	  //  {	  
// 	      fconv->cd(cutcent_5percentSmear);
	      
// 	      hEffic_5percentSmear[icent][icut] = static_cast<TH1F*>hEfficiency_counts->Clone();
// 	      hEffic_5percentSmear[icent][icut]->SetDirectory(gROOT);
// 	      //hEffic_5percentSmear[icent][icut]->SetName(cutcent_5percentSmear);
// 	      hEffic_5percentSmear[icent][icut]->SetTitle(cutcent_5percentSmear);
// 	      hEffic_5percentSmear[icent][icut]->SetMinimum(0.0);
// 	      hEffic_5percentSmear[icent][icut]->SetMaximum(1.2+y_offset);//0.6);
// 	      hEffic_5percentSmear[icent][icut]->GetXaxis()->SetRange(2.,14.);
// 	      hEffic_5percentSmear[icent][icut]->GetXaxis()->SetTitle("#pi^{0} p_{T}(GeV/c)");
// 	      hEffic_5percentSmear[icent][icut]->GetYaxis()->SetTitle("Efficiency (N_{meas. Cut X}^{#pi^{0}} / (N_{true}^{#pi^{0}})");
// 	      hEffic_5percentSmear[icent][icut]->GetXaxis()->SetTitleSize(0.05);
// 	      hEffic_5percentSmear[icent][icut]->GetYaxis()->SetTitleSize(0.05);
// 	      hEffic_5percentSmear[icent][icut]->GetXaxis()->SetTitleOffset(0.9);
// 	      hEffic_5percentSmear[icent][icut]->GetYaxis()->SetTitleOffset(1.2);
// 	      hEffic_5percentSmear[icent][icut]->SetMarkerStyle(20);
// 	      hEffic_5percentSmear[icent][icut]->SetMarkerSize(1.4);
// 	      hEffic_5percentSmear[icent][icut]->SetMarkerColor(2);
	      
// 	      effic[icent][icut]->cd();
// 	      cout << " plotting " << hEffic_5percentSmear[icent][icut]->GetTitle() << endl;
// 	      hEffic_5percentSmear[icent][icut]->Draw("");
// 	      leg[icent][icut]->AddEntry(hEffic_5percentSmear[icent][icut],"Effic. conversion 5percentSmear", "P");
// 	      leg[icent][icut]->Draw();
// 	      //  }

// 	  fconv->cd(cutcent_7percentSmear);
	  
// 	  hEffic_7percentSmear[icent][icut] = static_cast<TH1F*>hEfficiency_counts->Clone() ;//(gDirectory->Get("hEfficiency_counts"));
// 	  hEffic_7percentSmear[icent][icut]->SetDirectory(gROOT);
// 	  hEffic_7percentSmear[icent][icut]->SetTitle(cutcent_7percentSmear);
// 	  hEffic_7percentSmear[icent][icut]->SetMarkerStyle(21);
// 	  hEffic_7percentSmear[icent][icut]->SetMarkerSize(1.4);
// 	  hEffic_7percentSmear[icent][icut]->SetMarkerColor(4);

// 	  effic[icent][icut]->cd();
// 	  hEffic_7percentSmear[icent][icut]->Draw("same");
// 	  cout << " plotting " << hEffic_7percentSmear[icent][icut]->GetTitle() << endl;
// 	  leg[icent][icut]->AddEntry(hEffic_7percentSmear[icent][icut],"Effic. conversion 7percentSmear", "P");

// 	  effic[icent][icut]->Update();

// 	  //_____________________________________________________________________________
// 	  // ratio 5percentSmear/7percentSmear
	  
// 	  ratio_effics_5_7percentSmear[icent][icut] = new TCanvas(title2,title2,650,500);
// 	  ratio_effics_5_7percentSmear[icent][icut]->Range(-1.16015,-0.0746341,7.26071,0.616098);
// 	  ratio_effics_5_7percentSmear[icent][icut]->SetFillColor(0);
// 	  ratio_effics_5_7percentSmear[icent][icut]->SetBorderMode(0);
// 	  ratio_effics_5_7percentSmear[icent][icut]->SetBorderSize(2);
// 	  ratio_effics_5_7percentSmear[icent][icut]->SetLeftMargin(0.137771);
// 	  ratio_effics_5_7percentSmear[icent][icut]->SetRightMargin(0.0309598);
// 	  ratio_effics_5_7percentSmear[icent][icut]->SetTopMargin(0.07);
// 	  ratio_effics_5_7percentSmear[icent][icut]->SetFrameFillColor(0);
// 	  ratio_effics_5_7percentSmear[icent][icut]->SetFrameBorderMode(0);
	  
// 	  hratio_effics_5_7percentSmear[icent][icut] = (TH1F*)hEffic_5percentSmear[icent][icut]->Clone();
// 	  hratio_effics_5_7percentSmear[icent][icut]->SetDirectory(gROOT);
// 	  hratio_effics_5_7percentSmear[icent][icut]->SetTitle(title2);
// 	  hratio_effics_5_7percentSmear[icent][icut]->Divide(hEffic_7percentSmear[icent][icut]);
// 	  hratio_effics_5_7percentSmear[icent][icut]->SetMarkerStyle(21);
// 	  hratio_effics_5_7percentSmear[icent][icut]->SetMarkerSize(1.4);
// 	  hratio_effics_5_7percentSmear[icent][icut]->SetMarkerColor(1);
// 	  hratio_effics_5_7percentSmear[icent][icut]->GetXaxis()->SetTitle("#pi^{0} p_{T}(GeV/c)");
// 	  hratio_effics_5_7percentSmear[icent][icut]->GetYaxis()->SetTitle("Efficiency conversion 5percentSmear / 7percentSmear");
// 	  hratio_effics_5_7percentSmear[icent][icut]->GetXaxis()->SetTitleSize(0.05);
// 	  hratio_effics_5_7percentSmear[icent][icut]->GetYaxis()->SetTitleSize(0.05);
// 	  hratio_effics_5_7percentSmear[icent][icut]->GetXaxis()->SetTitleOffset(0.9);
// 	  hratio_effics_5_7percentSmear[icent][icut]->GetYaxis()->SetTitleOffset(1.2);
// 	  ratio_effics_5_7percentSmear[icent][icut]->cd();
// 	  cout << " plotting " << hratio_effics_5_7percentSmear[icent][icut]->GetTitle() << endl;
// 	  hratio_effics_5_7percentSmear[icent][icut]->Draw("");
// 	  hratio_effics_5_7percentSmear[icent][icut]->Fit("pol0");

// 	  ratio_effics_5_7percentSmear[icent][icut]->Update();

// 	}
//   }

// }


//_____________________________________________________________________________
// 

void determine_fidudeadwarn_ratios(char *partic="eta",
				   int sequence = 659, int smearing = 7, int pass = 2, 
				   size_t icent = 9,
				   int verbose = 1)
{
  gROOT->SetStyle("Plain") ;
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1111);
  gStyle->SetFitFormat("5.4g");
  gStyle->SetMarkerStyle(20);
  gROOT->ForceStyle();

  int typeOfPartic = 0;

  if (!strcmp(partic,"pi0")) typeOfPartic = 7;
  else typeOfPartic = 17;

  emcEfficiency eff(2,typeOfPartic); // Run-2 pi0 or eta
  eff.setVerbose(verbose);

  char name[400];
  sprintf(name,"%s/efficiencies_%s_run2/efficiency_%s_%dpercent_smear_%d_%dpass.root",
	  location,partic,partic,smearing,sequence,pass);
  eff.readEfficiencyFile(name);

  char title[400];
  double ptmin = 1.;
  double ptmax = 10.;

  std::string khEfficiency = "hEfficiency_counts";

  std::string cutstr = "NoCut";
  TH1F* nocut = (TH1F*)eff.getHistogram<TH1>(cutstr,khEfficiency,icent);
  cutstr = "FiduDeadWarn5x5Cut";
  TH1F* fidu5x5 = (TH1F*)eff.getHistogram<TH1>(cutstr,khEfficiency,icent);
  cutstr = "FiduDeadWarn3x3Cut";
  TH1* fidu3x3 = (TH1F*)eff.getHistogram<TH1>(cutstr,khEfficiency,icent);

  if (!nocut || !fidu5x5 || !fidu3x3) return;

  TH1F* ratio3x3_nocut = (TH1F*)fidu3x3->Clone();
  ratio3x3_nocut->Divide(nocut);
  ratio3x3_nocut->SetDirectory(gROOT);
  sprintf(title,"ratio3x3_nocut_%s_cent%d_%dsmear_%dpass",partic,icent,smearing,pass);
  TCanvas *ratio3x3nocut = new TCanvas(title,title,650,500);
  ratio3x3nocut->cd();
  ratio3x3_nocut->Draw();
  ratio3x3_nocut->Fit("pol0","R","",ptmin,ptmax); // Saskia finds: 0.80
  ratio3x3nocut->Update();
  
  TH1F* ratio5x5_nocut = (TH1F*)fidu5x5->Clone();
  ratio5x5_nocut->Divide(nocut);
  ratio5x5_nocut->SetDirectory(gROOT);
  sprintf(title,"ratio5x5_nocut_%s_cent%d_%dsmear_%dpass",partic,icent,smearing,pass);
  TCanvas *ratio5x5nocut = new TCanvas(title,title,650,500);
  ratio5x5nocut->cd();
  ratio5x5_nocut->Draw();
  ratio5x5_nocut->Fit("pol0","R","",ptmin,ptmax); // Saskia finds: 0.66
  ratio5x5nocut->Update();

  TH1F* ratio5x5_3x3 = (TH1F*)fidu5x5->Clone();
  ratio5x5_3x3->Divide(fidu3x3);
  ratio5x5_3x3->SetDirectory(gROOT);
  sprintf(title,"ratio5x5_3x3_%s_cent%d_%dsmear_%dpass",partic,icent,smearing,pass);
  TCanvas *ratio5x5o3x3 = new TCanvas(title,title,650,500);
  ratio5x5o3x3->cd(); 
  ratio5x5_3x3->Draw(); 
  ratio5x5_3x3->Fit("pol0","R","",ptmin,ptmax); // Saskia finds: 0.825
  ratio5x5o3x3->Update(); 

}
