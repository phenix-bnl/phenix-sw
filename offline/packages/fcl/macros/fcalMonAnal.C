#include <iostream>

#define NCOLS 9
#define NROWS 10
#define NDET 2

TH1F* hPreMean[NDET][NROWS][NCOLS];
TH1F* hPreMax[NDET][NROWS][NCOLS];
TH1F* hPreRMS[NDET][NROWS][NCOLS];
TH1F* hPreTop1[NDET][NROWS][NCOLS];
TH1F* hPreBottom1[NDET][NROWS][NCOLS];

TH2* hPreMaxSummary[NDET];
TH2* hPreBottom1Summary[NDET];
TH2* hPreTop1Summary[NDET];

void fcalMonAnal() {

  TFile *outfile = new TFile("FcalPreAnalysis_18Feb2005.hist.root","RECREATE");
  
  char* compass[] = {"north","south"};

  //define the histos...
  ostringstream tmp;
  for(int idet = 0; idet < NDET; idet++) {
    for(int icol = 0; icol < NCOLS; icol++) {
      for(int irow = 0; irow < NROWS; irow++) {
        tmp << "fcal_" << compass[idet] << "_pre_MEAN_" << irow << "_" << icol << ends;
        hPreMean[idet][irow][icol] = new TH1F(tmp.str().c_str(),tmp.str().c_str(),1001, -0.5,1000.5);
	tmp.str("");
	tmp << "fcal_" << compass[idet] << "_pre_MAX_" << irow << "_" << icol << ends;
	hPreMax[idet][irow][icol] = new TH1F(tmp.str().c_str(),tmp.str().c_str(),1001, -0.5,1000.5);
	tmp.str("");
	tmp << "fcal_" << compass[idet] << "_pre_RMS_" << irow << "_" << icol << ends;
        hPreRMS[idet][irow][icol] = new TH1F(tmp.str().c_str(),tmp.str().c_str(),1001, -0.5,1000.5);
        tmp.str("");
	tmp << "fcal_" << compass[idet] << "_pre_BOTTOM1_" << irow << "_" << icol << ends;
        hPreBottom1[idet][irow][icol] = new TH1F(tmp.str().c_str(),tmp.str().c_str(),1001, -0.5,1000.5);
        tmp.str("");
	tmp << "fcal_" << compass[idet] << "_pre_TOP1_" << irow << "_" << icol << ends;
        hPreTop1[idet][irow][icol] = new TH1F(tmp.str().c_str(),tmp.str().c_str(),1001, -0.5,1000.5);
        tmp.str("");
      }
    }
    tmp << "fcal_" << compass[idet] << "_pre_TOP1_summary" << ends;
    hPreTop1Summary[idet] = new TH2F(tmp.str().c_str(),tmp.str().c_str(),NCOLS,-0.5,8.5,NROWS,-0.5,9.5);
    tmp.str("");
    tmp << "fcal_" << compass[idet] << "_pre_MAX_summary" << ends;
    hPreMaxSummary[idet] =  new TH2F(tmp.str().c_str(),tmp.str().c_str(),NCOLS,-0.5,8.5,NROWS,-0.5,9.5);
    tmp.str("");
    tmp << "fcal_" << compass[idet] << "_pre_BOTTOM1_summary" << ends;
    hPreBottom1Summary[idet] =  new TH2F(tmp.str().c_str(),tmp.str().c_str(),NCOLS,-0.5,8.5,NROWS,-0.5,9.5);
    tmp.str("");
  }

  //Here we go through all the run5 online monitoring since the code fixes and
  //fill distributions of the mean, maxbin, top and bottom 1% of the pre's for each channel

  TString mDir = "/home/phnxrc/online_monitoring/save/done/";
  TString mExt = "va020_9081.root";
  TString mMatch = "Run_15";

  void *pDir = gSystem->OpenDirectory(mDir.Data());
  if(!pDir){
    cerr << "##Cannot open directory " << mDir.Data() << endl;
    cerr << "##Goodbye" << endl;
    exit(1);
  }

  const char* fileName(0);
  Int_t count(0);

  while((fileName = gSystem->GetDirEntry(pDir))){
    if(strcmp(fileName,".")==0 || strcmp(fileName,"..")==0) continue;

    if(strstr(fileName,mMatch.Data()) && strstr(fileName,mExt.Data())){
      char* fullFile = gSystem->ConcatFileName(mDir.Data(),fileName); 
      cout << "<I>Processing file " << fullFile << endl;
      doIt(fullFile);
      count++;
    }
  }

  cout << "Processed " << count << " files" << endl;

  // Now that we have all the distributions we take their mean (or maxbin) values 
  // and a "spread" variable to put in the DB using compact 2d histos
  for(int idet = 0; idet < NDET; idet++) {
    for(int icol = 0; icol < NCOLS; icol++) {
      for(int irow = 0; irow < NROWS; irow++) {

	float max = hPreMax[idet][irow][icol]->GetBinCenter(hPreMax[idet][irow][icol]->GetMaximumBin());
	float lowmax = max/2.;
	float bottom1 = hPreBottom1[idet][irow][icol]->GetBinCenter(hPreBottom1[idet][irow][icol]->GetMaximumBin());
	float lowbottom1 = bottom1/2.;
	float top1 = hPreTop1[idet][irow][icol]->GetMean();
	float hightop1cut = hPreTop1[idet][irow][icol]->GetRMS();

	hPreMaxSummary[idet]->SetBinContent(icol+1,irow+1,max);
	hPreMaxSummary[idet]->SetBinError(icol+1,irow+1,lowmax);
	hPreBottom1Summary[idet]->SetBinContent(icol+1,irow+1,bottom1);
	hPreBottom1Summary[idet]->SetBinError(icol+1,irow+1,lowbottom1);
	hPreTop1Summary[idet]->SetBinContent(icol+1,irow+1,top1);
	hPreTop1Summary[idet]->SetBinError(icol+1,irow+1,hightop1cut);

      }
    }
  }

  outfile->Write();
  outfile->Close();

}




void doIt(char* filename = "/home/phnxrc/online_monitoring/save/done/Run_154929_va020_9081.root") {

  TFile *f = new TFile(filename);

  char* compass[] = {"north","south"};

  TH1* hPreDist;

  ostringstream tmp;
  for(int idet = 0; idet < NDET; idet++) {
    for(int icol = 0; icol < NCOLS; icol++) {
      for(int irow = 0; irow < NROWS; irow++) {
        tmp << "fcal_" << compass[idet] << "_loadc_pre_mod_" << irow << "_" << icol << ends;
        hPreDist = (TH1*)f->Get(tmp.str().c_str());
	if(!hPreDist) {
	  if(idet == 0 && irow == 0 && icol == 0) cout << "\t This file contains no Fcal histos.  Skipping..." << endl;
	  continue;
	}

        hPreDist->SetDirectory(gROOT);
        tmp.str("");

	float mean = hPreDist->GetMean();
	int maxbin = hPreDist->GetMaximumBin();
	float max = hPreDist->GetBinCenter(maxbin);
	float rms = hPreDist->GetRMS();

        float sum = 0.;
        float bottom1 = -999.;
	float total = hPreDist->Integral();
	if(total == 0.) continue;
	int ctr = 1;
	while (sum/total < 0.01) {
	  bottom1 = hPreDist->GetBinCenter(ctr);
	  sum += hPreDist->GetBinContent(ctr);
	  ctr++;
	}

	//find position of top 1%				   
	float topsum = 0.;
	float top1 = -999.;
	int topctr = hPreDist->GetNbinsX();
	while (topsum/total < 0.01) {
	  top1 = hPreDist->GetBinCenter(topctr);
	  topsum += hPreDist->GetBinContent(topctr);
	  topctr--;
	}

	//Now we have them, fill the histos
	hPreMean[idet][irow][icol]->Fill(mean);
	hPreMax[idet][irow][icol]->Fill(max);
	hPreRMS[idet][irow][icol]->Fill(rms);
	hPreBottom1[idet][irow][icol]->Fill(bottom1);
	hPreTop1[idet][irow][icol]->Fill(top1);

      }
    }
  }

  f->Close();
}



