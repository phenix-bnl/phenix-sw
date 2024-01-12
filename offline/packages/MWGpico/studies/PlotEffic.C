void PlotEffic(char* filename="muteffic.root")
{
  //    gROOT->Reset();  gROOT->Delete("*"); gStyle->Reset();

  int scol = 4; 
  int ncol = 2;
  float mineff = 0.;
  float maxeff = 1.05;


// Setting the sytle of our plots (mostly inehrited from a PHENIX official)
  gStyle->SetOptStat(0);
//  gStyle->SetOptFit(1111);
  gStyle->SetOptTitle(0); // remove the titles for showing plots
  gStyle->SetPalette(1);
//  gStyle->SetCanvasColor(10);
//  gStyle->SetPadColor(10);
//  gStyle->SetHistFillColor(10);
  gStyle->SetHistLineWidth(5);
//  gStyle->SetStatFontSize(0.06);
//  gStyle->SetTitleFontSize(0.075);
  gStyle->SetErrorX(0);
//  gStyle->SetTitleXSize(0.07);
//  gStyle->SetTitleYSize(0.07);
//  gStyle->SetTickLength(0.03, "X");
//  gStyle->SetTickLength(0.03, "Y");
//  gStyle->SetTitleXOffset(1.0); 
//  gStyle->SetTitleYOffset(1.0); 
//  gStyle->SetPadTickX(1);
//  gStyle->SetPadTickY(1);
//  gStyle->SetLabelOffset(0.01, "X");
//  gStyle->SetLabelOffset(0.02, "Y");
//  gStyle->SetLabelSize(0.08, "X");
//  gStyle->SetLabelSize(0.08, "Y");
  gStyle->SetPadRightMargin(0.15);
  gStyle->SetPadBottomMargin(0.15);
//
  TFile* file = new TFile(filename);

  //////////////////////////////////////////////// MutEfficPlanes
  TCanvas *C1 = new TCanvas("MutEfficPlanes","MutEfficPlanes");
  C1->cd();
  C1->SetGridy();
  C1->Draw();

  MutEfficPlanesNorthArm->SetMarkerColor(ncol);
  MutEfficPlanesNorthArm->SetMarkerStyle(20);
  MutEfficPlanesNorthArm->SetMarkerSize(0.8);
  MutEfficPlanesNorthArm->SetMaximum(maxeff);
  MutEfficPlanesNorthArm->SetMinimum(mineff);
  MutEfficPlanesNorthArm->Draw("");

  MutHitProbaNorthArm->SetMarkerColor(ncol);
  MutHitProbaNorthArm->SetMarkerStyle(24);
  MutHitProbaNorthArm->SetMarkerSize(0.8);
  MutHitProbaNorthArm->Draw("same");

  Float_t northeffic = MutEfficPlanesNorthArm->GetBinContent(1);
  Float_t northerror = MutEfficPlanesNorthArm->GetBinError(1);
  char* neffic = new char[100]; 
  sprintf (neffic,"North %1.1f +/- %1.1f",northeffic*100,northerror*100);
  cout << "North mean efficiency " << northeffic*100 << " +/- " << northerror*100 << endl;

  TText *text = new TText ;
  text->SetTextSize(0.04) ;
  text->SetTextColor(ncol);
  //  TString string = "North mean" + northeffic;
  text->DrawText(1.,1.-0.8*(1-mineff),neffic);

  MutEfficPlanesSouthArm->SetMarkerColor(scol);
  MutEfficPlanesSouthArm->SetMarkerStyle(21);
  MutEfficPlanesSouthArm->SetMarkerSize(0.8);
  MutEfficPlanesSouthArm->Draw("same");

  MutHitProbaSouthArm->SetMarkerColor(scol);
  MutHitProbaSouthArm->SetMarkerStyle(25);
  MutHitProbaSouthArm->SetMarkerSize(0.8);
  MutHitProbaSouthArm->Draw("same");

  Float_t southeffic = MutEfficPlanesSouthArm->GetBinContent(1);
  Float_t southerror = MutEfficPlanesSouthArm->GetBinError(1);
  char* seffic = new char[100]; 
  sprintf (seffic,"South %1.1f +/- %1.1f",southeffic*100,southerror*100);
  cout << "South mean efficiency " << southeffic*100 << " +/- " << southerror*100 << endl;

  text->SetTextColor(scol);
  text->DrawText(1.,1.-0.6*(1-mineff),seffic);

  // vertical lines to show mass range
  TLine *line = new TLine ;
  line->SetLineWidth(2);
  line->DrawLine(0.5,mineff,0.5,maxeff);
  line->DrawLine(6.5,mineff,6.5,maxeff);
  line->DrawLine(12.5,mineff,12.5,maxeff);

  C1->Print("MutEfficPlanes.gif","gif");

  //////////////////////////////////////////////// MutNumberHits
  TCanvas *C2 = new TCanvas("MutNumberHits","MutNumberHis");
  C2->cd();
  C2->SetGridy();
  C2->Draw();

  MutNumberHitsNorthArm->SetLineColor(ncol);
  MutNumberHitsNorthArm->Draw("");
  Float_t fmean = MutNumberHitsNorthArm->GetMean();
  char* cmean = new char[100]; 
  sprintf (cmean,"North Hits %1.2f",fmean);
  cout << cmean << endl;
  text->SetTextColor(ncol);
  text->DrawText(1.,0.5*MutNumberHitsNorthArm->GetMaximum(),cmean);

  MutNumberHitsSouthArm->SetLineColor(scol);
  MutNumberHitsSouthArm->Draw("sames");
  fmean = MutNumberHitsSouthArm->GetMean();
  sprintf (cmean,"South Hits %1.2f",fmean);
  cout << cmean << endl;
  text->SetTextColor(scol);
  text->DrawText(1.,0.4*MutNumberHitsNorthArm->GetMaximum(),cmean);

  C2->Print("MutNumberHits.gif","gif");

  //////////////////////////////////////////////// MutEfficMaps
  //  return;
  gStyle->SetOptStat(10);
  gStyle->SetOptTitle(1);

  TProfile2D* myprof = new TProfile2D*;
  char* histname = new char[20]; // to find back histogram
  char* canvname = new char[20]; // canva and output name
  char* arm = "South";
  TCanvas* mycan[6];
  int ican = 0;

  for (int iarm=1;iarm<3;iarm++) {
    
    for (int istat=1;istat<4;istat++) {
	  sprintf(canvname,"%sSta%dMaps",arm,istat);
	  cout << "Creating canvas " << ican << " " << canvname << endl;
	  mycan[ican] = new TCanvas(canvname,canvname);
	  mycan[ican]->Divide(3,2);
	  int npla = 6; if (istat==3) npla = 4;
	  for (int ihist=1;ihist<npla+1;ihist++) {
	    sprintf(histname,"MutEfficMap%sS%dP%d",arm,istat,ihist);
	    cout << "Getting Map " << histname << endl;
	    int it = ((ihist-1)%2)*3+(ihist-1)/2 +1; // reorder planes
	    mycan[ican]->cd(it);
	    myprof = (TProfile2D*)gROOT->FindObject(histname);
	    
	    // cout << " nx " << myprof->GetNbinsX() << " " << myprof->GetNbinsY() << " " << endl;
	    int BigBins = (myprof->GetNbinsX()+1)*(myprof->GetNbinsY()+1); 
	    cout << " This histo has " << myprof->GetNbinsX() << " xbins and " << myprof->GetNbinsY() << " ybins and " << myprof->GetEntries() << " tracks" << endl;
	    for (int ix=0;ix < BigBins ;ix++) {
// a trick to make empty region look different as dead region
// dead will be at -0.1 // empty will be at 0.0001 
// (otherwise they appear white even if scale starts below !) 
// (in other words : 
	      if (myprof->GetBinEntries(ix)==0) {
		myprof->SetBinContent(ix,-0.1);
		// myprof->SetBinEntries(ix,1);
	      } else if (myprof->GetBinContent(ix)==0) {
		myprof->SetBinContent(ix,0.0001);
	      } else {
		// cout << " entry " << ix << " content " << myprof->GetBinContent(ix) << " entries " << myprof->GetBinEntries(ix) << endl;
	      }
	    }
	    myprof->SetMaximum(1.);
	    myprof->SetMinimum(-0.05);
	    myprof->Draw("colz");
	  }
	  sprintf(canvname,"%sSta%dMaps.gif",arm,istat);
	  cout << "Plotting Map " << canvname << endl;
	  mycan[ican]->Print(canvname,"gif");
	  sprintf(canvname,"%sSta%dMaps.eps",arm,istat);
	  mycan[ican]->Print(canvname,"eps");
	  ican++;
    }
    arm = "North";
  }

}
