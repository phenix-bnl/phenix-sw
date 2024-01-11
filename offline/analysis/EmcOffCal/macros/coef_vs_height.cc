void coef_vs_height(){
  //code for a special one-time check Edward wanted
  TGraph * coef_vs_height[8];
  c1 = new TCanvas("c1","c1",400,400);

  TFile *fin = TFile::Open("height_vs_calib.root");
  char hname[200];
  char cname[200];
  char SaveAsName[200];
  for(int i_as = 0; i_as < 8; i_as++){
    coef_vs_height[i_as] = new TGraph();

    sprintf(hname,"height_study%d",i_as);
    TH2D *hh = (TH2D *)fin->Get(hname);
    sprintf(cname,"h2_coef%d",i_as);
    TH2D *hc = (TH2D *)fin->Get(cname);
    
    int npoint = 0;
    for(int i_x = 1; i_x < hh->GetNbinsX() + 1; i_x++){
      for(int i_y = 1; i_y < hh->GetNbinsY() + 1; i_y++){
	if(hh->GetBinContent(i_x,i_y)<1) continue;
	coef_vs_height[i_as]->SetPoint(npoint,hh->GetBinContent(i_x,i_y),hc->GetBinContent(i_x,i_y));
	npoint++;
      }
    }
    
    c1->cd();
    coef_vs_height[i_as]->SetMarkerStyle(7);
    coef_vs_height[i_as]->Draw("");
    coef_vs_height[i_as]->GetXaxis()->SetTitle("height of gaussian fit to peak");
    coef_vs_height[i_as]->GetYaxis()->SetTitle("calibration coefficient");
    coef_vs_height[i_as]->Draw("AP");

    sprintf(SaveAsName,"coef_vs_heigh%d.C",i_as);
    c1->SaveAs(SaveAsName);
    c1->Clear();
  }

  return;
}
