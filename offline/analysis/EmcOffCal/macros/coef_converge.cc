void coef_converge(int n_itr){
  TGraph * coef_diff_sigma[8];
  TGraph * coef_diff_mean[8];

  TH1F * _coef_diff = new TH1F("coef_diff","coef_diff",50,-1,1);

  c1 = new TCanvas("c1","c1",1200,800);

  TFile *fin[7];
  fin[0] = TFile::Open("output/calib/coeff_hists1.root");
  fin[1] = TFile::Open("output/calib/coeff_hists2.root");
  fin[2] = TFile::Open("output/calib/coeff_hists3.root");
  fin[3] = TFile::Open("output/calib/coeff_hists4.root");
  fin[4] = TFile::Open("output/calib/coeff_hists5.root");
  fin[5] = TFile::Open("output/calib/coeff_hists6.root");
  fin[6] = TFile::Open("output/calib/coeff_hists7.root");

  for(int i_as = 0; i_as < 8 ; i_as++){
    coef_diff_sigma[i_as] = new TGraph();
    coef_diff_sigma[i_as]->SetMarkerStyle(22);
    coef_diff_sigma[i_as]->SetMarkerColor(2);
    coef_diff_mean[i_as] = new TGraph();
    coef_diff_mean[i_as]->SetMarkerStyle(22);
    coef_diff_mean[i_as]->SetMarkerColor(2);
  }

  char hname[200];
  char SaveAsName[200];
  for(int i_as = 0; i_as < 8; i_as++){
    int n_point = 0;
    for(int i_itr = 1; i_itr < n_itr; i_itr++){

      sprintf(hname,"h2_coef%d",i_as);
      TH2D *hc_after = (TH2D *)fin[i_itr]->Get(hname);
      TH2D *hc_before = (TH2D *)fin[i_itr - 1]->Get(hname);
    
      for(int i_x = 1; i_x < hc_before->GetNbinsX() + 1; i_x++){
	for(int i_y = 1; i_y < hc_before->GetNbinsY() + 1; i_y++){
	  if(hc_before->GetBinContent(i_x,i_y)<=0) continue;
	  if(hc_after->GetBinContent(i_x,i_y)<=0) continue;
	  if(hc_before->GetBinContent(i_x,i_y)>=10) continue;
	  if(hc_after->GetBinContent(i_x,i_y)>=10) continue;
	  _coef_diff->Fill(hc_after->GetBinContent(i_x,i_y) - hc_before->GetBinContent(i_x,i_y));
	  cout<<"filled"<<endl;
	}
      }
      coef_diff_sigma[i_as]->SetPoint(n_point,(float)i_itr+1,_coef_diff->GetRMS());
      coef_diff_mean[i_as]->SetPoint(n_point,(float)i_itr+1,_coef_diff->GetMean());
      n_point++;
      _coef_diff->Scale(0);
    }

    c1->cd();
    c1->Divide(2,1);

    c1->cd(1);
    coef_diff_sigma[i_as]->Draw("AP");
    coef_diff_sigma[i_as]->GetYaxis()->SetTitle("#sigma of tower coef. differences");
    coef_diff_sigma[i_as]->GetXaxis()->SetTitle("iteration");
    coef_diff_sigma[i_as]->Draw("AP");

    c1->cd(2);
    coef_diff_mean[i_as]->Draw("AP");
    coef_diff_mean[i_as]->GetYaxis()->SetTitle("Mean of tower coef. differences");
    coef_diff_mean[i_as]->GetXaxis()->SetTitle("iteration");
    
    coef_diff_mean[i_as]->Draw("AP");
  
    
    sprintf(SaveAsName,"coef_diff_RMS%d.C",i_as);
    c1->SaveAs(SaveAsName);
    sprintf(SaveAsName,"coef_diff_RMS%d.png",i_as);
    c1->SaveAs(SaveAsName);
    c1->Clear();
  }




 


  return;
}
