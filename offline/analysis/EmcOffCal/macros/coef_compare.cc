void coef_compare(){
  TGraphErrors * coef_ratio[8];
  TGraphErrors * coef_vs_coef[8];
  
  TH1F * coef_ratio_hist[8];
  TH1F * coef_diff_hist[8];

  TH1F * coef_1_hist[8];
  TH1F * coef_2_hist[8];

  TH1F * coef_err_1_hist[8];
  TH1F * coef_err_2_hist[8];

  TF1 * one = new TF1("one","x",0,10);
  one->SetLineColor(2);
  
  c1 = new TCanvas("c1","c1",400,400);

  TFile *fin_old = TFile::Open("coeff_hists_old.root");
  TFile *fin_new = TFile::Open("coeff_hists_new.root");

  char hname[200];
  char SaveAsName[200];
  for(int i_as = 0; i_as < 8; i_as++){
    coef_ratio[i_as] = new TGraphErrors();
    coef_vs_coef[i_as] = new TGraphErrors();

    coef_ratio_hist[i_as] = new TH1F("coef_ratio",";a^{post}/a^{pre}",50,0.6,1.4);
    //coef_diff_hist[i_as] = new TH1F("coef_diff",";#frac{a_{post-cryo} - a_{pre-cryo}}{#sqrt{#delta^{2}(a_{post-cryo}) + #delta^{2}(a_{pre-cryo})}",20,-6.5,6.5);
    coef_diff_hist[i_as] = new TH1F("coef_diff",";(a^{post} - a^{pre}) / (#delta^{2}(a^{post}) + #delta^{2}(a^{pre}))",20,-6.5,6.5);

    if(i_as==4||i_as==5){
      coef_1_hist[i_as] = new TH1F("coef_1_hist","coef_1_hist",75,0.6,1.3);
      coef_2_hist[i_as] = new TH1F("coef_2_hist","coef_2_hist",75,0.6,1.3);
    } else{
      if(i_as!=6){
	coef_1_hist[i_as] = new TH1F("coef_1_hist","coef_1_hist",100,0.5,0.9);
	coef_2_hist[i_as] = new TH1F("coef_2_hist","coef_2_hist",100,0.5,0.9);
      } else{
	coef_1_hist[i_as] = new TH1F("coef_1_hist","coef_1_hist",100,0.7,1.1);
	coef_2_hist[i_as] = new TH1F("coef_2_hist","coef_2_hist",100,0.7,1.1);
      }
    }

    if(i_as==4||i_as==5){
      coef_err_1_hist[i_as] = new TH1F("coef_err_1_hist",";#delta(a)",100,0,0.3);
      coef_err_2_hist[i_as] = new TH1F("coef_err_2_hist",";#delta(a)",100,0,0.3);
      coef_err_2_hist[i_as]->SetLineColor(2);

    }else {
      coef_err_1_hist[i_as] = new TH1F("coef_err_1_hist",";#delta(a)",100,0,0.2);
      coef_err_2_hist[i_as] = new TH1F("coef_err_2_hist",";#delta(a)",100,0,0.2);
      coef_err_2_hist[i_as]->SetLineColor(2);


    }

    sprintf(hname,"h2_coef%d",i_as);
    TH2D *hc1 = (TH2D *)fin_old->Get(hname);
    TH2D *hc2 = (TH2D *)fin_new->Get(hname);
    
    sprintf(hname,"h2_coef_err%d",i_as);
    TH2D *hce1 = (TH2D *)fin_old->Get(hname);
    TH2D *hce2 = (TH2D *)fin_new->Get(hname);

    int npoint = 0;
    for(int i_x = 1; i_x < hc1->GetNbinsX() + 1; i_x++){
      for(int i_y = 1; i_y < hc1->GetNbinsY() + 1; i_y++){
	if(hc1->GetBinContent(i_x,i_y)<=0) continue;
	if(hc2->GetBinContent(i_x,i_y)<=0) continue;
	if(hc1->GetBinContent(i_x,i_y)>=10) continue;
	if(hc2->GetBinContent(i_x,i_y)>=10) continue;
	//
	coef_ratio[i_as]->SetPoint(npoint,hc1->GetBinContent(i_x,i_y),(hc2->GetBinContent(i_x,i_y))/(hc1->GetBinContent(i_x,i_y)));
	coef_ratio[i_as]->SetPointError(npoint,hce1->GetBinContent(i_x,i_y),(hc2->GetBinContent(i_x,i_y))/(hc1->GetBinContent(i_x,i_y))*sqrt(pow(hce2->GetBinContent(i_x,i_y)/hc2->GetBinContent(i_x,i_y),2)+pow(hce1->GetBinContent(i_x,i_y)/hc1->GetBinContent(i_x,i_y),2)));
	//
	coef_ratio_hist[i_as]->Fill((hc2->GetBinContent(i_x,i_y))/(hc1->GetBinContent(i_x,i_y)));
	coef_diff_hist[i_as]->Fill( (hc2->GetBinContent(i_x,i_y) - hc1->GetBinContent(i_x,i_y))/sqrt(pow(hce2->GetBinContent(i_x,i_y),2) + pow(hce1->GetBinContent(i_x,i_y),2)));
	//
	coef_vs_coef[i_as]->SetPoint(npoint,hc1->GetBinContent(i_x,i_y),hc2->GetBinContent(i_x,i_y));
	coef_vs_coef[i_as]->SetPointError(npoint,hce1->GetBinContent(i_x,i_y),hce2->GetBinContent(i_x,i_y));
	//
	coef_1_hist[i_as]->Fill(hc1->GetBinContent(i_x,i_y));
	coef_2_hist[i_as]->Fill(hc2->GetBinContent(i_x,i_y));
	coef_err_1_hist[i_as]->Fill(hce1->GetBinContent(i_x,i_y));
	coef_err_2_hist[i_as]->Fill(hce2->GetBinContent(i_x,i_y));
	npoint++;
      }
    }
    
    c1->cd();
    coef_ratio[i_as]->SetMarkerStyle(7);
    coef_ratio[i_as]->Draw("");
    coef_ratio[i_as]->GetXaxis()->SetTitle("calib. coeff. from 1st half");
    coef_ratio[i_as]->GetYaxis()->SetTitle("calib. coeff. 2nd half / calib. coeff. 1st half");
    coef_ratio[i_as]->Draw("AP");

    sprintf(SaveAsName,"coef_ratio%d.C",i_as);
    c1->SaveAs(SaveAsName);
    sprintf(SaveAsName,"coef_ratio%d.png",i_as);
    c1->SaveAs(SaveAsName);
    c1->Clear();

    c1->cd();
    coef_vs_coef[i_as]->SetMarkerStyle(7);
    coef_vs_coef[i_as]->Draw("");
    coef_vs_coef[i_as]->GetXaxis()->SetTitle("calib. coeff. from 1st half");
    coef_vs_coef[i_as]->GetYaxis()->SetTitle("calib. coeff. from 2nd half");
    coef_vs_coef[i_as]->Draw("AP");
    one->Draw("SAME");

    sprintf(SaveAsName,"coef_vs_coef%d.C",i_as);
    c1->SaveAs(SaveAsName);
    sprintf(SaveAsName,"coef_vs_coef%d.png",i_as);
    c1->SaveAs(SaveAsName);
    c1->Clear();

    c1->cd();
    coef_ratio_hist[i_as]->Draw();
   //  coef_ratio_hist[i_as]->GetXaxis()->SetTitle("calib. coeff. 2nd half / calib. coeff. 1st half");
    coef_ratio_hist[i_as]->Draw();
    
    sprintf(SaveAsName,"coef_ratio_hist%d.C",i_as);
    c1->SaveAs(SaveAsName);
    sprintf(SaveAsName,"coef_ratio_hist%d.png",i_as);
    c1->SaveAs(SaveAsName);
    c1->Clear();

    c1->cd();
    coef_1_hist[i_as]->Draw();
    coef_1_hist[i_as]->GetXaxis()->SetTitle("calib. coeff");
    coef_1_hist[i_as]->Draw();
    coef_2_hist[i_as]->SetLineColor(4);
    coef_2_hist[i_as]->Draw("SAME");
    
    sprintf(SaveAsName,"coef_compare_hist%d.C",i_as);
    c1->SaveAs(SaveAsName);
    sprintf(SaveAsName,"coef_compare_hist%d.png",i_as);
    c1->SaveAs(SaveAsName);
    c1->Clear();


 
  }

  c1->cd();
  c1->Divide(2,4);
  for(int i_as = 0; i_as < 4; i_as ++){
    c1->cd(2*i_as + 1);
    coef_ratio_hist[i_as]->Draw();
    c1->cd(2*i_as + 1 + 1);
    coef_diff_hist[i_as]->Draw();
  }
  sprintf(SaveAsName,"cryo_ratioW.png");
  c1->SaveAs(SaveAsName);
  sprintf(SaveAsName,"cryo_ratioW.eps");
  c1->SaveAs(SaveAsName);
  c1->Clear();

  c1->cd();
  c1->Divide(2,4);
  for(int i_as = 0; i_as < 4; i_as ++){
    c1->cd(2*i_as + 1);
    coef_ratio_hist[4+i_as]->Draw();
    c1->cd(2*i_as + 1 + 1);
    coef_diff_hist[4+i_as]->Draw();
  }
  sprintf(SaveAsName,"cryo_ratioE.png");
  c1->SaveAs(SaveAsName);
  sprintf(SaveAsName,"cryo_ratioE.eps");
  c1->SaveAs(SaveAsName);
  c1->Clear();

  return;
}
