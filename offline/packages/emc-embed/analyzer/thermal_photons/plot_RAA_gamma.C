//_____________________________________________________________________________
//

void plot_RAA_gamma( bool logy = true )
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  TGraphErrors *eband;
  e.setFit(false);
  e.keepPlot(false);

  char *title="denterria_RAA_gamma";
  int xbins=100; double xmin=0.; double xmax=14.;
  int ybins=100; double ymin=0.; double ymax=1.4;

  if (logy){ ymax = 19.5; ymin = 0.2; }

  char *xtitle="p_{T} (GeV/#font[72]{c})";
  //char *ytitle="ratio #gamma_{tot} /#gamma_{pQCD}";
  char *ytitle="R_{AA}^{#gamma} = #gamma_{tot} /#gamma_{pQCD}";
  
  TCanvas *c1 = new TCanvas(title,title,36,84,697,503);
  c1->Range(-1.77595,-1.01385,14.2076,1.374);
  c1->SetLeftMargin(0.11111);
  c1->SetRightMargin(0.012987);
  c1->SetTopMargin(0.03516);
  c1->SetBottomMargin(0.1318);
  if (logy) c1->SetLogy();

  TH2F *myframe = (TH2F*)e.frame(title, xbins, xmin, xmax, ybins, ymin, ymax, xtitle, ytitle);
  myframe->Draw();
  myframe->GetYaxis()->SetTitleOffset(1.0);
  c1->Update();
  
  TLine *line = new TLine(xmin,1,xmax,1);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();
  
  //_____________________________________________________________________________
  // photons

  TGraphErrors *pp_gamma = (TGraphErrors*)e.plot_spectrum_from_ascii("pQCD_vogelsang_pp_gamma_200GeV_cteq6_sc1.dat",eband); 
  emcAnalyzerUtils::AddOrSubstractRelatError(*pp_gamma,0.2);
  double ptmin = 1., ptmax = 14.;
  TF1 *pp_gamma_fit = new TF1("pp_gamma_fit", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin,ptmax);
  pp_gamma_fit->SetParameters(1.,1.,0.,0.,10.);
  pp_gamma->Fit("pp_gamma_fit","Q","",ptmin,ptmax);

  TGraphErrors *gamma_cent = (TGraphErrors *)e.plot_spectrum_from_ascii("direct_gamma_0_10_chisq2.txt",eband);
  double TAA_cent = emcAnalyzerUtils::getTAB(0,10);
  double eTAA_cent = emcAnalyzerUtils::getTAB(0,10,"error")/TAA_cent;
  emcAnalyzerUtils::scale(*gamma_cent,1./(TAA_cent));

  TF1 *totOverpQCD = new TF1("totOverpQCD","pol5*(x<6.5)+1*(x>6.5)",1.,14.);
  totOverpQCD->SetParameters(35.6422,-31.1837,11.4864,-2.14625,0.202352,-0.0076749);
  //(14.5614,0.353384,-7.79236,4.02216,-0.887733,0.0932606,-0.00382792);
  totOverpQCD->SetLineColor(6);
  totOverpQCD->SetLineWidth(8);
  totOverpQCD->Draw("same");

  c1->cd();
  TGraphErrors* RAA_gammacent= (TGraphErrors*)emcAnalyzerUtils::ratio(gamma_cent,pp_gamma_fit,0,1);
  emcAnalyzerUtils::setMarkerLineType(RAA_gammacent, 20, 1, 1.8);
  //emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAA_gammacent,"RAA","-",1);  // fidudead error cancels
  //emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAA_gammacent,"pT","-",1);  // acceptance error cancels
  //emcAnalyzerUtils::displaceXvalues(*RAA_gammacent,0.05);
  c1->cd();
  //plot_error_boxes(RAA_gammacent,c1,2,logy);
  plot_upper_limits(*RAA_gammacent,c1);
  plot_RAAgamma_uncertainty(RAA_gammacent,c1);
  RAA_gammacent->Draw("P");

  TLegend *leg = new TLegend(0.401154,0.718681,0.961039,0.938462,
			     "Au+Au #rightarrow X + #gamma, #sqrt{s_{#font[72]{NN}}} = 200 GeV, 0-10% central","brNDC");
  leg->SetMargin(0.2);
  leg->SetTextSize(0.038);
  leg->SetFillColor(kWhite);
  leg->AddEntry(RAA_gammacent, "PHENIX #gamma / pQCD #gamma", "P");// (0-10% central)", "P");
  leg->AddEntry(totOverpQCD, "(Hydro+pQCD #gamma) / pQCD #gamma", "L");
  leg->Draw();

}

//_____________________________________________________________________________
//

void plot_RAAgamma_uncertainty( TGraphErrors *g, TCanvas *c )
{

  TF1 *dwn = new TF1("dwn","pol2*(x<4.)+[3]*(x>4.)",1.,14.);
  dwn->SetParameters(4.737,-1.890,2.27042e-01,0.8);
  dwn->SetLineColor(1);
  dwn->SetLineWidth(2);
  dwn->SetLineStyle(4);
  c->cd();
  dwn->Draw("same");

  TF1 *up = new TF1("up","pol4*(x<5.9)+[5]*(x>5.9)",1.,14.);
  up->SetParameters(43.73,-33.57,10.12,-1.363,0.06866,1.2);
  up->SetLineColor(1);
  up->SetLineWidth(2);
  up->SetLineStyle(4);
  c->cd();
  up->Draw("same");

  return;

  TF1 *up = new TF1("up","pol4",4.0,13.5);
  up->SetParameters(4.79905,-1.09595,0.150847,-0.0109309,0.000318571);
  up->SetLineColor(51);
  up->SetLineWidth(2);
  up->SetLineStyle(2);
  c->cd();
  up->Draw("same");

  TF1 *dwn = new TF1("dwn","[0]*([1]+[2]*x+[3]*x*x+[4]*x*x*x+[5]*x*x*x*x)",4.,13.5);
  dwn->SetParameters(0.5,4.79905,-1.09595,0.150847,-0.0109309,0.000318571);
  //dwn->SetParameters(6.54283,-2.54433,0.423924,-0.0313491,0.000853894);
  dwn->SetLineColor(51);
  dwn->SetLineWidth(2);
  dwn->SetLineStyle(2);
  c->cd();
  dwn->Draw("same");

  return;

//   int N = g->GetN();
//   int N0 = 6;  // skip 6 first (upper limits) points
//   const int N2 = N-N0;

//   TGraph *eg_up = 0;
//   TGraph *eg_dwn = 0;

//   double x[N2],y_up[N2],y_dwn[N2];
//   double y;

//   double ey = sqrt(.1*.1+.2*.2); // 10% Ncoll + 20% NLO uncertainty

//   int j =0;

//   for (int i=N0;i<N;i++) 
//     {
//       g->GetPoint(i,x[j],y);
//       y_up[j]=y*(1+ey);
//       y_dwn[j]=y*(1-ey);
//       j++;
//     }
 
//   eg_up  = new TGraph(N2,x,y_up);
//   eg_dwn = new TGraph(N2,x,y_dwn) ;

//   c->cd();
//   eg_up->Draw("C");
//   eg_dwn->Draw("C");

//   return;

}

//_____________________________________________________________________________
//

void plot_upper_limits( TGraphErrors &g, TCanvas *c )
{

  const int N = 6; // N low pT points (upper limits)
  double x[N],y[N],ex[N],ey[N];

  TArrow *arrow = 0;
  TLine *line = 0;

  for (int i=0;i<N;i++) 
    {
      g.GetPoint(i,x[i],y[i]);
      ex[i] = g.GetErrorX(i);
      ey[i] = g.GetErrorY(i);

      //g.SetPoint(i,0.,0.); // delete points from original graph
      g.SetPointError(i,0.,0.); // delete points from original graph

      //arrow = new TArrow(c->XtoPad(x[i]),c->YtoPad(y[i]+ey[i]),c->XtoPad(x[i]),c->YtoPad(y[i]),0.02,">");
      arrow = new TArrow(x[i],y[i]+ey[i],x[i],(y[i]+ey[i])*0.15,0.02,">");
      line  = new TLine(x[i]-ex[i],y[i]+ey[i],x[i]+ex[i],y[i]+ey[i]);
      c->cd();
      arrow->Draw();
      line->Draw();
    }

  return;
}

//_____________________________________________________________________________
//

void plot_error_boxes( TGraphErrors *g, TCanvas *c, int col, bool logy )
{

  TGraphErrors *errg= (TGraphErrors*)g->Clone();
  for (int i = 0; i < errg->GetN();i++) {
    //if (logy) errg->SetPoint(i,errg->GetX()[i],log(errg->GetY()[i]));
    errg->SetPointError(i,0.2,0.); // only x-error
  }
  emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(errg,"pT","+",1); // y-error

  TClonesArray* errboxes = (TClonesArray*)emcAnalyzerUtils::errorBoxes( errg, 0.);
  TBox *box = 0;
  for (int j = 0; j < errboxes->GetEntries();j++)
    {
      box = ((TBox*)(*errboxes)[j]);
      box->SetFillStyle(3001);
      box->SetFillColor(col);
      c->cd();
      box->Draw();
    }

  return;
}
