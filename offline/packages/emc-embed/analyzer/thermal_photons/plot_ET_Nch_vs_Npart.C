
void plot_ET_Nch_vs_Npart( char* inasciifile = 
"/home/enterria/papers/hydro_photons_rhic/resubmission/macros/sumvarvol_qgp_tau015_v00_Glau_Au_200GeV_ET.dat" )
{

  cout << "Reading " << inasciifile << " ... " << endl ;
  char title[200];

  //____________________________________________________________________________
  // Read hydro data file

  const int Ncent = 100;

  double E_T[Ncent];
  double dNchdeta[Ncent];
  double Vol[Ncent];
  double eE_T[Ncent];
  double edNchdeta[Ncent];

  ifstream in(inasciifile);
  if (!in)
    {
      cout << " Can't open file: " << inasciifile << endl;
      return ;
    }
  int k;
  double dump;
  int dumpI ;

   // icen - step in impact parameter (100 steps up to 14 fm)
  for(int icen=0; icen<Ncent; icen++)
    {
      // dNchdeta - pseudo-rapidity density of _charged_ hadrons [post-computed with T.C]
      // E_T - transverse energy [post-computed with T.C]

      in >> k >> dump >> dump >> dump >> dump >> dump >> dump >> Vol[icen] >> dNchdeta[icen] >> E_T[icen] ;

      // 10 effective slopes (with errors) per line.
      for(int is=0; is<10; is++)
      { 
         in >> dump >> dump ;  
      }
      cout << " " << k << " " << Vol[icen] << " " << dNchdeta[icen] << " " << E_T[icen] << endl ;;

      eE_T[icen] = 0.;
      edNchdeta[icen] = 0.;
    }

  // last lines of file are for chi-square values ...
  for(int cen=0; cen<11; cen++){
    in >> k >> dump >> dump >> dump ;
  }
  in.close();

  //____________________________________________________________________________
  // pQCD Nch and ET

  double npart[11] = {351.4, 299.0, 253.9, 215.3, 166.6, 114.2, 74.4, 45.5, 25.6, 13.4, 6.3};
  double enpart[11] = {3, 4, 5, 5, 5, 4, 4, 4, 4, 3, 1};

  //  Cent = 00-05% -->  <E_T>[pQCD] = 56.5057 GeV
  //  Cent = 05-10% -->  <E_T>[pQCD] = 54.8014 GeV
  //  Cent = 10-15% -->  <E_T>[pQCD] = 45.5683 GeV
  //  Cent = 15-20% -->  <E_T>[pQCD] = 42.3793 GeV
  //  Cent = 20-30% -->  <E_T>[pQCD] = 44.0443 GeV
  //  Cent = 30-40% -->  <E_T>[pQCD] = 32.3821 GeV
  //  Cent = 40-50% -->  <E_T>[pQCD] = 21.2679 GeV
  //  Cent = 50-60% -->  <E_T>[pQCD] = 12.5816 GeV
  //  Cent = 60-70% -->  <E_T>[pQCD] = 5.87828 GeV
  //  Cent = 70-80% -->  <E_T>[pQCD] = 2.89936 GeV
  //  Cent = 80-92% -->  <E_T>[pQCD] = 1.44379 GeV
  
  double ET_pQCD[11]={56.5057,54.8014,45.5683,42.3793,44.0443,32.3821,21.2679,12.5816,5.87828,2.89936,1.44379};
  double errET_pQCD[11];

  for(int icen=0;icen<11;icen++){
    ET_pQCD[icen]/=(npart[icen]/2.);
    errET_pQCD[icen]=ET_pQCD[icen]*0.2;
  }

  TGraphErrors *ETpQCD_vs_Npart = new TGraphErrors(11,npart,ET_pQCD,enpart,errET_pQCD);
  TF1 * fitETpQCD = new TF1("fitETpQCD","pol4",0.,1000.) ;
  fitETpQCD->SetLineColor(2);
  fitETpQCD->SetLineWidth(4);
  fitETpQCD->SetLineStyle(4);
  ETpQCD_vs_Npart->Fit("fitETpQCD") ;
  fitETpQCD->SetRange(4,386);
 
  //  Cent = 00-05% -->  <N_ch>[pQCD] = 28.7239
  //  Cent = 05-10% -->  <N_ch>[pQCD] = 27.8575
  //  Cent = 10-15% -->  <N_ch>[pQCD] = 23.164
  //  Cent = 15-20% -->  <N_ch>[pQCD] = 21.5429
  //  Cent = 20-30% -->  <N_ch>[pQCD] = 22.3893
  //  Cent = 30-40% -->  <N_ch>[pQCD] = 16.461
  //  Cent = 40-50% -->  <N_ch>[pQCD] = 10.8112
  //  Cent = 50-60% -->  <N_ch>[pQCD] = 6.39566
  //  Cent = 60-70% -->  <N_ch>[pQCD] = 2.98814
  //  Cent = 70-80% -->  <N_ch>[pQCD] = 1.47385
  //  Cent = 80-92% -->  <N_ch>[pQCD] = 0.733928
  
  double Nch_pQCD[11]={28.7239,27.8575,23.164,21.5429,22.3893,16.461,10.8112,6.39566,2.98814,1.47385,0.733928};
  double errNch_pQCD[11];

  for(int icen=0;icen<11;icen++){
    Nch_pQCD[icen]/=(npart[icen]/2.);
    errNch_pQCD[icen]=Nch_pQCD[icen]*0.2;
  }

  TGraphErrors *NchpQCD_vs_Npart = new TGraphErrors(11,npart,Nch_pQCD,enpart,errNch_pQCD);
  TF1 * fitNchpQCD = new TF1("fitNchpQCD","pol4",0.,1000.) ;
  fitNchpQCD->SetLineColor(2);
  fitNchpQCD->SetLineWidth(4);
  fitNchpQCD->SetLineStyle(4);
  NchpQCD_vs_Npart->Fit("fitNchpQCD") ;
  fitNchpQCD->SetRange(4,386);

  //____________________________________________________________________________
  // Fill E_T, dN/deta versus Npart

//   TF1 *fNpart_vs_b = new TF1("fNpart_vs_b","pol5/([6]+exp((x-[7])/[8]))",0.,14.5);
//   fNpart_vs_b->SetParameters(643.878,-32.7032,4.03631,-2.15492,0.215534,-0.00553247,1.62902,10.2914,0.900199);

  TF1 *fNpart_vs_b= new TF1("fNpart_vs_b","pol6/([7]+exp((x-[8])/[9]))",0.,20.);
  fNpart_vs_b->SetParameters(1142.49,-17.4869,-6.84553,-2.34426,0.414572,-0.0231652,0.000458723,2.98342,11.9418,1.22045);

  double Npart[Ncent];
  double eNpart[Ncent];
  double ET_hydro_Norm_Npart[Ncent];
  double dNdeta_hydro_Norm_Npart[Ncent];
  double ET_tot_Norm_Npart[Ncent];
  double dNdeta_tot_Norm_Npart[Ncent];

  double b =0;

  for(int icen=0; icen<Ncent; icen++)
    {
      b = 14./100.*icen;
      Npart[icen] = fNpart_vs_b->Eval(b);
      eNpart[icen] = 0.; //0.1*Npart[icen];
      //cout << "Npart = " << Npart[icen] << " for centrality: " << icen << endl;

      //EtNormNpart2[icen] = etvs_b_phnx->Eval(b)/(Npart[icen]/2.);
      ET_hydro_Norm_Npart[icen] = E_T[icen]/(Npart[icen]/2.);
      ET_tot_Norm_Npart[icen]= ET_hydro_Norm_Npart[icen]+fitETpQCD->Eval(Npart[icen]);

      //dNdeta_Norm_Npart[icen] = dndeta_vs_b_phnx->Eval(b)/(Npart[icen]/2.);
      dNdeta_hydro_Norm_Npart[icen] = dNchdeta[icen]/(Npart[icen]/2.);
      dNdeta_tot_Norm_Npart[icen] = dNdeta_hydro_Norm_Npart[icen]+fitNchpQCD->Eval(Npart[icen]);
    }

  TGraphErrors *ET_hydro_vs_Npart = new TGraphErrors(Ncent,Npart,ET_hydro_Norm_Npart,eNpart,eE_T);
  TGraphErrors *ET_tot_vs_Npart = new TGraphErrors(Ncent,Npart,ET_tot_Norm_Npart,eNpart,eE_T);

  TGraphErrors *dNdeta_hydro_vs_Npart = new TGraphErrors(Ncent,Npart,dNdeta_hydro_Norm_Npart,eNpart,edNchdeta);
  TGraphErrors *dNdeta_tot_vs_Npart = new TGraphErrors(Ncent,Npart,dNdeta_tot_Norm_Npart,eNpart,edNchdeta);

  //____________________________________________________________________________
  // Plot E_T a versus Npart

  sprintf(title,"dETdeta_vs_Npart");
  TCanvas *c = new TCanvas(title,title,700,500);
  c->Range(0.0822335,-28.6458,0.612183,206.771);
  c->SetLeftMargin(0.127874);
  c->SetRightMargin(0.0229885);
  c->SetTopMargin(0.0287611);
  c->SetBottomMargin(0.121681);

  double ET_min = 0.; 
  double ET_max = 4.5;
  char ytitle[200];
  sprintf(ytitle,"(dE_{T}/d#eta)/(0.5 N_{part}) (GeV)");
  TH2F* myframe = (TH2F*)frame(title, 14,0.,400., 14,ET_min,ET_max, "N_{part}",ytitle); 
  myframe->Draw();
  int v = plot_dET_Npart_RHIC(c);

  //ET_hydro_vs_Npart->Print("all");
  //ET_hydro_vs_Npart->SetMarkerStyle(20); 
  //ET_hydro_vs_Npart->SetMarkerSize(1.6); 
  //ET_hydro_vs_Npart->SetMarkerColor(1); 
  ET_hydro_vs_Npart->SetLineWidth(4);
  ET_hydro_vs_Npart->SetLineColor(2);
  ET_hydro_vs_Npart->SetLineStyle(2);

  ET_tot_vs_Npart->SetName(title); 
  ET_tot_vs_Npart->SetLineWidth(4);
  ET_tot_vs_Npart->SetLineColor(2);

  c->cd();
  ET_tot_vs_Npart->Draw("L");
  ET_hydro_vs_Npart->Draw("L");
  fitETpQCD->Draw("same");

  //ETpQCD_vs_Npart->SetMarkerStyle(20);
  //ETpQCD_vs_Npart->SetLineStyle(2);
  //ETpQCD_vs_Npart->SetMarkerColor(10);
  //ETpQCD_vs_Npart->Draw("PL");

  //____________________________________________________________________________
  // Plot dN/deta versus Npart

  sprintf(title,"dNchdeta_vs_Npart");
  TCanvas *c2 = new TCanvas(title,title,700,500);
  c2->Range(0.0822335,-28.6458,0.612183,206.771);
  c2->SetLeftMargin(0.127874);
  c2->SetRightMargin(0.0229885);
  c2->SetTopMargin(0.0287611);
  c2->SetBottomMargin(0.121681);

  double dNdeta_min = 0.; 
  double dNdeta_max = 5.;
  sprintf(ytitle,"(dN_{ch}/d#eta)/(0.5 N_{part})");
  TH2F* myframe2 = (TH2F*)frame(title,14,0.,400.,14,dNdeta_min,dNdeta_max,"N_{part}",ytitle); 
  myframe2->Draw();
  int v = plot_dNdeta_Npart_RHIC(c2);

  //dNdeta_hydro_vs_Npart->Print("all");
  //dNdeta_hydro_vs_Npart->SetMarkerStyle(28); 
  //dNdeta_hydro_vs_Npart->SetMarkerSize(1.6); 
  //dNdeta_hydro_vs_Npart->SetMarkerColor(1); 
  dNdeta_hydro_vs_Npart->SetLineWidth(4);
  dNdeta_hydro_vs_Npart->SetLineColor(2);
  dNdeta_hydro_vs_Npart->SetLineStyle(2);

  dNdeta_tot_vs_Npart->SetName(title); 
  dNdeta_tot_vs_Npart->SetLineWidth(4);
  dNdeta_tot_vs_Npart->SetLineColor(2);

  c2->cd();
  dNdeta_tot_vs_Npart->Draw("L");
  dNdeta_hydro_vs_Npart->Draw("L");
  fitNchpQCD->Draw("same");

  TLegend *leg = new TLegend(0.46,0.72,0.957,0.963,NULL,"bNDC");
  leg->AddEntry(dNdeta_tot_vs_Npart,"dN_{ch}/d#eta hydro+pQCD","l");
  leg->AddEntry(dNdeta_hydro_vs_Npart,"dN_{ch}/d#eta hydro","l");
  leg->AddEntry(fitNchpQCD,"dN_{ch}/d#eta pQCD","l");
  leg->SetTextSize(0.038);
  leg->SetMargin(0.2);
  leg->SetFillStyle(0);
  leg->SetBorderSize(4);
  c2->cd();
  leg->Draw();

  //NchpQCD_vs_Npart->SetMarkerStyle(20);
  //NchpQCD_vs_Npart->SetMarkerColor(10);
  //NchpQCD_vs_Npart->Draw("PL");

  return;
}


TH2F *frame(const char *title="frame", 
        const int xbins=100, const double xmin=0., const double xmax=100.,
        const int ybins=100, const double ymin=0., const double ymax=100.,
        const char *xtitle="p_{T} (GeV/c)", 
        const char *ytitle="d^{3}N/d^{3}p (GeV/c)^{-2}")
{

  TH2F *myframe = new TH2F(title, title, xbins, xmin, xmax, ybins, ymin, ymax);

  myframe->SetStats(0);
  myframe->SetTitle(title);
  myframe->SetXTitle(xtitle);
  myframe->SetYTitle(ytitle);
  myframe->SetTitle("");
  myframe->SetStats(0);

  myframe->GetXaxis()->SetTitleSize(0.05);
  myframe->GetXaxis()->SetTitleOffset(1.2);
  myframe->GetXaxis()->SetLabelSize(0.05);

  myframe->GetYaxis()->SetTitleSize(0.05);
  myframe->GetYaxis()->SetTitleOffset(1.2);
  myframe->GetYaxis()->SetLabelSize(0.05);

  myframe->Draw();

  return myframe;
}

int plot_dET_Npart_RHIC( TCanvas *c1)
{

   TGraphErrors *gre = new TGraphErrors(14);
   gre->SetName("Graph");
   gre->SetTitle("Graph");
   gre->SetFillColor(1);
   gre->SetMarkerStyle(8);
   gre->SetMarkerSize(1.3);
   gre->SetPoint(0,353.4,3.427);
   gre->SetPointError(0,10.1,0.207);
   gre->SetPoint(1,300.2,3.283);
   gre->SetPointError(1,9,0.203);
   gre->SetPoint(2,254.3,3.163);
   gre->SetPointError(2,8.1,0.203);
   gre->SetPoint(3,215.2,3.046);
   gre->SetPointError(3,7.3,0.205);
   gre->SetPoint(4,181,2.945);
   gre->SetPointError(4,6.6,0.21);
   gre->SetPoint(5,151.3,2.858);
   gre->SetPointError(5,6,0.22);
   gre->SetPoint(6,125.2,2.757);
   gre->SetPointError(6,5.5,0.231);
   gre->SetPoint(7,102.7,2.664);
   gre->SetPointError(7,5.1,0.25);
   gre->SetPoint(8,83.3,2.567);
   gre->SetPointError(8,4.7,0.27);
   gre->SetPoint(9,66.7,2.453);
   gre->SetPointError(9,4.3,0.29);
   gre->SetPoint(10,52.5,2.301);
   gre->SetPointError(10,4.1,0.328);
   gre->SetPoint(11,40.2,2.184);
   gre->SetPointError(11,3.8,0.359);
   gre->SetPoint(12,30.2,2.06);
   gre->SetPointError(12,3.6,0.407);
   gre->SetPoint(13,22,1.918);
   gre->SetPointError(13,3.4,0.469);
   c1->cd();
   gre->Draw("p");
   
   Double_t *dum = 0;
   TPolyLine *pline = new TPolyLine(14,dum,dum,"");
   pline->SetFillColor(19);
   pline->SetPoint(0,353.4,3.441);
   pline->SetPoint(1,300.2,3.317);
   pline->SetPoint(2,254.3,3.218);
   pline->SetPoint(3,215.2,3.118);
   pline->SetPoint(4,181,3.038);
   pline->SetPoint(5,151.3,2.974);
   pline->SetPoint(6,125.2,2.894);
   pline->SetPoint(7,102.7,2.83);
   pline->SetPoint(8,83.3,2.762);
   pline->SetPoint(9,66.7,2.677);
   pline->SetPoint(10,52.5,2.569);
   pline->SetPoint(11,40.2,2.492);
   pline->SetPoint(12,30.2,2.416);
   pline->SetPoint(13,22,2.346);
   pline->Draw("L");
   
   Double_t *dum = 0;
   pline = new TPolyLine(14,dum,dum,"");
   pline->SetFillColor(19);
   pline->SetPoint(0,353.4,3.413);
   pline->SetPoint(1,300.2,3.249);
   pline->SetPoint(2,254.3,3.108);
   pline->SetPoint(3,215.2,2.974);
   pline->SetPoint(4,181,2.852);
   pline->SetPoint(5,151.3,2.742);
   pline->SetPoint(6,125.2,2.62);
   pline->SetPoint(7,102.7,2.498);
   pline->SetPoint(8,83.3,2.372);
   pline->SetPoint(9,66.7,2.229);
   pline->SetPoint(10,52.5,2.033);
   pline->SetPoint(11,40.2,1.876);
   pline->SetPoint(12,30.2,1.704);
   pline->SetPoint(13,22,1.49);
   pline->Draw("L");


}

int plot_dNdeta_Npart_RHIC( TCanvas *c1)
{

   TGraphErrors *graph = new TGraphErrors(11);
   graph->SetName("phobos");
   graph->SetTitle("phobos");
   graph->SetFillColor(1);
   graph->SetMarkerStyle(25);
   graph->SetMarkerSize(1.5);
   graph->SetMarkerColor(4);
   graph->SetPoint(0,357.8,3.908);
   graph->SetPoint(1,330.9,3.804);
   graph->SetPoint(2,298.1,3.671);
   graph->SetPoint(3,255.9,3.553);
   graph->SetPoint(4,217.3,3.479);
   graph->SetPoint(5,182.7,3.405);
   graph->SetPoint(6,151.7,3.316);
   graph->SetPoint(7,123.6,3.242);
   graph->SetPoint(8,103.1,3.183);
   graph->SetPoint(9,83.2,3.138);
   graph->SetPoint(10,65,2.894);
   double xerr = 0., yerr = 0.06;
   graph->SetPointError(0,357.8*xerr,3.908*yerr);
   graph->SetPointError(1,330.9*xerr,3.804*yerr);
   graph->SetPointError(2,298.1*xerr,3.671*yerr);
   graph->SetPointError(3,255.9*xerr,3.553*yerr);
   graph->SetPointError(4,217.3*xerr,3.479*yerr);
   graph->SetPointError(5,182.7*xerr,3.405*yerr);
   graph->SetPointError(6,151.7*xerr,3.316*yerr);
   graph->SetPointError(7,123.6*xerr,3.242*yerr);
   graph->SetPointError(8,103.1*xerr,3.183*yerr);
   graph->SetPointError(9,83.2*xerr,3.138*yerr);
   graph->SetPointError(10,65*xerr,2.894*yerr);
   c1->cd();
   graph->Draw("p");
   
   TGraphErrors *gre = new TGraphErrors(6);
   gre->SetName("star");
   gre->SetTitle("star");
   gre->SetFillColor(1);
   gre->SetMarkerStyle(30);
   gre->SetMarkerSize(2.1);
   gre->SetMarkerColor(4);
   gre->SetPoint(0,48.9,2.997);
   gre->SetPointError(0,0,0.506);
   gre->SetPoint(1,129.4,3.242);
   gre->SetPointError(1,0,0.261);
   gre->SetPoint(2,185.2,3.35);
   gre->SetPointError(2,0,0.299);
   gre->SetPoint(3,248.5,3.541);
   gre->SetPointError(3,0,0.299);
   gre->SetPoint(4,294,3.641);
   gre->SetPointError(4,0,0.299);
   gre->SetPoint(5,362.8,3.917);
   gre->SetPointError(5,0,0.303);
   //gre->SetPoint(6,0,0);
   //gre->SetPointError(6,0,0);
   gre->Draw("p");
   
   gre = new TGraphErrors(14);
   gre->SetName("phenix");
   gre->SetTitle("phenix");
   gre->SetFillColor(1);
   gre->SetMarkerStyle(8);
   gre->SetMarkerSize(1.7);
   gre->SetPoint(0,353.4,3.89);
   gre->SetPointError(0,10.1,0.235);
   gre->SetPoint(1,300.2,3.734);
   gre->SetPointError(1,9,0.217);
   gre->SetPoint(2,254.3,3.593);
   gre->SetPointError(2,8.1,0.209);
   gre->SetPoint(3,215.2,3.453);
   gre->SetPointError(3,7.3,0.206);
   gre->SetPoint(4,181,3.343);
   gre->SetPointError(4,6.6,0.213);
   gre->SetPoint(5,151.3,3.247);
   gre->SetPointError(5,6,0.223);
   gre->SetPoint(6,125.2,3.15);
   gre->SetPointError(6,5.5,0.239);
   gre->SetPoint(7,102.7,3.046);
   gre->SetPointError(7,5.1,0.261);
   gre->SetPoint(8,83.3,2.965);
   gre->SetPointError(8,4.7,0.285);
   gre->SetPoint(9,66.7,2.858);
   gre->SetPointError(9,4.3,0.317);
   gre->SetPoint(10,52.5,2.701);
   gre->SetPointError(10,4.1,0.358);
   gre->SetPoint(11,40.2,2.597);
   gre->SetPointError(11,3.8,0.406);
   gre->SetPoint(12,30.2,2.483);
   gre->SetPointError(12,3.6,0.464);
   gre->SetPoint(13,22,2.327);
   gre->SetPointError(13,3.4,0.545);
   gre->Draw("p");
   
   gre = new TGraphErrors(6);
   gre->SetName("brahms");
   gre->SetTitle("brahms");
   gre->SetFillColor(1);
   gre->SetMarkerColor(4);
   gre->SetMarkerStyle(28);
   gre->SetMarkerSize(1.7);
   gre->SetPoint(0,357,3.501);
   gre->SetPointError(0,8,0.318);
   gre->SetPoint(1,306,3.275);
   gre->SetPointError(1,11,0.311);
   gre->SetPoint(2,239,3.155);
   gre->SetPointError(2,10,0.306);
   gre->SetPoint(3,168,3.06);
   gre->SetPointError(3,9,0.319);
   gre->SetPoint(4,114,3.053);
   gre->SetPointError(4,9,0.37);
   gre->SetPoint(5,73,3.014);
   gre->SetPointError(5,8,0.429);
   gre->Draw("p");

   return 0;
}

void fit_ET_Nch_vs_b( double bmin = 2.1,  double bmax = 12.1 )
{

  const int Ncent = 20;

  const double ET_phnx[Ncent] = { 606, 493., //   0-10%
                  402, 328,  //  10-20%
                  266, 216.,  //  20-30%
                  173, 137,  //  30-40%
                  107,  82,  //  40-50%
                  60,  44,  //  50-60%
                  31,  21,  //  60-70% // below this are invented ...
                  17,14, //  70-80%
                  12,9, //  80-90%
                  5,2}; //  90-100%

  const double errET_phnx[Ncent]    = { 27.0, 22.5, //   0-10%
                    19.2, 16.4, //  10-20%
                    14.4, 12.8, //  20-30%
                    11.3, 10.1, //  30-40%
                    8.9,  7.8,  //  40-50%
                    6.9,  5.8,  //  50-60%
                    4.8,  3.9,  //  60-70%
                    2.,2.,
                    2.,2.,
                    2.,2.};
  
  const double dNdeta_ch_phnx[Ncent] = { 687, 560, //  0-10%
                     457, 372, // 10-20%
                     302, 246, // 20-30% 
                     197, 156, // 30-40%
                     124,  95.3, // 40-50%
                     70.9,  52.2, // 50-60%
                     37.5,  25.6, // 60-70% // below this are invented ...
                     12,9, //  70-80%
                     7,6, //  80-90%
                     5,2}; //  90-100%

  double errdNdeta_ch_phnx[Ncent]  = { 36.6, 27.9,
                       22.3, 18.2,
                       15.8, 13.8,
                       12.2, 10.9,
                       9.6,  8.6,
                       7.6,  6.5,
                       5.4,  4.5, // 60-70%
                       2.,   2.,
                       2.,2.,
                       2.,2.};
  
  const double impParam[Ncent] =  { 2.3,  4.1, //  0- 5%,  5-10%
                    5.2,  6.2, // 10-15%, 15-20%
                    7.0,  7.8, // 20-25%, 25-30%
                    8.4,  9.1, // 30-35%, 35-40%
                    9.7, 10.2, // 40-45%, 45-50% 
                    10.7, 11.2, // 50-55%, 55-60%
                    11.7, 12.1, // 60-65%, 65-70%
                    12.6, 13.0, // 70-75%, 75-80%
                    13.5, 13.9, // 80-85%, 85-90%
                    14.5, 15.7};// 90-95%, 95-100%
  
  const double cc_percentile[Ncent] = {   2.5,  7.5  //   0-10%
                      12.5, 17.5, //  10-20%
                      22.5, 27.5, //  20-30%
                      32.5, 37.5, //  30-40%
                      42.5, 47.5, //  40-50%
                      52.5, 57.5, //  50-60%
                      62.5, 67.5, //  60-70%
                      72.5, 77.5, //  70-80%
                      82.5, 87.5, //  80-90%
                      92.5, 97.5};//  90-100%

  double errimpParam[Ncent];
  double errcc_percentile[Ncent];

  double ET_phnx_3[Ncent];
  double dNdeta_ch_phnx_4[Ncent];
  double errET_phnx_3[Ncent];
  double errdNdeta_ch_phnx_4[Ncent];

  // Trick-1: play with the (x,y) errors until you get a decent fit ...
  // Trick-2: play with the fit limits until you get a decent fit ...
  for (int i=0;i<Ncent;i++)
    {
      errimpParam[i]=impParam[i]*0.03;
      errcc_percentile[i]=2.5;

      //for test of g = s^4/epsilon^3
      ET_phnx_3[i]=pow(ET_phnx[i],3.);
      dNdeta_ch_phnx_4[i]=pow(errdNdeta_ch_phnx[i],4.);

      errET_phnx_3[i]=ET_phnx_3*(errET_phnx[i]/ET_phnx[i]);
      errdNdeta_ch_phnx_4[i]=dNdeta_ch_phnx_4*(errdNdeta_ch_phnx[i]/dNdeta_ch_phnx[i]);
    }

  
  TCanvas *c0 = new TCanvas("c0","c0",700,600);

  TF1 *et_vs_b_phnx = new TF1("et_vs_b_phnx","pol9",bmin,bmax);
  TF1 *dndeta_vs_b_phnx = new TF1("dndeta_vs_b_phnx","pol9",bmin,bmax);
  TF1 *centclass_vs_b = new TF1("centclass_vs_b","pol7",0.,14.);

  et_vs_b_phnx->SetLineColor(1);
  dndeta_vs_b_phnx->SetLineColor(4);
  centclass_vs_b->SetLineColor(2);

  //TGraphErrors *tmp = new TGraphErrors(Ncent,impParam,ET_phnx,errimpParam,errET_phnx) ;
  TGraphErrors *tmp = new TGraphErrors(Ncent,impParam,ET_phnx_3,errimpParam,errET_phnx_3) ;
  tmp->SetMaximum(800);
  tmp->SetMinimum(1);
  tmp->Fit("et_vs_b_phnx","QEIMR");
  tmp->SetMarkerStyle(20);
  tmp->Draw("AP");
  et_vs_b_phnx->Draw("same");

  //TGraphErrors *tmp2 = new TGraphErrors(Ncent,impParam,dNdeta_ch_phnx,errimpParam,errdNdeta_ch_phnx) ;
  TGraphErrors *tmp2 = new TGraphErrors(Ncent,impParam,dNdeta_ch_phnx_4,errimpParam,errdNdeta_ch_phnx_4) ;
  tmp2->SetMinimum(1);
  tmp2->Fit("dndeta_vs_b_phnx","QEIMR");
  c0->cd();
  tmp2->SetMarkerStyle(29);
  tmp2->Draw("P");
  dndeta_vs_b_phnx->Draw("same");

}
