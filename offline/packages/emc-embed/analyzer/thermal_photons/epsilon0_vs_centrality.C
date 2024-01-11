TF1 *epsilon0_vs_centrality()
{
  
  gSystem->Load("libemcAnalyzer"); 
  emcAnalyzer e; 
  TGraphErrors *eband;

  TF1 *f_ein_vs_b = new TF1("f_ein_vs_b","pol6",0.,20.);
  TF1 *f_ein_vs_cent = new TF1("f_ein_vs_cent","pol6",0.,100.);
  TF1 *f_ein_vs_cent2 = new TF1("f_ein_vs_cent2","pol6",0.,100.);
  TF1 *f_ein_vs_cent3 = new TF1("f_ein_vs_cent3","pol6",0.,100.);

  const int Ncent = 18;

  double cent[Ncent];
  double s[Ncent];
  double ein[Ncent];

  // s ~ 0.75*N_part + 0.25*N_coll

  double Npart[Ncent];
  double Ncoll[Ncent];
  double b[Ncent];

  double snorm = 1;
  double enorm = 1;

  double scale = 11.786; // arbitrary scale to coincide w/ Dima's

 for (int i=0;i<=80;i=i+5)
   {
     int j=(int)i/5;
     cout << j << " " << endl;
     cent[j] = 5.*emcAnalyzerUtils::getCentralityClass(i,i+5)+2.5; // center of bin of 5% width
     b[j] = emcAnalyzerUtils::getImpactParam(i,i+5); // center of bin of 5% width

     Npart[j] = emcAnalyzerUtils::getNpart(i,i+5);
     Ncoll[j] = emcAnalyzerUtils::getNcoll(i,i+5);

     s[j]=(0.75*Npart[j]+0.25*Ncoll[j]);
     ein[j]=pow(s[j],4./3.);

     // let's normalize to most central
     //snorm = (j==0)? s[j] : snorm;
     enorm = (j==0)? ein[j] : enorm;
     s[j]/=snorm;
     ein[j]/=enorm;
     ein[j]*=scale;
   }

 // last 2 centralities ...

 cent[Ncent-2]= 86; // 80-92%
 b[Ncent-2] = emcAnalyzerUtils::getImpactParam(80,92);
 s[Ncent-2] = 0.75*emcAnalyzerUtils::getNpart(80,92)+0.25*emcAnalyzerUtils::getNcoll(80,92); 
 s[Ncent-2]/=snorm;
 ein[Ncent-2]=pow(s[Ncent-2],4./3.);
 ein[Ncent-2]/=enorm;
 ein[Ncent-2]*=scale;

 cent[Ncent-1]=100; b[Ncent-1] = 14.; // fm 
 s[Ncent-1] = 0.001; ein[Ncent-1]=0.001;

 // Let's plot things ...

 TCanvas *c0 = new TCanvas("Ncoll_vs_b_dde","Ncoll_vs_b_dde",600,500);

 TGraph *Ncoll_vs_b = new TGraph(Ncent,b,Ncoll);
 Ncoll_vs_b->SetMinimum(0.0001);
 Ncoll_vs_b->SetMarkerStyle(20);
 Ncoll_vs_b->GetXaxis()->SetTitle("b (fm)");
 Ncoll_vs_b->GetYaxis()->SetTitle("N_{part}, 0.75N_{part}+0.25N_{coll},N_{coll}");
 Ncoll_vs_b->Draw("AL");

 TGraph *Npart_vs_b = new TGraph(Ncent,b,Npart);
 Npart_vs_b->SetMinimum(0.0001);
 Npart_vs_b->SetMarkerStyle(20);
 Npart_vs_b->Draw("L");

 TGraph *s_vs_b = new TGraph(Ncent,b,s);
 s_vs_b->SetMinimum(0.0001);
 s_vs_b->SetMarkerStyle(20);
 s_vs_b->Draw("L");
 s_vs_b->Fit("f_ein_vs_b");

 TCanvas *c11 = new TCanvas("ein_vs_b_dde","ein_vs_b_dde",600,500);

 TGraph *ein_vs_b = new TGraph(Ncent,b,ein);
 ein_vs_b->SetMinimum(0.0001);
 ein_vs_b->SetMarkerStyle(20);
 ein_vs_b->GetXaxis()->SetTitle("b (fm)");
 ein_vs_b->Draw("AL");
 ein_vs_b->Fit("f_ein_vs_b");

 TCanvas *c1 = new TCanvas("ein_vs_cent_dde","ein_vs_cent_dde",600,500);

 TGraph *ein_vs_cent = new TGraph(Ncent,cent,ein);
 ein_vs_cent->SetMinimum(0.0001);
 ein_vs_cent->SetMarkerStyle(20);
 ein_vs_cent->GetXaxis()->SetTitle("Cent. Class (%)");
 ein_vs_cent->Draw("AL");
 ein_vs_cent->Fit("f_ein_vs_cent");

 //
 // Dmitri:
 //

 char* inasciifile = "/home/enterria/afsphnx/thermal_photons/nov04/sumvarvol_30GeV_tau06_v023.dat";

 ifstream in(inasciifile) ;

 const int Npoints = 100;

 double centclass[Npoints] ;
 double EinVarVol[Npoints] ;
 double TinVarVol[Npoints] ;
 double EtVarVol[Npoints] ;
 double EtV[Npoints] ;
 double slopeV[Npoints][10] ;
 double slopeErrV[Npoints][10] ;
 double slope1V[Npoints] ;
 double slope1ErrV[Npoints] ;
 double slope2V[Npoints] ;
 double slope2ErrV[Npoints] ;
 double slope3V[Npoints] ;
 double slope3ErrV[Npoints] ;

 Int_t j;
 
 for(Int_t i=0;i<Npoints;i++){
   j = i;
   in >> centclass[j] >>  EinVarVol[j]  >>  TinVarVol[j]  >> EtVarVol[j]  >>  EtV[j];
   for(Int_t j=0;j<10;j++){
     in >> slopeV[i][j]  >>  slopeErrV[i][j];
   }
   centclass[i]/=(100./13.);
 }

 in.close() ;

//  for(Int_t i=0;i<100;i++){
//    //j= 0     1      2       3    4        5    6        7    8        9  
//    // 0-0.5 0.5-1. 1-1.5  1.5-2  2-2.5  2.5-3  3-3.5  3.5-4  4-4.5  4.5-5
//   }

 //TCanvas *c2 = new TCanvas("ein_vs_cent_dima","Ein_vs_cent_dima",600,500);
 c11->cd();
 TGraph *ein_vs_cent2 = new TGraph(Npoints,centclass,EinVarVol);
 ein_vs_cent2->SetMinimum(0.0001);
 ein_vs_cent2->SetMarkerStyle(20);
 ein_vs_cent2->GetXaxis()->SetTitle("Cent. Class (%)");
 ein_vs_cent2->Draw("P");
 ein_vs_cent2->Fit("f_ein_vs_cent2");

 TCanvas *c3 = new TCanvas("c3","et_vs_cent_dima",600,500);

 TGraph *ein_vs_cent3 = new TGraph(Npoints,centclass,EtVarVol);
 ein_vs_cent3->SetMinimum(0.0001);
 ein_vs_cent3->SetMarkerStyle(20);
 ein_vs_cent3->GetXaxis()->SetTitle("Cent. Class (%)");
 ein_vs_cent3->Draw("AP");
 ein_vs_cent3->Fit("f_ein_vs_cent3");


 // P. Kolb:

 inasciifile = "/home/enterria/papers/hydro_photons_rhic/epsilon0_vs_centrality_kolb.txt";

 return f_ein_vs_cent;

}
