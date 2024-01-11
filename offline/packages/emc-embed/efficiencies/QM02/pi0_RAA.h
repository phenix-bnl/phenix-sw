
//_____________________________________________________________________________

TGraphErrors *get_pi0_WA98_RAA()
{
  // produce WA98 central pi0 spectrum
  // pp param. at cms-energie of 17 GeV
  TF1* fpp17 = new TF1("fpp17","[0]*pow([1]/(x+[1]),[2])",0.5,4.);
  fpp17->SetParameters(4.125,9.02,55.77);
  //fpp17->SetLineColor(kBlue);
  //fpp17->Draw("same");
  
  double nccen_wa98 = 651.;
  const int nwa98 = 35;
  
  double xwa98[nwa98];
  double ywa98[nwa98];
  double xewa98[nwa98];
  double yewa98[nwa98];
  
  double xnwa98[nwa98];
  double ynwa98[nwa98];
  double xnewa98[nwa98];
  double ynewa98[nwa98];
  
  char wa98file[200];
  sprintf(wa98file,"/afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/wa98_pi0_cen_com.dat");
  cout << "reading " << wa98file << endl;  
  ifstream fwa98(wa98file);
  for (int i=0; i<nwa98; i++) {
    fwa98 >> xwa98[i] >> ywa98[i] >> xewa98[i] >> yewa98[i];
    
    xnwa98[i]  = xwa98[i];
    ynwa98[i]  = ywa98[i] / (nccen_wa98 * fpp17->Eval(xwa98[i],0.,0.));
    xnewa98[i] = xewa98[i];
    ynewa98[i] = yewa98[i] / (nccen_wa98 *fpp17->Eval(xwa98[i],0.,0.));
  }
  fwa98.close();

  int nwa98Min = 6;
  int nwa98Max = 25;
  int npwa98 = nwa98Max - nwa98Min + 1;

  TGraphErrors* RAA_WA98 = new TGraphErrors(npwa98,&xnwa98[nwa98Min],&ynwa98[nwa98Min],
					    &xnewa98[nwa98Min],&ynewa98[nwa98Min]);

  RAA_WA98->SetMarkerStyle(21);
  RAA_WA98->SetMarkerColor(8);
  //RAA_WA98->Draw("P");

  return RAA_WA98;
}


//_____________________________________________________________________________

TGraphErrors *get_pi0_130_RAA()
{
  //
  // read PHENIX pi0 central to p+p ratio
  //
  const int nPh = 6;
  
  double dPhX[nPh];
  double dPhXe[nPh];
  double dPhY[nPh];
  double dPhYeStat[nPh];
  double dPhYeTot[nPh];
  double dPhYeUp[nPh];
  double dPhYeLo[nPh];
  double dPhYePl[nPh];
  double dPhYeMi[nPh];
  
  ifstream fPh("/afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/phenix_pi0_cen_pp_ratio.dat");
  for (int i=0; i<nPh; i++) {
    fPh >> dPhX[i] >> dPhY[i] >> dPhYeStat[i] >> dPhYeTot[i]
	>> dPhYeUp[i] >> dPhYeLo[i] >> dPhYePl[i] >> dPhYeMi[i];
    dPhXe[i] = 0.;
  }
  fPh.close();

  TGraphErrors* RAA_130 = new TGraphErrors(nPh, dPhX, dPhY, dPhXe, dPhYeStat);  

  RAA_130->SetMarkerStyle(20);
  RAA_130->SetMarkerColor(4);
  //RAA_130->Draw("P");

  return RAA_130;
      
}

//_____________________________________________________________________________

TClonesArray *plot_pi0_130_errors()
{
  //
  // read PHENIX pi0 central to p+p ratio
  //
  const int nPh = 6;
  
  double dPhX[nPh];
  double dPhXe[nPh];
  double dPhY[nPh];
  double dPhYeStat[nPh];
  double dPhYeTot[nPh];
  double dPhYeUp[nPh];
  double dPhYeLo[nPh];
  double dPhYePl[nPh];
  double dPhYeMi[nPh];
  
  ifstream fPh("/afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/phenix_pi0_cen_pp_ratio.dat");
  for (int i=0; i<nPh; i++) {
    fPh >> dPhX[i] >> dPhY[i] >> dPhYeStat[i] >> dPhYeTot[i]
	>> dPhYeUp[i] >> dPhYeLo[i] >> dPhYePl[i] >> dPhYeMi[i];
    dPhXe[i] = 0.;
  }
  fPh.close();

  TBox* b = 0;
  TClonesArray *array = new TClonesArray("TBox", 1000);

  // draw sys. errors as boxes
  for (int i=0; i<nPh; i++) 
    {
      b = new TBox(dPhX[i]-0.25,dPhY[i]-dPhYeMi[i],
		   dPhX[i]+0.25,dPhY[i]+dPhYePl[i]);
      
      b->SetFillColor(38);
      
      new((*array)[i]) TBox(*b);
  }

  return array;
}

//_____________________________________________________________________________

//TClonesArray *plot_WA98_RAA_errors( TLine *&lu, TLine *&lul, TLine *&lur, TLine *&ll, TLine *&lll, TLine *&llr)
TClonesArray *plot_WA98_RAA_errors()
{

  TLine *lu = 0; TLine *lul = 0; TLine *lur = 0;
  TLine *ll = 0; TLine *lll = 0; TLine *llr = 0;

  // pp param. at cms-energie of 17 GeV
  TF1* fpp17 = new TF1("fpp17","[0]*pow([1]/(x+[1]),[2])",0.5,4.);
  fpp17->SetParameters(4.125,9.02,55.77);

  const int nwa98 = 35;
  double nccen_wa98 = 651.;

  int nwa98Min = 6;
  int nwa98Max = 25;
  int npwa98 = nwa98Max - nwa98Min + 1;

  double xwa98[nwa98];
  double ywa98[nwa98];
  double xewa98[nwa98];
  double yewa98[nwa98];
  
  double xnwa98[nwa98];
  double ynwa98[nwa98];
  double xnewa98[nwa98];
  double ynewa98[nwa98];

  char wa98file[200];
  sprintf(wa98file,"/afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/wa98_pi0_cen_com.dat");
  cout << "reading " << wa98file << endl;  
  ifstream fwa98(wa98file);
  for (int i=0; i<nwa98; i++) 
    {
      fwa98 >> xwa98[i] >> ywa98[i] >> xewa98[i] >> yewa98[i];
      xnwa98[i]  = xwa98[i];
      ynwa98[i]  = ywa98[i] / (nccen_wa98 * fpp17->Eval(xwa98[i],0.,0.));
      xnewa98[i] = xewa98[i];
      ynewa98[i] = yewa98[i] / (nccen_wa98 *fpp17->Eval(xwa98[i],0.,0.));
  }
  fwa98.close();

  TClonesArray *array_of_lines = new TClonesArray("TLine", 1000);

  Int_t j = 0;
  // WA98 errors bars (Ncoll + pp-param uncertainty)
  // draw sys. errors as boxes for WA98 data points
  for (int i=0; i<npwa98; i++) 
    {
    
      double v = 0.02;
      double h = 0.05;
      
      lu = new TLine(xnwa98[i+nwa98Min]-h, 1.32*ynwa98[i+nwa98Min],
		     xnwa98[i+nwa98Min]+h, 1.32*ynwa98[i+nwa98Min]);
      lul = new TLine(xnwa98[i+nwa98Min]-h, 1.32*ynwa98[i+nwa98Min]-v,
		      xnwa98[i+nwa98Min]-h, 1.32*ynwa98[i+nwa98Min]);
      lur = new TLine(xnwa98[i+nwa98Min]+h, 1.32*ynwa98[i+nwa98Min]-v,
		      xnwa98[i+nwa98Min]+h, 1.32*ynwa98[i+nwa98Min]);
      
      ll = new TLine(xnwa98[i+nwa98Min]-h, 0.68*ynwa98[i+nwa98Min],
		     xnwa98[i+nwa98Min]+h, 0.68*ynwa98[i+nwa98Min]);
      lll = new TLine(xnwa98[i+nwa98Min]-h, 0.68*ynwa98[i+nwa98Min]+v,
		      xnwa98[i+nwa98Min]-h, 0.68*ynwa98[i+nwa98Min]);
      llr = new TLine(xnwa98[i+nwa98Min]+h, 0.68*ynwa98[i+nwa98Min]+v,
		      xnwa98[i+nwa98Min]+h, 0.68*ynwa98[i+nwa98Min]);
      
      new((*array_of_lines)[j++]) TLine(*lu);
      new((*array_of_lines)[j++]) TLine(*lul);
      new((*array_of_lines)[j++]) TLine(*lur);
      new((*array_of_lines)[j++]) TLine(*ll);
      new((*array_of_lines)[j++]) TLine(*lll);
      new((*array_of_lines)[j++]) TLine(*llr);
    }

  return array_of_lines;
}  
