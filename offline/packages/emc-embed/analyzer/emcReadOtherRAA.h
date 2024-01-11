
const char wa98file[200] = "$AFSHOME/offline/packages/emc-embed/analyzer/data/wa98_tables/wa98_pi0_et09_bin200MeV_above_2GeV_selectedbins.dat";
const char phnx130file[300] = "$AFSHOME/offline/packages/emc-embed/analyzer/dataphnx_130_tables/PizeroAuAu130_RAA.dat";

//_____________________________________________________________________________
// construct WA98 central pi0 spectrum

TGraphErrors *get_klaus_pi0_WA98_RAA()
{

  // pp param. at cms-energies of 17 GeV

  TF1* fpp17 = new TF1("fpp17","[0]*pow([1]/(x+[1]),[2])",0.5,4.);
  fpp17->SetParameters(4.125,9.02,55.77);
  //fpp17->SetLineColor(kBlue);
  //fpp17->Draw("same");
  
  double nccen_wa98 = 651.; // Ncoll(Pb+Pb)
  const int nwa98 = 19;
  
  double xwa98[nwa98];
  double ywa98[nwa98];
  double xewa98[nwa98];
  double yewa98[nwa98];
  
  double xnwa98[nwa98];
  double ynwa98[nwa98];
  double xnewa98[nwa98];
  double ynewa98[nwa98];
  
  std::cout << " <I> reading " << wa98file << std::endl;  

  ifstream fwa98(wa98file);
  if (!fwa98)
    {
	  std::cout << " <E> Can't open file: " << wa98file << std::endl;
	  return 0;
    }

  for (int i=0; i<nwa98; i++) 
    {
    
      fwa98 >> xwa98[i] >> ywa98[i] >> xewa98[i] >> yewa98[i];
      
      xnwa98[i]  = xwa98[i];
      ynwa98[i]  = ywa98[i] / (nccen_wa98 * fpp17->Eval(xwa98[i]));
      xnewa98[i] = xewa98[i];
      ynewa98[i] = yewa98[i] / (nccen_wa98 *fpp17->Eval(xwa98[i]));
    }

  fwa98.close();

//   int nwa98Min = 6;
//   int nwa98Max = 25;
//   int npwa98 = nwa98Max - nwa98Min + 1;

//   TGraphErrors* RAA_WA98 = new TGraphErrors(npwa98,&xnwa98[nwa98Min],&ynwa98[nwa98Min],
// 					    &xnewa98[nwa98Min],&ynewa98[nwa98Min]);

  TGraphErrors* RAA_WA98 = new TGraphErrors(nwa98,&xnwa98[nwa98],&ynwa98[nwa98],
					    &xnewa98[nwa98],&ynewa98[nwa98]);


  RAA_WA98->SetMarkerStyle(21);
  RAA_WA98->SetMarkerColor(8);
  //RAA_WA98->Draw("P");

  return RAA_WA98;
}


//_____________________________________________________________________________

//TClonesArray *get_WA98_RAA_errors( TLine *&lu, TLine *&lul, TLine *&lur, TLine *&ll, TLine *&lll, TLine *&llr)
TClonesArray *get_klaus_pi0_WA98_RAA_errors()
{

  TLine *lu = 0; TLine *lul = 0; TLine *lur = 0;
  TLine *ll = 0; TLine *lll = 0; TLine *llr = 0;

  // pp param. at cms-energie of 17 GeV
  TF1* fpp17 = new TF1("fpp17","[0]*pow([1]/(x+[1]),[2])",0.5,4.);
  fpp17->SetParameters(4.125,9.02,55.77);

  const int nwa98 = 19;
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

  std::cout << " <I> reading " << wa98file << std::endl;  

  ifstream fwa98(wa98file);
  if (!fwa98)
    {
	  std::cout << " <E> Can't open file: " << wa98file << std::endl;
	  return 0;
    }

  for (int i=0; i<nwa98; i++) 
    {
      fwa98 >> xwa98[i] >> ywa98[i] >> xewa98[i] >> yewa98[i];

      xnwa98[i]  = xwa98[i];
      ynwa98[i]  = ywa98[i] / (nccen_wa98 * fpp17->Eval(xwa98[i]));
      xnewa98[i] = xewa98[i];
      ynewa98[i] = yewa98[i] / (nccen_wa98 *fpp17->Eval(xwa98[i]));
  }
  fwa98.close();

  TClonesArray *array_of_lines = new TClonesArray("TLine", 1000);

  int j = 0;

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

//_____________________________________________________________________________

TGraphErrors *get_pi0_130_RAA()
{
  //
  // read PHENIX pi0 central to p+p ratio
  //
  const int npi0 = 6;
  
  double pi0X[npi0];
  double pi0eX[npi0];
  double pi0Y[npi0];
  double pi0eYStat[npi0];
  double pi0eYTot[npi0];
  double pi0eYUp[npi0];
  double pi0eYLo[npi0];
  double pi0eYPl[npi0];
  double pi0eYMi[npi0];
  
  std::cout << " <I> Reading " << phnx130file << std::endl;

  ifstream fpi0(phnx130file);
  if (!fpi0)
    {
	  std::cout << " <E> Can't open file: " << phnx130file << std::endl;
	  return 0;
    }
  for (int i=0; i<npi0; i++) 
    {
      fpi0 >> pi0X[i] >> pi0Y[i] >> pi0eYStat[i] >> pi0eYTot[i]
	   >> pi0eYUp[i] >> pi0eYLo[i] >> pi0eYPl[i] >> pi0eYMi[i];
      pi0eX[i] = 0.;
    }
  fpi0.close();

  TGraphErrors* RAA_130 = new TGraphErrors(npi0, pi0X, pi0Y, pi0eX, pi0eYStat);  

  RAA_130->SetMarkerStyle(20);
  RAA_130->SetMarkerColor(4);
  //RAA_130->Draw("P");

  return RAA_130;
      
}

//_____________________________________________________________________________

TClonesArray *get_pi0_130_errors(const double boxWidth = 0.25)
{
  //
  // read PHENIX pi0 central to p+p ratio
  //
  const int npi0 = 6;
  
  double pi0X[npi0];
  double pi0eX[npi0];
  double pi0Y[npi0];
  double pi0eYStat[npi0];
  double pi0eYTot[npi0];
  double pi0eYUp[npi0];
  double pi0eYLo[npi0];
  double pi0eYPl[npi0];
  double pi0eYMi[npi0];
  
  std::cout << " <I> Reading " << phnx130file << std::endl;

  ifstream fpi0(phnx130file);
  if (!fpi0)
    {
	  std::cout << " <E> Can't open file: " << phnx130file << std::endl;
	  return 0;
    }

  for (int i=0; i<npi0; i++) 
    {
      fpi0 >> pi0X[i] >> pi0Y[i] >> pi0eYStat[i] >> pi0eYTot[i]
	   >> pi0eYUp[i] >> pi0eYLo[i] >> pi0eYPl[i] >> pi0eYMi[i];
      pi0eX[i] = 0.;
    }
  fpi0.close();

  TBox* b = 0;
  TClonesArray *array = new TClonesArray("TBox", 1000);

  // draw sys. errors as boxes
  for (int i=0; i<npi0; i++) 
    {
      b = new TBox(pi0X[i]-boxWidth,pi0Y[i]-pi0eYMi[i],
		   pi0X[i]+boxWidth,pi0Y[i]+pi0eYPl[i]);
      
      b->SetFillColor(38);
      
      new((*array)[i]) TBox(*b);
  }

  return array;
}
