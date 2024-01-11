void plottdcgain(const char *fname = "tdcgain.root")
{
  gStyle->SetOptStat(0);
  gStyle->SetOptFit();

  TFile *tdcgainrootfile = new TFile(fname,"READ");

  const int numboards = 5;
  const int numch = 8;
  const int ntdc = 2;

  TString ofile(fname);
  ofile.Remove(0,ofile.Last('/') + 1);
  ofile.Remove(ofile.Index("."),ofile.Length());
  ofile += ".pdf[";

  TCanvas *c1=new TCanvas();
  c1->Print(ofile,"pdf");
  ofile.Remove(ofile.Index("."),ofile.Length());
  ofile += ".pdf";

  TCanvas *tdcgaincvs[numboards];

  TGraphErrors *tdcgain[numboards][numch][ntdc];

  TString name;
  for (int iboard=0; iboard<numboards; iboard++)
    {
      name = "tdcgain"; name += iboard; name += "cvs";
      tdcgaincvs[iboard] = new TCanvas(name,name,550,425);
      tdcgaincvs[iboard]->Divide(4,2);

      for (int ich=0; ich<numch; ich++)
        {
          name = "tdc0ch"; name += iboard*numch + ich;
          tdcgain[iboard][ich][0] = (TGraphErrors*)tdcgainrootfile->Get(name);
	  if ( !tdcgain[iboard][ich][0] ) break;

	  tdcgain[iboard][ich][0]->SetTitle(name);
          name = "tdc1ch"; name += iboard*numch + ich;
          tdcgain[iboard][ich][1] = (TGraphErrors*)tdcgainrootfile->Get(name);
	  tdcgain[iboard][ich][1]->SetTitle(name);

	  tdcgaincvs[iboard]->cd(ich+1);
	  tdcgain[iboard][ich][0]->SetMaximum(5000);
	  tdcgain[iboard][ich][0]->SetMinimum(0);
	  tdcgain[iboard][ich][0]->Draw("ap");
	  tdcgain[iboard][ich][1]->Draw("p");
	}
      tdcgaincvs[iboard]->cd(0);
      tdcgaincvs[iboard]->Print(ofile,"pdf");
    }

  ofile.Remove(ofile.Index("."),ofile.Length());
  ofile += ".pdf]";
  c1->Print(ofile,"pdf");
}
