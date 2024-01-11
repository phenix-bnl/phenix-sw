void plotadcgain(const char *fname = "adcgain.root")
{
  TFile *adcgainrootfile = new TFile(fname,"READ");

  const int numboards = 5;
  const int numch = 8;

  TString ofile(fname);
  ofile.Remove(0,ofile.Last('/') + 1);
  ofile.Remove(ofile.Index("."),ofile.Length());
  ofile += ".pdf[";

  TCanvas *c1=new TCanvas();
  c1->Print(ofile,"pdf");
  ofile.Remove(ofile.Index("."),ofile.Length());
  ofile += ".pdf";

  TCanvas *adcgaincvs[numboards];
  TGraphErrors *adcgain[numboards][numch];

  TString name;
  for (int iboard=0; iboard<numboards; iboard++)
    {
      name = "adcgain"; name += iboard; name += "cvs";
      adcgaincvs[iboard] = new TCanvas(name,name,550,425);
      adcgaincvs[iboard]->Divide(4,2);

      for (int ich=0; ich<numch; ich++)
        {
          name = "dacch"; name += iboard*numch + ich;
          adcgain[iboard][ich] = (TGraphErrors*)adcgainrootfile->Get(name);
	  adcgain[iboard][ich]->SetTitle(name);

	  adcgaincvs[iboard]->cd(ich+1);
	  adcgain[iboard][ich]->SetMaximum(4096);
	  adcgain[iboard][ich]->SetMinimum(300);
	  adcgain[iboard][ich]->Draw("ap");
	}
      adcgaincvs[iboard]->cd(0);
      adcgaincvs[iboard]->Print(ofile,"pdf");
    }
  ofile.Remove(ofile.Index("."),ofile.Length());
  ofile += ".pdf]";
  c1->Print(ofile,"pdf");
}
