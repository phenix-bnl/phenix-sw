#include <TPad.h>
//
// plot the pi0 mass by towers
//
int deadhot[576];

void ReadHot()
{
  for (int ich=0; ich<576; ich++)
    {
      deadhot[ich] = 0;
    }

  ifstream infile("/phenix/u/chiu/mpc/database/deadhot_62trans_8014.list");
  int temp_ch, val;
  while ( infile >> temp_ch >> val )
    {
      deadhot[temp_ch] = 1;
    }
  infile.close();
}

void plotettowers()
{
  ReadHot();

  gStyle->SetOptStat(0);
  gSystem->Load("libmpc.so");
  MpcMap *mpcmap = MpcMap::instance();

  if ( mpcmap==0 ) return;

  ifstream inrunlist("runs.list");

  TFile *tfile[100];

  TH1F *hlopt[2][18][18];

  TString name;
  int nrun = 0;
  int run;
  while ( inrunlist >> run )
    {
      name = ""; name += run; name += "/"; name += "mpcslope.root";
      cout << "reading file " << name << endl;
      tfile[nrun] = new TFile(name,"READ");

      for (int iarm=0; iarm<1; iarm++)
        {
          for (int iy=0; iy<18; iy++)
            {
              for (int ix=0; ix<18; ix++)
                {
                  //name = "hlopt_"; name += iarm; name += "_"; name += ix; name += "_"; name += iy;
                  name = "het_"; name += iarm; name += "_"; name += ix; name += "_"; name += iy;
                  hlopt[iarm][iy][ix] = (TH1F*)tfile[nrun]->Get(name);
                  //if ( hlopt[iarm][iy][ix]==0 ) continue;
                }
            }
        }

      nrun++;
      if (nrun==1) break;
    }


  TCanvas *ac = new TCanvas("ac","ac",1000,1000);
  gPad->GetName();
  ac->Divide(18,18,0.0001,0.0001);
  for (int iy=0; iy<18; iy++)
    {
      if (iy>2) continue;
      for (int ix=0; ix<18; ix++)
        {
          int feech = mpcmap->getFeeCh(ix,iy,0);
          if ( feech < 0 ) continue;
          if ( hlopt[0][iy][ix]==0 ) continue;

          ac->cd( (17-iy)*18 + ix + 1 ); 

          hlopt[0][iy][ix]->SetLineColor(4);
          hlopt[0][iy][ix]->Rebin(10);
          hlopt[0][iy][ix]->SetAxisRange(-0.2,1.2);

          hlopt[0][iy][ix]->Fit("expo");

          //hlopt[0][iy][ix]->Draw();

          gPad->SetLogy(1);
          if ( deadhot[feech]>0 ) gPad->SetFillColor(5);

        }
    }

  ac->SaveAs("hlopt.gif");
  ac->SaveAs("hlopt.eps");

}

