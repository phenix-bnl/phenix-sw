void ana_instability()
{

  const int NCHIPS = 8;

  TFile* fin = new TFile("sorted.root");
  TCanvas* can = new TCanvas("can","can",1200,1800);
  can->Divide(1,16);
  TGraphErrors *gr[NCHIPS];

  for ( int iladder=0; iladder<2; iladder++ )
    {

      for ( int ichip=0; ichip<NCHIPS; ichip++ )
        {
          can->cd(NCHIPS*iladder+ichip+1);

          if ( iladder==0 ) gr[ichip] = (TGraphErrors*)fin->Get(Form("corr_vs_event_p0c%d",ichip));
          else              gr[ichip] = (TGraphErrors*)fin->Get(Form("corr_vs_event_p2c%d",ichip));
          gr[ichip]->GetXaxis()->SetLimits(0,1.8e+6);
          gr[ichip]->Draw("AP"); 
        }

    }

  can->SaveAs("instability.png");

  return;

}
