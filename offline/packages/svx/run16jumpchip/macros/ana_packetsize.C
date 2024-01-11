void ana_packetsize()
{

  const int   NDETS = 3;
  const float xHigh = 1.8e+6;
  const float yHigh = 5.e+2;

  Int_t     evt      = 0;
  Int_t     vtxpsum  = 0;
  Int_t     vtxssum  = 0;
  Int_t     fvtxsum  = 0;
  TString   gname;
  TGraphErrors *g_pktsize_vs_event[NDETS];


  TFile   *fin = new TFile("sorted.root");
  TCanvas *can = new TCanvas("can","can",1600,1000);
  TLegend *leg = new TLegend(0.7,0.7,0.88,0.88);
  TTree   *t   = (TTree*)fin->Get("t");
  t->SetBranchAddress("evt",&evt);
  t->SetBranchAddress("vtxpsum",&vtxpsum);
  t->SetBranchAddress("vtxssum",&vtxssum);
  t->SetBranchAddress("fvtxsum",&fvtxsum);

  for ( int idet=0; idet<NDETS; idet++ )
    {
      g_pktsize_vs_event[idet] = new TGraphErrors();
      gname = "pktsize_vs_event"; gname += idet;
      g_pktsize_vs_event[idet]->SetName(gname);
    }


    Long64_t nentries = t->GetEntries();
    for (Long64_t ientry=0; ientry<nentries; ientry++)
      {
        t->GetEntry(ientry);

        int npoint = g_pktsize_vs_event[0]->GetN();
        g_pktsize_vs_event[0]->SetPoint(npoint,evt,(float)vtxpsum*4/1024);

        int npoint = g_pktsize_vs_event[1]->GetN();
        g_pktsize_vs_event[1]->SetPoint(npoint,evt,(float)vtxssum*4/1024);

        int npoint = g_pktsize_vs_event[2]->GetN();
        g_pktsize_vs_event[2]->SetPoint(npoint,evt,(float)fvtxsum*4/1024);

      } // ientry

  can->cd();
  g_pktsize_vs_event[2]->Draw("AL");
  g_pktsize_vs_event[1]->Draw("Lsame");
  g_pktsize_vs_event[0]->Draw("Lsame");

  for ( int idet=0; idet<NDETS; idet++ ) 
    {
      g_pktsize_vs_event[idet]->GetXaxis()->SetRangeUser(0,xHigh);
      g_pktsize_vs_event[idet]->GetYaxis()->SetRangeUser(0,yHigh);
      g_pktsize_vs_event[idet]->SetLineWidth(3);
      g_pktsize_vs_event[idet]->SetTitle("Sum of Packet Size");
      g_pktsize_vs_event[idet]->GetXaxis()->SetTitle("Event Sequence");
      g_pktsize_vs_event[idet]->GetYaxis()->SetTitle("Sum of Packet Size [kB]");
    }

  g_pktsize_vs_event[0]->SetLineColor(kGreen);
  g_pktsize_vs_event[1]->SetLineColor(kPink-3);
  g_pktsize_vs_event[2]->SetLineColor(kAzure+1);

  leg->AddEntry(g_pktsize_vs_event[0],"VTXP","lep");
  leg->AddEntry(g_pktsize_vs_event[1],"VTXS","lep");
  leg->AddEntry(g_pktsize_vs_event[2],"FVTX","lep");
  leg->Draw("same");

  can->SaveAs("packetsize.png");

  return;

}
