int padoverlay(int pc, int arm, int sector)
{

  char Cuts[100];

  c1 = new TCanvas("c1","PC 2 Cluster Display",200,10,700,500);
  c1->SetFillColor(0);

  PadClus->SetMarkerSize(1.0);
  PadClus->SetMarkerStyle(2);
  PadClus->SetMarkerColor(2);

  PadGhit->SetMarkerSize(0.6);
  PadGhit->SetMarkerStyle(3);
  PadGhit->SetMarkerColor(4);

  sprintf(Cuts,"x!=0 && pc==%d && arm==%d && sector==%d",
	  pc,arm,sector);
  PadClus->Draw("atan(y/x):z",Cuts);
  PadGhit->Draw("atan(y/x):z",Cuts,"same");

}
