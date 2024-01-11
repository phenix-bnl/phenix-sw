int monitorFede() 
{
  init();
  monlist();  
}

void monlist() {
  cout << "--------------------------------------------------------------------------"<< endl;
  cout << "here you have a list of possible function to be called at the root prompt "<< endl; 
  cout << "--------------------------------------------------------------------------"<< endl;

    
  cout << "rawdist() "<< endl;
  cout << "onoffdist() "<< endl; 
  cout << "timedist(int arm) "<< endl;
  cout << "timedist(int arm, int,int,int,int) "<< endl;
  cout << "plottime1() "<< endl;
  cout << "plottime2() "<< endl;
  cout << "vtxbbcdist()" << endl;
  cout << "vtxdist() "<<endl; 
  cout << "vtxdist2() "<<endl;
  cout << "vtxdist3() " << endl; 
  cout << "statdistRawHit() "<< endl;
  cout << "statdistTrack() "<< endl;  
  

  cout << "NB:"<< endl;

}


int init()
{
  gStyle->SetOptStat(0);
  new TCanvas("c2","c2",200,10,1000,800);
  pad1 = new TPad("pad1","This is pad1",0.02,0.52,0.48,0.98,0);
  pad2 = new TPad("pad2","This is pad2",0.52,0.52,0.98,0.98,0);
  pad3 = new TPad("pad3","This is pad3",0.02,0.02,0.48,0.48,0);
  pad4 = new TPad("pad4","This is pad4",0.52,0.02,0.98,0.48,0);
  pad1->Draw();
  pad2->Draw();
  pad3->Draw();
  pad4->Draw();
 
  gStyle->SetStatW(0.30);
  gStyle->SetStatH(0.20);
  gStyle->SetStatColor(42);

  pad1->GetFrame()->SetFillColor(15);
  pad1->cd();
  t1 = new TText(0.05,0.8,"Raw Dist");
  t1->SetTextSize(0.1);
  pad2->GetFrame()->SetFillColor(15);
  pad2->cd();
  t2 = new TText(0.05,0.8,"Raw Dist");
  t2->SetTextSize(0.1);
  pad3->GetFrame()->SetFillColor(15);
  pad3->cd();
  t3 = new TText(0.05,0.8,"Raw Dist");
  t3->SetTextSize(0.1);
  pad4->GetFrame()->SetFillColor(15);
  pad4->cd();
  t4 = new TText(0.05,0.8,"Raw Dist");
  t4->SetTextSize(0.1);
}

int vtxbbcdist()
{
  pad1->cd();

  vtxbbc->SetMarkerStyle(20);
  vtxbbc->SetMarkerSize(0.25);
  vtxbbc->Draw("bbcvtx:vtxz");
  htemp->SetXTitle("Z vertex from DC (cm)");
  htemp->SetYTitle("Vertex Point from BBC (cm)");
  c2->Update();
  

}

int rawdist() {

  pad1->cd();
  referenceTime->Draw();
  referenceTime->SetXTitle("reference time (bins)");
  referenceTime->SetYTitle("nentries");
  pad2->cd();
  raw->Draw("time","time>0");
  htemp->SetXTitle("raw time (bins)");
  htemp->SetYTitle("nentries");

  pad3->cd();
  noise->Draw("plane:cell","ne0","lego");
  htemp->SetXTitle("cell");
  htemp->SetYTitle("plane");
  t3->SetText(0.05,0.8,"noisy channels");
  t3->Draw();
  
  pad4->cd();
  hit->Draw("time1");
  htemp->SetXTitle("hit time (bins)");
  htemp->SetYTitle("nentries");

  c2->Update();
}

int onoffdist()
{
  pad1.cd();
  //hit->Draw("plane:cell","side==0&&arm==0","cont");
  onoff00->Draw();
  onoff00->SetXTitle("cell EAST - SOUTH");
  onoff00->SetYTitle("plane EAST - SOUTH");
  pad2.cd();
  //hit->Draw("plane:cell","side==1&&arm==0","cont");
  onoff01->Draw();
  onoff01->SetXTitle("cell EAST - NORTH");
  onoff01->SetYTitle("plane EAST - NORTH");
  pad3.cd();
  //hit->Draw("plane:cell","side==0&&arm==1","cont");
  onoff10->Draw();
  onoff10->SetXTitle("cell  WEST - SOUTH");
  onoff10->SetYTitle("plane  WEST - SOUTH");
  pad4.cd();
  hit->Draw("plane:cell","side==1&&arm==1","cont");
  onoff11->Draw();
  onoff11->SetXTitle("cell WEST - NORTH");
  onoff11->SetYTitle("plane WEST - NORTH");




}

int timedist(int arm)
{
  timedist(arm, 0,150,450,750);
}

int timedist(int arm, int lowa, int higha, int lowb, int highb)
{
  mDchDVCalibrator->calculateCalibrationFromEdges(arm, lowa,higha,lowb,highb);
  if (arm ==0) {
    pad1->cd();
    timeDistributionEAST->GetXaxis()->SetRange(0,800);
    timeDistributionEAST->Draw();
    timeDistributionEAST->SetXTitle("hit time (bins)");
    timeDistributionEAST->SetYTitle("normalized nentries");
    pad2->cd();
    timeDistributionScaledEAST->GetXaxis()->SetRange(0,800);
    timeDistributionScaledEAST->Draw();
    timeDistributionScaledEAST->SetXTitle("hit time (bins)");
    timeDistributionScaledEAST->SetYTitle("normalized nentries");
    c2->Update();
    pad3->cd();
    timeDistributionEAST->GetXaxis()->SetRange(50,150);
    timeDistributionEAST->Draw();
    timeDistributionEAST->SetXTitle("hit time (bins)");
    timeDistributionEAST->SetYTitle("normalized nentries");
    pad4->cd();
    timeDistributionScaledEAST->GetXaxis()->SetRange(450,700);
    timeDistributionScaledEAST->Draw();
    timeDistributionScaledEAST->SetXTitle("hit time (bins)");
    timeDistributionScaledEAST->SetYTitle("normalized nentries");
    c2->Update();
  }else if (arm == 1) {
    pad1->cd();
    timeDistributionWEST->GetXaxis()->SetRange(0,800);
    timeDistributionWEST->Draw();
    timeDistributionWEST->SetXTitle("hit time (bins)");
    timeDistributionWEST->SetYTitle("normalized nentries");
    pad2->cd();
    timeDistributionScaledWEST->GetXaxis()->SetRange(0,800);
    timeDistributionScaledWEST->Draw();
    timeDistributionScaledWEST->SetXTitle("hit time (bins)");
    timeDistributionScaledWEST->SetYTitle("normalized nentries");
    c2->Update();
    pad3->cd();
    timeDistributionWEST->GetXaxis()->SetRange(50,150);
    timeDistributionWEST->Draw();
    timeDistributionWEST->SetXTitle("hit time (bins)");
    timeDistributionWEST->SetYTitle("normalized nentries");
    pad4->cd();
    timeDistributionScaledWEST->GetXaxis()->SetRange(450,700);
    timeDistributionScaledWEST->Draw();
    timeDistributionScaledWEST->SetXTitle("hit time (bins)");
    timeDistributionScaledWEST->SetYTitle("normalized nentries");
    c2->Update();
  }
 
}

int plottime1() 
{
  pad1->cd();
  timeDistributionEAST->GetXaxis()->SetRange(0,800);
  timeDistributionEAST->Draw();
  timeDistributionEAST->SetXTitle("hit time (bins)");
  timeDistributionEAST->SetYTitle("normalized nentries");
  pad2->cd();
  timeDistributionScaledEAST->GetXaxis()->SetRange(0,800);
  timeDistributionScaledEAST->Draw();
  timeDistributionScaledEAST->SetXTitle("hit time (bins)");
  timeDistributionScaledEAST->SetYTitle("normalized nentries");
  pad3->cd();
  timeDistributionWEST->GetXaxis()->SetRange(0,800);
  timeDistributionWEST->Draw();
  timeDistributionWEST->SetXTitle("hit time (bins)");
  timeDistributionWEST->SetYTitle("normalized nentries");
  pad4->cd();
  timeDistributionScaledWEST->GetXaxis()->SetRange(0,800);
  timeDistributionScaledWEST->Draw();
  timeDistributionScaledWEST->SetXTitle("hit time (bins)");
  timeDistributionScaledWEST->SetYTitle("normalized nentries");

  c2->Update();
 
}

int plottime2() 
{
  pad1->cd();
  timeDistributionEAST->GetXaxis()->SetRange(100,250);
  timeDistributionEAST->Draw();
  timeDistributionEAST->SetXTitle("hit time (bins)");
  timeDistributionEAST->SetYTitle("normalized nentries");
  pad2->cd();
  timeDistributionScaledEAST->GetXaxis()->SetRange(600,800);
  timeDistributionScaledEAST->Draw();
  timeDistributionScaledEAST->SetXTitle("hit time (bins)");
  timeDistributionScaledEAST->SetYTitle("normalized nentries");
  pad3->cd();
  timeDistributionWEST->GetXaxis()->SetRange(50,150);
  timeDistributionWEST->Draw();
  timeDistributionWEST->SetXTitle("hit time (bins)");
  timeDistributionWEST->SetYTitle("normalized nentries");
  pad4->cd();
  timeDistributionScaledWEST->GetXaxis()->SetRange(450,700);
  timeDistributionScaledWEST->Draw();
  timeDistributionScaledWEST->SetXTitle("hit time (bins)");
  timeDistributionScaledWEST->SetYTitle("normalized nentries");

  c2->Update();
 
}
int vtxdist() {

  //gROOT->Reset();
  //   gStyle->SetStatW(0.24);
  //  gStyle->SetOptStat(1110);
//   gStyle->SetMarkerStyle(20);
//   gStyle->SetMarkerSize(2); 
  //  c1->Divide(2,2);
  // c1_1->cd();

  pad1->cd();
  xvtxHist1->Draw();
  xvtxHist1->SetLineWidth(3);
  xvtxHist1->SetXTitle("Drift Chamber X vertex[cm]");

  pad2->cd();
  yvtxHist1->Draw();
  yvtxHist1->SetLineWidth(3);
  yvtxHist1->SetXTitle("Drift Chamber Y vertex [cm]");

  pad3->cd();
  xyvtxHist2->Draw();
  xyvtxHist2->SetLineWidth(3);
  xyvtxHist2->SetXTitle("Drift Chamber X vertex [cm]");
  xyvtxHist2->SetYTitle("Drift Chamber Y vertex [cm]");
  xyvtxHist2->SetMarkerStyle(20);
  xyvtxHist2->SetMarkerSize(0.3);
   
  pad4->cd();
  zvtxHist1->Draw();
  zvtxHist1->SetLineWidth(3);
  zvtxHist1->SetXTitle("Drift Chamber Z vertex [cm]");

}

int vtxdist2() 
{

  pad1->cd();
  distxHist1->Draw();
  distxHist1->SetLineWidth(3);
  distxHist1->SetXTitle("X Distance from vertex [cm]");

  pad2->cd();
  distyHist1->Draw();
  distyHist1->SetLineWidth(3);
  distyHist1->SetXTitle("Y Distance from vertex [cm]");

  pad3->cd();
  distzHist1->Draw();
  distzHist1->SetLineWidth(3);
  distzHist1->SetXTitle("Z Distance from vertex [cm]");

  pad4->cd();
  distxyHist2->Draw();
  distxyHist2->SetLineWidth(3);
  distxyHist2->SetXTitle("X Distance from vertex [cm]");
  distxyHist2->SetYTitle("Y Distance from vertex [cm]");
  distxyHist2->SetMarkerStyle(20);
  distxyHist2->SetMarkerSize(0.3);
   
}

int vtxdist3() 
{
  pad1->cd();
  accdistxHist1->Draw();
  accdistxHist1->SetLineWidth(3);
  accdistxHist1->SetXTitle("X Distance from vertex [cm]");

  pad2->cd();
  accdistyHist1->Draw();
  accdistyHist1->SetLineWidth(3);
  accdistyHist1->SetXTitle("Y Distance from vertex [cm]");

  pad3->cd();
  accdistzHist1->Draw();
  accdistzHist1->SetLineWidth(3);
  accdistzHist1->SetXTitle("Z Distance from vertex [cm]");

  pad4->cd();
  accdistxyHist2->Draw();
  accdistxyHist2->SetLineWidth(3);
  accdistxyHist2->SetXTitle("X Distance from vertex [cm]");
  accdistxyHist2->SetYTitle("Y Distance from vertex [cm]");
  accdistxyHist2->SetMarkerStyle(20);
  accdistxyHist2->SetMarkerSize(0.3);
 
}




int statdistRawHit() 
{
  pad1->cd();
  raw->Draw("nraw","(1/nraw)");
  htemp->SetXTitle("# raws");  
  htemp->SetYTitle("nentries");  

  pad2->cd();
  raw->Draw("evt");
  htemp->SetXTitle("event");  
  htemp->SetYTitle("# raws");  
  

  pad3->cd();
  hit->Draw("nhit","1/nhit");
  htemp->SetXTitle("# hits");  
  htemp->SetYTitle("nentries");
  
  pad4->cd();
  hit->Draw("evt");
  htemp->SetXTitle("event");  
  htemp->SetYTitle("# hits");  


  c2->Update();

}
int statdistTrack()
{
  pad1->cd();
  track->Draw("ntr","1/ntr");
  htemp->SetXTitle("# tracks");  
  htemp->SetYTitle("nentries");  


  pad2->cd();
  track->Draw("evt");
  htemp->SetXTitle("event");  
  htemp->SetYTitle("# tracks");  


}


void title(int run) {

  pad1->cd();
  TPavesText paves(0.1,0.1,0.5,0.5,1);
  paves.AddText("this is a pave text");
  paves.AddText("you can add new lines");
  paves.AddText(" formatting is automatic");
  paves.Draw();

}







