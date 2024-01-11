void plotStripHotDead(int rn=347129)
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  TFile* outFile = new TFile("stripDeadMap.root","recreate");

  // Note that these histograms are schematic. They do not correspond
  // to the geometric hardware layout.

  // first strip layer (3rd VTX layer)
  // hot and dead together so far
  TH2F* h0h = new TH2F("h0h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h1h = new TH2F("h1h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h2h = new TH2F("h2h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h3h = new TH2F("h3h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h4h = new TH2F("h4h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h5h = new TH2F("h5h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h6h = new TH2F("h6h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h7h = new TH2F("h7h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h8h = new TH2F("h8h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h9h = new TH2F("h9h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h10h = new TH2F("h10h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h11h = new TH2F("h11h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h12h = new TH2F("h12h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h13h = new TH2F("h13h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h14h = new TH2F("h14h","",10,-0.5,9.5,1,-0.5,0.5);
  TH2F* h15h = new TH2F("h15h","",10,-0.5,9.5,1,-0.5,0.5);
  h0h->SetTickLength(0.,"Y");
  h1h->SetTickLength(0.,"Y");
  h2h->SetTickLength(0.,"Y");
  h3h->SetTickLength(0.,"Y");
  h4h->SetTickLength(0.,"Y");
  h5h->SetTickLength(0.,"Y");
  h6h->SetTickLength(0.,"Y");
  h7h->SetTickLength(0.,"Y");
  h8h->SetTickLength(0.,"Y");
  h9h->SetTickLength(0.,"Y");
  h10h->SetTickLength(0.,"Y");
  h11h->SetTickLength(0.,"Y");
  h12h->SetTickLength(0.,"Y");
  h13h->SetTickLength(0.,"Y");
  h14h->SetTickLength(0.,"Y");
  h15h->SetTickLength(0.,"Y");

  // second striplayer
  TH2F* hh0h = new TH2F("hh0h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh1h = new TH2F("hh1h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh2h = new TH2F("hh2h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh3h = new TH2F("hh3h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh4h = new TH2F("hh4h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh5h = new TH2F("hh5h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh6h = new TH2F("hh6h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh7h = new TH2F("hh7h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh8h = new TH2F("hh8h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh9h = new TH2F("hh9h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh10h = new TH2F("hh10h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh11h = new TH2F("hh11h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh12h = new TH2F("hh12h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh13h = new TH2F("hh13h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh14h = new TH2F("hh14h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh15h = new TH2F("hh15h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh16h = new TH2F("hh16h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh17h = new TH2F("hh17h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh18h = new TH2F("hh18h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh19h = new TH2F("hh19h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh20h = new TH2F("hh20h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh21h = new TH2F("hh21h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh22h = new TH2F("hh22h","",12,-0.5,11.5,1,-0.5,0.5);
  TH2F* hh23h = new TH2F("hh23h","",12,-0.5,11.5,1,-0.5,0.5);
  hh0h->SetTickLength(0.,"Y");
  hh1h->SetTickLength(0.,"Y");
  hh2h->SetTickLength(0.,"Y");
  hh3h->SetTickLength(0.,"Y");
  hh4h->SetTickLength(0.,"Y");
  hh5h->SetTickLength(0.,"Y");
  hh6h->SetTickLength(0.,"Y");
  hh7h->SetTickLength(0.,"Y");
  hh8h->SetTickLength(0.,"Y");
  hh9h->SetTickLength(0.,"Y");
  hh10h->SetTickLength(0.,"Y");
  hh11h->SetTickLength(0.,"Y");
  hh12h->SetTickLength(0.,"Y");
  hh13h->SetTickLength(0.,"Y");
  hh14h->SetTickLength(0.,"Y");
  hh15h->SetTickLength(0.,"Y");
  hh16h->SetTickLength(0.,"Y");
  hh17h->SetTickLength(0.,"Y");
  hh18h->SetTickLength(0.,"Y");
  hh19h->SetTickLength(0.,"Y");
  hh20h->SetTickLength(0.,"Y");
  hh21h->SetTickLength(0.,"Y");
  hh22h->SetTickLength(0.,"Y");
  hh23h->SetTickLength(0.,"Y");

  int runnumber = rn;
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp* tStamp = rt->getBeginTime(runnumber);
  tStamp->print(); cout << endl;

  svxAddress address;
  address.set_usedatabase(1);
  address.setFetchTime(tStamp); 
  address.Initialize();

  SvxDeadMap stripMap; // Only for stripixels, despite general-sounding name
  //  stripMap.readHybridsFromDatabase(runnumber);
  stripMap.readReadoutsFromDatabase(runnumber);
  stripMap.readFromDatabase(runnumber);

  // first striplayer 
  for(int ilr=0; ilr<1; ilr++) {
    for(int ild=0; ild<16; ild++) {
      for(int isn=0; isn<5; isn++) {
	for(int isensec=0; isensec<2; isensec++) { 
	  int status1 = stripMap.readoutStatus(ilr,ild,isn,isensec,0); // last number is readout
	  int status2 = stripMap.readoutStatus(ilr,ild,isn,isensec,1); // last number is readout
          if(status1!=0 || status2!=0) { 
            cout << "dead readout: " << ild << " " << isn << " " << isensec << endl;
	    if(ild==0) {h0h->Fill(9-(isn*2+isensec),0.);}
	    if(ild==1) {h1h->Fill(9-(isn*2+isensec),0.);}
	    if(ild==2) {h2h->Fill(9-(isn*2+isensec),0.);}
	    if(ild==3) {h3h->Fill(9-(isn*2+isensec),0.);}
	    if(ild==4) {h4h->Fill(9-(isn*2+isensec),0.);}
	    if(ild==5) {h5h->Fill(9-(isn*2+isensec),0.);}
	    if(ild==6) {h6h->Fill(9-(isn*2+isensec),0.);}
	    if(ild==7) {h7h->Fill(9-(isn*2+isensec),0.);}
	    if(ild==8) {h8h->Fill(isn*2+isensec,0.);}
	    if(ild==9) {h9h->Fill(isn*2+isensec,0.);}
	    if(ild==10) {h10h->Fill(isn*2+isensec,0.);}
	    if(ild==11) {h11h->Fill(isn*2+isensec,0.);}
	    if(ild==12) {h12h->Fill(isn*2+isensec,0.);}
	    if(ild==13) {h13h->Fill(isn*2+isensec,0.);}
	    if(ild==14) {h14h->Fill(isn*2+isensec,0.);}
	    if(ild==15) {h15h->Fill(isn*2+isensec,0.);}
	  }
	} // sensor section
      } // sensor
    } // ladder
  } // striplayer
  
  for(int ilr=1; ilr<2; ilr++) {
    for(int ild=0; ild<24; ild++) {
      for(int isn=0; isn<6; isn++) {
	for(int isensec=0; isensec<2; isensec++) {
	  int status1 = stripMap.readoutStatus(ilr,ild,isn,isensec,0); // last number is readout
	  int status2 = stripMap.readoutStatus(ilr,ild,isn,isensec,1); // last number is readout
          if(status1!=0 || status2!=0) {
            cout << "dead readout: " << ild << " " << isn << " " << isensec << endl;
	    if(ild==0) {hh0h->Fill(isn*2+isensec,0.);}
	    if(ild==1) {hh1h->Fill(isn*2+isensec,0.);}
	    if(ild==2) {hh2h->Fill(isn*2+isensec,0.);}
	    if(ild==3) {hh3h->Fill(isn*2+isensec,0.);}
	    if(ild==4) {hh4h->Fill(isn*2+isensec,0.);}
	    if(ild==5) {hh5h->Fill(isn*2+isensec,0.);}
	    if(ild==6) {hh6h->Fill(isn*2+isensec,0.);}
	    if(ild==7) {hh7h->Fill(isn*2+isensec,0.);}
	    if(ild==8) {hh8h->Fill(isn*2+isensec,0.);}
	    if(ild==9) {hh9h->Fill(isn*2+isensec,0.);}
	    if(ild==10) {hh10h->Fill(isn*2+isensec,0.);}
	    if(ild==11) {hh11h->Fill(isn*2+isensec,0.);}
	    if(ild==12) {hh12h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==13) {hh13h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==14) {hh14h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==15) {hh15h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==16) {hh16h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==17) {hh17h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==18) {hh18h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==19) {hh19h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==20) {hh20h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==21) {hh21h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==22) {hh22h->Fill(11-(isn*2+isensec),0.);}
	    if(ild==23) {hh23h->Fill(11-(isn*2+isensec),0.);}
	  }
	} // sensor section
      } // sensor
    } // ladder
  } // striplayer
  
  // plot
  
  gStyle->SetPalette(2);
  gStyle->SetOptTitle(0);
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(1);
  gStyle->SetFrameBorderMode(0);
  gStyle->SetDrawBorder(0);
  gStyle->SetTitleColor(0);
  gStyle->SetStatColor(0);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetCanvasColor(0);
  gStyle->SetPadColor(0);
  gStyle->SetPadBorderMode(0);
  
  cout << h0h->GetMaximum() << " " 
       << h1h->GetMaximum() << " " 
       << h2h->GetMaximum() << " " 
       << h3h->GetMaximum() << " " 
       << h4h->GetMaximum() << " " 
       << h5h->GetMaximum() << " " 
       << h6h->GetMaximum() << " " 
       << h7h->GetMaximum() << endl;
  
  c1 = new TCanvas("c1"," ",0,0,500,800);
  c1->Divide(1,17,0.00001,0.00001);
  
  c1->cd(17); h0h->Draw("col"); 
  c1->cd(16); h1h->Draw("col"); 
  c1->cd(15); h2h->Draw("col"); 
  c1->cd(14); h3h->Draw("col"); 
  c1->cd(13); h4h->Draw("col"); 
  c1->cd(12); h5h->Draw("col"); 
  c1->cd(11); h6h->Draw("col"); 
  c1->cd(10); h7h->Draw("col"); 
  
  c1->cd(8); h8h->Draw("col"); 
  c1->cd(7); h9h->Draw("col"); 
  c1->cd(6); h10h->Draw("col"); 
  c1->cd(5); h11h->Draw("col"); 
  c1->cd(4); h12h->Draw("col"); 
  c1->cd(3); h13h->Draw("col"); 
  c1->cd(2); h14h->Draw("col"); 
  c1->cd(1); h15h->Draw("col"); 

  c2 = new TCanvas("c2"," ",0,0,500,900);
  c2->Divide(1,25,0.00001,0.00001);

  c2->cd(25); hh0h->Draw("col");
  c2->cd(24); hh1h->Draw("col");
  c2->cd(23); hh2h->Draw("col");
  c2->cd(22); hh3h->Draw("col");
  c2->cd(21); hh4h->Draw("col");
  c2->cd(20); hh5h->Draw("col");
  c2->cd(19); hh6h->Draw("col");
  c2->cd(18); hh7h->Draw("col");
  c2->cd(17); hh8h->Draw("col");
  c2->cd(16); hh9h->Draw("col");
  c2->cd(15); hh10h->Draw("col");
  c2->cd(14); hh11h->Draw("col");

  c2->cd(12); hh12h->Draw("col");
  c2->cd(11); hh13h->Draw("col");
  c2->cd(10); hh14h->Draw("col");
  c2->cd(9); hh15h->Draw("col");
  c2->cd(8); hh16h->Draw("col");
  c2->cd(7); hh17h->Draw("col");
  c2->cd(6); hh18h->Draw("col");
  c2->cd(5); hh19h->Draw("col");
  c2->cd(4); hh20h->Draw("col");
  c2->cd(3); hh21h->Draw("col");
  c2->cd(2); hh22h->Draw("col");
  c2->cd(1); hh23h->Draw("col");

  c1->Write();
  c2->Write();
  outFile->Write();
}

