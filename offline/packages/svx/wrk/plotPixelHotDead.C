void plotPixelHotDead(int rn=347129)
//void plotPixelHotDead(int rn=349369)
//void plotPixelHotDead(int rn=376433)
//void plotPixelHotDead(int rn=348424)
//void plotPixelHotDead(int rn=349667)
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");

// histograms representing ladders in layer 0
// separate histograms for hot and dead
  TH2F* h0h = new TH2F("h0h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h1h = new TH2F("h1h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h2h = new TH2F("h2h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h3h = new TH2F("h3h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h4h = new TH2F("h4h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h5h = new TH2F("h5h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h6h = new TH2F("h6h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h7h = new TH2F("h7h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h8h = new TH2F("h8h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h9h = new TH2F("h9h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h0d = new TH2F("h0d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h1d = new TH2F("h1d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h2d = new TH2F("h2d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h3d = new TH2F("h3d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h4d = new TH2F("h4d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h5d = new TH2F("h5d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h6d = new TH2F("h6d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h7d = new TH2F("h7d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h8d = new TH2F("h8d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* h9d = new TH2F("h9d","",512,-0.5,511.5,256,-0.5,255.5);
  h0d->GetXaxis()->SetLabelSize(0.0001);
  h0d->GetYaxis()->SetLabelSize(0.0001);
  h1d->GetXaxis()->SetLabelSize(0.0001);
  h1d->GetYaxis()->SetLabelSize(0.0001);
  h2d->GetXaxis()->SetLabelSize(0.0001);
  h2d->GetYaxis()->SetLabelSize(0.0001);
  h3d->GetXaxis()->SetLabelSize(0.0001);
  h3d->GetYaxis()->SetLabelSize(0.0001);
  h4d->GetXaxis()->SetLabelSize(0.0001);
  h4d->GetYaxis()->SetLabelSize(0.0001);
  h5d->GetXaxis()->SetLabelSize(0.0001);
  h5d->GetYaxis()->SetLabelSize(0.0001);
  h6d->GetXaxis()->SetLabelSize(0.0001);
  h6d->GetYaxis()->SetLabelSize(0.0001);
  h7d->GetXaxis()->SetLabelSize(0.0001);
  h7d->GetYaxis()->SetLabelSize(0.0001);
  h8d->GetXaxis()->SetLabelSize(0.0001);
  h8d->GetYaxis()->SetLabelSize(0.0001);
  h9d->GetXaxis()->SetLabelSize(0.0001);
  h9d->GetYaxis()->SetLabelSize(0.0001);

// layer 1
  TH2F* hh0h = new TH2F("hh0h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh1h = new TH2F("hh1h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh2h = new TH2F("hh2h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh3h = new TH2F("hh3h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh4h = new TH2F("hh4h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh5h = new TH2F("hh5h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh6h = new TH2F("hh6h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh7h = new TH2F("hh7h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh8h = new TH2F("hh8h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh9h = new TH2F("hh9h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh0d = new TH2F("hh0d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh1d = new TH2F("hh1d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh2d = new TH2F("hh2d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh3d = new TH2F("hh3d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh4d = new TH2F("hh4d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh5d = new TH2F("hh5d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh6d = new TH2F("hh6d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh7d = new TH2F("hh7d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh8d = new TH2F("hh8d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh9d = new TH2F("hh9d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh10h = new TH2F("hh10h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh11h = new TH2F("hh11h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh12h = new TH2F("hh12h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh13h = new TH2F("hh13h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh14h = new TH2F("hh14h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh15h = new TH2F("hh15h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh16h = new TH2F("hh16h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh17h = new TH2F("hh17h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh18h = new TH2F("hh18h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh19h = new TH2F("hh19h","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh10d = new TH2F("hh10d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh11d = new TH2F("hh11d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh12d = new TH2F("hh12d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh13d = new TH2F("hh13d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh14d = new TH2F("hh14d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh15d = new TH2F("hh15d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh16d = new TH2F("hh16d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh17d = new TH2F("hh17d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh18d = new TH2F("hh18d","",512,-0.5,511.5,256,-0.5,255.5);
  TH2F* hh19d = new TH2F("hh19d","",512,-0.5,511.5,256,-0.5,255.5);

  hh0d->GetXaxis()->SetLabelSize(0.0001);
  hh0d->GetYaxis()->SetLabelSize(0.0001);
  hh1d->GetXaxis()->SetLabelSize(0.0001);
  hh1d->GetYaxis()->SetLabelSize(0.0001);
  hh2d->GetXaxis()->SetLabelSize(0.0001);
  hh2d->GetYaxis()->SetLabelSize(0.0001);
  hh3d->GetXaxis()->SetLabelSize(0.0001);
  hh3d->GetYaxis()->SetLabelSize(0.0001);
  hh4d->GetXaxis()->SetLabelSize(0.0001);
  hh4d->GetYaxis()->SetLabelSize(0.0001);
  hh5d->GetXaxis()->SetLabelSize(0.0001);
  hh5d->GetYaxis()->SetLabelSize(0.0001);
  hh6d->GetXaxis()->SetLabelSize(0.0001);
  hh6d->GetYaxis()->SetLabelSize(0.0001);
  hh7d->GetXaxis()->SetLabelSize(0.0001);
  hh7d->GetYaxis()->SetLabelSize(0.0001);
  hh8d->GetXaxis()->SetLabelSize(0.0001);
  hh8d->GetYaxis()->SetLabelSize(0.0001);
  hh9d->GetXaxis()->SetLabelSize(0.0001);
  hh9d->GetYaxis()->SetLabelSize(0.0001);
  hh10d->GetXaxis()->SetLabelSize(0.0001);
  hh10d->GetYaxis()->SetLabelSize(0.0001);
  hh11d->GetXaxis()->SetLabelSize(0.0001);
  hh11d->GetYaxis()->SetLabelSize(0.0001);
  hh12d->GetXaxis()->SetLabelSize(0.0001);
  hh12d->GetYaxis()->SetLabelSize(0.0001);
  hh13d->GetXaxis()->SetLabelSize(0.0001);
  hh13d->GetYaxis()->SetLabelSize(0.0001);
  hh14d->GetXaxis()->SetLabelSize(0.0001);
  hh14d->GetYaxis()->SetLabelSize(0.0001);
  hh15d->GetXaxis()->SetLabelSize(0.0001);
  hh15d->GetYaxis()->SetLabelSize(0.0001);
  hh16d->GetXaxis()->SetLabelSize(0.0001);
  hh16d->GetYaxis()->SetLabelSize(0.0001);
  hh17d->GetXaxis()->SetLabelSize(0.0001);
  hh17d->GetYaxis()->SetLabelSize(0.0001);
  hh18d->GetXaxis()->SetLabelSize(0.0001);
  hh18d->GetYaxis()->SetLabelSize(0.0001);
  hh19d->GetXaxis()->SetLabelSize(0.0001);
  hh19d->GetYaxis()->SetLabelSize(0.0001);



// convert run number to time stamp
  int runnumber = rn;
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp* tStamp = rt->getBeginTime(runnumber);
  tStamp->print(); cout << endl;

// initialize hardware/software map object
  svxAddress address;
  address.set_usedatabase(1);
  address.setFetchTime(tStamp); 
  address.Initialize();

// read hot/dead map from database
  SvxPixelHotDeadMap pixelMap;
  pixelMap.readFromDatabase(runnumber);

  int countnormal=0;
  int countdead=0;
  int counthot=0;
  int countoutofrange=0;
  int countunknown=0;

  // loop over all pixels and check status
  // first layer 
  for(int ilr=0; ilr<1; ilr++) {       // layer
  for(int ild=0; ild<10; ild++) {      // ladder
    int arm=0; /* west arm */ if(ild>4) {arm=1;} /* east arm */
    for(int isn=0; isn<4; isn++) {     // sensor
      for(int iz=0; iz<128; iz++) {    // horizontal coordinate within sensor (0-127)
        for(int ix=0; ix<256; ix++) {  // vertical coordinate within sensor

          // hardware module corresponds to two softawre sensors
          // module is divided into eight ROCs (four ROCs per sensor) 
          int imodule = address.getModuleSensor0(ilr, ild, isn, ix, iz); // ix and iz are not really needed for module determination
          int iroc = address.getROCSensor0(ilr, ild, isn, ix, iz);       // only iz is needed for ROC determination (not ix)

          int izr = iz%32;                          // hot/dead map requires iz within ROC (0-31)
          int status = pixelMap.getStatus(imodule, iroc, izr, ix);      

          int izg=-1; // horizontal coordinate within ladder
          int ixg=-1; // vertical coordinate within ladder
          if(arm==0) { izg = iz + (3-isn)*128;   ixg = ix; }        // west arm  
          else       { izg = (127-iz) + isn*128; ixg = 255-ix; }    // east arm both coordinates inverted

          if(status== 0) countnormal++;
          if(status==-1) { countdead++; 
                           if(ild==0) h0d->Fill(izg,ixg);
                           if(ild==1) h1d->Fill(izg,ixg);
                           if(ild==2) h2d->Fill(izg,ixg);
                           if(ild==3) h3d->Fill(izg,ixg);
                           if(ild==4) h4d->Fill(izg,ixg);
                           if(ild==5) h5d->Fill(izg,ixg);
                           if(ild==6) h6d->Fill(izg,ixg);
                           if(ild==7) h7d->Fill(izg,ixg);
                           if(ild==8) h8d->Fill(izg,ixg);
                           if(ild==9) h9d->Fill(izg,ixg);
                         }
          if(status== 1) { counthot++;
                           if(ild==0) h0h->Fill(izg,ixg);
                           if(ild==1) h1h->Fill(izg,ixg);
                           if(ild==2) h2h->Fill(izg,ixg);
                           if(ild==3) h3h->Fill(izg,ixg);
                           if(ild==4) h4h->Fill(izg,ixg);
                           if(ild==5) h5h->Fill(izg,ixg);
                           if(ild==6) h6h->Fill(izg,ixg);
                           if(ild==7) h7h->Fill(izg,ixg);
                           if(ild==8) h8h->Fill(izg,ixg);
                           if(ild==9) h9h->Fill(izg,ixg);
                         }
          if(status==-2) countoutofrange++;
          if(status==-3) countunknown++;

        }
      }
    } // sensor
  } // ladder
  } // layer

  cout << "Layer 0:" << endl;
  cout << "normal: " << countnormal << endl;
  cout << "dead:   " << countdead << endl;
  cout << "hot:    " << counthot << endl;
  cout << "others: " << countoutofrange+countunknown << endl;

  countnormal=0;
  countdead=0;
  counthot=0;
  countoutofrange=0;
  countunknown=0;

  // second layer 
  for(int ilr=1; ilr<2; ilr++) {       // layer
  for(int ild=0; ild<20; ild++) {      // ladder
    int arm=0; /* west arm */ if(ild>9) {arm=1;} /* east arm */
    for(int isn=0; isn<4; isn++) {     // sensor
      for(int iz=0; iz<128; iz++) {    // horizontal coordinate within sensor (0-127)
        for(int ix=0; ix<256; ix++) {  // vertical coordinate within sensor

          // hardware module corresponds to two softawre sensors
          // module is divided into eight ROCs (four ROCs per sensor) 
          int imodule = address.getModuleSensor0(ilr, ild, isn, ix, iz); // ix and iz are not really needed for module determination
          int iroc = address.getROCSensor0(ilr, ild, isn, ix, iz);       // only iz is needed for ROC determination (not ix)

          int izr = iz%32;                          // hot/dead map requires iz within ROC (0-31)
          int status = pixelMap.getStatus(imodule, iroc, izr, ix);

          int izg=-1; // horizontal coordinate within ladder
          int ixg=-1; // vertical coordinate within ladder
          if(arm==0) { izg = iz + (3-isn)*128;   ixg = ix; }        // west arm  
          else       { izg = (127-iz) + isn*128; ixg = 255-ix; }    // east arm both coordinates inverted

          if(status== 0) countnormal++;
          if(status==-1) { countdead++;
                           if(ild==0) hh0d->Fill(izg,ixg);
                           if(ild==1) hh1d->Fill(izg,ixg);
                           if(ild==2) hh2d->Fill(izg,ixg);
                           if(ild==3) hh3d->Fill(izg,ixg);
                           if(ild==4) hh4d->Fill(izg,ixg);
                           if(ild==5) hh5d->Fill(izg,ixg);
                           if(ild==6) hh6d->Fill(izg,ixg);
                           if(ild==7) hh7d->Fill(izg,ixg);
                           if(ild==8) hh8d->Fill(izg,ixg);
                           if(ild==9) hh9d->Fill(izg,ixg);
                           if(ild==10) hh10d->Fill(izg,ixg);
                           if(ild==11) hh11d->Fill(izg,ixg);
                           if(ild==12) hh12d->Fill(izg,ixg);
                           if(ild==13) hh13d->Fill(izg,ixg);
                           if(ild==14) hh14d->Fill(izg,ixg);
                           if(ild==15) hh15d->Fill(izg,ixg);
                           if(ild==16) hh16d->Fill(izg,ixg);
                           if(ild==17) hh17d->Fill(izg,ixg);
                           if(ild==18) hh18d->Fill(izg,ixg);
                           if(ild==19) hh19d->Fill(izg,ixg);
                         }
          if(status== 1) { counthot++;
                           if(ild==0) hh0h->Fill(izg,ixg);
                           if(ild==1) hh1h->Fill(izg,ixg);
                           if(ild==2) hh2h->Fill(izg,ixg);
                           if(ild==3) hh3h->Fill(izg,ixg);
                           if(ild==4) hh4h->Fill(izg,ixg);
                           if(ild==5) hh5h->Fill(izg,ixg);
                           if(ild==6) hh6h->Fill(izg,ixg);
                           if(ild==7) hh7h->Fill(izg,ixg);
                           if(ild==8) hh8h->Fill(izg,ixg);
                           if(ild==9) hh9h->Fill(izg,ixg);
                           if(ild==10) hh10h->Fill(izg,ixg);
                           if(ild==11) hh11h->Fill(izg,ixg);
                           if(ild==12) hh12h->Fill(izg,ixg);
                           if(ild==13) hh13h->Fill(izg,ixg);
                           if(ild==14) hh14h->Fill(izg,ixg);
                           if(ild==15) hh15h->Fill(izg,ixg);
                           if(ild==16) hh16h->Fill(izg,ixg);
                           if(ild==17) hh17h->Fill(izg,ixg);
                           if(ild==18) hh18h->Fill(izg,ixg);
                           if(ild==19) hh19h->Fill(izg,ixg);
                         }
          if(status==-2) countoutofrange++;
          if(status==-3) countunknown++;

        }
      }
    } // sensor
  } // ladder
  } // layer

  cout << "Layer 1:" << endl;
  cout << "normal: " << countnormal << endl;
  cout << "dead:   " << countdead << endl;
  cout << "hot:    " << counthot << endl;
  cout << "others: " << countoutofrange+countunknown << endl;

// plot

gStyle->SetPalette(1);
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

c1 = new TCanvas("c1"," ",0,0,500,500);
c1->Divide(1,11,0.00001,0.00001);

c1->cd(11);
h0d->Draw(); h0h->Draw("colsame");
c1->cd(10);
h1d->Draw(); h1h->Draw("colsame");
c1->cd(9);
h2d->Draw(); h2h->Draw("colsame");
c1->cd(8);
h3d->Draw(); h3h->Draw("colsame");
c1->cd(7);
h4d->Draw(); h4h->Draw("colsame");
c1->cd(5);
h5d->Draw(); h5h->Draw("colsame");
c1->cd(4);
h6d->Draw(); h6h->Draw("colsame");
c1->cd(3);
h7d->Draw(); h7h->Draw("colsame");
c1->cd(2);
h8d->Draw(); h8h->Draw("colsame");
c1->cd(1);
h9d->Draw(); h9h->Draw("colsame");

c2 = new TCanvas("c2"," ",0,0,500,900);
c2->Divide(1,21,0.00001,0.00001);

c2->cd(21);
hh0d->Draw(); hh0h->Draw("colsame");
c2->cd(20);
hh1d->Draw(); hh1h->Draw("colsame");
c2->cd(19);
hh2d->Draw(); hh2h->Draw("colsame");
c2->cd(18);
hh3d->Draw(); hh3h->Draw("colsame");
c2->cd(17);
hh4d->Draw(); hh4h->Draw("colsame");
c2->cd(16);
hh5d->Draw(); hh5h->Draw("colsame");
c2->cd(15);
hh6d->Draw(); hh6h->Draw("colsame");
c2->cd(14);
hh7d->Draw(); hh7h->Draw("colsame");
c2->cd(13);
hh8d->Draw(); hh8h->Draw("colsame");
c2->cd(12);
hh9d->Draw(); hh9h->Draw("colsame");
c2->cd(10);
hh10d->Draw(); hh10h->Draw("colsame");
c2->cd(9);
hh11d->Draw(); hh11h->Draw("colsame");
c2->cd(8);
hh12d->Draw(); hh12h->Draw("colsame");
c2->cd(7);
hh13d->Draw(); hh13h->Draw("colsame");
c2->cd(6);
hh14d->Draw(); hh14h->Draw("colsame");
c2->cd(5);
hh15d->Draw(); hh15h->Draw("colsame");
c2->cd(4);
hh16d->Draw(); hh16h->Draw("colsame");
c2->cd(3);
hh17d->Draw(); hh17h->Draw("colsame");
c2->cd(2);
hh18d->Draw(); hh18h->Draw("colsame");
c2->cd(1);
hh19d->Draw(); hh19h->Draw("colsame");




}

