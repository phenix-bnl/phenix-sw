//************************************************************
// PRDF check macro for TOF (Run3 version, used DB) 
//************************************************************

void tofChkPrdf_Run3(const Int_t minEvents=0, const Int_t maxEvents=1000, 
		     const char *prdfIFile="phnx.prdf",  
		     const Int_t TVCMIN = 10, const Int_t TVCMAX = 3800, 
		     const Int_t QVCMIN = 10, const Int_t QVCMAX = 4100)
{   
  gROOT->Reset();
  gStyle->SetOptFit();
  Int_t eventNumber = 0;
  int trigLaser = 1;
  trigLaser = 0x40000000; //laser
  
  // Initialization
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");
  gSystem->Load("libtof.so");

  Int_t verbose = 7;

   // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* tofNode = new PHCompositeNode("TOF");
  topNode->addNode(tofNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  PHNodeReset reset;
  
  // Set up input and output files
  PHString prdfInFile = prdfIFile;
  Event *thisEvent = 0;
  PHString prdfInFile = prdfIFile;

  Event *thisEvent = 0;

  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfInFile.getString());
  
  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2003,1,20,0,0,0);
  
  // Set up the modules
  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  mTofUnpackModule* mTofUnpack = new mTofUnpackModule;
  
  // Initialize the tables
  size_t mr=1000;
  dTofRawWrapper* dTofRaw = new dTofRawWrapper("dTofRaw",mr);
  PHIODataNode<PHTable>* dTofRawNode = new PHIODataNode<PHTable>(dTofRaw,"dTofRaw");
   tofNode->addNode(dTofRawNode);

   size_t mr=8;
   dTofDCMWrapper* dTofDCM = new dTofDCMWrapper("dTofDCM",mr);
   PHIODataNode<PHTable>* dTofDCMNode = new PHIODataNode<PHTable>(dTofDCM,"dTofDCM");
   dcmNode->addNode(dTofDCMNode);

   PhRootHistogramFactory::buildFactory();

   mainIter.cd();

   // Histogram Setting
   gROOT->cd();
   Int_t itof, ntof;
   TH1F *tofhist1 = new TH1F("tofhist1","TOF hits as a function of Event ID",
 			    maxEvents - minEvents + 4,minEvents-2,(Float_t)maxEvents+2);
 	//tofhist1.SetXTitle("test");
   TH1F *tofhist2 = new TH1F("tofhist2","TOF hits as a function of Slat ID",1000,0.0,1000.0);
   TH1F *tofhist3 = new TH1F("tofhist3","TOF Multiplicity distribution",26,0,25);
   char title[80];
   sprintf(title,"Q3 - Q1 (charge distribution)  [%d - %d]",QVCMIN, QVCMAX);
   TH1F *tofCharge = new TH1F("tofCharge",title,250,0,2000);
   sprintf(title,"Q1 (crossing 1) [%d - %d]",QVCMIN, QVCMAX);
   TH1F *tofQ1 = new TH1F("tofQ1",title,105,0.0,4100);
   sprintf(title,"Q3 (crossing 3) [%d - %d]",QVCMIN, QVCMAX);
   TH1F *tofQ3 = new TH1F("tofQ3",title,105,0.0,4100);

   sprintf(title,"tvc  (%d - %d)",TVCMIN, TVCMAX);
   TH1F *tofTvc = new TH1F("tofTvc",title,105,0.0,4200);
   sprintf(title,"T3 (crossing 3) [%d - %d]",TVCMIN, TVCMAX);
   TH1F *tofT3 = new TH1F("tofT3",title,105,0.0,4200);
   sprintf(title,"T4 (crossing 4) [%d - %d]",TVCMIN, TVCMAX);
   TH1F *tofT4 = new TH1F("tofT4",title,105,0.0,4200);

   TH1F *tofTdiff = new TH1F("tofTdiff","T3 Upper and Lower PMT difference",
 			    200,-1000,1000);
   TH1F *tofTsum = new TH1F("tofTsam","TOF (T3_low + T3_up)/2",
 			    200,0.0,4200);
   TH2F *tofChargelu = new TH2F("tofChargelu","Charge Correlation between Upper and Lower PMT",
 			    100,0.,2000., 100,0.,2000);
   TH2F *tofTvclu = new TH2F("tofTvclu","T3 Correlation between Upper and Lower PMT",
 			    100,0.,4200., 100,0.,4200);
   TH2F *tofQT = new TH2F("tofhist9","Correlation between Charge and T3",
 			    100,0.,4200., 100,0.,2000);

   TH2F *tofYZ = new TH2F("tofYZ","Hit slat in Y-Z plane",280,-210, 210, 40,-300,100);

  if (verbose>5) printf("Entering event loop.\n");
  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

    // Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    if (verbose>10) printf("Calling first event only modules.\n");
    if (eventNumber == 1) {
      if (verbose>10) printf("Calling TofAddress\n");
      TofAddress->setTimeStamp(TimeStamp);
      TofAddress->fetch();
      
      if (verbose>10) printf("Calling TofGeometry\n");
      TofGeometry->setTimeStamp(TimeStamp);
      TofGeometry->fetch();
    }
    if(eventNumber < minEvents) continue;
    // GL1 info.
    int trigger = 0;
    int cross = 0;
    Packet *pGL1;
    if((pGL1 = thisEvent->getPacket(14001)) != 0)
      {
	trigger = pGL1->iValue(0,"SCALEDTRIG");
	// cross   = pGL1->iValue(0,"CROSSCTR");
	// cout << "trigger = " << hex << trigger <<endl;
      }
    
    if (verbose>5) if(eventNumber%100==0) printf("Fetched event %d\n",eventNumber);
    if (verbose>10) printf("Calling event modules\n");
    if (verbose>10) printf("Calling TofGetDCM\n");
    TofGetDCM(topNode);
    
    if((trigger & trigLaser) ==0)
      {
	
	if (verbose>10) printf("Calling mTofUnpack\n");
	mTofUnpack->event(topNode, TofAddress);
	
	// Histogram Filling
	ntof = 0;
	itof = 0;
	
	for(itof = 0; itof < dTofRaw->RowCount(); itof++) 
	  {	
	    if((sqrt(dTofRaw->get_qvc(0,itof) * dTofRaw->get_qvc(1,itof))>150.0)&&
	       (((dTofRaw->get_t3(0,itof)>TVCMIN&&dTofRaw->get_t3(0,itof)<TVCMAX)&&
		 (dTofRaw->get_t3(1,itof)>TVCMIN&&dTofRaw->get_t3(1,itof)<TVCMAX))||
		((dTofRaw->get_t4(0,itof)>TVCMIN&&dTofRaw->get_t4(0,itof)<TVCMAX)&&
		 (dTofRaw->get_t4(1,itof)>TVCMIN&&dTofRaw->get_t4(1,itof)<TVCMAX))))
	      {
		Int_t slatid = dTofRaw->get_slatid(itof);
		
		tofhist1->Fill((Float_t) eventNumber);
		tofhist2->Fill((Float_t) slatid);
		
		tofCharge->Fill((Float_t) dTofRaw->get_qvc(0,itof));
		tofCharge->Fill((Float_t) dTofRaw->get_qvc(1,itof));
		tofQ1->Fill((Float_t) dTofRaw->get_q1(0,itof));
		tofQ1->Fill((Float_t) dTofRaw->get_q1(1,itof));
		tofQ3->Fill((Float_t) dTofRaw->get_q2(0,itof));
		tofQ3->Fill((Float_t) dTofRaw->get_q2(1,itof));
		
		tofTvc->Fill((Float_t) dTofRaw->get_tvc(0,itof));
		tofTvc->Fill((Float_t) dTofRaw->get_tvc(1,itof));
		tofT3->Fill((Float_t) dTofRaw->get_t3(0,itof));
		tofT3->Fill((Float_t) dTofRaw->get_t3(1,itof));
		tofT4->Fill((Float_t) dTofRaw->get_t4(0,itof));
		tofT4->Fill((Float_t) dTofRaw->get_t4(1,itof));
		
		Int_t diff = dTofRaw->get_t3(0,itof) - dTofRaw->get_t3(1,itof);
		if(dTofRaw->get_t3(0,itof)<TVCMAX)tofTdiff->Fill((Float_t) diff/2.);
		Int_t diff = dTofRaw->get_t4(0,itof) - dTofRaw->get_t4(1,itof);
		if(dTofRaw->get_t4(0,itof)<TVCMAX)tofTdiff->Fill((Float_t) diff/2.);
	    
		tofTsum->Fill(((Float_t) dTofRaw->get_tvc(0,itof) +
			       (Float_t) dTofRaw->get_tvc(1,itof))/2);
		
		tofChargelu->Fill((Float_t) dTofRaw->get_qvc(0,itof),
				  (Float_t) dTofRaw->get_qvc(1,itof));
		
		tofTvclu->Fill((Float_t) dTofRaw->get_tvc(0,itof),
			       (Float_t) dTofRaw->get_tvc(1,itof));
		
		tofQT->Fill((Float_t) dTofRaw->get_tvc(0,itof),
			    (Float_t) dTofRaw->get_qvc(0,itof));
		tofQT->Fill((Float_t) dTofRaw->get_tvc(1,itof),
			    (Float_t) dTofRaw->get_qvc(1,itof));
		
		tofYZ->Fill(TofGeometry->getZpos(slatid),TofGeometry->getYpos(slatid));
		
		ntof++;
	      }
	  }
	if(ntof >= 0) tofhist3->Fill((Float_t)ntof);
      } // endif trig
    
    // Reset all data for this event
    if (mainIter.cd("DCM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TOF")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
  }
  
  // Let's look at the health histograms
  char *w = new char[1];
  //  TCanvas *c1  = new TCanvas("c1","TOF Plot",500,500); 
  tofhist1.SetXTitle("Event ID Number");
  tofhist2.SetXTitle("Slat ID Number");
  tofhist3.SetXTitle("Number of hist on TOF");
  tofQ1.SetXTitle("Q1 [ch]");
  tofQ3.SetXTitle("Q3 [ch]");
  tofT3.SetXTitle("T3 [ch]");
  tofT4.SetXTitle("T4 [ch]");
  tofCharge.SetXTitle("Q3 - Q1 (charge) [ch]");
  tofTvclu.SetXTitle("T3 (lower PMT) [ch]");
  tofTvclu.SetYTitle("T3 (upper PMT) [ch]");
  tofChargelu.SetXTitle("Q3 - Q1 (lower PMT) [ch]");
  tofChargelu.SetYTitle("Q3 - Q1 (upper PMT) [ch]");
  tofTdiff.SetXTitle("T3 : (lower PMT - upper PMT)/2 [ch]");
  tofQT.SetXTitle("Q3 - Q1 (charge) [ch]");
  tofQT.SetYTitle("T3 [ch]");
  tofYZ.SetXTitle("z [cm]");
  tofYZ.SetYTitle("y [cm]");
  //
  tofhist1.GetXaxis()->SetTitleOffset(1.3);
  tofhist2.GetXaxis()->SetTitleOffset(1.3);
  tofhist3.GetXaxis()->SetTitleOffset(1.3);
  tofQ1.GetXaxis()->SetTitleOffset(1.3);
  tofQ3.GetXaxis()->SetTitleOffset(1.3);
  tofT3.GetXaxis()->SetTitleOffset(1.3);
  tofT4.GetXaxis()->SetTitleOffset(1.3);
  tofCharge.GetXaxis()->SetTitleOffset(1.3);
  tofTdiff.GetXaxis()->SetTitleOffset(1.3);
  tofChargelu.GetXaxis()->SetTitleOffset(1.3);
  tofChargelu.GetYaxis()->SetTitleOffset(1.3);
  tofTvclu.GetXaxis()->SetTitleOffset(1.3);
  tofTvclu.GetYaxis()->SetTitleOffset(1.3);
  tofYZ.GetXaxis()->SetTitleOffset(1.3);
  tofYZ.GetYaxis()->SetTitleOffset(1.3);
  //
  tofhist1.SetLineWidth(3);
  tofhist2.SetLineWidth(3);
  tofhist3.SetLineWidth(3);
  tofQ1.SetLineWidth(3);
  tofQ3.SetLineWidth(3);
  tofT3.SetLineWidth(3);
  tofT4.SetLineWidth(3);
  tofCharge.SetLineWidth(3);
  tofTdiff.SetLineWidth(3);
  //
  tofhist1.SetLineColor(2);
  tofhist2.SetLineColor(6);
  tofhist3.SetLineColor(6);
  //tofYZ.SetLineColor(6);
  
  
  //***
  TCanvas *c1  = new TCanvas("c1","TOF Plot1",600,600); 
  c1->Divide(2,2);
  c1->cd(1);
  tofhist1->Draw();
  c1->cd(2);
  tofhist2->Draw();
  c1->cd(3);
  c1_3->SetLogy();
  tofhist3->Draw();
  c1->Update();

  TCanvas *c2  = new TCanvas("c2","TOF Plot2",600,600); 
  tofQ1->SetFillColor(5);
  tofQ3->SetFillColor(5);
  tofT3->SetFillColor(5);
  tofT4->SetFillColor(5);
  c2->Divide(2,2);
  
  c2->cd(1); tofQ1->Draw(); 
  c2->cd(2); tofQ3->Draw();
  c2->cd(3); tofT3.Fit("gaus"); tofT3->Draw();
  c2->cd(4); tofT4->Draw();
  c2->Update();
  
  //****
  TCanvas *c3  = new TCanvas("c3","TOF Plot3",600,600); 
  c3->Divide(2,2);
  c3->cd(1);
  tofCharge->Draw();
  c3->cd(2);
  tofTdiff.Fit("gaus");
  tofTdiff->Draw();
  c3->cd(3);
  tofChargelu->Draw("colz");
  c3_3->SetGridx();
  c3_3->SetGridy();
  c3->cd(4);
  tofTvclu->Draw("colz");
  c3_4->SetGridx();
  c3_4->SetGridy();
  c3->Update();
  //tofQT->Draw();
  
  //   TCanvas *cc = new TCanvas("cc","TOF Plot",600,400); 
  //   tofYZ->SetFillColor(2);
  //   tofYZ->SetLineColor(6);
  //   tofYZ->SetMarkerStyle(8);
  //   tofYZ->SetMarkerSize(0.7);
  //   tofYZ->SetMarkerColor(2);
  //   cc->SetGridx();
  //   cc->SetGridy();
  //   tofYZ->Draw();
  //   cc->Update();
  //   //
  
  //   TBox a( 150,-100, 200,  80);
  //   TBox b( 100,-100, 150,  80);
  //   TBox c(  50,-100, 100,  80);
  //   TBox d(   0,-100,  50,  80);
  //   TBox f( -50,-100,   0,  80);
  //   TBox g(-100,-100, -50,  80);
  //   TBox h(-150,-100,-100,  80);
  //   TBox i(-200,-100,-150,  80);
  //   TBox e(   0,-270,  50,-100);
  //   TBox j( -50,-270,   0,-100);
  //   //
  //   a->SetFillStyle(0);
  //   b->SetFillStyle(0);
  //   c->SetFillStyle(0);
  //   d->SetFillStyle(0);
  //   e->SetFillStyle(0);
  //   f->SetFillStyle(0);
  //   g->SetFillStyle(0);
  //   h->SetFillStyle(0);
  //   i->SetFillStyle(0);
  //   j->SetFillStyle(0);
  //   //  
  //   a.Draw(); b.Draw(); c.Draw(); d.Draw();
  //   f.Draw(); g.Draw(); h.Draw(); i.Draw();
  //   e.Draw(); j.Draw();
  //   cc.Update();
  //  tofYZ->SetGridx();
  //  tofYZ->SetGridy();
  
  //  TText t1(100, -200, "North");
  //  t1->SetTextColor(6);
  //  t1.Draw(); 
  //  TText t2(-100, -200, "South");
  //  t2->SetTextColor(6);
  //  t2.Draw(); 

  c1->SaveAs("tof_c1.gif");
  c2->SaveAs("tof_c2.gif");
  c3->SaveAs("tof_c3.gif");
}

