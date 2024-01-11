 { 


   //-----------------------------------
   gSystem->Load("./.libs/libclusttr.so");
   TH1F* h1 = new TH1F("h1","test",100,0,10);
   TH1F* h2 = new TH1F("h2","test",100,5,15);
   TH1F* h3 = new TH1F("h3","test",100,0,10);
   gStyle->SetOptStat(1111111);
   h2->Fill(2.5);
   h2->Fill(7.5);
   h2->Fill(12.5);
   h2->Fill(17.5);
   h2->Draw();
   SumupTH1(h3,h1,h2,0);

   //-----------------------------------
   gSystem->Load("./.libs/libclusttr.so");
   TH2F* hh1 = new TH2F("hh1","test",20,0,10,3,0,3);
   TH2F* hh2 = new TH2F("hh2","test",20,5,15,3,0,3);
   TH2F* hh3 = new TH2F("hh3","test",20,0,10,3,0,3);
   gStyle->SetOptStat(1111111);
   hh2->Fill(2.5,1);
   hh2->Fill(7.5,2);
   hh2->Fill(12.5,1);
   hh2->Fill(17.5,1);
   hh2->Draw("boxtext");
   TH1F* hshift = new TH1F("hshift","shift da",3,0,3);
   hshift->SetBinContent(2,-7);
   SumupTH2(hh3,hh1,hh2);
   SumupTH2(hh3,hh1,hh2,0,hshift);

   //-----------------------------------

   gSystem->Load("./.libs/libclusttr.so");
   gStyle->SetOptStat(1111111);
   TH1Xch* th1 = new TH1Xch("th1","test",3,20,0,10);
   TH1Xch* th2 = new TH1Xch("th2","test",3,20,5,15);
   th2->Fill(1,2.5);
   th2->Fill(1,7.5);
   th2->Fill(2,12.5);
   th2->Fill(1,17.5);
   th2->h_all.Draw();
   th2->h2_ch.Draw("boxtext");
   TH1F* hshift1 = new TH1F("hshift1","shift da",3,0,3);
   hshift1->SetBinContent(3,-6);
   th1->Sumup(th2,hshift1);

   //-----------------------------------
   gSystem->Load("./.libs/libclusttr.so");
   gStyle->SetOptStat(1111111);
   TH1Xch* th1xch = new TH1Xch("th1xch","test",3,20,0,10);
   th1xch->h_all->Fill(5.5);

   TFile* nf = new TFile("test.root","RECREATE");
   th1xch->Write();
   nf->Write();
   nf->Close();

   //-----------------------------------
   gSystem->Load("./.libs/libclusttr.so");
   gStyle->SetOptStat(1111111);

   TFile* nf = new TFile("test.root","RECREATE");
   CalibHist* clh = new CalibHist("test","test data",10);
   clh->SetOptBuffersize(3);
   clh->SetOptPrefit(true);
   clh->SetOptWindow(10);
   clh->SetOptNbin(300);
   clh->SetOptBinwidth(0.2);

   clh->Fill(100,0,0,100);
   clh->Fill(100,0,1,150);
   clh->Fill(100,0,2,50);
   clh->Fill(100,0,3,103);

   clh->Fill(101,0,0,100);
   clh->Fill(101,0,1,150);
   clh->Fill(101,0,2,50);
   clh->Fill(101,0,3,103);

   //TFile* nf = new TFile("test.root","RECREATE");
   clh->Write();

   nf->Close();

   //-----------------------------------

   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   TFile* f = new TFile("/phenix/data08/microDSTv03_1/ntuple/CCJ/run0000012397_0000.root");
   clust_calib();

   //-----------------------------------
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   TFile* f = new TFile("test.root");
   CalibHist* c = new CalibHist("test","test test",10);
   c->Read();
   c->Print();

   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   //gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   gSystem->Load("/phenix/u/htorii/local/photon/clusttr/.libs/libclusttr.so");
   gStyle->SetOptStat(1111111);
   CalibHist* calh_sc = new CalibHist("calh_sc","TOF Calibration PbSc",5184);
   CalibHist* calh_gl = new CalibHist("calh_gl","TOF Calibration PbGl",4608);
   TFile* f;
   f = new TFile("/phenix/data24/htorii/Myana_v04tof/calhist/v04DSTkoichi_data0_5/uDST_v04-10126-1.root")
   calh_sc->Read();
   calh_gl->Read();
   f = new TFile("/phenix/data24/htorii/Myana_v04tof/calhist/v04DSTkoichi_data0_5/uDST_v04-10126-9.root")
   calh_sc->Read();
   calh_gl->Read();
   f = new TFile("/phenix/data24/htorii/Myana_v04tof/calhist/v04DSTkoichi_data0_5/uDST_v04-10152-0.root")
   calh_sc->Read();
   calh_gl->Read();

   gROOT->GetListOfFiles()->ls();

   //EmcTofCalib* etc = calh_sc->Analyze("D");
   EmcTofCalib* etc = calh_sc->Analyze();

   etc->_th1xch->h_all->Print();
   calh_sc->Read(4630,0);
   calh_sc->_th1xch->h_all->Print();
   calh_sc->Read(4630,1);
   calh_sc->_th1xch->h_all->Print();
   calh_sc->Read(4634,0);
   calh_sc->_th1xch->h_all->Print();


   TH1F* h = calh_sc->_th1xch->h_all;
   TH1F* hout = new TH1F("hout","test",300,-10,50);

   SumupTH1(hout,hout,h,0,37.0,"D");
   //
   // End of looping...................

   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   //gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   gSystem->Load("/phenix/u/htorii/local/photon/clusttr/.libs/libclusttr.so");
   gStyle->SetOptStat(1111111);
   gStyle->SetOptFit();
   CalibHist* calh_sc = new CalibHist("calh_sc","TOF Calibration PbSc",5184);
   EmcTofCalib* emctof_sc = new EmcTofCalib("emctof_sc","TOF Calibration parameter");
   EmcMIPCalib* emcmip_sc = new EmcMIPCalib("emcmip_sc","MIP Calibration parameter");

   //calh_sc->Read("/phenix/data24/htorii/Myana_v04tof/calhist/v04DSTkoichi_data0_5_udst.root");
   calh_sc->Read("/phenix/data24/htorii/Myana_v04tof/calhist/v04DSTkoichi_data0_5/uDST_v04-12397-20.root");
   calh_sc->Read("/phenix/data24/htorii/Myana_v04tof/calhist/v04DSTkoichi_data0_5/uDST_v04-12397-21.root");
   //calh_sc->Read("/phenix/data24/htorii/Myana_v04tof/calhist/v04DSTkoichi_data0_5/uDST_v04-12322-0.root");
   
   gROOT->GetListOfFiles()->ls();

   emctof_sc->AnalyzeRunAll(calh_sc);
   //
   emctof_sc->AnalyzeTwr("");
   //
   emctof_sc->_gra_run->Draw("AL*");
   emctof_sc->_th1xch->h_all->Draw();
   emctof_sc->_th1xch->h2_ch->Draw();

   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   //gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   gSystem->Load("/phenix/u/htorii/local/photon/clusttr/.libs/libclusttr.so");
   gStyle->SetOptStat(1111111);
   gStyle->SetOptFit();
   EmcTofCalib* emctof_sc;
   TFile* f = new TFile("/phenix/data24/htorii/Myana_v04tof/calhist/test.root");
   emctof_sc = (EmcTofCalib*) f->Get("emctof_sc");
   emctof_sc->_gra_run->Draw("*AP");

   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   TFile* f = new TFile("./run_23530_0.root");
   calh_mip_sm_sc = new CalibHist("calh_mip_sm_sc","Energy MIP Calibration PbSc",172);
   calh_mip_sm_sc->Read();
   EmcMIPCalib* emcmip_sc = new EmcMIPCalib("emcmip_sc","MIP Calibration parameter",172,100,0,1);
   emcmip_sc->AnalyzeAdd(calh_mip_sm_sc);
   emcmip_sc->Print();
   emcmip_sc->AnalyzeSlope("DP");
   emcmip_sc->_h_twr_slope->Draw();

   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   //   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   gSystem->Load("/phenix/u/htorii/local/photon/clusttr/.libs/libclusttr.so");

//   TFile*  f = new TFile("/phenix/data24/htorii/01udst/hpss/uDST_v00-22173-0.root");
   TTree* nt_evt = (TTree*)(gROOT->FindObject("nt_evt"));
   TTree* nt_trk = 0 ; // = (TTree*)(gROOT->FindObject("nt_trk"));
   TTree* nt_emc = (TTree*)(gROOT->FindObject("nt_emc"));
   
   Evt* evt =  new Evt();
   evt->Init_run1udst(nt_evt,nt_trk,nt_emc);

   TH1F* h_tof = new TH1F("h_tof","TDC",1000,-100,100);
   TH2F* h2_etof = new TH2F("h2_etof","TDC vs ADC",100,0,1.0,200,-100,100);
   int itwr,isect;
   TH1F* h_itwr = new TH1F("h_itwr","itwr dist",1000,0,20000);
   {
     while ( evt->Next() && evt->_current_evn < 100 ){
       vector<Clust>& vec_clt = evt->_vec_clt;
       int nclt = vec_clt.size();
       while( nclt-- ){
	 Clust& clt = vec_clt[nclt];
	 //calibhist->Fill(glb.run,glb.seq,1,clt.tofcorr);
	 //cout<<" clt.tofcorr = "<<clt.tofcorr<<endl;
	 //itwr = (clt.arm==1 ? 4608*(clt.sector-1)+96*clt.ind[1]+clt.ind[0] : 2592*clt.sector+72*clt.ind[1]+clt.ind[0] );
	 isect = clt.arm==1 ? clt.sector-2+clt.arm*4 : clt.sector+clt.arm*4;
	 itwr = 2592*isect+72*clt.ind[1]+clt.ind[0];
	 //cout<<" itwr = "<<itwr<<endl;
	 cout<<" arm,sector,ind[0,1],itwr = "<<clt.arm<<" , "<<clt.sector<<" , "<<clt.ind[0]<<" , "<<clt.ind[1]<<" , "<<itwr<<endl;
	 h_itwr->Fill(itwr);
	 h_tof->Fill(clt.tofcorr);
	 h2_etof->Fill(clt.ecore,clt.tofcorr);
       }
     }
   }
   h_tof->Draw();
   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   //gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   gSystem->Load("./.libs/libclusttr.so");
   //TFile* f = new TFile("/phenix/data24/htorii/01udst/hpss/uDST_v00-22173-0.root");
   TFile* f = new TFile("/phenix/data24/htorii/01udst/v080701/uDST_v00-27896-1.root");
   clust_calib();
   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   int i;
   //
   EmcQACalib* qa = new EmcQACalib("qa","test",100);
   i = 50000; while(i--) qa->Fill(gRandom->Integer(100),gRandom->Gaus());
   qa->SetOptSigma(2);
   qa->Analyze();
   //
   EmcQACalib* qa2 = new EmcQACalib("qa2","test",100);
   i = 10000; while(i--) qa2->Fill(gRandom->Integer(100),gRandom->Gaus());
   qa2->SetOptSigma(2);
   qa2->Analyze();

   (*qa2) = (*qa2) + (*qa);

   qa2->Print();


   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   //gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
   gSystem->Load("./.libs/libclusttr.so");
   TFile* f = new TFile("/phenix/data24/htorii/01udst/hpss/uDST_v00-22173-0.root");
   clust_calib();

   //------------------------

   //------------------------
   //   CalibObjQA* qa_sect[8];
   //   qa_sect[0] = (CalibObjQA*) f->Get("qa_sect0");
   //   EmcCalibratorQA* c_qa_sect[0] = new EmcCalibratorQA("c_qa_sect0","calibrator qa sector0",2592);
   //   c_qa_sect[0]->SetOptDebug();
   //   c_qa_sect[0]->Analyze(tof_sect[0]);


   //------------------------

   TH1F* h_e = new TH1F("h_e","Energy",50,0,1);
   TH1F* h_e_s = new TH1F("h_e_s","Energy",50,0,1);
   TH1F* h_e_d = new TH1F("h_e_d","Energy",50,0,1);
   nt_trk->Draw("e>>h_e","mom>0.3&&mom<5&&e<1&&emcx>0&&abs(pemcz-emcz)<10&&abs(pemcy-emcy)<10");
   nt_trk->Draw("se>>h_e_s","mom>0.3&&mom<5&&se<1&&semcx>0&&abs(spemcz-semcz)<10&&abs(pemcy-semcy)<10");
   h_e_d->Add(h_e,h_e_s,1,-1);
   h_e_d->Draw();

   TH1F* h_prob = new TH1F("h_prob","Photon Probability",50,0,1);
   TH1F* h_prob_s = new TH1F("h_prob_s","Photon Probability",50,0,1);
   TH1F* h_prob_d = new TH1F("h_prob_d","Photon Probability",50,0,1);
   nt_trk->Draw("prob>>h_prob","mom>0.3&&mom<5&&e<1&&emcx>0&&abs(pemcz-emcz)<10&&abs(pemcy-emcy)<10&&abs(e-0.3)<0.1");
   nt_trk->Draw("sprob>>h_prob_s","mom>0.3&&mom<5&&se<1&&semcx>0&&abs(spemcz-semcz)<10&&abs(pemcy-semcy)<10&&abs(se-0.3)<0.1");
   h_prob_d->Add(h_prob,h_prob_s,1,-1);
   h_prob_d->Draw();

   TH2F* h_probz = new TH2F("h_probz","Photon Probability vs z",50,-2.5,2.5,50,0,1);
   TH2F* h_probz_s = new TH2F("h_probz_s","Photon Probability vs z",50,-2.5,2.5,50,0,1);
   TH2F* h_probz_d = new TH2F("h_probz_d","Photon Probability vs z",50,-2.5,2.5,50,0,1);
   nt_trk->Draw("prob:emcz>>h_probz","mom>0.3&&mom<5&&e<1&&emcx>0&&abs(pemcz-emcz)<10&&abs(pemcy-emcy)<10&&abs(e-0.3)<0.1");
   nt_trk->Draw("sprob:semcz>>h_probz_s","mom>0.3&&mom<5&&se<1&&semcx>0&&abs(spemcz-semcz)<10&&abs(pemcy-semcy)<10&&abs(se-0.3)<0.1");
   h_probz_d->Add(h_probz,h_probz_s,1,-1);
   h_probz_d->Draw("colz");

   TH2F* h_probe = new TH2F("h_probe","Photon Probability vs e",50,0,1.0,50,0,1);
   TH2F* h_probe_s = new TH2F("h_probe_s","Photon Probability vs e",50,0,1,50,0,1);
   TH2F* h_probe_d = new TH2F("h_probe_d","Photon Probability vs e",50,0,1,50,0,1);
   nt_trk->Draw("prob:e>>h_probe","mom>0.3&&mom<5&&e<1&&emcx>0&&abs(pemcz-emcz)<10&&abs(pemcy-emcy)<10");
   nt_trk->Draw("sprob:se>>h_probe_s","mom>0.3&&mom<5&&se<1&&semcx>0&&abs(spemcz-semcz)<10&&abs(pemcy-semcy)<10");
   h_probe_d->Add(h_probe,h_probe_s,1,-1);
   h_probe_d->Draw("colz");

   TH2F* h_probe0 = new TH2F("h_probe0","Photon Probability vs e",50,0,1.0,50,0,1);
   TH2F* h_probe0_s = new TH2F("h_probe0_s","Photon Probability vs e",50,0,1,50,0,1);
   TH2F* h_probe0_d = new TH2F("h_probe0_d","Photon Probability vs e",50,0,1,50,0,1);
   nt_trk->Draw("prob:e>>h_probe0","mom>0.0&&mom<5&&e<1&&emcx>0&&abs(pemcz-emcz)<10&&abs(pemcy-emcy)<10");
   nt_trk->Draw("sprob:se>>h_probe0_s","mom>0.0&&mom<5&&se<1&&semcx>0&&abs(spemcz-semcz)<10&&abs(pemcy-semcy)<10");
   h_probe0_d->Add(h_probe0,h_probe0_s,1,-1);
   h_probe0_d->Draw("colz");

   TH2F* h_probm = new TH2F("h_probm","Photon Probability vs mom",50,0,1.0,50,0,1);
   TH2F* h_probm_s = new TH2F("h_probm_s","Photon Probability vs mom",50,0,1,50,0,1);
   TH2F* h_probm_d = new TH2F("h_probm_d","Photon Probability vs mom",50,0,1,50,0,1);
   nt_trk->Draw("prob:mom>>h_probm","mom>0.0&&mom<5&&e<1&&emcx>0&&abs(pemcz-emcz)<10&&abs(pemcy-emcy)<10&&abs(e-0.3)<0.1");
   nt_trk->Draw("sprob:mom>>h_probm_s","mom>0.0&&mom<5&&se<1&&semcx>0&&abs(spemcz-semcz)<10&&abs(pemcy-semcy)<10&&abs(e-0.3)<0.1");
   h_probm_d->Add(h_probm,h_probm_s,1,-1);
   h_probm_d->Draw("colz");


   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");                   
   gSystem->Load("/phenix/u/htorii/local/photon/clusttr/.libs/libclusttr.so");

   TH0Xch ttt("ttt","test",100);
   TH0Xch test("test","test",100);
   test = ttt;

   TFile* f = new TFile("/phenix/data24/htorii/Myana_01ana/tofcalib/Step7/chain_201_300init.root") ;
   CalibRunsTH0* qa_ene_sect7 = new CalibRunsTH0("qa_ene_sect7","test",4608);
   EmcCalibratorQA* c_qa_ene_sect7 = new EmcCalibratorQA("c_qa_ene_sect7","cqa ene 7",4608);
   qa_ene_sect7->Read(f);
   qa_ene_sect7->Print();

   c_qa_ene_sect7->AnalyzeCalibObj(qa_ene_sect7->_calibobj,"d");
   c_qa_ene_sect7->AnalyzeTwr("d");

   //------------------------
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");                   
   gSystem->Load("/phenix/u/htorii/local/photon/clusttr/.libs/libclusttr.so");
   gStyle->SetOptStat(1111111);
   TF1* fx = new TF1("fx","x+0.5");
   TF2* fy = new TF2("fy","0.5*x+y");
   TF3* fz = new TF3("fz","z");

   TH1F* h1 = new TH1F("h1","test",10,0,10);
   TH1F* h1n = new TH1F("h1n","test",10,0,10);
   h1->Fill(1,1);
   SumupTH1(h1n,h1,fx,"d");
   h1n->Draw();

   TH2F* h2 = new TH2F("h2","test",10,0,10,10,0,10);
   TH2F* h2n = new TH2F("h2n","test",10,0,10,10,0,10);
   h2->Fill(1,1);
   SumupTH2(h2n,h2,fx,fy,"d");
   h2n->Draw("boxtext");

   TH3F* h3 = new TH3F("h3","test",2000,0,10,250,0,10,5,0,10);
   TH3F* h3n = new TH3F("h3n","test",2000,0,10,250,0,10,5,0,10);
   h3->Fill(1,1,1);
   SumupTH3(h3n,h3,fx,fy,fz,"d");
   h3n->Draw("boxtext");

   TH1Xch* th1 = new TH1Xch("th1","test",10,10,0,10);
   TH1Xch* th1n = new TH1Xch("th1n","test",10,10,0,10);
   th1->Fill(5,5);
   th1->_h2_ch->Draw("boxtext");
   th1n->Sumup(th1,fx,fy,"");

   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");                   
   gSystem->Load("/phenix/u/htorii/local/photon/clusttr/.libs/libclusttr.so");

   TH0Xch ttt("ttt","test",100);
   TH0Xch test("test","test",100);
   test = ttt;

   //================================================================================
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");
   //   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");

   TFile* nf = new TFile("test.root","RECREATE");
   CalibRunsTH1* th1_tdcped = new CalibRunsTH1("th1_tdcped","TDC pedestal vs ch",24768,1000,500,1500);
   th1_tdcped->Print();
   th1_tdcped->Fill(20000,0,100,600);

   //================================================================================
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");
   //   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");

   CalibRunsTH1* th1_tofped = new CalibRunsTH1("th1_tofped","TDC pedestal vs ch",24768,1000,500,1500);
   th1_tofped->Read("test.root");

   //================================================================================
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   TFile* nf = new TFile("test.root","RECREATE");
   EmcCalibratorTOFPed* caltofped = new EmcCalibratorTOFPed("caltofped","TOF pedestal calibration",10);
   caltofped->Write();
   delete caltofped;
   .q;

   root -b -l
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   TFile* f= new TFile("test.root");
   



   //================================================================================
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   //gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");

   CalibRunsTH1* th1_tofped = new CalibRunsTH1("th1_tofped","TDC pedestal vs ch",24768,1000,500,1500);
   th1_tofped->Read("/phenix/data23/htorii/amuped/hist_33095_8.root");
   th1_tofped->Read("/phenix/data23/htorii/amuped/hist_33095_9.root");
   th1_tofped->Read("/phenix/data23/htorii/amuped/hist_33098_0.root");

   TFile* nf = new TFile("test.root","RECREATE");
   EmcCalibratorTOFPed* caltofped = new EmcCalibratorTOFPed("caltofped","TOF pedestal calibration",24768);
   caltofped->AnalyzeCalibRuns(th1_tofped);
   //   caltofped->_calibruns->Read(33099,0);
   //   ((TH1Xch*)caltofped->_calibruns->_calibobj)->_h_all->Print();
   //   caltofped->_calibruns->Read(33095,0);
   //   ((TH1Xch*)caltofped->_calibruns->_calibobj)->_h_all->Print();
   caltofped->Write();
   delete caltofped;
   nf->Close();
   delete th1_tofped;
   //
   //
   //------
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libcalibset2.so");
   //gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   TFile* nf = new TFile("test.root");
   EmcCalibratorTOFPed* caltofped = (EmcCalibratorTOFPed*)nf->Get("caltofped");
   caltofped->_calibruns->Reset();
   caltofped->_calibruns->Read(nf);
   //caltofped->AnalyzeTwr("d");
   caltofped->AnalyzeTwr();
   

   //================================================================================
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libcalibset2.so");
   //gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   TFile* nf = new TFile("test.root","RECREATE")
   CalibRunsTH1* tof_e05_sect0 = new CalibRunsTH1("tof_e05_sect0","TOF sector 0",2592);
   tof_e05_sect0->SetOptDebug();
   TFile* f = new TFile("/phenix/data03/enoki/preco/jobs3/MgHistTDC/MgEmcTdcCalib_28750.root");
   tof_e05_sect0->Append(f);
   delete f;
   f = new TFile("/phenix/data03/enoki/preco/jobs3/MgHistTDC/MgEmcTdcCalib_28749.root");
   tof_e05_sect0->Append(f);

   delete f;

   TKey* key = f->GetKey("tof_e05_sect0_28750_0",2);
   TH1Xch* h = (TH1Xch*) key->ReadObj();
   tof_e05_sect0->Append(h,28750,0);
   delete h;

   //
   //================================================================================
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");

   TFile* f = new TFile("/phenix/data21/phnxreco/run2_v01_pro14/run2tree/run2tree_run2_v01-0000028123-0001.root");
   TTree* udst = (TTree*) f->Get("udst");
   Evt* evt =  new Evt();
   evt->Init_run2tree(udst);


 }
//
