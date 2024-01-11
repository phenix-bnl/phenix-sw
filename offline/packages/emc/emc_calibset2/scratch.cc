

 {

   //=============================================================================
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   Hist_ass* hs = new Hist_ass("hs","test");
   TFile* nf = new TFile("test.root","RECREATE");
   hs->Write();
   //=============================================================================


   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   TFile* nf = new TFile("test.root");
   nf->ls();
   hm->Print();

   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   //   TFile* f = new TFile("/phenix/data24/htorii/01udst/v080701/uDST_v00-27826-1.root");
   gROOT->cd();
   Hist::SetOptDebug();
   Hist_hoge* hm = new Hist_hoge("hm","test");
   TFile* nf = new TFile("test.root","RECREATE");
   hm->Write();
   hm->Print();

   Hist_ele* he = new Hist_ele("he","test");
   //=============================================================================


   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   //   TFile* f = new TFile("/phenix/data24/htorii/01udst/v080701/uDST_v00-27826-1.root");
   TFile* f = new TFile("/phenix/data17/phnxreco/v05/uDST/uDST_v05-0000012468-0001.root");
   TNtuple* nt_evt = (TNtuple*) f->Get("nt_evt");
   TNtuple* nt_emc = (TNtuple*) f->Get("nt_emc");
   TNtuple* nt_trk = (TNtuple*) f->Get("nt_trk");

   trk_ass(nt_evt,nt_emc,nt_trk,"test.root",25,true);

   //=============================================================================
   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   TFile* f = new TFile("/phenix/data24/htorii/01udst/v080701/uDST_v00-27826-1.root")
   TNtuple* nt_evt = (TNtuple*) f->Get("nt_evt");
   TNtuple* nt_emc = (TNtuple*) f->Get("nt_emc");
   TNtuple* nt_trk = 0; //(TNtuple*) f->Get("nt_trk");

   Evt* evt =  new Evt();
   evt->Init_run1mdst(nt_evt,nt_trk,nt_emc);
   gROOT->cd();
   Hist::SetOptDebug();
   hist_ass = new Hist_ass("hist_ass","Association");
   TFile* nf = new TFile("test.root","RECREATE");
   hist_ass->Write();

   //=============================================================================

   gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
   hist_ass = new Hist_ass("hist_ass","Association");
   hist_ele = new Hist_ele("hist_ele","Electron analysis");
   hist_mip = new Hist_mip("hist_mip","MIP analysis");
   //=============================================================================

   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libdstrun.so");
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libcalibset2.so") ;

   hist_ass = new Hist_ass("hist_ass","Association");
   hist_ele = new Hist_ele("hist_ele","Electron analysis");
   hist_mip = new Hist_mip("hist_mip","MIP analysis");

   //=============================================================================
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libdstrun.so");
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/install/pro.14/lib/libcalibset2.so") ;
   TFile* nf = new TFile("test.root","RECREATE");
   hist_miptwr = new Hist_miptwr("hist_miptwr","MIPTWR analysis");

   TFile*  f = new TFile("/phenix/data21/phnxreco/run2_v01_pro14/run2tree/run2tree_run2_v01-0000028284-0002.root");
   TTree* udst = (TTree*)(gROOT->FindObject("udst"));
   Evt* evt =  new Evt();
   evt->Init_run2tree(udst);
   evt->Next();

   Pid emcpid;
   hist_miptwr->Fill(evt->_glb,evt->_vec_trk[0],evt->_vec_clt[0],emcpid);

   nf->cd();
   hist_miptwr->Write();

   //=============================================================================
   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");
   TFile* f= new TFile("tmp.root");
   TFile* nf = new TFile("tmp_out.root","RECREATE");
   CalibRunsTH1* miptwr_sect0 = new CalibRunsTH1("hist_miptwr_sect0","test",2592);
   CalibRunsTH1* miptwr_sect0_s = new CalibRunsTH1("hist_miptwr_s_sect0","test",2592);
   CalibRunsTH1* miptwr_sm_sect0 = new CalibRunsTH1("hist_miptwr_sm_sect0","test",18);
   CalibRunsTH1* miptwr_sm_sect0_s = new CalibRunsTH1("hist_miptwr_s_sm_sect0","test",18);
   miptwr_sect0->Read(f);
   miptwr_sect0_s->Read(f);
   miptwr_sm_sect0->Read(f);
   miptwr_sm_sect0_s->Read(f);
   miptwr_sect0->Append(miptwr_sect0_s,-1.0);
   miptwr_sm_sect0->Append(miptwr_sm_sect0_s,-1.0);

   miptwr_sect0->Read(28123,0);
   ((TH1Xch*)miptwr_sect0->_calibobj)->_h_all->Draw();
   miptwr_sm_sect0->Read(28123,0);
   ((TH1Xch*)miptwr_sm_sect0->_calibobj)->_h_all->Draw();

   gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
   gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");

 }


