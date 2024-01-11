{
#include "strstream"
#include "stdlib.h"

  gSystem->Load("libuti.so");

  gROOT->LoadMacro("run2tbase.C");

  // histogram definition
  TFile *hfile = new TFile("read2.root","recreate");
  hfile->cd();
  TH1F *hchargesum = new TH1F("hchargesum","bbc chargesum",220,0,2200);
  TH2F *ntrkvschar = new TH2F("ntrkvschar","charge,ntrack",100,0,2000,
                                                           100,0,600);
  // reactionPlane init
  reactionPlane *rp = new reactionPlane();
  short adc[128],tdc[128];
  int nuse[2000],used[2000];
  int nmul=rp->getNmul();
  int nzps=rp->getNzps();
  cout << nmul << " " << nzps << " "
       << rp->getNdet() << " " 
       << rp->getNhar() << " "
       << rp->getNord() << endl;

  // access run2tree
  char name[8],name2[16];
  TFile *f;
  TTree *udst;
  run2tbase *data_ptr;
  int totalEvent=0;
  int totalAnalyzed=0;
//for (int ifile=0; ifile<130; ifile++) {
//for (int ifile=130; ifile<180; ifile++) {
//for (int ifile=180; ifile<230; ifile++) {
//for (int ifile=230; ifile<280; ifile++) {
//for (int ifile=280; ifile<330; ifile++) {
//for (int ifile=330; ifile<380; ifile++) {
  for (int ifile=0; ifile<567; ifile++) {
    sprintf(name,"%d",10000000+ifile);
    sprintf(name2,"dat/file0000.dat");
    for (int i=0; i<4; i++) name2[i+8]=name[i+4];
    cout << name2;
    f = TFile::Open(name2);
    udst = (TTree*)f->Get("udst");
    data_ptr = new run2tbase(udst);
    int nevt = data_ptr->fChain->GetEntries();
    cout << " " << nevt << endl;
//  nevt=10;

    // start event analysis
    int ievent=0; 
    for(int ievt=0; ievt<nevt; ievt++){
      data_ptr->GetEntry(ievt);
      int run=data_ptr->run;
      int evt=data_ptr->evt;
      if((ievt%1000)==0) cout<< "Process : "<<ievt<<" "<<ievent<<" "
                             << run << " " << evt << endl;
   
      float charge = data_ptr->bbcqs + data_ptr->bbcqn;
      float bbczps = data_ptr->bbcz;
      int   ntrack = data_ptr->ntrk;
      int   npc1hit = data_ptr->npc1;
      int   trigger = data_ptr->trig;
      if (fabs(bbczps)>50 || trigger>100000000) continue;
      if (charge>50.0+ntrack*7.0 || charge<-50.0+ntrack*2.0) continue;
      if (ntrack<5) continue;

      for(int ipmt=0; ipmt<128; ipmt++){
        adc[ipmt]=data_ptr->bbca[ipmt];
        tdc[ipmt]=data_ptr->bbct[ipmt];
      }
      int imul = (int)(nmul*charge/1700.0);
      if (imul<0) imul=0;
      if (imul>nmul-1) imul=nmul-1;
      int izps = (int)(nzps*(bbczps+50.0)/100.0);
      if (izps<0) izps=0;
      if (izps>nzps-1) izps=nzps-1;
      rp->calBbcPlane(imul,izps,adc,tdc);

      for (int ih=0; ih<npc1hit; ih++) nuse[ih]=0;
      for (int it=0; it<ntrack; it++) {
        int hitid=data_ptr->pc1id[it];
        if (hitid>-1 && hitid<npc1hit) {
          nuse[hitid]++;
        } else {
          if (hitid!=-1)
          cout << "error exceed pc1 array "
               << hitid << " " << npc1hit << endl;
        }
      }

      rp->clrPlane();
      for(int it=0; it<ntrack; it++){
        float ptot=fabs(data_ptr->mom[it]);
        float phi=data_ptr->phi0[it];
        float theta=data_ptr->the0[it];
        int hitid=data_ptr->pc1id[it];
        int flag=0;
        used[it]=0;
        if (hitid>-1 && hitid<npc1hit) flag=nuse[hitid];
        if (theta>-1000 && phi>-1000 && ptot<10 && flag>0) {
          nuse[hitid]=0;
          used[it]=1;
          phi=atan2(sin(phi),cos(phi));
          float ptra=ptot*sin(theta);
          float eta=-log(tan(theta/2.0));
          if (fabs(eta)>1.0) cout << "error wrong eta " 
          << eta << " " << theta << " " <<  phi << endl;
          float wt=1;
          float pm=1;
          if (eta>0) pm=-1;
          float phj=data_ptr->zed[it]+80.0;
          int iphi=(int)(phj*20.0/160.0);
          int ibbc=iphi%4+1;
          rp->filHitPlane(0,  phi,wt,pm);
          rp->filHitPlane(ibbc,phi,wt,pm);
        }
      }
      rp->calPlane(imul,izps);
      if (ievent%500==0) {
        for (int idet=0; idet<3; idet++) {
          cout << idet << " ";
          for (int ihar=0; ihar<4; ihar++)
          cout << rp->getBbcPlane(idet,ihar) << " ";
          cout << endl;
        }
        for (int idet=0; idet<5; idet++) {
          cout << idet << " ";
          for (int ihar=0; ihar<4; ihar++)
          cout << rp->getHitPlane(idet,ihar) << " ";
          cout << endl;
        }
      }
      rp->checkResolution(imul,izps);
      hchargesum->Fill(charge);
      ntrkvschar->Fill(charge,ntrack);
      ievent++;
    }
    delete data_ptr; 
    totalEvent    += nevt;
    totalAnalyzed += ievent;
    cout << "totalEvent=" << totalEvent <<
    " totalAnalyzed=" << totalAnalyzed << endl;
  }
  rp->closePlane();
  hfile->Write();
  hfile->Close();
}
