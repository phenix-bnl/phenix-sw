int readdst(int from=0, int to=-1) {

  gROOT->Reset();

TFile *top = new TFile("tmp.root","recreate");
float ntpart[9];
TNtuple *ntp0 = new TNtuple("ntp0","tecdch","evt:multtec:multdch:tecalpha:dchalpha:dchphi:zdch:ztec:mom");

  TFile *f=new TFile("dstData.root");

  TTree *T=(TTree*)gDirectory->Get("T");
  //  T->Print();

//  dRunHeaderWrapper *dRunHeader=new dRunHeaderWrapper();
//  dEventHeaderWrapper *dEventHeader=new dEventHeaderWrapper();
  dBbcOutWrapper *dBbcOut=new dBbcOutWrapper();
//  dZdcOutWrapper *dZdcOut=new dZdcOutWrapper();
  dTecTrackWrapper *dTecTrack=new dTecTrackWrapper();
  dDchTracksWrapper *dDchTracks=new dDchTracksWrapper();
  dCglTrackWrapper *dCglTrack=new dCglTrackWrapper();
  dCglParticleWrapper *dCglParticle=new dCglParticleWrapper();
//  dEmcClusterLocalWrapper* dEmcClusterLocal=new dEmcClusterLocalWrapper();
//  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt=new dEmcClusterLocalExtWrapper();
//  dEmcCalibTowerWrapper* dEmcCalibTower=new dEmcCalibTowerWrapper();
//  T->SetBranchAddress("DST/dRunHeader", &dRunHeader);
//  T->SetBranchAddress("DST/dEventHeader", &dEventHeader);
  T->SetBranchAddress("DST/dBbcOut", &dBbcOut);
//  T->SetBranchAddress("DST/dZdcOut", &dZdcOut);
  T->SetBranchAddress("DST/dTecTrack", &dTecTrack);
  T->SetBranchAddress("DST/dDchTracks", &dDchTracks);
  T->SetBranchAddress("DST/dCglTrack", &dCglTrack);
  T->SetBranchAddress("DST/dCglParticle", &dCglParticle);
//  T->SetBranchAddress("DST/dEmcClusterLocal", &dEmcClusterLocal);
//  T->SetBranchAddress("DST/dEmcClusterLocalExt", &dEmcClusterLocalExt);
//  T->SetBranchAddress("DST/dEmcCalibTower", &dEmcCalibTower);
  T->SetBranchStatus("*", 0);
//  T->SetBranchStatus("DST/dRunHeader", 1);
//  T->SetBranchStatus("DST/dEventHeader", 1);
  T->SetBranchStatus("DST/dBbcOut", 1);
//  T->SetBranchStatus("DST/dZdcOut", 1);
  T->SetBranchStatus("DST/dTecTrack", 1);
  T->SetBranchStatus("DST/dDchTracks", 1);
  T->SetBranchStatus("DST/dCglTrack", 1);
  T->SetBranchStatus("DST/dCglParticle", 1);
//  T->SetBranchStatus("DST/dEmcClusterLocal", 1);
//  T->SetBranchStatus("DST/dEmcClusterLocalExt", 1);
//  T->SetBranchStatus("DST/dEmcCalibTower", 1);

  if(to<0 || to>T->GetEntries()) to=T->GetEntries();
  if(from<0 || from>to) from=0;
  cout<<"Processing events "<<from<<" through "<<to<<endl;

  for(int j=from; j<to; j++){
//  for(int j=1; j<1500; j++){
    
    T->GetEvent(j);
//    if((j%100)==0) cout<<"event header contains "<<dEventHeader->get_event(0)<<endl;

//    bbcz=dBbcOut->get_VertexPoint(0);
//    zdcz=dZdcOut->get_Zvertex(0);
//    ezdc=dZdcOut->get_Energy(0,0);
//=====> Choose vertices +-40 cm
//    if(zdcz>40. || zdcz<-40.) continue;

    float tecAlpha,dchAlpha,px,py,pz,pp,tecSlope,tecPhi;

    for(int i=0; i<dCglTrack->RowCount(); i++) {

      tecAlpha=-9999.; dchAlpha=-9999.;

      int tecptr=dCglTrack->get_tectrackid(i);
      int dchptr=dCglTrack->get_dctracksid(i);

      if(tecptr>-1 && dchptr>-1) {

        px = dCglParticle->get_pxyz(0,i);
        py = dCglParticle->get_pxyz(1,i);
        pz = dCglParticle->get_pxyz(2,i);
        pp=sqrt(px*px+py*py+pz*pz);

        float in[3],out[3];
        float in[0]=dTecTrack->get_xyzin(0,tecptr);
        float in[1]=dTecTrack->get_xyzin(1,tecptr);
        float in[2]=dTecTrack->get_xyzin(2,tecptr);
        float out[0]=dTecTrack->get_xyzout(0,tecptr);
        float out[1]=dTecTrack->get_xyzout(1,tecptr);
        float out[2]=dTecTrack->get_xyzout(2,tecptr);
// get Tec Alpha
          TecTrack* track = new TecTrack(in,out);
          tecPhi = track->getPhi();
          tecAlpha = track->getAlpha();
          delete track;
// get Dch Alpha
          dchAlpha = dDchTracks->get_alpha(dchptr);

          ntpart[0]=(float)j;
          ntpart[1]=(float)dTecTrack->RowCount();
          ntpart[2]=(float)dDchTracks->RowCount();
          ntpart[3]=tecAlpha;
          ntpart[4]=dchAlpha;
          ntpart[5]=dDchTracks->get_phi(dchptr);
          ntpart[6]=(float)dDchTracks->get_side(dchptr);
          ntpart[7]=dTecTrack->get_xyzin(2,tecptr);
          ntpart[8]=pp;
          ntp0->Fill(ntpart);
      }

    } // end loop over dCglTrack
    
  } // end event loop

  top->Write();
  top->Close();

}

