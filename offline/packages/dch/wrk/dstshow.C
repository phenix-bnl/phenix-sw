int event=0;
double par[4];
char buff[256];

//PHNodeIterator iter1(node1);
//PHNodeIterator iter2(node2);
PHIODataNode<TObject> *FKINNode,*PRIMARYNode,*HITNode,*GHITNode,*Node;
PHNodeIterator iter3(topNode);

dDchHitWrapper * hit;
DchHitLineTable * hitTable;
dDchTracksWrapper *trkWrapper,*perf;
DchTrack * trkTable;
fkinWrapper * fkin;
primaryWrapper * primary;
dcghitWrapper * ghit;
int files=10;
int count =0;
char *nn[]={
  //"/direct/phenix+data16/jjia/mc2001/offline/single/output/pizero/ver1/DST.pizero10000-1.root",
  "/direct/phenix+data16/jjia/mc2001/offline/single/output/pizero/DST.pizero200000-1-2D97.root",

  "/phenix/data36/phnxreco/run2_v03_pro19/highptdst/highptdstWest_run2_v03-0000029186-0014.proot",
  "/phenix/data36/phnxreco/run2_v03_pro19/highptdst/highptdstWest_run2_v03-0000029186-0015.proot",
  "/phenix/data36/phnxreco/run2_v03_pro19/highptdst/highptdstWest_run2_v03-0000029529-0015.proot",
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-8.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-17.root",
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-9.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-12.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-13.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-14.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-19.root",
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-11.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-20.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-21.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-22.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-23.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-24.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-29.root",    
  "/direct/phenix+data16/jjia/mc2001/offline/hij/dst/DST.West.muMDC-38.root",    
};

void dstshow(int start=1)
{

  //dstIn    = new PHNodeIOManager("/phenix/data03/jjia/embed/output/piplus/PISAEvent.piplus1000-542.root.DST");
  //dstIn    = new PHNodeIOManager("/phenix/data30/phnxreco/run2_v03_pro19/highptdst/highptdstWest_run2_v03-0000029991-0000.proot",PHReadOnly);
  //dstIn    = new PHNodeIOManager("/phenix/data30/phnxreco/run2_v03_pro19/highptdst/highptdstWest_run2_v03-0000029991-0017.proot");
  dstIn    = new PHNodeIOManager(nn[0],PHReadOnly);
  //dstIn    = new PHNodeIOManager("dst.root",PHReadOnly);
  // dstIn->read(dstNode,start);
  run1();  

}
void run1(int disp=1){
  double px,py,pz,ppt,mass,e,eta;
  PHIODataNode<TObject> *HITNode;
  dDchHitWrapper* dDchHit1;
 next:
  int ret = dstIn->read(dstNode);
  if(!ret){
    count++;
    delete dstIn;
    cout<<nn[count]<<endl;
    dstNode->prune();
    dstIn    = new PHNodeIOManager(nn[count],PHReadOnly);
    goto next;
  }else{
    event++;

    if(event==1){//first event only stuff
      //gSystem->Load("/phenix/workarea/jjia/newdch/offline/display/install/lib/libphgui.so");
      //gSystem->Load("/phenix/workarea/jjia/newdch/offline/display/install/lib/libdchdisplay.so");
      //gSystem->Load("/phenix/workarea/jjia/newdch/offline/display/install/lib/libpaddisplay.so");
      //setup event display
      topNode->print();
      //mDchInitializer= new mNewDchInitializer(0,1,1,29529);
      mDchInitializer= new mNewDchInitializer(1,0,0);
      mDchInitializer->setGeometryFileNames("/afs/rhic/phenix/software/calibration/sim00/DchGeometry.info",
					    "/afs/rhic/phenix/software/calibration/sim00/DchGeometry.wireMc",
					    "/afs/rhic/phenix/software/calibration/sim00/DchGeometry.frame00NoRetracted");
      mDchInitializer->setNoiseFileName("/afs/rhic/phenix/software/calibration/sim00/DchNoise.Mc","");
      mDchInitializer->setCalibrationFileName("/afs/rhic/phenix/software/calibration/sim00/DchCalibration.Mc");
      PhenixRun* phenix = new PhenixRun("Phenix-Run","Phenix-Run",topNode);
      PhDchDisplay  *dchdisplay = new PhDchDisplay("Dch-Display","Dch-Display");
      gPhenix->GetDisplay()->AddSubsystem(dchdisplay);
      gPhenix->GetDisplay()->AddSubsystem(new PhPadDisplay("Pad-Display","Pad-Display"));
      Int_t TrackDraw_Mask   = 0x7;//BIT(0)|BIT(1)|BIT(2)
      Int_t HitBaseDraw_Mask = 0x3<<5;//BIT(5)|BIT(6)
      Int_t HitTypeDraw_Mask = 0x3<<11;//BIT(11)|BIT(12)|BIT(13)|BIT(14)|BIT(15)|BIT(16);
      dchdisplay->SetControlMode(11);
      dchdisplay->GetCurEventAttr()->SetTrackDrawMode(TrackDraw_Mask|HitBaseDraw_Mask|HitTypeDraw_Mask);
      //dchdisplay->SetHowtoBuildDrawableEvent(2);
      // gPhenix->GetDisplay()->AddSubsystem(new PhTecDisplay("Tec-Display","Tec-Display"));
      gPhenix->StartRun();
      PhEventDisplay* dch = new PhEventDisplay("Main",gClient->GetRoot(),1000,1000,gPhenix->GetDisplay()->GetList());
      dch->Popup();
      if (verbose>10) printf("Calling mDchInitializer\n");
      mDchInitializer->event(topNode);
      mDchInitializer->getDGO()->rotateAndTranslate();

      double pc1Radius = 248.891;
      double pc2Radius = 419.173;
      double pc3Radius = 492.012;
      double ThetaArm[2] = { -0.589049, 2.15984};
      mPadDetGeo->set_pc1Radius(pc1Radius);
      mPadDetGeo->set_pc2Radius(pc2Radius);
      mPadDetGeo->set_pc3Radius(pc3Radius);
      mPadDetGeo->set_Theta0(ThetaArm);

      mPadDetGeo = new PHpadDetectorGeo();
      PHIODataNode<TObject>* mPadDetGeoNode = new PHIODataNode<TObject>(mPadDetGeo,"mPadDetGeo");
      topNode->addNode(mPadDetGeoNode);

      dPadGeomWrapper* dPadGeom = new dPadGeomWrapper("dPadGeom",1);
      PHIODataNode<PHTable>* dPadGeomNode = new PHIODataNode<PHTable>(dPadGeom,"dPadGeom");
      topNode->addNode(dPadGeomNode);
      int nrc=1;
      dPadGeom->SetRowCount(nrc);
      dPadGeom->set_pdxoff(0,0,-24.31);
      dPadGeom->set_pdxoff(1,0,-81.2);
      dPadGeom->set_pdxoff(2,0,-95.7);
      dPadGeom->set_pdzoff(0,0,-89.5575);
      dPadGeom->set_pdzoff(1,0,-152.475);
      dPadGeom->set_pdzoff(2,0,-178.69);
      dPadGeom->set_pdgas(0,0,0.60);
      dPadGeom->set_pdgas(1,0,1.00);
      dPadGeom->set_pdgas(2,0,1.20);
      dPadGeom->set_aasep(0,0,0.84);
      dPadGeom->set_aasep(1,0,1.40);
      dPadGeom->set_aasep(2,0,1.65);
      dPadGeom->set_pxlen(0,0,0.82);
      dPadGeom->set_pxlen(1,0,1.375);
      dPadGeom->set_pxlen(2,0,1.622);
      dPadGeom->set_wside(0,0,0.27);
      dPadGeom->set_wside(1,0,0.47);
      dPadGeom->set_wside(2,0,0.55);
      dPadGeom->set_wcent(0,0,0.15);
      dPadGeom->set_wcent(1,0,0.26);
      dPadGeom->set_wcent(2,0,0.31);
      dPadGeom->set_pxsep(0,0,0.025);
      dPadGeom->set_pxsep(1,0,0.025);
      dPadGeom->set_pxsep(2,0,0.025);
      dPadGeom->set_clsep(0,0,0.1);
      dPadGeom->set_clsep(1,0,0.15);
      dPadGeom->set_clsep(2,0,0.2);
      dPadGeom->set_npdsec(0,0,16);
      dPadGeom->set_npdsec(1,0,8);
      dPadGeom->set_npdsec(2,0,8);
      dPadGeom->set_npdwr(0,0,58);
      dPadGeom->set_npdwr(1,0,116);
      dPadGeom->set_npdwr(2,0,116);
      dPadGeom->set_npdx(0,0,20);
      dPadGeom->set_npdx(1,0,40);
      dPadGeom->set_npdx(2,0,40);
      dPadGeom->set_npdz(0,0,216);
      dPadGeom->set_npdz(1,0,216);
      dPadGeom->set_npdz(2,0,216);
      dPadGeom->set_sectperarm(0,0,8);
      dPadGeom->set_sectperarm(1,0,4);
      dPadGeom->set_sectperarm(2,0,4);
      dPadGeom->set_inradius(0,0,248.891);
      dPadGeom->set_inradius(1,0,419.173);
      dPadGeom->set_inradius(2,0,492.012);
      dPadGeom->set_zgap(0,0,0.0);
      dPadGeom->set_zgap(1,0,8.106);
      dPadGeom->set_zgap(2,0,8.106);
      dPadGeom->set_phibote(0,213.75);
      dPadGeom->set_phitope(0,123.75);
      dPadGeom->set_phibotw(0,-33.75);
      dPadGeom->set_phitopw(0,56.25);
      
      mPadDetGeo->FetchFromFile();  // simulation still reads from ASCII file
      mPadDetGeo->Fetch_dPadGeom(topNode);
    
    }
    if(1)cout << "analysed events  "<< event<<endl;//dEventHeader->get_event(0) << endl;
  }
  
  if(disp){
    PRIMARYNode = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","primary");
    if (PRIMARYNode)   primary = (primaryWrapper*)(PRIMARYNode->getData());
    HITNode = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dDchHit");
    if (HITNode)   hit = (dDchHitWrapper*)(HITNode->getData());
    GHITNode = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dcghit");
    if (GHITNode)   ghit = (dcghitWrapper*)(GHITNode->getData());
    cout<<hit->RowCount()<<" "<<endl;
    cout<<ghit->RowCount()<<endl;
    /*
    px = primary->get_px_momentum(0);
    py = primary->get_py_momentum(0);
    pz = primary->get_pz_momentum(0);
    mass = 0.13957;
    e = sqrt(px*px + py*py + pz*pz + mass*mass);
    ppt = sqrt(px*px + py*py);
    eta = 0.5*log((e+pz)/(e-pz));
    cout<<ppt<<endl;
    //if(ppt>0.5)goto next;
    */
    Node = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dDchTracksPerf");
    if (Node)   perf = (dDchTracksWrapper*)(Node->getData());
    Node = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dDchTracks");
    if (Node)   trkWrapper = (dDchTracksWrapper*)(Node->getData());
    Node = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","DchTrack");
    if (Node)   trkTable = (DchTrack*)(Node->getData());
    for(int i=0;i<40;i++){  
      //int hitid=perf->get_hits(i,0);
      //cout<<ghit->get_id(i)<<endl;
    }

    if(trkWrapper&&trkTable){
      //cout<<"set"<<endl;
      for(int i=0;i<trkWrapper->RowCount();i++){
 	//trkTable->set_phi0(i,trkWrapper->get_phi0(i));
	//trkTable->set_theta0(i,trkWrapper->get_theta0(i));
	//trkTable->set_momentum(i,trkWrapper->get_momentum(i));
      }      
    }
 
    HITNode = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","DchHitLineTable");
    if (HITNode)   hitTable = (DchHitLineTable*)(HITNode->getData());
    cout<<"hits "<<hitTable->Entries()<<endl;
    //cout<<"event "<<event<<" "<<ppt<<endl;
    // if(hit->RowCount()<1000) goto next;
    Filter->eventdebug(topNode);
    cout<<"West: "<< Filter->get_isHighPtEventWest()<<endl;
    cout<<"East: "<< Filter->get_isHighPtEventEast()<<endl;
    gPhenix->event(topNode);
    gPhenix->Draw();
  }    
}
void run(int disp=1){
  double px,py,pz,ppt,mass,e,eta;
  PHIODataNode<TObject> *HITNode;
  dDchHitWrapper* dDchHit1;
  if(dstIn->read(topNode)){
    event++;

    if(event==1){//first event only stuff
      //gSystem->Load("/phenix/workarea/jjia/newdch/offline/display2.25/install/lib/libphdisplay.so");
      //gSystem->Load("/phenix/workarea/jjia/newdch/offline/display2.25/install/lib/libphgui.so");
      //gSystem->Load("/phenix/workarea/jjia/newdch/offline/display2.25/install/lib/libdchdisplay.so");
      //gSystem->Load("/phenix/workarea/jjia/newdch/offline/display2.25/install/lib/libpaddisplay.so");
      //setup event display
      PhenixRun* phenix = new PhenixRun("Phenix-Run","Phenix-Run",topNode);
      PhDchDisplay  *dchdisplay = new PhDchDisplay("Dch-Display","Dch-Display");
      gPhenix->GetDisplay()->AddSubsystem(dchdisplay);
      gPhenix->GetDisplay()->AddSubsystem(new PhPadDisplay("Pad-Display","Pad-Display"));
      Int_t TrackDraw_Mask   = 0x7;//BIT(0)|BIT(1)|BIT(2)
      Int_t HitBaseDraw_Mask = 0x3<<5;//BIT(5)|BIT(6)
      Int_t HitTypeDraw_Mask = 0x0<<11;//BIT(11)|BIT(12)|BIT(13)|BIT(14)|BIT(15)|BIT(16);
      dchdisplay->SetControlMode(7);
      dchdisplay->GetCurEventAttr()->SetTrackDrawMode(TrackDraw_Mask|HitBaseDraw_Mask|HitTypeDraw_Mask);
      // gPhenix->GetDisplay()->AddSubsystem(new PhTecDisplay("Tec-Display","Tec-Display"));
      gPhenix->StartRun();
      PhEventDisplay* dch = new PhEventDisplay("Main",gClient->GetRoot(),1000,1000,gPhenix->GetDisplay()->GetList());
      dch->Popup();
      if (verbose>10) printf("Calling mDchInitializer\n");
      mDchInitializer->event(topNode);
      mDchInitializer->getDGO()->rotateAndTranslate();
    }
    Filter->eventdebug(topNode);
    if(1)cout << "analysed events  "<< event<<endl;//dEventHeader->get_event(0) << endl;
  }
  
  if(disp){
    /* PRIMARYNode = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","primary");
    if (PRIMARYNode)   primary = (primaryWrapper*)(PRIMARYNode->getData());
    HITNode = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dDchHit");
    if (HITNode)   hit = (dDchHitWrapper*)(HITNode->getData());
    GHITNode = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dcghit");
    if (GHITNode)   ghit = (dcghitWrapper*)(GHITNode->getData());
    cout<<hit->RowCount()<<" "<<endl;
    cout<<ghit->RowCount()<<endl;
    
    px = primary->get_px_momentum(0);
    py = primary->get_py_momentum(0);
    pz = primary->get_pz_momentum(0);
    mass = 0.13957;
    e = sqrt(px*px + py*py + pz*pz + mass*mass);
    ppt = sqrt(px*px + py*py);
    eta = 0.5*log((e+pz)/(e-pz));
    cout<<ppt<<endl;
    */
    gPhenix->event(topNode);
    gPhenix->Draw();
  }    
}
int analyze(){

  PHIODataNode<TObject> *RUNNode;
  PHIODataNode<TObject> *EVENTNode;
  PHIODataNode<TObject> *BBCNode;
  PHIODataNode<TObject> *ZDCNode;
  PHIODataNode<TObject> *HITNode;
  PHIODataNode<TObject> *TRACKNode;
  PHIODataNode<TObject> *PAD1Node;
  PHIODataNode<TObject> *PAD3Node;
  dRunHeaderWrapper   *dRunHeader;
  dEventHeaderWrapper *dEventHeader;
  //dZdcOutWrapper *dZdcOut;
  //dBbcOutWrapper *dBbcOut;
  dDchHitWrapper *dDchHit3,*dDchHit2,*dDchHit1;
  dDchTracksWrapper *dDchTracks3,*dDchTracks2,*dDchTracks1;
  dPadClusterWrapper* dPc1Cluster3,*dPc1Cluster2,*dPc1Cluster1;
  dPadClusterWrapper* dPc3Cluster3,*dPc3Cluster2,*dPc3Cluster1;
  
  RUNNode     = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dRunHeader");
  EVENTNode   = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dEventHeader");
  BBCNode     = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dBbcOut");
  ZDCNode     = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dZdcOut");
  HITNode     = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dDchHit");
  TRACKNode   = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dDchTracks");      
  PAD1Node    = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dPc1Cluster"); 
  PAD3Node    = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dPc3Cluster"); 
  if (RUNNode)   dRunHeader    = (dRunHeaderWrapper*)(RUNNode->getData());
  if (EVENTNode) dEventHeader  = (dEventHeaderWrapper*)(EVENTNode->getData());
  if (BBCNode)   dBbcOut       = (dBbcOutWrapper*)(BBCNode->getData());
  if (ZDCNode)   dZdcOut       = (dZdcOutWrapper*)(ZDCNode->getData());
  if (HITNode)   dDchHit3      = (dDchHitWrapper*)(HITNode->getData());
  if (TRACKNode) dDchTracks3   = (dDchTracksWrapper*)(TRACKNode->getData());
  if (PAD1Node)   dPc1Cluster3 = (dPadClusterWrapper*)(PAD1Node->getData());
  if (PAD3Node)   dPc3Cluster3 = (dPadClusterWrapper*)(PAD3Node->getData());
  if(event==1)cout<<"run: "<<dEventHeader->get_run(0)<<endl;

  cout<<dDchHit3->RowCount()<<endl;
  return dDchHit3->RowCount();
}
