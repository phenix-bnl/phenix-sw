int event=0;
int run;
double par[4];
char buff[256];

int numofdst =50;
 
const char *singleParticleDst[] ={
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-1.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-2.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-3.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-4.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-5.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-6.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-7.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-8.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-9.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-10.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-11.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-12.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-13.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-14.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-15.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-16.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-17.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-18.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-19.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-20.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-21.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-22.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-23.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-24.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-25.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-26.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-27.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-28.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-29.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-30.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-31.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-32.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-33.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-34.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-35.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-36.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-37.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-38.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-39.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-40.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-41.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-42.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-43.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-44.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-45.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-46.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-47.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-48.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-49.root.DST",
"/phenix/data20/ralf/reco_qm2001/output/PISAEvent.piminus-tof-50.root.DST"
};
int DEN1=0,DEN3=0;
int NOR1=0,NOR3=0;
void embedRunMc(int events=1000000,TString Name= "embedmc-test.root",int filenumber=1)
{
  mixer->setkickOutHitsToSpeedupReco(1);
  float array[30];

  PHIODataNode<TObject> *RUNNode;
  PHIODataNode<TObject> *EVENTNode;
  PHIODataNode<TObject> *BBCNode;
  PHIODataNode<TObject> *ZDCNode;
  PHIODataNode<TObject> *HITNode;
  PHIODataNode<TObject> *TRACKNode;
  PHIODataNode<TObject> *PERFTRACKNode;
  PHIODataNode<TObject> *PAD1Node,*PADR1Node;
  PHIODataNode<TObject> *PAD3Node,*PADR3Node;
  dRunHeaderWrapper   *dRunHeader;
  dEventHeaderWrapper *dEventHeader;

  dDchHitWrapper *dDchHit3,*dDchHit2,*dDchHit1;
  dDchTracksWrapper *dDchTracks3,*dDchTracks2,*dDchTracks1,*dDchPerfTracks1;
  dPadClusterWrapper* dPc1Cluster3,*dPc1Cluster2,*dPc1Cluster1,*tmp;
  dPadClusterWrapper* dPc3Cluster3,*dPc3Cluster2,*dPc3Cluster1;
  dPadRawWrapper* dPc1Raw3,*dPc1Raw2,*dPc1Raw1,*tmpR;
  dPadRawWrapper* dPc3Raw3,*dPc3Raw2,*dPc3Raw1;
  int current=0;
  int currentsingle =0;
  int depc1=0,nupc1 =0,depc3 =0,nupc3 =0;
  current =  ((int)(gRandom->Rndm(1)*1299721)+filenumber)%numofruns;
  currentsingle =  ((int)(gRandom->Rndm(1)*1299721)+filenumber)%numofdst;
  //int current=13;

  cout<<"start with"<<dstFileName[current]<<endl;
  cout<<"start with"<<singleParticleDst[currentsingle]<<endl;
  singleIn = new PHNodeIOManager(singleParticleDst[currentsingle],PHReadOnly);
  dstIn    = new PHNodeIOManager(dstFileName[current],PHReadOnly);
  histogrammer->setFileName(Name.Data());
  PHNodeIterator iter1(node1);
  PHNodeIterator iter2(node2);
  PHNodeIterator iter3(topNode);
  int a;
  int start = ((int)(gRandom->Rndm(1)*1299721*filenumber))%12317;
  if(start<1) start=1;
  //start =40000;
  cout<<"Start with "<< start <<" track"<<endl;
  //singleIn->read(node1,start);
  int next1=1,next2=1;
  int count1=0,count2=0,count3=0,nb =0;
  float ncoll,npart;
  while(1){
    while(!dstIn->read(node2)){
      node2->prune();
      if(dstIn)delete dstIn;
      current++;
      if(current>=numofruns){
	current=0;
      }
      cout<<dstFileName[current]<<endl;
      dstIn = new PHNodeIOManager(dstFileName[current],PHReadOnly);
      // dstIn->read(node2);
    }
    count2++;
    if(next1){
      //while(1){
       if(!count1)nb =singleIn->read(node1,start);
       else nb =singleIn->read(node1);
       if(!nb){
	 currentsingle++;
	 if(currentsingle>=numofdst) {
	   currentsingle = 0;
	 }
	 node1->prune();
	 if(singleIn) delete singleIn;
	 cout<<singleParticleDst[currentsingle]<<endl;
	 singleIn = new PHNodeIOManager(singleParticleDst[currentsingle],PHReadOnly);
	 
	 //start = ((int)(gRandom->Rndm(1)*1299721))%47677;
	 //if(start<1) start =1;
	 //cout<<"start "<<start<<endl;
	 singleIn->read(node1);
       }
       //HITNode = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dDchHit");
       //PERFTRACKNode = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dDchTracksPerf");
       //PAD1Node  = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dPc1Cluster"); 
       //if (PAD1Node)   dPc1Cluster1 = (dPadClusterWrapper*)(PAD1Node->getData());
        
       //if(PERFTRACKNode) dDchPerfTracks1 = (dDchTracksWrapper*)(PERFTRACKNode->getData());
       //if (HITNode)   dDchHit1      = (dDchHitWrapper*)(HITNode->getData());
       //if(dDchHit1->RowCount()<2)break;
       //else 	  cout<<"this event have no DC hits,skip"<<endl;
       //if(dDchPerfTracks1->RowCount()>1) break;
       //else cout<<dDchPerfTracks1->RowCount()<<" this event have less than 3 pertracks,skip"<<endl;
       //if(dPc1Cluster1->RowCount()>0)break;
       //count1++;
     
    }
    count1++;
      //}

    event++;
    if(event>events) break;
    if(event==1){//first event only stuff
      node1->print();
      //cout<<"/*******************************************/"<<endl;
      PHEmbedInitializer::instance()->MakeCopy(node2,topNode);
      //node2->print();
      //cout<<"/*******************************************/"<<endl;
      //topNode->print();
      dMvdVertexOut->set_ErrorCode(0,-1);
      if (verbose>10) printf("Calling mDchInitializer\n");
      mDchInitializer->event(topNode);
      mDchInitializer->getDGO()->rotateAndTranslate();
 
     if (verbose>10) printf("Calling PadCalibration\n");
      padStatCh = PadCalibration->FetchBadChFromFile();
      padStatROC = PadCalibration->FetchBadROCFromFile();

      if (verbose>10) printf("Calling mPadDetGeo\n");
      mPadDetGeo->FetchFromFile();
      mPadDetGeo->Fetch_dPadGeom(topNode);
      if (verbose>10) printf("Calling padInclBad\n");
      padInclStat = padInclBad->FetchCalDataFromFiles();
      //padInclStat = padInclBad->FetchCalDataFromObjy(TimeStamp);

      if (verbose>10) printf("Calling TecAddress\n");
      TecAddress->FetchFromFile();
      if (verbose>10) printf("Calling TecGeometry\n");
      TecGeometry->FetchFromFile();
      TecCalibration->FetchFromFile();
      if (verbose>10) printf("Calling mTecAlign\n");
      // mTecAlign->set_UseObjy(0,0);
      //if (geomFlag == 1) mTecAlign->set_Retracted(0,0);
      //if (geomFlag == 0) mTecAlign->set_Retracted(0,1);
      mTecAlign->event(topNode,TecAddress,TecGeometry);
      PHIODataNode<TObject>* TecDetGeoNode = new PHIODataNode<TObject>(TecGeometry,"TecGeometry");
      parNode->addNode(TecDetGeoNode);
      PHIODataNode<TObject>* TecDetCalNode = new PHIODataNode<TObject>(TecCalibration,"TecCalibration");
      parNode->addNode(TecDetCalNode);

      
      if (verbose>10) printf("Calling mCrkSetGeo\n");
      mCrkSetGeo->event(topNode);

      if (verbose>10) printf("Calling mCrkSetUcal\n");
      mCrkSetUcal->event(topNode);
      if (verbose>10) printf("Calling mCrkSetCal\n");
      TCrkModule::setCal(topNode,"crk_adc.txt","crk_t0.txt","crk_tac.txt","crk_slew.txt");

      if (verbose>10) printf("Calling mTofSetGeo\n");
      mTofSetGeo->event(topNode);
      if (verbose>10) printf("Calling mTofSetUcal\n");
      mTofSetUcal->event(topNode);
      if (verbose>10) printf("Calling mTofSetCal\n");
      mTofSetCal->event(topNode);
      if (verbose>10) printf("Calling TofAddress\n");
      TofAddress->setTimeStamp(TimeStamp);
      TofAddress->fetchFromFile("toffemmap.txt");
      
      if (verbose>10) printf("Calling TofGeometry\n");
      TofGeometry->setTimeStamp(TimeStamp);
      TofGeometry->fetchFromFile("tofpanelgeo.txt","tofslatoffset.txt");
      if (verbose>10) printf("Calling TofCalib\n");
      TofCalib->setTimeStamp(TimeStamp);
      TofCalib->fetchPedestalFromFile("tofPedestal.txt",TofAddress);
      TofCalib->fetchTvcConvFromFile("tofTvcConv.txt",TofAddress);
      TofCalib->fetchQvcConvFromFile("tofQvcConv.txt",TofAddress);
      TofCalib->fetchSlewParFromFile("tofSlewPar.txt");
      TofCalib->fetchToffsetFromFile("tofToffset.txt");
      TofCalib->fetchYoffsetFromFile("tofYoffset.txt");
      TofCalib->fetchVelocityFromFile("tofVelocity.txt");
      TofCalib->fetchElossConvFromFile("tofElossConv.txt");
      TofCalib->fetchGlobalTFromFile("tofGlobalT.txt");

      if (verbose>10) printf("Calling mEmcGeaParams\n");
      mEmcGeaParams->event(topNode);
      if (verbose>10) printf("Calling cglDetectorGeo\n");
      cglDetectorGeo->BuildAllGeo();      
// JV 05/30/01 to get the Tof geometry with cracks between panels

      short arm=0;
      cglDetectorGeo->fetch_tofGeo(topNode,arm);
    }
    //if(1)cout << "analysed events "<< event << endl;
    // get node information from three node trees
    BBCNode     = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dBbcOut");
    HITNode = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dDchHit");
    TRACKNode = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dDchTracks");      
    PERFTRACKNode = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dDchTracksPerf");      
    PADR1Node   = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dPc1Raw");
    PADR3Node   = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dPc3Raw");
    PAD1Node  = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dPc1Cluster"); 
    PAD3Node  = (PHIODataNode<TObject>*)iter1.findFirst("PHIODataNode","dPc3Cluster"); 
    if (BBCNode)   dBbcOut       = (dBbcOutWrapper*)(BBCNode->getData());
    if (HITNode)   dDchHit1      = (dDchHitWrapper*)(HITNode->getData());
    if (TRACKNode) dDchTracks1   = (dDchTracksWrapper*)(TRACKNode->getData());
    if(PERFTRACKNode) dDchPerfTracks1 = (dDchTracksWrapper*)(PERFTRACKNode->getData());
    if (PADR1Node)   dPc1Raw1 = (dPadRawWrapper*)(PADR1Node->getData());
    if (PADR3Node)   dPc3Raw1 = (dPadRawWrapper*)(PADR3Node->getData());
    if (PAD1Node)   dPc1Cluster1 = (dPadClusterWrapper*)(PAD1Node->getData());
    if (PAD3Node)   dPc3Cluster1 = (dPadClusterWrapper*)(PAD3Node->getData());
    float vertex;
    vertex = dBbcOut->get_VertexPoint(0);
    //cout<<"vertex single "<<vertex<<" "<<dDchPerfTracks1->RowCount()<<endl;

    BBCNode     = (PHIODataNode<TObject>*)iter2.findFirst("PHIODataNode","dBbcOut");
    HITNode   = (PHIODataNode<TObject>*)iter2.findFirst("PHIODataNode","dDchHit");
    TRACKNode = (PHIODataNode<TObject>*)iter2.findFirst("PHIODataNode","dDchTracks");
    PADR1Node   = (PHIODataNode<TObject>*)iter2.findFirst("PHIODataNode","dPc1Raw");
    PADR3Node   = (PHIODataNode<TObject>*)iter2.findFirst("PHIODataNode","dPc3Raw");      
    PAD1Node  = (PHIODataNode<TObject>*)iter2.findFirst("PHIODataNode","dPc1Cluster"); 
    PAD3Node  = (PHIODataNode<TObject>*)iter2.findFirst("PHIODataNode","dPc3Cluster"); 
    if (BBCNode)   dBbcOut       = (dBbcOutWrapper*)(BBCNode->getData());
    if (HITNode)   dDchHit2      = (dDchHitWrapper*)(HITNode->getData());
    if (TRACKNode) dDchTracks2   = (dDchTracksWrapper*)(TRACKNode->getData());
    if (PADR1Node)   dPc1Raw2 = (dPadRawWrapper*)(PADR1Node->getData());
    if (PADR3Node)   dPc3Raw2 = (dPadRawWrapper*)(PADR3Node->getData());
    if (PAD1Node)   dPc1Cluster2 = (dPadClusterWrapper*)(PAD1Node->getData());
    if (PAD3Node)   dPc3Cluster2 = (dPadClusterWrapper*)(PAD3Node->getData());
    vertex = dBbcOut->get_VertexPoint(0);
    cout<<"vertex "<<vertex<<endl;
    if(fabs(vertex)>40) {
      next1=0;
      continue;
    }
    next1=1;
    next2=1;
    RUNNode     = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dRunHeader");
    EVENTNode   = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dEventHeader");
    BBCNode     = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dBbcOut");
    ZDCNode     = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dZdcOut");
    HITNode     = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dDchHit");
    TRACKNode   = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dDchTracks");      
    PADR1Node   = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dPc1Raw");
    PADR3Node   = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dPc3Raw");
    PAD1Node    = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dPc1Cluster"); 
    PAD3Node    = (PHIODataNode<TObject>*)iter3.findFirst("PHIODataNode","dPc3Cluster"); 
    if (RUNNode)   dRunHeader    = (dRunHeaderWrapper*)(RUNNode->getData());
    if (EVENTNode) dEventHeader  = (dEventHeaderWrapper*)(EVENTNode->getData());
    if (BBCNode)   dBbcOut       = (dBbcOutWrapper*)(BBCNode->getData());
    if (ZDCNode)   dZdcOut       = (dZdcOutWrapper*)(ZDCNode->getData());
    if (HITNode)   dDchHit3      = (dDchHitWrapper*)(HITNode->getData());
    if (TRACKNode) dDchTracks3   = (dDchTracksWrapper*)(TRACKNode->getData());
    if (PADR1Node)   dPc1Raw3 = (dPadRawWrapper*)(PADR1Node->getData());
    if (PADR3Node)   dPc3Raw3 = (dPadRawWrapper*)(PADR3Node->getData());
    if (PAD1Node)   dPc1Cluster3 = (dPadClusterWrapper*)(PAD1Node->getData());
    if (PAD3Node)   dPc3Cluster3 = (dPadClusterWrapper*)(PAD3Node->getData());
    depc1 +=dPc1Cluster1 ->RowCount();
    depc3 +=dPc3Cluster1 ->RowCount();
    int a1  = dPc1Cluster1 ->RowCount() + dPc1Cluster2 ->RowCount();
    int a3  = dPc3Cluster1 ->RowCount() + dPc3Cluster2 ->RowCount();

    /*cout<<"----Begin: Hit "<<dDchHit1 ->RowCount()<<" "<<dDchHit2 ->RowCount()<<" "<<dDchHit3 ->RowCount()<<endl;
    cout<<"Track "<<dDchTracks1->RowCount()<<" "<<dDchTracks2->RowCount()<<" "<<dDchTracks3->RowCount()<<endl;
    cout<<"rawpc1 "<<dPc1Raw1 ->RowCount()<<" "<<dPc1Raw2 ->RowCount()<<" "<<dPc1Raw3 ->RowCount()<<endl;
    cout<<"clusterpc1 "<<dPc1Cluster1->RowCount()<<" "<<dPc1Cluster2->RowCount()<<" "<<dPc1Cluster3->RowCount()<<endl;
    cout<<"rawpc3 "<<dPc3Raw1 ->RowCount()<<" "<<dPc3Raw2 ->RowCount()<<" "<<dPc3Raw3 ->RowCount()<<endl;
    cout<<"clusterpc3 "<<dPc3Cluster1->RowCount()<<" "<<dPc3Cluster2->RowCount()<<" "<<dPc3Cluster3->RowCount()<<endl;
    */

    mixer->event();
    //copy vertex of single particle to the event vertex

    /*cout<<"----After Embed : Hit"<<dDchHit1 ->RowCount()<<" "<<dDchHit2 ->RowCount()<<" "<<dDchHit3 ->RowCount()<<endl;
    cout<<"Track "<<dDchTracks1 ->RowCount()<<" "<<dDchTracks2 ->RowCount()<<" "<<dDchTracks3 ->RowCount()<<endl;
    cout<<"rawpc1 "<<dPc1Raw1 ->RowCount()<<" "<<dPc1Raw2 ->RowCount()<<" "<<dPc1Raw3 ->RowCount()<<endl;
    cout<<"clusterpc1 "<<dPc1Cluster1->RowCount()<<" "<<dPc1Cluster2->RowCount()<<" "<<dPc1Cluster3->RowCount()<<endl;
    cout<<"rawpc3 "<<dPc3Raw1 ->RowCount()<<" "<<dPc3Raw2 ->RowCount()<<" "<<dPc3Raw3 ->RowCount()<<endl;
    cout<<"clusterpc3 "<<dPc3Cluster1->RowCount()<<" "<<dPc3Cluster2->RowCount()<<" "<<dPc3Cluster3->RowCount()<<endl;
    */
   
    fillVtxOut(dstNode, dBbcOut, dMvdVertexOut);
 
    if(dDchHit1->RowCount()>0){    

      if (verbose>10) printf("Calling padInclBad\n");
      padInclStat = padInclBad->event(topNode);
      
      short modePad = 0;  // 0 is no split, 1 is split large clusters
      Pc1Rec->setSplitMode(modePad); // Split mode 0: Do not split large clusters
      Pc3Rec->setSplitMode(modePad);

      Pc1Rec->setEventNumber(event);
      if (verbose>10) printf("Calling Pc1Rec\n");
      Pc1Rec->event(0,mPadDetGeo,topNode);
      Pc3Rec->setEventNumber(event);
      if (verbose>10) printf("Calling Pc3Rec\n");
      Pc3Rec->event(2,mPadDetGeo,topNode);
      mDchCandidatory->event(topNode);
      cout<<"---After Reconstruction : Hit"<<dDchHit1 ->RowCount()<<" "<<dDchHit2 ->RowCount()<<" "<<dDchHit3 ->RowCount()<<endl;
      cout<<"Track "<<dDchTracks1 ->RowCount()<<" "<<dDchTracks2 ->RowCount()<<" "<<dDchTracks3 ->RowCount()<<endl;
      cout<<"rawpc1 "<<dPc1Raw1 ->RowCount()<<" "<<dPc1Raw2 ->RowCount()<<" "<<dPc1Raw3 ->RowCount()<<endl;
      cout<<"clusterpc1 "<<dPc1Cluster1->RowCount()<<" "<<dPc1Cluster2->RowCount()<<" "<<dPc1Cluster3->RowCount()<<endl;
      cout<<"rawpc3 "<<dPc3Raw1 ->RowCount()<<" "<<dPc3Raw2 ->RowCount()<<" "<<dPc3Raw3 ->RowCount()<<endl;
      cout<<"clusterpc3 "<<dPc3Cluster1->RowCount()<<" "<<dPc3Cluster2->RowCount()<<" "<<dPc3Cluster3->RowCount()<<endl;

      a1-=dPc1Cluster3 ->RowCount();
      a3-=dPc3Cluster3 ->RowCount();
      nupc1+=a1;nupc3+=a3;
      cout<<"depc1 = "<<depc1<<" depc3 = "<<depc3<<" nupc1 = "<<nupc1<<" nupc3 = "<<nupc3<<endl;


      /* cout<<"____________________"<<endl;
      tmpR = dPc1Raw1;
      for(int aa=0;aa<tmpR->RowCount();aa++) cout<<tmpR->get_arm(aa)<<" "<<tmpR->get_side(aa)<<" "<<tmpR->get_sector(aa)<<" "<<tmpR->get_padx(aa)<<" "<<tmpR->get_padz(aa)<<endl;
      cout<<"____________________"<<endl;
      tmpR = dPc1Raw2;
      for(int aa=0;aa<tmpR->RowCount();aa++) cout<<tmpR->get_arm(aa)<<" "<<tmpR->get_side(aa)<<" "<<tmpR->get_sector(aa)<<" "<<tmpR->get_padx(aa)<<" "<<tmpR->get_padz(aa)<<endl;
      cout<<"____________________"<<endl;
      tmpR = dPc1Raw3;
      for(int aa=0;aa<tmpR->RowCount();aa++) cout<<tmpR->get_arm(aa)<<" "<<tmpR->get_side(aa)<<" "<<tmpR->get_sector(aa)<<" "<<tmpR->get_padx(aa)<<" "<<tmpR->get_padz(aa)<<endl;
      
     cout<<"-----____________________"<<endl;
      tmp = dPc1Cluster1;
      for(int aa=0;aa<tmp->RowCount();aa++) cout<<tmp->get_xyz(0,aa)<<" "<<tmp->get_xyz(1,aa)<<" "<<tmp->get_xyz(2,aa)<<endl;
      cout<<"____________________"<<endl;
      tmp = dPc1Cluster3;
      for(int aa=0;aa<tmp->RowCount();aa++) cout<<tmp->get_xyz(0,aa)<<" "<<tmp->get_xyz(1,aa)<<" "<<tmp->get_xyz(2,aa)<<endl;
      cout<<"____________________"<<endl;
      */
      DEN1+=dPc1Cluster1->RowCount();
      NOR1+=compare(dPc1Cluster1,dPc1Cluster3);
      //cout<<"PC3"<<endl;
      DEN3+=dPc3Cluster1->RowCount();
      NOR3+=compare(dPc3Cluster1,dPc3Cluster3);
      //cout<<"DEN "<<DEN1<<" "<<DEN3<<" "<<NOR1<<" "<<NOR3<<endl;


      if (bFieldFlag == 0) {
	if (verbose>10) printf("Calling mPHLineTrack\n");
	cglLineTrack->event(topNode);
      }
      
      if (bFieldFlag == 1) {
	if (verbose>10) printf("Calling mPHDchTrackModel\n");
	dchTrackModel->event(topNode);
      }
      if (verbose>10) printf("Calling cglHitAssociate\n");
      cglHitAssociate->event(topNode);
      
      //MCevaluator->setEventNum(dEventHeader->get_event(0));
    }else{
      cout<<"no reconstruction nessesary"<<endl;
    }
    MCevaluator->evaluate();
    MCevaluator->associateDC();
    MCevaluator->associatePC(1);
    MCevaluator->associatePC(3);
    MCevaluator->associateTOF();
    MCevaluator->fillPHEmbedMcRecoTrack(); 
    
    count3++;
    if(count3%20==1) histogrammer->flush();
    
  }
  histogrammer->saveToFile(0);
}
int compare(dPadClusterWrapper * a,dPadClusterWrapper *b){
 int no =1;
  for(int i=0;i<a->RowCount();i++){
    no=1;
    for(int j=0;j<b->RowCount();j++){
      if(fabs(a->get_xyz(0,i) - b->get_xyz(0,j))<0.001&&fabs(a->get_xyz(1,i) - b->get_xyz(1,j))<0.001&&fabs(a->get_xyz(2,i) - b->get_xyz(2,j))<0.001){
	no =0;
	break;
      }
    }
    if(no) break;
  }
 print:
  if(no&&a->RowCount()>0){
    
    dPadClusterWrapper * tmp;
    tmp = a;
    cout<<"************************************************"<<endl;
    for(int aa=0;aa<tmp->RowCount();aa++) cout<<tmp->get_xyz(0,aa)<<" "<<tmp->get_xyz(1,aa)<<" "<<tmp->get_xyz(2,aa)<<endl;
    cout<<"____________________"<<endl;
    tmp = b;
    for(int aa=0;aa<tmp->RowCount();aa++) cout<<tmp->get_xyz(0,aa)<<" "<<tmp->get_xyz(1,aa)<<" "<<tmp->get_xyz(2,aa)<<endl;
    cout<<"____________________"<<endl;
    
    return 1;
  }
  return 0;
}





