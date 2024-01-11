#include "fkinWrapper.h"
#include "dDchHitWrapper.h"
#include "DchHitLineTablev1.hh"
#include "DchTrack.h"
#include "PHIODataNode.h"
#include "PHDchGeometryObject.h"
#include "PHDchCalibrationObject.h"
#include "PHEmbedStat.h"
#include "DchMixer.hh"
#include <iostream>
#include "CglTrack.h"
#include "PHTrackOut.h"
#include "PHDchTrackOut.h"
#include "Fun4AllReturnCodes.h"
#include "TFile.h"  // moved up from the middle of this file
#include "TNtuple.h"
#include "mNewDchCalibrator.hh"

#include <cmath>

using namespace std;

DchMixer::DchMixer() {
  
  rand = new TRandom3(41074);

  verbose = 0;
}
DchMixer::~DchMixer() {


}
int DchMixer::InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged) {
  if ((!sngl)||(!real)||(!merged)) {
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }
  

  node1  = sngl;
  node2  = real;
  node3  = merged;  
  return EVENT_OK;
}
int DchMixer::merge() {
  if ((!node1)||(!node2)||(!node3)) {
    return ABORTEVENT;
  }

  
  PHNodeIterator iter1(node1);
  PHNodeIterator iter2(node2);
  PHNodeIterator iter3(node3);  

  //add few tables that is needed for reconstruction
  PHIODataNode<PHTable>* NodeTable;
  PHIODataNode<PHObject>* NodeObj;
  DchHitLineTable* hitLine;
  DchTrack* dchtrack = 0;


  NodeTable = (PHIODataNode<PHTable>*)iter1.findFirst("PHIODataNode","dDchHit");
  if (!NodeTable) {
    dDchHitWrapper* hit =  new dDchHitWrapper("dDchHit",60000);
    NodeTable = new PHIODataNode<PHTable>(hit,"dDchHit");
    node1->addNode(NodeTable);
  }
  
  NodeObj = (PHIODataNode<PHObject>*)iter1.findFirst("PHIODataNode","DchHitLineTablev1");
  if (!NodeObj) {
    hitLine =   new DchHitLineTablev1();
    NodeObj  = new PHIODataNode<PHObject>(hitLine,"DchHitLineTablev1","PHObject");
    node1->addNode(NodeObj);
  }

  NodeTable = (PHIODataNode<PHTable>*)iter2.findFirst("PHIODataNode","dDchHit");
  if (!NodeTable) {
    dDchHitWrapper* hit =   new dDchHitWrapper("dDchHit",60000);
    NodeTable = new PHIODataNode<PHTable>(hit,"dDchHit");
    node2->addNode(NodeTable);
  }
  NodeObj = (PHIODataNode<PHObject>*)iter2.findFirst("PHIODataNode","DchHitLineTablev1");
  if (!NodeObj) {
    hitLine =   new DchHitLineTablev1();
    NodeObj    = new PHIODataNode<PHObject>(hitLine,"DchHitLineTablev1","PHObject");
    node2->addNode(NodeObj);
  }
  //added tables to reconstructed

  NodeTable = (PHIODataNode<PHTable>*)iter3.findFirst("PHIODataNode","dDchHit");
  if (!NodeTable) {
    dDchHitWrapper* hit =   new dDchHitWrapper("dDchHit",60000);
    NodeTable = new PHIODataNode<PHTable>(hit,"dDchHit");
    node3->addNode(NodeTable);
  }
  NodeObj = (PHIODataNode<PHObject>*)iter3.findFirst("PHIODataNode","DchHitLineTablev1");
  if (!NodeObj) {
    hitLine =   new DchHitLineTablev1();
    NodeObj    = new PHIODataNode<PHObject>(hitLine,"DchHitLineTablev1","PHObject");
    node3->addNode(NodeObj);
  }
  NodeObj = (PHIODataNode<PHObject>*)iter3.findFirst("PHIODataNode","DchTrack");
  if (NodeObj) {
    dchtrack = (DchTrack*)NodeObj->getData();
  }
  PHCompositeNode*embedNode = static_cast<PHCompositeNode*>(iter1.findFirst("PHCompositeNode","EMBED"));
  if (!embedNode) {
    embedNode = new PHCompositeNode("EMBED");
    node1->addNode(embedNode);
  }
  PHDataNode<PHEmbedStat> *embedStatNode = (PHDataNode<PHEmbedStat>*)iter1.findFirst("PHDataNode","PHEmbedStat");
  if (!embedStatNode) {
    PHEmbedStat *stat = new PHEmbedStat;
    embedStatNode = new PHDataNode<PHEmbedStat>(stat,"PHEmbedStat");
    embedNode->addNode(embedStatNode);
  }
  embedStat = embedStatNode->getData();

  //we need to reset some of the table, since reconstruction 
  //sometimes does not clean it
  dchtrack->Reset();

  PHIODataNode<CglTrack> *cgltracknode;
  PHTypedNodeIterator<CglTrack> cgl3(node3);
  cgltracknode    = cgl3.find("CglTrack");
  if (cgltracknode) {
    CglTrack* cgltrack = cgltracknode->getData();
    cgltrack->Reset();
  }
 
  
  cgltracknode    = cgl3.find("CglTrackBack");
  if (cgltracknode) {
    CglTrack* cgltrack = cgltracknode->getData();
    cgltrack->Reset();
  }
  
  
  PHIODataNode<PHTrackOut> *phtracknode;
  PHTypedNodeIterator<PHTrackOut> ph3(node3);
  phtracknode    = ph3.find("PHTrackOut");
  if (phtracknode) {
    PHTrackOut* phtrack = phtracknode->getData();
    phtrack->Reset();
  }
  phtracknode    = ph3.find("PHTrackOutBack");
  if (phtracknode) {
    PHTrackOut* phtrack = phtracknode->getData();
    phtrack->Reset();
  }

 

  PHIODataNode<PHDchTrackOut> *phdchtracknode;
  PHTypedNodeIterator<PHDchTrackOut> phdch3(node3);
  phdchtracknode    = phdch3.find("PHDchTrackOut");
  if (phdchtracknode) {
    PHDchTrackOut* phdchtrack = phdchtracknode->getData();
    phdchtrack->Reset();
  }

  //get handles to calibration objects
  PHDataNode<PHDchCalibrationObject>* dchDcoNode = 
    (PHDataNode<PHDchCalibrationObject>*)iter3.findFirst("PHDataNode","DchDCO"); 
  if (!dchDcoNode) { 
    PHMessage("PHEmbedMixer::event",PHError,"DCO missing in the tree");
  }
  dchCalibrationObject = dchDcoNode->getData();
  PHDataNode<PHDchAddressObject>* dchDaoNode = 
    (PHDataNode<PHDchAddressObject>*)iter3.findFirst("PHDataNode","DchDAO"); 
  if (!dchDaoNode) { 
    PHMessage("PHEmbedMixer::event",PHError,"DAO missing in the tree");
    return ABORTEVENT;
  }
  dchAddressObject = dchDaoNode->getData();

  PHDataNode<PHDchGeometryObject>* dchDgoNode =
    (PHDataNode<PHDchGeometryObject>*)iter3.findFirst("PHDataNode","DchDGO"); 
  if (!dchDgoNode) { 
    PHMessage("PHEmbedMixer::event",PHError,"DGO missing in the tree");
    return ABORTEVENT;
  }
  dchGeometry = dchDgoNode->getData();

  
  int mc=0; // will set mc>0 if one or more GEANT particles found
  fkinWrapper *fkin=0;
  NodeTable = (PHIODataNode<PHTable>*)iter1.findFirst("PHIODataNode","fkin");
  if (NodeTable) {
    fkin = (fkinWrapper*)NodeTable->getData();
  }
  if (fkin && fkin->RowCount()) mc=1;


  PHIODataNode<DchHitLineTable> *dchhitnode;
  PHTypedNodeIterator<DchHitLineTable> dchhit1(node1);
  dchhitnode = dchhit1.find("DchHitLineTable");
  DchHitLineTable* HIT1 = dchhitnode->getData();


  PHTypedNodeIterator<DchHitLineTable> dchhit2(node2);
  dchhitnode = dchhit2.find("DchHitLineTable");
  DchHitLineTable* HIT2 = dchhitnode->getData();


  PHTypedNodeIterator<DchHitLineTable> dchhit3(node3);
  dchhitnode = dchhit3.find("DchHitLineTable");
  DchHitLineTable* HIT3 = dchhitnode->getData();


  int numOfHits1 = HIT1->Entries();
  int numOfHits2 = HIT2->Entries();
  int numOfHits3 = HIT3->Entries();
  int i,j,k,type,x1,x2,uv;
  PHPoint xyz;
  int armEmbed = 2;

  if (kickOutHitsToSpeedupReco) {
    if (mc) {
      int armWest = 0, armEast = 0;
      for (int i=0; i<numOfHits1; i++) {
	if (HIT1->getId(i)>-1) {
	  if (HIT1->getXYZ(i).getX()>0) armWest = 1;
	  else armEast = 1;
	}
      }
      if (!armEast && !armWest)     armEmbed = -1;
      if (armEast && armWest)       armEmbed =  2;
      if (armEast==1 && armWest==0) armEmbed =  0;
      if (armEast==0 && armWest==1) armEmbed =  1;
    }
  }

  if (verbose > 10)  cout << "DchMixer::merge(): armEmbed: " << armEmbed << endl;

  DchHitLineOut *hit;
  if (armEmbed==0 || armEmbed==1) { // need to remove hits in one arm
    int nok=0;
     for (i=0;i<numOfHits2;i++) {
      if (HIT2->getArm(i)==armEmbed) {
	nok++;
      }
    }
     //copy from HIT2 to HIT3

    HIT3->Reset();
    HIT3->Expand(nok);
    nok=0;
     for (i=0;i<numOfHits2;i++) {
      if (HIT2->getArm(i)==armEmbed) {
	hit = HIT2->getHit(i);
	HIT3->AddHit(hit);
	HIT3->setId(nok,nok);
	nok++;
      }
    }
  }else{
     //copy from HIT2 to HIT3
    HIT3->Reset();
    HIT3->Expand(numOfHits2);
    for (i=0;i<numOfHits2;i++) {
      hit = HIT2->getHit(i);
      HIT3->AddHit(hit);
      HIT3->setId(i,i);
    }
  }

  float dcT0Westoffset = dcT0West - dchCalibrationObject->getNominalT0(1);
  float dcT0Eastoffset = dcT0East - dchCalibrationObject->getNominalT0(0);
  if (verbose > 1)  cout << "dcT0Westoffset " << dcT0Westoffset 
			 << " dcT0Eastoffset " << dcT0Eastoffset << endl;

  for (i=0;i<HIT3->Entries();i++) {
    if (HIT3->getArm(i)==1) {
      HIT3->setTime1(i,int(HIT3->getTime1(i)-dcT0Westoffset));
    }else{
      HIT3->setTime1(i,int(HIT3->getTime1(i)-dcT0Eastoffset));      
    }
  }
  numOfHits3  = HIT3->Entries();

  //evaluation information.
  vector<int>& dchitE    = embedStat->get_dchitEmbed();
  vector<int>& dchitStat = embedStat->get_dchitEmbedStat();
  dchitE.clear();
  dchitStat.clear();
  if (HIT1) {
    int nentries = HIT1->Entries();
    dchitE.resize(nentries,-1);               
    dchitStat.resize(nentries,-1);
  }
  
  static int denorm=0,norm=0,norm1=0; // some counters  
  //next we need to merge hit1 into hit3  
  x1 = x2 = uv = 0;
  for (i=0;i<numOfHits1;i++) {
    if (HIT1->getId(i)>-1) {
      j = HIT1->getPlane(i);
      k = wireType[j];
      
      if (k==X1Wire)x1++; 
      else if (k==X2Wire)x2++;
      else uv++;
    }
  }

  if (verbose > 0) {
    cout<< "DchMixer: MC Event has " << x1 << " x1hits, " <<x2 << " x2hits, " 
	<< uv << " uvhits." << endl;
    cout<< "          Real Event has " << numOfHits2 << " hits" 
	<< ", after kicking out unused arm: " << numOfHits3 << endl;
  }

  float timel1,timet1,timel2,timet2,maxt,mint;
  int isXhit=0,mergedd=0,id;
  int needcheck;

  //Do the embedding work!!!!
  k=numOfHits3;
  for (i=0; i<numOfHits1; i++) {
    if (HIT1->getId(i)>-1) {
      id =-1;
      hit = HIT1->getHit(i);
      if (hit->getId()!=i) cout<<"dc mixing: id not matched"<<endl;

      // type is bitmask for what kind of time overlap btween MC/RD pulses
      type = 0;
      isXhit=0; // Is the hit on an x-plane?
      mergedd=0;
      if (hit->getPlane()<12 || (hit->getPlane()>19 && hit->getPlane()<32) ) isXhit =1;
      
      // loop over hits in embedded table
      for (j=0;j<k;j++) { 
	needcheck=0;
	if (HIT3->getPlane(j) == hit->getPlane()&&HIT3->getCell(j) == hit->getCell()&&
	   HIT3->getSide(j) == hit->getSide()&&HIT3->getArm(j) == hit->getArm()) {
	  //same cell
	  if (isXhit) needcheck =1;
	  else if (HIT3->getIdmirror(j) == hit->getIdmirror()) needcheck=1;
	}

	if (needcheck) {
	  timel1 = hit->getTime1();
	  timet1 = timel1 + hit->getWidth();
	  timel2 = HIT3->getTime1(j);
	  timet2 = timel2 + HIT3->getWidth(j);
	  
	  // type = 0 here, now set bits...
	  // if MC hit satisfies none of the 4 cases, it occurs after the RD and type remains 0x0.
	  // if MC pulse overlaps with RD pulse, type will be set to something between 0 and 16.
	  // if MC pulse ends before RD pulse starts, all 4 bits are set, so type will be set to 0xf.
	  if (timel1 < timel2) type |= 0x1; //    1
	  if (timel1 < timet2) type |= 0x2; //   10
	  if (timet1 < timel2) type |= 0x4; //  100
	  if (timet1 < timet2) type |= 0x8; // 1000

	  if (type != 0x0 && type != 0xf) { // overlap
	    mergedd |= 0x01;
	    maxt = timet1;
	    if (maxt < timet2) maxt = timet2;
	    mint = timel1;
	    if (mint > timel2) mint = timel2;
	    
	    if (mint>maxt) cout<<"Warning: Hit times screwed up in DchMixer::merge()"<<endl;
	    if (timel1 <= timel2) { // embedded hit survived
	      mergedd |= 0x02;
	      id = HIT3->getId(j);
	      if (id!=j) cout<<"dc mixing: id not matched"<<endl;
	      HIT3->setIdmirror(j,hit->getIdmirror());
	      HIT3->setArm(j,hit->getArm());
	      HIT3->setSide(j,hit->getSide());
	      HIT3->setPlane(j,hit->getPlane());
	      HIT3->setCell(j,hit->getCell());
	      HIT3->setWidth(j,maxt-mint);
	      HIT3->setTime1(j,hit->getTime1());
	      HIT3->setXYZ(j,hit->getXYZ());

	      if (verbose>10) {
		cout << "embedded hit won. " << std::flush;
		cout << "(isXhit, timel1, timet1, timel2, timet2, hitid, HIT3id, idmirror): " 
		     << std::flush;
		cout << "(" << isXhit << ", " << timel1 << ", " << timet1 
		     << ", " << timel2 << ", " << timet2
		     << ", " << hit->getId() << ", " << HIT3->getId(j) 
		     << ", " << hit->getIdmirror() << ")" << endl;
	      }
	    } // end if embedded MC hit survived
	    else { // background hit survived
	      id =HIT3->getId(j);
	      HIT3->setWidth(j,maxt-mint);
	      if (verbose>10) {
		cout << "background hit won. " << std::flush;
		cout << "(isXhit, timel1, timet1, timel2, timet2, hitid, HIT3id, idmirror): " 
		     << std::flush;
		cout << "(" << isXhit << ", " << timel1 << ", " << timet1 
		     << ", " << timel2 << ", " << timet2
		     << ", " << hit->getId() << ", " << HIT3->getId(j) 
		     << ", " << hit->getIdmirror() << ")" << endl;
	      }
	    } // end if RD hit survived    
	    goto finishchecking; // at least it is a short, forward goto
	  } // end if overlap
	} // end check
      } // end j loop ()
    finishchecking:
      if (type==0x0||type==0xf) { //no overlap	

	HIT3->AddHit(hit);
	id = numOfHits3;
	HIT3 ->setId(numOfHits3,numOfHits3);
	numOfHits3++;
      }
      //counting
      if (isXhit) {
	denorm++;    
	if (mergedd & 0x01) { // least sig. bit of mergedd: overlap
	  norm++;
	  if (mergedd & 0x02) norm1++; // 2nd LSB: MC hit survived
	}
      }
      //fill the merged hitid and status.
      dchitE[i]    = id;
      dchitStat[i] = mergedd;
    }
  }

  if (verbose>10) cout << "# x hits: " << denorm << "\n# merged & overlapped: " 
		       << norm << "\n# merged where MC hit survived: " << norm1 << endl;
  // checkhits();
  constructHitLineOut();
  return EVENT_OK;
}


static TFile *f;
static TNtuple* nt;
void DchMixer::checkhits() {
  if (!node1||!node2||!node3) {
    cout<<"PHEmbedMixer:: missing top nodes"<<endl;
    return;
  }
  static int a =1,cnt=0;
  if (a) {
    TFile*ff = gFile;
    nt = new TNtuple("nt","nt","p:dist1:dist2:dx:dy:dz");
    f = new TFile("test.root","recreate");
    if (ff) ff->cd();
    a = 0;
  }
  cnt++;

  PHNodeIterator iter1(node1);

  DchHitLineTable* table1v1=0,*table1v2=0; 
  PHIODataNode<PHObject> *phnode;
  phnode = static_cast<PHIODataNode<PHObject>*>(iter1.findFirst("PHIODataNode","DchHitLineTablev1"));
  if (phnode) table1v1                   = static_cast<DchHitLineTable*>(phnode->getData());
  phnode = static_cast<PHIODataNode<PHObject>*>(iter1.findFirst("PHIODataNode","DchHitLineTable"));
  if (phnode) table1v2                   = static_cast<DchHitLineTable*>(phnode->getData());

  int i,global,arm,side,cell,plane,time1,mirror;
  float dist,array[100];
  PHVector drift;
  PHLine line;
  PdbIndex thisIndexAccess(0, 12799, 0, "GLOBAL");
  DchHitLineOutv1 hitLineOut;

  for (i=0;i<table1v2->Entries();i++) {
    mirror  = table1v2->getIdmirror(i);
    arm     = table1v2->getArm(i);
    side    = table1v2->getSide(i);
    plane   = table1v2->getPlane(i);
    cell    = table1v2->getCell(i);
    time1   = table1v2->getTime1(i);
    dchAddressObject->setSoft(arm,side,plane,cell);
    global = dchAddressObject->getGlobalIndex()->getValue();
    thisIndexAccess.setValue(global);
    if (mirror==0) {
      drift = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, time1, kDchLinearCalib, 0, 1);
    }else if (mirror == -1) {
      drift = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, time1, kDchInnerStereoCalib, 0, 1);
    }else if (mirror==1) {
      drift = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, time1, kDchOuterStereoCalib, 0, 1);
    }else{
      cout<<"serious error"<<endl;
    }
    dist  = drift.getX();
    line = dchGeometry->transformDistanceToLine(arm,side,plane,cell,drift);
    PHLine line2(line.getBasepoint(),table1v2->getXYZ(i));
    PHVector dir=line2.getDirection();
    int m=0;
    array[m++] = plane;
    array[m++] = dist;
    array[m++] = table1v1->getDistance(i);
    array[m++] = table1v1->getXYZ(i).getX()- table1v2->getXYZ(i).getX();
    array[m++] = table1v1->getXYZ(i).getY()- table1v2->getXYZ(i).getY();
    array[m++] = table1v1->getXYZ(i).getZ()- table1v2->getXYZ(i).getZ();
    if (nt) nt->Fill(array);
  }

  if (cnt==10) {
    TFile*ff = gFile;
    if (gFile == f) ff=0;
    f->cd();
    nt->Write();
    f->Close();
    if (ff) ff->cd();
    nt = 0;
  }
}
int DchMixer::constructHitLineOut() {
  if (!node1||!node2||!node3) {
    cout<<"PHEmbedMixer:: missing top nodes"<<endl;
    return ABORTEVENT;
  }
  int mc_flag = dchAddressObject->getFlagMC();
  if (!mc_flag) cout<<" address should be MC mode!!!!!!!!!!!!!"<<endl;


  PHNodeIterator iter2(node2);
  PHNodeIterator iter3(node3);

  DchHitLineTable* table3v1=0,*table3v2=0,*table2v1=0,*table2v2=0; 
  dDchHitWrapper *hit2 = 0;
  dDchHitWrapper *hit3 = 0;
  PHIODataNode<PHObject> *phnode;
  PHIODataNode<PHTable>  *tablenode;
  phnode = static_cast<PHIODataNode<PHObject>*>(iter3.findFirst("PHIODataNode","DchHitLineTablev1"));
  if (phnode) table3v1                   = static_cast<DchHitLineTable*>(phnode->getData());
  phnode = static_cast<PHIODataNode<PHObject>*>(iter3.findFirst("PHIODataNode","DchHitLineTable"));
  if (phnode) table3v2                   = static_cast<DchHitLineTable*>(phnode->getData());
  tablenode = static_cast<PHIODataNode<PHTable>*>(iter3.findFirst("PHIODataNode","dDchHit"));
  if (tablenode) hit3               = static_cast<dDchHitWrapper*>(tablenode->getData());

  phnode = static_cast<PHIODataNode<PHObject>*>(iter2.findFirst("PHIODataNode","DchHitLineTablev1"));
  if (phnode) table2v1                   = static_cast<DchHitLineTable*>(phnode->getData());
  phnode = static_cast<PHIODataNode<PHObject>*>(iter2.findFirst("PHIODataNode","DchHitLineTable"));
  if (phnode) table2v2                   = static_cast<DchHitLineTable*>(phnode->getData());
  tablenode = static_cast<PHIODataNode<PHTable>*>(iter2.findFirst("PHIODataNode","dDchHit"));
  if (tablenode) hit2               = static_cast<dDchHitWrapper*>(tablenode->getData());


  int i,global,arm,side,cell,plane,time1,mirror;
  float dist;
  PHVector drift;
  PHLine line;
  PdbIndex thisIndexAccess(0,12799,0,"GLOBAL");
  DchHitLineOutv1 hitLineOut;
  DchHitLineOut* Hit= &hitLineOut;
 

  table3v1 ->Reset();
  table3v1 ->Expand(table3v2->Entries());
  hit3->SetRowCount(table3v2->Entries());
  for (i=0;i<table3v2->Entries();i++) {
    mirror  = table3v2->getIdmirror(i);
    arm     = table3v2->getArm(i);
    side    = table3v2->getSide(i);
    plane   = table3v2->getPlane(i);
    cell    = table3v2->getCell(i);
    time1   = table3v2->getTime1(i);

    dchAddressObject->setSoft(arm,side,plane,cell);
    global = dchAddressObject->getGlobalIndex()->getValue();
    thisIndexAccess.setValue(global);

    // found that the timing calibration and thus the calculated distance of a hit from a wire is different for simulation and data
    //  applying the simulation calculation on real data hits screws up the tracking
    //   so we just grab the distance from the DST and run the tracking over that
      
    PHPoint mypoint = table3v2->getXYZ(i);   // we are going to use the existing basepoint from the DST
    
    line = dchGeometry->transformDistanceToLine(arm,side,plane,cell,1);  // get direction of line using 1 drift
       

    // I hope this is ok and not really needed for anything, but I am suspicioius
    hitLineOut.setDistance(1);


    // first get the phi location of the hit in PHENIX coordinates
    double tempx, tempy;
    tempx = table3v2->getXYZ(i).getX();
    tempy = table3v2->getXYZ(i).getY();
    
    PHAngle temp_phi( tempx==0.0 && tempy==0.0 ? 0.0:atan2(tempy,tempx) );
    
    // reversal condition
    if( temp_phi.getPhi()<0.0 || (temp_phi.getPhi()<3.14 && temp_phi.getPhi()>3.14/2.) )
      {
	PHVector myvector = line.getDirection();
	myvector.setX(-myvector.getX());
	myvector.setY(-myvector.getY());
	myvector.setZ(-myvector.getZ());
	
	line.setDirection(myvector);
      }
    
    
    line.setBasepoint(mypoint);
    
    hitLineOut.setXYZ(line.getBasepoint());
    hitLineOut.setVXYZ(line.getDirection());
   

   	
    hitLineOut.setId(table3v2->getId(i));
    hitLineOut.setIdmirror(table3v2->getIdmirror(i));
    hitLineOut.setArm(table3v2->getArm(i));
    hitLineOut.setSide(table3v2->getSide(i));
    hitLineOut.setPlane(table3v2->getPlane(i));
    hitLineOut.setCell(table3v2->getCell(i));
    hitLineOut.setWidth(table3v2->getWidth(i));
    hitLineOut.setTime1(table3v2->getTime1(i));
    hitLineOut.setTime2((int) (table3v2->getTime1(i)+table3v2->getWidth(i)));
    table3v1->AddHit((Hit));
    hit3->set_id(i,hitLineOut.getId());
    hit3->set_arm(i,hitLineOut.getArm());
    hit3->set_plane(i,hitLineOut.getPlane());
    hit3->set_cell(i, hitLineOut.getCell());
    hit3->set_side(i, hitLineOut.getSide());
    hit3->set_distance(i, hitLineOut.getDistance());
    hit3->set_width(i, hitLineOut.getWidth());
    hit3->set_time1(i,hitLineOut.getTime1());
    hit3->set_time2(i,hitLineOut.getTime2());
    hit3->set_idraw1(i,hitLineOut.getIdraw1());
    hit3->set_idraw2(i,hitLineOut.getIdraw2());
    hit3->set_idmirror(i, hitLineOut.getIdmirror());
    hit3->set_used(i,hitLineOut.getUsed());
    hit3->set_xyz(0,i,hitLineOut.getXYZ().getX());
    hit3->set_xyz(1,i,hitLineOut.getXYZ().getY());
    hit3->set_xyz(2,i,hitLineOut.getXYZ().getZ());
    hit3->set_vxyz(0,i,hitLineOut.getVXYZ().getX());
    hit3->set_vxyz(1,i,hitLineOut.getVXYZ().getY());
    hit3->set_vxyz(2,i,hitLineOut.getVXYZ().getZ());
  }


  table2v1 ->Reset();
  table2v1 ->Expand(table2v2->Entries());
  hit2->SetRowCount(table2v2->Entries());
  for (i=0;i<table2v2->Entries();i++) {
    mirror  = table2v2->getIdmirror(i);
    arm     = table2v2->getArm(i);
    side    = table2v2->getSide(i);
    plane   = table2v2->getPlane(i);
    cell    = table2v2->getCell(i);
    time1   = table2v2->getTime1(i);
    dchAddressObject->setSoft(arm,side,plane,cell);
    global = dchAddressObject->getGlobalIndex()->getValue();
    thisIndexAccess.setValue(global);
    if (mirror==0) {
      drift = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, time1, kDchLinearCalib, 0, 1);
    }else if (mirror == -1) {
      drift = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, time1, kDchInnerStereoCalib, 0, 1);
    }else if (mirror==1) {
      drift = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, time1, kDchOuterStereoCalib, 0, 1);
    }else{
      cout<<"serious error"<<endl;
    }
    dist  = drift.getX();
    line = dchGeometry->transformDistanceToLine(arm,side,plane,cell,drift);


    hitLineOut.setId(table2v2->getId(i));
    hitLineOut.setIdmirror(table2v2->getIdmirror(i));
    hitLineOut.setArm(table2v2->getArm(i));
    hitLineOut.setSide(table2v2->getSide(i));
    hitLineOut.setPlane(table2v2->getPlane(i));
    hitLineOut.setCell(table2v2->getCell(i));
    hitLineOut.setWidth(table2v2->getWidth(i));
    hitLineOut.setTime1(table2v2->getTime1(i));
    hitLineOut.setXYZ(line.getBasepoint());
    hitLineOut.setTime2((int) (table2v2->getTime1(i)+table2v2->getWidth(i)));
    hitLineOut.setVXYZ(line.getDirection());
    hitLineOut.setDistance(dist);
    table2v1->AddHit(Hit);
    hit2->set_id(i,hitLineOut.getId());
    hit2->set_arm(i,hitLineOut.getArm());
    hit2->set_plane(i,hitLineOut.getPlane());
    hit2->set_cell(i, hitLineOut.getCell());
    hit2->set_side(i, hitLineOut.getSide());
    hit2->set_distance(i, hitLineOut.getDistance());
    hit2->set_width(i, hitLineOut.getWidth());
    hit2->set_time1(i,hitLineOut.getTime1());
    hit2->set_time2(i,hitLineOut.getTime2());
    hit2->set_idraw1(i,hitLineOut.getIdraw1());
    hit2->set_idraw2(i,hitLineOut.getIdraw2());
    hit2->set_idmirror(i, hitLineOut.getIdmirror());
    hit2->set_used(i,hitLineOut.getUsed());
    hit2->set_xyz(0,i,hitLineOut.getXYZ().getX());
    hit2->set_xyz(1,i,hitLineOut.getXYZ().getY());
    hit2->set_xyz(2,i,hitLineOut.getXYZ().getZ());
    hit2->set_vxyz(0,i,hitLineOut.getVXYZ().getX());
    hit2->set_vxyz(1,i,hitLineOut.getVXYZ().getY());
    hit2->set_vxyz(2,i,hitLineOut.getVXYZ().getZ());
  }
  return EVENT_OK;

}
