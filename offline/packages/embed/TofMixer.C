#include "TofMixer.hh"
#include "TofGeometryObject.hh"
#include "TofOut.h"
#include "dTofReconstructedWrapper.h"
#include "PHIODataNode.h"
#include "PHEmbedStat.h"


#include "Fun4AllReturnCodes.h"

#include "getClass.h"

#include <iostream>

using namespace std;

TofMixer::TofMixer()
{
  verbose = 0;
}


int TofMixer::InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged){
  if((!sngl)||(!real)||(!merged)){
    cout<< PHWHERE << " one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  node1  = sngl;
  node2  = real;
  node3  = merged;
  return EVENT_OK;
}

// merge Tof hits (Sasha Lebedev 24-09-2002 lebedev@iastate.edu)
int TofMixer::merge(){
  if((!node1)||(!node2)||(!node3)){
    cout<< PHWHERE << "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  PHNodeIterator iter1(node1);
  PHNodeIterator iter2(node2);
  PHNodeIterator iter3(node3);  

  PHCompositeNode* embedNode = static_cast<PHCompositeNode*>(iter1.findFirst("PHCompositeNode","EMBED"));
  if(!embedNode){
    embedNode = new PHCompositeNode("EMBED");
    node1->addNode(embedNode);
  }

  PHDataNode<PHEmbedStat> *embedStatNode = (PHDataNode<PHEmbedStat>*)iter1.findFirst("PHDataNode","PHEmbedStat");
  if(!embedStatNode){
    PHEmbedStat *event = new PHEmbedStat;
    embedStatNode = new PHDataNode<PHEmbedStat>(event,"PHEmbedStat");
    embedNode->addNode(embedStatNode);
  }
  embedStat = embedStatNode->getData();

  TofGeometry = findNode::getClass<TofGeometryObject>(node3,"TofGeometry");
  if (!TofGeometry)
    {
    PHMessage("PHEmbedMixer::event",PHError,"TofGeometry missing in the tree");
    return ABORTEVENT;
  }


  PHTypedNodeIterator<TofOut> tofout1(node1);
  PHTypedNodeIterator<TofOut> tofout2(node2);
  PHTypedNodeIterator<TofOut> tofout3(node3);
  
  PHIODataNode<TofOut> *tofnode = tofout1.find("TofOut");
  TofOut* TOFREC1 = tofnode->getData();
  tofnode        = tofout2.find("TofOut");
  if(!tofnode){
    cout << PHWHERE << "Real Data does not have TofOut Node" << endl;
    return  EVENT_OK;
  }

  TofOut* TOFREC2  = tofnode->getData();
  tofnode          = tofout3.find("TofOut");
  TofOut* TOFREC3  = tofnode->getData();

  vector<int>& tofoutE    = embedStat->get_tofoutEmbed();
  vector<int>& tofoutStat = embedStat->get_tofoutEmbedStat();
  tofoutE.clear();          
  tofoutStat.clear();

  if(TOFREC1){
    int nentries = TOFREC1->get_TofNHit();
    tofoutE.resize(nentries,-1);                 
    tofoutStat.resize(nentries,-1);
  }

  int numOfTof1 = TOFREC1->get_TofNHit();
  int numOfTof2 = TOFREC2->get_TofNHit();

  //COPY TOFREC2 to TOFREC3
  TOFREC3->set_TClonesArraySize(numOfTof2);

  for(int i=0;i<numOfTof2;i++){
    TOFREC3->AddTofHit(i);
    TOFREC3->set_id(i,i);
    TOFREC3->set_slatid(i,TOFREC2->get_slatid(i));
    TOFREC3->set_sector(i,TOFREC2->get_sector(i));
    TOFREC3->set_side(i,TOFREC2->get_side(i));
    TOFREC3->set_panel(i,TOFREC2->get_panel(i));
    TOFREC3->set_slat(i,TOFREC2->get_slat(i));
    TOFREC3->set_tof(i,TOFREC2->get_tof(i));
    TOFREC3->set_tof_err(i,TOFREC2->get_tof_err(i));
    TOFREC3->set_eloss(i,TOFREC2->get_eloss(i));
    TOFREC3->set_eloss_err(i,TOFREC2->get_eloss_err(i));
    TOFREC3->set_xtof(i,0,TOFREC2->get_xtof(i,0));
    TOFREC3->set_xtof(i,1,TOFREC2->get_xtof(i,1));
    TOFREC3->set_xtof(i,2,TOFREC2->get_xtof(i,2));
    TOFREC3->set_xtof_err(i,0,TOFREC2->get_xtof_err(i,0));
    TOFREC3->set_xtof_err(i,1,TOFREC2->get_xtof_err(i,1));
    TOFREC3->set_xtof_err(i,2,TOFREC2->get_xtof_err(i,2));
    TOFREC3->set_qvc(i,0,TOFREC2->get_qvc(i,0));
    TOFREC3->set_qvc(i,1,TOFREC2->get_qvc(i,1));
    TOFREC3->set_tvc(i,0,TOFREC2->get_tvc(i,0));
    TOFREC3->set_tvc(i,1,TOFREC2->get_tvc(i,1));
    //TOFREC3->set_tdiff(i,TOFREC2->get_tdiff(i));
  }
  //finish copying

  int numOfTof3  = TOFREC3->get_TofNHit();

  if( verbose > 3 ){
    for (int i = 0;i < numOfTof1;i++)
      {
        if (TOFREC1->get_id(i) != i)
          cout << "TofOut not consistent "
               << i << " " << TOFREC1->get_id(i) << endl;
      }
    for (int i = 0;i < numOfTof3;i++)
      {
        if (TOFREC3->get_id(i) != i)
          cout << "TofOut not consistent "
               << i << " " << TOFREC3->get_id(i) << endl;
      }
  }

  // now we merge two tables
  TOFREC3->set_TClonesArraySize(numOfTof3 + numOfTof1);//simply add to tof table

  int total = numOfTof3;
//  int overlap = 0;
//  int mergedd, mergedid, mergedslatid;

  static int alltof=0, overtof=0;

  //Do the embedding work!!!!
  for(int i=0;i<numOfTof1;i++){
    int mergedid =-1;
    int overlap = 0;
    int mergedd  = 0;

    for(int j =0;j<numOfTof3;j++){
      if(TOFREC1->get_slatid(i) == TOFREC3->get_slatid(j)){ //overlapped
	int slatid   = TOFREC1->get_slatid(i);

	if( verbose > 3 ){
          cout<<"TOF hits are overlapped in one slat"<<endl;
	  //TofGeometry->print(slatid);
	}

	overlap = 1;
	mergedd |= 0x01;
	//+----------------------------------+
	//| Calculate overlap effect for TOF |
	//+----------------------------------+
	// scintillator slat info.
	float    attenu = 128.;  // attenuation length of scinti.[cm]
	float    vlight = 14.8;  // light velocity in scinti. [cm/ns]
	float    tsigma = 0.080; // intrinsic timing resolution;
	PHPoint  slatcenter = TofGeometry->getSlatXYZ(slatid);
	PHVector slatvector = TofGeometry->getSlatVector(slatid);
	float    slatlength = TofGeometry->getSlatLength(slatid);
	float    slathalflength = slatlength/2.0;

	// node1 info.
	float tof1   = TOFREC1->get_tof(i);
	float eloss1 = TOFREC1->get_eloss(i);
	float xtof1  = TOFREC1->get_xtof(i,0);
	float ytof1  = TOFREC1->get_xtof(i,1);
	float ztof1  = TOFREC1->get_xtof(i,2);
	PHPoint xyz1(xtof1, ytof1, ztof1);

	float ypos1 = PHGeometry::distancePointToPoint(xyz1, slatcenter);
	if(ytof1<slatcenter.getY()) ypos1 = -ypos1;

	float T_lower1 = tof1 + (slathalflength + ypos1)/vlight;
	float T_upper1 = tof1 + (slathalflength - ypos1)/vlight;
	float Q_lower1 = eloss1*exp(-(slathalflength + ypos1)/attenu);
	float Q_upper1 = eloss1*exp(-(slathalflength - ypos1)/attenu);

	// node3 info.
	float tof3   = TOFREC3->get_tof(j);
	float eloss3 = TOFREC3->get_eloss(j);
	float xtof3  = TOFREC3->get_xtof(j,0);
	float ytof3  = TOFREC3->get_xtof(j,1);
	float ztof3  = TOFREC3->get_xtof(j,2);
	PHPoint xyz3(xtof3, ytof3, ztof3);

	float ypos3 = PHGeometry::distancePointToPoint(xyz3, slatcenter);
	if(ytof3<slatcenter.getY()) ypos3 = -ypos3;

	float T_lower3 = tof3 + (slathalflength + ypos3)/vlight;
	float T_upper3 = tof3 + (slathalflength - ypos3)/vlight;
	float Q_lower3 = eloss3*exp(-(slathalflength + ypos3)/attenu);
	float Q_upper3 = eloss3*exp(-(slathalflength - ypos3)/attenu);

	// select/merge for each PMT
	float T_lower, T_upper, Q_lower, Q_upper;
	if(T_lower1 < T_lower3) T_lower = T_lower1; // select early timing.
	else T_lower = T_lower3;
	if(T_upper1 < T_upper3) T_upper = T_upper1;
	else T_upper = T_upper3;
	Q_lower = Q_lower1 + Q_lower3; // charge sum.
	Q_upper = Q_upper1 + Q_upper3;

	// OK. re-calculate the tof, eloss and xyz;
	float tof, ypos, eloss;
	PHPoint xyz;
	tof   = (T_lower + T_upper)/2 - slathalflength/vlight;
	ypos  = (T_lower - T_upper)/2*vlight;
	xyz   = slatcenter + (PHPoint)slatvector*ypos;
	eloss = sqrt(Q_lower*Q_upper)/exp(-slathalflength/attenu);

	// These values put in node3
	TOFREC3->set_tof(j,tof);
	TOFREC3->set_eloss(j,eloss);
	TOFREC3->set_xtof(j,0,(float)xyz.getX());
	TOFREC3->set_xtof(j,1,(float)xyz.getY());
	TOFREC3->set_xtof(j,2,(float)xyz.getZ());

	mergedid = TOFREC3->get_id(j);
	
	float tof2sigma = 2*tsigma;
	float pos2sigma = 2*tsigma*vlight;
	if(fabs(tof-tof1)<tof2sigma&&fabs(ypos-ypos1)<pos2sigma){ 
	  //embedded hit survived
	  mergedd |=  0x02;
	}

        if( verbose > 3 ){
	  cout<<"--------------------------------------------------"<<endl;
	  cout<<"slatid = "<<slatid<<endl;
	  cout<<"overlap:   singleMC + real ==> merged"<<endl;
	  cout<<"  tof  :   "<<tof1<<"   + "<<tof3<<" ==> "<<tof<<endl;
	  cout<<" eloss :   "<<eloss1<<" + "<<eloss3<<" ==> "<<eloss<<endl;
	  cout<<"  ypos :   "<<ypos1<<" + "<<ypos3<<" ==> "<<ypos<<endl;
	  cout<<"--------------------------------------------------"<<endl;
	}

	break;
      } //overlapped
    } // for(int j =0;j<numOfTof3;j++)
    if(!overlap){ // no overlap, simply added
      TOFREC3->AddTofHit(total);
      TOFREC3->set_id(total,total);
      TOFREC3->set_slatid(total,TOFREC1->get_slatid(i));
      TOFREC3->set_sector(total,TOFREC1->get_sector(i));
      TOFREC3->set_side(total,TOFREC1->get_side(i));
      TOFREC3->set_panel(total,TOFREC1->get_panel(i));
      TOFREC3->set_slat(total,TOFREC1->get_slat(i));
      TOFREC3->set_tof(total,TOFREC1->get_tof(i));
      TOFREC3->set_tof_err(total,TOFREC1->get_tof_err(i));
      TOFREC3->set_eloss(total,TOFREC1->get_eloss(i));
      TOFREC3->set_eloss_err(total,TOFREC1->get_eloss_err(i));
      TOFREC3->set_xtof(total,0,TOFREC1->get_xtof(i,0));
      TOFREC3->set_xtof(total,1,TOFREC1->get_xtof(i,1));
      TOFREC3->set_xtof(total,2,TOFREC1->get_xtof(i,2));
      TOFREC3->set_xtof_err(total,0,TOFREC1->get_xtof_err(i,0));
      TOFREC3->set_xtof_err(total,1,TOFREC1->get_xtof_err(i,1));
      TOFREC3->set_xtof_err(total,2,TOFREC1->get_xtof_err(i,2));
      TOFREC3->set_qvc(total,0,TOFREC1->get_qvc(i,0));
      TOFREC3->set_qvc(total,1,TOFREC1->get_qvc(i,1));
      TOFREC3->set_tvc(total,0,TOFREC1->get_tvc(i,0));
      TOFREC3->set_tvc(total,1,TOFREC1->get_tvc(i,1));
      mergedid          = total;
      total++;
    }
    tofoutE[i]    = mergedid;
    tofoutStat[i] = mergedd;

    if( verbose > 3 ){
      alltof++;
      if(overlap) overtof++;
    }
  }

  TOFREC3->set_TofNHit(total);
 

  //now make a copy to old dTofReconstructed table because CglReco need it

  numOfTof3= total;


  PHTypedNodeIterator<dTofReconstructedWrapper> tofrec(node3);
  PHIODataNode<dTofReconstructedWrapper> *tofrecnode = tofrec.find("dTofReconstructed");
  dTofReconstructedWrapper* TOFRECW3                 = tofrecnode->getData();
  TOFRECW3->SetRowCount(numOfTof3);

  DTOFRECONSTRUCTED_ST* tofrec3     = TOFRECW3->TableData();
  for(int i=0;i<numOfTof3;i++){
    tofrec3[i].id           = TOFREC3->get_id(i);
    tofrec3[i].slatid       = TOFREC3->get_slatid(i);
    tofrec3[i].sector       = TOFREC3->get_sector(i);
    tofrec3[i].side         = TOFREC3->get_side(i);
    tofrec3[i].panel        = TOFREC3->get_panel(i);
    tofrec3[i].slat         = TOFREC3->get_slat(i);
    tofrec3[i].tof          = TOFREC3->get_tof(i);
    tofrec3[i].tof_err      = TOFREC3->get_tof_err(i);
    tofrec3[i].eloss        = TOFREC3->get_eloss(i);
    tofrec3[i].eloss_err    = TOFREC3->get_eloss_err(i);
    tofrec3[i].xtof[0]      = TOFREC3->get_xtof(i,0);
    tofrec3[i].xtof[1]      = TOFREC3->get_xtof(i,1);
    tofrec3[i].xtof[2]      = TOFREC3->get_xtof(i,2);
    tofrec3[i].xtof_err[0]  = TOFREC3->get_xtof_err(i,0);
    tofrec3[i].xtof_err[1]  = TOFREC3->get_xtof_err(i,1);
    tofrec3[i].xtof_err[2]  = TOFREC3->get_xtof_err(i,2);
    tofrec3[i].qvc[0]       = TOFREC3->get_qvc(i,0);
    tofrec3[i].qvc[1]       = TOFREC3->get_qvc(i,1);
    tofrec3[i].tvc[0]       = TOFREC3->get_tvc(i,0);
    tofrec3[i].tvc[1]       = TOFREC3->get_tvc(i,1);
    tofrec3[i].tdiff        = 0;//TOFREC3->get_(i);
  }

  if( verbose > 3 ){
    cout<<" tof "<<alltof<<" "<<overtof<<" embed raws: "<<numOfTof1<<endl;
  }


  return EVENT_OK;
  //---------------------------------------------------------------
  // MEMO: TOF calculation in TofEvent.cc
  //
  //   tof = (T_lower + T_upper)/2 - slew - Toffset - globalT
  //  ypos = (T_lower - T_upper)/2*velocity;
  //   xyz = TofGeometry->getSlatXYZ(islat) + 
  //    (PHPoint)TofGeometry->getSlatVector(islat)*(ypos - Yoffset);
  //
  //     T_lower = tvc[0]*calib->getTvcConv(0,islat)
  //---------------------------------------------------------------  return True;
}
