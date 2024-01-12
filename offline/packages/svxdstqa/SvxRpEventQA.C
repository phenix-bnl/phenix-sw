
#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include "SvxRpEventQA.h"
#include <TH1.h>
#include <TH2.h>
#include "TProfile.h"
#include <Fun4AllServer.h>
#include "Fun4AllHistoManager.h"
#include "Fun4AllReturnCodes.h"
#include <getClass.h>
#include <RunHeader.h>
#include <Event.h>
#include <RunHeader.h>
#include <algorithm>
#include "PHGlobal.h"
#include "RpSumXYObject.h"
#include "RpSnglSumXY.h"
#include "BbcOut.h"
#include "VtxOut.h"
#include "RpConst.h"

//static Fun4AllHistoManager* HistoManager = 0;
using namespace std;
using namespace findNode;

SvxRpEventQA::SvxRpEventQA(const string& filename): 
  SubsysReco("SvxRpEventQA"), m_outFileName(filename)
{
  HistoManager = new Fun4AllHistoManager("svxrpeventqa");
}

SvxRpEventQA::~SvxRpEventQA()
{
  delete HistoManager;
  return;   
}

int SvxRpEventQA::Init(PHCompositeNode *topNode)
{  
  char name[80];
  for(int icent=0;icent<ncent;icent++){
    for(int izv=0;izv<nzv;izv++){
      for(int idet=0;idet<ndet;idet++){
	//vtx:0<=idet<62 mpc:62<=idet<65 bbc:65<=idet<68 smd:68<=idet<71 cnt:71<=idet<76
	
	for(int i=0;i<2;i++){
	  if(i==0)      sprintf(name,"C%dZ%dD%dQx",icent,izv,idet);
	  else if(i==1) sprintf(name,"C%dZ%dD%dQy",icent,izv,idet);

	  HistoManager->registerHisto(new TProfile(name,name,nhar,-0.5,nhar-0.5,-10,10,"S"));
	  avexy[icent][izv][idet][i]=static_cast<TProfile*>(HistoManager->getHisto(name));
	}
      }
      sprintf(name,"C%dZ%dQw",icent,izv);
      HistoManager->registerHisto(new TProfile(name,name,ndet,-0.5,ndet-0.5,0,10000));
      wei[icent][izv]=static_cast<TProfile*>(HistoManager->getHisto(name));
    }
  }
  return EVENT_OK;
}

int SvxRpEventQA::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK; 
}

int SvxRpEventQA::process_event(PHCompositeNode *topNode)
{
  //EventClass
  PHGlobal *global=getClass<PHGlobal>(topNode,"PHGlobal");
  BbcOut *bbcout=getClass<BbcOut>(topNode,"BbcOut");
  VtxOut *vtxout=getClass<VtxOut>(topNode,"VtxOut");
  float vertex=-9999.0;
  float vtxz=-9999.0;
  float bbcz=-9999.0;

  if(vtxout&&vtxout->isVtx("SVX")) vtxz = vtxout->get_ZVertex("SVX");
  if(global){
    bbcz=global->getBbcZVertex();
  }
  else if(bbcout){
    bbcz=bbcout->get_VertexPoint();
  }
  if(vtxz>-9000) vertex=vtxz;
  else if(bbcz>-9000) vertex=bbcz;
  else vertex=-9999;  
  if(vertex<-9000){
    if(verbosity>0) cerr<< PHWHERE <<"no z-vertex available, skip the event" << endl;
    return  EVENT_OK;
  }
  
  float zlim=10.0; // +-10cm window
  int   izv = (int)(nzv*(vertex+zlim)/(2.0*zlim));
  if (izv<0 || izv>nzv-1) return EVENT_OK; 
 
  float bbccha_s=-9999.0;
  float bbccha_n=-9999.0;
  float bbccha_t=-9999.0;
  if(global){
    bbccha_s = global->getBbcChargeS();
    bbccha_n = global->getBbcChargeN();
  }	    
  else if(bbcout){
    bbccha_s = bbcout->get_ChargeSum(0);
    bbccha_n = bbcout->get_ChargeSum(1);
  }
  if(bbccha_s>-9000 && bbccha_n>-9000){
    bbccha_t = bbccha_s + bbccha_n;
  }
  else bbccha_t=-9999;
  
  if(bbccha_t<-9000){
    cerr<< PHWHERE <<"no bbccharge ailable, skip the event" << endl;
    return EVENT_OK;
  }

  int icent=4; //9;
  //float bbc_cut[9]={233,307,366,411,445,469,484,493,498};
  float bbc_cut[4]={307,411,469,493};
  for(int jcent=3;jcent>=0;jcent--){
    if(bbccha_t>2500.0*(500.0-bbc_cut[jcent])/500.0) icent=jcent;
  }
  float Q[ndet][nhar][3];
  for(int idet=0;idet<ndet;idet++){
    for(int ihar=0;ihar<nhar;ihar++){
      for(int i=0;i<3;i++){
	Q[idet][ihar][i]=0.0;
      }
    }
  }


  static const char objname[3][256]={"RpSumXYObject", "SvxRpSumXYObject", "CntRpSumXYObject"};
  RpSumXYObject *sumxy=NULL;

  for(int iobj=0; iobj<3; iobj++){
    sumxy = getClass<RpSumXYObject>(topNode,"RpSumXYObject");
    if(sumxy!=NULL) {
      if(verbosity>0){cout<<"RpSumXYObject is found : "<<objname[iobj]<<endl; }
      break;
    }
  }
  if(verbosity>0){
    if(sumxy==NULL){cout<<"Error : RpSumXYObject is not found : "<<endl; }
  }
  
  if(sumxy){
    //Svx clusterinfo & segmentinfo
    for(int idet=0;idet<62;idet++){
      for(int ihar=0;ihar<nhar;ihar++){
	int id=RP::ID_SVX;
	int idcode=RP::calcIdCode(id,idet,ihar); 
	float qx=sumxy->getRpSumXY(idcode)->QVector(0);
	float qy=sumxy->getRpSumXY(idcode)->QVector(1);
	float qw=sumxy->getRpSumXY(idcode)->Weight();
	Q[idet][ihar][0]=qx;
	Q[idet][ihar][1]=qy;
	Q[idet][ihar][2]=qw;
      }
    }    
    //MPC
    for(int idet=0;idet<3;idet++){
      for(int ihar=0;ihar<nhar;ihar++){
	int id=RP::ID_MPC;
	int idcode=RP::calcIdCode(id,idet,ihar);
	float qx=sumxy->getRpSumXY(idcode)->QVector(0);
	float qy=sumxy->getRpSumXY(idcode)->QVector(1);
	float qw=sumxy->getRpSumXY(idcode)->Weight();
	Q[idet+62][ihar][0]=qx;
	Q[idet+62][ihar][1]=qy;
	Q[idet+62][ihar][2]=qw;	
      }
    }
    //BBC
    for(int idet=0;idet<3;idet++){
      for(int ihar=0;ihar<nhar;ihar++){
	int id=RP::ID_BBC;
	int idcode=RP::calcIdCode(id,idet,ihar);
	float qx=sumxy->getRpSumXY(idcode)->QVector(0);
	float qy=sumxy->getRpSumXY(idcode)->QVector(1);
	float qw=sumxy->getRpSumXY(idcode)->Weight();
	Q[idet+65][ihar][0]=qx;
	Q[idet+65][ihar][1]=qy;
	Q[idet+65][ihar][2]=qw;	
      }
    }
    ///SMD///
    for(int idet=0;idet<3;idet++){
      for(int ihar=0;ihar<1;ihar++){
	int id=RP::ID_SMD;
	int idcode = RP::calcIdCode(id, idet, ihar);
	float qx = sumxy->getRpSumXY(idcode)->QVector(0);
	float qy = sumxy->getRpSumXY(idcode)->QVector(1);
	float qw = sumxy->getRpSumXY(idcode)->Weight();
	Q[idet+68][ihar][0]=qx;
	Q[idet+68][ihar][1]=qy;
	Q[idet+68][ihar][2]=qw;
      }
    }
    ///CNT///
    for(int idet=0;idet<5;idet++){
      for(int ihar=0;ihar<nhar;ihar++){
	int id=RP::ID_CNT;
	int idcode = RP::calcIdCode(id, idet, ihar);
	float qx = sumxy->getRpSumXY(idcode)->QVector(0);
	float qy = sumxy->getRpSumXY(idcode)->QVector(1);
	float qw = sumxy->getRpSumXY(idcode)->Weight();
	Q[idet+71][ihar][0]=qx;
	Q[idet+71][ihar][1]=qy;
	Q[idet+71][ihar][2]=qw;
      }
    }
    for(int idet=0;idet<ndet;idet++){
      wei[icent][izv]->Fill(idet,Q[idet][0][2]);
      if(Q[idet][0][2]!=0.0){
	for(int i=0;i<2;i++){
	  for(int ihar=0;ihar<nhar;ihar++){
	    //if(idet==0) cout << Q[idet][ihar][i]<< endl;
	    float ave_xy=Q[idet][ihar][i]/Q[idet][ihar][2];
	    avexy[icent][izv][idet][i]->Fill(ihar,ave_xy);
	  }
	}
      }
    }
  }
  return EVENT_OK;
}

int SvxRpEventQA::End(PHCompositeNode *topNode)
{
  HistoManager -> dumpHistos(m_outFileName);
  return EVENT_OK;
}
