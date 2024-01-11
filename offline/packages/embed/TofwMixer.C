#include <TofwMixer.hh>
#include <TofwGeometry.h>
#include <TofwHit.h>
#include <TofwCalib.h>
#include "PHIODataNode.h"
#include "PHEmbedStat.h"

#include "Fun4AllReturnCodes.h"
#include <RunHeader.h>
#include "getClass.h"

#include <iostream>

using namespace std;

TofwMixer::TofwMixer()
{
  verbose = 0;
  d_calib  = 0;
}

TofwMixer::~TofwMixer()
{
  if(d_calib)
    {
      delete d_calib;
    }
}
/*
TofwMixer::Init(PHCompositeNode* real)
{
  d_calib = new TofwCalib();
}
*/
int TofwMixer::InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged){
  if((!sngl)||(!real)||(!merged)){
    cout<< PHWHERE << " one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  node1  = sngl;
  node2  = real;
  node3  = merged;

  //initialize the recalibration parameters
  DeltaTRun=0;
  for(int istrip=0; istrip<TOFW_NSTRIP_TOTAL; istrip++)
    {
      DeltaT[istrip] = 0;
      Slewing_A[istrip] = 0;
      Slewing_B[istrip] = 0;
    } 
  
  RunHeader *d_runhdr = findNode::getClass<RunHeader>(node2, "RunHeader");
  
  if (!d_runhdr)
    {
      cout << PHWHERE << "TofwMixer:: runhdr not in Node Tree" << endl;
      cout << PHWHERE << "You get zeroes for your Tofw Recalibrations" << endl;
      return -1;
    }

  int runnumber = d_runhdr->get_RunNumber();

  d_calib = new TofwCalib();
  if(runnumber>= 228042 && runnumber <= 240121)//Run7
    {
      if(d_calib->fetchrunoff(runnumber)){
	DeltaTRun=d_calib->get_runoffset();
	cout<<"recalibration for tofw: "<<DeltaTRun<<endl;
      }

      if(d_calib->fetchstripoff(runnumber))
	{
	  for(int istrip=0; istrip<TOFW_NSTRIP_TOTAL; istrip++)
	    {
	      DeltaT[istrip] = d_calib->get_DeltaT(istrip);
	      Slewing_A[istrip] = d_calib->get_Slewing_A(istrip);
	      Slewing_B[istrip] = d_calib->get_Slewing_B(istrip);
	    }  
	}
    }
  
  return EVENT_OK;
}

// merge Tofw hits 
int TofwMixer::merge(){
  cout<<"TOFW Mixer"<<endl;
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
  
  PHTypedNodeIterator<TofwHit> tofwout1(node1);
  PHTypedNodeIterator<TofwHit> tofwout2(node2);
  PHTypedNodeIterator<TofwHit> tofwout3(node3);

  PHIODataNode<TofwHit> *tofwnode = tofwout1.find("TofwHit");
  TofwHit* TOFWREC1 = tofwnode->getData();
  if(!TOFWREC1){
    cout << PHWHERE << "MC Data does not have TofwHit Node" << endl;
    return  EVENT_OK;
  }

  tofwnode        = tofwout2.find("TofwHit");
  TofwHit* TOFWREC2 = tofwnode->getData();
  if(!TOFWREC2){
    cout << PHWHERE << "Real Data does not have TofwHit Node" << endl;
    return  EVENT_OK;
  }

  tofwnode        = tofwout3.find("TofwHit");
  TofwHit *TOFWREC3 = tofwnode->getData(); 
  if(!TOFWREC3){
    cout << PHWHERE << "Merge Data does not have TofwHit Node" << endl;
    return  EVENT_OK;
  }

  //debug it tomorrow  
  vector<int>& tofwhitE    = embedStat->get_tofwhitEmbed();
  vector<int>& tofwhitStat = embedStat->get_tofwhitEmbedStat();

  tofwhitE.clear();          
  tofwhitStat.clear();

  if(TOFWREC1){
    int nentries = TOFWREC1->get_nhit();
    tofwhitE.resize(nentries,-1);                 
    tofwhitStat.resize(nentries,-1);
  }
  
  int numOfTofw1 = TOFWREC1->get_nhit();
  int numOfTofw2 = TOFWREC2->get_nhit();

  //COPY TOFWREC2 to TOFWREC3
  TOFWREC3->set_TClonesArraySize(numOfTofw1+numOfTofw2);
  
  for(int ihit=0;ihit<numOfTofw2;ihit++){
    int boxid = TOFWREC2->get_boxid(ihit);//local box 0-3
    int chamberid = TOFWREC2->get_chamberid(ihit);//local chamber 0-31
    int nstripgood = TOFWREC2->get_nstrip(ihit);
    int max_strip = TOFWREC2->get_max(ihit);

    TOFWREC3->AddHit(ihit);
    TOFWREC3->set_boxid(ihit, boxid);
    TOFWREC3->set_chamberid(ihit, chamberid);	
    TOFWREC3->set_nstrip(ihit,nstripgood);
    TOFWREC3->set_max(ihit, max_strip);

    for(int istripgood=0; istripgood<nstripgood; istripgood++){
      float x = TOFWREC2->get_xyz(ihit,istripgood,0);
      float y = TOFWREC2->get_xyz(ihit,istripgood,1);
      float z = TOFWREC2->get_xyz(ihit,istripgood,2);
      float A0 = TOFWREC2->get_rawadc(ihit,istripgood,0);
      float A1 = TOFWREC2->get_rawadc(ihit,istripgood,1);
      float T0 = TOFWREC2->get_rawtdc(ihit,istripgood,0);
      float T1 = TOFWREC2->get_rawtdc(ihit,istripgood,1);
   
      int stripid = TOFWREC2->get_stripid(ihit,istripgood);//local strip index 0-3
      int strip = boxid*32*4+chamberid*4+stripid; 

      float qtofw  = 0.5*(A0+A1);
      float SlewingT = CalSlewing(strip,qtofw);
      T0 = T0-(DeltaTRun + 25.0 + DeltaT[strip]+SlewingT);
      T1 = T1-(DeltaTRun + 25.0 + DeltaT[strip]+SlewingT);

      //copy
      TOFWREC3->set_stripid(ihit, istripgood, stripid);
      
      TOFWREC3->set_xyz(ihit, istripgood, 0, x);
      TOFWREC3->set_xyz(ihit, istripgood, 1, y);
      TOFWREC3->set_xyz(ihit, istripgood, 2, z);
      TOFWREC3->set_rawadc(ihit, istripgood, 0, A0);
      TOFWREC3->set_rawadc(ihit, istripgood, 1, A1);
      TOFWREC3->set_rawtdc(ihit, istripgood, 0, T0);
      TOFWREC3->set_rawtdc(ihit, istripgood, 1, T1);

      TOFWREC3->set_time(ihit, istripgood, 0.5*(T0+T1));
      TOFWREC3->set_charge(ihit, istripgood, 0.5*(A0+A1));
    }
  }
  //finish copying
  
  //Do the embedding work!!!!
  
  int total = numOfTofw2;

  for(int ihit=0;ihit<numOfTofw1;ihit++){
    int mergedid =-1;
    int overlap = 0;
    int mergedd  = 0;
    int mergedchamberid  = 0;

    int iboxid = TOFWREC1->get_boxid(ihit);//local box 0-3
    int ichamberid = TOFWREC1->get_chamberid(ihit);//local chamber 0-31
    int instripgood = TOFWREC1->get_nstrip(ihit);
    //int imax_strip = TOFWREC1->get_max(ihit);

    for(int jhit =0;jhit<numOfTofw2;jhit++){
      int jboxid = TOFWREC3->get_boxid(jhit);//local box 0-3
      int jchamberid = TOFWREC3->get_chamberid(jhit);//local chamber 0-31
      int jnstripgood = TOFWREC3->get_nstrip(jhit);
      //int jmax_strip = TOFWREC3->get_max(jhit);

      if(iboxid == jboxid && ichamberid == jchamberid){ //overlapped
	mergedchamberid   = iboxid*32 + ichamberid;
	
	if( verbose > 3 ){
          cout<<"TOFW hits are overlapped in one chamber: "<<mergedchamberid<<endl;
	}
	
	int jnstripgood_total = jnstripgood;
	for(int istripgood=0; istripgood<instripgood; istripgood++){
	  bool strip_overlap = false;

	  float ix = TOFWREC1->get_xyz(ihit,istripgood,0);
	  float iy = TOFWREC1->get_xyz(ihit,istripgood,1);
	  float iz = TOFWREC1->get_xyz(ihit,istripgood,2);
	  float iA0 = TOFWREC1->get_rawadc(ihit,istripgood,0);
	  float iA1 = TOFWREC1->get_rawadc(ihit,istripgood,1);
	  float iT0 = TOFWREC1->get_rawtdc(ihit,istripgood,0);
	  float iT1 = TOFWREC1->get_rawtdc(ihit,istripgood,1);
	  int istripid = TOFWREC1->get_stripid(ihit,istripgood);//local strip index 0-3

	  //adjust timing
	  iT0 = iT0 - 1.118;//subtract propogation time 1.118ns
	  iT1 = iT1 - 1.118;

	  //adjust adc
	  //................
	  iA0 = iA0 * 80;//need to change random number from real data
	  iA1 = iA1 * 80;//

	  for(int jstripgood=0; jstripgood<jnstripgood; jstripgood++){
	    float jx = TOFWREC3->get_xyz(jhit,jstripgood,0);
	    float jy = TOFWREC3->get_xyz(jhit,jstripgood,1);
	    float jz = TOFWREC3->get_xyz(jhit,jstripgood,2);
	    float jA0 = TOFWREC3->get_rawadc(jhit,jstripgood,0);
	    float jA1 = TOFWREC3->get_rawadc(jhit,jstripgood,1);
	    float jT0 = TOFWREC3->get_rawtdc(jhit,jstripgood,0);
	    float jT1 = TOFWREC3->get_rawtdc(jhit,jstripgood,1);
	    int jstripid = TOFWREC3->get_stripid(jhit,jstripgood);//local strip index 0-3

	    if(istripid==jstripid){
	      strip_overlap=true;
	      //calculate the ix, iy iz later
	      
	      TOFWREC3->set_rawadc(jhit, jstripgood, 0, iA0+jA0);
	      TOFWREC3->set_rawadc(jhit, jstripgood, 1, iA1+jA1);
	      TOFWREC3->set_charge(jhit, jstripgood, 0.5*(iA0+iA1+jA0+jA1));

	      //calculate the slewing shift
	      int strip = TOFWREC3->get_stripid(jhit);

	      float slewing_shift_0= CalSlewing(strip,iA0+jA0)-CalSlewing(strip,jA0);
	      float slewing_shift_1= CalSlewing(strip,iA0+jA1)-CalSlewing(strip,jA1);

	      float T0new=iT0<jT0?iT0:jT0;
	      float T1new=iT1<jT1?iT1:jT1;
	      T0new = T0new + slewing_shift_0;
	      T1new = T1new + slewing_shift_1;

	      TOFWREC3->set_rawtdc(jhit, jstripgood, 0, T0new);
	      TOFWREC3->set_rawtdc(jhit, jstripgood, 1, T1new);
	      TOFWREC3->set_time(jhit, jstripgood, 0.5*(T0new+T1new));

	      TOFWREC3->set_xyz(jhit, jstripgood, 0, jx);//need be improve
	      TOFWREC3->set_xyz(jhit, jstripgood, 1, jy);//need be improve
	      TOFWREC3->set_xyz(jhit, jstripgood, 2, jz);//need be improve
	      break;
	    }
	  }//for jstripgood
	  if(!strip_overlap){
	    
	    TOFWREC3->set_nstrip(jhit, jnstripgood_total+1);
	    TOFWREC3->set_stripid(jhit, jnstripgood_total, istripid);      
	    TOFWREC3->set_xyz(jhit, jnstripgood_total, 0, ix);
	    TOFWREC3->set_xyz(jhit, jnstripgood_total, 1, iy);
	    TOFWREC3->set_xyz(jhit, jnstripgood_total, 2, iz);
	    TOFWREC3->set_rawadc(jhit, jnstripgood_total, 0, iA0);
	    TOFWREC3->set_rawadc(jhit, jnstripgood_total, 1, iA1);
	    TOFWREC3->set_rawtdc(jhit, jnstripgood_total, 0, iT0);
	    TOFWREC3->set_rawtdc(jhit, jnstripgood_total, 1, iT1);
	    TOFWREC3->set_time(jhit, jnstripgood_total, 0.5*(iT0+iT1));
	    TOFWREC3->set_charge(jhit, jnstripgood_total, 0.5*(iA0+iA1));

	    jnstripgood_total++;
	  }
	}//for istripgood

	float tmp_adc=0;
	for(int jstripgood=0; jstripgood<jnstripgood_total; jstripgood++){
	  float jA0 = TOFWREC3->get_rawadc(jhit,jstripgood,0);
	  float jA1 = TOFWREC3->get_rawadc(jhit,jstripgood,1);
	  if( jA0+jA1>tmp_adc){
	    TOFWREC3->set_max(jhit, jstripgood);
	    tmp_adc = jA0+jA1;
	  }
	}

	overlap = 1;
	mergedd |= 0x01;

	mergedid = jhit;

	break;
      } //overlapped
    } // for(int j =0;j<numOfTofw2;j++)

    if(!overlap){ // no overlap, simply added
      int boxid = TOFWREC1->get_boxid(ihit);//local box 0-3
      int chamberid = TOFWREC1->get_chamberid(ihit);//local chamber 0-31
      int nstripgood = TOFWREC1->get_nstrip(ihit);
      int max_strip = TOFWREC1->get_max(ihit);

      TOFWREC3->AddHit(total);
      TOFWREC3->set_boxid(total, boxid);
      TOFWREC3->set_chamberid(total, chamberid);	
      TOFWREC3->set_nstrip(total,nstripgood);
      TOFWREC3->set_max(total, max_strip);

      for(int istripgood=0; istripgood<nstripgood; istripgood++){
	float x = TOFWREC1->get_xyz(ihit,istripgood,0);
	float y = TOFWREC1->get_xyz(ihit,istripgood,1);
	float z = TOFWREC1->get_xyz(ihit,istripgood,2);
	float A0 = TOFWREC1->get_rawadc(ihit,istripgood,0);
	float A1 = TOFWREC1->get_rawadc(ihit,istripgood,1);
	float T0 = TOFWREC1->get_rawtdc(ihit,istripgood,0);
	float T1 = TOFWREC1->get_rawtdc(ihit,istripgood,1);
	int stripid = TOFWREC1->get_stripid(ihit,istripgood);//local strip index 0-3
	
	T0 = T0 - 1.118;
	T1 = T1 - 1.118;
	A0 = A0 * 80;
	A1 = A1 * 80;
	//copy
	TOFWREC3->set_stripid(total, istripgood, stripid);
	TOFWREC3->set_xyz(total, istripgood, 0, x);
	TOFWREC3->set_xyz(total, istripgood, 1, y);
	TOFWREC3->set_xyz(total, istripgood, 2, z);
	TOFWREC3->set_rawadc(total, istripgood, 0, A0);
	TOFWREC3->set_rawadc(total, istripgood, 1, A1);
	TOFWREC3->set_rawtdc(total, istripgood, 0, T0);
	TOFWREC3->set_rawtdc(total, istripgood, 1, T1);
	TOFWREC3->set_time(total, istripgood, 0.5*(T0+T1));
	TOFWREC3->set_charge(total, istripgood, 0.5*(A0+A1));
      }
      
      mergedid          = total;
      total++;
    }

    tofwhitE[ihit]    = mergedid;
    tofwhitStat[ihit] = mergedd; 
  }
  

  TOFWREC3->set_nhit(total);
  if(verbose>0) {
    numOfTofw1 = TOFWREC1->get_nhit();
    numOfTofw2 = TOFWREC2->get_nhit();
    
    //cout<<"*********************************************"<<endl;
    //cout<<numOfTofw1<<" "<<numOfTofw2<<" "<<numOfTofw3<<endl;
  }

  return EVENT_OK;
}

float TofwMixer::CalSlewing(int strip, float adc)
{
  //TF1 *fun_slewing =new TF1("fun_slewing","[0]+[1]/x^0.4",0,2000);
  //fun_slewing->SetParameter(0,Slewing_A[strip]);
  //fun_slewing->SetParameter(1,Slewing_B[strip]);
  //float offset = fun_slewing->Eval(adc);
  //fun_slewing->Delete();
  float par = 0.4;
  float offset = Slewing_A[strip]+Slewing_B[strip]/pow(adc,par);
  return offset;  
}
