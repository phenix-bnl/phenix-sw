#include "SvxRpSumXYReco.h"
#include "RpConst.h"
#include "RpSumXYObjectv3.h"

#include <BbcRaw.h>
#include <BbcOut.h>
#include <BbcCalib.hh>
#include <BbcGeo.hh>
#include <BbcEvent.hh>

#include <DchTrack.h>

#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <MpcMap.h>

#include <PHGlobal.h>
#include <PHCentralTrack.h>

#include <SmdOut.h>

#include <SvxCluster.h>
#include <SvxClusterList.h>
#include <SvxSegment.h>
#include <SvxSegmentList.h>

//FVTX
//#include <MUTOO.h>
#include <FVTXOO.h>
#include <TFvtxCoordMap.h>
#include <TFvtxCoord.h>

#include <VtxOut.h>

#include <Fun4AllReturnCodes.h>
#include <phool.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <getClass.h>
#include <recoConsts.h>

#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>

using namespace std;
using namespace findNode;

//SvxRpSumXYReco::SvxRpSumXYReco(const string &name, int flag) : 
SvxRpSumXYReco::SvxRpSumXYReco(const string &name, RpRecoConst::RPRECO flag) : 
  SubsysReco(name),
  m_rpsumxy(NULL),
  m_eventNumber(0),
  calflag(flag),
  m_vertexflag(0)
{
  m_bbccalib = new BbcCalib();
  m_bbcgeo   = new BbcGeo();

  if(verbosity>0)
    {
      cout << PHWHERE << " calflag : " << calflag << " " << name << endl;
    }
}

SvxRpSumXYReco::~SvxRpSumXYReco()
{
  delete m_bbccalib;
  delete m_bbcgeo;
}


int SvxRpSumXYReco::End(PHCompositeNode* topNode)
{
  return EVENT_OK;
}

int SvxRpSumXYReco::InitRun(PHCompositeNode* topNode)
{
  recoConsts *rc = recoConsts::instance();
  int runnumber  = rc->get_IntFlag("RUNNUMBER");
  int icalibversion = 4002; // after Run5 and Field_ON

  cout << "SvxRpSumXYReco::InitRun - run number: " << runnumber << endl;
  cout << "SvxRpSumXYReco::InitRun - calibration version:  " << icalibversion << endl;
  rc->set_IntFlag("BBCCALIBVERSION", icalibversion);

  if(m_bbccalib){
    PHTimeStamp TimeStp = rc->get_TimeStamp();
    int BBCCALIBVERSION = rc->get_IntFlag("BBCCALIBVERSION");
    cout << "SvxRpSumXYReco::InitRun - restored constants are for " << TimeStp << endl;
    m_bbccalib->restore(TimeStp, BBCCALIBVERSION);
  }
  
  int i = CreateNodeTree(topNode);
  if(verbosity>0) cout << "SvxRpSumXYReco::InitRun() CreateNodeTree() returned " << i << endl;
  if(!(i==EVENT_OK)) {return ABORTEVENT;}

  return EVENT_OK;
}

int SvxRpSumXYReco::process_event(PHCompositeNode* topNode)
{
  if(verbosity>0)
    cout<<"SvxRpSumXYReco::process_event start"<<endl;

  // find SvxCluster
  SvxClusterList *svxclslist = getClass<SvxClusterList>(topNode, "SvxClusterList");
  if(svxclslist==NULL)
    {
      if (verbosity > 0) cout<<"SvxRpSumXYReco::process_event : not SvxClusterList"<<endl;
    }
  
  // find SvxSegment
  SvxSegmentList *svxseglist = getClass<SvxSegmentList>(topNode, "SvxSegmentList");
  if (svxseglist == NULL)
    {
      if (verbosity > 0) cout<<"SvxRpSumXYReco::process_event : not SvxSegmentList"<<endl;
    }

//##################################################################//
// write your code and/or functions to calcualate the reaction plane

  //////////////////////////////////
  // get Z-vertex
  float vertex = getZVertex(topNode);

  if(verbosity>0)
    {
      if(m_eventNumber%1000==0) 
        cout<<"SvxRpSumXYReco::"<<__FUNCTION__<<" event processed "<<m_eventNumber<< "//------------------------------"<< endl;
    }
  m_eventNumber++;

  
  /////////////////////////////
  //SVX Cluster
  static const int NRPSVX=53;
  float Qsvx[NRPSVX][nhar][3];
  memset(Qsvx, 0, sizeof(Qsvx));
  if(svxclslist){
    static const float gap_cls[13]  = {-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0};
    static const int   ngap_start[4]= { 0, 1, 2, 3};
    static const int   ngap_end[4]  = {12,11,10, 9};

    int nclus = svxclslist->get_nClusters();
    for(int j=0; j<nclus; j++){ // use all the clusters including good only and hot/cold cluster. no size cut is applied
      SvxCluster *cls = svxclslist -> get_Cluster(j);
      int   ilayer = cls->get_layer();
      float vtx_x  = cls->get_xyz_global(0);
      float vtx_y  = cls->get_xyz_global(1);
      float vtx_z  = cls->get_xyz_global(2);

      // select good_cluster
      bool is_noise = (fabs(vtx_x)>50.0 || fabs(vtx_y)>50.0 || fabs(vtx_z)>50.0);
      if(is_noise) {
        cout<<"bad value : ";
        cout<<((is_noise) ? "noise : " : "");
        cout<<vtx_x<<" "<<vtx_y<<" "<<vtx_z<<endl;

        continue;
      }

      float vtx_r   = sqrt(pow(vtx_x, 2.0) + pow(vtx_y, 2.0));
      float vtx_the = (vertex>-9000) ? atan2(vtx_r, vtx_z-vertex) : -9999.0;
      float vtx_eta = (vertex>-9000) ? -log(tan(0.5*vtx_the))     : -9999.0;
      float vtx_phi = atan2(vtx_y, vtx_x);

      /////////////////
      // calculate eta bin
      int ieta     = -1;
      int ieta_lay = -1;
      if(vtx_eta>-9000)
        {
          for(ieta=0; ieta<12; ieta++){
            if(ieta==0) 
              {
                if(vtx_eta<=gap_cls[ieta+1])               { break; }
              }
            else if(ieta==11)
              {
                if(gap_cls[ieta]<vtx_eta )                 { break; }
              }
            else 
              {
                if(gap_cls[ieta]<vtx_eta && 
                                 vtx_eta<=gap_cls[ieta+1]) { break; }
              }
          }
          if(ieta!=-1) {
            int ietabin = ieta;
            if(      ieta<ngap_start[ilayer] ){ ietabin = ngap_start[ilayer]; }
            else if( ngap_end[ilayer]<ieta )  { ietabin = ngap_end[ilayer];   }

            static const int start_eta[4]={0,11,20,27};
            ieta_lay = start_eta[ilayer]+ietabin ;

            //cout << "debug : "<< ilayer<<" "<<vtx_eta<<" : "<<ieta<<" "<<ietabin<<" "<<ieta_lay<<"  ";
            //cout << (ieta!=ietabin?"diff":"same")<<endl;
          } 
       }

      /////////////////
      // eta bin (0-11, 12-21, 22-29, 30-35, 36-47)= (B0,B1,B2,B3,Ball)
      // no eta bin (48, 49, 50, 51, 52) = (B0, B1, B2, B3, Ball)
      for(int ihar=0;ihar<nhar;ihar++){
        float qx = cos(vtx_phi*(ihar+1.0));
        float qy = sin(vtx_phi*(ihar+1.0)); 

        // layer by layer
        Qsvx[48+ilayer][ihar][0]+= qx;
        Qsvx[48+ilayer][ihar][1]+= qy;
        Qsvx[48+ilayer][ihar][2]++; 

        // layer by layer combined 
        Qsvx[52][ihar][0]+=qx;      // all layer for all eta bin combined
        Qsvx[52][ihar][1]+=qy;
        Qsvx[52][ihar][2]++;

        // eta bin
        if(ieta!=-1){ // only ieta exist = zvertex successfully calculated
          Qsvx[ieta_lay][ihar][0]+=qx;  // eta bin
          Qsvx[ieta_lay][ihar][1]+=qy;
          Qsvx[ieta_lay][ihar][2]++;
          Qsvx[36+ieta][ihar][0]+=qx;   // all layer for each eta bin
          Qsvx[36+ieta][ihar][1]+=qy;
          Qsvx[36+ieta][ihar][2]++;
        }
      }
    }

  }

  ///////////////////////////////////
  //---vtx-segment---
  static const int NRPSEG=9;
  float Qseg[NRPSEG][nhar][3];
  memset(Qseg, 0, sizeof(Qseg));

  if(svxseglist){
    static const float gap_seg[9]={-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0};

    int nsegm = svxseglist->get_nSegments();
    for(int iseg=0;iseg<nsegm;iseg++){
      SvxSegment *seg=svxseglist->get_segment(iseg);
      float vtx_px = seg->get3MomentumAtPrimaryVertex(0);
      float vtx_py = seg->get3MomentumAtPrimaryVertex(1);
      float vtx_pz = seg->get3MomentumAtPrimaryVertex(2);
      
      float vtx_phi = atan2(vtx_py,vtx_px);
      float vtx_pt  = sqrt(pow(vtx_py,2.0)+pow(vtx_px,2.0));
      float vtx_the = atan2(vtx_pt,vtx_pz);
      float vtx_eta = -log(tan(0.5*vtx_the));

      int ieta=-1;
      for(int i=0;i<8;i++){
	if(gap_seg[i]<vtx_eta && vtx_eta<=gap_seg[i+1]) {
          ieta=i;
          break;
        }
      }
      if(ieta==-1) continue;

      for(int ihar=0;ihar<nhar;ihar++){
	float qx=cos(vtx_phi*(ihar+1.0));
	float qy=sin(vtx_phi*(ihar+1.0));
	Qseg[ieta][ihar][0]+=qx;
	Qseg[ieta][ihar][1]+=qy;
	Qseg[ieta][ihar][2]++;
	Qseg[8][ihar][0]+=qx;
	Qseg[8][ihar][1]+=qy;
	Qseg[8][ihar][2]++;
      }
    }
  }

  ///////////////////////////////////
  //mpc
  ///////
  float Qmpc[3][nhar][3];
  memset(Qmpc, 0, sizeof(Qmpc));

  mpcTowerContainer *mpctower=findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if(mpctower){
     MpcMap *mpcmap = findNode::getClass<MpcMap>(topNode, "MpcMap");
     if (!mpcmap)
       {
	 cout << PHWHERE << " Could not locate MpcMap on node tree" << endl;
	 exit(1);
       }
    recoConsts *rc = recoConsts::instance();
    int runnumber = rc->get_IntFlag("RUNNUMBER");

    int ntowers=mpctower->size();
    for(int itower=0;itower<ntowers;itower++){
      mpcTowerContent *tower = mpctower->getTower(itower);
      int   fee_ch  = tower->get_ch();
      float mpc_e   = tower->get_energy();
      float mpc_tof = tower->get_tof();
      float mpc_x   = mpcmap->getX(fee_ch);
      float mpc_y   = mpcmap->getY(fee_ch);
      float mpc_z   = mpcmap->getZ(fee_ch);
      float mpc_phi = atan2(mpc_y,mpc_x);
      //if(mpc_e>0) mpctof->Fill(fee_ch,mpc_tof);
      
      if ( runnumber < 369000 )	// Before Run12 U+U
        {
          if(mpc_e<0 || mpc_tof<500 || mpc_tof>2200) continue;
        }
      else
        {
          if(mpc_e<0 || mpc_tof<-10. || mpc_tof>50.) continue;
        }

      if(fabs(mpc_z)<1.) continue;
      float mpc_r   = sqrt(pow(mpc_y,2.0)+pow(mpc_x,2.0));
      float mpc_the = atan2(mpc_r,mpc_z-vertex);
      //float mpc_eta=-log(tan(0.5*mpc_the));

      //int   idet = (mpc_z>0) ? 0 : 1; // 0:north, 1:south
      int   idet = (mpc_z<0) ? 0 : 1; // 0:south, 1:north
      float val  = mpc_e*sin(mpc_the);
      
      //float ccc=0.0;
      //if(fee_ch>-1 || fee_ch<600) ccc=mgain[fee_ch];
      //if(ccc>0.0) ccc=1.0/ccc;
      //else ccc=0.0;
      //if(calFlag>0.0){mpcgain->Fill(fee_ch+0.0,val); mpcma_p->Fill(fee_ch+0.0,val);}
      //val=val*ccc;
      //if(calFlag>0.0){mpcgain->Fill(fee_ch+600.0,val); mpcma_p->Fill(fee_ch+600.0,val);}
     
      for(int ihar=0;ihar<nhar;++ihar){
	float qx=cos(mpc_phi*(ihar+1.0));
	float qy=sin(mpc_phi*(ihar+1.0));

        Qmpc[idet][ihar][0]+=val*qx;
	Qmpc[idet][ihar][1]+=val*qy;
        Qmpc[idet][ihar][2]+=val;
        Qmpc[2][ihar][0]+=val*qx;
        Qmpc[2][ihar][1]+=val*qy;
        Qmpc[2][ihar][2]+=val;
      }
    }
  }

  ////////////////////////////////////////////
  //bbc
  ////////////
  float Qbbc[3][nhar][3];
  memset(Qbbc, 0, sizeof(Qbbc));
  BbcRaw *bbcraw=findNode::getClass<BbcRaw>(topNode,"BbcRaw");
  if(!bbcraw){
    cout << "could not find Bbcraw!" << endl;
  }
  if(bbcraw){
    for(int ipmt=0;ipmt<128;++ipmt){
      short adc    = bbcraw->get_Adc(ipmt);
      short tdc    = bbcraw->get_Tdc0(ipmt);
      //float tdc1=bbcraw->get_Tdc1(ipmt);
      float time0  = m_bbccalib->getHitTime0(ipmt,tdc,adc);
      float charge = m_bbccalib->getCharge(ipmt,adc);
      float bbc_x  = m_bbcgeo->getX(ipmt);
      float bbc_y  = m_bbcgeo->getY(ipmt);
      float bbc_z  = m_bbcgeo->getZ(ipmt);

      if(time0>0 && charge>0){
	float bbc_phi= atan2(bbc_y, bbc_x);
	float val    = charge;

	int idet= (bbc_z<0) ? 0 : 1;// 0:south, 1:north

	for(int ihar=0;ihar<nhar;++ihar){
	  float qx=cos(bbc_phi*(ihar+1.0));
	  float qy=sin(bbc_phi*(ihar+1.0));
	  Qbbc[idet][ihar][0]+=val*qx;
	  Qbbc[idet][ihar][1]+=val*qy;
	  Qbbc[idet][ihar][2]+=val;
          Qbbc[2][ihar][0]   +=val*qx;
          Qbbc[2][ihar][1]   +=val*qy;
          Qbbc[2][ihar][2]   +=val;
	}
      }
    }
  }

  ////SMD////
  static float Qsmd[3][1][3];
  memset(Qsmd, 0, sizeof(Qsmd));
  SmdOut* smdout = findNode::getClass<SmdOut>(topNode, "SmdOut");
  if ( smdout )
    {
      for (int iarm = 0; iarm < 2; iarm++)
	{
	  if (isfinite(smdout->get_Xpos(iarm)) && 
              isfinite(smdout->get_Ypos(iarm)) && 
              fabs(smdout->get_Xpos(iarm)) < 9999 && 
	      fabs(smdout->get_Ypos(iarm)) < 9999)
	    {
	      Qsmd[iarm][0][0] = smdout->get_Xpos(iarm);
	      Qsmd[iarm][0][1] = smdout->get_Ypos(iarm);
	      Qsmd[iarm][0][2] = 1;
	    }
	}
      // SMD south + north
      if (Qsmd[0][0][0] > -9999 && Qsmd[0][0][1] > -9999 && 
          Qsmd[1][0][0] > -9999 && Qsmd[1][0][1] > -9999 && 
          Qsmd[0][0][2] > 0 && Qsmd[1][0][2] > 0)
	{
	  Qsmd[2][0][0] = Qsmd[0][0][0] - Qsmd[1][0][0];
	  Qsmd[2][0][1] = Qsmd[0][0][1] - Qsmd[1][0][1];
	  Qsmd[2][0][2] = 1.0;
	}
    }
  else
    {
      PHGlobal* phg = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
      if (fabs(phg->get_SmdXS()) < 9999 && fabs(phg->get_SmdYS()) < 9999)
	{
	  // SMD south or north
	  Qsmd[0][0][0] = phg->get_SmdXS();
	  Qsmd[0][0][1] = phg->get_SmdYS();
	  Qsmd[0][0][2] = 1.0;
	}
      if (fabs(phg->get_SmdXN()) < 9999 && fabs(phg->get_SmdYN()) < 9999)
	{
	  // SMD south or north
	  Qsmd[1][0][0] = phg->get_SmdXN();
	  Qsmd[1][0][1] = phg->get_SmdYN();
	  Qsmd[1][0][2] = 1.0;
	}
      // SMD south + north
      if (Qsmd[0][0][0] > -9999 && Qsmd[0][0][1] > -9999 && Qsmd[1][0][0] > -9999 && Qsmd[1][0][1] > -9999)
	{
	  Qsmd[2][0][0] = Qsmd[0][0][0] - Qsmd[1][0][0];
	  Qsmd[2][0][1] = Qsmd[0][0][1] - Qsmd[1][0][1];
	  Qsmd[2][0][2] = 1.0;
        }
    }

  /////Central_track
  static float Qcnt[5][nhar][3];
  memset(Qcnt,0,sizeof(Qcnt));
  PHCentralTrack* central = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if ( central )
    {
      for (unsigned int itrk = 0; itrk < central->get_npart(); itrk++)
	{
	  short quality  = central->get_quality(itrk);
	  //float pc3dphi = central->get_pc3dphi(itrk);
	  //float pc3sdphi = pc3dphi/0.001; // around 1 sigma for dphi
	  //float pc3dz   = central->get_pc3dz(itrk);
	  //float pc3sdz = pc3dz/0.7; // around 1 sigma for dz
	  float mom      = central->get_mom(itrk);
	  float the0     = central->get_the0(itrk);
	  float zed      = central->get_zed(itrk);
	  float phi0     = central->get_phi0(itrk);

	  // Good track selection
	  if ((quality == 31 || quality == 63)
	      //&& sqrt(pc3sdphi*pc3sdphi+pc3sdz*pc3sdz)<3.0
	      && the0 > -100
	      && fabs(zed) < 100
	      && phi0 > -100
	      && mom != 0.0 && mom < 20.0)
	    {

	      float phi = atan2(sin(phi0), cos(phi0));
	      float pt   = mom * sin(the0);
	      float weight = pt;
	      if (pt < 0.0 || pt > 2.0) weight = 0.0;
	      //float eta  = -log(tan(the0/2.0));
	      //float sign = (eta<0.0) ? 1.0 : -1.0;

	      // sub event definition from zed
	      int ized = (int)( (zed + 80.0) / 160.0 * 20 );
	      int icnt = ized % 4;
	      if (icnt < 0) icnt = 0;
	      if (icnt > 3) icnt = 3;

	      // fill Flow Vector for each event plane
	      // idet : the detector id
	      // phi  : azimuthal angle of particle or PMT.
	      // weight : appropriate weight (pT, multiplicity etc)
	      for (int ihar = 0; ihar < nhar; ihar++)
		{
		  float w = weight;
		  //if (ihar % 2 == 0) w *= sign;//for non-flipping v3
		  Qcnt[icnt][ihar][0] += w * cos( (ihar + 1.0) * phi );
		  Qcnt[icnt][ihar][1] += w * sin( (ihar + 1.0) * phi );
		  Qcnt[icnt][ihar][2] += weight;

		  Qcnt[4][ihar][0] += w * cos( (ihar + 1.0) * phi );
		  Qcnt[4][ihar][1] += w * sin( (ihar + 1.0) * phi );
		  Qcnt[4][ihar][2] += weight;
		}
	    }
	}
    }
  // use Dch Track if central track is not around 
  // central track is typically build during dst analysis, not during production
  else
    {
      DchTrack* dchtrk = findNode::getClass<DchTrack>(topNode, "DchTrack");
      if ( dchtrk )
	{
	  for (unsigned int itrk = 0; itrk < dchtrk->get_DchNTrack(); itrk++)
	    {
	      short quality  = dchtrk->get_quality(itrk);
	      //float pc3dphi = dchtrk->get_pc3dphi(itrk);
	      //float pc3sdphi = pc3dphi/0.001; // around 1 sigma for dphi
	      //float pc3dz   = dchtrk->get_pc3dz(itrk);
	      //float pc3sdz = pc3dz/0.7; // around 1 sigma for dz
	      float mom      = dchtrk->get_momentum(itrk);
	      float the0     = dchtrk->get_theta0(itrk);
	      float zed      = dchtrk->get_zed(itrk);
	      float phi0     = dchtrk->get_phi0(itrk);

	      // Good track selection
	      if ((quality == 31 || quality == 63)
		  //&& sqrt(pc3sdphi*pc3sdphi+pc3sdz*pc3sdz)<3.0
		  && the0 > -100
		  && fabs(zed) < 100
		  && phi0 > -100
		  && mom != 0.0 && mom < 20.0)
		{

		  float phi = atan2(sin(phi0), cos(phi0));
		  float pt   = mom * sin(the0);
		  float weight = pt;
		  if (pt < 0.0 || pt > 2.0) weight = 0.0;
		  //float eta  = -log(tan(the0/2.0));
		  //float sign = (eta<0.0) ? 1.0 : -1.0;

		  // sub event definition from zed
		  int ized = (int)( (zed + 80.0) / 160.0 * 20 );
		  int icnt = ized % 4;
		  if (icnt < 0) icnt = 0;
		  if (icnt > 3) icnt = 3;

		  // fill Flow Vector for each event plane
		  // idet : the detector id
		  // phi  : azimuthal angle of particle or PMT.
		  // weight : appropriate weight (pT, multiplicity etc)
		  for (int ihar = 0; ihar < nhar; ihar++)
		    {
		      float w = weight;
		      //if (ihar % 2 == 0) w *= sign;//for non-flipping v3
		      Qcnt[icnt][ihar][0] += w * cos( (ihar + 1.0) * phi );
		      Qcnt[icnt][ihar][1] += w * sin( (ihar + 1.0) * phi );
		      Qcnt[icnt][ihar][2] += weight;

		      Qcnt[4][ihar][0] += w * cos( (ihar + 1.0) * phi );
		      Qcnt[4][ihar][1] += w * sin( (ihar + 1.0) * phi );
		      Qcnt[4][ihar][2] += weight;
		    }
		}
	    }
	}
    }
  
  
  /////FVTX//////
  static float Qfvt[nfvt][nhar][3];
  memset(Qfvt, 0, sizeof(Qfvt));

  //cout<<"start of FVTX RP code"<<endl;
  
  PHNodeIterator node_iter(topNode);
  PHCompositeNode *_fvtxoo_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode","FVTXOO"));
  if(_fvtxoo_node)
    {
      TFvtxCoordMap* _coord_map = TMutNode<TFvtxCoordMap>::find_node(_fvtxoo_node, "TFvtxCoordMap" );
      
      if (_coord_map)
	{
	  for (int iarm = 0; iarm < FVTXOO::MAX_ARM ; iarm++)//0:south; 1:north
	    {
	      for (int istation = 0; istation < FVTXOO::MAX_STATION; istation++)//station 4
		{
		  for (int icage = 0; icage < FVTXOO::MAX_CAGE ; icage++)//0:west; 1:east
		    {
		      for (int isector = 0; isector < FVTXOO::MAX_SECTOR ; isector++)//sector:24 sectors in two faces
			{
			  
			  TFvtxCoordMap::iterator coord_iter = _coord_map->get(iarm, icage, istation, isector);
			  
			  while (TFvtxCoordMap::pointer coord_ptr = coord_iter.next())
			    {
			      PHPoint CoordMidPoint = coord_ptr->get()->get_coord_midpoint();
			      double fvtx_x = CoordMidPoint.getX();
			      double fvtx_y = CoordMidPoint.getY();
			      double fvtx_z = CoordMidPoint.getZ();
			      double fvtx_r = sqrt(pow(fvtx_x,2.0)+pow(fvtx_y,2.0));
			      
			      if( (fabs(fvtx_x)>999.0) ||(fabs(fvtx_y)>999.0) || (fabs(fvtx_z)>999.0)) continue;
			      double fvtx_the = atan2(fvtx_r,fvtx_z-vertex); //fvtx_z-bbcv
			      double fvtx_phi = atan2(fvtx_y,fvtx_x);
			      double fvtx_eta = -log(tan(0.5*fvtx_the));

			      if(fabs(fvtx_eta)>1.0 && fabs(fvtx_eta)<3.5){
				int igap = (fabs(fvtx_eta)-1.0)/0.5;

				int id_fvtx = iarm*20+istation*5+igap;

				if(id_fvtx>=0 && id_fvtx<40){
				  for (int ihar = 0; ihar < nhar; ihar++)
				    {
				      Qfvt[id_fvtx][ihar][0] += cos((ihar+1)*fvtx_phi);
				      Qfvt[id_fvtx][ihar][1] += sin((ihar+1)*fvtx_phi);
				      Qfvt[id_fvtx][ihar][2] ++;
				    }//ihar
				}//id_fvtx
			      }//eta>1.0
			    }//while
			  
			}//isector
		    }//icage
		}//istattion 
	    }//iarm
	}
    }
  
  char name[20];
 // SVX Cluster
 for(int idet=0;idet<NRPSVX;idet++){
   for(int ihar=0;ihar<nhar;ihar++){
     if(Qsvx[idet][ihar][2]>0){
       int id_det = idet;
       sprintf(name,"SVXsum%02d%d", id_det, ihar);
       int rpid = RP::calcIdCode(RP::ID_SVX, id_det, ihar);
       m_rpsumxy->AddRpSumXY(name, rpid, Qsvx[idet][ihar][0], Qsvx[idet][ihar][1], Qsvx[idet][ihar][2]);
     }
   }
 }

 // VTX Segment
 for(int idet=0;idet<NRPSEG;idet++){
   for(int ihar=0;ihar<nhar;ihar++){
     if(Qseg[idet][ihar][2]>0){
       int id_det = idet; // idet + 53 <- this is bug
       sprintf(name,"SVXsum%02d%d",id_det,ihar);
       //int rpid = RP::calcIdCode(RP::ID_SVX, id_det, ihar);
       int rpid = RP::calcIdCode(RP::ID_SEG, id_det, ihar);
       m_rpsumxy->AddRpSumXY(name, rpid, Qseg[idet][ihar][0], Qseg[idet][ihar][1], Qseg[idet][ihar][2]);
     }
   }
 }

 //MPC
 for(int idet=0;idet<3;idet++){
   for(int ihar=0;ihar<nhar;ihar++){
     sprintf(name,"MPCsum%02d%d",idet,ihar);
     int rpid = RP::calcIdCode(RP::ID_MPC, idet, ihar);
     m_rpsumxy->AddRpSumXY(name, rpid, Qmpc[idet][ihar][0], Qmpc[idet][ihar][1], Qmpc[idet][ihar][2]);
   }
 }
 // BBC
 for(int idet=0;idet<3;idet++){
   for(int ihar=0;ihar<nhar;ihar++){
     sprintf(name,"BBCsum%02d%d",idet,ihar);
     int rpid = RP::calcIdCode(RP::ID_BBC, idet, ihar);   
     m_rpsumxy->AddRpSumXY(name, rpid, Qbbc[idet][ihar][0], Qbbc[idet][ihar][1], Qbbc[idet][ihar][2]);   
   }
 }
 // SMD
 for(int idet=0;idet<3;idet++){
   for(int ihar=0;ihar<1;ihar++){
     sprintf(name,"SMDsum%02d%d",idet,ihar);
     int rpid = RP::calcIdCode(RP::ID_SMD, idet, ihar);
     m_rpsumxy->AddRpSumXY(name, rpid, Qsmd[idet][ihar][0], Qsmd[idet][ihar][1], Qsmd[idet][ihar][2]);
   }
 }
 // CNT
 for(int idet=0;idet<5;idet++){
   for(int ihar=0;ihar<nhar;ihar++){
     sprintf(name,"CNTsum%02d%d",idet,ihar);
     int rpid = RP::calcIdCode(RP::ID_CNT, idet, ihar);
     m_rpsumxy->AddRpSumXY(name, rpid, Qcnt[idet][ihar][0], Qcnt[idet][ihar][1], Qcnt[idet][ihar][2]);
   }
 } 

 //FVTX
 for(int idet=0;idet<nfvt;idet++){
   for(int ihar=0;ihar<nhar;ihar++){
     sprintf(name,"FVTsum%02d%d",idet,ihar);
     //cout<<name<<" "<< RP::calcIdCode(RP::ID_FVT, idet, ihar)<<endl;
     int rpid = RP::calcIdCode(RP::ID_FVT, idet, ihar);
     m_rpsumxy->AddRpSumXY(name, rpid, Qfvt[idet][ihar][0], Qfvt[idet][ihar][1], Qfvt[idet][ihar][2]);
   }
 } 

 // example to put the Qx, Qy, weight for each detector
 //m_rpsumxy->AddRpSumXY("BBCsum00", RP::calcIdCode(RP::ID_BBC, 0, 0),  0.5,  0.5, 1);
 //m_rpsumxy->AddRpSumXY("BBCsum01", RP::calcIdCode(RP::ID_BBC, 1, 0), -0.5, -0.5, 1);
 //######################################################################//
 
  //cout<<"SvxRpSumXYReco::process_event finished"<<endl;

  return EVENT_OK;
}


int SvxRpSumXYReco::CreateNodeTree(PHCompositeNode* topNode)
{

  // Find the DST node so we can put objects there
  PHNodeIterator iter(topNode);

  PHCompositeNode* dstNode = 
                 static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));
  if (!dstNode) { cerr << PHWHERE << "DST node missing, doing nothing." << endl; return ABORTEVENT; }


  // Find/Create SvxRpSumXY node
  char name[80];
  sprintf(name, "RpSumXYObject");
  if      (calflag==RpRecoConst::SVX)  sprintf(name, "SvxRpSumXYObject");
  else if (calflag==RpRecoConst::CNT)  sprintf(name, "CntRpSumXYObject");
  else if (calflag==RpRecoConst::FVTX) sprintf(name, "FvtxRpSumXYObject");
  else if (calflag==RpRecoConst::MUON) sprintf(name, "MuonRpSumXYObject");
  else {
    cout<< " SvxRpSumXYReco:: out of range : flag = "<< calflag <<endl;
    cout<< "    Flag should in 0-4."<<endl;
    cout<< "    Default node name is used"<<endl;
  }
  cout << PHWHERE << "creating node : " << name << endl;

  PHIODataNode<PHObject>* SvxRpSumXYNode 
            = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", name);

  if(SvxRpSumXYNode==NULL){
    m_rpsumxy = new RpSumXYObjectv3();

    SvxRpSumXYNode = new PHIODataNode<PHObject>(m_rpsumxy, name, "PHObject");

    dstNode->addNode(SvxRpSumXYNode);
  }

  return EVENT_OK;
}

float SvxRpSumXYReco::getZVertex(PHCompositeNode *topNode)
{
  float vertex =-9999.;
  VtxOut *vtxout=getClass<VtxOut>(topNode, "VtxOut");
  if(vtxout==NULL) 
    {
      if(m_eventNumber==0||verbosity>0) cout<<"SvxRpSumXYReco::"<<__FUNCTION__<<" No VtxOut Object"<<endl;
      return vertex;
    }

  //////////////////////////////////
  // get Z-vertex
  if(m_vertexflag==1) 
    {
      vertex = vtxout->get_ZVertex("SVX_PRICISE");
      if(verbosity>1) cout<<"SvxRpSumXYReco::"<<__FUNCTION__<<"Use ZVertex SVX_PRICISE "<< vertex <<endl;
    }
  else if(m_vertexflag==2) 
    {
      vertex = vtxout->get_ZVertex("SVX");
      if(verbosity>1) cout<<"SvxRpSumXYReco::"<<__FUNCTION__<<"Use ZVertex SVX "<< vertex <<endl;
    }
  else if(m_vertexflag==3) 
    {
      vertex = vtxout->get_ZVertex("BBC");
      if(verbosity>1) cout<<"SvxRpSumXYReco::"<<__FUNCTION__<<"Use ZVertex BBC "<< vertex <<endl;
    }
  else  // this is default (vertexflag==0)                   
    {
      vertex = vtxout->get_ZVertex();
      if(verbosity>1) cout<<"SvxRpSumXYReco::"<<__FUNCTION__<<"Use ZVertex Default "<< vertex <<endl;
    }

  return vertex;
}
