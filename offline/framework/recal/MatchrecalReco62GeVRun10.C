#include <MatchrecalReco62GeVRun10.h>

#include <getClass.h>
#include <PHGlobal.h>
#include <PHCompositeNode.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <RunHeader.h>

#include <iostream>

using namespace std;

//_____________________________________________________________________________________________________________________________
MatchrecalReco62GeVRun10::MatchrecalReco62GeVRun10(const string &name) : Recalibrator(name)
{
  baseclasses.insert("PHCentralTrackv23");
  baseclasses.insert("PHCentralTrackv24");
  return;
}
int MatchrecalReco62GeVRun10::isValidRun(const int RunNumber) const
{
  if(RunNumber>= 310698 && RunNumber<=313322)return 1;
  else return 0;
}
//_____________________________________________________________________________________________________________________________
MatchrecalReco62GeVRun10::~MatchrecalReco62GeVRun10()
{

}
//_____________________________________________________________________________________________________________________________
int MatchrecalReco62GeVRun10::Init(PHCompositeNode *topNode)
{
  //! Initialize the matching arrays
  for(int ia=0;ia<NARM;ia++){ //! arm
    for(int iz=0;iz<NZBN;iz++){ //! zed
      for(int ic=0;ic<NCH;ic++){ //! charge
	//! sigma
	//! PC2
	p0_pc2_s_dp[iz][ic]=p1_pc2_s_dp[iz][ic]=p2_pc2_s_dp[iz][ic]=p3_pc2_s_dp[iz][ic]=p4_pc2_s_dp[iz][ic]=0;
	p0_pc2_s_dz[iz][ic]=p1_pc2_s_dz[iz][ic]=p2_pc2_s_dz[iz][ic]=p3_pc2_s_dz[iz][ic]=p4_pc2_s_dz[iz][ic]=0;
	
	//! PC3
	p0_pc3_s_dp[ia][iz][ic]=p1_pc3_s_dp[ia][iz][ic]=p2_pc3_s_dp[ia][iz][ic]=p3_pc3_s_dp[ia][iz][ic]=p4_pc3_s_dp[ia][iz][ic]=0;
	p0_pc3_s_dz[ia][iz][ic]=p1_pc3_s_dz[ia][iz][ic]=p2_pc3_s_dz[ia][iz][ic]=p3_pc3_s_dz[ia][iz][ic]=p4_pc3_s_dz[ia][iz][ic]=0;
	
	//! ToF
	p0_tof_s_dp[ia][iz][ic]=p1_tof_s_dp[ia][iz][ic]=p2_tof_s_dp[ia][iz][ic]=p3_tof_s_dp[ia][iz][ic]=p4_tof_s_dp[ia][iz][ic]=0;
	p0_tof_s_dz[ia][iz][ic]=p1_tof_s_dz[ia][iz][ic]=p2_tof_s_dz[ia][iz][ic]=p3_tof_s_dz[ia][iz][ic]=p4_tof_s_dz[ia][iz][ic]=0;

	for(int ip=0;ip<2;ip++){ //! pt
	  //! mean
	  //! PC2
	  p0_pc2_m_dp[ip][iz][ic]=p1_pc2_m_dp[ip][iz][ic]=p2_pc2_m_dp[ip][iz][ic]=p3_pc2_m_dp[ip][iz][ic]=p4_pc2_m_dp[ip][iz][ic]=0;
	  p0_pc2_m_dz[ip][iz][ic]=p1_pc2_m_dz[ip][iz][ic]=p2_pc2_m_dz[ip][iz][ic]=p3_pc2_m_dz[ip][iz][ic]=p4_pc2_m_dz[ip][iz][ic]=0;
	  
	  //! PC3
	  p0_pc3_m_dp[ip][ia][iz][ic]=p1_pc3_m_dp[ip][ia][iz][ic]=p2_pc3_m_dp[ip][ia][iz][ic]=0,p3_pc3_m_dp[ip][ia][iz][ic]=0,p4_pc3_m_dp[ip][ia][iz][ic]=0;
	  p0_pc3_m_dz[ip][ia][iz][ic]=p1_pc3_m_dz[ip][ia][iz][ic]=p2_pc3_m_dz[ip][ia][iz][ic]=0,p3_pc3_m_dz[ip][ia][iz][ic]=0,p4_pc3_m_dz[ip][ia][iz][ic]=0;
	  
	  //! ToF
	  p0_tof_m_dp[ip][ia][iz][ic]=p1_tof_m_dp[ip][ia][iz][ic]=p2_tof_m_dp[ip][ia][iz][ic]=0,p3_tof_m_dp[ip][ia][iz][ic]=0,p4_tof_m_dp[ip][ia][iz][ic]=0;
	  p0_tof_m_dz[ip][ia][iz][ic]=p1_tof_m_dz[ip][ia][iz][ic]=p2_tof_m_dz[ip][ia][iz][ic]=0,p3_tof_m_dz[ip][ia][iz][ic]=0,p4_tof_m_dz[ip][ia][iz][ic]=0;
	  
	  //!
	  //! Afterburn
	  //!

	  //! PC2
	  p0_pc2_m_sdp[ip][iz][ic]=p1_pc2_m_sdp[ip][iz][ic]=p2_pc2_m_sdp[ip][iz][ic]=p3_pc2_m_sdp[ip][iz][ic]=p4_pc2_m_sdp[ip][iz][ic]=0;
	  p0_pc2_m_sdz[ip][iz][ic]=p1_pc2_m_sdz[ip][iz][ic]=p2_pc2_m_sdz[ip][iz][ic]=p3_pc2_m_sdz[ip][iz][ic]=p4_pc2_m_sdp[ip][iz][ic]=0;
	  p0_pc2_s_sdp[ip][iz][ic]=p1_pc2_s_sdp[ip][iz][ic]=p2_pc2_s_sdp[ip][iz][ic]=p3_pc2_s_sdp[ip][iz][ic]=p4_pc2_s_sdp[ip][iz][ic]=0;
	  p0_pc2_s_sdz[ip][iz][ic]=p1_pc2_s_sdz[ip][iz][ic]=p2_pc2_s_sdz[ip][iz][ic]=p3_pc2_s_sdz[ip][iz][ic]=p4_pc2_s_sdp[ip][iz][ic]=0;
	  
	  //! PC3
	  p0_pc3_m_sdp[ip][ia][iz][ic]=p1_pc3_m_sdp[ip][ia][iz][ic]=p2_pc3_m_sdp[ip][ia][iz][ic]=0,p3_pc3_m_sdp[ip][ia][iz][ic]=0,p4_pc3_m_sdp[ip][ia][iz][ic]=0;
	  p0_pc3_m_sdz[ip][ia][iz][ic]=p1_pc3_m_sdz[ip][ia][iz][ic]=p2_pc3_m_sdz[ip][ia][iz][ic]=0,p3_pc3_m_sdz[ip][ia][iz][ic]=0,p4_pc3_m_sdz[ip][ia][iz][ic]=0;
	  p0_pc3_s_sdp[ip][ia][iz][ic]=p1_pc3_s_sdp[ip][ia][iz][ic]=p2_pc3_s_sdp[ip][ia][iz][ic]=0,p3_pc3_s_sdp[ip][ia][iz][ic]=0,p4_pc3_s_sdp[ip][ia][iz][ic]=0;
	  p0_pc3_s_sdz[ip][ia][iz][ic]=p1_pc3_s_sdz[ip][ia][iz][ic]=p2_pc3_s_sdz[ip][ia][iz][ic]=0,p3_pc3_s_sdz[ip][ia][iz][ic]=0,p4_pc3_s_sdz[ip][ia][iz][ic]=0;
	  
	  //! ToF
	  p0_tof_m_sdp[ip][ia][iz][ic]=p1_tof_m_sdp[ip][ia][iz][ic]=p2_tof_m_sdp[ip][ia][iz][ic]=0,p3_tof_m_sdp[ip][ia][iz][ic]=0,p4_tof_m_sdp[ip][ia][iz][ic]=0;
	  p0_tof_m_sdz[ip][ia][iz][ic]=p1_tof_m_sdz[ip][ia][iz][ic]=p2_tof_m_sdz[ip][ia][iz][ic]=0,p3_tof_m_sdz[ip][ia][iz][ic]=0,p4_tof_m_sdz[ip][ia][iz][ic]=0;
	  p0_tof_s_sdp[ip][ia][iz][ic]=p1_tof_s_sdp[ip][ia][iz][ic]=p2_tof_s_sdp[ip][ia][iz][ic]=0,p3_tof_s_sdp[ip][ia][iz][ic]=0,p4_tof_s_sdp[ip][ia][iz][ic]=0;
	  p0_tof_s_sdz[ip][ia][iz][ic]=p1_tof_s_sdz[ip][ia][iz][ic]=p2_tof_s_sdz[ip][ia][iz][ic]=0,p3_tof_s_sdz[ip][ia][iz][ic]=0,p4_tof_s_sdz[ip][ia][iz][ic]=0;
	}
      }
    }
  }
  
  //! For EMC
  for(int ie=0;ie<NEMC;ie++){   //! iemc
    for(int iz=0;iz<NZBN;iz++){ //! zed
      for(int ic=0;ic<NCH;ic++){  //! charge

	//! EMC sigma
	p0_emc_s_dp[ie][iz][ic]=p1_emc_s_dp[ie][iz][ic]=p2_emc_s_dp[ie][iz][ic]=p3_emc_s_dp[ie][iz][ic]=p4_emc_s_dp[ie][iz][ic]=0;
	p0_emc_s_dz[ie][iz][ic]=p1_emc_s_dz[ie][iz][ic]=p2_emc_s_dz[ie][iz][ic]=p3_emc_s_dz[ie][iz][ic]=p4_emc_s_dz[ie][iz][ic]=0;
	
	//! EMC mean
	p0_emc_m_dp[ie][iz][ic]=p1_emc_m_dp[ie][iz][ic]=p2_emc_m_dp[ie][iz][ic]=p3_emc_m_dp[ie][iz][ic]=p4_emc_m_dp[ie][iz][ic]=0;
	p0_emc_m_dz[ie][iz][ic]=p1_emc_m_dz[ie][iz][ic]=p2_emc_m_dz[ie][iz][ic]=p3_emc_m_dz[ie][iz][ic]=p4_emc_m_dz[ie][iz][ic]=0;
	
	for(int ip=0;ip<2;ip++){ //! pt
	  //! 
	  //! After burn
	  //!
	  //! EMC
	  p0_emc_s_sdp[ip][ie][iz][ic]=p1_emc_s_sdp[ip][ie][iz][ic]=p2_emc_s_sdp[ip][ie][iz][ic]=p3_emc_s_sdp[ip][ie][iz][ic]=p4_emc_s_sdp[ip][ie][iz][ic]=0;
	  p0_emc_s_sdz[ip][ie][iz][ic]=p1_emc_s_sdz[ip][ie][iz][ic]=p2_emc_s_sdz[ip][ie][iz][ic]=p3_emc_s_sdz[ip][ie][iz][ic]=p4_emc_s_sdz[ip][ie][iz][ic]=0;
	  p0_emc_m_sdp[ip][ie][iz][ic]=p1_emc_m_sdp[ip][ie][iz][ic]=p2_emc_m_sdp[ip][ie][iz][ic]=p3_emc_m_sdp[ip][ie][iz][ic]=p4_emc_m_sdp[ip][ie][iz][ic]=0;
	  p0_emc_m_sdz[ip][ie][iz][ic]=p1_emc_m_sdz[ip][ie][iz][ic]=p2_emc_m_sdz[ip][ie][iz][ic]=p3_emc_m_sdz[ip][ie][iz][ic]=p4_emc_m_sdz[ip][ie][iz][ic]=0;
	}
      }
    }
  }  

  //! Matching Calibration
  InitPars();
  return 0;
}

//_____________________________________________________________________________________________________________________________
int MatchrecalReco62GeVRun10::InitRun(PHCompositeNode *topNode)
{
  RunHeader* d_runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(!d_runheader){
    cout << PHWHERE << " RunHeader not found"  << endl;
    return 0;
  }
  RunNumber = d_runheader->get_RunNumber();
  if(!isValidRun(RunNumber)){
    cout << " Not Valid Run! " << endl;
    return -1;
  }

  cout << "---------------------------------------------------------------" << endl;
  cout << " MatchrecalReco62GeVRun10::InitRun() :  run number = " << RunNumber << endl;
  cout << "---------------------------------------------------------------" << endl;
  return 0;
}

//_____________________________________________________________________________________________________________________________
int MatchrecalReco62GeVRun10::process_event(PHCompositeNode *topNode)
{

  PHGlobal*global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if(!global){
    cout << PHWHERE << "Could not find PHGlobal !" << endl;
    return 0;
  }
  int cent = (int)global->getCentrality();
  cent = cent/5;
  //  cout<<"cent : "<<cent<<"\t ph cent : "<<global->getCentrality()<<endl;

  PHCentralTrack *track = findNode::getClass<PHCentralTrack>(topNode,inputnodename.c_str());
  if (!track){
    cout << PHWHERE << "Could not find PHCentralTrack !" << endl;
    return 0;
  }



  unsigned int trks = track->get_npart();
  for(unsigned int itrk=0;itrk<trks;itrk++){

    PHSnglCentralTrack* sngltrk = track->get_track(itrk);

    //! track variables
    float mom      = sngltrk->get_mom();
    float the0     = sngltrk->get_the0();
    float phi0     = sngltrk->get_phi0();
    float zed      = sngltrk->get_zed();
    float phi1     = sngltrk->get_phi();
    float pt       = mom * sin(the0);
    float charge   = sngltrk->get_charge();
    float alpha    = sngltrk->get_alpha();

    //! 0-10 bins from -75cm to +75cm
    int ized  = Zed(zed);
    if(ized<0)continue;

    //! Charge not found
    if(charge==0 || alpha==0)continue;
    //! Charge for matching calibrations
    int ich=0;
    if(charge>0)ich=1;
    
    //! Apply the0 and phi0 cut here
    if(the0<-100 || phi0<-100)continue;

    //! Arm and sector  (Central Arm)
    int   isec = sngltrk->get_sect();
    int   iarm = 0;             //! East
    if (cos(phi1)>0) iarm = 1;  //! West

    //! all the sectors of emc sectors from 0-7
    int   isd  = iarm*4 + isec; 
    if (isd<-1 || isd>8) continue;
    //! isd = 0 & 1 (East PbGl)
    //! isd = 2 & 3 (East PbSc)
    //! isd = 4,5,6,7 (West PbSc)
    
    //! EMC calibrations
    int iemc=-1;
    if(isd==0 || isd==1)iemc=0;
    else if(isd==2 || isd==3)iemc=1;
    else if(isd>3)iemc=2;
    if(iemc<0)continue;

    //! pc2
    float pc2dphi  = sngltrk->get_pc2dphi();
    float pc2dz    = sngltrk->get_pc2dz();

    //! pc3
    float pc3dphi  = sngltrk->get_pc3dphi();
    float pc3dz    = sngltrk->get_pc3dz();

    //! tofe
    float   tofdphi  = sngltrk->get_tofdphi();
    float   tofdz    = sngltrk->get_tofdz();

    //! tofw
    float   tofwdphi = sngltrk->get_tofwdphi();
    float   tofwdz   = sngltrk->get_tofwdz();

    //! emc
    float   emcdphi  = sngltrk->get_emcdphi();
    float   emcdz    = sngltrk->get_emcdz();

    float pc2sdphi = -9999.;
    float pc2sdz   = -9999.;
    float pc3sdphi = -9999.;
    float pc3sdz   = -9999.;
    float emcsdphi = -9999.;
    float emcsdz   = -9999.;
    float tofsdphi = -9999.;
    float tofsdz   = -9999.;
    float tofwsdphi= -9999.;
    float tofwsdz  = -9999.;
    

    //! *************************************************************************************//
    //!
    //! To get the sigmalized dphi and dz variables for each detector
    //! Function to return sdval (val=dphi||dz) GetSval(int itype,int idet,int isub,int ized,int ich,float fpt,float fdval)      
    //! GetFineTSval(int iarm,int icent,float pt,float pc3sdz) : Remove the centrality dependence in the PC3 sdZ

    //! itype
    //! 0 : dphi
    //! 1 : dz
    
    //! idet
    //! 0 : pc2 (only ized as this is only one side no isub)
    //! 1 : pc3 (arm wise) 
    //! isub (iarm)
    //! 0 : Arm  (East)
    //! 1 : Arm  (West)
    
    //! 2 : emc 
    //! isub (iemc)
    //! 0 : (PbGlE)
    //! 1 : (PbScE) 
    //! 2 : (PbScW)
    
    //! 3 : tof 
    //! isub (iarm) 
    //! 0 : Arm (East)
    //! 1 : Arm (West)
    
    //! *************************************************************************************//
    //!
    //! PC3
    if(pc3dphi > -9999 || pc3dz > -9999){
      pc3sdphi = GetSval(0,1,iarm,ized,ich,pt,pc3dphi);
      pc3sdz   = GetSval(1,1,iarm,ized,ich,pt,pc3dz);

      //! Centrality dependence
      pc3sdz   = GetFineTSval(iarm,cent,pt,pc3sdz);
    }

    //! remove the background
    if(fabs(pc3sdphi)>3.00){
      pc2dphi=-9999; emcdphi=-9999; tofdphi=-9999; tofwdphi=-9999;
    }
    if(fabs(pc3sdz)>3.00){
      pc2dz=-9999; emcdz=-9999; tofdz=-9999; tofwdz=-9999;
    }


    //! PC2
    if(pc2dphi > -9999 || pc2dz > -9999){
      pc2sdphi = GetSval(0,0,iarm,ized,ich,pt,pc2dphi);
      pc2sdz   = GetSval(1,0,iarm,ized,ich,pt,pc2dz);
    }

    //! EMC
    if(emcdphi > -9999 || emcdz > -9999){
      emcsdphi = GetSval(0,2,iemc,ized,ich,pt,emcdphi);
      emcsdz   = GetSval(1,2,iemc,ized,ich,pt,emcdz);
    }
      
    //! ToF east
    if(tofdphi > -9999 || tofdz > -9999){ //! east
      tofsdphi = GetSval(0,3,iarm,ized,ich,pt,tofdphi);
      tofsdz   = GetSval(1,3,iarm,ized,ich,pt,tofdz);
    }
      
    //! ToF west
    if(tofwdphi > -9999 || tofwdz > -9999){ //! west
      tofwsdphi = GetSval(0,3,iarm,ized,ich,pt,tofwdphi);
      tofwsdz   = GetSval(1,3,iarm,ized,ich,pt,tofwdz);
    }

    // Set all variables
    sngltrk->set_pc2sdphi(pc2sdphi);
    sngltrk->set_pc2sdz(pc2sdz);
    sngltrk->set_pc3sdphi(pc3sdphi);
    sngltrk->set_pc3sdz(pc3sdz);
    sngltrk->set_tofsdphi(tofsdphi);
    sngltrk->set_tofsdz(tofsdz);
    sngltrk->set_tofwsdphi(tofwsdphi);
    sngltrk->set_tofwsdz(tofwsdz);
    sngltrk->set_emcsdphi(emcsdphi);
    sngltrk->set_emcsdz(emcsdz);
    
  }//! itrk loop ends

  return 0;
}
//_____________________________________________________________________________________________________________________________
int MatchrecalReco62GeVRun10::Zed(float zed)
{
  int ized=(int)(NZBN*(zed + 75.)/150.); 
  if(ized<0 || ized>=NZBN)return -999;
  else  return ized;
}
//_____________________________________________________________________________________________________________________________
float MatchrecalReco62GeVRun10::GetFineTSval(int isub,int cent,float pt,float sdz)
{

  float A0=1.0;
  float A1=0.0;
  float A2=0.0;

  //! Only for PC3 sdz
  if(isub==0){ //! arm : 0
    A0 = 0.840545   + 0.0544007/pt  - 0.00445003/pt/pt;
    A1 = -0.015345  + 0.0332662*pt  - 0.00778157*pt*pt;
    A2 = 0.00212588 - 0.00022534/pt - 1.1392e-05/pt/pt;
  }else if(isub==1){//! arm : 1
    A0 = 0.816671   + 0.0693902/pt   - 0.00629625/pt/pt;
    A1 =-0.0173205  + 0.0356379*pt   - 0.00787966*pt*pt;
    A2 =0.00274934  - 0.000640068/pt + 5.42491e-05/pt/pt;
  }

  float ftsdz = A0 + A1*cent + A2*cent*cent;
  sdz /= ftsdz;
  
  return sdz;
}

float MatchrecalReco62GeVRun10::GetSval(int itype,int idet,int isub,int ized,int ich,float fpt,float fdval)
{
  if(fdval<-9000 || fpt<0)return -9999;

  float p0_m=0,p1_m=0,p2_m=0,p3_m=0,p4_m=0;
  float p0_s=1,p1_s=0,p2_s=0,p3_s=0,p4_s=0;

  float p0_abm=0,p1_abm=0,p2_abm=0,p3_abm=0,p4_abm=0;
  float p0_abs=1,p1_abs=0,p2_abs=0,p3_abs=0,p4_abs=0;

  int   ipt=0;
  if(fpt>=1.5)ipt=1;
  
  if(itype==0){ //! dphi
    //cout<<"GetSval :: dPhi ::::::::: "<<endl;
    if(idet==0){//! PC2 
      p0_m=p0_pc2_m_dp[ipt][ized][ich]; p1_m=p1_pc2_m_dp[ipt][ized][ich]; p2_m=p2_pc2_m_dp[ipt][ized][ich];p3_m=p3_pc2_m_dp[ipt][ized][ich];p4_m=p4_pc2_m_dp[ipt][ized][ich];
      p0_s=p0_pc2_s_dp[ized][ich]     ; p1_s=p1_pc2_s_dp[ized][ich]     ; p2_s=p2_pc2_s_dp[ized][ich]     ;p3_s=p3_pc2_s_dp[ized][ich]     ;p4_s=p4_pc2_s_dp[ized][ich];
      
      //! Afterburn
      p0_abm=p0_pc2_m_sdp[ipt][ized][ich]; p1_abm=p1_pc2_m_sdp[ipt][ized][ich]; p2_abm=p2_pc2_m_sdp[ipt][ized][ich];p3_abm=p3_pc2_m_sdp[ipt][ized][ich];p4_abm=p4_pc2_m_sdp[ipt][ized][ich];
      p0_abs=p0_pc2_s_sdp[ipt][ized][ich]; p1_abs=p1_pc2_s_sdp[ipt][ized][ich]; p2_abs=p2_pc2_s_sdp[ipt][ized][ich];p3_abs=p3_pc2_s_sdp[ipt][ized][ich];p4_abs=p4_pc2_s_sdp[ipt][ized][ich];
    }
    else if(idet==1){//! PC3
      p0_m=p0_pc3_m_dp[ipt][isub][ized][ich];  p1_m=p1_pc3_m_dp[ipt][isub][ized][ich]; p2_m=p2_pc3_m_dp[ipt][isub][ized][ich];p3_m=p3_pc3_m_dp[ipt][isub][ized][ich];p4_m=p4_pc3_m_dp[ipt][isub][ized][ich];
      p0_s=p0_pc3_s_dp[isub][ized][ich]     ;  p1_s=p1_pc3_s_dp[isub][ized][ich]     ; p2_s=p2_pc3_s_dp[isub][ized][ich]     ;p3_s=p3_pc3_s_dp[isub][ized][ich]     ;p4_s=p4_pc3_s_dp[isub][ized][ich];

      //! Afterburn
      p0_abm=p0_pc3_m_sdp[ipt][isub][ized][ich]; p1_abm=p1_pc3_m_sdp[ipt][isub][ized][ich]; p2_abm=p2_pc3_m_sdp[ipt][isub][ized][ich];p3_abm=p3_pc3_m_sdp[ipt][isub][ized][ich];p4_abm=p4_pc3_m_sdp[ipt][isub][ized][ich];
      p0_abs=p0_pc3_s_sdp[ipt][isub][ized][ich]; p1_abs=p1_pc3_s_sdp[ipt][isub][ized][ich]; p2_abs=p2_pc3_s_sdp[ipt][isub][ized][ich];p3_abs=p3_pc3_s_sdp[ipt][isub][ized][ich];p4_abs=p4_pc3_s_sdp[ipt][isub][ized][ich];
    }
    else if(idet==2){//! EMC
      p0_m=p0_emc_m_dp[isub][ized][ich];  p1_m=p1_emc_m_dp[isub][ized][ich]; p2_m=p2_emc_m_dp[isub][ized][ich];p3_m=p3_emc_m_dp[isub][ized][ich];p4_m=p4_emc_m_dp[isub][ized][ich];
      p0_s=p0_emc_s_dp[isub][ized][ich];  p1_s=p1_emc_s_dp[isub][ized][ich]; p2_s=p2_emc_s_dp[isub][ized][ich];p3_s=p3_emc_s_dp[isub][ized][ich];p4_s=p4_emc_s_dp[isub][ized][ich];

      //! Afterburn
      p0_abm=p0_emc_m_sdp[ipt][isub][ized][ich]; p1_abm=p1_emc_m_sdp[ipt][isub][ized][ich]; p2_abm=p2_emc_m_sdp[ipt][isub][ized][ich];p3_abm=p3_emc_m_sdp[ipt][isub][ized][ich]; p4_abm=p4_emc_m_sdp[ipt][isub][ized][ich];
      p0_abs=p0_emc_s_sdp[ipt][isub][ized][ich]; p1_abs=p1_emc_s_sdp[ipt][isub][ized][ich]; p2_abs=p2_emc_s_sdp[ipt][isub][ized][ich];p3_abs=p3_emc_s_sdp[ipt][isub][ized][ich]; p4_abs=p4_emc_s_sdp[ipt][isub][ized][ich];
    }
    else if(idet==3){//! ToF
      p0_m=p0_tof_m_dp[ipt][isub][ized][ich];  p1_m=p1_tof_m_dp[ipt][isub][ized][ich]; p2_m=p2_tof_m_dp[ipt][isub][ized][ich];p3_m=p3_tof_m_dp[ipt][isub][ized][ich];p4_m=p4_tof_m_dp[ipt][isub][ized][ich];
      p0_s=p0_tof_s_dp[isub][ized][ich]     ;  p1_s=p1_tof_s_dp[isub][ized][ich]     ; p2_s=p2_tof_s_dp[isub][ized][ich]     ;p3_s=p3_tof_s_dp[isub][ized][ich]     ;p4_s=p4_tof_s_dp[isub][ized][ich];

      //! Afterburn
      p0_abm=p0_tof_m_sdp[0][isub][ized][ich]; p1_abm=p1_tof_m_sdp[0][isub][ized][ich]; p2_abm=p2_tof_m_sdp[0][isub][ized][ich];p3_abm=p3_tof_m_sdp[0][isub][ized][ich]; p4_abm=p4_tof_m_sdp[0][isub][ized][ich];
      p0_abs=p0_tof_s_sdp[0][isub][ized][ich]; p1_abs=p1_tof_s_sdp[0][isub][ized][ich]; p2_abs=p2_tof_s_sdp[0][isub][ized][ich];p3_abs=p3_tof_s_sdp[0][isub][ized][ich]; p4_abs=p4_tof_s_sdp[0][isub][ized][ich];
    }
  }
  else if(itype==1){  //! dz
    //cout<<"GetSval :: dZ ******** "<<endl;
    if(idet==0){//! PC2 
      p0_m=p0_pc2_m_dz[ipt][ized][ich]; p1_m=p1_pc2_m_dz[ipt][ized][ich]; p2_m= p2_pc2_m_dz[ipt][ized][ich]; p3_m= p3_pc2_m_dz[ipt][ized][ich]; p4_m= p4_pc2_m_dz[ipt][ized][ich];
      p0_s=p0_pc2_s_dz[ized][ich]     ; p1_s=p1_pc2_s_dz[ized][ich]     ; p2_s= p2_pc2_s_dz[ized][ich]     ; p3_s= p3_pc2_s_dz[ized][ich]     ; p4_s= p4_pc2_s_dz[ized][ich];

      //! Afterburn
      p0_abm=p0_pc2_m_sdz[ipt][ized][ich]; p1_abm=p1_pc2_m_sdz[ipt][ized][ich]; p2_abm= p2_pc2_m_sdz[ipt][ized][ich]; p3_abm= p3_pc2_m_sdz[ipt][ized][ich]; p4_abm= p4_pc2_m_sdz[ipt][ized][ich];
      p0_abs=p0_pc2_s_sdz[ipt][ized][ich]; p1_abs=p1_pc2_s_sdz[ipt][ized][ich]; p2_abs= p2_pc2_s_sdz[ipt][ized][ich]; p3_abs= p3_pc2_s_sdz[ipt][ized][ich]; p4_abs= p4_pc2_s_sdz[ipt][ized][ich];
    }
    else if(idet==1){//! PC3
      p0_m=p0_pc3_m_dz[ipt][isub][ized][ich];  p1_m=p1_pc3_m_dz[ipt][isub][ized][ich]; p2_m=p2_pc3_m_dz[ipt][isub][ized][ich];p3_m=p3_pc3_m_dz[ipt][isub][ized][ich];p4_m=p4_pc3_m_dz[ipt][isub][ized][ich];
      p0_s=p0_pc3_s_dz[isub][ized][ich]     ;  p1_s=p1_pc3_s_dz[isub][ized][ich]     ; p2_s=p2_pc3_s_dz[isub][ized][ich]     ;p3_s=p3_pc3_s_dz[isub][ized][ich]     ;p4_s=p4_pc3_s_dz[isub][ized][ich];
      
      //! Afterburn
      p0_abm=p0_pc3_m_sdz[ipt][isub][ized][ich]; p1_abm=p1_pc3_m_sdz[ipt][isub][ized][ich]; p2_abm=p2_pc3_m_sdz[ipt][isub][ized][ich];p3_abm=p3_pc3_m_sdz[ipt][isub][ized][ich]; p4_abm=p4_pc3_m_sdz[ipt][isub][ized][ich];
      p0_abs=p0_pc3_s_sdz[ipt][isub][ized][ich]; p1_abs=p1_pc3_s_sdz[ipt][isub][ized][ich]; p2_abs=p2_pc3_s_sdz[ipt][isub][ized][ich];p3_abs=p3_pc3_s_sdz[ipt][isub][ized][ich]; p4_abs=p4_pc3_s_sdz[ipt][isub][ized][ich];
    }
    else if(idet==2){//! EMC
      p0_m=p0_emc_m_dz[isub][ized][ich];  p1_m=p1_emc_m_dz[isub][ized][ich]; p2_m=p2_emc_m_dz[isub][ized][ich];p3_m=p3_emc_m_dz[isub][ized][ich];p4_m=p4_emc_m_dz[isub][ized][ich];
      p0_s=p0_emc_s_dz[isub][ized][ich];  p1_s=p1_emc_s_dz[isub][ized][ich]; p2_s=p2_emc_s_dz[isub][ized][ich];p3_s=p3_emc_s_dz[isub][ized][ich];p4_s=p4_emc_s_dz[isub][ized][ich];

      //! Afterburn
      p0_abm=p0_emc_m_sdz[ipt][isub][ized][ich]; p1_abm=p1_emc_m_sdz[ipt][isub][ized][ich]; p2_abm=p2_emc_m_sdz[ipt][isub][ized][ich];p3_abm=p3_emc_m_sdz[ipt][isub][ized][ich]; p4_abm=p4_emc_m_sdz[ipt][isub][ized][ich];
      p0_abs=p0_emc_s_sdz[ipt][isub][ized][ich]; p1_abs=p1_emc_s_sdz[ipt][isub][ized][ich]; p2_abs=p2_emc_s_sdz[ipt][isub][ized][ich];p3_abs=p3_emc_s_sdz[ipt][isub][ized][ich]; p4_abs=p4_emc_s_sdz[ipt][isub][ized][ich];
    }
    else if(idet==3){//! ToF
      p0_m=p0_tof_m_dz[ipt][isub][ized][ich];  p1_m=p1_tof_m_dz[ipt][isub][ized][ich]; p2_m=p2_tof_m_dz[ipt][isub][ized][ich];p3_m=p3_tof_m_dz[ipt][isub][ized][ich];p4_m=p4_tof_m_dz[ipt][isub][ized][ich];
      p0_s=p0_tof_s_dz[isub][ized][ich]     ;  p1_s=p1_tof_s_dz[isub][ized][ich]     ; p2_s=p2_tof_s_dz[isub][ized][ich]     ;p3_s=p3_tof_s_dz[isub][ized][ich]     ;p4_s=p4_tof_s_dz[isub][ized][ich];

      //! Afterburn
      p0_abm=p0_tof_m_sdz[0][isub][ized][ich]; p1_abm=p1_tof_m_sdz[0][isub][ized][ich]; p2_abm=p2_tof_m_sdz[0][isub][ized][ich];p3_abm=p3_tof_m_sdz[0][isub][ized][ich]; p4_abm=p4_tof_m_sdz[0][isub][ized][ich];
      p0_abs=p0_tof_s_sdz[0][isub][ized][ich]; p1_abs=p1_tof_s_sdz[0][isub][ized][ich]; p2_abs=p2_tof_s_sdz[0][isub][ized][ich];p3_abs=p3_tof_s_sdz[0][isub][ized][ich]; p4_abs=p4_tof_s_sdz[0][isub][ized][ich];
    }
  }

  float mean=0 , smean =0;
  float sigma=1, ssigma=1;

  //! First pass
  mean  = p0_m + p1_m/fpt + p2_m/fpt/fpt + p3_m/fpt/fpt/fpt + p4_m/fpt/fpt/fpt/fpt;
  sigma = p0_s + p1_s/fpt + p2_s/fpt/fpt + p3_s/fpt/fpt/fpt + p4_s/fpt/fpt/fpt/fpt;

  //! Afterburn
  smean  = p0_abm + p1_abm/fpt + p2_abm/fpt/fpt + p3_abm/fpt/fpt/fpt + p4_abm/fpt/fpt/fpt/fpt;
  ssigma = p0_abs + p1_abs/fpt + p2_abs/fpt/fpt + p3_abs/fpt/fpt/fpt + p4_abs/fpt/fpt/fpt/fpt;
  
  float sdval = (((fdval - mean)/sigma) - smean)/ssigma;

  return sdval;
}
//_____________________________________________________________________________________________________________________________
int MatchrecalReco62GeVRun10::InitPars(void)
{
  //! PC2 dphi neg
  p0_pc2_m_dp[0][0][0]=-0.00102262; p1_pc2_m_dp[0][0][0]=0.000435713; p2_pc2_m_dp[0][0][0]=-0.000337245; p3_pc2_m_dp[0][0][0]=9.27597e-05; p4_pc2_m_dp[0][0][0]=-1.76087e-05; 
  p0_pc2_m_dp[1][0][0]=-0.000631413; p1_pc2_m_dp[1][0][0]=-0.000456999; p2_pc2_m_dp[1][0][0]=-1.18421e-05; p3_pc2_m_dp[1][0][0]=0.000401488; p4_pc2_m_dp[1][0][0]=2.23918e-05; 
  p0_pc2_s_dp[0][0]=0.00181373; p1_pc2_s_dp[0][0]=0.000525832; p2_pc2_s_dp[0][0]=4.29969e-05; p3_pc2_s_dp[0][0]=9.37062e-06; p4_pc2_s_dp[0][0]=8.83736e-07; 
  p0_pc2_m_dp[0][1][0]=-0.00102264; p1_pc2_m_dp[0][1][0]=0.000544286; p2_pc2_m_dp[0][1][0]=-0.00026562; p3_pc2_m_dp[0][1][0]=8.61349e-05; p4_pc2_m_dp[0][1][0]=-1.23499e-05; 
  p0_pc2_m_dp[1][1][0]=-0.000481806; p1_pc2_m_dp[1][1][0]=-0.000977481; p2_pc2_m_dp[1][1][0]=0.000429746; p3_pc2_m_dp[1][1][0]=0.00197855; p4_pc2_m_dp[1][1][0]=-0.00184212; 
  p0_pc2_s_dp[1][0]=0.00185518; p1_pc2_s_dp[1][0]=0.000384235; p2_pc2_s_dp[1][0]=9.34051e-05; p3_pc2_s_dp[1][0]=2.23902e-05; p4_pc2_s_dp[1][0]=-4.16311e-06; 
  p0_pc2_m_dp[0][2][0]=-0.000929995; p1_pc2_m_dp[0][2][0]=0.00043839; p2_pc2_m_dp[0][2][0]=-0.000262321; p3_pc2_m_dp[0][2][0]=0.0001492; p4_pc2_m_dp[0][2][0]=-2.26289e-05; 
  p0_pc2_m_dp[1][2][0]=0.000259123; p1_pc2_m_dp[1][2][0]=-0.00404523; p2_pc2_m_dp[1][2][0]=0.00152049; p3_pc2_m_dp[1][2][0]=0.011003; p4_pc2_m_dp[1][2][0]=-0.0111785; 
  p0_pc2_s_dp[2][0]=0.00188369; p1_pc2_s_dp[2][0]=0.000143262; p2_pc2_s_dp[2][0]=0.000360167; p3_pc2_s_dp[2][0]=-8.087e-05; p4_pc2_s_dp[2][0]=8.78955e-06; 
  p0_pc2_m_dp[0][3][0]=-0.00102126; p1_pc2_m_dp[0][3][0]=0.000721095; p2_pc2_m_dp[0][3][0]=-0.000415705; p3_pc2_m_dp[0][3][0]=0.000203024; p4_pc2_m_dp[0][3][0]=-2.83396e-05; 
  p0_pc2_m_dp[1][3][0]=0.00183469; p1_pc2_m_dp[1][3][0]=-0.0100031; p2_pc2_m_dp[1][3][0]=0.00304601; p3_pc2_m_dp[1][3][0]=0.0269095; p4_pc2_m_dp[1][3][0]=-0.026115; 
  p0_pc2_s_dp[3][0]=0.00175983; p1_pc2_s_dp[3][0]=0.000521211; p2_pc2_s_dp[3][0]=-0.000143772; p3_pc2_s_dp[3][0]=0.000144208; p4_pc2_s_dp[3][0]=-2.20879e-05; 
  p0_pc2_m_dp[0][4][0]=-0.000557344; p1_pc2_m_dp[0][4][0]=-0.000359639; p2_pc2_m_dp[0][4][0]=0.000536766; p3_pc2_m_dp[0][4][0]=-0.000109706; p4_pc2_m_dp[0][4][0]=6.57023e-06; 
  p0_pc2_m_dp[1][4][0]=0.00157263; p1_pc2_m_dp[1][4][0]=-0.00883337; p2_pc2_m_dp[1][4][0]=0.0028942; p3_pc2_m_dp[1][4][0]=0.0240289; p4_pc2_m_dp[1][4][0]=-0.0237257; 
  p0_pc2_s_dp[4][0]=0.00178011; p1_pc2_s_dp[4][0]=0.000409038; p2_pc2_s_dp[4][0]=6.45026e-06; p3_pc2_s_dp[4][0]=7.9192e-05; p4_pc2_s_dp[4][0]=-1.26199e-05; 
  p0_pc2_m_dp[0][5][0]=-0.000753612; p1_pc2_m_dp[0][5][0]=0.000257272; p2_pc2_m_dp[0][5][0]=0.000103556; p3_pc2_m_dp[0][5][0]=-1.41048e-06; p4_pc2_m_dp[0][5][0]=-1.61367e-06; 
  p0_pc2_m_dp[1][5][0]=-4.2215e-05; p1_pc2_m_dp[1][5][0]=-0.00174518; p2_pc2_m_dp[1][5][0]=0.000140042; p3_pc2_m_dp[1][5][0]=0.00357683; p4_pc2_m_dp[1][5][0]=-0.00225749; 
  p0_pc2_s_dp[5][0]=0.00184321; p1_pc2_s_dp[5][0]=0.000267301; p2_pc2_s_dp[5][0]=0.000249936; p3_pc2_s_dp[5][0]=-5.46608e-05; p4_pc2_s_dp[5][0]=7.76621e-06; 
  p0_pc2_m_dp[0][6][0]=-0.000755459; p1_pc2_m_dp[0][6][0]=0.000184134; p2_pc2_m_dp[0][6][0]=0.00016809; p3_pc2_m_dp[0][6][0]=-3.62151e-05; p4_pc2_m_dp[0][6][0]=3.17951e-06; 
  p0_pc2_m_dp[1][6][0]=-0.000162097; p1_pc2_m_dp[1][6][0]=-0.00127597; p2_pc2_m_dp[1][6][0]=-0.000149076; p3_pc2_m_dp[1][6][0]=0.00235054; p4_pc2_m_dp[1][6][0]=-0.000957572; 
  p0_pc2_s_dp[6][0]=0.00175146; p1_pc2_s_dp[6][0]=0.000505451; p2_pc2_s_dp[6][0]=-3.85745e-06; p3_pc2_s_dp[6][0]=3.55642e-05; p4_pc2_s_dp[6][0]=-2.69784e-06; 
  p0_pc2_m_dp[0][7][0]=-0.000769784; p1_pc2_m_dp[0][7][0]=6.83547e-05; p2_pc2_m_dp[0][7][0]=0.00018294; p3_pc2_m_dp[0][7][0]=-4.429e-05; p4_pc2_m_dp[0][7][0]=3.3809e-06; 
  p0_pc2_m_dp[1][7][0]=-3.33956e-05; p1_pc2_m_dp[1][7][0]=-0.00211101; p2_pc2_m_dp[1][7][0]=0.000282594; p3_pc2_m_dp[1][7][0]=0.00461006; p4_pc2_m_dp[1][7][0]=-0.00358505; 
  p0_pc2_s_dp[7][0]=0.0017218; p1_pc2_s_dp[7][0]=0.000554394; p2_pc2_s_dp[7][0]=-8.59851e-05; p3_pc2_s_dp[7][0]=7.00566e-05; p4_pc2_s_dp[7][0]=-7.35963e-06; 
  p0_pc2_m_dp[0][8][0]=-0.000858897; p1_pc2_m_dp[0][8][0]=0.000118218; p2_pc2_m_dp[0][8][0]=6.32162e-05; p3_pc2_m_dp[0][8][0]=-2.38567e-05; p4_pc2_m_dp[0][8][0]=8.05035e-07; 
  p0_pc2_m_dp[1][8][0]=-0.000690829; p1_pc2_m_dp[1][8][0]=0.000126991; p2_pc2_m_dp[1][8][0]=-0.000217625; p3_pc2_m_dp[1][8][0]=-0.00163402; p4_pc2_m_dp[1][8][0]=0.00218116; 
  p0_pc2_s_dp[8][0]=0.00157878; p1_pc2_s_dp[8][0]=0.000930026; p2_pc2_s_dp[8][0]=-0.000477924; p3_pc2_s_dp[8][0]=0.00022374; p4_pc2_s_dp[8][0]=-2.81905e-05; 
  p0_pc2_m_dp[0][9][0]=-0.000993508; p1_pc2_m_dp[0][9][0]=0.000281994; p2_pc2_m_dp[0][9][0]=-0.000184487; p3_pc2_m_dp[0][9][0]=4.52402e-05; p4_pc2_m_dp[0][9][0]=-1.19055e-05; 
  p0_pc2_m_dp[1][9][0]=-0.000598619; p1_pc2_m_dp[1][9][0]=-0.000823078; p2_pc2_m_dp[1][9][0]=0.000266337; p3_pc2_m_dp[1][9][0]=0.00160939; p4_pc2_m_dp[1][9][0]=-0.00164323; 
  p0_pc2_s_dp[9][0]=0.00162552; p1_pc2_s_dp[9][0]=0.000687527; p2_pc2_s_dp[9][0]=-0.000218615; p3_pc2_s_dp[9][0]=0.000108602; p4_pc2_s_dp[9][0]=-1.07686e-05;

  //! PC2 dphi pos
  p0_pc2_m_dp[0][0][1]=-0.000878058; p1_pc2_m_dp[0][0][1]=-0.00124903; p2_pc2_m_dp[0][0][1]=0.000971039; p3_pc2_m_dp[0][0][1]=-0.00035045; p4_pc2_m_dp[0][0][1]=5.34608e-05; 
  p0_pc2_m_dp[1][0][1]=-0.00120224; p1_pc2_m_dp[1][0][1]=-0.00120722; p2_pc2_m_dp[1][0][1]=0.000910001; p3_pc2_m_dp[1][0][1]=0.00286678; p4_pc2_m_dp[1][0][1]=-0.0033273; 
  p0_pc2_s_dp[0][1]=0.0023318; p1_pc2_s_dp[0][1]=-0.000450743; p2_pc2_s_dp[0][1]=0.000883015; p3_pc2_s_dp[0][1]=-0.000253375; p4_pc2_s_dp[0][1]=2.7008e-05; 
  p0_pc2_m_dp[0][1][1]=-0.000772907; p1_pc2_m_dp[0][1][1]=-0.00181113; p2_pc2_m_dp[0][1][1]=0.00135386; p3_pc2_m_dp[0][1][1]=-0.000494255; p4_pc2_m_dp[0][1][1]=6.43365e-05; 
  p0_pc2_m_dp[1][1][1]=-0.00373562; p1_pc2_m_dp[1][1][1]=0.00672006; p2_pc2_m_dp[1][1][1]=5.53958e-05; p3_pc2_m_dp[1][1][1]=-0.0156276; p4_pc2_m_dp[1][1][1]=0.0116021; 
  p0_pc2_s_dp[1][1]=0.00224108; p1_pc2_s_dp[1][1]=-0.000202118; p2_pc2_s_dp[1][1]=0.000623124; p3_pc2_s_dp[1][1]=-0.000151757; p4_pc2_s_dp[1][1]=1.37758e-05; 
  p0_pc2_m_dp[0][2][1]=-0.000624022; p1_pc2_m_dp[0][2][1]=-0.00231436; p2_pc2_m_dp[0][2][1]=0.0015878; p3_pc2_m_dp[0][2][1]=-0.000544842; p4_pc2_m_dp[0][2][1]=6.51423e-05; 
  p0_pc2_m_dp[1][2][1]=-0.00305916; p1_pc2_m_dp[1][2][1]=0.00387722; p2_pc2_m_dp[1][2][1]=0.00063134; p3_pc2_m_dp[1][2][1]=-0.00844654; p4_pc2_m_dp[1][2][1]=0.00518038; 
  p0_pc2_s_dp[2][1]=0.00216264; p1_pc2_s_dp[2][1]=1.59118e-05; p2_pc2_s_dp[2][1]=0.000385113; p3_pc2_s_dp[2][1]=-5.90177e-05; p4_pc2_s_dp[2][1]=2.65912e-06; 
  p0_pc2_m_dp[0][3][1]=-0.00065866; p1_pc2_m_dp[0][3][1]=-0.00209749; p2_pc2_m_dp[0][3][1]=0.00134507; p3_pc2_m_dp[0][3][1]=-0.000459138; p4_pc2_m_dp[0][3][1]=5.39109e-05; 
  p0_pc2_m_dp[1][3][1]=-0.00215521; p1_pc2_m_dp[1][3][1]=0.00161029; p2_pc2_m_dp[1][3][1]=0.000263487; p3_pc2_m_dp[1][3][1]=-0.00356225; p4_pc2_m_dp[1][3][1]=0.00193585; 
  p0_pc2_s_dp[3][1]=0.00195905; p1_pc2_s_dp[3][1]=0.000379703; p2_pc2_s_dp[3][1]=7.98317e-06; p3_pc2_s_dp[3][1]=9.47402e-05; p4_pc2_s_dp[3][1]=-1.75842e-05; 
  p0_pc2_m_dp[0][4][1]=-0.000819169; p1_pc2_m_dp[0][4][1]=-0.00187471; p2_pc2_m_dp[0][4][1]=0.00115146; p3_pc2_m_dp[0][4][1]=-0.000394908; p4_pc2_m_dp[0][4][1]=4.59431e-05; 
  p0_pc2_m_dp[1][4][1]=-0.00421282; p1_pc2_m_dp[1][4][1]=0.00917997; p2_pc2_m_dp[1][4][1]=-0.0017618; p3_pc2_m_dp[1][4][1]=-0.0238383; p4_pc2_m_dp[1][4][1]=0.0215035; 
  p0_pc2_s_dp[4][1]=0.0020819; p1_pc2_s_dp[4][1]=9.75459e-05; p2_pc2_s_dp[4][1]=0.000322019; p3_pc2_s_dp[4][1]=-3.76322e-05; p4_pc2_s_dp[4][1]=8.20536e-07; 
  p0_pc2_m_dp[0][5][1]=-0.000544976; p1_pc2_m_dp[0][5][1]=-0.0022097; p2_pc2_m_dp[0][5][1]=0.00142385; p3_pc2_m_dp[0][5][1]=-0.000471589; p4_pc2_m_dp[0][5][1]=5.244e-05; 
  p0_pc2_m_dp[1][5][1]=-0.00287983; p1_pc2_m_dp[1][5][1]=0.00461401; p2_pc2_m_dp[1][5][1]=-0.000578796; p3_pc2_m_dp[1][5][1]=-0.0121272; p4_pc2_m_dp[1][5][1]=0.0105876; 
  p0_pc2_s_dp[5][1]=0.00200351; p1_pc2_s_dp[5][1]=0.000206235; p2_pc2_s_dp[5][1]=0.000290204; p3_pc2_s_dp[5][1]=-5.24128e-05; p4_pc2_s_dp[5][1]=4.99813e-06; 
  p0_pc2_m_dp[0][6][1]=-0.00051795; p1_pc2_m_dp[0][6][1]=-0.00207438; p2_pc2_m_dp[0][6][1]=0.00135316; p3_pc2_m_dp[0][6][1]=-0.000448819; p4_pc2_m_dp[0][6][1]=4.99925e-05; 
  p0_pc2_m_dp[1][6][1]=-0.00154917; p1_pc2_m_dp[1][6][1]=-0.000349054; p2_pc2_m_dp[1][6][1]=0.00113588; p3_pc2_m_dp[1][6][1]=0.00134628; p4_pc2_m_dp[1][6][1]=-0.00308626; 
  p0_pc2_s_dp[6][1]=0.00190998; p1_pc2_s_dp[6][1]=0.000350923; p2_pc2_s_dp[6][1]=0.000148172; p3_pc2_s_dp[6][1]=2.56249e-06; p4_pc2_s_dp[6][1]=-2.64346e-06; 
  p0_pc2_m_dp[0][7][1]=-0.000733961; p1_pc2_m_dp[0][7][1]=-0.00134138; p2_pc2_m_dp[0][7][1]=0.000841382; p3_pc2_m_dp[0][7][1]=-0.000290657; p4_pc2_m_dp[0][7][1]=3.44606e-05; 
  p0_pc2_m_dp[1][7][1]=-0.00226393; p1_pc2_m_dp[1][7][1]=0.00243136; p2_pc2_m_dp[1][7][1]=0.000636907; p3_pc2_m_dp[1][7][1]=-0.00544914; p4_pc2_m_dp[1][7][1]=0.00291295; 
  p0_pc2_s_dp[7][1]=0.00195913; p1_pc2_s_dp[7][1]=0.000102067; p2_pc2_s_dp[7][1]=0.000380793; p3_pc2_s_dp[7][1]=-8.91448e-05; p4_pc2_s_dp[7][1]=9.29522e-06; 
  p0_pc2_m_dp[0][8][1]=-0.000761771; p1_pc2_m_dp[0][8][1]=-0.00113342; p2_pc2_m_dp[0][8][1]=0.000789767; p3_pc2_m_dp[0][8][1]=-0.000279205; p4_pc2_m_dp[0][8][1]=3.61807e-05; 
  p0_pc2_m_dp[1][8][1]=-0.00122376; p1_pc2_m_dp[1][8][1]=-0.00115665; p2_pc2_m_dp[1][8][1]=0.00140273; p3_pc2_m_dp[1][8][1]=0.00380047; p4_pc2_m_dp[1][8][1]=-0.00532465; 
  p0_pc2_s_dp[8][1]=0.00199155; p1_pc2_s_dp[8][1]=-0.000141867; p2_pc2_s_dp[8][1]=0.000619518; p3_pc2_s_dp[8][1]=-0.000179771; p4_pc2_s_dp[8][1]=2.01966e-05; 
  p0_pc2_m_dp[0][9][1]=-0.000701169; p1_pc2_m_dp[0][9][1]=-0.00115317; p2_pc2_m_dp[0][9][1]=0.000963081; p3_pc2_m_dp[0][9][1]=-0.000336231; p4_pc2_m_dp[0][9][1]=4.81942e-05; 
  p0_pc2_m_dp[1][9][1]=-0.00118121; p1_pc2_m_dp[1][9][1]=-0.000109622; p2_pc2_m_dp[1][9][1]=0.000318366; p3_pc2_m_dp[1][9][1]=-0.00028422; p4_pc2_m_dp[1][9][1]=0.00014927; 
  p0_pc2_s_dp[9][1]=0.0020195; p1_pc2_s_dp[9][1]=-0.000255528; p2_pc2_s_dp[9][1]=0.000688239; p3_pc2_s_dp[9][1]=-0.000203634; p4_pc2_s_dp[9][1]=2.38788e-05; 

  //! PC2 dz neg
  p0_pc2_m_dz[0][0][0]=-0.316383; p1_pc2_m_dz[0][0][0]=0.0432845; p2_pc2_m_dz[0][0][0]=0.0665986; p3_pc2_m_dz[0][0][0]=-1.18423e-05; p4_pc2_m_dz[0][0][0]=-0.00328459; 
  p0_pc2_m_dz[1][0][0]=-0.38937; p1_pc2_m_dz[1][0][0]=-0.629366; p2_pc2_m_dz[1][0][0]=0.588082; p3_pc2_m_dz[1][0][0]=2.16756; p4_pc2_m_dz[1][0][0]=-1.76653; 
  p0_pc2_s_dz[0][0]=0.753236; p1_pc2_s_dz[0][0]=0.028273; p2_pc2_s_dz[0][0]=0.196007; p3_pc2_s_dz[0][0]=-0.0223711; p4_pc2_s_dz[0][0]=0.00144464; 
  p0_pc2_m_dz[0][1][0]=-0.519398; p1_pc2_m_dz[0][1][0]=0.102629; p2_pc2_m_dz[0][1][0]=0.00289655; p3_pc2_m_dz[0][1][0]=0.0219911; p4_pc2_m_dz[0][1][0]=-0.00442146; 
  p0_pc2_m_dz[1][1][0]=-0.600237; p1_pc2_m_dz[1][1][0]=-0.16247; p2_pc2_m_dz[1][1][0]=0.408943; p3_pc2_m_dz[1][1][0]=0.639454; p4_pc2_m_dz[1][1][0]=-0.510965; 
  p0_pc2_s_dz[1][0]=0.737797; p1_pc2_s_dz[1][0]=0.133336; p2_pc2_s_dz[1][0]=0.0741981; p3_pc2_s_dz[1][0]=0.019579; p4_pc2_s_dz[1][0]=-0.00333898; 
  p0_pc2_m_dz[0][2][0]=-0.65534; p1_pc2_m_dz[0][2][0]=0.11852; p2_pc2_m_dz[0][2][0]=-0.0275411; p3_pc2_m_dz[0][2][0]=0.025094; p4_pc2_m_dz[0][2][0]=-0.00357037; 
  p0_pc2_m_dz[1][2][0]=-0.759963; p1_pc2_m_dz[1][2][0]=0.175851; p2_pc2_m_dz[1][2][0]=0.262206; p3_pc2_m_dz[1][2][0]=-0.451112; p4_pc2_m_dz[1][2][0]=0.428526; 
  p0_pc2_s_dz[2][0]=0.715507; p1_pc2_s_dz[2][0]=0.16824; p2_pc2_s_dz[2][0]=0.0431575; p3_pc2_s_dz[2][0]=0.0247064; p4_pc2_s_dz[2][0]=-0.0033563; 
  p0_pc2_m_dz[0][3][0]=-0.799796; p1_pc2_m_dz[0][3][0]=0.126212; p2_pc2_m_dz[0][3][0]=-0.0410685; p3_pc2_m_dz[0][3][0]=0.0202672; p4_pc2_m_dz[0][3][0]=-0.00230633; 
  p0_pc2_m_dz[1][3][0]=-0.658985; p1_pc2_m_dz[1][3][0]=-0.606218; p2_pc2_m_dz[1][3][0]=0.446785; p3_pc2_m_dz[1][3][0]=1.49779; p4_pc2_m_dz[1][3][0]=-1.51601; 
  p0_pc2_s_dz[3][0]=0.701243; p1_pc2_s_dz[3][0]=0.175868; p2_pc2_s_dz[3][0]=0.0404125; p3_pc2_s_dz[3][0]=0.0242952; p4_pc2_s_dz[3][0]=-0.00328481; 
  p0_pc2_m_dz[0][4][0]=-0.895389; p1_pc2_m_dz[0][4][0]=0.0318501; p2_pc2_m_dz[0][4][0]=-0.00177774; p3_pc2_m_dz[0][4][0]=0.00357905; p4_pc2_m_dz[0][4][0]=-0.00017226; 
  p0_pc2_m_dz[1][4][0]=-0.909833; p1_pc2_m_dz[1][4][0]=0.0380568; p2_pc2_m_dz[1][4][0]=0.152152; p3_pc2_m_dz[1][4][0]=-0.46809; p4_pc2_m_dz[1][4][0]=0.447621; 
  p0_pc2_s_dz[4][0]=0.754873; p1_pc2_s_dz[4][0]=0.0706346; p2_pc2_s_dz[4][0]=0.129035; p3_pc2_s_dz[4][0]=-0.00649229; p4_pc2_s_dz[4][0]=0.000151796; 
  p0_pc2_m_dz[0][5][0]=-0.672286; p1_pc2_m_dz[0][5][0]=0.0166761; p2_pc2_m_dz[0][5][0]=-0.0382093; p3_pc2_m_dz[0][5][0]=0.0106708; p4_pc2_m_dz[0][5][0]=-0.000910577; 
  p0_pc2_m_dz[1][5][0]=-0.631498; p1_pc2_m_dz[1][5][0]=-0.0947362; p2_pc2_m_dz[1][5][0]=0.0947394; p3_pc2_m_dz[1][5][0]=-0.0994749; p4_pc2_m_dz[1][5][0]=0.0315845; 
  p0_pc2_s_dz[5][0]=0.797845; p1_pc2_s_dz[5][0]=-0.0109003; p2_pc2_s_dz[5][0]=0.213333; p3_pc2_s_dz[5][0]=-0.0410129; p4_pc2_s_dz[5][0]=0.0044579; 
  p0_pc2_m_dz[0][6][0]=-0.755236; p1_pc2_m_dz[0][6][0]=-0.00582167; p2_pc2_m_dz[0][6][0]=-0.0632478; p3_pc2_m_dz[0][6][0]=0.0174288; p4_pc2_m_dz[0][6][0]=-0.00142163; 
  p0_pc2_m_dz[1][6][0]=-1.0261; p1_pc2_m_dz[1][6][0]=1.13951; p2_pc2_m_dz[1][6][0]=-0.200556; p3_pc2_m_dz[1][6][0]=-3.5931; p4_pc2_m_dz[1][6][0]=3.23229; 
  p0_pc2_s_dz[6][0]=0.767338; p1_pc2_s_dz[6][0]=-0.00210617; p2_pc2_s_dz[6][0]=0.215889; p3_pc2_s_dz[6][0]=-0.0433451; p4_pc2_s_dz[6][0]=0.00487898; 
  p0_pc2_m_dz[0][7][0]=-0.856035; p1_pc2_m_dz[0][7][0]=0.0274248; p2_pc2_m_dz[0][7][0]=-0.123035; p3_pc2_m_dz[0][7][0]=0.0332485; p4_pc2_m_dz[0][7][0]=-0.00299573; 
  p0_pc2_m_dz[1][7][0]=-1.02646; p1_pc2_m_dz[1][7][0]=0.976025; p2_pc2_m_dz[1][7][0]=-0.275782; p3_pc2_m_dz[1][7][0]=-3.30152; p4_pc2_m_dz[1][7][0]=3.0053; 
  p0_pc2_s_dz[7][0]=0.753611; p1_pc2_s_dz[7][0]=0.0281226; p2_pc2_s_dz[7][0]=0.190512; p3_pc2_s_dz[7][0]=-0.0328259; p4_pc2_s_dz[7][0]=0.00349348; 
  p0_pc2_m_dz[0][8][0]=-0.868284; p1_pc2_m_dz[0][8][0]=-0.10871; p2_pc2_m_dz[0][8][0]=-0.0289961; p3_pc2_m_dz[0][8][0]=-0.00179723; p4_pc2_m_dz[0][8][0]=0.00157067; 
  p0_pc2_m_dz[1][8][0]=-0.972262; p1_pc2_m_dz[1][8][0]=0.691303; p2_pc2_m_dz[1][8][0]=-0.185025; p3_pc2_m_dz[1][8][0]=-2.59105; p4_pc2_m_dz[1][8][0]=2.03889; 
  p0_pc2_s_dz[8][0]=0.759336; p1_pc2_s_dz[8][0]=0.0217783; p2_pc2_s_dz[8][0]=0.201668; p3_pc2_s_dz[8][0]=-0.0357243; p4_pc2_s_dz[8][0]=0.00374904; 
  p0_pc2_m_dz[0][9][0]=-1.02796; p1_pc2_m_dz[0][9][0]=-0.0579905; p2_pc2_m_dz[0][9][0]=-0.0973088; p3_pc2_m_dz[0][9][0]=0.0228898; p4_pc2_m_dz[0][9][0]=-0.00026479; 
  p0_pc2_m_dz[1][9][0]=-0.701674; p1_pc2_m_dz[1][9][0]=-0.548833; p2_pc2_m_dz[1][9][0]=0.0524756; p3_pc2_m_dz[1][9][0]=0.482321; p4_pc2_m_dz[1][9][0]=-1.05478; 
  p0_pc2_s_dz[9][0]=0.73463; p1_pc2_s_dz[9][0]=0.0406488; p2_pc2_s_dz[9][0]=0.212219; p3_pc2_s_dz[9][0]=-0.039623; p4_pc2_s_dz[9][0]=0.0040858;

  //! PC2 dz pos
  p0_pc2_m_dz[0][0][1]=-0.408369; p1_pc2_m_dz[0][0][1]=0.119279; p2_pc2_m_dz[0][0][1]=-0.139327; p3_pc2_m_dz[0][0][1]=0.0679641; p4_pc2_m_dz[0][0][1]=-0.0113158; 
  p0_pc2_m_dz[1][0][1]=-0.0364188; p1_pc2_m_dz[1][0][1]=-2.19207; p2_pc2_m_dz[1][0][1]=0.893361; p3_pc2_m_dz[1][0][1]=6.23812; p4_pc2_m_dz[1][0][1]=-5.64964; 
  p0_pc2_s_dz[0][1]=0.769805; p1_pc2_s_dz[0][1]=-0.084658; p2_pc2_s_dz[0][1]=0.329323; p3_pc2_s_dz[0][1]=-0.0712771; p4_pc2_s_dz[0][1]=0.00724734; 
  p0_pc2_m_dz[0][1][1]=-0.527101; p1_pc2_m_dz[0][1][1]=-0.0244621; p2_pc2_m_dz[0][1][1]=0.0306727; p3_pc2_m_dz[0][1][1]=0.00401378; p4_pc2_m_dz[0][1][1]=-0.00187226; 
  p0_pc2_m_dz[1][1][1]=-0.246109; p1_pc2_m_dz[1][1][1]=-1.60746; p2_pc2_m_dz[1][1][1]=0.613522; p3_pc2_m_dz[1][1][1]=4.33706; p4_pc2_m_dz[1][1][1]=-3.87497; 
  p0_pc2_s_dz[1][1]=0.762872; p1_pc2_s_dz[1][1]=-0.00675072; p2_pc2_s_dz[1][1]=0.235677; p3_pc2_s_dz[1][1]=-0.040065; p4_pc2_s_dz[1][1]=0.00362884; 
  p0_pc2_m_dz[0][2][1]=-0.664955; p1_pc2_m_dz[0][2][1]=0.0360484; p2_pc2_m_dz[0][2][1]=-0.00406104; p3_pc2_m_dz[0][2][1]=0.00874867; p4_pc2_m_dz[0][2][1]=-0.00129759; 
  p0_pc2_m_dz[1][2][1]=-0.524361; p1_pc2_m_dz[1][2][1]=-0.776185; p2_pc2_m_dz[1][2][1]=0.424425; p3_pc2_m_dz[1][2][1]=1.88146; p4_pc2_m_dz[1][2][1]=-1.71178; 
  p0_pc2_s_dz[2][1]=0.750457; p1_pc2_s_dz[2][1]=-0.00769355; p2_pc2_s_dz[2][1]=0.254077; p3_pc2_s_dz[2][1]=-0.0558195; p4_pc2_s_dz[2][1]=0.00635364; 
  p0_pc2_m_dz[0][3][1]=-0.788152; p1_pc2_m_dz[0][3][1]=0.0623687; p2_pc2_m_dz[0][3][1]=-0.0290847; p3_pc2_m_dz[0][3][1]=0.0145794; p4_pc2_m_dz[0][3][1]=-0.00172646; 
  p0_pc2_m_dz[1][3][1]=-0.55934; p1_pc2_m_dz[1][3][1]=-0.980553; p2_pc2_m_dz[1][3][1]=0.477776; p3_pc2_m_dz[1][3][1]=2.46714; p4_pc2_m_dz[1][3][1]=-2.45085; 
  p0_pc2_s_dz[3][1]=0.756609; p1_pc2_s_dz[3][1]=-0.0412318; p2_pc2_s_dz[3][1]=0.295538; p3_pc2_s_dz[3][1]=-0.0748005; p4_pc2_s_dz[3][1]=0.00896453; 
  p0_pc2_m_dz[0][4][1]=-0.897866; p1_pc2_m_dz[0][4][1]=0.0768637; p2_pc2_m_dz[0][4][1]=-0.0331108; p3_pc2_m_dz[0][4][1]=0.0146116; p4_pc2_m_dz[0][4][1]=-0.00178644; 
  p0_pc2_m_dz[1][4][1]=-0.68849; p1_pc2_m_dz[1][4][1]=-0.68136; p2_pc2_m_dz[1][4][1]=0.297316; p3_pc2_m_dz[1][4][1]=1.22146; p4_pc2_m_dz[1][4][1]=-1.03689; 
  p0_pc2_s_dz[4][1]=0.787785; p1_pc2_s_dz[4][1]=-0.0683213; p2_pc2_s_dz[4][1]=0.30845; p3_pc2_s_dz[4][1]=-0.0769756; p4_pc2_s_dz[4][1]=0.00893615; 
  p0_pc2_m_dz[0][5][1]=-0.660615; p1_pc2_m_dz[0][5][1]=0.0778995; p2_pc2_m_dz[0][5][1]=-0.0433113; p3_pc2_m_dz[0][5][1]=0.0116655; p4_pc2_m_dz[0][5][1]=-0.00118235; 
  p0_pc2_m_dz[1][5][1]=-0.691711; p1_pc2_m_dz[1][5][1]=0.229909; p2_pc2_m_dz[1][5][1]=0.011864; p3_pc2_m_dz[1][5][1]=-0.792162; p4_pc2_m_dz[1][5][1]=0.736037; 
  p0_pc2_s_dz[5][1]=0.805487; p1_pc2_s_dz[5][1]=-0.114751; p2_pc2_s_dz[5][1]=0.357969; p3_pc2_s_dz[5][1]=-0.0980603; p4_pc2_s_dz[5][1]=0.0114965; 
  p0_pc2_m_dz[0][6][1]=-0.759822; p1_pc2_m_dz[0][6][1]=0.0954088; p2_pc2_m_dz[0][6][1]=-0.0450531; p3_pc2_m_dz[0][6][1]=0.00689667; p4_pc2_m_dz[0][6][1]=-0.000181327; 
  p0_pc2_m_dz[1][6][1]=-0.463556; p1_pc2_m_dz[1][6][1]=-0.949015; p2_pc2_m_dz[1][6][1]=0.418546; p3_pc2_m_dz[1][6][1]=2.23638; p4_pc2_m_dz[1][6][1]=-2.37599; 
  p0_pc2_s_dz[6][1]=0.772333; p1_pc2_s_dz[6][1]=-0.0797252; p2_pc2_s_dz[6][1]=0.327706; p3_pc2_s_dz[6][1]=-0.0871318; p4_pc2_s_dz[6][1]=0.0104228; 
  p0_pc2_m_dz[0][7][1]=-0.831496; p1_pc2_m_dz[0][7][1]=0.118227; p2_pc2_m_dz[0][7][1]=-0.0738305; p3_pc2_m_dz[0][7][1]=0.0148547; p4_pc2_m_dz[0][7][1]=-0.0010549; 
  p0_pc2_m_dz[1][7][1]=-0.712017; p1_pc2_m_dz[1][7][1]=-0.117521; p2_pc2_m_dz[1][7][1]=0.143549; p3_pc2_m_dz[1][7][1]=-0.0427641; p4_pc2_m_dz[1][7][1]=-0.229774; 
  p0_pc2_s_dz[7][1]=0.77256; p1_pc2_s_dz[7][1]=-0.0727196; p2_pc2_s_dz[7][1]=0.322573; p3_pc2_s_dz[7][1]=-0.0844932; p4_pc2_s_dz[7][1]=0.0101122; 
  p0_pc2_m_dz[0][8][1]=-0.883631; p1_pc2_m_dz[0][8][1]=0.133778; p2_pc2_m_dz[0][8][1]=-0.0672155; p3_pc2_m_dz[0][8][1]=0.00655373; p4_pc2_m_dz[0][8][1]=0.000697197; 
  p0_pc2_m_dz[1][8][1]=-0.818026; p1_pc2_m_dz[1][8][1]=0.256109; p2_pc2_m_dz[1][8][1]=0.111483; p3_pc2_m_dz[1][8][1]=-0.981256; p4_pc2_m_dz[1][8][1]=0.288997; 
  p0_pc2_s_dz[8][1]=0.804022; p1_pc2_s_dz[8][1]=-0.166949; p2_pc2_s_dz[8][1]=0.41354; p3_pc2_s_dz[8][1]=-0.114872; p4_pc2_s_dz[8][1]=0.013691; 
  p0_pc2_m_dz[0][9][1]=-0.962219; p1_pc2_m_dz[0][9][1]=0.00327781; p2_pc2_m_dz[0][9][1]=0.0557369; p3_pc2_m_dz[0][9][1]=-0.0356459; p4_pc2_m_dz[0][9][1]=0.00679849; 
  p0_pc2_m_dz[1][9][1]=-1.14249; p1_pc2_m_dz[1][9][1]=1.41081; p2_pc2_m_dz[1][9][1]=-0.299736; p3_pc2_m_dz[1][9][1]=-4.58401; p4_pc2_m_dz[1][9][1]=3.76327; 
  p0_pc2_s_dz[9][1]=0.790496; p1_pc2_s_dz[9][1]=-0.173283; p2_pc2_s_dz[9][1]=0.442183; p3_pc2_s_dz[9][1]=-0.12076; p4_pc2_s_dz[9][1]=0.0142544;


  //! PC3 dphi neg
  p0_pc3_m_dp[0][0][0][0]=0.000453707; p1_pc3_m_dp[0][0][0][0]=0.000755869; p2_pc3_m_dp[0][0][0][0]=-0.000382436; p3_pc3_m_dp[0][0][0][0]=0.000100782; p4_pc3_m_dp[0][0][0][0]=-1.94572e-05; 
  p0_pc3_m_dp[1][0][0][0]=0.000421524; p1_pc3_m_dp[1][0][0][0]=0.00100166; p2_pc3_m_dp[1][0][0][0]=-8.70307e-05; p3_pc3_m_dp[1][0][0][0]=-0.00173539; p4_pc3_m_dp[1][0][0][0]=0.00132658; 
  p0_pc3_s_dp[0][0][0]=0.00210167; p1_pc3_s_dp[0][0][0]=-0.000298297; p2_pc3_s_dp[0][0][0]=0.000971794; p3_pc3_s_dp[0][0][0]=-0.000283478; p4_pc3_s_dp[0][0][0]=3.39541e-05; 
  p0_pc3_m_dp[0][0][1][0]=0.000475991; p1_pc3_m_dp[0][0][1][0]=0.000900543; p2_pc3_m_dp[0][0][1][0]=-0.000334351; p3_pc3_m_dp[0][0][1][0]=0.000106956; p4_pc3_m_dp[0][0][1][0]=-1.59025e-05; 
  p0_pc3_m_dp[1][0][1][0]=0.000417686; p1_pc3_m_dp[1][0][1][0]=0.000963816; p2_pc3_m_dp[1][0][1][0]=0.000291883; p3_pc3_m_dp[1][0][1][0]=-0.00128795; p4_pc3_m_dp[1][0][1][0]=0.000677648; 
  p0_pc3_s_dp[0][1][0]=0.00213864; p1_pc3_s_dp[0][1][0]=-0.000249171; p2_pc3_s_dp[0][1][0]=0.000891512; p3_pc3_s_dp[0][1][0]=-0.000244204; p4_pc3_s_dp[0][1][0]=2.78768e-05; 
  p0_pc3_m_dp[0][0][2][0]=0.000382168; p1_pc3_m_dp[0][0][2][0]=0.00137952; p2_pc3_m_dp[0][0][2][0]=-0.000602336; p3_pc3_m_dp[0][0][2][0]=0.000201004; p4_pc3_m_dp[0][0][2][0]=-2.41336e-05; 
  p0_pc3_m_dp[1][0][2][0]=0.000235731; p1_pc3_m_dp[1][0][2][0]=0.00205837; p2_pc3_m_dp[1][0][2][0]=1.14962e-05; p3_pc3_m_dp[1][0][2][0]=-0.00382221; p4_pc3_m_dp[1][0][2][0]=0.00302159; 
  p0_pc3_s_dp[0][2][0]=0.00205579; p1_pc3_s_dp[0][2][0]=4.07872e-05; p2_pc3_s_dp[0][2][0]=0.000600617; p3_pc3_s_dp[0][2][0]=-0.000143396; p4_pc3_s_dp[0][2][0]=1.68528e-05; 
  p0_pc3_m_dp[0][0][3][0]=0.000447985; p1_pc3_m_dp[0][0][3][0]=0.0014986; p2_pc3_m_dp[0][0][3][0]=-0.000595369; p3_pc3_m_dp[0][0][3][0]=0.000187518; p4_pc3_m_dp[0][0][3][0]=-2.02037e-05; 
  p0_pc3_m_dp[1][0][3][0]=0.00117246; p1_pc3_m_dp[1][0][3][0]=-0.000834596; p2_pc3_m_dp[1][0][3][0]=0.000508558; p3_pc3_m_dp[1][0][3][0]=0.00370151; p4_pc3_m_dp[1][0][3][0]=-0.00367188; 
  p0_pc3_s_dp[0][3][0]=0.002036; p1_pc3_s_dp[0][3][0]=0.000198463; p2_pc3_s_dp[0][3][0]=0.000452218; p3_pc3_s_dp[0][3][0]=-8.5212e-05; p4_pc3_s_dp[0][3][0]=9.61918e-06; 
  p0_pc3_m_dp[0][0][4][0]=0.0006176; p1_pc3_m_dp[0][0][4][0]=0.00111558; p2_pc3_m_dp[0][0][4][0]=-0.00024579; p3_pc3_m_dp[0][0][4][0]=6.94778e-05; p4_pc3_m_dp[0][0][4][0]=-6.24036e-06; 
  p0_pc3_m_dp[1][0][4][0]=0.00124542; p1_pc3_m_dp[1][0][4][0]=-0.00104296; p2_pc3_m_dp[1][0][4][0]=0.000610819; p3_pc3_m_dp[1][0][4][0]=0.00448969; p4_pc3_m_dp[1][0][4][0]=-0.00452573; 
  p0_pc3_s_dp[0][4][0]=0.00196124; p1_pc3_s_dp[0][4][0]=0.000456118; p2_pc3_s_dp[0][4][0]=0.000220919; p3_pc3_s_dp[0][4][0]=-2.84342e-06; p4_pc3_s_dp[0][4][0]=8.69443e-08; 
  p0_pc3_m_dp[0][0][5][0]=1.97007e-05; p1_pc3_m_dp[0][0][5][0]=0.00202921; p2_pc3_m_dp[0][0][5][0]=-0.00116631; p3_pc3_m_dp[0][0][5][0]=0.000401045; p4_pc3_m_dp[0][0][5][0]=-4.47491e-05; 
  p0_pc3_m_dp[1][0][5][0]=0.000485149; p1_pc3_m_dp[1][0][5][0]=0.00150628; p2_pc3_m_dp[1][0][5][0]=-0.00037716; p3_pc3_m_dp[1][0][5][0]=-0.0034396; p4_pc3_m_dp[1][0][5][0]=0.00354551; 
  p0_pc3_s_dp[0][5][0]=0.00159719; p1_pc3_s_dp[0][5][0]=0.00106773; p2_pc3_s_dp[0][5][0]=6.81591e-05; p3_pc3_s_dp[0][5][0]=-5.23374e-05; p4_pc3_s_dp[0][5][0]=1.38283e-05; 
  p0_pc3_m_dp[0][0][6][0]=1.54532e-06; p1_pc3_m_dp[0][0][6][0]=0.00204233; p2_pc3_m_dp[0][0][6][0]=-0.00129806; p3_pc3_m_dp[0][0][6][0]=0.000456746; p4_pc3_m_dp[0][0][6][0]=-5.27949e-05; 
  p0_pc3_m_dp[1][0][6][0]=0.000756489; p1_pc3_m_dp[1][0][6][0]=0.000341144; p2_pc3_m_dp[1][0][6][0]=-0.000104028; p3_pc3_m_dp[1][0][6][0]=-0.000283441; p4_pc3_m_dp[1][0][6][0]=0.000415053; 
  p0_pc3_s_dp[0][6][0]=0.00161191; p1_pc3_s_dp[0][6][0]=0.00110752; p2_pc3_s_dp[0][6][0]=-8.14578e-05; p3_pc3_s_dp[0][6][0]=2.85804e-05; p4_pc3_s_dp[0][6][0]=1.82158e-06; 
  p0_pc3_m_dp[0][0][7][0]=0.000133221; p1_pc3_m_dp[0][0][7][0]=0.00187778; p2_pc3_m_dp[0][0][7][0]=-0.00135369; p3_pc3_m_dp[0][0][7][0]=0.000478942; p4_pc3_m_dp[0][0][7][0]=-5.63618e-05; 
  p0_pc3_m_dp[1][0][7][0]=0.00201963; p1_pc3_m_dp[1][0][7][0]=-0.00443155; p2_pc3_m_dp[1][0][7][0]=0.00142894; p3_pc3_m_dp[1][0][7][0]=0.0119821; p4_pc3_m_dp[1][0][7][0]=-0.0118681; 
  p0_pc3_s_dp[0][7][0]=0.00165448; p1_pc3_s_dp[0][7][0]=0.00126067; p2_pc3_s_dp[0][7][0]=-0.000328198; p3_pc3_s_dp[0][7][0]=0.000126419; p4_pc3_s_dp[0][7][0]=-1.04938e-05; 
  p0_pc3_m_dp[0][0][8][0]=0.000343478; p1_pc3_m_dp[0][0][8][0]=0.00137988; p2_pc3_m_dp[0][0][8][0]=-0.00110798; p3_pc3_m_dp[0][0][8][0]=0.000380032; p4_pc3_m_dp[0][0][8][0]=-4.61091e-05; 
  p0_pc3_m_dp[1][0][8][0]=0.00206843; p1_pc3_m_dp[1][0][8][0]=-0.00389863; p2_pc3_m_dp[1][0][8][0]=0.000543097; p3_pc3_m_dp[1][0][8][0]=0.00922699; p4_pc3_m_dp[1][0][8][0]=-0.00792204; 
  p0_pc3_s_dp[0][8][0]=0.0018672; p1_pc3_s_dp[0][8][0]=0.000795334; p2_pc3_s_dp[0][8][0]=-3.43924e-06; p3_pc3_s_dp[0][8][0]=3.58173e-05; p4_pc3_s_dp[0][8][0]=-2.32526e-06; 
  p0_pc3_m_dp[0][0][9][0]=0.000491386; p1_pc3_m_dp[0][0][9][0]=0.00109562; p2_pc3_m_dp[0][0][9][0]=-0.00105481; p3_pc3_m_dp[0][0][9][0]=0.000345064; p4_pc3_m_dp[0][0][9][0]=-4.68349e-05; 
  p0_pc3_m_dp[1][0][9][0]=0.00337384; p1_pc3_m_dp[1][0][9][0]=-0.00851735; p2_pc3_m_dp[1][0][9][0]=0.00139; p3_pc3_m_dp[1][0][9][0]=0.0213734; p4_pc3_m_dp[1][0][9][0]=-0.019247; 
  p0_pc3_s_dp[0][9][0]=0.00195621; p1_pc3_s_dp[0][9][0]=0.000484226; p2_pc3_s_dp[0][9][0]=0.00027776; p3_pc3_s_dp[0][9][0]=-6.37637e-05; p4_pc3_s_dp[0][9][0]=9.62174e-06; 
  p0_pc3_m_dp[0][1][0][0]=-0.000625415; p1_pc3_m_dp[0][1][0][0]=0.000725031; p2_pc3_m_dp[0][1][0][0]=-0.000529778; p3_pc3_m_dp[0][1][0][0]=0.00013782; p4_pc3_m_dp[0][1][0][0]=-2.1544e-05; 
  p0_pc3_m_dp[1][1][0][0]=-0.000288437; p1_pc3_m_dp[1][1][0][0]=0.000877963; p2_pc3_m_dp[1][1][0][0]=-0.000873749; p3_pc3_m_dp[1][1][0][0]=-0.00295253; p4_pc3_m_dp[1][1][0][0]=0.00348206; 
  p0_pc3_s_dp[1][0][0]=0.00147738; p1_pc3_s_dp[1][0][0]=0.002669; p2_pc3_s_dp[1][0][0]=-0.00151599; p3_pc3_s_dp[1][0][0]=0.000549857; p4_pc3_s_dp[1][0][0]=-6.0864e-05; 
  p0_pc3_m_dp[0][1][1][0]=-0.000592458; p1_pc3_m_dp[0][1][1][0]=0.000805517; p2_pc3_m_dp[0][1][1][0]=-0.00041493; p3_pc3_m_dp[0][1][1][0]=0.0001221; p4_pc3_m_dp[0][1][1][0]=-1.56369e-05; 
  p0_pc3_m_dp[1][1][1][0]=-0.00107747; p1_pc3_m_dp[1][1][1][0]=0.00406999; p2_pc3_m_dp[1][1][1][0]=-0.00141406; p3_pc3_m_dp[1][1][1][0]=-0.0121007; p4_pc3_m_dp[1][1][1][0]=0.0122746; 
  p0_pc3_s_dp[1][1][0]=0.00149102; p1_pc3_s_dp[1][1][0]=0.00228187; p2_pc3_s_dp[1][1][0]=-0.00109118; p3_pc3_s_dp[1][1][0]=0.000402074; p4_pc3_s_dp[1][1][0]=-4.45041e-05; 
  p0_pc3_m_dp[0][1][2][0]=-0.000526083; p1_pc3_m_dp[0][1][2][0]=0.0006802; p2_pc3_m_dp[0][1][2][0]=-0.000299597; p3_pc3_m_dp[0][1][2][0]=0.000128771; p4_pc3_m_dp[0][1][2][0]=-1.75035e-05; 
  p0_pc3_m_dp[1][1][2][0]=-0.000129694; p1_pc3_m_dp[1][1][2][0]=0.000210898; p2_pc3_m_dp[1][1][2][0]=-0.000356518; p3_pc3_m_dp[1][1][2][0]=-0.000743943; p4_pc3_m_dp[1][1][2][0]=0.00107773; 
  p0_pc3_s_dp[1][2][0]=0.00157085; p1_pc3_s_dp[1][2][0]=0.00168765; p2_pc3_s_dp[1][2][0]=-0.000407907; p3_pc3_s_dp[1][2][0]=0.000139033; p4_pc3_s_dp[1][2][0]=-1.19745e-05; 
  p0_pc3_m_dp[0][1][3][0]=-0.000622642; p1_pc3_m_dp[0][1][3][0]=0.00101703; p2_pc3_m_dp[0][1][3][0]=-0.00040319; p3_pc3_m_dp[0][1][3][0]=0.000140907; p4_pc3_m_dp[0][1][3][0]=-1.55564e-05; 
  p0_pc3_m_dp[1][1][3][0]=0.00134717; p1_pc3_m_dp[1][1][3][0]=-0.00536744; p2_pc3_m_dp[1][1][3][0]=0.00101833; p3_pc3_m_dp[1][1][3][0]=0.0145613; p4_pc3_m_dp[1][1][3][0]=-0.0132184; 
  p0_pc3_s_dp[1][3][0]=0.00187898; p1_pc3_s_dp[1][3][0]=0.000381664; p2_pc3_s_dp[1][3][0]=0.000959581; p3_pc3_s_dp[1][3][0]=-0.000432355; p4_pc3_s_dp[1][3][0]=7.21316e-05; 
  p0_pc3_m_dp[0][1][4][0]=-0.000545298; p1_pc3_m_dp[0][1][4][0]=0.00082788; p2_pc3_m_dp[0][1][4][0]=-0.000179004; p3_pc3_m_dp[0][1][4][0]=7.74086e-05; p4_pc3_m_dp[0][1][4][0]=-9.97234e-06; 
  p0_pc3_m_dp[1][1][4][0]=0.000776223; p1_pc3_m_dp[1][1][4][0]=-0.0030415; p2_pc3_m_dp[1][1][4][0]=0.000434173; p3_pc3_m_dp[1][1][4][0]=0.00813207; p4_pc3_m_dp[1][1][4][0]=-0.00702615; 
  p0_pc3_s_dp[1][4][0]=0.00166256; p1_pc3_s_dp[1][4][0]=0.00110319; p2_pc3_s_dp[1][4][0]=0.000102109; p3_pc3_s_dp[1][4][0]=-2.37967e-05; p4_pc3_s_dp[1][4][0]=6.34488e-06; 
  p0_pc3_m_dp[0][1][5][0]=-0.000664713; p1_pc3_m_dp[0][1][5][0]=0.00158635; p2_pc3_m_dp[0][1][5][0]=-0.000836482; p3_pc3_m_dp[0][1][5][0]=0.000302807; p4_pc3_m_dp[0][1][5][0]=-3.55782e-05; 
  p0_pc3_m_dp[1][1][5][0]=0.00100367; p1_pc3_m_dp[1][1][5][0]=-0.00334275; p2_pc3_m_dp[1][1][5][0]=0.000184538; p3_pc3_m_dp[1][1][5][0]=0.00879643; p4_pc3_m_dp[1][1][5][0]=-0.00687311; 
  p0_pc3_s_dp[1][5][0]=0.00170994; p1_pc3_s_dp[1][5][0]=0.00131299; p2_pc3_s_dp[1][5][0]=-0.000115335; p3_pc3_s_dp[1][5][0]=4.48825e-05; p4_pc3_s_dp[1][5][0]=8.64168e-07; 
  p0_pc3_m_dp[0][1][6][0]=-0.000455356; p1_pc3_m_dp[0][1][6][0]=0.000943245; p2_pc3_m_dp[0][1][6][0]=-0.000397041; p3_pc3_m_dp[0][1][6][0]=0.000170555; p4_pc3_m_dp[0][1][6][0]=-2.20927e-05; 
  p0_pc3_m_dp[1][1][6][0]=0.0011478; p1_pc3_m_dp[1][1][6][0]=-0.00413064; p2_pc3_m_dp[1][1][6][0]=0.000570899; p3_pc3_m_dp[1][1][6][0]=0.0107129; p4_pc3_m_dp[1][1][6][0]=-0.00908491; 
  p0_pc3_s_dp[1][6][0]=0.00151815; p1_pc3_s_dp[1][6][0]=0.00200634; p2_pc3_s_dp[1][6][0]=-0.000853434; p3_pc3_s_dp[1][6][0]=0.000328057; p4_pc3_s_dp[1][6][0]=-3.36458e-05; 
  p0_pc3_m_dp[0][1][7][0]=-0.000542609; p1_pc3_m_dp[0][1][7][0]=0.000743336; p2_pc3_m_dp[0][1][7][0]=-0.000366738; p3_pc3_m_dp[0][1][7][0]=0.000157125; p4_pc3_m_dp[0][1][7][0]=-2.08485e-05; 
  p0_pc3_m_dp[1][1][7][0]=0.000467722; p1_pc3_m_dp[1][1][7][0]=-0.0020297; p2_pc3_m_dp[1][1][7][0]=-7.95569e-06; p3_pc3_m_dp[1][1][7][0]=0.00478239; p4_pc3_m_dp[1][1][7][0]=-0.00356954; 
  p0_pc3_s_dp[1][7][0]=0.00143451; p1_pc3_s_dp[1][7][0]=0.00205276; p2_pc3_s_dp[1][7][0]=-0.00088312; p3_pc3_s_dp[1][7][0]=0.000335898; p4_pc3_s_dp[1][7][0]=-3.46301e-05; 
  p0_pc3_m_dp[0][1][8][0]=-0.000531929; p1_pc3_m_dp[0][1][8][0]=0.000127226; p2_pc3_m_dp[0][1][8][0]=2.6673e-05; p3_pc3_m_dp[0][1][8][0]=-3.26551e-06; p4_pc3_m_dp[0][1][8][0]=-8.72038e-07; 
  p0_pc3_m_dp[1][1][8][0]=-0.000140885; p1_pc3_m_dp[1][1][8][0]=-0.000397705; p2_pc3_m_dp[1][1][8][0]=-0.000454943; p3_pc3_m_dp[1][1][8][0]=-9.23744e-05; p4_pc3_m_dp[1][1][8][0]=0.00103604; 
  p0_pc3_s_dp[1][8][0]=0.00184336; p1_pc3_s_dp[1][8][0]=0.000598379; p2_pc3_s_dp[1][8][0]=0.000621719; p3_pc3_s_dp[1][8][0]=-0.000309088; p4_pc3_s_dp[1][8][0]=6.11836e-05; 
  p0_pc3_m_dp[0][1][9][0]=-0.000549439; p1_pc3_m_dp[0][1][9][0]=-0.000220755; p2_pc3_m_dp[0][1][9][0]=0.00012925; p3_pc3_m_dp[0][1][9][0]=-4.15786e-05; p4_pc3_m_dp[0][1][9][0]=-3.09872e-06; 
  p0_pc3_m_dp[1][1][9][0]=-0.000474181; p1_pc3_m_dp[1][1][9][0]=0.000260995; p2_pc3_m_dp[1][1][9][0]=-0.000631141; p3_pc3_m_dp[1][1][9][0]=-0.00157797; p4_pc3_m_dp[1][1][9][0]=0.00203533; 
  p0_pc3_s_dp[1][9][0]=0.0016281; p1_pc3_s_dp[1][9][0]=0.00140259; p2_pc3_s_dp[1][9][0]=-0.000454256; p3_pc3_s_dp[1][9][0]=0.000217538; p4_pc3_s_dp[1][9][0]=-2.37418e-05;
  
  //! PC3 dphi pos
  p0_pc3_m_dp[0][0][0][1]=0.000145066; p1_pc3_m_dp[0][0][0][1]=0.00053141; p2_pc3_m_dp[0][0][0][1]=-0.000401046; p3_pc3_m_dp[0][0][0][1]=0.000103883; p4_pc3_m_dp[0][0][0][1]=-4.24725e-07; 
  p0_pc3_m_dp[1][0][0][1]=-0.00045814; p1_pc3_m_dp[1][0][0][1]=0.00250504; p2_pc3_m_dp[1][0][0][1]=-9.98462e-06; p3_pc3_m_dp[1][0][0][1]=-0.00596885; p4_pc3_m_dp[1][0][0][1]=0.00468947; 
  p0_pc3_s_dp[0][0][1]=0.00234093; p1_pc3_s_dp[0][0][1]=-0.00120518; p2_pc3_s_dp[0][0][1]=0.00195227; p3_pc3_s_dp[0][0][1]=-0.000588476; p4_pc3_s_dp[0][0][1]=5.82266e-05; 
  p0_pc3_m_dp[0][0][1][1]=0.000304789; p1_pc3_m_dp[0][0][1][1]=-9.32108e-05; p2_pc3_m_dp[0][0][1][1]=-0.000121055; p3_pc3_m_dp[0][0][1][1]=2.20604e-05; p4_pc3_m_dp[0][0][1][1]=1.63697e-06; 
  p0_pc3_m_dp[1][0][1][1]=-0.000854272; p1_pc3_m_dp[1][0][1][1]=0.00290342; p2_pc3_m_dp[1][0][1][1]=0.000533001; p3_pc3_m_dp[1][0][1][1]=-0.00604998; p4_pc3_m_dp[1][0][1][1]=0.00335273; 
  p0_pc3_s_dp[0][1][1]=0.00228664; p1_pc3_s_dp[0][1][1]=-0.00113817; p2_pc3_s_dp[0][1][1]=0.00196194; p3_pc3_s_dp[0][1][1]=-0.000592884; p4_pc3_s_dp[0][1][1]=5.7889e-05; 
  p0_pc3_m_dp[0][0][2][1]=0.00052807; p1_pc3_m_dp[0][0][2][1]=-0.00075362; p2_pc3_m_dp[0][0][2][1]=0.000265007; p3_pc3_m_dp[0][0][2][1]=-9.41144e-05; p4_pc3_m_dp[0][0][2][1]=1.10216e-05; 
  p0_pc3_m_dp[1][0][2][1]=-0.00127392; p1_pc3_m_dp[1][0][2][1]=0.00424332; p2_pc3_m_dp[1][0][2][1]=0.000140619; p3_pc3_m_dp[1][0][2][1]=-0.0093818; p4_pc3_m_dp[1][0][2][1]=0.00637969; 
  p0_pc3_s_dp[0][2][1]=0.00207452; p1_pc3_s_dp[0][2][1]=-0.000574785; p2_pc3_s_dp[0][2][1]=0.00144507; p3_pc3_s_dp[0][2][1]=-0.000406279; p4_pc3_s_dp[0][2][1]=3.55679e-05; 
  p0_pc3_m_dp[0][0][3][1]=0.000576516; p1_pc3_m_dp[0][0][3][1]=-0.000831536; p2_pc3_m_dp[0][0][3][1]=0.000214241; p3_pc3_m_dp[0][0][3][1]=-6.71859e-05; p4_pc3_m_dp[0][0][3][1]=6.23819e-06; 
  p0_pc3_m_dp[1][0][3][1]=-0.0016859; p1_pc3_m_dp[1][0][3][1]=0.00599944; p2_pc3_m_dp[1][0][3][1]=-0.000452477; p3_pc3_m_dp[1][0][3][1]=-0.0144747; p4_pc3_m_dp[1][0][3][1]=0.0114595; 
  p0_pc3_s_dp[0][3][1]=0.00222889; p1_pc3_s_dp[0][3][1]=-0.00106555; p2_pc3_s_dp[0][3][1]=0.00196863; p3_pc3_s_dp[0][3][1]=-0.000597536; p4_pc3_s_dp[0][3][1]=5.77245e-05; 
  p0_pc3_m_dp[0][0][4][1]=0.000493116; p1_pc3_m_dp[0][0][4][1]=-0.000638678; p2_pc3_m_dp[0][0][4][1]=6.98175e-06; p3_pc3_m_dp[0][0][4][1]=6.68803e-06; p4_pc3_m_dp[0][0][4][1]=-2.56011e-06; 
  p0_pc3_m_dp[1][0][4][1]=-0.00183138; p1_pc3_m_dp[1][0][4][1]=0.00642994; p2_pc3_m_dp[1][0][4][1]=-0.000584708; p3_pc3_m_dp[1][0][4][1]=-0.0155533; p4_pc3_m_dp[1][0][4][1]=0.0125426; 
  p0_pc3_s_dp[0][4][1]=0.00195791; p1_pc3_s_dp[0][4][1]=-0.000209907; p2_pc3_s_dp[0][4][1]=0.00117168; p3_pc3_s_dp[0][4][1]=-0.000302044; p4_pc3_s_dp[0][4][1]=2.135e-05; 
  p0_pc3_m_dp[0][0][5][1]=0.000348402; p1_pc3_m_dp[0][0][5][1]=-0.000617714; p2_pc3_m_dp[0][0][5][1]=-1.4628e-05; p3_pc3_m_dp[0][0][5][1]=3.89892e-05; p4_pc3_m_dp[0][0][5][1]=-9.02827e-06; 
  p0_pc3_m_dp[1][0][5][1]=-0.00123171; p1_pc3_m_dp[1][0][5][1]=0.00310986; p2_pc3_m_dp[1][0][5][1]=0.000583983; p3_pc3_m_dp[1][0][5][1]=-0.00619258; p4_pc3_m_dp[1][0][5][1]=0.00328687; 
  p0_pc3_s_dp[0][5][1]=0.00189748; p1_pc3_s_dp[0][5][1]=-9.00647e-05; p2_pc3_s_dp[0][5][1]=0.00120552; p3_pc3_s_dp[0][5][1]=-0.000356589; p4_pc3_s_dp[0][5][1]=3.12351e-05; 
  p0_pc3_m_dp[0][0][6][1]=0.000317541; p1_pc3_m_dp[0][0][6][1]=-0.000453805; p2_pc3_m_dp[0][0][6][1]=2.42388e-05; p3_pc3_m_dp[0][0][6][1]=8.36227e-06; p4_pc3_m_dp[0][0][6][1]=-4.43914e-06; 
  p0_pc3_m_dp[1][0][6][1]=-0.00196071; p1_pc3_m_dp[1][0][6][1]=0.00635485; p2_pc3_m_dp[1][0][6][1]=-0.000429201; p3_pc3_m_dp[1][0][6][1]=-0.0151203; p4_pc3_m_dp[1][0][6][1]=0.0122421; 
  p0_pc3_s_dp[0][6][1]=0.00192998; p1_pc3_s_dp[0][6][1]=-0.000169481; p2_pc3_s_dp[0][6][1]=0.00128042; p3_pc3_s_dp[0][6][1]=-0.000392834; p4_pc3_s_dp[0][6][1]=3.67292e-05; 
  p0_pc3_m_dp[0][0][7][1]=6.79394e-05; p1_pc3_m_dp[0][0][7][1]=0.000653429; p2_pc3_m_dp[0][0][7][1]=-0.000718009; p3_pc3_m_dp[0][0][7][1]=0.000242395; p4_pc3_m_dp[0][0][7][1]=-2.86764e-05; 
  p0_pc3_m_dp[1][0][7][1]=-0.00143263; p1_pc3_m_dp[1][0][7][1]=0.00459491; p2_pc3_m_dp[1][0][7][1]=0.000439412; p3_pc3_m_dp[1][0][7][1]=-0.00966008; p4_pc3_m_dp[1][0][7][1]=0.00657603; 
  p0_pc3_s_dp[0][7][1]=0.00221832; p1_pc3_s_dp[0][7][1]=-0.000881509; p2_pc3_s_dp[0][7][1]=0.0018633; p3_pc3_s_dp[0][7][1]=-0.000597408; p4_pc3_s_dp[0][7][1]=6.14414e-05; 
  p0_pc3_m_dp[0][0][8][1]=-5.17317e-05; p1_pc3_m_dp[0][0][8][1]=0.00155521; p2_pc3_m_dp[0][0][8][1]=-0.00119972; p3_pc3_m_dp[0][0][8][1]=0.000374632; p4_pc3_m_dp[0][0][8][1]=-3.88728e-05; 
  p0_pc3_m_dp[1][0][8][1]=-0.00121154; p1_pc3_m_dp[1][0][8][1]=0.00489566; p2_pc3_m_dp[1][0][8][1]=0.000118722; p3_pc3_m_dp[1][0][8][1]=-0.0106011; p4_pc3_m_dp[1][0][8][1]=0.00817845; 
  p0_pc3_s_dp[0][8][1]=0.00248416; p1_pc3_s_dp[0][8][1]=-0.00172546; p2_pc3_s_dp[0][8][1]=0.00300821; p3_pc3_s_dp[0][8][1]=-0.00122543; p4_pc3_s_dp[0][8][1]=0.000172769; 
  p0_pc3_m_dp[0][0][9][1]=-0.00010333; p1_pc3_m_dp[0][0][9][1]=0.00233865; p2_pc3_m_dp[0][0][9][1]=-0.00164307; p3_pc3_m_dp[0][0][9][1]=0.000519902; p4_pc3_m_dp[0][0][9][1]=-4.9612e-05; 
  p0_pc3_m_dp[1][0][9][1]=0.000580871; p1_pc3_m_dp[1][0][9][1]=-0.000357207; p2_pc3_m_dp[1][0][9][1]=0.000943909; p3_pc3_m_dp[1][0][9][1]=0.00275164; p4_pc3_m_dp[1][0][9][1]=-0.00345201; 
  p0_pc3_s_dp[0][9][1]=0.00196227; p1_pc3_s_dp[0][9][1]=0.000359909; p2_pc3_s_dp[0][9][1]=0.000573714; p3_pc3_s_dp[0][9][1]=-0.00015438; p4_pc3_s_dp[0][9][1]=1.28867e-05; 
  p0_pc3_m_dp[0][1][0][1]=-0.000755378; p1_pc3_m_dp[0][1][0][1]=-0.000374554; p2_pc3_m_dp[0][1][0][1]=0.000437844; p3_pc3_m_dp[0][1][0][1]=-0.000195949; p4_pc3_m_dp[0][1][0][1]=3.72676e-05; 
  p0_pc3_m_dp[1][1][0][1]=-0.00257205; p1_pc3_m_dp[1][1][0][1]=0.00452408; p2_pc3_m_dp[1][1][0][1]=0.000816405; p3_pc3_m_dp[1][1][0][1]=-0.00999941; p4_pc3_m_dp[1][1][0][1]=0.00654702; 
  p0_pc3_s_dp[1][0][1]=0.00304374; p1_pc3_s_dp[1][0][1]=-0.00104046; p2_pc3_s_dp[1][0][1]=0.0015084; p3_pc3_s_dp[1][0][1]=-0.000357855; p4_pc3_s_dp[1][0][1]=2.29939e-05; 
  p0_pc3_m_dp[0][1][1][1]=-0.00065918; p1_pc3_m_dp[0][1][1][1]=-0.00114579; p2_pc3_m_dp[0][1][1][1]=0.000969052; p3_pc3_m_dp[0][1][1][1]=-0.00040329; p4_pc3_m_dp[0][1][1][1]=5.69086e-05; 
  p0_pc3_m_dp[1][1][1][1]=-0.00536881; p1_pc3_m_dp[1][1][1][1]=0.0131402; p2_pc3_m_dp[1][1][1][1]=-7.11379e-05; p3_pc3_m_dp[1][1][1][1]=-0.0307155; p4_pc3_m_dp[1][1][1][1]=0.0234186; 
  p0_pc3_s_dp[1][1][1]=0.00318015; p1_pc3_s_dp[1][1][1]=-0.00164987; p2_pc3_s_dp[1][1][1]=0.00204746; p3_pc3_s_dp[1][1][1]=-0.000531715; p4_pc3_s_dp[1][1][1]=4.1757e-05; 
  p0_pc3_m_dp[0][1][2][1]=-0.000666344; p1_pc3_m_dp[0][1][2][1]=-0.00155355; p2_pc3_m_dp[0][1][2][1]=0.00107697; p3_pc3_m_dp[0][1][2][1]=-0.000410721; p4_pc3_m_dp[0][1][2][1]=5.26387e-05; 
  p0_pc3_m_dp[1][1][2][1]=-0.00708948; p1_pc3_m_dp[1][1][2][1]=0.0191298; p2_pc3_m_dp[1][1][2][1]=-0.00203238; p3_pc3_m_dp[1][1][2][1]=-0.046668; p4_pc3_m_dp[1][1][2][1]=0.0391993; 
  p0_pc3_s_dp[1][2][1]=0.00326606; p1_pc3_s_dp[1][2][1]=-0.00187232; p2_pc3_s_dp[1][2][1]=0.00213522; p3_pc3_s_dp[1][2][1]=-0.000524876; p4_pc3_s_dp[1][2][1]=3.77023e-05; 
  p0_pc3_m_dp[0][1][3][1]=-0.000512535; p1_pc3_m_dp[0][1][3][1]=-0.00176594; p2_pc3_m_dp[0][1][3][1]=0.00110897; p3_pc3_m_dp[0][1][3][1]=-0.000414379; p4_pc3_m_dp[0][1][3][1]=5.15679e-05; 
  p0_pc3_m_dp[1][1][3][1]=-0.00545148; p1_pc3_m_dp[1][1][3][1]=0.014711; p2_pc3_m_dp[1][1][3][1]=-0.00224816; p3_pc3_m_dp[1][1][3][1]=-0.0371087; p4_pc3_m_dp[1][1][3][1]=0.032203; 
  p0_pc3_s_dp[1][3][1]=0.0022293; p1_pc3_s_dp[1][3][1]=0.000352071; p2_pc3_s_dp[1][3][1]=0.000278218; p3_pc3_s_dp[1][3][1]=0.000108642; p4_pc3_s_dp[1][3][1]=-3.53208e-05; 
  p0_pc3_m_dp[0][1][4][1]=-0.000704226; p1_pc3_m_dp[0][1][4][1]=-0.00170997; p2_pc3_m_dp[0][1][4][1]=0.00103409; p3_pc3_m_dp[0][1][4][1]=-0.000386341; p4_pc3_m_dp[0][1][4][1]=4.76537e-05; 
  p0_pc3_m_dp[1][1][4][1]=-0.00728712; p1_pc3_m_dp[1][1][4][1]=0.020933; p2_pc3_m_dp[1][1][4][1]=-0.00388827; p3_pc3_m_dp[1][1][4][1]=-0.0534572; p4_pc3_m_dp[1][1][4][1]=0.0477942; 
  p0_pc3_s_dp[1][4][1]=0.00308396; p1_pc3_s_dp[1][4][1]=-0.00176372; p2_pc3_s_dp[1][4][1]=0.00212016; p3_pc3_s_dp[1][4][1]=-0.000528727; p4_pc3_s_dp[1][4][1]=3.86206e-05; 
  p0_pc3_m_dp[0][1][5][1]=-0.000325086; p1_pc3_m_dp[0][1][5][1]=-0.00187836; p2_pc3_m_dp[0][1][5][1]=0.00111637; p3_pc3_m_dp[0][1][5][1]=-0.000378099; p4_pc3_m_dp[0][1][5][1]=4.26463e-05; 
  p0_pc3_m_dp[1][1][5][1]=-0.00691766; p1_pc3_m_dp[1][1][5][1]=0.0209288; p2_pc3_m_dp[1][1][5][1]=-0.00455643; p3_pc3_m_dp[1][1][5][1]=-0.0539725; p4_pc3_m_dp[1][1][5][1]=0.0497171; 
  p0_pc3_s_dp[1][5][1]=0.00290142; p1_pc3_s_dp[1][5][1]=-0.00153304; p2_pc3_s_dp[1][5][1]=0.00207769; p3_pc3_s_dp[1][5][1]=-0.000539941; p4_pc3_s_dp[1][5][1]=4.11871e-05; 
  p0_pc3_m_dp[0][1][6][1]=-0.000295342; p1_pc3_m_dp[0][1][6][1]=-0.00166923; p2_pc3_m_dp[0][1][6][1]=0.000952007; p3_pc3_m_dp[0][1][6][1]=-0.000310742; p4_pc3_m_dp[0][1][6][1]=3.41539e-05; 
  p0_pc3_m_dp[1][1][6][1]=-0.00483276; p1_pc3_m_dp[1][1][6][1]=0.0133009; p2_pc3_m_dp[1][1][6][1]=-0.00226763; p3_pc3_m_dp[1][1][6][1]=-0.0339835; p4_pc3_m_dp[1][1][6][1]=0.0301971; 
  p0_pc3_s_dp[1][6][1]=0.00222678; p1_pc3_s_dp[1][6][1]=0.000165035; p2_pc3_s_dp[1][6][1]=0.000577427; p3_pc3_s_dp[1][6][1]=-1.123e-05; p4_pc3_s_dp[1][6][1]=-2.11212e-05; 
  p0_pc3_m_dp[0][1][7][1]=-0.000412279; p1_pc3_m_dp[0][1][7][1]=-0.00135625; p2_pc3_m_dp[0][1][7][1]=0.000804064; p3_pc3_m_dp[0][1][7][1]=-0.000261457; p4_pc3_m_dp[0][1][7][1]=2.98033e-05; 
  p0_pc3_m_dp[1][1][7][1]=-0.0045267; p1_pc3_m_dp[1][1][7][1]=0.0120094; p2_pc3_m_dp[1][1][7][1]=-0.00155102; p3_pc3_m_dp[1][1][7][1]=-0.0302589; p4_pc3_m_dp[1][1][7][1]=0.0259947; 
  p0_pc3_s_dp[1][7][1]=0.00283698; p1_pc3_s_dp[1][7][1]=-0.00160448; p2_pc3_s_dp[1][7][1]=0.00210635; p3_pc3_s_dp[1][7][1]=-0.000535352; p4_pc3_s_dp[1][7][1]=3.96167e-05; 
  p0_pc3_m_dp[0][1][8][1]=-0.000402798; p1_pc3_m_dp[0][1][8][1]=-0.0014801; p2_pc3_m_dp[0][1][8][1]=0.0010701; p3_pc3_m_dp[0][1][8][1]=-0.000356613; p4_pc3_m_dp[0][1][8][1]=4.3699e-05; 
  p0_pc3_m_dp[1][1][8][1]=-0.00213607; p1_pc3_m_dp[1][1][8][1]=0.00324845; p2_pc3_m_dp[1][1][8][1]=0.000271693; p3_pc3_m_dp[1][1][8][1]=-0.00731221; p4_pc3_m_dp[1][1][8][1]=0.00499745; 
  p0_pc3_s_dp[1][8][1]=0.0027314; p1_pc3_s_dp[1][8][1]=-0.00146315; p2_pc3_s_dp[1][8][1]=0.00203017; p3_pc3_s_dp[1][8][1]=-0.000542676; p4_pc3_s_dp[1][8][1]=4.44132e-05; 
  p0_pc3_m_dp[0][1][9][1]=-0.000371791; p1_pc3_m_dp[0][1][9][1]=-0.0014041; p2_pc3_m_dp[0][1][9][1]=0.00117296; p3_pc3_m_dp[0][1][9][1]=-0.000392679; p4_pc3_m_dp[0][1][9][1]=5.36481e-05; 
  p0_pc3_m_dp[1][1][9][1]=-0.000541888; p1_pc3_m_dp[1][1][9][1]=-0.00149645; p2_pc3_m_dp[1][1][9][1]=0.000460934; p3_pc3_m_dp[1][1][9][1]=0.00388165; p4_pc3_m_dp[1][1][9][1]=-0.00365124; 
  p0_pc3_s_dp[1][9][1]=0.00214251; p1_pc3_s_dp[1][9][1]=4.01795e-05; p2_pc3_s_dp[1][9][1]=0.000722631; p3_pc3_s_dp[1][9][1]=-0.00010408; p4_pc3_s_dp[1][9][1]=-4.704e-06;
  
  //! PC3 dz neg
  p0_pc3_m_dz[0][0][0][0]=-1.17458; p1_pc3_m_dz[0][0][0][0]=-0.00133028; p2_pc3_m_dz[0][0][0][0]=-0.0552305; p3_pc3_m_dz[0][0][0][0]=0.0391674; p4_pc3_m_dz[0][0][0][0]=-0.00836535; 
  p0_pc3_m_dz[1][0][0][0]=-1.08033; p1_pc3_m_dz[1][0][0][0]=-1.6202; p2_pc3_m_dz[1][0][0][0]=0.92402; p3_pc3_m_dz[1][0][0][0]=4.30474; p4_pc3_m_dz[1][0][0][0]=-3.54681; 
  p0_pc3_s_dz[0][0][0]=1.05768; p1_pc3_s_dz[0][0][0]=-0.0395124; p2_pc3_s_dz[0][0][0]=0.469803; p3_pc3_s_dz[0][0][0]=-0.131991; p4_pc3_s_dz[0][0][0]=0.01557; 
  p0_pc3_m_dz[0][0][1][0]=-1.26012; p1_pc3_m_dz[0][0][1][0]=-0.191796; p2_pc3_m_dz[0][0][1][0]=0.131942; p3_pc3_m_dz[0][0][1][0]=-0.0184561; p4_pc3_m_dz[0][0][1][0]=-0.000269563; 
  p0_pc3_m_dz[1][0][1][0]=-0.866883; p1_pc3_m_dz[1][0][1][0]=-2.55653; p2_pc3_m_dz[1][0][1][0]=1.11882; p3_pc3_m_dz[1][0][1][0]=6.51265; p4_pc3_m_dz[1][0][1][0]=-5.993; 
  p0_pc3_s_dz[0][1][0]=1.0708; p1_pc3_s_dz[0][1][0]=0.0371144; p2_pc3_s_dz[0][1][0]=0.356052; p3_pc3_s_dz[0][1][0]=-0.0856701; p4_pc3_s_dz[0][1][0]=0.0095248; 
  p0_pc3_m_dz[0][0][2][0]=-1.26846; p1_pc3_m_dz[0][0][2][0]=-0.226984; p2_pc3_m_dz[0][0][2][0]=0.163284; p3_pc3_m_dz[0][0][2][0]=-0.0329949; p4_pc3_m_dz[0][0][2][0]=0.00238308; 
  p0_pc3_m_dz[1][0][2][0]=-1.14312; p1_pc3_m_dz[1][0][2][0]=-1.17601; p2_pc3_m_dz[1][0][2][0]=0.599768; p3_pc3_m_dz[1][0][2][0]=2.45882; p4_pc3_m_dz[1][0][2][0]=-2.11236; 
  p0_pc3_s_dz[0][2][0]=1.05785; p1_pc3_s_dz[0][2][0]=0.135141; p2_pc3_s_dz[0][2][0]=0.252915; p3_pc3_s_dz[0][2][0]=-0.0535497; p4_pc3_s_dz[0][2][0]=0.00633819; 
  p0_pc3_m_dz[0][0][3][0]=-1.30784; p1_pc3_m_dz[0][0][3][0]=-0.119591; p2_pc3_m_dz[0][0][3][0]=0.0633245; p3_pc3_m_dz[0][0][3][0]=-0.00534613; p4_pc3_m_dz[0][0][3][0]=-0.000457035; 
  p0_pc3_m_dz[1][0][3][0]=-0.970792; p1_pc3_m_dz[1][0][3][0]=-1.71521; p2_pc3_m_dz[1][0][3][0]=0.728469; p3_pc3_m_dz[1][0][3][0]=4.04579; p4_pc3_m_dz[1][0][3][0]=-3.90323; 
  p0_pc3_s_dz[0][3][0]=1.07074; p1_pc3_s_dz[0][3][0]=0.114099; p2_pc3_s_dz[0][3][0]=0.278424; p3_pc3_s_dz[0][3][0]=-0.0652913; p4_pc3_s_dz[0][3][0]=0.00810296; 
  p0_pc3_m_dz[0][0][4][0]=-1.4716; p1_pc3_m_dz[0][0][4][0]=0.124775; p2_pc3_m_dz[0][0][4][0]=-0.10953; p3_pc3_m_dz[0][0][4][0]=0.0441292; p4_pc3_m_dz[0][0][4][0]=-0.00522779; 
  p0_pc3_m_dz[1][0][4][0]=-1.5564; p1_pc3_m_dz[1][0][4][0]=0.54941; p2_pc3_m_dz[1][0][4][0]=-0.0378935; p3_pc3_m_dz[1][0][4][0]=-2.23423; p4_pc3_m_dz[1][0][4][0]=2.28756; 
  p0_pc3_s_dz[0][4][0]=1.10899; p1_pc3_s_dz[0][4][0]=0.038795; p2_pc3_s_dz[0][4][0]=0.332956; p3_pc3_s_dz[0][4][0]=-0.0782708; p4_pc3_s_dz[0][4][0]=0.0083979; 
  p0_pc3_m_dz[0][0][5][0]=-1.29422; p1_pc3_m_dz[0][0][5][0]=0.0514182; p2_pc3_m_dz[0][0][5][0]=-0.0581648; p3_pc3_m_dz[0][0][5][0]=0.0183486; p4_pc3_m_dz[0][0][5][0]=-0.00199218; 
  p0_pc3_m_dz[1][0][5][0]=-1.15275; p1_pc3_m_dz[1][0][5][0]=-0.478966; p2_pc3_m_dz[1][0][5][0]=0.385217; p3_pc3_m_dz[1][0][5][0]=0.725903; p4_pc3_m_dz[1][0][5][0]=-1.00226; 
  p0_pc3_s_dz[0][5][0]=1.12766; p1_pc3_s_dz[0][5][0]=-0.0279561; p2_pc3_s_dz[0][5][0]=0.382727; p3_pc3_s_dz[0][5][0]=-0.0948961; p4_pc3_s_dz[0][5][0]=0.0102303; 
  p0_pc3_m_dz[0][0][6][0]=-1.30038; p1_pc3_m_dz[0][0][6][0]=-0.0405185; p2_pc3_m_dz[0][0][6][0]=-0.000220871; p3_pc3_m_dz[0][0][6][0]=-0.00686682; p4_pc3_m_dz[0][0][6][0]=0.00122425; 
  p0_pc3_m_dz[1][0][6][0]=-2.07093; p1_pc3_m_dz[1][0][6][0]=3.16972; p2_pc3_m_dz[1][0][6][0]=-0.755181; p3_pc3_m_dz[1][0][6][0]=-9.31056; p4_pc3_m_dz[1][0][6][0]=8.73594; 
  p0_pc3_s_dz[0][6][0]=1.11479; p1_pc3_s_dz[0][6][0]=0.0753236; p2_pc3_s_dz[0][6][0]=0.287541; p3_pc3_s_dz[0][6][0]=-0.0641451; p4_pc3_s_dz[0][6][0]=0.00753475; 
  p0_pc3_m_dz[0][0][7][0]=-1.17085; p1_pc3_m_dz[0][0][7][0]=-0.171566; p2_pc3_m_dz[0][0][7][0]=0.131532; p3_pc3_m_dz[0][0][7][0]=-0.0632884; p4_pc3_m_dz[0][0][7][0]=0.00793607; 
  p0_pc3_m_dz[1][0][7][0]=-1.81616; p1_pc3_m_dz[1][0][7][0]=2.71669; p2_pc3_m_dz[1][0][7][0]=-0.731421; p3_pc3_m_dz[1][0][7][0]=-8.00191; p4_pc3_m_dz[1][0][7][0]=7.3757; 
  p0_pc3_s_dz[0][7][0]=1.09345; p1_pc3_s_dz[0][7][0]=0.189961; p2_pc3_s_dz[0][7][0]=0.160183; p3_pc3_s_dz[0][7][0]=-0.0159236; p4_pc3_s_dz[0][7][0]=0.00163515; 
  p0_pc3_m_dz[0][0][8][0]=-1.18414; p1_pc3_m_dz[0][0][8][0]=-0.0718691; p2_pc3_m_dz[0][0][8][0]=0.0582139; p3_pc3_m_dz[0][0][8][0]=-0.0503212; p4_pc3_m_dz[0][0][8][0]=0.00757849; 
  p0_pc3_m_dz[1][0][8][0]=-1.26415; p1_pc3_m_dz[1][0][8][0]=0.880372; p2_pc3_m_dz[1][0][8][0]=-0.182776; p3_pc3_m_dz[1][0][8][0]=-3.14546; p4_pc3_m_dz[1][0][8][0]=2.32692; 
  p0_pc3_s_dz[0][8][0]=1.09918; p1_pc3_s_dz[0][8][0]=0.12813; p2_pc3_s_dz[0][8][0]=0.248844; p3_pc3_s_dz[0][8][0]=-0.0510949; p4_pc3_s_dz[0][8][0]=0.00619453; 
  p0_pc3_m_dz[0][0][9][0]=-1.27781; p1_pc3_m_dz[0][0][9][0]=0.0258142; p2_pc3_m_dz[0][0][9][0]=-0.0206448; p3_pc3_m_dz[0][0][9][0]=-0.0215104; p4_pc3_m_dz[0][0][9][0]=0.00478106; 
  p0_pc3_m_dz[1][0][9][0]=-0.922839; p1_pc3_m_dz[1][0][9][0]=-0.13054; p2_pc3_m_dz[1][0][9][0]=-0.118015; p3_pc3_m_dz[1][0][9][0]=-0.659981; p4_pc3_m_dz[1][0][9][0]=-0.165065; 
  p0_pc3_s_dz[0][9][0]=1.07484; p1_pc3_s_dz[0][9][0]=0.0489252; p2_pc3_s_dz[0][9][0]=0.377486; p3_pc3_s_dz[0][9][0]=-0.103726; p4_pc3_s_dz[0][9][0]=0.0129627; 
  p0_pc3_m_dz[0][1][0][0]=-0.572559; p1_pc3_m_dz[0][1][0][0]=0.160177; p2_pc3_m_dz[0][1][0][0]=0.0123694; p3_pc3_m_dz[0][1][0][0]=0.0184356; p4_pc3_m_dz[0][1][0][0]=-0.00565314; 
  p0_pc3_m_dz[1][1][0][0]=-0.546869; p1_pc3_m_dz[1][1][0][0]=-1.15968; p2_pc3_m_dz[1][1][0][0]=0.780851; p3_pc3_m_dz[1][1][0][0]=3.66336; p4_pc3_m_dz[1][1][0][0]=-2.8265; 
  p0_pc3_s_dz[1][0][0]=0.978172; p1_pc3_s_dz[1][0][0]=0.22742; p2_pc3_s_dz[1][0][0]=0.277456; p3_pc3_s_dz[1][0][0]=-0.0536002; p4_pc3_s_dz[1][0][0]=0.00523922; 
  p0_pc3_m_dz[0][1][1][0]=-0.749848; p1_pc3_m_dz[0][1][1][0]=0.0187539; p2_pc3_m_dz[0][1][1][0]=0.120121; p3_pc3_m_dz[0][1][1][0]=-0.0120352; p4_pc3_m_dz[0][1][1][0]=-0.00104563; 
  p0_pc3_m_dz[1][1][1][0]=-1.06275; p1_pc3_m_dz[1][1][1][0]=0.456142; p2_pc3_m_dz[1][1][1][0]=0.284612; p3_pc3_m_dz[1][1][1][0]=-1.14265; p4_pc3_m_dz[1][1][1][0]=1.49283; 
  p0_pc3_s_dz[1][1][0]=0.967603; p1_pc3_s_dz[1][1][0]=0.27865; p2_pc3_s_dz[1][1][0]=0.205185; p3_pc3_s_dz[1][1][0]=-0.0281332; p4_pc3_s_dz[1][1][0]=0.00241287; 
  p0_pc3_m_dz[0][1][2][0]=-0.91678; p1_pc3_m_dz[0][1][2][0]=0.0432919; p2_pc3_m_dz[0][1][2][0]=0.0818393; p3_pc3_m_dz[0][1][2][0]=-0.0071619; p4_pc3_m_dz[0][1][2][0]=-0.000280634; 
  p0_pc3_m_dz[1][1][2][0]=-0.958516; p1_pc3_m_dz[1][1][2][0]=-0.31908; p2_pc3_m_dz[1][1][2][0]=0.520242; p3_pc3_m_dz[1][1][2][0]=0.862899; p4_pc3_m_dz[1][1][2][0]=-0.821152; 
  p0_pc3_s_dz[1][2][0]=0.960492; p1_pc3_s_dz[1][2][0]=0.273752; p2_pc3_s_dz[1][2][0]=0.199191; p3_pc3_s_dz[1][2][0]=-0.0284125; p4_pc3_s_dz[1][2][0]=0.00282522; 
  p0_pc3_m_dz[0][1][3][0]=-1.08625; p1_pc3_m_dz[0][1][3][0]=0.0701982; p2_pc3_m_dz[0][1][3][0]=0.00742307; p3_pc3_m_dz[0][1][3][0]=0.0138771; p4_pc3_m_dz[0][1][3][0]=-0.00236032; 
  p0_pc3_m_dz[1][1][3][0]=-0.952739; p1_pc3_m_dz[1][1][3][0]=-0.703459; p2_pc3_m_dz[1][1][3][0]=0.525932; p3_pc3_m_dz[1][1][3][0]=1.60112; p4_pc3_m_dz[1][1][3][0]=-1.57778; 
  p0_pc3_s_dz[1][3][0]=0.961584; p1_pc3_s_dz[1][3][0]=0.264031; p2_pc3_s_dz[1][3][0]=0.211021; p3_pc3_s_dz[1][3][0]=-0.0329985; p4_pc3_s_dz[1][3][0]=0.00344247; 
  p0_pc3_m_dz[0][1][4][0]=-1.25287; p1_pc3_m_dz[0][1][4][0]=0.0910491; p2_pc3_m_dz[0][1][4][0]=-0.0491424; p3_pc3_m_dz[0][1][4][0]=0.0263115; p4_pc3_m_dz[0][1][4][0]=-0.00289822; 
  p0_pc3_m_dz[1][1][4][0]=-1.21066; p1_pc3_m_dz[1][1][4][0]=-0.0778929; p2_pc3_m_dz[1][1][4][0]=0.194938; p3_pc3_m_dz[1][1][4][0]=-0.353924; p4_pc3_m_dz[1][1][4][0]=0.41838; 
  p0_pc3_s_dz[1][4][0]=1.0261; p1_pc3_s_dz[1][4][0]=0.175261; p2_pc3_s_dz[1][4][0]=0.266409; p3_pc3_s_dz[1][4][0]=-0.0474905; p4_pc3_s_dz[1][4][0]=0.00387637; 
  p0_pc3_m_dz[0][1][5][0]=-1.02893; p1_pc3_m_dz[0][1][5][0]=0.0270617; p2_pc3_m_dz[0][1][5][0]=-0.0386261; p3_pc3_m_dz[0][1][5][0]=0.0087123; p4_pc3_m_dz[0][1][5][0]=0.000101854; 
  p0_pc3_m_dz[1][1][5][0]=-1.27934; p1_pc3_m_dz[1][1][5][0]=1.13434; p2_pc3_m_dz[1][1][5][0]=-0.260833; p3_pc3_m_dz[1][1][5][0]=-3.59711; p4_pc3_m_dz[1][1][5][0]=3.43712; 
  p0_pc3_s_dz[1][5][0]=1.04022; p1_pc3_s_dz[1][5][0]=0.229682; p2_pc3_s_dz[1][5][0]=0.217382; p3_pc3_s_dz[1][5][0]=-0.0306161; p4_pc3_s_dz[1][5][0]=0.00207859; 
  p0_pc3_m_dz[0][1][6][0]=-1.11914; p1_pc3_m_dz[0][1][6][0]=-0.12863; p2_pc3_m_dz[0][1][6][0]=0.0658748; p3_pc3_m_dz[0][1][6][0]=-0.0383273; p4_pc3_m_dz[0][1][6][0]=0.00673886; 
  p0_pc3_m_dz[1][1][6][0]=-1.7203; p1_pc3_m_dz[1][1][6][0]=2.384; p2_pc3_m_dz[1][1][6][0]=-0.498506; p3_pc3_m_dz[1][1][6][0]=-7.38634; p4_pc3_m_dz[1][1][6][0]=6.86992; 
  p0_pc3_s_dz[1][6][0]=0.981157; p1_pc3_s_dz[1][6][0]=0.314963; p2_pc3_s_dz[1][6][0]=0.143025; p3_pc3_s_dz[1][6][0]=-0.00160252; p4_pc3_s_dz[1][6][0]=-0.000534172; 
  p0_pc3_m_dz[0][1][7][0]=-1.22092; p1_pc3_m_dz[0][1][7][0]=-0.141159; p2_pc3_m_dz[0][1][7][0]=0.0353831; p3_pc3_m_dz[0][1][7][0]=-0.0336425; p4_pc3_m_dz[0][1][7][0]=0.00584624; 
  p0_pc3_m_dz[1][1][7][0]=-1.57182; p1_pc3_m_dz[1][1][7][0]=1.63779; p2_pc3_m_dz[1][1][7][0]=-0.435959; p3_pc3_m_dz[1][1][7][0]=-5.51069; p4_pc3_m_dz[1][1][7][0]=5.05173; 
  p0_pc3_s_dz[1][7][0]=0.98189; p1_pc3_s_dz[1][7][0]=0.273426; p2_pc3_s_dz[1][7][0]=0.183608; p3_pc3_s_dz[1][7][0]=-0.0176344; p4_pc3_s_dz[1][7][0]=0.00138371; 
  p0_pc3_m_dz[0][1][8][0]=-1.30418; p1_pc3_m_dz[0][1][8][0]=-0.161405; p2_pc3_m_dz[0][1][8][0]=0.0255597; p3_pc3_m_dz[0][1][8][0]=-0.0351849; p4_pc3_m_dz[0][1][8][0]=0.00688852; 
  p0_pc3_m_dz[1][1][8][0]=-1.2596; p1_pc3_m_dz[1][1][8][0]=0.208317; p2_pc3_m_dz[1][1][8][0]=0.0678538; p3_pc3_m_dz[1][1][8][0]=-1.44628; p4_pc3_m_dz[1][1][8][0]=0.489726; 
  p0_pc3_s_dz[1][8][0]=0.990747; p1_pc3_s_dz[1][8][0]=0.257382; p2_pc3_s_dz[1][8][0]=0.201073; p3_pc3_s_dz[1][8][0]=-0.0220164; p4_pc3_s_dz[1][8][0]=0.00139832; 
  p0_pc3_m_dz[0][1][9][0]=-1.50569; p1_pc3_m_dz[0][1][9][0]=-0.119457; p2_pc3_m_dz[0][1][9][0]=-0.0395232; p3_pc3_m_dz[0][1][9][0]=-0.00824864; p4_pc3_m_dz[0][1][9][0]=0.00464516; 
  p0_pc3_m_dz[1][1][9][0]=-1.39468; p1_pc3_m_dz[1][1][9][0]=0.633035; p2_pc3_m_dz[1][1][9][0]=-0.328923; p3_pc3_m_dz[1][1][9][0]=-3.09749; p4_pc3_m_dz[1][1][9][0]=2.13919; 
  p0_pc3_s_dz[1][9][0]=0.951951; p1_pc3_s_dz[1][9][0]=0.306527; p2_pc3_s_dz[1][9][0]=0.19733; p3_pc3_s_dz[1][9][0]=-0.0255486; p4_pc3_s_dz[1][9][0]=0.00204046;
  
  //! PC3 dz pos
  p0_pc3_m_dz[0][0][0][1]=-1.12126; p1_pc3_m_dz[0][0][0][1]=0.168197; p2_pc3_m_dz[0][0][0][1]=0.079148; p3_pc3_m_dz[0][0][0][1]=-0.0110804; p4_pc3_m_dz[0][0][0][1]=-0.00214301; 
  p0_pc3_m_dz[1][0][0][1]=-1.36187; p1_pc3_m_dz[1][0][0][1]=-0.349119; p2_pc3_m_dz[1][0][0][1]=0.97229; p3_pc3_m_dz[1][0][0][1]=1.48825; p4_pc3_m_dz[1][0][0][1]=-1.24965; 
  p0_pc3_s_dz[0][0][1]=0.986252; p1_pc3_s_dz[0][0][1]=0.148258; p2_pc3_s_dz[0][0][1]=0.371595; p3_pc3_s_dz[0][0][1]=-0.10873; p4_pc3_s_dz[0][0][1]=0.0130808; 
  p0_pc3_m_dz[0][0][1][1]=-1.3393; p1_pc3_m_dz[0][0][1][1]=0.250783; p2_pc3_m_dz[0][0][1][1]=-0.000213928; p3_pc3_m_dz[0][0][1][1]=0.0139552; p4_pc3_m_dz[0][0][1][1]=-0.00298744; 
  p0_pc3_m_dz[1][0][1][1]=-1.25965; p1_pc3_m_dz[1][0][1][1]=-0.88796; p2_pc3_m_dz[1][0][1][1]=0.908441; p3_pc3_m_dz[1][0][1][1]=2.53588; p4_pc3_m_dz[1][0][1][1]=-2.33671; 
  p0_pc3_s_dz[0][1][1]=0.989732; p1_pc3_s_dz[0][1][1]=0.258645; p2_pc3_s_dz[0][1][1]=0.235724; p3_pc3_s_dz[0][1][1]=-0.0561877; p4_pc3_s_dz[0][1][1]=0.00662513; 
  p0_pc3_m_dz[0][0][2][1]=-1.34765; p1_pc3_m_dz[0][0][2][1]=0.13964; p2_pc3_m_dz[0][0][2][1]=0.0516543; p3_pc3_m_dz[0][0][2][1]=-0.00806996; p4_pc3_m_dz[0][0][2][1]=0.000244799; 
  p0_pc3_m_dz[1][0][2][1]=-1.28239; p1_pc3_m_dz[1][0][2][1]=-0.601934; p2_pc3_m_dz[1][0][2][1]=0.64082; p3_pc3_m_dz[1][0][2][1]=1.52899; p4_pc3_m_dz[1][0][2][1]=-1.41359; 
  p0_pc3_s_dz[0][2][1]=1.01872; p1_pc3_s_dz[0][2][1]=0.236381; p2_pc3_s_dz[0][2][1]=0.242333; p3_pc3_s_dz[0][2][1]=-0.0619268; p4_pc3_s_dz[0][2][1]=0.00772466; 
  p0_pc3_m_dz[0][0][3][1]=-1.34573; p1_pc3_m_dz[0][0][3][1]=0.061675; p2_pc3_m_dz[0][0][3][1]=0.0512563; p3_pc3_m_dz[0][0][3][1]=-0.0109827; p4_pc3_m_dz[0][0][3][1]=0.000778228; 
  p0_pc3_m_dz[1][0][3][1]=-1.46606; p1_pc3_m_dz[1][0][3][1]=0.167777; p2_pc3_m_dz[1][0][3][1]=0.457335; p3_pc3_m_dz[1][0][3][1]=-0.797454; p4_pc3_m_dz[1][0][3][1]=0.546203; 
  p0_pc3_s_dz[0][3][1]=1.0104; p1_pc3_s_dz[0][3][1]=0.268565; p2_pc3_s_dz[0][3][1]=0.211583; p3_pc3_s_dz[0][3][1]=-0.0504063; p4_pc3_s_dz[0][3][1]=0.00637376; 
  p0_pc3_m_dz[0][0][4][1]=-1.43865; p1_pc3_m_dz[0][0][4][1]=0.0640165; p2_pc3_m_dz[0][0][4][1]=0.0203067; p3_pc3_m_dz[0][0][4][1]=-0.00700458; p4_pc3_m_dz[0][0][4][1]=0.00104111; 
  p0_pc3_m_dz[1][0][4][1]=-0.838318; p1_pc3_m_dz[1][0][4][1]=-2.40179; p2_pc3_m_dz[1][0][4][1]=0.953198; p3_pc3_m_dz[1][0][4][1]=6.11847; p4_pc3_m_dz[1][0][4][1]=-5.99418; 
  p0_pc3_s_dz[0][4][1]=1.04732; p1_pc3_s_dz[0][4][1]=0.183461; p2_pc3_s_dz[0][4][1]=0.29344; p3_pc3_s_dz[0][4][1]=-0.0757937; p4_pc3_s_dz[0][4][1]=0.00820396; 
  p0_pc3_m_dz[0][0][5][1]=-1.33194; p1_pc3_m_dz[0][0][5][1]=0.118439; p2_pc3_m_dz[0][0][5][1]=-0.0950247; p3_pc3_m_dz[0][0][5][1]=0.0291456; p4_pc3_m_dz[0][0][5][1]=-0.00309088; 
  p0_pc3_m_dz[1][0][5][1]=-1.78499; p1_pc3_m_dz[1][0][5][1]=1.95384; p2_pc3_m_dz[1][0][5][1]=-0.237376; p3_pc3_m_dz[1][0][5][1]=-5.99311; p4_pc3_m_dz[1][0][5][1]=5.47619; 
  p0_pc3_s_dz[0][5][1]=1.08589; p1_pc3_s_dz[0][5][1]=0.0317299; p2_pc3_s_dz[0][5][1]=0.421622; p3_pc3_s_dz[0][5][1]=-0.11853; p4_pc3_s_dz[0][5][1]=0.0129529; 
  p0_pc3_m_dz[0][0][6][1]=-1.36268; p1_pc3_m_dz[0][0][6][1]=0.0666541; p2_pc3_m_dz[0][0][6][1]=-0.139009; p3_pc3_m_dz[0][0][6][1]=0.0419639; p4_pc3_m_dz[0][0][6][1]=-0.00436905; 
  p0_pc3_m_dz[1][0][6][1]=-1.54033; p1_pc3_m_dz[1][0][6][1]=0.944839; p2_pc3_m_dz[1][0][6][1]=-0.0719668; p3_pc3_m_dz[1][0][6][1]=-3.42634; p4_pc3_m_dz[1][0][6][1]=2.99132; 
  p0_pc3_s_dz[0][6][1]=1.07794; p1_pc3_s_dz[0][6][1]=0.125732; p2_pc3_s_dz[0][6][1]=0.337906; p3_pc3_s_dz[0][6][1]=-0.0936333; p4_pc3_s_dz[0][6][1]=0.0113384; 
  p0_pc3_m_dz[0][0][7][1]=-1.34383; p1_pc3_m_dz[0][0][7][1]=0.205183; p2_pc3_m_dz[0][0][7][1]=-0.294915; p3_pc3_m_dz[0][0][7][1]=0.0886124; p4_pc3_m_dz[0][0][7][1]=-0.009666; 
  p0_pc3_m_dz[1][0][7][1]=-1.3519; p1_pc3_m_dz[1][0][7][1]=0.64897; p2_pc3_m_dz[1][0][7][1]=-0.00532232; p3_pc3_m_dz[1][0][7][1]=-2.64061; p4_pc3_m_dz[1][0][7][1]=1.96035; 
  p0_pc3_s_dz[0][7][1]=1.11354; p1_pc3_s_dz[0][7][1]=0.0706127; p2_pc3_s_dz[0][7][1]=0.380385; p3_pc3_s_dz[0][7][1]=-0.109976; p4_pc3_s_dz[0][7][1]=0.0133678; 
  p0_pc3_m_dz[0][0][8][1]=-1.28127; p1_pc3_m_dz[0][0][8][1]=0.0439381; p2_pc3_m_dz[0][0][8][1]=-0.186585; p3_pc3_m_dz[0][0][8][1]=0.043373; p4_pc3_m_dz[0][0][8][1]=-0.00342075; 
  p0_pc3_m_dz[1][0][8][1]=-1.39127; p1_pc3_m_dz[1][0][8][1]=1.20305; p2_pc3_m_dz[1][0][8][1]=-0.350125; p3_pc3_m_dz[1][0][8][1]=-4.38702; p4_pc3_m_dz[1][0][8][1]=3.62769; 
  p0_pc3_s_dz[0][8][1]=1.07887; p1_pc3_s_dz[0][8][1]=0.14108; p2_pc3_s_dz[0][8][1]=0.335575; p3_pc3_s_dz[0][8][1]=-0.0935104; p4_pc3_s_dz[0][8][1]=0.0112504; 
  p0_pc3_m_dz[0][0][9][1]=-1.44392; p1_pc3_m_dz[0][0][9][1]=0.166381; p2_pc3_m_dz[0][0][9][1]=-0.286752; p3_pc3_m_dz[0][0][9][1]=0.0700898; p4_pc3_m_dz[0][0][9][1]=-0.00539668; 
  p0_pc3_m_dz[1][0][9][1]=-1.05916; p1_pc3_m_dz[1][0][9][1]=0.0519319; p2_pc3_m_dz[1][0][9][1]=-0.227105; p3_pc3_m_dz[1][0][9][1]=-1.4654; p4_pc3_m_dz[1][0][9][1]=0.553047; 
  p0_pc3_s_dz[0][9][1]=1.04848; p1_pc3_s_dz[0][9][1]=0.0929458; p2_pc3_s_dz[0][9][1]=0.432162; p3_pc3_s_dz[0][9][1]=-0.138427; p4_pc3_s_dz[0][9][1]=0.0176122; 
  p0_pc3_m_dz[0][1][0][1]=-0.626382; p1_pc3_m_dz[0][1][0][1]=0.105904; p2_pc3_m_dz[0][1][0][1]=-0.138156; p3_pc3_m_dz[0][1][0][1]=0.0689463; p4_pc3_m_dz[0][1][0][1]=-0.0116066; 
  p0_pc3_m_dz[1][1][0][1]=-0.585141; p1_pc3_m_dz[1][1][0][1]=-1.19514; p2_pc3_m_dz[1][1][0][1]=0.630746; p3_pc3_m_dz[1][1][0][1]=3.54939; p4_pc3_m_dz[1][1][0][1]=-2.71996; 
  p0_pc3_s_dz[1][0][1]=0.944458; p1_pc3_s_dz[1][0][1]=0.132115; p2_pc3_s_dz[1][0][1]=0.411028; p3_pc3_s_dz[1][0][1]=-0.110968; p4_pc3_s_dz[1][0][1]=0.0127937; 
  p0_pc3_m_dz[0][1][1][1]=-0.793341; p1_pc3_m_dz[0][1][1][1]=-0.0309542; p2_pc3_m_dz[0][1][1][1]=0.0185929; p3_pc3_m_dz[0][1][1][1]=0.0146458; p4_pc3_m_dz[0][1][1][1]=-0.00328988; 
  p0_pc3_m_dz[1][1][1][1]=-0.703966; p1_pc3_m_dz[1][1][1][1]=-1.06087; p2_pc3_m_dz[1][1][1][1]=0.545952; p3_pc3_m_dz[1][1][1][1]=2.73721; p4_pc3_m_dz[1][1][1][1]=-2.20415; 
  p0_pc3_s_dz[1][1][1]=0.904174; p1_pc3_s_dz[1][1][1]=0.280988; p2_pc3_s_dz[1][1][1]=0.251088; p3_pc3_s_dz[1][1][1]=-0.057483; p4_pc3_s_dz[1][1][1]=0.00687964; 
  p0_pc3_m_dz[0][1][2][1]=-0.981502; p1_pc3_m_dz[0][1][2][1]=0.0967828; p2_pc3_m_dz[0][1][2][1]=-0.0693041; p3_pc3_m_dz[0][1][2][1]=0.0371699; p4_pc3_m_dz[0][1][2][1]=-0.00462687; 
  p0_pc3_m_dz[1][1][2][1]=-0.690395; p1_pc3_m_dz[1][1][2][1]=-1.37937; p2_pc3_m_dz[1][1][2][1]=0.643435; p3_pc3_m_dz[1][1][2][1]=3.36244; p4_pc3_m_dz[1][1][2][1]=-3.02555; 
  p0_pc3_s_dz[1][2][1]=0.895592; p1_pc3_s_dz[1][2][1]=0.304602; p2_pc3_s_dz[1][2][1]=0.233376; p3_pc3_s_dz[1][2][1]=-0.0584451; p4_pc3_s_dz[1][2][1]=0.00772827; 
  p0_pc3_m_dz[0][1][3][1]=-1.10394; p1_pc3_m_dz[0][1][3][1]=0.079325; p2_pc3_m_dz[0][1][3][1]=-0.0498441; p3_pc3_m_dz[0][1][3][1]=0.0282242; p4_pc3_m_dz[0][1][3][1]=-0.00359454; 
  p0_pc3_m_dz[1][1][3][1]=-1.069; p1_pc3_m_dz[1][1][3][1]=-0.231759; p2_pc3_m_dz[1][1][3][1]=0.30648; p3_pc3_m_dz[1][1][3][1]=0.300485; p4_pc3_m_dz[1][1][3][1]=-0.308788; 
  p0_pc3_s_dz[1][3][1]=0.898097; p1_pc3_s_dz[1][3][1]=0.30076; p2_pc3_s_dz[1][3][1]=0.246656; p3_pc3_s_dz[1][3][1]=-0.0633352; p4_pc3_s_dz[1][3][1]=0.00840633; 
  p0_pc3_m_dz[0][1][4][1]=-1.24094; p1_pc3_m_dz[0][1][4][1]=0.115638; p2_pc3_m_dz[0][1][4][1]=-0.0639311; p3_pc3_m_dz[0][1][4][1]=0.0306348; p4_pc3_m_dz[0][1][4][1]=-0.00350014; 
  p0_pc3_m_dz[1][1][4][1]=-1.17968; p1_pc3_m_dz[1][1][4][1]=0.00554944; p2_pc3_m_dz[1][1][4][1]=0.0873616; p3_pc3_m_dz[1][1][4][1]=-0.823536; p4_pc3_m_dz[1][1][4][1]=1.04602; 
  p0_pc3_s_dz[1][4][1]=0.999705; p1_pc3_s_dz[1][4][1]=0.101784; p2_pc3_s_dz[1][4][1]=0.409871; p3_pc3_s_dz[1][4][1]=-0.117195; p4_pc3_s_dz[1][4][1]=0.0135374; 
  p0_pc3_m_dz[0][1][5][1]=-1.00348; p1_pc3_m_dz[0][1][5][1]=0.0161821; p2_pc3_m_dz[0][1][5][1]=0.0359864; p3_pc3_m_dz[0][1][5][1]=-0.0147846; p4_pc3_m_dz[0][1][5][1]=0.00229679; 
  p0_pc3_m_dz[1][1][5][1]=-0.958769; p1_pc3_m_dz[1][1][5][1]=-0.0476114; p2_pc3_m_dz[1][1][5][1]=0.116238; p3_pc3_m_dz[1][1][5][1]=-0.389116; p4_pc3_m_dz[1][1][5][1]=0.392384; 
  p0_pc3_s_dz[1][5][1]=1.04792; p1_pc3_s_dz[1][5][1]=-0.0167846; p2_pc3_s_dz[1][5][1]=0.555377; p3_pc3_s_dz[1][5][1]=-0.176707; p4_pc3_s_dz[1][5][1]=0.0210084; 
  p0_pc3_m_dz[0][1][6][1]=-1.14608; p1_pc3_m_dz[0][1][6][1]=0.0997272; p2_pc3_m_dz[0][1][6][1]=-0.0274456; p3_pc3_m_dz[0][1][6][1]=-0.00202994; p4_pc3_m_dz[0][1][6][1]=0.00202522; 
  p0_pc3_m_dz[1][1][6][1]=-1.06553; p1_pc3_m_dz[1][1][6][1]=-0.0748056; p2_pc3_m_dz[1][1][6][1]=0.254116; p3_pc3_m_dz[1][1][6][1]=-0.133566; p4_pc3_m_dz[1][1][6][1]=-0.276103; 
  p0_pc3_s_dz[1][6][1]=0.965127; p1_pc3_s_dz[1][6][1]=0.149309; p2_pc3_s_dz[1][6][1]=0.41066; p3_pc3_s_dz[1][6][1]=-0.126061; p4_pc3_s_dz[1][6][1]=0.016476; 
  p0_pc3_m_dz[0][1][7][1]=-1.23332; p1_pc3_m_dz[0][1][7][1]=0.135089; p2_pc3_m_dz[0][1][7][1]=-0.0755637; p3_pc3_m_dz[0][1][7][1]=0.0136701; p4_pc3_m_dz[0][1][7][1]=-0.000485492; 
  p0_pc3_m_dz[1][1][7][1]=-1.36511; p1_pc3_m_dz[1][1][7][1]=1.00408; p2_pc3_m_dz[1][1][7][1]=-0.0683987; p3_pc3_m_dz[1][1][7][1]=-3.33138; p4_pc3_m_dz[1][1][7][1]=2.7137; 
  p0_pc3_s_dz[1][7][1]=0.94427; p1_pc3_s_dz[1][7][1]=0.164643; p2_pc3_s_dz[1][7][1]=0.402528; p3_pc3_s_dz[1][7][1]=-0.127402; p4_pc3_s_dz[1][7][1]=0.0170279; 
  p0_pc3_m_dz[0][1][8][1]=-1.29229; p1_pc3_m_dz[0][1][8][1]=0.093201; p2_pc3_m_dz[0][1][8][1]=-0.0345211; p3_pc3_m_dz[0][1][8][1]=-0.00340712; p4_pc3_m_dz[0][1][8][1]=0.00222371; 
  p0_pc3_m_dz[1][1][8][1]=-1.3712; p1_pc3_m_dz[1][1][8][1]=0.962475; p2_pc3_m_dz[1][1][8][1]=-0.0105559; p3_pc3_m_dz[1][1][8][1]=-3.26434; p4_pc3_m_dz[1][1][8][1]=2.24982; 
  p0_pc3_s_dz[1][8][1]=0.96811; p1_pc3_s_dz[1][8][1]=0.111573; p2_pc3_s_dz[1][8][1]=0.454783; p3_pc3_s_dz[1][8][1]=-0.143311; p4_pc3_s_dz[1][8][1]=0.0185216; 
  p0_pc3_m_dz[0][1][9][1]=-1.44652; p1_pc3_m_dz[0][1][9][1]=0.0376855; p2_pc3_m_dz[0][1][9][1]=0.0451679; p3_pc3_m_dz[0][1][9][1]=-0.0302277; p4_pc3_m_dz[0][1][9][1]=0.00645455; 
  p0_pc3_m_dz[1][1][9][1]=-1.60424; p1_pc3_m_dz[1][1][9][1]=1.77964; p2_pc3_m_dz[1][1][9][1]=-0.424532; p3_pc3_m_dz[1][1][9][1]=-5.89535; p4_pc3_m_dz[1][1][9][1]=4.73463; 
  p0_pc3_s_dz[1][9][1]=0.9531; p1_pc3_s_dz[1][9][1]=0.114294; p2_pc3_s_dz[1][9][1]=0.483533; p3_pc3_s_dz[1][9][1]=-0.151331; p4_pc3_s_dz[1][9][1]=0.0195136;
  
  //! EMC dphi neg
  p0_emc_m_dp[0][0][0]=0.0010708; p1_emc_m_dp[0][0][0]=0.00270586; p2_emc_m_dp[0][0][0]=0.00090264; p3_emc_m_dp[0][0][0]=-0.000407773; p4_emc_m_dp[0][0][0]=3.85009e-05; 
  p0_emc_s_dp[0][0][0]=0.00385222; p1_emc_s_dp[0][0][0]=0.000209707; p2_emc_s_dp[0][0][0]=0.000103637; p3_emc_s_dp[0][0][0]=0.000186102; p4_emc_s_dp[0][0][0]=-4.05607e-05; 
  p0_emc_m_dp[0][1][0]=0.000826086; p1_emc_m_dp[0][1][0]=0.00372156; p2_emc_m_dp[0][1][0]=2.13415e-05; p3_emc_m_dp[0][1][0]=-1.19844e-05; p4_emc_m_dp[0][1][0]=-1.42598e-05; 
  p0_emc_s_dp[0][1][0]=0.00413693; p1_emc_s_dp[0][1][0]=-0.000689715; p2_emc_s_dp[0][1][0]=0.00103093; p3_emc_s_dp[0][1][0]=-0.000194984; p4_emc_s_dp[0][1][0]=1.41639e-05; 
  p0_emc_m_dp[0][2][0]=0.000682195; p1_emc_m_dp[0][2][0]=0.00438696; p2_emc_m_dp[0][2][0]=-0.000438854; p3_emc_m_dp[0][2][0]=0.000167106; p4_emc_m_dp[0][2][0]=-3.5781e-05; 
  p0_emc_s_dp[0][2][0]=0.00405454; p1_emc_s_dp[0][2][0]=-0.000420901; p2_emc_s_dp[0][2][0]=0.000819491; p3_emc_s_dp[0][2][0]=-0.000137124; p4_emc_s_dp[0][2][0]=9.86793e-06; 
  p0_emc_m_dp[0][3][0]=0.00062695; p1_emc_m_dp[0][3][0]=0.00483528; p2_emc_m_dp[0][3][0]=-0.000730266; p3_emc_m_dp[0][3][0]=0.00025295; p4_emc_m_dp[0][3][0]=-4.3934e-05; 
  p0_emc_s_dp[0][3][0]=0.0041198; p1_emc_s_dp[0][3][0]=-0.000486359; p2_emc_s_dp[0][3][0]=0.000903583; p3_emc_s_dp[0][3][0]=-0.000170982; p4_emc_s_dp[0][3][0]=1.47561e-05; 
  p0_emc_m_dp[0][4][0]=0.000325554; p1_emc_m_dp[0][4][0]=0.00578224; p2_emc_m_dp[0][4][0]=-0.00141547; p3_emc_m_dp[0][4][0]=0.000447507; p4_emc_m_dp[0][4][0]=-6.38321e-05; 
  p0_emc_s_dp[0][4][0]=0.00400925; p1_emc_s_dp[0][4][0]=-0.00015901; p2_emc_s_dp[0][4][0]=0.000681963; p3_emc_s_dp[0][4][0]=-0.000121253; p4_emc_s_dp[0][4][0]=1.30034e-05; 
  p0_emc_m_dp[0][5][0]=0.000292615; p1_emc_m_dp[0][5][0]=0.00462133; p2_emc_m_dp[0][5][0]=-0.000428431; p3_emc_m_dp[0][5][0]=0.000128574; p4_emc_m_dp[0][5][0]=-2.67581e-05; 
  p0_emc_s_dp[0][5][0]=0.00390798; p1_emc_s_dp[0][5][0]=-0.000482366; p2_emc_s_dp[0][5][0]=0.00117716; p3_emc_s_dp[0][5][0]=-0.000294957; p4_emc_s_dp[0][5][0]=2.80812e-05; 
  p0_emc_m_dp[0][6][0]=0.00035903; p1_emc_m_dp[0][6][0]=0.00453632; p2_emc_m_dp[0][6][0]=-0.000546213; p3_emc_m_dp[0][6][0]=0.000185387; p4_emc_m_dp[0][6][0]=-3.40361e-05; 
  p0_emc_s_dp[0][6][0]=0.00393598; p1_emc_s_dp[0][6][0]=-0.000475188; p2_emc_s_dp[0][6][0]=0.00112838; p3_emc_s_dp[0][6][0]=-0.000278555; p4_emc_s_dp[0][6][0]=2.60264e-05; 
  p0_emc_m_dp[0][7][0]=0.000776998; p1_emc_m_dp[0][7][0]=0.00290306; p2_emc_m_dp[0][7][0]=0.00092615; p3_emc_m_dp[0][7][0]=-0.000323743; p4_emc_m_dp[0][7][0]=2.31559e-05; 
  p0_emc_s_dp[0][7][0]=0.00377686; p1_emc_s_dp[0][7][0]=5.58328e-05; p2_emc_s_dp[0][7][0]=0.000605538; p3_emc_s_dp[0][7][0]=-0.000101395; p4_emc_s_dp[0][7][0]=5.91691e-06; 
  p0_emc_m_dp[0][8][0]=0.00063621; p1_emc_m_dp[0][8][0]=0.00288698; p2_emc_m_dp[0][8][0]=0.00077123; p3_emc_m_dp[0][8][0]=-0.000272096; p4_emc_m_dp[0][8][0]=1.58598e-05; 
  p0_emc_s_dp[0][8][0]=0.00384022; p1_emc_s_dp[0][8][0]=-1.12065e-05; p2_emc_s_dp[0][8][0]=0.000577884; p3_emc_s_dp[0][8][0]=-8.5841e-05; p4_emc_s_dp[0][8][0]=4.53473e-06; 
  p0_emc_m_dp[0][9][0]=0.000349854; p1_emc_m_dp[0][9][0]=0.00319044; p2_emc_m_dp[0][9][0]=0.000544931; p3_emc_m_dp[0][9][0]=-0.000272061; p4_emc_m_dp[0][9][0]=1.74112e-05; 
  p0_emc_s_dp[0][9][0]=0.0038215; p1_emc_s_dp[0][9][0]=0.000113876; p2_emc_s_dp[0][9][0]=0.000440839; p3_emc_s_dp[0][9][0]=-4.46012e-05; p4_emc_s_dp[0][9][0]=8.6538e-07; 
  p0_emc_m_dp[1][0][0]=0.00175832; p1_emc_m_dp[1][0][0]=0.00118875; p2_emc_m_dp[1][0][0]=0.000758221; p3_emc_m_dp[1][0][0]=-0.000413422; p4_emc_m_dp[1][0][0]=4.39259e-05; 
  p0_emc_s_dp[1][0][0]=0.00501601; p1_emc_s_dp[1][0][0]=-0.000936286; p2_emc_s_dp[1][0][0]=0.00103175; p3_emc_s_dp[1][0][0]=-0.000208801; p4_emc_s_dp[1][0][0]=1.62684e-05; 
  p0_emc_m_dp[1][1][0]=0.00193294; p1_emc_m_dp[1][1][0]=0.000876582; p2_emc_m_dp[1][1][0]=0.00123847; p3_emc_m_dp[1][1][0]=-0.000551173; p4_emc_m_dp[1][1][0]=6.29812e-05; 
  p0_emc_s_dp[1][1][0]=0.00512451; p1_emc_s_dp[1][1][0]=-0.00109396; p2_emc_s_dp[1][1][0]=0.0011105; p3_emc_s_dp[1][1][0]=-0.000215604; p4_emc_s_dp[1][1][0]=1.48701e-05; 
  p0_emc_m_dp[1][2][0]=0.00236498; p1_emc_m_dp[1][2][0]=0.000712335; p2_emc_m_dp[1][2][0]=0.00142471; p3_emc_m_dp[1][2][0]=-0.000585276; p4_emc_m_dp[1][2][0]=6.66416e-05; 
  p0_emc_s_dp[1][2][0]=0.00503161; p1_emc_s_dp[1][2][0]=-0.000743657; p2_emc_s_dp[1][2][0]=0.000842212; p3_emc_s_dp[1][2][0]=-0.000147807; p4_emc_s_dp[1][2][0]=1.0133e-05; 
  p0_emc_m_dp[1][3][0]=0.00248141; p1_emc_m_dp[1][3][0]=0.00136268; p2_emc_m_dp[1][3][0]=0.000927103; p3_emc_m_dp[1][3][0]=-0.000412912; p4_emc_m_dp[1][3][0]=4.73982e-05; 
  p0_emc_s_dp[1][3][0]=0.00492156; p1_emc_s_dp[1][3][0]=-0.000438764; p2_emc_s_dp[1][3][0]=0.000672633; p3_emc_s_dp[1][3][0]=-0.000113471; p4_emc_s_dp[1][3][0]=8.47371e-06; 
  p0_emc_m_dp[1][4][0]=0.00260704; p1_emc_m_dp[1][4][0]=0.00139881; p2_emc_m_dp[1][4][0]=0.000896142; p3_emc_m_dp[1][4][0]=-0.000384332; p4_emc_m_dp[1][4][0]=4.2483e-05; 
  p0_emc_s_dp[1][4][0]=0.00501777; p1_emc_s_dp[1][4][0]=-0.000451646; p2_emc_s_dp[1][4][0]=0.000625992; p3_emc_s_dp[1][4][0]=-9.2872e-05; p4_emc_s_dp[1][4][0]=6.52125e-06; 
  p0_emc_m_dp[1][5][0]=0.0029221; p1_emc_m_dp[1][5][0]=0.000832028; p2_emc_m_dp[1][5][0]=0.00119451; p3_emc_m_dp[1][5][0]=-0.000461532; p4_emc_m_dp[1][5][0]=5.14733e-05; 
  p0_emc_s_dp[1][5][0]=0.00504729; p1_emc_s_dp[1][5][0]=-0.000609455; p2_emc_s_dp[1][5][0]=0.000664071; p3_emc_s_dp[1][5][0]=-6.73732e-05; p4_emc_s_dp[1][5][0]=-2.46676e-06; 
  p0_emc_m_dp[1][6][0]=0.00285406; p1_emc_m_dp[1][6][0]=0.000998378; p2_emc_m_dp[1][6][0]=0.00106134; p3_emc_m_dp[1][6][0]=-0.00041928; p4_emc_m_dp[1][6][0]=4.56881e-05; 
  p0_emc_s_dp[1][6][0]=0.0049884; p1_emc_s_dp[1][6][0]=-0.000852247; p2_emc_s_dp[1][6][0]=0.000911778; p3_emc_s_dp[1][6][0]=-0.000157145; p4_emc_s_dp[1][6][0]=7.90622e-06; 
  p0_emc_m_dp[1][7][0]=0.00311201; p1_emc_m_dp[1][7][0]=0.000812748; p2_emc_m_dp[1][7][0]=0.00110744; p3_emc_m_dp[1][7][0]=-0.000450156; p4_emc_m_dp[1][7][0]=4.95249e-05; 
  p0_emc_s_dp[1][7][0]=0.00499792; p1_emc_s_dp[1][7][0]=-0.000933477; p2_emc_s_dp[1][7][0]=0.00105832; p3_emc_s_dp[1][7][0]=-0.000231506; p4_emc_s_dp[1][7][0]=1.79971e-05; 
  p0_emc_m_dp[1][8][0]=0.00331777; p1_emc_m_dp[1][8][0]=0.000700296; p2_emc_m_dp[1][8][0]=0.000986957; p3_emc_m_dp[1][8][0]=-0.000407812; p4_emc_m_dp[1][8][0]=4.24505e-05; 
  p0_emc_s_dp[1][8][0]=0.00504126; p1_emc_s_dp[1][8][0]=-0.000776077; p2_emc_s_dp[1][8][0]=0.000936014; p3_emc_s_dp[1][8][0]=-0.000209515; p4_emc_s_dp[1][8][0]=1.79764e-05; 
  p0_emc_m_dp[1][9][0]=0.0033763; p1_emc_m_dp[1][9][0]=0.000544648; p2_emc_m_dp[1][9][0]=0.000902382; p3_emc_m_dp[1][9][0]=-0.000378464; p4_emc_m_dp[1][9][0]=3.39435e-05; 
  p0_emc_s_dp[1][9][0]=0.00484674; p1_emc_s_dp[1][9][0]=-0.000111073; p2_emc_s_dp[1][9][0]=0.00034738; p3_emc_s_dp[1][9][0]=-1.55574e-05; p4_emc_s_dp[1][9][0]=-3.05504e-06; 
  p0_emc_m_dp[2][0][0]=-0.0020623; p1_emc_m_dp[2][0][0]=0.00337615; p2_emc_m_dp[2][0][0]=-0.00107472; p3_emc_m_dp[2][0][0]=0.000174235; p4_emc_m_dp[2][0][0]=-1.97285e-05; 
  p0_emc_s_dp[2][0][0]=0.00550372; p1_emc_s_dp[2][0][0]=-0.000470883; p2_emc_s_dp[2][0][0]=0.0007069; p3_emc_s_dp[2][0][0]=-8.76051e-05; p4_emc_s_dp[2][0][0]=3.5834e-07; 
  p0_emc_m_dp[2][1][0]=-0.00183992; p1_emc_m_dp[2][1][0]=0.00315862; p2_emc_m_dp[2][1][0]=-0.000627459; p3_emc_m_dp[2][1][0]=3.32735e-05; p4_emc_m_dp[2][1][0]=1.30353e-07; 
  p0_emc_s_dp[2][1][0]=0.00534078; p1_emc_s_dp[2][1][0]=-0.000281612; p2_emc_s_dp[2][1][0]=0.000524016; p3_emc_s_dp[2][1][0]=-4.72772e-06; p4_emc_s_dp[2][1][0]=-1.10649e-05; 
  p0_emc_m_dp[2][2][0]=-0.00155699; p1_emc_m_dp[2][2][0]=0.00289268; p2_emc_m_dp[2][2][0]=-0.00030694; p3_emc_m_dp[2][2][0]=-3.40498e-05; p4_emc_m_dp[2][2][0]=5.44085e-06; 
  p0_emc_s_dp[2][2][0]=0.00519829; p1_emc_s_dp[2][2][0]=-5.88705e-05; p2_emc_s_dp[2][2][0]=0.000346572; p3_emc_s_dp[2][2][0]=6.4934e-05; p4_emc_s_dp[2][2][0]=-2.03772e-05; 
  p0_emc_m_dp[2][3][0]=-0.00142673; p1_emc_m_dp[2][3][0]=0.00300774; p2_emc_m_dp[2][3][0]=-0.000153405; p3_emc_m_dp[2][3][0]=-0.000128891; p4_emc_m_dp[2][3][0]=1.98323e-05; 
  p0_emc_s_dp[2][3][0]=0.00522556; p1_emc_s_dp[2][3][0]=-0.000313113; p2_emc_s_dp[2][3][0]=0.000654624; p3_emc_s_dp[2][3][0]=-6.80875e-05; p4_emc_s_dp[2][3][0]=-5.55716e-07; 
  p0_emc_m_dp[2][4][0]=-0.0012763; p1_emc_m_dp[2][4][0]=0.00286601; p2_emc_m_dp[2][4][0]=0.000141669; p3_emc_m_dp[2][4][0]=-0.000231674; p4_emc_m_dp[2][4][0]=2.98681e-05; 
  p0_emc_s_dp[2][4][0]=0.00499492; p1_emc_s_dp[2][4][0]=4.38856e-05; p2_emc_s_dp[2][4][0]=0.000394089; p3_emc_s_dp[2][4][0]=2.03569e-05; p4_emc_s_dp[2][4][0]=-1.09358e-05; 
  p0_emc_m_dp[2][5][0]=-0.00174526; p1_emc_m_dp[2][5][0]=0.0031345; p2_emc_m_dp[2][5][0]=-0.000215227; p3_emc_m_dp[2][5][0]=-7.12147e-05; p4_emc_m_dp[2][5][0]=1.03179e-05; 
  p0_emc_s_dp[2][5][0]=0.00486893; p1_emc_s_dp[2][5][0]=0.000300886; p2_emc_s_dp[2][5][0]=0.000396435; p3_emc_s_dp[2][5][0]=-2.43659e-05; p4_emc_s_dp[2][5][0]=-3.50736e-06; 
  p0_emc_m_dp[2][6][0]=-0.00179911; p1_emc_m_dp[2][6][0]=0.00291327; p2_emc_m_dp[2][6][0]=-6.70444e-05; p3_emc_m_dp[2][6][0]=-0.000121696; p4_emc_m_dp[2][6][0]=1.5561e-05; 
  p0_emc_s_dp[2][6][0]=0.00494765; p1_emc_s_dp[2][6][0]=0.000199722; p2_emc_s_dp[2][6][0]=0.000328264; p3_emc_s_dp[2][6][0]=2.09487e-05; p4_emc_s_dp[2][6][0]=-8.76095e-06; 
  p0_emc_m_dp[2][7][0]=-0.00202061; p1_emc_m_dp[2][7][0]=0.0027682; p2_emc_m_dp[2][7][0]=8.15716e-05; p3_emc_m_dp[2][7][0]=-0.000202269; p4_emc_m_dp[2][7][0]=2.67831e-05; 
  p0_emc_s_dp[2][7][0]=0.00472498; p1_emc_s_dp[2][7][0]=0.000556815; p2_emc_s_dp[2][7][0]=5.26994e-06; p3_emc_s_dp[2][7][0]=0.000128725; p4_emc_s_dp[2][7][0]=-2.12641e-05; 
  p0_emc_m_dp[2][8][0]=-0.00216026; p1_emc_m_dp[2][8][0]=0.00239091; p2_emc_m_dp[2][8][0]=0.000243988; p3_emc_m_dp[2][8][0]=-0.000261142; p4_emc_m_dp[2][8][0]=3.25363e-05; 
  p0_emc_s_dp[2][8][0]=0.0045515; p1_emc_s_dp[2][8][0]=0.000746688; p2_emc_s_dp[2][8][0]=-0.000155944; p3_emc_s_dp[2][8][0]=0.000173963; p4_emc_s_dp[2][8][0]=-2.60024e-05; 
  p0_emc_m_dp[2][9][0]=-0.00244581; p1_emc_m_dp[2][9][0]=0.00244648; p2_emc_m_dp[2][9][0]=7.38811e-05; p3_emc_m_dp[2][9][0]=-0.000228694; p4_emc_m_dp[2][9][0]=2.60533e-05; 
  p0_emc_s_dp[2][9][0]=0.00452929; p1_emc_s_dp[2][9][0]=0.000643199; p2_emc_s_dp[2][9][0]=-8.98932e-05; p3_emc_s_dp[2][9][0]=0.000149092; p4_emc_s_dp[2][9][0]=-2.26595e-05;
  
  //! EMC dphi pos
  p0_emc_m_dp[0][0][1]=0.000704165; p1_emc_m_dp[0][0][1]=-0.00390859; p2_emc_m_dp[0][0][1]=0.000216993; p3_emc_m_dp[0][0][1]=2.99452e-05; p4_emc_m_dp[0][0][1]=9.2335e-06; 
  p0_emc_s_dp[0][0][1]=0.0040894; p1_emc_s_dp[0][0][1]=-0.00182008; p2_emc_s_dp[0][0][1]=0.00239264; p3_emc_s_dp[0][0][1]=-0.000686671; p4_emc_s_dp[0][0][1]=7.07752e-05; 
  p0_emc_m_dp[0][1][1]=0.000576085; p1_emc_m_dp[0][1][1]=-0.0037791; p2_emc_m_dp[0][1][1]=-0.000115164; p3_emc_m_dp[0][1][1]=0.000134098; p4_emc_m_dp[0][1][1]=-5.90433e-06; 
  p0_emc_s_dp[0][1][1]=0.00399086; p1_emc_s_dp[0][1][1]=-0.0016041; p2_emc_s_dp[0][1][1]=0.00223481; p3_emc_s_dp[0][1][1]=-0.000631266; p4_emc_s_dp[0][1][1]=6.42738e-05; 
  p0_emc_m_dp[0][2][1]=0.000469013; p1_emc_m_dp[0][2][1]=-0.00370895; p2_emc_m_dp[0][2][1]=-0.000369914; p3_emc_m_dp[0][2][1]=0.000209864; p4_emc_m_dp[0][2][1]=-1.34268e-05; 
  p0_emc_s_dp[0][2][1]=0.00398521; p1_emc_s_dp[0][2][1]=-0.00174948; p2_emc_s_dp[0][2][1]=0.00245722; p3_emc_s_dp[0][2][1]=-0.000733434; p4_emc_s_dp[0][2][1]=7.95277e-05; 
  p0_emc_m_dp[0][3][1]=0.000663282; p1_emc_m_dp[0][3][1]=-0.0044282; p2_emc_m_dp[0][3][1]=0.000149061; p3_emc_m_dp[0][3][1]=2.50128e-05; p4_emc_m_dp[0][3][1]=8.75576e-06; 
  p0_emc_s_dp[0][3][1]=0.0040564; p1_emc_s_dp[0][3][1]=-0.00184563; p2_emc_s_dp[0][3][1]=0.0025415; p3_emc_s_dp[0][3][1]=-0.000763964; p4_emc_s_dp[0][3][1]=8.42432e-05; 
  p0_emc_m_dp[0][4][1]=0.000434061; p1_emc_m_dp[0][4][1]=-0.00367219; p2_emc_m_dp[0][4][1]=-0.000587836; p3_emc_m_dp[0][4][1]=0.000294445; p4_emc_m_dp[0][4][1]=-2.17812e-05; 
  p0_emc_s_dp[0][4][1]=0.00410528; p1_emc_s_dp[0][4][1]=-0.00199742; p2_emc_s_dp[0][4][1]=0.00273324; p3_emc_s_dp[0][4][1]=-0.000841132; p4_emc_s_dp[0][4][1]=9.61024e-05; 
  p0_emc_m_dp[0][5][1]=-6.28091e-05; p1_emc_m_dp[0][5][1]=-0.00354739; p2_emc_m_dp[0][5][1]=-0.000293958; p3_emc_m_dp[0][5][1]=0.00015966; p4_emc_m_dp[0][5][1]=-7.44683e-06; 
  p0_emc_s_dp[0][5][1]=0.00379105; p1_emc_s_dp[0][5][1]=-0.00147669; p2_emc_s_dp[0][5][1]=0.00238421; p3_emc_s_dp[0][5][1]=-0.000739872; p4_emc_s_dp[0][5][1]=8.4609e-05; 
  p0_emc_m_dp[0][6][1]=0.000335273; p1_emc_m_dp[0][6][1]=-0.00416033; p2_emc_m_dp[0][6][1]=0.000205748; p3_emc_m_dp[0][6][1]=2.19223e-05; p4_emc_m_dp[0][6][1]=4.8029e-06; 
  p0_emc_s_dp[0][6][1]=0.0037413; p1_emc_s_dp[0][6][1]=-0.00132277; p2_emc_s_dp[0][6][1]=0.00221293; p3_emc_s_dp[0][6][1]=-0.000686049; p4_emc_s_dp[0][6][1]=7.78711e-05; 
  p0_emc_m_dp[0][7][1]=0.000301325; p1_emc_m_dp[0][7][1]=-0.00339123; p2_emc_m_dp[0][7][1]=-0.000529553; p3_emc_m_dp[0][7][1]=0.000327225; p4_emc_m_dp[0][7][1]=-3.40725e-05; 
  p0_emc_s_dp[0][7][1]=0.00387944; p1_emc_s_dp[0][7][1]=-0.001499; p2_emc_s_dp[0][7][1]=0.00225726; p3_emc_s_dp[0][7][1]=-0.000688314; p4_emc_s_dp[0][7][1]=7.563e-05; 
  p0_emc_m_dp[0][8][1]=0.000238758; p1_emc_m_dp[0][8][1]=-0.00308537; p2_emc_m_dp[0][8][1]=-0.00054402; p3_emc_m_dp[0][8][1]=0.000310911; p4_emc_m_dp[0][8][1]=-2.96425e-05; 
  p0_emc_s_dp[0][8][1]=0.00388742; p1_emc_s_dp[0][8][1]=-0.00137198; p2_emc_s_dp[0][8][1]=0.00210294; p3_emc_s_dp[0][8][1]=-0.000628415; p4_emc_s_dp[0][8][1]=6.69576e-05; 
  p0_emc_m_dp[0][9][1]=0.000334007; p1_emc_m_dp[0][9][1]=-0.00274712; p2_emc_m_dp[0][9][1]=-0.000765388; p3_emc_m_dp[0][9][1]=0.000436193; p4_emc_m_dp[0][9][1]=-4.6006e-05; 
  p0_emc_s_dp[0][9][1]=0.00409327; p1_emc_s_dp[0][9][1]=-0.00184308; p2_emc_s_dp[0][9][1]=0.00245896; p3_emc_s_dp[0][9][1]=-0.000733682; p4_emc_s_dp[0][9][1]=7.564e-05; 
  p0_emc_m_dp[1][0][1]=0.000881989; p1_emc_m_dp[1][0][1]=-1.41016e-05; p2_emc_m_dp[1][0][1]=-0.00122116; p3_emc_m_dp[1][0][1]=0.000370253; p4_emc_m_dp[1][0][1]=-2.34606e-05; 
  p0_emc_s_dp[1][0][1]=0.0055283; p1_emc_s_dp[1][0][1]=-0.00273364; p2_emc_s_dp[1][0][1]=0.00259974; p3_emc_s_dp[1][0][1]=-0.000677666; p4_emc_s_dp[1][0][1]=6.21517e-05; 
  p0_emc_m_dp[1][1][1]=0.000996762; p1_emc_m_dp[1][1][1]=-0.000780009; p2_emc_m_dp[1][1][1]=-0.000749545; p3_emc_m_dp[1][1][1]=0.000222781; p4_emc_m_dp[1][1][1]=-1.39409e-05; 
  p0_emc_s_dp[1][1][1]=0.00565284; p1_emc_s_dp[1][1][1]=-0.0030244; p2_emc_s_dp[1][1][1]=0.00287407; p3_emc_s_dp[1][1][1]=-0.000759203; p4_emc_s_dp[1][1][1]=6.973e-05; 
  p0_emc_m_dp[1][2][1]=0.00149131; p1_emc_m_dp[1][2][1]=-0.00170905; p2_emc_m_dp[1][2][1]=-0.000186842; p3_emc_m_dp[1][2][1]=4.99691e-05; p4_emc_m_dp[1][2][1]=2.54634e-06; 
  p0_emc_s_dp[1][2][1]=0.00566733; p1_emc_s_dp[1][2][1]=-0.0029517; p2_emc_s_dp[1][2][1]=0.00280514; p3_emc_s_dp[1][2][1]=-0.000737451; p4_emc_s_dp[1][2][1]=6.84273e-05; 
  p0_emc_m_dp[1][3][1]=0.0016823; p1_emc_m_dp[1][3][1]=-0.00167999; p2_emc_m_dp[1][3][1]=-0.000395831; p3_emc_m_dp[1][3][1]=0.000128575; p4_emc_m_dp[1][3][1]=-6.88114e-06; 
  p0_emc_s_dp[1][3][1]=0.00522406; p1_emc_s_dp[1][3][1]=-0.00176429; p2_emc_s_dp[1][3][1]=0.00200284; p3_emc_s_dp[1][3][1]=-0.000524018; p4_emc_s_dp[1][3][1]=4.92087e-05; 
  p0_emc_m_dp[1][4][1]=0.00174583; p1_emc_m_dp[1][4][1]=-0.00180491; p2_emc_m_dp[1][4][1]=-0.000282913; p3_emc_m_dp[1][4][1]=9.05861e-05; p4_emc_m_dp[1][4][1]=-2.64557e-06; 
  p0_emc_s_dp[1][4][1]=0.00563166; p1_emc_s_dp[1][4][1]=-0.0025397; p2_emc_s_dp[1][4][1]=0.00249131; p3_emc_s_dp[1][4][1]=-0.000639734; p4_emc_s_dp[1][4][1]=5.89193e-05; 
  p0_emc_m_dp[1][5][1]=0.00179938; p1_emc_m_dp[1][5][1]=-0.0014746; p2_emc_m_dp[1][5][1]=-0.000521539; p3_emc_m_dp[1][5][1]=0.000191067; p4_emc_m_dp[1][5][1]=-1.62987e-05; 
  p0_emc_s_dp[1][5][1]=0.00524841; p1_emc_s_dp[1][5][1]=-0.00169689; p2_emc_s_dp[1][5][1]=0.00187442; p3_emc_s_dp[1][5][1]=-0.000460657; p4_emc_s_dp[1][5][1]=4.0365e-05; 
  p0_emc_m_dp[1][6][1]=0.00177902; p1_emc_m_dp[1][6][1]=-0.00145254; p2_emc_m_dp[1][6][1]=-0.000278493; p3_emc_m_dp[1][6][1]=8.29323e-05; p4_emc_m_dp[1][6][1]=-2.75582e-06; 
  p0_emc_s_dp[1][6][1]=0.00510769; p1_emc_s_dp[1][6][1]=-0.00162135; p2_emc_s_dp[1][6][1]=0.00189181; p3_emc_s_dp[1][6][1]=-0.000474878; p4_emc_s_dp[1][6][1]=4.21341e-05; 
  p0_emc_m_dp[1][7][1]=0.00238261; p1_emc_m_dp[1][7][1]=-0.00222796; p2_emc_m_dp[1][7][1]=0.000574273; p3_emc_m_dp[1][7][1]=-0.000204106; p4_emc_m_dp[1][7][1]=2.93282e-05; 
  p0_emc_s_dp[1][7][1]=0.00510925; p1_emc_s_dp[1][7][1]=-0.00150905; p2_emc_s_dp[1][7][1]=0.0017351; p3_emc_s_dp[1][7][1]=-0.000410047; p4_emc_s_dp[1][7][1]=3.23696e-05; 
  p0_emc_m_dp[1][8][1]=0.00242965; p1_emc_m_dp[1][8][1]=-0.00132827; p2_emc_m_dp[1][8][1]=-2.57256e-05; p3_emc_m_dp[1][8][1]=3.64701e-06; p4_emc_m_dp[1][8][1]=6.72827e-06; 
  p0_emc_s_dp[1][8][1]=0.00510646; p1_emc_s_dp[1][8][1]=-0.00128701; p2_emc_s_dp[1][8][1]=0.00155456; p3_emc_s_dp[1][8][1]=-0.000374147; p4_emc_s_dp[1][8][1]=3.02659e-05; 
  p0_emc_m_dp[1][9][1]=0.00250968; p1_emc_m_dp[1][9][1]=-0.00100326; p2_emc_m_dp[1][9][1]=-0.000131054; p3_emc_m_dp[1][9][1]=6.01213e-05; p4_emc_m_dp[1][9][1]=2.7793e-06; 
  p0_emc_s_dp[1][9][1]=0.00507355; p1_emc_s_dp[1][9][1]=-0.00119595; p2_emc_s_dp[1][9][1]=0.00150428; p3_emc_s_dp[1][9][1]=-0.000370153; p4_emc_s_dp[1][9][1]=2.94776e-05; 
  p0_emc_m_dp[2][0][1]=-0.00187131; p1_emc_m_dp[2][0][1]=-0.00215743; p2_emc_m_dp[2][0][1]=7.37906e-05; p3_emc_m_dp[2][0][1]=3.7223e-05; p4_emc_m_dp[2][0][1]=8.68544e-06; 
  p0_emc_s_dp[2][0][1]=0.00600075; p1_emc_s_dp[2][0][1]=-0.00238328; p2_emc_s_dp[2][0][1]=0.00253175; p3_emc_s_dp[2][0][1]=-0.000680752; p4_emc_s_dp[2][0][1]=6.05999e-05; 
  p0_emc_m_dp[2][1][1]=-0.00184994; p1_emc_m_dp[2][1][1]=-0.00283253; p2_emc_m_dp[2][1][1]=0.000702914; p3_emc_m_dp[2][1][1]=-0.00023515; p4_emc_m_dp[2][1][1]=3.86056e-05; 
  p0_emc_s_dp[2][1][1]=0.00589821; p1_emc_s_dp[2][1][1]=-0.00265117; p2_emc_s_dp[2][1][1]=0.0028027; p3_emc_s_dp[2][1][1]=-0.000757715; p4_emc_s_dp[2][1][1]=6.78324e-05; 
  p0_emc_m_dp[2][2][1]=-0.00169732; p1_emc_m_dp[2][2][1]=-0.00362947; p2_emc_m_dp[2][2][1]=0.00110723; p3_emc_m_dp[2][2][1]=-0.000325974; p4_emc_m_dp[2][2][1]=4.3321e-05; 
  p0_emc_s_dp[2][2][1]=0.00578275; p1_emc_s_dp[2][2][1]=-0.00254058; p2_emc_s_dp[2][2][1]=0.00273161; p3_emc_s_dp[2][2][1]=-0.000742833; p4_emc_s_dp[2][2][1]=6.82784e-05; 
  p0_emc_m_dp[2][3][1]=-0.00152277; p1_emc_m_dp[2][3][1]=-0.00365677; p2_emc_m_dp[2][3][1]=0.000998568; p3_emc_m_dp[2][3][1]=-0.0002847; p4_emc_m_dp[2][3][1]=3.7871e-05; 
  p0_emc_s_dp[2][3][1]=0.0058224; p1_emc_s_dp[2][3][1]=-0.00283185; p2_emc_s_dp[2][3][1]=0.00294907; p3_emc_s_dp[2][3][1]=-0.000808715; p4_emc_s_dp[2][3][1]=7.63324e-05; 
  p0_emc_m_dp[2][4][1]=-0.00192137; p1_emc_m_dp[2][4][1]=-0.00300068; p2_emc_m_dp[2][4][1]=0.000362724; p3_emc_m_dp[2][4][1]=-6.93267e-05; p4_emc_m_dp[2][4][1]=1.37826e-05; 
  p0_emc_s_dp[2][4][1]=0.00573956; p1_emc_s_dp[2][4][1]=-0.00264557; p2_emc_s_dp[2][4][1]=0.00279866; p3_emc_s_dp[2][4][1]=-0.000764981; p4_emc_s_dp[2][4][1]=7.24978e-05; 
  p0_emc_m_dp[2][5][1]=-0.0021161; p1_emc_m_dp[2][5][1]=-0.00306804; p2_emc_m_dp[2][5][1]=0.000545431; p3_emc_m_dp[2][5][1]=-0.000118195; p4_emc_m_dp[2][5][1]=1.65171e-05; 
  p0_emc_s_dp[2][5][1]=0.00558012; p1_emc_s_dp[2][5][1]=-0.00264246; p2_emc_s_dp[2][5][1]=0.00289865; p3_emc_s_dp[2][5][1]=-0.000828386; p4_emc_s_dp[2][5][1]=8.21429e-05; 
  p0_emc_m_dp[2][6][1]=-0.00219826; p1_emc_m_dp[2][6][1]=-0.0028613; p2_emc_m_dp[2][6][1]=0.00043844; p3_emc_m_dp[2][6][1]=-8.13938e-05; p4_emc_m_dp[2][6][1]=1.19385e-05; 
  p0_emc_s_dp[2][6][1]=0.00543159; p1_emc_s_dp[2][6][1]=-0.00241059; p2_emc_s_dp[2][6][1]=0.00272636; p3_emc_s_dp[2][6][1]=-0.00077758; p4_emc_s_dp[2][6][1]=7.70219e-05; 
  p0_emc_m_dp[2][7][1]=-0.00246244; p1_emc_m_dp[2][7][1]=-0.00233842; p2_emc_m_dp[2][7][1]=0.000162028; p3_emc_m_dp[2][7][1]=-2.19127e-06; p4_emc_m_dp[2][7][1]=4.85909e-06; 
  p0_emc_s_dp[2][7][1]=0.00524581; p1_emc_s_dp[2][7][1]=-0.00214596; p2_emc_s_dp[2][7][1]=0.00248783; p3_emc_s_dp[2][7][1]=-0.000700327; p4_emc_s_dp[2][7][1]=6.84584e-05; 
  p0_emc_m_dp[2][8][1]=-0.00260479; p1_emc_m_dp[2][8][1]=-0.00216382; p2_emc_m_dp[2][8][1]=0.000179269; p3_emc_m_dp[2][8][1]=-7.82854e-06; p4_emc_m_dp[2][8][1]=7.1323e-06; 
  p0_emc_s_dp[2][8][1]=0.00500924; p1_emc_s_dp[2][8][1]=-0.00171632; p2_emc_s_dp[2][8][1]=0.00216304; p3_emc_s_dp[2][8][1]=-0.000608628; p4_emc_s_dp[2][8][1]=5.83563e-05; 
  p0_emc_m_dp[2][9][1]=-0.00271722; p1_emc_m_dp[2][9][1]=-0.00198978; p2_emc_m_dp[2][9][1]=0.000248881; p3_emc_m_dp[2][9][1]=-2.80173e-05; p4_emc_m_dp[2][9][1]=1.37808e-05; 
  p0_emc_s_dp[2][9][1]=0.00498507; p1_emc_s_dp[2][9][1]=-0.00170341; p2_emc_s_dp[2][9][1]=0.00211255; p3_emc_s_dp[2][9][1]=-0.000588362; p4_emc_s_dp[2][9][1]=5.63654e-05; 
  
  //! EMC dz neg
  p0_emc_m_dz[0][0][0]=-2.89256; p1_emc_m_dz[0][0][0]=0.853991; p2_emc_m_dz[0][0][0]=-0.774786; p3_emc_m_dz[0][0][0]=0.414789; p4_emc_m_dz[0][0][0]=-0.0587865; 
  p0_emc_s_dz[0][0][0]=3.12292; p1_emc_s_dz[0][0][0]=-0.847176; p2_emc_s_dz[0][0][0]=0.728431; p3_emc_s_dz[0][0][0]=-0.145986; p4_emc_s_dz[0][0][0]=0.0130583; 
  p0_emc_m_dz[0][1][0]=-2.94765; p1_emc_m_dz[0][1][0]=0.559398; p2_emc_m_dz[0][1][0]=-0.468082; p3_emc_m_dz[0][1][0]=0.294009; p4_emc_m_dz[0][1][0]=-0.0429036; 
  p0_emc_s_dz[0][1][0]=3.00532; p1_emc_s_dz[0][1][0]=-0.735431; p2_emc_s_dz[0][1][0]=0.623868; p3_emc_s_dz[0][1][0]=-0.102936; p4_emc_s_dz[0][1][0]=0.00782012; 
  p0_emc_m_dz[0][2][0]=-2.77164; p1_emc_m_dz[0][2][0]=0.281597; p2_emc_m_dz[0][2][0]=-0.218168; p3_emc_m_dz[0][2][0]=0.180217; p4_emc_m_dz[0][2][0]=-0.0276225; 
  p0_emc_s_dz[0][2][0]=2.68479; p1_emc_s_dz[0][2][0]=-0.45296; p2_emc_s_dz[0][2][0]=0.503005; p3_emc_s_dz[0][2][0]=-0.080088; p4_emc_s_dz[0][2][0]=0.00660059; 
  p0_emc_m_dz[0][3][0]=-2.68029; p1_emc_m_dz[0][3][0]=0.324882; p2_emc_m_dz[0][3][0]=-0.267571; p3_emc_m_dz[0][3][0]=0.161126; p4_emc_m_dz[0][3][0]=-0.0229015; 
  p0_emc_s_dz[0][3][0]=2.52811; p1_emc_s_dz[0][3][0]=-0.43621; p2_emc_s_dz[0][3][0]=0.56467; p3_emc_s_dz[0][3][0]=-0.107483; p4_emc_s_dz[0][3][0]=0.00969219; 
  p0_emc_m_dz[0][4][0]=-2.66546; p1_emc_m_dz[0][4][0]=0.384462; p2_emc_m_dz[0][4][0]=-0.245482; p3_emc_m_dz[0][4][0]=0.107959; p4_emc_m_dz[0][4][0]=-0.0135102; 
  p0_emc_s_dz[0][4][0]=2.34457; p1_emc_s_dz[0][4][0]=-0.275226; p2_emc_s_dz[0][4][0]=0.492423; p3_emc_s_dz[0][4][0]=-0.0922754; p4_emc_s_dz[0][4][0]=0.0084431; 
  p0_emc_m_dz[0][5][0]=-2.1207; p1_emc_m_dz[0][5][0]=-0.119679; p2_emc_m_dz[0][5][0]=0.142343; p3_emc_m_dz[0][5][0]=-0.081991; p4_emc_m_dz[0][5][0]=0.0121005; 
  p0_emc_s_dz[0][5][0]=2.30313; p1_emc_s_dz[0][5][0]=-0.260007; p2_emc_s_dz[0][5][0]=0.459875; p3_emc_s_dz[0][5][0]=-0.0736759; p4_emc_s_dz[0][5][0]=0.00541688; 
  p0_emc_m_dz[0][6][0]=-1.69459; p1_emc_m_dz[0][6][0]=-0.741675; p2_emc_m_dz[0][6][0]=0.539109; p3_emc_m_dz[0][6][0]=-0.226502; p4_emc_m_dz[0][6][0]=0.0293051; 
  p0_emc_s_dz[0][6][0]=2.51432; p1_emc_s_dz[0][6][0]=-0.448066; p2_emc_s_dz[0][6][0]=0.551965; p3_emc_s_dz[0][6][0]=-0.0983817; p4_emc_s_dz[0][6][0]=0.0083954; 
  p0_emc_m_dz[0][7][0]=-1.22657; p1_emc_m_dz[0][7][0]=-1.17926; p2_emc_m_dz[0][7][0]=0.932933; p3_emc_m_dz[0][7][0]=-0.412128; p4_emc_m_dz[0][7][0]=0.0538773; 
  p0_emc_s_dz[0][7][0]=2.7564; p1_emc_s_dz[0][7][0]=-0.523849; p2_emc_s_dz[0][7][0]=0.510449; p3_emc_s_dz[0][7][0]=-0.0786304; p4_emc_s_dz[0][7][0]=0.00660543; 
  p0_emc_m_dz[0][8][0]=-1.03016; p1_emc_m_dz[0][8][0]=-1.41142; p2_emc_m_dz[0][8][0]=1.12249; p3_emc_m_dz[0][8][0]=-0.508537; p4_emc_m_dz[0][8][0]=0.0667414; 
  p0_emc_s_dz[0][8][0]=2.85614; p1_emc_s_dz[0][8][0]=-0.533195; p2_emc_s_dz[0][8][0]=0.490033; p3_emc_s_dz[0][8][0]=-0.0698753; p4_emc_s_dz[0][8][0]=0.00532166; 
  p0_emc_m_dz[0][9][0]=-0.996905; p1_emc_m_dz[0][9][0]=-1.36718; p2_emc_m_dz[0][9][0]=1.03551; p3_emc_m_dz[0][9][0]=-0.493647; p4_emc_m_dz[0][9][0]=0.0663239; 
  p0_emc_s_dz[0][9][0]=3.02005; p1_emc_s_dz[0][9][0]=-0.55257; p2_emc_s_dz[0][9][0]=0.438385; p3_emc_s_dz[0][9][0]=-0.0485888; p4_emc_s_dz[0][9][0]=0.00301169; 
  p0_emc_m_dz[1][0][0]=-4.30782; p1_emc_m_dz[1][0][0]=0.468074; p2_emc_m_dz[1][0][0]=-0.263836; p3_emc_m_dz[1][0][0]=0.201701; p4_emc_m_dz[1][0][0]=-0.0324876; 
  p0_emc_s_dz[1][0][0]=3.0503; p1_emc_s_dz[1][0][0]=-0.305632; p2_emc_s_dz[1][0][0]=0.167966; p3_emc_s_dz[1][0][0]=0.0520627; p4_emc_s_dz[1][0][0]=-0.0113461; 
  p0_emc_m_dz[1][1][0]=-3.87293; p1_emc_m_dz[1][1][0]=0.199264; p2_emc_m_dz[1][1][0]=-0.0781079; p3_emc_m_dz[1][1][0]=0.127289; p4_emc_m_dz[1][1][0]=-0.0214018; 
  p0_emc_s_dz[1][1][0]=2.81733; p1_emc_s_dz[1][1][0]=-0.0106538; p2_emc_s_dz[1][1][0]=-0.0946602; p3_emc_s_dz[1][1][0]=0.150802; p4_emc_s_dz[1][1][0]=-0.0232134; 
  p0_emc_m_dz[1][2][0]=-3.33278; p1_emc_m_dz[1][2][0]=0.106884; p2_emc_m_dz[1][2][0]=-0.0808462; p3_emc_m_dz[1][2][0]=0.113178; p4_emc_m_dz[1][2][0]=-0.0179872; 
  p0_emc_s_dz[1][2][0]=2.59831; p1_emc_s_dz[1][2][0]=-0.0736061; p2_emc_s_dz[1][2][0]=0.0792055; p3_emc_s_dz[1][2][0]=0.0787765; p4_emc_s_dz[1][2][0]=-0.014081; 
  p0_emc_m_dz[1][3][0]=-2.63504; p1_emc_m_dz[1][3][0]=-0.244941; p2_emc_m_dz[1][3][0]=0.168588; p3_emc_m_dz[1][3][0]=0.00778048; p4_emc_m_dz[1][3][0]=-0.00480444; 
  p0_emc_s_dz[1][3][0]=2.484; p1_emc_s_dz[1][3][0]=-0.13241; p2_emc_s_dz[1][3][0]=0.263018; p3_emc_s_dz[1][3][0]=-0.00379578; p4_emc_s_dz[1][3][0]=-0.00365047; 
  p0_emc_m_dz[1][4][0]=-2.33982; p1_emc_m_dz[1][4][0]=0.0667687; p2_emc_m_dz[1][4][0]=-0.10623; p3_emc_m_dz[1][4][0]=0.0770303; p4_emc_m_dz[1][4][0]=-0.0107896; 
  p0_emc_s_dz[1][4][0]=2.3815; p1_emc_s_dz[1][4][0]=0.0725033; p2_emc_s_dz[1][4][0]=0.156543; p3_emc_s_dz[1][4][0]=0.0158083; p4_emc_s_dz[1][4][0]=-0.00487005; 
  p0_emc_m_dz[1][5][0]=-1.13675; p1_emc_m_dz[1][5][0]=-0.0880642; p2_emc_m_dz[1][5][0]=-0.0567078; p3_emc_m_dz[1][5][0]=0.017006; p4_emc_m_dz[1][5][0]=-0.00157627; 
  p0_emc_s_dz[1][5][0]=2.42021; p1_emc_s_dz[1][5][0]=-0.0435837; p2_emc_s_dz[1][5][0]=0.252218; p3_emc_s_dz[1][5][0]=-0.016472; p4_emc_s_dz[1][5][0]=-0.00132417; 
  p0_emc_m_dz[1][6][0]=-0.612498; p1_emc_m_dz[1][6][0]=-0.305564; p2_emc_m_dz[1][6][0]=0.0125042; p3_emc_m_dz[1][6][0]=-0.0162832; p4_emc_m_dz[1][6][0]=0.00287235; 
  p0_emc_s_dz[1][6][0]=2.60115; p1_emc_s_dz[1][6][0]=-0.160804; p2_emc_s_dz[1][6][0]=0.330467; p3_emc_s_dz[1][6][0]=-0.0454576; p4_emc_s_dz[1][6][0]=0.00251082; 
  p0_emc_m_dz[1][7][0]=0.128588; p1_emc_m_dz[1][7][0]=-0.521493; p2_emc_m_dz[1][7][0]=0.165618; p3_emc_m_dz[1][7][0]=-0.105327; p4_emc_m_dz[1][7][0]=0.0153407; 
  p0_emc_s_dz[1][7][0]=2.89953; p1_emc_s_dz[1][7][0]=-0.395216; p2_emc_s_dz[1][7][0]=0.541989; p3_emc_s_dz[1][7][0]=-0.13474; p4_emc_s_dz[1][7][0]=0.0145785; 
  p0_emc_m_dz[1][8][0]=0.799284; p1_emc_m_dz[1][8][0]=-0.705871; p2_emc_m_dz[1][8][0]=0.285773; p3_emc_m_dz[1][8][0]=-0.173947; p4_emc_m_dz[1][8][0]=0.0250577; 
  p0_emc_s_dz[1][8][0]=3.1662; p1_emc_s_dz[1][8][0]=-0.685167; p2_emc_s_dz[1][8][0]=0.750053; p3_emc_s_dz[1][8][0]=-0.197196; p4_emc_s_dz[1][8][0]=0.0205906; 
  p0_emc_m_dz[1][9][0]=1.17623; p1_emc_m_dz[1][9][0]=-1.03064; p2_emc_m_dz[1][9][0]=0.520387; p3_emc_m_dz[1][9][0]=-0.258707; p4_emc_m_dz[1][9][0]=0.0354436; 
  p0_emc_s_dz[1][9][0]=3.19393; p1_emc_s_dz[1][9][0]=-0.622675; p2_emc_s_dz[1][9][0]=0.634805; p3_emc_s_dz[1][9][0]=-0.145677; p4_emc_s_dz[1][9][0]=0.014055; 
  p0_emc_m_dz[2][0][0]=-3.09392; p1_emc_m_dz[2][0][0]=0.781983; p2_emc_m_dz[2][0][0]=-0.193614; p3_emc_m_dz[2][0][0]=0.170695; p4_emc_m_dz[2][0][0]=-0.0286694; 
  p0_emc_s_dz[2][0][0]=3.1199; p1_emc_s_dz[2][0][0]=-0.366584; p2_emc_s_dz[2][0][0]=0.52796; p3_emc_s_dz[2][0][0]=-0.111155; p4_emc_s_dz[2][0][0]=0.00971092; 
  p0_emc_m_dz[2][1][0]=-2.76217; p1_emc_m_dz[2][1][0]=0.401227; p2_emc_m_dz[2][1][0]=0.0510534; p3_emc_m_dz[2][1][0]=0.0858792; p4_emc_m_dz[2][1][0]=-0.0177789; 
  p0_emc_s_dz[2][1][0]=2.88934; p1_emc_s_dz[2][1][0]=-0.101463; p2_emc_s_dz[2][1][0]=0.331587; p3_emc_s_dz[2][1][0]=-0.0503399; p4_emc_s_dz[2][1][0]=0.0037369; 
  p0_emc_m_dz[2][2][0]=-2.26563; p1_emc_m_dz[2][2][0]=0.357251; p2_emc_m_dz[2][2][0]=0.0102236; p3_emc_m_dz[2][2][0]=0.0749897; p4_emc_m_dz[2][2][0]=-0.0142667; 
  p0_emc_s_dz[2][2][0]=2.62156; p1_emc_s_dz[2][2][0]=0.119765; p2_emc_s_dz[2][2][0]=0.181477; p3_emc_s_dz[2][2][0]=-0.0032886; p4_emc_s_dz[2][2][0]=-0.00109121; 
  p0_emc_m_dz[2][3][0]=-1.72618; p1_emc_m_dz[2][3][0]=0.31024; p2_emc_m_dz[2][3][0]=-0.0611747; p3_emc_m_dz[2][3][0]=0.0710072; p4_emc_m_dz[2][3][0]=-0.0115717; 
  p0_emc_s_dz[2][3][0]=2.45569; p1_emc_s_dz[2][3][0]=0.0393671; p2_emc_s_dz[2][3][0]=0.271463; p3_emc_s_dz[2][3][0]=-0.0262236; p4_emc_s_dz[2][3][0]=0.0004376; 
  p0_emc_m_dz[2][4][0]=-1.44227; p1_emc_m_dz[2][4][0]=0.263219; p2_emc_m_dz[2][4][0]=-0.121284; p3_emc_m_dz[2][4][0]=0.071865; p4_emc_m_dz[2][4][0]=-0.00969914; 
  p0_emc_s_dz[2][4][0]=2.324; p1_emc_s_dz[2][4][0]=0.124188; p2_emc_s_dz[2][4][0]=0.211609; p3_emc_s_dz[2][4][0]=-0.00597174; p4_emc_s_dz[2][4][0]=-0.00208525; 
  p0_emc_m_dz[2][5][0]=-0.29938; p1_emc_m_dz[2][5][0]=0.218432; p2_emc_m_dz[2][5][0]=-0.22849; p3_emc_m_dz[2][5][0]=0.067924; p4_emc_m_dz[2][5][0]=-0.00684002; 
  p0_emc_s_dz[2][5][0]=2.37007; p1_emc_s_dz[2][5][0]=-0.0417232; p2_emc_s_dz[2][5][0]=0.310071; p3_emc_s_dz[2][5][0]=-0.0232004; p4_emc_s_dz[2][5][0]=-0.00117873; 
  p0_emc_m_dz[2][6][0]=0.123561; p1_emc_m_dz[2][6][0]=0.0131386; p2_emc_m_dz[2][6][0]=-0.104042; p3_emc_m_dz[2][6][0]=-0.00245567; p4_emc_m_dz[2][6][0]=0.00348891; 
  p0_emc_s_dz[2][6][0]=2.41364; p1_emc_s_dz[2][6][0]=-0.0683194; p2_emc_s_dz[2][6][0]=0.322244; p3_emc_s_dz[2][6][0]=-0.0271213; p4_emc_s_dz[2][6][0]=-0.000365849; 
  p0_emc_m_dz[2][7][0]=0.705211; p1_emc_m_dz[2][7][0]=-0.257436; p2_emc_m_dz[2][7][0]=0.0318854; p3_emc_m_dz[2][7][0]=-0.0702941; p4_emc_m_dz[2][7][0]=0.0125585; 
  p0_emc_s_dz[2][7][0]=2.59848; p1_emc_s_dz[2][7][0]=-0.224588; p2_emc_s_dz[2][7][0]=0.390853; p3_emc_s_dz[2][7][0]=-0.0458888; p4_emc_s_dz[2][7][0]=0.00178637; 
  p0_emc_m_dz[2][8][0]=1.24545; p1_emc_m_dz[2][8][0]=-0.455191; p2_emc_m_dz[2][8][0]=0.0948481; p3_emc_m_dz[2][8][0]=-0.107453; p4_emc_m_dz[2][8][0]=0.0182375; 
  p0_emc_s_dz[2][8][0]=2.88878; p1_emc_s_dz[2][8][0]=-0.451067; p2_emc_s_dz[2][8][0]=0.478722; p3_emc_s_dz[2][8][0]=-0.0664444; p4_emc_s_dz[2][8][0]=0.00345475; 
  p0_emc_m_dz[2][9][0]=1.54655; p1_emc_m_dz[2][9][0]=-0.856334; p2_emc_m_dz[2][9][0]=0.29913; p3_emc_m_dz[2][9][0]=-0.171177; p4_emc_m_dz[2][9][0]=0.0262333; 
  p0_emc_s_dz[2][9][0]=3.00506; p1_emc_s_dz[2][9][0]=-0.403092; p2_emc_s_dz[2][9][0]=0.400326; p3_emc_s_dz[2][9][0]=-0.0427501; p4_emc_s_dz[2][9][0]=0.00128877;
      
  //! EMC dz pos
  p0_emc_m_dz[0][0][1]=-3.34233; p1_emc_m_dz[0][0][1]=2.07394; p2_emc_m_dz[0][0][1]=-1.33319; p3_emc_m_dz[0][0][1]=0.595282; p4_emc_m_dz[0][0][1]=-0.079556; 
  p0_emc_s_dz[0][0][1]=3.01936; p1_emc_s_dz[0][0][1]=-1.28828; p2_emc_s_dz[0][0][1]=1.33919; p3_emc_s_dz[0][0][1]=-0.364857; p4_emc_s_dz[0][0][1]=0.0370168; 
  p0_emc_m_dz[0][1][1]=-3.33275; p1_emc_m_dz[0][1][1]=1.64849; p2_emc_m_dz[0][1][1]=-0.989868; p3_emc_m_dz[0][1][1]=0.457051; p4_emc_m_dz[0][1][1]=-0.0614223; 
  p0_emc_s_dz[0][1][1]=2.9152; p1_emc_s_dz[0][1][1]=-1.22716; p2_emc_s_dz[0][1][1]=1.29519; p3_emc_s_dz[0][1][1]=-0.342973; p4_emc_s_dz[0][1][1]=0.033857; 
  p0_emc_m_dz[0][2][1]=-3.10208; p1_emc_m_dz[0][2][1]=1.23688; p2_emc_m_dz[0][2][1]=-0.736049; p3_emc_m_dz[0][2][1]=0.346764; p4_emc_m_dz[0][2][1]=-0.0469987; 
  p0_emc_s_dz[0][2][1]=2.69071; p1_emc_s_dz[0][2][1]=-1.16325; p2_emc_s_dz[0][2][1]=1.36793; p3_emc_s_dz[0][2][1]=-0.389559; p4_emc_s_dz[0][2][1]=0.0409832; 
  p0_emc_m_dz[0][3][1]=-2.82777; p1_emc_m_dz[0][3][1]=0.771348; p2_emc_m_dz[0][3][1]=-0.456267; p3_emc_m_dz[0][3][1]=0.222525; p4_emc_m_dz[0][3][1]=-0.0304852; 
  p0_emc_s_dz[0][3][1]=2.51989; p1_emc_s_dz[0][3][1]=-1.07153; p2_emc_s_dz[0][3][1]=1.37586; p3_emc_s_dz[0][3][1]=-0.404283; p4_emc_s_dz[0][3][1]=0.0431439; 
  p0_emc_m_dz[0][4][1]=-2.65239; p1_emc_m_dz[0][4][1]=0.466837; p2_emc_m_dz[0][4][1]=-0.279972; p3_emc_m_dz[0][4][1]=0.129383; p4_emc_m_dz[0][4][1]=-0.016848; 
  p0_emc_s_dz[0][4][1]=2.36275; p1_emc_s_dz[0][4][1]=-0.93778; p2_emc_s_dz[0][4][1]=1.32384; p3_emc_s_dz[0][4][1]=-0.396312; p4_emc_s_dz[0][4][1]=0.0428281; 
  p0_emc_m_dz[0][5][1]=-2.02743; p1_emc_m_dz[0][5][1]=-0.582563; p2_emc_m_dz[0][5][1]=0.484814; p3_emc_m_dz[0][5][1]=-0.189377; p4_emc_m_dz[0][5][1]=0.0241854; 
  p0_emc_s_dz[0][5][1]=2.30538; p1_emc_s_dz[0][5][1]=-0.940407; p2_emc_s_dz[0][5][1]=1.35227; p3_emc_s_dz[0][5][1]=-0.405948; p4_emc_s_dz[0][5][1]=0.0438707; 
  p0_emc_m_dz[0][6][1]=-1.63182; p1_emc_m_dz[0][6][1]=-1.23158; p2_emc_m_dz[0][6][1]=0.78458; p3_emc_m_dz[0][6][1]=-0.301487; p4_emc_m_dz[0][6][1]=0.0382074; 
  p0_emc_s_dz[0][6][1]=2.53311; p1_emc_s_dz[0][6][1]=-1.18567; p2_emc_s_dz[0][6][1]=1.48246; p3_emc_s_dz[0][6][1]=-0.436478; p4_emc_s_dz[0][6][1]=0.0466544; 
  p0_emc_m_dz[0][7][1]=-1.11261; p1_emc_m_dz[0][7][1]=-1.73513; p2_emc_m_dz[0][7][1]=1.08698; p3_emc_m_dz[0][7][1]=-0.438373; p4_emc_m_dz[0][7][1]=0.0562133; 
  p0_emc_s_dz[0][7][1]=2.71619; p1_emc_s_dz[0][7][1]=-1.2133; p2_emc_s_dz[0][7][1]=1.43249; p3_emc_s_dz[0][7][1]=-0.415386; p4_emc_s_dz[0][7][1]=0.0443919; 
  p0_emc_m_dz[0][8][1]=-0.744994; p1_emc_m_dz[0][8][1]=-2.42766; p2_emc_m_dz[0][8][1]=1.59391; p3_emc_m_dz[0][8][1]=-0.639722; p4_emc_m_dz[0][8][1]=0.0806118; 
  p0_emc_s_dz[0][8][1]=2.80027; p1_emc_s_dz[0][8][1]=-1.21809; p2_emc_s_dz[0][8][1]=1.42328; p3_emc_s_dz[0][8][1]=-0.409292; p4_emc_s_dz[0][8][1]=0.0432205; 
  p0_emc_m_dz[0][9][1]=-0.529556; p1_emc_m_dz[0][9][1]=-3.01522; p2_emc_m_dz[0][9][1]=2.02872; p3_emc_m_dz[0][9][1]=-0.818452; p4_emc_m_dz[0][9][1]=0.102537; 
  p0_emc_s_dz[0][9][1]=2.98403; p1_emc_s_dz[0][9][1]=-1.31043; p2_emc_s_dz[0][9][1]=1.43212; p3_emc_s_dz[0][9][1]=-0.4067; p4_emc_s_dz[0][9][1]=0.0424526; 
  p0_emc_m_dz[1][0][1]=-4.96956; p1_emc_m_dz[1][0][1]=1.81571; p2_emc_m_dz[1][0][1]=-0.75643; p3_emc_m_dz[1][0][1]=0.282723; p4_emc_m_dz[1][0][1]=-0.0363968; 
  p0_emc_s_dz[1][0][1]=2.99186; p1_emc_s_dz[1][0][1]=-1.19508; p2_emc_s_dz[1][0][1]=1.40548; p3_emc_s_dz[1][0][1]=-0.422778; p4_emc_s_dz[1][0][1]=0.043998; 
  p0_emc_m_dz[1][1][1]=-4.43994; p1_emc_m_dz[1][1][1]=1.33735; p2_emc_m_dz[1][1][1]=-0.469485; p3_emc_m_dz[1][1][1]=0.182088; p4_emc_m_dz[1][1][1]=-0.022828; 
  p0_emc_s_dz[1][1][1]=2.89591; p1_emc_s_dz[1][1][1]=-1.16888; p2_emc_s_dz[1][1][1]=1.31211; p3_emc_s_dz[1][1][1]=-0.3673; p4_emc_s_dz[1][1][1]=0.035905; 
  p0_emc_m_dz[1][2][1]=-3.77065; p1_emc_m_dz[1][2][1]=0.909125; p2_emc_m_dz[1][2][1]=-0.296128; p3_emc_m_dz[1][2][1]=0.128238; p4_emc_m_dz[1][2][1]=-0.0167726; 
  p0_emc_s_dz[1][2][1]=2.78495; p1_emc_s_dz[1][2][1]=-1.15272; p2_emc_s_dz[1][2][1]=1.28727; p3_emc_s_dz[1][2][1]=-0.357129; p4_emc_s_dz[1][2][1]=0.0351941; 
  p0_emc_m_dz[1][3][1]=-3.00237; p1_emc_m_dz[1][3][1]=0.500385; p2_emc_m_dz[1][3][1]=-0.137281; p3_emc_m_dz[1][3][1]=0.0706905; p4_emc_m_dz[1][3][1]=-0.00986497; 
  p0_emc_s_dz[1][3][1]=2.62967; p1_emc_s_dz[1][3][1]=-0.932289; p2_emc_s_dz[1][3][1]=1.13106; p3_emc_s_dz[1][3][1]=-0.309244; p4_emc_s_dz[1][3][1]=0.0300565; 
  p0_emc_m_dz[1][4][1]=-2.44054; p1_emc_m_dz[1][4][1]=0.0732046; p2_emc_m_dz[1][4][1]=0.13218; p3_emc_m_dz[1][4][1]=-0.0309236; p4_emc_m_dz[1][4][1]=0.00263943; 
  p0_emc_s_dz[1][4][1]=2.5798; p1_emc_s_dz[1][4][1]=-0.775279; p2_emc_s_dz[1][4][1]=0.989246; p3_emc_s_dz[1][4][1]=-0.261315; p4_emc_s_dz[1][4][1]=0.0246627; 
  p0_emc_m_dz[1][5][1]=-1.04259; p1_emc_m_dz[1][5][1]=-0.368561; p2_emc_m_dz[1][5][1]=0.241873; p3_emc_m_dz[1][5][1]=-0.0739593; p4_emc_m_dz[1][5][1]=0.00730511; 
  p0_emc_s_dz[1][5][1]=2.64874; p1_emc_s_dz[1][5][1]=-0.897769; p2_emc_s_dz[1][5][1]=1.08506; p3_emc_s_dz[1][5][1]=-0.294744; p4_emc_s_dz[1][5][1]=0.0285976; 
  p0_emc_m_dz[1][6][1]=-0.444657; p1_emc_m_dz[1][6][1]=-0.596617; p2_emc_m_dz[1][6][1]=0.218531; p3_emc_m_dz[1][6][1]=-0.0637592; p4_emc_m_dz[1][6][1]=0.00617474; 
  p0_emc_s_dz[1][6][1]=2.74661; p1_emc_s_dz[1][6][1]=-0.79186; p2_emc_s_dz[1][6][1]=1.00749; p3_emc_s_dz[1][6][1]=-0.274898; p4_emc_s_dz[1][6][1]=0.0268319; 
  p0_emc_m_dz[1][7][1]=0.283669; p1_emc_m_dz[1][7][1]=-0.615213; p2_emc_m_dz[1][7][1]=0.0603751; p3_emc_m_dz[1][7][1]=-0.0238768; p4_emc_m_dz[1][7][1]=0.00248377; 
  p0_emc_s_dz[1][7][1]=2.91154; p1_emc_s_dz[1][7][1]=-0.793571; p2_emc_s_dz[1][7][1]=1.05413; p3_emc_s_dz[1][7][1]=-0.307413; p4_emc_s_dz[1][7][1]=0.0316164; 
  p0_emc_m_dz[1][8][1]=1.06337; p1_emc_m_dz[1][8][1]=-0.958872; p2_emc_m_dz[1][8][1]=0.180312; p3_emc_m_dz[1][8][1]=-0.0688876; p4_emc_m_dz[1][8][1]=0.00793334; 
  p0_emc_s_dz[1][8][1]=3.06033; p1_emc_s_dz[1][8][1]=-0.912749; p2_emc_s_dz[1][8][1]=1.18558; p3_emc_s_dz[1][8][1]=-0.356981; p4_emc_s_dz[1][8][1]=0.0370401; 
  p0_emc_m_dz[1][9][1]=1.50661; p1_emc_m_dz[1][9][1]=-1.40939; p2_emc_m_dz[1][9][1]=0.437428; p3_emc_m_dz[1][9][1]=-0.148516; p4_emc_m_dz[1][9][1]=0.0166879; 
  p0_emc_s_dz[1][9][1]=3.08483; p1_emc_s_dz[1][9][1]=-0.987064; p2_emc_s_dz[1][9][1]=1.30421; p3_emc_s_dz[1][9][1]=-0.407357; p4_emc_s_dz[1][9][1]=0.0430449; 
  p0_emc_m_dz[2][0][1]=-3.81432; p1_emc_m_dz[2][0][1]=1.6219; p2_emc_m_dz[2][0][1]=-0.675132; p3_emc_m_dz[2][0][1]=0.246568; p4_emc_m_dz[2][0][1]=-0.0315264; 
  p0_emc_s_dz[2][0][1]=3.01585; p1_emc_s_dz[2][0][1]=-1.09376; p2_emc_s_dz[2][0][1]=1.49894; p3_emc_s_dz[2][0][1]=-0.486178; p4_emc_s_dz[2][0][1]=0.0541842; 
  p0_emc_m_dz[2][1][1]=-3.21828; p1_emc_m_dz[2][1][1]=0.74512; p2_emc_m_dz[2][1][1]=-0.0358384; p3_emc_m_dz[2][1][1]=0.0403152; p4_emc_m_dz[2][1][1]=-0.0073888; 
  p0_emc_s_dz[2][1][1]=2.95474; p1_emc_s_dz[2][1][1]=-1.01582; p2_emc_s_dz[2][1][1]=1.37627; p3_emc_s_dz[2][1][1]=-0.4362; p4_emc_s_dz[2][1][1]=0.0485195; 
  p0_emc_m_dz[2][2][1]=-2.63172; p1_emc_m_dz[2][2][1]=0.76716; p2_emc_m_dz[2][2][1]=-0.160532; p3_emc_m_dz[2][2][1]=0.0658; p4_emc_m_dz[2][2][1]=-0.0083837; 
  p0_emc_s_dz[2][2][1]=2.74904; p1_emc_s_dz[2][2][1]=-0.738317; p2_emc_s_dz[2][2][1]=1.07075; p3_emc_s_dz[2][2][1]=-0.316875; p4_emc_s_dz[2][2][1]=0.0340792; 
  p0_emc_m_dz[2][3][1]=-1.95412; p1_emc_m_dz[2][3][1]=0.657087; p2_emc_m_dz[2][3][1]=-0.211656; p3_emc_m_dz[2][3][1]=0.0719725; p4_emc_m_dz[2][3][1]=-0.00815499; 
  p0_emc_s_dz[2][3][1]=2.63359; p1_emc_s_dz[2][3][1]=-0.805172; p2_emc_s_dz[2][3][1]=1.07839; p3_emc_s_dz[2][3][1]=-0.300602; p4_emc_s_dz[2][3][1]=0.0306633; 
  p0_emc_m_dz[2][4][1]=-1.47523; p1_emc_m_dz[2][4][1]=0.325427; p2_emc_m_dz[2][4][1]=-0.0597711; p3_emc_m_dz[2][4][1]=0.0244079; p4_emc_m_dz[2][4][1]=-0.00265659; 
  p0_emc_s_dz[2][4][1]=2.55238; p1_emc_s_dz[2][4][1]=-0.852513; p2_emc_s_dz[2][4][1]=1.11692; p3_emc_s_dz[2][4][1]=-0.31085; p4_emc_s_dz[2][4][1]=0.0313251; 
  p0_emc_m_dz[2][5][1]=-0.0837147; p1_emc_m_dz[2][5][1]=-0.205614; p2_emc_m_dz[2][5][1]=0.133339; p3_emc_m_dz[2][5][1]=-0.0488896; p4_emc_m_dz[2][5][1]=0.0058528; 
  p0_emc_s_dz[2][5][1]=2.60079; p1_emc_s_dz[2][5][1]=-1.06368; p2_emc_s_dz[2][5][1]=1.2792; p3_emc_s_dz[2][5][1]=-0.355861; p4_emc_s_dz[2][5][1]=0.0351613; 
  p0_emc_m_dz[2][6][1]=0.426787; p1_emc_m_dz[2][6][1]=-0.43167; p2_emc_m_dz[2][6][1]=0.232529; p3_emc_m_dz[2][6][1]=-0.0890845; p4_emc_m_dz[2][6][1]=0.0109033; 
  p0_emc_s_dz[2][6][1]=2.61581; p1_emc_s_dz[2][6][1]=-1.07653; p2_emc_s_dz[2][6][1]=1.32065; p3_emc_s_dz[2][6][1]=-0.37477; p4_emc_s_dz[2][6][1]=0.0381463; 
  p0_emc_m_dz[2][7][1]=1.10492; p1_emc_m_dz[2][7][1]=-0.72365; p2_emc_m_dz[2][7][1]=0.343204; p3_emc_m_dz[2][7][1]=-0.132727; p4_emc_m_dz[2][7][1]=0.0162431; 
  p0_emc_s_dz[2][7][1]=2.73563; p1_emc_s_dz[2][7][1]=-1.18675; p2_emc_s_dz[2][7][1]=1.3991; p3_emc_s_dz[2][7][1]=-0.405078; p4_emc_s_dz[2][7][1]=0.0423841; 
  p0_emc_m_dz[2][8][1]=1.76235; p1_emc_m_dz[2][8][1]=-1.1119; p2_emc_m_dz[2][8][1]=0.531193; p3_emc_m_dz[2][8][1]=-0.196502; p4_emc_m_dz[2][8][1]=0.0239298; 
  p0_emc_s_dz[2][8][1]=2.95599; p1_emc_s_dz[2][8][1]=-1.3448; p2_emc_s_dz[2][8][1]=1.49968; p3_emc_s_dz[2][8][1]=-0.441728; p4_emc_s_dz[2][8][1]=0.0465754; 
  p0_emc_m_dz[2][9][1]=2.20232; p1_emc_m_dz[2][9][1]=-1.73624; p2_emc_m_dz[2][9][1]=0.908818; p3_emc_m_dz[2][9][1]=-0.308467; p4_emc_m_dz[2][9][1]=0.036789; 
  p0_emc_s_dz[2][9][1]=3.00075; p1_emc_s_dz[2][9][1]=-1.22726; p2_emc_s_dz[2][9][1]=1.42629; p3_emc_s_dz[2][9][1]=-0.426368; p4_emc_s_dz[2][9][1]=0.0460805; 
  
  //! ToF dphi neg
  p0_tof_m_dp[0][0][0][0]=-0.00109038; p1_tof_m_dp[0][0][0][0]=0.00612416; p2_tof_m_dp[0][0][0][0]=-0.00458627; p3_tof_m_dp[0][0][0][0]=0.00147688; p4_tof_m_dp[0][0][0][0]=-0.00017287; 
  p0_tof_m_dp[1][0][0][0]=-0.000243114; p1_tof_m_dp[1][0][0][0]=0.00429458; p2_tof_m_dp[1][0][0][0]=0.000229069; p3_tof_m_dp[1][0][0][0]=-0.0102303; p4_tof_m_dp[1][0][0][0]=0.00855224; 
  p0_tof_s_dp[0][0][0]=0.00515352; p1_tof_s_dp[0][0][0]=-0.000112409; p2_tof_s_dp[0][0][0]=0.000402939; p3_tof_s_dp[0][0][0]=-4.72677e-05; p4_tof_s_dp[0][0][0]=4.54174e-06; 
  p0_tof_m_dp[0][0][1][0]=-0.000686657; p1_tof_m_dp[0][0][1][0]=0.00490085; p2_tof_m_dp[0][0][1][0]=-0.00336086; p3_tof_m_dp[0][0][1][0]=0.00111188; p4_tof_m_dp[0][0][1][0]=-0.00013009; 
  p0_tof_m_dp[1][0][1][0]=0.00130939; p1_tof_m_dp[1][0][1][0]=-0.00124089; p2_tof_m_dp[1][0][1][0]=0.00143769; p3_tof_m_dp[1][0][1][0]=0.00582989; p4_tof_m_dp[1][0][1][0]=-0.00732542; 
  p0_tof_s_dp[0][1][0]=0.00510971; p1_tof_s_dp[0][1][0]=-0.00011287; p2_tof_s_dp[0][1][0]=0.000410614; p3_tof_s_dp[0][1][0]=-2.34538e-05; p4_tof_s_dp[0][1][0]=-3.66763e-06; 
  p0_tof_m_dp[0][0][2][0]=-0.00181485; p1_tof_m_dp[0][0][2][0]=0.00800936; p2_tof_m_dp[0][0][2][0]=-0.00562819; p3_tof_m_dp[0][0][2][0]=0.00179476; p4_tof_m_dp[0][0][2][0]=-0.000197564; 
  p0_tof_m_dp[1][0][2][0]=0.00103841; p1_tof_m_dp[1][0][2][0]=0.000660126; p2_tof_m_dp[1][0][2][0]=0.000205628; p3_tof_m_dp[1][0][2][0]=-5.91043e-05; p4_tof_m_dp[1][0][2][0]=0.000415016; 
  p0_tof_s_dp[0][2][0]=0.00510193; p1_tof_s_dp[0][2][0]=-0.000147977; p2_tof_s_dp[0][2][0]=0.00063955; p3_tof_s_dp[0][2][0]=-0.000161246; p4_tof_s_dp[0][2][0]=2.03753e-05; 
  p0_tof_m_dp[0][0][3][0]=-0.00105439; p1_tof_m_dp[0][0][3][0]=0.00641076; p2_tof_m_dp[0][0][3][0]=-0.00407172; p3_tof_m_dp[0][0][3][0]=0.00127765; p4_tof_m_dp[0][0][3][0]=-0.000140542; 
  p0_tof_m_dp[1][0][3][0]=0.00165997; p1_tof_m_dp[1][0][3][0]=-0.00164916; p2_tof_m_dp[1][0][3][0]=0.00209776; p3_tof_m_dp[1][0][3][0]=0.00653648; p4_tof_m_dp[1][0][3][0]=-0.00813214; 
  p0_tof_s_dp[0][3][0]=0.00552588; p1_tof_s_dp[0][3][0]=-0.000384466; p2_tof_s_dp[0][3][0]=0.000911842; p3_tof_s_dp[0][3][0]=-0.000275895; p4_tof_s_dp[0][3][0]=3.65191e-05; 
  p0_tof_m_dp[0][0][4][0]=0.000391759; p1_tof_m_dp[0][0][4][0]=0.00230084; p2_tof_m_dp[0][0][4][0]=-0.000535265; p3_tof_m_dp[0][0][4][0]=0.000150335; p4_tof_m_dp[0][0][4][0]=-2.03472e-05; 
  p0_tof_m_dp[1][0][4][0]=0.00048642; p1_tof_m_dp[1][0][4][0]=0.00379936; p2_tof_m_dp[1][0][4][0]=-0.000919324; p3_tof_m_dp[1][0][4][0]=-0.00790368; p4_tof_m_dp[1][0][4][0]=0.00773629; 
  p0_tof_s_dp[0][4][0]=0.00617614; p1_tof_s_dp[0][4][0]=-0.00165694; p2_tof_s_dp[0][4][0]=0.00169624; p3_tof_s_dp[0][4][0]=-0.000461109; p4_tof_s_dp[0][4][0]=5.2015e-05; 
  p0_tof_m_dp[0][0][5][0]=-0.00136994; p1_tof_m_dp[0][0][5][0]=0.00724494; p2_tof_m_dp[0][0][5][0]=-0.00511203; p3_tof_m_dp[0][0][5][0]=0.00163992; p4_tof_m_dp[0][0][5][0]=-0.000178606; 
  p0_tof_m_dp[1][0][5][0]=0.00290964; p1_tof_m_dp[1][0][5][0]=-0.00637032; p2_tof_m_dp[1][0][5][0]=0.00343668; p3_tof_m_dp[1][0][5][0]=0.0200916; p4_tof_m_dp[1][0][5][0]=-0.0224326; 
  p0_tof_s_dp[0][5][0]=0.00525195; p1_tof_s_dp[0][5][0]=-0.000426426; p2_tof_s_dp[0][5][0]=0.00111807; p3_tof_s_dp[0][5][0]=-0.000399623; p4_tof_s_dp[0][5][0]=5.49501e-05; 
  p0_tof_m_dp[0][0][6][0]=-0.00168013; p1_tof_m_dp[0][0][6][0]=0.00773918; p2_tof_m_dp[0][0][6][0]=-0.00585029; p3_tof_m_dp[0][0][6][0]=0.00193856; p4_tof_m_dp[0][0][6][0]=-0.000215737; 
  p0_tof_m_dp[1][0][6][0]=0.00141747; p1_tof_m_dp[1][0][6][0]=0.000449118; p2_tof_m_dp[1][0][6][0]=-0.000619075; p3_tof_m_dp[1][0][6][0]=-0.000890339; p4_tof_m_dp[1][0][6][0]=0.00149493; 
  p0_tof_s_dp[0][6][0]=0.00482545; p1_tof_s_dp[0][6][0]=0.000599188; p2_tof_s_dp[0][6][0]=0.000165153; p3_tof_s_dp[0][6][0]=-7.50067e-05; p4_tof_s_dp[0][6][0]=1.77564e-05; 
  p0_tof_m_dp[0][0][7][0]=-0.000235103; p1_tof_m_dp[0][0][7][0]=0.00485174; p2_tof_m_dp[0][0][7][0]=-0.00445552; p3_tof_m_dp[0][0][7][0]=0.00168286; p4_tof_m_dp[0][0][7][0]=-0.000202968; 
  p0_tof_m_dp[1][0][7][0]=-0.00142533; p1_tof_m_dp[1][0][7][0]=0.00986455; p2_tof_m_dp[1][0][7][0]=-0.000857394; p3_tof_m_dp[1][0][7][0]=-0.0256797; p4_tof_m_dp[1][0][7][0]=0.0206958; 
  p0_tof_s_dp[0][7][0]=0.00394471; p1_tof_s_dp[0][7][0]=0.00366385; p2_tof_s_dp[0][7][0]=-0.00250176; p3_tof_s_dp[0][7][0]=0.000786379; p4_tof_s_dp[0][7][0]=-7.6836e-05; 
  p0_tof_m_dp[0][0][8][0]=0.00134885; p1_tof_m_dp[0][0][8][0]=0.00125477; p2_tof_m_dp[0][0][8][0]=-0.00210692; p3_tof_m_dp[0][0][8][0]=0.00101247; p4_tof_m_dp[0][0][8][0]=-0.000137973; 
  p0_tof_m_dp[1][0][8][0]=-0.00393842; p1_tof_m_dp[1][0][8][0]=0.0205559; p2_tof_m_dp[1][0][8][0]=-0.00580273; p3_tof_m_dp[1][0][8][0]=-0.0564559; p4_tof_m_dp[1][0][8][0]=0.0547137; 
  p0_tof_s_dp[0][8][0]=0.00310739; p1_tof_s_dp[0][8][0]=0.00614232; p2_tof_s_dp[0][8][0]=-0.00463488; p3_tof_s_dp[0][8][0]=0.00146854; p4_tof_s_dp[0][8][0]=-0.000150741; 
  p0_tof_m_dp[0][0][9][0]=0.00129379; p1_tof_m_dp[0][0][9][0]=0.000214318; p2_tof_m_dp[0][0][9][0]=-0.0012993; p3_tof_m_dp[0][0][9][0]=0.000714714; p4_tof_m_dp[0][0][9][0]=-0.000107328; 
  p0_tof_m_dp[1][0][9][0]=-0.00907901; p1_tof_m_dp[1][0][9][0]=0.03962; p2_tof_m_dp[1][0][9][0]=-0.00991931; p3_tof_m_dp[1][0][9][0]=-0.110751; p4_tof_m_dp[1][0][9][0]=0.105839; 
  p0_tof_s_dp[0][9][0]=0.00341811; p1_tof_s_dp[0][9][0]=0.00508666; p2_tof_s_dp[0][9][0]=-0.00374945; p3_tof_s_dp[0][9][0]=0.00118853; p4_tof_s_dp[0][9][0]=-0.000121157; 
  p0_tof_m_dp[0][1][0][0]=-0.0018101; p1_tof_m_dp[0][1][0][0]=0.00402304; p2_tof_m_dp[0][1][0][0]=-0.00368006; p3_tof_m_dp[0][1][0][0]=0.00103402; p4_tof_m_dp[0][1][0][0]=-9.34097e-05; 
  p0_tof_m_dp[1][1][0][0]=0.00214653; p1_tof_m_dp[1][1][0][0]=-0.00852462; p2_tof_m_dp[1][1][0][0]=0.00194781; p3_tof_m_dp[1][1][0][0]=0.0237392; p4_tof_m_dp[1][1][0][0]=-0.0237119; 
  p0_tof_s_dp[1][0][0]=0.000923958; p1_tof_s_dp[1][0][0]=0.00514852; p2_tof_s_dp[1][0][0]=-0.00340768; p3_tof_s_dp[1][0][0]=0.00105022; p4_tof_s_dp[1][0][0]=-0.000101591; 
  p0_tof_m_dp[0][1][1][0]=-0.00135331; p1_tof_m_dp[0][1][1][0]=0.00371349; p2_tof_m_dp[0][1][1][0]=-0.00285539; p3_tof_m_dp[0][1][1][0]=0.000707962; p4_tof_m_dp[0][1][1][0]=-4.92537e-05; 
  p0_tof_m_dp[1][1][1][0]=0.00289909; p1_tof_m_dp[1][1][1][0]=-0.0100628; p2_tof_m_dp[1][1][1][0]=0.00301739; p3_tof_m_dp[1][1][1][0]=0.0269933; p4_tof_m_dp[1][1][1][0]=-0.0269758; 
  p0_tof_s_dp[1][1][0]=0.0011332; p1_tof_s_dp[1][1][0]=0.00442182; p2_tof_s_dp[1][1][0]=-0.00286401; p3_tof_s_dp[1][1][0]=0.000898803; p4_tof_s_dp[1][1][0]=-8.77609e-05; 
  p0_tof_m_dp[0][1][2][0]=-0.00119671; p1_tof_m_dp[0][1][2][0]=0.00373951; p2_tof_m_dp[0][1][2][0]=-0.00276533; p3_tof_m_dp[0][1][2][0]=0.000716516; p4_tof_m_dp[0][1][2][0]=-5.32749e-05; 
  p0_tof_m_dp[1][1][2][0]=0.00287162; p1_tof_m_dp[1][1][2][0]=-0.0081919; p2_tof_m_dp[1][1][2][0]=0.00150287; p3_tof_m_dp[1][1][2][0]=0.0217067; p4_tof_m_dp[1][1][2][0]=-0.0205692; 
  p0_tof_s_dp[1][2][0]=0.00180945; p1_tof_s_dp[1][2][0]=0.00274554; p2_tof_s_dp[1][2][0]=-0.00138257; p3_tof_s_dp[1][2][0]=0.000380622; p4_tof_s_dp[1][2][0]=-2.65795e-05; 
  p0_tof_m_dp[0][1][3][0]=-0.00141062; p1_tof_m_dp[0][1][3][0]=0.0034855; p2_tof_m_dp[0][1][3][0]=-0.00177049; p3_tof_m_dp[0][1][3][0]=0.000204257; p4_tof_m_dp[0][1][3][0]=2.11677e-05; 
  p0_tof_m_dp[1][1][3][0]=0.00302346; p1_tof_m_dp[1][1][3][0]=-0.0104093; p2_tof_m_dp[1][1][3][0]=0.00270977; p3_tof_m_dp[1][1][3][0]=0.0289113; p4_tof_m_dp[1][1][3][0]=-0.0280325; 
  p0_tof_s_dp[1][3][0]=0.00205514; p1_tof_s_dp[1][3][0]=0.00200796; p2_tof_s_dp[1][3][0]=-0.00101189; p3_tof_s_dp[1][3][0]=0.000356074; p4_tof_s_dp[1][3][0]=-3.40602e-05; 
  p0_tof_m_dp[0][1][4][0]=-0.00187413; p1_tof_m_dp[0][1][4][0]=0.00413198; p2_tof_m_dp[0][1][4][0]=-0.00281902; p3_tof_m_dp[0][1][4][0]=0.000786484; p4_tof_m_dp[0][1][4][0]=-6.79157e-05; 
  p0_tof_m_dp[1][1][4][0]=0.00287317; p1_tof_m_dp[1][1][4][0]=-0.00955072; p2_tof_m_dp[1][1][4][0]=0.000479637; p3_tof_m_dp[1][1][4][0]=0.0253122; p4_tof_m_dp[1][1][4][0]=-0.0214887; 
  p0_tof_s_dp[1][4][0]=0.00257151; p1_tof_s_dp[1][4][0]=0.000910261; p2_tof_s_dp[1][4][0]=2.66447e-05; p3_tof_s_dp[1][4][0]=-4.39347e-05; p4_tof_s_dp[1][4][0]=1.77786e-05; 
  p0_tof_m_dp[0][1][5][0]=-0.00358235; p1_tof_m_dp[0][1][5][0]=0.0102477; p2_tof_m_dp[0][1][5][0]=-0.00823597; p3_tof_m_dp[0][1][5][0]=0.00272578; p4_tof_m_dp[0][1][5][0]=-0.000301385; 
  p0_tof_m_dp[1][1][5][0]=0.00179917; p1_tof_m_dp[1][1][5][0]=-0.00435146; p2_tof_m_dp[1][1][5][0]=-0.00051108; p3_tof_m_dp[1][1][5][0]=0.0112845; p4_tof_m_dp[1][1][5][0]=-0.00765356; 
  p0_tof_s_dp[1][5][0]=0.00286988; p1_tof_s_dp[1][5][0]=0.00068708; p2_tof_s_dp[1][5][0]=-0.000180187; p3_tof_s_dp[1][5][0]=0.000174304; p4_tof_s_dp[1][5][0]=-2.06251e-05; 
  p0_tof_m_dp[0][1][6][0]=-0.00316294; p1_tof_m_dp[0][1][6][0]=0.00935174; p2_tof_m_dp[0][1][6][0]=-0.00805007; p3_tof_m_dp[0][1][6][0]=0.00279967; p4_tof_m_dp[0][1][6][0]=-0.000320216; 
  p0_tof_m_dp[1][1][6][0]=0.00433467; p1_tof_m_dp[1][1][6][0]=-0.0151291; p2_tof_m_dp[1][1][6][0]=0.00295111; p3_tof_m_dp[1][1][6][0]=0.0402692; p4_tof_m_dp[1][1][6][0]=-0.0361556; 
  p0_tof_s_dp[1][6][0]=0.00298536; p1_tof_s_dp[1][6][0]=0.000178907; p2_tof_s_dp[1][6][0]=0.000164967; p3_tof_s_dp[1][6][0]=8.10138e-05; p4_tof_s_dp[1][6][0]=-1.18124e-05; 
  p0_tof_m_dp[0][1][7][0]=-0.00413529; p1_tof_m_dp[0][1][7][0]=0.010888; p2_tof_m_dp[0][1][7][0]=-0.00899169; p3_tof_m_dp[0][1][7][0]=0.00297582; p4_tof_m_dp[0][1][7][0]=-0.000329517; 
  p0_tof_m_dp[1][1][7][0]=0.00221781; p1_tof_m_dp[1][1][7][0]=-0.00756705; p2_tof_m_dp[1][1][7][0]=0.000472287; p3_tof_m_dp[1][1][7][0]=0.0188889; p4_tof_m_dp[1][1][7][0]=-0.0145037; 
  p0_tof_s_dp[1][7][0]=0.00247803; p1_tof_s_dp[1][7][0]=0.00114183; p2_tof_s_dp[1][7][0]=-0.000454594; p3_tof_s_dp[1][7][0]=0.000229253; p4_tof_s_dp[1][7][0]=-2.42079e-05; 
  p0_tof_m_dp[0][1][8][0]=-0.00418902; p1_tof_m_dp[0][1][8][0]=0.0108243; p2_tof_m_dp[0][1][8][0]=-0.00911333; p3_tof_m_dp[0][1][8][0]=0.00300929; p4_tof_m_dp[0][1][8][0]=-0.00033555; 
  p0_tof_m_dp[1][1][8][0]=0.0038612; p1_tof_m_dp[1][1][8][0]=-0.0148955; p2_tof_m_dp[1][1][8][0]=0.00282545; p3_tof_m_dp[1][1][8][0]=0.0403283; p4_tof_m_dp[1][1][8][0]=-0.0366029; 
  p0_tof_s_dp[1][8][0]=0.00219499; p1_tof_s_dp[1][8][0]=0.0018272; p2_tof_s_dp[1][8][0]=-0.000949295; p3_tof_s_dp[1][8][0]=0.000371057; p4_tof_s_dp[1][8][0]=-4.1612e-05; 
  p0_tof_m_dp[0][1][9][0]=-0.00261407; p1_tof_m_dp[0][1][9][0]=0.00644817; p2_tof_m_dp[0][1][9][0]=-0.00564043; p3_tof_m_dp[0][1][9][0]=0.0018524; p4_tof_m_dp[0][1][9][0]=-0.000209227; 
  p0_tof_m_dp[1][1][9][0]=0.000181817; p1_tof_m_dp[1][1][9][0]=-0.000457307; p2_tof_m_dp[1][1][9][0]=-0.000903015; p3_tof_m_dp[1][1][9][0]=-0.000441223; p4_tof_m_dp[1][1][9][0]=0.00228217; 
  p0_tof_s_dp[1][9][0]=0.00270996; p1_tof_s_dp[1][9][0]=0.000198316; p2_tof_s_dp[1][9][0]=0.000324542; p3_tof_s_dp[1][9][0]=-4.14555e-05; p4_tof_s_dp[1][9][0]=7.46899e-06;
  
  //! ToF dphi pos
  p0_tof_m_dp[0][0][0][1]=0.00103178; p1_tof_m_dp[0][0][0][1]=-0.00211403; p2_tof_m_dp[0][0][0][1]=0.00110236; p3_tof_m_dp[0][0][0][1]=-0.000321098; p4_tof_m_dp[0][0][0][1]=4.30478e-05; 
  p0_tof_m_dp[1][0][0][1]=-0.00270418; p1_tof_m_dp[1][0][0][1]=0.0110115; p2_tof_m_dp[1][0][0][1]=-0.00247688; p3_tof_m_dp[1][0][0][1]=-0.030343; p4_tof_m_dp[1][0][0][1]=0.0276839; 
  p0_tof_s_dp[0][0][1]=0.0054612; p1_tof_s_dp[0][0][1]=-0.00108685; p2_tof_s_dp[0][0][1]=0.00117495; p3_tof_s_dp[0][0][1]=-0.000257197; p4_tof_s_dp[0][0][1]=2.25276e-05; 
  p0_tof_m_dp[0][0][1][1]=0.001073; p1_tof_m_dp[0][0][1][1]=-0.0022174; p2_tof_m_dp[0][0][1][1]=0.000943183; p3_tof_m_dp[0][0][1][1]=-0.000254447; p4_tof_m_dp[0][0][1][1]=2.85851e-05; 
  p0_tof_m_dp[1][0][1][1]=-0.00091986; p1_tof_m_dp[1][0][1][1]=0.0035638; p2_tof_m_dp[1][0][1][1]=-0.00083814; p3_tof_m_dp[1][0][1][1]=-0.00798276; p4_tof_m_dp[1][0][1][1]=0.00610523; 
  p0_tof_s_dp[0][1][1]=0.00524533; p1_tof_s_dp[0][1][1]=-0.000758284; p2_tof_s_dp[0][1][1]=0.000982443; p3_tof_s_dp[0][1][1]=-0.000195329; p4_tof_s_dp[0][1][1]=1.53523e-05; 
  p0_tof_m_dp[0][0][2][1]=0.000728773; p1_tof_m_dp[0][0][2][1]=-0.00164691; p2_tof_m_dp[0][0][2][1]=0.000460855; p3_tof_m_dp[0][0][2][1]=-0.000126532; p4_tof_m_dp[0][0][2][1]=1.42454e-05; 
  p0_tof_m_dp[1][0][2][1]=-0.00050246; p1_tof_m_dp[1][0][2][1]=0.001711; p2_tof_m_dp[1][0][2][1]=1.80675e-05; p3_tof_m_dp[1][0][2][1]=-0.00506055; p4_tof_m_dp[1][0][2][1]=0.00323198; 
  p0_tof_s_dp[0][2][1]=0.00529268; p1_tof_s_dp[0][2][1]=-0.000630961; p2_tof_s_dp[0][2][1]=0.00092147; p3_tof_s_dp[0][2][1]=-0.000188906; p4_tof_s_dp[0][2][1]=1.71697e-05; 
  p0_tof_m_dp[0][0][3][1]=0.000283799; p1_tof_m_dp[0][0][3][1]=9.22007e-05; p2_tof_m_dp[0][0][3][1]=-0.00129258; p3_tof_m_dp[0][0][3][1]=0.000555509; p4_tof_m_dp[0][0][3][1]=-7.33188e-05; 
  p0_tof_m_dp[1][0][3][1]=-0.00244827; p1_tof_m_dp[1][0][3][1]=0.010467; p2_tof_m_dp[1][0][3][1]=-0.00299149; p3_tof_m_dp[1][0][3][1]=-0.0297746; p4_tof_m_dp[1][0][3][1]=0.0279434; 
  p0_tof_s_dp[0][3][1]=0.00664466; p1_tof_s_dp[0][3][1]=-0.00343565; p2_tof_s_dp[0][3][1]=0.00368506; p3_tof_s_dp[0][3][1]=-0.00125162; p4_tof_s_dp[0][3][1]=0.000149519; 
  p0_tof_m_dp[0][0][4][1]=0.000586002; p1_tof_m_dp[0][0][4][1]=-0.000343582; p2_tof_m_dp[0][0][4][1]=-0.000547509; p3_tof_m_dp[0][0][4][1]=0.000154773; p4_tof_m_dp[0][0][4][1]=-1.31485e-05; 
  p0_tof_m_dp[1][0][4][1]=-0.000880308; p1_tof_m_dp[1][0][4][1]=0.0052623; p2_tof_m_dp[1][0][4][1]=-0.00246129; p3_tof_m_dp[1][0][4][1]=-0.014465; p4_tof_m_dp[1][0][4][1]=0.0145868; 
  p0_tof_s_dp[0][4][1]=0.00606877; p1_tof_s_dp[0][4][1]=-0.000878285; p2_tof_s_dp[0][4][1]=0.000803997; p3_tof_s_dp[0][4][1]=-7.82495e-05; p4_tof_s_dp[0][4][1]=-1.18346e-07; 
  p0_tof_m_dp[0][0][5][1]=0.000404515; p1_tof_m_dp[0][0][5][1]=-0.000524862; p2_tof_m_dp[0][0][5][1]=0.000193675; p3_tof_m_dp[0][0][5][1]=-0.000157244; p4_tof_m_dp[0][0][5][1]=2.25713e-05; 
  p0_tof_m_dp[1][0][5][1]=-0.000356928; p1_tof_m_dp[1][0][5][1]=0.000629028; p2_tof_m_dp[1][0][5][1]=0.00157182; p3_tof_m_dp[1][0][5][1]=0.000893427; p4_tof_m_dp[1][0][5][1]=-0.0046365; 
  p0_tof_s_dp[0][5][1]=0.00492365; p1_tof_s_dp[0][5][1]=0.00127811; p2_tof_s_dp[0][5][1]=-0.00108927; p3_tof_s_dp[0][5][1]=0.000549586; p4_tof_s_dp[0][5][1]=-6.89873e-05; 
  p0_tof_m_dp[0][0][6][1]=0.000871182; p1_tof_m_dp[0][0][6][1]=-0.0022493; p2_tof_m_dp[0][0][6][1]=0.00177364; p3_tof_m_dp[0][0][6][1]=-0.000666217; p4_tof_m_dp[0][0][6][1]=7.61296e-05; 
  p0_tof_m_dp[1][0][6][1]=-0.00460545; p1_tof_m_dp[1][0][6][1]=0.0170768; p2_tof_m_dp[1][0][6][1]=-0.00316997; p3_tof_m_dp[1][0][6][1]=-0.0473308; p4_tof_m_dp[1][0][6][1]=0.0439625; 
  p0_tof_s_dp[0][6][1]=0.00456309; p1_tof_s_dp[0][6][1]=0.00174334; p2_tof_s_dp[0][6][1]=-0.00141507; p3_tof_s_dp[0][6][1]=0.000621353; p4_tof_s_dp[0][6][1]=-7.36658e-05; 
  p0_tof_m_dp[0][0][7][1]=0.00103387; p1_tof_m_dp[0][0][7][1]=-0.00338888; p2_tof_m_dp[0][0][7][1]=0.00300813; p3_tof_m_dp[0][0][7][1]=-0.00110505; p4_tof_m_dp[0][0][7][1]=0.000128304; 
  p0_tof_m_dp[1][0][7][1]=-0.0069949; p1_tof_m_dp[1][0][7][1]=0.0253776; p2_tof_m_dp[1][0][7][1]=-0.00559083; p3_tof_m_dp[1][0][7][1]=-0.0678542; p4_tof_m_dp[1][0][7][1]=0.0635143; 
  p0_tof_s_dp[0][7][1]=0.00618313; p1_tof_s_dp[0][7][1]=-0.00347797; p2_tof_s_dp[0][7][1]=0.00436519; p3_tof_s_dp[0][7][1]=-0.00193522; p4_tof_s_dp[0][7][1]=0.000307424; 
  p0_tof_m_dp[0][0][8][1]=0.000777717; p1_tof_m_dp[0][0][8][1]=-0.00313482; p2_tof_m_dp[0][0][8][1]=0.0029961; p3_tof_m_dp[0][0][8][1]=-0.00108757; p4_tof_m_dp[0][0][8][1]=0.000128296; 
  p0_tof_m_dp[1][0][8][1]=-0.00297424; p1_tof_m_dp[1][0][8][1]=0.00877822; p2_tof_m_dp[1][0][8][1]=-0.000397551; p3_tof_m_dp[1][0][8][1]=-0.0224774; p4_tof_m_dp[1][0][8][1]=0.0185858; 
  p0_tof_s_dp[0][8][1]=0.00454479; p1_tof_s_dp[0][8][1]=0.00169919; p2_tof_s_dp[0][8][1]=-0.00115818; p3_tof_s_dp[0][8][1]=0.000443947; p4_tof_s_dp[0][8][1]=-5.04388e-05; 
  p0_tof_m_dp[0][0][9][1]=-0.000712219; p1_tof_m_dp[0][0][9][1]=0.000160501; p2_tof_m_dp[0][0][9][1]=0.000618067; p3_tof_m_dp[0][0][9][1]=-0.000371277; p4_tof_m_dp[0][0][9][1]=6.22339e-05; 
  p0_tof_m_dp[1][0][9][1]=-0.00265411; p1_tof_m_dp[1][0][9][1]=0.00687929; p2_tof_m_dp[1][0][9][1]=-4.08406e-05; p3_tof_m_dp[1][0][9][1]=-0.0142415; p4_tof_m_dp[1][0][9][1]=0.0101327; 
  p0_tof_s_dp[0][9][1]=0.00524696; p1_tof_s_dp[0][9][1]=-0.000281532; p2_tof_s_dp[0][9][1]=0.000217504; p3_tof_s_dp[0][9][1]=5.25184e-05; p4_tof_s_dp[0][9][1]=-8.52418e-06; 
  p0_tof_m_dp[0][1][0][1]=-0.00272684; p1_tof_m_dp[0][1][0][1]=0.00653818; p2_tof_m_dp[0][1][0][1]=-0.00557914; p3_tof_m_dp[0][1][0][1]=0.00188378; p4_tof_m_dp[0][1][0][1]=-0.000201655; 
  p0_tof_m_dp[1][1][0][1]=-0.00122827; p1_tof_m_dp[1][1][0][1]=0.00360451; p2_tof_m_dp[1][1][0][1]=-0.00150368; p3_tof_m_dp[1][1][0][1]=-0.0100525; p4_tof_m_dp[1][1][0][1]=0.0110537; 
  p0_tof_s_dp[1][0][1]=0.00323451; p1_tof_s_dp[1][0][1]=-0.00104991; p2_tof_s_dp[1][0][1]=0.00164505; p3_tof_s_dp[1][0][1]=-0.000451388; p4_tof_s_dp[1][0][1]=4.47681e-05; 
  p0_tof_m_dp[0][1][1][1]=-0.0022945; p1_tof_m_dp[0][1][1][1]=0.00591385; p2_tof_m_dp[0][1][1][1]=-0.00500281; p3_tof_m_dp[0][1][1][1]=0.00157412; p4_tof_m_dp[0][1][1][1]=-0.000161218; 
  p0_tof_m_dp[1][1][1][1]=-0.00101486; p1_tof_m_dp[1][1][1][1]=0.00266871; p2_tof_m_dp[1][1][1][1]=-0.000309434; p3_tof_m_dp[1][1][1][1]=-0.00524392; p4_tof_m_dp[1][1][1][1]=0.00426214; 
  p0_tof_s_dp[1][1][1]=0.00309992; p1_tof_s_dp[1][1][1]=-0.000569098; p2_tof_s_dp[1][1][1]=0.00100428; p3_tof_s_dp[1][1][1]=-0.000166803; p4_tof_s_dp[1][1][1]=6.31103e-06; 
  p0_tof_m_dp[0][1][2][1]=-0.00131663; p1_tof_m_dp[0][1][2][1]=0.00397013; p2_tof_m_dp[0][1][2][1]=-0.00383598; p3_tof_m_dp[0][1][2][1]=0.00124254; p4_tof_m_dp[0][1][2][1]=-0.000130929; 
  p0_tof_m_dp[1][1][2][1]=-0.0048837; p1_tof_m_dp[1][1][2][1]=0.0176486; p2_tof_m_dp[1][1][2][1]=-0.00407428; p3_tof_m_dp[1][1][2][1]=-0.0444307; p4_tof_m_dp[1][1][2][1]=0.0410623; 
  p0_tof_s_dp[1][2][1]=0.00285131; p1_tof_s_dp[1][2][1]=0.000449524; p2_tof_s_dp[1][2][1]=0.000167359; p3_tof_s_dp[1][2][1]=9.00963e-05; p4_tof_s_dp[1][2][1]=-1.97257e-05; 
  p0_tof_m_dp[0][1][3][1]=-0.00218153; p1_tof_m_dp[0][1][3][1]=0.00490185; p2_tof_m_dp[0][1][3][1]=-0.0045201; p3_tof_m_dp[0][1][3][1]=0.00139867; p4_tof_m_dp[0][1][3][1]=-0.000142544; 
  p0_tof_m_dp[1][1][3][1]=-0.00552016; p1_tof_m_dp[1][1][3][1]=0.0176197; p2_tof_m_dp[1][1][3][1]=-0.00253246; p3_tof_m_dp[1][1][3][1]=-0.0454365; p4_tof_m_dp[1][1][3][1]=0.0398826; 
  p0_tof_s_dp[1][3][1]=0.00247646; p1_tof_s_dp[1][3][1]=0.00134296; p2_tof_s_dp[1][3][1]=-0.000770325; p3_tof_s_dp[1][3][1]=0.000474625; p4_tof_s_dp[1][3][1]=-7.02513e-05; 
  p0_tof_m_dp[0][1][4][1]=-0.00304099; p1_tof_m_dp[0][1][4][1]=0.0058479; p2_tof_m_dp[0][1][4][1]=-0.00511059; p3_tof_m_dp[0][1][4][1]=0.00155618; p4_tof_m_dp[0][1][4][1]=-0.000158573; 
  p0_tof_m_dp[1][1][4][1]=-0.00551601; p1_tof_m_dp[1][1][4][1]=0.0159133; p2_tof_m_dp[1][1][4][1]=-0.00221861; p3_tof_m_dp[1][1][4][1]=-0.0400859; p4_tof_m_dp[1][1][4][1]=0.0347108; 
  p0_tof_s_dp[1][4][1]=0.0028593; p1_tof_s_dp[1][4][1]=0.000275475; p2_tof_s_dp[1][4][1]=0.000259595; p3_tof_s_dp[1][4][1]=0.000100716; p4_tof_s_dp[1][4][1]=-2.25125e-05; 
  p0_tof_m_dp[0][1][5][1]=0.00107853; p1_tof_m_dp[0][1][5][1]=-0.00393191; p2_tof_m_dp[0][1][5][1]=0.00301747; p3_tof_m_dp[0][1][5][1]=-0.00107712; p4_tof_m_dp[0][1][5][1]=0.000129077; 
  p0_tof_m_dp[1][1][5][1]=-0.00296155; p1_tof_m_dp[1][1][5][1]=0.00750411; p2_tof_m_dp[1][1][5][1]=-0.00027986; p3_tof_m_dp[1][1][5][1]=-0.0170286; p4_tof_m_dp[1][1][5][1]=0.0129259; 
  p0_tof_s_dp[1][5][1]=0.00313055; p1_tof_s_dp[1][5][1]=-9.76461e-05; p2_tof_s_dp[1][5][1]=0.000529801; p3_tof_s_dp[1][5][1]=5.04699e-06; p4_tof_s_dp[1][5][1]=-1.26596e-05; 
  p0_tof_m_dp[0][1][6][1]=0.00140562; p1_tof_m_dp[0][1][6][1]=-0.00581785; p2_tof_m_dp[0][1][6][1]=0.00475193; p3_tof_m_dp[0][1][6][1]=-0.00163403; p4_tof_m_dp[0][1][6][1]=0.000188697; 
  p0_tof_m_dp[1][1][6][1]=-0.00250147; p1_tof_m_dp[1][1][6][1]=0.00506201; p2_tof_m_dp[1][1][6][1]=-0.000290616; p3_tof_m_dp[1][1][6][1]=-0.0112035; p4_tof_m_dp[1][1][6][1]=0.00843788; 
  p0_tof_s_dp[1][6][1]=0.00299331; p1_tof_s_dp[1][6][1]=-0.000103036; p2_tof_s_dp[1][6][1]=0.000473806; p3_tof_s_dp[1][6][1]=4.22252e-05; p4_tof_s_dp[1][6][1]=-1.97572e-05; 
  p0_tof_m_dp[0][1][7][1]=0.000556205; p1_tof_m_dp[0][1][7][1]=-0.0031576; p2_tof_m_dp[0][1][7][1]=0.00262909; p3_tof_m_dp[0][1][7][1]=-0.000958965; p4_tof_m_dp[0][1][7][1]=0.000116706; 
  p0_tof_m_dp[1][1][7][1]=-0.00354652; p1_tof_m_dp[1][1][7][1]=0.00920635; p2_tof_m_dp[1][1][7][1]=-0.000277575; p3_tof_m_dp[1][1][7][1]=-0.0224691; p4_tof_m_dp[1][1][7][1]=0.0177194; 
  p0_tof_s_dp[1][7][1]=0.00281337; p1_tof_s_dp[1][7][1]=0.000368833; p2_tof_s_dp[1][7][1]=-3.26396e-06; p3_tof_s_dp[1][7][1]=0.000200865; p4_tof_s_dp[1][7][1]=-3.61055e-05; 
  p0_tof_m_dp[0][1][8][1]=0.00051251; p1_tof_m_dp[0][1][8][1]=-0.00304452; p2_tof_m_dp[0][1][8][1]=0.0027349; p3_tof_m_dp[0][1][8][1]=-0.00100376; p4_tof_m_dp[0][1][8][1]=0.0001249; 
  p0_tof_m_dp[1][1][8][1]=-0.00267305; p1_tof_m_dp[1][1][8][1]=0.00673361; p2_tof_m_dp[1][1][8][1]=1.71421e-05; p3_tof_m_dp[1][1][8][1]=-0.0170568; p4_tof_m_dp[1][1][8][1]=0.013487; 
  p0_tof_s_dp[1][8][1]=0.00315144; p1_tof_s_dp[1][8][1]=-0.00013155; p2_tof_s_dp[1][8][1]=0.000417627; p3_tof_s_dp[1][8][1]=2.41963e-05; p4_tof_s_dp[1][8][1]=-1.21602e-05; 
  p0_tof_m_dp[0][1][9][1]=0.00025619; p1_tof_m_dp[0][1][9][1]=-0.00132711; p2_tof_m_dp[0][1][9][1]=0.00108914; p3_tof_m_dp[0][1][9][1]=-0.000402344; p4_tof_m_dp[0][1][9][1]=6.15599e-05; 
  p0_tof_m_dp[1][1][9][1]=0.000129595; p1_tof_m_dp[1][1][9][1]=-0.00409656; p2_tof_m_dp[1][1][9][1]=0.00312828; p3_tof_m_dp[1][1][9][1]=0.0137225; p4_tof_m_dp[1][1][9][1]=-0.0159905; 
  p0_tof_s_dp[1][9][1]=0.00265945; p1_tof_s_dp[1][9][1]=0.000882743; p2_tof_s_dp[1][9][1]=-0.000341268; p3_tof_s_dp[1][9][1]=0.000242368; p4_tof_s_dp[1][9][1]=-3.37112e-05;
  
  //! ToF dz neg
  p0_tof_m_dz[0][0][0][0]=-1.04156; p1_tof_m_dz[0][0][0][0]=0.553765; p2_tof_m_dz[0][0][0][0]=-0.530837; p3_tof_m_dz[0][0][0][0]=0.199189; p4_tof_m_dz[0][0][0][0]=-0.0262084; 
  p0_tof_m_dz[1][0][0][0]=-0.721044; p1_tof_m_dz[1][0][0][0]=-1.44016; p2_tof_m_dz[1][0][0][0]=0.545017; p3_tof_m_dz[1][0][0][0]=3.481; p4_tof_m_dz[1][0][0][0]=-2.08959; 
  p0_tof_s_dz[0][0][0]=1.38757; p1_tof_s_dz[0][0][0]=-0.337929; p2_tof_s_dz[0][0][0]=0.638186; p3_tof_s_dz[0][0][0]=-0.149396; p4_tof_s_dz[0][0][0]=0.0168536; 
  p0_tof_m_dz[0][0][1][0]=-0.892625; p1_tof_m_dz[0][0][1][0]=-0.162985; p2_tof_m_dz[0][0][1][0]=0.0750208; p3_tof_m_dz[0][0][1][0]=0.0106867; p4_tof_m_dz[0][0][1][0]=-0.00409199; 
  p0_tof_m_dz[1][0][1][0]=-0.326413; p1_tof_m_dz[1][0][1][0]=-3.26487; p2_tof_m_dz[1][0][1][0]=1.31089; p3_tof_m_dz[1][0][1][0]=8.6698; p4_tof_m_dz[1][0][1][0]=-8.22313; 
  p0_tof_s_dz[0][1][0]=1.37543; p1_tof_s_dz[0][1][0]=-0.234168; p2_tof_s_dz[0][1][0]=0.511581; p3_tof_s_dz[0][1][0]=-0.106883; p4_tof_s_dz[0][1][0]=0.0120766; 
  p0_tof_m_dz[0][0][2][0]=-0.869949; p1_tof_m_dz[0][0][2][0]=-0.20756; p2_tof_m_dz[0][0][2][0]=0.113067; p3_tof_m_dz[0][0][2][0]=-0.00535575; p4_tof_m_dz[0][0][2][0]=-0.00121635; 
  p0_tof_m_dz[1][0][2][0]=-0.48523; p1_tof_m_dz[1][0][2][0]=-2.30992; p2_tof_m_dz[1][0][2][0]=0.953373; p3_tof_m_dz[1][0][2][0]=5.94456; p4_tof_m_dz[1][0][2][0]=-5.66536; 
  p0_tof_s_dz[0][2][0]=1.14454; p1_tof_s_dz[0][2][0]=0.0591548; p2_tof_s_dz[0][2][0]=0.289412; p3_tof_s_dz[0][2][0]=-0.0394969; p4_tof_s_dz[0][2][0]=0.00505854; 
  p0_tof_m_dz[0][0][3][0]=-1.21043; p1_tof_m_dz[0][0][3][0]=0.11565; p2_tof_m_dz[0][0][3][0]=-0.111564; p3_tof_m_dz[0][0][3][0]=0.0519682; p4_tof_m_dz[0][0][3][0]=-0.00639439; 
  p0_tof_m_dz[1][0][3][0]=-0.764403; p1_tof_m_dz[1][0][3][0]=-1.90614; p2_tof_m_dz[1][0][3][0]=0.860616; p3_tof_m_dz[1][0][3][0]=4.89932; p4_tof_m_dz[1][0][3][0]=-4.91713; 
  p0_tof_s_dz[0][3][0]=1.08313; p1_tof_s_dz[0][3][0]=0.080217; p2_tof_s_dz[0][3][0]=0.264969; p3_tof_s_dz[0][3][0]=-0.0271424; p4_tof_s_dz[0][3][0]=0.00319471; 
  p0_tof_m_dz[0][0][4][0]=-1.55037; p1_tof_m_dz[0][0][4][0]=0.217027; p2_tof_m_dz[0][0][4][0]=-0.115954; p3_tof_m_dz[0][0][4][0]=0.035851; p4_tof_m_dz[0][0][4][0]=-0.00310073; 
  p0_tof_m_dz[1][0][4][0]=-1.67247; p1_tof_m_dz[1][0][4][0]=0.892913; p2_tof_m_dz[1][0][4][0]=-0.286134; p3_tof_m_dz[1][0][4][0]=-2.95098; p4_tof_m_dz[1][0][4][0]=3.25976; 
  p0_tof_s_dz[0][4][0]=1.16384; p1_tof_s_dz[0][4][0]=-0.0260161; p2_tof_s_dz[0][4][0]=0.326984; p3_tof_s_dz[0][4][0]=-0.0431733; p4_tof_s_dz[0][4][0]=0.00452765; 
  p0_tof_m_dz[0][0][5][0]=-1.48644; p1_tof_m_dz[0][0][5][0]=0.26966; p2_tof_m_dz[0][0][5][0]=-0.191677; p3_tof_m_dz[0][0][5][0]=0.0569658; p4_tof_m_dz[0][0][5][0]=-0.00601825; 
  p0_tof_m_dz[1][0][5][0]=-1.12746; p1_tof_m_dz[1][0][5][0]=-1.09303; p2_tof_m_dz[1][0][5][0]=0.579588; p3_tof_m_dz[1][0][5][0]=2.35006; p4_tof_m_dz[1][0][5][0]=-2.4007; 
  p0_tof_s_dz[0][5][0]=1.1695; p1_tof_s_dz[0][5][0]=-0.0548891; p2_tof_s_dz[0][5][0]=0.353126; p3_tof_s_dz[0][5][0]=-0.0529036; p4_tof_s_dz[0][5][0]=0.00540011; 
  p0_tof_m_dz[0][0][6][0]=-1.55105; p1_tof_m_dz[0][0][6][0]=0.042103; p2_tof_m_dz[0][0][6][0]=-0.0231544; p3_tof_m_dz[0][0][6][0]=-0.000892646; p4_tof_m_dz[0][0][6][0]=0.000526923; 
  p0_tof_m_dz[1][0][6][0]=-1.63972; p1_tof_m_dz[1][0][6][0]=0.464493; p2_tof_m_dz[1][0][6][0]=0.104143; p3_tof_m_dz[1][0][6][0]=-1.91089; p4_tof_m_dz[1][0][6][0]=1.57454; 
  p0_tof_s_dz[0][6][0]=1.19311; p1_tof_s_dz[0][6][0]=-0.0602989; p2_tof_s_dz[0][6][0]=0.349006; p3_tof_s_dz[0][6][0]=-0.0462075; p4_tof_s_dz[0][6][0]=0.00447617; 
  p0_tof_m_dz[0][0][7][0]=-1.5442; p1_tof_m_dz[0][0][7][0]=-0.00517253; p2_tof_m_dz[0][0][7][0]=-0.0330026; p3_tof_m_dz[0][0][7][0]=-0.0106284; p4_tof_m_dz[0][0][7][0]=0.00255804; 
  p0_tof_m_dz[1][0][7][0]=-1.41216; p1_tof_m_dz[1][0][7][0]=-0.488457; p2_tof_m_dz[1][0][7][0]=0.63392; p3_tof_m_dz[1][0][7][0]=0.888085; p4_tof_m_dz[1][0][7][0]=-2.01177; 
  p0_tof_s_dz[0][7][0]=1.07658; p1_tof_s_dz[0][7][0]=0.273162; p2_tof_s_dz[0][7][0]=0.0121402; p3_tof_s_dz[0][7][0]=0.0737326; p4_tof_s_dz[0][7][0]=-0.00906946; 
  p0_tof_m_dz[0][0][8][0]=-1.95847; p1_tof_m_dz[0][0][8][0]=0.514932; p2_tof_m_dz[0][0][8][0]=-0.419789; p3_tof_m_dz[0][0][8][0]=0.0902829; p4_tof_m_dz[0][0][8][0]=-0.00615146; 
  p0_tof_m_dz[1][0][8][0]=-2.03161; p1_tof_m_dz[1][0][8][0]=1.54047; p2_tof_m_dz[1][0][8][0]=-0.128045; p3_tof_m_dz[1][0][8][0]=-5.40146; p4_tof_m_dz[1][0][8][0]=4.43612; 
  p0_tof_s_dz[0][8][0]=1.04107; p1_tof_s_dz[0][8][0]=0.324998; p2_tof_s_dz[0][8][0]=-0.00432782; p3_tof_s_dz[0][8][0]=0.0733848; p4_tof_s_dz[0][8][0]=-0.00839907; 
  p0_tof_m_dz[0][0][9][0]=-2.16183; p1_tof_m_dz[0][0][9][0]=1.14201; p2_tof_m_dz[0][0][9][0]=-0.942226; p3_tof_m_dz[0][0][9][0]=0.236466; p4_tof_m_dz[0][0][9][0]=-0.0192501; 
  p0_tof_m_dz[1][0][9][0]=-1.34855; p1_tof_m_dz[1][0][9][0]=-0.616626; p2_tof_m_dz[1][0][9][0]=0.327231; p3_tof_m_dz[1][0][9][0]=0.614109; p4_tof_m_dz[1][0][9][0]=-1.64672; 
  p0_tof_s_dz[0][9][0]=1.2102; p1_tof_s_dz[0][9][0]=-0.0590223; p2_tof_s_dz[0][9][0]=0.30936; p3_tof_s_dz[0][9][0]=-0.0178553; p4_tof_s_dz[0][9][0]=0.000724185; 
  p0_tof_m_dz[0][1][0][0]=0.641065; p1_tof_m_dz[0][1][0][0]=-1.10023; p2_tof_m_dz[0][1][0][0]=1.08633; p3_tof_m_dz[0][1][0][0]=-0.377305; p4_tof_m_dz[0][1][0][0]=0.0437508; 
  p0_tof_m_dz[1][1][0][0]=1.11434; p1_tof_m_dz[1][1][0][0]=-4.73944; p2_tof_m_dz[1][1][0][0]=1.74278; p3_tof_m_dz[1][1][0][0]=14.0584; p4_tof_m_dz[1][1][0][0]=-13.2449; 
  p0_tof_s_dz[1][0][0]=1.31425; p1_tof_s_dz[1][0][0]=0.00632453; p2_tof_s_dz[1][0][0]=0.201073; p3_tof_s_dz[1][0][0]=0.00898518; p4_tof_s_dz[1][0][0]=-0.00150481; 
  p0_tof_m_dz[0][1][1][0]=0.485088; p1_tof_m_dz[0][1][1][0]=-1.19254; p2_tof_m_dz[0][1][1][0]=1.12663; p3_tof_m_dz[0][1][1][0]=-0.371277; p4_tof_m_dz[0][1][1][0]=0.042325; 
  p0_tof_m_dz[1][1][1][0]=-0.0124135; p1_tof_m_dz[1][1][1][0]=-0.706575; p2_tof_m_dz[1][1][1][0]=0.669038; p3_tof_m_dz[1][1][1][0]=2.74987; p4_tof_m_dz[1][1][1][0]=-2.79495; 
  p0_tof_s_dz[1][1][0]=1.41439; p1_tof_s_dz[1][1][0]=-0.184243; p2_tof_s_dz[1][1][0]=0.352077; p3_tof_s_dz[1][1][0]=-0.0414907; p4_tof_s_dz[1][1][0]=0.00426184; 
  p0_tof_m_dz[0][1][2][0]=0.180437; p1_tof_m_dz[0][1][2][0]=-0.837126; p2_tof_m_dz[0][1][2][0]=0.780727; p3_tof_m_dz[0][1][2][0]=-0.25548; p4_tof_m_dz[0][1][2][0]=0.0296799; 
  p0_tof_m_dz[1][1][2][0]=0.298262; p1_tof_m_dz[1][1][2][0]=-2.20704; p2_tof_m_dz[1][1][2][0]=0.895396; p3_tof_m_dz[1][1][2][0]=6.43604; p4_tof_m_dz[1][1][2][0]=-6.25074; 
  p0_tof_s_dz[1][2][0]=1.44433; p1_tof_s_dz[1][2][0]=-0.247434; p2_tof_s_dz[1][2][0]=0.39363; p3_tof_s_dz[1][2][0]=-0.0581674; p4_tof_s_dz[1][2][0]=0.00627777; 
  p0_tof_m_dz[0][1][3][0]=-0.120366; p1_tof_m_dz[0][1][3][0]=-0.318787; p2_tof_m_dz[0][1][3][0]=0.327716; p3_tof_m_dz[0][1][3][0]=-0.10498; p4_tof_m_dz[0][1][3][0]=0.0123057; 
  p0_tof_m_dz[1][1][3][0]=-0.947773; p1_tof_m_dz[1][1][3][0]=2.66399; p2_tof_m_dz[1][1][3][0]=-0.894111; p3_tof_m_dz[1][1][3][0]=-7.11348; p4_tof_m_dz[1][1][3][0]=7.49967; 
  p0_tof_s_dz[1][3][0]=1.19306; p1_tof_s_dz[1][3][0]=0.326156; p2_tof_s_dz[1][3][0]=-0.103774; p3_tof_s_dz[1][3][0]=0.117943; p4_tof_s_dz[1][3][0]=-0.0139311; 
  p0_tof_m_dz[0][1][4][0]=-0.191914; p1_tof_m_dz[0][1][4][0]=-0.563361; p2_tof_m_dz[0][1][4][0]=0.462664; p3_tof_m_dz[0][1][4][0]=-0.145457; p4_tof_m_dz[0][1][4][0]=0.0168683; 
  p0_tof_m_dz[1][1][4][0]=0.26427; p1_tof_m_dz[1][1][4][0]=-2.71257; p2_tof_m_dz[1][1][4][0]=0.889749; p3_tof_m_dz[1][1][4][0]=7.07586; p4_tof_m_dz[1][1][4][0]=-6.88041; 
  p0_tof_s_dz[1][4][0]=1.08899; p1_tof_s_dz[1][4][0]=0.763976; p2_tof_s_dz[1][4][0]=-0.578241; p3_tof_s_dz[1][4][0]=0.311238; p4_tof_s_dz[1][4][0]=-0.0387454; 
  p0_tof_m_dz[0][1][5][0]=-0.453559; p1_tof_m_dz[0][1][5][0]=0.271378; p2_tof_m_dz[0][1][5][0]=-0.417599; p3_tof_m_dz[0][1][5][0]=0.187464; p4_tof_m_dz[0][1][5][0]=-0.024344; 
  p0_tof_m_dz[1][1][5][0]=-2.1387; p1_tof_m_dz[1][1][5][0]=7.12519; p2_tof_m_dz[1][1][5][0]=-2.2031; p3_tof_m_dz[1][1][5][0]=-19.9329; p4_tof_m_dz[1][1][5][0]=19.5984; 
  p0_tof_s_dz[1][5][0]=1.21145; p1_tof_s_dz[1][5][0]=0.34579; p2_tof_s_dz[1][5][0]=-0.132564; p3_tof_s_dz[1][5][0]=0.137479; p4_tof_s_dz[1][5][0]=-0.0182463; 
  p0_tof_m_dz[0][1][6][0]=-0.431247; p1_tof_m_dz[0][1][6][0]=-0.284676; p2_tof_m_dz[0][1][6][0]=0.0246062; p3_tof_m_dz[0][1][6][0]=0.0292023; p4_tof_m_dz[0][1][6][0]=-0.00567298; 
  p0_tof_m_dz[1][1][6][0]=-0.662702; p1_tof_m_dz[1][1][6][0]=0.738105; p2_tof_m_dz[1][1][6][0]=-0.298254; p3_tof_m_dz[1][1][6][0]=-2.63516; p4_tof_m_dz[1][1][6][0]=2.38839; 
  p0_tof_s_dz[1][6][0]=1.2317; p1_tof_s_dz[1][6][0]=0.260538; p2_tof_s_dz[1][6][0]=-0.0460141; p3_tof_s_dz[1][6][0]=0.102565; p4_tof_s_dz[1][6][0]=-0.0131102; 
  p0_tof_m_dz[0][1][7][0]=-0.622109; p1_tof_m_dz[0][1][7][0]=-0.245025; p2_tof_m_dz[0][1][7][0]=-0.0579408; p3_tof_m_dz[0][1][7][0]=0.0551242; p4_tof_m_dz[0][1][7][0]=-0.00912944; 
  p0_tof_m_dz[1][1][7][0]=-1.17542; p1_tof_m_dz[1][1][7][0]=2.20235; p2_tof_m_dz[1][1][7][0]=-0.814262; p3_tof_m_dz[1][1][7][0]=-6.82539; p4_tof_m_dz[1][1][7][0]=6.50312; 
  p0_tof_s_dz[1][7][0]=1.26736; p1_tof_s_dz[1][7][0]=0.198383; p2_tof_s_dz[1][7][0]=0.013871; p3_tof_s_dz[1][7][0]=0.0822945; p4_tof_s_dz[1][7][0]=-0.0111534; 
  p0_tof_m_dz[0][1][8][0]=-0.748192; p1_tof_m_dz[0][1][8][0]=-0.355412; p2_tof_m_dz[0][1][8][0]=-0.0204404; p3_tof_m_dz[0][1][8][0]=0.046821; p4_tof_m_dz[0][1][8][0]=-0.00778433; 
  p0_tof_m_dz[1][1][8][0]=-0.584141; p1_tof_m_dz[1][1][8][0]=-0.45641; p2_tof_m_dz[1][1][8][0]=-0.208655; p3_tof_m_dz[1][1][8][0]=0.0198774; p4_tof_m_dz[1][1][8][0]=-0.119935; 
  p0_tof_s_dz[1][8][0]=1.38643; p1_tof_s_dz[1][8][0]=-0.0180421; p2_tof_s_dz[1][8][0]=0.214738; p3_tof_s_dz[1][8][0]=0.00804728; p4_tof_s_dz[1][8][0]=-0.0018622; 
  p0_tof_m_dz[0][1][9][0]=-0.931421; p1_tof_m_dz[0][1][9][0]=-0.387096; p2_tof_m_dz[0][1][9][0]=0.0196312; p3_tof_m_dz[0][1][9][0]=0.0184367; p4_tof_m_dz[0][1][9][0]=-0.00220324; 
  p0_tof_m_dz[1][1][9][0]=0.644949; p1_tof_m_dz[1][1][9][0]=-5.58549; p2_tof_m_dz[1][1][9][0]=1.08327; p3_tof_m_dz[1][1][9][0]=13.8469; p4_tof_m_dz[1][1][9][0]=-13.7436; 
  p0_tof_s_dz[1][9][0]=1.28492; p1_tof_s_dz[1][9][0]=0.425003; p2_tof_s_dz[1][9][0]=-0.26151; p3_tof_s_dz[1][9][0]=0.23891; p4_tof_s_dz[1][9][0]=-0.0365976; 
  
  //! ToF dz pos
  p0_tof_m_dz[0][0][0][1]=-0.918108; p1_tof_m_dz[0][0][0][1]=0.402351; p2_tof_m_dz[0][0][0][1]=-0.14108; p3_tof_m_dz[0][0][0][1]=0.0740179; p4_tof_m_dz[0][0][0][1]=-0.0122245; 
  p0_tof_m_dz[1][0][0][1]=-1.09313; p1_tof_m_dz[1][0][0][1]=-0.200126; p2_tof_m_dz[1][0][0][1]=0.93153; p3_tof_m_dz[1][0][0][1]=1.30579; p4_tof_m_dz[1][0][0][1]=-1.24845; 
  p0_tof_s_dz[0][0][1]=1.21498; p1_tof_s_dz[0][0][1]=0.194874; p2_tof_s_dz[0][0][1]=0.281386; p3_tof_s_dz[0][0][1]=-0.0281673; p4_tof_s_dz[0][0][1]=0.00119037; 
  p0_tof_m_dz[0][0][1][1]=-1.05049; p1_tof_m_dz[0][0][1][1]=0.378615; p2_tof_m_dz[0][0][1][1]=-0.162817; p3_tof_m_dz[0][0][1][1]=0.0834137; p4_tof_m_dz[0][0][1][1]=-0.0113722; 
  p0_tof_m_dz[1][0][1][1]=-0.877154; p1_tof_m_dz[1][0][1][1]=-0.995017; p2_tof_m_dz[1][0][1][1]=0.853807; p3_tof_m_dz[1][0][1][1]=2.75999; p4_tof_m_dz[1][0][1][1]=-2.4853; 
  p0_tof_s_dz[0][1][1]=1.23699; p1_tof_s_dz[0][1][1]=0.125731; p2_tof_s_dz[0][1][1]=0.338064; p3_tof_s_dz[0][1][1]=-0.0517381; p4_tof_s_dz[0][1][1]=0.00433383; 
  p0_tof_m_dz[0][0][2][1]=-0.950757; p1_tof_m_dz[0][0][2][1]=0.0854694; p2_tof_m_dz[0][0][2][1]=0.0393391; p3_tof_m_dz[0][0][2][1]=0.0139681; p4_tof_m_dz[0][0][2][1]=-0.00326342; 
  p0_tof_m_dz[1][0][2][1]=-0.573241; p1_tof_m_dz[1][0][2][1]=-2.0041; p2_tof_m_dz[1][0][2][1]=1.0783; p3_tof_m_dz[1][0][2][1]=5.721; p4_tof_m_dz[1][0][2][1]=-5.73381; 
  p0_tof_s_dz[0][2][1]=1.06851; p1_tof_s_dz[0][2][1]=0.24369; p2_tof_s_dz[0][2][1]=0.255652; p3_tof_s_dz[0][2][1]=-0.0288873; p4_tof_s_dz[0][2][1]=0.00225511; 
  p0_tof_m_dz[0][0][3][1]=-1.14365; p1_tof_m_dz[0][0][3][1]=0.0453228; p2_tof_m_dz[0][0][3][1]=0.0638467; p3_tof_m_dz[0][0][3][1]=-0.00922119; p4_tof_m_dz[0][0][3][1]=0.000283418; 
  p0_tof_m_dz[1][0][3][1]=-0.705445; p1_tof_m_dz[1][0][3][1]=-2.02158; p2_tof_m_dz[1][0][3][1]=0.980137; p3_tof_m_dz[1][0][3][1]=5.3826; p4_tof_m_dz[1][0][3][1]=-5.39014; 
  p0_tof_s_dz[0][3][1]=1.05194; p1_tof_s_dz[0][3][1]=0.107232; p2_tof_s_dz[0][3][1]=0.362427; p3_tof_s_dz[0][3][1]=-0.0599874; p4_tof_s_dz[0][3][1]=0.00535553; 
  p0_tof_m_dz[0][0][4][1]=-1.49519; p1_tof_m_dz[0][0][4][1]=0.142272; p2_tof_m_dz[0][0][4][1]=-0.0742761; p3_tof_m_dz[0][0][4][1]=0.0329919; p4_tof_m_dz[0][0][4][1]=-0.00384289; 
  p0_tof_m_dz[1][0][4][1]=-1.26596; p1_tof_m_dz[1][0][4][1]=-0.802576; p2_tof_m_dz[1][0][4][1]=0.57796; p3_tof_m_dz[1][0][4][1]=1.6199; p4_tof_m_dz[1][0][4][1]=-1.79219; 
  p0_tof_s_dz[0][4][1]=1.07423; p1_tof_s_dz[0][4][1]=0.142007; p2_tof_s_dz[0][4][1]=0.314246; p3_tof_s_dz[0][4][1]=-0.0391289; p4_tof_s_dz[0][4][1]=0.00245631; 
  p0_tof_m_dz[0][0][5][1]=-1.54205; p1_tof_m_dz[0][0][5][1]=0.240729; p2_tof_m_dz[0][0][5][1]=-0.277665; p3_tof_m_dz[0][0][5][1]=0.09093; p4_tof_m_dz[0][0][5][1]=-0.00963115; 
  p0_tof_m_dz[1][0][5][1]=-1.95208; p1_tof_m_dz[1][0][5][1]=2.092; p2_tof_m_dz[1][0][5][1]=-0.400619; p3_tof_m_dz[1][0][5][1]=-6.72603; p4_tof_m_dz[1][0][5][1]=6.33087; 
  p0_tof_s_dz[0][5][1]=1.08142; p1_tof_s_dz[0][5][1]=0.15852; p2_tof_s_dz[0][5][1]=0.268691; p3_tof_s_dz[0][5][1]=-0.0155704; p4_tof_s_dz[0][5][1]=-0.00111987; 
  p0_tof_m_dz[0][0][6][1]=-1.63973; p1_tof_m_dz[0][0][6][1]=0.0276202; p2_tof_m_dz[0][0][6][1]=-0.181757; p3_tof_m_dz[0][0][6][1]=0.054159; p4_tof_m_dz[0][0][6][1]=-0.00528315; 
  p0_tof_m_dz[1][0][6][1]=-1.90642; p1_tof_m_dz[1][0][6][1]=1.54465; p2_tof_m_dz[1][0][6][1]=-0.554852; p3_tof_m_dz[1][0][6][1]=-5.94066; p4_tof_m_dz[1][0][6][1]=6.05785; 
  p0_tof_s_dz[0][6][1]=1.1019; p1_tof_s_dz[0][6][1]=0.136161; p2_tof_s_dz[0][6][1]=0.314289; p3_tof_s_dz[0][6][1]=-0.0353447; p4_tof_s_dz[0][6][1]=0.00190185; 
  p0_tof_m_dz[0][0][7][1]=-1.63018; p1_tof_m_dz[0][0][7][1]=0.125542; p2_tof_m_dz[0][0][7][1]=-0.302715; p3_tof_m_dz[0][0][7][1]=0.089329; p4_tof_m_dz[0][0][7][1]=-0.0102773; 
  p0_tof_m_dz[1][0][7][1]=-1.56686; p1_tof_m_dz[1][0][7][1]=0.368679; p2_tof_m_dz[1][0][7][1]=0.03293; p3_tof_m_dz[1][0][7][1]=-2.53; p4_tof_m_dz[1][0][7][1]=2.03791; 
  p0_tof_s_dz[0][7][1]=1.11882; p1_tof_s_dz[0][7][1]=0.00494254; p2_tof_s_dz[0][7][1]=0.471904; p3_tof_s_dz[0][7][1]=-0.10833; p4_tof_s_dz[0][7][1]=0.0123191; 
  p0_tof_m_dz[0][0][8][1]=-1.8049; p1_tof_m_dz[0][0][8][1]=-0.0161446; p2_tof_m_dz[0][0][8][1]=-0.136736; p3_tof_m_dz[0][0][8][1]=0.0119209; p4_tof_m_dz[0][0][8][1]=5.84886e-05; 
  p0_tof_m_dz[1][0][8][1]=-1.76312; p1_tof_m_dz[1][0][8][1]=0.770099; p2_tof_m_dz[1][0][8][1]=-0.310527; p3_tof_m_dz[1][0][8][1]=-3.95171; p4_tof_m_dz[1][0][8][1]=3.44075; 
  p0_tof_s_dz[0][8][1]=1.08414; p1_tof_s_dz[0][8][1]=0.0393987; p2_tof_s_dz[0][8][1]=0.455511; p3_tof_s_dz[0][8][1]=-0.105467; p4_tof_s_dz[0][8][1]=0.0122297; 
  p0_tof_m_dz[0][0][9][1]=-1.83067; p1_tof_m_dz[0][0][9][1]=0.317357; p2_tof_m_dz[0][0][9][1]=-0.597394; p3_tof_m_dz[0][0][9][1]=0.173096; p4_tof_m_dz[0][0][9][1]=-0.0182859; 
  p0_tof_m_dz[1][0][9][1]=-2.17183; p1_tof_m_dz[1][0][9][1]=2.41998; p2_tof_m_dz[1][0][9][1]=-0.840485; p3_tof_m_dz[1][0][9][1]=-7.22048; p4_tof_m_dz[1][0][9][1]=6.0281; 
  p0_tof_s_dz[0][9][1]=1.22852; p1_tof_s_dz[0][9][1]=-0.30336; p2_tof_s_dz[0][9][1]=0.742465; p3_tof_s_dz[0][9][1]=-0.197155; p4_tof_s_dz[0][9][1]=0.0223569; 
  p0_tof_m_dz[0][1][0][1]=0.0289072; p1_tof_m_dz[0][1][0][1]=0.397236; p2_tof_m_dz[0][1][0][1]=-0.327217; p3_tof_m_dz[0][1][0][1]=0.124932; p4_tof_m_dz[0][1][0][1]=-0.0176864; 
  p0_tof_m_dz[1][1][0][1]=2.25083; p1_tof_m_dz[1][1][0][1]=-9.64357; p2_tof_m_dz[1][1][0][1]=3.4691; p3_tof_m_dz[1][1][0][1]=26.8478; p4_tof_m_dz[1][1][0][1]=-26.0313; 
  p0_tof_s_dz[1][0][1]=1.30543; p1_tof_s_dz[1][0][1]=-0.0880133; p2_tof_s_dz[1][0][1]=0.307858; p3_tof_s_dz[1][0][1]=0.000695131; p4_tof_s_dz[1][0][1]=-0.00408264; 
  p0_tof_m_dz[0][1][1][1]=-0.0678077; p1_tof_m_dz[0][1][1][1]=0.0440868; p2_tof_m_dz[0][1][1][1]=0.0417355; p3_tof_m_dz[0][1][1][1]=0.00195457; p4_tof_m_dz[0][1][1][1]=-0.00231196; 
  p0_tof_m_dz[1][1][1][1]=-0.0971276; p1_tof_m_dz[1][1][1][1]=-0.64419; p2_tof_m_dz[1][1][1][1]=0.633199; p3_tof_m_dz[1][1][1][1]=2.22813; p4_tof_m_dz[1][1][1][1]=-2.16002; 
  p0_tof_s_dz[1][1][1]=1.35641; p1_tof_s_dz[1][1][1]=-0.0843019; p2_tof_s_dz[1][1][1]=0.356401; p3_tof_s_dz[1][1][1]=-0.0287125; p4_tof_s_dz[1][1][1]=-0.000639114; 
  p0_tof_m_dz[0][1][2][1]=-0.248688; p1_tof_m_dz[0][1][2][1]=0.239853; p2_tof_m_dz[0][1][2][1]=-0.111129; p3_tof_m_dz[0][1][2][1]=0.0420117; p4_tof_m_dz[0][1][2][1]=-0.0054076; 
  p0_tof_m_dz[1][1][2][1]=0.455342; p1_tof_m_dz[1][1][2][1]=-3.146; p2_tof_m_dz[1][1][2][1]=1.39184; p3_tof_m_dz[1][1][2][1]=9.44185; p4_tof_m_dz[1][1][2][1]=-9.65107; 
  p0_tof_s_dz[1][2][1]=1.37276; p1_tof_s_dz[1][2][1]=-0.0638476; p2_tof_s_dz[1][2][1]=0.37384; p3_tof_s_dz[1][2][1]=-0.0480626; p4_tof_s_dz[1][2][1]=0.00284462; 
  p0_tof_m_dz[0][1][3][1]=-0.223478; p1_tof_m_dz[0][1][3][1]=-0.159781; p2_tof_m_dz[0][1][3][1]=0.262778; p3_tof_m_dz[0][1][3][1]=-0.0920453; p4_tof_m_dz[0][1][3][1]=0.00981137; 
  p0_tof_m_dz[1][1][3][1]=-1.06727; p1_tof_m_dz[1][1][3][1]=2.8764; p2_tof_m_dz[1][1][3][1]=-0.631912; p3_tof_m_dz[1][1][3][1]=-7.45775; p4_tof_m_dz[1][1][3][1]=7.11392; 
  p0_tof_s_dz[1][3][1]=1.26343; p1_tof_s_dz[1][3][1]=0.00561135; p2_tof_s_dz[1][3][1]=0.281096; p3_tof_s_dz[1][3][1]=-0.00907063; p4_tof_s_dz[1][3][1]=-0.000932621; 
  p0_tof_m_dz[0][1][4][1]=-0.32051; p1_tof_m_dz[0][1][4][1]=-0.303572; p2_tof_m_dz[0][1][4][1]=0.408117; p3_tof_m_dz[0][1][4][1]=-0.14615; p4_tof_m_dz[0][1][4][1]=0.0165139; 
  p0_tof_m_dz[1][1][4][1]=-0.789233; p1_tof_m_dz[1][1][4][1]=1.48378; p2_tof_m_dz[1][1][4][1]=-0.276449; p3_tof_m_dz[1][1][4][1]=-4.43523; p4_tof_m_dz[1][1][4][1]=4.33717; 
  p0_tof_s_dz[1][4][1]=1.26188; p1_tof_s_dz[1][4][1]=0.055308; p2_tof_s_dz[1][4][1]=0.179392; p3_tof_s_dz[1][4][1]=0.0517697; p4_tof_s_dz[1][4][1]=-0.0104166; 
  p0_tof_m_dz[0][1][5][1]=-0.394531; p1_tof_m_dz[0][1][5][1]=-0.020824; p2_tof_m_dz[0][1][5][1]=0.0111323; p3_tof_m_dz[0][1][5][1]=0.00716959; p4_tof_m_dz[0][1][5][1]=-0.00191563; 
  p0_tof_m_dz[1][1][5][1]=-0.241808; p1_tof_m_dz[1][1][5][1]=-0.616234; p2_tof_m_dz[1][1][5][1]=0.273998; p3_tof_m_dz[1][1][5][1]=1.17429; p4_tof_m_dz[1][1][5][1]=-1.11586; 
  p0_tof_s_dz[1][5][1]=1.27332; p1_tof_s_dz[1][5][1]=-0.0403981; p2_tof_s_dz[1][5][1]=0.338579; p3_tof_s_dz[1][5][1]=-0.0329255; p4_tof_s_dz[1][5][1]=0.000397075; 
  p0_tof_m_dz[0][1][6][1]=-0.510459; p1_tof_m_dz[0][1][6][1]=-0.0465973; p2_tof_m_dz[0][1][6][1]=-0.0283224; p3_tof_m_dz[0][1][6][1]=0.0174511; p4_tof_m_dz[0][1][6][1]=-0.00213186; 
  p0_tof_m_dz[1][1][6][1]=0.0295315; p1_tof_m_dz[1][1][6][1]=-2.25864; p2_tof_m_dz[1][1][6][1]=0.962428; p3_tof_m_dz[1][1][6][1]=5.54157; p4_tof_m_dz[1][1][6][1]=-5.82686; 
  p0_tof_s_dz[1][6][1]=1.22599; p1_tof_s_dz[1][6][1]=0.118077; p2_tof_s_dz[1][6][1]=0.172349; p3_tof_s_dz[1][6][1]=0.02935; p4_tof_s_dz[1][6][1]=-0.00693932; 
  p0_tof_m_dz[0][1][7][1]=-0.724791; p1_tof_m_dz[0][1][7][1]=0.0720894; p2_tof_m_dz[0][1][7][1]=-0.0829898; p3_tof_m_dz[0][1][7][1]=0.0201989; p4_tof_m_dz[0][1][7][1]=-0.00171332; 
  p0_tof_m_dz[1][1][7][1]=-0.73002; p1_tof_m_dz[1][1][7][1]=0.391568; p2_tof_m_dz[1][1][7][1]=-0.0224569; p3_tof_m_dz[1][1][7][1]=-1.55389; p4_tof_m_dz[1][1][7][1]=1.15302; 
  p0_tof_s_dz[1][7][1]=1.24358; p1_tof_s_dz[1][7][1]=0.0818206; p2_tof_s_dz[1][7][1]=0.199696; p3_tof_s_dz[1][7][1]=0.022595; p4_tof_s_dz[1][7][1]=-0.00626548; 
  p0_tof_m_dz[0][1][8][1]=-1.03536; p1_tof_m_dz[0][1][8][1]=0.548993; p2_tof_m_dz[0][1][8][1]=-0.507132; p3_tof_m_dz[0][1][8][1]=0.156472; p4_tof_m_dz[0][1][8][1]=-0.0156083; 
  p0_tof_m_dz[1][1][8][1]=-1.46634; p1_tof_m_dz[1][1][8][1]=3.03736; p2_tof_m_dz[1][1][8][1]=-0.822564; p3_tof_m_dz[1][1][8][1]=-8.64285; p4_tof_m_dz[1][1][8][1]=7.7121; 
  p0_tof_s_dz[1][8][1]=1.35842; p1_tof_s_dz[1][8][1]=-0.19227; p2_tof_s_dz[1][8][1]=0.501959; p3_tof_s_dz[1][8][1]=-0.0908022; p4_tof_s_dz[1][8][1]=0.0074401; 
  p0_tof_m_dz[0][1][9][1]=-0.881006; p1_tof_m_dz[0][1][9][1]=-0.257713; p2_tof_m_dz[0][1][9][1]=0.225223; p3_tof_m_dz[0][1][9][1]=-0.104935; p4_tof_m_dz[0][1][9][1]=0.0170555; 
  p0_tof_m_dz[1][1][9][1]=-0.261518; p1_tof_m_dz[1][1][9][1]=-1.96357; p2_tof_m_dz[1][1][9][1]=0.586167; p3_tof_m_dz[1][1][9][1]=4.76085; p4_tof_m_dz[1][1][9][1]=-5.6002; 
  p0_tof_s_dz[1][9][1]=1.41702; p1_tof_s_dz[1][9][1]=-0.17398; p2_tof_s_dz[1][9][1]=0.445452; p3_tof_s_dz[1][9][1]=-0.0371324; p4_tof_s_dz[1][9][1]=-0.00204197; 
  
  //! 
  //! After burner
  //! 
  
  //! PC2 sdphi neg
  p0_pc2_m_sdp[0][0][0]=-0.0772571; p1_pc2_m_sdp[0][0][0]=0.266627; p2_pc2_m_sdp[0][0][0]=-0.257064; p3_pc2_m_sdp[0][0][0]=0.0910131; p4_pc2_m_sdp[0][0][0]=-0.0105878; 
  p0_pc2_s_sdp[0][0][0]=0.759614; p1_pc2_s_sdp[0][0][0]=0.19833; p2_pc2_s_sdp[0][0][0]=-0.100781; p3_pc2_s_sdp[0][0][0]=0.0251601; p4_pc2_s_sdp[0][0][0]=-0.00249283; 
  p0_pc2_m_sdp[1][0][0]=0.239381; p1_pc2_m_sdp[1][0][0]=-1.06599; p2_pc2_m_sdp[1][0][0]=0.311479; p3_pc2_m_sdp[1][0][0]=2.89757; p4_pc2_m_sdp[1][0][0]=-2.63529; 
  p0_pc2_s_sdp[1][0][0]=3.1719; p1_pc2_s_sdp[1][0][0]=-8.72991; p2_pc2_s_sdp[1][0][0]=2.34334; p3_pc2_s_sdp[1][0][0]=23.4171; p4_pc2_s_sdp[1][0][0]=-22.7293; 
  p0_pc2_m_sdp[0][1][0]=-0.150789; p1_pc2_m_sdp[0][1][0]=0.475513; p2_pc2_m_sdp[0][1][0]=-0.428335; p3_pc2_m_sdp[0][1][0]=0.14531; p4_pc2_m_sdp[0][1][0]=-0.0165171; 
  p0_pc2_s_sdp[0][1][0]=0.76743; p1_pc2_s_sdp[0][1][0]=0.177913; p2_pc2_s_sdp[0][1][0]=-0.0808706; p3_pc2_s_sdp[0][1][0]=0.017419; p4_pc2_s_sdp[0][1][0]=-0.00143151; 
  p0_pc2_m_sdp[1][1][0]=-0.359149; p1_pc2_m_sdp[1][1][0]=1.27842; p2_pc2_m_sdp[1][1][0]=-0.309942; p3_pc2_m_sdp[1][1][0]=-3.31189; p4_pc2_m_sdp[1][1][0]=3.26103; 
  p0_pc2_s_sdp[1][1][0]=1.73016; p1_pc2_s_sdp[1][1][0]=-2.73921; p2_pc2_s_sdp[1][1][0]=0.101553; p3_pc2_s_sdp[1][1][0]=6.65105; p4_pc2_s_sdp[1][1][0]=-5.36246; 
  p0_pc2_m_sdp[0][2][0]=-0.067735; p1_pc2_m_sdp[0][2][0]=0.252302; p2_pc2_m_sdp[0][2][0]=-0.227499; p3_pc2_m_sdp[0][2][0]=0.074394; p4_pc2_m_sdp[0][2][0]=-0.00810697; 
  p0_pc2_s_sdp[0][2][0]=0.647829; p1_pc2_s_sdp[0][2][0]=0.489422; p2_pc2_s_sdp[0][2][0]=-0.330303; p3_pc2_s_sdp[0][2][0]=0.0957144; p4_pc2_s_sdp[0][2][0]=-0.00983808; 
  p0_pc2_m_sdp[1][2][0]=0.685216; p1_pc2_m_sdp[1][2][0]=-2.61765; p2_pc2_m_sdp[1][2][0]=0.500662; p3_pc2_m_sdp[1][2][0]=7.09476; p4_pc2_m_sdp[1][2][0]=-6.30287; 
  p0_pc2_s_sdp[1][2][0]=3.14378; p1_pc2_s_sdp[1][2][0]=-8.07702; p2_pc2_s_sdp[1][2][0]=1.25973; p3_pc2_s_sdp[1][2][0]=21.2895; p4_pc2_s_sdp[1][2][0]=-19.1039; 
  p0_pc2_m_sdp[0][3][0]=-0.16581; p1_pc2_m_sdp[0][3][0]=0.433053; p2_pc2_m_sdp[0][3][0]=-0.376515; p3_pc2_m_sdp[0][3][0]=0.124927; p4_pc2_m_sdp[0][3][0]=-0.01387; 
  p0_pc2_s_sdp[0][3][0]=0.877429; p1_pc2_s_sdp[0][3][0]=0.0731333; p2_pc2_s_sdp[0][3][0]=-0.0505568; p3_pc2_s_sdp[0][3][0]=0.0125776; p4_pc2_s_sdp[0][3][0]=-0.000909183; 
  p0_pc2_m_sdp[1][3][0]=-0.22871; p1_pc2_m_sdp[1][3][0]=0.996207; p2_pc2_m_sdp[1][3][0]=-0.545424; p3_pc2_m_sdp[1][3][0]=-2.74635; p4_pc2_m_sdp[1][3][0]=3.14075; 
  p0_pc2_s_sdp[1][3][0]=-0.437599; p1_pc2_s_sdp[1][3][0]=6.1448; p2_pc2_s_sdp[1][3][0]=-2.91291; p3_pc2_s_sdp[1][3][0]=-17.0615; p4_pc2_s_sdp[1][3][0]=18.2872; 
  p0_pc2_m_sdp[0][4][0]=0.0233382; p1_pc2_m_sdp[0][4][0]=0.0129934; p2_pc2_m_sdp[0][4][0]=-0.0777954; p3_pc2_m_sdp[0][4][0]=0.0391691; p4_pc2_m_sdp[0][4][0]=-0.00536466; 
  p0_pc2_s_sdp[0][4][0]=0.60838; p1_pc2_s_sdp[0][4][0]=0.729364; p2_pc2_s_sdp[0][4][0]=-0.568952; p3_pc2_s_sdp[0][4][0]=0.174954; p4_pc2_s_sdp[0][4][0]=-0.0183206; 
  p0_pc2_m_sdp[1][4][0]=1.58304; p1_pc2_m_sdp[1][4][0]=-6.48515; p2_pc2_m_sdp[1][4][0]=2.04913; p3_pc2_m_sdp[1][4][0]=17.6995; p4_pc2_m_sdp[1][4][0]=-17.4161; 
  p0_pc2_s_sdp[1][4][0]=4.0461; p1_pc2_s_sdp[1][4][0]=-11.931; p2_pc2_s_sdp[1][4][0]=2.57649; p3_pc2_s_sdp[1][4][0]=32.1916; p4_pc2_s_sdp[1][4][0]=-29.8632; 
  p0_pc2_m_sdp[0][5][0]=0.166572; p1_pc2_m_sdp[0][5][0]=-0.280662; p2_pc2_m_sdp[0][5][0]=0.15741; p3_pc2_m_sdp[0][5][0]=-0.0344127; p4_pc2_m_sdp[0][5][0]=0.00242054; 
  p0_pc2_s_sdp[0][5][0]=0.785384; p1_pc2_s_sdp[0][5][0]=0.409616; p2_pc2_s_sdp[0][5][0]=-0.346625; p3_pc2_s_sdp[0][5][0]=0.10978; p4_pc2_s_sdp[0][5][0]=-0.0116273; 
  p0_pc2_m_sdp[1][5][0]=-0.409914; p1_pc2_m_sdp[1][5][0]=1.66771; p2_pc2_m_sdp[1][5][0]=-0.381494; p3_pc2_m_sdp[1][5][0]=-4.83607; p4_pc2_m_sdp[1][5][0]=4.63003; 
  p0_pc2_s_sdp[1][5][0]=0.0782878; p1_pc2_s_sdp[1][5][0]=3.80286; p2_pc2_s_sdp[1][5][0]=-1.63392; p3_pc2_s_sdp[1][5][0]=-10.6705; p4_pc2_s_sdp[1][5][0]=11.1122; 
  p0_pc2_m_sdp[0][6][0]=0.074694; p1_pc2_m_sdp[0][6][0]=-0.0750662; p2_pc2_m_sdp[0][6][0]=0.0120659; p3_pc2_m_sdp[0][6][0]=0.0076401; p4_pc2_m_sdp[0][6][0]=-0.00183743; 
  p0_pc2_s_sdp[0][6][0]=0.789231; p1_pc2_s_sdp[0][6][0]=0.384878; p2_pc2_s_sdp[0][6][0]=-0.319501; p3_pc2_s_sdp[0][6][0]=0.0982136; p4_pc2_s_sdp[0][6][0]=-0.0100916; 
  p0_pc2_m_sdp[1][6][0]=0.262683; p1_pc2_m_sdp[1][6][0]=-1.28486; p2_pc2_m_sdp[1][6][0]=0.72057; p3_pc2_m_sdp[1][6][0]=3.87773; p4_pc2_m_sdp[1][6][0]=-4.40421; 
  p0_pc2_s_sdp[1][6][0]=1.50041; p1_pc2_s_sdp[1][6][0]=-2.30552; p2_pc2_s_sdp[1][6][0]=0.636068; p3_pc2_s_sdp[1][6][0]=6.91921; p4_pc2_s_sdp[1][6][0]=-7.09354; 
  p0_pc2_m_sdp[0][7][0]=-0.279407; p1_pc2_m_sdp[0][7][0]=0.729751; p2_pc2_m_sdp[0][7][0]=-0.583328; p3_pc2_m_sdp[0][7][0]=0.185246; p4_pc2_m_sdp[0][7][0]=-0.0202058; 
  p0_pc2_s_sdp[0][7][0]=0.763363; p1_pc2_s_sdp[0][7][0]=0.387514; p2_pc2_s_sdp[0][7][0]=-0.324798; p3_pc2_s_sdp[0][7][0]=0.103945; p4_pc2_s_sdp[0][7][0]=-0.0111476; 
  p0_pc2_m_sdp[1][7][0]=-0.393687; p1_pc2_m_sdp[1][7][0]=1.95006; p2_pc2_m_sdp[1][7][0]=-1.04119; p3_pc2_m_sdp[1][7][0]=-5.77674; p4_pc2_m_sdp[1][7][0]=6.49372; 
  p0_pc2_s_sdp[1][7][0]=1.2908; p1_pc2_s_sdp[1][7][0]=-0.0990279; p2_pc2_s_sdp[1][7][0]=-1.54944; p3_pc2_s_sdp[1][7][0]=-1.35791; p4_pc2_s_sdp[1][7][0]=4.0397; 
  p0_pc2_m_sdp[0][8][0]=-0.0912309; p1_pc2_m_sdp[0][8][0]=0.212258; p2_pc2_m_sdp[0][8][0]=-0.137471; p3_pc2_m_sdp[0][8][0]=0.036545; p4_pc2_m_sdp[0][8][0]=-0.00347028; 
  p0_pc2_s_sdp[0][8][0]=0.660912; p1_pc2_s_sdp[0][8][0]=0.538545; p2_pc2_s_sdp[0][8][0]=-0.396228; p3_pc2_s_sdp[0][8][0]=0.115651; p4_pc2_s_sdp[0][8][0]=-0.0115819; 
  p0_pc2_m_sdp[1][8][0]=-0.163129; p1_pc2_m_sdp[1][8][0]=0.198748; p2_pc2_m_sdp[1][8][0]=0.512273; p3_pc2_m_sdp[1][8][0]=0.291891; p4_pc2_m_sdp[1][8][0]=-1.44221; 
  p0_pc2_s_sdp[1][8][0]=3.66024; p1_pc2_s_sdp[1][8][0]=-10.494; p2_pc2_s_sdp[1][8][0]=2.38854; p3_pc2_s_sdp[1][8][0]=29.3294; p4_pc2_s_sdp[1][8][0]=-28.1286; 
  p0_pc2_m_sdp[0][9][0]=0.0247249; p1_pc2_m_sdp[0][9][0]=-0.0854465; p2_pc2_m_sdp[0][9][0]=0.0720325; p3_pc2_m_sdp[0][9][0]=-0.0213192; p4_pc2_m_sdp[0][9][0]=0.00221601; 
  p0_pc2_s_sdp[0][9][0]=0.978814; p1_pc2_s_sdp[0][9][0]=-0.199534; p2_pc2_s_sdp[0][9][0]=0.16807; p3_pc2_s_sdp[0][9][0]=-0.0589248; p4_pc2_s_sdp[0][9][0]=0.0069929; 
  p0_pc2_m_sdp[1][9][0]=-0.128904; p1_pc2_m_sdp[1][9][0]=0.320361; p2_pc2_m_sdp[1][9][0]=-0.0340502; p3_pc2_m_sdp[1][9][0]=-0.463026; p4_pc2_m_sdp[1][9][0]=0.258732; 
  p0_pc2_s_sdp[1][9][0]=2.35435; p1_pc2_s_sdp[1][9][0]=-5.17602; p2_pc2_s_sdp[1][9][0]=0.665259; p3_pc2_s_sdp[1][9][0]=13.5122; p4_pc2_s_sdp[1][9][0]=-11.6033; 
  
  //! PC2 sdphi pos
  p0_pc2_m_sdp[0][0][1]=-0.117743; p1_pc2_m_sdp[0][0][1]=0.369455; p2_pc2_m_sdp[0][0][1]=-0.292427; p3_pc2_m_sdp[0][0][1]=0.0901209; p4_pc2_m_sdp[0][0][1]=-0.00964952; 
  p0_pc2_s_sdp[0][0][1]=0.930105; p1_pc2_s_sdp[0][0][1]=-0.140053; p2_pc2_s_sdp[0][0][1]=0.138905; p3_pc2_s_sdp[0][0][1]=-0.0427772; p4_pc2_s_sdp[0][0][1]=0.00410427; 
  p0_pc2_m_sdp[1][0][1]=0.117031; p1_pc2_m_sdp[1][0][1]=0.195447; p2_pc2_m_sdp[1][0][1]=-0.575346; p3_pc2_m_sdp[1][0][1]=-1.18238; p4_pc2_m_sdp[1][0][1]=1.98752; 
  p0_pc2_s_sdp[1][0][1]=1.09029; p1_pc2_s_sdp[1][0][1]=-0.861375; p2_pc2_s_sdp[1][0][1]=0.147137; p3_pc2_s_sdp[1][0][1]=2.73599; p4_pc2_s_sdp[1][0][1]=-2.70892; 
  p0_pc2_m_sdp[0][1][1]=0.0926497; p1_pc2_m_sdp[0][1][1]=-0.191172; p2_pc2_m_sdp[0][1][1]=0.157839; p3_pc2_m_sdp[0][1][1]=-0.0517667; p4_pc2_m_sdp[0][1][1]=0.00571214; 
  p0_pc2_s_sdp[0][1][1]=0.871079; p1_pc2_s_sdp[0][1][1]=-0.0864154; p2_pc2_s_sdp[0][1][1]=0.138105; p3_pc2_s_sdp[0][1][1]=-0.0519039; p4_pc2_s_sdp[0][1][1]=0.00596795; 
  p0_pc2_m_sdp[1][1][1]=0.781551; p1_pc2_m_sdp[1][1][1]=-2.52706; p2_pc2_m_sdp[1][1][1]=0.235846; p3_pc2_m_sdp[1][1][1]=6.20076; p4_pc2_m_sdp[1][1][1]=-5.22116; 
  p0_pc2_s_sdp[1][1][1]=0.281016; p1_pc2_s_sdp[1][1][1]=2.08947; p2_pc2_s_sdp[1][1][1]=-0.712952; p3_pc2_s_sdp[1][1][1]=-5.22498; p4_pc2_s_sdp[1][1][1]=5.36842; 
  p0_pc2_m_sdp[0][2][1]=0.0729508; p1_pc2_m_sdp[0][2][1]=-0.192727; p2_pc2_m_sdp[0][2][1]=0.163995; p3_pc2_m_sdp[0][2][1]=-0.0527963; p4_pc2_m_sdp[0][2][1]=0.00572398; 
  p0_pc2_s_sdp[0][2][1]=0.743967; p1_pc2_s_sdp[0][2][1]=0.197648; p2_pc2_s_sdp[0][2][1]=-0.082975; p3_pc2_s_sdp[0][2][1]=0.0169189; p4_pc2_s_sdp[0][2][1]=-0.00130119; 
  p0_pc2_m_sdp[1][2][1]=0.370318; p1_pc2_m_sdp[1][2][1]=-1.22508; p2_pc2_m_sdp[1][2][1]=0.254492; p3_pc2_m_sdp[1][2][1]=3.00864; p4_pc2_m_sdp[1][2][1]=-2.87646; 
  p0_pc2_s_sdp[1][2][1]=0.124314; p1_pc2_s_sdp[1][2][1]=2.66145; p2_pc2_s_sdp[1][2][1]=-0.936577; p3_pc2_s_sdp[1][2][1]=-6.74089; p4_pc2_s_sdp[1][2][1]=6.91801; 
  p0_pc2_m_sdp[0][3][1]=0.110638; p1_pc2_m_sdp[0][3][1]=-0.353836; p2_pc2_m_sdp[0][3][1]=0.304822; p3_pc2_m_sdp[0][3][1]=-0.0989227; p4_pc2_m_sdp[0][3][1]=0.0108865; 
  p0_pc2_s_sdp[0][3][1]=0.686958; p1_pc2_s_sdp[0][3][1]=0.430722; p2_pc2_s_sdp[0][3][1]=-0.30729; p3_pc2_s_sdp[0][3][1]=0.0925306; p4_pc2_s_sdp[0][3][1]=-0.00974507; 
  p0_pc2_m_sdp[1][3][1]=-0.200517; p1_pc2_m_sdp[1][3][1]=1.01938; p2_pc2_m_sdp[1][3][1]=-0.740992; p3_pc2_m_sdp[1][3][1]=-3.45213; p4_pc2_m_sdp[1][3][1]=4.32714; 
  p0_pc2_s_sdp[1][3][1]=4.17407; p1_pc2_s_sdp[1][3][1]=-13.0469; p2_pc2_s_sdp[1][3][1]=3.54609; p3_pc2_s_sdp[1][3][1]=36.2189; p4_pc2_s_sdp[1][3][1]=-35.0602; 
  p0_pc2_m_sdp[0][4][1]=0.189432; p1_pc2_m_sdp[0][4][1]=-0.522459; p2_pc2_m_sdp[0][4][1]=0.423317; p3_pc2_m_sdp[0][4][1]=-0.13364; p4_pc2_m_sdp[0][4][1]=0.01444; 
  p0_pc2_s_sdp[0][4][1]=0.583677; p1_pc2_s_sdp[0][4][1]=0.664618; p2_pc2_s_sdp[0][4][1]=-0.493702; p3_pc2_s_sdp[0][4][1]=0.148973; p4_pc2_s_sdp[0][4][1]=-0.0153733; 
  p0_pc2_m_sdp[1][4][1]=0.399566; p1_pc2_m_sdp[1][4][1]=-1.03388; p2_pc2_m_sdp[1][4][1]=-0.339479; p3_pc2_m_sdp[1][4][1]=2.23324; p4_pc2_m_sdp[1][4][1]=-1.2616; 
  p0_pc2_s_sdp[1][4][1]=1.47471; p1_pc2_s_sdp[1][4][1]=-1.85809; p2_pc2_s_sdp[1][4][1]=-0.717673; p3_pc2_s_sdp[1][4][1]=5.28553; p4_pc2_s_sdp[1][4][1]=-2.93223; 
  p0_pc2_m_sdp[0][5][1]=0.129374; p1_pc2_m_sdp[0][5][1]=-0.341303; p2_pc2_m_sdp[0][5][1]=0.292999; p3_pc2_m_sdp[0][5][1]=-0.0945763; p4_pc2_m_sdp[0][5][1]=0.0101418; 
  p0_pc2_s_sdp[0][5][1]=0.784616; p1_pc2_s_sdp[0][5][1]=0.28063; p2_pc2_s_sdp[0][5][1]=-0.232868; p3_pc2_s_sdp[0][5][1]=0.0747114; p4_pc2_s_sdp[0][5][1]=-0.00793655; 
  p0_pc2_m_sdp[1][5][1]=0.568622; p1_pc2_m_sdp[1][5][1]=-2.34374; p2_pc2_m_sdp[1][5][1]=0.79466; p3_pc2_m_sdp[1][5][1]=6.22052; p4_pc2_m_sdp[1][5][1]=-6.13551; 
  p0_pc2_s_sdp[1][5][1]=-1.13792; p1_pc2_s_sdp[1][5][1]=8.41865; p2_pc2_s_sdp[1][5][1]=-3.093; p3_pc2_s_sdp[1][5][1]=-23.0837; p4_pc2_s_sdp[1][5][1]=23.6547; 
  p0_pc2_m_sdp[0][6][1]=0.00310859; p1_pc2_m_sdp[0][6][1]=-0.0230382; p2_pc2_m_sdp[0][6][1]=0.0505868; p3_pc2_m_sdp[0][6][1]=-0.0221027; p4_pc2_m_sdp[0][6][1]=0.00271946; 
  p0_pc2_s_sdp[0][6][1]=0.784706; p1_pc2_s_sdp[0][6][1]=0.302228; p2_pc2_s_sdp[0][6][1]=-0.268096; p3_pc2_s_sdp[0][6][1]=0.0912295; p4_pc2_s_sdp[0][6][1]=-0.0103009; 
  p0_pc2_m_sdp[1][6][1]=-0.00209887; p1_pc2_m_sdp[1][6][1]=0.00576616; p2_pc2_m_sdp[1][6][1]=0.00990673; p3_pc2_m_sdp[1][6][1]=-0.00318915; p4_pc2_m_sdp[1][6][1]=-0.0630375; 
  p0_pc2_s_sdp[1][6][1]=0.133991; p1_pc2_s_sdp[1][6][1]=3.08924; p2_pc2_s_sdp[1][6][1]=-1.12824; p3_pc2_s_sdp[1][6][1]=-8.17753; p4_pc2_s_sdp[1][6][1]=8.44589; 
  p0_pc2_m_sdp[0][7][1]=-0.0111326; p1_pc2_m_sdp[0][7][1]=-0.00966112; p2_pc2_m_sdp[0][7][1]=0.0589082; p3_pc2_m_sdp[0][7][1]=-0.0302374; p4_pc2_m_sdp[0][7][1]=0.00411474; 
  p0_pc2_s_sdp[0][7][1]=0.810584; p1_pc2_s_sdp[0][7][1]=0.266109; p2_pc2_s_sdp[0][7][1]=-0.239141; p3_pc2_s_sdp[0][7][1]=0.0815194; p4_pc2_s_sdp[0][7][1]=-0.00912851; 
  p0_pc2_m_sdp[1][7][1]=-1.2122; p1_pc2_m_sdp[1][7][1]=5.11095; p2_pc2_m_sdp[1][7][1]=-1.83645; p3_pc2_m_sdp[1][7][1]=-14.3005; p4_pc2_m_sdp[1][7][1]=14.4954; 
  p0_pc2_s_sdp[1][7][1]=0.70032; p1_pc2_s_sdp[1][7][1]=0.895626; p2_pc2_s_sdp[1][7][1]=-0.658819; p3_pc2_s_sdp[1][7][1]=-2.19432; p4_pc2_s_sdp[1][7][1]=2.83155; 
  p0_pc2_m_sdp[0][8][1]=-0.14466; p1_pc2_m_sdp[0][8][1]=0.26663; p2_pc2_m_sdp[0][8][1]=-0.132722; p3_pc2_m_sdp[0][8][1]=0.0240746; p4_pc2_m_sdp[0][8][1]=-0.00133592; 
  p0_pc2_s_sdp[0][8][1]=0.804417; p1_pc2_s_sdp[0][8][1]=0.318521; p2_pc2_s_sdp[0][8][1]=-0.296137; p3_pc2_s_sdp[0][8][1]=0.101141; p4_pc2_s_sdp[0][8][1]=-0.0112874; 
  p0_pc2_m_sdp[1][8][1]=-0.759567; p1_pc2_m_sdp[1][8][1]=2.88978; p2_pc2_m_sdp[1][8][1]=-0.85016; p3_pc2_m_sdp[1][8][1]=-7.83176; p4_pc2_m_sdp[1][8][1]=7.74346; 
  p0_pc2_s_sdp[1][8][1]=1.58379; p1_pc2_s_sdp[1][8][1]=-2.35952; p2_pc2_s_sdp[1][8][1]=-0.0863801; p3_pc2_s_sdp[1][8][1]=6.87151; p4_pc2_s_sdp[1][8][1]=-5.46309; 
  p0_pc2_m_sdp[0][9][1]=0.251787; p1_pc2_m_sdp[0][9][1]=-0.654839; p2_pc2_m_sdp[0][9][1]=0.549003; p3_pc2_m_sdp[0][9][1]=-0.178372; p4_pc2_m_sdp[0][9][1]=0.0193804; 
  p0_pc2_s_sdp[0][9][1]=0.819423; p1_pc2_s_sdp[0][9][1]=0.331433; p2_pc2_s_sdp[0][9][1]=-0.341103; p3_pc2_s_sdp[0][9][1]=0.12455; p4_pc2_s_sdp[0][9][1]=-0.0147608; 
  p0_pc2_m_sdp[1][9][1]=0.767852; p1_pc2_m_sdp[1][9][1]=-3.11439; p2_pc2_m_sdp[1][9][1]=1.00613; p3_pc2_m_sdp[1][9][1]=8.27847; p4_pc2_m_sdp[1][9][1]=-8.02412; 
  p0_pc2_s_sdp[1][9][1]=2.35267; p1_pc2_s_sdp[1][9][1]=-5.45033; p2_pc2_s_sdp[1][9][1]=1.32563; p3_pc2_s_sdp[1][9][1]=14.4806; p4_pc2_s_sdp[1][9][1]=-13.6652;
  
  //! PC2 sdz neg
  p0_pc2_m_sdz[0][0][0]=-0.0375747; p1_pc2_m_sdz[0][0][0]=0.0196539; p2_pc2_m_sdz[0][0][0]=-0.0355569; p3_pc2_m_sdz[0][0][0]=0.0184379; p4_pc2_m_sdz[0][0][0]=-0.00268136; 
  p0_pc2_s_sdz[0][0][0]=0.719545; p1_pc2_s_sdz[0][0][0]=0.0834154; p2_pc2_s_sdz[0][0][0]=-0.00679763; p3_pc2_s_sdz[0][0][0]=-0.00893947; p4_pc2_s_sdz[0][0][0]=0.00180582; 
  p0_pc2_m_sdz[1][0][0]=-0.928984; p1_pc2_m_sdz[1][0][0]=3.59015; p2_pc2_m_sdz[1][0][0]=-1.2185; p3_pc2_m_sdz[1][0][0]=-9.71185; p4_pc2_m_sdz[1][0][0]=9.69604; 
  p0_pc2_s_sdz[1][0][0]=1.63558; p1_pc2_s_sdz[1][0][0]=-3.91245; p2_pc2_s_sdz[1][0][0]=1.42531; p3_pc2_s_sdz[1][0][0]=12.6789; p4_pc2_s_sdz[1][0][0]=-13.4412; 
  p0_pc2_m_sdz[0][1][0]=-0.0133852; p1_pc2_m_sdz[0][1][0]=-0.0236251; p2_pc2_m_sdz[0][1][0]=0.00989485; p3_pc2_m_sdz[0][1][0]=-0.000663086; p4_pc2_m_sdz[0][1][0]=-0.000113185; 
  p0_pc2_s_sdz[0][1][0]=0.708907; p1_pc2_s_sdz[0][1][0]=0.133954; p2_pc2_s_sdz[0][1][0]=-0.0551795; p3_pc2_s_sdz[0][1][0]=0.00894768; p4_pc2_s_sdz[0][1][0]=-0.000469658; 
  p0_pc2_m_sdz[1][1][0]=-54.8356; p1_pc2_m_sdz[1][1][0]=419.689; p2_pc2_m_sdz[1][1][0]=-1194.23; p3_pc2_m_sdz[1][1][0]=1496.86; p4_pc2_m_sdz[1][1][0]=-697.547; 
  p0_pc2_s_sdz[1][1][0]=0.542194; p1_pc2_s_sdz[1][1][0]=0.524093; p2_pc2_s_sdz[1][1][0]=0.317002; p3_pc2_s_sdz[1][1][0]=-0.17714; p4_pc2_s_sdz[1][1][0]=-1.06308; 
  p0_pc2_m_sdz[0][2][0]=-0.0433391; p1_pc2_m_sdz[0][2][0]=0.0315408; p2_pc2_m_sdz[0][2][0]=-0.0203594; p3_pc2_m_sdz[0][2][0]=0.00584725; p4_pc2_m_sdz[0][2][0]=-0.000566727; 
  p0_pc2_s_sdz[0][2][0]=0.737269; p1_pc2_s_sdz[0][2][0]=0.136016; p2_pc2_s_sdz[0][2][0]=-0.083609; p3_pc2_s_sdz[0][2][0]=0.0237909; p4_pc2_s_sdz[0][2][0]=-0.00255169; 
  p0_pc2_m_sdz[1][2][0]=1.849; p1_pc2_m_sdz[1][2][0]=-7.78447; p2_pc2_m_sdz[1][2][0]=2.65632; p3_pc2_m_sdz[1][2][0]=21.6151; p4_pc2_m_sdz[1][2][0]=-21.7379; 
  p0_pc2_s_sdz[1][2][0]=1.5987; p1_pc2_s_sdz[1][2][0]=-2.95846; p2_pc2_s_sdz[1][2][0]=0.740992; p3_pc2_s_sdz[1][2][0]=7.53367; p4_pc2_s_sdz[1][2][0]=-7.18768; 
  p0_pc2_m_sdz[0][3][0]=0.00387697; p1_pc2_m_sdz[0][3][0]=-0.0334194; p2_pc2_m_sdz[0][3][0]=0.0236855; p3_pc2_m_sdz[0][3][0]=-0.00723725; p4_pc2_m_sdz[0][3][0]=0.000793819; 
  p0_pc2_s_sdz[0][3][0]=0.715192; p1_pc2_s_sdz[0][3][0]=0.184506; p2_pc2_s_sdz[0][3][0]=-0.120623; p3_pc2_s_sdz[0][3][0]=0.0349428; p4_pc2_s_sdz[0][3][0]=-0.00370089; 
  p0_pc2_m_sdz[1][3][0]=0.176663; p1_pc2_m_sdz[1][3][0]=-0.654778; p2_pc2_m_sdz[1][3][0]=0.00379751; p3_pc2_m_sdz[1][3][0]=1.82251; p4_pc2_m_sdz[1][3][0]=-1.48599; 
  p0_pc2_s_sdz[1][3][0]=1.1151; p1_pc2_s_sdz[1][3][0]=-1.54484; p2_pc2_s_sdz[1][3][0]=0.604408; p3_pc2_s_sdz[1][3][0]=5.41389; p4_pc2_s_sdz[1][3][0]=-5.9424; 
  p0_pc2_m_sdz[0][4][0]=-0.0128892; p1_pc2_m_sdz[0][4][0]=0.00525508; p2_pc2_m_sdz[0][4][0]=-0.00440639; p3_pc2_m_sdz[0][4][0]=0.00146585; p4_pc2_m_sdz[0][4][0]=-0.000153681; 
  p0_pc2_s_sdz[0][4][0]=0.755141; p1_pc2_s_sdz[0][4][0]=0.0932107; p2_pc2_s_sdz[0][4][0]=-0.0496247; p3_pc2_s_sdz[0][4][0]=0.0110109; p4_pc2_s_sdz[0][4][0]=-0.000927036; 
  p0_pc2_m_sdz[1][4][0]=-0.389867; p1_pc2_m_sdz[1][4][0]=1.4875; p2_pc2_m_sdz[1][4][0]=-0.407947; p3_pc2_m_sdz[1][4][0]=-3.97858; p4_pc2_m_sdz[1][4][0]=3.77629; 
  p0_pc2_s_sdz[1][4][0]=0.503154; p1_pc2_s_sdz[1][4][0]=1.24079; p2_pc2_s_sdz[1][4][0]=-0.634211; p3_pc2_s_sdz[1][4][0]=-3.075; p4_pc2_s_sdz[1][4][0]=3.40107; 
  p0_pc2_m_sdz[0][5][0]=0.00740814; p1_pc2_m_sdz[0][5][0]=-0.0243101; p2_pc2_m_sdz[0][5][0]=0.0140097; p3_pc2_m_sdz[0][5][0]=-0.00323158; p4_pc2_m_sdz[0][5][0]=0.000258879; 
  p0_pc2_s_sdz[0][5][0]=0.757915; p1_pc2_s_sdz[0][5][0]=0.0956623; p2_pc2_s_sdz[0][5][0]=-0.0586916; p3_pc2_s_sdz[0][5][0]=0.0155153; p4_pc2_s_sdz[0][5][0]=-0.00157504; 
  p0_pc2_m_sdz[1][5][0]=-0.116229; p1_pc2_m_sdz[1][5][0]=0.437127; p2_pc2_m_sdz[1][5][0]=-0.116983; p3_pc2_m_sdz[1][5][0]=-1.16875; p4_pc2_m_sdz[1][5][0]=1.11687; 
  p0_pc2_s_sdz[1][5][0]=0.86147; p1_pc2_s_sdz[1][5][0]=-0.384163; p2_pc2_s_sdz[1][5][0]=0.0427556; p3_pc2_s_sdz[1][5][0]=1.71374; p4_pc2_s_sdz[1][5][0]=-1.68375; 
  p0_pc2_m_sdz[0][6][0]=-0.053568; p1_pc2_m_sdz[0][6][0]=0.0774569; p2_pc2_m_sdz[0][6][0]=-0.0524286; p3_pc2_m_sdz[0][6][0]=0.0152514; p4_pc2_m_sdz[0][6][0]=-0.00157532; 
  p0_pc2_s_sdz[0][6][0]=0.741042; p1_pc2_s_sdz[0][6][0]=0.134067; p2_pc2_s_sdz[0][6][0]=-0.0893526; p3_pc2_s_sdz[0][6][0]=0.026283; p4_pc2_s_sdz[0][6][0]=-0.00288688; 
  p0_pc2_m_sdz[1][6][0]=-0.490102; p1_pc2_m_sdz[1][6][0]=1.73117; p2_pc2_m_sdz[1][6][0]=-0.170697; p3_pc2_m_sdz[1][6][0]=-4.93661; p4_pc2_m_sdz[1][6][0]=4.33554; 
  p0_pc2_s_sdz[1][6][0]=0.141333; p1_pc2_s_sdz[1][6][0]=2.54693; p2_pc2_s_sdz[1][6][0]=-0.677109; p3_pc2_s_sdz[1][6][0]=-6.55636; p4_pc2_s_sdz[1][6][0]=6.07638; 
  p0_pc2_m_sdz[0][7][0]=-0.010117; p1_pc2_m_sdz[0][7][0]=-0.0649781; p2_pc2_m_sdz[0][7][0]=0.0653577; p3_pc2_m_sdz[0][7][0]=-0.0221059; p4_pc2_m_sdz[0][7][0]=0.00249382; 
  p0_pc2_s_sdz[0][7][0]=0.8325; p1_pc2_s_sdz[0][7][0]=-0.0622081; p2_pc2_s_sdz[0][7][0]=0.0585837; p3_pc2_s_sdz[0][7][0]=-0.0191108; p4_pc2_s_sdz[0][7][0]=0.00192042; 
  p0_pc2_m_sdz[1][7][0]=2.11616; p1_pc2_m_sdz[1][7][0]=-8.66331; p2_pc2_m_sdz[1][7][0]=2.3612; p3_pc2_m_sdz[1][7][0]=24.5391; p4_pc2_m_sdz[1][7][0]=-23.838; 
  p0_pc2_s_sdz[1][7][0]=0.271701; p1_pc2_s_sdz[1][7][0]=2.34156; p2_pc2_s_sdz[1][7][0]=-1.56281; p3_pc2_s_sdz[1][7][0]=-5.39111; p4_pc2_s_sdz[1][7][0]=6.61601; 
  p0_pc2_m_sdz[0][8][0]=-0.0872816; p1_pc2_m_sdz[0][8][0]=0.107521; p2_pc2_m_sdz[0][8][0]=-0.0673746; p3_pc2_m_sdz[0][8][0]=0.0186179; p4_pc2_m_sdz[0][8][0]=-0.00181161; 
  p0_pc2_s_sdz[0][8][0]=0.723792; p1_pc2_s_sdz[0][8][0]=0.194047; p2_pc2_s_sdz[0][8][0]=-0.131445; p3_pc2_s_sdz[0][8][0]=0.0364615; p4_pc2_s_sdz[0][8][0]=-0.00367463; 
  p0_pc2_m_sdz[1][8][0]=-54.7796; p1_pc2_m_sdz[1][8][0]=412.521; p2_pc2_m_sdz[1][8][0]=-1156.11; p3_pc2_m_sdz[1][8][0]=1428.49; p4_pc2_m_sdz[1][8][0]=-656.805; 
  p0_pc2_s_sdz[1][8][0]=-0.795509; p1_pc2_s_sdz[1][8][0]=7.28545; p2_pc2_s_sdz[1][8][0]=-3.4722; p3_pc2_s_sdz[1][8][0]=-20.7269; p4_pc2_s_sdz[1][8][0]=22.6757; 
  p0_pc2_m_sdz[0][9][0]=-0.069077; p1_pc2_m_sdz[0][9][0]=-0.0075415; p2_pc2_m_sdz[0][9][0]=0.0459401; p3_pc2_m_sdz[0][9][0]=-0.0208457; p4_pc2_m_sdz[0][9][0]=0.00271781; 
  p0_pc2_s_sdz[0][9][0]=0.763096; p1_pc2_s_sdz[0][9][0]=0.0103355; p2_pc2_s_sdz[0][9][0]=0.0397225; p3_pc2_s_sdz[0][9][0]=-0.0236775; p4_pc2_s_sdz[0][9][0]=0.0033946; 
  p0_pc2_m_sdz[1][9][0]=1.41091; p1_pc2_m_sdz[1][9][0]=-5.84311; p2_pc2_m_sdz[1][9][0]=1.61309; p3_pc2_m_sdz[1][9][0]=16.2828; p4_pc2_m_sdz[1][9][0]=-15.7962; 
  p0_pc2_s_sdz[1][9][0]=3.65845; p1_pc2_s_sdz[1][9][0]=-11.9169; p2_pc2_s_sdz[1][9][0]=3.8737; p3_pc2_s_sdz[1][9][0]=33.8855; p4_pc2_s_sdz[1][9][0]=-34.0844; 
  
  //! PC2 sdz pos
  p0_pc2_m_sdz[0][0][1]=-0.0610826; p1_pc2_m_sdz[0][0][1]=0.0292362; p2_pc2_m_sdz[0][0][1]=-0.0222512; p3_pc2_m_sdz[0][0][1]=0.00909301; p4_pc2_m_sdz[0][0][1]=-0.00123952; 
  p0_pc2_s_sdz[0][0][1]=0.869724; p1_pc2_s_sdz[0][0][1]=-0.147344; p2_pc2_s_sdz[0][0][1]=0.114062; p3_pc2_s_sdz[0][0][1]=-0.0375682; p4_pc2_s_sdz[0][0][1]=0.00430738; 
  p0_pc2_m_sdz[1][0][1]=-44.0117; p1_pc2_m_sdz[1][0][1]=338.724; p2_pc2_m_sdz[1][0][1]=-970.538; p3_pc2_m_sdz[1][0][1]=1226.11; p4_pc2_m_sdz[1][0][1]=-576.487; 
  p0_pc2_s_sdz[1][0][1]=60.8051; p1_pc2_s_sdz[1][0][1]=-466.084; p2_pc2_s_sdz[1][0][1]=1344.42; p3_pc2_s_sdz[1][0][1]=-1708.3; p4_pc2_s_sdz[1][0][1]=807.338; 
  p0_pc2_m_sdz[0][1][1]=-0.073138; p1_pc2_m_sdz[0][1][1]=0.118298; p2_pc2_m_sdz[0][1][1]=-0.101071; p3_pc2_m_sdz[0][1][1]=0.0337375; p4_pc2_m_sdz[0][1][1]=-0.00377951; 
  p0_pc2_s_sdz[0][1][1]=0.712145; p1_pc2_s_sdz[0][1][1]=0.206924; p2_pc2_s_sdz[0][1][1]=-0.153473; p3_pc2_s_sdz[0][1][1]=0.0461234; p4_pc2_s_sdz[0][1][1]=-0.00486867; 
  p0_pc2_m_sdz[1][1][1]=-60.5966; p1_pc2_m_sdz[1][1][1]=468.276; p2_pc2_m_sdz[1][1][1]=-1342.56; p3_pc2_m_sdz[1][1][1]=1692.7; p4_pc2_m_sdz[1][1][1]=-792.368; 
  p0_pc2_s_sdz[1][1][1]=1.35795; p1_pc2_s_sdz[1][1][1]=-3.42026; p2_pc2_s_sdz[1][1][1]=2.10749; p3_pc2_s_sdz[1][1][1]=12.7504; p4_pc2_s_sdz[1][1][1]=-15.5646; 
  p0_pc2_m_sdz[0][2][1]=-0.0267007; p1_pc2_m_sdz[0][2][1]=-0.0261854; p2_pc2_m_sdz[0][2][1]=0.038172; p3_pc2_m_sdz[0][2][1]=-0.0152265; p4_pc2_m_sdz[0][2][1]=0.00183414; 
  p0_pc2_s_sdz[0][2][1]=0.687059; p1_pc2_s_sdz[0][2][1]=0.312527; p2_pc2_s_sdz[0][2][1]=-0.259102; p3_pc2_s_sdz[0][2][1]=0.084466; p4_pc2_s_sdz[0][2][1]=-0.00940352; 
  p0_pc2_m_sdz[1][2][1]=-0.507341; p1_pc2_m_sdz[1][2][1]=1.64322; p2_pc2_m_sdz[1][2][1]=0.14178; p3_pc2_m_sdz[1][2][1]=-4.42389; p4_pc2_m_sdz[1][2][1]=3.09742; 
  p0_pc2_s_sdz[1][2][1]=-1.81546; p1_pc2_s_sdz[1][2][1]=10.1612; p2_pc2_s_sdz[1][2][1]=-2.69363; p3_pc2_s_sdz[1][2][1]=-27.9994; p4_pc2_s_sdz[1][2][1]=27.1493; 
  p0_pc2_m_sdz[0][3][1]=-0.0081815; p1_pc2_m_sdz[0][3][1]=-0.0230936; p2_pc2_m_sdz[0][3][1]=0.0209753; p3_pc2_m_sdz[0][3][1]=-0.00739334; p4_pc2_m_sdz[0][3][1]=0.000875843; 
  p0_pc2_s_sdz[0][3][1]=0.675698; p1_pc2_s_sdz[0][3][1]=0.332234; p2_pc2_s_sdz[0][3][1]=-0.266488; p3_pc2_s_sdz[0][3][1]=0.0845173; p4_pc2_s_sdz[0][3][1]=-0.00924723; 
  p0_pc2_m_sdz[1][3][1]=0.61661; p1_pc2_m_sdz[1][3][1]=-2.41644; p2_pc2_m_sdz[1][3][1]=0.484041; p3_pc2_m_sdz[1][3][1]=6.78645; p4_pc2_m_sdz[1][3][1]=-6.33742; 
  p0_pc2_s_sdz[1][3][1]=1.05905; p1_pc2_s_sdz[1][3][1]=-0.808708; p2_pc2_s_sdz[1][3][1]=-0.181107; p3_pc2_s_sdz[1][3][1]=2.42121; p4_pc2_s_sdz[1][3][1]=-1.76203; 
  p0_pc2_m_sdz[0][4][1]=-0.0208049; p1_pc2_m_sdz[0][4][1]=0.0168999; p2_pc2_m_sdz[0][4][1]=-0.00885097; p3_pc2_m_sdz[0][4][1]=0.00150013; p4_pc2_m_sdz[0][4][1]=8.4083e-06; 
  p0_pc2_s_sdz[0][4][1]=0.671843; p1_pc2_s_sdz[0][4][1]=0.346421; p2_pc2_s_sdz[0][4][1]=-0.274584; p3_pc2_s_sdz[0][4][1]=0.0844672; p4_pc2_s_sdz[0][4][1]=-0.00893564; 
  p0_pc2_m_sdz[1][4][1]=-0.0785729; p1_pc2_m_sdz[1][4][1]=0.342518; p2_pc2_m_sdz[1][4][1]=-0.184495; p3_pc2_m_sdz[1][4][1]=-1.27148; p4_pc2_m_sdz[1][4][1]=1.53617; 
  p0_pc2_s_sdz[1][4][1]=0.923451; p1_pc2_s_sdz[1][4][1]=-0.509521; p2_pc2_s_sdz[1][4][1]=0.176527; p3_pc2_s_sdz[1][4][1]=1.66939; p4_pc2_s_sdz[1][4][1]=-1.84078; 
  p0_pc2_m_sdz[0][5][1]=-0.00390153; p1_pc2_m_sdz[0][5][1]=-0.00482811; p2_pc2_m_sdz[0][5][1]=0.0061873; p3_pc2_m_sdz[0][5][1]=-0.0027076; p4_pc2_m_sdz[0][5][1]=0.000367369; 
  p0_pc2_s_sdz[0][5][1]=0.66454; p1_pc2_s_sdz[0][5][1]=0.326167; p2_pc2_s_sdz[0][5][1]=-0.258753; p3_pc2_s_sdz[0][5][1]=0.0804767; p4_pc2_s_sdz[0][5][1]=-0.00858652; 
  p0_pc2_m_sdz[1][5][1]=0.194822; p1_pc2_m_sdz[1][5][1]=-0.644883; p2_pc2_m_sdz[1][5][1]=-0.00618372; p3_pc2_m_sdz[1][5][1]=1.68227; p4_pc2_m_sdz[1][5][1]=-1.33774; 
  p0_pc2_s_sdz[1][5][1]=1.22311; p1_pc2_s_sdz[1][5][1]=-1.7247; p2_pc2_s_sdz[1][5][1]=0.401852; p3_pc2_s_sdz[1][5][1]=4.83275; p4_pc2_s_sdz[1][5][1]=-4.53498; 
  p0_pc2_m_sdz[0][6][1]=-0.0429879; p1_pc2_m_sdz[0][6][1]=0.0536378; p2_pc2_m_sdz[0][6][1]=-0.035172; p3_pc2_m_sdz[0][6][1]=0.0100078; p4_pc2_m_sdz[0][6][1]=-0.00099969; 
  p0_pc2_s_sdz[0][6][1]=0.713797; p1_pc2_s_sdz[0][6][1]=0.231159; p2_pc2_s_sdz[0][6][1]=-0.200281; p3_pc2_s_sdz[0][6][1]=0.0677155; p4_pc2_s_sdz[0][6][1]=-0.00775862; 
  p0_pc2_m_sdz[1][6][1]=-0.571755; p1_pc2_m_sdz[1][6][1]=2.12762; p2_pc2_m_sdz[1][6][1]=-0.517746; p3_pc2_m_sdz[1][6][1]=-5.78588; p4_pc2_m_sdz[1][6][1]=5.46989; 
  p0_pc2_s_sdz[1][6][1]=2.21437; p1_pc2_s_sdz[1][6][1]=-5.89011; p2_pc2_s_sdz[1][6][1]=1.76561; p3_pc2_s_sdz[1][6][1]=16.9338; p4_pc2_s_sdz[1][6][1]=-16.7508; 
  p0_pc2_m_sdz[0][7][1]=-0.0177117; p1_pc2_m_sdz[0][7][1]=-0.0274225; p2_pc2_m_sdz[0][7][1]=0.0253687; p3_pc2_m_sdz[0][7][1]=-0.00748484; p4_pc2_m_sdz[0][7][1]=0.000794801; 
  p0_pc2_s_sdz[0][7][1]=0.676688; p1_pc2_s_sdz[0][7][1]=0.317577; p2_pc2_s_sdz[0][7][1]=-0.254338; p3_pc2_s_sdz[0][7][1]=0.0815587; p4_pc2_s_sdz[0][7][1]=-0.00906848; 
  p0_pc2_m_sdz[1][7][1]=-1.2211; p1_pc2_m_sdz[1][7][1]=4.90708; p2_pc2_m_sdz[1][7][1]=-1.55883; p3_pc2_m_sdz[1][7][1]=-13.8785; p4_pc2_m_sdz[1][7][1]=13.7833; 
  p0_pc2_s_sdz[1][7][1]=1.28974; p1_pc2_s_sdz[1][7][1]=-2.58814; p2_pc2_s_sdz[1][7][1]=1.35222; p3_pc2_s_sdz[1][7][1]=8.98628; p4_pc2_s_sdz[1][7][1]=-10.5001; 
  p0_pc2_m_sdz[0][8][1]=-0.0385311; p1_pc2_m_sdz[0][8][1]=-0.0221598; p2_pc2_m_sdz[0][8][1]=0.0411801; p3_pc2_m_sdz[0][8][1]=-0.0167793; p4_pc2_m_sdz[0][8][1]=0.00212063; 
  p0_pc2_s_sdz[0][8][1]=0.783302; p1_pc2_s_sdz[0][8][1]=0.0891892; p2_pc2_s_sdz[0][8][1]=-0.0853852; p3_pc2_s_sdz[0][8][1]=0.0295197; p4_pc2_s_sdz[0][8][1]=-0.00346562; 
  p0_pc2_m_sdz[1][8][1]=3.81404; p1_pc2_m_sdz[1][8][1]=-15.5631; p2_pc2_m_sdz[1][8][1]=4.48095; p3_pc2_m_sdz[1][8][1]=43.5388; p4_pc2_m_sdz[1][8][1]=-42.527; 
  p0_pc2_s_sdz[1][8][1]=-2.4631; p1_pc2_s_sdz[1][8][1]=13.1591; p2_pc2_s_sdz[1][8][1]=-4.54567; p3_pc2_s_sdz[1][8][1]=-35.3769; p4_pc2_s_sdz[1][8][1]=35.8739; 
  p0_pc2_m_sdz[0][9][1]=-0.043602; p1_pc2_m_sdz[0][9][1]=-0.0738758; p2_pc2_m_sdz[0][9][1]=0.103598; p3_pc2_m_sdz[0][9][1]=-0.0396835; p4_pc2_m_sdz[0][9][1]=0.00478771; 
  p0_pc2_s_sdz[0][9][1]=0.779254; p1_pc2_s_sdz[0][9][1]=0.0971634; p2_pc2_s_sdz[0][9][1]=-0.10126; p3_pc2_s_sdz[0][9][1]=0.0343583; p4_pc2_s_sdz[0][9][1]=-0.00382804; 
  p0_pc2_m_sdz[1][9][1]=-0.795431; p1_pc2_m_sdz[1][9][1]=2.73867; p2_pc2_m_sdz[1][9][1]=-0.411749; p3_pc2_m_sdz[1][9][1]=-7.81239; p4_pc2_m_sdz[1][9][1]=7.23128; 
  p0_pc2_s_sdz[1][9][1]=-4.52693; p1_pc2_s_sdz[1][9][1]=21.3275; p2_pc2_s_sdz[1][9][1]=-6.4406; p3_pc2_s_sdz[1][9][1]=-58.6882; p4_pc2_s_sdz[1][9][1]=57.94;
  
  //! PC3 sdphi neg
  p0_pc3_m_sdp[0][0][0][0]=0.231726; p1_pc3_m_sdp[0][0][0][0]=-0.457591; p2_pc3_m_sdp[0][0][0][0]=0.313793; p3_pc3_m_sdp[0][0][0][0]=-0.0896768; p4_pc3_m_sdp[0][0][0][0]=0.00923036; 
  p0_pc3_s_sdp[0][0][0][0]=0.439371; p1_pc3_s_sdp[0][0][0][0]=0.536491; p2_pc3_s_sdp[0][0][0][0]=-0.325682; p3_pc3_s_sdp[0][0][0][0]=0.0950614; p4_pc3_s_sdp[0][0][0][0]=-0.0102423; 
  p0_pc3_m_sdp[1][0][0][0]=0.265815; p1_pc3_m_sdp[1][0][0][0]=-0.676686; p2_pc3_m_sdp[1][0][0][0]=-0.0290625; p3_pc3_m_sdp[1][0][0][0]=1.56562; p4_pc3_m_sdp[1][0][0][0]=-1.18057; 
  p0_pc3_s_sdp[1][0][0][0]=1.60476; p1_pc3_s_sdp[1][0][0][0]=-3.50652; p2_pc3_s_sdp[1][0][0][0]=0.489585; p3_pc3_s_sdp[1][0][0][0]=9.36263; p4_pc3_s_sdp[1][0][0][0]=-8.0286; 
  p0_pc3_m_sdp[0][0][1][0]=0.146432; p1_pc3_m_sdp[0][0][1][0]=-0.29369; p2_pc3_m_sdp[0][0][1][0]=0.199179; p3_pc3_m_sdp[0][0][1][0]=-0.0562743; p4_pc3_m_sdp[0][0][1][0]=0.00572701; 
  p0_pc3_s_sdp[0][0][1][0]=0.48256; p1_pc3_s_sdp[0][0][1][0]=0.398715; p2_pc3_s_sdp[0][0][1][0]=-0.2058; p3_pc3_s_sdp[0][0][1][0]=0.054963; p4_pc3_s_sdp[0][0][1][0]=-0.00566426; 
  p0_pc3_m_sdp[1][0][1][0]=18.8253; p1_pc3_m_sdp[1][0][1][0]=-146.274; p2_pc3_m_sdp[1][0][1][0]=424.697; p3_pc3_m_sdp[1][0][1][0]=-542.943; p4_pc3_m_sdp[1][0][1][0]=257.364; 
  p0_pc3_s_sdp[1][0][1][0]=1.03827; p1_pc3_s_sdp[1][0][1][0]=-2.11172; p2_pc3_s_sdp[1][0][1][0]=0.959102; p3_pc3_s_sdp[1][0][1][0]=6.67051; p4_pc3_s_sdp[1][0][1][0]=-6.95125; 
  p0_pc3_m_sdp[0][0][2][0]=0.216401; p1_pc3_m_sdp[0][0][2][0]=-0.459638; p2_pc3_m_sdp[0][0][2][0]=0.32702; p3_pc3_m_sdp[0][0][2][0]=-0.0964709; p4_pc3_m_sdp[0][0][2][0]=0.0100809; 
  p0_pc3_s_sdp[0][0][2][0]=0.350034; p1_pc3_s_sdp[0][0][2][0]=0.624217; p2_pc3_s_sdp[0][0][2][0]=-0.352798; p3_pc3_s_sdp[0][0][2][0]=0.0962129; p4_pc3_s_sdp[0][0][2][0]=-0.00977366; 
  p0_pc3_m_sdp[1][0][2][0]=1.47492; p1_pc3_m_sdp[1][0][2][0]=-5.11432; p2_pc3_m_sdp[1][0][2][0]=0.892624; p3_pc3_m_sdp[1][0][2][0]=12.9439; p4_pc3_m_sdp[1][0][2][0]=-11.4304; 
  p0_pc3_s_sdp[1][0][2][0]=1.69241; p1_pc3_s_sdp[1][0][2][0]=-4.12131; p2_pc3_s_sdp[1][0][2][0]=0.863006; p3_pc3_s_sdp[1][0][2][0]=11.1463; p4_pc3_s_sdp[1][0][2][0]=-10.0537; 
  p0_pc3_m_sdp[0][0][3][0]=0.129259; p1_pc3_m_sdp[0][0][3][0]=-0.276776; p2_pc3_m_sdp[0][0][3][0]=0.178375; p3_pc3_m_sdp[0][0][3][0]=-0.0469834; p4_pc3_m_sdp[0][0][3][0]=0.0044517; 
  p0_pc3_s_sdp[0][0][3][0]=0.497351; p1_pc3_s_sdp[0][0][3][0]=0.272127; p2_pc3_s_sdp[0][0][3][0]=-0.0705352; p3_pc3_s_sdp[0][0][3][0]=0.00448369; p4_pc3_s_sdp[0][0][3][0]=0.000479905; 
  p0_pc3_m_sdp[1][0][3][0]=-0.872461; p1_pc3_m_sdp[1][0][3][0]=4.33136; p2_pc3_m_sdp[1][0][3][0]=-1.99876; p3_pc3_m_sdp[1][0][3][0]=-12.6217; p4_pc3_m_sdp[1][0][3][0]=13.3114; 
  p0_pc3_s_sdp[1][0][3][0]=0.255459; p1_pc3_s_sdp[1][0][3][0]=1.17395; p2_pc3_s_sdp[1][0][3][0]=-0.255045; p3_pc3_s_sdp[1][0][3][0]=-2.48727; p4_pc3_s_sdp[1][0][3][0]=2.4019; 
  p0_pc3_m_sdp[0][0][4][0]=0.133947; p1_pc3_m_sdp[0][0][4][0]=-0.292066; p2_pc3_m_sdp[0][0][4][0]=0.185014; p3_pc3_m_sdp[0][0][4][0]=-0.0469035; p4_pc3_m_sdp[0][0][4][0]=0.00419953; 
  p0_pc3_s_sdp[0][0][4][0]=0.475492; p1_pc3_s_sdp[0][0][4][0]=0.333511; p2_pc3_s_sdp[0][0][4][0]=-0.120653; p3_pc3_s_sdp[0][0][4][0]=0.0201178; p4_pc3_s_sdp[0][0][4][0]=-0.00114126; 
  p0_pc3_m_sdp[1][0][4][0]=1.00642; p1_pc3_m_sdp[1][0][4][0]=-3.25724; p2_pc3_m_sdp[1][0][4][0]=0.186768; p3_pc3_m_sdp[1][0][4][0]=8.01018; p4_pc3_m_sdp[1][0][4][0]=-6.48733; 
  p0_pc3_s_sdp[1][0][4][0]=0.362733; p1_pc3_s_sdp[1][0][4][0]=1.06131; p2_pc3_s_sdp[1][0][4][0]=-0.49293; p3_pc3_s_sdp[1][0][4][0]=-2.47162; p4_pc3_s_sdp[1][0][4][0]=2.81544; 
  p0_pc3_m_sdp[0][0][5][0]=0.184435; p1_pc3_m_sdp[0][0][5][0]=-0.430699; p2_pc3_m_sdp[0][0][5][0]=0.321179; p3_pc3_m_sdp[0][0][5][0]=-0.0983874; p4_pc3_m_sdp[0][0][5][0]=0.0105935; 
  p0_pc3_s_sdp[0][0][5][0]=0.516342; p1_pc3_s_sdp[0][0][5][0]=0.2626; p2_pc3_s_sdp[0][0][5][0]=-0.0716327; p3_pc3_s_sdp[0][0][5][0]=0.00717697; p4_pc3_s_sdp[0][0][5][0]=-0.000110137; 
  p0_pc3_m_sdp[1][0][5][0]=-0.175575; p1_pc3_m_sdp[1][0][5][0]=1.23254; p2_pc3_m_sdp[1][0][5][0]=-0.928179; p3_pc3_m_sdp[1][0][5][0]=-3.93233; p4_pc3_m_sdp[1][0][5][0]=4.76694; 
  p0_pc3_s_sdp[1][0][5][0]=0.0134276; p1_pc3_s_sdp[1][0][5][0]=2.69891; p2_pc3_s_sdp[1][0][5][0]=-1.07919; p3_pc3_s_sdp[1][0][5][0]=-7.00701; p4_pc3_s_sdp[1][0][5][0]=7.22373; 
  p0_pc3_m_sdp[0][0][6][0]=0.136238; p1_pc3_m_sdp[0][0][6][0]=-0.328967; p2_pc3_m_sdp[0][0][6][0]=0.248822; p3_pc3_m_sdp[0][0][6][0]=-0.0780534; p4_pc3_m_sdp[0][0][6][0]=0.00866652; 
  p0_pc3_s_sdp[0][0][6][0]=0.393781; p1_pc3_s_sdp[0][0][6][0]=0.54028; p2_pc3_s_sdp[0][0][6][0]=-0.281316; p3_pc3_s_sdp[0][0][6][0]=0.0699776; p4_pc3_s_sdp[0][0][6][0]=-0.0065114; 
  p0_pc3_m_sdp[1][0][6][0]=-0.731461; p1_pc3_m_sdp[1][0][6][0]=3.08102; p2_pc3_m_sdp[1][0][6][0]=-0.911338; p3_pc3_m_sdp[1][0][6][0]=-8.63055; p4_pc3_m_sdp[1][0][6][0]=8.25488; 
  p0_pc3_s_sdp[1][0][6][0]=20.3596; p1_pc3_s_sdp[1][0][6][0]=-152.379; p2_pc3_s_sdp[1][0][6][0]=436.134; p3_pc3_s_sdp[1][0][6][0]=-547.519; p4_pc3_s_sdp[1][0][6][0]=254.6; 
  p0_pc3_m_sdp[0][0][7][0]=0.139842; p1_pc3_m_sdp[0][0][7][0]=-0.34449; p2_pc3_m_sdp[0][0][7][0]=0.274509; p3_pc3_m_sdp[0][0][7][0]=-0.0893394; p4_pc3_m_sdp[0][0][7][0]=0.0101395; 
  p0_pc3_s_sdp[0][0][7][0]=0.277976; p1_pc3_s_sdp[0][0][7][0]=0.723982; p2_pc3_s_sdp[0][0][7][0]=-0.395348; p3_pc3_s_sdp[0][0][7][0]=0.10013; p4_pc3_s_sdp[0][0][7][0]=-0.00931579; 
  p0_pc3_m_sdp[1][0][7][0]=-0.2286; p1_pc3_m_sdp[1][0][7][0]=1.37567; p2_pc3_m_sdp[1][0][7][0]=-1.12858; p3_pc3_m_sdp[1][0][7][0]=-4.15777; p4_pc3_m_sdp[1][0][7][0]=5.38146; 
  p0_pc3_s_sdp[1][0][7][0]=-0.390473; p1_pc3_s_sdp[1][0][7][0]=4.09527; p2_pc3_s_sdp[1][0][7][0]=-1.20934; p3_pc3_s_sdp[1][0][7][0]=-10.6492; p4_pc3_s_sdp[1][0][7][0]=9.95561; 
  p0_pc3_m_sdp[0][0][8][0]=0.093353; p1_pc3_m_sdp[0][0][8][0]=-0.267307; p2_pc3_m_sdp[0][0][8][0]=0.218382; p3_pc3_m_sdp[0][0][8][0]=-0.0701007; p4_pc3_m_sdp[0][0][8][0]=0.0078236; 
  p0_pc3_s_sdp[0][0][8][0]=0.331156; p1_pc3_s_sdp[0][0][8][0]=0.625384; p2_pc3_s_sdp[0][0][8][0]=-0.330347; p3_pc3_s_sdp[0][0][8][0]=0.0824334; p4_pc3_s_sdp[0][0][8][0]=-0.00762308; 
  p0_pc3_m_sdp[1][0][8][0]=-0.0655268; p1_pc3_m_sdp[1][0][8][0]=-0.454858; p2_pc3_m_sdp[1][0][8][0]=0.963778; p3_pc3_m_sdp[1][0][8][0]=1.91219; p4_pc3_m_sdp[1][0][8][0]=-3.23451; 
  p0_pc3_s_sdp[1][0][8][0]=-0.486088; p1_pc3_s_sdp[1][0][8][0]=4.83702; p2_pc3_s_sdp[1][0][8][0]=-2.22411; p3_pc3_s_sdp[1][0][8][0]=-13.3363; p4_pc3_s_sdp[1][0][8][0]=14.4172; 
  p0_pc3_m_sdp[0][0][9][0]=0.112699; p1_pc3_m_sdp[0][0][9][0]=-0.345984; p2_pc3_m_sdp[0][0][9][0]=0.305128; p3_pc3_m_sdp[0][0][9][0]=-0.102311; p4_pc3_m_sdp[0][0][9][0]=0.011702; 
  p0_pc3_s_sdp[0][0][9][0]=0.317117; p1_pc3_s_sdp[0][0][9][0]=0.666064; p2_pc3_s_sdp[0][0][9][0]=-0.361436; p3_pc3_s_sdp[0][0][9][0]=0.0922288; p4_pc3_s_sdp[0][0][9][0]=-0.00878704; 
  p0_pc3_m_sdp[1][0][9][0]=1.35554; p1_pc3_m_sdp[1][0][9][0]=-5.99184; p2_pc3_m_sdp[1][0][9][0]=2.27879; p3_pc3_m_sdp[1][0][9][0]=16.9014; p4_pc3_m_sdp[1][0][9][0]=-17.2285; 
  p0_pc3_s_sdp[1][0][9][0]=-1.16422; p1_pc3_s_sdp[1][0][9][0]=7.56147; p2_pc3_s_sdp[1][0][9][0]=-2.87679; p3_pc3_s_sdp[1][0][9][0]=-20.9229; p4_pc3_s_sdp[1][0][9][0]=21.582; 
  p0_pc3_m_sdp[0][1][0][0]=0.000176691; p1_pc3_m_sdp[0][1][0][0]=0.108392; p2_pc3_m_sdp[0][1][0][0]=-0.116404; p3_pc3_m_sdp[0][1][0][0]=0.0397218; p4_pc3_m_sdp[0][1][0][0]=-0.00424548; 
  p0_pc3_s_sdp[0][1][0][0]=0.686086; p1_pc3_s_sdp[0][1][0][0]=-0.0229758; p2_pc3_s_sdp[0][1][0][0]=0.139973; p3_pc3_s_sdp[0][1][0][0]=-0.0611958; p4_pc3_s_sdp[0][1][0][0]=0.0076945; 
  p0_pc3_m_sdp[1][1][0][0]=-0.176856; p1_pc3_m_sdp[1][1][0][0]=0.543351; p2_pc3_m_sdp[1][1][0][0]=0.0242272; p3_pc3_m_sdp[1][1][0][0]=-1.183; p4_pc3_m_sdp[1][1][0][0]=0.890642; 
  p0_pc3_s_sdp[1][1][0][0]=0.43978; p1_pc3_s_sdp[1][1][0][0]=1.64318; p2_pc3_s_sdp[1][1][0][0]=-1.02564; p3_pc3_s_sdp[1][1][0][0]=-4.89741; p4_pc3_s_sdp[1][1][0][0]=5.61391; 
  p0_pc3_m_sdp[0][1][1][0]=0.0290442; p1_pc3_m_sdp[0][1][1][0]=0.0165452; p2_pc3_m_sdp[0][1][1][0]=-0.0431258; p3_pc3_m_sdp[0][1][1][0]=0.0172031; p4_pc3_m_sdp[0][1][1][0]=-0.00193713; 
  p0_pc3_s_sdp[0][1][1][0]=0.542837; p1_pc3_s_sdp[0][1][1][0]=0.356611; p2_pc3_s_sdp[0][1][1][0]=-0.18054; p3_pc3_s_sdp[0][1][1][0]=0.0442835; p4_pc3_s_sdp[0][1][1][0]=-0.00399166; 
  p0_pc3_m_sdp[1][1][1][0]=7.63968; p1_pc3_m_sdp[1][1][1][0]=-58.7308; p2_pc3_m_sdp[1][1][1][0]=166.753; p3_pc3_m_sdp[1][1][1][0]=-207.455; p4_pc3_m_sdp[1][1][1][0]=95.6034; 
  p0_pc3_s_sdp[1][1][1][0]=0.517497; p1_pc3_s_sdp[1][1][1][0]=1.31364; p2_pc3_s_sdp[1][1][1][0]=-0.993879; p3_pc3_s_sdp[1][1][1][0]=-3.72824; p4_pc3_s_sdp[1][1][1][0]=4.51999; 
  p0_pc3_m_sdp[0][1][2][0]=0.0908392; p1_pc3_m_sdp[0][1][2][0]=-0.151987; p2_pc3_m_sdp[0][1][2][0]=0.0941416; p3_pc3_m_sdp[0][1][2][0]=-0.0266528; p4_pc3_m_sdp[0][1][2][0]=0.00283032; 
  p0_pc3_s_sdp[0][1][2][0]=0.599869; p1_pc3_s_sdp[0][1][2][0]=0.206103; p2_pc3_s_sdp[0][1][2][0]=-0.0384725; p3_pc3_s_sdp[0][1][2][0]=-0.00570557; p4_pc3_s_sdp[0][1][2][0]=0.00168525; 
  p0_pc3_m_sdp[1][1][2][0]=0.69975; p1_pc3_m_sdp[1][1][2][0]=-2.99358; p2_pc3_m_sdp[1][1][2][0]=1.03498; p3_pc3_m_sdp[1][1][2][0]=8.57384; p4_pc3_m_sdp[1][1][2][0]=-8.59397; 
  p0_pc3_s_sdp[1][1][2][0]=-0.267856; p1_pc3_s_sdp[1][1][2][0]=4.00418; p2_pc3_s_sdp[1][1][2][0]=-1.20114; p3_pc3_s_sdp[1][1][2][0]=-10.7533; p4_pc3_s_sdp[1][1][2][0]=10.3955; 
  p0_pc3_m_sdp[0][1][3][0]=0.0416794; p1_pc3_m_sdp[0][1][3][0]=-0.101313; p2_pc3_m_sdp[0][1][3][0]=0.076012; p3_pc3_m_sdp[0][1][3][0]=-0.0238509; p4_pc3_m_sdp[0][1][3][0]=0.00261422; 
  p0_pc3_s_sdp[0][1][3][0]=0.61794; p1_pc3_s_sdp[0][1][3][0]=0.265269; p2_pc3_s_sdp[0][1][3][0]=-0.178064; p3_pc3_s_sdp[0][1][3][0]=0.0660978; p4_pc3_s_sdp[0][1][3][0]=-0.00927488; 
  p0_pc3_m_sdp[1][1][3][0]=-0.406528; p1_pc3_m_sdp[1][1][3][0]=1.23765; p2_pc3_m_sdp[1][1][3][0]=-0.0478917; p3_pc3_m_sdp[1][1][3][0]=-2.73827; p4_pc3_m_sdp[1][1][3][0]=2.06779; 
  p0_pc3_s_sdp[1][1][3][0]=1.58474; p1_pc3_s_sdp[1][1][3][0]=-3.52779; p2_pc3_s_sdp[1][1][3][0]=0.993349; p3_pc3_s_sdp[1][1][3][0]=10.1019; p4_pc3_s_sdp[1][1][3][0]=-9.78816; 
  p0_pc3_m_sdp[0][1][4][0]=0.0465087; p1_pc3_m_sdp[0][1][4][0]=-0.115158; p2_pc3_m_sdp[0][1][4][0]=0.0852217; p3_pc3_m_sdp[0][1][4][0]=-0.0260971; p4_pc3_m_sdp[0][1][4][0]=0.00281874; 
  p0_pc3_s_sdp[0][1][4][0]=0.534871; p1_pc3_s_sdp[0][1][4][0]=0.469211; p2_pc3_s_sdp[0][1][4][0]=-0.32796; p3_pc3_s_sdp[0][1][4][0]=0.103941; p4_pc3_s_sdp[0][1][4][0]=-0.0115402; 
  p0_pc3_m_sdp[1][1][4][0]=-0.36595; p1_pc3_m_sdp[1][1][4][0]=0.979933; p2_pc3_m_sdp[1][1][4][0]=0.147817; p3_pc3_m_sdp[1][1][4][0]=-1.91856; p4_pc3_m_sdp[1][1][4][0]=1.08335; 
  p0_pc3_s_sdp[1][1][4][0]=0.653681; p1_pc3_s_sdp[1][1][4][0]=0.544295; p2_pc3_s_sdp[1][1][4][0]=-0.55373; p3_pc3_s_sdp[1][1][4][0]=-1.63295; p4_pc3_s_sdp[1][1][4][0]=2.3436; 
  p0_pc3_m_sdp[0][1][5][0]=-0.0176162; p1_pc3_m_sdp[0][1][5][0]=0.0116773; p2_pc3_m_sdp[0][1][5][0]=0.0014151; p3_pc3_m_sdp[0][1][5][0]=-0.00336308; p4_pc3_m_sdp[0][1][5][0]=0.000707917; 
  p0_pc3_s_sdp[0][1][5][0]=0.617101; p1_pc3_s_sdp[0][1][5][0]=0.287736; p2_pc3_s_sdp[0][1][5][0]=-0.184754; p3_pc3_s_sdp[0][1][5][0]=0.0571156; p4_pc3_s_sdp[0][1][5][0]=-0.00634409; 
  p0_pc3_m_sdp[1][1][5][0]=0.329688; p1_pc3_m_sdp[1][1][5][0]=-1.56755; p2_pc3_m_sdp[1][1][5][0]=0.647224; p3_pc3_m_sdp[1][1][5][0]=4.5393; p4_pc3_m_sdp[1][1][5][0]=-4.68061; 
  p0_pc3_s_sdp[1][1][5][0]=0.742496; p1_pc3_s_sdp[1][1][5][0]=0.203346; p2_pc3_s_sdp[1][1][5][0]=-0.430707; p3_pc3_s_sdp[1][1][5][0]=-0.545751; p4_pc3_s_sdp[1][1][5][0]=1.15361; 
  p0_pc3_m_sdp[0][1][6][0]=-0.0491118; p1_pc3_m_sdp[0][1][6][0]=0.0817662; p2_pc3_m_sdp[0][1][6][0]=-0.0495569; p3_pc3_m_sdp[0][1][6][0]=0.0114016; p4_pc3_m_sdp[0][1][6][0]=-0.000777648; 
  p0_pc3_s_sdp[0][1][6][0]=0.591987; p1_pc3_s_sdp[0][1][6][0]=0.279407; p2_pc3_s_sdp[0][1][6][0]=-0.154815; p3_pc3_s_sdp[0][1][6][0]=0.0415833; p4_pc3_s_sdp[0][1][6][0]=-0.00412279; 
  p0_pc3_m_sdp[1][1][6][0]=-2.12769; p1_pc3_m_sdp[1][1][6][0]=8.21256; p2_pc3_m_sdp[1][1][6][0]=-2.04708; p3_pc3_m_sdp[1][1][6][0]=-22.341; p4_pc3_m_sdp[1][1][6][0]=21.18; 
  p0_pc3_s_sdp[1][1][6][0]=0.784307; p1_pc3_s_sdp[1][1][6][0]=0.143929; p2_pc3_s_sdp[1][1][6][0]=-0.578488; p3_pc3_s_sdp[1][1][6][0]=-0.638536; p4_pc3_s_sdp[1][1][6][0]=1.52835; 
  p0_pc3_m_sdp[0][1][7][0]=-0.0135508; p1_pc3_m_sdp[0][1][7][0]=0.000477301; p2_pc3_m_sdp[0][1][7][0]=0.00912169; p3_pc3_m_sdp[0][1][7][0]=-0.00558125; p4_pc3_m_sdp[0][1][7][0]=0.000920024; 
  p0_pc3_s_sdp[0][1][7][0]=0.527136; p1_pc3_s_sdp[0][1][7][0]=0.40239; p2_pc3_s_sdp[0][1][7][0]=-0.2434; p3_pc3_s_sdp[0][1][7][0]=0.0683853; p4_pc3_s_sdp[0][1][7][0]=-0.00699167; 
  p0_pc3_m_sdp[1][1][7][0]=0.326499; p1_pc3_m_sdp[1][1][7][0]=-1.47805; p2_pc3_m_sdp[1][1][7][0]=0.463474; p3_pc3_m_sdp[1][1][7][0]=4.08611; p4_pc3_m_sdp[1][1][7][0]=-3.90811; 
  p0_pc3_s_sdp[1][1][7][0]=-0.385885; p1_pc3_s_sdp[1][1][7][0]=4.48035; p2_pc3_s_sdp[1][1][7][0]=-1.5765; p3_pc3_s_sdp[1][1][7][0]=-11.8054; p4_pc3_s_sdp[1][1][7][0]=11.7646; 
  p0_pc3_m_sdp[0][1][8][0]=-0.0384191; p1_pc3_m_sdp[0][1][8][0]=0.0948101; p2_pc3_m_sdp[0][1][8][0]=-0.0773251; p3_pc3_m_sdp[0][1][8][0]=0.0256421; p4_pc3_m_sdp[0][1][8][0]=-0.002985; 
  p0_pc3_s_sdp[0][1][8][0]=0.578033; p1_pc3_s_sdp[0][1][8][0]=0.315199; p2_pc3_s_sdp[0][1][8][0]=-0.209202; p3_pc3_s_sdp[0][1][8][0]=0.0723605; p4_pc3_s_sdp[0][1][8][0]=-0.0097221; 
  p0_pc3_m_sdp[1][1][8][0]=-0.0313788; p1_pc3_m_sdp[1][1][8][0]=-0.0191615; p2_pc3_m_sdp[1][1][8][0]=0.00576092; p3_pc3_m_sdp[1][1][8][0]=0.0421712; p4_pc3_m_sdp[1][1][8][0]=0.0821485; 
  p0_pc3_s_sdp[1][1][8][0]=-0.885807; p1_pc3_s_sdp[1][1][8][0]=6.45708; p2_pc3_s_sdp[1][1][8][0]=-2.09303; p3_pc3_s_sdp[1][1][8][0]=-17.514; p4_pc3_s_sdp[1][1][8][0]=17.3112; 
  p0_pc3_m_sdp[0][1][9][0]=-0.0397344; p1_pc3_m_sdp[0][1][9][0]=0.0628647; p2_pc3_m_sdp[0][1][9][0]=-0.0187616; p3_pc3_m_sdp[0][1][9][0]=-0.00156497; p4_pc3_m_sdp[0][1][9][0]=0.000824303; 
  p0_pc3_s_sdp[0][1][9][0]=0.484851; p1_pc3_s_sdp[0][1][9][0]=0.48764; p2_pc3_s_sdp[0][1][9][0]=-0.299168; p3_pc3_s_sdp[0][1][9][0]=0.0830566; p4_pc3_s_sdp[0][1][9][0]=-0.00836095; 
  p0_pc3_m_sdp[1][1][9][0]=-0.329035; p1_pc3_m_sdp[1][1][9][0]=1.13595; p2_pc3_m_sdp[1][1][9][0]=-0.35272; p3_pc3_m_sdp[1][1][9][0]=-2.89318; p4_pc3_m_sdp[1][1][9][0]=2.89824; 
  p0_pc3_s_sdp[1][1][9][0]=1.71962; p1_pc3_s_sdp[1][1][9][0]=-4.02954; p2_pc3_s_sdp[1][1][9][0]=1.02028; p3_pc3_s_sdp[1][1][9][0]=11.2221; p4_pc3_s_sdp[1][1][9][0]=-10.7303; 
  
  //! PC3 sdphi pos
  p0_pc3_m_sdp[0][0][0][1]=0.199025; p1_pc3_m_sdp[0][0][0][1]=-0.380357; p2_pc3_m_sdp[0][0][0][1]=0.26334; p3_pc3_m_sdp[0][0][0][1]=-0.075124; p4_pc3_m_sdp[0][0][0][1]=0.00738524; 
  p0_pc3_s_sdp[0][0][0][1]=0.509312; p1_pc3_s_sdp[0][0][0][1]=0.545125; p2_pc3_s_sdp[0][0][0][1]=-0.421102; p3_pc3_s_sdp[0][0][0][1]=0.135774; p4_pc3_s_sdp[0][0][0][1]=-0.0143592; 
  p0_pc3_m_sdp[1][0][0][1]=0.631889; p1_pc3_m_sdp[1][0][0][1]=-2.14732; p2_pc3_m_sdp[1][0][0][1]=0.485278; p3_pc3_m_sdp[1][0][0][1]=5.71543; p4_pc3_m_sdp[1][0][0][1]=-5.39239; 
  p0_pc3_s_sdp[1][0][0][1]=-3.25337; p1_pc3_s_sdp[1][0][0][1]=30.0571; p2_pc3_s_sdp[1][0][0][1]=-85.4151; p3_pc3_s_sdp[1][0][0][1]=106.071; p4_pc3_s_sdp[1][0][0][1]=-48.3174; 
  p0_pc3_m_sdp[0][0][1][1]=0.244726; p1_pc3_m_sdp[0][0][1][1]=-0.462507; p2_pc3_m_sdp[0][0][1][1]=0.326517; p3_pc3_m_sdp[0][0][1][1]=-0.0961674; p4_pc3_m_sdp[0][0][1][1]=0.00989565; 
  p0_pc3_s_sdp[0][0][1][1]=0.475959; p1_pc3_s_sdp[0][0][1][1]=0.608737; p2_pc3_s_sdp[0][0][1][1]=-0.460313; p3_pc3_s_sdp[0][0][1][1]=0.145135; p4_pc3_s_sdp[0][0][1][1]=-0.0150317; 
  p0_pc3_m_sdp[1][0][1][1]=0.365157; p1_pc3_m_sdp[1][0][1][1]=-0.625002; p2_pc3_m_sdp[1][0][1][1]=-0.39714; p3_pc3_m_sdp[1][0][1][1]=0.873408; p4_pc3_m_sdp[1][0][1][1]=0.173537; 
  p0_pc3_s_sdp[1][0][1][1]=1.47607; p1_pc3_s_sdp[1][0][1][1]=-3.80732; p2_pc3_s_sdp[1][0][1][1]=1.60676; p3_pc3_s_sdp[1][0][1][1]=11.9146; p4_pc3_s_sdp[1][0][1][1]=-12.5588; 
  p0_pc3_m_sdp[0][0][2][1]=0.0670866; p1_pc3_m_sdp[0][0][2][1]=-0.0636705; p2_pc3_m_sdp[0][0][2][1]=0.0276891; p3_pc3_m_sdp[0][0][2][1]=-0.00592342; p4_pc3_m_sdp[0][0][2][1]=0.0004904; 
  p0_pc3_s_sdp[0][0][2][1]=0.485388; p1_pc3_s_sdp[0][0][2][1]=0.557366; p2_pc3_s_sdp[0][0][2][1]=-0.407881; p3_pc3_s_sdp[0][0][2][1]=0.124644; p4_pc3_s_sdp[0][0][2][1]=-0.0123263; 
  p0_pc3_m_sdp[1][0][2][1]=0.645667; p1_pc3_m_sdp[1][0][2][1]=-1.80036; p2_pc3_m_sdp[1][0][2][1]=0.113859; p3_pc3_m_sdp[1][0][2][1]=4.54414; p4_pc3_m_sdp[1][0][2][1]=-4.02335; 
  p0_pc3_s_sdp[1][0][2][1]=0.072064; p1_pc3_s_sdp[1][0][2][1]=2.45699; p2_pc3_s_sdp[1][0][2][1]=-0.905394; p3_pc3_s_sdp[1][0][2][1]=-6.84469; p4_pc3_s_sdp[1][0][2][1]=7.19739; 
  p0_pc3_m_sdp[0][0][3][1]=0.0677027; p1_pc3_m_sdp[0][0][3][1]=-0.063977; p2_pc3_m_sdp[0][0][3][1]=0.020086; p3_pc3_m_sdp[0][0][3][1]=-0.00148861; p4_pc3_m_sdp[0][0][3][1]=-0.000126729; 
  p0_pc3_s_sdp[0][0][3][1]=0.541517; p1_pc3_s_sdp[0][0][3][1]=0.46864; p2_pc3_s_sdp[0][0][3][1]=-0.381663; p3_pc3_s_sdp[0][0][3][1]=0.127416; p4_pc3_s_sdp[0][0][3][1]=-0.0135011; 
  p0_pc3_m_sdp[1][0][3][1]=0.757567; p1_pc3_m_sdp[1][0][3][1]=-2.25554; p2_pc3_m_sdp[1][0][3][1]=0.271709; p3_pc3_m_sdp[1][0][3][1]=5.74395; p4_pc3_m_sdp[1][0][3][1]=-5.26229; 
  p0_pc3_s_sdp[1][0][3][1]=-0.186203; p1_pc3_s_sdp[1][0][3][1]=3.46563; p2_pc3_s_sdp[1][0][3][1]=-1.20311; p3_pc3_s_sdp[1][0][3][1]=-9.59858; p4_pc3_s_sdp[1][0][3][1]=9.98451; 
  p0_pc3_m_sdp[0][0][4][1]=0.0781374; p1_pc3_m_sdp[0][0][4][1]=-0.0869952; p2_pc3_m_sdp[0][0][4][1]=0.0359525; p3_pc3_m_sdp[0][0][4][1]=-0.00680785; p4_pc3_m_sdp[0][0][4][1]=0.000538309; 
  p0_pc3_s_sdp[0][0][4][1]=0.627686; p1_pc3_s_sdp[0][0][4][1]=0.252565; p2_pc3_s_sdp[0][0][4][1]=-0.199166; p3_pc3_s_sdp[0][0][4][1]=0.0648613; p4_pc3_s_sdp[0][0][4][1]=-0.00615776; 
  p0_pc3_m_sdp[1][0][4][1]=1.05104; p1_pc3_m_sdp[1][0][4][1]=-3.23848; p2_pc3_m_sdp[1][0][4][1]=0.37969; p3_pc3_m_sdp[1][0][4][1]=7.96917; p4_pc3_m_sdp[1][0][4][1]=-7.04019; 
  p0_pc3_s_sdp[1][0][4][1]=-0.126671; p1_pc3_s_sdp[1][0][4][1]=3.16114; p2_pc3_s_sdp[1][0][4][1]=-0.963941; p3_pc3_s_sdp[1][0][4][1]=-8.32203; p4_pc3_s_sdp[1][0][4][1]=8.28997; 
  p0_pc3_m_sdp[0][0][5][1]=0.0999138; p1_pc3_m_sdp[0][0][5][1]=-0.111565; p2_pc3_m_sdp[0][0][5][1]=0.0474945; p3_pc3_m_sdp[0][0][5][1]=-0.0101673; p4_pc3_m_sdp[0][0][5][1]=0.00100616; 
  p0_pc3_s_sdp[0][0][5][1]=0.590629; p1_pc3_s_sdp[0][0][5][1]=0.351802; p2_pc3_s_sdp[0][0][5][1]=-0.278514; p3_pc3_s_sdp[0][0][5][1]=0.0901436; p4_pc3_s_sdp[0][0][5][1]=-0.00890128; 
  p0_pc3_m_sdp[1][0][5][1]=1.22697; p1_pc3_m_sdp[1][0][5][1]=-3.96033; p2_pc3_m_sdp[1][0][5][1]=0.540051; p3_pc3_m_sdp[1][0][5][1]=10.1652; p4_pc3_m_sdp[1][0][5][1]=-9.04935; 
  p0_pc3_s_sdp[1][0][5][1]=-8.82045; p1_pc3_s_sdp[1][0][5][1]=72.6502; p2_pc3_s_sdp[1][0][5][1]=-206.485; p3_pc3_s_sdp[1][0][5][1]=258.181; p4_pc3_s_sdp[1][0][5][1]=-119.689; 
  p0_pc3_m_sdp[0][0][6][1]=0.0119101; p1_pc3_m_sdp[0][0][6][1]=0.0545593; p2_pc3_m_sdp[0][0][6][1]=-0.0637936; p3_pc3_m_sdp[0][0][6][1]=0.0215634; p4_pc3_m_sdp[0][0][6][1]=-0.00223201; 
  p0_pc3_s_sdp[0][0][6][1]=0.468849; p1_pc3_s_sdp[0][0][6][1]=0.580188; p2_pc3_s_sdp[0][0][6][1]=-0.426222; p3_pc3_s_sdp[0][0][6][1]=0.130804; p4_pc3_s_sdp[0][0][6][1]=-0.0129687; 
  p0_pc3_m_sdp[1][0][6][1]=0.332642; p1_pc3_m_sdp[1][0][6][1]=-0.380056; p2_pc3_m_sdp[1][0][6][1]=-0.464305; p3_pc3_m_sdp[1][0][6][1]=0.202209; p4_pc3_m_sdp[1][0][6][1]=0.385672; 
  p0_pc3_s_sdp[1][0][6][1]=2.14329; p1_pc3_s_sdp[1][0][6][1]=-6.41536; p2_pc3_s_sdp[1][0][6][1]=2.33638; p3_pc3_s_sdp[1][0][6][1]=19.199; p4_pc3_s_sdp[1][0][6][1]=-19.7556; 
  p0_pc3_m_sdp[0][0][7][1]=0.0876748; p1_pc3_m_sdp[0][0][7][1]=-0.157394; p2_pc3_m_sdp[0][0][7][1]=0.103623; p3_pc3_m_sdp[0][0][7][1]=-0.0299311; p4_pc3_m_sdp[0][0][7][1]=0.00323738; 
  p0_pc3_s_sdp[0][0][7][1]=0.370609; p1_pc3_s_sdp[0][0][7][1]=0.806614; p2_pc3_s_sdp[0][0][7][1]=-0.611058; p3_pc3_s_sdp[0][0][7][1]=0.192803; p4_pc3_s_sdp[0][0][7][1]=-0.0202067; 
  p0_pc3_m_sdp[1][0][7][1]=-0.0528254; p1_pc3_m_sdp[1][0][7][1]=1.03855; p2_pc3_m_sdp[1][0][7][1]=-0.691917; p3_pc3_m_sdp[1][0][7][1]=-3.65197; p4_pc3_m_sdp[1][0][7][1]=3.82647; 
  p0_pc3_s_sdp[1][0][7][1]=35.0213; p1_pc3_s_sdp[1][0][7][1]=-258.913; p2_pc3_s_sdp[1][0][7][1]=722.721; p3_pc3_s_sdp[1][0][7][1]=-886.628; p4_pc3_s_sdp[1][0][7][1]=403.976; 
  p0_pc3_m_sdp[0][0][8][1]=0.0868495; p1_pc3_m_sdp[0][0][8][1]=-0.190553; p2_pc3_m_sdp[0][0][8][1]=0.147159; p3_pc3_m_sdp[0][0][8][1]=-0.0464541; p4_pc3_m_sdp[0][0][8][1]=0.00509056; 
  p0_pc3_s_sdp[0][0][8][1]=0.47822; p1_pc3_s_sdp[0][0][8][1]=0.565454; p2_pc3_s_sdp[0][0][8][1]=-0.454154; p3_pc3_s_sdp[0][0][8][1]=0.16454; p4_pc3_s_sdp[0][0][8][1]=-0.0211485; 
  p0_pc3_m_sdp[1][0][8][1]=-32.6829; p1_pc3_m_sdp[1][0][8][1]=246.402; p2_pc3_m_sdp[1][0][8][1]=-686.086; p3_pc3_m_sdp[1][0][8][1]=838.869; p4_pc3_m_sdp[1][0][8][1]=-380.728; 
  p0_pc3_s_sdp[1][0][8][1]=-0.873787; p1_pc3_s_sdp[1][0][8][1]=5.66396; p2_pc3_s_sdp[1][0][8][1]=-1.14624; p3_pc3_s_sdp[1][0][8][1]=-15.2967; p4_pc3_s_sdp[1][0][8][1]=14.4269; 
  p0_pc3_m_sdp[0][0][9][1]=0.125263; p1_pc3_m_sdp[0][0][9][1]=-0.317805; p2_pc3_m_sdp[0][0][9][1]=0.247327; p3_pc3_m_sdp[0][0][9][1]=-0.076854; p4_pc3_m_sdp[0][0][9][1]=0.00828489; 
  p0_pc3_s_sdp[0][0][9][1]=0.503573; p1_pc3_s_sdp[0][0][9][1]=0.336923; p2_pc3_s_sdp[0][0][9][1]=-0.162649; p3_pc3_s_sdp[0][0][9][1]=0.0357007; p4_pc3_s_sdp[0][0][9][1]=-0.00224682; 
  p0_pc3_m_sdp[1][0][9][1]=1.45873; p1_pc3_m_sdp[1][0][9][1]=-5.7221; p2_pc3_m_sdp[1][0][9][1]=1.75805; p3_pc3_m_sdp[1][0][9][1]=15.7636; p4_pc3_m_sdp[1][0][9][1]=-15.6872; 
  p0_pc3_s_sdp[1][0][9][1]=1.57325; p1_pc3_s_sdp[1][0][9][1]=-3.55249; p2_pc3_s_sdp[1][0][9][1]=0.871023; p3_pc3_s_sdp[1][0][9][1]=9.70868; p4_pc3_s_sdp[1][0][9][1]=-9.21607; 
  p0_pc3_m_sdp[0][1][0][1]=0.105256; p1_pc3_m_sdp[0][1][0][1]=-0.069967; p2_pc3_m_sdp[0][1][0][1]=0.00224411; p3_pc3_m_sdp[0][1][0][1]=0.0073632; p4_pc3_m_sdp[0][1][0][1]=-0.00149777; 
  p0_pc3_s_sdp[0][1][0][1]=0.388276; p1_pc3_s_sdp[0][1][0][1]=0.71527; p2_pc3_s_sdp[0][1][0][1]=-0.447501; p3_pc3_s_sdp[0][1][0][1]=0.119658; p4_pc3_s_sdp[0][1][0][1]=-0.0106064; 
  p0_pc3_m_sdp[1][1][0][1]=0.749697; p1_pc3_m_sdp[1][1][0][1]=-1.97996; p2_pc3_m_sdp[1][1][0][1]=-0.0272216; p3_pc3_m_sdp[1][1][0][1]=4.39701; p4_pc3_m_sdp[1][1][0][1]=-3.34326; 
  p0_pc3_s_sdp[1][1][0][1]=0.340699; p1_pc3_s_sdp[1][1][0][1]=1.11272; p2_pc3_s_sdp[1][1][0][1]=-0.182847; p3_pc3_s_sdp[1][1][0][1]=-2.41359; p4_pc3_s_sdp[1][1][0][1]=2.11874; 
  p0_pc3_m_sdp[0][1][1][1]=0.110776; p1_pc3_m_sdp[0][1][1][1]=-0.0896293; p2_pc3_m_sdp[0][1][1][1]=0.0154814; p3_pc3_m_sdp[0][1][1][1]=0.00567542; p4_pc3_m_sdp[0][1][1][1]=-0.00162658; 
  p0_pc3_s_sdp[0][1][1][1]=0.310458; p1_pc3_s_sdp[0][1][1][1]=0.878721; p2_pc3_s_sdp[0][1][1][1]=-0.578954; p3_pc3_s_sdp[0][1][1][1]=0.16249; p4_pc3_s_sdp[0][1][1][1]=-0.0152826; 
  p0_pc3_m_sdp[1][1][1][1]=13.6692; p1_pc3_m_sdp[1][1][1][1]=-102.068; p2_pc3_m_sdp[1][1][1][1]=288.971; p3_pc3_m_sdp[1][1][1][1]=-363.883; p4_pc3_m_sdp[1][1][1][1]=171.277; 
  p0_pc3_s_sdp[1][1][1][1]=0.16986; p1_pc3_s_sdp[1][1][1][1]=1.88947; p2_pc3_s_sdp[1][1][1][1]=-0.639978; p3_pc3_s_sdp[1][1][1][1]=-4.9325; p4_pc3_s_sdp[1][1][1][1]=5.08888; 
  p0_pc3_m_sdp[0][1][2][1]=0.0353244; p1_pc3_m_sdp[0][1][2][1]=0.0912623; p2_pc3_m_sdp[0][1][2][1]=-0.115991; p3_pc3_m_sdp[0][1][2][1]=0.0430962; p4_pc3_m_sdp[0][1][2][1]=-0.00522875; 
  p0_pc3_s_sdp[0][1][2][1]=0.201103; p1_pc3_s_sdp[0][1][2][1]=1.09103; p2_pc3_s_sdp[0][1][2][1]=-0.728507; p3_pc3_s_sdp[0][1][2][1]=0.205175; p4_pc3_s_sdp[0][1][2][1]=-0.0194081; 
  p0_pc3_m_sdp[1][1][2][1]=1.17085; p1_pc3_m_sdp[1][1][2][1]=-3.40332; p2_pc3_m_sdp[1][1][2][1]=0.13354; p3_pc3_m_sdp[1][1][2][1]=7.83859; p4_pc3_m_sdp[1][1][2][1]=-6.23236; 
  p0_pc3_s_sdp[1][1][2][1]=0.822936; p1_pc3_s_sdp[1][1][2][1]=-1.03346; p2_pc3_s_sdp[1][1][2][1]=0.410288; p3_pc3_s_sdp[1][1][2][1]=3.35848; p4_pc3_s_sdp[1][1][2][1]=-3.26748; 
  p0_pc3_m_sdp[0][1][3][1]=-0.00184129; p1_pc3_m_sdp[0][1][3][1]=0.119881; p2_pc3_m_sdp[0][1][3][1]=-0.111989; p3_pc3_m_sdp[0][1][3][1]=0.0365206; p4_pc3_m_sdp[0][1][3][1]=-0.00404042; 
  p0_pc3_s_sdp[0][1][3][1]=0.348614; p1_pc3_s_sdp[0][1][3][1]=0.737121; p2_pc3_s_sdp[0][1][3][1]=-0.465122; p3_pc3_s_sdp[0][1][3][1]=0.126198; p4_pc3_s_sdp[0][1][3][1]=-0.011322; 
  p0_pc3_m_sdp[1][1][3][1]=1.50587; p1_pc3_m_sdp[1][1][3][1]=-5.30448; p2_pc3_m_sdp[1][1][3][1]=1.0822; p3_pc3_m_sdp[1][1][3][1]=13.619; p4_pc3_m_sdp[1][1][3][1]=-12.4422; 
  p0_pc3_s_sdp[1][1][3][1]=0.282456; p1_pc3_s_sdp[1][1][3][1]=1.55668; p2_pc3_s_sdp[1][1][3][1]=-0.768706; p3_pc3_s_sdp[1][1][3][1]=-3.92155; p4_pc3_s_sdp[1][1][3][1]=4.40171; 
  p0_pc3_m_sdp[0][1][4][1]=0.106108; p1_pc3_m_sdp[0][1][4][1]=-0.103789; p2_pc3_m_sdp[0][1][4][1]=0.0452606; p3_pc3_m_sdp[0][1][4][1]=-0.00936526; p4_pc3_m_sdp[0][1][4][1]=0.000606148; 
  p0_pc3_s_sdp[0][1][4][1]=0.297889; p1_pc3_s_sdp[0][1][4][1]=0.91489; p2_pc3_s_sdp[0][1][4][1]=-0.627246; p3_pc3_s_sdp[0][1][4][1]=0.179756; p4_pc3_s_sdp[0][1][4][1]=-0.0170274; 
  p0_pc3_m_sdp[1][1][4][1]=1.87949; p1_pc3_m_sdp[1][1][4][1]=-6.73299; p2_pc3_m_sdp[1][1][4][1]=1.51143; p3_pc3_m_sdp[1][1][4][1]=17.5407; p4_pc3_m_sdp[1][1][4][1]=-16.3012; 
  p0_pc3_s_sdp[1][1][4][1]=0.520588; p1_pc3_s_sdp[1][1][4][1]=0.272834; p2_pc3_s_sdp[1][1][4][1]=-0.0639209; p3_pc3_s_sdp[1][1][4][1]=-0.223368; p4_pc3_s_sdp[1][1][4][1]=0.392151; 
  p0_pc3_m_sdp[0][1][5][1]=-0.0190644; p1_pc3_m_sdp[0][1][5][1]=0.147025; p2_pc3_m_sdp[0][1][5][1]=-0.126466; p3_pc3_m_sdp[0][1][5][1]=0.038769; p4_pc3_m_sdp[0][1][5][1]=-0.00407869; 
  p0_pc3_s_sdp[0][1][5][1]=0.269391; p1_pc3_s_sdp[0][1][5][1]=1.0428; p2_pc3_s_sdp[0][1][5][1]=-0.756382; p3_pc3_s_sdp[0][1][5][1]=0.223475; p4_pc3_s_sdp[0][1][5][1]=-0.0218042; 
  p0_pc3_m_sdp[1][1][5][1]=1.49225; p1_pc3_m_sdp[1][1][5][1]=-5.17978; p2_pc3_m_sdp[1][1][5][1]=0.872499; p3_pc3_m_sdp[1][1][5][1]=13.4855; p4_pc3_m_sdp[1][1][5][1]=-12.1717; 
  p0_pc3_s_sdp[1][1][5][1]=1.32247; p1_pc3_s_sdp[1][1][5][1]=-3.02657; p2_pc3_s_sdp[1][1][5][1]=1.19594; p3_pc3_s_sdp[1][1][5][1]=8.97932; p4_pc3_s_sdp[1][1][5][1]=-9.1314; 
  p0_pc3_m_sdp[0][1][6][1]=-0.0385047; p1_pc3_m_sdp[0][1][6][1]=0.164836; p2_pc3_m_sdp[0][1][6][1]=-0.11902; p3_pc3_m_sdp[0][1][6][1]=0.0300367; p4_pc3_m_sdp[0][1][6][1]=-0.00248346; 
  p0_pc3_s_sdp[0][1][6][1]=0.342145; p1_pc3_s_sdp[0][1][6][1]=0.83837; p2_pc3_s_sdp[0][1][6][1]=-0.577606; p3_pc3_s_sdp[0][1][6][1]=0.161176; p4_pc3_s_sdp[0][1][6][1]=-0.0145896; 
  p0_pc3_m_sdp[1][1][6][1]=1.39206; p1_pc3_m_sdp[1][1][6][1]=-4.99741; p2_pc3_m_sdp[1][1][6][1]=1.09864; p3_pc3_m_sdp[1][1][6][1]=12.576; p4_pc3_m_sdp[1][1][6][1]=-11.4107; 
  p0_pc3_s_sdp[1][1][6][1]=17.7986; p1_pc3_s_sdp[1][1][6][1]=-130.679; p2_pc3_s_sdp[1][1][6][1]=370.021; p3_pc3_s_sdp[1][1][6][1]=-461.129; p4_pc3_s_sdp[1][1][6][1]=213.735; 
  p0_pc3_m_sdp[0][1][7][1]=0.0197639; p1_pc3_m_sdp[0][1][7][1]=0.0372423; p2_pc3_m_sdp[0][1][7][1]=-0.0451351; p3_pc3_m_sdp[0][1][7][1]=0.0154077; p4_pc3_m_sdp[0][1][7][1]=-0.00172766; 
  p0_pc3_s_sdp[0][1][7][1]=0.302896; p1_pc3_s_sdp[0][1][7][1]=0.96293; p2_pc3_s_sdp[0][1][7][1]=-0.70773; p3_pc3_s_sdp[0][1][7][1]=0.210832; p4_pc3_s_sdp[0][1][7][1]=-0.0206378; 
  p0_pc3_m_sdp[1][1][7][1]=1.17831; p1_pc3_m_sdp[1][1][7][1]=-4.30915; p2_pc3_m_sdp[1][1][7][1]=0.883805; p3_pc3_m_sdp[1][1][7][1]=11.1621; p4_pc3_m_sdp[1][1][7][1]=-10.078; 
  p0_pc3_s_sdp[1][1][7][1]=0.198879; p1_pc3_s_sdp[1][1][7][1]=1.44135; p2_pc3_s_sdp[1][1][7][1]=-0.279233; p3_pc3_s_sdp[1][1][7][1]=-3.24241; p4_pc3_s_sdp[1][1][7][1]=3.16425; 
  p0_pc3_m_sdp[0][1][8][1]=-0.0266888; p1_pc3_m_sdp[0][1][8][1]=0.114042; p2_pc3_m_sdp[0][1][8][1]=-0.0891349; p3_pc3_m_sdp[0][1][8][1]=0.0263253; p4_pc3_m_sdp[0][1][8][1]=-0.00281142; 
  p0_pc3_s_sdp[0][1][8][1]=0.39085; p1_pc3_s_sdp[0][1][8][1]=0.728812; p2_pc3_s_sdp[0][1][8][1]=-0.506982; p3_pc3_s_sdp[0][1][8][1]=0.145228; p4_pc3_s_sdp[0][1][8][1]=-0.0135782; 
  p0_pc3_m_sdp[1][1][8][1]=0.177827; p1_pc3_m_sdp[1][1][8][1]=-0.64488; p2_pc3_m_sdp[1][1][8][1]=0.093275; p3_pc3_m_sdp[1][1][8][1]=1.6869; p4_pc3_m_sdp[1][1][8][1]=-1.41893; 
  p0_pc3_s_sdp[1][1][8][1]=17.8326; p1_pc3_s_sdp[1][1][8][1]=-132.411; p2_pc3_s_sdp[1][1][8][1]=378.197; p3_pc3_s_sdp[1][1][8][1]=-475.668; p4_pc3_s_sdp[1][1][8][1]=222.764; 
  p0_pc3_m_sdp[0][1][9][1]=-0.0873663; p1_pc3_m_sdp[0][1][9][1]=0.209409; p2_pc3_m_sdp[0][1][9][1]=-0.147122; p3_pc3_m_sdp[0][1][9][1]=0.0424121; p4_pc3_m_sdp[0][1][9][1]=-0.00453641; 
  p0_pc3_s_sdp[0][1][9][1]=0.410775; p1_pc3_s_sdp[0][1][9][1]=0.644425; p2_pc3_s_sdp[0][1][9][1]=-0.408445; p3_pc3_s_sdp[0][1][9][1]=0.106504; p4_pc3_s_sdp[0][1][9][1]=-0.00887651; 
  p0_pc3_m_sdp[1][1][9][1]=0.817982; p1_pc3_m_sdp[1][1][9][1]=-3.3071; p2_pc3_m_sdp[1][1][9][1]=0.830752; p3_pc3_m_sdp[1][1][9][1]=9.00409; p4_pc3_m_sdp[1][1][9][1]=-8.42505; 
  p0_pc3_s_sdp[1][1][9][1]=2.24086; p1_pc3_s_sdp[1][1][9][1]=-6.05257; p2_pc3_s_sdp[1][1][9][1]=1.26891; p3_pc3_s_sdp[1][1][9][1]=16.1965; p4_pc3_s_sdp[1][1][9][1]=-14.5706; 
  
  //! PC3 sdz neg
  p0_pc3_m_sdz[0][0][0][0]=-0.0890138; p1_pc3_m_sdz[0][0][0][0]=0.0613624; p2_pc3_m_sdz[0][0][0][0]=-0.0210654; p3_pc3_m_sdz[0][0][0][0]=0.00215817; p4_pc3_m_sdz[0][0][0][0]=0.000172597; 
  p0_pc3_s_sdz[0][0][0][0]=0.671357; p1_pc3_s_sdz[0][0][0][0]=0.269071; p2_pc3_s_sdz[0][0][0][0]=-0.158884; p3_pc3_s_sdz[0][0][0][0]=0.0434411; p4_pc3_s_sdz[0][0][0][0]=-0.00438799; 
  p0_pc3_m_sdz[1][0][0][0]=-0.572101; p1_pc3_m_sdz[1][0][0][0]=1.90168; p2_pc3_m_sdz[1][0][0][0]=-0.486649; p3_pc3_m_sdz[1][0][0][0]=-4.65798; p4_pc3_m_sdz[1][0][0][0]=4.30309; 
  p0_pc3_s_sdz[1][0][0][0]=1.22264; p1_pc3_s_sdz[1][0][0][0]=-2.35088; p2_pc3_s_sdz[1][0][0][0]=1.11057; p3_pc3_s_sdz[1][0][0][0]=8.95487; p4_pc3_s_sdz[1][0][0][0]=-10.482; 
  p0_pc3_m_sdz[0][0][1][0]=-0.106784; p1_pc3_m_sdz[0][0][1][0]=0.136742; p2_pc3_m_sdz[0][0][1][0]=-0.0883693; p3_pc3_m_sdz[0][0][1][0]=0.0244101; p4_pc3_m_sdz[0][0][1][0]=-0.00235875; 
  p0_pc3_s_sdz[0][0][1][0]=0.766313; p1_pc3_s_sdz[0][0][1][0]=0.0908346; p2_pc3_s_sdz[0][0][1][0]=-0.0367389; p3_pc3_s_sdz[0][0][1][0]=0.00837039; p4_pc3_s_sdz[0][0][1][0]=-0.000822196; 
  p0_pc3_m_sdz[1][0][1][0]=-0.101887; p1_pc3_m_sdz[1][0][1][0]=0.269167; p2_pc3_m_sdz[1][0][1][0]=-0.137595; p3_pc3_m_sdz[1][0][1][0]=-0.660361; p4_pc3_m_sdz[1][0][1][0]=0.655762; 
  p0_pc3_s_sdz[1][0][1][0]=2.6036; p1_pc3_s_sdz[1][0][1][0]=-7.71365; p2_pc3_s_sdz[1][0][1][0]=2.86523; p3_pc3_s_sdz[1][0][1][0]=22.1891; p4_pc3_s_sdz[1][0][1][0]=-22.9888; 
  p0_pc3_m_sdz[0][0][2][0]=-0.0438286; p1_pc3_m_sdz[0][0][2][0]=0.0249274; p2_pc3_m_sdz[0][0][2][0]=-0.0117427; p3_pc3_m_sdz[0][0][2][0]=0.00268319; p4_pc3_m_sdz[0][0][2][0]=-0.000226484; 
  p0_pc3_s_sdz[0][0][2][0]=0.732849; p1_pc3_s_sdz[0][0][2][0]=0.165362; p2_pc3_s_sdz[0][0][2][0]=-0.0937144; p3_pc3_s_sdz[0][0][2][0]=0.0257902; p4_pc3_s_sdz[0][0][2][0]=-0.00262658; 
  p0_pc3_m_sdz[1][0][2][0]=0.162751; p1_pc3_m_sdz[1][0][2][0]=-1.02347; p2_pc3_m_sdz[1][0][2][0]=0.626982; p3_pc3_m_sdz[1][0][2][0]=3.05652; p4_pc3_m_sdz[1][0][2][0]=-3.60993; 
  p0_pc3_s_sdz[1][0][2][0]=1.39581; p1_pc3_s_sdz[1][0][2][0]=-2.45643; p2_pc3_s_sdz[1][0][2][0]=0.634044; p3_pc3_s_sdz[1][0][2][0]=7.79775; p4_pc3_s_sdz[1][0][2][0]=-7.8552; 
  p0_pc3_m_sdz[0][0][3][0]=-0.0293594; p1_pc3_m_sdz[0][0][3][0]=0.0197816; p2_pc3_m_sdz[0][0][3][0]=-0.0100081; p3_pc3_m_sdz[0][0][3][0]=0.0021984; p4_pc3_m_sdz[0][0][3][0]=-0.000122097; 
  p0_pc3_s_sdz[0][0][3][0]=0.72115; p1_pc3_s_sdz[0][0][3][0]=0.195665; p2_pc3_s_sdz[0][0][3][0]=-0.118309; p3_pc3_s_sdz[0][0][3][0]=0.0329808; p4_pc3_s_sdz[0][0][3][0]=-0.00334555; 
  p0_pc3_m_sdz[1][0][3][0]=-0.166845; p1_pc3_m_sdz[1][0][3][0]=0.350679; p2_pc3_m_sdz[1][0][3][0]=0.168805; p3_pc3_m_sdz[1][0][3][0]=-0.669001; p4_pc3_m_sdz[1][0][3][0]=0.133574; 
  p0_pc3_s_sdz[1][0][3][0]=1.18046; p1_pc3_s_sdz[1][0][3][0]=-1.56325; p2_pc3_s_sdz[1][0][3][0]=0.379744; p3_pc3_s_sdz[1][0][3][0]=4.9864; p4_pc3_s_sdz[1][0][3][0]=-4.97726; 
  p0_pc3_m_sdz[0][0][4][0]=-0.03036; p1_pc3_m_sdz[0][0][4][0]=0.0356091; p2_pc3_m_sdz[0][0][4][0]=-0.0226018; p3_pc3_m_sdz[0][0][4][0]=0.00583996; p4_pc3_m_sdz[0][0][4][0]=-0.000443042; 
  p0_pc3_s_sdz[0][0][4][0]=0.654392; p1_pc3_s_sdz[0][0][4][0]=0.327261; p2_pc3_s_sdz[0][0][4][0]=-0.215048; p3_pc3_s_sdz[0][0][4][0]=0.0614902; p4_pc3_s_sdz[0][0][4][0]=-0.00611895; 
  p0_pc3_m_sdz[1][0][4][0]=0.194337; p1_pc3_m_sdz[1][0][4][0]=-0.827054; p2_pc3_m_sdz[1][0][4][0]=0.199495; p3_pc3_m_sdz[1][0][4][0]=2.30673; p4_pc3_m_sdz[1][0][4][0]=-2.18334; 
  p0_pc3_s_sdz[1][0][4][0]=0.468318; p1_pc3_s_sdz[1][0][4][0]=1.45587; p2_pc3_s_sdz[1][0][4][0]=-0.737236; p3_pc3_s_sdz[1][0][4][0]=-3.85402; p4_pc3_s_sdz[1][0][4][0]=4.24106; 
  p0_pc3_m_sdz[0][0][5][0]=-0.0234512; p1_pc3_m_sdz[0][0][5][0]=0.00387432; p2_pc3_m_sdz[0][0][5][0]=0.00378733; p3_pc3_m_sdz[0][0][5][0]=-0.00228314; p4_pc3_m_sdz[0][0][5][0]=0.00033786; 
  p0_pc3_s_sdz[0][0][5][0]=0.661831; p1_pc3_s_sdz[0][0][5][0]=0.297607; p2_pc3_s_sdz[0][0][5][0]=-0.191729; p3_pc3_s_sdz[0][0][5][0]=0.0552772; p4_pc3_s_sdz[0][0][5][0]=-0.00558903; 
  p0_pc3_m_sdz[1][0][5][0]=-0.344727; p1_pc3_m_sdz[1][0][5][0]=1.28908; p2_pc3_m_sdz[1][0][5][0]=-0.38359; p3_pc3_m_sdz[1][0][5][0]=-3.52246; p4_pc3_m_sdz[1][0][5][0]=3.47297; 
  p0_pc3_s_sdz[1][0][5][0]=0.576228; p1_pc3_s_sdz[1][0][5][0]=0.97272; p2_pc3_s_sdz[1][0][5][0]=-0.583749; p3_pc3_s_sdz[1][0][5][0]=-2.61186; p4_pc3_s_sdz[1][0][5][0]=3.11423; 
  p0_pc3_m_sdz[0][0][6][0]=0.00227018; p1_pc3_m_sdz[0][0][6][0]=-0.0413604; p2_pc3_m_sdz[0][0][6][0]=0.0337519; p3_pc3_m_sdz[0][0][6][0]=-0.0101115; p4_pc3_m_sdz[0][0][6][0]=0.00100473; 
  p0_pc3_s_sdz[0][0][6][0]=0.710652; p1_pc3_s_sdz[0][0][6][0]=0.210893; p2_pc3_s_sdz[0][0][6][0]=-0.128431; p3_pc3_s_sdz[0][0][6][0]=0.0361776; p4_pc3_s_sdz[0][0][6][0]=-0.00370208; 
  p0_pc3_m_sdz[1][0][6][0]=0.338006; p1_pc3_m_sdz[1][0][6][0]=-1.39847; p2_pc3_m_sdz[1][0][6][0]=0.352058; p3_pc3_m_sdz[1][0][6][0]=3.97887; p4_pc3_m_sdz[1][0][6][0]=-3.81674; 
  p0_pc3_s_sdz[1][0][6][0]=1.03091; p1_pc3_s_sdz[1][0][6][0]=-0.915781; p2_pc3_s_sdz[1][0][6][0]=0.261427; p3_pc3_s_sdz[1][0][6][0]=2.80448; p4_pc3_s_sdz[1][0][6][0]=-2.82438; 
  p0_pc3_m_sdz[0][0][7][0]=-0.0335028; p1_pc3_m_sdz[0][0][7][0]=0.0120393; p2_pc3_m_sdz[0][0][7][0]=0.000424834; p3_pc3_m_sdz[0][0][7][0]=-0.000401931; p4_pc3_m_sdz[0][0][7][0]=-3.67724e-05; 
  p0_pc3_s_sdz[0][0][7][0]=0.727692; p1_pc3_s_sdz[0][0][7][0]=0.179201; p2_pc3_s_sdz[0][0][7][0]=-0.104156; p3_pc3_s_sdz[0][0][7][0]=0.0283759; p4_pc3_s_sdz[0][0][7][0]=-0.00279837; 
  p0_pc3_m_sdz[1][0][7][0]=16.992; p1_pc3_m_sdz[1][0][7][0]=-130.476; p2_pc3_m_sdz[1][0][7][0]=371.697; p3_pc3_m_sdz[1][0][7][0]=-466.169; p4_pc3_m_sdz[1][0][7][0]=217.2; 
  p0_pc3_s_sdz[1][0][7][0]=0.450032; p1_pc3_s_sdz[1][0][7][0]=1.46983; p2_pc3_s_sdz[1][0][7][0]=-0.533657; p3_pc3_s_sdz[1][0][7][0]=-3.3936; p4_pc3_s_sdz[1][0][7][0]=3.14191; 
  p0_pc3_m_sdz[0][0][8][0]=-0.0765511; p1_pc3_m_sdz[0][0][8][0]=0.114718; p2_pc3_m_sdz[0][0][8][0]=-0.0808356; p3_pc3_m_sdz[0][0][8][0]=0.025565; p4_pc3_m_sdz[0][0][8][0]=-0.00287548; 
  p0_pc3_s_sdz[0][0][8][0]=0.710838; p1_pc3_s_sdz[0][0][8][0]=0.2167; p2_pc3_s_sdz[0][0][8][0]=-0.134414; p3_pc3_s_sdz[0][0][8][0]=0.0382463; p4_pc3_s_sdz[0][0][8][0]=-0.00395381; 
  p0_pc3_m_sdz[1][0][8][0]=-0.387872; p1_pc3_m_sdz[1][0][8][0]=1.13316; p2_pc3_m_sdz[1][0][8][0]=0.184729; p3_pc3_m_sdz[1][0][8][0]=-2.93151; p4_pc3_m_sdz[1][0][8][0]=1.93875; 
  p0_pc3_s_sdz[1][0][8][0]=-2.21662; p1_pc3_s_sdz[1][0][8][0]=12.0532; p2_pc3_s_sdz[1][0][8][0]=-3.72347; p3_pc3_s_sdz[1][0][8][0]=-31.9838; p4_pc3_s_sdz[1][0][8][0]=31.1423; 
  p0_pc3_m_sdz[0][0][9][0]=0.0352173; p1_pc3_m_sdz[0][0][9][0]=-0.125401; p2_pc3_m_sdz[0][0][9][0]=0.0720713; p3_pc3_m_sdz[0][0][9][0]=-0.0142468; p4_pc3_m_sdz[0][0][9][0]=0.000838423; 
  p0_pc3_s_sdz[0][0][9][0]=0.708596; p1_pc3_s_sdz[0][0][9][0]=0.229859; p2_pc3_s_sdz[0][0][9][0]=-0.161613; p3_pc3_s_sdz[0][0][9][0]=0.0531951; p4_pc3_s_sdz[0][0][9][0]=-0.00617148; 
  p0_pc3_m_sdz[1][0][9][0]=-2.57155; p1_pc3_m_sdz[1][0][9][0]=10.2096; p2_pc3_m_sdz[1][0][9][0]=-2.88612; p3_pc3_m_sdz[1][0][9][0]=-29.1264; p4_pc3_m_sdz[1][0][9][0]=28.7396; 
  p0_pc3_s_sdz[1][0][9][0]=-5.06528; p1_pc3_s_sdz[1][0][9][0]=23.8584; p2_pc3_s_sdz[1][0][9][0]=-7.50979; p3_pc3_s_sdz[1][0][9][0]=-66.5215; p4_pc3_s_sdz[1][0][9][0]=66.2605; 
  p0_pc3_m_sdz[0][1][0][0]=-0.139915; p1_pc3_m_sdz[0][1][0][0]=0.197079; p2_pc3_m_sdz[0][1][0][0]=-0.138837; p3_pc3_m_sdz[0][1][0][0]=0.0424417; p4_pc3_m_sdz[0][1][0][0]=-0.00445882; 
  p0_pc3_s_sdz[0][1][0][0]=0.842335; p1_pc3_s_sdz[0][1][0][0]=-0.12313; p2_pc3_s_sdz[0][1][0][0]=0.145142; p3_pc3_s_sdz[0][1][0][0]=-0.050442; p4_pc3_s_sdz[0][1][0][0]=0.00566548; 
  p0_pc3_m_sdz[1][1][0][0]=1.17897; p1_pc3_m_sdz[1][1][0][0]=-5.34746; p2_pc3_m_sdz[1][1][0][0]=1.93275; p3_pc3_m_sdz[1][1][0][0]=15.9285; p4_pc3_m_sdz[1][1][0][0]=-16.6342; 
  p0_pc3_s_sdz[1][1][0][0]=-1.51691; p1_pc3_s_sdz[1][1][0][0]=9.14792; p2_pc3_s_sdz[1][1][0][0]=-3.17403; p3_pc3_s_sdz[1][1][0][0]=-23.7346; p4_pc3_s_sdz[1][1][0][0]=23.7242; 
  p0_pc3_m_sdz[0][1][1][0]=-0.0934423; p1_pc3_m_sdz[0][1][1][0]=0.119377; p2_pc3_m_sdz[0][1][1][0]=-0.081973; p3_pc3_m_sdz[0][1][1][0]=0.0243539; p4_pc3_m_sdz[0][1][1][0]=-0.00249876; 
  p0_pc3_s_sdz[0][1][1][0]=0.739781; p1_pc3_s_sdz[0][1][1][0]=0.108238; p2_pc3_s_sdz[0][1][1][0]=-0.0329799; p3_pc3_s_sdz[0][1][1][0]=0.00463842; p4_pc3_s_sdz[0][1][1][0]=-0.000217632; 
  p0_pc3_m_sdz[1][1][1][0]=0.201422; p1_pc3_m_sdz[1][1][1][0]=-0.946077; p2_pc3_m_sdz[1][1][1][0]=0.295946; p3_pc3_m_sdz[1][1][1][0]=2.39026; p4_pc3_m_sdz[1][1][1][0]=-2.31695; 
  p0_pc3_s_sdz[1][1][1][0]=2.09002; p1_pc3_s_sdz[1][1][1][0]=-5.27187; p2_pc3_s_sdz[1][1][1][0]=1.49393; p3_pc3_s_sdz[1][1][1][0]=14.7722; p4_pc3_s_sdz[1][1][1][0]=-14.3069; 
  p0_pc3_m_sdz[0][1][2][0]=-0.0537533; p1_pc3_m_sdz[0][1][2][0]=0.056797; p2_pc3_m_sdz[0][1][2][0]=-0.0385585; p3_pc3_m_sdz[0][1][2][0]=0.0112843; p4_pc3_m_sdz[0][1][2][0]=-0.00113823; 
  p0_pc3_s_sdz[0][1][2][0]=0.681727; p1_pc3_s_sdz[0][1][2][0]=0.233027; p2_pc3_s_sdz[0][1][2][0]=-0.130949; p3_pc3_s_sdz[0][1][2][0]=0.0360243; p4_pc3_s_sdz[0][1][2][0]=-0.00365816; 
  p0_pc3_m_sdz[1][1][2][0]=-0.0780226; p1_pc3_m_sdz[1][1][2][0]=0.0333239; p2_pc3_m_sdz[1][1][2][0]=0.1448; p3_pc3_m_sdz[1][1][2][0]=0.11306; p4_pc3_m_sdz[1][1][2][0]=-0.357751; 
  p0_pc3_s_sdz[1][1][2][0]=-0.702947; p1_pc3_s_sdz[1][1][2][0]=5.6584; p2_pc3_s_sdz[1][1][2][0]=-1.55635; p3_pc3_s_sdz[1][1][2][0]=-14.3087; p4_pc3_s_sdz[1][1][2][0]=13.4375; 
  p0_pc3_m_sdz[0][1][3][0]=-0.0120487; p1_pc3_m_sdz[0][1][3][0]=-0.0254283; p2_pc3_m_sdz[0][1][3][0]=0.0272527; p3_pc3_m_sdz[0][1][3][0]=-0.0100112; p4_pc3_m_sdz[0][1][3][0]=0.00123282; 
  p0_pc3_s_sdz[0][1][3][0]=0.677018; p1_pc3_s_sdz[0][1][3][0]=0.238096; p2_pc3_s_sdz[0][1][3][0]=-0.136402; p3_pc3_s_sdz[0][1][3][0]=0.0374551; p4_pc3_s_sdz[0][1][3][0]=-0.00379767; 
  p0_pc3_m_sdz[1][1][3][0]=-0.480249; p1_pc3_m_sdz[1][1][3][0]=1.68931; p2_pc3_m_sdz[1][1][3][0]=-0.341292; p3_pc3_m_sdz[1][1][3][0]=-4.34993; p4_pc3_m_sdz[1][1][3][0]=3.93818; 
  p0_pc3_s_sdz[1][1][3][0]=0.837938; p1_pc3_s_sdz[1][1][3][0]=-0.23148; p2_pc3_s_sdz[1][1][3][0]=-0.0509212; p3_pc3_s_sdz[1][1][3][0]=0.786471; p4_pc3_s_sdz[1][1][3][0]=-0.533484; 
  p0_pc3_m_sdz[0][1][4][0]=-0.0247743; p1_pc3_m_sdz[0][1][4][0]=0.0195215; p2_pc3_m_sdz[0][1][4][0]=-0.0107399; p3_pc3_m_sdz[0][1][4][0]=0.0022687; p4_pc3_m_sdz[0][1][4][0]=-0.000112408; 
  p0_pc3_s_sdz[0][1][4][0]=0.647349; p1_pc3_s_sdz[0][1][4][0]=0.303846; p2_pc3_s_sdz[0][1][4][0]=-0.195258; p3_pc3_s_sdz[0][1][4][0]=0.0564758; p4_pc3_s_sdz[0][1][4][0]=-0.00566854; 
  p0_pc3_m_sdz[1][1][4][0]=-0.0415217; p1_pc3_m_sdz[1][1][4][0]=0.0188884; p2_pc3_m_sdz[1][1][4][0]=0.0775503; p3_pc3_m_sdz[1][1][4][0]=0.0564897; p4_pc3_m_sdz[1][1][4][0]=-0.20316; 
  p0_pc3_s_sdz[1][1][4][0]=1.16399; p1_pc3_s_sdz[1][1][4][0]=-1.58063; p2_pc3_s_sdz[1][1][4][0]=0.379677; p3_pc3_s_sdz[1][1][4][0]=4.66634; p4_pc3_s_sdz[1][1][4][0]=-4.50677; 
  p0_pc3_m_sdz[0][1][5][0]=-0.0167683; p1_pc3_m_sdz[0][1][5][0]=-0.00269897; p2_pc3_m_sdz[0][1][5][0]=0.00189256; p3_pc3_m_sdz[0][1][5][0]=-0.00175122; p4_pc3_m_sdz[0][1][5][0]=0.00028836; 
  p0_pc3_s_sdz[0][1][5][0]=0.636778; p1_pc3_s_sdz[0][1][5][0]=0.33476; p2_pc3_s_sdz[0][1][5][0]=-0.220515; p3_pc3_s_sdz[0][1][5][0]=0.0646022; p4_pc3_s_sdz[0][1][5][0]=-0.00655359; 
  p0_pc3_m_sdz[1][1][5][0]=-0.530185; p1_pc3_m_sdz[1][1][5][0]=1.97379; p2_pc3_m_sdz[1][1][5][0]=-0.497772; p3_pc3_m_sdz[1][1][5][0]=-5.30645; p4_pc3_m_sdz[1][1][5][0]=5.01484; 
  p0_pc3_s_sdz[1][1][5][0]=0.469737; p1_pc3_s_sdz[1][1][5][0]=1.23567; p2_pc3_s_sdz[1][1][5][0]=-0.518552; p3_pc3_s_sdz[1][1][5][0]=-3.00189; p4_pc3_s_sdz[1][1][5][0]=3.08717; 
  p0_pc3_m_sdz[0][1][6][0]=-0.0352901; p1_pc3_m_sdz[0][1][6][0]=0.0293076; p2_pc3_m_sdz[0][1][6][0]=-0.0148648; p3_pc3_m_sdz[0][1][6][0]=0.00315718; p4_pc3_m_sdz[0][1][6][0]=-0.000368984; 
  p0_pc3_s_sdz[0][1][6][0]=0.651193; p1_pc3_s_sdz[0][1][6][0]=0.272308; p2_pc3_s_sdz[0][1][6][0]=-0.156139; p3_pc3_s_sdz[0][1][6][0]=0.0402498; p4_pc3_s_sdz[0][1][6][0]=-0.00374517; 
  p0_pc3_m_sdz[1][1][6][0]=0.0162311; p1_pc3_m_sdz[1][1][6][0]=-0.2655; p2_pc3_m_sdz[1][1][6][0]=0.313907; p3_pc3_m_sdz[1][1][6][0]=0.901132; p4_pc3_m_sdz[1][1][6][0]=-1.41298; 
  p0_pc3_s_sdz[1][1][6][0]=0.676946; p1_pc3_s_sdz[1][1][6][0]=0.331022; p2_pc3_s_sdz[1][1][6][0]=-0.150755; p3_pc3_s_sdz[1][1][6][0]=-0.403585; p4_pc3_s_sdz[1][1][6][0]=0.382721; 
  p0_pc3_m_sdz[0][1][7][0]=-0.076449; p1_pc3_m_sdz[0][1][7][0]=0.138599; p2_pc3_m_sdz[0][1][7][0]=-0.106534; p3_pc3_m_sdz[0][1][7][0]=0.0343594; p4_pc3_m_sdz[0][1][7][0]=-0.00387261; 
  p0_pc3_s_sdz[0][1][7][0]=0.636118; p1_pc3_s_sdz[0][1][7][0]=0.33907; p2_pc3_s_sdz[0][1][7][0]=-0.215428; p3_pc3_s_sdz[0][1][7][0]=0.0613774; p4_pc3_s_sdz[0][1][7][0]=-0.00629006; 
  p0_pc3_m_sdz[1][1][7][0]=-0.143896; p1_pc3_m_sdz[1][1][7][0]=0.43548; p2_pc3_m_sdz[1][1][7][0]=-0.0602173; p3_pc3_m_sdz[1][1][7][0]=-0.948659; p4_pc3_m_sdz[1][1][7][0]=0.659443; 
  p0_pc3_s_sdz[1][1][7][0]=-0.441027; p1_pc3_s_sdz[1][1][7][0]=4.99626; p2_pc3_s_sdz[1][1][7][0]=-1.64905; p3_pc3_s_sdz[1][1][7][0]=-14.004; p4_pc3_s_sdz[1][1][7][0]=14.2135; 
  p0_pc3_m_sdz[0][1][8][0]=-0.0366494; p1_pc3_m_sdz[0][1][8][0]=0.0126446; p2_pc3_m_sdz[0][1][8][0]=-0.00151718; p3_pc3_m_sdz[0][1][8][0]=0.000675188; p4_pc3_m_sdz[0][1][8][0]=-0.000231829; 
  p0_pc3_s_sdz[0][1][8][0]=0.780536; p1_pc3_s_sdz[0][1][8][0]=0.0164047; p2_pc3_s_sdz[0][1][8][0]=0.0301851; p3_pc3_s_sdz[0][1][8][0]=-0.0142954; p4_pc3_s_sdz[0][1][8][0]=0.00176036; 
  p0_pc3_m_sdz[1][1][8][0]=-0.645373; p1_pc3_m_sdz[1][1][8][0]=2.71374; p2_pc3_m_sdz[1][1][8][0]=-1.17273; p3_pc3_m_sdz[1][1][8][0]=-7.92224; p4_pc3_m_sdz[1][1][8][0]=8.62192; 
  p0_pc3_s_sdz[1][1][8][0]=2.26729; p1_pc3_s_sdz[1][1][8][0]=-5.99753; p2_pc3_s_sdz[1][1][8][0]=1.47402; p3_pc3_s_sdz[1][1][8][0]=17.2343; p4_pc3_s_sdz[1][1][8][0]=-16.4174; 
  p0_pc3_m_sdz[0][1][9][0]=-0.252978; p1_pc3_m_sdz[0][1][9][0]=0.468857; p2_pc3_m_sdz[0][1][9][0]=-0.33464; p3_pc3_m_sdz[0][1][9][0]=0.0988066; p4_pc3_m_sdz[0][1][9][0]=-0.0102524; 
  p0_pc3_s_sdz[0][1][9][0]=0.507499; p1_pc3_s_sdz[0][1][9][0]=0.604669; p2_pc3_s_sdz[0][1][9][0]=-0.396994; p3_pc3_s_sdz[0][1][9][0]=0.112453; p4_pc3_s_sdz[0][1][9][0]=-0.0113348; 
  p0_pc3_m_sdz[1][1][9][0]=2.06398; p1_pc3_m_sdz[1][1][9][0]=-8.63777; p2_pc3_m_sdz[1][1][9][0]=2.67997; p3_pc3_m_sdz[1][1][9][0]=24.7297; p4_pc3_m_sdz[1][1][9][0]=-24.8544; 
  p0_pc3_s_sdz[1][1][9][0]=1.03259; p1_pc3_s_sdz[1][1][9][0]=-1.63219; p2_pc3_s_sdz[1][1][9][0]=1.00285; p3_pc3_s_sdz[1][1][9][0]=6.29936; p4_pc3_s_sdz[1][1][9][0]=-7.64012; 
  
  //! PC3 sdz pos
  p0_pc3_m_sdz[0][0][0][1]=-0.193817; p1_pc3_m_sdz[0][0][0][1]=0.303066; p2_pc3_m_sdz[0][0][0][1]=-0.214004; p3_pc3_m_sdz[0][0][0][1]=0.0646795; p4_pc3_m_sdz[0][0][0][1]=-0.00679416; 
  p0_pc3_s_sdz[0][0][0][1]=0.758523; p1_pc3_s_sdz[0][0][0][1]=0.17038; p2_pc3_s_sdz[0][0][0][1]=-0.129353; p3_pc3_s_sdz[0][0][0][1]=0.0440611; p4_pc3_s_sdz[0][0][0][1]=-0.00515476; 
  p0_pc3_m_sdz[1][0][0][1]=-16.4713; p1_pc3_m_sdz[1][0][0][1]=124.948; p2_pc3_m_sdz[1][0][0][1]=-354.981; p3_pc3_m_sdz[1][0][0][1]=446.074; p4_pc3_m_sdz[1][0][0][1]=-209.141; 
  p0_pc3_s_sdz[1][0][0][1]=-0.825171; p1_pc3_s_sdz[1][0][0][1]=7.15559; p2_pc3_s_sdz[1][0][0][1]=-2.97808; p3_pc3_s_sdz[1][0][0][1]=-21.1122; p4_pc3_s_sdz[1][0][0][1]=22.885; 
  p0_pc3_m_sdz[0][0][1][1]=-0.029076; p1_pc3_m_sdz[0][0][1][1]=-0.0203345; p2_pc3_m_sdz[0][0][1][1]=0.0145313; p3_pc3_m_sdz[0][0][1][1]=-0.00314476; p4_pc3_m_sdz[0][0][1][1]=0.000252944; 
  p0_pc3_s_sdz[0][0][1][1]=0.726814; p1_pc3_s_sdz[0][0][1][1]=0.243009; p2_pc3_s_sdz[0][0][1][1]=-0.169782; p3_pc3_s_sdz[0][0][1][1]=0.0511667; p4_pc3_s_sdz[0][0][1][1]=-0.00542523; 
  p0_pc3_m_sdz[1][0][1][1]=-0.426812; p1_pc3_m_sdz[1][0][1][1]=1.49786; p2_pc3_m_sdz[1][0][1][1]=-0.358712; p3_pc3_m_sdz[1][0][1][1]=-4.23163; p4_pc3_m_sdz[1][0][1][1]=4.05308; 
  p0_pc3_s_sdz[1][0][1][1]=1.21102; p1_pc3_s_sdz[1][0][1][1]=-1.65482; p2_pc3_s_sdz[1][0][1][1]=0.594415; p3_pc3_s_sdz[1][0][1][1]=4.81343; p4_pc3_s_sdz[1][0][1][1]=-4.95234; 
  p0_pc3_m_sdz[0][0][2][1]=-0.0325292; p1_pc3_m_sdz[0][0][2][1]=0.013102; p2_pc3_m_sdz[0][0][2][1]=-0.0115582; p3_pc3_m_sdz[0][0][2][1]=0.00444101; p4_pc3_m_sdz[0][0][2][1]=-0.000512599; 
  p0_pc3_s_sdz[0][0][2][1]=0.696509; p1_pc3_s_sdz[0][0][2][1]=0.304855; p2_pc3_s_sdz[0][0][2][1]=-0.219559; p3_pc3_s_sdz[0][0][2][1]=0.0674381; p4_pc3_s_sdz[0][0][2][1]=-0.00721598; 
  p0_pc3_m_sdz[1][0][2][1]=1.55289; p1_pc3_m_sdz[1][0][2][1]=-6.64426; p2_pc3_m_sdz[1][0][2][1]=2.16679; p3_pc3_m_sdz[1][0][2][1]=19.5375; p4_pc3_m_sdz[1][0][2][1]=-19.9631; 
  p0_pc3_s_sdz[1][0][2][1]=-0.287537; p1_pc3_s_sdz[1][0][2][1]=4.833; p2_pc3_s_sdz[1][0][2][1]=-2.05825; p3_pc3_s_sdz[1][0][2][1]=-13.7811; p4_pc3_s_sdz[1][0][2][1]=14.8078; 
  p0_pc3_m_sdz[0][0][3][1]=-0.0105837; p1_pc3_m_sdz[0][0][3][1]=-0.0159854; p2_pc3_m_sdz[0][0][3][1]=0.012494; p3_pc3_m_sdz[0][0][3][1]=-0.00344094; p4_pc3_m_sdz[0][0][3][1]=0.000351404; 
  p0_pc3_s_sdz[0][0][3][1]=0.725953; p1_pc3_s_sdz[0][0][3][1]=0.246118; p2_pc3_s_sdz[0][0][3][1]=-0.179437; p3_pc3_s_sdz[0][0][3][1]=0.0547082; p4_pc3_s_sdz[0][0][3][1]=-0.00575442; 
  p0_pc3_m_sdz[1][0][3][1]=-0.3752; p1_pc3_m_sdz[1][0][3][1]=1.40941; p2_pc3_m_sdz[1][0][3][1]=-0.329775; p3_pc3_m_sdz[1][0][3][1]=-4.11606; p4_pc3_m_sdz[1][0][3][1]=3.97013; 
  p0_pc3_s_sdz[1][0][3][1]=0.644576; p1_pc3_s_sdz[1][0][3][1]=0.7653; p2_pc3_s_sdz[1][0][3][1]=-0.377932; p3_pc3_s_sdz[1][0][3][1]=-1.94184; p4_pc3_s_sdz[1][0][3][1]=2.1249; 
  p0_pc3_m_sdz[0][0][4][1]=-0.021137; p1_pc3_m_sdz[0][0][4][1]=0.0225943; p2_pc3_m_sdz[0][0][4][1]=-0.0190794; p3_pc3_m_sdz[0][0][4][1]=0.00667538; p4_pc3_m_sdz[0][0][4][1]=-0.000743248; 
  p0_pc3_s_sdz[0][0][4][1]=0.669509; p1_pc3_s_sdz[0][0][4][1]=0.372429; p2_pc3_s_sdz[0][0][4][1]=-0.288116; p3_pc3_s_sdz[0][0][4][1]=0.090191; p4_pc3_s_sdz[0][0][4][1]=-0.00947714; 
  p0_pc3_m_sdz[1][0][4][1]=-0.108622; p1_pc3_m_sdz[1][0][4][1]=0.335417; p2_pc3_m_sdz[1][0][4][1]=-0.0648892; p3_pc3_m_sdz[1][0][4][1]=-0.857623; p4_pc3_m_sdz[1][0][4][1]=0.791365; 
  p0_pc3_s_sdz[1][0][4][1]=1.01418; p1_pc3_s_sdz[1][0][4][1]=-0.730082; p2_pc3_s_sdz[1][0][4][1]=-0.0342902; p3_pc3_s_sdz[1][0][4][1]=2.20624; p4_pc3_s_sdz[1][0][4][1]=-1.75085; 
  p0_pc3_m_sdz[0][0][5][1]=-0.0298095; p1_pc3_m_sdz[0][0][5][1]=0.0313443; p2_pc3_m_sdz[0][0][5][1]=-0.0211395; p3_pc3_m_sdz[0][0][5][1]=0.00581544; p4_pc3_m_sdz[0][0][5][1]=-0.000533153; 
  p0_pc3_s_sdz[0][0][5][1]=0.667826; p1_pc3_s_sdz[0][0][5][1]=0.359418; p2_pc3_s_sdz[0][0][5][1]=-0.276451; p3_pc3_s_sdz[0][0][5][1]=0.0880418; p4_pc3_s_sdz[0][0][5][1]=-0.00945259; 
  p0_pc3_m_sdz[1][0][5][1]=-0.220998; p1_pc3_m_sdz[1][0][5][1]=0.8017; p2_pc3_m_sdz[1][0][5][1]=-0.190534; p3_pc3_m_sdz[1][0][5][1]=-2.33116; p4_pc3_m_sdz[1][0][5][1]=2.28716; 
  p0_pc3_s_sdz[1][0][5][1]=0.527266; p1_pc3_s_sdz[1][0][5][1]=1.12894; p2_pc3_s_sdz[1][0][5][1]=-0.53146; p3_pc3_s_sdz[1][0][5][1]=-2.46496; p4_pc3_s_sdz[1][0][5][1]=2.51801; 
  p0_pc3_m_sdz[0][0][6][1]=-0.0232377; p1_pc3_m_sdz[0][0][6][1]=0.0118851; p2_pc3_m_sdz[0][0][6][1]=-0.000225229; p3_pc3_m_sdz[0][0][6][1]=-0.00139025; p4_pc3_m_sdz[0][0][6][1]=0.000239878; 
  p0_pc3_s_sdz[0][0][6][1]=0.71246; p1_pc3_s_sdz[0][0][6][1]=0.278851; p2_pc3_s_sdz[0][0][6][1]=-0.207991; p3_pc3_s_sdz[0][0][6][1]=0.0653231; p4_pc3_s_sdz[0][0][6][1]=-0.00708994; 
  p0_pc3_m_sdz[1][0][6][1]=-0.0887621; p1_pc3_m_sdz[1][0][6][1]=0.270251; p2_pc3_m_sdz[1][0][6][1]=-0.0345189; p3_pc3_m_sdz[1][0][6][1]=-0.701463; p4_pc3_m_sdz[1][0][6][1]=0.574193; 
  p0_pc3_s_sdz[1][0][6][1]=1.34517; p1_pc3_s_sdz[1][0][6][1]=-1.94612; p2_pc3_s_sdz[1][0][6][1]=0.24915; p3_pc3_s_sdz[1][0][6][1]=5.50725; p4_pc3_s_sdz[1][0][6][1]=-4.87093; 
  p0_pc3_m_sdz[0][0][7][1]=-0.0791153; p1_pc3_m_sdz[0][0][7][1]=0.114465; p2_pc3_m_sdz[0][0][7][1]=-0.0675284; p3_pc3_m_sdz[0][0][7][1]=0.0166185; p4_pc3_m_sdz[0][0][7][1]=-0.00140298; 
  p0_pc3_s_sdz[0][0][7][1]=0.739853; p1_pc3_s_sdz[0][0][7][1]=0.227441; p2_pc3_s_sdz[0][0][7][1]=-0.170246; p3_pc3_s_sdz[0][0][7][1]=0.0545679; p4_pc3_s_sdz[0][0][7][1]=-0.00604994; 
  p0_pc3_m_sdz[1][0][7][1]=0.832429; p1_pc3_m_sdz[1][0][7][1]=-3.61882; p2_pc3_m_sdz[1][0][7][1]=1.33; p3_pc3_m_sdz[1][0][7][1]=10.0897; p4_pc3_m_sdz[1][0][7][1]=-10.285; 
  p0_pc3_s_sdz[1][0][7][1]=2.78918; p1_pc3_s_sdz[1][0][7][1]=-8.24738; p2_pc3_s_sdz[1][0][7][1]=2.79957; p3_pc3_s_sdz[1][0][7][1]=23.6761; p4_pc3_s_sdz[1][0][7][1]=-24.0272; 
  p0_pc3_m_sdz[0][0][8][1]=0.0110716; p1_pc3_m_sdz[0][0][8][1]=-0.0908174; p2_pc3_m_sdz[0][0][8][1]=0.0876703; p3_pc3_m_sdz[0][0][8][1]=-0.0304991; p4_pc3_m_sdz[0][0][8][1]=0.00347481; 
  p0_pc3_s_sdz[0][0][8][1]=0.822968; p1_pc3_s_sdz[0][0][8][1]=0.0147191; p2_pc3_s_sdz[0][0][8][1]=0.00158631; p3_pc3_s_sdz[0][0][8][1]=-0.00103453; p4_pc3_s_sdz[0][0][8][1]=7.99877e-05; 
  p0_pc3_m_sdz[1][0][8][1]=1.09413; p1_pc3_m_sdz[1][0][8][1]=-4.42729; p2_pc3_m_sdz[1][0][8][1]=1.03848; p3_pc3_m_sdz[1][0][8][1]=12.6757; p4_pc3_m_sdz[1][0][8][1]=-12.0829; 
  p0_pc3_s_sdz[1][0][8][1]=2.76213; p1_pc3_s_sdz[1][0][8][1]=-7.73272; p2_pc3_s_sdz[1][0][8][1]=1.73776; p3_pc3_s_sdz[1][0][8][1]=22.1344; p4_pc3_s_sdz[1][0][8][1]=-20.7624; 
  p0_pc3_m_sdz[0][0][9][1]=-0.0357987; p1_pc3_m_sdz[0][0][9][1]=-0.0231352; p2_pc3_m_sdz[0][0][9][1]=0.0461203; p3_pc3_m_sdz[0][0][9][1]=-0.0200342; p4_pc3_m_sdz[0][0][9][1]=0.00259672; 
  p0_pc3_s_sdz[0][0][9][1]=0.912907; p1_pc3_s_sdz[0][0][9][1]=-0.21474; p2_pc3_s_sdz[0][0][9][1]=0.18371; p3_pc3_s_sdz[0][0][9][1]=-0.0571175; p4_pc3_s_sdz[0][0][9][1]=0.00594117; 
  p0_pc3_m_sdz[1][0][9][1]=-1.48554; p1_pc3_m_sdz[1][0][9][1]=5.83819; p2_pc3_m_sdz[1][0][9][1]=-1.59315; p3_pc3_m_sdz[1][0][9][1]=-17.011; p4_pc3_m_sdz[1][0][9][1]=16.8856; 
  p0_pc3_s_sdz[1][0][9][1]=-0.918773; p1_pc3_s_sdz[1][0][9][1]=7.13507; p2_pc3_s_sdz[1][0][9][1]=-2.49554; p3_pc3_s_sdz[1][0][9][1]=-19.7915; p4_pc3_s_sdz[1][0][9][1]=20.1355; 
  p0_pc3_m_sdz[0][1][0][1]=-0.271093; p1_pc3_m_sdz[0][1][0][1]=0.460405; p2_pc3_m_sdz[0][1][0][1]=-0.33107; p3_pc3_m_sdz[0][1][0][1]=0.100689; p4_pc3_m_sdz[0][1][0][1]=-0.0105987; 
  p0_pc3_s_sdz[0][1][0][1]=0.628109; p1_pc3_s_sdz[0][1][0][1]=0.369425; p2_pc3_s_sdz[0][1][0][1]=-0.244513; p3_pc3_s_sdz[0][1][0][1]=0.0719837; p4_pc3_s_sdz[0][1][0][1]=-0.00751558; 
  p0_pc3_m_sdz[1][1][0][1]=0.225175; p1_pc3_m_sdz[1][1][0][1]=-1.23466; p2_pc3_m_sdz[1][1][0][1]=0.466881; p3_pc3_m_sdz[1][1][0][1]=3.49053; p4_pc3_m_sdz[1][1][0][1]=-3.56375; 
  p0_pc3_s_sdz[1][1][0][1]=-24.1004; p1_pc3_s_sdz[1][1][0][1]=191.461; p2_pc3_s_sdz[1][1][0][1]=-546.361; p3_pc3_s_sdz[1][1][0][1]=685.568; p4_pc3_s_sdz[1][1][0][1]=-319.29; 
  p0_pc3_m_sdz[0][1][1][1]=-0.0382585; p1_pc3_m_sdz[0][1][1][1]=-0.00948339; p2_pc3_m_sdz[0][1][1][1]=0.0151506; p3_pc3_m_sdz[0][1][1][1]=-0.00534551; p4_pc3_m_sdz[0][1][1][1]=0.000643202; 
  p0_pc3_s_sdz[0][1][1][1]=0.588093; p1_pc3_s_sdz[0][1][1][1]=0.482828; p2_pc3_s_sdz[0][1][1][1]=-0.338252; p3_pc3_s_sdz[0][1][1][1]=0.101802; p4_pc3_s_sdz[0][1][1][1]=-0.0107339; 
  p0_pc3_m_sdz[1][1][1][1]=1.13485; p1_pc3_m_sdz[1][1][1][1]=-4.60876; p2_pc3_m_sdz[1][1][1][1]=1.14822; p3_pc3_m_sdz[1][1][1][1]=12.379; p4_pc3_m_sdz[1][1][1][1]=-11.5508; 
  p0_pc3_s_sdz[1][1][1][1]=0.713769; p1_pc3_s_sdz[1][1][1][1]=0.366376; p2_pc3_s_sdz[1][1][1][1]=-0.135552; p3_pc3_s_sdz[1][1][1][1]=-0.46616; p4_pc3_s_sdz[1][1][1][1]=0.112514; 
  p0_pc3_m_sdz[0][1][2][1]=-0.0652392; p1_pc3_m_sdz[0][1][2][1]=0.0812107; p2_pc3_m_sdz[0][1][2][1]=-0.0570366; p3_pc3_m_sdz[0][1][2][1]=0.0167429; p4_pc3_m_sdz[0][1][2][1]=-0.00168643; 
  p0_pc3_s_sdz[0][1][2][1]=0.63871; p1_pc3_s_sdz[0][1][2][1]=0.377456; p2_pc3_s_sdz[0][1][2][1]=-0.263845; p3_pc3_s_sdz[0][1][2][1]=0.0804673; p4_pc3_s_sdz[0][1][2][1]=-0.00858492; 
  p0_pc3_m_sdz[1][1][2][1]=-0.897791; p1_pc3_m_sdz[1][1][2][1]=3.42047; p2_pc3_m_sdz[1][1][2][1]=-0.854075; p3_pc3_m_sdz[1][1][2][1]=-9.58314; p4_pc3_m_sdz[1][1][2][1]=9.19555; 
  p0_pc3_s_sdz[1][1][2][1]=0.562241; p1_pc3_s_sdz[1][1][2][1]=0.805111; p2_pc3_s_sdz[1][1][2][1]=-0.243991; p3_pc3_s_sdz[1][1][2][1]=-1.68272; p4_pc3_s_sdz[1][1][2][1]=1.55405; 
  p0_pc3_m_sdz[0][1][3][1]=-0.036515; p1_pc3_m_sdz[0][1][3][1]=0.0415901; p2_pc3_m_sdz[0][1][3][1]=-0.0284901; p3_pc3_m_sdz[0][1][3][1]=0.00769594; p4_pc3_m_sdz[0][1][3][1]=-0.000663381; 
  p0_pc3_s_sdz[0][1][3][1]=0.640962; p1_pc3_s_sdz[0][1][3][1]=0.368446; p2_pc3_s_sdz[0][1][3][1]=-0.258955; p3_pc3_s_sdz[0][1][3][1]=0.0781821; p4_pc3_s_sdz[0][1][3][1]=-0.00825499; 
  p0_pc3_m_sdz[1][1][3][1]=0.0559186; p1_pc3_m_sdz[1][1][3][1]=-0.436796; p2_pc3_m_sdz[1][1][3][1]=0.241356; p3_pc3_m_sdz[1][1][3][1]=1.34745; p4_pc3_m_sdz[1][1][3][1]=-1.48601; 
  p0_pc3_s_sdz[1][1][3][1]=0.861873; p1_pc3_s_sdz[1][1][3][1]=0.14441; p2_pc3_s_sdz[1][1][3][1]=-0.654059; p3_pc3_s_sdz[1][1][3][1]=-0.699048; p4_pc3_s_sdz[1][1][3][1]=1.76286; 
  p0_pc3_m_sdz[0][1][4][1]=-0.0384847; p1_pc3_m_sdz[0][1][4][1]=0.0568024; p2_pc3_m_sdz[0][1][4][1]=-0.0433389; p3_pc3_m_sdz[0][1][4][1]=0.0130281; p4_pc3_m_sdz[0][1][4][1]=-0.00127578; 
  p0_pc3_s_sdz[0][1][4][1]=0.587311; p1_pc3_s_sdz[0][1][4][1]=0.498274; p2_pc3_s_sdz[0][1][4][1]=-0.374888; p3_pc3_s_sdz[0][1][4][1]=0.116714; p4_pc3_s_sdz[0][1][4][1]=-0.0123532; 
  p0_pc3_m_sdz[1][1][4][1]=0.377851; p1_pc3_m_sdz[1][1][4][1]=-1.67015; p2_pc3_m_sdz[1][1][4][1]=0.625672; p3_pc3_m_sdz[1][1][4][1]=4.56775; p4_pc3_m_sdz[1][1][4][1]=-4.64977; 
  p0_pc3_s_sdz[1][1][4][1]=0.892747; p1_pc3_s_sdz[1][1][4][1]=-0.344545; p2_pc3_s_sdz[1][1][4][1]=-0.130359; p3_pc3_s_sdz[1][1][4][1]=1.14894; p4_pc3_s_sdz[1][1][4][1]=-0.768733; 
  p0_pc3_m_sdz[0][1][5][1]=-0.0259435; p1_pc3_m_sdz[0][1][5][1]=0.0226667; p2_pc3_m_sdz[0][1][5][1]=-0.0174296; p3_pc3_m_sdz[0][1][5][1]=0.00471467; p4_pc3_m_sdz[0][1][5][1]=-0.000465835; 
  p0_pc3_s_sdz[0][1][5][1]=0.583883; p1_pc3_s_sdz[0][1][5][1]=0.506643; p2_pc3_s_sdz[0][1][5][1]=-0.38513; p3_pc3_s_sdz[0][1][5][1]=0.12084; p4_pc3_s_sdz[0][1][5][1]=-0.0129199; 
  p0_pc3_m_sdz[1][1][5][1]=0.222626; p1_pc3_m_sdz[1][1][5][1]=-1.00042; p2_pc3_m_sdz[1][1][5][1]=0.282389; p3_pc3_m_sdz[1][1][5][1]=2.91465; p4_pc3_m_sdz[1][1][5][1]=-2.85514; 
  p0_pc3_s_sdz[1][1][5][1]=1.25567; p1_pc3_s_sdz[1][1][5][1]=-1.76916; p2_pc3_s_sdz[1][1][5][1]=0.27306; p3_pc3_s_sdz[1][1][5][1]=4.9271; p4_pc3_s_sdz[1][1][5][1]=-4.41049; 
  p0_pc3_m_sdz[0][1][6][1]=-0.0215739; p1_pc3_m_sdz[0][1][6][1]=0.00432177; p2_pc3_m_sdz[0][1][6][1]=0.00441205; p3_pc3_m_sdz[0][1][6][1]=-0.00298837; p4_pc3_m_sdz[0][1][6][1]=0.000345497; 
  p0_pc3_s_sdz[0][1][6][1]=0.602607; p1_pc3_s_sdz[0][1][6][1]=0.45665; p2_pc3_s_sdz[0][1][6][1]=-0.337043; p3_pc3_s_sdz[0][1][6][1]=0.103687; p4_pc3_s_sdz[0][1][6][1]=-0.011092; 
  p0_pc3_m_sdz[1][1][6][1]=0.68857; p1_pc3_m_sdz[1][1][6][1]=-2.84304; p2_pc3_m_sdz[1][1][6][1]=0.881812; p3_pc3_m_sdz[1][1][6][1]=7.81908; p4_pc3_m_sdz[1][1][6][1]=-7.73724; 
  p0_pc3_s_sdz[1][1][6][1]=1.13571; p1_pc3_s_sdz[1][1][6][1]=-1.46562; p2_pc3_s_sdz[1][1][6][1]=0.377349; p3_pc3_s_sdz[1][1][6][1]=4.72955; p4_pc3_s_sdz[1][1][6][1]=-4.81285; 
  p0_pc3_m_sdz[0][1][7][1]=-0.071923; p1_pc3_m_sdz[0][1][7][1]=0.10058; p2_pc3_m_sdz[0][1][7][1]=-0.0598032; p3_pc3_m_sdz[0][1][7][1]=0.0159079; p4_pc3_m_sdz[0][1][7][1]=-0.00155359; 
  p0_pc3_s_sdz[0][1][7][1]=0.633826; p1_pc3_s_sdz[0][1][7][1]=0.397659; p2_pc3_s_sdz[0][1][7][1]=-0.29148; p3_pc3_s_sdz[0][1][7][1]=0.0915233; p4_pc3_s_sdz[0][1][7][1]=-0.0100542; 
  p0_pc3_m_sdz[1][1][7][1]=0.466089; p1_pc3_m_sdz[1][1][7][1]=-1.88078; p2_pc3_m_sdz[1][1][7][1]=0.543548; p3_pc3_m_sdz[1][1][7][1]=4.91371; p4_pc3_m_sdz[1][1][7][1]=-4.76665; 
  p0_pc3_s_sdz[1][1][7][1]=-1.2822; p1_pc3_s_sdz[1][1][7][1]=8.13536; p2_pc3_s_sdz[1][1][7][1]=-2.31131; p3_pc3_s_sdz[1][1][7][1]=-21.51; p4_pc3_s_sdz[1][1][7][1]=20.5708; 
  p0_pc3_m_sdz[0][1][8][1]=-0.117707; p1_pc3_m_sdz[0][1][8][1]=0.183081; p2_pc3_m_sdz[0][1][8][1]=-0.114956; p3_pc3_m_sdz[0][1][8][1]=0.0313754; p4_pc3_m_sdz[0][1][8][1]=-0.00312146; 
  p0_pc3_s_sdz[0][1][8][1]=0.671473; p1_pc3_s_sdz[0][1][8][1]=0.302407; p2_pc3_s_sdz[0][1][8][1]=-0.212563; p3_pc3_s_sdz[0][1][8][1]=0.0655189; p4_pc3_s_sdz[0][1][8][1]=-0.00714997; 
  p0_pc3_m_sdz[1][1][8][1]=-0.305656; p1_pc3_m_sdz[1][1][8][1]=0.822949; p2_pc3_m_sdz[1][1][8][1]=0.0614112; p3_pc3_m_sdz[1][1][8][1]=-1.98012; p4_pc3_m_sdz[1][1][8][1]=1.47166; 
  p0_pc3_s_sdz[1][1][8][1]=2.4235; p1_pc3_s_sdz[1][1][8][1]=-6.46399; p2_pc3_s_sdz[1][1][8][1]=1.61336; p3_pc3_s_sdz[1][1][8][1]=17.7364; p4_pc3_s_sdz[1][1][8][1]=-16.6199; 
  p0_pc3_m_sdz[0][1][9][1]=0.11867; p1_pc3_m_sdz[0][1][9][1]=-0.342192; p2_pc3_m_sdz[0][1][9][1]=0.272625; p3_pc3_m_sdz[0][1][9][1]=-0.0858244; p4_pc3_m_sdz[0][1][9][1]=0.00918829; 
  p0_pc3_s_sdz[0][1][9][1]=0.607756; p1_pc3_s_sdz[0][1][9][1]=0.431862; p2_pc3_s_sdz[0][1][9][1]=-0.300057; p3_pc3_s_sdz[0][1][9][1]=0.0905741; p4_pc3_s_sdz[0][1][9][1]=-0.00970548; 
  p0_pc3_m_sdz[1][1][9][1]=0.698063; p1_pc3_m_sdz[1][1][9][1]=-2.70615; p2_pc3_m_sdz[1][1][9][1]=0.436949; p3_pc3_m_sdz[1][1][9][1]=7.14614; p4_pc3_m_sdz[1][1][9][1]=-6.29394; 
  p0_pc3_s_sdz[1][1][9][1]=2.60856; p1_pc3_s_sdz[1][1][9][1]=-6.60742; p2_pc3_s_sdz[1][1][9][1]=1.12833; p3_pc3_s_sdz[1][1][9][1]=16.9188; p4_pc3_s_sdz[1][1][9][1]=-14.8042; 
      
  //! EMC sdphi neg
  p0_emc_m_sdp[0][0][0][0]=-0.0710847; p1_emc_m_sdp[0][0][0][0]=0.226546; p2_emc_m_sdp[0][0][0][0]=-0.197207; p3_emc_m_sdp[0][0][0][0]=0.0706379; p4_emc_m_sdp[0][0][0][0]=-0.00847843; 
  p0_emc_s_sdp[0][0][0][0]=0.845884; p1_emc_s_sdp[0][0][0][0]=-0.042074; p2_emc_s_sdp[0][0][0][0]=0.0878859; p3_emc_s_sdp[0][0][0][0]=-0.0404251; p4_emc_s_sdp[0][0][0][0]=0.00576533; 
  p0_emc_m_sdp[1][0][0][0]=0.27208; p1_emc_m_sdp[1][0][0][0]=-0.74443; p2_emc_m_sdp[1][0][0][0]=-0.0278696; p3_emc_m_sdp[1][0][0][0]=1.64862; p4_emc_m_sdp[1][0][0][0]=-1.14365; 
  p0_emc_s_sdp[1][0][0][0]=1.39028; p1_emc_s_sdp[1][0][0][0]=-1.95564; p2_emc_s_sdp[1][0][0][0]=0.413306; p3_emc_s_sdp[1][0][0][0]=5.26637; p4_emc_s_sdp[1][0][0][0]=-4.98539; 
  p0_emc_m_sdp[0][0][1][0]=-0.0876699; p1_emc_m_sdp[0][0][1][0]=0.253156; p2_emc_m_sdp[0][0][1][0]=-0.206686; p3_emc_m_sdp[0][0][1][0]=0.068186; p4_emc_m_sdp[0][0][1][0]=-0.00730472; 
  p0_emc_s_sdp[0][0][1][0]=0.835795; p1_emc_s_sdp[0][0][1][0]=0.00102037; p2_emc_s_sdp[0][0][1][0]=0.038527; p3_emc_s_sdp[0][0][1][0]=-0.0181885; p4_emc_s_sdp[0][0][1][0]=0.0022176; 
  p0_emc_m_sdp[1][0][1][0]=0.149399; p1_emc_m_sdp[1][0][1][0]=-0.0897035; p2_emc_m_sdp[1][0][1][0]=-0.315707; p3_emc_m_sdp[1][0][1][0]=-0.236471; p4_emc_m_sdp[1][0][1][0]=0.7354; 
  p0_emc_s_sdp[1][0][1][0]=1.00074; p1_emc_s_sdp[1][0][1][0]=-0.318205; p2_emc_s_sdp[1][0][1][0]=-0.203318; p3_emc_s_sdp[1][0][1][0]=0.810847; p4_emc_s_sdp[1][0][1][0]=-0.427673; 
  p0_emc_m_sdp[0][0][2][0]=-0.126712; p1_emc_m_sdp[0][0][2][0]=0.34431; p2_emc_m_sdp[0][0][2][0]=-0.274157; p3_emc_m_sdp[0][0][2][0]=0.0876201; p4_emc_m_sdp[0][0][2][0]=-0.00914998; 
  p0_emc_s_sdp[0][0][2][0]=0.885538; p1_emc_s_sdp[0][0][2][0]=-0.110175; p2_emc_s_sdp[0][0][2][0]=0.124094; p3_emc_s_sdp[0][0][2][0]=-0.0447889; p4_emc_s_sdp[0][0][2][0]=0.00502556; 
  p0_emc_m_sdp[1][0][2][0]=0.334825; p1_emc_m_sdp[1][0][2][0]=-0.691284; p2_emc_m_sdp[1][0][2][0]=-0.233482; p3_emc_m_sdp[1][0][2][0]=1.40222; p4_emc_m_sdp[1][0][2][0]=-0.83292; 
  p0_emc_s_sdp[1][0][2][0]=1.22692; p1_emc_s_sdp[1][0][2][0]=-1.25605; p2_emc_s_sdp[1][0][2][0]=0.0710972; p3_emc_s_sdp[1][0][2][0]=3.55383; p4_emc_s_sdp[1][0][2][0]=-3.1197; 
  p0_emc_m_sdp[0][0][3][0]=-0.103789; p1_emc_m_sdp[0][0][3][0]=0.29583; p2_emc_m_sdp[0][0][3][0]=-0.240601; p3_emc_m_sdp[0][0][3][0]=0.0783085; p4_emc_m_sdp[0][0][3][0]=-0.00824848; 
  p0_emc_s_sdp[0][0][3][0]=0.883359; p1_emc_s_sdp[0][0][3][0]=-0.101174; p2_emc_s_sdp[0][0][3][0]=0.117318; p3_emc_s_sdp[0][0][3][0]=-0.0428302; p4_emc_s_sdp[0][0][3][0]=0.00482788; 
  p0_emc_m_sdp[1][0][3][0]=0.0796821; p1_emc_m_sdp[1][0][3][0]=0.395006; p2_emc_m_sdp[1][0][3][0]=-0.564011; p3_emc_m_sdp[1][0][3][0]=-1.67311; p4_emc_m_sdp[1][0][3][0]=2.17223; 
  p0_emc_s_sdp[1][0][3][0]=1.37422; p1_emc_s_sdp[1][0][3][0]=-1.9194; p2_emc_s_sdp[1][0][3][0]=0.355473; p3_emc_s_sdp[1][0][3][0]=5.61264; p4_emc_s_sdp[1][0][3][0]=-5.39398; 
  p0_emc_m_sdp[0][0][4][0]=-0.120083; p1_emc_m_sdp[0][0][4][0]=0.317476; p2_emc_m_sdp[0][0][4][0]=-0.249759; p3_emc_m_sdp[0][0][4][0]=0.0803071; p4_emc_m_sdp[0][0][4][0]=-0.0084507; 
  p0_emc_s_sdp[0][0][4][0]=0.885676; p1_emc_s_sdp[0][0][4][0]=-0.107756; p2_emc_s_sdp[0][0][4][0]=0.116807; p3_emc_s_sdp[0][0][4][0]=-0.0411373; p4_emc_s_sdp[0][0][4][0]=0.00454201; 
  p0_emc_m_sdp[1][0][4][0]=0.57658; p1_emc_m_sdp[1][0][4][0]=-1.59974; p2_emc_m_sdp[1][0][4][0]=0.216149; p3_emc_m_sdp[1][0][4][0]=3.5705; p4_emc_m_sdp[1][0][4][0]=-3.27057; 
  p0_emc_s_sdp[1][0][4][0]=1.68709; p1_emc_s_sdp[1][0][4][0]=-2.84988; p2_emc_s_sdp[1][0][4][0]=0.381131; p3_emc_s_sdp[1][0][4][0]=7.23034; p4_emc_s_sdp[1][0][4][0]=-6.31778; 
  p0_emc_m_sdp[0][0][5][0]=-0.128723; p1_emc_m_sdp[0][0][5][0]=0.377472; p2_emc_m_sdp[0][0][5][0]=-0.316418; p3_emc_m_sdp[0][0][5][0]=0.104149; p4_emc_m_sdp[0][0][5][0]=-0.0111934; 
  p0_emc_s_sdp[0][0][5][0]=0.719619; p1_emc_s_sdp[0][0][5][0]=0.250294; p2_emc_s_sdp[0][0][5][0]=-0.124812; p3_emc_s_sdp[0][0][5][0]=0.0241515; p4_emc_s_sdp[0][0][5][0]=-0.00152266; 
  p0_emc_m_sdp[1][0][5][0]=0.549443; p1_emc_m_sdp[1][0][5][0]=-1.52785; p2_emc_m_sdp[1][0][5][0]=0.0514696; p3_emc_m_sdp[1][0][5][0]=3.7615; p4_emc_m_sdp[1][0][5][0]=-3.26865; 
  p0_emc_s_sdp[1][0][5][0]=0.803662; p1_emc_s_sdp[1][0][5][0]=0.317366; p2_emc_s_sdp[1][0][5][0]=-0.29413; p3_emc_s_sdp[1][0][5][0]=-0.534441; p4_emc_s_sdp[1][0][5][0]=0.648869; 
  p0_emc_m_sdp[0][0][6][0]=-0.102253; p1_emc_m_sdp[0][0][6][0]=0.332173; p2_emc_m_sdp[0][0][6][0]=-0.284945; p3_emc_m_sdp[0][0][6][0]=0.0947607; p4_emc_m_sdp[0][0][6][0]=-0.0102392; 
  p0_emc_s_sdp[0][0][6][0]=0.725468; p1_emc_s_sdp[0][0][6][0]=0.244775; p2_emc_s_sdp[0][0][6][0]=-0.125319; p3_emc_s_sdp[0][0][6][0]=0.0252153; p4_emc_s_sdp[0][0][6][0]=-0.00168211; 
  p0_emc_m_sdp[1][0][6][0]=0.452964; p1_emc_m_sdp[1][0][6][0]=-0.874213; p2_emc_m_sdp[1][0][6][0]=-0.472593; p3_emc_m_sdp[1][0][6][0]=1.601; p4_emc_m_sdp[1][0][6][0]=-0.541843; 
  p0_emc_s_sdp[1][0][6][0]=1.361; p1_emc_s_sdp[1][0][6][0]=-2.03984; p2_emc_s_sdp[1][0][6][0]=0.645042; p3_emc_s_sdp[1][0][6][0]=5.69632; p4_emc_s_sdp[1][0][6][0]=-5.72571; 
  p0_emc_m_sdp[0][0][7][0]=0.0174468; p1_emc_m_sdp[0][0][7][0]=0.050003; p2_emc_m_sdp[0][0][7][0]=-0.0817896; p3_emc_m_sdp[0][0][7][0]=0.0365804; p4_emc_m_sdp[0][0][7][0]=-0.00453195; 
  p0_emc_s_sdp[0][0][7][0]=0.667691; p1_emc_s_sdp[0][0][7][0]=0.41994; p2_emc_s_sdp[0][0][7][0]=-0.279961; p3_emc_s_sdp[0][0][7][0]=0.0769799; p4_emc_s_sdp[0][0][7][0]=-0.00747128; 
  p0_emc_m_sdp[1][0][7][0]=0.352165; p1_emc_m_sdp[1][0][7][0]=-0.771209; p2_emc_m_sdp[1][0][7][0]=-0.135229; p3_emc_m_sdp[1][0][7][0]=1.45944; p4_emc_m_sdp[1][0][7][0]=-1.01877; 
  p0_emc_s_sdp[1][0][7][0]=1.74028; p1_emc_s_sdp[1][0][7][0]=-3.32506; p2_emc_s_sdp[1][0][7][0]=0.748513; p3_emc_s_sdp[1][0][7][0]=9.36998; p4_emc_s_sdp[1][0][7][0]=-9.02782; 
  p0_emc_m_sdp[0][0][8][0]=0.0266741; p1_emc_m_sdp[0][0][8][0]=-0.00380049; p2_emc_m_sdp[0][0][8][0]=-0.0292333; p3_emc_m_sdp[0][0][8][0]=0.018641; p4_emc_m_sdp[0][0][8][0]=-0.00256028; 
  p0_emc_s_sdp[0][0][8][0]=0.720425; p1_emc_s_sdp[0][0][8][0]=0.301342; p2_emc_s_sdp[0][0][8][0]=-0.20154; p3_emc_s_sdp[0][0][8][0]=0.056189; p4_emc_s_sdp[0][0][8][0]=-0.00554559; 
  p0_emc_m_sdp[1][0][8][0]=0.19493; p1_emc_m_sdp[1][0][8][0]=-0.128368; p2_emc_m_sdp[1][0][8][0]=-0.437505; p3_emc_m_sdp[1][0][8][0]=-0.340618; p4_emc_m_sdp[1][0][8][0]=0.958846; 
  p0_emc_s_sdp[1][0][8][0]=2.28382; p1_emc_s_sdp[1][0][8][0]=-5.31817; p2_emc_s_sdp[1][0][8][0]=1.18353; p3_emc_s_sdp[1][0][8][0]=14.151; p4_emc_s_sdp[1][0][8][0]=-13.1954; 
  p0_emc_m_sdp[0][0][9][0]=-0.0137529; p1_emc_m_sdp[0][0][9][0]=0.0650673; p2_emc_m_sdp[0][0][9][0]=-0.0766055; p3_emc_m_sdp[0][0][9][0]=0.0330321; p4_emc_m_sdp[0][0][9][0]=-0.00415493; 
  p0_emc_s_sdp[0][0][9][0]=0.680779; p1_emc_s_sdp[0][0][9][0]=0.370412; p2_emc_s_sdp[0][0][9][0]=-0.252039; p3_emc_s_sdp[0][0][9][0]=0.0718685; p4_emc_s_sdp[0][0][9][0]=-0.00723659; 
  p0_emc_m_sdp[1][0][9][0]=0.156641; p1_emc_m_sdp[1][0][9][0]=-0.0925055; p2_emc_m_sdp[1][0][9][0]=-0.338812; p3_emc_m_sdp[1][0][9][0]=-0.280512; p4_emc_m_sdp[1][0][9][0]=0.694051; 
  p0_emc_s_sdp[1][0][9][0]=1.45272; p1_emc_s_sdp[1][0][9][0]=-2.41537; p2_emc_s_sdp[1][0][9][0]=0.50299; p3_emc_s_sdp[1][0][9][0]=7.53312; p4_emc_s_sdp[1][0][9][0]=-7.39688; 
  p0_emc_m_sdp[0][1][0][0]=0.0236688; p1_emc_m_sdp[0][1][0][0]=-0.0191591; p2_emc_m_sdp[0][1][0][0]=0.00572051; p3_emc_m_sdp[0][1][0][0]=0.00137154; p4_emc_m_sdp[0][1][0][0]=-0.00036977; 
  p0_emc_s_sdp[0][1][0][0]=1.01223; p1_emc_s_sdp[0][1][0][0]=-0.161158; p2_emc_s_sdp[0][1][0][0]=0.108475; p3_emc_s_sdp[0][1][0][0]=-0.0307185; p4_emc_s_sdp[0][1][0][0]=0.00299869; 
  p0_emc_m_sdp[1][1][0][0]=0.568823; p1_emc_m_sdp[1][1][0][0]=-2.21345; p2_emc_m_sdp[1][1][0][0]=0.605881; p3_emc_m_sdp[1][1][0][0]=6.00629; p4_emc_m_sdp[1][1][0][0]=-5.74173; 
  p0_emc_s_sdp[1][1][0][0]=0.853645; p1_emc_s_sdp[1][1][0][0]=0.357504; p2_emc_s_sdp[1][1][0][0]=-0.290619; p3_emc_s_sdp[1][1][0][0]=-0.559413; p4_emc_s_sdp[1][1][0][0]=0.704503; 
  p0_emc_m_sdp[0][1][1][0]=0.0220718; p1_emc_m_sdp[0][1][1][0]=-0.0110902; p2_emc_m_sdp[0][1][1][0]=-0.000896238; p3_emc_m_sdp[0][1][1][0]=0.00433583; p4_emc_m_sdp[0][1][1][0]=-0.000815702; 
  p0_emc_s_sdp[0][1][1][0]=0.977137; p1_emc_s_sdp[0][1][1][0]=-0.0659165; p2_emc_s_sdp[0][1][1][0]=0.0339981; p3_emc_s_sdp[0][1][1][0]=-0.00867738; p4_emc_s_sdp[0][1][1][0]=0.00076781; 
  p0_emc_m_sdp[1][1][1][0]=0.0953259; p1_emc_m_sdp[1][1][1][0]=-0.492113; p2_emc_m_sdp[1][1][1][0]=0.32078; p3_emc_m_sdp[1][1][1][0]=1.48966; p4_emc_m_sdp[1][1][1][0]=-1.74849; 
  p0_emc_s_sdp[1][1][1][0]=0.799688; p1_emc_s_sdp[1][1][1][0]=0.914325; p2_emc_s_sdp[1][1][1][0]=-0.804079; p3_emc_s_sdp[1][1][1][0]=-2.48823; p4_emc_s_sdp[1][1][1][0]=3.22153; 
  p0_emc_m_sdp[0][1][2][0]=0.0138719; p1_emc_m_sdp[0][1][2][0]=0.0100466; p2_emc_m_sdp[0][1][2][0]=-0.0152848; p3_emc_m_sdp[0][1][2][0]=0.00895671; p4_emc_m_sdp[0][1][2][0]=-0.0013072; 
  p0_emc_s_sdp[0][1][2][0]=0.99816; p1_emc_s_sdp[0][1][2][0]=-0.0951134; p2_emc_s_sdp[0][1][2][0]=0.0516094; p3_emc_s_sdp[0][1][2][0]=-0.0139578; p4_emc_s_sdp[0][1][2][0]=0.0012574; 
  p0_emc_m_sdp[1][1][2][0]=-0.236673; p1_emc_m_sdp[1][1][2][0]=1.0389; p2_emc_m_sdp[1][1][2][0]=-0.384922; p3_emc_m_sdp[1][1][2][0]=-3.00103; p4_emc_m_sdp[1][1][2][0]=3.15721; 
  p0_emc_s_sdp[1][1][2][0]=2.10525; p1_emc_s_sdp[1][1][2][0]=-4.41724; p2_emc_s_sdp[1][1][2][0]=0.96956; p3_emc_s_sdp[1][1][2][0]=12.2727; p4_emc_s_sdp[1][1][2][0]=-11.5795; 
  p0_emc_m_sdp[0][1][3][0]=0.00126166; p1_emc_m_sdp[0][1][3][0]=0.0418071; p2_emc_m_sdp[0][1][3][0]=-0.0417055; p3_emc_m_sdp[0][1][3][0]=0.0182191; p4_emc_m_sdp[0][1][3][0]=-0.0023796; 
  p0_emc_s_sdp[0][1][3][0]=0.956671; p1_emc_s_sdp[0][1][3][0]=0.00275928; p2_emc_s_sdp[0][1][3][0]=-0.0210145; p3_emc_s_sdp[0][1][3][0]=0.0067266; p4_emc_s_sdp[0][1][3][0]=-0.000749095; 
  p0_emc_m_sdp[1][1][3][0]=-0.189429; p1_emc_m_sdp[1][1][3][0]=0.983277; p2_emc_m_sdp[1][1][3][0]=-0.394362; p3_emc_m_sdp[1][1][3][0]=-3.08131; p4_emc_m_sdp[1][1][3][0]=3.23241; 
  p0_emc_s_sdp[1][1][3][0]=1.49401; p1_emc_s_sdp[1][1][3][0]=-1.8526; p2_emc_s_sdp[1][1][3][0]=0.0768461; p3_emc_s_sdp[1][1][3][0]=5.15362; p4_emc_s_sdp[1][1][3][0]=-4.39083; 
  p0_emc_m_sdp[0][1][4][0]=-0.0360229; p1_emc_m_sdp[0][1][4][0]=0.119082; p2_emc_m_sdp[0][1][4][0]=-0.0988686; p3_emc_m_sdp[0][1][4][0]=0.0357216; p4_emc_m_sdp[0][1][4][0]=-0.00420378; 
  p0_emc_s_sdp[0][1][4][0]=0.967341; p1_emc_s_sdp[0][1][4][0]=-0.0216949; p2_emc_s_sdp[0][1][4][0]=-0.00305017; p3_emc_s_sdp[0][1][4][0]=0.00121786; p4_emc_s_sdp[0][1][4][0]=-0.00017606; 
  p0_emc_m_sdp[1][1][4][0]=-0.244422; p1_emc_m_sdp[1][1][4][0]=1.27777; p2_emc_m_sdp[1][1][4][0]=-0.635057; p3_emc_m_sdp[1][1][4][0]=-3.55602; p4_emc_m_sdp[1][1][4][0]=3.75245; 
  p0_emc_s_sdp[1][1][4][0]=1.83731; p1_emc_s_sdp[1][1][4][0]=-3.06978; p2_emc_s_sdp[1][1][4][0]=0.270426; p3_emc_s_sdp[1][1][4][0]=8.21587; p4_emc_s_sdp[1][1][4][0]=-7.03131; 
  p0_emc_m_sdp[0][1][5][0]=-0.0492763; p1_emc_m_sdp[0][1][5][0]=0.15799; p2_emc_m_sdp[0][1][5][0]=-0.122607; p3_emc_m_sdp[0][1][5][0]=0.0404231; p4_emc_m_sdp[0][1][5][0]=-0.0044051; 
  p0_emc_s_sdp[0][1][5][0]=0.969046; p1_emc_s_sdp[0][1][5][0]=-0.0047234; p2_emc_s_sdp[0][1][5][0]=-0.0289807; p3_emc_s_sdp[0][1][5][0]=0.0114166; p4_emc_s_sdp[0][1][5][0]=-0.00125261; 
  p0_emc_m_sdp[1][1][5][0]=-0.236984; p1_emc_m_sdp[1][1][5][0]=1.39793; p2_emc_m_sdp[1][1][5][0]=-0.762262; p3_emc_m_sdp[1][1][5][0]=-4.1145; p4_emc_m_sdp[1][1][5][0]=4.47033; 
  p0_emc_s_sdp[1][1][5][0]=1.58407; p1_emc_s_sdp[1][1][5][0]=-2.1315; p2_emc_s_sdp[1][1][5][0]=0.0803064; p3_emc_s_sdp[1][1][5][0]=5.72861; p4_emc_s_sdp[1][1][5][0]=-4.76355; 
  p0_emc_m_sdp[0][1][6][0]=-0.00145482; p1_emc_m_sdp[0][1][6][0]=0.0687704; p2_emc_m_sdp[0][1][6][0]=-0.0670333; p3_emc_m_sdp[0][1][6][0]=0.0261111; p4_emc_m_sdp[0][1][6][0]=-0.00311847; 
  p0_emc_s_sdp[0][1][6][0]=0.934519; p1_emc_s_sdp[0][1][6][0]=0.0711855; p2_emc_s_sdp[0][1][6][0]=-0.084884; p3_emc_s_sdp[0][1][6][0]=0.028433; p4_emc_s_sdp[0][1][6][0]=-0.00307147; 
  p0_emc_m_sdp[1][1][6][0]=0.124217; p1_emc_m_sdp[1][1][6][0]=-0.0716231; p2_emc_m_sdp[1][1][6][0]=-0.2616; p3_emc_m_sdp[1][1][6][0]=-0.201009; p4_emc_m_sdp[1][1][6][0]=0.607845; 
  p0_emc_s_sdp[1][1][6][0]=1.40147; p1_emc_s_sdp[1][1][6][0]=-1.5188; p2_emc_s_sdp[1][1][6][0]=0.0371714; p3_emc_s_sdp[1][1][6][0]=4.31578; p4_emc_s_sdp[1][1][6][0]=-3.72823; 
  p0_emc_m_sdp[0][1][7][0]=0.0676219; p1_emc_m_sdp[0][1][7][0]=-0.0489624; p2_emc_m_sdp[0][1][7][0]=-0.00031661; p3_emc_m_sdp[0][1][7][0]=0.0104972; p4_emc_m_sdp[0][1][7][0]=-0.00186425; 
  p0_emc_s_sdp[0][1][7][0]=0.915416; p1_emc_s_sdp[0][1][7][0]=0.104283; p2_emc_s_sdp[0][1][7][0]=-0.101379; p3_emc_s_sdp[0][1][7][0]=0.0313859; p4_emc_s_sdp[0][1][7][0]=-0.00320831; 
  p0_emc_m_sdp[1][1][7][0]=0.0425017; p1_emc_m_sdp[1][1][7][0]=0.382024; p2_emc_m_sdp[1][1][7][0]=-0.51112; p3_emc_m_sdp[1][1][7][0]=-1.39733; p4_emc_m_sdp[1][1][7][0]=1.92621; 
  p0_emc_s_sdp[1][1][7][0]=1.35222; p1_emc_s_sdp[1][1][7][0]=-1.14698; p2_emc_s_sdp[1][1][7][0]=-0.120491; p3_emc_s_sdp[1][1][7][0]=2.95809; p4_emc_s_sdp[1][1][7][0]=-2.30112; 
  p0_emc_m_sdp[0][1][8][0]=0.00915868; p1_emc_m_sdp[0][1][8][0]=0.116507; p2_emc_m_sdp[0][1][8][0]=-0.137129; p3_emc_m_sdp[0][1][8][0]=0.0537898; p4_emc_m_sdp[0][1][8][0]=-0.00651386; 
  p0_emc_s_sdp[0][1][8][0]=0.883481; p1_emc_s_sdp[0][1][8][0]=0.159861; p2_emc_s_sdp[0][1][8][0]=-0.135503; p3_emc_s_sdp[0][1][8][0]=0.040265; p4_emc_s_sdp[0][1][8][0]=-0.00402976; 
  p0_emc_m_sdp[1][1][8][0]=0.322045; p1_emc_m_sdp[1][1][8][0]=-0.688862; p2_emc_m_sdp[1][1][8][0]=-0.0838281; p3_emc_m_sdp[1][1][8][0]=1.31875; p4_emc_m_sdp[1][1][8][0]=-0.880362; 
  p0_emc_s_sdp[1][1][8][0]=1.93221; p1_emc_s_sdp[1][1][8][0]=-3.44889; p2_emc_s_sdp[1][1][8][0]=0.585963; p3_emc_s_sdp[1][1][8][0]=9.30109; p4_emc_s_sdp[1][1][8][0]=-8.62618; 
  p0_emc_m_sdp[0][1][9][0]=-0.022386; p1_emc_m_sdp[0][1][9][0]=0.182785; p2_emc_m_sdp[0][1][9][0]=-0.180378; p3_emc_m_sdp[0][1][9][0]=0.0646112; p4_emc_m_sdp[0][1][9][0]=-0.00742567; 
  p0_emc_s_sdp[0][1][9][0]=0.837286; p1_emc_s_sdp[0][1][9][0]=0.259937; p2_emc_s_sdp[0][1][9][0]=-0.208103; p3_emc_s_sdp[0][1][9][0]=0.0612754; p4_emc_s_sdp[0][1][9][0]=-0.00604083; 
  p0_emc_m_sdp[1][1][9][0]=0.140947; p1_emc_m_sdp[1][1][9][0]=-0.0471819; p2_emc_m_sdp[1][1][9][0]=-0.238389; p3_emc_m_sdp[1][1][9][0]=-0.207371; p4_emc_m_sdp[1][1][9][0]=0.505043; 
  p0_emc_s_sdp[1][1][9][0]=1.90668; p1_emc_s_sdp[1][1][9][0]=-3.34259; p2_emc_s_sdp[1][1][9][0]=0.459481; p3_emc_s_sdp[1][1][9][0]=9.18653; p4_emc_s_sdp[1][1][9][0]=-8.40356; 
  p0_emc_m_sdp[0][2][0][0]=-0.0786853; p1_emc_m_sdp[0][2][0][0]=0.137164; p2_emc_m_sdp[0][2][0][0]=-0.0851418; p3_emc_m_sdp[0][2][0][0]=0.025176; p4_emc_m_sdp[0][2][0][0]=-0.00258154; 
  p0_emc_s_sdp[0][2][0][0]=1.02104; p1_emc_s_sdp[0][2][0][0]=-0.0552046; p2_emc_s_sdp[0][2][0][0]=0.0169127; p3_emc_s_sdp[0][2][0][0]=-0.00271735; p4_emc_s_sdp[0][2][0][0]=0.000201939; 
  p0_emc_m_sdp[1][2][0][0]=-0.431006; p1_emc_m_sdp[1][2][0][0]=1.89846; p2_emc_m_sdp[1][2][0][0]=-0.608404; p3_emc_m_sdp[1][2][0][0]=-5.42547; p4_emc_m_sdp[1][2][0][0]=5.25748; 
  p0_emc_s_sdp[1][2][0][0]=0.724229; p1_emc_s_sdp[1][2][0][0]=1.0216; p2_emc_s_sdp[1][2][0][0]=-0.473443; p3_emc_s_sdp[1][2][0][0]=-2.43163; p4_emc_s_sdp[1][2][0][0]=2.57595; 
  p0_emc_m_sdp[0][2][1][0]=-0.11116; p1_emc_m_sdp[0][2][1][0]=0.236459; p2_emc_m_sdp[0][2][1][0]=-0.16503; p3_emc_m_sdp[0][2][1][0]=0.049235; p4_emc_m_sdp[0][2][1][0]=-0.00507084; 
  p0_emc_s_sdp[0][2][1][0]=1.00564; p1_emc_s_sdp[0][2][1][0]=-0.0457506; p2_emc_s_sdp[0][2][1][0]=0.0130062; p3_emc_s_sdp[0][2][1][0]=-0.00232553; p4_emc_s_sdp[0][2][1][0]=0.000266185; 
  p0_emc_m_sdp[1][2][1][0]=-0.392744; p1_emc_m_sdp[1][2][1][0]=1.68727; p2_emc_m_sdp[1][2][1][0]=-0.467336; p3_emc_m_sdp[1][2][1][0]=-4.74079; p4_emc_m_sdp[1][2][1][0]=4.43617; 
  p0_emc_s_sdp[1][2][1][0]=0.744703; p1_emc_s_sdp[1][2][1][0]=0.943209; p2_emc_s_sdp[1][2][1][0]=-0.477493; p3_emc_s_sdp[1][2][1][0]=-2.1934; p4_emc_s_sdp[1][2][1][0]=2.356; 
  p0_emc_m_sdp[0][2][2][0]=-0.090019; p1_emc_m_sdp[0][2][2][0]=0.189566; p2_emc_m_sdp[0][2][2][0]=-0.128362; p3_emc_m_sdp[0][2][2][0]=0.037478; p4_emc_m_sdp[0][2][2][0]=-0.00372225; 
  p0_emc_s_sdp[0][2][2][0]=0.977295; p1_emc_s_sdp[0][2][2][0]=0.0218712; p2_emc_s_sdp[0][2][2][0]=-0.0428179; p3_emc_s_sdp[0][2][2][0]=0.0158783; p4_emc_s_sdp[0][2][2][0]=-0.00175487; 
  p0_emc_m_sdp[1][2][2][0]=-0.0265559; p1_emc_m_sdp[1][2][2][0]=0.0639718; p2_emc_m_sdp[1][2][2][0]=0.132298; p3_emc_m_sdp[1][2][2][0]=0.037699; p4_emc_m_sdp[1][2][2][0]=-0.496977; 
  p0_emc_s_sdp[1][2][2][0]=0.948617; p1_emc_s_sdp[1][2][2][0]=0.0939138; p2_emc_s_sdp[1][2][2][0]=-0.155213; p3_emc_s_sdp[1][2][2][0]=0.278169; p4_emc_s_sdp[1][2][2][0]=-0.271842; 
  p0_emc_m_sdp[0][2][3][0]=-0.0586394; p1_emc_m_sdp[0][2][3][0]=0.15421; p2_emc_m_sdp[0][2][3][0]=-0.113051; p3_emc_m_sdp[0][2][3][0]=0.0345026; p4_emc_m_sdp[0][2][3][0]=-0.00344756; 
  p0_emc_s_sdp[0][2][3][0]=0.953565; p1_emc_s_sdp[0][2][3][0]=0.078774; p2_emc_s_sdp[0][2][3][0]=-0.0905777; p3_emc_s_sdp[0][2][3][0]=0.033632; p4_emc_s_sdp[0][2][3][0]=-0.00427806; 
  p0_emc_m_sdp[1][2][3][0]=0.331448; p1_emc_m_sdp[1][2][3][0]=-1.16089; p2_emc_m_sdp[1][2][3][0]=0.29385; p3_emc_m_sdp[1][2][3][0]=3.12385; p4_emc_m_sdp[1][2][3][0]=-3.08655; 
  p0_emc_s_sdp[1][2][3][0]=0.72041; p1_emc_s_sdp[1][2][3][0]=0.970103; p2_emc_s_sdp[1][2][3][0]=-0.346483; p3_emc_s_sdp[1][2][3][0]=-2.02026; p4_emc_s_sdp[1][2][3][0]=1.81352; 
  p0_emc_m_sdp[0][2][4][0]=-0.0171874; p1_emc_m_sdp[0][2][4][0]=0.0553765; p2_emc_m_sdp[0][2][4][0]=-0.0338667; p3_emc_m_sdp[0][2][4][0]=0.010421; p4_emc_m_sdp[0][2][4][0]=-0.00102406; 
  p0_emc_s_sdp[0][2][4][0]=0.935567; p1_emc_s_sdp[0][2][4][0]=0.109124; p2_emc_s_sdp[0][2][4][0]=-0.115933; p3_emc_s_sdp[0][2][4][0]=0.0409851; p4_emc_s_sdp[0][2][4][0]=-0.00483406; 
  p0_emc_m_sdp[1][2][4][0]=0.206394; p1_emc_m_sdp[1][2][4][0]=-0.748651; p2_emc_m_sdp[1][2][4][0]=0.202646; p3_emc_m_sdp[1][2][4][0]=2.23965; p4_emc_m_sdp[1][2][4][0]=-2.31425; 
  p0_emc_s_sdp[1][2][4][0]=0.600809; p1_emc_s_sdp[1][2][4][0]=1.46182; p2_emc_s_sdp[1][2][4][0]=-0.522684; p3_emc_s_sdp[1][2][4][0]=-3.58894; p4_emc_s_sdp[1][2][4][0]=3.52641; 
  p0_emc_m_sdp[0][2][5][0]=-0.0192462; p1_emc_m_sdp[0][2][5][0]=0.0424048; p2_emc_m_sdp[0][2][5][0]=-0.0247136; p3_emc_m_sdp[0][2][5][0]=0.00741515; p4_emc_m_sdp[0][2][5][0]=-0.000603744; 
  p0_emc_s_sdp[0][2][5][0]=0.896457; p1_emc_s_sdp[0][2][5][0]=0.216408; p2_emc_s_sdp[0][2][5][0]=-0.187141; p3_emc_s_sdp[0][2][5][0]=0.0595863; p4_emc_s_sdp[0][2][5][0]=-0.00654804; 
  p0_emc_m_sdp[1][2][5][0]=0.960561; p1_emc_m_sdp[1][2][5][0]=-3.67722; p2_emc_m_sdp[1][2][5][0]=0.919395; p3_emc_m_sdp[1][2][5][0]=9.90517; p4_emc_m_sdp[1][2][5][0]=-9.39774; 
  p0_emc_s_sdp[1][2][5][0]=0.410415; p1_emc_s_sdp[1][2][5][0]=2.34408; p2_emc_s_sdp[1][2][5][0]=-0.876791; p3_emc_s_sdp[1][2][5][0]=-6.24561; p4_emc_s_sdp[1][2][5][0]=6.30917; 
  p0_emc_m_sdp[0][2][6][0]=-0.0777799; p1_emc_m_sdp[0][2][6][0]=0.181379; p2_emc_m_sdp[0][2][6][0]=-0.133671; p3_emc_m_sdp[0][2][6][0]=0.0417923; p4_emc_m_sdp[0][2][6][0]=-0.00434982; 
  p0_emc_s_sdp[0][2][6][0]=0.92633; p1_emc_s_sdp[0][2][6][0]=0.157304; p2_emc_s_sdp[0][2][6][0]=-0.144271; p3_emc_s_sdp[0][2][6][0]=0.0460479; p4_emc_s_sdp[0][2][6][0]=-0.00504391; 
  p0_emc_m_sdp[1][2][6][0]=0.631797; p1_emc_m_sdp[1][2][6][0]=-2.37781; p2_emc_m_sdp[1][2][6][0]=0.547302; p3_emc_m_sdp[1][2][6][0]=6.3807; p4_emc_m_sdp[1][2][6][0]=-5.99489; 
  p0_emc_s_sdp[1][2][6][0]=0.716511; p1_emc_s_sdp[1][2][6][0]=1.07585; p2_emc_s_sdp[1][2][6][0]=-0.466709; p3_emc_s_sdp[1][2][6][0]=-2.60358; p4_emc_s_sdp[1][2][6][0]=2.63682; 
  p0_emc_m_sdp[0][2][7][0]=-0.0906247; p1_emc_m_sdp[0][2][7][0]=0.195007; p2_emc_m_sdp[0][2][7][0]=-0.137973; p3_emc_m_sdp[0][2][7][0]=0.0420369; p4_emc_m_sdp[0][2][7][0]=-0.00433136; 
  p0_emc_s_sdp[0][2][7][0]=0.887084; p1_emc_s_sdp[0][2][7][0]=0.235735; p2_emc_s_sdp[0][2][7][0]=-0.202965; p3_emc_s_sdp[0][2][7][0]=0.0644561; p4_emc_s_sdp[0][2][7][0]=-0.0070822; 
  p0_emc_m_sdp[1][2][7][0]=0.8161; p1_emc_m_sdp[1][2][7][0]=-3.10425; p2_emc_m_sdp[1][2][7][0]=0.776093; p3_emc_m_sdp[1][2][7][0]=8.32745; p4_emc_m_sdp[1][2][7][0]=-7.93435; 
  p0_emc_s_sdp[1][2][7][0]=0.665019; p1_emc_s_sdp[1][2][7][0]=1.29701; p2_emc_s_sdp[1][2][7][0]=-0.553201; p3_emc_s_sdp[1][2][7][0]=-3.22578; p4_emc_s_sdp[1][2][7][0]=3.26343; 
  p0_emc_m_sdp[0][2][8][0]=-0.107242; p1_emc_m_sdp[0][2][8][0]=0.228197; p2_emc_m_sdp[0][2][8][0]=-0.162486; p3_emc_m_sdp[0][2][8][0]=0.0492273; p4_emc_m_sdp[0][2][8][0]=-0.00506221; 
  p0_emc_s_sdp[0][2][8][0]=0.891148; p1_emc_s_sdp[0][2][8][0]=0.206391; p2_emc_s_sdp[0][2][8][0]=-0.17525; p3_emc_s_sdp[0][2][8][0]=0.0554652; p4_emc_s_sdp[0][2][8][0]=-0.00615111; 
  p0_emc_m_sdp[1][2][8][0]=0.384839; p1_emc_m_sdp[1][2][8][0]=-1.40548; p2_emc_m_sdp[1][2][8][0]=0.308312; p3_emc_m_sdp[1][2][8][0]=3.65891; p4_emc_m_sdp[1][2][8][0]=-3.42476; 
  p0_emc_s_sdp[1][2][8][0]=0.970352; p1_emc_s_sdp[1][2][8][0]=0.10693; p2_emc_s_sdp[1][2][8][0]=-0.299035; p3_emc_s_sdp[1][2][8][0]=0.0350827; p4_emc_s_sdp[1][2][8][0]=0.238067; 
  p0_emc_m_sdp[0][2][9][0]=-0.0751534; p1_emc_m_sdp[0][2][9][0]=0.146516; p2_emc_m_sdp[0][2][9][0]=-0.0969682; p3_emc_m_sdp[0][2][9][0]=0.0286963; p4_emc_m_sdp[0][2][9][0]=-0.0028594; 
  p0_emc_s_sdp[0][2][9][0]=0.873587; p1_emc_s_sdp[0][2][9][0]=0.222483; p2_emc_s_sdp[0][2][9][0]=-0.18335; p3_emc_s_sdp[0][2][9][0]=0.0580992; p4_emc_s_sdp[0][2][9][0]=-0.00640249; 
  p0_emc_m_sdp[1][2][9][0]=-0.044695; p1_emc_m_sdp[1][2][9][0]=0.323239; p2_emc_m_sdp[1][2][9][0]=-0.203846; p3_emc_m_sdp[1][2][9][0]=-1.09858; p4_emc_m_sdp[1][2][9][0]=1.22217; 
  p0_emc_s_sdp[1][2][9][0]=0.428734; p1_emc_s_sdp[1][2][9][0]=2.2117; p2_emc_s_sdp[1][2][9][0]=-0.858905; p3_emc_s_sdp[1][2][9][0]=-5.76454; p4_emc_s_sdp[1][2][9][0]=5.82141; 

  //! EMC sdphi pos
  p0_emc_m_sdp[0][0][0][1]=0.04793; p1_emc_m_sdp[0][0][0][1]=-0.155869; p2_emc_m_sdp[0][0][0][1]=0.137338; p3_emc_m_sdp[0][0][0][1]=-0.05068; p4_emc_m_sdp[0][0][0][1]=0.00608113; 
  p0_emc_s_sdp[0][0][0][1]=0.725184; p1_emc_s_sdp[0][0][0][1]=0.377428; p2_emc_s_sdp[0][0][0][1]=-0.289322; p3_emc_s_sdp[0][0][0][1]=0.0899182; p4_emc_s_sdp[0][0][0][1]=-0.00973454; 
  p0_emc_m_sdp[1][0][0][1]=0.0280618; p1_emc_m_sdp[1][0][0][1]=-0.353602; p2_emc_m_sdp[1][0][0][1]=0.173909; p3_emc_m_sdp[1][0][0][1]=1.38799; p4_emc_m_sdp[1][0][0][1]=-1.48372; 
  p0_emc_s_sdp[1][0][0][1]=1.04451; p1_emc_s_sdp[1][0][0][1]=-0.605364; p2_emc_s_sdp[1][0][0][1]=-0.0244755; p3_emc_s_sdp[1][0][0][1]=1.95847; p4_emc_s_sdp[1][0][0][1]=-1.70285; 
  p0_emc_m_sdp[0][0][1][1]=0.046129; p1_emc_m_sdp[0][0][1][1]=-0.154823; p2_emc_m_sdp[0][0][1][1]=0.13732; p3_emc_m_sdp[0][0][1][1]=-0.0508407; p4_emc_m_sdp[0][0][1][1]=0.00602938; 
  p0_emc_s_sdp[0][0][1][1]=0.737997; p1_emc_s_sdp[0][0][1][1]=0.356943; p2_emc_s_sdp[0][0][1][1]=-0.27464; p3_emc_s_sdp[0][0][1][1]=0.0843508; p4_emc_s_sdp[0][0][1][1]=-0.00904931; 
  p0_emc_m_sdp[1][0][1][1]=0.057286; p1_emc_m_sdp[1][0][1][1]=-0.593571; p2_emc_m_sdp[1][0][1][1]=0.430299; p3_emc_m_sdp[1][0][1][1]=1.96624; p4_emc_m_sdp[1][0][1][1]=-2.29503; 
  p0_emc_s_sdp[1][0][1][1]=1.17658; p1_emc_s_sdp[1][0][1][1]=-0.91561; p2_emc_s_sdp[1][0][1][1]=-0.222494; p3_emc_s_sdp[1][0][1][1]=2.75961; p4_emc_s_sdp[1][0][1][1]=-2.03533; 
  p0_emc_m_sdp[0][0][2][1]=0.0709624; p1_emc_m_sdp[0][0][2][1]=-0.221212; p2_emc_m_sdp[0][0][2][1]=0.193442; p3_emc_m_sdp[0][0][2][1]=-0.0696095; p4_emc_m_sdp[0][0][2][1]=0.00813084; 
  p0_emc_s_sdp[0][0][2][1]=0.746251; p1_emc_s_sdp[0][0][2][1]=0.327644; p2_emc_s_sdp[0][0][2][1]=-0.244144; p3_emc_s_sdp[0][0][2][1]=0.0720902; p4_emc_s_sdp[0][0][2][1]=-0.00749018; 
  p0_emc_m_sdp[1][0][2][1]=0.0200846; p1_emc_m_sdp[1][0][2][1]=-0.463638; p2_emc_m_sdp[1][0][2][1]=0.353592; p3_emc_m_sdp[1][0][2][1]=1.83457; p4_emc_m_sdp[1][0][2][1]=-2.2054; 
  p0_emc_s_sdp[1][0][2][1]=1.38489; p1_emc_s_sdp[1][0][2][1]=-1.70759; p2_emc_s_sdp[1][0][2][1]=0.0316259; p3_emc_s_sdp[1][0][2][1]=4.73143; p4_emc_s_sdp[1][0][2][1]=-3.96351; 
  p0_emc_m_sdp[0][0][3][1]=0.110955; p1_emc_m_sdp[0][0][3][1]=-0.308015; p2_emc_m_sdp[0][0][3][1]=0.252584; p3_emc_m_sdp[0][0][3][1]=-0.0851459; p4_emc_m_sdp[0][0][3][1]=0.00947906; 
  p0_emc_s_sdp[0][0][3][1]=0.777561; p1_emc_s_sdp[0][0][3][1]=0.240063; p2_emc_s_sdp[0][0][3][1]=-0.167527; p3_emc_s_sdp[0][0][3][1]=0.0460963; p4_emc_s_sdp[0][0][3][1]=-0.00455388; 
  p0_emc_m_sdp[1][0][3][1]=-0.236369; p1_emc_m_sdp[1][0][3][1]=0.429815; p2_emc_m_sdp[1][0][3][1]=0.265851; p3_emc_m_sdp[1][0][3][1]=-0.654432; p4_emc_m_sdp[1][0][3][1]=0.028476; 
  p0_emc_s_sdp[1][0][3][1]=1.216; p1_emc_s_sdp[1][0][3][1]=-1.08463; p2_emc_s_sdp[1][0][3][1]=-0.0993853; p3_emc_s_sdp[1][0][3][1]=3.14644; p4_emc_s_sdp[1][0][3][1]=-2.49749; 
  p0_emc_m_sdp[0][0][4][1]=0.0737807; p1_emc_m_sdp[0][0][4][1]=-0.22654; p2_emc_m_sdp[0][0][4][1]=0.199741; p3_emc_m_sdp[0][0][4][1]=-0.0729376; p4_emc_m_sdp[0][0][4][1]=0.00862132; 
  p0_emc_s_sdp[0][0][4][1]=0.775299; p1_emc_s_sdp[0][0][4][1]=0.23578; p2_emc_s_sdp[0][0][4][1]=-0.158117; p3_emc_s_sdp[0][0][4][1]=0.0405873; p4_emc_s_sdp[0][0][4][1]=-0.00371588; 
  p0_emc_m_sdp[1][0][4][1]=0.355035; p1_emc_m_sdp[1][0][4][1]=-1.81307; p2_emc_m_sdp[1][0][4][1]=0.79654; p3_emc_m_sdp[1][0][4][1]=5.33305; p4_emc_m_sdp[1][0][4][1]=-5.57542; 
  p0_emc_s_sdp[1][0][4][1]=0.954289; p1_emc_s_sdp[1][0][4][1]=-0.124289; p2_emc_s_sdp[1][0][4][1]=-0.256404; p3_emc_s_sdp[1][0][4][1]=0.467777; p4_emc_s_sdp[1][0][4][1]=-0.0541522; 
  p0_emc_m_sdp[0][0][5][1]=0.0403598; p1_emc_m_sdp[0][0][5][1]=-0.169943; p2_emc_m_sdp[0][0][5][1]=0.167384; p3_emc_m_sdp[0][0][5][1]=-0.0646984; p4_emc_m_sdp[0][0][5][1]=0.00781637; 
  p0_emc_s_sdp[0][0][5][1]=0.688802; p1_emc_s_sdp[0][0][5][1]=0.427179; p2_emc_s_sdp[0][0][5][1]=-0.311467; p3_emc_s_sdp[0][0][5][1]=0.0891616; p4_emc_s_sdp[0][0][5][1]=-0.00886077; 
  p0_emc_m_sdp[1][0][5][1]=-1.16927; p1_emc_m_sdp[1][0][5][1]=4.56033; p2_emc_m_sdp[1][0][5][1]=-1.39044; p3_emc_m_sdp[1][0][5][1]=-12.591; p4_emc_m_sdp[1][0][5][1]=12.4179; 
  p0_emc_s_sdp[1][0][5][1]=0.387659; p1_emc_s_sdp[1][0][5][1]=1.89227; p2_emc_s_sdp[1][0][5][1]=-0.470309; p3_emc_s_sdp[1][0][5][1]=-5.14321; p4_emc_s_sdp[1][0][5][1]=4.77755; 
  p0_emc_m_sdp[0][0][6][1]=0.079142; p1_emc_m_sdp[0][0][6][1]=-0.245329; p2_emc_m_sdp[0][0][6][1]=0.213225; p3_emc_m_sdp[0][0][6][1]=-0.0764179; p4_emc_m_sdp[0][0][6][1]=0.00887205; 
  p0_emc_s_sdp[0][0][6][1]=0.688541; p1_emc_s_sdp[0][0][6][1]=0.427869; p2_emc_s_sdp[0][0][6][1]=-0.318631; p3_emc_s_sdp[0][0][6][1]=0.0934524; p4_emc_s_sdp[0][0][6][1]=-0.00952513; 
  p0_emc_m_sdp[1][0][6][1]=-0.682295; p1_emc_m_sdp[1][0][6][1]=2.56938; p2_emc_m_sdp[1][0][6][1]=-0.80971; p3_emc_m_sdp[1][0][6][1]=-6.90395; p4_emc_m_sdp[1][0][6][1]=6.83689; 
  p0_emc_s_sdp[1][0][6][1]=0.762994; p1_emc_s_sdp[1][0][6][1]=0.365339; p2_emc_s_sdp[1][0][6][1]=-0.190515; p3_emc_s_sdp[1][0][6][1]=-0.485588; p4_emc_s_sdp[1][0][6][1]=0.420351; 
  p0_emc_m_sdp[0][0][7][1]=0.0126406; p1_emc_m_sdp[0][0][7][1]=-0.0975904; p2_emc_m_sdp[0][0][7][1]=0.0971812; p3_emc_m_sdp[0][0][7][1]=-0.0410057; p4_emc_m_sdp[0][0][7][1]=0.0052068; 
  p0_emc_s_sdp[0][0][7][1]=0.682391; p1_emc_s_sdp[0][0][7][1]=0.475862; p2_emc_s_sdp[0][0][7][1]=-0.375387; p3_emc_s_sdp[0][0][7][1]=0.116038; p4_emc_s_sdp[0][0][7][1]=-0.0123535; 
  p0_emc_m_sdp[1][0][7][1]=-0.407724; p1_emc_m_sdp[1][0][7][1]=1.33121; p2_emc_m_sdp[1][0][7][1]=-0.330429; p3_emc_m_sdp[1][0][7][1]=-3.41551; p4_emc_m_sdp[1][0][7][1]=3.28909; 
  p0_emc_s_sdp[1][0][7][1]=1.77564; p1_emc_s_sdp[1][0][7][1]=-3.41957; p2_emc_s_sdp[1][0][7][1]=0.664818; p3_emc_s_sdp[1][0][7][1]=9.33491; p4_emc_s_sdp[1][0][7][1]=-8.53935; 
  p0_emc_m_sdp[0][0][8][1]=-0.0176543; p1_emc_m_sdp[0][0][8][1]=-0.0118994; p2_emc_m_sdp[0][0][8][1]=0.0157643; p3_emc_m_sdp[0][0][8][1]=-0.00922912; p4_emc_m_sdp[0][0][8][1]=0.000959409; 
  p0_emc_s_sdp[0][0][8][1]=0.69414; p1_emc_s_sdp[0][0][8][1]=0.44065; p2_emc_s_sdp[0][0][8][1]=-0.342723; p3_emc_s_sdp[0][0][8][1]=0.105839; p4_emc_s_sdp[0][0][8][1]=-0.011412; 
  p0_emc_m_sdp[1][0][8][1]=0.0779497; p1_emc_m_sdp[1][0][8][1]=-0.428721; p2_emc_m_sdp[1][0][8][1]=0.0101261; p3_emc_m_sdp[1][0][8][1]=1.26953; p4_emc_m_sdp[1][0][8][1]=-1.01937; 
  p0_emc_s_sdp[1][0][8][1]=0.295785; p1_emc_s_sdp[1][0][8][1]=2.48251; p2_emc_s_sdp[1][0][8][1]=-1.03518; p3_emc_s_sdp[1][0][8][1]=-6.79404; p4_emc_s_sdp[1][0][8][1]=7.10355; 
  p0_emc_m_sdp[0][0][9][1]=-0.0379021; p1_emc_m_sdp[0][0][9][1]=0.0213401; p2_emc_m_sdp[0][0][9][1]=0.00186315; p3_emc_m_sdp[0][0][9][1]=-0.0111517; p4_emc_m_sdp[0][0][9][1]=0.00227101; 
  p0_emc_s_sdp[0][0][9][1]=0.695839; p1_emc_s_sdp[0][0][9][1]=0.426115; p2_emc_s_sdp[0][0][9][1]=-0.326488; p3_emc_s_sdp[0][0][9][1]=0.0984978; p4_emc_s_sdp[0][0][9][1]=-0.010104; 
  p0_emc_m_sdp[1][0][9][1]=0.329728; p1_emc_m_sdp[1][0][9][1]=-1.24193; p2_emc_m_sdp[1][0][9][1]=0.0949456; p3_emc_m_sdp[1][0][9][1]=3.1772; p4_emc_m_sdp[1][0][9][1]=-2.61641; 
  p0_emc_s_sdp[1][0][9][1]=0.365149; p1_emc_s_sdp[1][0][9][1]=2.35109; p2_emc_s_sdp[1][0][9][1]=-1.27599; p3_emc_s_sdp[1][0][9][1]=-6.61828; p4_emc_s_sdp[1][0][9][1]=7.48567; 
  p0_emc_m_sdp[0][1][0][1]=-0.142281; p1_emc_m_sdp[0][1][0][1]=0.301794; p2_emc_m_sdp[0][1][0][1]=-0.212351; p3_emc_m_sdp[0][1][0][1]=0.0602925; p4_emc_m_sdp[0][1][0][1]=-0.00617988; 
  p0_emc_s_sdp[0][1][0][1]=1.01806; p1_emc_s_sdp[0][1][0][1]=-0.151417; p2_emc_s_sdp[0][1][0][1]=0.127077; p3_emc_s_sdp[0][1][0][1]=-0.0441586; p4_emc_s_sdp[0][1][0][1]=0.00511386; 
  p0_emc_m_sdp[1][1][0][1]=0.338967; p1_emc_m_sdp[1][1][0][1]=-1.24399; p2_emc_m_sdp[1][1][0][1]=0.257511; p3_emc_m_sdp[1][1][0][1]=3.38233; p4_emc_m_sdp[1][1][0][1]=-3.19342; 
  p0_emc_s_sdp[1][1][0][1]=0.778927; p1_emc_s_sdp[1][1][0][1]=0.448311; p2_emc_s_sdp[1][1][0][1]=-0.0738332; p3_emc_s_sdp[1][1][0][1]=-0.439138; p4_emc_s_sdp[1][1][0][1]=0.169889; 
  p0_emc_m_sdp[0][1][1][1]=-0.116086; p1_emc_m_sdp[0][1][1][1]=0.222181; p2_emc_m_sdp[0][1][1][1]=-0.145644; p3_emc_m_sdp[0][1][1][1]=0.0385235; p4_emc_m_sdp[0][1][1][1]=-0.00371794; 
  p0_emc_s_sdp[0][1][1][1]=1.04718; p1_emc_s_sdp[0][1][1][1]=-0.201862; p2_emc_s_sdp[0][1][1][1]=0.16189; p3_emc_s_sdp[0][1][1][1]=-0.0546388; p4_emc_s_sdp[0][1][1][1]=0.00621847; 
  p0_emc_m_sdp[1][1][1][1]=0.236544; p1_emc_m_sdp[1][1][1][1]=-1.10614; p2_emc_m_sdp[1][1][1][1]=0.486188; p3_emc_m_sdp[1][1][1][1]=3.27957; p4_emc_m_sdp[1][1][1][1]=-3.53512; 
  p0_emc_s_sdp[1][1][1][1]=0.713673; p1_emc_s_sdp[1][1][1][1]=0.880409; p2_emc_s_sdp[1][1][1][1]=-0.413871; p3_emc_s_sdp[1][1][1][1]=-1.89275; p4_emc_s_sdp[1][1][1][1]=2.05354; 
  p0_emc_m_sdp[0][1][2][1]=-0.0480399; p1_emc_m_sdp[0][1][2][1]=0.0561006; p2_emc_m_sdp[0][1][2][1]=-0.0161652; p3_emc_m_sdp[0][1][2][1]=-0.001821; p4_emc_m_sdp[0][1][2][1]=0.00058748; 
  p0_emc_s_sdp[0][1][2][1]=1.04666; p1_emc_s_sdp[0][1][2][1]=-0.189209; p2_emc_s_sdp[0][1][2][1]=0.153682; p3_emc_s_sdp[0][1][2][1]=-0.0525718; p4_emc_s_sdp[0][1][2][1]=0.00594427; 
  p0_emc_m_sdp[1][1][2][1]=-0.179542; p1_emc_m_sdp[1][1][2][1]=0.471487; p2_emc_m_sdp[1][1][2][1]=0.0882772; p3_emc_m_sdp[1][1][2][1]=-1.08962; p4_emc_m_sdp[1][1][2][1]=0.717034; 
  p0_emc_s_sdp[1][1][2][1]=0.662058; p1_emc_s_sdp[1][1][2][1]=1.14316; p2_emc_s_sdp[1][1][2][1]=-0.51659; p3_emc_s_sdp[1][1][2][1]=-2.7121; p4_emc_s_sdp[1][1][2][1]=2.93829; 
  p0_emc_m_sdp[0][1][3][1]=-0.000619262; p1_emc_m_sdp[0][1][3][1]=-0.0529853; p2_emc_m_sdp[0][1][3][1]=0.0626459; p3_emc_m_sdp[0][1][3][1]=-0.0251079; p4_emc_m_sdp[0][1][3][1]=0.00300208; 
  p0_emc_s_sdp[0][1][3][1]=1.10864; p1_emc_s_sdp[0][1][3][1]=-0.336439; p2_emc_s_sdp[0][1][3][1]=0.260875; p3_emc_s_sdp[0][1][3][1]=-0.0832701; p4_emc_s_sdp[0][1][3][1]=0.00896673; 
  p0_emc_m_sdp[1][1][3][1]=-0.646324; p1_emc_m_sdp[1][1][3][1]=2.29952; p2_emc_m_sdp[1][1][3][1]=-0.417219; p3_emc_m_sdp[1][1][3][1]=-6.09839; p4_emc_m_sdp[1][1][3][1]=5.57779; 
  p0_emc_s_sdp[1][1][3][1]=1.03932; p1_emc_s_sdp[1][1][3][1]=-0.19644; p2_emc_s_sdp[1][1][3][1]=-0.221174; p3_emc_s_sdp[1][1][3][1]=0.949574; p4_emc_s_sdp[1][1][3][1]=-0.624915; 
  p0_emc_m_sdp[0][1][4][1]=0.0138671; p1_emc_m_sdp[0][1][4][1]=-0.0906067; p2_emc_m_sdp[0][1][4][1]=0.0907161; p3_emc_m_sdp[0][1][4][1]=-0.0334857; p4_emc_m_sdp[0][1][4][1]=0.00386517; 
  p0_emc_s_sdp[0][1][4][1]=1.02077; p1_emc_s_sdp[0][1][4][1]=-0.106604; p2_emc_s_sdp[0][1][4][1]=0.0920101; p3_emc_s_sdp[0][1][4][1]=-0.0347057; p4_emc_s_sdp[0][1][4][1]=0.004094; 
  p0_emc_m_sdp[1][1][4][1]=-0.388817; p1_emc_m_sdp[1][1][4][1]=1.21265; p2_emc_m_sdp[1][1][4][1]=-0.0577632; p3_emc_m_sdp[1][1][4][1]=-3.07816; p4_emc_m_sdp[1][1][4][1]=2.57564; 
  p0_emc_s_sdp[1][1][4][1]=0.856461; p1_emc_s_sdp[1][1][4][1]=0.38314; p2_emc_s_sdp[1][1][4][1]=-0.266193; p3_emc_s_sdp[1][1][4][1]=-0.552035; p4_emc_s_sdp[1][1][4][1]=0.72755; 
  p0_emc_m_sdp[0][1][5][1]=-0.0603104; p1_emc_m_sdp[0][1][5][1]=0.0836629; p2_emc_m_sdp[0][1][5][1]=-0.0441823; p3_emc_m_sdp[0][1][5][1]=0.00795885; p4_emc_m_sdp[0][1][5][1]=-0.000474344; 
  p0_emc_s_sdp[0][1][5][1]=0.958827; p1_emc_s_sdp[0][1][5][1]=0.00107974; p2_emc_s_sdp[0][1][5][1]=0.0257983; p3_emc_s_sdp[0][1][5][1]=-0.0185317; p4_emc_s_sdp[0][1][5][1]=0.00285977; 
  p0_emc_m_sdp[1][1][5][1]=-0.0641504; p1_emc_m_sdp[1][1][5][1]=0.0436832; p2_emc_m_sdp[1][1][5][1]=0.148734; p3_emc_m_sdp[1][1][5][1]=0.109296; p4_emc_m_sdp[1][1][5][1]=-0.363229; 
  p0_emc_s_sdp[1][1][5][1]=0.748265; p1_emc_s_sdp[1][1][5][1]=0.726305; p2_emc_s_sdp[1][1][5][1]=-0.206916; p3_emc_s_sdp[1][1][5][1]=-1.43401; p4_emc_s_sdp[1][1][5][1]=1.23512; 
  p0_emc_m_sdp[0][1][6][1]=-0.0667324; p1_emc_m_sdp[0][1][6][1]=0.0855659; p2_emc_m_sdp[0][1][6][1]=-0.0453512; p3_emc_m_sdp[0][1][6][1]=0.00897386; p4_emc_m_sdp[0][1][6][1]=-0.000687116; 
  p0_emc_s_sdp[0][1][6][1]=0.950993; p1_emc_s_sdp[0][1][6][1]=0.00374336; p2_emc_s_sdp[0][1][6][1]=0.0247197; p3_emc_s_sdp[0][1][6][1]=-0.0189082; p4_emc_s_sdp[0][1][6][1]=0.00300624; 
  p0_emc_m_sdp[1][1][6][1]=-0.456351; p1_emc_m_sdp[1][1][6][1]=1.5058; p2_emc_m_sdp[1][1][6][1]=-0.0896416; p3_emc_m_sdp[1][1][6][1]=-3.7255; p4_emc_m_sdp[1][1][6][1]=2.95348; 
  p0_emc_s_sdp[1][1][6][1]=1.14607; p1_emc_s_sdp[1][1][6][1]=-0.865317; p2_emc_s_sdp[1][1][6][1]=0.211843; p3_emc_s_sdp[1][1][6][1]=2.75978; p4_emc_s_sdp[1][1][6][1]=-2.64495; 
  p0_emc_m_sdp[0][1][7][1]=-0.112505; p1_emc_m_sdp[0][1][7][1]=0.243546; p2_emc_m_sdp[0][1][7][1]=-0.18803; p3_emc_m_sdp[0][1][7][1]=0.058469; p4_emc_m_sdp[0][1][7][1]=-0.00653155; 
  p0_emc_s_sdp[0][1][7][1]=0.902292; p1_emc_s_sdp[0][1][7][1]=0.124611; p2_emc_s_sdp[0][1][7][1]=-0.0698858; p3_emc_s_sdp[0][1][7][1]=0.0099834; p4_emc_s_sdp[0][1][7][1]=9.43596e-05; 
  p0_emc_m_sdp[1][1][7][1]=-0.523406; p1_emc_m_sdp[1][1][7][1]=1.75159; p2_emc_m_sdp[1][1][7][1]=-0.178893; p3_emc_m_sdp[1][1][7][1]=-4.43081; p4_emc_m_sdp[1][1][7][1]=3.78526; 
  p0_emc_s_sdp[1][1][7][1]=1.05803; p1_emc_s_sdp[1][1][7][1]=-0.480318; p2_emc_s_sdp[1][1][7][1]=0.000872484; p3_emc_s_sdp[1][1][7][1]=1.9114; p4_emc_s_sdp[1][1][7][1]=-1.74436; 
  p0_emc_m_sdp[0][1][8][1]=-0.068724; p1_emc_m_sdp[0][1][8][1]=0.180821; p2_emc_m_sdp[0][1][8][1]=-0.145413; p3_emc_m_sdp[0][1][8][1]=0.0456411; p4_emc_m_sdp[0][1][8][1]=-0.00523646; 
  p0_emc_s_sdp[0][1][8][1]=0.924122; p1_emc_s_sdp[0][1][8][1]=0.0790257; p2_emc_s_sdp[0][1][8][1]=-0.0464249; p3_emc_s_sdp[0][1][8][1]=0.00901817; p4_emc_s_sdp[0][1][8][1]=-0.000684831; 
  p0_emc_m_sdp[1][1][8][1]=-0.669384; p1_emc_m_sdp[1][1][8][1]=2.43726; p2_emc_m_sdp[1][1][8][1]=-0.336343; p3_emc_m_sdp[1][1][8][1]=-6.51312; p4_emc_m_sdp[1][1][8][1]=5.73872; 
  p0_emc_s_sdp[1][1][8][1]=1.06903; p1_emc_s_sdp[1][1][8][1]=-0.411192; p2_emc_s_sdp[1][1][8][1]=-0.119835; p3_emc_s_sdp[1][1][8][1]=1.32562; p4_emc_s_sdp[1][1][8][1]=-0.873894; 
  p0_emc_m_sdp[0][1][9][1]=-0.0806562; p1_emc_m_sdp[0][1][9][1]=0.227229; p2_emc_m_sdp[0][1][9][1]=-0.171587; p3_emc_m_sdp[0][1][9][1]=0.0489954; p4_emc_m_sdp[0][1][9][1]=-0.00494045; 
  p0_emc_s_sdp[0][1][9][1]=0.959007; p1_emc_s_sdp[0][1][9][1]=0.0224651; p2_emc_s_sdp[0][1][9][1]=-0.00617721; p3_emc_s_sdp[0][1][9][1]=-0.00612504; p4_emc_s_sdp[0][1][9][1]=0.00168726; 
  p0_emc_m_sdp[1][1][9][1]=-0.0759601; p1_emc_m_sdp[1][1][9][1]=0.32247; p2_emc_m_sdp[1][1][9][1]=0.00726377; p3_emc_m_sdp[1][1][9][1]=-0.981705; p4_emc_m_sdp[1][1][9][1]=0.849426; 
  p0_emc_s_sdp[1][1][9][1]=1.00754; p1_emc_s_sdp[1][1][9][1]=-0.0932614; p2_emc_s_sdp[1][1][9][1]=-0.347616; p3_emc_s_sdp[1][1][9][1]=0.699884; p4_emc_s_sdp[1][1][9][1]=-0.13986; 
  p0_emc_m_sdp[0][2][0][1]=-0.0336009; p1_emc_m_sdp[0][2][0][1]=-0.0553869; p2_emc_m_sdp[0][2][0][1]=0.0885039; p3_emc_m_sdp[0][2][0][1]=-0.0393512; p4_emc_m_sdp[0][2][0][1]=0.00519663; 
  p0_emc_s_sdp[0][2][0][1]=0.996044; p1_emc_s_sdp[0][2][0][1]=0.0178692; p2_emc_s_sdp[0][2][0][1]=-0.0366702; p3_emc_s_sdp[0][2][0][1]=0.0109497; p4_emc_s_sdp[0][2][0][1]=-0.000859126; 
  p0_emc_m_sdp[1][2][0][1]=-0.584775; p1_emc_m_sdp[1][2][0][1]=1.65571; p2_emc_m_sdp[1][2][0][1]=-0.115762; p3_emc_m_sdp[1][2][0][1]=-4.0175; p4_emc_m_sdp[1][2][0][1]=3.45709; 
  p0_emc_s_sdp[1][2][0][1]=1.59407; p1_emc_s_sdp[1][2][0][1]=-1.81996; p2_emc_s_sdp[1][2][0][1]=-0.16957; p3_emc_s_sdp[1][2][0][1]=4.65632; p4_emc_s_sdp[1][2][0][1]=-3.50144; 
  p0_emc_m_sdp[0][2][1][1]=-0.011101; p1_emc_m_sdp[0][2][1][1]=-0.111753; p2_emc_m_sdp[0][2][1][1]=0.127248; p3_emc_m_sdp[0][2][1][1]=-0.0492851; p4_emc_m_sdp[0][2][1][1]=0.00591996; 
  p0_emc_s_sdp[0][2][1][1]=0.992079; p1_emc_s_sdp[0][2][1][1]=0.00676919; p2_emc_s_sdp[0][2][1][1]=-0.0301806; p3_emc_s_sdp[0][2][1][1]=0.00977492; p4_emc_s_sdp[0][2][1][1]=-0.000855656; 
  p0_emc_m_sdp[1][2][1][1]=-0.99801; p1_emc_m_sdp[1][2][1][1]=2.9842; p2_emc_m_sdp[1][2][1][1]=-0.20685; p3_emc_m_sdp[1][2][1][1]=-7.15838; p4_emc_m_sdp[1][2][1][1]=5.97336; 
  p0_emc_s_sdp[1][2][1][1]=1.11781; p1_emc_s_sdp[1][2][1][1]=-0.0190457; p2_emc_s_sdp[1][2][1][1]=-0.637878; p3_emc_s_sdp[1][2][1][1]=-0.20494; p4_emc_s_sdp[1][2][1][1]=1.14708; 
  p0_emc_m_sdp[0][2][2][1]=-0.0158927; p1_emc_m_sdp[0][2][2][1]=-0.111105; p2_emc_m_sdp[0][2][2][1]=0.129553; p3_emc_m_sdp[0][2][2][1]=-0.0513952; p4_emc_m_sdp[0][2][2][1]=0.00627903; 
  p0_emc_s_sdp[0][2][2][1]=1.04993; p1_emc_s_sdp[0][2][2][1]=-0.131272; p2_emc_s_sdp[0][2][2][1]=0.0781867; p3_emc_s_sdp[0][2][2][1]=-0.0246915; p4_emc_s_sdp[0][2][2][1]=0.00280053; 
  p0_emc_m_sdp[1][2][2][1]=-1.44783; p1_emc_m_sdp[1][2][2][1]=4.71338; p2_emc_m_sdp[1][2][2][1]=-0.748986; p3_emc_m_sdp[1][2][2][1]=-11.779; p4_emc_m_sdp[1][2][2][1]=10.5968; 
  p0_emc_s_sdp[1][2][2][1]=1.38339; p1_emc_s_sdp[1][2][2][1]=-1.16912; p2_emc_s_sdp[1][2][2][1]=-0.214048; p3_emc_s_sdp[1][2][2][1]=3.07589; p4_emc_s_sdp[1][2][2][1]=-2.19311; 
  p0_emc_m_sdp[0][2][3][1]=0.0132693; p1_emc_m_sdp[0][2][3][1]=-0.145583; p2_emc_m_sdp[0][2][3][1]=0.145456; p3_emc_m_sdp[0][2][3][1]=-0.0557765; p4_emc_m_sdp[0][2][3][1]=0.00679749; 
  p0_emc_s_sdp[0][2][3][1]=1.0697; p1_emc_s_sdp[0][2][3][1]=-0.183621; p2_emc_s_sdp[0][2][3][1]=0.120252; p3_emc_s_sdp[0][2][3][1]=-0.0363807; p4_emc_s_sdp[0][2][3][1]=0.00357805; 
  p0_emc_m_sdp[1][2][3][1]=-1.25087; p1_emc_m_sdp[1][2][3][1]=4.24967; p2_emc_m_sdp[1][2][3][1]=-0.730669; p3_emc_m_sdp[1][2][3][1]=-10.7903; p4_emc_m_sdp[1][2][3][1]=9.69432; 
  p0_emc_s_sdp[1][2][3][1]=1.20812; p1_emc_s_sdp[1][2][3][1]=-0.904731; p2_emc_s_sdp[1][2][3][1]=0.0890493; p3_emc_s_sdp[1][2][3][1]=2.94311; p4_emc_s_sdp[1][2][3][1]=-2.72666; 
  p0_emc_m_sdp[0][2][4][1]=-0.0128108; p1_emc_m_sdp[0][2][4][1]=-0.101701; p2_emc_m_sdp[0][2][4][1]=0.115351; p3_emc_m_sdp[0][2][4][1]=-0.0469933; p4_emc_m_sdp[0][2][4][1]=0.00591066; 
  p0_emc_s_sdp[0][2][4][1]=1.04031; p1_emc_s_sdp[0][2][4][1]=-0.14545; p2_emc_s_sdp[0][2][4][1]=0.104294; p3_emc_s_sdp[0][2][4][1]=-0.0359696; p4_emc_s_sdp[0][2][4][1]=0.00412886; 
  p0_emc_m_sdp[1][2][4][1]=-1.32004; p1_emc_m_sdp[1][2][4][1]=4.36; p2_emc_m_sdp[1][2][4][1]=-0.676831; p3_emc_m_sdp[1][2][4][1]=-10.7875; p4_emc_m_sdp[1][2][4][1]=9.52161; 
  p0_emc_s_sdp[1][2][4][1]=1.19789; p1_emc_s_sdp[1][2][4][1]=-0.73305; p2_emc_s_sdp[1][2][4][1]=-0.118771; p3_emc_s_sdp[1][2][4][1]=2.27675; p4_emc_s_sdp[1][2][4][1]=-1.79079; 
  p0_emc_m_sdp[0][2][5][1]=0.0227493; p1_emc_m_sdp[0][2][5][1]=-0.156324; p2_emc_m_sdp[0][2][5][1]=0.143502; p3_emc_m_sdp[0][2][5][1]=-0.0536487; p4_emc_m_sdp[0][2][5][1]=0.00654834; 
  p0_emc_s_sdp[0][2][5][1]=1.03588; p1_emc_s_sdp[0][2][5][1]=-0.17481; p2_emc_s_sdp[0][2][5][1]=0.131448; p3_emc_s_sdp[0][2][5][1]=-0.0453477; p4_emc_s_sdp[0][2][5][1]=0.00522951; 
  p0_emc_m_sdp[1][2][5][1]=-1.51054; p1_emc_m_sdp[1][2][5][1]=5.28893; p2_emc_m_sdp[1][2][5][1]=-1.01762; p3_emc_m_sdp[1][2][5][1]=-13.7079; p4_emc_m_sdp[1][2][5][1]=12.5384; 
  p0_emc_s_sdp[1][2][5][1]=0.758273; p1_emc_s_sdp[1][2][5][1]=0.939944; p2_emc_s_sdp[1][2][5][1]=-0.537091; p3_emc_s_sdp[1][2][5][1]=-2.39237; p4_emc_s_sdp[1][2][5][1]=2.64181; 
  p0_emc_m_sdp[0][2][6][1]=0.0334386; p1_emc_m_sdp[0][2][6][1]=-0.191377; p2_emc_m_sdp[0][2][6][1]=0.175175; p3_emc_m_sdp[0][2][6][1]=-0.0642404; p4_emc_m_sdp[0][2][6][1]=0.00772208; 
  p0_emc_s_sdp[0][2][6][1]=1.00445; p1_emc_s_sdp[0][2][6][1]=-0.114326; p2_emc_s_sdp[0][2][6][1]=0.0878283; p3_emc_s_sdp[0][2][6][1]=-0.0309869; p4_emc_s_sdp[0][2][6][1]=0.0033985; 
  p0_emc_m_sdp[1][2][6][1]=-1.04235; p1_emc_m_sdp[1][2][6][1]=3.50796; p2_emc_m_sdp[1][2][6][1]=-0.58227; p3_emc_m_sdp[1][2][6][1]=-8.94154; p4_emc_m_sdp[1][2][6][1]=7.98967; 
  p0_emc_s_sdp[1][2][6][1]=1.16233; p1_emc_s_sdp[1][2][6][1]=-0.726025; p2_emc_s_sdp[1][2][6][1]=-0.0137595; p3_emc_s_sdp[1][2][6][1]=2.20407; p4_emc_s_sdp[1][2][6][1]=-1.89922; 
  p0_emc_m_sdp[0][2][7][1]=0.0133958; p1_emc_m_sdp[0][2][7][1]=-0.140016; p2_emc_m_sdp[0][2][7][1]=0.134574; p3_emc_m_sdp[0][2][7][1]=-0.0509889; p4_emc_m_sdp[0][2][7][1]=0.00624124; 
  p0_emc_s_sdp[0][2][7][1]=0.978284; p1_emc_s_sdp[0][2][7][1]=-0.0773489; p2_emc_s_sdp[0][2][7][1]=0.0691613; p3_emc_s_sdp[0][2][7][1]=-0.028258; p4_emc_s_sdp[0][2][7][1]=0.00357542; 
  p0_emc_m_sdp[1][2][7][1]=-0.733691; p1_emc_m_sdp[1][2][7][1]=2.31195; p2_emc_m_sdp[1][2][7][1]=-0.273712; p3_emc_m_sdp[1][2][7][1]=-5.61391; p4_emc_m_sdp[1][2][7][1]=4.77217; 
  p0_emc_s_sdp[1][2][7][1]=1.37003; p1_emc_s_sdp[1][2][7][1]=-1.5127; p2_emc_s_sdp[1][2][7][1]=0.158457; p3_emc_s_sdp[1][2][7][1]=4.28584; p4_emc_s_sdp[1][2][7][1]=-3.82231; 
  p0_emc_m_sdp[0][2][8][1]=-0.0050025; p1_emc_m_sdp[0][2][8][1]=-0.0896762; p2_emc_m_sdp[0][2][8][1]=0.0928753; p3_emc_m_sdp[0][2][8][1]=-0.0369941; p4_emc_m_sdp[0][2][8][1]=0.00464565; 
  p0_emc_s_sdp[0][2][8][1]=0.945925; p1_emc_s_sdp[0][2][8][1]=-0.0232376; p2_emc_s_sdp[0][2][8][1]=0.0348317; p3_emc_s_sdp[0][2][8][1]=-0.0193599; p4_emc_s_sdp[0][2][8][1]=0.00290004; 
  p0_emc_m_sdp[1][2][8][1]=-0.701748; p1_emc_m_sdp[1][2][8][1]=2.42241; p2_emc_m_sdp[1][2][8][1]=-0.46204; p3_emc_m_sdp[1][2][8][1]=-6.48911; p4_emc_m_sdp[1][2][8][1]=6.0033; 
  p0_emc_s_sdp[1][2][8][1]=1.17805; p1_emc_s_sdp[1][2][8][1]=-0.761194; p2_emc_s_sdp[1][2][8][1]=-0.0484697; p3_emc_s_sdp[1][2][8][1]=2.11051; p4_emc_s_sdp[1][2][8][1]=-1.69853; 
  p0_emc_m_sdp[0][2][9][1]=-0.00512242; p1_emc_m_sdp[0][2][9][1]=-0.0845515; p2_emc_m_sdp[0][2][9][1]=0.0903411; p3_emc_m_sdp[0][2][9][1]=-0.0367562; p4_emc_m_sdp[0][2][9][1]=0.00471715; 
  p0_emc_s_sdp[0][2][9][1]=0.930949; p1_emc_s_sdp[0][2][9][1]=-0.000380488; p2_emc_s_sdp[0][2][9][1]=0.0198508; p3_emc_s_sdp[0][2][9][1]=-0.0143697; p4_emc_s_sdp[0][2][9][1]=0.00227807; 
  p0_emc_m_sdp[1][2][9][1]=-0.498427; p1_emc_m_sdp[1][2][9][1]=1.8851; p2_emc_m_sdp[1][2][9][1]=-0.502291; p3_emc_m_sdp[1][2][9][1]=-5.47524; p4_emc_m_sdp[1][2][9][1]=5.37949; 
  p0_emc_s_sdp[1][2][9][1]=1.56738; p1_emc_s_sdp[1][2][9][1]=-2.38104; p2_emc_s_sdp[1][2][9][1]=0.428914; p3_emc_s_sdp[1][2][9][1]=6.66795; p4_emc_s_sdp[1][2][9][1]=-6.1513; 
      
  //! EMC sdz neg
  p0_emc_m_sdz[0][0][0][0]=0.233248; p1_emc_m_sdz[0][0][0][0]=-0.543562; p2_emc_m_sdz[0][0][0][0]=0.435399; p3_emc_m_sdz[0][0][0][0]=-0.139181; p4_emc_m_sdz[0][0][0][0]=0.0154372; 
  p0_emc_s_sdz[0][0][0][0]=0.927366; p1_emc_s_sdz[0][0][0][0]=0.0391246; p2_emc_s_sdz[0][0][0][0]=-0.0311699; p3_emc_s_sdz[0][0][0][0]=0.0132331; p4_emc_s_sdz[0][0][0][0]=-0.00147169; 
  p0_emc_m_sdz[1][0][0][0]=0.59688; p1_emc_m_sdz[1][0][0][0]=-2.59576; p2_emc_m_sdz[1][0][0][0]=0.628507; p3_emc_m_sdz[1][0][0][0]=6.88097; p4_emc_m_sdz[1][0][0][0]=-5.92898; 
  p0_emc_s_sdz[1][0][0][0]=1.0856; p1_emc_s_sdz[1][0][0][0]=-0.229702; p2_emc_s_sdz[1][0][0][0]=-0.382114; p3_emc_s_sdz[1][0][0][0]=0.842801; p4_emc_s_sdz[1][0][0][0]=-0.310447; 
  p0_emc_m_sdz[0][0][1][0]=0.186718; p1_emc_m_sdz[0][0][1][0]=-0.438209; p2_emc_m_sdz[0][0][1][0]=0.350236; p3_emc_m_sdz[0][0][1][0]=-0.112667; p4_emc_m_sdz[0][0][1][0]=0.0126162; 
  p0_emc_s_sdz[0][0][1][0]=0.940878; p1_emc_s_sdz[0][0][1][0]=-0.0239898; p2_emc_s_sdz[0][0][1][0]=0.0249304; p3_emc_s_sdz[0][0][1][0]=-0.005845; p4_emc_s_sdz[0][0][1][0]=0.000752991; 
  p0_emc_m_sdz[1][0][1][0]=0.860185; p1_emc_m_sdz[1][0][1][0]=-3.65957; p2_emc_m_sdz[1][0][1][0]=1.04948; p3_emc_m_sdz[1][0][1][0]=9.832; p4_emc_m_sdz[1][0][1][0]=-9.12964; 
  p0_emc_s_sdz[1][0][1][0]=1.29119; p1_emc_s_sdz[1][0][1][0]=-1.12435; p2_emc_s_sdz[1][0][1][0]=-0.0328129; p3_emc_s_sdz[1][0][1][0]=3.11515; p4_emc_s_sdz[1][0][1][0]=-2.63073; 
  p0_emc_m_sdz[0][0][2][0]=0.124946; p1_emc_m_sdz[0][0][2][0]=-0.304589; p2_emc_m_sdz[0][0][2][0]=0.243075; p3_emc_m_sdz[0][0][2][0]=-0.0777722; p4_emc_m_sdz[0][0][2][0]=0.00867979; 
  p0_emc_s_sdz[0][0][2][0]=0.903528; p1_emc_s_sdz[0][0][2][0]=0.0362222; p2_emc_s_sdz[0][0][2][0]=-0.00552113; p3_emc_s_sdz[0][0][2][0]=8.45152e-06; p4_emc_s_sdz[0][0][2][0]=0.000420803; 
  p0_emc_m_sdz[1][0][2][0]=-0.044698; p1_emc_m_sdz[1][0][2][0]=-0.0458167; p2_emc_m_sdz[1][0][2][0]=-0.0156536; p3_emc_m_sdz[1][0][2][0]=0.0784741; p4_emc_m_sdz[1][0][2][0]=0.290929; 
  p0_emc_s_sdz[1][0][2][0]=1.82724; p1_emc_s_sdz[1][0][2][0]=-3.59055; p2_emc_s_sdz[1][0][2][0]=1.02147; p3_emc_s_sdz[1][0][2][0]=10.2108; p4_emc_s_sdz[1][0][2][0]=-10.1316; 
  p0_emc_m_sdz[0][0][3][0]=0.0511815; p1_emc_m_sdz[0][0][3][0]=-0.15348; p2_emc_m_sdz[0][0][3][0]=0.131657; p3_emc_m_sdz[0][0][3][0]=-0.0439992; p4_emc_m_sdz[0][0][3][0]=0.00505645; 
  p0_emc_s_sdz[0][0][3][0]=0.898706; p1_emc_s_sdz[0][0][3][0]=0.0515459; p2_emc_s_sdz[0][0][3][0]=-0.0184494; p3_emc_s_sdz[0][0][3][0]=0.00346975; p4_emc_s_sdz[0][0][3][0]=0.00016057; 
  p0_emc_m_sdz[1][0][3][0]=0.288341; p1_emc_m_sdz[1][0][3][0]=-1.40586; p2_emc_m_sdz[1][0][3][0]=0.506184; p3_emc_m_sdz[1][0][3][0]=4.01523; p4_emc_m_sdz[1][0][3][0]=-3.98058; 
  p0_emc_s_sdz[1][0][3][0]=0.838681; p1_emc_s_sdz[1][0][3][0]=0.390311; p2_emc_s_sdz[1][0][3][0]=-0.22012; p3_emc_s_sdz[1][0][3][0]=-0.541062; p4_emc_s_sdz[1][0][3][0]=0.423493; 
  p0_emc_m_sdz[0][0][4][0]=-0.00227756; p1_emc_m_sdz[0][0][4][0]=-0.0379969; p2_emc_m_sdz[0][0][4][0]=0.0432998; p3_emc_m_sdz[0][0][4][0]=-0.0165724; p4_emc_m_sdz[0][0][4][0]=0.00206492; 
  p0_emc_s_sdz[0][0][4][0]=0.895934; p1_emc_s_sdz[0][0][4][0]=0.0449436; p2_emc_s_sdz[0][0][4][0]=-0.0166534; p3_emc_s_sdz[0][0][4][0]=0.00333035; p4_emc_s_sdz[0][0][4][0]=0.000200138; 
  p0_emc_m_sdz[1][0][4][0]=0.0288547; p1_emc_m_sdz[1][0][4][0]=-0.0733817; p2_emc_m_sdz[1][0][4][0]=-0.156237; p3_emc_m_sdz[1][0][4][0]=-0.073186; p4_emc_m_sdz[1][0][4][0]=0.471661; 
  p0_emc_s_sdz[1][0][4][0]=0.978936; p1_emc_s_sdz[1][0][4][0]=0.218938; p2_emc_s_sdz[1][0][4][0]=-0.661404; p3_emc_s_sdz[1][0][4][0]=-0.791096; p4_emc_s_sdz[1][0][4][0]=1.66914; 
  p0_emc_m_sdz[0][0][5][0]=-0.107723; p1_emc_m_sdz[0][0][5][0]=0.229602; p2_emc_m_sdz[0][0][5][0]=-0.171764; p3_emc_m_sdz[0][0][5][0]=0.0526025; p4_emc_m_sdz[0][0][5][0]=-0.0056123; 
  p0_emc_s_sdz[0][0][5][0]=0.651974; p1_emc_s_sdz[0][0][5][0]=0.541438; p2_emc_s_sdz[0][0][5][0]=-0.330803; p3_emc_s_sdz[0][0][5][0]=0.082068; p4_emc_s_sdz[0][0][5][0]=-0.00670335; 
  p0_emc_m_sdz[1][0][5][0]=0.0218411; p1_emc_m_sdz[1][0][5][0]=-0.00141808; p2_emc_m_sdz[1][0][5][0]=-0.0300956; p3_emc_m_sdz[1][0][5][0]=-0.0481012; p4_emc_m_sdz[1][0][5][0]=-0.0187196; 
  p0_emc_s_sdz[1][0][5][0]=0.309297; p1_emc_s_sdz[1][0][5][0]=2.78026; p2_emc_s_sdz[1][0][5][0]=-1.27098; p3_emc_s_sdz[1][0][5][0]=-7.41594; p4_emc_s_sdz[1][0][5][0]=7.7485; 
  p0_emc_m_sdz[0][0][6][0]=-0.11762; p1_emc_m_sdz[0][0][6][0]=0.258229; p2_emc_m_sdz[0][0][6][0]=-0.200513; p3_emc_m_sdz[0][0][6][0]=0.0635562; p4_emc_m_sdz[0][0][6][0]=-0.00696126; 
  p0_emc_s_sdz[0][0][6][0]=0.908117; p1_emc_s_sdz[0][0][6][0]=0.0345377; p2_emc_s_sdz[0][0][6][0]=-0.0113756; p3_emc_s_sdz[0][0][6][0]=0.000569681; p4_emc_s_sdz[0][0][6][0]=0.000638617; 
  p0_emc_m_sdz[1][0][6][0]=-0.650705; p1_emc_m_sdz[1][0][6][0]=2.5763; p2_emc_m_sdz[1][0][6][0]=-0.642165; p3_emc_m_sdz[1][0][6][0]=-6.71945; p4_emc_m_sdz[1][0][6][0]=6.06064; 
  p0_emc_s_sdz[1][0][6][0]=1.48111; p1_emc_s_sdz[1][0][6][0]=-1.91362; p2_emc_s_sdz[1][0][6][0]=0.131795; p3_emc_s_sdz[1][0][6][0]=5.642; p4_emc_s_sdz[1][0][6][0]=-5.12042; 
  p0_emc_m_sdz[0][0][7][0]=-0.162263; p1_emc_m_sdz[0][0][7][0]=0.363068; p2_emc_m_sdz[0][0][7][0]=-0.283791; p3_emc_m_sdz[0][0][7][0]=0.0902496; p4_emc_m_sdz[0][0][7][0]=-0.010006; 
  p0_emc_s_sdz[0][0][7][0]=0.917502; p1_emc_s_sdz[0][0][7][0]=0.0600086; p2_emc_s_sdz[0][0][7][0]=-0.0463788; p3_emc_s_sdz[0][0][7][0]=0.0137288; p4_emc_s_sdz[0][0][7][0]=-0.000955047; 
  p0_emc_m_sdz[1][0][7][0]=-1.01392; p1_emc_m_sdz[1][0][7][0]=3.92831; p2_emc_m_sdz[1][0][7][0]=-0.830965; p3_emc_m_sdz[1][0][7][0]=-10.1693; p4_emc_m_sdz[1][0][7][0]=8.94783; 
  p0_emc_s_sdz[1][0][7][0]=1.90205; p1_emc_s_sdz[1][0][7][0]=-3.50447; p2_emc_s_sdz[1][0][7][0]=0.536032; p3_emc_s_sdz[1][0][7][0]=9.70873; p4_emc_s_sdz[1][0][7][0]=-8.78779; 
  p0_emc_m_sdz[0][0][8][0]=-0.222161; p1_emc_m_sdz[0][0][8][0]=0.504752; p2_emc_m_sdz[0][0][8][0]=-0.396998; p3_emc_m_sdz[0][0][8][0]=0.126081; p4_emc_m_sdz[0][0][8][0]=-0.0139551; 
  p0_emc_s_sdz[0][0][8][0]=0.94614; p1_emc_s_sdz[0][0][8][0]=0.0071463; p2_emc_s_sdz[0][0][8][0]=-0.00860611; p3_emc_s_sdz[0][0][8][0]=0.00204154; p4_emc_s_sdz[0][0][8][0]=0.000270553; 
  p0_emc_m_sdz[1][0][8][0]=-0.865125; p1_emc_m_sdz[1][0][8][0]=3.4856; p2_emc_m_sdz[1][0][8][0]=-0.792052; p3_emc_m_sdz[1][0][8][0]=-9.18139; p4_emc_m_sdz[1][0][8][0]=8.13721; 
  p0_emc_s_sdz[1][0][8][0]=2.67743; p1_emc_s_sdz[1][0][8][0]=-6.6441; p2_emc_s_sdz[1][0][8][0]=1.51167; p3_emc_s_sdz[1][0][8][0]=18.6165; p4_emc_s_sdz[1][0][8][0]=-17.7357; 
  p0_emc_m_sdz[0][0][9][0]=-0.284794; p1_emc_m_sdz[0][0][9][0]=0.641664; p2_emc_m_sdz[0][0][9][0]=-0.501496; p3_emc_m_sdz[0][0][9][0]=0.158421; p4_emc_m_sdz[0][0][9][0]=-0.0174765; 
  p0_emc_s_sdz[0][0][9][0]=1.00414; p1_emc_s_sdz[0][0][9][0]=-0.0883716; p2_emc_s_sdz[0][0][9][0]=0.0550475; p3_emc_s_sdz[0][0][9][0]=-0.0154863; p4_emc_s_sdz[0][0][9][0]=0.00184741; 
  p0_emc_m_sdz[1][0][9][0]=-0.433232; p1_emc_m_sdz[1][0][9][0]=1.87348; p2_emc_m_sdz[1][0][9][0]=-0.449477; p3_emc_m_sdz[1][0][9][0]=-4.45577; p4_emc_m_sdz[1][0][9][0]=3.49033; 
  p0_emc_s_sdz[1][0][9][0]=1.43517; p1_emc_s_sdz[1][0][9][0]=-1.82672; p2_emc_s_sdz[1][0][9][0]=0.35772; p3_emc_s_sdz[1][0][9][0]=5.4662; p4_emc_s_sdz[1][0][9][0]=-5.27137; 
  p0_emc_m_sdz[0][1][0][0]=0.0844548; p1_emc_m_sdz[0][1][0][0]=-0.38919; p2_emc_m_sdz[0][1][0][0]=0.353481; p3_emc_m_sdz[0][1][0][0]=-0.116755; p4_emc_m_sdz[0][1][0][0]=0.0130233; 
  p0_emc_s_sdz[0][1][0][0]=0.805664; p1_emc_s_sdz[0][1][0][0]=0.25918; p2_emc_s_sdz[0][1][0][0]=-0.182559; p3_emc_s_sdz[0][1][0][0]=0.0592807; p4_emc_s_sdz[0][1][0][0]=-0.0066951; 
  p0_emc_m_sdz[1][1][0][0]=0.749823; p1_emc_m_sdz[1][1][0][0]=-2.90641; p2_emc_m_sdz[1][1][0][0]=0.237897; p3_emc_m_sdz[1][1][0][0]=6.54194; p4_emc_m_sdz[1][1][0][0]=-4.60188; 
  p0_emc_s_sdz[1][1][0][0]=1.45325; p1_emc_s_sdz[1][1][0][0]=-1.73743; p2_emc_s_sdz[1][1][0][0]=-0.00202971; p3_emc_s_sdz[1][1][0][0]=4.22347; p4_emc_s_sdz[1][1][0][0]=-3.19254; 
  p0_emc_m_sdz[0][1][1][0]=0.0802475; p1_emc_m_sdz[0][1][1][0]=-0.269021; p2_emc_m_sdz[0][1][1][0]=0.218232; p3_emc_m_sdz[0][1][1][0]=-0.0679007; p4_emc_m_sdz[0][1][1][0]=0.00735061; 
  p0_emc_s_sdz[0][1][1][0]=0.900735; p1_emc_s_sdz[0][1][1][0]=0.0656167; p2_emc_s_sdz[0][1][1][0]=-0.0622264; p3_emc_s_sdz[0][1][1][0]=0.0286485; p4_emc_s_sdz[0][1][1][0]=-0.00383612; 
  p0_emc_m_sdz[1][1][1][0]=0.877139; p1_emc_m_sdz[1][1][1][0]=-3.34491; p2_emc_m_sdz[1][1][1][0]=0.524795; p3_emc_m_sdz[1][1][1][0]=8.09531; p4_emc_m_sdz[1][1][1][0]=-6.58822; 
  p0_emc_s_sdz[1][1][1][0]=0.478901; p1_emc_s_sdz[1][1][1][0]=2.09441; p2_emc_s_sdz[1][1][1][0]=-1.10969; p3_emc_s_sdz[1][1][1][0]=-5.84856; p4_emc_s_sdz[1][1][1][0]=6.5266; 
  p0_emc_m_sdz[0][1][2][0]=0.099543; p1_emc_m_sdz[0][1][2][0]=-0.319226; p2_emc_m_sdz[0][1][2][0]=0.249955; p3_emc_m_sdz[0][1][2][0]=-0.0761638; p4_emc_m_sdz[0][1][2][0]=0.00810358; 
  p0_emc_s_sdz[0][1][2][0]=0.914048; p1_emc_s_sdz[0][1][2][0]=-0.00113892; p2_emc_s_sdz[0][1][2][0]=-0.000358195; p3_emc_s_sdz[0][1][2][0]=0.00743994; p4_emc_s_sdz[0][1][2][0]=-0.00132365; 
  p0_emc_m_sdz[1][1][2][0]=1.082; p1_emc_m_sdz[1][1][2][0]=-4.31506; p2_emc_m_sdz[1][1][2][0]=1.01569; p3_emc_m_sdz[1][1][2][0]=11.1761; p4_emc_m_sdz[1][1][2][0]=-10.131; 
  p0_emc_s_sdz[1][1][2][0]=1.68562; p1_emc_s_sdz[1][1][2][0]=-2.6536; p2_emc_s_sdz[1][1][2][0]=0.167541; p3_emc_s_sdz[1][1][2][0]=7.10041; p4_emc_s_sdz[1][1][2][0]=-5.93583; 
  p0_emc_m_sdz[0][1][3][0]=0.0563316; p1_emc_m_sdz[0][1][3][0]=-0.242968; p2_emc_m_sdz[0][1][3][0]=0.200278; p3_emc_m_sdz[0][1][3][0]=-0.0622393; p4_emc_m_sdz[0][1][3][0]=0.00663493; 
  p0_emc_s_sdz[0][1][3][0]=0.900494; p1_emc_s_sdz[0][1][3][0]=0.0614006; p2_emc_s_sdz[0][1][3][0]=-0.0375679; p3_emc_s_sdz[0][1][3][0]=0.0151553; p4_emc_s_sdz[0][1][3][0]=-0.00184796; 
  p0_emc_m_sdz[1][1][3][0]=0.500638; p1_emc_m_sdz[1][1][3][0]=-2.01284; p2_emc_m_sdz[1][1][3][0]=0.352508; p3_emc_m_sdz[1][1][3][0]=5.01801; p4_emc_m_sdz[1][1][3][0]=-4.25783; 
  p0_emc_s_sdz[1][1][3][0]=0.986427; p1_emc_s_sdz[1][1][3][0]=0.217911; p2_emc_s_sdz[1][1][3][0]=-0.686646; p3_emc_s_sdz[1][1][3][0]=-0.811683; p4_emc_s_sdz[1][1][3][0]=1.78064; 
  p0_emc_m_sdz[0][1][4][0]=0.00883633; p1_emc_m_sdz[0][1][4][0]=-0.0986201; p2_emc_m_sdz[0][1][4][0]=0.0834538; p3_emc_m_sdz[0][1][4][0]=-0.0262172; p4_emc_m_sdz[0][1][4][0]=0.00288126; 
  p0_emc_s_sdz[0][1][4][0]=0.967123; p1_emc_s_sdz[0][1][4][0]=-0.0082776; p2_emc_s_sdz[0][1][4][0]=0.00442203; p3_emc_s_sdz[0][1][4][0]=0.000698933; p4_emc_s_sdz[0][1][4][0]=1.95527e-05; 
  p0_emc_m_sdz[1][1][4][0]=-0.0189926; p1_emc_m_sdz[1][1][4][0]=-0.0178299; p2_emc_m_sdz[1][1][4][0]=-0.01102; p3_emc_m_sdz[1][1][4][0]=0.00293706; p4_emc_m_sdz[1][1][4][0]=0.0155739; 
  p0_emc_s_sdz[1][1][4][0]=1.39082; p1_emc_s_sdz[1][1][4][0]=-1.46343; p2_emc_s_sdz[1][1][4][0]=-0.0996232; p3_emc_s_sdz[1][1][4][0]=4.63163; p4_emc_s_sdz[1][1][4][0]=-3.96857; 
  p0_emc_m_sdz[0][1][5][0]=-0.013822; p1_emc_m_sdz[0][1][5][0]=0.0369853; p2_emc_m_sdz[0][1][5][0]=-0.03657; p3_emc_m_sdz[0][1][5][0]=0.0120585; p4_emc_m_sdz[0][1][5][0]=-0.00126528; 
  p0_emc_s_sdz[0][1][5][0]=1.01037; p1_emc_s_sdz[0][1][5][0]=-0.0518865; p2_emc_s_sdz[0][1][5][0]=0.0333975; p3_emc_s_sdz[0][1][5][0]=-0.00989998; p4_emc_s_sdz[0][1][5][0]=0.00133478; 
  p0_emc_m_sdz[1][1][5][0]=-0.0467768; p1_emc_m_sdz[1][1][5][0]=0.0634619; p2_emc_m_sdz[1][1][5][0]=0.156796; p3_emc_m_sdz[1][1][5][0]=0.0769685; p4_emc_m_sdz[1][1][5][0]=-0.495662; 
  p0_emc_s_sdz[1][1][5][0]=1.18046; p1_emc_s_sdz[1][1][5][0]=-0.732434; p2_emc_s_sdz[1][1][5][0]=0.0772194; p3_emc_s_sdz[1][1][5][0]=2.29141; p4_emc_s_sdz[1][1][5][0]=-2.13875; 
  p0_emc_m_sdz[0][1][6][0]=-0.0795469; p1_emc_m_sdz[0][1][6][0]=0.172031; p2_emc_m_sdz[0][1][6][0]=-0.136293; p3_emc_m_sdz[0][1][6][0]=0.0420341; p4_emc_m_sdz[0][1][6][0]=-0.00440448; 
  p0_emc_s_sdz[0][1][6][0]=0.947941; p1_emc_s_sdz[0][1][6][0]=0.101778; p2_emc_s_sdz[0][1][6][0]=-0.0883901; p3_emc_s_sdz[0][1][6][0]=0.0290573; p4_emc_s_sdz[0][1][6][0]=-0.00298647; 
  p0_emc_m_sdz[1][1][6][0]=-0.808514; p1_emc_m_sdz[1][1][6][0]=3.08914; p2_emc_m_sdz[1][1][6][0]=-0.706587; p3_emc_m_sdz[1][1][6][0]=-8.11979; p4_emc_m_sdz[1][1][6][0]=7.39459; 
  p0_emc_s_sdz[1][1][6][0]=1.52406; p1_emc_s_sdz[1][1][6][0]=-2.0023; p2_emc_s_sdz[1][1][6][0]=0.254775; p3_emc_s_sdz[1][1][6][0]=5.79367; p4_emc_s_sdz[1][1][6][0]=-5.25032; 
  p0_emc_m_sdz[0][1][7][0]=-0.11407; p1_emc_m_sdz[0][1][7][0]=0.259513; p2_emc_m_sdz[0][1][7][0]=-0.201945; p3_emc_m_sdz[0][1][7][0]=0.0617751; p4_emc_m_sdz[0][1][7][0]=-0.00650027; 
  p0_emc_s_sdz[0][1][7][0]=0.954126; p1_emc_s_sdz[0][1][7][0]=0.0761142; p2_emc_s_sdz[0][1][7][0]=-0.0709075; p3_emc_s_sdz[0][1][7][0]=0.0257929; p4_emc_s_sdz[0][1][7][0]=-0.00290439; 
  p0_emc_m_sdz[1][1][7][0]=-1.06676; p1_emc_m_sdz[1][1][7][0]=4.12926; p2_emc_m_sdz[1][1][7][0]=-1.05762; p3_emc_m_sdz[1][1][7][0]=-10.6931; p4_emc_m_sdz[1][1][7][0]=9.88698; 
  p0_emc_s_sdz[1][1][7][0]=1.17209; p1_emc_s_sdz[1][1][7][0]=-0.535395; p2_emc_s_sdz[1][1][7][0]=-0.222402; p3_emc_s_sdz[1][1][7][0]=1.32561; p4_emc_s_sdz[1][1][7][0]=-0.623611; 
  p0_emc_m_sdz[0][1][8][0]=-0.118473; p1_emc_m_sdz[0][1][8][0]=0.313469; p2_emc_m_sdz[0][1][8][0]=-0.252487; p3_emc_m_sdz[0][1][8][0]=0.0789949; p4_emc_m_sdz[0][1][8][0]=-0.00848921; 
  p0_emc_s_sdz[0][1][8][0]=0.965798; p1_emc_s_sdz[0][1][8][0]=0.0527567; p2_emc_s_sdz[0][1][8][0]=-0.0471411; p3_emc_s_sdz[0][1][8][0]=0.0171771; p4_emc_s_sdz[0][1][8][0]=-0.00192152; 
  p0_emc_m_sdz[1][1][8][0]=-0.643068; p1_emc_m_sdz[1][1][8][0]=2.49639; p2_emc_m_sdz[1][1][8][0]=-0.430443; p3_emc_m_sdz[1][1][8][0]=-6.37482; p4_emc_m_sdz[1][1][8][0]=5.35416; 
  p0_emc_s_sdz[1][1][8][0]=2.14555; p1_emc_s_sdz[1][1][8][0]=-4.58936; p2_emc_s_sdz[1][1][8][0]=1.12064; p3_emc_s_sdz[1][1][8][0]=12.769; p4_emc_s_sdz[1][1][8][0]=-12.1308; 
  p0_emc_m_sdz[0][1][9][0]=-0.152622; p1_emc_m_sdz[0][1][9][0]=0.402062; p2_emc_m_sdz[0][1][9][0]=-0.327514; p3_emc_m_sdz[0][1][9][0]=0.103644; p4_emc_m_sdz[0][1][9][0]=-0.0112627; 
  p0_emc_s_sdz[0][1][9][0]=0.933701; p1_emc_s_sdz[0][1][9][0]=0.0982837; p2_emc_s_sdz[0][1][9][0]=-0.0503844; p3_emc_s_sdz[0][1][9][0]=0.00958967; p4_emc_s_sdz[0][1][9][0]=-0.000389825; 
  p0_emc_m_sdz[1][1][9][0]=-0.690927; p1_emc_m_sdz[1][1][9][0]=2.71467; p2_emc_m_sdz[1][1][9][0]=-0.383668; p3_emc_m_sdz[1][1][9][0]=-6.78647; p4_emc_m_sdz[1][1][9][0]=5.38701; 
  p0_emc_s_sdz[1][1][9][0]=1.64014; p1_emc_s_sdz[1][1][9][0]=-2.38548; p2_emc_s_sdz[1][1][9][0]=0.183588; p3_emc_s_sdz[1][1][9][0]=6.51859; p4_emc_s_sdz[1][1][9][0]=-5.50289; 
  p0_emc_m_sdz[0][2][0][0]=0.177652; p1_emc_m_sdz[0][2][0][0]=-0.514479; p2_emc_m_sdz[0][2][0][0]=0.434453; p3_emc_m_sdz[0][2][0][0]=-0.140672; p4_emc_m_sdz[0][2][0][0]=0.0154918; 
  p0_emc_s_sdz[0][2][0][0]=0.881088; p1_emc_s_sdz[0][2][0][0]=0.152909; p2_emc_s_sdz[0][2][0][0]=-0.0813054; p3_emc_s_sdz[0][2][0][0]=0.0205688; p4_emc_s_sdz[0][2][0][0]=-0.00183444; 
  p0_emc_m_sdz[1][2][0][0]=0.341442; p1_emc_m_sdz[1][2][0][0]=-1.67788; p2_emc_m_sdz[1][2][0][0]=0.249863; p3_emc_m_sdz[1][2][0][0]=4.42757; p4_emc_m_sdz[1][2][0][0]=-3.40575; 
  p0_emc_s_sdz[1][2][0][0]=1.39904; p1_emc_s_sdz[1][2][0][0]=-1.77503; p2_emc_s_sdz[1][2][0][0]=0.231923; p3_emc_s_sdz[1][2][0][0]=4.99678; p4_emc_s_sdz[1][2][0][0]=-4.3284; 
  p0_emc_m_sdz[0][2][1][0]=0.163208; p1_emc_m_sdz[0][2][1][0]=-0.437522; p2_emc_m_sdz[0][2][1][0]=0.357727; p3_emc_m_sdz[0][2][1][0]=-0.11409; p4_emc_m_sdz[0][2][1][0]=0.012453; 
  p0_emc_s_sdz[0][2][1][0]=0.915686; p1_emc_s_sdz[0][2][1][0]=0.127966; p2_emc_s_sdz[0][2][1][0]=-0.0889964; p3_emc_s_sdz[0][2][1][0]=0.028625; p4_emc_s_sdz[0][2][1][0]=-0.0031368; 
  p0_emc_m_sdz[1][2][1][0]=0.502464; p1_emc_m_sdz[1][2][1][0]=-2.25294; p2_emc_m_sdz[1][2][1][0]=0.572926; p3_emc_m_sdz[1][2][1][0]=6.16897; p4_emc_m_sdz[1][2][1][0]=-5.55215; 
  p0_emc_s_sdz[1][2][1][0]=1.1719; p1_emc_s_sdz[1][2][1][0]=-0.86916; p2_emc_s_sdz[1][2][1][0]=0.0958731; p3_emc_s_sdz[1][2][1][0]=2.89081; p4_emc_s_sdz[1][2][1][0]=-2.67892; 
  p0_emc_m_sdz[0][2][2][0]=0.109853; p1_emc_m_sdz[0][2][2][0]=-0.288882; p2_emc_m_sdz[0][2][2][0]=0.235431; p3_emc_m_sdz[0][2][2][0]=-0.075138; p4_emc_m_sdz[0][2][2][0]=0.00822704; 
  p0_emc_s_sdz[0][2][2][0]=0.938115; p1_emc_s_sdz[0][2][2][0]=0.0852429; p2_emc_s_sdz[0][2][2][0]=-0.0576947; p3_emc_s_sdz[0][2][2][0]=0.0186275; p4_emc_s_sdz[0][2][2][0]=-0.00200359; 
  p0_emc_m_sdz[1][2][2][0]=0.132552; p1_emc_m_sdz[1][2][2][0]=-0.653792; p2_emc_m_sdz[1][2][2][0]=0.140968; p3_emc_m_sdz[1][2][2][0]=1.69533; p4_emc_m_sdz[1][2][2][0]=-1.33545; 
  p0_emc_s_sdz[1][2][2][0]=0.79574; p1_emc_s_sdz[1][2][2][0]=0.805309; p2_emc_s_sdz[1][2][2][0]=-0.482723; p3_emc_s_sdz[1][2][2][0]=-2.00962; p4_emc_s_sdz[1][2][2][0]=2.30143; 
  p0_emc_m_sdz[0][2][3][0]=0.050281; p1_emc_m_sdz[0][2][3][0]=-0.14476; p2_emc_m_sdz[0][2][3][0]=0.122691; p3_emc_m_sdz[0][2][3][0]=-0.0401415; p4_emc_m_sdz[0][2][3][0]=0.00447617; 
  p0_emc_s_sdz[0][2][3][0]=0.957505; p1_emc_s_sdz[0][2][3][0]=0.0515069; p2_emc_s_sdz[0][2][3][0]=-0.0385151; p3_emc_s_sdz[0][2][3][0]=0.0142421; p4_emc_s_sdz[0][2][3][0]=-0.00167203; 
  p0_emc_m_sdz[1][2][3][0]=0.202718; p1_emc_m_sdz[1][2][3][0]=-0.87444; p2_emc_m_sdz[1][2][3][0]=0.164214; p3_emc_m_sdz[1][2][3][0]=2.40758; p4_emc_m_sdz[1][2][3][0]=-2.0659; 
  p0_emc_s_sdz[1][2][3][0]=1.24625; p1_emc_s_sdz[1][2][3][0]=-1.0839; p2_emc_s_sdz[1][2][3][0]=0.095054; p3_emc_s_sdz[1][2][3][0]=3.50039; p4_emc_s_sdz[1][2][3][0]=-3.18475; 
  p0_emc_m_sdz[0][2][4][0]=0.00568807; p1_emc_m_sdz[0][2][4][0]=-0.0300676; p2_emc_m_sdz[0][2][4][0]=0.0241811; p3_emc_m_sdz[0][2][4][0]=-0.00664499; p4_emc_m_sdz[0][2][4][0]=0.000614749; 
  p0_emc_s_sdz[0][2][4][0]=0.44511; p1_emc_s_sdz[0][2][4][0]=1.4466; p2_emc_s_sdz[0][2][4][0]=-1.32189; p3_emc_s_sdz[0][2][4][0]=0.483826; p4_emc_s_sdz[0][2][4][0]=-0.0586241; 
  p0_emc_m_sdz[1][2][4][0]=0.356012; p1_emc_m_sdz[1][2][4][0]=-1.46; p2_emc_m_sdz[1][2][4][0]=0.433038; p3_emc_m_sdz[1][2][4][0]=3.75782; p4_emc_m_sdz[1][2][4][0]=-3.53155; 
  p0_emc_s_sdz[1][2][4][0]=1.11577; p1_emc_s_sdz[1][2][4][0]=-0.399037; p2_emc_s_sdz[1][2][4][0]=-0.185249; p3_emc_s_sdz[1][2][4][0]=1.26216; p4_emc_s_sdz[1][2][4][0]=-0.818154; 
  p0_emc_m_sdz[0][2][5][0]=-0.00634235; p1_emc_m_sdz[0][2][5][0]=0.0262813; p2_emc_m_sdz[0][2][5][0]=-0.0224014; p3_emc_m_sdz[0][2][5][0]=0.00680474; p4_emc_m_sdz[0][2][5][0]=-0.00065678; 
  p0_emc_s_sdz[0][2][5][0]=0.991286; p1_emc_s_sdz[0][2][5][0]=0.0618911; p2_emc_s_sdz[0][2][5][0]=-0.0797411; p3_emc_s_sdz[0][2][5][0]=0.0307396; p4_emc_s_sdz[0][2][5][0]=-0.0035392; 
  p0_emc_m_sdz[1][2][5][0]=0.0285455; p1_emc_m_sdz[1][2][5][0]=-0.0191831; p2_emc_m_sdz[1][2][5][0]=-0.0654886; p3_emc_m_sdz[1][2][5][0]=-0.0482781; p4_emc_m_sdz[1][2][5][0]=0.159209; 
  p0_emc_s_sdz[1][2][5][0]=1.43509; p1_emc_s_sdz[1][2][5][0]=-1.71268; p2_emc_s_sdz[1][2][5][0]=0.326253; p3_emc_s_sdz[1][2][5][0]=5.1378; p4_emc_s_sdz[1][2][5][0]=-4.88393; 
  p0_emc_m_sdz[0][2][6][0]=-0.0670277; p1_emc_m_sdz[0][2][6][0]=0.172494; p2_emc_m_sdz[0][2][6][0]=-0.139293; p3_emc_m_sdz[0][2][6][0]=0.0445445; p4_emc_m_sdz[0][2][6][0]=-0.00485531; 
  p0_emc_s_sdz[0][2][6][0]=1.00231; p1_emc_s_sdz[0][2][6][0]=-0.0324514; p2_emc_s_sdz[0][2][6][0]=-0.000145729; p3_emc_s_sdz[0][2][6][0]=0.0086615; p4_emc_s_sdz[0][2][6][0]=-0.00159979; 
  p0_emc_m_sdz[1][2][6][0]=-0.205156; p1_emc_m_sdz[1][2][6][0]=0.909822; p2_emc_m_sdz[1][2][6][0]=-0.237301; p3_emc_m_sdz[1][2][6][0]=-2.62096; p4_emc_m_sdz[1][2][6][0]=2.44053; 
  p0_emc_s_sdz[1][2][6][0]=1.26592; p1_emc_s_sdz[1][2][6][0]=-1.17033; p2_emc_s_sdz[1][2][6][0]=0.164542; p3_emc_s_sdz[1][2][6][0]=3.76085; p4_emc_s_sdz[1][2][6][0]=-3.53052; 
  p0_emc_m_sdz[0][2][7][0]=-0.124957; p1_emc_m_sdz[0][2][7][0]=0.312563; p2_emc_m_sdz[0][2][7][0]=-0.248293; p3_emc_m_sdz[0][2][7][0]=0.0783608; p4_emc_m_sdz[0][2][7][0]=-0.00850636; 
  p0_emc_s_sdz[0][2][7][0]=0.977076; p1_emc_s_sdz[0][2][7][0]=-0.0414946; p2_emc_s_sdz[0][2][7][0]=0.0263608; p3_emc_s_sdz[0][2][7][0]=-0.00194989; p4_emc_s_sdz[0][2][7][0]=-0.000349762; 
  p0_emc_m_sdz[1][2][7][0]=-0.388158; p1_emc_m_sdz[1][2][7][0]=1.5288; p2_emc_m_sdz[1][2][7][0]=-0.290629; p3_emc_m_sdz[1][2][7][0]=-3.8233; p4_emc_m_sdz[1][2][7][0]=3.18459; 
  p0_emc_s_sdz[1][2][7][0]=1.47712; p1_emc_s_sdz[1][2][7][0]=-1.97082; p2_emc_s_sdz[1][2][7][0]=0.281204; p3_emc_s_sdz[1][2][7][0]=5.55897; p4_emc_s_sdz[1][2][7][0]=-4.95556; 
  p0_emc_m_sdz[0][2][8][0]=-0.1665; p1_emc_m_sdz[0][2][8][0]=0.416643; p2_emc_m_sdz[0][2][8][0]=-0.33308; p3_emc_m_sdz[0][2][8][0]=0.105364; p4_emc_m_sdz[0][2][8][0]=-0.0114588; 
  p0_emc_s_sdz[0][2][8][0]=0.963251; p1_emc_s_sdz[0][2][8][0]=0.0345774; p2_emc_s_sdz[0][2][8][0]=-0.0431699; p3_emc_s_sdz[0][2][8][0]=0.0199718; p4_emc_s_sdz[0][2][8][0]=-0.00264924; 
  p0_emc_m_sdz[1][2][8][0]=-0.387859; p1_emc_m_sdz[1][2][8][0]=1.62997; p2_emc_m_sdz[1][2][8][0]=-0.37254; p3_emc_m_sdz[1][2][8][0]=-4.20042; p4_emc_m_sdz[1][2][8][0]=3.60626; 
  p0_emc_s_sdz[1][2][8][0]=1.66426; p1_emc_s_sdz[1][2][8][0]=-2.73264; p2_emc_s_sdz[1][2][8][0]=0.545808; p3_emc_s_sdz[1][2][8][0]=7.91312; p4_emc_s_sdz[1][2][8][0]=-7.42351; 
  p0_emc_m_sdz[0][2][9][0]=-0.175158; p1_emc_m_sdz[0][2][9][0]=0.474091; p2_emc_m_sdz[0][2][9][0]=-0.395668; p3_emc_m_sdz[0][2][9][0]=0.128035; p4_emc_m_sdz[0][2][9][0]=-0.0141225; 
  p0_emc_s_sdz[0][2][9][0]=0.913269; p1_emc_s_sdz[0][2][9][0]=0.113582; p2_emc_s_sdz[0][2][9][0]=-0.0835035; p3_emc_s_sdz[0][2][9][0]=0.0282447; p4_emc_s_sdz[0][2][9][0]=-0.00323949; 
  p0_emc_m_sdz[1][2][9][0]=-0.179587; p1_emc_m_sdz[1][2][9][0]=0.962357; p2_emc_m_sdz[1][2][9][0]=-0.151717; p3_emc_m_sdz[1][2][9][0]=-2.44729; p4_emc_m_sdz[1][2][9][0]=1.68982; 
  p0_emc_s_sdz[1][2][9][0]=0.706037; p1_emc_s_sdz[1][2][9][0]=1.04194; p2_emc_s_sdz[1][2][9][0]=-0.532164; p3_emc_s_sdz[1][2][9][0]=-2.69405; p4_emc_s_sdz[1][2][9][0]=3.0294; 

  //! EMC sdz pos
  p0_emc_m_sdz[0][0][0][1]=0.395318; p1_emc_m_sdz[0][0][0][1]=-0.899855; p2_emc_m_sdz[0][0][0][1]=0.713123; p3_emc_m_sdz[0][0][0][1]=-0.226929; p4_emc_m_sdz[0][0][0][1]=0.0250119; 
  p0_emc_s_sdz[0][0][0][1]=0.825411; p1_emc_s_sdz[0][0][0][1]=0.335064; p2_emc_s_sdz[0][0][0][1]=-0.27254; p3_emc_s_sdz[0][0][0][1]=0.0895938; p4_emc_s_sdz[0][0][0][1]=-0.00951289; 
  p0_emc_m_sdz[1][0][0][1]=1.00301; p1_emc_m_sdz[1][0][0][1]=-4.32837; p2_emc_m_sdz[1][0][0][1]=1.24896; p3_emc_m_sdz[1][0][0][1]=12.1948; p4_emc_m_sdz[1][0][0][1]=-11.4694; 
  p0_emc_s_sdz[1][0][0][1]=1.59879; p1_emc_s_sdz[1][0][0][1]=-2.41832; p2_emc_s_sdz[1][0][0][1]=0.422086; p3_emc_s_sdz[1][0][0][1]=6.99933; p4_emc_s_sdz[1][0][0][1]=-6.49662; 
  p0_emc_m_sdz[0][0][1][1]=0.326829; p1_emc_m_sdz[0][0][1][1]=-0.736836; p2_emc_m_sdz[0][0][1][1]=0.577529; p3_emc_m_sdz[0][0][1][1]=-0.183448; p4_emc_m_sdz[0][0][1][1]=0.0202453; 
  p0_emc_s_sdz[0][0][1][1]=0.819037; p1_emc_s_sdz[0][0][1][1]=0.328815; p2_emc_s_sdz[0][0][1][1]=-0.263701; p3_emc_s_sdz[0][0][1][1]=0.0862356; p4_emc_s_sdz[0][0][1][1]=-0.00908891; 
  p0_emc_m_sdz[1][0][1][1]=0.417035; p1_emc_m_sdz[1][0][1][1]=-1.86983; p2_emc_m_sdz[1][0][1][1]=0.441859; p3_emc_m_sdz[1][0][1][1]=5.34148; p4_emc_m_sdz[1][0][1][1]=-4.70889; 
  p0_emc_s_sdz[1][0][1][1]=1.03315; p1_emc_s_sdz[1][0][1][1]=-0.131082; p2_emc_s_sdz[1][0][1][1]=-0.334366; p3_emc_s_sdz[1][0][1][1]=0.660959; p4_emc_s_sdz[1][0][1][1]=-0.165458; 
  p0_emc_m_sdz[0][0][2][1]=0.263713; p1_emc_m_sdz[0][0][2][1]=-0.604043; p2_emc_m_sdz[0][0][2][1]=0.473933; p3_emc_m_sdz[0][0][2][1]=-0.150809; p4_emc_m_sdz[0][0][2][1]=0.0166877; 
  p0_emc_s_sdz[0][0][2][1]=0.801375; p1_emc_s_sdz[0][0][2][1]=0.356343; p2_emc_s_sdz[0][0][2][1]=-0.276533; p3_emc_s_sdz[0][0][2][1]=0.0888769; p4_emc_s_sdz[0][0][2][1]=-0.0092631; 
  p0_emc_m_sdz[1][0][2][1]=0.470367; p1_emc_m_sdz[1][0][2][1]=-2.03008; p2_emc_m_sdz[1][0][2][1]=0.486885; p3_emc_m_sdz[1][0][2][1]=5.64165; p4_emc_m_sdz[1][0][2][1]=-5.03294; 
  p0_emc_s_sdz[1][0][2][1]=1.60576; p1_emc_s_sdz[1][0][2][1]=-2.50718; p2_emc_s_sdz[1][0][2][1]=0.441146; p3_emc_s_sdz[1][0][2][1]=7.12935; p4_emc_s_sdz[1][0][2][1]=-6.54612; 
  p0_emc_m_sdz[0][0][3][1]=0.151761; p1_emc_m_sdz[0][0][3][1]=-0.362671; p2_emc_m_sdz[0][0][3][1]=0.286967; p3_emc_m_sdz[0][0][3][1]=-0.0921314; p4_emc_m_sdz[0][0][3][1]=0.0102795; 
  p0_emc_s_sdz[0][0][3][1]=0.849609; p1_emc_s_sdz[0][0][3][1]=0.26793; p2_emc_s_sdz[0][0][3][1]=-0.214969; p3_emc_s_sdz[0][0][3][1]=0.0705912; p4_emc_s_sdz[0][0][3][1]=-0.00732729; 
  p0_emc_m_sdz[1][0][3][1]=-0.346317; p1_emc_m_sdz[1][0][3][1]=1.23814; p2_emc_m_sdz[1][0][3][1]=-0.331681; p3_emc_m_sdz[1][0][3][1]=-3.47877; p4_emc_m_sdz[1][0][3][1]=3.56726; 
  p0_emc_s_sdz[1][0][3][1]=1.33516; p1_emc_s_sdz[1][0][3][1]=-1.48635; p2_emc_s_sdz[1][0][3][1]=0.178716; p3_emc_s_sdz[1][0][3][1]=4.75189; p4_emc_s_sdz[1][0][3][1]=-4.44304; 
  p0_emc_m_sdz[0][0][4][1]=0.0626248; p1_emc_m_sdz[0][0][4][1]=-0.166937; p2_emc_m_sdz[0][0][4][1]=0.135296; p3_emc_m_sdz[0][0][4][1]=-0.0449937; p4_emc_m_sdz[0][0][4][1]=0.00520539; 
  p0_emc_s_sdz[0][0][4][1]=0.845586; p1_emc_s_sdz[0][0][4][1]=0.261101; p2_emc_s_sdz[0][0][4][1]=-0.210035; p3_emc_s_sdz[0][0][4][1]=0.0695707; p4_emc_s_sdz[0][0][4][1]=-0.00730572; 
  p0_emc_m_sdz[1][0][4][1]=-0.260307; p1_emc_m_sdz[1][0][4][1]=0.953035; p2_emc_m_sdz[1][0][4][1]=-0.330609; p3_emc_m_sdz[1][0][4][1]=-2.52395; p4_emc_m_sdz[1][0][4][1]=2.60975; 
  p0_emc_s_sdz[1][0][4][1]=0.621552; p1_emc_s_sdz[1][0][4][1]=1.49949; p2_emc_s_sdz[1][0][4][1]=-0.861295; p3_emc_s_sdz[1][0][4][1]=-3.73796; p4_emc_s_sdz[1][0][4][1]=4.19283; 
  p0_emc_m_sdz[0][0][5][1]=-0.086194; p1_emc_m_sdz[0][0][5][1]=0.195749; p2_emc_m_sdz[0][0][5][1]=-0.161771; p3_emc_m_sdz[0][0][5][1]=0.0521263; p4_emc_m_sdz[0][0][5][1]=-0.0057119; 
  p0_emc_s_sdz[0][0][5][1]=0.824413; p1_emc_s_sdz[0][0][5][1]=0.309556; p2_emc_s_sdz[0][0][5][1]=-0.243649; p3_emc_s_sdz[0][0][5][1]=0.0780902; p4_emc_s_sdz[0][0][5][1]=-0.00805701; 
  p0_emc_m_sdz[1][0][5][1]=0.0125183; p1_emc_m_sdz[1][0][5][1]=0.0106241; p2_emc_m_sdz[1][0][5][1]=-0.00178613; p3_emc_m_sdz[1][0][5][1]=-0.036863; p4_emc_m_sdz[1][0][5][1]=-0.116995; 
  p0_emc_s_sdz[1][0][5][1]=1.13677; p1_emc_s_sdz[1][0][5][1]=-0.795466; p2_emc_s_sdz[1][0][5][1]=0.227325; p3_emc_s_sdz[1][0][5][1]=2.52702; p4_emc_s_sdz[1][0][5][1]=-2.58091; 
  p0_emc_m_sdz[0][0][6][1]=-0.175086; p1_emc_m_sdz[0][0][6][1]=0.401089; p2_emc_m_sdz[0][0][6][1]=-0.323555; p3_emc_m_sdz[0][0][6][1]=0.102777; p4_emc_m_sdz[0][0][6][1]=-0.0111707; 
  p0_emc_s_sdz[0][0][6][1]=0.788076; p1_emc_s_sdz[0][0][6][1]=0.410371; p2_emc_s_sdz[0][0][6][1]=-0.32464; p3_emc_s_sdz[0][0][6][1]=0.1023; p4_emc_s_sdz[0][0][6][1]=-0.0104678; 
  p0_emc_m_sdz[1][0][6][1]=-0.819986; p1_emc_m_sdz[1][0][6][1]=3.31103; p2_emc_m_sdz[1][0][6][1]=-0.867968; p3_emc_m_sdz[1][0][6][1]=-9.22151; p4_emc_m_sdz[1][0][6][1]=8.73387; 
  p0_emc_s_sdz[1][0][6][1]=0.639616; p1_emc_s_sdz[1][0][6][1]=1.27546; p2_emc_s_sdz[1][0][6][1]=-0.55757; p3_emc_s_sdz[1][0][6][1]=-2.89835; p4_emc_s_sdz[1][0][6][1]=2.89758; 
  p0_emc_m_sdz[0][0][7][1]=-0.267485; p1_emc_m_sdz[0][0][7][1]=0.598671; p2_emc_m_sdz[0][0][7][1]=-0.473835; p3_emc_m_sdz[0][0][7][1]=0.149876; p4_emc_m_sdz[0][0][7][1]=-0.0163813; 
  p0_emc_s_sdz[0][0][7][1]=0.795459; p1_emc_s_sdz[0][0][7][1]=0.381192; p2_emc_s_sdz[0][0][7][1]=-0.296545; p3_emc_s_sdz[0][0][7][1]=0.0923956; p4_emc_s_sdz[0][0][7][1]=-0.0093452; 
  p0_emc_m_sdz[1][0][7][1]=-0.129353; p1_emc_m_sdz[1][0][7][1]=0.681072; p2_emc_m_sdz[1][0][7][1]=-0.20261; p3_emc_m_sdz[1][0][7][1]=-2.08373; p4_emc_m_sdz[1][0][7][1]=1.85781; 
  p0_emc_s_sdz[1][0][7][1]=1.19787; p1_emc_s_sdz[1][0][7][1]=-0.883552; p2_emc_s_sdz[1][0][7][1]=0.0337344; p3_emc_s_sdz[1][0][7][1]=2.85571; p4_emc_s_sdz[1][0][7][1]=-2.60737; 
  p0_emc_m_sdz[0][0][8][1]=-0.337201; p1_emc_m_sdz[0][0][8][1]=0.751119; p2_emc_m_sdz[0][0][8][1]=-0.591786; p3_emc_m_sdz[0][0][8][1]=0.186633; p4_emc_m_sdz[0][0][8][1]=-0.0204276; 
  p0_emc_s_sdz[0][0][8][1]=0.880998; p1_emc_s_sdz[0][0][8][1]=0.163971; p2_emc_s_sdz[0][0][8][1]=-0.109203; p3_emc_s_sdz[0][0][8][1]=0.0283571; p4_emc_s_sdz[0][0][8][1]=-0.00210273; 
  p0_emc_m_sdz[1][0][8][1]=-0.643973; p1_emc_m_sdz[1][0][8][1]=2.66676; p2_emc_m_sdz[1][0][8][1]=-0.590648; p3_emc_m_sdz[1][0][8][1]=-7.53506; p4_emc_m_sdz[1][0][8][1]=6.79788; 
  p0_emc_s_sdz[1][0][8][1]=1.02619; p1_emc_s_sdz[1][0][8][1]=-0.261612; p2_emc_s_sdz[1][0][8][1]=-0.112393; p3_emc_s_sdz[1][0][8][1]=1.20041; p4_emc_s_sdz[1][0][8][1]=-1.01508; 
  p0_emc_m_sdz[0][0][9][1]=-0.367448; p1_emc_m_sdz[0][0][9][1]=0.822019; p2_emc_m_sdz[0][0][9][1]=-0.656413; p3_emc_m_sdz[0][0][9][1]=0.209546; p4_emc_m_sdz[0][0][9][1]=-0.0233786; 
  p0_emc_s_sdz[0][0][9][1]=0.94171; p1_emc_s_sdz[0][0][9][1]=0.0470819; p2_emc_s_sdz[0][0][9][1]=-0.0234873; p3_emc_s_sdz[0][0][9][1]=0.00467339; p4_emc_s_sdz[0][0][9][1]=-2.86782e-05; 
  p0_emc_m_sdz[1][0][9][1]=-0.00962635; p1_emc_m_sdz[1][0][9][1]=0.0989559; p2_emc_m_sdz[1][0][9][1]=0.162512; p3_emc_m_sdz[1][0][9][1]=-0.0165318; p4_emc_m_sdz[1][0][9][1]=-0.825608; 
  p0_emc_s_sdz[1][0][9][1]=1.24571; p1_emc_s_sdz[1][0][9][1]=-1.09534; p2_emc_s_sdz[1][0][9][1]=0.0126387; p3_emc_s_sdz[1][0][9][1]=3.76295; p4_emc_s_sdz[1][0][9][1]=-3.3227; 
  p0_emc_m_sdz[0][1][0][1]=-0.179613; p1_emc_m_sdz[0][1][0][1]=0.223069; p2_emc_m_sdz[0][1][0][1]=-0.0922131; p3_emc_m_sdz[0][1][0][1]=0.0133772; p4_emc_m_sdz[0][1][0][1]=-0.000209947; 
  p0_emc_s_sdz[0][1][0][1]=0.796692; p1_emc_s_sdz[0][1][0][1]=0.287505; p2_emc_s_sdz[0][1][0][1]=-0.178996; p3_emc_s_sdz[0][1][0][1]=0.0525515; p4_emc_s_sdz[0][1][0][1]=-0.00557959; 
  p0_emc_m_sdz[1][1][0][1]=0.954296; p1_emc_m_sdz[1][1][0][1]=-3.92726; p2_emc_m_sdz[1][1][0][1]=0.788481; p3_emc_m_sdz[1][1][0][1]=10.2287; p4_emc_m_sdz[1][1][0][1]=-8.89737; 
  p0_emc_s_sdz[1][1][0][1]=1.57122; p1_emc_s_sdz[1][1][0][1]=-2.45415; p2_emc_s_sdz[1][1][0][1]=0.37344; p3_emc_s_sdz[1][1][0][1]=7.06938; p4_emc_s_sdz[1][1][0][1]=-6.43064; 
  p0_emc_m_sdz[0][1][1][1]=-0.115704; p1_emc_m_sdz[0][1][1][1]=0.154134; p2_emc_m_sdz[0][1][1][1]=-0.0716434; p3_emc_m_sdz[0][1][1][1]=0.0134118; p4_emc_m_sdz[0][1][1][1]=-0.000752662; 
  p0_emc_s_sdz[0][1][1][1]=0.93279; p1_emc_s_sdz[0][1][1][1]=0.00190457; p2_emc_s_sdz[0][1][1][1]=0.0127873; p3_emc_s_sdz[0][1][1][1]=-0.000397748; p4_emc_s_sdz[0][1][1][1]=-0.000360428; 
  p0_emc_m_sdz[1][1][1][1]=0.125539; p1_emc_m_sdz[1][1][1][1]=-0.445891; p2_emc_m_sdz[1][1][1][1]=-0.16502; p3_emc_m_sdz[1][1][1][1]=0.696568; p4_emc_m_sdz[1][1][1][1]=0.128336; 
  p0_emc_s_sdz[1][1][1][1]=1.30763; p1_emc_s_sdz[1][1][1][1]=-1.35459; p2_emc_s_sdz[1][1][1][1]=0.101719; p3_emc_s_sdz[1][1][1][1]=3.86048; p4_emc_s_sdz[1][1][1][1]=-3.33112; 
  p0_emc_m_sdz[0][1][2][1]=-0.0999924; p1_emc_m_sdz[0][1][2][1]=0.124395; p2_emc_m_sdz[0][1][2][1]=-0.0573363; p3_emc_m_sdz[0][1][2][1]=0.0106593; p4_emc_m_sdz[0][1][2][1]=-0.000555215; 
  p0_emc_s_sdz[0][1][2][1]=0.938394; p1_emc_s_sdz[0][1][2][1]=-0.0494245; p2_emc_s_sdz[0][1][2][1]=0.0734142; p3_emc_s_sdz[0][1][2][1]=-0.0232059; p4_emc_s_sdz[0][1][2][1]=0.00232426; 
  p0_emc_m_sdz[1][1][2][1]=0.021975; p1_emc_m_sdz[1][1][2][1]=-0.0868819; p2_emc_m_sdz[1][1][2][1]=-0.169926; p3_emc_m_sdz[1][1][2][1]=-0.0605968; p4_emc_m_sdz[1][1][2][1]=0.575079; 
  p0_emc_s_sdz[1][1][2][1]=0.410909; p1_emc_s_sdz[1][1][2][1]=2.06493; p2_emc_s_sdz[1][1][2][1]=-0.74798; p3_emc_s_sdz[1][1][2][1]=-5.38363; p4_emc_s_sdz[1][1][2][1]=5.46373; 
  p0_emc_m_sdz[0][1][3][1]=-0.088021; p1_emc_m_sdz[0][1][3][1]=0.0888687; p2_emc_m_sdz[0][1][3][1]=-0.0305911; p3_emc_m_sdz[0][1][3][1]=0.00292616; p4_emc_m_sdz[0][1][3][1]=0.000184378; 
  p0_emc_s_sdz[0][1][3][1]=0.889229; p1_emc_s_sdz[0][1][3][1]=0.0802963; p2_emc_s_sdz[0][1][3][1]=-0.0131312; p3_emc_s_sdz[0][1][3][1]=-0.000186444; p4_emc_s_sdz[0][1][3][1]=0.000153849; 
  p0_emc_m_sdz[1][1][3][1]=0.306704; p1_emc_m_sdz[1][1][3][1]=-1.32431; p2_emc_m_sdz[1][1][3][1]=0.287577; p3_emc_m_sdz[1][1][3][1]=3.38143; p4_emc_m_sdz[1][1][3][1]=-2.97232; 
  p0_emc_s_sdz[1][1][3][1]=1.57623; p1_emc_s_sdz[1][1][3][1]=-2.4893; p2_emc_s_sdz[1][1][3][1]=0.404356; p3_emc_s_sdz[1][1][3][1]=7.12948; p4_emc_s_sdz[1][1][3][1]=-6.463; 
  p0_emc_m_sdz[0][1][4][1]=-0.0476095; p1_emc_m_sdz[0][1][4][1]=0.0356879; p2_emc_m_sdz[0][1][4][1]=-0.00509742; p3_emc_m_sdz[0][1][4][1]=-0.00167279; p4_emc_m_sdz[0][1][4][1]=0.00037176; 
  p0_emc_s_sdz[0][1][4][1]=0.99584; p1_emc_s_sdz[0][1][4][1]=-0.0625472; p2_emc_s_sdz[0][1][4][1]=0.069918; p3_emc_s_sdz[0][1][4][1]=-0.023741; p4_emc_s_sdz[0][1][4][1]=0.00267556; 
  p0_emc_m_sdz[1][1][4][1]=0.753628; p1_emc_m_sdz[1][1][4][1]=-3.14641; p2_emc_m_sdz[1][1][4][1]=0.921958; p3_emc_m_sdz[1][1][4][1]=8.78243; p4_emc_m_sdz[1][1][4][1]=-8.619; 
  p0_emc_s_sdz[1][1][4][1]=1.44655; p1_emc_s_sdz[1][1][4][1]=-1.92676; p2_emc_s_sdz[1][1][4][1]=0.390438; p3_emc_s_sdz[1][1][4][1]=5.90986; p4_emc_s_sdz[1][1][4][1]=-5.66797; 
  p0_emc_m_sdz[0][1][5][1]=0.0461074; p1_emc_m_sdz[0][1][5][1]=-0.0939274; p2_emc_m_sdz[0][1][5][1]=0.065851; p3_emc_m_sdz[0][1][5][1]=-0.0188538; p4_emc_m_sdz[0][1][5][1]=0.00189073; 
  p0_emc_s_sdz[0][1][5][1]=1.03774; p1_emc_s_sdz[0][1][5][1]=-0.10914; p2_emc_s_sdz[0][1][5][1]=0.0919737; p3_emc_s_sdz[0][1][5][1]=-0.0288227; p4_emc_s_sdz[0][1][5][1]=0.00308553; 
  p0_emc_m_sdz[1][1][5][1]=-0.402012; p1_emc_m_sdz[1][1][5][1]=1.47811; p2_emc_m_sdz[1][1][5][1]=-0.255441; p3_emc_m_sdz[1][1][5][1]=-4.02157; p4_emc_m_sdz[1][1][5][1]=3.67669; 
  p0_emc_s_sdz[1][1][5][1]=0.968185; p1_emc_s_sdz[1][1][5][1]=0.0442298; p2_emc_s_sdz[1][1][5][1]=-0.184011; p3_emc_s_sdz[1][1][5][1]=0.533745; p4_emc_s_sdz[1][1][5][1]=-0.416371; 
  p0_emc_m_sdz[0][1][6][1]=0.0620902; p1_emc_m_sdz[0][1][6][1]=-0.134906; p2_emc_m_sdz[0][1][6][1]=0.0944079; p3_emc_m_sdz[0][1][6][1]=-0.0268598; p4_emc_m_sdz[0][1][6][1]=0.00272384; 
  p0_emc_s_sdz[0][1][6][1]=1.0295; p1_emc_s_sdz[0][1][6][1]=-0.0904171; p2_emc_s_sdz[0][1][6][1]=0.0697988; p3_emc_s_sdz[0][1][6][1]=-0.0189779; p4_emc_s_sdz[0][1][6][1]=0.00175927; 
  p0_emc_m_sdz[1][1][6][1]=-0.351723; p1_emc_m_sdz[1][1][6][1]=1.27959; p2_emc_m_sdz[1][1][6][1]=-0.253337; p3_emc_m_sdz[1][1][6][1]=-3.44028; p4_emc_m_sdz[1][1][6][1]=3.18138; 
  p0_emc_s_sdz[1][1][6][1]=0.598353; p1_emc_s_sdz[1][1][6][1]=1.50993; p2_emc_s_sdz[1][1][6][1]=-0.610497; p3_emc_s_sdz[1][1][6][1]=-3.60838; p4_emc_s_sdz[1][1][6][1]=3.69579; 
  p0_emc_m_sdz[0][1][7][1]=0.0763826; p1_emc_m_sdz[0][1][7][1]=-0.149784; p2_emc_m_sdz[0][1][7][1]=0.098507; p3_emc_m_sdz[0][1][7][1]=-0.027275; p4_emc_m_sdz[0][1][7][1]=0.0027705; 
  p0_emc_s_sdz[0][1][7][1]=0.996721; p1_emc_s_sdz[0][1][7][1]=-0.0325897; p2_emc_s_sdz[0][1][7][1]=0.0323067; p3_emc_s_sdz[0][1][7][1]=-0.00883223; p4_emc_s_sdz[0][1][7][1]=0.000811323; 
  p0_emc_m_sdz[1][1][7][1]=-0.134807; p1_emc_m_sdz[1][1][7][1]=0.34001; p2_emc_m_sdz[1][1][7][1]=0.13593; p3_emc_m_sdz[1][1][7][1]=-0.634089; p4_emc_m_sdz[1][1][7][1]=0.138743; 
  p0_emc_s_sdz[1][1][7][1]=1.05704; p1_emc_s_sdz[1][1][7][1]=-0.34897; p2_emc_s_sdz[1][1][7][1]=-0.0507891; p3_emc_s_sdz[1][1][7][1]=1.5451; p4_emc_s_sdz[1][1][7][1]=-1.41833; 
  p0_emc_m_sdz[0][1][8][1]=0.104721; p1_emc_m_sdz[0][1][8][1]=-0.179844; p2_emc_m_sdz[0][1][8][1]=0.109401; p3_emc_m_sdz[0][1][8][1]=-0.0285091; p4_emc_m_sdz[0][1][8][1]=0.00277285; 
  p0_emc_s_sdz[0][1][8][1]=0.94752; p1_emc_s_sdz[0][1][8][1]=0.0746196; p2_emc_s_sdz[0][1][8][1]=-0.0491522; p3_emc_s_sdz[0][1][8][1]=0.0162852; p4_emc_s_sdz[0][1][8][1]=-0.00188151; 
  p0_emc_m_sdz[1][1][8][1]=-0.664044; p1_emc_m_sdz[1][1][8][1]=2.5512; p2_emc_m_sdz[1][1][8][1]=-0.553528; p3_emc_m_sdz[1][1][8][1]=-6.67065; p4_emc_m_sdz[1][1][8][1]=6.03701; 
  p0_emc_s_sdz[1][1][8][1]=1.44678; p1_emc_s_sdz[1][1][8][1]=-1.95507; p2_emc_s_sdz[1][1][8][1]=0.415274; p3_emc_s_sdz[1][1][8][1]=6.16379; p4_emc_s_sdz[1][1][8][1]=-5.98466; 
  p0_emc_m_sdz[0][1][9][1]=0.0879069; p1_emc_m_sdz[0][1][9][1]=-0.119359; p2_emc_m_sdz[0][1][9][1]=0.0526498; p3_emc_m_sdz[0][1][9][1]=-0.00917885; p4_emc_m_sdz[0][1][9][1]=0.000621197; 
  p0_emc_s_sdz[0][1][9][1]=0.865071; p1_emc_s_sdz[0][1][9][1]=0.264966; p2_emc_s_sdz[0][1][9][1]=-0.185906; p3_emc_s_sdz[0][1][9][1]=0.0564339; p4_emc_s_sdz[0][1][9][1]=-0.00608139; 
  p0_emc_m_sdz[1][1][9][1]=-0.364625; p1_emc_m_sdz[1][1][9][1]=1.40009; p2_emc_m_sdz[1][1][9][1]=-0.0726892; p3_emc_m_sdz[1][1][9][1]=-3.36186; p4_emc_m_sdz[1][1][9][1]=2.3246; 
  p0_emc_s_sdz[1][1][9][1]=1.28076; p1_emc_s_sdz[1][1][9][1]=-1.19164; p2_emc_s_sdz[1][1][9][1]=0.00983943; p3_emc_s_sdz[1][1][9][1]=3.73005; p4_emc_s_sdz[1][1][9][1]=-3.13625; 
  p0_emc_m_sdz[0][2][0][1]=-0.239614; p1_emc_m_sdz[0][2][0][1]=0.308186; p2_emc_m_sdz[0][2][0][1]=-0.134934; p3_emc_m_sdz[0][2][0][1]=0.0240363; p4_emc_m_sdz[0][2][0][1]=-0.00129561; 
  p0_emc_s_sdz[0][2][0][1]=0.730059; p1_emc_s_sdz[0][2][0][1]=0.409185; p2_emc_s_sdz[0][2][0][1]=-0.233181; p3_emc_s_sdz[0][2][0][1]=0.0630791; p4_emc_s_sdz[0][2][0][1]=-0.00640804; 
  p0_emc_m_sdz[1][2][0][1]=0.426401; p1_emc_m_sdz[1][2][0][1]=-2.27981; p2_emc_m_sdz[1][2][0][1]=0.76699; p3_emc_m_sdz[1][2][0][1]=6.09139; p4_emc_m_sdz[1][2][0][1]=-5.63845; 
  p0_emc_s_sdz[1][2][0][1]=0.694236; p1_emc_s_sdz[1][2][0][1]=0.456759; p2_emc_s_sdz[1][2][0][1]=0.0677556; p3_emc_s_sdz[1][2][0][1]=-0.278813; p4_emc_s_sdz[1][2][0][1]=-0.104054; 
  p0_emc_m_sdz[0][2][1][1]=-0.199255; p1_emc_m_sdz[0][2][1][1]=0.312781; p2_emc_m_sdz[0][2][1][1]=-0.180154; p3_emc_m_sdz[0][2][1][1]=0.0455393; p4_emc_m_sdz[0][2][1][1]=-0.00417583; 
  p0_emc_s_sdz[0][2][1][1]=0.796514; p1_emc_s_sdz[0][2][1][1]=0.352901; p2_emc_s_sdz[0][2][1][1]=-0.232334; p3_emc_s_sdz[0][2][1][1]=0.0703723; p4_emc_s_sdz[0][2][1][1]=-0.00769692; 
  p0_emc_m_sdz[1][2][1][1]=0.461618; p1_emc_m_sdz[1][2][1][1]=-1.74041; p2_emc_m_sdz[1][2][1][1]=0.117578; p3_emc_m_sdz[1][2][1][1]=4.07511; p4_emc_m_sdz[1][2][1][1]=-2.98196; 
  p0_emc_s_sdz[1][2][1][1]=1.18642; p1_emc_s_sdz[1][2][1][1]=-0.8643; p2_emc_s_sdz[1][2][1][1]=-0.0602759; p3_emc_s_sdz[1][2][1][1]=2.60586; p4_emc_s_sdz[1][2][1][1]=-2.03429; 
  p0_emc_m_sdz[0][2][2][1]=-0.123914; p1_emc_m_sdz[0][2][2][1]=0.211039; p2_emc_m_sdz[0][2][2][1]=-0.131225; p3_emc_m_sdz[0][2][2][1]=0.0354545; p4_emc_m_sdz[0][2][2][1]=-0.00343001; 
  p0_emc_s_sdz[0][2][2][1]=0.942065; p1_emc_s_sdz[0][2][2][1]=0.0780257; p2_emc_s_sdz[0][2][2][1]=-0.0491955; p3_emc_s_sdz[0][2][2][1]=0.0180579; p4_emc_s_sdz[0][2][2][1]=-0.00223269; 
  p0_emc_m_sdz[1][2][2][1]=0.626507; p1_emc_m_sdz[1][2][2][1]=-2.4277; p2_emc_m_sdz[1][2][2][1]=0.574409; p3_emc_m_sdz[1][2][2][1]=6.27955; p4_emc_m_sdz[1][2][2][1]=-5.77262; 
  p0_emc_s_sdz[1][2][2][1]=0.87167; p1_emc_s_sdz[1][2][2][1]=0.374821; p2_emc_s_sdz[1][2][2][1]=-0.283135; p3_emc_s_sdz[1][2][2][1]=-0.559375; p4_emc_s_sdz[1][2][2][1]=0.735952; 
  p0_emc_m_sdz[0][2][3][1]=-0.102538; p1_emc_m_sdz[0][2][3][1]=0.184814; p2_emc_m_sdz[0][2][3][1]=-0.120587; p3_emc_m_sdz[0][2][3][1]=0.0336308; p4_emc_m_sdz[0][2][3][1]=-0.00333647; 
  p0_emc_s_sdz[0][2][3][1]=0.961699; p1_emc_s_sdz[0][2][3][1]=0.028084; p2_emc_s_sdz[0][2][3][1]=-0.00186161; p3_emc_s_sdz[0][2][3][1]=0.00145438; p4_emc_s_sdz[0][2][3][1]=-0.00034314; 
  p0_emc_m_sdz[1][2][3][1]=0.285502; p1_emc_m_sdz[1][2][3][1]=-1.04445; p2_emc_m_sdz[1][2][3][1]=0.180763; p3_emc_m_sdz[1][2][3][1]=2.56947; p4_emc_m_sdz[1][2][3][1]=-2.24752; 
  p0_emc_s_sdz[1][2][3][1]=1.01323; p1_emc_s_sdz[1][2][3][1]=-0.201823; p2_emc_s_sdz[1][2][3][1]=-0.0517608; p3_emc_s_sdz[1][2][3][1]=0.839378; p4_emc_s_sdz[1][2][3][1]=-0.64636; 
  p0_emc_m_sdz[0][2][4][1]=-0.0757589; p1_emc_m_sdz[0][2][4][1]=0.132118; p2_emc_m_sdz[0][2][4][1]=-0.0851067; p3_emc_m_sdz[0][2][4][1]=0.0235176; p4_emc_m_sdz[0][2][4][1]=-0.00234649; 
  p0_emc_s_sdz[0][2][4][1]=0.985815; p1_emc_s_sdz[0][2][4][1]=-0.0268126; p2_emc_s_sdz[0][2][4][1]=0.0509683; p3_emc_s_sdz[0][2][4][1]=-0.0189012; p4_emc_s_sdz[0][2][4][1]=0.00224249; 
  p0_emc_m_sdz[1][2][4][1]=0.2013; p1_emc_m_sdz[1][2][4][1]=-0.594286; p2_emc_m_sdz[1][2][4][1]=-0.0961871; p3_emc_m_sdz[1][2][4][1]=1.22018; p4_emc_m_sdz[1][2][4][1]=-0.6653; 
  p0_emc_s_sdz[1][2][4][1]=1.25738; p1_emc_s_sdz[1][2][4][1]=-1.08295; p2_emc_s_sdz[1][2][4][1]=0.0264122; p3_emc_s_sdz[1][2][4][1]=3.53452; p4_emc_s_sdz[1][2][4][1]=-3.07965; 
  p0_emc_m_sdz[0][2][5][1]=0.0105907; p1_emc_m_sdz[0][2][5][1]=-0.00489642; p2_emc_m_sdz[0][2][5][1]=-0.00801331; p3_emc_m_sdz[0][2][5][1]=0.00503471; p4_emc_m_sdz[0][2][5][1]=-0.000759848; 
  p0_emc_s_sdz[0][2][5][1]=1.00672; p1_emc_s_sdz[0][2][5][1]=0.023495; p2_emc_s_sdz[0][2][5][1]=-0.0230065; p3_emc_s_sdz[0][2][5][1]=0.00873985; p4_emc_s_sdz[0][2][5][1]=-0.00100599; 
  p0_emc_m_sdz[1][2][5][1]=0.0903619; p1_emc_m_sdz[1][2][5][1]=-0.276299; p2_emc_m_sdz[1][2][5][1]=-0.0231091; p3_emc_m_sdz[1][2][5][1]=0.840715; p4_emc_m_sdz[1][2][5][1]=-0.718362; 
  p0_emc_s_sdz[1][2][5][1]=1.17213; p1_emc_s_sdz[1][2][5][1]=-0.643111; p2_emc_s_sdz[1][2][5][1]=-0.0501641; p3_emc_s_sdz[1][2][5][1]=2.16261; p4_emc_s_sdz[1][2][5][1]=-1.7611; 
  p0_emc_m_sdz[0][2][6][1]=0.0454685; p1_emc_m_sdz[0][2][6][1]=-0.0699057; p2_emc_m_sdz[0][2][6][1]=0.0359788; p3_emc_m_sdz[0][2][6][1]=-0.00712211; p4_emc_m_sdz[0][2][6][1]=0.000419917; 
  p0_emc_s_sdz[0][2][6][1]=0.9604; p1_emc_s_sdz[0][2][6][1]=0.0550204; p2_emc_s_sdz[0][2][6][1]=-0.0426183; p3_emc_s_sdz[0][2][6][1]=0.0197045; p4_emc_s_sdz[0][2][6][1]=-0.0029765; 
  p0_emc_m_sdz[1][2][6][1]=-0.210183; p1_emc_m_sdz[1][2][6][1]=0.815967; p2_emc_m_sdz[1][2][6][1]=-0.175192; p3_emc_m_sdz[1][2][6][1]=-2.02947; p4_emc_m_sdz[1][2][6][1]=1.77842; 
  p0_emc_s_sdz[1][2][6][1]=1.47223; p1_emc_s_sdz[1][2][6][1]=-1.93681; p2_emc_s_sdz[1][2][6][1]=0.309553; p3_emc_s_sdz[1][2][6][1]=5.7786; p4_emc_s_sdz[1][2][6][1]=-5.36097; 
  p0_emc_m_sdz[0][2][7][1]=0.0740062; p1_emc_m_sdz[0][2][7][1]=-0.117832; p2_emc_m_sdz[0][2][7][1]=0.0675633; p3_emc_m_sdz[0][2][7][1]=-0.0163391; p4_emc_m_sdz[0][2][7][1]=0.00136113; 
  p0_emc_s_sdz[0][2][7][1]=0.910634; p1_emc_s_sdz[0][2][7][1]=0.0551392; p2_emc_s_sdz[0][2][7][1]=-0.00160383; p3_emc_s_sdz[0][2][7][1]=0.000105012; p4_emc_s_sdz[0][2][7][1]=-0.000420302; 
  p0_emc_m_sdz[1][2][7][1]=-0.317539; p1_emc_m_sdz[1][2][7][1]=1.07967; p2_emc_m_sdz[1][2][7][1]=-0.0575218; p3_emc_m_sdz[1][2][7][1]=-2.48545; p4_emc_m_sdz[1][2][7][1]=1.85331; 
  p0_emc_s_sdz[1][2][7][1]=0.774958; p1_emc_s_sdz[1][2][7][1]=0.797936; p2_emc_s_sdz[1][2][7][1]=-0.407979; p3_emc_s_sdz[1][2][7][1]=-2.12562; p4_emc_s_sdz[1][2][7][1]=2.25182; 
  p0_emc_m_sdz[0][2][8][1]=0.0995943; p1_emc_m_sdz[0][2][8][1]=-0.15921; p2_emc_m_sdz[0][2][8][1]=0.0875622; p3_emc_m_sdz[0][2][8][1]=-0.0200459; p4_emc_m_sdz[0][2][8][1]=0.00154551; 
  p0_emc_s_sdz[0][2][8][1]=0.883; p1_emc_s_sdz[0][2][8][1]=0.1564; p2_emc_s_sdz[0][2][8][1]=-0.0901723; p3_emc_s_sdz[0][2][8][1]=0.0291841; p4_emc_s_sdz[0][2][8][1]=-0.00365984; 
  p0_emc_m_sdz[1][2][8][1]=-0.625243; p1_emc_m_sdz[1][2][8][1]=2.18936; p2_emc_m_sdz[1][2][8][1]=-0.157797; p3_emc_m_sdz[1][2][8][1]=-5.32289; p4_emc_m_sdz[1][2][8][1]=4.12664; 
  p0_emc_s_sdz[1][2][8][1]=1.13557; p1_emc_s_sdz[1][2][8][1]=-0.566185; p2_emc_s_sdz[1][2][8][1]=-0.166522; p3_emc_s_sdz[1][2][8][1]=1.92193; p4_emc_s_sdz[1][2][8][1]=-1.51102; 
  p0_emc_m_sdz[0][2][9][1]=0.189507; p1_emc_m_sdz[0][2][9][1]=-0.319588; p2_emc_m_sdz[0][2][9][1]=0.18519; p3_emc_m_sdz[0][2][9][1]=-0.0449133; p4_emc_m_sdz[0][2][9][1]=0.00372246; 
  p0_emc_s_sdz[0][2][9][1]=0.77891; p1_emc_s_sdz[0][2][9][1]=0.373892; p2_emc_s_sdz[0][2][9][1]=-0.242736; p3_emc_s_sdz[0][2][9][1]=0.0738086; p4_emc_s_sdz[0][2][9][1]=-0.00834746; 
  p0_emc_m_sdz[1][2][9][1]=-0.746033; p1_emc_m_sdz[1][2][9][1]=2.71874; p2_emc_m_sdz[1][2][9][1]=-0.241411; p3_emc_m_sdz[1][2][9][1]=-6.39899; p4_emc_m_sdz[1][2][9][1]=4.81789; 
  p0_emc_s_sdz[1][2][9][1]=1.53416; p1_emc_s_sdz[1][2][9][1]=-2.11204; p2_emc_s_sdz[1][2][9][1]=0.156778; p3_emc_s_sdz[1][2][9][1]=5.68931; p4_emc_s_sdz[1][2][9][1]=-4.78601; 

  //! ToF sdphi neg
  p0_tof_m_sdp[0][0][0][0]=-0.182235; p1_tof_m_sdp[0][0][0][0]=0.400427; p2_tof_m_sdp[0][0][0][0]=-0.31887; p3_tof_m_sdp[0][0][0][0]=0.104587; p4_tof_m_sdp[0][0][0][0]=-0.011173; 
  p0_tof_s_sdp[0][0][0][0]=0.729798; p1_tof_s_sdp[0][0][0][0]=0.204556; p2_tof_s_sdp[0][0][0][0]=-0.0909085; p3_tof_s_sdp[0][0][0][0]=0.0146088; p4_tof_s_sdp[0][0][0][0]=-0.000485029; 
  p0_tof_m_sdp[1][0][0][0]=-0.543723; p1_tof_m_sdp[1][0][0][0]=0.529362; p2_tof_m_sdp[1][0][0][0]=1.52401; p3_tof_m_sdp[1][0][0][0]=1.02261; p4_tof_m_sdp[1][0][0][0]=-4.22877; 
  p0_tof_s_sdp[1][0][0][0]=-0.59264; p1_tof_s_sdp[1][0][0][0]=1.77784; p2_tof_s_sdp[1][0][0][0]=3.67002; p3_tof_s_sdp[1][0][0][0]=1.91919; p4_tof_s_sdp[1][0][0][0]=-10.4984; 
  p0_tof_m_sdp[0][0][1][0]=-0.151111; p1_tof_m_sdp[0][0][1][0]=0.352251; p2_tof_m_sdp[0][0][1][0]=-0.285015; p3_tof_m_sdp[0][0][1][0]=0.0934032; p4_tof_m_sdp[0][0][1][0]=-0.0100065; 
  p0_tof_s_sdp[0][0][1][0]=0.818989; p1_tof_s_sdp[0][0][1][0]=0.0461097; p2_tof_s_sdp[0][0][1][0]=0.0193298; p3_tof_s_sdp[0][0][1][0]=-0.0190674; p4_tof_s_sdp[0][0][1][0]=0.00350817; 
  p0_tof_m_sdp[1][0][1][0]=-0.292591; p1_tof_m_sdp[1][0][1][0]=0.321976; p2_tof_m_sdp[1][0][1][0]=0.862284; p3_tof_m_sdp[1][0][1][0]=0.512535; p4_tof_m_sdp[1][0][1][0]=-2.54538; 
  p0_tof_s_sdp[1][0][1][0]=-39.4426; p1_tof_s_sdp[1][0][1][0]=145.701; p2_tof_s_sdp[1][0][1][0]=-25.0386; p3_tof_s_sdp[1][0][1][0]=-383.271; p4_tof_s_sdp[1][0][1][0]=344.442; 
  p0_tof_m_sdp[0][0][2][0]=-0.113764; p1_tof_m_sdp[0][0][2][0]=0.274129; p2_tof_m_sdp[0][0][2][0]=-0.224973; p3_tof_m_sdp[0][0][2][0]=0.0747784; p4_tof_m_sdp[0][0][2][0]=-0.00803465; 
  p0_tof_s_sdp[0][0][2][0]=0.658969; p1_tof_s_sdp[0][0][2][0]=0.468109; p2_tof_s_sdp[0][0][2][0]=-0.359617; p3_tof_s_sdp[0][0][2][0]=0.115861; p4_tof_s_sdp[0][0][2][0]=-0.0128512; 
  p0_tof_m_sdp[1][0][2][0]=-0.100178; p1_tof_m_sdp[1][0][2][0]=0.0983727; p2_tof_m_sdp[1][0][2][0]=0.28774; p3_tof_m_sdp[1][0][2][0]=0.209718; p4_tof_m_sdp[1][0][2][0]=-0.732206; 
  p0_tof_s_sdp[1][0][2][0]=0.828523; p1_tof_s_sdp[1][0][2][0]=0.348894; p2_tof_s_sdp[1][0][2][0]=-0.363381; p3_tof_s_sdp[1][0][2][0]=-0.754383; p4_tof_s_sdp[1][0][2][0]=0.819925; 
  p0_tof_m_sdp[0][0][3][0]=-0.102977; p1_tof_m_sdp[0][0][3][0]=0.224177; p2_tof_m_sdp[0][0][3][0]=-0.201605; p3_tof_m_sdp[0][0][3][0]=0.0708232; p4_tof_m_sdp[0][0][3][0]=-0.00791762; 
  p0_tof_s_sdp[0][0][3][0]=0.797608; p1_tof_s_sdp[0][0][3][0]=0.106379; p2_tof_s_sdp[0][0][3][0]=-0.0957192; p3_tof_s_sdp[0][0][3][0]=0.0406278; p4_tof_s_sdp[0][0][3][0]=-0.0054644; 
  p0_tof_m_sdp[1][0][3][0]=-0.365121; p1_tof_m_sdp[1][0][3][0]=0.37368; p2_tof_m_sdp[1][0][3][0]=1.04118; p3_tof_m_sdp[1][0][3][0]=0.667055; p4_tof_m_sdp[1][0][3][0]=-2.93592; 
  p0_tof_s_sdp[1][0][3][0]=-48.4412; p1_tof_s_sdp[1][0][3][0]=176.573; p2_tof_s_sdp[1][0][3][0]=-25.7958; p3_tof_s_sdp[1][0][3][0]=-468.991; p4_tof_s_sdp[1][0][3][0]=415.868; 
  p0_tof_m_sdp[0][0][4][0]=-0.0290561; p1_tof_m_sdp[0][0][4][0]=0.0537035; p2_tof_m_sdp[0][0][4][0]=-0.057385; p3_tof_m_sdp[0][0][4][0]=0.0232206; p4_tof_m_sdp[0][0][4][0]=-0.00283587; 
  p0_tof_s_sdp[0][0][4][0]=0.647; p1_tof_s_sdp[0][0][4][0]=0.45539; p2_tof_s_sdp[0][0][4][0]=-0.348524; p3_tof_s_sdp[0][0][4][0]=0.1138; p4_tof_s_sdp[0][0][4][0]=-0.0128304; 
  p0_tof_m_sdp[1][0][4][0]=-0.203715; p1_tof_m_sdp[1][0][4][0]=0.140554; p2_tof_m_sdp[1][0][4][0]=0.490095; p3_tof_m_sdp[1][0][4][0]=0.38059; p4_tof_m_sdp[1][0][4][0]=-1.31261; 
  p0_tof_s_sdp[1][0][4][0]=-0.191782; p1_tof_s_sdp[1][0][4][0]=1.19683; p2_tof_s_sdp[1][0][4][0]=2.35099; p3_tof_s_sdp[1][0][4][0]=1.32689; p4_tof_s_sdp[1][0][4][0]=-6.2748; 
  p0_tof_m_sdp[0][0][5][0]=-0.0738465; p1_tof_m_sdp[0][0][5][0]=0.149381; p2_tof_m_sdp[0][0][5][0]=-0.127866; p3_tof_m_sdp[0][0][5][0]=0.0453504; p4_tof_m_sdp[0][0][5][0]=-0.0050767; 
  p0_tof_s_sdp[0][0][5][0]=0.485396; p1_tof_s_sdp[0][0][5][0]=0.806611; p2_tof_s_sdp[0][0][5][0]=-0.574531; p3_tof_s_sdp[0][0][5][0]=0.17237; p4_tof_s_sdp[0][0][5][0]=-0.0182344; 
  p0_tof_m_sdp[1][0][5][0]=0.236316; p1_tof_m_sdp[1][0][5][0]=-0.245319; p2_tof_m_sdp[1][0][5][0]=-0.679358; p3_tof_m_sdp[1][0][5][0]=-0.448801; p4_tof_m_sdp[1][0][5][0]=1.83609; 
  p0_tof_s_sdp[1][0][5][0]=37.9784; p1_tof_s_sdp[1][0][5][0]=-132.313; p2_tof_s_sdp[1][0][5][0]=18.1554; p3_tof_s_sdp[1][0][5][0]=352.712; p4_tof_s_sdp[1][0][5][0]=-311.938; 
  p0_tof_m_sdp[0][0][6][0]=-0.060888; p1_tof_m_sdp[0][0][6][0]=0.200653; p2_tof_m_sdp[0][0][6][0]=-0.209555; p3_tof_m_sdp[0][0][6][0]=0.0796032; p4_tof_m_sdp[0][0][6][0]=-0.00929023; 
  p0_tof_s_sdp[0][0][6][0]=0.627476; p1_tof_s_sdp[0][0][6][0]=0.52238; p2_tof_s_sdp[0][0][6][0]=-0.362794; p3_tof_s_sdp[0][0][6][0]=0.107948; p4_tof_s_sdp[0][0][6][0]=-0.0113835; 
  p0_tof_m_sdp[1][0][6][0]=0.10077; p1_tof_m_sdp[1][0][6][0]=-0.166313; p2_tof_m_sdp[1][0][6][0]=-0.384158; p3_tof_m_sdp[1][0][6][0]=-0.18311; p4_tof_m_sdp[1][0][6][0]=1.25547; 
  p0_tof_s_sdp[1][0][6][0]=-29.0415; p1_tof_s_sdp[1][0][6][0]=111.159; p2_tof_s_sdp[1][0][6][0]=-21.7149; p3_tof_s_sdp[1][0][6][0]=-299.429; p4_tof_s_sdp[1][0][6][0]=275.209; 
  p0_tof_m_sdp[0][0][7][0]=-0.227739; p1_tof_m_sdp[0][0][7][0]=0.598892; p2_tof_m_sdp[0][0][7][0]=-0.51226; p3_tof_m_sdp[0][0][7][0]=0.171613; p4_tof_m_sdp[0][0][7][0]=-0.0188211; 
  p0_tof_s_sdp[0][0][7][0]=0.78828; p1_tof_s_sdp[0][0][7][0]=0.227984; p2_tof_s_sdp[0][0][7][0]=-0.149359; p3_tof_s_sdp[0][0][7][0]=0.0408944; p4_tof_s_sdp[0][0][7][0]=-0.00369843; 
  p0_tof_m_sdp[1][0][7][0]=-0.476707; p1_tof_m_sdp[1][0][7][0]=0.452872; p2_tof_m_sdp[1][0][7][0]=1.32628; p3_tof_m_sdp[1][0][7][0]=0.911207; p4_tof_m_sdp[1][0][7][0]=-3.6303; 
  p0_tof_s_sdp[1][0][7][0]=-50.2542; p1_tof_s_sdp[1][0][7][0]=177.256; p2_tof_s_sdp[1][0][7][0]=-14.3617; p3_tof_s_sdp[1][0][7][0]=-476.049; p4_tof_s_sdp[1][0][7][0]=407.465; 
  p0_tof_m_sdp[0][0][8][0]=-0.390694; p1_tof_m_sdp[0][0][8][0]=0.907158; p2_tof_m_sdp[0][0][8][0]=-0.706364; p3_tof_m_sdp[0][0][8][0]=0.222595; p4_tof_m_sdp[0][0][8][0]=-0.0236237; 
  p0_tof_s_sdp[0][0][8][0]=0.561789; p1_tof_s_sdp[0][0][8][0]=0.69262; p2_tof_s_sdp[0][0][8][0]=-0.474127; p3_tof_s_sdp[0][0][8][0]=0.133221; p4_tof_s_sdp[0][0][8][0]=-0.0126693; 
  p0_tof_m_sdp[1][0][8][0]=-0.736464; p1_tof_m_sdp[1][0][8][0]=0.797138; p2_tof_m_sdp[1][0][8][0]=2.2001; p3_tof_m_sdp[1][0][8][0]=1.43662; p4_tof_m_sdp[1][0][8][0]=-6.24593; 
  p0_tof_s_sdp[1][0][8][0]=49.6131; p1_tof_s_sdp[1][0][8][0]=-171.92; p2_tof_s_sdp[1][0][8][0]=16.5569; p3_tof_s_sdp[1][0][8][0]=468.279; p4_tof_s_sdp[1][0][8][0]=-406.222; 
  p0_tof_m_sdp[0][0][9][0]=-0.418594; p1_tof_m_sdp[0][0][9][0]=1.06984; p2_tof_m_sdp[0][0][9][0]=-0.89011; p3_tof_m_sdp[0][0][9][0]=0.292217; p4_tof_m_sdp[0][0][9][0]=-0.03186; 
  p0_tof_s_sdp[0][0][9][0]=0.756239; p1_tof_s_sdp[0][0][9][0]=0.377127; p2_tof_s_sdp[0][0][9][0]=-0.30137; p3_tof_s_sdp[0][0][9][0]=0.0926133; p4_tof_s_sdp[0][0][9][0]=-0.00938697; 
  p0_tof_m_sdp[1][0][9][0]=-1.05759; p1_tof_m_sdp[1][0][9][0]=1.09283; p2_tof_m_sdp[1][0][9][0]=3.10382; p3_tof_m_sdp[1][0][9][0]=2.10499; p4_tof_m_sdp[1][0][9][0]=-8.49702; 
  p0_tof_s_sdp[1][0][9][0]=5.40733; p1_tof_s_sdp[1][0][9][0]=-3.65086; p2_tof_s_sdp[1][0][9][0]=-12.9141; p3_tof_s_sdp[1][0][9][0]=-10.0519; p4_tof_s_sdp[1][0][9][0]=33.9794; 
  p0_tof_m_sdp[0][1][0][0]=0.148454; p1_tof_m_sdp[0][1][0][0]=-0.431079; p2_tof_m_sdp[0][1][0][0]=0.388566; p3_tof_m_sdp[0][1][0][0]=-0.138248; p4_tof_m_sdp[0][1][0][0]=0.0161984; 
  p0_tof_s_sdp[0][1][0][0]=0.0623083; p1_tof_s_sdp[0][1][0][0]=1.82071; p2_tof_s_sdp[0][1][0][0]=-1.38581; p3_tof_s_sdp[0][1][0][0]=0.426593; p4_tof_s_sdp[0][1][0][0]=-0.0452636; 
  p0_tof_m_sdp[1][1][0][0]=0.380248; p1_tof_m_sdp[1][1][0][0]=-0.418281; p2_tof_m_sdp[1][1][0][0]=-1.14789; p3_tof_m_sdp[1][1][0][0]=-0.738825; p4_tof_m_sdp[1][1][0][0]=3.27175; 
  p0_tof_s_sdp[1][1][0][0]=-35.8323; p1_tof_s_sdp[1][1][0][0]=127.206; p2_tof_s_sdp[1][1][0][0]=-13.7681; p3_tof_s_sdp[1][1][0][0]=-331.871; p4_tof_s_sdp[1][1][0][0]=285.394; 
  p0_tof_m_sdp[0][1][1][0]=0.16568; p1_tof_m_sdp[0][1][1][0]=-0.509981; p2_tof_m_sdp[0][1][1][0]=0.460062; p3_tof_m_sdp[0][1][1][0]=-0.161292; p4_tof_m_sdp[0][1][1][0]=0.0186562; 
  p0_tof_s_sdp[0][1][1][0]=0.374193; p1_tof_s_sdp[0][1][1][0]=1.03457; p2_tof_s_sdp[0][1][1][0]=-0.774524; p3_tof_s_sdp[0][1][1][0]=0.236821; p4_tof_s_sdp[0][1][1][0]=-0.0248097; 
  p0_tof_m_sdp[1][1][1][0]=-0.458388; p1_tof_m_sdp[1][1][1][0]=0.332929; p2_tof_m_sdp[1][1][1][0]=1.10777; p3_tof_m_sdp[1][1][1][0]=0.881886; p4_tof_m_sdp[1][1][1][0]=-2.6454; 
  p0_tof_s_sdp[1][1][1][0]=1.68616; p1_tof_s_sdp[1][1][1][0]=-0.566306; p2_tof_s_sdp[1][1][1][0]=-2.84925; p3_tof_s_sdp[1][1][1][0]=-2.3467; p4_tof_s_sdp[1][1][1][0]=7.63301; 
  p0_tof_m_sdp[0][1][2][0]=0.138814; p1_tof_m_sdp[0][1][2][0]=-0.429033; p2_tof_m_sdp[0][1][2][0]=0.367279; p3_tof_m_sdp[0][1][2][0]=-0.12492; p4_tof_m_sdp[0][1][2][0]=0.0142434; 
  p0_tof_s_sdp[0][1][2][0]=0.3128; p1_tof_s_sdp[0][1][2][0]=1.21038; p2_tof_s_sdp[0][1][2][0]=-0.912722; p3_tof_s_sdp[0][1][2][0]=0.279105; p4_tof_s_sdp[0][1][2][0]=-0.0293871; 
  p0_tof_m_sdp[1][1][2][0]=0.249933; p1_tof_m_sdp[1][1][2][0]=-0.246453; p2_tof_m_sdp[1][1][2][0]=-0.730025; p3_tof_m_sdp[1][1][2][0]=-0.526513; p4_tof_m_sdp[1][1][2][0]=1.96919; 
  p0_tof_s_sdp[1][1][2][0]=0.835984; p1_tof_s_sdp[1][1][2][0]=0.394817; p2_tof_s_sdp[1][1][2][0]=-0.284569; p3_tof_s_sdp[1][1][2][0]=-0.754663; p4_tof_s_sdp[1][1][2][0]=0.315042; 
  p0_tof_m_sdp[0][1][3][0]=0.285033; p1_tof_m_sdp[0][1][3][0]=-0.833436; p2_tof_m_sdp[0][1][3][0]=0.72412; p3_tof_m_sdp[0][1][3][0]=-0.244341; p4_tof_m_sdp[0][1][3][0]=0.0274263; 
  p0_tof_s_sdp[0][1][3][0]=0.379092; p1_tof_s_sdp[0][1][3][0]=1.04726; p2_tof_s_sdp[0][1][3][0]=-0.798125; p3_tof_s_sdp[0][1][3][0]=0.247608; p4_tof_s_sdp[0][1][3][0]=-0.026245; 
  p0_tof_m_sdp[1][1][3][0]=0.466211; p1_tof_m_sdp[1][1][3][0]=-0.468228; p2_tof_m_sdp[1][1][3][0]=-1.3441; p3_tof_m_sdp[1][1][3][0]=-0.912481; p4_tof_m_sdp[1][1][3][0]=3.69975; 
  p0_tof_s_sdp[1][1][3][0]=1.23309; p1_tof_s_sdp[1][1][3][0]=-0.0950159; p2_tof_s_sdp[1][1][3][0]=-1.52667; p3_tof_s_sdp[1][1][3][0]=-1.42619; p4_tof_s_sdp[1][1][3][0]=4.05112; 
  p0_tof_m_sdp[0][1][4][0]=0.222331; p1_tof_m_sdp[0][1][4][0]=-0.590504; p2_tof_m_sdp[0][1][4][0]=0.493914; p3_tof_m_sdp[0][1][4][0]=-0.165299; p4_tof_m_sdp[0][1][4][0]=0.0186953; 
  p0_tof_s_sdp[0][1][4][0]=0.55155; p1_tof_s_sdp[0][1][4][0]=0.682409; p2_tof_s_sdp[0][1][4][0]=-0.504356; p3_tof_s_sdp[0][1][4][0]=0.148394; p4_tof_s_sdp[0][1][4][0]=-0.015121; 
  p0_tof_m_sdp[1][1][4][0]=0.648247; p1_tof_m_sdp[1][1][4][0]=-0.696041; p2_tof_m_sdp[1][1][4][0]=-1.93683; p3_tof_m_sdp[1][1][4][0]=-1.26955; p4_tof_m_sdp[1][1][4][0]=5.48016; 
  p0_tof_s_sdp[1][1][4][0]=-30.6761; p1_tof_s_sdp[1][1][4][0]=112.76; p2_tof_s_sdp[1][1][4][0]=-15.1237; p3_tof_s_sdp[1][1][4][0]=-302.54; p4_tof_s_sdp[1][1][4][0]=267.438; 
  p0_tof_m_sdp[0][1][5][0]=-0.165763; p1_tof_m_sdp[0][1][5][0]=0.397057; p2_tof_m_sdp[0][1][5][0]=-0.338136; p3_tof_m_sdp[0][1][5][0]=0.109151; p4_tof_m_sdp[0][1][5][0]=-0.011325; 
  p0_tof_s_sdp[0][1][5][0]=0.606627; p1_tof_s_sdp[0][1][5][0]=0.577631; p2_tof_s_sdp[0][1][5][0]=-0.401099; p3_tof_s_sdp[0][1][5][0]=0.111053; p4_tof_s_sdp[0][1][5][0]=-0.01049; 
  p0_tof_m_sdp[1][1][5][0]=-0.126089; p1_tof_m_sdp[1][1][5][0]=0.0605996; p2_tof_m_sdp[1][1][5][0]=0.2566; p3_tof_m_sdp[1][1][5][0]=0.253709; p4_tof_m_sdp[1][1][5][0]=-0.433039; 
  p0_tof_s_sdp[1][1][5][0]=29.7189; p1_tof_s_sdp[1][1][5][0]=-101.976; p2_tof_s_sdp[1][1][5][0]=11.8158; p3_tof_s_sdp[1][1][5][0]=274.433; p4_tof_s_sdp[1][1][5][0]=-240.63; 
  p0_tof_m_sdp[0][1][6][0]=-0.161149; p1_tof_m_sdp[0][1][6][0]=0.36479; p2_tof_m_sdp[0][1][6][0]=-0.323556; p3_tof_m_sdp[0][1][6][0]=0.109269; p4_tof_m_sdp[0][1][6][0]=-0.011665; 
  p0_tof_s_sdp[0][1][6][0]=0.75274; p1_tof_s_sdp[0][1][6][0]=0.220887; p2_tof_s_sdp[0][1][6][0]=-0.121141; p3_tof_s_sdp[0][1][6][0]=0.0244385; p4_tof_s_sdp[0][1][6][0]=-0.00128656; 
  p0_tof_m_sdp[1][1][6][0]=-6.81216; p1_tof_m_sdp[1][1][6][0]=24.0972; p2_tof_m_sdp[1][1][6][0]=-2.90716; p3_tof_m_sdp[1][1][6][0]=-65.2297; p4_tof_m_sdp[1][1][6][0]=57.5809; 
  p0_tof_s_sdp[1][1][6][0]=0.448357; p1_tof_s_sdp[1][1][6][0]=0.599147; p2_tof_s_sdp[1][1][6][0]=0.575848; p3_tof_s_sdp[1][1][6][0]=0.0966794; p4_tof_s_sdp[1][1][6][0]=-1.32066; 
  p0_tof_m_sdp[0][1][7][0]=-0.130264; p1_tof_m_sdp[0][1][7][0]=0.270156; p2_tof_m_sdp[0][1][7][0]=-0.235583; p3_tof_m_sdp[0][1][7][0]=0.0791839; p4_tof_m_sdp[0][1][7][0]=-0.00825264; 
  p0_tof_s_sdp[0][1][7][0]=0.638362; p1_tof_s_sdp[0][1][7][0]=0.431532; p2_tof_s_sdp[0][1][7][0]=-0.258116; p3_tof_s_sdp[0][1][7][0]=0.0609222; p4_tof_s_sdp[0][1][7][0]=-0.00469553; 
  p0_tof_m_sdp[1][1][7][0]=0.0649024; p1_tof_m_sdp[1][1][7][0]=-0.112062; p2_tof_m_sdp[1][1][7][0]=-0.270371; p3_tof_m_sdp[1][1][7][0]=-0.146284; p4_tof_m_sdp[1][1][7][0]=0.88382; 
  p0_tof_s_sdp[1][1][7][0]=0.283564; p1_tof_s_sdp[1][1][7][0]=0.876946; p2_tof_s_sdp[1][1][7][0]=1.222; p3_tof_s_sdp[1][1][7][0]=0.375453; p4_tof_s_sdp[1][1][7][0]=-3.64494; 
  p0_tof_m_sdp[0][1][8][0]=-0.335643; p1_tof_m_sdp[0][1][8][0]=0.7241; p2_tof_m_sdp[0][1][8][0]=-0.565843; p3_tof_m_sdp[0][1][8][0]=0.175635; p4_tof_m_sdp[0][1][8][0]=-0.0181636; 
  p0_tof_s_sdp[0][1][8][0]=0.53509; p1_tof_s_sdp[0][1][8][0]=0.643316; p2_tof_s_sdp[0][1][8][0]=-0.385805; p3_tof_s_sdp[0][1][8][0]=0.0892684; p4_tof_s_sdp[0][1][8][0]=-0.00655543; 
  p0_tof_m_sdp[1][1][8][0]=0.705005; p1_tof_m_sdp[1][1][8][0]=-0.640961; p2_tof_m_sdp[1][1][8][0]=-1.98122; p3_tof_m_sdp[1][1][8][0]=-1.48265; p4_tof_m_sdp[1][1][8][0]=5.26961; 
  p0_tof_s_sdp[1][1][8][0]=-12.9808; p1_tof_s_sdp[1][1][8][0]=51.1974; p2_tof_s_sdp[1][1][8][0]=-8.73981; p3_tof_s_sdp[1][1][8][0]=-142.84; p4_tof_s_sdp[1][1][8][0]=131.497; 
  p0_tof_m_sdp[0][1][9][0]=-0.273773; p1_tof_m_sdp[0][1][9][0]=0.572; p2_tof_m_sdp[0][1][9][0]=-0.441405; p3_tof_m_sdp[0][1][9][0]=0.138077; p4_tof_m_sdp[0][1][9][0]=-0.0144477; 
  p0_tof_s_sdp[0][1][9][0]=0.714338; p1_tof_s_sdp[0][1][9][0]=0.178815; p2_tof_s_sdp[0][1][9][0]=-0.0395712; p3_tof_s_sdp[0][1][9][0]=-0.0097242; p4_tof_s_sdp[0][1][9][0]=0.00279406; 
  p0_tof_m_sdp[1][1][9][0]=-0.534843; p1_tof_m_sdp[1][1][9][0]=0.48399; p2_tof_m_sdp[1][1][9][0]=1.48879; p3_tof_m_sdp[1][1][9][0]=1.08491; p4_tof_m_sdp[1][1][9][0]=-4.06228; 
  p0_tof_s_sdp[1][1][9][0]=0.264469; p1_tof_s_sdp[1][1][9][0]=0.652405; p2_tof_s_sdp[1][1][9][0]=0.951789; p3_tof_s_sdp[1][1][9][0]=0.544942; p4_tof_s_sdp[1][1][9][0]=-2.05035; 

  //! ToF sdphi pos
  p0_tof_m_sdp[0][0][0][1]=-0.0624332; p1_tof_m_sdp[0][0][0][1]=0.0896836; p2_tof_m_sdp[0][0][0][1]=-0.032967; p3_tof_m_sdp[0][0][0][1]=0.00157886; p4_tof_m_sdp[0][0][0][1]=0.00026622; 
  p0_tof_s_sdp[0][0][0][1]=0.737548; p1_tof_s_sdp[0][0][0][1]=0.200058; p2_tof_s_sdp[0][0][0][1]=-0.0881403; p3_tof_s_sdp[0][0][0][1]=0.0143484; p4_tof_s_sdp[0][0][0][1]=-0.000490013; 
  p0_tof_m_sdp[1][0][0][1]=0.666444; p1_tof_m_sdp[1][0][0][1]=-0.664859; p2_tof_m_sdp[1][0][0][1]=-1.9251; p3_tof_m_sdp[1][0][0][1]=-1.31952; p4_tof_m_sdp[1][0][0][1]=5.31376; 
  p0_tof_s_sdp[1][0][0][1]=-37.6347; p1_tof_s_sdp[1][0][0][1]=140.368; p2_tof_s_sdp[1][0][0][1]=-22.1139; p3_tof_s_sdp[1][0][0][1]=-382.162; p4_tof_s_sdp[1][0][0][1]=345.008; 
  p0_tof_m_sdp[0][0][1][1]=0.0821419; p1_tof_m_sdp[0][0][1][1]=-0.190774; p2_tof_m_sdp[0][0][1][1]=0.150723; p3_tof_m_sdp[0][0][1][1]=-0.0479933; p4_tof_m_sdp[0][0][1][1]=0.00510969; 
  p0_tof_s_sdp[0][0][1][1]=0.908117; p1_tof_s_sdp[0][0][1][1]=-0.150784; p2_tof_s_sdp[0][0][1][1]=0.163687; p3_tof_s_sdp[0][0][1][1]=-0.0603166; p4_tof_s_sdp[0][0][1][1]=0.00724487; 
  p0_tof_m_sdp[1][0][1][1]=0.273984; p1_tof_m_sdp[1][0][1][1]=-0.300594; p2_tof_m_sdp[1][0][1][1]=-0.833334; p3_tof_m_sdp[1][0][1][1]=-0.530064; p4_tof_m_sdp[1][0][1][1]=2.44707; 
  p0_tof_s_sdp[1][0][1][1]=1.15093; p1_tof_s_sdp[1][0][1][1]=-0.00861466; p2_tof_s_sdp[1][0][1][1]=-1.32533; p3_tof_s_sdp[1][0][1][1]=-1.33044; p4_tof_s_sdp[1][0][1][1]=3.64093; 
  p0_tof_m_sdp[0][0][2][1]=-0.0180034; p1_tof_m_sdp[0][0][2][1]=0.0556227; p2_tof_m_sdp[0][0][2][1]=-0.0471291; p3_tof_m_sdp[0][0][2][1]=0.0139676; p4_tof_m_sdp[0][0][2][1]=-0.00141454; 
  p0_tof_s_sdp[0][0][2][1]=0.938894; p1_tof_s_sdp[0][0][2][1]=-0.215508; p2_tof_s_sdp[0][0][2][1]=0.198574; p3_tof_s_sdp[0][0][2][1]=-0.0660078; p4_tof_s_sdp[0][0][2][1]=0.00735647; 
  p0_tof_m_sdp[1][0][2][1]=12.2808; p1_tof_m_sdp[1][0][2][1]=-43.272; p2_tof_m_sdp[1][0][2][1]=4.67017; p3_tof_m_sdp[1][0][2][1]=116.312; p4_tof_m_sdp[1][0][2][1]=-101.257; 
  p0_tof_s_sdp[1][0][2][1]=-25.2784; p1_tof_s_sdp[1][0][2][1]=90.651; p2_tof_s_sdp[1][0][2][1]=-9.20172; p3_tof_s_sdp[1][0][2][1]=-237.281; p4_tof_s_sdp[1][0][2][1]=203.041; 
  p0_tof_m_sdp[0][0][3][1]=-0.0664698; p1_tof_m_sdp[0][0][3][1]=0.141335; p2_tof_m_sdp[0][0][3][1]=-0.122106; p3_tof_m_sdp[0][0][3][1]=0.0397607; p4_tof_m_sdp[0][0][3][1]=-0.00410712; 
  p0_tof_s_sdp[0][0][3][1]=0.760303; p1_tof_s_sdp[0][0][3][1]=0.237973; p2_tof_s_sdp[0][0][3][1]=-0.223424; p3_tof_s_sdp[0][0][3][1]=0.0911369; p4_tof_s_sdp[0][0][3][1]=-0.0121507; 
  p0_tof_m_sdp[1][0][3][1]=0.384605; p1_tof_m_sdp[1][0][3][1]=-0.407088; p2_tof_m_sdp[1][0][3][1]=-1.15341; p3_tof_m_sdp[1][0][3][1]=-0.783916; p4_tof_m_sdp[1][0][3][1]=3.18638; 
  p0_tof_s_sdp[1][0][3][1]=23.7176; p1_tof_s_sdp[1][0][3][1]=-83.0454; p2_tof_s_sdp[1][0][3][1]=10.9552; p3_tof_s_sdp[1][0][3][1]=230.134; p4_tof_s_sdp[1][0][3][1]=-205.851; 
  p0_tof_m_sdp[0][0][4][1]=-0.077187; p1_tof_m_sdp[0][0][4][1]=0.143104; p2_tof_m_sdp[0][0][4][1]=-0.112948; p3_tof_m_sdp[0][0][4][1]=0.0368608; p4_tof_m_sdp[0][0][4][1]=-0.00411472; 
  p0_tof_s_sdp[0][0][4][1]=0.795058; p1_tof_s_sdp[0][0][4][1]=0.117751; p2_tof_s_sdp[0][0][4][1]=-0.0781094; p3_tof_s_sdp[0][0][4][1]=0.0264336; p4_tof_s_sdp[0][0][4][1]=-0.00305585; 
  p0_tof_m_sdp[1][0][4][1]=0.111626; p1_tof_m_sdp[1][0][4][1]=-0.117824; p2_tof_m_sdp[1][0][4][1]=-0.336331; p3_tof_m_sdp[1][0][4][1]=-0.248643; p4_tof_m_sdp[1][0][4][1]=0.820975; 
  p0_tof_s_sdp[1][0][4][1]=52.1563; p1_tof_s_sdp[1][0][4][1]=-181.281; p2_tof_s_sdp[1][0][4][1]=20.5148; p3_tof_s_sdp[1][0][4][1]=489.018; p4_tof_s_sdp[1][0][4][1]=-428.507; 
  p0_tof_m_sdp[0][0][5][1]=-0.0503075; p1_tof_m_sdp[0][0][5][1]=0.0691233; p2_tof_m_sdp[0][0][5][1]=-0.0329819; p3_tof_m_sdp[0][0][5][1]=0.00631735; p4_tof_m_sdp[0][0][5][1]=-0.000504629; 
  p0_tof_s_sdp[0][0][5][1]=0.939791; p1_tof_s_sdp[0][0][5][1]=-0.172742; p2_tof_s_sdp[0][0][5][1]=0.161381; p3_tof_s_sdp[0][0][5][1]=-0.057118; p4_tof_s_sdp[0][0][5][1]=0.00685575; 
  p0_tof_m_sdp[1][0][5][1]=-0.0793841; p1_tof_m_sdp[1][0][5][1]=0.0319173; p2_tof_m_sdp[1][0][5][1]=0.130761; p3_tof_m_sdp[1][0][5][1]=0.0933363; p4_tof_m_sdp[1][0][5][1]=-0.330144; 
  p0_tof_s_sdp[1][0][5][1]=0.31002; p1_tof_s_sdp[1][0][5][1]=0.843482; p2_tof_s_sdp[1][0][5][1]=1.14117; p3_tof_s_sdp[1][0][5][1]=0.343874; p4_tof_s_sdp[1][0][5][1]=-3.39061; 
  p0_tof_m_sdp[0][0][6][1]=0.0161743; p1_tof_m_sdp[0][0][6][1]=-0.0614074; p2_tof_m_sdp[0][0][6][1]=0.0569388; p3_tof_m_sdp[0][0][6][1]=-0.0172226; p4_tof_m_sdp[0][0][6][1]=0.00148034; 
  p0_tof_s_sdp[0][0][6][1]=0.692367; p1_tof_s_sdp[0][0][6][1]=0.376395; p2_tof_s_sdp[0][0][6][1]=-0.247431; p3_tof_s_sdp[0][0][6][1]=0.0680006; p4_tof_s_sdp[0][0][6][1]=-0.00645799; 
  p0_tof_m_sdp[1][0][6][1]=0.107504; p1_tof_m_sdp[1][0][6][1]=-0.227881; p2_tof_m_sdp[1][0][6][1]=-0.492752; p3_tof_m_sdp[1][0][6][1]=-0.203099; p4_tof_m_sdp[1][0][6][1]=1.7046; 
  p0_tof_s_sdp[1][0][6][1]=1.13367; p1_tof_s_sdp[1][0][6][1]=0.0796477; p2_tof_s_sdp[1][0][6][1]=-1.14967; p3_tof_s_sdp[1][0][6][1]=-1.27467; p4_tof_s_sdp[1][0][6][1]=2.94309; 
  p0_tof_m_sdp[0][0][7][1]=-0.0128615; p1_tof_m_sdp[0][0][7][1]=0.0236596; p2_tof_m_sdp[0][0][7][1]=-0.00502011; p3_tof_m_sdp[0][0][7][1]=-0.00182295; p4_tof_m_sdp[0][0][7][1]=0.000344019; 
  p0_tof_s_sdp[0][0][7][1]=0.758911; p1_tof_s_sdp[0][0][7][1]=0.311759; p2_tof_s_sdp[0][0][7][1]=-0.263636; p3_tof_s_sdp[0][0][7][1]=0.105193; p4_tof_s_sdp[0][0][7][1]=-0.0157844; 
  p0_tof_m_sdp[1][0][7][1]=0.380286; p1_tof_m_sdp[1][0][7][1]=-0.35103; p2_tof_m_sdp[1][0][7][1]=-1.07053; p3_tof_m_sdp[1][0][7][1]=-0.779814; p4_tof_m_sdp[1][0][7][1]=2.89775; 
  p0_tof_s_sdp[1][0][7][1]=39.825; p1_tof_s_sdp[1][0][7][1]=-134.31; p2_tof_s_sdp[1][0][7][1]=8.41163; p3_tof_s_sdp[1][0][7][1]=365.033; p4_tof_s_sdp[1][0][7][1]=-310.651; 
  p0_tof_m_sdp[0][0][8][1]=0.122575; p1_tof_m_sdp[0][0][8][1]=-0.28863; p2_tof_m_sdp[0][0][8][1]=0.252673; p3_tof_m_sdp[0][0][8][1]=-0.0860752; p4_tof_m_sdp[0][0][8][1]=0.00943363; 
  p0_tof_s_sdp[0][0][8][1]=0.632697; p1_tof_s_sdp[0][0][8][1]=0.484994; p2_tof_s_sdp[0][0][8][1]=-0.311071; p3_tof_s_sdp[0][0][8][1]=0.0846664; p4_tof_s_sdp[0][0][8][1]=-0.00813055; 
  p0_tof_m_sdp[1][0][8][1]=0.111182; p1_tof_m_sdp[1][0][8][1]=0.0104911; p2_tof_m_sdp[1][0][8][1]=-0.13591; p3_tof_m_sdp[1][0][8][1]=-0.240892; p4_tof_m_sdp[1][0][8][1]=-0.052659; 
  p0_tof_s_sdp[1][0][8][1]=49.0333; p1_tof_s_sdp[1][0][8][1]=-172.39; p2_tof_s_sdp[1][0][8][1]=21.536; p3_tof_s_sdp[1][0][8][1]=470.239; p4_tof_s_sdp[1][0][8][1]=-416.756; 
  p0_tof_m_sdp[0][0][9][1]=0.447879; p1_tof_m_sdp[0][0][9][1]=-0.921363; p2_tof_m_sdp[0][0][9][1]=0.661043; p3_tof_m_sdp[0][0][9][1]=-0.196919; p4_tof_m_sdp[0][0][9][1]=0.020227; 
  p0_tof_s_sdp[0][0][9][1]=0.800279; p1_tof_s_sdp[0][0][9][1]=-0.00429264; p2_tof_s_sdp[0][0][9][1]=0.104308; p3_tof_s_sdp[0][0][9][1]=-0.0507374; p4_tof_s_sdp[0][0][9][1]=0.00662149; 
  p0_tof_m_sdp[1][0][9][1]=1.61172; p1_tof_m_sdp[1][0][9][1]=-1.3413; p2_tof_m_sdp[1][0][9][1]=-4.30002; p3_tof_m_sdp[1][0][9][1]=-3.3478; p4_tof_m_sdp[1][0][9][1]=11.1361; 
  p0_tof_s_sdp[1][0][9][1]=-101.236; p1_tof_s_sdp[1][0][9][1]=353.026; p2_tof_s_sdp[1][0][9][1]=-17.7646; p3_tof_s_sdp[1][0][9][1]=-982.706; p4_tof_s_sdp[1][0][9][1]=840.511; 
  p0_tof_m_sdp[0][1][0][1]=-0.156154; p1_tof_m_sdp[0][1][0][1]=0.400167; p2_tof_m_sdp[0][1][0][1]=-0.358913; p3_tof_m_sdp[0][1][0][1]=0.123181; p4_tof_m_sdp[0][1][0][1]=-0.013604; 
  p0_tof_s_sdp[0][1][0][1]=0.580788; p1_tof_s_sdp[0][1][0][1]=0.628675; p2_tof_s_sdp[0][1][0][1]=-0.406575; p3_tof_s_sdp[0][1][0][1]=0.102302; p4_tof_s_sdp[0][1][0][1]=-0.00900818; 
  p0_tof_m_sdp[1][1][0][1]=-0.0200604; p1_tof_m_sdp[1][1][0][1]=-0.0168021; p2_tof_m_sdp[1][1][0][1]=-0.000329453; p3_tof_m_sdp[1][1][0][1]=0.0425778; p4_tof_m_sdp[1][1][0][1]=0.134506; 
  p0_tof_s_sdp[1][1][0][1]=1.38995; p1_tof_s_sdp[1][1][0][1]=-0.24342; p2_tof_s_sdp[1][1][0][1]=-1.97434; p3_tof_s_sdp[1][1][0][1]=-1.76707; p4_tof_s_sdp[1][1][0][1]=5.23891; 
  p0_tof_m_sdp[0][1][1][1]=-0.191564; p1_tof_m_sdp[0][1][1][1]=0.33878; p2_tof_m_sdp[0][1][1][1]=-0.2524; p3_tof_m_sdp[0][1][1][1]=0.0796005; p4_tof_m_sdp[0][1][1][1]=-0.00833947; 
  p0_tof_s_sdp[0][1][1][1]=0.419196; p1_tof_s_sdp[0][1][1][1]=0.971751; p2_tof_s_sdp[0][1][1][1]=-0.69996; p3_tof_s_sdp[0][1][1][1]=0.202741; p4_tof_s_sdp[0][1][1][1]=-0.0203074; 
  p0_tof_m_sdp[1][1][1][1]=0.569425; p1_tof_m_sdp[1][1][1][1]=-0.550865; p2_tof_m_sdp[1][1][1][1]=-1.60983; p3_tof_m_sdp[1][1][1][1]=-1.14318; p4_tof_m_sdp[1][1][1][1]=4.2096; 
  p0_tof_s_sdp[1][1][1][1]=1.27699; p1_tof_s_sdp[1][1][1][1]=-0.205231; p2_tof_s_sdp[1][1][1][1]=-1.76889; p3_tof_s_sdp[1][1][1][1]=-1.55083; p4_tof_s_sdp[1][1][1][1]=4.89989; 
  p0_tof_m_sdp[0][1][2][1]=-0.0914178; p1_tof_m_sdp[0][1][2][1]=0.242758; p2_tof_m_sdp[0][1][2][1]=-0.25434; p3_tof_m_sdp[0][1][2][1]=0.0944615; p4_tof_m_sdp[0][1][2][1]=-0.0108258; 
  p0_tof_s_sdp[0][1][2][1]=0.585212; p1_tof_s_sdp[0][1][2][1]=0.711862; p2_tof_s_sdp[0][1][2][1]=-0.523704; p3_tof_s_sdp[0][1][2][1]=0.149215; p4_tof_s_sdp[0][1][2][1]=-0.0145284; 
  p0_tof_m_sdp[1][1][2][1]=-0.159262; p1_tof_m_sdp[1][1][2][1]=0.107694; p2_tof_m_sdp[1][1][2][1]=0.384068; p3_tof_m_sdp[1][1][2][1]=0.310242; p4_tof_m_sdp[1][1][2][1]=-0.996696; 
  p0_tof_s_sdp[1][1][2][1]=0.489757; p1_tof_s_sdp[1][1][2][1]=0.686661; p2_tof_s_sdp[1][1][2][1]=0.682118; p3_tof_s_sdp[1][1][2][1]=0.0358415; p4_tof_s_sdp[1][1][2][1]=-2.1607; 
  p0_tof_m_sdp[0][1][3][1]=0.189094; p1_tof_m_sdp[0][1][3][1]=-0.446538; p2_tof_m_sdp[0][1][3][1]=0.331163; p3_tof_m_sdp[0][1][3][1]=-0.0996274; p4_tof_m_sdp[0][1][3][1]=0.0107521; 
  p0_tof_s_sdp[0][1][3][1]=0.362288; p1_tof_s_sdp[0][1][3][1]=1.16915; p2_tof_s_sdp[0][1][3][1]=-0.841238; p3_tof_s_sdp[0][1][3][1]=0.237214; p4_tof_s_sdp[0][1][3][1]=-0.0227533; 
  p0_tof_m_sdp[1][1][3][1]=-0.0893896; p1_tof_m_sdp[1][1][3][1]=0.120441; p2_tof_m_sdp[1][1][3][1]=0.298037; p3_tof_m_sdp[1][1][3][1]=0.165686; p4_tof_m_sdp[1][1][3][1]=-0.883171; 
  p0_tof_s_sdp[1][1][3][1]=19.1018; p1_tof_s_sdp[1][1][3][1]=-65.4914; p2_tof_s_sdp[1][1][3][1]=7.38062; p3_tof_s_sdp[1][1][3][1]=181.998; p4_tof_s_sdp[1][1][3][1]=-161.363; 
  p0_tof_m_sdp[0][1][4][1]=-0.0818923; p1_tof_m_sdp[0][1][4][1]=0.167282; p2_tof_m_sdp[0][1][4][1]=-0.12094; p3_tof_m_sdp[0][1][4][1]=0.0344449; p4_tof_m_sdp[0][1][4][1]=-0.00305799; 
  p0_tof_s_sdp[0][1][4][1]=0.355879; p1_tof_s_sdp[0][1][4][1]=1.09985; p2_tof_s_sdp[0][1][4][1]=-0.748969; p3_tof_s_sdp[0][1][4][1]=0.20162; p4_tof_s_sdp[0][1][4][1]=-0.0187017; 
  p0_tof_m_sdp[1][1][4][1]=0.483487; p1_tof_m_sdp[1][1][4][1]=-0.397554; p2_tof_m_sdp[1][1][4][1]=-1.27116; p3_tof_m_sdp[1][1][4][1]=-0.974922; p4_tof_m_sdp[1][1][4][1]=3.25289; 
  p0_tof_s_sdp[1][1][4][1]=0.182029; p1_tof_s_sdp[1][1][4][1]=0.812675; p2_tof_s_sdp[1][1][4][1]=1.2497; p3_tof_s_sdp[1][1][4][1]=0.591033; p4_tof_s_sdp[1][1][4][1]=-3.06914; 
  p0_tof_m_sdp[0][1][5][1]=0.172707; p1_tof_m_sdp[0][1][5][1]=-0.421282; p2_tof_m_sdp[0][1][5][1]=0.313986; p3_tof_m_sdp[0][1][5][1]=-0.0895142; p4_tof_m_sdp[0][1][5][1]=0.00841256; 
  p0_tof_s_sdp[0][1][5][1]=0.517212; p1_tof_s_sdp[0][1][5][1]=0.867983; p2_tof_s_sdp[0][1][5][1]=-0.640312; p3_tof_s_sdp[0][1][5][1]=0.183456; p4_tof_s_sdp[0][1][5][1]=-0.017912; 
  p0_tof_m_sdp[1][1][5][1]=-0.084711; p1_tof_m_sdp[1][1][5][1]=0.148165; p2_tof_m_sdp[1][1][5][1]=0.316953; p3_tof_m_sdp[1][1][5][1]=0.0988237; p4_tof_m_sdp[1][1][5][1]=-1.17447; 
  p0_tof_s_sdp[1][1][5][1]=0.107034; p1_tof_s_sdp[1][1][5][1]=0.897434; p2_tof_s_sdp[1][1][5][1]=1.50002; p3_tof_s_sdp[1][1][5][1]=0.779012; p4_tof_s_sdp[1][1][5][1]=-3.75319; 
  p0_tof_m_sdp[0][1][6][1]=0.0620928; p1_tof_m_sdp[0][1][6][1]=-0.172599; p2_tof_m_sdp[0][1][6][1]=0.131599; p3_tof_m_sdp[0][1][6][1]=-0.0338453; p4_tof_m_sdp[0][1][6][1]=0.0022975; 
  p0_tof_s_sdp[0][1][6][1]=0.706218; p1_tof_s_sdp[0][1][6][1]=0.420893; p2_tof_s_sdp[0][1][6][1]=-0.316277; p3_tof_s_sdp[0][1][6][1]=0.0910517; p4_tof_s_sdp[0][1][6][1]=-0.00865167; 
  p0_tof_m_sdp[1][1][6][1]=-0.0831807; p1_tof_m_sdp[1][1][6][1]=0.106881; p2_tof_m_sdp[1][1][6][1]=0.275991; p3_tof_m_sdp[1][1][6][1]=0.154117; p4_tof_m_sdp[1][1][6][1]=-0.88178; 
  p0_tof_s_sdp[1][1][6][1]=1.04807; p1_tof_s_sdp[1][1][6][1]=0.120182; p2_tof_s_sdp[1][1][6][1]=-0.965455; p3_tof_s_sdp[1][1][6][1]=-1.10037; p4_tof_s_sdp[1][1][6][1]=2.498; 
  p0_tof_m_sdp[0][1][7][1]=0.0794957; p1_tof_m_sdp[0][1][7][1]=-0.206552; p2_tof_m_sdp[0][1][7][1]=0.136291; p3_tof_m_sdp[0][1][7][1]=-0.0304825; p4_tof_m_sdp[0][1][7][1]=0.00172534; 
  p0_tof_s_sdp[0][1][7][1]=0.544126; p1_tof_s_sdp[0][1][7][1]=0.816178; p2_tof_s_sdp[0][1][7][1]=-0.638644; p3_tof_s_sdp[0][1][7][1]=0.194795; p4_tof_s_sdp[0][1][7][1]=-0.020047; 
  p0_tof_m_sdp[1][1][7][1]=0.0170494; p1_tof_m_sdp[1][1][7][1]=-0.0164824; p2_tof_m_sdp[1][1][7][1]=-0.0508774; p3_tof_m_sdp[1][1][7][1]=-0.0540312; p4_tof_m_sdp[1][1][7][1]=0.0437954; 
  p0_tof_s_sdp[1][1][7][1]=-0.044039; p1_tof_s_sdp[1][1][7][1]=1.14456; p2_tof_s_sdp[1][1][7][1]=2.08185; p3_tof_s_sdp[1][1][7][1]=1.04403; p4_tof_s_sdp[1][1][7][1]=-5.82979; 
  p0_tof_m_sdp[0][1][8][1]=0.276062; p1_tof_m_sdp[0][1][8][1]=-0.672073; p2_tof_m_sdp[0][1][8][1]=0.48074; p3_tof_m_sdp[0][1][8][1]=-0.131123; p4_tof_m_sdp[0][1][8][1]=0.0117828; 
  p0_tof_s_sdp[0][1][8][1]=0.995455; p1_tof_s_sdp[0][1][8][1]=-0.187075; p2_tof_s_sdp[0][1][8][1]=0.121573; p3_tof_s_sdp[0][1][8][1]=-0.0377531; p4_tof_s_sdp[0][1][8][1]=0.0043491; 
  p0_tof_m_sdp[1][1][8][1]=-0.452403; p1_tof_m_sdp[1][1][8][1]=0.386433; p2_tof_m_sdp[1][1][8][1]=1.21616; p3_tof_m_sdp[1][1][8][1]=0.917111; p4_tof_m_sdp[1][1][8][1]=-3.16935; 
  p0_tof_s_sdp[1][1][8][1]=14.0098; p1_tof_s_sdp[1][1][8][1]=-47.761; p2_tof_s_sdp[1][1][8][1]=6.13389; p3_tof_s_sdp[1][1][8][1]=132.743; p4_tof_s_sdp[1][1][8][1]=-118.348; 
  p0_tof_m_sdp[0][1][9][1]=0.221551; p1_tof_m_sdp[0][1][9][1]=-0.554308; p2_tof_m_sdp[0][1][9][1]=0.430659; p3_tof_m_sdp[0][1][9][1]=-0.133042; p4_tof_m_sdp[0][1][9][1]=0.0138102; 
  p0_tof_s_sdp[0][1][9][1]=0.757783; p1_tof_s_sdp[0][1][9][1]=0.314387; p2_tof_s_sdp[0][1][9][1]=-0.254196; p3_tof_s_sdp[0][1][9][1]=0.0793754; p4_tof_s_sdp[0][1][9][1]=-0.00836619; 
  p0_tof_m_sdp[1][1][9][1]=0.340676; p1_tof_m_sdp[1][1][9][1]=-0.327698; p2_tof_m_sdp[1][1][9][1]=-0.984599; p3_tof_m_sdp[1][1][9][1]=-0.706412; p4_tof_m_sdp[1][1][9][1]=2.7193; 
  p0_tof_s_sdp[1][1][9][1]=-20.6835; p1_tof_s_sdp[1][1][9][1]=76.7682; p2_tof_s_sdp[1][1][9][1]=-8.47648; p3_tof_s_sdp[1][1][9][1]=-212.529; p4_tof_s_sdp[1][1][9][1]=188.336; 

  //! ToF sdz neg
  p0_tof_m_sdz[0][0][0][0]=0.14082; p1_tof_m_sdz[0][0][0][0]=-0.146786; p2_tof_m_sdz[0][0][0][0]=0.0483951; p3_tof_m_sdz[0][0][0][0]=-0.00242987; p4_tof_m_sdz[0][0][0][0]=-0.000469119; 
  p0_tof_s_sdz[0][0][0][0]=0.707086; p1_tof_s_sdz[0][0][0][0]=0.0182095; p2_tof_s_sdz[0][0][0][0]=0.133906; p3_tof_s_sdz[0][0][0][0]=-0.0618763; p4_tof_s_sdz[0][0][0][0]=0.00762218; 
  p0_tof_m_sdz[1][0][0][0]=-0.197168; p1_tof_m_sdz[1][0][0][0]=0.326007; p2_tof_m_sdz[1][0][0][0]=0.778564; p3_tof_m_sdz[1][0][0][0]=0.440535; p4_tof_m_sdz[1][0][0][0]=-2.31004; 
  p0_tof_s_sdz[1][0][0][0]=1.34942; p1_tof_s_sdz[1][0][0][0]=-0.381065; p2_tof_s_sdz[1][0][0][0]=-2.14875; p3_tof_s_sdz[1][0][0][0]=-1.76089; p4_tof_s_sdz[1][0][0][0]=5.9522; 
  p0_tof_m_sdz[0][0][1][0]=0.132643; p1_tof_m_sdz[0][0][1][0]=-0.139299; p2_tof_m_sdz[0][0][1][0]=0.0571788; p3_tof_m_sdz[0][0][1][0]=-0.0100943; p4_tof_m_sdz[0][0][1][0]=0.000651093; 
  p0_tof_s_sdz[0][0][1][0]=0.555306; p1_tof_s_sdz[0][0][1][0]=0.307608; p2_tof_s_sdz[0][0][1][0]=-0.0813143; p3_tof_s_sdz[0][0][1][0]=0.0049773; p4_tof_s_sdz[0][0][1][0]=0.000487848; 
  p0_tof_m_sdz[1][0][1][0]=0.0855374; p1_tof_m_sdz[1][0][1][0]=0.0165038; p2_tof_m_sdz[1][0][1][0]=-0.0697789; p3_tof_m_sdz[1][0][1][0]=-0.100682; p4_tof_m_sdz[1][0][1][0]=0.10914; 
  p0_tof_s_sdz[1][0][1][0]=0.913379; p1_tof_s_sdz[1][0][1][0]=0.0443149; p2_tof_s_sdz[1][0][1][0]=-0.92772; p3_tof_s_sdz[1][0][1][0]=-0.958944; p4_tof_s_sdz[1][0][1][0]=2.4987; 
  p0_tof_m_sdz[0][0][2][0]=-0.0649921; p1_tof_m_sdz[0][0][2][0]=0.180577; p2_tof_m_sdz[0][0][2][0]=-0.153073; p3_tof_m_sdz[0][0][2][0]=0.0489352; p4_tof_m_sdz[0][0][2][0]=-0.00522326; 
  p0_tof_s_sdz[0][0][2][0]=0.668197; p1_tof_s_sdz[0][0][2][0]=0.255752; p2_tof_s_sdz[0][0][2][0]=-0.111917; p3_tof_s_sdz[0][0][2][0]=0.0246163; p4_tof_s_sdz[0][0][2][0]=-0.00210754; 
  p0_tof_m_sdz[1][0][2][0]=0.339779; p1_tof_m_sdz[1][0][2][0]=-0.293314; p2_tof_m_sdz[1][0][2][0]=-0.90248; p3_tof_m_sdz[1][0][2][0]=-0.662878; p4_tof_m_sdz[1][0][2][0]=2.34487; 
  p0_tof_s_sdz[1][0][2][0]=0.935804; p1_tof_s_sdz[1][0][2][0]=0.182586; p2_tof_s_sdz[1][0][2][0]=-0.747705; p3_tof_s_sdz[1][0][2][0]=-0.975839; p4_tof_s_sdz[1][0][2][0]=1.77523; 
  p0_tof_m_sdz[0][0][3][0]=-0.103604; p1_tof_m_sdz[0][0][3][0]=0.195391; p2_tof_m_sdz[0][0][3][0]=-0.14373; p3_tof_m_sdz[0][0][3][0]=0.0430208; p4_tof_m_sdz[0][0][3][0]=-0.00443606; 
  p0_tof_s_sdz[0][0][3][0]=0.779675; p1_tof_s_sdz[0][0][3][0]=0.127444; p2_tof_s_sdz[0][0][3][0]=-0.0512418; p3_tof_s_sdz[0][0][3][0]=0.0119784; p4_tof_s_sdz[0][0][3][0]=-0.001175; 
  p0_tof_m_sdz[1][0][3][0]=0.109417; p1_tof_m_sdz[1][0][3][0]=-0.0746672; p2_tof_m_sdz[1][0][3][0]=-0.276961; p3_tof_m_sdz[1][0][3][0]=-0.257394; p4_tof_m_sdz[1][0][3][0]=0.58526; 
  p0_tof_s_sdz[1][0][3][0]=0.786452; p1_tof_s_sdz[1][0][3][0]=0.259287; p2_tof_s_sdz[1][0][3][0]=-0.4018; p3_tof_s_sdz[1][0][3][0]=-0.591909; p4_tof_s_sdz[1][0][3][0]=1.19575; 
  p0_tof_m_sdz[0][0][4][0]=-0.0535924; p1_tof_m_sdz[0][0][4][0]=0.0567944; p2_tof_m_sdz[0][0][4][0]=-0.0341887; p3_tof_m_sdz[0][0][4][0]=0.00911653; p4_tof_m_sdz[0][0][4][0]=-0.000868708; 
  p0_tof_s_sdz[0][0][4][0]=0.718975; p1_tof_s_sdz[0][0][4][0]=0.286338; p2_tof_s_sdz[0][0][4][0]=-0.188301; p3_tof_s_sdz[0][0][4][0]=0.0571105; p4_tof_s_sdz[0][0][4][0]=-0.0061756; 
  p0_tof_m_sdz[1][0][4][0]=0.0286891; p1_tof_m_sdz[1][0][4][0]=-0.0892157; p2_tof_m_sdz[1][0][4][0]=-0.164039; p3_tof_m_sdz[1][0][4][0]=-0.046401; p4_tof_m_sdz[1][0][4][0]=0.541075; 
  p0_tof_s_sdz[1][0][4][0]=1.10426; p1_tof_s_sdz[1][0][4][0]=-0.0813054; p2_tof_s_sdz[1][0][4][0]=-1.28452; p3_tof_s_sdz[1][0][4][0]=-1.11234; p4_tof_s_sdz[1][0][4][0]=3.61313; 
  p0_tof_m_sdz[0][0][5][0]=-0.0403753; p1_tof_m_sdz[0][0][5][0]=0.0520479; p2_tof_m_sdz[0][0][5][0]=-0.0382168; p3_tof_m_sdz[0][0][5][0]=0.0121925; p4_tof_m_sdz[0][0][5][0]=-0.00131995; 
  p0_tof_s_sdz[0][0][5][0]=0.740797; p1_tof_s_sdz[0][0][5][0]=0.284229; p2_tof_s_sdz[0][0][5][0]=-0.183105; p3_tof_s_sdz[0][0][5][0]=0.0513881; p4_tof_s_sdz[0][0][5][0]=-0.00512771; 
  p0_tof_m_sdz[1][0][5][0]=0.321627; p1_tof_m_sdz[1][0][5][0]=-0.310579; p2_tof_m_sdz[1][0][5][0]=-0.910448; p3_tof_m_sdz[1][0][5][0]=-0.650448; p4_tof_m_sdz[1][0][5][0]=2.38371; 
  p0_tof_s_sdz[1][0][5][0]=0.853161; p1_tof_s_sdz[1][0][5][0]=0.174675; p2_tof_s_sdz[1][0][5][0]=-0.610669; p3_tof_s_sdz[1][0][5][0]=-0.68947; p4_tof_s_sdz[1][0][5][0]=1.94299; 
  p0_tof_m_sdz[0][0][6][0]=-0.0572939; p1_tof_m_sdz[0][0][6][0]=0.0676287; p2_tof_m_sdz[0][0][6][0]=-0.0400223; p3_tof_m_sdz[0][0][6][0]=0.0110623; p4_tof_m_sdz[0][0][6][0]=-0.00113901; 
  p0_tof_s_sdz[0][0][6][0]=0.744183; p1_tof_s_sdz[0][0][6][0]=0.283776; p2_tof_s_sdz[0][0][6][0]=-0.192946; p3_tof_s_sdz[0][0][6][0]=0.0576258; p4_tof_s_sdz[0][0][6][0]=-0.00602615; 
  p0_tof_m_sdz[1][0][6][0]=0.40995; p1_tof_m_sdz[1][0][6][0]=-0.469236; p2_tof_m_sdz[1][0][6][0]=-1.23802; p3_tof_m_sdz[1][0][6][0]=-0.78253; p4_tof_m_sdz[1][0][6][0]=3.36229; 
  p0_tof_s_sdz[1][0][6][0]=0.941664; p1_tof_s_sdz[1][0][6][0]=0.0844213; p2_tof_s_sdz[1][0][6][0]=-0.832251; p3_tof_s_sdz[1][0][6][0]=-0.813793; p4_tof_s_sdz[1][0][6][0]=2.47587; 
  p0_tof_m_sdz[0][0][7][0]=0.0571014; p1_tof_m_sdz[0][0][7][0]=-0.15296; p2_tof_m_sdz[0][0][7][0]=0.106204; p3_tof_m_sdz[0][0][7][0]=-0.0293163; p4_tof_m_sdz[0][0][7][0]=0.00281565; 
  p0_tof_s_sdz[0][0][7][0]=0.724767; p1_tof_s_sdz[0][0][7][0]=0.282034; p2_tof_s_sdz[0][0][7][0]=-0.186268; p3_tof_s_sdz[0][0][7][0]=0.0545164; p4_tof_s_sdz[0][0][7][0]=-0.00550571; 
  p0_tof_m_sdz[1][0][7][0]=1.04091; p1_tof_m_sdz[1][0][7][0]=-1.06423; p2_tof_m_sdz[1][0][7][0]=-3.04356; p3_tof_m_sdz[1][0][7][0]=-2.07391; p4_tof_m_sdz[1][0][7][0]=8.37534; 
  p0_tof_s_sdz[1][0][7][0]=0.509366; p1_tof_s_sdz[1][0][7][0]=0.527027; p2_tof_s_sdz[1][0][7][0]=0.382836; p3_tof_s_sdz[1][0][7][0]=-0.0531824; p4_tof_s_sdz[1][0][7][0]=-0.941877; 
  p0_tof_m_sdz[0][0][8][0]=-0.0364407; p1_tof_m_sdz[0][0][8][0]=-0.0417903; p2_tof_m_sdz[0][0][8][0]=0.0504359; p3_tof_m_sdz[0][0][8][0]=-0.0168907; p4_tof_m_sdz[0][0][8][0]=0.00179193; 
  p0_tof_s_sdz[0][0][8][0]=0.667818; p1_tof_s_sdz[0][0][8][0]=0.320151; p2_tof_s_sdz[0][0][8][0]=-0.173857; p3_tof_s_sdz[0][0][8][0]=0.0447384; p4_tof_s_sdz[0][0][8][0]=-0.00429814; 
  p0_tof_m_sdz[1][0][8][0]=-0.231159; p1_tof_m_sdz[1][0][8][0]=0.0642239; p2_tof_m_sdz[1][0][8][0]=0.422245; p3_tof_m_sdz[1][0][8][0]=0.488224; p4_tof_m_sdz[1][0][8][0]=-0.665296; 
  p0_tof_s_sdz[1][0][8][0]=-0.0774963; p1_tof_s_sdz[1][0][8][0]=0.939711; p2_tof_s_sdz[1][0][8][0]=1.8703; p3_tof_s_sdz[1][0][8][0]=1.23745; p4_tof_s_sdz[1][0][8][0]=-4.4109; 
  p0_tof_m_sdz[0][0][9][0]=-0.0866799; p1_tof_m_sdz[0][0][9][0]=0.157656; p2_tof_m_sdz[0][0][9][0]=-0.144021; p3_tof_m_sdz[0][0][9][0]=0.0496523; p4_tof_m_sdz[0][0][9][0]=-0.00559625; 
  p0_tof_s_sdz[0][0][9][0]=0.684501; p1_tof_s_sdz[0][0][9][0]=0.33097; p2_tof_s_sdz[0][0][9][0]=-0.186287; p3_tof_s_sdz[0][0][9][0]=0.0476713; p4_tof_s_sdz[0][0][9][0]=-0.00455094; 
  p0_tof_m_sdz[1][0][9][0]=0.501204; p1_tof_m_sdz[1][0][9][0]=-0.759659; p2_tof_m_sdz[1][0][9][0]=-1.90047; p3_tof_m_sdz[1][0][9][0]=-1.06936; p4_tof_m_sdz[1][0][9][0]=6.01199; 
  p0_tof_s_sdz[1][0][9][0]=0.253578; p1_tof_s_sdz[1][0][9][0]=0.922146; p2_tof_s_sdz[1][0][9][0]=1.45862; p3_tof_s_sdz[1][0][9][0]=0.626098; p4_tof_s_sdz[1][0][9][0]=-4.64251; 
  p0_tof_m_sdz[0][1][0][0]=0.0369215; p1_tof_m_sdz[0][1][0][0]=-0.149202; p2_tof_m_sdz[0][1][0][0]=0.120568; p3_tof_m_sdz[0][1][0][0]=-0.0389464; p4_tof_m_sdz[0][1][0][0]=0.00420634; 
  p0_tof_s_sdz[0][1][0][0]=0.819137; p1_tof_s_sdz[0][1][0][0]=0.278627; p2_tof_s_sdz[0][1][0][0]=-0.235034; p3_tof_s_sdz[0][1][0][0]=0.0751858; p4_tof_s_sdz[0][1][0][0]=-0.0079126; 
  p0_tof_m_sdz[1][1][0][0]=-0.355733; p1_tof_m_sdz[1][1][0][0]=0.22125; p2_tof_m_sdz[1][1][0][0]=0.81388; p3_tof_m_sdz[1][1][0][0]=0.70426; p4_tof_m_sdz[1][1][0][0]=-1.81214; 
  p0_tof_s_sdz[1][1][0][0]=0.960329; p1_tof_s_sdz[1][1][0][0]=0.245662; p2_tof_s_sdz[1][1][0][0]=-0.594368; p3_tof_s_sdz[1][1][0][0]=-0.788562; p4_tof_s_sdz[1][1][0][0]=1.51025; 
  p0_tof_m_sdz[0][1][1][0]=-0.0236396; p1_tof_m_sdz[0][1][1][0]=-0.0383868; p2_tof_m_sdz[0][1][1][0]=0.0490965; p3_tof_m_sdz[0][1][1][0]=-0.0189641; p4_tof_m_sdz[0][1][1][0]=0.00217009; 
  p0_tof_s_sdz[0][1][1][0]=0.733038; p1_tof_s_sdz[0][1][1][0]=0.398111; p2_tof_s_sdz[0][1][1][0]=-0.312735; p3_tof_s_sdz[0][1][1][0]=0.0968936; p4_tof_s_sdz[0][1][1][0]=-0.0100531; 
  p0_tof_m_sdz[1][1][1][0]=-0.133606; p1_tof_m_sdz[1][1][1][0]=0.079576; p2_tof_m_sdz[1][1][1][0]=0.29852; p3_tof_m_sdz[1][1][1][0]=0.23906; p4_tof_m_sdz[1][1][1][0]=-0.789477; 
  p0_tof_s_sdz[1][1][1][0]=0.592251; p1_tof_s_sdz[1][1][1][0]=0.549692; p2_tof_s_sdz[1][1][1][0]=0.349453; p3_tof_s_sdz[1][1][1][0]=-0.104235; p4_tof_s_sdz[1][1][1][0]=-0.947135; 
  p0_tof_m_sdz[0][1][2][0]=-0.00340586; p1_tof_m_sdz[0][1][2][0]=-0.0808909; p2_tof_m_sdz[0][1][2][0]=0.0969295; p3_tof_m_sdz[0][1][2][0]=-0.0362642; p4_tof_m_sdz[0][1][2][0]=0.00417665; 
  p0_tof_s_sdz[0][1][2][0]=0.753545; p1_tof_s_sdz[0][1][2][0]=0.358835; p2_tof_s_sdz[0][1][2][0]=-0.290084; p3_tof_s_sdz[0][1][2][0]=0.0909204; p4_tof_s_sdz[0][1][2][0]=-0.00948359; 
  p0_tof_m_sdz[1][1][2][0]=-0.53999; p1_tof_m_sdz[1][1][2][0]=0.468782; p2_tof_m_sdz[1][1][2][0]=1.48135; p3_tof_m_sdz[1][1][2][0]=1.10786; p4_tof_m_sdz[1][1][2][0]=-3.99269; 
  p0_tof_s_sdz[1][1][2][0]=0.647785; p1_tof_s_sdz[1][1][2][0]=0.510725; p2_tof_s_sdz[1][1][2][0]=0.194248; p3_tof_s_sdz[1][1][2][0]=-0.240561; p4_tof_s_sdz[1][1][2][0]=-0.502701; 
  p0_tof_m_sdz[0][1][3][0]=-0.057897; p1_tof_m_sdz[0][1][3][0]=0.0936674; p2_tof_m_sdz[0][1][3][0]=-0.0527207; p3_tof_m_sdz[0][1][3][0]=0.0118745; p4_tof_m_sdz[0][1][3][0]=-0.000961846; 
  p0_tof_s_sdz[0][1][3][0]=0.853287; p1_tof_s_sdz[0][1][3][0]=0.156728; p2_tof_s_sdz[0][1][3][0]=-0.127901; p3_tof_s_sdz[0][1][3][0]=0.0374069; p4_tof_s_sdz[0][1][3][0]=-0.00352406; 
  p0_tof_m_sdz[1][1][3][0]=0.0541375; p1_tof_m_sdz[1][1][3][0]=-0.0477607; p2_tof_m_sdz[1][1][3][0]=-0.134747; p3_tof_m_sdz[1][1][3][0]=-0.0918791; p4_tof_m_sdz[1][1][3][0]=0.309833; 
  p0_tof_s_sdz[1][1][3][0]=1.03841; p1_tof_s_sdz[1][1][3][0]=0.0851491; p2_tof_s_sdz[1][1][3][0]=-0.980927; p3_tof_s_sdz[1][1][3][0]=-1.00313; p4_tof_s_sdz[1][1][3][0]=2.8361; 
  p0_tof_m_sdz[0][1][4][0]=-0.0322547; p1_tof_m_sdz[0][1][4][0]=0.0695057; p2_tof_m_sdz[0][1][4][0]=-0.0483557; p3_tof_m_sdz[0][1][4][0]=0.0141145; p4_tof_m_sdz[0][1][4][0]=-0.00150404; 
  p0_tof_s_sdz[0][1][4][0]=0.604844; p1_tof_s_sdz[0][1][4][0]=0.721501; p2_tof_s_sdz[0][1][4][0]=-0.558149; p3_tof_s_sdz[0][1][4][0]=0.167392; p4_tof_s_sdz[0][1][4][0]=-0.0170153; 
  p0_tof_m_sdz[1][1][4][0]=-0.28805; p1_tof_m_sdz[1][1][4][0]=0.274765; p2_tof_m_sdz[1][1][4][0]=0.81456; p3_tof_m_sdz[1][1][4][0]=0.586621; p4_tof_m_sdz[1][1][4][0]=-2.14898; 
  p0_tof_s_sdz[1][1][4][0]=0.399286; p1_tof_s_sdz[1][1][4][0]=0.787035; p2_tof_s_sdz[1][1][4][0]=0.970167; p3_tof_s_sdz[1][1][4][0]=0.283079; p4_tof_s_sdz[1][1][4][0]=-2.69056; 
  p0_tof_m_sdz[0][1][5][0]=0.00568156; p1_tof_m_sdz[0][1][5][0]=-0.00772076; p2_tof_m_sdz[0][1][5][0]=0.00307617; p3_tof_m_sdz[0][1][5][0]=0.0006375; p4_tof_m_sdz[0][1][5][0]=-0.000181768; 
  p0_tof_s_sdz[0][1][5][0]=0.766948; p1_tof_s_sdz[0][1][5][0]=0.384323; p2_tof_s_sdz[0][1][5][0]=-0.33591; p3_tof_s_sdz[0][1][5][0]=0.111396; p4_tof_s_sdz[0][1][5][0]=-0.0121593; 
  p0_tof_m_sdz[1][1][5][0]=0.356098; p1_tof_m_sdz[1][1][5][0]=-0.330279; p2_tof_m_sdz[1][1][5][0]=-1.00149; p3_tof_m_sdz[1][1][5][0]=-0.732439; p4_tof_m_sdz[1][1][5][0]=2.66919; 
  p0_tof_s_sdz[1][1][5][0]=0.790458; p1_tof_s_sdz[1][1][5][0]=0.408963; p2_tof_s_sdz[1][1][5][0]=-0.152234; p3_tof_s_sdz[1][1][5][0]=-0.521002; p4_tof_s_sdz[1][1][5][0]=0.322099; 
  p0_tof_m_sdz[0][1][6][0]=0.104978; p1_tof_m_sdz[0][1][6][0]=-0.247572; p2_tof_m_sdz[0][1][6][0]=0.183183; p3_tof_m_sdz[0][1][6][0]=-0.0541672; p4_tof_m_sdz[0][1][6][0]=0.00554525; 
  p0_tof_s_sdz[0][1][6][0]=0.819831; p1_tof_s_sdz[0][1][6][0]=0.234586; p2_tof_s_sdz[0][1][6][0]=-0.199125; p3_tof_s_sdz[0][1][6][0]=0.064521; p4_tof_s_sdz[0][1][6][0]=-0.00692293; 
  p0_tof_m_sdz[1][1][6][0]=-0.0433369; p1_tof_m_sdz[1][1][6][0]=0.0495259; p2_tof_m_sdz[1][1][6][0]=0.134507; p3_tof_m_sdz[1][1][6][0]=0.0832349; p4_tof_m_sdz[1][1][6][0]=-0.401684; 
  p0_tof_s_sdz[1][1][6][0]=0.837394; p1_tof_s_sdz[1][1][6][0]=0.297377; p2_tof_s_sdz[1][1][6][0]=-0.368326; p3_tof_s_sdz[1][1][6][0]=-0.573204; p4_tof_s_sdz[1][1][6][0]=1.13809; 
  p0_tof_m_sdz[0][1][7][0]=-0.0784692; p1_tof_m_sdz[0][1][7][0]=0.134088; p2_tof_m_sdz[0][1][7][0]=-0.0998585; p3_tof_m_sdz[0][1][7][0]=0.0327599; p4_tof_m_sdz[0][1][7][0]=-0.00366778; 
  p0_tof_s_sdz[0][1][7][0]=0.870445; p1_tof_s_sdz[0][1][7][0]=0.161645; p2_tof_s_sdz[0][1][7][0]=-0.167292; p3_tof_s_sdz[0][1][7][0]=0.0607223; p4_tof_s_sdz[0][1][7][0]=-0.00691684; 
  p0_tof_m_sdz[1][1][7][0]=-0.218456; p1_tof_m_sdz[1][1][7][0]=0.184572; p2_tof_m_sdz[1][1][7][0]=0.584031; p3_tof_m_sdz[1][1][7][0]=0.423876; p4_tof_m_sdz[1][1][7][0]=-1.632; 
  p0_tof_s_sdz[1][1][7][0]=0.463814; p1_tof_s_sdz[1][1][7][0]=0.717291; p2_tof_s_sdz[1][1][7][0]=0.78963; p3_tof_s_sdz[1][1][7][0]=0.168526; p4_tof_s_sdz[1][1][7][0]=-2.25639; 
  p0_tof_m_sdz[0][1][8][0]=-0.127514; p1_tof_m_sdz[0][1][8][0]=0.244256; p2_tof_m_sdz[0][1][8][0]=-0.191044; p3_tof_m_sdz[0][1][8][0]=0.0606154; p4_tof_m_sdz[0][1][8][0]=-0.00649812; 
  p0_tof_s_sdz[0][1][8][0]=0.767816; p1_tof_s_sdz[0][1][8][0]=0.353255; p2_tof_s_sdz[0][1][8][0]=-0.295968; p3_tof_s_sdz[0][1][8][0]=0.0949111; p4_tof_s_sdz[0][1][8][0]=-0.0101221; 
  p0_tof_m_sdz[1][1][8][0]=0.137692; p1_tof_m_sdz[1][1][8][0]=-0.145257; p2_tof_m_sdz[1][1][8][0]=-0.426155; p3_tof_m_sdz[1][1][8][0]=-0.32468; p4_tof_m_sdz[1][1][8][0]=1.07215; 
  p0_tof_s_sdz[1][1][8][0]=0.241875; p1_tof_s_sdz[1][1][8][0]=0.867675; p2_tof_s_sdz[1][1][8][0]=1.31857; p3_tof_s_sdz[1][1][8][0]=0.619953; p4_tof_s_sdz[1][1][8][0]=-3.4725; 
  p0_tof_m_sdz[0][1][9][0]=0.0102206; p1_tof_m_sdz[0][1][9][0]=-0.098415; p2_tof_m_sdz[0][1][9][0]=0.0829471; p3_tof_m_sdz[0][1][9][0]=-0.024902; p4_tof_m_sdz[0][1][9][0]=0.00253171; 
  p0_tof_s_sdz[0][1][9][0]=0.571094; p1_tof_s_sdz[0][1][9][0]=0.958777; p2_tof_s_sdz[0][1][9][0]=-0.854132; p3_tof_s_sdz[0][1][9][0]=0.282882; p4_tof_s_sdz[0][1][9][0]=-0.0306842; 
  p0_tof_m_sdz[1][1][9][0]=-0.432071; p1_tof_m_sdz[1][1][9][0]=0.247262; p2_tof_m_sdz[1][1][9][0]=0.977551; p3_tof_m_sdz[1][1][9][0]=0.88178; p4_tof_m_sdz[1][1][9][0]=-2.2215; 
  p0_tof_s_sdz[1][1][9][0]=0.366334; p1_tof_s_sdz[1][1][9][0]=0.687371; p2_tof_s_sdz[1][1][9][0]=0.918423; p3_tof_s_sdz[1][1][9][0]=0.458429; p4_tof_s_sdz[1][1][9][0]=-2.20915; 

  //! ToF sdz pos
  p0_tof_m_sdz[0][0][0][1]=0.0505322; p1_tof_m_sdz[0][0][0][1]=0.123075; p2_tof_m_sdz[0][0][0][1]=-0.171317; p3_tof_m_sdz[0][0][0][1]=0.064866; p4_tof_m_sdz[0][0][0][1]=-0.00747936; 
  p0_tof_s_sdz[0][0][0][1]=0.63877; p1_tof_s_sdz[0][0][0][1]=0.13502; p2_tof_s_sdz[0][0][0][1]=0.0617547; p3_tof_s_sdz[0][0][0][1]=-0.0423012; p4_tof_s_sdz[0][0][0][1]=0.0058321; 
  p0_tof_m_sdz[1][0][0][1]=-0.0780844; p1_tof_m_sdz[1][0][0][1]=0.163206; p2_tof_m_sdz[1][0][0][1]=0.392818; p3_tof_m_sdz[1][0][0][1]=0.26524; p4_tof_m_sdz[1][0][0][1]=-1.05426; 
  p0_tof_s_sdz[1][0][0][1]=0.405803; p1_tof_s_sdz[1][0][0][1]=0.504129; p2_tof_s_sdz[1][0][0][1]=0.466019; p3_tof_s_sdz[1][0][0][1]=0.0690488; p4_tof_s_sdz[1][0][0][1]=-1.10128; 
  p0_tof_m_sdz[0][0][1][1]=0.0949803; p1_tof_m_sdz[0][0][1][1]=-0.0140746; p2_tof_m_sdz[0][0][1][1]=-0.0451304; p3_tof_m_sdz[0][0][1][1]=0.021225; p4_tof_m_sdz[0][0][1][1]=-0.00256571; 
  p0_tof_s_sdz[0][0][1][1]=0.510508; p1_tof_s_sdz[0][0][1][1]=0.413441; p2_tof_s_sdz[0][0][1][1]=-0.14282; p3_tof_s_sdz[0][0][1][1]=0.0188677; p4_tof_s_sdz[0][0][1][1]=-0.000540749; 
  p0_tof_m_sdz[1][0][1][1]=0.111515; p1_tof_m_sdz[1][0][1][1]=-0.0304931; p2_tof_m_sdz[1][0][1][1]=-0.172888; p3_tof_m_sdz[1][0][1][1]=-0.12517; p4_tof_m_sdz[1][0][1][1]=0.564553; 
  p0_tof_s_sdz[1][0][1][1]=0.440068; p1_tof_s_sdz[1][0][1][1]=0.466249; p2_tof_s_sdz[1][0][1][1]=0.355847; p3_tof_s_sdz[1][0][1][1]=-0.0271146; p4_tof_s_sdz[1][0][1][1]=-0.899842; 
  p0_tof_m_sdz[0][0][2][1]=0.0777714; p1_tof_m_sdz[0][0][2][1]=-0.136861; p2_tof_m_sdz[0][0][2][1]=0.0868213; p3_tof_m_sdz[0][0][2][1]=-0.024332; p4_tof_m_sdz[0][0][2][1]=0.00248643; 
  p0_tof_s_sdz[0][0][2][1]=0.636959; p1_tof_s_sdz[0][0][2][1]=0.303127; p2_tof_s_sdz[0][0][2][1]=-0.12138; p3_tof_s_sdz[0][0][2][1]=0.0222977; p4_tof_s_sdz[0][0][2][1]=-0.00148266; 
  p0_tof_m_sdz[1][0][2][1]=0.296861; p1_tof_m_sdz[1][0][2][1]=-0.269672; p2_tof_m_sdz[1][0][2][1]=-0.822195; p3_tof_m_sdz[1][0][2][1]=-0.593022; p4_tof_m_sdz[1][0][2][1]=2.22861; 
  p0_tof_s_sdz[1][0][2][1]=0.662482; p1_tof_s_sdz[1][0][2][1]=0.289814; p2_tof_s_sdz[1][0][2][1]=-0.210554; p3_tof_s_sdz[1][0][2][1]=-0.399509; p4_tof_s_sdz[1][0][2][1]=0.859466; 
  p0_tof_m_sdz[0][0][3][1]=-0.0462257; p1_tof_m_sdz[0][0][3][1]=0.0638987; p2_tof_m_sdz[0][0][3][1]=-0.0408192; p3_tof_m_sdz[0][0][3][1]=0.0110919; p4_tof_m_sdz[0][0][3][1]=-0.0010599; 
  p0_tof_s_sdz[0][0][3][1]=0.803148; p1_tof_s_sdz[0][0][3][1]=0.0770589; p2_tof_s_sdz[0][0][3][1]=0.00441617; p3_tof_s_sdz[0][0][3][1]=-0.00922271; p4_tof_s_sdz[0][0][3][1]=0.00146259; 
  p0_tof_m_sdz[1][0][3][1]=0.0797348; p1_tof_m_sdz[1][0][3][1]=-0.103335; p2_tof_m_sdz[1][0][3][1]=-0.262935; p3_tof_m_sdz[1][0][3][1]=-0.160559; p4_tof_m_sdz[1][0][3][1]=0.728556; 
  p0_tof_s_sdz[1][0][3][1]=0.579461; p1_tof_s_sdz[1][0][3][1]=0.4916; p2_tof_s_sdz[1][0][3][1]=0.246178; p3_tof_s_sdz[1][0][3][1]=-0.158102; p4_tof_s_sdz[1][0][3][1]=-0.59495; 
  p0_tof_m_sdz[0][0][4][1]=-0.065486; p1_tof_m_sdz[0][0][4][1]=0.103414; p2_tof_m_sdz[0][0][4][1]=-0.0791223; p3_tof_m_sdz[0][0][4][1]=0.025249; p4_tof_m_sdz[0][0][4][1]=-0.00275589; 
  p0_tof_s_sdz[0][0][4][1]=0.733779; p1_tof_s_sdz[0][0][4][1]=0.269283; p2_tof_s_sdz[0][0][4][1]=-0.16014; p3_tof_s_sdz[0][0][4][1]=0.0447899; p4_tof_s_sdz[0][0][4][1]=-0.00452996; 
  p0_tof_m_sdz[1][0][4][1]=0.199014; p1_tof_m_sdz[1][0][4][1]=-0.23935; p2_tof_m_sdz[1][0][4][1]=-0.653134; p3_tof_m_sdz[1][0][4][1]=-0.43588; p4_tof_m_sdz[1][0][4][1]=1.83322; 
  p0_tof_s_sdz[1][0][4][1]=0.557697; p1_tof_s_sdz[1][0][4][1]=0.558279; p2_tof_s_sdz[1][0][4][1]=0.374745; p3_tof_s_sdz[1][0][4][1]=-0.119898; p4_tof_s_sdz[1][0][4][1]=-1.06542; 
  p0_tof_m_sdz[0][0][5][1]=-0.0830256; p1_tof_m_sdz[0][0][5][1]=0.158934; p2_tof_m_sdz[0][0][5][1]=-0.128129; p3_tof_m_sdz[0][0][5][1]=0.0408771; p4_tof_m_sdz[0][0][5][1]=-0.00438388; 
  p0_tof_s_sdz[0][0][5][1]=0.790429; p1_tof_s_sdz[0][0][5][1]=0.194466; p2_tof_s_sdz[0][0][5][1]=-0.11375; p3_tof_s_sdz[0][0][5][1]=0.0308598; p4_tof_s_sdz[0][0][5][1]=-0.00298; 
  p0_tof_m_sdz[1][0][5][1]=0.0963356; p1_tof_m_sdz[1][0][5][1]=-0.0727902; p2_tof_m_sdz[1][0][5][1]=-0.248033; p3_tof_m_sdz[1][0][5][1]=-0.214823; p4_tof_m_sdz[1][0][5][1]=0.543639; 
  p0_tof_s_sdz[1][0][5][1]=1.10012; p1_tof_s_sdz[1][0][5][1]=0.0606374; p2_tof_s_sdz[1][0][5][1]=-1.10955; p3_tof_s_sdz[1][0][5][1]=-1.17453; p4_tof_s_sdz[1][0][5][1]=2.88332; 
  p0_tof_m_sdz[0][0][6][1]=-0.00926167; p1_tof_m_sdz[0][0][6][1]=-0.0204219; p2_tof_m_sdz[0][0][6][1]=0.00624751; p3_tof_m_sdz[0][0][6][1]=0.00138262; p4_tof_m_sdz[0][0][6][1]=-0.000375565; 
  p0_tof_s_sdz[0][0][6][1]=0.631425; p1_tof_s_sdz[0][0][6][1]=0.575461; p2_tof_s_sdz[0][0][6][1]=-0.406837; p3_tof_s_sdz[0][0][6][1]=0.119636; p4_tof_s_sdz[0][0][6][1]=-0.0122547; 
  p0_tof_m_sdz[1][0][6][1]=-0.6621; p1_tof_m_sdz[1][0][6][1]=0.637116; p2_tof_m_sdz[1][0][6][1]=1.88317; p3_tof_m_sdz[1][0][6][1]=1.31138; p4_tof_m_sdz[1][0][6][1]=-5.21403; 
  p0_tof_s_sdz[1][0][6][1]=0.577874; p1_tof_s_sdz[1][0][6][1]=0.575778; p2_tof_s_sdz[1][0][6][1]=0.379453; p3_tof_s_sdz[1][0][6][1]=-0.148738; p4_tof_s_sdz[1][0][6][1]=-1.16393; 
  p0_tof_m_sdz[0][0][7][1]=-0.044644; p1_tof_m_sdz[0][0][7][1]=0.0605946; p2_tof_m_sdz[0][0][7][1]=-0.0502104; p3_tof_m_sdz[0][0][7][1]=0.0178992; p4_tof_m_sdz[0][0][7][1]=-0.00207095; 
  p0_tof_s_sdz[0][0][7][1]=0.646362; p1_tof_s_sdz[0][0][7][1]=0.517227; p2_tof_s_sdz[0][0][7][1]=-0.355575; p3_tof_s_sdz[0][0][7][1]=0.102964; p4_tof_s_sdz[0][0][7][1]=-0.0105626; 
  p0_tof_m_sdz[1][0][7][1]=-0.021755; p1_tof_m_sdz[1][0][7][1]=0.03501; p2_tof_m_sdz[1][0][7][1]=0.0839497; p3_tof_m_sdz[1][0][7][1]=0.0396978; p4_tof_m_sdz[1][0][7][1]=-0.295835; 
  p0_tof_s_sdz[1][0][7][1]=0.390655; p1_tof_s_sdz[1][0][7][1]=0.671349; p2_tof_s_sdz[1][0][7][1]=0.794619; p3_tof_s_sdz[1][0][7][1]=0.253358; p4_tof_s_sdz[1][0][7][1]=-2.06593; 
  p0_tof_m_sdz[0][0][8][1]=0.201581; p1_tof_m_sdz[0][0][8][1]=-0.543472; p2_tof_m_sdz[0][0][8][1]=0.419281; p3_tof_m_sdz[0][0][8][1]=-0.128157; p4_tof_m_sdz[0][0][8][1]=0.0134674; 
  p0_tof_s_sdz[0][0][8][1]=0.55874; p1_tof_s_sdz[0][0][8][1]=0.664932; p2_tof_s_sdz[0][0][8][1]=-0.456214; p3_tof_s_sdz[0][0][8][1]=0.131539; p4_tof_s_sdz[0][0][8][1]=-0.01337; 
  p0_tof_m_sdz[1][0][8][1]=0.441046; p1_tof_m_sdz[1][0][8][1]=-0.450259; p2_tof_m_sdz[1][0][8][1]=-1.34961; p3_tof_m_sdz[1][0][8][1]=-0.991926; p4_tof_m_sdz[1][0][8][1]=3.71117; 
  p0_tof_s_sdz[1][0][8][1]=1.06571; p1_tof_s_sdz[1][0][8][1]=0.0592574; p2_tof_s_sdz[1][0][8][1]=-1.14453; p3_tof_s_sdz[1][0][8][1]=-1.27072; p4_tof_s_sdz[1][0][8][1]=2.98824; 
  p0_tof_m_sdz[0][0][9][1]=0.0437671; p1_tof_m_sdz[0][0][9][1]=-0.12417; p2_tof_m_sdz[0][0][9][1]=0.0685228; p3_tof_m_sdz[0][0][9][1]=-0.0145856; p4_tof_m_sdz[0][0][9][1]=0.00112299; 
  p0_tof_s_sdz[0][0][9][1]=0.697189; p1_tof_s_sdz[0][0][9][1]=0.317801; p2_tof_s_sdz[0][0][9][1]=-0.166864; p3_tof_s_sdz[0][0][9][1]=0.0372487; p4_tof_s_sdz[0][0][9][1]=-0.00306671; 
  p0_tof_m_sdz[1][0][9][1]=-0.850162; p1_tof_m_sdz[1][0][9][1]=0.818274; p2_tof_m_sdz[1][0][9][1]=2.39826; p3_tof_m_sdz[1][0][9][1]=1.6499; p4_tof_m_sdz[1][0][9][1]=-6.70052; 
  p0_tof_s_sdz[1][0][9][1]=1.38409; p1_tof_s_sdz[1][0][9][1]=-0.27494; p2_tof_s_sdz[1][0][9][1]=-1.94122; p3_tof_s_sdz[1][0][9][1]=-1.67868; p4_tof_s_sdz[1][0][9][1]=5.1678; 
  p0_tof_m_sdz[0][1][0][1]=-0.0277219; p1_tof_m_sdz[0][1][0][1]=-0.00460434; p2_tof_m_sdz[0][1][0][1]=0.00453443; p3_tof_m_sdz[0][1][0][1]=0.000755208; p4_tof_m_sdz[0][1][0][1]=-0.000192469; 
  p0_tof_s_sdz[0][1][0][1]=0.761136; p1_tof_s_sdz[0][1][0][1]=0.405219; p2_tof_s_sdz[0][1][0][1]=-0.311313; p3_tof_s_sdz[0][1][0][1]=0.0938454; p4_tof_s_sdz[0][1][0][1]=-0.00966487; 
  p0_tof_m_sdz[1][1][0][1]=-17.8415; p1_tof_m_sdz[1][1][0][1]=62.7431; p2_tof_m_sdz[1][1][0][1]=-6.967; p3_tof_m_sdz[1][1][0][1]=-167.592; p4_tof_m_sdz[1][1][0][1]=145.596; 
  p0_tof_s_sdz[1][1][0][1]=0.624509; p1_tof_s_sdz[1][1][0][1]=0.490752; p2_tof_s_sdz[1][1][0][1]=0.225971; p3_tof_s_sdz[1][1][0][1]=-0.114706; p4_tof_s_sdz[1][1][0][1]=-0.334571; 
  p0_tof_m_sdz[0][1][1][1]=-0.108512; p1_tof_m_sdz[0][1][1][1]=0.153402; p2_tof_m_sdz[0][1][1][1]=-0.0954433; p3_tof_m_sdz[0][1][1][1]=0.0265052; p4_tof_m_sdz[0][1][1][1]=-0.00267077; 
  p0_tof_s_sdz[0][1][1][1]=0.894032; p1_tof_s_sdz[0][1][1][1]=-0.00865729; p2_tof_s_sdz[0][1][1][1]=0.0388269; p3_tof_s_sdz[0][1][1][1]=-0.0200006; p4_tof_s_sdz[0][1][1][1]=0.00276335; 
  p0_tof_m_sdz[1][1][1][1]=0.393945; p1_tof_m_sdz[1][1][1][1]=-0.434311; p2_tof_m_sdz[1][1][1][1]=-1.18692; p3_tof_m_sdz[1][1][1][1]=-0.776654; p4_tof_m_sdz[1][1][1][1]=3.24679; 
  p0_tof_s_sdz[1][1][1][1]=0.855942; p1_tof_s_sdz[1][1][1][1]=0.273699; p2_tof_s_sdz[1][1][1][1]=-0.434722; p3_tof_s_sdz[1][1][1][1]=-0.620924; p4_tof_s_sdz[1][1][1][1]=1.31006; 
  p0_tof_m_sdz[0][1][2][1]=0.0530859; p1_tof_m_sdz[0][1][2][1]=-0.187834; p2_tof_m_sdz[0][1][2][1]=0.157783; p3_tof_m_sdz[0][1][2][1]=-0.0490197; p4_tof_m_sdz[0][1][2][1]=0.00512537; 
  p0_tof_s_sdz[0][1][2][1]=0.719632; p1_tof_s_sdz[0][1][2][1]=0.402813; p2_tof_s_sdz[0][1][2][1]=-0.298008; p3_tof_s_sdz[0][1][2][1]=0.0873935; p4_tof_s_sdz[0][1][2][1]=-0.00876051; 
  p0_tof_m_sdz[1][1][2][1]=0.462684; p1_tof_m_sdz[1][1][2][1]=-0.449032; p2_tof_m_sdz[1][1][2][1]=-1.29291; p3_tof_m_sdz[1][1][2][1]=-0.87875; p4_tof_m_sdz[1][1][2][1]=3.50901; 
  p0_tof_s_sdz[1][1][2][1]=0.591039; p1_tof_s_sdz[1][1][2][1]=0.561423; p2_tof_s_sdz[1][1][2][1]=0.343935; p3_tof_s_sdz[1][1][2][1]=-0.162128; p4_tof_s_sdz[1][1][2][1]=-1.04182; 
  p0_tof_m_sdz[0][1][3][1]=-0.036901; p1_tof_m_sdz[0][1][3][1]=0.0591172; p2_tof_m_sdz[0][1][3][1]=-0.031584; p3_tof_m_sdz[0][1][3][1]=0.00621306; p4_tof_m_sdz[0][1][3][1]=-0.000447812; 
  p0_tof_s_sdz[0][1][3][1]=0.785438; p1_tof_s_sdz[0][1][3][1]=0.324223; p2_tof_s_sdz[0][1][3][1]=-0.25612; p3_tof_s_sdz[0][1][3][1]=0.0743188; p4_tof_s_sdz[0][1][3][1]=-0.00725665; 
  p0_tof_m_sdz[1][1][3][1]=-0.0894178; p1_tof_m_sdz[1][1][3][1]=0.173446; p2_tof_m_sdz[1][1][3][1]=0.371592; p3_tof_m_sdz[1][1][3][1]=0.135978; p4_tof_m_sdz[1][1][3][1]=-1.31699; 
  p0_tof_s_sdz[1][1][3][1]=0.581931; p1_tof_s_sdz[1][1][3][1]=0.572261; p2_tof_s_sdz[1][1][3][1]=0.39561; p3_tof_s_sdz[1][1][3][1]=-0.0729734; p4_tof_s_sdz[1][1][3][1]=-1.01343; 
  p0_tof_m_sdz[0][1][4][1]=-0.0631927; p1_tof_m_sdz[0][1][4][1]=0.112454; p2_tof_m_sdz[0][1][4][1]=-0.0718681; p3_tof_m_sdz[0][1][4][1]=0.0203324; p4_tof_m_sdz[0][1][4][1]=-0.00216082; 
  p0_tof_s_sdz[0][1][4][1]=0.649158; p1_tof_s_sdz[0][1][4][1]=0.653549; p2_tof_s_sdz[0][1][4][1]=-0.514965; p3_tof_s_sdz[0][1][4][1]=0.155374; p4_tof_s_sdz[0][1][4][1]=-0.0158584; 
  p0_tof_m_sdz[1][1][4][1]=-0.0885516; p1_tof_m_sdz[1][1][4][1]=0.0910718; p2_tof_m_sdz[1][1][4][1]=0.272588; p3_tof_m_sdz[1][1][4][1]=0.196297; p4_tof_m_sdz[1][1][4][1]=-0.77786; 
  p0_tof_s_sdz[1][1][4][1]=0.576031; p1_tof_s_sdz[1][1][4][1]=0.649842; p2_tof_s_sdz[1][1][4][1]=0.521011; p3_tof_s_sdz[1][1][4][1]=-0.083791; p4_tof_s_sdz[1][1][4][1]=-1.65799; 
  p0_tof_m_sdz[0][1][5][1]=-0.0276685; p1_tof_m_sdz[0][1][5][1]=0.0760664; p2_tof_m_sdz[0][1][5][1]=-0.0717673; p3_tof_m_sdz[0][1][5][1]=0.0261477; p4_tof_m_sdz[0][1][5][1]=-0.00310848; 
  p0_tof_s_sdz[0][1][5][1]=0.640883; p1_tof_s_sdz[0][1][5][1]=0.618202; p2_tof_s_sdz[0][1][5][1]=-0.4813; p3_tof_s_sdz[0][1][5][1]=0.146995; p4_tof_s_sdz[0][1][5][1]=-0.0152763; 
  p0_tof_m_sdz[1][1][5][1]=-0.150695; p1_tof_m_sdz[1][1][5][1]=0.164191; p2_tof_m_sdz[1][1][5][1]=0.441844; p3_tof_m_sdz[1][1][5][1]=0.263531; p4_tof_m_sdz[1][1][5][1]=-1.30336; 
  p0_tof_s_sdz[1][1][5][1]=0.932431; p1_tof_s_sdz[1][1][5][1]=0.274061; p2_tof_s_sdz[1][1][5][1]=-0.527614; p3_tof_s_sdz[1][1][5][1]=-0.768502; p4_tof_s_sdz[1][1][5][1]=1.28675; 
  p0_tof_m_sdz[0][1][6][1]=-0.0993152; p1_tof_m_sdz[0][1][6][1]=0.202992; p2_tof_m_sdz[0][1][6][1]=-0.148233; p3_tof_m_sdz[0][1][6][1]=0.0441384; p4_tof_m_sdz[0][1][6][1]=-0.00457008; 
  p0_tof_s_sdz[0][1][6][1]=0.794727; p1_tof_s_sdz[0][1][6][1]=0.290544; p2_tof_s_sdz[0][1][6][1]=-0.242857; p3_tof_s_sdz[0][1][6][1]=0.0785372; p4_tof_s_sdz[0][1][6][1]=-0.00841452; 
  p0_tof_m_sdz[1][1][6][1]=-0.221028; p1_tof_m_sdz[1][1][6][1]=0.241456; p2_tof_m_sdz[1][1][6][1]=0.659842; p3_tof_m_sdz[1][1][6][1]=0.403785; p4_tof_m_sdz[1][1][6][1]=-1.96195; 
  p0_tof_s_sdz[1][1][6][1]=0.962514; p1_tof_s_sdz[1][1][6][1]=0.313724; p2_tof_s_sdz[1][1][6][1]=-0.541697; p3_tof_s_sdz[1][1][6][1]=-0.894599; p4_tof_s_sdz[1][1][6][1]=1.1183; 
  p0_tof_m_sdz[0][1][7][1]=-0.103528; p1_tof_m_sdz[0][1][7][1]=0.182719; p2_tof_m_sdz[0][1][7][1]=-0.126904; p3_tof_m_sdz[0][1][7][1]=0.0374009; p4_tof_m_sdz[0][1][7][1]=-0.00388174; 
  p0_tof_s_sdz[0][1][7][1]=0.861494; p1_tof_s_sdz[0][1][7][1]=0.175462; p2_tof_s_sdz[0][1][7][1]=-0.170343; p3_tof_s_sdz[0][1][7][1]=0.0598691; p4_tof_s_sdz[0][1][7][1]=-0.00676273; 
  p0_tof_m_sdz[1][1][7][1]=-0.307467; p1_tof_m_sdz[1][1][7][1]=0.315945; p2_tof_m_sdz[1][1][7][1]=0.896189; p3_tof_m_sdz[1][1][7][1]=0.583751; p4_tof_m_sdz[1][1][7][1]=-2.59281; 
  p0_tof_s_sdz[1][1][7][1]=0.964256; p1_tof_s_sdz[1][1][7][1]=0.285393; p2_tof_s_sdz[1][1][7][1]=-0.594208; p3_tof_s_sdz[1][1][7][1]=-0.880724; p4_tof_s_sdz[1][1][7][1]=1.50135; 
  p0_tof_m_sdz[0][1][8][1]=-0.174732; p1_tof_m_sdz[0][1][8][1]=0.322678; p2_tof_m_sdz[0][1][8][1]=-0.229729; p3_tof_m_sdz[0][1][8][1]=0.0676864; p4_tof_m_sdz[0][1][8][1]=-0.00689623; 
  p0_tof_s_sdz[0][1][8][1]=0.712779; p1_tof_s_sdz[0][1][8][1]=0.474219; p2_tof_s_sdz[0][1][8][1]=-0.387757; p3_tof_s_sdz[0][1][8][1]=0.123184; p4_tof_s_sdz[0][1][8][1]=-0.0131764; 
  p0_tof_m_sdz[1][1][8][1]=0.317038; p1_tof_m_sdz[1][1][8][1]=-0.288071; p2_tof_m_sdz[1][1][8][1]=-0.869732; p3_tof_m_sdz[1][1][8][1]=-0.652469; p4_tof_m_sdz[1][1][8][1]=2.154; 
  p0_tof_s_sdz[1][1][8][1]=0.796454; p1_tof_s_sdz[1][1][8][1]=0.468593; p2_tof_s_sdz[1][1][8][1]=-0.0683458; p3_tof_s_sdz[1][1][8][1]=-0.552908; p4_tof_s_sdz[1][1][8][1]=-0.193569; 
  p0_tof_m_sdz[0][1][9][1]=-0.00919064; p1_tof_m_sdz[0][1][9][1]=-0.100169; p2_tof_m_sdz[0][1][9][1]=0.11055; p3_tof_m_sdz[0][1][9][1]=-0.0394938; p4_tof_m_sdz[0][1][9][1]=0.00435002; 
  p0_tof_s_sdz[0][1][9][1]=0.557613; p1_tof_s_sdz[0][1][9][1]=0.992751; p2_tof_s_sdz[0][1][9][1]=-0.89768; p3_tof_s_sdz[0][1][9][1]=0.302808; p4_tof_s_sdz[0][1][9][1]=-0.0335366; 
  p0_tof_m_sdz[1][1][9][1]=0.531703; p1_tof_m_sdz[1][1][9][1]=-0.451829; p2_tof_m_sdz[1][1][9][1]=-1.42138; p3_tof_m_sdz[1][1][9][1]=-1.11005; p4_tof_m_sdz[1][1][9][1]=3.4729; 
  p0_tof_s_sdz[1][1][9][1]=0.155798; p1_tof_s_sdz[1][1][9][1]=1.05902; p2_tof_s_sdz[1][1][9][1]=1.67316; p3_tof_s_sdz[1][1][9][1]=0.686823; p4_tof_s_sdz[1][1][9][1]=-4.73798; 

  //cout<<"MatchrecalReco62GeVRun10::InitPars() ::::: Loaded array for Mean and Sigma for Matching Calibrations"<<endl;
  return 1;
}
//_____________________________________________________________________________________________________________________________
