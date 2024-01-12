#include <MatchrecalReco27GeVRun11.h>
#include <getClass.h>
#include <PHGlobal.h>
#include <PHCompositeNode.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <RunHeader.h>

#include <iostream>

using namespace std;

//_____________________________________________________________________________________________________________________________
MatchrecalReco27GeVRun11::MatchrecalReco27GeVRun11(const string &name) : Recalibrator(name)
{
  baseclasses.insert("PHCentralTrackv23");
  baseclasses.insert("PHCentralTrackv24");
  return;
}
int MatchrecalReco27GeVRun11::isValidRun(const int RunNumber) const
{
  if(RunNumber>=349830 && RunNumber<=350577)return 1;
  else return 0;
}
//_____________________________________________________________________________________________________________________________
MatchrecalReco27GeVRun11::~MatchrecalReco27GeVRun11()
{

}
//_____________________________________________________________________________________________________________________________
int MatchrecalReco27GeVRun11::Init(PHCompositeNode *topNode)
{
    
  //! Initialize the matching arrays
  for(int ia=0;ia<NARM;ia++){ //! arm
    for(int iz=0;iz<NZBN;iz++){ //! zed
      for(int ic=0;ic<2;ic++){ //! charge
	//! sigma
	//! PC3
	p0_pc3_s_dp[ia][iz][ic]=p1_pc3_s_dp[ia][iz][ic]=p2_pc3_s_dp[ia][iz][ic]=p3_pc3_s_dp[ia][iz][ic]=p4_pc3_s_dp[ia][iz][ic]=0;
	p0_pc3_s_dz[ia][iz][ic]=p1_pc3_s_dz[ia][iz][ic]=p2_pc3_s_dz[ia][iz][ic]=p3_pc3_s_dz[ia][iz][ic]=p4_pc3_s_dz[ia][iz][ic]=0;
	
		
	for(int ip=0;ip<2;ip++){ //! pt
	  //! mean
	  
	  //! PC3
	  p0_pc3_m_dp[ip][ia][iz][ic]=p1_pc3_m_dp[ip][ia][iz][ic]=p2_pc3_m_dp[ip][ia][iz][ic]=0,p3_pc3_m_dp[ip][ia][iz][ic]=0,p4_pc3_m_dp[ip][ia][iz][ic]=0;
	  p0_pc3_m_dz[ip][ia][iz][ic]=p1_pc3_m_dz[ip][ia][iz][ic]=p2_pc3_m_dz[ip][ia][iz][ic]=0,p3_pc3_m_dz[ip][ia][iz][ic]=0,p4_pc3_m_dz[ip][ia][iz][ic]=0;
	  
	  
	  //! Afterburn
	  
	  
	  //! PC3
	  p0_pc3_m_sdp[ip][ia][iz][ic]=p1_pc3_m_sdp[ip][ia][iz][ic]=p2_pc3_m_sdp[ip][ia][iz][ic]=0,p3_pc3_m_sdp[ip][ia][iz][ic]=0,p4_pc3_m_sdp[ip][ia][iz][ic]=0;
	  p0_pc3_m_sdz[ip][ia][iz][ic]=p1_pc3_m_sdz[ip][ia][iz][ic]=p2_pc3_m_sdz[ip][ia][iz][ic]=0,p3_pc3_m_sdz[ip][ia][iz][ic]=0,p4_pc3_m_sdz[ip][ia][iz][ic]=0;
	  p0_pc3_s_sdp[ip][ia][iz][ic]=p1_pc3_s_sdp[ip][ia][iz][ic]=p2_pc3_s_sdp[ip][ia][iz][ic]=0,p3_pc3_s_sdp[ip][ia][iz][ic]=0,p4_pc3_s_sdp[ip][ia][iz][ic]=0;
	  p0_pc3_s_sdz[ip][ia][iz][ic]=p1_pc3_s_sdz[ip][ia][iz][ic]=p2_pc3_s_sdz[ip][ia][iz][ic]=0,p3_pc3_s_sdz[ip][ia][iz][ic]=0,p4_pc3_s_sdz[ip][ia][iz][ic]=0;
	  
	  
	}
      }
    }
  }

  //! For EMC
  for(int ie=0;ie<NEMC;ie++){   //! iEmc
    for(int iz=0;iz<NZBN;iz++){ //! zed
      for(int ic=0;ic<2;ic++){  //! charge

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
  

  //! Matching Calibration Parameters
  InitPars();
  return 0;
}

//_____________________________________________________________________________________________________________________________
int MatchrecalReco27GeVRun11::InitRun(PHCompositeNode *topNode)
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
  cout << " MatchrecalReco27GeVRun11::InitRun() :  run number = " << RunNumber << endl;
  cout << "---------------------------------------------------------------" << endl;
  return 0;
}

//_____________________________________________________________________________________________________________________________
int MatchrecalReco27GeVRun11::process_event(PHCompositeNode *topNode)
{

  PHGlobal*global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if(!global){
    cout << PHWHERE << "Could not find PHGlobal !" << endl;
    return 0;
  }

  int cent = (int)global->getCentrality();
  cent = cent/5;

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
    //    float pc2dphi  = sngltrk->get_pc2dphi();
    //float pc2dz    = sngltrk->get_pc2dz();
    
    //! pc3
    float pc3dphi  = sngltrk->get_pc3dphi();
    float pc3dz    = sngltrk->get_pc3dz();
    

    
    //! emc
    float   emcdphi  = sngltrk->get_emcdphi();
    float   emcdz    = sngltrk->get_emcdz();
    
  
    float pc3sdphi = -9999.;
    float pc3sdz   = -9999.;
    float emcsdphi = -9999.;
    float emcsdz   = -9999.;
    
    

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
      if(fabs(pc3sdz) < 5.00){
	pc3sdz   = GetFineTSval(iarm,cent,pt,pc3sdz);
      }
    }
    
    //! remove the background
    if(fabs(pc3sdphi)>3.00){
        emcdphi=-9999; 
    }
    if(fabs(pc3sdz)>3.00){
       emcdz=-9999;
    }
    
    //! EMC
    if(emcdphi > -9999 || emcdz > -9999){
      emcsdphi = GetSval(0,2,iemc,ized,ich,pt,emcdphi);
      emcsdz   = GetSval(1,2,iemc,ized,ich,pt,emcdz);
    }
      

    // Set all variables
    sngltrk->set_pc3sdphi(pc3sdphi);
    sngltrk->set_pc3sdz(pc3sdz);
    sngltrk->set_emcsdphi(emcsdphi);
    sngltrk->set_emcsdz(emcsdz);
    
  }//! itrk loop ends

  return 0;
}
//_____________________________________________________________________________________________________________________________
int MatchrecalReco27GeVRun11::Zed(float zed)
{
  int ized=(int)(NZBN*(zed + 75.)/150.); 
  if(ized<0 || ized>=NZBN)return -9999;
  else  return ized;
}

float MatchrecalReco27GeVRun11::GetFineTSval(int isub,int cent,float pt,float sdz)
{
	
  float A0=1.0;
  float A1=0.0;
  float A2=0.0;
  
  //! Only for PC3 sdz
  if(isub==0){ //! arm : 0
    
    A0 = 0.8771       +   0.0473905/pt       + (-0.0152935)/pt/pt;
    A1 = 0.0226181    + (-0.0857552)*pt      +   0.0599097*pt*pt;
    A2 = 0.000888015  +   0.000199212/pt     + (-0.000129312)/pt/pt;
    
  }else if(isub==1){//! arm : 1

    A0 =  0.882105    +   (0.0343677)/pt  + (-0.0109955)/pt/pt;
    A1 =  0.0135246   +   (-0.0555747)*pt  + (0.0427627)*pt*pt;
    A2 = (-0.000770478) +   (0.00151922)/pt + (-0.000390496)/pt/pt;
  }
	
  float ftsdz = A0 + A1*cent + A2*cent*cent;
  sdz /= ftsdz;
	  
  return sdz;
}
	


//_____________________________________________________________________________________________________________________________

float MatchrecalReco27GeVRun11::GetSval(int itype,int idet,int isub,int ized,int ich,float fpt,float fdval)
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
    
//     if(idet==0){//! PC2 
//     }
    
    if(idet==1){//! PC3
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
//     else if(idet==3){//! ToF
      
//     }
  }
  else if(itype==1){  //! dz
    //cout<<"GetSval :: dZ ******** "<<endl;
//     if(idet==0){//! PC2 
//     }
    if(idet==1){//! PC3
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
int MatchrecalReco27GeVRun11::InitPars(void)
{
 //EMC DZ NEG 
    p0_emc_m_dz[0][0][0]=-1.19907; p1_emc_m_dz[0][0][0]=5.15106; p2_emc_m_dz[0][0][0]=-5.21115; p3_emc_m_dz[0][0][0]=2.45496; p4_emc_m_dz[0][0][0]=-0.403377; 
    p0_emc_s_dz[0][0][0]=3.21389; p1_emc_s_dz[0][0][0]=-1.5606; p2_emc_s_dz[0][0][0]=2.03439; p3_emc_s_dz[0][0][0]=-0.94478; p4_emc_s_dz[0][0][0]=0.171653; 
    p0_emc_m_dz[0][1][0]=-0.897776; p1_emc_m_dz[0][1][0]=4.49505; p2_emc_m_dz[0][1][0]=-4.58851; p3_emc_m_dz[0][1][0]=2.11319; p4_emc_m_dz[0][1][0]=-0.325296; 
    p0_emc_s_dz[0][1][0]=2.26635; p1_emc_s_dz[0][1][0]=1.08007; p2_emc_s_dz[0][1][0]=-0.768401; p3_emc_s_dz[0][1][0]=0.363046; p4_emc_s_dz[0][1][0]=-0.0478491; 
    p0_emc_m_dz[0][2][0]=0.120312; p1_emc_m_dz[0][2][0]=1.58047; p2_emc_m_dz[0][2][0]=-1.49841; p3_emc_m_dz[0][2][0]=0.732494; p4_emc_m_dz[0][2][0]=-0.11391; 
    p0_emc_s_dz[0][2][0]=2.76438; p1_emc_s_dz[0][2][0]=-1.22448; p2_emc_s_dz[0][2][0]=1.71864; p3_emc_s_dz[0][2][0]=-0.634937; p4_emc_s_dz[0][2][0]=0.0855864; 
    p0_emc_m_dz[0][3][0]=-0.00133332; p1_emc_m_dz[0][3][0]=2.56033; p2_emc_m_dz[0][3][0]=-2.78667; p3_emc_m_dz[0][3][0]=1.29771; p4_emc_m_dz[0][3][0]=-0.201109; 
    p0_emc_s_dz[0][3][0]=2.0651; p1_emc_s_dz[0][3][0]=0.313045; p2_emc_s_dz[0][3][0]=0.29529; p3_emc_s_dz[0][3][0]=-0.0842916; p4_emc_s_dz[0][3][0]=0.0136033; 
    p0_emc_m_dz[0][4][0]=0.957101; p1_emc_m_dz[0][4][0]=-0.203442; p2_emc_m_dz[0][4][0]=0.274821; p3_emc_m_dz[0][4][0]=-0.134112; p4_emc_m_dz[0][4][0]=0.0212102; 
    p0_emc_s_dz[0][4][0]=1.77962; p1_emc_s_dz[0][4][0]=0.829708; p2_emc_s_dz[0][4][0]=-0.116167; p3_emc_s_dz[0][4][0]=0.0663099; p4_emc_s_dz[0][4][0]=-0.00575966; 
    p0_emc_m_dz[0][5][0]=1.42404; p1_emc_m_dz[0][5][0]=-1.94112; p2_emc_m_dz[0][5][0]=1.88641; p3_emc_m_dz[0][5][0]=-0.81895; p4_emc_m_dz[0][5][0]=0.118051; 
    p0_emc_s_dz[0][5][0]=2.06069; p1_emc_s_dz[0][5][0]=0.441902; p2_emc_s_dz[0][5][0]=-0.0836346; p3_emc_s_dz[0][5][0]=0.147847; p4_emc_s_dz[0][5][0]=-0.0268701; 
    p0_emc_m_dz[0][6][0]=1.49297; p1_emc_m_dz[0][6][0]=-1.76937; p2_emc_m_dz[0][6][0]=1.67494; p3_emc_m_dz[0][6][0]=-0.786405; p4_emc_m_dz[0][6][0]=0.118081; 
    p0_emc_s_dz[0][6][0]=2.42755; p1_emc_s_dz[0][6][0]=-0.304124; p2_emc_s_dz[0][6][0]=0.547311; p3_emc_s_dz[0][6][0]=-0.103858; p4_emc_s_dz[0][6][0]=0.0079919; 
    p0_emc_m_dz[0][7][0]=1.69553; p1_emc_m_dz[0][7][0]=-1.78851; p2_emc_m_dz[0][7][0]=1.71404; p3_emc_m_dz[0][7][0]=-0.906323; p4_emc_m_dz[0][7][0]=0.147297; 
    p0_emc_s_dz[0][7][0]=2.69471; p1_emc_s_dz[0][7][0]=-0.406756; p2_emc_s_dz[0][7][0]=0.453152; p3_emc_s_dz[0][7][0]=-0.0388989; p4_emc_s_dz[0][7][0]=-0.00206434; 
    p0_emc_m_dz[0][8][0]=2.5906; p1_emc_m_dz[0][8][0]=-4.00352; p2_emc_m_dz[0][8][0]=3.78955; p3_emc_m_dz[0][8][0]=-1.74852; p4_emc_m_dz[0][8][0]=0.266175; 
    p0_emc_s_dz[0][8][0]=2.79473; p1_emc_s_dz[0][8][0]=-0.372526; p2_emc_s_dz[0][8][0]=0.487987; p3_emc_s_dz[0][8][0]=-0.112423; p4_emc_s_dz[0][8][0]=0.0151416; 
    p0_emc_m_dz[0][9][0]=2.95357; p1_emc_m_dz[0][9][0]=-5.00778; p2_emc_m_dz[0][9][0]=4.78257; p3_emc_m_dz[0][9][0]=-2.19518; p4_emc_m_dz[0][9][0]=0.351573; 
    p0_emc_s_dz[0][9][0]=3.20665; p1_emc_s_dz[0][9][0]=-1.11298; p2_emc_s_dz[0][9][0]=1.20754; p3_emc_s_dz[0][9][0]=-0.47054; p4_emc_s_dz[0][9][0]=0.0816524; 
    p0_emc_m_dz[1][0][0]=-3.3035; p1_emc_m_dz[1][0][0]=4.79476; p2_emc_m_dz[1][0][0]=-4.43039; p3_emc_m_dz[1][0][0]=1.94384; p4_emc_m_dz[1][0][0]=-0.30906; 
    p0_emc_s_dz[1][0][0]=3.29571; p1_emc_s_dz[1][0][0]=-1.04475; p2_emc_s_dz[1][0][0]=1.13591; p3_emc_s_dz[1][0][0]=-0.442346; p4_emc_s_dz[1][0][0]=0.0795796; 
    p0_emc_m_dz[1][1][0]=-2.63477; p1_emc_m_dz[1][1][0]=4.06102; p2_emc_m_dz[1][1][0]=-4.08857; p3_emc_m_dz[1][1][0]=1.86345; p4_emc_m_dz[1][1][0]=-0.288197; 
    p0_emc_s_dz[1][1][0]=2.96233; p1_emc_s_dz[1][1][0]=-0.346903; p2_emc_s_dz[1][1][0]=0.402448; p3_emc_s_dz[1][1][0]=-0.0642505; p4_emc_s_dz[1][1][0]=0.00593324; 
    p0_emc_m_dz[1][2][0]=-1.73492; p1_emc_m_dz[1][2][0]=2.72817; p2_emc_m_dz[1][2][0]=-2.83484; p3_emc_m_dz[1][2][0]=1.31269; p4_emc_m_dz[1][2][0]=-0.201193; 
    p0_emc_s_dz[1][2][0]=2.57806; p1_emc_s_dz[1][2][0]=0.420549; p2_emc_s_dz[1][2][0]=-0.380301; p3_emc_s_dz[1][2][0]=0.277664; p4_emc_s_dz[1][2][0]=-0.0458206; 
    p0_emc_m_dz[1][3][0]=-0.614346; p1_emc_m_dz[1][3][0]=1.23364; p2_emc_m_dz[1][3][0]=-1.36873; p3_emc_m_dz[1][3][0]=0.660798; p4_emc_m_dz[1][3][0]=-0.104373; 
    p0_emc_s_dz[1][3][0]=2.43004; p1_emc_s_dz[1][3][0]=0.514515; p2_emc_s_dz[1][3][0]=-0.448329; p3_emc_s_dz[1][3][0]=0.327376; p4_emc_s_dz[1][3][0]=-0.0565213; 
    p0_emc_m_dz[1][4][0]=0.0520341; p1_emc_m_dz[1][4][0]=0.773594; p2_emc_m_dz[1][4][0]=-0.85086; p3_emc_m_dz[1][4][0]=0.367984; p4_emc_m_dz[1][4][0]=-0.0544105; 
    p0_emc_s_dz[1][4][0]=2.48744; p1_emc_s_dz[1][4][0]=0.283194; p2_emc_s_dz[1][4][0]=-0.116875; p3_emc_s_dz[1][4][0]=0.175419; p4_emc_s_dz[1][4][0]=-0.0335112; 
    p0_emc_m_dz[1][5][0]=1.54703; p1_emc_m_dz[1][5][0]=-1.26477; p2_emc_m_dz[1][5][0]=1.12142; p3_emc_m_dz[1][5][0]=-0.495156; p4_emc_m_dz[1][5][0]=0.0732249; 
    p0_emc_s_dz[1][5][0]=2.22819; p1_emc_s_dz[1][5][0]=1.00615; p2_emc_s_dz[1][5][0]=-0.768861; p3_emc_s_dz[1][5][0]=0.422842; p4_emc_s_dz[1][5][0]=-0.0672765; 
    p0_emc_m_dz[1][6][0]=2.52747; p1_emc_m_dz[1][6][0]=-2.31261; p2_emc_m_dz[1][6][0]=2.08778; p3_emc_m_dz[1][6][0]=-0.941856; p4_emc_m_dz[1][6][0]=0.142974; 
    p0_emc_s_dz[1][6][0]=2.54773; p1_emc_s_dz[1][6][0]=0.267725; p2_emc_s_dz[1][6][0]=-0.010012; p3_emc_s_dz[1][6][0]=0.0943018; p4_emc_s_dz[1][6][0]=-0.0191763; 
    p0_emc_m_dz[1][7][0]=3.47012; p1_emc_m_dz[1][7][0]=-3.42849; p2_emc_m_dz[1][7][0]=3.20143; p3_emc_m_dz[1][7][0]=-1.4532; p4_emc_m_dz[1][7][0]=0.220503; 
    p0_emc_s_dz[1][7][0]=2.42374; p1_emc_s_dz[1][7][0]=1.1533; p2_emc_s_dz[1][7][0]=-0.844832; p3_emc_s_dz[1][7][0]=0.395203; p4_emc_s_dz[1][7][0]=-0.0574619; 
    p0_emc_m_dz[1][8][0]=4.26579; p1_emc_m_dz[1][8][0]=-4.10332; p2_emc_m_dz[1][8][0]=3.68918; p3_emc_m_dz[1][8][0]=-1.64114; p4_emc_m_dz[1][8][0]=0.246539; 
    p0_emc_s_dz[1][8][0]=3.11114; p1_emc_s_dz[1][8][0]=0.0153341; p2_emc_s_dz[1][8][0]=-0.0374315; p3_emc_s_dz[1][8][0]=0.159582; p4_emc_s_dz[1][8][0]=-0.034758; 
    p0_emc_m_dz[1][9][0]=5.12717; p1_emc_m_dz[1][9][0]=-5.67; p2_emc_m_dz[1][9][0]=5.09681; p3_emc_m_dz[1][9][0]=-2.20746; p4_emc_m_dz[1][9][0]=0.340814; 
    p0_emc_s_dz[1][9][0]=3.46005; p1_emc_s_dz[1][9][0]=-1.02415; p2_emc_s_dz[1][9][0]=1.33197; p3_emc_s_dz[1][9][0]=-0.569321; p4_emc_s_dz[1][9][0]=0.0980877; 
    p0_emc_m_dz[2][0][0]=-1.13129; p1_emc_m_dz[2][0][0]=3.09939; p2_emc_m_dz[2][0][0]=-2.40918; p3_emc_m_dz[2][0][0]=1.14419; p4_emc_m_dz[2][0][0]=-0.202691; 
    p0_emc_s_dz[2][0][0]=3.33543; p1_emc_s_dz[2][0][0]=-1.02948; p2_emc_s_dz[2][0][0]=1.3142; p3_emc_s_dz[2][0][0]=-0.508732; p4_emc_s_dz[2][0][0]=0.084982; 
    p0_emc_m_dz[2][1][0]=-1.01579; p1_emc_m_dz[2][1][0]=3.74657; p2_emc_m_dz[2][1][0]=-3.37807; p3_emc_m_dz[2][1][0]=1.57922; p4_emc_m_dz[2][1][0]=-0.251736; 
    p0_emc_s_dz[2][1][0]=2.94131; p1_emc_s_dz[2][1][0]=0.0467551; p2_emc_s_dz[2][1][0]=0.117246; p3_emc_s_dz[2][1][0]=0.0585689; p4_emc_s_dz[2][1][0]=-0.0134452; 
    p0_emc_m_dz[2][2][0]=0.265347; p1_emc_m_dz[2][2][0]=1.18525; p2_emc_m_dz[2][2][0]=-0.785528; p3_emc_m_dz[2][2][0]=0.426177; p4_emc_m_dz[2][2][0]=-0.0735614; 
    p0_emc_s_dz[2][2][0]=2.42633; p1_emc_s_dz[2][2][0]=1.16948; p2_emc_s_dz[2][2][0]=-0.961576; p3_emc_s_dz[2][2][0]=0.507543; p4_emc_s_dz[2][2][0]=-0.080274; 
    p0_emc_m_dz[2][3][0]=0.758657; p1_emc_m_dz[2][3][0]=1.81803; p2_emc_m_dz[2][3][0]=-1.81429; p3_emc_m_dz[2][3][0]=0.876725; p4_emc_m_dz[2][3][0]=-0.140559; 
    p0_emc_s_dz[2][3][0]=2.35614; p1_emc_s_dz[2][3][0]=0.733887; p2_emc_s_dz[2][3][0]=-0.414106; p3_emc_s_dz[2][3][0]=0.256115; p4_emc_s_dz[2][3][0]=-0.0392357; 
    p0_emc_m_dz[2][4][0]=1.90186; p1_emc_m_dz[2][4][0]=-0.363085; p2_emc_m_dz[2][4][0]=0.486335; p3_emc_m_dz[2][4][0]=-0.204241; p4_emc_m_dz[2][4][0]=0.0311325; 
    p0_emc_s_dz[2][4][0]=1.99714; p1_emc_s_dz[2][4][0]=1.39078; p2_emc_s_dz[2][4][0]=-1.0232; p3_emc_s_dz[2][4][0]=0.526052; p4_emc_s_dz[2][4][0]=-0.0820878; 
    p0_emc_m_dz[2][5][0]=2.62408; p1_emc_m_dz[2][5][0]=-0.45287; p2_emc_m_dz[2][5][0]=0.477115; p3_emc_m_dz[2][5][0]=-0.252331; p4_emc_m_dz[2][5][0]=0.039675; 
    p0_emc_s_dz[2][5][0]=2.43597; p1_emc_s_dz[2][5][0]=0.187497; p2_emc_s_dz[2][5][0]=0.141661; p3_emc_s_dz[2][5][0]=0.0541156; p4_emc_s_dz[2][5][0]=-0.0139176; 
    p0_emc_m_dz[2][6][0]=3.69551; p1_emc_m_dz[2][6][0]=-2.19802; p2_emc_m_dz[2][6][0]=2.1181; p3_emc_m_dz[2][6][0]=-0.970048; p4_emc_m_dz[2][6][0]=0.148195; 
    p0_emc_s_dz[2][6][0]=2.23328; p1_emc_s_dz[2][6][0]=0.986969; p2_emc_s_dz[2][6][0]=-0.860274; p3_emc_s_dz[2][6][0]=0.513229; p4_emc_s_dz[2][6][0]=-0.0848817; 
    p0_emc_m_dz[2][7][0]=4.43896; p1_emc_m_dz[2][7][0]=-2.77429; p2_emc_m_dz[2][7][0]=2.42458; p3_emc_m_dz[2][7][0]=-1.09129; p4_emc_m_dz[2][7][0]=0.166044; 
    p0_emc_s_dz[2][7][0]=2.48313; p1_emc_s_dz[2][7][0]=0.373238; p2_emc_s_dz[2][7][0]=-0.0993806; p3_emc_s_dz[2][7][0]=0.124214; p4_emc_s_dz[2][7][0]=-0.0195741; 
    p0_emc_m_dz[2][8][0]=5.20855; p1_emc_m_dz[2][8][0]=-3.99638; p2_emc_m_dz[2][8][0]=3.54824; p3_emc_m_dz[2][8][0]=-1.59926; p4_emc_m_dz[2][8][0]=0.248382; 
    p0_emc_s_dz[2][8][0]=3.02627; p1_emc_s_dz[2][8][0]=-0.568274; p2_emc_s_dz[2][8][0]=0.797185; p3_emc_s_dz[2][8][0]=-0.262358; p4_emc_s_dz[2][8][0]=0.0385182; 
    p0_emc_m_dz[2][9][0]=5.42027; p1_emc_m_dz[2][9][0]=-3.76923; p2_emc_m_dz[2][9][0]=2.91389; p3_emc_m_dz[2][9][0]=-1.26537; p4_emc_m_dz[2][9][0]=0.207873; 
    p0_emc_s_dz[2][9][0]=3.36891; p1_emc_s_dz[2][9][0]=-1.02713; p2_emc_s_dz[2][9][0]=1.28699; p3_emc_s_dz[2][9][0]=-0.542788; p4_emc_s_dz[2][9][0]=0.0970416; 
	
    //****** FitZedParam ******** PosCharge for 27 GeV
    //Writing the data out for Pos 
	
    p0_emc_m_dz[0][0][1]=-1.99616; p1_emc_m_dz[0][0][1]=7.24041; p2_emc_m_dz[0][0][1]=-6.77524; p3_emc_m_dz[0][0][1]=3.03436; p4_emc_m_dz[0][0][1]=-0.477129; 
    p0_emc_s_dz[0][0][1]=3.2683; p1_emc_s_dz[0][0][1]=-2.59389; p2_emc_s_dz[0][0][1]=3.41382; p3_emc_s_dz[0][0][1]=-1.66622; p4_emc_s_dz[0][0][1]=0.308109; 
    p0_emc_m_dz[0][1][1]=-1.29063; p1_emc_m_dz[0][1][1]=5.49883; p2_emc_m_dz[0][1][1]=-5.17444; p3_emc_m_dz[0][1][1]=2.35379; p4_emc_m_dz[0][1][1]=-0.362916; 
    p0_emc_s_dz[0][1][1]=2.88227; p1_emc_s_dz[0][1][1]=-1.70786; p2_emc_s_dz[0][1][1]=2.23404; p3_emc_s_dz[0][1][1]=-0.859323; p4_emc_s_dz[0][1][1]=0.124456; 
    p0_emc_m_dz[0][2][1]=-0.802278; p1_emc_m_dz[0][2][1]=4.11242; p2_emc_m_dz[0][2][1]=-3.5766; p3_emc_m_dz[0][2][1]=1.52713; p4_emc_m_dz[0][2][1]=-0.225159; 
    p0_emc_s_dz[0][2][1]=2.57921; p1_emc_s_dz[0][2][1]=-1.06728; p2_emc_s_dz[0][2][1]=1.45301; p3_emc_s_dz[0][2][1]=-0.460806; p4_emc_s_dz[0][2][1]=0.0516631; 
    p0_emc_m_dz[0][3][1]=-0.0129752; p1_emc_m_dz[0][3][1]=2.32962; p2_emc_m_dz[0][3][1]=-1.9923; p3_emc_m_dz[0][3][1]=0.816944; p4_emc_m_dz[0][3][1]=-0.112648; 
    p0_emc_s_dz[0][3][1]=2.32081; p1_emc_s_dz[0][3][1]=-0.988339; p2_emc_s_dz[0][3][1]=1.56489; p3_emc_s_dz[0][3][1]=-0.533264; p4_emc_s_dz[0][3][1]=0.0688392; 
    p0_emc_m_dz[0][4][1]=0.985146; p1_emc_m_dz[0][4][1]=-0.149973; p2_emc_m_dz[0][4][1]=0.241147; p3_emc_m_dz[0][4][1]=-0.08109; p4_emc_m_dz[0][4][1]=0.00848766; 
    p0_emc_s_dz[0][4][1]=1.89331; p1_emc_s_dz[0][4][1]=0.158403; p2_emc_s_dz[0][4][1]=0.358149; p3_emc_s_dz[0][4][1]=-0.0130077; p4_emc_s_dz[0][4][1]=-0.00903976; 
    p0_emc_m_dz[0][5][1]=1.08918; p1_emc_m_dz[0][5][1]=-0.797149; p2_emc_m_dz[0][5][1]=0.617766; p3_emc_m_dz[0][5][1]=-0.270011; p4_emc_m_dz[0][5][1]=0.0379156; 
    p0_emc_s_dz[0][5][1]=2.40882; p1_emc_s_dz[0][5][1]=-1.36383; p2_emc_s_dz[0][5][1]=1.96994; p3_emc_s_dz[0][5][1]=-0.689065; p4_emc_s_dz[0][5][1]=0.087622; 
    p0_emc_m_dz[0][6][1]=2.19694; p1_emc_m_dz[0][6][1]=-4.00034; p2_emc_m_dz[0][6][1]=3.89672; p3_emc_m_dz[0][6][1]=-1.72097; p4_emc_m_dz[0][6][1]=0.255425; 
    p0_emc_s_dz[0][6][1]=2.44563; p1_emc_s_dz[0][6][1]=-1.12191; p2_emc_s_dz[0][6][1]=1.60295; p3_emc_s_dz[0][6][1]=-0.543533; p4_emc_s_dz[0][6][1]=0.0694693; 
    p0_emc_m_dz[0][7][1]=2.67966; p1_emc_m_dz[0][7][1]=-4.86165; p2_emc_m_dz[0][7][1]=4.55693; p3_emc_m_dz[0][7][1]=-2.01232; p4_emc_m_dz[0][7][1]=0.298025; 
    p0_emc_s_dz[0][7][1]=2.59388; p1_emc_s_dz[0][7][1]=-0.940313; p2_emc_s_dz[0][7][1]=1.21543; p3_emc_s_dz[0][7][1]=-0.326841; p4_emc_s_dz[0][7][1]=0.0301249; 
    p0_emc_m_dz[0][8][1]=3.24045; p1_emc_m_dz[0][8][1]=-6.06809; p2_emc_m_dz[0][8][1]=5.52102; p3_emc_m_dz[0][8][1]=-2.3991; p4_emc_m_dz[0][8][1]=0.351331; 
    p0_emc_s_dz[0][8][1]=3.06588; p1_emc_s_dz[0][8][1]=-1.91821; p2_emc_s_dz[0][8][1]=2.13265; p3_emc_s_dz[0][8][1]=-0.716035; p4_emc_s_dz[0][8][1]=0.0887763; 
    p0_emc_m_dz[0][9][1]=3.66272; p1_emc_m_dz[0][9][1]=-6.95532; p2_emc_m_dz[0][9][1]=6.11746; p3_emc_m_dz[0][9][1]=-2.60887; p4_emc_m_dz[0][9][1]=0.394329; 
    p0_emc_s_dz[0][9][1]=3.84543; p1_emc_s_dz[0][9][1]=-4.21117; p2_emc_s_dz[0][9][1]=4.84616; p3_emc_s_dz[0][9][1]=-2.06932; p4_emc_s_dz[0][9][1]=0.322241; 
    p0_emc_m_dz[1][0][1]=-2.8582; p1_emc_m_dz[1][0][1]=2.60276; p2_emc_m_dz[1][0][1]=-0.818736; p3_emc_m_dz[1][0][1]=0.132427; p4_emc_m_dz[1][0][1]=-0.0221507; 
    p0_emc_s_dz[1][0][1]=3.69977; p1_emc_s_dz[1][0][1]=-3.58033; p2_emc_s_dz[1][0][1]=4.20497; p3_emc_s_dz[1][0][1]=-1.75566; p4_emc_s_dz[1][0][1]=0.265858; 
    p0_emc_m_dz[1][1][1]=-2.53245; p1_emc_m_dz[1][1][1]=2.95585; p2_emc_m_dz[1][1][1]=-1.78811; p3_emc_m_dz[1][1][1]=0.659606; p4_emc_m_dz[1][1][1]=-0.0934632; 
    p0_emc_s_dz[1][1][1]=2.95847; p1_emc_s_dz[1][1][1]=-1.35041; p2_emc_s_dz[1][1][1]=1.63916; p3_emc_s_dz[1][1][1]=-0.519588; p4_emc_s_dz[1][1][1]=0.0561675; 
    p0_emc_m_dz[1][2][1]=-1.43374; p1_emc_m_dz[1][2][1]=1.12917; p2_emc_m_dz[1][2][1]=-0.168847; p3_emc_m_dz[1][2][1]=-0.0308974; p4_emc_m_dz[1][2][1]=0.0140043; 
    p0_emc_s_dz[1][2][1]=2.98179; p1_emc_s_dz[1][2][1]=-1.60756; p2_emc_s_dz[1][2][1]=1.81798; p3_emc_s_dz[1][2][1]=-0.562816; p4_emc_s_dz[1][2][1]=0.0595495; 
    p0_emc_m_dz[1][3][1]=-0.537687; p1_emc_m_dz[1][3][1]=0.536486; p2_emc_m_dz[1][3][1]=0.0407282; p3_emc_m_dz[1][3][1]=-0.0797501; p4_emc_m_dz[1][3][1]=0.017785; 
    p0_emc_s_dz[1][3][1]=2.79969; p1_emc_s_dz[1][3][1]=-1.42307; p2_emc_s_dz[1][3][1]=1.75047; p3_emc_s_dz[1][3][1]=-0.565838; p4_emc_s_dz[1][3][1]=0.0634297; 
    p0_emc_m_dz[1][4][1]=0.0850359; p1_emc_m_dz[1][4][1]=0.618745; p2_emc_m_dz[1][4][1]=-0.414572; p3_emc_m_dz[1][4][1]=0.147066; p4_emc_m_dz[1][4][1]=-0.0184661; 
    p0_emc_s_dz[1][4][1]=2.97391; p1_emc_s_dz[1][4][1]=-1.83977; p2_emc_s_dz[1][4][1]=2.16961; p3_emc_s_dz[1][4][1]=-0.734456; p4_emc_s_dz[1][4][1]=0.0885168; 
    p0_emc_m_dz[1][5][1]=1.32228; p1_emc_m_dz[1][5][1]=-0.310333; p2_emc_m_dz[1][5][1]=0.071143; p3_emc_m_dz[1][5][1]=-0.0185553; p4_emc_m_dz[1][5][1]=0.00133864; 
    p0_emc_s_dz[1][5][1]=2.97014; p1_emc_s_dz[1][5][1]=-1.62911; p2_emc_s_dz[1][5][1]=1.88652; p3_emc_s_dz[1][5][1]=-0.577437; p4_emc_s_dz[1][5][1]=0.0592645; 
    p0_emc_m_dz[1][6][1]=1.77537; p1_emc_m_dz[1][6][1]=0.376979; p2_emc_m_dz[1][6][1]=-1.04799; p3_emc_m_dz[1][6][1]=0.498004; p4_emc_m_dz[1][6][1]=-0.0791151; 
    p0_emc_s_dz[1][6][1]=2.93754; p1_emc_s_dz[1][6][1]=-1.28194; p2_emc_s_dz[1][6][1]=1.53325; p3_emc_s_dz[1][6][1]=-0.440478; p4_emc_s_dz[1][6][1]=0.040111; 
    p0_emc_m_dz[1][7][1]=2.70169; p1_emc_m_dz[1][7][1]=-0.198827; p2_emc_m_dz[1][7][1]=-0.909124; p3_emc_m_dz[1][7][1]=0.470589; p4_emc_m_dz[1][7][1]=-0.0752824; 
    p0_emc_s_dz[1][7][1]=3.12987; p1_emc_s_dz[1][7][1]=-1.25057; p2_emc_s_dz[1][7][1]=1.47199; p3_emc_s_dz[1][7][1]=-0.422529; p4_emc_s_dz[1][7][1]=0.0375921; 
    p0_emc_m_dz[1][8][1]=3.48872; p1_emc_m_dz[1][8][1]=-0.792382; p2_emc_m_dz[1][8][1]=-0.560114; p3_emc_m_dz[1][8][1]=0.300098; p4_emc_m_dz[1][8][1]=-0.0422807; 
    p0_emc_s_dz[1][8][1]=3.33529; p1_emc_s_dz[1][8][1]=-1.5244; p2_emc_s_dz[1][8][1]=1.83738; p3_emc_s_dz[1][8][1]=-0.597474; p4_emc_s_dz[1][8][1]=0.0643339; 
    p0_emc_m_dz[1][9][1]=3.78544; p1_emc_m_dz[1][9][1]=-0.459552; p2_emc_m_dz[1][9][1]=-1.39402; p3_emc_m_dz[1][9][1]=0.767053; p4_emc_m_dz[1][9][1]=-0.108833; 
    p0_emc_s_dz[1][9][1]=3.67261; p1_emc_s_dz[1][9][1]=-2.47658; p2_emc_s_dz[1][9][1]=3.0186; p3_emc_s_dz[1][9][1]=-1.20993; p4_emc_s_dz[1][9][1]=0.17701; 
    p0_emc_m_dz[2][0][1]=-1.43752; p1_emc_m_dz[2][0][1]=2.39205; p2_emc_m_dz[2][0][1]=-0.361676; p3_emc_m_dz[2][0][1]=-0.12999; p4_emc_m_dz[2][0][1]=0.0208237; 
    p0_emc_s_dz[2][0][1]=3.15047; p1_emc_s_dz[2][0][1]=-1.44774; p2_emc_s_dz[2][0][1]=2.22614; p3_emc_s_dz[2][0][1]=-0.910846; p4_emc_s_dz[2][0][1]=0.134329; 
    p0_emc_m_dz[2][1][1]=-0.515755; p1_emc_m_dz[2][1][1]=0.742796; p2_emc_m_dz[2][1][1]=0.968927; p3_emc_m_dz[2][1][1]=-0.647461; p4_emc_m_dz[2][1][1]=0.110077; 
    p0_emc_s_dz[2][1][1]=3.5934; p1_emc_s_dz[2][1][1]=-2.7304; p2_emc_s_dz[2][1][1]=3.20114; p3_emc_s_dz[2][1][1]=-1.16332; p4_emc_s_dz[2][1][1]=0.144804; 
    p0_emc_m_dz[2][2][1]=0.047204; p1_emc_m_dz[2][2][1]=0.898838; p2_emc_m_dz[2][2][1]=0.359956; p3_emc_m_dz[2][2][1]=-0.338976; p4_emc_m_dz[2][2][1]=0.0642772; 
    p0_emc_s_dz[2][2][1]=3.20914; p1_emc_s_dz[2][2][1]=-1.89634; p2_emc_s_dz[2][2][1]=2.33055; p3_emc_s_dz[2][2][1]=-0.803218; p4_emc_s_dz[2][2][1]=0.0955512; 
    p0_emc_m_dz[2][3][1]=0.94171; p1_emc_m_dz[2][3][1]=0.445476; p2_emc_m_dz[2][3][1]=0.327855; p3_emc_m_dz[2][3][1]=-0.262322; p4_emc_m_dz[2][3][1]=0.0480526; 
    p0_emc_s_dz[2][3][1]=2.95296; p1_emc_s_dz[2][3][1]=-1.54626; p2_emc_s_dz[2][3][1]=1.88945; p3_emc_s_dz[2][3][1]=-0.566231; p4_emc_s_dz[2][3][1]=0.0551828; 
    p0_emc_m_dz[2][4][1]=1.73116; p1_emc_m_dz[2][4][1]=0.0203349; p2_emc_m_dz[2][4][1]=0.362667; p3_emc_m_dz[2][4][1]=-0.253132; p4_emc_m_dz[2][4][1]=0.0477353; 
    p0_emc_s_dz[2][4][1]=2.76564; p1_emc_s_dz[2][4][1]=-1.30748; p2_emc_s_dz[2][4][1]=1.65469; p3_emc_s_dz[2][4][1]=-0.44095; p4_emc_s_dz[2][4][1]=0.0331798; 
    p0_emc_m_dz[2][5][1]=2.27461; p1_emc_m_dz[2][5][1]=0.93514; p2_emc_m_dz[2][5][1]=-1.09746; p3_emc_m_dz[2][5][1]=0.418295; p4_emc_m_dz[2][5][1]=-0.0555888; 
    p0_emc_s_dz[2][5][1]=2.52524; p1_emc_s_dz[2][5][1]=-0.601443; p2_emc_s_dz[2][5][1]=0.899526; p3_emc_s_dz[2][5][1]=-0.127067; p4_emc_s_dz[2][5][1]=-0.0116234; 
    p0_emc_m_dz[2][6][1]=3.31919; p1_emc_m_dz[2][6][1]=-0.417587; p2_emc_m_dz[2][6][1]=-0.0644821; p3_emc_m_dz[2][6][1]=0.0377413; p4_emc_m_dz[2][6][1]=-0.0052884; 
    p0_emc_s_dz[2][6][1]=2.77621; p1_emc_s_dz[2][6][1]=-1.46471; p2_emc_s_dz[2][6][1]=1.85416; p3_emc_s_dz[2][6][1]=-0.560028; p4_emc_s_dz[2][6][1]=0.0545092; 
    p0_emc_m_dz[2][7][1]=4.07149; p1_emc_m_dz[2][7][1]=-0.776351; p2_emc_m_dz[2][7][1]=-0.0486838; p3_emc_m_dz[2][7][1]=0.0704795; p4_emc_m_dz[2][7][1]=-0.0130758; 
    p0_emc_s_dz[2][7][1]=2.90775; p1_emc_s_dz[2][7][1]=-1.72279; p2_emc_s_dz[2][7][1]=2.25848; p3_emc_s_dz[2][7][1]=-0.792802; p4_emc_s_dz[2][7][1]=0.0959127; 
    p0_emc_m_dz[2][8][1]=4.63143; p1_emc_m_dz[2][8][1]=-1.00778; p2_emc_m_dz[2][8][1]=-0.0664923; p3_emc_m_dz[2][8][1]=0.08221; p4_emc_m_dz[2][8][1]=-0.0105185; 
    p0_emc_s_dz[2][8][1]=3.25572; p1_emc_s_dz[2][8][1]=-2.13251; p2_emc_s_dz[2][8][1]=2.54099; p3_emc_s_dz[2][8][1]=-0.882167; p4_emc_s_dz[2][8][1]=0.10536; 
    p0_emc_m_dz[2][9][1]=5.26173; p1_emc_m_dz[2][9][1]=-1.89828; p2_emc_m_dz[2][9][1]=0.433329; p3_emc_m_dz[2][9][1]=-0.0382856; p4_emc_m_dz[2][9][1]=0.0123295; 
    p0_emc_s_dz[2][9][1]=3.95901; p1_emc_s_dz[2][9][1]=-3.95429; p2_emc_s_dz[2][9][1]=4.54903; p3_emc_s_dz[2][9][1]=-1.81277; p4_emc_s_dz[2][9][1]=0.25913; 
    
    //EMC DPHI NEG 
    
    p0_emc_m_dp[0][0][0]=0.00245444; p1_emc_m_dp[0][0][0]=0.00475237; p2_emc_m_dp[0][0][0]=-0.00370114; p3_emc_m_dp[0][0][0]=0.00252025; p4_emc_m_dp[0][0][0]=-0.000726919; 
    p0_emc_s_dp[0][0][0]=0.00310468; p1_emc_s_dp[0][0][0]=0.006543; p2_emc_s_dp[0][0][0]=-0.00877788; p3_emc_s_dp[0][0][0]=0.00470149; p4_emc_s_dp[0][0][0]=-0.000751822; 
    p0_emc_m_dp[0][1][0]=0.00116799; p1_emc_m_dp[0][1][0]=0.00941669; p2_emc_m_dp[0][1][0]=-0.0090567; p3_emc_m_dp[0][1][0]=0.00544805; p4_emc_m_dp[0][1][0]=-0.00118197; 
    p0_emc_s_dp[0][1][0]=0.00419535; p1_emc_s_dp[0][1][0]=0.0017634; p2_emc_s_dp[0][1][0]=-0.0023299; p3_emc_s_dp[0][1][0]=0.00141067; p4_emc_s_dp[0][1][0]=-0.000193754; 
    p0_emc_m_dp[0][2][0]=0.00260815; p1_emc_m_dp[0][2][0]=0.00521063; p2_emc_m_dp[0][2][0]=-0.00375676; p3_emc_m_dp[0][2][0]=0.00286181; p4_emc_m_dp[0][2][0]=-0.00067093; 
    p0_emc_s_dp[0][2][0]=0.00531393; p1_emc_s_dp[0][2][0]=-0.00255083; p2_emc_s_dp[0][2][0]=0.00281608; p3_emc_s_dp[0][2][0]=-0.000858991; p4_emc_s_dp[0][2][0]=0.000117559; 
    p0_emc_m_dp[0][3][0]=0.00397768; p1_emc_m_dp[0][3][0]=0.0013639; p2_emc_m_dp[0][3][0]=0.000865638; p3_emc_m_dp[0][3][0]=0.000651001; p4_emc_m_dp[0][3][0]=-0.000264705; 
    p0_emc_s_dp[0][3][0]=0.00614371; p1_emc_s_dp[0][3][0]=-0.00550203; p2_emc_s_dp[0][3][0]=0.00653562; p3_emc_s_dp[0][3][0]=-0.00260134; p4_emc_s_dp[0][3][0]=0.000376342; 
    p0_emc_m_dp[0][4][0]=0.00559157; p1_emc_m_dp[0][4][0]=-0.00424018; p2_emc_m_dp[0][4][0]=0.00818356; p3_emc_m_dp[0][4][0]=-0.00311341; p4_emc_m_dp[0][4][0]=0.000417593; 
    p0_emc_s_dp[0][4][0]=0.00481896; p1_emc_s_dp[0][4][0]=-0.00176037; p2_emc_s_dp[0][4][0]=0.00283293; p3_emc_s_dp[0][4][0]=-0.000902147; p4_emc_s_dp[0][4][0]=8.2009e-05; 
    p0_emc_m_dp[0][5][0]=0.00283616; p1_emc_m_dp[0][5][0]=0.00133789; p2_emc_m_dp[0][5][0]=0.00260418; p3_emc_m_dp[0][5][0]=-0.000556105; p4_emc_m_dp[0][5][0]=-2.68505e-05; 
    p0_emc_s_dp[0][5][0]=0.00920096; p1_emc_s_dp[0][5][0]=-0.0143336; p2_emc_s_dp[0][5][0]=0.0159021; p3_emc_s_dp[0][5][0]=-0.00679807; p4_emc_s_dp[0][5][0]=0.00106062; 
    p0_emc_m_dp[0][6][0]=0.00336441; p1_emc_m_dp[0][6][0]=-0.00100605; p2_emc_m_dp[0][6][0]=0.00548835; p3_emc_m_dp[0][6][0]=-0.00216983; p4_emc_m_dp[0][6][0]=0.000281992; 
    p0_emc_s_dp[0][6][0]=0.00807139; p1_emc_s_dp[0][6][0]=-0.0106084; p2_emc_s_dp[0][6][0]=0.0113054; p3_emc_s_dp[0][6][0]=-0.00451818; p4_emc_s_dp[0][6][0]=0.000661402; 
    p0_emc_m_dp[0][7][0]=0.00239964; p1_emc_m_dp[0][7][0]=0.00181629; p2_emc_m_dp[0][7][0]=0.00197109; p3_emc_m_dp[0][7][0]=-0.000443961; p4_emc_m_dp[0][7][0]=-5.45431e-05; 
    p0_emc_s_dp[0][7][0]=0.00727629; p1_emc_s_dp[0][7][0]=-0.00719654; p2_emc_s_dp[0][7][0]=0.00677166; p3_emc_s_dp[0][7][0]=-0.00229115; p4_emc_s_dp[0][7][0]=0.000303676; 
    p0_emc_m_dp[0][8][0]=0.00134442; p1_emc_m_dp[0][8][0]=0.004676; p2_emc_m_dp[0][8][0]=-0.00130501; p3_emc_m_dp[0][8][0]=0.000866057; p4_emc_m_dp[0][8][0]=-0.000287303; 
    p0_emc_s_dp[0][8][0]=0.00599196; p1_emc_s_dp[0][8][0]=-0.00260207; p2_emc_s_dp[0][8][0]=0.0013563; p3_emc_s_dp[0][8][0]=0.000197017; p4_emc_s_dp[0][8][0]=-7.4117e-05; 
    p0_emc_m_dp[0][9][0]=0.000911353; p1_emc_m_dp[0][9][0]=0.00571871; p2_emc_m_dp[0][9][0]=-0.00263134; p3_emc_m_dp[0][9][0]=0.00114813; p4_emc_m_dp[0][9][0]=-0.000365077; 
    p0_emc_s_dp[0][9][0]=0.00549137; p1_emc_s_dp[0][9][0]=-0.00085672; p2_emc_s_dp[0][9][0]=-0.000626822; p3_emc_s_dp[0][9][0]=0.00105749; p4_emc_s_dp[0][9][0]=-0.000197786; 
    p0_emc_m_dp[1][0][0]=0.00160435; p1_emc_m_dp[1][0][0]=0.00282223; p2_emc_m_dp[1][0][0]=-0.000965598; p3_emc_m_dp[1][0][0]=6.66745e-05; p4_emc_m_dp[1][0][0]=-0.000132368; 
    p0_emc_s_dp[1][0][0]=0.00684853; p1_emc_s_dp[1][0][0]=-0.00448657; p2_emc_s_dp[1][0][0]=0.00468213; p3_emc_s_dp[1][0][0]=-0.00194361; p4_emc_s_dp[1][0][0]=0.000374535; 
    p0_emc_m_dp[1][1][0]=0.00213621; p1_emc_m_dp[1][1][0]=0.00192426; p2_emc_m_dp[1][1][0]=-0.000684161; p3_emc_m_dp[1][1][0]=0.000710307; p4_emc_m_dp[1][1][0]=-0.000269121; 
    p0_emc_s_dp[1][1][0]=0.00742839; p1_emc_s_dp[1][1][0]=-0.00575183; p2_emc_s_dp[1][1][0]=0.00540387; p3_emc_s_dp[1][1][0]=-0.00190588; p4_emc_s_dp[1][1][0]=0.000292637; 
    p0_emc_m_dp[1][2][0]=0.00187693; p1_emc_m_dp[1][2][0]=0.00308275; p2_emc_m_dp[1][2][0]=-0.00178388; p3_emc_m_dp[1][2][0]=0.00142027; p4_emc_m_dp[1][2][0]=-0.000371182; 
    p0_emc_s_dp[1][2][0]=0.00697565; p1_emc_s_dp[1][2][0]=-0.00335613; p2_emc_s_dp[1][2][0]=0.001428; p3_emc_s_dp[1][2][0]=0.000661452; p4_emc_s_dp[1][2][0]=-0.000265762; 
    p0_emc_m_dp[1][3][0]=0.00256341; p1_emc_m_dp[1][3][0]=0.000726696; p2_emc_m_dp[1][3][0]=0.00209763; p3_emc_m_dp[1][3][0]=-0.000836712; p4_emc_m_dp[1][3][0]=9.44354e-05; 
    p0_emc_s_dp[1][3][0]=0.00886664; p1_emc_s_dp[1][3][0]=-0.00995878; p2_emc_s_dp[1][3][0]=0.00984672; p3_emc_s_dp[1][3][0]=-0.00379846; p4_emc_s_dp[1][3][0]=0.000557952; 
    p0_emc_m_dp[1][4][0]=0.0020564; p1_emc_m_dp[1][4][0]=0.00309553; p2_emc_m_dp[1][4][0]=-0.000905124; p3_emc_m_dp[1][4][0]=0.000808181; p4_emc_m_dp[1][4][0]=-0.00020745; 
    p0_emc_s_dp[1][4][0]=0.0109258; p1_emc_s_dp[1][4][0]=-0.0163433; p2_emc_s_dp[1][4][0]=0.017; p3_emc_s_dp[1][4][0]=-0.0071405; p4_emc_s_dp[1][4][0]=0.00111401; 
    p0_emc_m_dp[1][5][0]=0.0022; p1_emc_m_dp[1][5][0]=0.00256412; p2_emc_m_dp[1][5][0]=0.000436592; p3_emc_m_dp[1][5][0]=-0.000214044; p4_emc_m_dp[1][5][0]=2.77811e-05; 
    p0_emc_s_dp[1][5][0]=0.00966554; p1_emc_s_dp[1][5][0]=-0.0119055; p2_emc_s_dp[1][5][0]=0.0116777; p3_emc_s_dp[1][5][0]=-0.00447554; p4_emc_s_dp[1][5][0]=0.000637372; 
    p0_emc_m_dp[1][6][0]=0.00271853; p1_emc_m_dp[1][6][0]=0.00093518; p2_emc_m_dp[1][6][0]=0.0021085; p3_emc_m_dp[1][6][0]=-0.000908834; p4_emc_m_dp[1][6][0]=0.000111114; 
    p0_emc_s_dp[1][6][0]=0.00902182; p1_emc_s_dp[1][6][0]=-0.0102126; p2_emc_s_dp[1][6][0]=0.00997672; p3_emc_s_dp[1][6][0]=-0.00384743; p4_emc_s_dp[1][6][0]=0.000568501; 
    p0_emc_m_dp[1][7][0]=0.00254622; p1_emc_m_dp[1][7][0]=0.00156255; p2_emc_m_dp[1][7][0]=0.00118299; p3_emc_m_dp[1][7][0]=-0.000520611; p4_emc_m_dp[1][7][0]=2.37582e-05; 
    p0_emc_s_dp[1][7][0]=0.00852017; p1_emc_s_dp[1][7][0]=-0.00863329; p2_emc_s_dp[1][7][0]=0.00837564; p3_emc_s_dp[1][7][0]=-0.00324005; p4_emc_s_dp[1][7][0]=0.000500545; 
    p0_emc_m_dp[1][8][0]=0.00220409; p1_emc_m_dp[1][8][0]=0.0036719; p2_emc_m_dp[1][8][0]=-0.0020623; p3_emc_m_dp[1][8][0]=0.00112677; p4_emc_m_dp[1][8][0]=-0.000321104; 
    p0_emc_s_dp[1][8][0]=0.00610134; p1_emc_s_dp[1][8][0]=-0.00071198; p2_emc_s_dp[1][8][0]=-0.000743265; p3_emc_s_dp[1][8][0]=0.0010704; p4_emc_s_dp[1][8][0]=-0.000206578; 
    p0_emc_m_dp[1][9][0]=0.00233193; p1_emc_m_dp[1][9][0]=0.00323346; p2_emc_m_dp[1][9][0]=-0.00136081; p3_emc_m_dp[1][9][0]=0.000262296; p4_emc_m_dp[1][9][0]=-0.000181389; 
    p0_emc_s_dp[1][9][0]=0.00742544; p1_emc_s_dp[1][9][0]=-0.00607846; p2_emc_s_dp[1][9][0]=0.00653626; p3_emc_s_dp[1][9][0]=-0.00292479; p4_emc_s_dp[1][9][0]=0.000560246; 
    p0_emc_m_dp[2][0][0]=-0.00173983; p1_emc_m_dp[2][0][0]=0.00255549; p2_emc_m_dp[2][0][0]=-0.000797599; p3_emc_m_dp[2][0][0]=4.61827e-05; p4_emc_m_dp[2][0][0]=-0.000157805; 
    p0_emc_s_dp[2][0][0]=0.00839297; p1_emc_s_dp[2][0][0]=-0.00734458; p2_emc_s_dp[2][0][0]=0.00885346; p3_emc_s_dp[2][0][0]=-0.00428022; p4_emc_s_dp[2][0][0]=0.000800166; 
    p0_emc_m_dp[2][1][0]=-0.000970935; p1_emc_m_dp[2][1][0]=0.00086241; p2_emc_m_dp[2][1][0]=0.00132233; p3_emc_m_dp[2][1][0]=-0.000725036; p4_emc_m_dp[2][1][0]=3.10278e-05; 
    p0_emc_s_dp[2][1][0]=0.00922887; p1_emc_s_dp[2][1][0]=-0.0103776; p2_emc_s_dp[2][1][0]=0.0124624; p3_emc_s_dp[2][1][0]=-0.00607835; p4_emc_s_dp[2][1][0]=0.00110992; 
    p0_emc_m_dp[2][2][0]=-0.000328274; p1_emc_m_dp[2][2][0]=-0.00166496; p2_emc_m_dp[2][2][0]=0.0049485; p3_emc_m_dp[2][2][0]=-0.00247494; p4_emc_m_dp[2][2][0]=0.000369113; 
    p0_emc_s_dp[2][2][0]=0.00811527; p1_emc_s_dp[2][2][0]=-0.00672901; p2_emc_s_dp[2][2][0]=0.00793338; p3_emc_s_dp[2][2][0]=-0.00372699; p4_emc_s_dp[2][2][0]=0.000671692; 
    p0_emc_m_dp[2][3][0]=7.96201e-05; p1_emc_m_dp[2][3][0]=-0.00198227; p2_emc_m_dp[2][3][0]=0.00542689; p3_emc_m_dp[2][3][0]=-0.00268332; p4_emc_m_dp[2][3][0]=0.0004221; 
    p0_emc_s_dp[2][3][0]=0.00889243; p1_emc_s_dp[2][3][0]=-0.00943594; p2_emc_s_dp[2][3][0]=0.0110078; p3_emc_s_dp[2][3][0]=-0.00506271; p4_emc_s_dp[2][3][0]=0.000860069; 
    p0_emc_m_dp[2][4][0]=-1.55893e-05; p1_emc_m_dp[2][4][0]=-0.00102085; p2_emc_m_dp[2][4][0]=0.00430226; p3_emc_m_dp[2][4][0]=-0.00202144; p4_emc_m_dp[2][4][0]=0.000299293; 
    p0_emc_s_dp[2][4][0]=0.00778239; p1_emc_s_dp[2][4][0]=-0.00590478; p2_emc_s_dp[2][4][0]=0.00706804; p3_emc_s_dp[2][4][0]=-0.00312438; p4_emc_s_dp[2][4][0]=0.000513327; 
    p0_emc_m_dp[2][5][0]=-0.00192165; p1_emc_m_dp[2][5][0]=0.00354624; p2_emc_m_dp[2][5][0]=-0.00111768; p3_emc_m_dp[2][5][0]=0.000749819; p4_emc_m_dp[2][5][0]=-0.000194759; 
    p0_emc_s_dp[2][5][0]=0.00771181; p1_emc_s_dp[2][5][0]=-0.00599913; p2_emc_s_dp[2][5][0]=0.00748562; p3_emc_s_dp[2][5][0]=-0.00347123; p4_emc_s_dp[2][5][0]=0.000598438; 
    p0_emc_m_dp[2][6][0]=-0.00041926; p1_emc_m_dp[2][6][0]=-0.000894057; p2_emc_m_dp[2][6][0]=0.00333359; p3_emc_m_dp[2][6][0]=-0.00134505; p4_emc_m_dp[2][6][0]=0.000159456; 
    p0_emc_s_dp[2][6][0]=0.00766227; p1_emc_s_dp[2][6][0]=-0.00570683; p2_emc_s_dp[2][6][0]=0.00670053; p3_emc_s_dp[2][6][0]=-0.00299924; p4_emc_s_dp[2][6][0]=0.000514598; 
    p0_emc_m_dp[2][7][0]=-0.00225456; p1_emc_m_dp[2][7][0]=0.00463711; p2_emc_m_dp[2][7][0]=-0.00336147; p3_emc_m_dp[2][7][0]=0.001933; p4_emc_m_dp[2][7][0]=-0.000440184; 
    p0_emc_s_dp[2][7][0]=0.00650804; p1_emc_s_dp[2][7][0]=-0.00192861; p2_emc_s_dp[2][7][0]=0.00221175; p3_emc_s_dp[2][7][0]=-0.000827688; p4_emc_s_dp[2][7][0]=0.000149727; 
    p0_emc_m_dp[2][8][0]=-0.00127916; p1_emc_m_dp[2][8][0]=0.000293253; p2_emc_m_dp[2][8][0]=0.00191889; p3_emc_m_dp[2][8][0]=-0.000978869; p4_emc_m_dp[2][8][0]=7.29247e-05; 
    p0_emc_s_dp[2][8][0]=0.00638444; p1_emc_s_dp[2][8][0]=-0.00148492; p2_emc_s_dp[2][8][0]=0.00143438; p3_emc_s_dp[2][8][0]=-0.000386258; p4_emc_s_dp[2][8][0]=7.80853e-05; 
    p0_emc_m_dp[2][9][0]=-0.00192127; p1_emc_m_dp[2][9][0]=0.00250912; p2_emc_m_dp[2][9][0]=-0.00124898; p3_emc_m_dp[2][9][0]=0.000385575; p4_emc_m_dp[2][9][0]=-0.000215279; 
    p0_emc_s_dp[2][9][0]=0.00662465; p1_emc_s_dp[2][9][0]=-0.00301907; p2_emc_s_dp[2][9][0]=0.00381575; p3_emc_s_dp[2][9][0]=-0.00181504; p4_emc_s_dp[2][9][0]=0.000381296; 
	
    //EMC DPHI POS 
	
    p0_emc_m_dp[0][0][1]=0.00187095; p1_emc_m_dp[0][0][1]=-0.00606491; p2_emc_m_dp[0][0][1]=0.00193728; p3_emc_m_dp[0][0][1]=-0.000245939; p4_emc_m_dp[0][0][1]=0.00011416; 
    p0_emc_s_dp[0][0][1]=0.00604305; p1_emc_s_dp[0][0][1]=-0.00760388; p2_emc_s_dp[0][0][1]=0.0100656; p3_emc_s_dp[0][0][1]=-0.00481601; p4_emc_s_dp[0][0][1]=0.000875317; 
    p0_emc_m_dp[0][1][1]=0.00251806; p1_emc_m_dp[0][1][1]=-0.0104426; p2_emc_m_dp[0][1][1]=0.00934713; p3_emc_m_dp[0][1][1]=-0.00544432; p4_emc_m_dp[0][1][1]=0.00118297; 
    p0_emc_s_dp[0][1][1]=0.00656061; p1_emc_s_dp[0][1][1]=-0.00897024; p2_emc_s_dp[0][1][1]=0.0111262; p3_emc_s_dp[0][1][1]=-0.00500727; p4_emc_s_dp[0][1][1]=0.000850562; 
    p0_emc_m_dp[0][2][1]=0.000687963; p1_emc_m_dp[0][2][1]=-0.00372447; p2_emc_m_dp[0][2][1]=0.000458091; p3_emc_m_dp[0][2][1]=-0.000962831; p4_emc_m_dp[0][2][1]=0.000346922; 
    p0_emc_s_dp[0][2][1]=0.00763262; p1_emc_s_dp[0][2][1]=-0.012973; p2_emc_s_dp[0][2][1]=0.0158467; p3_emc_s_dp[0][2][1]=-0.00704868; p4_emc_s_dp[0][2][1]=0.00111546; 
    p0_emc_m_dp[0][3][1]=0.000755263; p1_emc_m_dp[0][3][1]=-0.00360797; p2_emc_m_dp[0][3][1]=-0.000313494; p3_emc_m_dp[0][3][1]=-0.000410824; p4_emc_m_dp[0][3][1]=0.000184034; 
    p0_emc_s_dp[0][3][1]=0.00878216; p1_emc_s_dp[0][3][1]=-0.0174431; p2_emc_s_dp[0][3][1]=0.0218916; p3_emc_s_dp[0][3][1]=-0.0104357; p4_emc_s_dp[0][3][1]=0.00178147; 
    p0_emc_m_dp[0][4][1]=-0.000605525; p1_emc_m_dp[0][4][1]=0.000419886; p2_emc_m_dp[0][4][1]=-0.00411389; p3_emc_m_dp[0][4][1]=0.000806944; p4_emc_m_dp[0][4][1]=7.4455e-05; 
    p0_emc_s_dp[0][4][1]=0.00963218; p1_emc_s_dp[0][4][1]=-0.021266; p2_emc_s_dp[0][4][1]=0.027832; p3_emc_s_dp[0][4][1]=-0.0140393; p4_emc_s_dp[0][4][1]=0.00251599; 
    p0_emc_m_dp[0][5][1]=-0.00230209; p1_emc_m_dp[0][5][1]=0.00426876; p2_emc_m_dp[0][5][1]=-0.00958289; p3_emc_m_dp[0][5][1]=0.00421647; p4_emc_m_dp[0][5][1]=-0.000636952; 
    p0_emc_s_dp[0][5][1]=0.00868693; p1_emc_s_dp[0][5][1]=-0.0174073; p2_emc_s_dp[0][5][1]=0.0229806; p3_emc_s_dp[0][5][1]=-0.0113237; p4_emc_s_dp[0][5][1]=0.00195469; 
    p0_emc_m_dp[0][6][1]=-0.0016023; p1_emc_m_dp[0][6][1]=0.00132568; p2_emc_m_dp[0][6][1]=-0.00546262; p3_emc_m_dp[0][6][1]=0.00200117; p4_emc_m_dp[0][6][1]=-0.000213892; 
    p0_emc_s_dp[0][6][1]=0.00807691; p1_emc_s_dp[0][6][1]=-0.0149148; p2_emc_s_dp[0][6][1]=0.0194515; p3_emc_s_dp[0][6][1]=-0.00944743; p4_emc_s_dp[0][6][1]=0.00162097; 
    p0_emc_m_dp[0][7][1]=-0.00231107; p1_emc_m_dp[0][7][1]=0.00413325; p2_emc_m_dp[0][7][1]=-0.00854587; p3_emc_m_dp[0][7][1]=0.00358523; p4_emc_m_dp[0][7][1]=-0.000476588; 
    p0_emc_s_dp[0][7][1]=0.00767387; p1_emc_s_dp[0][7][1]=-0.01272; p2_emc_s_dp[0][7][1]=0.0159465; p3_emc_s_dp[0][7][1]=-0.00740859; p4_emc_s_dp[0][7][1]=0.00123279; 
    p0_emc_m_dp[0][8][1]=-0.000824489; p1_emc_m_dp[0][8][1]=-9.92378e-05; p2_emc_m_dp[0][8][1]=-0.00399422; p3_emc_m_dp[0][8][1]=0.00182188; p4_emc_m_dp[0][8][1]=-0.000194645; 
    p0_emc_s_dp[0][8][1]=0.00690237; p1_emc_s_dp[0][8][1]=-0.00996759; p2_emc_s_dp[0][8][1]=0.0125741; p3_emc_s_dp[0][8][1]=-0.00583316; p4_emc_s_dp[0][8][1]=0.000999413; 
    p0_emc_m_dp[0][9][1]=-0.000232448; p1_emc_m_dp[0][9][1]=-0.00140603; p2_emc_m_dp[0][9][1]=-0.0025778; p3_emc_m_dp[0][9][1]=0.00157082; p4_emc_m_dp[0][9][1]=-0.000131108; 
    p0_emc_s_dp[0][9][1]=0.00577732; p1_emc_s_dp[0][9][1]=-0.00564389; p2_emc_s_dp[0][9][1]=0.00704179; p3_emc_s_dp[0][9][1]=-0.0031791; p4_emc_s_dp[0][9][1]=0.000583715; 
    p0_emc_m_dp[1][0][1]=0.000526546; p1_emc_m_dp[1][0][1]=0.00208527; p2_emc_m_dp[1][0][1]=-0.00448757; p3_emc_m_dp[1][0][1]=0.00281492; p4_emc_m_dp[1][0][1]=-0.000420274; 
    p0_emc_s_dp[1][0][1]=0.00524695; p1_emc_s_dp[1][0][1]=0.000647046; p2_emc_s_dp[1][0][1]=-0.00178332; p3_emc_s_dp[1][0][1]=0.00162242; p4_emc_s_dp[1][0][1]=-0.000314348; 
    p0_emc_m_dp[1][1][1]=0.000277976; p1_emc_m_dp[1][1][1]=0.00141965; p2_emc_m_dp[1][1][1]=-0.00281135; p3_emc_m_dp[1][1][1]=0.0012809; p4_emc_m_dp[1][1][1]=-0.000139271; 
    p0_emc_s_dp[1][1][1]=0.00571195; p1_emc_s_dp[1][1][1]=-0.000444776; p2_emc_s_dp[1][1][1]=-0.00107814; p3_emc_s_dp[1][1][1]=0.00162037; p4_emc_s_dp[1][1][1]=-0.000384898; 
    p0_emc_m_dp[1][2][1]=-0.00144893; p1_emc_m_dp[1][2][1]=0.00711186; p2_emc_m_dp[1][2][1]=-0.00990076; p3_emc_m_dp[1][2][1]=0.00469813; p4_emc_m_dp[1][2][1]=-0.000766792; 
    p0_emc_s_dp[1][2][1]=0.00639434; p1_emc_s_dp[1][2][1]=-0.00282747; p2_emc_s_dp[1][2][1]=0.00191163; p3_emc_s_dp[1][2][1]=0.000135613; p4_emc_s_dp[1][2][1]=-0.000138336; 
    p0_emc_m_dp[1][3][1]=-0.000921744; p1_emc_m_dp[1][3][1]=0.00496778; p2_emc_m_dp[1][3][1]=-0.00732286; p3_emc_m_dp[1][3][1]=0.0032699; p4_emc_m_dp[1][3][1]=-0.000514791; 
    p0_emc_s_dp[1][3][1]=0.0066206; p1_emc_s_dp[1][3][1]=-0.00342853; p2_emc_s_dp[1][3][1]=0.00258159; p3_emc_s_dp[1][3][1]=-0.000146352; p4_emc_s_dp[1][3][1]=-0.000100355; 
    p0_emc_m_dp[1][4][1]=-0.00136711; p1_emc_m_dp[1][4][1]=0.00653982; p2_emc_m_dp[1][4][1]=-0.00920074; p3_emc_m_dp[1][4][1]=0.00417843; p4_emc_m_dp[1][4][1]=-0.000685862; 
    p0_emc_s_dp[1][4][1]=0.00684322; p1_emc_s_dp[1][4][1]=-0.00430653; p2_emc_s_dp[1][4][1]=0.00398788; p3_emc_s_dp[1][4][1]=-0.000930352; p4_emc_s_dp[1][4][1]=4.48794e-05; 
    p0_emc_m_dp[1][5][1]=-0.000361658; p1_emc_m_dp[1][5][1]=0.00333344; p2_emc_m_dp[1][5][1]=-0.00543034; p3_emc_m_dp[1][5][1]=0.0024425; p4_emc_m_dp[1][5][1]=-0.000400741; 
    p0_emc_s_dp[1][5][1]=0.0055481; p1_emc_s_dp[1][5][1]=0.000767836; p2_emc_s_dp[1][5][1]=-0.00244856; p3_emc_s_dp[1][5][1]=0.00238825; p4_emc_s_dp[1][5][1]=-0.000555907; 
    p0_emc_m_dp[1][6][1]=-0.00017332; p1_emc_m_dp[1][6][1]=0.00275122; p2_emc_m_dp[1][6][1]=-0.00419012; p3_emc_m_dp[1][6][1]=0.00172225; p4_emc_m_dp[1][6][1]=-0.000254927; 
    p0_emc_s_dp[1][6][1]=0.0060612; p1_emc_s_dp[1][6][1]=-0.00182174; p2_emc_s_dp[1][6][1]=0.00127039; p3_emc_s_dp[1][6][1]=0.000229264; p4_emc_s_dp[1][6][1]=-0.000126311; 
    p0_emc_m_dp[1][7][1]=0.000779729; p1_emc_m_dp[1][7][1]=0.000627661; p2_emc_m_dp[1][7][1]=-0.00212018; p3_emc_m_dp[1][7][1]=0.00106763; p4_emc_m_dp[1][7][1]=-0.000169369; 
    p0_emc_s_dp[1][7][1]=0.00595128; p1_emc_s_dp[1][7][1]=-0.00128018; p2_emc_s_dp[1][7][1]=0.000266646; p3_emc_s_dp[1][7][1]=0.000830429; p4_emc_s_dp[1][7][1]=-0.000239114; 
    p0_emc_m_dp[1][8][1]=0.00135416; p1_emc_m_dp[1][8][1]=-0.000750165; p2_emc_m_dp[1][8][1]=-0.000138946; p3_emc_m_dp[1][8][1]=0.000175317; p4_emc_m_dp[1][8][1]=1.74622e-05; 
    p0_emc_s_dp[1][8][1]=0.00519925; p1_emc_s_dp[1][8][1]=0.00108131; p2_emc_s_dp[1][8][1]=-0.0020785; p3_emc_s_dp[1][8][1]=0.00168626; p4_emc_s_dp[1][8][1]=-0.000328229; 
    p0_emc_m_dp[1][9][1]=0.00270023; p1_emc_m_dp[1][9][1]=-0.00420144; p2_emc_m_dp[1][9][1]=0.00381847; p3_emc_m_dp[1][9][1]=-0.00139016; p4_emc_m_dp[1][9][1]=0.000318835; 
    p0_emc_s_dp[1][9][1]=0.00527931; p1_emc_s_dp[1][9][1]=0.00105852; p2_emc_s_dp[1][9][1]=-0.00213164; p3_emc_s_dp[1][9][1]=0.00172769; p4_emc_s_dp[1][9][1]=-0.000328434; 
    p0_emc_m_dp[2][0][1]=-0.00278427; p1_emc_m_dp[2][0][1]=-0.00273276; p2_emc_m_dp[2][0][1]=-0.000413626; p3_emc_m_dp[2][0][1]=0.00133099; p4_emc_m_dp[2][0][1]=-0.000209846; 
    p0_emc_s_dp[2][0][1]=0.00707459; p1_emc_s_dp[2][0][1]=-0.00075221; p2_emc_s_dp[2][0][1]=-0.00146176; p3_emc_s_dp[2][0][1]=0.00201933; p4_emc_s_dp[2][0][1]=-0.000469527; 
    p0_emc_m_dp[2][1][1]=-0.00394139; p1_emc_m_dp[2][1][1]=1.70875e-05; p2_emc_m_dp[2][1][1]=-0.00303673; p3_emc_m_dp[2][1][1]=0.00205566; p4_emc_m_dp[2][1][1]=-0.000347365; 
    p0_emc_s_dp[2][1][1]=0.00653889; p1_emc_s_dp[2][1][1]=0.000680893; p2_emc_s_dp[2][1][1]=-0.00321836; p3_emc_s_dp[2][1][1]=0.00291994; p4_emc_s_dp[2][1][1]=-0.000633091; 
    p0_emc_m_dp[2][2][1]=-0.00444397; p1_emc_m_dp[2][2][1]=0.000736164; p2_emc_m_dp[2][2][1]=-0.00397534; p3_emc_m_dp[2][2][1]=0.00243673; p4_emc_m_dp[2][2][1]=-0.0004517; 
    p0_emc_s_dp[2][2][1]=0.00595431; p1_emc_s_dp[2][2][1]=0.00219427; p2_emc_s_dp[2][2][1]=-0.00504868; p3_emc_s_dp[2][2][1]=0.00389744; p4_emc_s_dp[2][2][1]=-0.00082657; 
    p0_emc_m_dp[2][3][1]=-0.00377785; p1_emc_m_dp[2][3][1]=-0.000759679; p2_emc_m_dp[2][3][1]=-0.00257245; p3_emc_m_dp[2][3][1]=0.00180832; p4_emc_m_dp[2][3][1]=-0.000375952; 
    p0_emc_s_dp[2][3][1]=0.00641974; p1_emc_s_dp[2][3][1]=7.99702e-05; p2_emc_s_dp[2][3][1]=-0.00250714; p3_emc_s_dp[2][3][1]=0.00275321; p4_emc_s_dp[2][3][1]=-0.000659994; 
    p0_emc_m_dp[2][4][1]=-0.00509347; p1_emc_m_dp[2][4][1]=0.00273234; p2_emc_m_dp[2][4][1]=-0.00630363; p3_emc_m_dp[2][4][1]=0.00345759; p4_emc_m_dp[2][4][1]=-0.000650048; 
    p0_emc_s_dp[2][4][1]=0.00682987; p1_emc_s_dp[2][4][1]=-0.00172175; p2_emc_s_dp[2][4][1]=0.000224662; p3_emc_s_dp[2][4][1]=0.00118482; p4_emc_s_dp[2][4][1]=-0.00035729; 
    p0_emc_m_dp[2][5][1]=-0.00452371; p1_emc_m_dp[2][5][1]=0.000192452; p2_emc_m_dp[2][5][1]=-0.0031567; p3_emc_m_dp[2][5][1]=0.00182768; p4_emc_m_dp[2][5][1]=-0.000348813; 
    p0_emc_s_dp[2][5][1]=0.00669672; p1_emc_s_dp[2][5][1]=-0.00207259; p2_emc_s_dp[2][5][1]=0.000746811; p3_emc_s_dp[2][5][1]=0.000963935; p4_emc_s_dp[2][5][1]=-0.000330001; 
    p0_emc_m_dp[2][6][1]=-0.0046816; p1_emc_m_dp[2][6][1]=0.00181073; p2_emc_m_dp[2][6][1]=-0.00522285; p3_emc_m_dp[2][6][1]=0.00295659; p4_emc_m_dp[2][6][1]=-0.000554046; 
    p0_emc_s_dp[2][6][1]=0.00607725; p1_emc_s_dp[2][6][1]=-0.000153624; p2_emc_s_dp[2][6][1]=-0.00145299; p3_emc_s_dp[2][6][1]=0.00190348; p4_emc_s_dp[2][6][1]=-0.000466531; 
    p0_emc_m_dp[2][7][1]=-0.00417151; p1_emc_m_dp[2][7][1]=0.0005585; p2_emc_m_dp[2][7][1]=-0.00346413; p3_emc_m_dp[2][7][1]=0.00210663; p4_emc_m_dp[2][7][1]=-0.0003856; 
    p0_emc_s_dp[2][7][1]=0.00555751; p1_emc_s_dp[2][7][1]=0.00139983; p2_emc_s_dp[2][7][1]=-0.00315297; p3_emc_s_dp[2][7][1]=0.00258562; p4_emc_s_dp[2][7][1]=-0.000550002; 
    p0_emc_m_dp[2][8][1]=-0.00368215; p1_emc_m_dp[2][8][1]=-0.000372925; p2_emc_m_dp[2][8][1]=-0.00217959; p3_emc_m_dp[2][8][1]=0.00153183; p4_emc_m_dp[2][8][1]=-0.000246152; 
    p0_emc_s_dp[2][8][1]=0.00539583; p1_emc_s_dp[2][8][1]=0.00143599; p2_emc_s_dp[2][8][1]=-0.00275313; p3_emc_s_dp[2][8][1]=0.00212144; p4_emc_s_dp[2][8][1]=-0.000414498; 
    p0_emc_m_dp[2][9][1]=-0.0023568; p1_emc_m_dp[2][9][1]=-0.00378135; p2_emc_m_dp[2][9][1]=0.00170971; p3_emc_m_dp[2][9][1]=4.74268e-05; p4_emc_m_dp[2][9][1]=3.32359e-05; 
    p0_emc_s_dp[2][9][1]=0.00564493; p1_emc_s_dp[2][9][1]=0.000313146; p2_emc_s_dp[2][9][1]=-0.00115262; p3_emc_s_dp[2][9][1]=0.00117823; p4_emc_s_dp[2][9][1]=-0.000217797; 

    //PC3 DZ NEG

    
    p0_pc3_m_dz[0][0][0][0]=1.15563; p1_pc3_m_dz[0][0][0][0]=0.0485154; p2_pc3_m_dz[0][0][0][0]=-0.125201; p3_pc3_m_dz[0][0][0][0]=0.0929417; p4_pc3_m_dz[0][0][0][0]=-0.0402624; 
    p0_pc3_m_dz[1][0][0][0]=1.15563; p1_pc3_m_dz[1][0][0][0]=0.0485154; p2_pc3_m_dz[1][0][0][0]=-0.125201; p3_pc3_m_dz[1][0][0][0]=0.0929417; p4_pc3_m_dz[1][0][0][0]=-0.0402624; 
    p0_pc3_s_dz[0][0][0]=1.00954; p1_pc3_s_dz[0][0][0]=-0.179804; p2_pc3_s_dz[0][0][0]=0.903482; p3_pc3_s_dz[0][0][0]=-0.413757; p4_pc3_s_dz[0][0][0]=0.076014; 
    p0_pc3_m_dz[0][0][1][0]=1.07107; p1_pc3_m_dz[0][0][1][0]=0.062685; p2_pc3_m_dz[0][0][1][0]=-0.0868844; p3_pc3_m_dz[0][0][1][0]=0.0643788; p4_pc3_m_dz[0][0][1][0]=-0.019439; 
    p0_pc3_m_dz[1][0][1][0]=1.07107; p1_pc3_m_dz[1][0][1][0]=0.062685; p2_pc3_m_dz[1][0][1][0]=-0.0868844; p3_pc3_m_dz[1][0][1][0]=0.0643788; p4_pc3_m_dz[1][0][1][0]=-0.019439; 
    p0_pc3_s_dz[0][1][0]=0.907863; p1_pc3_s_dz[0][1][0]=0.268925; p2_pc3_s_dz[0][1][0]=0.448661; p3_pc3_s_dz[0][1][0]=-0.21444; p4_pc3_s_dz[0][1][0]=0.0383384; 
    p0_pc3_m_dz[0][0][2][0]=1.27203; p1_pc3_m_dz[0][0][2][0]=-0.585121; p2_pc3_m_dz[0][0][2][0]=0.506906; p3_pc3_m_dz[0][0][2][0]=-0.169293; p4_pc3_m_dz[0][0][2][0]=0.0172699; 
    p0_pc3_m_dz[1][0][2][0]=1.27203; p1_pc3_m_dz[1][0][2][0]=-0.585121; p2_pc3_m_dz[1][0][2][0]=0.506906; p3_pc3_m_dz[1][0][2][0]=-0.169293; p4_pc3_m_dz[1][0][2][0]=0.0172699; 
    p0_pc3_s_dz[0][2][0]=0.977963; p1_pc3_s_dz[0][2][0]=0.178606; p2_pc3_s_dz[0][2][0]=0.537249; p3_pc3_s_dz[0][2][0]=-0.263948; p4_pc3_s_dz[0][2][0]=0.0469762; 
    p0_pc3_m_dz[0][0][3][0]=0.744752; p1_pc3_m_dz[0][0][3][0]=0.790437; p2_pc3_m_dz[0][0][3][0]=-0.787254; p3_pc3_m_dz[0][0][3][0]=0.316857; p4_pc3_m_dz[0][0][3][0]=-0.0465154; 
    p0_pc3_m_dz[1][0][3][0]=0.744752; p1_pc3_m_dz[1][0][3][0]=0.790437; p2_pc3_m_dz[1][0][3][0]=-0.787254; p3_pc3_m_dz[1][0][3][0]=0.316857; p4_pc3_m_dz[1][0][3][0]=-0.0465154; 
    p0_pc3_s_dz[0][3][0]=0.764319; p1_pc3_s_dz[0][3][0]=0.879855; p2_pc3_s_dz[0][3][0]=-0.243223; p3_pc3_s_dz[0][3][0]=0.0920169; p4_pc3_s_dz[0][3][0]=-0.00918065; 
    p0_pc3_m_dz[0][0][4][0]=0.941016; p1_pc3_m_dz[0][0][4][0]=0.13642; p2_pc3_m_dz[0][0][4][0]=-0.0882655; p3_pc3_m_dz[0][0][4][0]=0.00587545; p4_pc3_m_dz[0][0][4][0]=0.0011141; 
    p0_pc3_m_dz[1][0][4][0]=0.941016; p1_pc3_m_dz[1][0][4][0]=0.13642; p2_pc3_m_dz[1][0][4][0]=-0.0882655; p3_pc3_m_dz[1][0][4][0]=0.00587545; p4_pc3_m_dz[1][0][4][0]=0.0011141; 
    p0_pc3_s_dz[0][4][0]=0.871298; p1_pc3_s_dz[0][4][0]=0.700648; p2_pc3_s_dz[0][4][0]=-0.166746; p3_pc3_s_dz[0][4][0]=0.0966312; p4_pc3_s_dz[0][4][0]=-0.0140426; 
    p0_pc3_m_dz[0][0][5][0]=1.19173; p1_pc3_m_dz[0][0][5][0]=-0.39189; p2_pc3_m_dz[0][0][5][0]=0.343679; p3_pc3_m_dz[0][0][5][0]=-0.156398; p4_pc3_m_dz[0][0][5][0]=0.0223831; 
    p0_pc3_m_dz[1][0][5][0]=1.19173; p1_pc3_m_dz[1][0][5][0]=-0.39189; p2_pc3_m_dz[1][0][5][0]=0.343679; p3_pc3_m_dz[1][0][5][0]=-0.156398; p4_pc3_m_dz[1][0][5][0]=0.0223831; 
    p0_pc3_s_dz[0][5][0]=0.748633; p1_pc3_s_dz[0][5][0]=0.901794; p2_pc3_s_dz[0][5][0]=-0.272127; p3_pc3_s_dz[0][5][0]=0.108219; p4_pc3_s_dz[0][5][0]=-0.0119187; 
    p0_pc3_m_dz[0][0][6][0]=1.12194; p1_pc3_m_dz[0][0][6][0]=-0.125521; p2_pc3_m_dz[0][0][6][0]=0.0917911; p3_pc3_m_dz[0][0][6][0]=-0.056688; p4_pc3_m_dz[0][0][6][0]=0.00947996; 
    p0_pc3_m_dz[1][0][6][0]=1.12194; p1_pc3_m_dz[1][0][6][0]=-0.125521; p2_pc3_m_dz[1][0][6][0]=0.0917911; p3_pc3_m_dz[1][0][6][0]=-0.056688; p4_pc3_m_dz[1][0][6][0]=0.00947996; 
    p0_pc3_s_dz[0][6][0]=0.879767; p1_pc3_s_dz[0][6][0]=0.588351; p2_pc3_s_dz[0][6][0]=0.0315582; p3_pc3_s_dz[0][6][0]=-0.0202232; p4_pc3_s_dz[0][6][0]=0.00776696; 
    p0_pc3_m_dz[0][0][7][0]=0.969596; p1_pc3_m_dz[0][0][7][0]=0.363961; p2_pc3_m_dz[0][0][7][0]=-0.301359; p3_pc3_m_dz[0][0][7][0]=0.0570403; p4_pc3_m_dz[0][0][7][0]=-0.000289031; 
    p0_pc3_m_dz[1][0][7][0]=0.969596; p1_pc3_m_dz[1][0][7][0]=0.363961; p2_pc3_m_dz[1][0][7][0]=-0.301359; p3_pc3_m_dz[1][0][7][0]=0.0570403; p4_pc3_m_dz[1][0][7][0]=-0.000289031; 
    p0_pc3_s_dz[0][7][0]=0.910531; p1_pc3_s_dz[0][7][0]=0.703085; p2_pc3_s_dz[0][7][0]=-0.184589; p3_pc3_s_dz[0][7][0]=0.0946589; p4_pc3_s_dz[0][7][0]=-0.0106576; 
    p0_pc3_m_dz[0][0][8][0]=0.93045; p1_pc3_m_dz[0][0][8][0]=0.732464; p2_pc3_m_dz[0][0][8][0]=-0.763752; p3_pc3_m_dz[0][0][8][0]=0.268197; p4_pc3_m_dz[0][0][8][0]=-0.0299745; 
    p0_pc3_m_dz[1][0][8][0]=0.93045; p1_pc3_m_dz[1][0][8][0]=0.732464; p2_pc3_m_dz[1][0][8][0]=-0.763752; p3_pc3_m_dz[1][0][8][0]=0.268197; p4_pc3_m_dz[1][0][8][0]=-0.0299745; 
    p0_pc3_s_dz[0][8][0]=0.871559; p1_pc3_s_dz[0][8][0]=0.680608; p2_pc3_s_dz[0][8][0]=-0.0834437; p3_pc3_s_dz[0][8][0]=0.027385; p4_pc3_s_dz[0][8][0]=0.00320437; 
    p0_pc3_m_dz[0][0][9][0]=0.599681; p1_pc3_m_dz[0][0][9][0]=1.65131; p2_pc3_m_dz[0][0][9][0]=-1.5604; p3_pc3_m_dz[0][0][9][0]=0.561913; p4_pc3_m_dz[0][0][9][0]=-0.0542183; 
    p0_pc3_m_dz[1][0][9][0]=0.599681; p1_pc3_m_dz[1][0][9][0]=1.65131; p2_pc3_m_dz[1][0][9][0]=-1.5604; p3_pc3_m_dz[1][0][9][0]=0.561913; p4_pc3_m_dz[1][0][9][0]=-0.0542183; 
    p0_pc3_s_dz[0][9][0]=1.34484; p1_pc3_s_dz[0][9][0]=-0.776592; p2_pc3_s_dz[0][9][0]=1.41951; p3_pc3_s_dz[0][9][0]=-0.613627; p4_pc3_s_dz[0][9][0]=0.104626; 
    p0_pc3_m_dz[0][1][0][0]=2.28362; p1_pc3_m_dz[0][1][0][0]=-0.150842; p2_pc3_m_dz[0][1][0][0]=0.188197; p3_pc3_m_dz[0][1][0][0]=0.014728; p4_pc3_m_dz[0][1][0][0]=-0.0363196; 
    p0_pc3_m_dz[1][1][0][0]=2.28362; p1_pc3_m_dz[1][1][0][0]=-0.150842; p2_pc3_m_dz[1][1][0][0]=0.188197; p3_pc3_m_dz[1][1][0][0]=0.014728; p4_pc3_m_dz[1][1][0][0]=-0.0363196; 
    p0_pc3_s_dz[1][0][0]=1.81736; p1_pc3_s_dz[1][0][0]=-1.66163; p2_pc3_s_dz[1][0][0]=1.95384; p3_pc3_s_dz[1][0][0]=-0.713741; p4_pc3_s_dz[1][0][0]=0.106776; 
    p0_pc3_m_dz[0][1][1][0]=2.25698; p1_pc3_m_dz[0][1][1][0]=-0.449325; p2_pc3_m_dz[0][1][1][0]=0.463086; p3_pc3_m_dz[0][1][1][0]=-0.101316; p4_pc3_m_dz[0][1][1][0]=-0.00241308; 
    p0_pc3_m_dz[1][1][1][0]=2.25698; p1_pc3_m_dz[1][1][1][0]=-0.449325; p2_pc3_m_dz[1][1][1][0]=0.463086; p3_pc3_m_dz[1][1][1][0]=-0.101316; p4_pc3_m_dz[1][1][1][0]=-0.00241308; 
    p0_pc3_s_dz[1][1][0]=1.61449; p1_pc3_s_dz[1][1][0]=-0.993818; p2_pc3_s_dz[1][1][0]=1.32224; p3_pc3_s_dz[1][1][0]=-0.456191; p4_pc3_s_dz[1][1][0]=0.0638099; 
    p0_pc3_m_dz[0][1][2][0]=1.68043; p1_pc3_m_dz[0][1][2][0]=0.562341; p2_pc3_m_dz[0][1][2][0]=-0.421356; p3_pc3_m_dz[0][1][2][0]=0.194676; p4_pc3_m_dz[0][1][2][0]=-0.033073; 
    p0_pc3_m_dz[1][1][2][0]=1.68043; p1_pc3_m_dz[1][1][2][0]=0.562341; p2_pc3_m_dz[1][1][2][0]=-0.421356; p3_pc3_m_dz[1][1][2][0]=0.194676; p4_pc3_m_dz[1][1][2][0]=-0.033073; 
    p0_pc3_s_dz[1][2][0]=1.29635; p1_pc3_s_dz[1][2][0]=0.112681; p2_pc3_s_dz[1][2][0]=0.132774; p3_pc3_s_dz[1][2][0]=0.0522742; p4_pc3_s_dz[1][2][0]=-0.0115953; 
    p0_pc3_m_dz[0][1][3][0]=1.6133; p1_pc3_m_dz[0][1][3][0]=0.413033; p2_pc3_m_dz[0][1][3][0]=-0.452855; p3_pc3_m_dz[0][1][3][0]=0.240575; p4_pc3_m_dz[0][1][3][0]=-0.0419205; 
    p0_pc3_m_dz[1][1][3][0]=1.6133; p1_pc3_m_dz[1][1][3][0]=0.413033; p2_pc3_m_dz[1][1][3][0]=-0.452855; p3_pc3_m_dz[1][1][3][0]=0.240575; p4_pc3_m_dz[1][1][3][0]=-0.0419205; 
    p0_pc3_s_dz[1][3][0]=1.35975; p1_pc3_s_dz[1][3][0]=-0.186818; p2_pc3_s_dz[1][3][0]=0.484476; p3_pc3_s_dz[1][3][0]=-0.104193; p4_pc3_s_dz[1][3][0]=0.0117418; 
    p0_pc3_m_dz[0][1][4][0]=1.46615; p1_pc3_m_dz[0][1][4][0]=0.364244; p2_pc3_m_dz[0][1][4][0]=-0.341951; p3_pc3_m_dz[0][1][4][0]=0.154339; p4_pc3_m_dz[0][1][4][0]=-0.0237488; 
    p0_pc3_m_dz[1][1][4][0]=1.46615; p1_pc3_m_dz[1][1][4][0]=0.364244; p2_pc3_m_dz[1][1][4][0]=-0.341951; p3_pc3_m_dz[1][1][4][0]=0.154339; p4_pc3_m_dz[1][1][4][0]=-0.0237488; 
    p0_pc3_s_dz[1][4][0]=1.31571; p1_pc3_s_dz[1][4][0]=-0.0981959; p2_pc3_s_dz[1][4][0]=0.413537; p3_pc3_s_dz[1][4][0]=-0.0802245; p4_pc3_s_dz[1][4][0]=0.00796014; 
    p0_pc3_m_dz[0][1][5][0]=1.38127; p1_pc3_m_dz[0][1][5][0]=0.583128; p2_pc3_m_dz[0][1][5][0]=-0.569419; p3_pc3_m_dz[0][1][5][0]=0.205554; p4_pc3_m_dz[0][1][5][0]=-0.0273026; 
    p0_pc3_m_dz[1][1][5][0]=1.38127; p1_pc3_m_dz[1][1][5][0]=0.583128; p2_pc3_m_dz[1][1][5][0]=-0.569419; p3_pc3_m_dz[1][1][5][0]=0.205554; p4_pc3_m_dz[1][1][5][0]=-0.0273026; 
    p0_pc3_s_dz[1][5][0]=1.28174; p1_pc3_s_dz[1][5][0]=0.120945; p2_pc3_s_dz[1][5][0]=0.189921; p3_pc3_s_dz[1][5][0]=0.0338389; p4_pc3_s_dz[1][5][0]=-0.0116441; 
    p0_pc3_m_dz[0][1][6][0]=1.54394; p1_pc3_m_dz[0][1][6][0]=0.103972; p2_pc3_m_dz[0][1][6][0]=-0.141502; p3_pc3_m_dz[0][1][6][0]=0.0440618; p4_pc3_m_dz[0][1][6][0]=-0.00466731; 
    p0_pc3_m_dz[1][1][6][0]=1.54394; p1_pc3_m_dz[1][1][6][0]=0.103972; p2_pc3_m_dz[1][1][6][0]=-0.141502; p3_pc3_m_dz[1][1][6][0]=0.0440618; p4_pc3_m_dz[1][1][6][0]=-0.00466731; 
    p0_pc3_s_dz[1][6][0]=1.30784; p1_pc3_s_dz[1][6][0]=-0.100703; p2_pc3_s_dz[1][6][0]=0.408691; p3_pc3_s_dz[1][6][0]=-0.0539257; p4_pc3_s_dz[1][6][0]=0.00179151; 
    p0_pc3_m_dz[0][1][7][0]=1.27875; p1_pc3_m_dz[0][1][7][0]=0.561195; p2_pc3_m_dz[0][1][7][0]=-0.687451; p3_pc3_m_dz[0][1][7][0]=0.275634; p4_pc3_m_dz[0][1][7][0]=-0.037122; 
    p0_pc3_m_dz[1][1][7][0]=1.27875; p1_pc3_m_dz[1][1][7][0]=0.561195; p2_pc3_m_dz[1][1][7][0]=-0.687451; p3_pc3_m_dz[1][1][7][0]=0.275634; p4_pc3_m_dz[1][1][7][0]=-0.037122; 
    p0_pc3_s_dz[1][7][0]=1.17952; p1_pc3_s_dz[1][7][0]=0.000823834; p2_pc3_s_dz[1][7][0]=0.547977; p3_pc3_s_dz[1][7][0]=-0.195255; p4_pc3_s_dz[1][7][0]=0.0319605; 
    p0_pc3_m_dz[0][1][8][0]=1.1114; p1_pc3_m_dz[0][1][8][0]=0.573829; p2_pc3_m_dz[0][1][8][0]=-0.560545; p3_pc3_m_dz[0][1][8][0]=0.151092; p4_pc3_m_dz[0][1][8][0]=-0.00637219; 
    p0_pc3_m_dz[1][1][8][0]=1.1114; p1_pc3_m_dz[1][1][8][0]=0.573829; p2_pc3_m_dz[1][1][8][0]=-0.560545; p3_pc3_m_dz[1][1][8][0]=0.151092; p4_pc3_m_dz[1][1][8][0]=-0.00637219; 
    p0_pc3_s_dz[1][8][0]=1.14356; p1_pc3_s_dz[1][8][0]=0.004997; p2_pc3_s_dz[1][8][0]=0.479548; p3_pc3_s_dz[1][8][0]=-0.134139; p4_pc3_s_dz[1][8][0]=0.019154; 
    p0_pc3_m_dz[0][1][9][0]=0.861962; p1_pc3_m_dz[0][1][9][0]=1.12484; p2_pc3_m_dz[0][1][9][0]=-1.21117; p3_pc3_m_dz[0][1][9][0]=0.448303; p4_pc3_m_dz[0][1][9][0]=-0.0382131; 
    p0_pc3_m_dz[1][1][9][0]=0.861962; p1_pc3_m_dz[1][1][9][0]=1.12484; p2_pc3_m_dz[1][1][9][0]=-1.21117; p3_pc3_m_dz[1][1][9][0]=0.448303; p4_pc3_m_dz[1][1][9][0]=-0.0382131; 
    p0_pc3_s_dz[1][9][0]=1.15141; p1_pc3_s_dz[1][9][0]=-0.344819; p2_pc3_s_dz[1][9][0]=1.02195; p3_pc3_s_dz[1][9][0]=-0.434779; p4_pc3_s_dz[1][9][0]=0.0778354; 	
	
    //PC3 DZ POS 
	
    p0_pc3_m_dz[0][0][0][1]=1.22457; p1_pc3_m_dz[0][0][0][1]=-0.091325; p2_pc3_m_dz[0][0][0][1]=0.405967; p3_pc3_m_dz[0][0][0][1]=-0.156803; p4_pc3_m_dz[0][0][0][1]=-0.00115473; 
    p0_pc3_m_dz[1][0][0][1]=1.22457; p1_pc3_m_dz[1][0][0][1]=-0.091325; p2_pc3_m_dz[1][0][0][1]=0.405967; p3_pc3_m_dz[1][0][0][1]=-0.156803; p4_pc3_m_dz[1][0][0][1]=-0.00115473; 
    p0_pc3_s_dz[0][0][1]=0.722523; p1_pc3_s_dz[0][0][1]=0.679873; p2_pc3_s_dz[0][0][1]=0.13181; p3_pc3_s_dz[0][0][1]=-0.0900451; p4_pc3_s_dz[0][0][1]=0.0258397; 
    p0_pc3_m_dz[0][0][1][1]=0.870617; p1_pc3_m_dz[0][0][1][1]=0.807669; p2_pc3_m_dz[0][0][1][1]=-0.613724; p3_pc3_m_dz[0][0][1][1]=0.2886; p4_pc3_m_dz[0][0][1][1]=-0.0534472; 
    p0_pc3_m_dz[1][0][1][1]=0.870617; p1_pc3_m_dz[1][0][1][1]=0.807669; p2_pc3_m_dz[1][0][1][1]=-0.613724; p3_pc3_m_dz[1][0][1][1]=0.2886; p4_pc3_m_dz[1][0][1][1]=-0.0534472; 
    p0_pc3_s_dz[0][1][1]=0.810919; p1_pc3_s_dz[0][1][1]=0.660442; p2_pc3_s_dz[0][1][1]=0.00487098; p3_pc3_s_dz[0][1][1]=0.0318786; p4_pc3_s_dz[0][1][1]=-0.00794003; 
    p0_pc3_m_dz[0][0][2][1]=1.0707; p1_pc3_m_dz[0][0][2][1]=0.00933601; p2_pc3_m_dz[0][0][2][1]=0.195841; p3_pc3_m_dz[0][0][2][1]=-0.0858459; p4_pc3_m_dz[0][0][2][1]=0.00999599; 
    p0_pc3_m_dz[1][0][2][1]=1.0707; p1_pc3_m_dz[1][0][2][1]=0.00933601; p2_pc3_m_dz[1][0][2][1]=0.195841; p3_pc3_m_dz[1][0][2][1]=-0.0858459; p4_pc3_m_dz[1][0][2][1]=0.00999599; 
    p0_pc3_s_dz[0][2][1]=0.77366; p1_pc3_s_dz[0][2][1]=0.805949; p2_pc3_s_dz[0][2][1]=-0.0934263; p3_pc3_s_dz[0][2][1]=0.0417795; p4_pc3_s_dz[0][2][1]=-0.00524404; 
    p0_pc3_m_dz[0][0][3][1]=1.23941; p1_pc3_m_dz[0][0][3][1]=-0.542748; p2_pc3_m_dz[0][0][3][1]=0.638648; p3_pc3_m_dz[0][0][3][1]=-0.267839; p4_pc3_m_dz[0][0][3][1]=0.0367252; 
    p0_pc3_m_dz[1][0][3][1]=1.23941; p1_pc3_m_dz[1][0][3][1]=-0.542748; p2_pc3_m_dz[1][0][3][1]=0.638648; p3_pc3_m_dz[1][0][3][1]=-0.267839; p4_pc3_m_dz[1][0][3][1]=0.0367252; 
    p0_pc3_s_dz[0][3][1]=0.976042; p1_pc3_s_dz[0][3][1]=0.192455; p2_pc3_s_dz[0][3][1]=0.527642; p3_pc3_s_dz[0][3][1]=-0.208018; p4_pc3_s_dz[0][3][1]=0.0287237; 
    p0_pc3_m_dz[0][0][4][1]=0.942914; p1_pc3_m_dz[0][0][4][1]=0.281619; p2_pc3_m_dz[0][0][4][1]=-0.263962; p3_pc3_m_dz[0][0][4][1]=0.107019; p4_pc3_m_dz[0][0][4][1]=-0.0172625; 
    p0_pc3_m_dz[1][0][4][1]=0.942914; p1_pc3_m_dz[1][0][4][1]=0.281619; p2_pc3_m_dz[1][0][4][1]=-0.263962; p3_pc3_m_dz[1][0][4][1]=0.107019; p4_pc3_m_dz[1][0][4][1]=-0.0172625; 
    p0_pc3_s_dz[0][4][1]=0.990447; p1_pc3_s_dz[0][4][1]=0.138104; p2_pc3_s_dz[0][4][1]=0.599314; p3_pc3_s_dz[0][4][1]=-0.233628; p4_pc3_s_dz[0][4][1]=0.0298545; 
    p0_pc3_m_dz[0][0][5][1]=0.919174; p1_pc3_m_dz[0][0][5][1]=0.361194; p2_pc3_m_dz[0][0][5][1]=-0.380749; p3_pc3_m_dz[0][0][5][1]=0.126878; p4_pc3_m_dz[0][0][5][1]=-0.0166638; 
    p0_pc3_m_dz[1][0][5][1]=0.919174; p1_pc3_m_dz[1][0][5][1]=0.361194; p2_pc3_m_dz[1][0][5][1]=-0.380749; p3_pc3_m_dz[1][0][5][1]=0.126878; p4_pc3_m_dz[1][0][5][1]=-0.0166638; 
    p0_pc3_s_dz[0][5][1]=1.09177; p1_pc3_s_dz[0][5][1]=-0.239313; p2_pc3_s_dz[0][5][1]=1.0045; p3_pc3_s_dz[0][5][1]=-0.415364; p4_pc3_s_dz[0][5][1]=0.059149; 
    p0_pc3_m_dz[0][0][6][1]=0.960132; p1_pc3_m_dz[0][0][6][1]=0.317085; p2_pc3_m_dz[0][0][6][1]=-0.400653; p3_pc3_m_dz[0][0][6][1]=0.148982; p4_pc3_m_dz[0][0][6][1]=-0.0205846; 
    p0_pc3_m_dz[1][0][6][1]=0.960132; p1_pc3_m_dz[1][0][6][1]=0.317085; p2_pc3_m_dz[1][0][6][1]=-0.400653; p3_pc3_m_dz[1][0][6][1]=0.148982; p4_pc3_m_dz[1][0][6][1]=-0.0205846; 
    p0_pc3_s_dz[0][6][1]=1.05295; p1_pc3_s_dz[0][6][1]=-0.00438966; p2_pc3_s_dz[0][6][1]=0.707571; p3_pc3_s_dz[0][6][1]=-0.287041; p4_pc3_s_dz[0][6][1]=0.041502; 
    p0_pc3_m_dz[0][0][7][1]=1.05302; p1_pc3_m_dz[0][0][7][1]=0.219108; p2_pc3_m_dz[0][0][7][1]=-0.383171; p3_pc3_m_dz[0][0][7][1]=0.136789; p4_pc3_m_dz[0][0][7][1]=-0.0165074; 
    p0_pc3_m_dz[1][0][7][1]=1.05302; p1_pc3_m_dz[1][0][7][1]=0.219108; p2_pc3_m_dz[1][0][7][1]=-0.383171; p3_pc3_m_dz[1][0][7][1]=0.136789; p4_pc3_m_dz[1][0][7][1]=-0.0165074; 
    p0_pc3_s_dz[0][7][1]=0.982155; p1_pc3_s_dz[0][7][1]=0.301313; p2_pc3_s_dz[0][7][1]=0.393788; p3_pc3_s_dz[0][7][1]=-0.164094; p4_pc3_s_dz[0][7][1]=0.0251762; 
    p0_pc3_m_dz[0][0][8][1]=0.920098; p1_pc3_m_dz[0][0][8][1]=0.607333; p2_pc3_m_dz[0][0][8][1]=-0.723083; p3_pc3_m_dz[0][0][8][1]=0.235302; p4_pc3_m_dz[0][0][8][1]=-0.0227368; 
    p0_pc3_m_dz[1][0][8][1]=0.920098; p1_pc3_m_dz[1][0][8][1]=0.607333; p2_pc3_m_dz[1][0][8][1]=-0.723083; p3_pc3_m_dz[1][0][8][1]=0.235302; p4_pc3_m_dz[1][0][8][1]=-0.0227368; 
    p0_pc3_s_dz[0][8][1]=1.1619; p1_pc3_s_dz[0][8][1]=-0.374018; p2_pc3_s_dz[0][8][1]=1.11712; p3_pc3_s_dz[0][8][1]=-0.475724; p4_pc3_s_dz[0][8][1]=0.072674; 
    p0_pc3_m_dz[0][0][9][1]=0.610522; p1_pc3_m_dz[0][0][9][1]=1.54302; p2_pc3_m_dz[0][0][9][1]=-1.68426; p3_pc3_m_dz[0][0][9][1]=0.636848; p4_pc3_m_dz[0][0][9][1]=-0.0663552; 
    p0_pc3_m_dz[1][0][9][1]=0.610522; p1_pc3_m_dz[1][0][9][1]=1.54302; p2_pc3_m_dz[1][0][9][1]=-1.68426; p3_pc3_m_dz[1][0][9][1]=0.636848; p4_pc3_m_dz[1][0][9][1]=-0.0663552; 
    p0_pc3_s_dz[0][9][1]=1.32586; p1_pc3_s_dz[0][9][1]=-0.998224; p2_pc3_s_dz[0][9][1]=1.86601; p3_pc3_s_dz[0][9][1]=-0.841011; p4_pc3_s_dz[0][9][1]=0.138856; 
    p0_pc3_m_dz[0][1][0][1]=2.70902; p1_pc3_m_dz[0][1][0][1]=-1.54253; p2_pc3_m_dz[0][1][0][1]=1.49178; p3_pc3_m_dz[0][1][0][1]=-0.576082; p4_pc3_m_dz[0][1][0][1]=0.0570033; 
    p0_pc3_m_dz[1][1][0][1]=2.70902; p1_pc3_m_dz[1][1][0][1]=-1.54253; p2_pc3_m_dz[1][1][0][1]=1.49178; p3_pc3_m_dz[1][1][0][1]=-0.576082; p4_pc3_m_dz[1][1][0][1]=0.0570033; 
    p0_pc3_s_dz[1][0][1]=1.73612; p1_pc3_s_dz[1][0][1]=-1.64098; p2_pc3_s_dz[1][0][1]=2.11008; p3_pc3_s_dz[1][0][1]=-0.79967; p4_pc3_s_dz[1][0][1]=0.118052; 
    p0_pc3_m_dz[0][1][1][1]=2.27408; p1_pc3_m_dz[0][1][1][1]=-0.701798; p2_pc3_m_dz[0][1][1][1]=0.650081; p3_pc3_m_dz[0][1][1][1]=-0.220403; p4_pc3_m_dz[0][1][1][1]=0.0197327; 
    p0_pc3_m_dz[1][1][1][1]=2.27408; p1_pc3_m_dz[1][1][1][1]=-0.701798; p2_pc3_m_dz[1][1][1][1]=0.650081; p3_pc3_m_dz[1][1][1][1]=-0.220403; p4_pc3_m_dz[1][1][1][1]=0.0197327; 
    p0_pc3_s_dz[1][1][1]=1.49428; p1_pc3_s_dz[1][1][1]=-0.724612; p2_pc3_s_dz[1][1][1]=1.07739; p3_pc3_s_dz[1][1][1]=-0.326902; p4_pc3_s_dz[1][1][1]=0.0376694; 
    p0_pc3_m_dz[0][1][2][1]=1.61181; p1_pc3_m_dz[0][1][2][1]=0.613741; p2_pc3_m_dz[0][1][2][1]=-0.58142; p3_pc3_m_dz[0][1][2][1]=0.259799; p4_pc3_m_dz[0][1][2][1]=-0.0435383; 
    p0_pc3_m_dz[1][1][2][1]=1.61181; p1_pc3_m_dz[1][1][2][1]=0.613741; p2_pc3_m_dz[1][1][2][1]=-0.58142; p3_pc3_m_dz[1][1][2][1]=0.259799; p4_pc3_m_dz[1][1][2][1]=-0.0435383; 
    p0_pc3_s_dz[1][2][1]=1.4664; p1_pc3_s_dz[1][2][1]=-0.612986; p2_pc3_s_dz[1][2][1]=1.00536; p3_pc3_s_dz[1][2][1]=-0.316331; p4_pc3_s_dz[1][2][1]=0.0382143; 
    p0_pc3_m_dz[0][1][3][1]=1.59115; p1_pc3_m_dz[0][1][3][1]=0.253825; p2_pc3_m_dz[0][1][3][1]=-0.255048; p3_pc3_m_dz[0][1][3][1]=0.116816; p4_pc3_m_dz[0][1][3][1]=-0.019932; 
    p0_pc3_m_dz[1][1][3][1]=1.59115; p1_pc3_m_dz[1][1][3][1]=0.253825; p2_pc3_m_dz[1][1][3][1]=-0.255048; p3_pc3_m_dz[1][1][3][1]=0.116816; p4_pc3_m_dz[1][1][3][1]=-0.019932; 
    p0_pc3_s_dz[1][3][1]=1.59635; p1_pc3_s_dz[1][3][1]=-1.17325; p2_pc3_s_dz[1][3][1]=1.6722; p3_pc3_s_dz[1][3][1]=-0.609999; p4_pc3_s_dz[1][3][1]=0.0810615; 
    p0_pc3_m_dz[0][1][4][1]=1.6147; p1_pc3_m_dz[0][1][4][1]=-0.0278778; p2_pc3_m_dz[0][1][4][1]=-0.00765358; p3_pc3_m_dz[0][1][4][1]=0.0192951; p4_pc3_m_dz[0][1][4][1]=-0.00448474; 
    p0_pc3_m_dz[1][1][4][1]=1.6147; p1_pc3_m_dz[1][1][4][1]=-0.0278778; p2_pc3_m_dz[1][1][4][1]=-0.00765358; p3_pc3_m_dz[1][1][4][1]=0.0192951; p4_pc3_m_dz[1][1][4][1]=-0.00448474; 
    p0_pc3_s_dz[1][4][1]=1.51029; p1_pc3_s_dz[1][4][1]=-0.852594; p2_pc3_s_dz[1][4][1]=1.28574; p3_pc3_s_dz[1][4][1]=-0.415077; p4_pc3_s_dz[1][4][1]=0.0473359; 
    p0_pc3_m_dz[0][1][5][1]=1.40953; p1_pc3_m_dz[0][1][5][1]=0.573589; p2_pc3_m_dz[0][1][5][1]=-0.604089; p3_pc3_m_dz[0][1][5][1]=0.217713; p4_pc3_m_dz[0][1][5][1]=-0.0268368; 
    p0_pc3_m_dz[1][1][5][1]=1.40953; p1_pc3_m_dz[1][1][5][1]=0.573589; p2_pc3_m_dz[1][1][5][1]=-0.604089; p3_pc3_m_dz[1][1][5][1]=0.217713; p4_pc3_m_dz[1][1][5][1]=-0.0268368; 
    p0_pc3_s_dz[1][5][1]=1.73839; p1_pc3_s_dz[1][5][1]=-1.44731; p2_pc3_s_dz[1][5][1]=1.92761; p3_pc3_s_dz[1][5][1]=-0.690907; p4_pc3_s_dz[1][5][1]=0.0876781; 
    p0_pc3_m_dz[0][1][6][1]=1.51898; p1_pc3_m_dz[0][1][6][1]=0.198737; p2_pc3_m_dz[0][1][6][1]=-0.188819; p3_pc3_m_dz[0][1][6][1]=0.063486; p4_pc3_m_dz[0][1][6][1]=-0.00683049; 
    p0_pc3_m_dz[1][1][6][1]=1.51898; p1_pc3_m_dz[1][1][6][1]=0.198737; p2_pc3_m_dz[1][1][6][1]=-0.188819; p3_pc3_m_dz[1][1][6][1]=0.063486; p4_pc3_m_dz[1][1][6][1]=-0.00683049; 
    p0_pc3_s_dz[1][6][1]=1.3943; p1_pc3_s_dz[1][6][1]=-0.6031; p2_pc3_s_dz[1][6][1]=1.17552; p3_pc3_s_dz[1][6][1]=-0.437095; p4_pc3_s_dz[1][6][1]=0.061368; 
    p0_pc3_m_dz[0][1][7][1]=1.21408; p1_pc3_m_dz[0][1][7][1]=0.621604; p2_pc3_m_dz[0][1][7][1]=-0.477483; p3_pc3_m_dz[0][1][7][1]=0.130297; p4_pc3_m_dz[0][1][7][1]=-0.00894789; 
    p0_pc3_m_dz[1][1][7][1]=1.21408; p1_pc3_m_dz[1][1][7][1]=0.621604; p2_pc3_m_dz[1][1][7][1]=-0.477483; p3_pc3_m_dz[1][1][7][1]=0.130297; p4_pc3_m_dz[1][1][7][1]=-0.00894789; 
    p0_pc3_s_dz[1][7][1]=1.31904; p1_pc3_s_dz[1][7][1]=-0.462112; p2_pc3_s_dz[1][7][1]=1.07948; p3_pc3_s_dz[1][7][1]=-0.408116; p4_pc3_s_dz[1][7][1]=0.0575823; 
    p0_pc3_m_dz[0][1][8][1]=1.24511; p1_pc3_m_dz[0][1][8][1]=0.362599; p2_pc3_m_dz[0][1][8][1]=-0.234504; p3_pc3_m_dz[0][1][8][1]=0.0271878; p4_pc3_m_dz[0][1][8][1]=0.00978524; 
    p0_pc3_m_dz[1][1][8][1]=1.24511; p1_pc3_m_dz[1][1][8][1]=0.362599; p2_pc3_m_dz[1][1][8][1]=-0.234504; p3_pc3_m_dz[1][1][8][1]=0.0271878; p4_pc3_m_dz[1][1][8][1]=0.00978524; 
    p0_pc3_s_dz[1][8][1]=1.41192; p1_pc3_s_dz[1][8][1]=-1.00558; p2_pc3_s_dz[1][8][1]=1.75875; p3_pc3_s_dz[1][8][1]=-0.717604; p4_pc3_s_dz[1][8][1]=0.105128; 
    p0_pc3_m_dz[0][1][9][1]=0.766819; p1_pc3_m_dz[0][1][9][1]=1.44486; p2_pc3_m_dz[0][1][9][1]=-1.20668; p3_pc3_m_dz[0][1][9][1]=0.412654; p4_pc3_m_dz[0][1][9][1]=-0.0309698; 
    p0_pc3_m_dz[1][1][9][1]=0.766819; p1_pc3_m_dz[1][1][9][1]=1.44486; p2_pc3_m_dz[1][1][9][1]=-1.20668; p3_pc3_m_dz[1][1][9][1]=0.412654; p4_pc3_m_dz[1][1][9][1]=-0.0309698; 
    p0_pc3_s_dz[1][9][1]=1.82792; p1_pc3_s_dz[1][9][1]=-2.37246; p2_pc3_s_dz[1][9][1]=3.24951; p3_pc3_s_dz[1][9][1]=-1.38257; p4_pc3_s_dz[1][9][1]=0.213768; 

    //PC3 DPHI NEG
   	
    p0_pc3_m_dp[0][0][0][0]=0.00715752; p1_pc3_m_dp[0][0][0][0]=-0.0187578; p2_pc3_m_dp[0][0][0][0]=0.0193936; p3_pc3_m_dp[0][0][0][0]=-0.00890057; p4_pc3_m_dp[0][0][0][0]=0.0012751; 
    p0_pc3_m_dp[1][0][0][0]=0.00715752; p1_pc3_m_dp[1][0][0][0]=-0.0187578; p2_pc3_m_dp[1][0][0][0]=0.0193936; p3_pc3_m_dp[1][0][0][0]=-0.00890057; p4_pc3_m_dp[1][0][0][0]=0.0012751; 
    p0_pc3_s_dp[0][0][0]=0.0262583; p1_pc3_s_dp[0][0][0]=-0.0748201; p2_pc3_s_dp[0][0][0]=0.0742573; p3_pc3_s_dp[0][0][0]=-0.029009; p4_pc3_s_dp[0][0][0]=0.00396054; 
    p0_pc3_m_dp[0][0][1][0]=0.00183024; p1_pc3_m_dp[0][0][1][0]=-0.00195858; p2_pc3_m_dp[0][0][1][0]=0.00219197; p3_pc3_m_dp[0][0][1][0]=-0.00125257; p4_pc3_m_dp[0][0][1][0]=0.000160741; 
    p0_pc3_m_dp[1][0][1][0]=0.00183024; p1_pc3_m_dp[1][0][1][0]=-0.00195858; p2_pc3_m_dp[1][0][1][0]=0.00219197; p3_pc3_m_dp[1][0][1][0]=-0.00125257; p4_pc3_m_dp[1][0][1][0]=0.000160741; 
    p0_pc3_s_dp[0][1][0]=0.0033086; p1_pc3_s_dp[0][1][0]=-0.00263665; p2_pc3_s_dp[0][1][0]=0.00177236; p3_pc3_s_dp[0][1][0]=8.30503e-05; p4_pc3_s_dp[0][1][0]=-9.27482e-05; 
    p0_pc3_m_dp[0][0][2][0]=-0.0002296; p1_pc3_m_dp[0][0][2][0]=0.00457421; p2_pc3_m_dp[0][0][2][0]=-0.004469; p3_pc3_m_dp[0][0][2][0]=0.00182531; p4_pc3_m_dp[0][0][2][0]=-0.000292947; 
    p0_pc3_m_dp[1][0][2][0]=-0.0002296; p1_pc3_m_dp[1][0][2][0]=0.00457421; p2_pc3_m_dp[1][0][2][0]=-0.004469; p3_pc3_m_dp[1][0][2][0]=0.00182531; p4_pc3_m_dp[1][0][2][0]=-0.000292947; 
    p0_pc3_s_dp[0][2][0]=0.0013151; p1_pc3_s_dp[0][2][0]=0.00305752; p2_pc3_s_dp[0][2][0]=-0.00340079; p3_pc3_s_dp[0][2][0]=0.00183489; p4_pc3_s_dp[0][2][0]=-0.000286403; 
    p0_pc3_m_dp[0][0][3][0]=0.00172823; p1_pc3_m_dp[0][0][3][0]=-0.000582403; p2_pc3_m_dp[0][0][3][0]=0.000846002; p3_pc3_m_dp[0][0][3][0]=-0.000326373; p4_pc3_m_dp[0][0][3][0]=3.55976e-05; 
    p0_pc3_m_dp[1][0][3][0]=0.00172823; p1_pc3_m_dp[1][0][3][0]=-0.000582403; p2_pc3_m_dp[1][0][3][0]=0.000846002; p3_pc3_m_dp[1][0][3][0]=-0.000326373; p4_pc3_m_dp[1][0][3][0]=3.55976e-05; 
    p0_pc3_s_dp[0][3][0]=-0.00228545; p1_pc3_s_dp[0][3][0]=0.0149174; p2_pc3_s_dp[0][3][0]=-0.0163; p3_pc3_s_dp[0][3][0]=0.00732004; p4_pc3_s_dp[0][3][0]=-0.0010865; 
    p0_pc3_m_dp[0][0][4][0]=0.000982079; p1_pc3_m_dp[0][0][4][0]=0.00123037; p2_pc3_m_dp[0][0][4][0]=-0.000742456; p3_pc3_m_dp[0][0][4][0]=0.000323728; p4_pc3_m_dp[0][0][4][0]=-5.02055e-05; 
    p0_pc3_m_dp[1][0][4][0]=0.000982079; p1_pc3_m_dp[1][0][4][0]=0.00123037; p2_pc3_m_dp[1][0][4][0]=-0.000742456; p3_pc3_m_dp[1][0][4][0]=0.000323728; p4_pc3_m_dp[1][0][4][0]=-5.02055e-05; 
    p0_pc3_s_dp[0][4][0]=0.0017306; p1_pc3_s_dp[0][4][0]=0.00138811; p2_pc3_s_dp[0][4][0]=-0.00108554; p3_pc3_s_dp[0][4][0]=0.00063748; p4_pc3_s_dp[0][4][0]=-9.86753e-05; 
    p0_pc3_m_dp[0][0][5][0]=0.00126596; p1_pc3_m_dp[0][0][5][0]=-2.47819e-06; p2_pc3_m_dp[0][0][5][0]=-2.58186e-05; p3_pc3_m_dp[0][0][5][0]=0.000210347; p4_pc3_m_dp[0][0][5][0]=-5.30356e-05; 
    p0_pc3_m_dp[1][0][5][0]=0.00126596; p1_pc3_m_dp[1][0][5][0]=-2.47819e-06; p2_pc3_m_dp[1][0][5][0]=-2.58186e-05; p3_pc3_m_dp[1][0][5][0]=0.000210347; p4_pc3_m_dp[1][0][5][0]=-5.30356e-05; 
    p0_pc3_s_dp[0][5][0]=0.00131144; p1_pc3_s_dp[0][5][0]=0.00270986; p2_pc3_s_dp[0][5][0]=-0.0020928; p3_pc3_s_dp[0][5][0]=0.000908602; p4_pc3_s_dp[0][5][0]=-0.000119147; 
    p0_pc3_m_dp[0][0][6][0]=0.000789495; p1_pc3_m_dp[0][0][6][0]=0.00115625; p2_pc3_m_dp[0][0][6][0]=-0.00120182; p3_pc3_m_dp[0][0][6][0]=0.000599657; p4_pc3_m_dp[0][0][6][0]=-0.000105627; 
    p0_pc3_m_dp[1][0][6][0]=0.000789495; p1_pc3_m_dp[1][0][6][0]=0.00115625; p2_pc3_m_dp[1][0][6][0]=-0.00120182; p3_pc3_m_dp[1][0][6][0]=0.000599657; p4_pc3_m_dp[1][0][6][0]=-0.000105627; 
    p0_pc3_s_dp[0][6][0]=0.00135179; p1_pc3_s_dp[0][6][0]=0.00254099; p2_pc3_s_dp[0][6][0]=-0.00201573; p3_pc3_s_dp[0][6][0]=0.000909864; p4_pc3_s_dp[0][6][0]=-0.000114489; 
    p0_pc3_m_dp[0][0][7][0]=0.00185708; p1_pc3_m_dp[0][0][7][0]=-0.0018708; p2_pc3_m_dp[0][0][7][0]=0.00155303; p3_pc3_m_dp[0][0][7][0]=-0.000562362; p4_pc3_m_dp[0][0][7][0]=4.21588e-05; 
    p0_pc3_m_dp[1][0][7][0]=0.00185708; p1_pc3_m_dp[1][0][7][0]=-0.0018708; p2_pc3_m_dp[1][0][7][0]=0.00155303; p3_pc3_m_dp[1][0][7][0]=-0.000562362; p4_pc3_m_dp[1][0][7][0]=4.21588e-05; 
    p0_pc3_s_dp[0][7][0]=0.00273837; p1_pc3_s_dp[0][7][0]=-0.00105514; p2_pc3_s_dp[0][7][0]=0.0010188; p3_pc3_s_dp[0][7][0]=-9.05431e-05; p4_pc3_s_dp[0][7][0]=1.0707e-06; 
    p0_pc3_m_dp[0][0][8][0]=0.00293118; p1_pc3_m_dp[0][0][8][0]=-0.00533937; p2_pc3_m_dp[0][0][8][0]=0.00523126; p3_pc3_m_dp[0][0][8][0]=-0.00245406; p4_pc3_m_dp[0][0][8][0]=0.000328454; 
    p0_pc3_m_dp[1][0][8][0]=0.00293118; p1_pc3_m_dp[1][0][8][0]=-0.00533937; p2_pc3_m_dp[1][0][8][0]=0.00523126; p3_pc3_m_dp[1][0][8][0]=-0.00245406; p4_pc3_m_dp[1][0][8][0]=0.000328454; 
    p0_pc3_s_dp[0][8][0]=0.000480018; p1_pc3_s_dp[0][8][0]=0.00607964; p2_pc3_s_dp[0][8][0]=-0.00709673; p3_pc3_s_dp[0][8][0]=0.00376293; p4_pc3_s_dp[0][8][0]=-0.000625508; 
    p0_pc3_m_dp[0][0][9][0]=0.00432515; p1_pc3_m_dp[0][0][9][0]=-0.00958764; p2_pc3_m_dp[0][0][9][0]=0.00968302; p3_pc3_m_dp[0][0][9][0]=-0.00485039; p4_pc3_m_dp[0][0][9][0]=0.00068612; 
    p0_pc3_m_dp[1][0][9][0]=0.00432515; p1_pc3_m_dp[1][0][9][0]=-0.00958764; p2_pc3_m_dp[1][0][9][0]=0.00968302; p3_pc3_m_dp[1][0][9][0]=-0.00485039; p4_pc3_m_dp[1][0][9][0]=0.00068612; 
    p0_pc3_s_dp[0][9][0]=0.000310507; p1_pc3_s_dp[0][9][0]=0.00693955; p2_pc3_s_dp[0][9][0]=-0.00873968; p3_pc3_s_dp[0][9][0]=0.00487837; p4_pc3_s_dp[0][9][0]=-0.000848975; 
    p0_pc3_m_dp[0][1][0][0]=-0.000456737; p1_pc3_m_dp[0][1][0][0]=-0.00202124; p2_pc3_m_dp[0][1][0][0]=0.00254276; p3_pc3_m_dp[0][1][0][0]=-0.00199788; p4_pc3_m_dp[0][1][0][0]=0.000284731; 
    p0_pc3_m_dp[1][1][0][0]=-0.000456737; p1_pc3_m_dp[1][1][0][0]=-0.00202124; p2_pc3_m_dp[1][1][0][0]=0.00254276; p3_pc3_m_dp[1][1][0][0]=-0.00199788; p4_pc3_m_dp[1][1][0][0]=0.000284731; 
    p0_pc3_s_dp[1][0][0]=-0.000978848; p1_pc3_s_dp[1][0][0]=0.0129059; p2_pc3_s_dp[1][0][0]=-0.0149798; p3_pc3_s_dp[1][0][0]=0.00756959; p4_pc3_s_dp[1][0][0]=-0.00126295; 
    p0_pc3_m_dp[0][1][1][0]=-0.000651972; p1_pc3_m_dp[0][1][1][0]=-0.0010059; p2_pc3_m_dp[0][1][1][0]=0.00122764; p3_pc3_m_dp[0][1][1][0]=-0.000877041; p4_pc3_m_dp[0][1][1][0]=0.000113435; 
    p0_pc3_m_dp[1][1][1][0]=-0.000651972; p1_pc3_m_dp[1][1][1][0]=-0.0010059; p2_pc3_m_dp[1][1][1][0]=0.00122764; p3_pc3_m_dp[1][1][1][0]=-0.000877041; p4_pc3_m_dp[1][1][1][0]=0.000113435; 
    p0_pc3_s_dp[1][1][0]=-0.00194511; p1_pc3_s_dp[1][1][0]=0.0148293; p2_pc3_s_dp[1][1][0]=-0.0156525; p3_pc3_s_dp[1][1][0]=0.00725363; p4_pc3_s_dp[1][1][0]=-0.00113792; 
    p0_pc3_m_dp[0][1][2][0]=-0.000232082; p1_pc3_m_dp[0][1][2][0]=-0.00199702; p2_pc3_m_dp[0][1][2][0]=0.00190446; p3_pc3_m_dp[0][1][2][0]=-0.000759839; p4_pc3_m_dp[0][1][2][0]=7.23323e-05; 
    p0_pc3_m_dp[1][1][2][0]=-0.000232082; p1_pc3_m_dp[1][1][2][0]=-0.00199702; p2_pc3_m_dp[1][1][2][0]=0.00190446; p3_pc3_m_dp[1][1][2][0]=-0.000759839; p4_pc3_m_dp[1][1][2][0]=7.23323e-05; 
    p0_pc3_s_dp[1][2][0]=0.000547047; p1_pc3_s_dp[1][2][0]=0.0065805; p2_pc3_s_dp[1][2][0]=-0.00666232; p3_pc3_s_dp[1][2][0]=0.00321476; p4_pc3_s_dp[1][2][0]=-0.000509343; 
    p0_pc3_m_dp[0][1][3][0]=-0.000493252; p1_pc3_m_dp[0][1][3][0]=-0.00087034; p2_pc3_m_dp[0][1][3][0]=0.000572313; p3_pc3_m_dp[0][1][3][0]=-1.76282e-05; p4_pc3_m_dp[0][1][3][0]=-3.40474e-05; 
    p0_pc3_m_dp[1][1][3][0]=-0.000493252; p1_pc3_m_dp[1][1][3][0]=-0.00087034; p2_pc3_m_dp[1][1][3][0]=0.000572313; p3_pc3_m_dp[1][1][3][0]=-1.76282e-05; p4_pc3_m_dp[1][1][3][0]=-3.40474e-05; 
    p0_pc3_s_dp[1][3][0]=0.00114183; p1_pc3_s_dp[1][3][0]=0.00356652; p2_pc3_s_dp[1][3][0]=-0.00286049; p3_pc3_s_dp[1][3][0]=0.00131548; p4_pc3_s_dp[1][3][0]=-0.000190019; 
    p0_pc3_m_dp[0][1][4][0]=-0.000314632; p1_pc3_m_dp[0][1][4][0]=-0.0014618; p2_pc3_m_dp[0][1][4][0]=0.0013568; p3_pc3_m_dp[0][1][4][0]=-0.000331731; p4_pc3_m_dp[0][1][4][0]=2.01659e-05; 
    p0_pc3_m_dp[1][1][4][0]=-0.000314632; p1_pc3_m_dp[1][1][4][0]=-0.0014618; p2_pc3_m_dp[1][1][4][0]=0.0013568; p3_pc3_m_dp[1][1][4][0]=-0.000331731; p4_pc3_m_dp[1][1][4][0]=2.01659e-05; 
    p0_pc3_s_dp[1][4][0]=0.00101785; p1_pc3_s_dp[1][4][0]=0.0037826; p2_pc3_s_dp[1][4][0]=-0.00270687; p3_pc3_s_dp[1][4][0]=0.00108672; p4_pc3_s_dp[1][4][0]=-0.000140022; 
    p0_pc3_m_dp[0][1][5][0]=-0.000275509; p1_pc3_m_dp[0][1][5][0]=-0.00101596; p2_pc3_m_dp[0][1][5][0]=0.000980916; p3_pc3_m_dp[0][1][5][0]=-0.000249271; p4_pc3_m_dp[0][1][5][0]=1.80664e-05; 
    p0_pc3_m_dp[1][1][5][0]=-0.000275509; p1_pc3_m_dp[1][1][5][0]=-0.00101596; p2_pc3_m_dp[1][1][5][0]=0.000980916; p3_pc3_m_dp[1][1][5][0]=-0.000249271; p4_pc3_m_dp[1][1][5][0]=1.80664e-05; 
    p0_pc3_s_dp[1][5][0]=0.000865994; p1_pc3_s_dp[1][5][0]=0.00456421; p2_pc3_s_dp[1][5][0]=-0.00367246; p3_pc3_s_dp[1][5][0]=0.00151106; p4_pc3_s_dp[1][5][0]=-0.000197619; 
    p0_pc3_m_dp[0][1][6][0]=-0.00206801; p1_pc3_m_dp[0][1][6][0]=0.00426484; p2_pc3_m_dp[0][1][6][0]=-0.00462457; p3_pc3_m_dp[0][1][6][0]=0.00209088; p4_pc3_m_dp[0][1][6][0]=-0.000327889; 
    p0_pc3_m_dp[1][1][6][0]=-0.00206801; p1_pc3_m_dp[1][1][6][0]=0.00426484; p2_pc3_m_dp[1][1][6][0]=-0.00462457; p3_pc3_m_dp[1][1][6][0]=0.00209088; p4_pc3_m_dp[1][1][6][0]=-0.000327889; 
    p0_pc3_s_dp[1][6][0]=0.00116217; p1_pc3_s_dp[1][6][0]=0.00388907; p2_pc3_s_dp[1][6][0]=-0.00324142; p3_pc3_s_dp[1][6][0]=0.00144413; p4_pc3_s_dp[1][6][0]=-0.000202517; 
    p0_pc3_m_dp[0][1][7][0]=0.000308068; p1_pc3_m_dp[0][1][7][0]=-0.00342382; p2_pc3_m_dp[0][1][7][0]=0.00314165; p3_pc3_m_dp[0][1][7][0]=-0.00126405; p4_pc3_m_dp[0][1][7][0]=0.000151706; 
    p0_pc3_m_dp[1][1][7][0]=0.000308068; p1_pc3_m_dp[1][1][7][0]=-0.00342382; p2_pc3_m_dp[1][1][7][0]=0.00314165; p3_pc3_m_dp[1][1][7][0]=-0.00126405; p4_pc3_m_dp[1][1][7][0]=0.000151706; 
    p0_pc3_s_dp[1][7][0]=0.00072046; p1_pc3_s_dp[1][7][0]=0.00636314; p2_pc3_s_dp[1][7][0]=-0.00706262; p3_pc3_s_dp[1][7][0]=0.00350311; p4_pc3_s_dp[1][7][0]=-0.000554513; 
    p0_pc3_m_dp[0][1][8][0]=0.0157918; p1_pc3_m_dp[0][1][8][0]=-0.0478267; p2_pc3_m_dp[0][1][8][0]=0.0453921; p3_pc3_m_dp[0][1][8][0]=-0.0180427; p4_pc3_m_dp[0][1][8][0]=0.0024706; 
    p0_pc3_m_dp[1][1][8][0]=0.0157918; p1_pc3_m_dp[1][1][8][0]=-0.0478267; p2_pc3_m_dp[1][1][8][0]=0.0453921; p3_pc3_m_dp[1][1][8][0]=-0.0180427; p4_pc3_m_dp[1][1][8][0]=0.0024706; 
    p0_pc3_s_dp[1][8][0]=0.000172608; p1_pc3_s_dp[1][8][0]=0.0085359; p2_pc3_s_dp[1][8][0]=-0.0102481; p3_pc3_s_dp[1][8][0]=0.00529455; p4_pc3_s_dp[1][8][0]=-0.000876227; 
    p0_pc3_m_dp[0][1][9][0]=0.000853083; p1_pc3_m_dp[0][1][9][0]=-0.00619373; p2_pc3_m_dp[0][1][9][0]=0.00606978; p3_pc3_m_dp[0][1][9][0]=-0.0032273; p4_pc3_m_dp[0][1][9][0]=0.000446444; 
    p0_pc3_m_dp[1][1][9][0]=0.000853083; p1_pc3_m_dp[1][1][9][0]=-0.00619373; p2_pc3_m_dp[1][1][9][0]=0.00606978; p3_pc3_m_dp[1][1][9][0]=-0.0032273; p4_pc3_m_dp[1][1][9][0]=0.000446444; 
    p0_pc3_s_dp[1][9][0]=-0.000242705; p1_pc3_s_dp[1][9][0]=0.0102262; p2_pc3_s_dp[1][9][0]=-0.0127363; p3_pc3_s_dp[1][9][0]=0.00674644; p4_pc3_s_dp[1][9][0]=-0.00114583; 
	
    //PC3 DPHI POS 
	
    p0_pc3_m_dp[0][0][0][1]=-0.000621156; p1_pc3_m_dp[0][0][0][1]=0.00508542; p2_pc3_m_dp[0][0][0][1]=-0.00535713; p3_pc3_m_dp[0][0][0][1]=0.00299098; p4_pc3_m_dp[0][0][0][1]=-0.000416553; 
    p0_pc3_m_dp[1][0][0][1]=-0.000621156; p1_pc3_m_dp[1][0][0][1]=0.00508542; p2_pc3_m_dp[1][0][0][1]=-0.00535713; p3_pc3_m_dp[1][0][0][1]=0.00299098; p4_pc3_m_dp[1][0][0][1]=-0.000416553; 
    p0_pc3_s_dp[0][0][1]=0.000170163; p1_pc3_s_dp[0][0][1]=0.00730025; p2_pc3_s_dp[0][0][1]=-0.00899284; p3_pc3_s_dp[0][0][1]=0.00506261; p4_pc3_s_dp[0][0][1]=-0.000895807; 
    p0_pc3_m_dp[0][0][1][1]=-0.000967484; p1_pc3_m_dp[0][0][1][1]=0.0052911; p2_pc3_m_dp[0][0][1][1]=-0.0053113; p3_pc3_m_dp[0][0][1][1]=0.00243059; p4_pc3_m_dp[0][0][1][1]=-0.000322519; 
    p0_pc3_m_dp[1][0][1][1]=-0.000967484; p1_pc3_m_dp[1][0][1][1]=0.0052911; p2_pc3_m_dp[1][0][1][1]=-0.0053113; p3_pc3_m_dp[1][0][1][1]=0.00243059; p4_pc3_m_dp[1][0][1][1]=-0.000322519; 
    p0_pc3_s_dp[0][1][1]=0.000736964; p1_pc3_s_dp[0][1][1]=0.00482098; p2_pc3_s_dp[0][1][1]=-0.0056285; p3_pc3_s_dp[0][1][1]=0.00329842; p4_pc3_s_dp[0][1][1]=-0.000592676; 
    p0_pc3_m_dp[0][0][2][1]=-0.000239186; p1_pc3_m_dp[0][0][2][1]=0.00280005; p2_pc3_m_dp[0][0][2][1]=-0.00284627; p3_pc3_m_dp[0][0][2][1]=0.00121115; p4_pc3_m_dp[0][0][2][1]=-0.000152817; 
    p0_pc3_m_dp[1][0][2][1]=-0.000239186; p1_pc3_m_dp[1][0][2][1]=0.00280005; p2_pc3_m_dp[1][0][2][1]=-0.00284627; p3_pc3_m_dp[1][0][2][1]=0.00121115; p4_pc3_m_dp[1][0][2][1]=-0.000152817; 
    p0_pc3_s_dp[0][2][1]=0.00234719; p1_pc3_s_dp[0][2][1]=-0.00100765; p2_pc3_s_dp[0][2][1]=0.00134919; p3_pc3_s_dp[0][2][1]=-9.51205e-05; p4_pc3_s_dp[0][2][1]=-3.39659e-05; 
    p0_pc3_m_dp[0][0][3][1]=-0.000509532; p1_pc3_m_dp[0][0][3][1]=0.00360961; p2_pc3_m_dp[0][0][3][1]=-0.00385639; p3_pc3_m_dp[0][0][3][1]=0.00157904; p4_pc3_m_dp[0][0][3][1]=-0.000220651; 
    p0_pc3_m_dp[1][0][3][1]=-0.000509532; p1_pc3_m_dp[1][0][3][1]=0.00360961; p2_pc3_m_dp[1][0][3][1]=-0.00385639; p3_pc3_m_dp[1][0][3][1]=0.00157904; p4_pc3_m_dp[1][0][3][1]=-0.000220651; 
    p0_pc3_s_dp[0][3][1]=0.00299229; p1_pc3_s_dp[0][3][1]=-0.00347952; p2_pc3_s_dp[0][3][1]=0.00448524; p3_pc3_s_dp[0][3][1]=-0.0016759; p4_pc3_s_dp[0][3][1]=0.000230938; 
    p0_pc3_m_dp[0][0][4][1]=0.000136762; p1_pc3_m_dp[0][0][4][1]=0.0014299; p2_pc3_m_dp[0][0][4][1]=-0.00163177; p3_pc3_m_dp[0][0][4][1]=0.000576484; p4_pc3_m_dp[0][0][4][1]=-6.98859e-05; 
    p0_pc3_m_dp[1][0][4][1]=0.000136762; p1_pc3_m_dp[1][0][4][1]=0.0014299; p2_pc3_m_dp[1][0][4][1]=-0.00163177; p3_pc3_m_dp[1][0][4][1]=0.000576484; p4_pc3_m_dp[1][0][4][1]=-6.98859e-05; 
    p0_pc3_s_dp[0][4][1]=0.00293302; p1_pc3_s_dp[0][4][1]=-0.0032161; p2_pc3_s_dp[0][4][1]=0.00416971; p3_pc3_s_dp[0][4][1]=-0.00149647; p4_pc3_s_dp[0][4][1]=0.000189398; 
    p0_pc3_m_dp[0][0][5][1]=6.07237e-05; p1_pc3_m_dp[0][0][5][1]=0.00113897; p2_pc3_m_dp[0][0][5][1]=-0.00135816; p3_pc3_m_dp[0][0][5][1]=0.000549684; p4_pc3_m_dp[0][0][5][1]=-8.06819e-05; 
    p0_pc3_m_dp[1][0][5][1]=6.07237e-05; p1_pc3_m_dp[1][0][5][1]=0.00113897; p2_pc3_m_dp[1][0][5][1]=-0.00135816; p3_pc3_m_dp[1][0][5][1]=0.000549684; p4_pc3_m_dp[1][0][5][1]=-8.06819e-05; 
    p0_pc3_s_dp[0][5][1]=0.00249257; p1_pc3_s_dp[0][5][1]=-0.00187523; p2_pc3_s_dp[0][5][1]=0.00305922; p3_pc3_s_dp[0][5][1]=-0.00113291; p4_pc3_s_dp[0][5][1]=0.000146668; 
    p0_pc3_m_dp[0][0][6][1]=-0.000369552; p1_pc3_m_dp[0][0][6][1]=0.00232611; p2_pc3_m_dp[0][0][6][1]=-0.0022507; p3_pc3_m_dp[0][0][6][1]=0.00090323; p4_pc3_m_dp[0][0][6][1]=-0.00012273; 
    p0_pc3_m_dp[1][0][6][1]=-0.000369552; p1_pc3_m_dp[1][0][6][1]=0.00232611; p2_pc3_m_dp[1][0][6][1]=-0.0022507; p3_pc3_m_dp[1][0][6][1]=0.00090323; p4_pc3_m_dp[1][0][6][1]=-0.00012273; 
    p0_pc3_s_dp[0][6][1]=0.00235102; p1_pc3_s_dp[0][6][1]=-0.00129325; p2_pc3_s_dp[0][6][1]=0.00242088; p3_pc3_s_dp[0][6][1]=-0.00090624; p4_pc3_s_dp[0][6][1]=0.000129947; 
    p0_pc3_m_dp[0][0][7][1]=-0.000706573; p1_pc3_m_dp[0][0][7][1]=0.00372965; p2_pc3_m_dp[0][0][7][1]=-0.0034948; p3_pc3_m_dp[0][0][7][1]=0.001488; p4_pc3_m_dp[0][0][7][1]=-0.000195345; 
    p0_pc3_m_dp[1][0][7][1]=-0.000706573; p1_pc3_m_dp[1][0][7][1]=0.00372965; p2_pc3_m_dp[1][0][7][1]=-0.0034948; p3_pc3_m_dp[1][0][7][1]=0.001488; p4_pc3_m_dp[1][0][7][1]=-0.000195345; 
    p0_pc3_s_dp[0][7][1]=0.00239357; p1_pc3_s_dp[0][7][1]=-0.000726261; p2_pc3_s_dp[0][7][1]=0.00117682; p3_pc3_s_dp[0][7][1]=-0.000120393; p4_pc3_s_dp[0][7][1]=-1.67648e-05; 
    p0_pc3_m_dp[0][0][8][1]=-0.00109729; p1_pc3_m_dp[0][0][8][1]=0.00572808; p2_pc3_m_dp[0][0][8][1]=-0.00555223; p3_pc3_m_dp[0][0][8][1]=0.00261384; p4_pc3_m_dp[0][0][8][1]=-0.000364123; 
    p0_pc3_m_dp[1][0][8][1]=-0.00109729; p1_pc3_m_dp[1][0][8][1]=0.00572808; p2_pc3_m_dp[1][0][8][1]=-0.00555223; p3_pc3_m_dp[1][0][8][1]=0.00261384; p4_pc3_m_dp[1][0][8][1]=-0.000364123; 
    p0_pc3_s_dp[0][8][1]=0.000791948; p1_pc3_s_dp[0][8][1]=0.00525702; p2_pc3_s_dp[0][8][1]=-0.00605588; p3_pc3_s_dp[0][8][1]=0.00335807; p4_pc3_s_dp[0][8][1]=-0.00058072; 
    p0_pc3_m_dp[0][0][9][1]=-0.00116999; p1_pc3_m_dp[0][0][9][1]=0.00711013; p2_pc3_m_dp[0][0][9][1]=-0.00725656; p3_pc3_m_dp[0][0][9][1]=0.00383604; p4_pc3_m_dp[0][0][9][1]=-0.000545751; 
    p0_pc3_m_dp[1][0][9][1]=-0.00116999; p1_pc3_m_dp[1][0][9][1]=0.00711013; p2_pc3_m_dp[1][0][9][1]=-0.00725656; p3_pc3_m_dp[1][0][9][1]=0.00383604; p4_pc3_m_dp[1][0][9][1]=-0.000545751; 
    p0_pc3_s_dp[0][9][1]=0.000192287; p1_pc3_s_dp[0][9][1]=0.00793324; p2_pc3_s_dp[0][9][1]=-0.00983559; p3_pc3_s_dp[0][9][1]=0.00539931; p4_pc3_s_dp[0][9][1]=-0.000937885; 
    p0_pc3_m_dp[0][1][0][1]=-0.00355333; p1_pc3_m_dp[0][1][0][1]=0.0069148; p2_pc3_m_dp[0][1][0][1]=-0.00757071; p3_pc3_m_dp[0][1][0][1]=0.00395596; p4_pc3_m_dp[0][1][0][1]=-0.000538256; 
    p0_pc3_m_dp[1][1][0][1]=-0.00355333; p1_pc3_m_dp[1][1][0][1]=0.0069148; p2_pc3_m_dp[1][1][0][1]=-0.00757071; p3_pc3_m_dp[1][1][0][1]=0.00395596; p4_pc3_m_dp[1][1][0][1]=-0.000538256; 
    p0_pc3_s_dp[1][0][1]=0.00401855; p1_pc3_s_dp[1][0][1]=-0.00269762; p2_pc3_s_dp[1][0][1]=0.00208476; p3_pc3_s_dp[1][0][1]=0.000148948; p4_pc3_s_dp[1][0][1]=-0.00016212; 
    p0_pc3_m_dp[0][1][1][1]=-0.00306235; p1_pc3_m_dp[0][1][1][1]=0.00448593; p2_pc3_m_dp[0][1][1][1]=-0.00490094; p3_pc3_m_dp[0][1][1][1]=0.00237376; p4_pc3_m_dp[0][1][1][1]=-0.000314642; 
    p0_pc3_m_dp[1][1][1][1]=-0.00306235; p1_pc3_m_dp[1][1][1][1]=0.00448593; p2_pc3_m_dp[1][1][1][1]=-0.00490094; p3_pc3_m_dp[1][1][1][1]=0.00237376; p4_pc3_m_dp[1][1][1][1]=-0.000314642; 
    p0_pc3_s_dp[1][1][1]=0.00168164; p1_pc3_s_dp[1][1][1]=0.00462931; p2_pc3_s_dp[1][1][1]=-0.00576487; p3_pc3_s_dp[1][1][1]=0.00349243; p4_pc3_s_dp[1][1][1]=-0.000649834; 
    p0_pc3_m_dp[0][1][2][1]=-0.00218264; p1_pc3_m_dp[0][1][2][1]=0.00090971; p2_pc3_m_dp[0][1][2][1]=-0.00103192; p3_pc3_m_dp[0][1][2][1]=0.000466045; p4_pc3_m_dp[0][1][2][1]=-3.05716e-05; 
    p0_pc3_m_dp[1][1][2][1]=-0.00218264; p1_pc3_m_dp[1][1][2][1]=0.00090971; p2_pc3_m_dp[1][1][2][1]=-0.00103192; p3_pc3_m_dp[1][1][2][1]=0.000466045; p4_pc3_m_dp[1][1][2][1]=-3.05716e-05; 
    p0_pc3_s_dp[1][2][1]=0.00359403; p1_pc3_s_dp[1][2][1]=-0.00206194; p2_pc3_s_dp[1][2][1]=0.00177481; p3_pc3_s_dp[1][2][1]=1.73937e-05; p4_pc3_s_dp[1][2][1]=-9.89913e-05; 
    p0_pc3_m_dp[0][1][3][1]=-0.0021854; p1_pc3_m_dp[0][1][3][1]=0.000918448; p2_pc3_m_dp[0][1][3][1]=-0.00113579; p3_pc3_m_dp[0][1][3][1]=0.000449512; p4_pc3_m_dp[0][1][3][1]=-4.45273e-05; 
    p0_pc3_m_dp[1][1][3][1]=-0.0021854; p1_pc3_m_dp[1][1][3][1]=0.000918448; p2_pc3_m_dp[1][1][3][1]=-0.00113579; p3_pc3_m_dp[1][1][3][1]=0.000449512; p4_pc3_m_dp[1][1][3][1]=-4.45273e-05; 
    p0_pc3_s_dp[1][3][1]=0.00365322; p1_pc3_s_dp[1][3][1]=-0.00304579; p2_pc3_s_dp[1][3][1]=0.00340618; p3_pc3_s_dp[1][3][1]=-0.000951298; p4_pc3_s_dp[1][3][1]=8.15016e-05; 
    p0_pc3_m_dp[0][1][4][1]=-0.00183768; p1_pc3_m_dp[0][1][4][1]=-0.000549931; p2_pc3_m_dp[0][1][4][1]=0.000398626; p3_pc3_m_dp[0][1][4][1]=-0.000257243; p4_pc3_m_dp[0][1][4][1]=5.55104e-05; 
    p0_pc3_m_dp[1][1][4][1]=-0.00183768; p1_pc3_m_dp[1][1][4][1]=-0.000549931; p2_pc3_m_dp[1][1][4][1]=0.000398626; p3_pc3_m_dp[1][1][4][1]=-0.000257243; p4_pc3_m_dp[1][1][4][1]=5.55104e-05; 
    p0_pc3_s_dp[1][4][1]=0.00319822; p1_pc3_s_dp[1][4][1]=-0.00170408; p2_pc3_s_dp[1][4][1]=0.00242893; p3_pc3_s_dp[1][4][1]=-0.000741612; p4_pc3_s_dp[1][4][1]=7.71677e-05; 
    p0_pc3_m_dp[0][1][5][1]=-0.00223439; p1_pc3_m_dp[0][1][5][1]=0.00136059; p2_pc3_m_dp[0][1][5][1]=-0.0012562; p3_pc3_m_dp[0][1][5][1]=0.000312161; p4_pc3_m_dp[0][1][5][1]=-1.32068e-05; 
    p0_pc3_m_dp[1][1][5][1]=-0.00223439; p1_pc3_m_dp[1][1][5][1]=0.00136059; p2_pc3_m_dp[1][1][5][1]=-0.0012562; p3_pc3_m_dp[1][1][5][1]=0.000312161; p4_pc3_m_dp[1][1][5][1]=-1.32068e-05; 
    p0_pc3_s_dp[1][5][1]=0.00311207; p1_pc3_s_dp[1][5][1]=-0.00164475; p2_pc3_s_dp[1][5][1]=0.002427; p3_pc3_s_dp[1][5][1]=-0.000772579; p4_pc3_s_dp[1][5][1]=8.69525e-05; 
    p0_pc3_m_dp[0][1][6][1]=-0.00184615; p1_pc3_m_dp[0][1][6][1]=0.000571284; p2_pc3_m_dp[0][1][6][1]=-0.000328595; p3_pc3_m_dp[0][1][6][1]=-1.9536e-05; p4_pc3_m_dp[0][1][6][1]=3.1385e-05; 
    p0_pc3_m_dp[1][1][6][1]=-0.00184615; p1_pc3_m_dp[1][1][6][1]=0.000571284; p2_pc3_m_dp[1][1][6][1]=-0.000328595; p3_pc3_m_dp[1][1][6][1]=-1.9536e-05; p4_pc3_m_dp[1][1][6][1]=3.1385e-05; 
    p0_pc3_s_dp[1][6][1]=0.00263897; p1_pc3_s_dp[1][6][1]=-0.000345232; p2_pc3_s_dp[1][6][1]=0.00101931; p3_pc3_s_dp[1][6][1]=-0.000158599; p4_pc3_s_dp[1][6][1]=-4.11687e-06; 
    p0_pc3_m_dp[0][1][7][1]=-0.0017442; p1_pc3_m_dp[0][1][7][1]=0.000425482; p2_pc3_m_dp[0][1][7][1]=-9.91785e-05; p3_pc3_m_dp[0][1][7][1]=-1.04136e-05; p4_pc3_m_dp[0][1][7][1]=3.94218e-05; 
    p0_pc3_m_dp[1][1][7][1]=-0.0017442; p1_pc3_m_dp[1][1][7][1]=0.000425482; p2_pc3_m_dp[1][1][7][1]=-9.91785e-05; p3_pc3_m_dp[1][1][7][1]=-1.04136e-05; p4_pc3_m_dp[1][1][7][1]=3.94218e-05; 
    p0_pc3_s_dp[1][7][1]=0.00321572; p1_pc3_s_dp[1][7][1]=-0.00152836; p2_pc3_s_dp[1][7][1]=0.00148635; p3_pc3_s_dp[1][7][1]=-3.93881e-05; p4_pc3_s_dp[1][7][1]=-6.09819e-05; 
    p0_pc3_m_dp[0][1][8][1]=-0.00190561; p1_pc3_m_dp[0][1][8][1]=0.000963752; p2_pc3_m_dp[0][1][8][1]=-0.000618309; p3_pc3_m_dp[0][1][8][1]=0.000444059; p4_pc3_m_dp[0][1][8][1]=-2.92524e-05; 
    p0_pc3_m_dp[1][1][8][1]=-0.00190561; p1_pc3_m_dp[1][1][8][1]=0.000963752; p2_pc3_m_dp[1][1][8][1]=-0.000618309; p3_pc3_m_dp[1][1][8][1]=0.000444059; p4_pc3_m_dp[1][1][8][1]=-2.92524e-05; 
    p0_pc3_s_dp[1][8][1]=0.00116664; p1_pc3_s_dp[1][8][1]=0.00511563; p2_pc3_s_dp[1][8][1]=-0.00616188; p3_pc3_s_dp[1][8][1]=0.00356516; p4_pc3_s_dp[1][8][1]=-0.000639776; 
    p0_pc3_m_dp[0][1][9][1]=-0.00145889; p1_pc3_m_dp[0][1][9][1]=0.000429065; p2_pc3_m_dp[0][1][9][1]=-0.000336313; p3_pc3_m_dp[0][1][9][1]=0.000834037; p4_pc3_m_dp[0][1][9][1]=-8.97328e-05; 
    p0_pc3_m_dp[1][1][9][1]=-0.00145889; p1_pc3_m_dp[1][1][9][1]=0.000429065; p2_pc3_m_dp[1][1][9][1]=-0.000336313; p3_pc3_m_dp[1][1][9][1]=0.000834037; p4_pc3_m_dp[1][1][9][1]=-8.97328e-05; 
    p0_pc3_s_dp[1][9][1]=0.00030836; p1_pc3_s_dp[1][9][1]=0.0078768; p2_pc3_s_dp[1][9][1]=-0.0094105; p3_pc3_s_dp[1][9][1]=0.0051618; p4_pc3_s_dp[1][9][1]=-0.000906153;

    //---------------------------------------------
    // After Burn Sigmaized Variables
    //--------------------------------------------------------
    //EMC//DPHI//27//
       
    p0_emc_m_sdp[0][0][0][0]=-1.35445; p1_emc_m_sdp[0][0][0][0]=3.35838; p2_emc_m_sdp[0][0][0][0]=-2.77591; p3_emc_m_sdp[0][0][0][0]=0.888348; p4_emc_m_sdp[0][0][0][0]=-0.0860104; 
    p0_emc_s_sdp[0][0][0][0]=0.0912041; p1_emc_s_sdp[0][0][0][0]=1.3795; p2_emc_s_sdp[0][0][0][0]=-0.976106; p3_emc_s_sdp[0][0][0][0]=0.281363; p4_emc_s_sdp[0][0][0][0]=-0.0230836; 
    p0_emc_m_sdp[1][0][0][0]=3.85542; p1_emc_m_sdp[1][0][0][0]=-15.7893; p2_emc_m_sdp[1][0][0][0]=5.40504; p3_emc_m_sdp[1][0][0][0]=41.5562; p4_emc_m_sdp[1][0][0][0]=-41.1328; 
    p0_emc_s_sdp[1][0][0][0]=-7.45718; p1_emc_s_sdp[1][0][0][0]=31.3893; p2_emc_s_sdp[1][0][0][0]=-8.59463; p3_emc_s_sdp[1][0][0][0]=-77.9962; p4_emc_s_sdp[1][0][0][0]=71.667; 
    p0_emc_m_sdp[0][0][1][0]=-1.40102; p1_emc_m_sdp[0][0][1][0]=3.51783; p2_emc_m_sdp[0][0][1][0]=-2.92335; p3_emc_m_sdp[0][0][1][0]=0.946284; p4_emc_m_sdp[0][0][1][0]=-0.0943265; 
    p0_emc_s_sdp[0][0][1][0]=0.294593; p1_emc_s_sdp[0][0][1][0]=1.12978; p2_emc_s_sdp[0][0][1][0]=-0.925577; p3_emc_s_sdp[0][0][1][0]=0.315875; p4_emc_s_sdp[0][0][1][0]=-0.0355597; 
    p0_emc_m_sdp[1][0][1][0]=-0.0588082; p1_emc_m_sdp[1][0][1][0]=0.0926078; p2_emc_m_sdp[1][0][1][0]=0.198104; p3_emc_m_sdp[1][0][1][0]=0.0422409; p4_emc_m_sdp[1][0][1][0]=-0.803474; 
    p0_emc_s_sdp[1][0][1][0]=8.19962; p1_emc_s_sdp[1][0][1][0]=-29.2061; p2_emc_s_sdp[1][0][1][0]=7.93079; p3_emc_s_sdp[1][0][1][0]=79.0936; p4_emc_s_sdp[1][0][1][0]=-76.1266; 
    p0_emc_m_sdp[0][0][2][0]=-0.918283; p1_emc_m_sdp[0][0][2][0]=2.30355; p2_emc_m_sdp[0][0][2][0]=-1.85989; p3_emc_m_sdp[0][0][2][0]=0.579731; p4_emc_m_sdp[0][0][2][0]=-0.0543892; 
    p0_emc_s_sdp[0][0][2][0]=0.483355; p1_emc_s_sdp[0][0][2][0]=0.753194; p2_emc_s_sdp[0][0][2][0]=-0.661865; p3_emc_s_sdp[0][0][2][0]=0.232813; p4_emc_s_sdp[0][0][2][0]=-0.0262867; 
    p0_emc_m_sdp[1][0][2][0]=-2.08626; p1_emc_m_sdp[1][0][2][0]=7.42011; p2_emc_m_sdp[1][0][2][0]=-0.416407; p3_emc_m_sdp[1][0][2][0]=-21.6998; p4_emc_m_sdp[1][0][2][0]=18.9408; 
    p0_emc_s_sdp[1][0][2][0]=4.48198; p1_emc_s_sdp[1][0][2][0]=-15.6472; p2_emc_s_sdp[1][0][2][0]=5.54758; p3_emc_s_sdp[1][0][2][0]=44.2363; p4_emc_s_sdp[1][0][2][0]=-45.5269; 
    p0_emc_m_sdp[0][0][3][0]=-0.607807; p1_emc_m_sdp[0][0][3][0]=1.6254; p2_emc_m_sdp[0][0][3][0]=-1.33168; p3_emc_m_sdp[0][0][3][0]=0.423628; p4_emc_m_sdp[0][0][3][0]=-0.0415845; 
    p0_emc_s_sdp[0][0][3][0]=0.64664; p1_emc_s_sdp[0][0][3][0]=0.23221; p2_emc_s_sdp[0][0][3][0]=-0.186902; p3_emc_s_sdp[0][0][3][0]=0.0734735; p4_emc_s_sdp[0][0][3][0]=-0.010238; 
    p0_emc_m_sdp[1][0][3][0]=-4.20166; p1_emc_m_sdp[1][0][3][0]=17.4634; p2_emc_m_sdp[1][0][3][0]=-6.28731; p3_emc_m_sdp[1][0][3][0]=-49.7331; p4_emc_m_sdp[1][0][3][0]=51.7005; 
    p0_emc_s_sdp[1][0][3][0]=-6.14785; p1_emc_s_sdp[1][0][3][0]=28.2162; p2_emc_s_sdp[1][0][3][0]=-9.88637; p3_emc_s_sdp[1][0][3][0]=-76.8043; p4_emc_s_sdp[1][0][3][0]=77.4509; 
    p0_emc_m_sdp[0][0][4][0]=0.0423573; p1_emc_m_sdp[0][0][4][0]=0.0679043; p2_emc_m_sdp[0][0][4][0]=-0.135674; p3_emc_m_sdp[0][0][4][0]=0.0892305; p4_emc_m_sdp[0][0][4][0]=-0.018156; 
    p0_emc_s_sdp[0][0][4][0]=0.712602; p1_emc_s_sdp[0][0][4][0]=0.0711131; p2_emc_s_sdp[0][0][4][0]=-0.0509759; p3_emc_s_sdp[0][0][4][0]=0.010308; p4_emc_s_sdp[0][0][4][0]=0.00177847; 
    p0_emc_m_sdp[1][0][4][0]=-0.0272267; p1_emc_m_sdp[1][0][4][0]=-0.0972883; p2_emc_m_sdp[1][0][4][0]=-0.0970057; p3_emc_m_sdp[1][0][4][0]=0.096501; p4_emc_m_sdp[1][0][4][0]=0.66861; 
    p0_emc_s_sdp[1][0][4][0]=-5.20965; p1_emc_s_sdp[1][0][4][0]=24.5615; p2_emc_s_sdp[1][0][4][0]=-9.01166; p3_emc_s_sdp[1][0][4][0]=-67.211; p4_emc_s_sdp[1][0][4][0]=68.8141; 
    p0_emc_m_sdp[0][0][5][0]=-0.273928; p1_emc_m_sdp[0][0][5][0]=0.809196; p2_emc_m_sdp[0][0][5][0]=-0.74449; p3_emc_m_sdp[0][0][5][0]=0.27359; p4_emc_m_sdp[0][0][5][0]=-0.0329818; 
    p0_emc_s_sdp[0][0][5][0]=0.984911; p1_emc_s_sdp[0][0][5][0]=-0.592514; p2_emc_s_sdp[0][0][5][0]=0.559083; p3_emc_s_sdp[0][0][5][0]=-0.205892; p4_emc_s_sdp[0][0][5][0]=0.0237129; 
    p0_emc_m_sdp[1][0][5][0]=0.168239; p1_emc_m_sdp[1][0][5][0]=-0.208095; p2_emc_m_sdp[1][0][5][0]=-0.531837; p3_emc_m_sdp[1][0][5][0]=-0.271976; p4_emc_m_sdp[1][0][5][0]=1.65404; 
    p0_emc_s_sdp[1][0][5][0]=1.10398; p1_emc_s_sdp[1][0][5][0]=-0.300276; p2_emc_s_sdp[1][0][5][0]=-1.65841; p3_emc_s_sdp[1][0][5][0]=-1.16463; p4_emc_s_sdp[1][0][5][0]=4.99068; 
    p0_emc_m_sdp[0][0][6][0]=0.0561147; p1_emc_m_sdp[0][0][6][0]=-0.0647851; p2_emc_m_sdp[0][0][6][0]=0.0292594; p3_emc_m_sdp[0][0][6][0]=0.00298372; p4_emc_m_sdp[0][0][6][0]=-0.00297468; 
    p0_emc_s_sdp[0][0][6][0]=0.60545; p1_emc_s_sdp[0][0][6][0]=0.414314; p2_emc_s_sdp[0][0][6][0]=-0.320472; p3_emc_s_sdp[0][0][6][0]=0.10648; p4_emc_s_sdp[0][0][6][0]=-0.0131925; 
    p0_emc_m_sdp[1][0][6][0]=2.99097; p1_emc_m_sdp[1][0][6][0]=-11.6764; p2_emc_m_sdp[1][0][6][0]=3.04213; p3_emc_m_sdp[1][0][6][0]=32.1155; p4_emc_m_sdp[1][0][6][0]=-30.8448; 
    p0_emc_s_sdp[1][0][6][0]=-1.40019; p1_emc_s_sdp[1][0][6][0]=7.61287; p2_emc_s_sdp[1][0][6][0]=-0.889885; p3_emc_s_sdp[1][0][6][0]=-21.4475; p4_emc_s_sdp[1][0][6][0]=19.4481; 
    p0_emc_m_sdp[0][0][7][0]=-0.250203; p1_emc_m_sdp[0][0][7][0]=0.608971; p2_emc_m_sdp[0][0][7][0]=-0.486036; p3_emc_m_sdp[0][0][7][0]=0.153427; p4_emc_m_sdp[0][0][7][0]=-0.0145462; 
    p0_emc_s_sdp[0][0][7][0]=0.386649; p1_emc_s_sdp[0][0][7][0]=0.969405; p2_emc_s_sdp[0][0][7][0]=-0.811818; p3_emc_s_sdp[0][0][7][0]=0.283113; p4_emc_s_sdp[0][0][7][0]=-0.0333345; 
    p0_emc_m_sdp[1][0][7][0]=2.42804; p1_emc_m_sdp[1][0][7][0]=-9.74552; p2_emc_m_sdp[1][0][7][0]=3.29536; p3_emc_m_sdp[1][0][7][0]=25.3561; p4_emc_m_sdp[1][0][7][0]=-24.9639; 
    p0_emc_s_sdp[1][0][7][0]=-0.315789; p1_emc_s_sdp[1][0][7][0]=3.42853; p2_emc_s_sdp[1][0][7][0]=-0.277046; p3_emc_s_sdp[1][0][7][0]=-8.84279; p4_emc_s_sdp[1][0][7][0]=7.78163; 
    p0_emc_m_sdp[0][0][8][0]=-0.340476; p1_emc_m_sdp[0][0][8][0]=0.713677; p2_emc_m_sdp[0][0][8][0]=-0.481504; p3_emc_m_sdp[0][0][8][0]=0.104213; p4_emc_m_sdp[0][0][8][0]=-0.000362485; 
    p0_emc_s_sdp[0][0][8][0]=0.33066; p1_emc_s_sdp[0][0][8][0]=1.08615; p2_emc_s_sdp[0][0][8][0]=-0.920356; p3_emc_s_sdp[0][0][8][0]=0.326287; p4_emc_s_sdp[0][0][8][0]=-0.0384748; 
    p0_emc_m_sdp[1][0][8][0]=-0.214449; p1_emc_m_sdp[1][0][8][0]=0.209459; p2_emc_m_sdp[1][0][8][0]=0.581248; p3_emc_m_sdp[1][0][8][0]=0.336216; p4_emc_m_sdp[1][0][8][0]=-1.65714; 
    p0_emc_s_sdp[1][0][8][0]=-0.322457; p1_emc_s_sdp[1][0][8][0]=3.92984; p2_emc_s_sdp[1][0][8][0]=-0.316428; p3_emc_s_sdp[1][0][8][0]=-12.431; p4_emc_s_sdp[1][0][8][0]=11.3302; 
    p0_emc_m_sdp[0][0][9][0]=-0.516267; p1_emc_m_sdp[0][0][9][0]=1.14542; p2_emc_m_sdp[0][0][9][0]=-0.845508; p3_emc_m_sdp[0][0][9][0]=0.216743; p4_emc_m_sdp[0][0][9][0]=-0.0100748; 
    p0_emc_s_sdp[0][0][9][0]=0.540782; p1_emc_s_sdp[0][0][9][0]=0.438111; p2_emc_s_sdp[0][0][9][0]=-0.29133; p3_emc_s_sdp[0][0][9][0]=0.095245; p4_emc_s_sdp[0][0][9][0]=-0.0104396; 
    p0_emc_m_sdp[1][0][9][0]=-0.256497; p1_emc_m_sdp[1][0][9][0]=0.229906; p2_emc_m_sdp[1][0][9][0]=0.646547; p3_emc_m_sdp[1][0][9][0]=0.395751; p4_emc_m_sdp[1][0][9][0]=-1.7383; 
    p0_emc_s_sdp[1][0][9][0]=4.07212; p1_emc_s_sdp[1][0][9][0]=-11.902; p2_emc_s_sdp[1][0][9][0]=2.62824; p3_emc_s_sdp[1][0][9][0]=28.4606; p4_emc_s_sdp[1][0][9][0]=-25.8015; 
    p0_emc_m_sdp[0][1][0][0]=0.324714; p1_emc_m_sdp[0][1][0][0]=-1.1385; p2_emc_m_sdp[0][1][0][0]=1.16207; p3_emc_m_sdp[0][1][0][0]=-0.516466; p4_emc_m_sdp[0][1][0][0]=0.0832604; 
    p0_emc_s_sdp[0][1][0][0]=-1.88366; p1_emc_s_sdp[0][1][0][0]=7.38994; p2_emc_s_sdp[0][1][0][0]=-6.93142; p3_emc_s_sdp[0][1][0][0]=2.69583; p4_emc_s_sdp[0][1][0][0]=-0.370913; 
    p0_emc_m_sdp[1][1][0][0]=-0.153105; p1_emc_m_sdp[1][1][0][0]=0.131823; p2_emc_m_sdp[1][1][0][0]=0.36817; p3_emc_m_sdp[1][1][0][0]=0.189207; p4_emc_m_sdp[1][1][0][0]=-1.12846; 
    p0_emc_s_sdp[1][1][0][0]=-5.24329; p1_emc_s_sdp[1][1][0][0]=23.7888; p2_emc_s_sdp[1][1][0][0]=-6.93614; p3_emc_s_sdp[1][1][0][0]=-64.0467; p4_emc_s_sdp[1][1][0][0]=62.0713; 
    p0_emc_m_sdp[0][1][1][0]=-0.47284; p1_emc_m_sdp[0][1][1][0]=1.12023; p2_emc_m_sdp[0][1][1][0]=-0.975356; p3_emc_m_sdp[0][1][1][0]=0.311759; p4_emc_m_sdp[0][1][1][0]=-0.0291172; 
    p0_emc_s_sdp[0][1][1][0]=0.618858; p1_emc_s_sdp[0][1][1][0]=0.443981; p2_emc_s_sdp[0][1][1][0]=-0.398508; p3_emc_s_sdp[0][1][1][0]=0.157974; p4_emc_s_sdp[0][1][1][0]=-0.0215242; 
    p0_emc_m_sdp[1][1][1][0]=3.99906; p1_emc_m_sdp[1][1][1][0]=-17.0449; p2_emc_m_sdp[1][1][1][0]=5.61013; p3_emc_m_sdp[1][1][1][0]=49.0118; p4_emc_m_sdp[1][1][1][0]=-49.4554; 
    p0_emc_s_sdp[1][1][1][0]=0.352727; p1_emc_s_sdp[1][1][1][0]=0.653899; p2_emc_s_sdp[1][1][1][0]=0.746582; p3_emc_s_sdp[1][1][1][0]=0.115348; p4_emc_s_sdp[1][1][1][0]=-2.22449; 
    p0_emc_m_sdp[0][1][2][0]=-1.19556; p1_emc_m_sdp[0][1][2][0]=3.09275; p2_emc_m_sdp[0][1][2][0]=-2.87731; p3_emc_m_sdp[0][1][2][0]=1.07618; p4_emc_m_sdp[0][1][2][0]=-0.1376; 
    p0_emc_s_sdp[0][1][2][0]=-0.018075; p1_emc_s_sdp[0][1][2][0]=2.19129; p2_emc_s_sdp[0][1][2][0]=-2.01145; p3_emc_s_sdp[0][1][2][0]=0.745797; p4_emc_s_sdp[0][1][2][0]=-0.092849; 
    p0_emc_m_sdp[1][1][2][0]=1.28566; p1_emc_m_sdp[1][1][2][0]=-5.47853; p2_emc_m_sdp[1][1][2][0]=1.49704; p3_emc_m_sdp[1][1][2][0]=15.9359; p4_emc_m_sdp[1][1][2][0]=-15.7573; 
    p0_emc_s_sdp[1][1][2][0]=0.53386; p1_emc_s_sdp[1][1][2][0]=0.433964; p2_emc_s_sdp[1][1][2][0]=0.19031; p3_emc_s_sdp[1][1][2][0]=-0.157094; p4_emc_s_sdp[1][1][2][0]=-0.493397; 
    p0_emc_m_sdp[0][1][3][0]=-0.0262614; p1_emc_m_sdp[0][1][3][0]=-0.0265129; p2_emc_m_sdp[0][1][3][0]=-0.0324416; p3_emc_m_sdp[0][1][3][0]=0.0213348; p4_emc_m_sdp[0][1][3][0]=-0.00319494; 
    p0_emc_s_sdp[0][1][3][0]=0.611763; p1_emc_s_sdp[0][1][3][0]=0.378506; p2_emc_s_sdp[0][1][3][0]=-0.27525; p3_emc_s_sdp[0][1][3][0]=0.0884219; p4_emc_s_sdp[0][1][3][0]=-0.0113619; 
    p0_emc_m_sdp[1][1][3][0]=0.195707; p1_emc_m_sdp[1][1][3][0]=-0.261369; p2_emc_m_sdp[1][1][3][0]=-0.673495; p3_emc_m_sdp[1][1][3][0]=-0.422349; p4_emc_m_sdp[1][1][3][0]=1.76264; 
    p0_emc_s_sdp[1][1][3][0]=2.21287; p1_emc_s_sdp[1][1][3][0]=-6.9971; p2_emc_s_sdp[1][1][3][0]=3.08435; p3_emc_s_sdp[1][1][3][0]=20.4339; p4_emc_s_sdp[1][1][3][0]=-21.392; 
    p0_emc_m_sdp[0][1][4][0]=-0.274743; p1_emc_m_sdp[0][1][4][0]=0.662159; p2_emc_m_sdp[0][1][4][0]=-0.604055; p3_emc_m_sdp[0][1][4][0]=0.212736; p4_emc_m_sdp[0][1][4][0]=-0.0238668; 
    p0_emc_s_sdp[0][1][4][0]=1.0351; p1_emc_s_sdp[0][1][4][0]=-0.666646; p2_emc_s_sdp[0][1][4][0]=0.590982; p3_emc_s_sdp[0][1][4][0]=-0.203244; p4_emc_s_sdp[0][1][4][0]=0.0215603; 
    p0_emc_m_sdp[1][1][4][0]=-1.19714; p1_emc_m_sdp[1][1][4][0]=4.76783; p2_emc_m_sdp[1][1][4][0]=-2.31708; p3_emc_m_sdp[1][1][4][0]=-10.9561; p4_emc_m_sdp[1][1][4][0]=11.5722; 
    p0_emc_s_sdp[1][1][4][0]=4.9739; p1_emc_s_sdp[1][1][4][0]=-16.1591; p2_emc_s_sdp[1][1][4][0]=3.33221; p3_emc_s_sdp[1][1][4][0]=42.8654; p4_emc_s_sdp[1][1][4][0]=-38.4449; 
    p0_emc_m_sdp[0][1][5][0]=0.135555; p1_emc_m_sdp[0][1][5][0]=-0.235623; p2_emc_m_sdp[0][1][5][0]=0.205285; p3_emc_m_sdp[0][1][5][0]=-0.0722605; p4_emc_m_sdp[0][1][5][0]=0.00791761; 
    p0_emc_s_sdp[0][1][5][0]=0.727353; p1_emc_s_sdp[0][1][5][0]=0.173038; p2_emc_s_sdp[0][1][5][0]=-0.17129; p3_emc_s_sdp[0][1][5][0]=0.0713544; p4_emc_s_sdp[0][1][5][0]=-0.0109688; 
    p0_emc_m_sdp[1][1][5][0]=-0.471999; p1_emc_m_sdp[1][1][5][0]=2.70763; p2_emc_m_sdp[1][1][5][0]=-1.90784; p3_emc_m_sdp[1][1][5][0]=-7.07738; p4_emc_m_sdp[1][1][5][0]=8.53027; 
    p0_emc_s_sdp[1][1][5][0]=0.590285; p1_emc_s_sdp[1][1][5][0]=0.33354; p2_emc_s_sdp[1][1][5][0]=-0.0393818; p3_emc_s_sdp[1][1][5][0]=-0.264156; p4_emc_s_sdp[1][1][5][0]=0.221744; 
    p0_emc_m_sdp[0][1][6][0]=0.123041; p1_emc_m_sdp[0][1][6][0]=-0.106204; p2_emc_m_sdp[0][1][6][0]=0.0532146; p3_emc_m_sdp[0][1][6][0]=-0.0118873; p4_emc_m_sdp[0][1][6][0]=0.000377631; 
    p0_emc_s_sdp[0][1][6][0]=0.824327; p1_emc_s_sdp[0][1][6][0]=-0.0687845; p2_emc_s_sdp[0][1][6][0]=0.0351622; p3_emc_s_sdp[0][1][6][0]=0.00346417; p4_emc_s_sdp[0][1][6][0]=-0.00332597; 
    p0_emc_m_sdp[1][1][6][0]=0.124702; p1_emc_m_sdp[1][1][6][0]=-0.0548024; p2_emc_m_sdp[1][1][6][0]=-0.224153; p3_emc_m_sdp[1][1][6][0]=-0.153774; p4_emc_m_sdp[1][1][6][0]=0.601935; 
    p0_emc_s_sdp[1][1][6][0]=2.40188; p1_emc_s_sdp[1][1][6][0]=-7.34032; p2_emc_s_sdp[1][1][6][0]=2.57592; p3_emc_s_sdp[1][1][6][0]=22.4259; p4_emc_s_sdp[1][1][6][0]=-23.1152; 
    p0_emc_m_sdp[0][1][7][0]=0.0492664; p1_emc_m_sdp[0][1][7][0]=0.0837663; p2_emc_m_sdp[0][1][7][0]=-0.104306; p3_emc_m_sdp[0][1][7][0]=0.0392437; p4_emc_m_sdp[0][1][7][0]=-0.00449552; 
    p0_emc_s_sdp[0][1][7][0]=0.773478; p1_emc_s_sdp[0][1][7][0]=0.0686986; p2_emc_s_sdp[0][1][7][0]=-0.0930261; p3_emc_s_sdp[0][1][7][0]=0.0530516; p4_emc_s_sdp[0][1][7][0]=-0.00943161; 
    p0_emc_m_sdp[1][1][7][0]=3.6376; p1_emc_m_sdp[1][1][7][0]=-14.6217; p2_emc_m_sdp[1][1][7][0]=4.72814; p3_emc_m_sdp[1][1][7][0]=41.3111; p4_emc_m_sdp[1][1][7][0]=-41.5842; 
    p0_emc_s_sdp[1][1][7][0]=1.41913; p1_emc_s_sdp[1][1][7][0]=-2.60723; p2_emc_s_sdp[1][1][7][0]=0.660961; p3_emc_s_sdp[1][1][7][0]=6.12731; p4_emc_s_sdp[1][1][7][0]=-5.03774; 
    p0_emc_m_sdp[0][1][8][0]=-0.36327; p1_emc_m_sdp[0][1][8][0]=1.07555; p2_emc_m_sdp[0][1][8][0]=-0.887462; p3_emc_m_sdp[0][1][8][0]=0.264845; p4_emc_m_sdp[0][1][8][0]=-0.0201815; 
    p0_emc_s_sdp[0][1][8][0]=0.386809; p1_emc_s_sdp[0][1][8][0]=1.04592; p2_emc_s_sdp[0][1][8][0]=-0.951906; p3_emc_s_sdp[0][1][8][0]=0.356396; p4_emc_s_sdp[0][1][8][0]=-0.0436163; 
    p0_emc_m_sdp[1][1][8][0]=-1.72455; p1_emc_m_sdp[1][1][8][0]=7.80791; p2_emc_m_sdp[1][1][8][0]=-3.02949; p3_emc_m_sdp[1][1][8][0]=-22.5188; p4_emc_m_sdp[1][1][8][0]=23.3201; 
    p0_emc_s_sdp[1][1][8][0]=1.51206; p1_emc_s_sdp[1][1][8][0]=-3.40506; p2_emc_s_sdp[1][1][8][0]=1.61459; p3_emc_s_sdp[1][1][8][0]=9.93326; p4_emc_s_sdp[1][1][8][0]=-10.9341; 
    p0_emc_m_sdp[0][1][9][0]=-0.407547; p1_emc_m_sdp[0][1][9][0]=1.17992; p2_emc_m_sdp[0][1][9][0]=-1.05717; p3_emc_m_sdp[0][1][9][0]=0.369399; p4_emc_m_sdp[0][1][9][0]=-0.0410682; 
    p0_emc_s_sdp[0][1][9][0]=1.37598; p1_emc_s_sdp[0][1][9][0]=-1.50914; p2_emc_s_sdp[0][1][9][0]=1.29928; p3_emc_s_sdp[0][1][9][0]=-0.430501; p4_emc_s_sdp[0][1][9][0]=0.0462345; 
    p0_emc_m_sdp[1][1][9][0]=-2.4606; p1_emc_m_sdp[1][1][9][0]=9.43208; p2_emc_m_sdp[1][1][9][0]=-2.59168; p3_emc_m_sdp[1][1][9][0]=-24.4207; p4_emc_m_sdp[1][1][9][0]=23.4178; 
    p0_emc_s_sdp[1][1][9][0]=2.08472; p1_emc_s_sdp[1][1][9][0]=-5.71806; p2_emc_s_sdp[1][1][9][0]=2.35935; p3_emc_s_sdp[1][1][9][0]=15.6133; p4_emc_s_sdp[1][1][9][0]=-16.237; 
    p0_emc_m_sdp[0][2][0][0]=-0.572762; p1_emc_m_sdp[0][2][0][0]=1.38489; p2_emc_m_sdp[0][2][0][0]=-1.10974; p3_emc_m_sdp[0][2][0][0]=0.359394; p4_emc_m_sdp[0][2][0][0]=-0.037995; 
    p0_emc_s_sdp[0][2][0][0]=1.32102; p1_emc_s_sdp[0][2][0][0]=-1.2216; p2_emc_s_sdp[0][2][0][0]=1.08152; p3_emc_s_sdp[0][2][0][0]=-0.372064; p4_emc_s_sdp[0][2][0][0]=0.0411018; 
    p0_emc_m_sdp[1][2][0][0]=-2.20378; p1_emc_m_sdp[1][2][0][0]=9.17372; p2_emc_m_sdp[1][2][0][0]=-2.90694; p3_emc_m_sdp[1][2][0][0]=-26.5671; p4_emc_m_sdp[1][2][0][0]=26.7563; 
    p0_emc_s_sdp[1][2][0][0]=2.12766; p1_emc_s_sdp[1][2][0][0]=-5.46456; p2_emc_s_sdp[1][2][0][0]=1.80081; p3_emc_s_sdp[1][2][0][0]=15.3419; p4_emc_s_sdp[1][2][0][0]=-15.0893; 
    p0_emc_m_sdp[0][2][1][0]=-0.351729; p1_emc_m_sdp[0][2][1][0]=0.79378; p2_emc_m_sdp[0][2][1][0]=-0.576784; p3_emc_m_sdp[0][2][1][0]=0.174824; p4_emc_m_sdp[0][2][1][0]=-0.0182265; 
    p0_emc_s_sdp[0][2][1][0]=1.17113; p1_emc_s_sdp[0][2][1][0]=-0.796509; p2_emc_s_sdp[0][2][1][0]=0.660095; p3_emc_s_sdp[0][2][1][0]=-0.197625; p4_emc_s_sdp[0][2][1][0]=0.0146821; 
    p0_emc_m_sdp[1][2][1][0]=1.47256; p1_emc_m_sdp[1][2][1][0]=-6.47104; p2_emc_m_sdp[1][2][1][0]=2.92493; p3_emc_m_sdp[1][2][1][0]=17.3061; p4_emc_m_sdp[1][2][1][0]=-18.3792; 
    p0_emc_s_sdp[1][2][1][0]=2.34041; p1_emc_s_sdp[1][2][1][0]=-6.27618; p2_emc_s_sdp[1][2][1][0]=2.0648; p3_emc_s_sdp[1][2][1][0]=17.4773; p4_emc_s_sdp[1][2][1][0]=-17.3965; 
    p0_emc_m_sdp[0][2][2][0]=0.112147; p1_emc_m_sdp[0][2][2][0]=-0.384784; p2_emc_m_sdp[0][2][2][0]=0.427065; p3_emc_m_sdp[0][2][2][0]=-0.1638; p4_emc_m_sdp[0][2][2][0]=0.0187634; 
    p0_emc_s_sdp[0][2][2][0]=1.22013; p1_emc_s_sdp[0][2][2][0]=-0.946929; p2_emc_s_sdp[0][2][2][0]=0.81613; p3_emc_s_sdp[0][2][2][0]=-0.273269; p4_emc_s_sdp[0][2][2][0]=0.0287349; 
    p0_emc_m_sdp[1][2][2][0]=0.108134; p1_emc_m_sdp[1][2][2][0]=-0.101532; p2_emc_m_sdp[1][2][2][0]=-0.298018; p3_emc_m_sdp[1][2][2][0]=-0.207743; p4_emc_m_sdp[1][2][2][0]=0.741624; 
    p0_emc_s_sdp[1][2][2][0]=-1.15875; p1_emc_s_sdp[1][2][2][0]=7.86124; p2_emc_s_sdp[1][2][2][0]=-2.37483; p3_emc_s_sdp[1][2][2][0]=-20.9131; p4_emc_s_sdp[1][2][2][0]=20.3321; 
    p0_emc_m_sdp[0][2][3][0]=0.29264; p1_emc_m_sdp[0][2][3][0]=-0.832992; p2_emc_m_sdp[0][2][3][0]=0.783436; p3_emc_m_sdp[0][2][3][0]=-0.276872; p4_emc_m_sdp[0][2][3][0]=0.0310246; 
    p0_emc_s_sdp[0][2][3][0]=1.15854; p1_emc_s_sdp[0][2][3][0]=-0.833464; p2_emc_s_sdp[0][2][3][0]=0.738182; p3_emc_s_sdp[0][2][3][0]=-0.253422; p4_emc_s_sdp[0][2][3][0]=0.0270657; 
    p0_emc_m_sdp[1][2][3][0]=-1.37855; p1_emc_m_sdp[1][2][3][0]=4.83992; p2_emc_m_sdp[1][2][3][0]=-0.915656; p3_emc_m_sdp[1][2][3][0]=-12.0098; p4_emc_m_sdp[1][2][3][0]=10.7147; 
    p0_emc_s_sdp[1][2][3][0]=2.04871; p1_emc_s_sdp[1][2][3][0]=-3.93638; p2_emc_s_sdp[1][2][3][0]=-0.194109; p3_emc_s_sdp[1][2][3][0]=9.218; p4_emc_s_sdp[1][2][3][0]=-6.05978; 
    p0_emc_m_sdp[0][2][4][0]=0.0120659; p1_emc_m_sdp[0][2][4][0]=-0.138015; p2_emc_m_sdp[0][2][4][0]=0.180079; p3_emc_m_sdp[0][2][4][0]=-0.0641835; p4_emc_m_sdp[0][2][4][0]=0.0059523; 
    p0_emc_s_sdp[0][2][4][0]=0.929609; p1_emc_s_sdp[0][2][4][0]=-0.219586; p2_emc_s_sdp[0][2][4][0]=0.180596; p3_emc_s_sdp[0][2][4][0]=-0.056338; p4_emc_s_sdp[0][2][4][0]=0.00419671; 
    p0_emc_m_sdp[1][2][4][0]=-0.0680823; p1_emc_m_sdp[1][2][4][0]=0.0083603; p2_emc_m_sdp[1][2][4][0]=0.095415; p3_emc_m_sdp[1][2][4][0]=0.0983294; p4_emc_m_sdp[1][2][4][0]=-0.176987; 
    p0_emc_s_sdp[1][2][4][0]=2.95034; p1_emc_s_sdp[1][2][4][0]=-9.22665; p2_emc_s_sdp[1][2][4][0]=4.13358; p3_emc_s_sdp[1][2][4][0]=24.6168; p4_emc_s_sdp[1][2][4][0]=-26.0731; 
    p0_emc_m_sdp[0][2][5][0]=-0.358822; p1_emc_m_sdp[0][2][5][0]=0.808742; p2_emc_m_sdp[0][2][5][0]=-0.646866; p3_emc_m_sdp[0][2][5][0]=0.214457; p4_emc_m_sdp[0][2][5][0]=-0.0231797; 
    p0_emc_s_sdp[0][2][5][0]=0.613667; p1_emc_s_sdp[0][2][5][0]=0.636054; p2_emc_s_sdp[0][2][5][0]=-0.613059; p3_emc_s_sdp[0][2][5][0]=0.258469; p4_emc_s_sdp[0][2][5][0]=-0.0409882; 
    p0_emc_m_sdp[1][2][5][0]=1.85992; p1_emc_m_sdp[1][2][5][0]=-7.55612; p2_emc_m_sdp[1][2][5][0]=2.60743; p3_emc_m_sdp[1][2][5][0]=19.4384; p4_emc_m_sdp[1][2][5][0]=-19.1305; 
    p0_emc_s_sdp[1][2][5][0]=4.20538; p1_emc_s_sdp[1][2][5][0]=-14.1051; p2_emc_s_sdp[1][2][5][0]=4.48506; p3_emc_s_sdp[1][2][5][0]=40.4887; p4_emc_s_sdp[1][2][5][0]=-40.6642; 
    p0_emc_m_sdp[0][2][6][0]=-0.184379; p1_emc_m_sdp[0][2][6][0]=0.340897; p2_emc_m_sdp[0][2][6][0]=-0.244439; p3_emc_m_sdp[0][2][6][0]=0.0796227; p4_emc_m_sdp[0][2][6][0]=-0.00963503; 
    p0_emc_s_sdp[0][2][6][0]=0.972801; p1_emc_s_sdp[0][2][6][0]=-0.332152; p2_emc_s_sdp[0][2][6][0]=0.328747; p3_emc_s_sdp[0][2][6][0]=-0.118886; p4_emc_s_sdp[0][2][6][0]=0.0123711; 
    p0_emc_m_sdp[1][2][6][0]=-0.0216085; p1_emc_m_sdp[1][2][6][0]=-0.13245; p2_emc_m_sdp[1][2][6][0]=-0.18408; p3_emc_m_sdp[1][2][6][0]=0.0154011; p4_emc_m_sdp[1][2][6][0]=0.846263; 
    p0_emc_s_sdp[1][2][6][0]=-0.836713; p1_emc_s_sdp[1][2][6][0]=6.86284; p2_emc_s_sdp[1][2][6][0]=-2.66391; p3_emc_s_sdp[1][2][6][0]=-18.0423; p4_emc_s_sdp[1][2][6][0]=18.5853; 
    p0_emc_m_sdp[0][2][7][0]=-0.399026; p1_emc_m_sdp[0][2][7][0]=0.82607; p2_emc_m_sdp[0][2][7][0]=-0.60216; p3_emc_m_sdp[0][2][7][0]=0.157632; p4_emc_m_sdp[0][2][7][0]=-0.00686539; 
    p0_emc_s_sdp[0][2][7][0]=0.858334; p1_emc_s_sdp[0][2][7][0]=-0.0229021; p2_emc_s_sdp[0][2][7][0]=0.0125479; p3_emc_s_sdp[0][2][7][0]=0.00243335; p4_emc_s_sdp[0][2][7][0]=-0.000941194; 
    p0_emc_m_sdp[1][2][7][0]=-1.23055; p1_emc_m_sdp[1][2][7][0]=5.29239; p2_emc_m_sdp[1][2][7][0]=-2.14985; p3_emc_m_sdp[1][2][7][0]=-15.8048; p4_emc_m_sdp[1][2][7][0]=16.8167; 
    p0_emc_s_sdp[1][2][7][0]=-1.51679; p1_emc_s_sdp[1][2][7][0]=8.92089; p2_emc_s_sdp[1][2][7][0]=-2.0253; p3_emc_s_sdp[1][2][7][0]=-23.2701; p4_emc_s_sdp[1][2][7][0]=21.3567; 
    p0_emc_m_sdp[0][2][8][0]=-0.165078; p1_emc_m_sdp[0][2][8][0]=0.206915; p2_emc_m_sdp[0][2][8][0]=-0.123895; p3_emc_m_sdp[0][2][8][0]=0.0301632; p4_emc_m_sdp[0][2][8][0]=9.10802e-05; 
    p0_emc_s_sdp[0][2][8][0]=0.799607; p1_emc_s_sdp[0][2][8][0]=0.0500154; p2_emc_s_sdp[0][2][8][0]=-0.0280796; p3_emc_s_sdp[0][2][8][0]=0.0151336; p4_emc_s_sdp[0][2][8][0]=-0.002521; 
    p0_emc_m_sdp[1][2][8][0]=4.16106; p1_emc_m_sdp[1][2][8][0]=-16.2547; p2_emc_m_sdp[1][2][8][0]=4.39539; p3_emc_m_sdp[1][2][8][0]=42.786; p4_emc_m_sdp[1][2][8][0]=-40.6775; 
    p0_emc_s_sdp[1][2][8][0]=2.79548; p1_emc_s_sdp[1][2][8][0]=-7.36021; p2_emc_s_sdp[1][2][8][0]=0.885036; p3_emc_s_sdp[1][2][8][0]=21.067; p4_emc_s_sdp[1][2][8][0]=-18.6559; 
    p0_emc_m_sdp[0][2][9][0]=-0.400482; p1_emc_m_sdp[0][2][9][0]=0.803505; p2_emc_m_sdp[0][2][9][0]=-0.638988; p3_emc_m_sdp[0][2][9][0]=0.208965; p4_emc_m_sdp[0][2][9][0]=-0.0204909; 
    p0_emc_s_sdp[0][2][9][0]=1.26633; p1_emc_s_sdp[0][2][9][0]=-1.1189; p2_emc_s_sdp[0][2][9][0]=0.967305; p3_emc_s_sdp[0][2][9][0]=-0.323855; p4_emc_s_sdp[0][2][9][0]=0.0353388; 
    p0_emc_m_sdp[1][2][9][0]=2.4157; p1_emc_m_sdp[1][2][9][0]=-9.87842; p2_emc_m_sdp[1][2][9][0]=3.36185; p3_emc_m_sdp[1][2][9][0]=24.6553; p4_emc_m_sdp[1][2][9][0]=-23.8721; 
    p0_emc_s_sdp[1][2][9][0]=-1.99104; p1_emc_s_sdp[1][2][9][0]=11.1919; p2_emc_s_sdp[1][2][9][0]=-3.61505; p3_emc_s_sdp[1][2][9][0]=-29.549; p4_emc_s_sdp[1][2][9][0]=29.1194; 

	
    //After burn

    //EMC DPHI POS 27 GeV
   	
    p0_emc_m_sdp[0][0][0][1]=0.113253; p1_emc_m_sdp[0][0][0][1]=-0.153019; p2_emc_m_sdp[0][0][0][1]=0.0403921; p3_emc_m_sdp[0][0][0][1]=0.00690482; p4_emc_m_sdp[0][0][0][1]=-0.00300476; 
    p0_emc_s_sdp[0][0][0][1]=1.41825; p1_emc_s_sdp[0][0][0][1]=-1.75076; p2_emc_s_sdp[0][0][0][1]=1.58307; p3_emc_s_sdp[0][0][0][1]=-0.542353; p4_emc_s_sdp[0][0][0][1]=0.0604076; 
    p0_emc_m_sdp[1][0][0][1]=3.07942; p1_emc_m_sdp[1][0][0][1]=-13.4516; p2_emc_m_sdp[1][0][0][1]=4.76888; p3_emc_m_sdp[1][0][0][1]=40.6164; p4_emc_m_sdp[1][0][0][1]=-41.9288; 
    p0_emc_s_sdp[1][0][0][1]=0.780585; p1_emc_s_sdp[1][0][0][1]=0.165122; p2_emc_s_sdp[1][0][0][1]=-0.545272; p3_emc_s_sdp[1][0][0][1]=-0.599002; p4_emc_s_sdp[1][0][0][1]=1.55395; 
    p0_emc_m_sdp[0][0][1][1]=1.16897; p1_emc_m_sdp[0][0][1][1]=-2.97885; p2_emc_m_sdp[0][0][1][1]=2.52369; p3_emc_m_sdp[0][0][1][1]=-0.842178; p4_emc_m_sdp[0][0][1][1]=0.0893788; 
    p0_emc_s_sdp[0][0][1][1]=0.9777; p1_emc_s_sdp[0][0][1][1]=-0.580601; p2_emc_s_sdp[0][0][1][1]=0.512307; p3_emc_s_sdp[0][0][1][1]=-0.157457; p4_emc_s_sdp[0][0][1][1]=0.0144806; 
    p0_emc_m_sdp[1][0][1][1]=-0.270657; p1_emc_m_sdp[1][0][1][1]=0.347762; p2_emc_m_sdp[1][0][1][1]=0.882889; p3_emc_m_sdp[1][0][1][1]=0.504669; p4_emc_m_sdp[1][0][1][1]=-2.43297; 
    p0_emc_s_sdp[1][0][1][1]=0.507051; p1_emc_s_sdp[1][0][1][1]=0.442378; p2_emc_s_sdp[1][0][1][1]=0.228818; p3_emc_s_sdp[1][0][1][1]=-0.145797; p4_emc_s_sdp[1][0][1][1]=-0.642571; 
    p0_emc_m_sdp[0][0][2][1]=0.74747; p1_emc_m_sdp[0][0][2][1]=-1.96682; p2_emc_m_sdp[0][0][2][1]=1.70813; p3_emc_m_sdp[0][0][2][1]=-0.586647; p4_emc_m_sdp[0][0][2][1]=0.0655717; 
    p0_emc_s_sdp[0][0][2][1]=0.991117; p1_emc_s_sdp[0][0][2][1]=-0.542402; p2_emc_s_sdp[0][0][2][1]=0.418533; p3_emc_s_sdp[0][0][2][1]=-0.112578; p4_emc_s_sdp[0][0][2][1]=0.00841653; 
    p0_emc_m_sdp[1][0][2][1]=2.46272; p1_emc_m_sdp[1][0][2][1]=-9.5751; p2_emc_m_sdp[1][0][2][1]=2.77698; p3_emc_m_sdp[1][0][2][1]=25.8818; p4_emc_m_sdp[1][0][2][1]=-25.2197; 
    p0_emc_s_sdp[1][0][2][1]=0.518893; p1_emc_s_sdp[1][0][2][1]=0.443017; p2_emc_s_sdp[1][0][2][1]=0.20947; p3_emc_s_sdp[1][0][2][1]=-0.170998; p4_emc_s_sdp[1][0][2][1]=-0.616667; 
    p0_emc_m_sdp[0][0][3][1]=0.367146; p1_emc_m_sdp[0][0][3][1]=-0.970224; p2_emc_m_sdp[0][0][3][1]=0.838075; p3_emc_m_sdp[0][0][3][1]=-0.289298; p4_emc_m_sdp[0][0][3][1]=0.0334186; 
    p0_emc_s_sdp[0][0][3][1]=1.40014; p1_emc_s_sdp[0][0][3][1]=-1.74463; p2_emc_s_sdp[0][0][3][1]=1.59537; p3_emc_s_sdp[0][0][3][1]=-0.567707; p4_emc_s_sdp[0][0][3][1]=0.0654438; 
    p0_emc_m_sdp[1][0][3][1]=-0.0326285; p1_emc_m_sdp[1][0][3][1]=0.134595; p2_emc_m_sdp[1][0][3][1]=0.254249; p3_emc_m_sdp[1][0][3][1]=0.0647794; p4_emc_m_sdp[1][0][3][1]=-0.954696; 
    p0_emc_s_sdp[1][0][3][1]=0.5635; p1_emc_s_sdp[1][0][3][1]=0.358475; p2_emc_s_sdp[1][0][3][1]=0.0225947; p3_emc_s_sdp[1][0][3][1]=-0.23835; p4_emc_s_sdp[1][0][3][1]=0.0466086; 
    p0_emc_m_sdp[0][0][4][1]=0.472797; p1_emc_m_sdp[0][0][4][1]=-1.29557; p2_emc_m_sdp[0][0][4][1]=1.14854; p3_emc_m_sdp[0][0][4][1]=-0.403905; p4_emc_m_sdp[0][0][4][1]=0.0474142; 
    p0_emc_s_sdp[0][0][4][1]=1.59867; p1_emc_s_sdp[0][0][4][1]=-2.28326; p2_emc_s_sdp[0][0][4][1]=2.04325; p3_emc_s_sdp[0][0][4][1]=-0.700723; p4_emc_s_sdp[0][0][4][1]=0.0756836; 
    p0_emc_m_sdp[1][0][4][1]=0.160188; p1_emc_m_sdp[1][0][4][1]=0.00241248; p2_emc_m_sdp[1][0][4][1]=-0.203371; p3_emc_m_sdp[1][0][4][1]=-0.284022; p4_emc_m_sdp[1][0][4][1]=0.159114; 
    p0_emc_s_sdp[1][0][4][1]=-1.29859; p1_emc_s_sdp[1][0][4][1]=9.34965; p2_emc_s_sdp[1][0][4][1]=-3.97809; p3_emc_s_sdp[1][0][4][1]=-30.1667; p4_emc_s_sdp[1][0][4][1]=33.6073; 
    p0_emc_m_sdp[0][0][5][1]=-0.131631; p1_emc_m_sdp[0][0][5][1]=0.417412; p2_emc_m_sdp[0][0][5][1]=-0.390665; p3_emc_m_sdp[0][0][5][1]=0.134431; p4_emc_m_sdp[0][0][5][1]=-0.0140275; 
    p0_emc_s_sdp[0][0][5][1]=1.89001; p1_emc_s_sdp[0][0][5][1]=-2.91097; p2_emc_s_sdp[0][0][5][1]=2.48722; p3_emc_s_sdp[0][0][5][1]=-0.843124; p4_emc_s_sdp[0][0][5][1]=0.0947644; 
    p0_emc_m_sdp[1][0][5][1]=2.75943; p1_emc_m_sdp[1][0][5][1]=-10.7067; p2_emc_m_sdp[1][0][5][1]=3.3129; p3_emc_m_sdp[1][0][5][1]=29.2438; p4_emc_m_sdp[1][0][5][1]=-29.053; 
    p0_emc_s_sdp[1][0][5][1]=-2.81597; p1_emc_s_sdp[1][0][5][1]=14.1407; p2_emc_s_sdp[1][0][5][1]=-4.49287; p3_emc_s_sdp[1][0][5][1]=-37.0873; p4_emc_s_sdp[1][0][5][1]=36.1728; 
    p0_emc_m_sdp[0][0][6][1]=0.192238; p1_emc_m_sdp[0][0][6][1]=-0.488865; p2_emc_m_sdp[0][0][6][1]=0.456938; p3_emc_m_sdp[0][0][6][1]=-0.171994; p4_emc_m_sdp[0][0][6][1]=0.0215344; 
    p0_emc_s_sdp[0][0][6][1]=1.68109; p1_emc_s_sdp[0][0][6][1]=-2.30447; p2_emc_s_sdp[0][0][6][1]=1.94894; p3_emc_s_sdp[0][0][6][1]=-0.654152; p4_emc_s_sdp[0][0][6][1]=0.0725506; 
    p0_emc_m_sdp[1][0][6][1]=2.58438; p1_emc_m_sdp[1][0][6][1]=-10.3783; p2_emc_m_sdp[1][0][6][1]=3.61015; p3_emc_m_sdp[1][0][6][1]=28.3946; p4_emc_m_sdp[1][0][6][1]=-28.604; 
    p0_emc_s_sdp[1][0][6][1]=2.7928; p1_emc_s_sdp[1][0][6][1]=-7.59732; p2_emc_s_sdp[1][0][6][1]=1.39979; p3_emc_s_sdp[1][0][6][1]=20.6621; p4_emc_s_sdp[1][0][6][1]=-18.8593; 
    p0_emc_m_sdp[0][0][7][1]=-0.0761904; p1_emc_m_sdp[0][0][7][1]=0.205964; p2_emc_m_sdp[0][0][7][1]=-0.194604; p3_emc_m_sdp[0][0][7][1]=0.071595; p4_emc_m_sdp[0][0][7][1]=-0.00790634; 
    p0_emc_s_sdp[0][0][7][1]=1.2288; p1_emc_s_sdp[0][0][7][1]=-1.08141; p2_emc_s_sdp[0][0][7][1]=0.853951; p3_emc_s_sdp[0][0][7][1]=-0.261889; p4_emc_s_sdp[0][0][7][1]=0.0258708; 
    p0_emc_m_sdp[1][0][7][1]=2.54063; p1_emc_m_sdp[1][0][7][1]=-10.0496; p2_emc_m_sdp[1][0][7][1]=3.0176; p3_emc_m_sdp[1][0][7][1]=28.2031; p4_emc_m_sdp[1][0][7][1]=-28.1475; 
    p0_emc_s_sdp[1][0][7][1]=0.663664; p1_emc_s_sdp[1][0][7][1]=0.366421; p2_emc_s_sdp[1][0][7][1]=-0.0808566; p3_emc_s_sdp[1][0][7][1]=-0.396072; p4_emc_s_sdp[1][0][7][1]=0.0416187; 
    p0_emc_m_sdp[0][0][8][1]=0.248986; p1_emc_m_sdp[0][0][8][1]=-0.708457; p2_emc_m_sdp[0][0][8][1]=0.614226; p3_emc_m_sdp[0][0][8][1]=-0.201738; p4_emc_m_sdp[0][0][8][1]=0.0222257; 
    p0_emc_s_sdp[0][0][8][1]=1.3694; p1_emc_s_sdp[0][0][8][1]=-1.41449; p2_emc_s_sdp[0][0][8][1]=1.13878; p3_emc_s_sdp[0][0][8][1]=-0.3577; p4_emc_s_sdp[0][0][8][1]=0.0368117; 
    p0_emc_m_sdp[1][0][8][1]=-0.565767; p1_emc_m_sdp[1][0][8][1]=2.46194; p2_emc_m_sdp[1][0][8][1]=-0.921045; p3_emc_m_sdp[1][0][8][1]=-7.49232; p4_emc_m_sdp[1][0][8][1]=7.91343; 
    p0_emc_s_sdp[1][0][8][1]=0.615919; p1_emc_s_sdp[1][0][8][1]=0.42958; p2_emc_s_sdp[1][0][8][1]=0.0796014; p3_emc_s_sdp[1][0][8][1]=-0.32258; p4_emc_s_sdp[1][0][8][1]=-0.488719; 
    p0_emc_m_sdp[0][0][9][1]=0.43684; p1_emc_m_sdp[0][0][9][1]=-1.21713; p2_emc_m_sdp[0][0][9][1]=1.05791; p3_emc_m_sdp[0][0][9][1]=-0.348717; p4_emc_m_sdp[0][0][9][1]=0.0375856; 
    p0_emc_s_sdp[0][0][9][1]=1.37997; p1_emc_s_sdp[0][0][9][1]=-1.53213; p2_emc_s_sdp[0][0][9][1]=1.31268; p3_emc_s_sdp[0][0][9][1]=-0.434214; p4_emc_s_sdp[0][0][9][1]=0.0472407; 
    p0_emc_m_sdp[1][0][9][1]=2.17165; p1_emc_m_sdp[1][0][9][1]=-8.25278; p2_emc_m_sdp[1][0][9][1]=1.90766; p3_emc_m_sdp[1][0][9][1]=21.6229; p4_emc_m_sdp[1][0][9][1]=-20.0118; 
    p0_emc_s_sdp[1][0][9][1]=-1.49445; p1_emc_s_sdp[1][0][9][1]=8.95799; p2_emc_s_sdp[1][0][9][1]=-2.38007; p3_emc_s_sdp[1][0][9][1]=-25.7339; p4_emc_s_sdp[1][0][9][1]=25.2864; 
    p0_emc_m_sdp[0][1][0][1]=-0.142497; p1_emc_m_sdp[0][1][0][1]=0.14918; p2_emc_m_sdp[0][1][0][1]=-0.276291; p3_emc_m_sdp[0][1][0][1]=0.143256; p4_emc_m_sdp[0][1][0][1]=-0.021441; 
    p0_emc_s_sdp[0][1][0][1]=0.990546; p1_emc_s_sdp[0][1][0][1]=-0.603186; p2_emc_s_sdp[0][1][0][1]=0.594154; p3_emc_s_sdp[0][1][0][1]=-0.227544; p4_emc_s_sdp[0][1][0][1]=0.0323816; 
    p0_emc_m_sdp[1][1][0][1]=-0.962281; p1_emc_m_sdp[1][1][0][1]=3.88221; p2_emc_m_sdp[1][1][0][1]=-1.77946; p3_emc_m_sdp[1][1][0][1]=-10.9825; p4_emc_m_sdp[1][1][0][1]=11.7798; 
    p0_emc_s_sdp[1][1][0][1]=4.62139; p1_emc_s_sdp[1][1][0][1]=-13.4673; p2_emc_s_sdp[1][1][0][1]=2.35495; p3_emc_s_sdp[1][1][0][1]=31.5913; p4_emc_s_sdp[1][1][0][1]=-26.5981; 
    p0_emc_m_sdp[0][1][1][1]=0.344887; p1_emc_m_sdp[0][1][1][1]=-1.18496; p2_emc_m_sdp[0][1][1][1]=0.963987; p3_emc_m_sdp[0][1][1][1]=-0.322929; p4_emc_m_sdp[0][1][1][1]=0.0379945; 
    p0_emc_s_sdp[0][1][1][1]=0.152257; p1_emc_s_sdp[0][1][1][1]=1.74725; p2_emc_s_sdp[0][1][1][1]=-1.65393; p3_emc_s_sdp[0][1][1][1]=0.639794; p4_emc_s_sdp[0][1][1][1]=-0.0815694; 
    p0_emc_m_sdp[1][1][1][1]=-0.637855; p1_emc_m_sdp[1][1][1][1]=2.40464; p2_emc_m_sdp[1][1][1][1]=-1.19936; p3_emc_m_sdp[1][1][1][1]=-6.00514; p4_emc_m_sdp[1][1][1][1]=6.37386; 
    p0_emc_s_sdp[1][1][1][1]=-0.611856; p1_emc_s_sdp[1][1][1][1]=5.38833; p2_emc_s_sdp[1][1][1][1]=-1.62312; p3_emc_s_sdp[1][1][1][1]=-13.8142; p4_emc_s_sdp[1][1][1][1]=13.2001; 
    p0_emc_m_sdp[0][1][2][1]=0.0463512; p1_emc_m_sdp[0][1][2][1]=-0.54052; p2_emc_m_sdp[0][1][2][1]=0.517506; p3_emc_m_sdp[0][1][2][1]=-0.235523; p4_emc_m_sdp[0][1][2][1]=0.0426808; 
    p0_emc_s_sdp[0][1][2][1]=0.889167; p1_emc_s_sdp[0][1][2][1]=-0.365101; p2_emc_s_sdp[0][1][2][1]=0.418223; p3_emc_s_sdp[0][1][2][1]=-0.189371; p4_emc_s_sdp[0][1][2][1]=0.0323218; 
    p0_emc_m_sdp[1][1][2][1]=-3.13805; p1_emc_m_sdp[1][1][2][1]=12.4776; p2_emc_m_sdp[1][1][2][1]=-3.66732; p3_emc_m_sdp[1][1][2][1]=-34.9837; p4_emc_m_sdp[1][1][2][1]=33.9583; 
    p0_emc_s_sdp[1][1][2][1]=4.34029; p1_emc_s_sdp[1][1][2][1]=-14.6335; p2_emc_s_sdp[1][1][2][1]=4.42051; p3_emc_s_sdp[1][1][2][1]=40.6936; p4_emc_s_sdp[1][1][2][1]=-39.8075; 
    p0_emc_m_sdp[0][1][3][1]=-0.797588; p1_emc_m_sdp[0][1][3][1]=2.0391; p2_emc_m_sdp[0][1][3][1]=-2.1921; p3_emc_m_sdp[0][1][3][1]=0.935581; p4_emc_m_sdp[0][1][3][1]=-0.1354; 
    p0_emc_s_sdp[0][1][3][1]=1.67931; p1_emc_s_sdp[0][1][3][1]=-1.72132; p2_emc_s_sdp[0][1][3][1]=1.11309; p3_emc_s_sdp[0][1][3][1]=-0.277322; p4_emc_s_sdp[0][1][3][1]=0.0230589; 
    p0_emc_m_sdp[1][1][3][1]=-0.614032; p1_emc_m_sdp[1][1][3][1]=2.25595; p2_emc_m_sdp[1][1][3][1]=-0.535514; p3_emc_m_sdp[1][1][3][1]=-6.69382; p4_emc_m_sdp[1][1][3][1]=6.09758; 
    p0_emc_s_sdp[1][1][3][1]=-0.338518; p1_emc_s_sdp[1][1][3][1]=4.27876; p2_emc_s_sdp[1][1][3][1]=-1.26247; p3_emc_s_sdp[1][1][3][1]=-10.7385; p4_emc_s_sdp[1][1][3][1]=10.0461; 
    p0_emc_m_sdp[0][1][4][1]=0.015005; p1_emc_m_sdp[0][1][4][1]=-0.260288; p2_emc_m_sdp[0][1][4][1]=0.18824; p3_emc_m_sdp[0][1][4][1]=-0.0865759; p4_emc_m_sdp[0][1][4][1]=0.0204336; 
    p0_emc_s_sdp[0][1][4][1]=0.893545; p1_emc_s_sdp[0][1][4][1]=-0.293147; p2_emc_s_sdp[0][1][4][1]=0.286433; p3_emc_s_sdp[0][1][4][1]=-0.109504; p4_emc_s_sdp[0][1][4][1]=0.0161161; 
    p0_emc_m_sdp[1][1][4][1]=-1.53454; p1_emc_m_sdp[1][1][4][1]=5.68646; p2_emc_m_sdp[1][1][4][1]=-1.12664; p3_emc_m_sdp[1][1][4][1]=-15.4579; p4_emc_m_sdp[1][1][4][1]=13.9597; 
    p0_emc_s_sdp[1][1][4][1]=-1.24106; p1_emc_s_sdp[1][1][4][1]=7.50856; p2_emc_s_sdp[1][1][4][1]=-2.0295; p3_emc_s_sdp[1][1][4][1]=-18.7149; p4_emc_s_sdp[1][1][4][1]=17.6341; 
    p0_emc_m_sdp[0][1][5][1]=-0.210448; p1_emc_m_sdp[0][1][5][1]=0.615469; p2_emc_m_sdp[0][1][5][1]=-0.657436; p3_emc_m_sdp[0][1][5][1]=0.279015; p4_emc_m_sdp[0][1][5][1]=-0.0399251; 
    p0_emc_s_sdp[0][1][5][1]=0.390346; p1_emc_s_sdp[0][1][5][1]=1.1925; p2_emc_s_sdp[0][1][5][1]=-1.15493; p3_emc_s_sdp[0][1][5][1]=0.455852; p4_emc_s_sdp[0][1][5][1]=-0.0605323; 
    p0_emc_m_sdp[1][1][5][1]=0.0225679; p1_emc_m_sdp[1][1][5][1]=0.073659; p2_emc_m_sdp[1][1][5][1]=0.0808701; p3_emc_m_sdp[1][1][5][1]=-0.0472215; p4_emc_m_sdp[1][1][5][1]=-0.468497; 
    p0_emc_s_sdp[1][1][5][1]=4.46399; p1_emc_s_sdp[1][1][5][1]=-13.9559; p2_emc_s_sdp[1][1][5][1]=2.9454; p3_emc_s_sdp[1][1][5][1]=37.7207; p4_emc_s_sdp[1][1][5][1]=-34.7152; 
    p0_emc_m_sdp[0][1][6][1]=-0.489308; p1_emc_m_sdp[0][1][6][1]=1.39428; p2_emc_m_sdp[0][1][6][1]=-1.37928; p3_emc_m_sdp[0][1][6][1]=0.558372; p4_emc_m_sdp[0][1][6][1]=-0.0781942; 
    p0_emc_s_sdp[0][1][6][1]=0.686762; p1_emc_s_sdp[0][1][6][1]=0.305463; p2_emc_s_sdp[0][1][6][1]=-0.247163; p3_emc_s_sdp[0][1][6][1]=0.0799387; p4_emc_s_sdp[0][1][6][1]=-0.00720193; 
    p0_emc_m_sdp[1][1][6][1]=1.95433; p1_emc_m_sdp[1][1][6][1]=-7.6054; p2_emc_m_sdp[1][1][6][1]=2.14004; p3_emc_m_sdp[1][1][6][1]=21.1422; p4_emc_m_sdp[1][1][6][1]=-20.7998; 
    p0_emc_s_sdp[1][1][6][1]=2.13567; p1_emc_s_sdp[1][1][6][1]=-5.23381; p2_emc_s_sdp[1][1][6][1]=1.50105; p3_emc_s_sdp[1][1][6][1]=13.2317; p4_emc_s_sdp[1][1][6][1]=-12.3598; 
    p0_emc_m_sdp[0][1][7][1]=-0.235665; p1_emc_m_sdp[0][1][7][1]=0.772983; p2_emc_m_sdp[0][1][7][1]=-0.77838; p3_emc_m_sdp[0][1][7][1]=0.310012; p4_emc_m_sdp[0][1][7][1]=-0.0418536; 
    p0_emc_s_sdp[0][1][7][1]=0.345105; p1_emc_s_sdp[0][1][7][1]=1.3278; p2_emc_s_sdp[0][1][7][1]=-1.26187; p3_emc_s_sdp[0][1][7][1]=0.485194; p4_emc_s_sdp[0][1][7][1]=-0.0620593; 
    p0_emc_m_sdp[1][1][7][1]=-2.64493; p1_emc_m_sdp[1][1][7][1]=10.1814; p2_emc_m_sdp[1][1][7][1]=-2.41806; p3_emc_m_sdp[1][1][7][1]=-26.6466; p4_emc_m_sdp[1][1][7][1]=24.6893; 
    p0_emc_s_sdp[1][1][7][1]=0.643483; p1_emc_s_sdp[1][1][7][1]=0.363948; p2_emc_s_sdp[1][1][7][1]=-0.0369113; p3_emc_s_sdp[1][1][7][1]=-0.307402; p4_emc_s_sdp[1][1][7][1]=0.126804; 
    p0_emc_m_sdp[0][1][8][1]=0.380924; p1_emc_m_sdp[0][1][8][1]=-0.916807; p2_emc_m_sdp[0][1][8][1]=0.824231; p3_emc_m_sdp[0][1][8][1]=-0.309368; p4_emc_m_sdp[0][1][8][1]=0.0402413; 
    p0_emc_s_sdp[0][1][8][1]=0.336923; p1_emc_s_sdp[0][1][8][1]=1.25216; p2_emc_s_sdp[0][1][8][1]=-1.09074; p3_emc_s_sdp[0][1][8][1]=0.387284; p4_emc_s_sdp[0][1][8][1]=-0.0446581; 
    p0_emc_m_sdp[1][1][8][1]=-1.27144; p1_emc_m_sdp[1][1][8][1]=5.28209; p2_emc_m_sdp[1][1][8][1]=-1.9385; p3_emc_m_sdp[1][1][8][1]=-13.4417; p4_emc_m_sdp[1][1][8][1]=13.4338; 
    p0_emc_s_sdp[1][1][8][1]=-1.50293; p1_emc_s_sdp[1][1][8][1]=8.8498; p2_emc_s_sdp[1][1][8][1]=-2.44221; p3_emc_s_sdp[1][1][8][1]=-23.3908; p4_emc_s_sdp[1][1][8][1]=22.5512; 
    p0_emc_m_sdp[0][1][9][1]=0.00952426; p1_emc_m_sdp[0][1][9][1]=0.0885124; p2_emc_m_sdp[0][1][9][1]=-0.208837; p3_emc_m_sdp[0][1][9][1]=0.149415; p4_emc_m_sdp[0][1][9][1]=-0.0336549; 
    p0_emc_s_sdp[0][1][9][1]=0.465996; p1_emc_s_sdp[0][1][9][1]=0.921671; p2_emc_s_sdp[0][1][9][1]=-0.820771; p3_emc_s_sdp[0][1][9][1]=0.307865; p4_emc_s_sdp[0][1][9][1]=-0.0388177; 
    p0_emc_m_sdp[1][1][9][1]=-1.27479; p1_emc_m_sdp[1][1][9][1]=5.14146; p2_emc_m_sdp[1][1][9][1]=-1.62135; p3_emc_m_sdp[1][1][9][1]=-14.3967; p4_emc_m_sdp[1][1][9][1]=14.6204; 
    p0_emc_s_sdp[1][1][9][1]=0.808422; p1_emc_s_sdp[1][1][9][1]=0.248429; p2_emc_s_sdp[1][1][9][1]=-0.485748; p3_emc_s_sdp[1][1][9][1]=-0.695954; p4_emc_s_sdp[1][1][9][1]=1.22632; 
    p0_emc_m_sdp[0][2][0][1]=-0.197309; p1_emc_m_sdp[0][2][0][1]=0.476317; p2_emc_m_sdp[0][2][0][1]=-0.518289; p3_emc_m_sdp[0][2][0][1]=0.218942; p4_emc_m_sdp[0][2][0][1]=-0.0298166; 
    p0_emc_s_sdp[0][2][0][1]=0.0778717; p1_emc_s_sdp[0][2][0][1]=2.01523; p2_emc_s_sdp[0][2][0][1]=-1.82639; p3_emc_s_sdp[0][2][0][1]=0.680469; p4_emc_s_sdp[0][2][0][1]=-0.0846833; 
    p0_emc_m_sdp[1][2][0][1]=0.16051; p1_emc_m_sdp[1][2][0][1]=-0.792766; p2_emc_m_sdp[1][2][0][1]=0.212717; p3_emc_m_sdp[1][2][0][1]=2.05469; p4_emc_m_sdp[1][2][0][1]=-1.85631; 
    p0_emc_s_sdp[1][2][0][1]=1.4742; p1_emc_s_sdp[1][2][0][1]=-2.31005; p2_emc_s_sdp[1][2][0][1]=0.378913; p3_emc_s_sdp[1][2][0][1]=6.08372; p4_emc_s_sdp[1][2][0][1]=-5.4777; 
    p0_emc_m_sdp[0][2][1][1]=-0.442955; p1_emc_m_sdp[0][2][1][1]=1.10406; p2_emc_m_sdp[0][2][1][1]=-1.10081; p3_emc_m_sdp[0][2][1][1]=0.445704; p4_emc_m_sdp[0][2][1][1]=-0.0603585; 
    p0_emc_s_sdp[0][2][1][1]=0.0753591; p1_emc_s_sdp[0][2][1][1]=1.99866; p2_emc_s_sdp[0][2][1][1]=-1.80936; p3_emc_s_sdp[0][2][1][1]=0.676388; p4_emc_s_sdp[0][2][1][1]=-0.0850339; 
    p0_emc_m_sdp[1][2][1][1]=-1.78367; p1_emc_m_sdp[1][2][1][1]=7.10491; p2_emc_m_sdp[1][2][1][1]=-2.16037; p3_emc_m_sdp[1][2][1][1]=-20.1417; p4_emc_m_sdp[1][2][1][1]=20.0417; 
    p0_emc_s_sdp[1][2][1][1]=-0.978087; p1_emc_s_sdp[1][2][1][1]=7.58202; p2_emc_s_sdp[1][2][1][1]=-2.64226; p3_emc_s_sdp[1][2][1][1]=-21.348; p4_emc_s_sdp[1][2][1][1]=21.5328; 
    p0_emc_m_sdp[0][2][2][1]=-0.524195; p1_emc_m_sdp[0][2][2][1]=1.32758; p2_emc_m_sdp[0][2][2][1]=-1.30445; p3_emc_m_sdp[0][2][2][1]=0.52291; p4_emc_m_sdp[0][2][2][1]=-0.0705364; 
    p0_emc_s_sdp[0][2][2][1]=0.194866; p1_emc_s_sdp[0][2][2][1]=1.71672; p2_emc_s_sdp[0][2][2][1]=-1.59301; p3_emc_s_sdp[0][2][2][1]=0.613126; p4_emc_s_sdp[0][2][2][1]=-0.0801633; 
    p0_emc_m_sdp[1][2][2][1]=-3.15301; p1_emc_m_sdp[1][2][2][1]=11.8417; p2_emc_m_sdp[1][2][2][1]=-2.79375; p3_emc_m_sdp[1][2][2][1]=-31.4411; p4_emc_m_sdp[1][2][2][1]=29.413; 
    p0_emc_s_sdp[1][2][2][1]=0.377789; p1_emc_s_sdp[1][2][2][1]=1.96495; p2_emc_s_sdp[1][2][2][1]=-0.605951; p3_emc_s_sdp[1][2][2][1]=-6.20344; p4_emc_s_sdp[1][2][2][1]=6.38907; 
    p0_emc_m_sdp[0][2][3][1]=-0.500105; p1_emc_m_sdp[0][2][3][1]=1.33386; p2_emc_m_sdp[0][2][3][1]=-1.34265; p3_emc_m_sdp[0][2][3][1]=0.543385; p4_emc_m_sdp[0][2][3][1]=-0.0736398; 
    p0_emc_s_sdp[0][2][3][1]=0.326062; p1_emc_s_sdp[0][2][3][1]=1.39891; p2_emc_s_sdp[0][2][3][1]=-1.3215; p3_emc_s_sdp[0][2][3][1]=0.513705; p4_emc_s_sdp[0][2][3][1]=-0.0675785; 
    p0_emc_m_sdp[1][2][3][1]=-2.64088; p1_emc_m_sdp[1][2][3][1]=9.42223; p2_emc_m_sdp[1][2][3][1]=-1.60095; p3_emc_m_sdp[1][2][3][1]=-23.9118; p4_emc_m_sdp[1][2][3][1]=20.979; 
    p0_emc_s_sdp[1][2][3][1]=3.77002; p1_emc_s_sdp[1][2][3][1]=-11.5184; p2_emc_s_sdp[1][2][3][1]=3.20357; p3_emc_s_sdp[1][2][3][1]=30.4041; p4_emc_s_sdp[1][2][3][1]=-28.9888; 
    p0_emc_m_sdp[0][2][4][1]=-1.22036; p1_emc_m_sdp[0][2][4][1]=3.30983; p2_emc_m_sdp[0][2][4][1]=-3.21894; p3_emc_m_sdp[0][2][4][1]=1.26759; p4_emc_m_sdp[0][2][4][1]=-0.170122; 
    p0_emc_s_sdp[0][2][4][1]=0.4155; p1_emc_s_sdp[0][2][4][1]=1.15154; p2_emc_s_sdp[0][2][4][1]=-1.09928; p3_emc_s_sdp[0][2][4][1]=0.426646; p4_emc_s_sdp[0][2][4][1]=-0.055574; 
    p0_emc_m_sdp[1][2][4][1]=-2.22582; p1_emc_m_sdp[1][2][4][1]=8.93159; p2_emc_m_sdp[1][2][4][1]=-2.63671; p3_emc_m_sdp[1][2][4][1]=-24.3009; p4_emc_m_sdp[1][2][4][1]=23.513; 
    p0_emc_s_sdp[1][2][4][1]=2.614; p1_emc_s_sdp[1][2][4][1]=-6.96801; p2_emc_s_sdp[1][2][4][1]=1.76887; p3_emc_s_sdp[1][2][4][1]=18.6765; p4_emc_s_sdp[1][2][4][1]=-17.6327; 
    p0_emc_m_sdp[0][2][5][1]=-0.750118; p1_emc_m_sdp[0][2][5][1]=1.92616; p2_emc_m_sdp[0][2][5][1]=-1.85765; p3_emc_m_sdp[0][2][5][1]=0.722983; p4_emc_m_sdp[0][2][5][1]=-0.095955; 
    p0_emc_s_sdp[0][2][5][1]=0.279475; p1_emc_s_sdp[0][2][5][1]=1.44395; p2_emc_s_sdp[0][2][5][1]=-1.32676; p3_emc_s_sdp[0][2][5][1]=0.498653; p4_emc_s_sdp[0][2][5][1]=-0.0634824; 
    p0_emc_m_sdp[1][2][5][1]=1.51914; p1_emc_m_sdp[1][2][5][1]=-6.91536; p2_emc_m_sdp[1][2][5][1]=3.06152; p3_emc_m_sdp[1][2][5][1]=19.6053; p4_emc_m_sdp[1][2][5][1]=-21.1001; 
    p0_emc_s_sdp[1][2][5][1]=1.92065; p1_emc_s_sdp[1][2][5][1]=-4.83856; p2_emc_s_sdp[1][2][5][1]=1.83652; p3_emc_s_sdp[1][2][5][1]=13.6821; p4_emc_s_sdp[1][2][5][1]=-14.0668; 
    p0_emc_m_sdp[0][2][6][1]=-1.0103; p1_emc_m_sdp[0][2][6][1]=2.61825; p2_emc_m_sdp[0][2][6][1]=-2.50186; p3_emc_m_sdp[0][2][6][1]=0.969117; p4_emc_m_sdp[0][2][6][1]=-0.12833; 
    p0_emc_s_sdp[0][2][6][1]=0.320133; p1_emc_s_sdp[0][2][6][1]=1.40609; p2_emc_s_sdp[0][2][6][1]=-1.30145; p3_emc_s_sdp[0][2][6][1]=0.494366; p4_emc_s_sdp[0][2][6][1]=-0.0637711; 
    p0_emc_m_sdp[1][2][6][1]=-1.90681; p1_emc_m_sdp[1][2][6][1]=7.4057; p2_emc_m_sdp[1][2][6][1]=-2.25357; p3_emc_m_sdp[1][2][6][1]=-19.9288; p4_emc_m_sdp[1][2][6][1]=19.3977; 
    p0_emc_s_sdp[1][2][6][1]=-0.931038; p1_emc_s_sdp[1][2][6][1]=6.38641; p2_emc_s_sdp[1][2][6][1]=-1.13096; p3_emc_s_sdp[1][2][6][1]=-16.6593; p4_emc_s_sdp[1][2][6][1]=14.8113; 
    p0_emc_m_sdp[0][2][7][1]=-0.807221; p1_emc_m_sdp[0][2][7][1]=2.02126; p2_emc_m_sdp[0][2][7][1]=-1.90574; p3_emc_m_sdp[0][2][7][1]=0.731385; p4_emc_m_sdp[0][2][7][1]=-0.0961321; 
    p0_emc_s_sdp[0][2][7][1]=0.281402; p1_emc_s_sdp[0][2][7][1]=1.52089; p2_emc_s_sdp[0][2][7][1]=-1.42043; p3_emc_s_sdp[0][2][7][1]=0.54531; p4_emc_s_sdp[0][2][7][1]=-0.0710249; 
    p0_emc_m_sdp[1][2][7][1]=-0.01855; p1_emc_m_sdp[1][2][7][1]=-0.085661; p2_emc_m_sdp[1][2][7][1]=-0.124573; p3_emc_m_sdp[1][2][7][1]=-0.0214845; p4_emc_m_sdp[1][2][7][1]=0.455347; 
    p0_emc_s_sdp[1][2][7][1]=1.55771; p1_emc_s_sdp[1][2][7][1]=-2.73955; p2_emc_s_sdp[1][2][7][1]=0.566317; p3_emc_s_sdp[1][2][7][1]=7.14459; p4_emc_s_sdp[1][2][7][1]=-6.41912; 
    p0_emc_m_sdp[0][2][8][1]=-0.755083; p1_emc_m_sdp[0][2][8][1]=1.79655; p2_emc_m_sdp[0][2][8][1]=-1.63585; p3_emc_m_sdp[0][2][8][1]=0.607462; p4_emc_m_sdp[0][2][8][1]=-0.0771305; 
    p0_emc_s_sdp[0][2][8][1]=0.424567; p1_emc_s_sdp[0][2][8][1]=1.16038; p2_emc_s_sdp[0][2][8][1]=-1.08596; p3_emc_s_sdp[0][2][8][1]=0.408919; p4_emc_s_sdp[0][2][8][1]=-0.0503757; 
    p0_emc_m_sdp[1][2][8][1]=-0.1807; p1_emc_m_sdp[1][2][8][1]=0.0839001; p2_emc_m_sdp[1][2][8][1]=0.344216; p3_emc_m_sdp[1][2][8][1]=0.26003; p4_emc_m_sdp[1][2][8][1]=-0.86294; 
    p0_emc_s_sdp[1][2][8][1]=3.07633; p1_emc_s_sdp[1][2][8][1]=-8.93313; p2_emc_s_sdp[1][2][8][1]=2.19806; p3_emc_s_sdp[1][2][8][1]=25.1921; p4_emc_s_sdp[1][2][8][1]=-24.0381; 
    p0_emc_m_sdp[0][2][9][1]=-0.221279; p1_emc_m_sdp[0][2][9][1]=0.32163; p2_emc_m_sdp[0][2][9][1]=-0.257184; p3_emc_m_sdp[0][2][9][1]=0.0928254; p4_emc_m_sdp[0][2][9][1]=-0.0125418; 
    p0_emc_s_sdp[0][2][9][1]=0.82787; p1_emc_s_sdp[0][2][9][1]=0.012567; p2_emc_s_sdp[0][2][9][1]=0.00496468; p3_emc_s_sdp[0][2][9][1]=-0.000198654; p4_emc_s_sdp[0][2][9][1]=0.000208939; 
    p0_emc_m_sdp[1][2][9][1]=-1.12027; p1_emc_m_sdp[1][2][9][1]=3.79222; p2_emc_m_sdp[1][2][9][1]=-0.428505; p3_emc_m_sdp[1][2][9][1]=-10.8392; p4_emc_m_sdp[1][2][9][1]=9.72748; 
    p0_emc_s_sdp[1][2][9][1]=3.84235; p1_emc_s_sdp[1][2][9][1]=-12.2858; p2_emc_s_sdp[1][2][9][1]=3.84486; p3_emc_s_sdp[1][2][9][1]=34.2294; p4_emc_s_sdp[1][2][9][1]=-33.9842; 

	
    //EMC DZ NEG 27 
    
	
    p0_emc_m_sdz[0][0][0][0]=0.418982; p1_emc_m_sdz[0][0][0][0]=-0.929245; p2_emc_m_sdz[0][0][0][0]=0.741518; p3_emc_m_sdz[0][0][0][0]=-0.261858; p4_emc_m_sdz[0][0][0][0]=0.0349696; 
    p0_emc_s_sdz[0][0][0][0]=1.02254; p1_emc_s_sdz[0][0][0][0]=-0.345185; p2_emc_s_sdz[0][0][0][0]=0.363565; p3_emc_s_sdz[0][0][0][0]=-0.12711; p4_emc_s_sdz[0][0][0][0]=0.0137056; 
    p0_emc_m_sdz[1][0][0][0]=2.22233; p1_emc_m_sdz[1][0][0][0]=-8.22945; p2_emc_m_sdz[1][0][0][0]=1.33901; p3_emc_m_sdz[1][0][0][0]=19.2522; p4_emc_m_sdz[1][0][0][0]=-14.6547; 
    p0_emc_s_sdz[1][0][0][0]=0.777342; p1_emc_s_sdz[1][0][0][0]=0.442412; p2_emc_s_sdz[1][0][0][0]=-0.089569; p3_emc_s_sdz[1][0][0][0]=-0.46429; p4_emc_s_sdz[1][0][0][0]=0.120903; 
    p0_emc_m_sdz[0][0][1][0]=0.179518; p1_emc_m_sdz[0][0][1][0]=-0.439882; p2_emc_m_sdz[0][0][1][0]=0.415207; p3_emc_m_sdz[0][0][1][0]=-0.174731; p4_emc_m_sdz[0][0][1][0]=0.0270043; 
    p0_emc_s_sdz[0][0][1][0]=1.08685; p1_emc_s_sdz[0][0][1][0]=-0.477242; p2_emc_s_sdz[0][0][1][0]=0.435152; p3_emc_s_sdz[0][0][1][0]=-0.154576; p4_emc_s_sdz[0][0][1][0]=0.0195284; 
    p0_emc_m_sdz[1][0][1][0]=2.3575; p1_emc_m_sdz[1][0][1][0]=-9.23216; p2_emc_m_sdz[1][0][1][0]=1.96674; p3_emc_m_sdz[1][0][1][0]=25.9563; p4_emc_m_sdz[1][0][1][0]=-23.9505; 
    p0_emc_s_sdz[1][0][1][0]=1.27811; p1_emc_s_sdz[1][0][1][0]=-0.0289316; p2_emc_s_sdz[1][0][1][0]=-1.44382; p3_emc_s_sdz[1][0][1][0]=-1.35696; p4_emc_s_sdz[1][0][1][0]=3.40437; 
    p0_emc_m_sdz[0][0][2][0]=0.192111; p1_emc_m_sdz[0][0][2][0]=-0.44969; p2_emc_m_sdz[0][0][2][0]=0.392591; p3_emc_m_sdz[0][0][2][0]=-0.145444; p4_emc_m_sdz[0][0][2][0]=0.0194659; 
    p0_emc_s_sdz[0][0][2][0]=0.782002; p1_emc_s_sdz[0][0][2][0]=0.271632; p2_emc_s_sdz[0][0][2][0]=-0.236716; p3_emc_s_sdz[0][0][2][0]=0.0977772; p4_emc_s_sdz[0][0][2][0]=-0.0138089; 
    p0_emc_m_sdz[1][0][2][0]=-2.23097; p1_emc_m_sdz[1][0][2][0]=6.52844; p2_emc_m_sdz[1][0][2][0]=0.832382; p3_emc_m_sdz[1][0][2][0]=-14.5416; p4_emc_m_sdz[1][0][2][0]=9.21322; 
    p0_emc_s_sdz[1][0][2][0]=7.23154; p1_emc_s_sdz[1][0][2][0]=-26.5815; p2_emc_s_sdz[1][0][2][0]=8.9485; p3_emc_s_sdz[1][0][2][0]=75.8112; p4_emc_s_sdz[1][0][2][0]=-76.8576; 
    p0_emc_m_sdz[0][0][3][0]=0.144133; p1_emc_m_sdz[0][0][3][0]=-0.379546; p2_emc_m_sdz[0][0][3][0]=0.391443; p3_emc_m_sdz[0][0][3][0]=-0.174203; p4_emc_m_sdz[0][0][3][0]=0.0275411; 
    p0_emc_s_sdz[0][0][3][0]=0.909385; p1_emc_s_sdz[0][0][3][0]=-0.166487; p2_emc_s_sdz[0][0][3][0]=0.193915; p3_emc_s_sdz[0][0][3][0]=-0.0705226; p4_emc_s_sdz[0][0][3][0]=0.00907963; 
    p0_emc_m_sdz[1][0][3][0]=-0.0897278; p1_emc_m_sdz[1][0][3][0]=0.0321925; p2_emc_m_sdz[1][0][3][0]=0.170179; p3_emc_m_sdz[1][0][3][0]=0.196332; p4_emc_m_sdz[1][0][3][0]=-0.19676; 
    p0_emc_s_sdz[1][0][3][0]=4.01346; p1_emc_s_sdz[1][0][3][0]=-11.1517; p2_emc_s_sdz[1][0][3][0]=1.67146; p3_emc_s_sdz[1][0][3][0]=28.9704; p4_emc_s_sdz[1][0][3][0]=-25.6257; 
    p0_emc_m_sdz[0][0][4][0]=0.199394; p1_emc_m_sdz[0][0][4][0]=-0.427199; p2_emc_m_sdz[0][0][4][0]=0.288388; p3_emc_m_sdz[0][0][4][0]=-0.0719223; p4_emc_m_sdz[0][0][4][0]=0.00546415; 
    p0_emc_s_sdz[0][0][4][0]=1.06545; p1_emc_s_sdz[0][0][4][0]=-0.398019; p2_emc_s_sdz[0][0][4][0]=0.272893; p3_emc_s_sdz[0][0][4][0]=-0.0702723; p4_emc_s_sdz[0][0][4][0]=0.0063817; 
    p0_emc_m_sdz[1][0][4][0]=-0.889547; p1_emc_m_sdz[1][0][4][0]=0.989629; p2_emc_m_sdz[1][0][4][0]=2.57495; p3_emc_m_sdz[1][0][4][0]=1.39013; p4_emc_m_sdz[1][0][4][0]=-7.46597; 
    p0_emc_s_sdz[1][0][4][0]=1.6939; p1_emc_s_sdz[1][0][4][0]=-0.837629; p2_emc_s_sdz[1][0][4][0]=-3.21422; p3_emc_s_sdz[1][0][4][0]=-2.03964; p4_emc_s_sdz[1][0][4][0]=9.49325; 
    p0_emc_m_sdz[0][0][5][0]=-0.135403; p1_emc_m_sdz[0][0][5][0]=0.397731; p2_emc_m_sdz[0][0][5][0]=-0.386895; p3_emc_m_sdz[0][0][5][0]=0.150152; p4_emc_m_sdz[0][0][5][0]=-0.0206371; 
    p0_emc_s_sdz[0][0][5][0]=0.782359; p1_emc_s_sdz[0][0][5][0]=0.202133; p2_emc_s_sdz[0][0][5][0]=-0.130296; p3_emc_s_sdz[0][0][5][0]=0.0329106; p4_emc_s_sdz[0][0][5][0]=-0.00174764; 
    p0_emc_m_sdz[1][0][5][0]=-5.64369; p1_emc_m_sdz[1][0][5][0]=22.3205; p2_emc_m_sdz[1][0][5][0]=-7.49259; p3_emc_m_sdz[1][0][5][0]=-57.3438; p4_emc_m_sdz[1][0][5][0]=56.4851; 
    p0_emc_s_sdz[1][0][5][0]=-2.57314; p1_emc_s_sdz[1][0][5][0]=13.3517; p2_emc_s_sdz[1][0][5][0]=-2.99168; p3_emc_s_sdz[1][0][5][0]=-37.323; p4_emc_s_sdz[1][0][5][0]=35.3853; 
    p0_emc_m_sdz[0][0][6][0]=-0.108318; p1_emc_m_sdz[0][0][6][0]=0.314304; p2_emc_m_sdz[0][0][6][0]=-0.322814; p3_emc_m_sdz[0][0][6][0]=0.135603; p4_emc_m_sdz[0][0][6][0]=-0.0200712; 
    p0_emc_s_sdz[0][0][6][0]=1.04189; p1_emc_s_sdz[0][0][6][0]=-0.411712; p2_emc_s_sdz[0][0][6][0]=0.357944; p3_emc_s_sdz[0][0][6][0]=-0.116867; p4_emc_s_sdz[0][0][6][0]=0.0136594; 
    p0_emc_m_sdz[1][0][6][0]=2.12531; p1_emc_m_sdz[1][0][6][0]=-8.2536; p2_emc_m_sdz[1][0][6][0]=2.40673; p3_emc_m_sdz[1][0][6][0]=21.4503; p4_emc_m_sdz[1][0][6][0]=-20.8072; 
    p0_emc_s_sdz[1][0][6][0]=0.812879; p1_emc_s_sdz[1][0][6][0]=0.332885; p2_emc_s_sdz[1][0][6][0]=-0.295382; p3_emc_s_sdz[1][0][6][0]=-0.552314; p4_emc_s_sdz[1][0][6][0]=0.754133; 
    p0_emc_m_sdz[0][0][7][0]=-0.295993; p1_emc_m_sdz[0][0][7][0]=0.677789; p2_emc_m_sdz[0][0][7][0]=-0.580919; p3_emc_m_sdz[0][0][7][0]=0.217582; p4_emc_m_sdz[0][0][7][0]=-0.030006; 
    p0_emc_s_sdz[0][0][7][0]=1.11042; p1_emc_s_sdz[0][0][7][0]=-0.523775; p2_emc_s_sdz[0][0][7][0]=0.447117; p3_emc_s_sdz[0][0][7][0]=-0.151565; p4_emc_s_sdz[0][0][7][0]=0.0187789; 
    p0_emc_m_sdz[1][0][7][0]=-0.0643116; p1_emc_m_sdz[1][0][7][0]=0.158453; p2_emc_m_sdz[1][0][7][0]=0.310046; p3_emc_m_sdz[1][0][7][0]=0.0457888; p4_emc_m_sdz[1][0][7][0]=-1.29074; 
    p0_emc_s_sdz[1][0][7][0]=5.12682; p1_emc_s_sdz[1][0][7][0]=-17.633; p2_emc_s_sdz[1][0][7][0]=6.14361; p3_emc_s_sdz[1][0][7][0]=48.8118; p4_emc_s_sdz[1][0][7][0]=-48.9409; 
    p0_emc_m_sdz[0][0][8][0]=-0.493965; p1_emc_m_sdz[0][0][8][0]=1.21181; p2_emc_m_sdz[0][0][8][0]=-1.06854; p3_emc_m_sdz[0][0][8][0]=0.397363; p4_emc_m_sdz[0][0][8][0]=-0.0531004; 
    p0_emc_s_sdz[0][0][8][0]=0.913415; p1_emc_s_sdz[0][0][8][0]=-0.0513747; p2_emc_s_sdz[0][0][8][0]=0.0722596; p3_emc_s_sdz[0][0][8][0]=-0.0360078; p4_emc_s_sdz[0][0][8][0]=0.0068265; 
    p0_emc_m_sdz[1][0][8][0]=-0.62076; p1_emc_m_sdz[1][0][8][0]=2.76629; p2_emc_m_sdz[1][0][8][0]=-0.891988; p3_emc_m_sdz[1][0][8][0]=-7.36936; p4_emc_m_sdz[1][0][8][0]=6.49052; 
    p0_emc_s_sdz[1][0][8][0]=2.66416; p1_emc_s_sdz[1][0][8][0]=-7.51099; p2_emc_s_sdz[1][0][8][0]=2.88121; p3_emc_s_sdz[1][0][8][0]=20.2834; p4_emc_s_sdz[1][0][8][0]=-20.4932; 
    p0_emc_m_sdz[0][0][9][0]=-0.594789; p1_emc_m_sdz[0][0][9][0]=1.39625; p2_emc_m_sdz[0][0][9][0]=-1.19373; p3_emc_m_sdz[0][0][9][0]=0.429534; p4_emc_m_sdz[0][0][9][0]=-0.0559392; 
    p0_emc_s_sdz[0][0][9][0]=0.957962; p1_emc_s_sdz[0][0][9][0]=-0.188019; p2_emc_s_sdz[0][0][9][0]=0.215348; p3_emc_s_sdz[0][0][9][0]=-0.0825386; p4_emc_s_sdz[0][0][9][0]=0.0101151; 
    p0_emc_m_sdz[1][0][9][0]=-6.87113; p1_emc_m_sdz[1][0][9][0]=25.9287; p2_emc_m_sdz[1][0][9][0]=-6.26237; p3_emc_m_sdz[1][0][9][0]=-66.2612; p4_emc_m_sdz[1][0][9][0]=60.426; 
    p0_emc_s_sdz[1][0][9][0]=0.15507; p1_emc_s_sdz[1][0][9][0]=1.16766; p2_emc_s_sdz[1][0][9][0]=1.82072; p3_emc_s_sdz[1][0][9][0]=0.623605; p4_emc_s_sdz[1][0][9][0]=-5.34212; 
    p0_emc_m_sdz[0][1][0][0]=0.260319; p1_emc_m_sdz[0][1][0][0]=-0.851765; p2_emc_m_sdz[0][1][0][0]=0.805046; p3_emc_m_sdz[0][1][0][0]=-0.316125; p4_emc_m_sdz[0][1][0][0]=0.0442619; 
    p0_emc_s_sdz[0][1][0][0]=0.737523; p1_emc_s_sdz[0][1][0][0]=0.348302; p2_emc_s_sdz[0][1][0][0]=-0.250003; p3_emc_s_sdz[0][1][0][0]=0.0726858; p4_emc_s_sdz[0][1][0][0]=-0.00678329; 
    p0_emc_m_sdz[1][1][0][0]=0.90091; p1_emc_m_sdz[1][1][0][0]=-1.22547; p2_emc_m_sdz[1][1][0][0]=-3.0107; p3_emc_m_sdz[1][1][0][0]=-1.56614; p4_emc_m_sdz[1][1][0][0]=8.87229; 
    p0_emc_s_sdz[1][1][0][0]=7.17663; p1_emc_s_sdz[1][1][0][0]=-23.3046; p2_emc_s_sdz[1][1][0][0]=3.22877; p3_emc_s_sdz[1][1][0][0]=64.7837; p4_emc_s_sdz[1][1][0][0]=-57.5594; 
    p0_emc_m_sdz[0][1][1][0]=0.106728; p1_emc_m_sdz[0][1][1][0]=-0.43458; p2_emc_m_sdz[0][1][1][0]=0.45468; p3_emc_m_sdz[0][1][1][0]=-0.192357; p4_emc_m_sdz[0][1][1][0]=0.0285497; 
    p0_emc_s_sdz[0][1][1][0]=0.951383; p1_emc_s_sdz[0][1][1][0]=-0.153939; p2_emc_s_sdz[0][1][1][0]=0.130473; p3_emc_s_sdz[0][1][1][0]=-0.0442609; p4_emc_s_sdz[0][1][1][0]=0.00547032; 
    p0_emc_m_sdz[1][1][1][0]=1.76298; p1_emc_m_sdz[1][1][1][0]=-8.38675; p2_emc_m_sdz[1][1][1][0]=3.75701; p3_emc_m_sdz[1][1][1][0]=24.5705; p4_emc_m_sdz[1][1][1][0]=-26.3728; 
    p0_emc_s_sdz[1][1][1][0]=7.12944; p1_emc_s_sdz[1][1][1][0]=-24.4063; p2_emc_s_sdz[1][1][1][0]=5.82834; p3_emc_s_sdz[1][1][1][0]=67.6779; p4_emc_s_sdz[1][1][1][0]=-64.0809; 
    p0_emc_m_sdz[0][1][2][0]=0.10139; p1_emc_m_sdz[0][1][2][0]=-0.37762; p2_emc_m_sdz[0][1][2][0]=0.385282; p3_emc_m_sdz[0][1][2][0]=-0.158786; p4_emc_m_sdz[0][1][2][0]=0.0229207; 
    p0_emc_s_sdz[0][1][2][0]=0.771005; p1_emc_s_sdz[0][1][2][0]=0.201999; p2_emc_s_sdz[0][1][2][0]=-0.145168; p3_emc_s_sdz[0][1][2][0]=0.0485043; p4_emc_s_sdz[0][1][2][0]=-0.0053401; 
    p0_emc_m_sdz[1][1][2][0]=1.57474; p1_emc_m_sdz[1][1][2][0]=-5.63476; p2_emc_m_sdz[1][1][2][0]=0.959996; p3_emc_m_sdz[1][1][2][0]=13.4004; p4_emc_m_sdz[1][1][2][0]=-11.4165; 
    p0_emc_s_sdz[1][1][2][0]=2.1871; p1_emc_s_sdz[1][1][2][0]=-5.75431; p2_emc_s_sdz[1][1][2][0]=2.58665; p3_emc_s_sdz[1][1][2][0]=16.4565; p4_emc_s_sdz[1][1][2][0]=-18.0185; 
    p0_emc_m_sdz[0][1][3][0]=-0.10125; p1_emc_m_sdz[0][1][3][0]=0.00861335; p2_emc_m_sdz[0][1][3][0]=0.0811408; p3_emc_m_sdz[0][1][3][0]=-0.0555702; p4_emc_m_sdz[0][1][3][0]=0.0102979; 
    p0_emc_s_sdz[0][1][3][0]=0.845127; p1_emc_s_sdz[0][1][3][0]=0.0203811; p2_emc_s_sdz[0][1][3][0]=0.0122496; p3_emc_s_sdz[0][1][3][0]=-0.00910876; p4_emc_s_sdz[0][1][3][0]=0.00219251; 
    p0_emc_m_sdz[1][1][3][0]=-5.80927; p1_emc_m_sdz[1][1][3][0]=22.4009; p2_emc_m_sdz[1][1][3][0]=-6.6554; p3_emc_m_sdz[1][1][3][0]=-57.5067; p4_emc_m_sdz[1][1][3][0]=54.7934; 
    p0_emc_s_sdz[1][1][3][0]=1.11972; p1_emc_s_sdz[1][1][3][0]=-0.0635686; p2_emc_s_sdz[1][1][3][0]=-1.32108; p3_emc_s_sdz[1][1][3][0]=-1.14734; p4_emc_s_sdz[1][1][3][0]=3.74836; 
    p0_emc_m_sdz[0][1][4][0]=0.00468622; p1_emc_m_sdz[0][1][4][0]=-0.0531715; p2_emc_m_sdz[0][1][4][0]=0.0738218; p3_emc_m_sdz[0][1][4][0]=-0.0366258; p4_emc_m_sdz[0][1][4][0]=0.00612136; 
    p0_emc_s_sdz[0][1][4][0]=1.20951; p1_emc_s_sdz[0][1][4][0]=-0.686007; p2_emc_s_sdz[0][1][4][0]=0.526734; p3_emc_s_sdz[0][1][4][0]=-0.169811; p4_emc_s_sdz[0][1][4][0]=0.0200315; 
    p0_emc_m_sdz[1][1][4][0]=6.31968; p1_emc_m_sdz[1][1][4][0]=-25.3081; p2_emc_m_sdz[1][1][4][0]=6.92978; p3_emc_m_sdz[1][1][4][0]=69.734; p4_emc_m_sdz[1][1][4][0]=-67.035; 
    p0_emc_s_sdz[1][1][4][0]=3.79494; p1_emc_s_sdz[1][1][4][0]=-12.1461; p2_emc_s_sdz[1][1][4][0]=3.48675; p3_emc_s_sdz[1][1][4][0]=36.5195; p4_emc_s_sdz[1][1][4][0]=-36.5468; 
    p0_emc_m_sdz[0][1][5][0]=0.172269; p1_emc_m_sdz[0][1][5][0]=-0.0829769; p2_emc_m_sdz[0][1][5][0]=-0.0235836; p3_emc_m_sdz[0][1][5][0]=0.0282377; p4_emc_m_sdz[0][1][5][0]=-0.00589955; 
    p0_emc_s_sdz[0][1][5][0]=0.931244; p1_emc_s_sdz[0][1][5][0]=-0.177357; p2_emc_s_sdz[0][1][5][0]=0.154655; p3_emc_s_sdz[0][1][5][0]=-0.0498833; p4_emc_s_sdz[0][1][5][0]=0.0062446; 
    p0_emc_m_sdz[1][1][5][0]=5.52057; p1_emc_m_sdz[1][1][5][0]=-20.6756; p2_emc_m_sdz[1][1][5][0]=5.04267; p3_emc_m_sdz[1][1][5][0]=54.8436; p4_emc_m_sdz[1][1][5][0]=-51.4149; 
    p0_emc_s_sdz[1][1][5][0]=-3.79213; p1_emc_s_sdz[1][1][5][0]=18.4221; p2_emc_s_sdz[1][1][5][0]=-5.38553; p3_emc_s_sdz[1][1][5][0]=-51.6488; p4_emc_s_sdz[1][1][5][0]=51.5213; 
    p0_emc_m_sdz[0][1][6][0]=-0.0341422; p1_emc_m_sdz[0][1][6][0]=0.226781; p2_emc_m_sdz[0][1][6][0]=-0.262964; p3_emc_m_sdz[0][1][6][0]=0.116367; p4_emc_m_sdz[0][1][6][0]=-0.0178311; 
    p0_emc_s_sdz[0][1][6][0]=0.901221; p1_emc_s_sdz[0][1][6][0]=-0.0729358; p2_emc_s_sdz[0][1][6][0]=0.0805958; p3_emc_s_sdz[0][1][6][0]=-0.0256175; p4_emc_s_sdz[0][1][6][0]=0.00326813; 
    p0_emc_m_sdz[1][1][6][0]=-2.73265; p1_emc_m_sdz[1][1][6][0]=9.88351; p2_emc_m_sdz[1][1][6][0]=-1.34901; p3_emc_m_sdz[1][1][6][0]=-25.5615; p4_emc_m_sdz[1][1][6][0]=21.943; 
    p0_emc_s_sdz[1][1][6][0]=0.724128; p1_emc_s_sdz[1][1][6][0]=0.441526; p2_emc_s_sdz[1][1][6][0]=0.0176725; p3_emc_s_sdz[1][1][6][0]=-0.356244; p4_emc_s_sdz[1][1][6][0]=-0.258054; 
    p0_emc_m_sdz[0][1][7][0]=-0.0141785; p1_emc_m_sdz[0][1][7][0]=0.202402; p2_emc_m_sdz[0][1][7][0]=-0.241105; p3_emc_m_sdz[0][1][7][0]=0.109187; p4_emc_m_sdz[0][1][7][0]=-0.0174094; 
    p0_emc_s_sdz[0][1][7][0]=0.960057; p1_emc_s_sdz[0][1][7][0]=-0.185433; p2_emc_s_sdz[0][1][7][0]=0.162102; p3_emc_s_sdz[0][1][7][0]=-0.0524693; p4_emc_s_sdz[0][1][7][0]=0.0066891; 
    p0_emc_m_sdz[1][1][7][0]=-2.1197; p1_emc_m_sdz[1][1][7][0]=7.71487; p2_emc_m_sdz[1][1][7][0]=-1.11886; p3_emc_m_sdz[1][1][7][0]=-19.5301; p4_emc_m_sdz[1][1][7][0]=16.6414; 
    p0_emc_s_sdz[1][1][7][0]=2.25823; p1_emc_s_sdz[1][1][7][0]=-4.80125; p2_emc_s_sdz[1][1][7][0]=0.99422; p3_emc_s_sdz[1][1][7][0]=10.6806; p4_emc_s_sdz[1][1][7][0]=-9.01326; 
    p0_emc_m_sdz[0][1][8][0]=0.0325159; p1_emc_m_sdz[0][1][8][0]=0.184652; p2_emc_m_sdz[0][1][8][0]=-0.233817; p3_emc_m_sdz[0][1][8][0]=0.103528; p4_emc_m_sdz[0][1][8][0]=-0.0160121; 
    p0_emc_s_sdz[0][1][8][0]=0.925283; p1_emc_s_sdz[0][1][8][0]=-0.209272; p2_emc_s_sdz[0][1][8][0]=0.250401; p3_emc_s_sdz[0][1][8][0]=-0.095848; p4_emc_s_sdz[0][1][8][0]=0.0124622; 
    p0_emc_m_sdz[1][1][8][0]=-1.19434; p1_emc_m_sdz[1][1][8][0]=4.757; p2_emc_m_sdz[1][1][8][0]=-1.04507; p3_emc_m_sdz[1][1][8][0]=-12.4207; p4_emc_m_sdz[1][1][8][0]=11.4031; 
    p0_emc_s_sdz[1][1][8][0]=4.8563; p1_emc_s_sdz[1][1][8][0]=-15.9371; p2_emc_s_sdz[1][1][8][0]=4.91157; p3_emc_s_sdz[1][1][8][0]=42.3873; p4_emc_s_sdz[1][1][8][0]=-41.1882; 
    p0_emc_m_sdz[0][1][9][0]=-0.0574434; p1_emc_m_sdz[0][1][9][0]=0.476723; p2_emc_m_sdz[0][1][9][0]=-0.518172; p3_emc_m_sdz[0][1][9][0]=0.217257; p4_emc_m_sdz[0][1][9][0]=-0.0323111; 
    p0_emc_s_sdz[0][1][9][0]=1.04264; p1_emc_s_sdz[0][1][9][0]=-0.473393; p2_emc_s_sdz[0][1][9][0]=0.44099; p3_emc_s_sdz[0][1][9][0]=-0.144829; p4_emc_s_sdz[0][1][9][0]=0.0160623; 
    p0_emc_m_sdz[1][1][9][0]=3.30862; p1_emc_m_sdz[1][1][9][0]=-14.1394; p2_emc_m_sdz[1][1][9][0]=6.2997; p3_emc_m_sdz[1][1][9][0]=38.1605; p4_emc_m_sdz[1][1][9][0]=-40.3199; 
    p0_emc_s_sdz[1][1][9][0]=-2.64676; p1_emc_s_sdz[1][1][9][0]=14.7064; p2_emc_s_sdz[1][1][9][0]=-5.66934; p3_emc_s_sdz[1][1][9][0]=-39.3279; p4_emc_s_sdz[1][1][9][0]=40.1065; 
    p0_emc_m_sdz[0][2][0][0]=0.211797; p1_emc_m_sdz[0][2][0][0]=-0.633687; p2_emc_m_sdz[0][2][0][0]=0.586826; p3_emc_m_sdz[0][2][0][0]=-0.227343; p4_emc_m_sdz[0][2][0][0]=0.0319465; 
    p0_emc_s_sdz[0][2][0][0]=0.788293; p1_emc_s_sdz[0][2][0][0]=0.31643; p2_emc_s_sdz[0][2][0][0]=-0.27239; p3_emc_s_sdz[0][2][0][0]=0.100592; p4_emc_s_sdz[0][2][0][0]=-0.0122758; 
    p0_emc_m_sdz[1][2][0][0]=0.554313; p1_emc_m_sdz[1][2][0][0]=-0.704699; p2_emc_m_sdz[1][2][0][0]=-1.82677; p3_emc_m_sdz[1][2][0][0]=-1.01162; p4_emc_m_sdz[1][2][0][0]=5.42242; 
    p0_emc_s_sdz[1][2][0][0]=4.64274; p1_emc_s_sdz[1][2][0][0]=-12.5607; p2_emc_s_sdz[1][2][0][0]=-0.467307; p3_emc_s_sdz[1][2][0][0]=36.1063; p4_emc_s_sdz[1][2][0][0]=-29.4088; 
    p0_emc_m_sdz[0][2][1][0]=0.132208; p1_emc_m_sdz[0][2][1][0]=-0.423621; p2_emc_m_sdz[0][2][1][0]=0.420036; p3_emc_m_sdz[0][2][1][0]=-0.17299; p4_emc_m_sdz[0][2][1][0]=0.0257427; 
    p0_emc_s_sdz[0][2][1][0]=0.848147; p1_emc_s_sdz[0][2][1][0]=0.161235; p2_emc_s_sdz[0][2][1][0]=-0.138099; p3_emc_s_sdz[0][2][1][0]=0.0478763; p4_emc_s_sdz[0][2][1][0]=-0.00476997; 
    p0_emc_m_sdz[1][2][1][0]=-2.25265; p1_emc_m_sdz[1][2][1][0]=9.03578; p2_emc_m_sdz[1][2][1][0]=-2.95888; p3_emc_m_sdz[1][2][1][0]=-24.2566; p4_emc_m_sdz[1][2][1][0]=24.075; 
    p0_emc_s_sdz[1][2][1][0]=-3.89617; p1_emc_s_sdz[1][2][1][0]=18.612; p2_emc_s_sdz[1][2][1][0]=-4.70543; p3_emc_s_sdz[1][2][1][0]=-50.2237; p4_emc_s_sdz[1][2][1][0]=47.6106; 
    p0_emc_m_sdz[0][2][2][0]=0.122597; p1_emc_m_sdz[0][2][2][0]=-0.326281; p2_emc_m_sdz[0][2][2][0]=0.298637; p3_emc_m_sdz[0][2][2][0]=-0.112515; p4_emc_m_sdz[0][2][2][0]=0.0152004; 
    p0_emc_s_sdz[0][2][2][0]=0.700775; p1_emc_s_sdz[0][2][2][0]=0.425693; p2_emc_s_sdz[0][2][2][0]=-0.326306; p3_emc_s_sdz[0][2][2][0]=0.105914; p4_emc_s_sdz[0][2][2][0]=-0.0109842; 
    p0_emc_m_sdz[1][2][2][0]=1.91884; p1_emc_m_sdz[1][2][2][0]=-6.99464; p2_emc_m_sdz[1][2][2][0]=1.14713; p3_emc_m_sdz[1][2][2][0]=18.5801; p4_emc_m_sdz[1][2][2][0]=-16.559; 
    p0_emc_s_sdz[1][2][2][0]=5.10268; p1_emc_s_sdz[1][2][2][0]=-15.6955; p2_emc_s_sdz[1][2][2][0]=3.24469; p3_emc_s_sdz[1][2][2][0]=41.6367; p4_emc_s_sdz[1][2][2][0]=-38.2285; 
    p0_emc_m_sdz[0][2][3][0]=0.222578; p1_emc_m_sdz[0][2][3][0]=-0.575202; p2_emc_m_sdz[0][2][3][0]=0.522955; p3_emc_m_sdz[0][2][3][0]=-0.198807; p4_emc_m_sdz[0][2][3][0]=0.0271373; 
    p0_emc_s_sdz[0][2][3][0]=0.874404; p1_emc_s_sdz[0][2][3][0]=-0.00613041; p2_emc_s_sdz[0][2][3][0]=0.000153271; p3_emc_s_sdz[0][2][3][0]=0.00903154; p4_emc_s_sdz[0][2][3][0]=-0.00147097; 
    p0_emc_m_sdz[1][2][3][0]=0.166397; p1_emc_m_sdz[1][2][3][0]=-0.221728; p2_emc_m_sdz[1][2][3][0]=-0.548622; p3_emc_m_sdz[1][2][3][0]=-0.269764; p4_emc_m_sdz[1][2][3][0]=1.70711; 
    p0_emc_s_sdz[1][2][3][0]=0.78415; p1_emc_s_sdz[1][2][3][0]=0.377567; p2_emc_s_sdz[1][2][3][0]=-0.150928; p3_emc_s_sdz[1][2][3][0]=-0.466639; p4_emc_s_sdz[1][2][3][0]=0.138696; 
    p0_emc_m_sdz[0][2][4][0]=-0.0472537; p1_emc_m_sdz[0][2][4][0]=0.0163051; p2_emc_m_sdz[0][2][4][0]=-0.0107782; p3_emc_m_sdz[0][2][4][0]=0.0100192; p4_emc_m_sdz[0][2][4][0]=-0.00243274; 
    p0_emc_s_sdz[0][2][4][0]=0.776528; p1_emc_s_sdz[0][2][4][0]=0.223002; p2_emc_s_sdz[0][2][4][0]=-0.186802; p3_emc_s_sdz[0][2][4][0]=0.0668495; p4_emc_s_sdz[0][2][4][0]=-0.00729766; 
    p0_emc_m_sdz[1][2][4][0]=-3.1606; p1_emc_m_sdz[1][2][4][0]=12.8175; p2_emc_m_sdz[1][2][4][0]=-4.24663; p3_emc_m_sdz[1][2][4][0]=-35.6402; p4_emc_m_sdz[1][2][4][0]=35.8497; 
    p0_emc_s_sdz[1][2][4][0]=4.07181; p1_emc_s_sdz[1][2][4][0]=-12.8141; p2_emc_s_sdz[1][2][4][0]=5.24022; p3_emc_s_sdz[1][2][4][0]=31.147; p4_emc_s_sdz[1][2][4][0]=-31.7469; 
    p0_emc_m_sdz[0][2][5][0]=0.0851407; p1_emc_m_sdz[0][2][5][0]=0.0868611; p2_emc_m_sdz[0][2][5][0]=-0.173337; p3_emc_m_sdz[0][2][5][0]=0.0796508; p4_emc_m_sdz[0][2][5][0]=-0.0119176; 
    p0_emc_s_sdz[0][2][5][0]=0.878334; p1_emc_s_sdz[0][2][5][0]=-0.086549; p2_emc_s_sdz[0][2][5][0]=0.119836; p3_emc_s_sdz[0][2][5][0]=-0.0460626; p4_emc_s_sdz[0][2][5][0]=0.00638133; 
    p0_emc_m_sdz[1][2][5][0]=1.10686; p1_emc_m_sdz[1][2][5][0]=-4.00193; p2_emc_m_sdz[1][2][5][0]=1.55493; p3_emc_m_sdz[1][2][5][0]=9.36596; p4_emc_m_sdz[1][2][5][0]=-9.2764; 
    p0_emc_s_sdz[1][2][5][0]=-5.84637; p1_emc_s_sdz[1][2][5][0]=27.1354; p2_emc_s_sdz[1][2][5][0]=-9.88133; p3_emc_s_sdz[1][2][5][0]=-70.9125; p4_emc_s_sdz[1][2][5][0]=71.5178; 
    p0_emc_m_sdz[0][2][6][0]=-0.0245026; p1_emc_m_sdz[0][2][6][0]=0.327037; p2_emc_m_sdz[0][2][6][0]=-0.35798; p3_emc_m_sdz[0][2][6][0]=0.148225; p4_emc_m_sdz[0][2][6][0]=-0.0215493; 
    p0_emc_s_sdz[0][2][6][0]=0.897685; p1_emc_s_sdz[0][2][6][0]=-0.147507; p2_emc_s_sdz[0][2][6][0]=0.16474; p3_emc_s_sdz[0][2][6][0]=-0.0598008; p4_emc_s_sdz[0][2][6][0]=0.0083145; 
    p0_emc_m_sdz[1][2][6][0]=-3.6617; p1_emc_m_sdz[1][2][6][0]=13.9439; p2_emc_m_sdz[1][2][6][0]=-3.08029; p3_emc_m_sdz[1][2][6][0]=-35.8542; p4_emc_m_sdz[1][2][6][0]=32.6854; 
    p0_emc_s_sdz[1][2][6][0]=3.44191; p1_emc_s_sdz[1][2][6][0]=-10.3608; p2_emc_s_sdz[1][2][6][0]=3.19756; p3_emc_s_sdz[1][2][6][0]=27.252; p4_emc_s_sdz[1][2][6][0]=-26.2547; 
    p0_emc_m_sdz[0][2][7][0]=-0.0806717; p1_emc_m_sdz[0][2][7][0]=0.479704; p2_emc_m_sdz[0][2][7][0]=-0.506854; p3_emc_m_sdz[0][2][7][0]=0.20573; p4_emc_m_sdz[0][2][7][0]=-0.0291361; 
    p0_emc_s_sdz[0][2][7][0]=0.856476; p1_emc_s_sdz[0][2][7][0]=-0.0291119; p2_emc_s_sdz[0][2][7][0]=0.0446295; p3_emc_s_sdz[0][2][7][0]=-0.00940002; p4_emc_s_sdz[0][2][7][0]=0.000548968; 
    p0_emc_m_sdz[1][2][7][0]=-0.155532; p1_emc_m_sdz[1][2][7][0]=0.24136; p2_emc_m_sdz[1][2][7][0]=0.595314; p3_emc_m_sdz[1][2][7][0]=0.364182; p4_emc_m_sdz[1][2][7][0]=-1.53748; 
    p0_emc_s_sdz[1][2][7][0]=-2.5877; p1_emc_s_sdz[1][2][7][0]=15.0843; p2_emc_s_sdz[1][2][7][0]=-7.27086; p3_emc_s_sdz[1][2][7][0]=-38.0571; p4_emc_s_sdz[1][2][7][0]=40.146; 
    p0_emc_m_sdz[0][2][8][0]=-0.265997; p1_emc_m_sdz[0][2][8][0]=1.01099; p2_emc_m_sdz[0][2][8][0]=-1.00438; p3_emc_m_sdz[0][2][8][0]=0.401095; p4_emc_m_sdz[0][2][8][0]=-0.0566593; 
    p0_emc_s_sdz[0][2][8][0]=0.923705; p1_emc_s_sdz[0][2][8][0]=-0.21453; p2_emc_s_sdz[0][2][8][0]=0.240481; p3_emc_s_sdz[0][2][8][0]=-0.0942941; p4_emc_s_sdz[0][2][8][0]=0.0134277; 
    p0_emc_m_sdz[1][2][8][0]=-1.18341; p1_emc_m_sdz[1][2][8][0]=5.03012; p2_emc_m_sdz[1][2][8][0]=-1.1893; p3_emc_m_sdz[1][2][8][0]=-14.4223; p4_emc_m_sdz[1][2][8][0]=13.8858; 
    p0_emc_s_sdz[1][2][8][0]=0.995251; p1_emc_s_sdz[1][2][8][0]=0.0523695; p2_emc_s_sdz[1][2][8][0]=-0.949989; p3_emc_s_sdz[1][2][8][0]=-0.874496; p4_emc_s_sdz[1][2][8][0]=2.66762; 
    p0_emc_m_sdz[0][2][9][0]=-0.172087; p1_emc_m_sdz[0][2][9][0]=0.746113; p2_emc_m_sdz[0][2][9][0]=-0.726039; p3_emc_m_sdz[0][2][9][0]=0.279717; p4_emc_m_sdz[0][2][9][0]=-0.0382429; 
    p0_emc_s_sdz[0][2][9][0]=0.751379; p1_emc_s_sdz[0][2][9][0]=0.321548; p2_emc_s_sdz[0][2][9][0]=-0.260479; p3_emc_s_sdz[0][2][9][0]=0.0940238; p4_emc_s_sdz[0][2][9][0]=-0.0113639; 
    p0_emc_m_sdz[1][2][9][0]=-2.28951; p1_emc_m_sdz[1][2][9][0]=10.1064; p2_emc_m_sdz[1][2][9][0]=-3.51739; p3_emc_m_sdz[1][2][9][0]=-27.2594; p4_emc_m_sdz[1][2][9][0]=26.7727; 
    p0_emc_s_sdz[1][2][9][0]=0.771344; p1_emc_s_sdz[1][2][9][0]=0.291074; p2_emc_s_sdz[1][2][9][0]=-0.281113; p3_emc_s_sdz[1][2][9][0]=-0.44481; p4_emc_s_sdz[1][2][9][0]=0.901433; 

	
    //EMC DZ POS 27
	
    p0_emc_m_sdz[0][0][0][1]=0.0183976; p1_emc_m_sdz[0][0][0][1]=-0.0415214; p2_emc_m_sdz[0][0][0][1]=0.0510471; p3_emc_m_sdz[0][0][0][1]=-0.0331316; p4_emc_m_sdz[0][0][0][1]=0.00703337; 
    p0_emc_s_sdz[0][0][0][1]=0.877501; p1_emc_s_sdz[0][0][0][1]=0.124893; p2_emc_s_sdz[0][0][0][1]=-0.0260235; p3_emc_s_sdz[0][0][0][1]=-0.00610994; p4_emc_s_sdz[0][0][0][1]=0.000579315; 
    p0_emc_m_sdz[1][0][0][1]=2.81497; p1_emc_m_sdz[1][0][0][1]=-9.64168; p2_emc_m_sdz[1][0][0][1]=1.10414; p3_emc_m_sdz[1][0][0][1]=22.7476; p4_emc_m_sdz[1][0][0][1]=-18.1131; 
    p0_emc_s_sdz[1][0][0][1]=-1.83234; p1_emc_s_sdz[1][0][0][1]=12.6026; p2_emc_s_sdz[1][0][0][1]=-5.89604; p3_emc_s_sdz[1][0][0][1]=-34.2057; p4_emc_s_sdz[1][0][0][1]=36.2247; 
    p0_emc_m_sdz[0][0][1][1]=0.0837824; p1_emc_m_sdz[0][0][1][1]=-0.258629; p2_emc_m_sdz[0][0][1][1]=0.283751; p3_emc_m_sdz[0][0][1][1]=-0.130088; p4_emc_m_sdz[0][0][1][1]=0.0211785; 
    p0_emc_s_sdz[0][0][1][1]=0.526453; p1_emc_s_sdz[0][0][1][1]=1.01521; p2_emc_s_sdz[0][0][1][1]=-0.845555; p3_emc_s_sdz[0][0][1][1]=0.284476; p4_emc_s_sdz[0][0][1][1]=-0.032722; 
    p0_emc_m_sdz[1][0][1][1]=-1.20534; p1_emc_m_sdz[1][0][1][1]=5.81671; p2_emc_m_sdz[1][0][1][1]=-2.52307; p3_emc_m_sdz[1][0][1][1]=-18.8941; p4_emc_m_sdz[1][0][1][1]=20.7849; 
    p0_emc_s_sdz[1][0][1][1]=0.645117; p1_emc_s_sdz[1][0][1][1]=0.60968; p2_emc_s_sdz[1][0][1][1]=0.343263; p3_emc_s_sdz[1][0][1][1]=-0.232056; p4_emc_s_sdz[1][0][1][1]=-1.12599; 
    p0_emc_m_sdz[0][0][2][1]=0.269538; p1_emc_m_sdz[0][0][2][1]=-0.702124; p2_emc_m_sdz[0][0][2][1]=0.672109; p3_emc_m_sdz[0][0][2][1]=-0.267135; p4_emc_m_sdz[0][0][2][1]=0.0378282; 
    p0_emc_s_sdz[0][0][2][1]=0.875648; p1_emc_s_sdz[0][0][2][1]=0.143016; p2_emc_s_sdz[0][0][2][1]=-0.14566; p3_emc_s_sdz[0][0][2][1]=0.0570232; p4_emc_s_sdz[0][0][2][1]=-0.00592054; 
    p0_emc_m_sdz[1][0][2][1]=0.2852; p1_emc_m_sdz[1][0][2][1]=-0.332621; p2_emc_m_sdz[1][0][2][1]=-0.874095; p3_emc_m_sdz[1][0][2][1]=-0.490331; p4_emc_m_sdz[1][0][2][1]=2.57455; 
    p0_emc_s_sdz[1][0][2][1]=0.905668; p1_emc_s_sdz[1][0][2][1]=0.342465; p2_emc_s_sdz[1][0][2][1]=-0.367012; p3_emc_s_sdz[1][0][2][1]=-0.671255; p4_emc_s_sdz[1][0][2][1]=0.635339; 
    p0_emc_m_sdz[0][0][3][1]=0.0325371; p1_emc_m_sdz[0][0][3][1]=-0.0775303; p2_emc_m_sdz[0][0][3][1]=0.0866643; p3_emc_m_sdz[0][0][3][1]=-0.0430534; p4_emc_m_sdz[0][0][3][1]=0.00712879; 
    p0_emc_s_sdz[0][0][3][1]=0.749498; p1_emc_s_sdz[0][0][3][1]=0.35521; p2_emc_s_sdz[0][0][3][1]=-0.280975; p3_emc_s_sdz[0][0][3][1]=0.0962209; p4_emc_s_sdz[0][0][3][1]=-0.0113102; 
    p0_emc_m_sdz[1][0][3][1]=3.14607; p1_emc_m_sdz[1][0][3][1]=-12.5899; p2_emc_m_sdz[1][0][3][1]=3.29888; p3_emc_m_sdz[1][0][3][1]=36.4344; p4_emc_m_sdz[1][0][3][1]=-35.7586; 
    p0_emc_s_sdz[1][0][3][1]=0.886962; p1_emc_s_sdz[1][0][3][1]=0.2205; p2_emc_s_sdz[1][0][3][1]=-0.55168; p3_emc_s_sdz[1][0][3][1]=-0.645301; p4_emc_s_sdz[1][0][3][1]=1.51624; 
    p0_emc_m_sdz[0][0][4][1]=-0.0033973; p1_emc_m_sdz[0][0][4][1]=-0.0205221; p2_emc_m_sdz[0][0][4][1]=0.0150138; p3_emc_m_sdz[0][0][4][1]=-0.00150937; p4_emc_m_sdz[0][0][4][1]=-0.000251925; 
    p0_emc_s_sdz[0][0][4][1]=0.889454; p1_emc_s_sdz[0][0][4][1]=0.101615; p2_emc_s_sdz[0][0][4][1]=-0.134985; p3_emc_s_sdz[0][0][4][1]=0.058068; p4_emc_s_sdz[0][0][4][1]=-0.00696184; 
    p0_emc_m_sdz[1][0][4][1]=-1.65178; p1_emc_m_sdz[1][0][4][1]=6.54849; p2_emc_m_sdz[1][0][4][1]=-2.61116; p3_emc_m_sdz[1][0][4][1]=-15.979; p4_emc_m_sdz[1][0][4][1]=16.0803; 
    p0_emc_s_sdz[1][0][4][1]=-8.98602; p1_emc_s_sdz[1][0][4][1]=38.9336; p2_emc_s_sdz[1][0][4][1]=-10.6138; p3_emc_s_sdz[1][0][4][1]=-107.58; p4_emc_s_sdz[1][0][4][1]=104.547; 
    p0_emc_m_sdz[0][0][5][1]=-0.0253829; p1_emc_m_sdz[0][0][5][1]=0.0756588; p2_emc_m_sdz[0][0][5][1]=-0.0848593; p3_emc_m_sdz[0][0][5][1]=0.0348628; p4_emc_m_sdz[0][0][5][1]=-0.00504278; 
    p0_emc_s_sdz[0][0][5][1]=0.65868; p1_emc_s_sdz[0][0][5][1]=0.565879; p2_emc_s_sdz[0][0][5][1]=-0.452315; p3_emc_s_sdz[0][0][5][1]=0.152463; p4_emc_s_sdz[0][0][5][1]=-0.0177641; 
    p0_emc_m_sdz[1][0][5][1]=-0.269104; p1_emc_m_sdz[1][0][5][1]=0.348355; p2_emc_m_sdz[1][0][5][1]=0.852895; p3_emc_m_sdz[1][0][5][1]=0.43148; p4_emc_m_sdz[1][0][5][1]=-2.52972; 
    p0_emc_s_sdz[1][0][5][1]=4.78989; p1_emc_s_sdz[1][0][5][1]=-14.1191; p2_emc_s_sdz[1][0][5][1]=2.44647; p3_emc_s_sdz[1][0][5][1]=35.7578; p4_emc_s_sdz[1][0][5][1]=-31.1635; 
    p0_emc_m_sdz[0][0][6][1]=-0.174521; p1_emc_m_sdz[0][0][6][1]=0.497405; p2_emc_m_sdz[0][0][6][1]=-0.512577; p3_emc_m_sdz[0][0][6][1]=0.220198; p4_emc_m_sdz[0][0][6][1]=-0.0334039; 
    p0_emc_s_sdz[0][0][6][1]=0.841243; p1_emc_s_sdz[0][0][6][1]=0.145225; p2_emc_s_sdz[0][0][6][1]=-0.0974333; p3_emc_s_sdz[0][0][6][1]=0.0281994; p4_emc_s_sdz[0][0][6][1]=-0.00188329; 
    p0_emc_m_sdz[1][0][6][1]=-0.585686; p1_emc_m_sdz[1][0][6][1]=3.0616; p2_emc_m_sdz[1][0][6][1]=-1.67714; p3_emc_m_sdz[1][0][6][1]=-10.1152; p4_emc_m_sdz[1][0][6][1]=11.7695; 
    p0_emc_s_sdz[1][0][6][1]=4.52916; p1_emc_s_sdz[1][0][6][1]=-14.4557; p2_emc_s_sdz[1][0][6][1]=3.46652; p3_emc_s_sdz[1][0][6][1]=40.4746; p4_emc_s_sdz[1][0][6][1]=-38.0835; 
    p0_emc_m_sdz[0][0][7][1]=-0.0704177; p1_emc_m_sdz[0][0][7][1]=0.270581; p2_emc_m_sdz[0][0][7][1]=-0.367257; p3_emc_m_sdz[0][0][7][1]=0.185564; p4_emc_m_sdz[0][0][7][1]=-0.0309184; 
    p0_emc_s_sdz[0][0][7][1]=0.966157; p1_emc_s_sdz[0][0][7][1]=-0.15887; p2_emc_s_sdz[0][0][7][1]=0.147591; p3_emc_s_sdz[0][0][7][1]=-0.0523186; p4_emc_s_sdz[0][0][7][1]=0.00774409; 
    p0_emc_m_sdz[1][0][7][1]=-1.151; p1_emc_m_sdz[1][0][7][1]=4.9091; p2_emc_m_sdz[1][0][7][1]=-2.07791; p3_emc_m_sdz[1][0][7][1]=-13.6054; p4_emc_m_sdz[1][0][7][1]=14.4559; 
    p0_emc_s_sdz[1][0][7][1]=4.10483; p1_emc_s_sdz[1][0][7][1]=-13.6406; p2_emc_s_sdz[1][0][7][1]=5.307; p3_emc_s_sdz[1][0][7][1]=37.5493; p4_emc_s_sdz[1][0][7][1]=-38.4767; 
    p0_emc_m_sdz[0][0][8][1]=-0.195108; p1_emc_m_sdz[0][0][8][1]=0.512431; p2_emc_m_sdz[0][0][8][1]=-0.540177; p3_emc_m_sdz[0][0][8][1]=0.239197; p4_emc_m_sdz[0][0][8][1]=-0.0370832; 
    p0_emc_s_sdz[0][0][8][1]=1.35633; p1_emc_s_sdz[0][0][8][1]=-1.01732; p2_emc_s_sdz[0][0][8][1]=0.795005; p3_emc_s_sdz[0][0][8][1]=-0.255016; p4_emc_s_sdz[0][0][8][1]=0.0298528; 
    p0_emc_m_sdz[1][0][8][1]=1.10309; p1_emc_m_sdz[1][0][8][1]=-4.54468; p2_emc_m_sdz[1][0][8][1]=1.6977; p3_emc_m_sdz[1][0][8][1]=11.2087; p4_emc_m_sdz[1][0][8][1]=-11.0858; 
    p0_emc_s_sdz[1][0][8][1]=1.05853; p1_emc_s_sdz[1][0][8][1]=0.0505515; p2_emc_s_sdz[1][0][8][1]=-1.0305; p3_emc_s_sdz[1][0][8][1]=-0.925106; p4_emc_s_sdz[1][0][8][1]=2.96663; 
    p0_emc_m_sdz[0][0][9][1]=-0.217212; p1_emc_m_sdz[0][0][9][1]=0.536267; p2_emc_m_sdz[0][0][9][1]=-0.53304; p3_emc_m_sdz[0][0][9][1]=0.215102; p4_emc_m_sdz[0][0][9][1]=-0.0302753; 
    p0_emc_s_sdz[0][0][9][1]=0.990178; p1_emc_s_sdz[0][0][9][1]=-0.0925329; p2_emc_s_sdz[0][0][9][1]=-0.00108887; p3_emc_s_sdz[0][0][9][1]=0.0407854; p4_emc_s_sdz[0][0][9][1]=-0.00974393; 
    p0_emc_m_sdz[1][0][9][1]=-3.25901; p1_emc_m_sdz[1][0][9][1]=12.3874; p2_emc_m_sdz[1][0][9][1]=-3.2054; p3_emc_m_sdz[1][0][9][1]=-32.7802; p4_emc_m_sdz[1][0][9][1]=31.0765; 
    p0_emc_s_sdz[1][0][9][1]=1.69255; p1_emc_s_sdz[1][0][9][1]=-2.5071; p2_emc_s_sdz[1][0][9][1]=-0.485226; p3_emc_s_sdz[1][0][9][1]=8.30358; p4_emc_s_sdz[1][0][9][1]=-6.65673; 
    p0_emc_m_sdz[0][1][0][1]=-0.138977; p1_emc_m_sdz[0][1][0][1]=-0.0253322; p2_emc_m_sdz[0][1][0][1]=0.144011; p3_emc_m_sdz[0][1][0][1]=-0.070191; p4_emc_m_sdz[0][1][0][1]=0.0099247; 
    p0_emc_s_sdz[0][1][0][1]=0.743679; p1_emc_s_sdz[0][1][0][1]=0.314966; p2_emc_s_sdz[0][1][0][1]=-0.202336; p3_emc_s_sdz[0][1][0][1]=0.0753248; p4_emc_s_sdz[0][1][0][1]=-0.0112882; 
    p0_emc_m_sdz[1][1][0][1]=4.10139; p1_emc_m_sdz[1][1][0][1]=-15.9242; p2_emc_m_sdz[1][1][0][1]=3.33158; p3_emc_m_sdz[1][1][0][1]=42.1922; p4_emc_m_sdz[1][1][0][1]=-38.1583; 
    p0_emc_s_sdz[1][1][0][1]=6.0506; p1_emc_s_sdz[1][1][0][1]=-19.8964; p2_emc_s_sdz[1][1][0][1]=5.06824; p3_emc_s_sdz[1][1][0][1]=52.1731; p4_emc_s_sdz[1][1][0][1]=-48.7641; 
    p0_emc_m_sdz[0][1][1][1]=-0.043387; p1_emc_m_sdz[0][1][1][1]=-0.181283; p2_emc_m_sdz[0][1][1][1]=0.261259; p3_emc_m_sdz[0][1][1][1]=-0.11553; p4_emc_m_sdz[0][1][1][1]=0.0165015; 
    p0_emc_s_sdz[0][1][1][1]=1.0332; p1_emc_s_sdz[0][1][1][1]=-0.378876; p2_emc_s_sdz[0][1][1][1]=0.341875; p3_emc_s_sdz[0][1][1][1]=-0.107865; p4_emc_s_sdz[0][1][1][1]=0.0117044; 
    p0_emc_m_sdz[1][1][1][1]=0.15353; p1_emc_m_sdz[1][1][1][1]=-0.30587; p2_emc_m_sdz[1][1][1][1]=-0.668515; p3_emc_m_sdz[1][1][1][1]=-0.285636; p4_emc_m_sdz[1][1][1][1]=2.1579; 
    p0_emc_s_sdz[1][1][1][1]=-2.43912; p1_emc_s_sdz[1][1][1][1]=13.0104; p2_emc_s_sdz[1][1][1][1]=-3.47706; p3_emc_s_sdz[1][1][1][1]=-34.8659; p4_emc_s_sdz[1][1][1][1]=33.1811; 
    p0_emc_m_sdz[0][1][2][1]=-0.159636; p1_emc_m_sdz[0][1][2][1]=0.203895; p2_emc_m_sdz[0][1][2][1]=-0.140921; p3_emc_m_sdz[0][1][2][1]=0.0535131; p4_emc_m_sdz[0][1][2][1]=-0.008412; 
    p0_emc_s_sdz[0][1][2][1]=1.02902; p1_emc_s_sdz[0][1][2][1]=-0.472999; p2_emc_s_sdz[0][1][2][1]=0.468684; p3_emc_s_sdz[0][1][2][1]=-0.161105; p4_emc_s_sdz[0][1][2][1]=0.0187477; 
    p0_emc_m_sdz[1][1][2][1]=4.41148; p1_emc_m_sdz[1][1][2][1]=-17.1268; p2_emc_m_sdz[1][1][2][1]=4.21119; p3_emc_m_sdz[1][1][2][1]=46.2778; p4_emc_m_sdz[1][1][2][1]=-43.8402; 
    p0_emc_s_sdz[1][1][2][1]=-1.48877; p1_emc_s_sdz[1][1][2][1]=9.80708; p2_emc_s_sdz[1][1][2][1]=-3.46212; p3_emc_s_sdz[1][1][2][1]=-27.2278; p4_emc_s_sdz[1][1][2][1]=27.748; 
    p0_emc_m_sdz[0][1][3][1]=-0.152491; p1_emc_m_sdz[0][1][3][1]=0.122913; p2_emc_m_sdz[0][1][3][1]=-0.0743083; p3_emc_m_sdz[0][1][3][1]=0.0288505; p4_emc_m_sdz[0][1][3][1]=-0.0049159; 
    p0_emc_s_sdz[0][1][3][1]=0.831673; p1_emc_s_sdz[0][1][3][1]=0.0188864; p2_emc_s_sdz[0][1][3][1]=0.0503097; p3_emc_s_sdz[0][1][3][1]=-0.0150517; p4_emc_s_sdz[0][1][3][1]=0.00062528; 
    p0_emc_m_sdz[1][1][3][1]=-0.240609; p1_emc_m_sdz[1][1][3][1]=0.167626; p2_emc_m_sdz[1][1][3][1]=0.536322; p3_emc_m_sdz[1][1][3][1]=0.326912; p4_emc_m_sdz[1][1][3][1]=-1.53886; 
    p0_emc_s_sdz[1][1][3][1]=2.80076; p1_emc_s_sdz[1][1][3][1]=-8.10159; p2_emc_s_sdz[1][1][3][1]=2.73478; p3_emc_s_sdz[1][1][3][1]=21.9929; p4_emc_s_sdz[1][1][3][1]=-21.6717; 
    p0_emc_m_sdz[0][1][4][1]=-0.0541861; p1_emc_m_sdz[0][1][4][1]=0.0279028; p2_emc_m_sdz[0][1][4][1]=-0.00529822; p3_emc_m_sdz[0][1][4][1]=-0.00121687; p4_emc_m_sdz[0][1][4][1]=0.000402105; 
    p0_emc_s_sdz[0][1][4][1]=0.762355; p1_emc_s_sdz[0][1][4][1]=0.362426; p2_emc_s_sdz[0][1][4][1]=-0.272407; p3_emc_s_sdz[0][1][4][1]=0.0932468; p4_emc_s_sdz[0][1][4][1]=-0.0116273; 
    p0_emc_m_sdz[1][1][4][1]=2.30896; p1_emc_m_sdz[1][1][4][1]=-8.39333; p2_emc_m_sdz[1][1][4][1]=1.54796; p3_emc_m_sdz[1][1][4][1]=20.6674; p4_emc_m_sdz[1][1][4][1]=-18.0281; 
    p0_emc_s_sdz[1][1][4][1]=5.51875; p1_emc_s_sdz[1][1][4][1]=-17.8854; p2_emc_s_sdz[1][1][4][1]=3.38189; p3_emc_s_sdz[1][1][4][1]=51.5206; p4_emc_s_sdz[1][1][4][1]=-47.6693; 
    p0_emc_m_sdz[0][1][5][1]=0.154772; p1_emc_m_sdz[0][1][5][1]=-0.0956072; p2_emc_m_sdz[0][1][5][1]=0.00988325; p3_emc_m_sdz[0][1][5][1]=0.00464239; p4_emc_m_sdz[0][1][5][1]=-0.000933665; 
    p0_emc_s_sdz[0][1][5][1]=0.987255; p1_emc_s_sdz[0][1][5][1]=-0.251356; p2_emc_s_sdz[0][1][5][1]=0.206936; p3_emc_s_sdz[0][1][5][1]=-0.0516677; p4_emc_s_sdz[0][1][5][1]=0.00318487; 
    p0_emc_m_sdz[1][1][5][1]=-2.61167; p1_emc_m_sdz[1][1][5][1]=10.6149; p2_emc_m_sdz[1][1][5][1]=-3.00753; p3_emc_m_sdz[1][1][5][1]=-28.5961; p4_emc_m_sdz[1][1][5][1]=27.6229; 
    p0_emc_s_sdz[1][1][5][1]=2.33133; p1_emc_s_sdz[1][1][5][1]=-6.47978; p2_emc_s_sdz[1][1][5][1]=2.8416; p3_emc_s_sdz[1][1][5][1]=18.4939; p4_emc_s_sdz[1][1][5][1]=-19.8679; 
    p0_emc_m_sdz[0][1][6][1]=0.279157; p1_emc_m_sdz[0][1][6][1]=-0.576488; p2_emc_m_sdz[0][1][6][1]=0.48227; p3_emc_m_sdz[0][1][6][1]=-0.182846; p4_emc_m_sdz[0][1][6][1]=0.0258723; 
    p0_emc_s_sdz[0][1][6][1]=0.968072; p1_emc_s_sdz[0][1][6][1]=-0.200783; p2_emc_s_sdz[0][1][6][1]=0.219738; p3_emc_s_sdz[0][1][6][1]=-0.0758319; p4_emc_s_sdz[0][1][6][1]=0.00860865; 
    p0_emc_m_sdz[1][1][6][1]=0.0783327; p1_emc_m_sdz[1][1][6][1]=-0.0187489; p2_emc_m_sdz[1][1][6][1]=-0.119469; p3_emc_m_sdz[1][1][6][1]=-0.10321; p4_emc_m_sdz[1][1][6][1]=0.277131; 
    p0_emc_s_sdz[1][1][6][1]=-2.6012; p1_emc_s_sdz[1][1][6][1]=14.5218; p2_emc_s_sdz[1][1][6][1]=-5.77279; p3_emc_s_sdz[1][1][6][1]=-37.8982; p4_emc_s_sdz[1][1][6][1]=38.8474; 
    p0_emc_m_sdz[0][1][7][1]=0.28854; p1_emc_m_sdz[0][1][7][1]=-0.53073; p2_emc_m_sdz[0][1][7][1]=0.427446; p3_emc_m_sdz[0][1][7][1]=-0.161403; p4_emc_m_sdz[0][1][7][1]=0.0229715; 
    p0_emc_s_sdz[0][1][7][1]=0.921341; p1_emc_s_sdz[0][1][7][1]=-0.0920667; p2_emc_s_sdz[0][1][7][1]=0.119764; p3_emc_s_sdz[0][1][7][1]=-0.0391333; p4_emc_s_sdz[0][1][7][1]=0.00421952; 
    p0_emc_m_sdz[1][1][7][1]=0.44293; p1_emc_m_sdz[1][1][7][1]=-1.59789; p2_emc_m_sdz[1][1][7][1]=0.781682; p3_emc_m_sdz[1][1][7][1]=3.77668; p4_emc_m_sdz[1][1][7][1]=-4.0674; 
    p0_emc_s_sdz[1][1][7][1]=-2.61359; p1_emc_s_sdz[1][1][7][1]=14.037; p2_emc_s_sdz[1][1][7][1]=-4.8706; p3_emc_s_sdz[1][1][7][1]=-36.3073; p4_emc_s_sdz[1][1][7][1]=36.0245; 
    p0_emc_m_sdz[0][1][8][1]=0.196386; p1_emc_m_sdz[0][1][8][1]=-0.218248; p2_emc_m_sdz[0][1][8][1]=0.12906; p3_emc_m_sdz[0][1][8][1]=-0.0456004; p4_emc_m_sdz[0][1][8][1]=0.00706759; 
    p0_emc_s_sdz[0][1][8][1]=0.787586; p1_emc_s_sdz[0][1][8][1]=0.24125; p2_emc_s_sdz[0][1][8][1]=-0.182225; p3_emc_s_sdz[0][1][8][1]=0.0713655; p4_emc_s_sdz[0][1][8][1]=-0.0100127; 
    p0_emc_m_sdz[1][1][8][1]=0.298609; p1_emc_m_sdz[1][1][8][1]=-0.185482; p2_emc_m_sdz[1][1][8][1]=-0.639089; p3_emc_m_sdz[1][1][8][1]=-0.445662; p4_emc_m_sdz[1][1][8][1]=1.64092; 
    p0_emc_s_sdz[1][1][8][1]=2.89201; p1_emc_s_sdz[1][1][8][1]=-8.1548; p2_emc_s_sdz[1][1][8][1]=2.79647; p3_emc_s_sdz[1][1][8][1]=20.864; p4_emc_s_sdz[1][1][8][1]=-20.2422; 
    p0_emc_m_sdz[0][1][9][1]=0.144509; p1_emc_m_sdz[0][1][9][1]=-0.0612419; p2_emc_m_sdz[0][1][9][1]=-0.0312274; p3_emc_m_sdz[0][1][9][1]=0.0209641; p4_emc_m_sdz[0][1][9][1]=-0.00253089; 
    p0_emc_s_sdz[0][1][9][1]=0.622606; p1_emc_s_sdz[0][1][9][1]=0.647907; p2_emc_s_sdz[0][1][9][1]=-0.517891; p3_emc_s_sdz[0][1][9][1]=0.184067; p4_emc_s_sdz[0][1][9][1]=-0.0230481; 
    p0_emc_m_sdz[1][1][9][1]=-1.92247; p1_emc_m_sdz[1][1][9][1]=8.37314; p2_emc_m_sdz[1][1][9][1]=-2.19101; p3_emc_m_sdz[1][1][9][1]=-24.2384; p4_emc_m_sdz[1][1][9][1]=23.2591; 
    p0_emc_s_sdz[1][1][9][1]=1.98861; p1_emc_s_sdz[1][1][9][1]=-3.76913; p2_emc_s_sdz[1][1][9][1]=-0.319044; p3_emc_s_sdz[1][1][9][1]=10.8939; p4_emc_s_sdz[1][1][9][1]=-8.49525; 
    p0_emc_m_sdz[0][2][0][1]=0.0520089; p1_emc_m_sdz[0][2][0][1]=-0.251791; p2_emc_m_sdz[0][2][0][1]=0.284239; p3_emc_m_sdz[0][2][0][1]=-0.111616; p4_emc_m_sdz[0][2][0][1]=0.0141381; 
    p0_emc_s_sdz[0][2][0][1]=0.772534; p1_emc_s_sdz[0][2][0][1]=0.374616; p2_emc_s_sdz[0][2][0][1]=-0.320491; p3_emc_s_sdz[0][2][0][1]=0.125121; p4_emc_s_sdz[0][2][0][1]=-0.0166614; 
    p0_emc_m_sdz[1][2][0][1]=1.58142; p1_emc_m_sdz[1][2][0][1]=-6.46049; p2_emc_m_sdz[1][2][0][1]=1.8431; p3_emc_m_sdz[1][2][0][1]=17.3813; p4_emc_m_sdz[1][2][0][1]=-16.3915; 
    p0_emc_s_sdz[1][2][0][1]=0.983895; p1_emc_s_sdz[1][2][0][1]=0.110863; p2_emc_s_sdz[1][2][0][1]=-0.821784; p3_emc_s_sdz[1][2][0][1]=-0.767577; p4_emc_s_sdz[1][2][0][1]=2.42127; 
    p0_emc_m_sdz[0][2][1][1]=-0.0689748; p1_emc_m_sdz[0][2][1][1]=0.144194; p2_emc_m_sdz[0][2][1][1]=-0.139548; p3_emc_m_sdz[0][2][1][1]=0.0720603; p4_emc_m_sdz[0][2][1][1]=-0.0135356; 
    p0_emc_s_sdz[0][2][1][1]=1.06614; p1_emc_s_sdz[0][2][1][1]=-0.291289; p2_emc_s_sdz[0][2][1][1]=0.213094; p3_emc_s_sdz[0][2][1][1]=-0.0546568; p4_emc_s_sdz[0][2][1][1]=0.00464454; 
    p0_emc_m_sdz[1][2][1][1]=0.0832262; p1_emc_m_sdz[1][2][1][1]=-0.137026; p2_emc_m_sdz[1][2][1][1]=-0.309485; p3_emc_m_sdz[1][2][1][1]=-0.133109; p4_emc_m_sdz[1][2][1][1]=0.972585; 
    p0_emc_s_sdz[1][2][1][1]=-0.116144; p1_emc_s_sdz[1][2][1][1]=4.247; p2_emc_s_sdz[1][2][1][1]=-1.67843; p3_emc_s_sdz[1][2][1][1]=-11.9855; p4_emc_s_sdz[1][2][1][1]=12.7791; 
    p0_emc_m_sdz[0][2][2][1]=-0.0768093; p1_emc_m_sdz[0][2][2][1]=0.189355; p2_emc_m_sdz[0][2][2][1]=-0.171486; p3_emc_m_sdz[0][2][2][1]=0.0767282; p4_emc_m_sdz[0][2][2][1]=-0.0129597; 
    p0_emc_s_sdz[0][2][2][1]=0.784667; p1_emc_s_sdz[0][2][2][1]=0.304183; p2_emc_s_sdz[0][2][2][1]=-0.228522; p3_emc_s_sdz[0][2][2][1]=0.0791072; p4_emc_s_sdz[0][2][2][1]=-0.0092684; 
    p0_emc_m_sdz[1][2][2][1]=-3.61079; p1_emc_m_sdz[1][2][2][1]=14.6766; p2_emc_m_sdz[1][2][2][1]=-5.099; p3_emc_m_sdz[1][2][2][1]=-38.886; p4_emc_m_sdz[1][2][2][1]=38.8546; 
    p0_emc_s_sdz[1][2][2][1]=-4.69263; p1_emc_s_sdz[1][2][2][1]=22.1984; p2_emc_s_sdz[1][2][2][1]=-6.08574; p3_emc_s_sdz[1][2][2][1]=-61.6181; p4_emc_s_sdz[1][2][2][1]=59.7776; 
    p0_emc_m_sdz[0][2][3][1]=-0.118591; p1_emc_m_sdz[0][2][3][1]=0.285031; p2_emc_m_sdz[0][2][3][1]=-0.239145; p3_emc_m_sdz[0][2][3][1]=0.0895978; p4_emc_m_sdz[0][2][3][1]=-0.0125618; 
    p0_emc_s_sdz[0][2][3][1]=0.760125; p1_emc_s_sdz[0][2][3][1]=0.249775; p2_emc_s_sdz[0][2][3][1]=-0.149339; p3_emc_s_sdz[0][2][3][1]=0.0512786; p4_emc_s_sdz[0][2][3][1]=-0.00677554; 
    p0_emc_m_sdz[1][2][3][1]=-2.20414; p1_emc_m_sdz[1][2][3][1]=9.07948; p2_emc_m_sdz[1][2][3][1]=-3.08136; p3_emc_m_sdz[1][2][3][1]=-24.9707; p4_emc_m_sdz[1][2][3][1]=25.0823; 
    p0_emc_s_sdz[1][2][3][1]=3.38282; p1_emc_s_sdz[1][2][3][1]=-9.99673; p2_emc_s_sdz[1][2][3][1]=1.53019; p3_emc_s_sdz[1][2][3][1]=30.7218; p4_emc_s_sdz[1][2][3][1]=-28.1745; 
    p0_emc_m_sdz[0][2][4][1]=-0.176686; p1_emc_m_sdz[0][2][4][1]=0.289326; p2_emc_m_sdz[0][2][4][1]=-0.213487; p3_emc_m_sdz[0][2][4][1]=0.0776533; p4_emc_m_sdz[0][2][4][1]=-0.0109154; 
    p0_emc_s_sdz[0][2][4][1]=0.855998; p1_emc_s_sdz[0][2][4][1]=0.0542872; p2_emc_s_sdz[0][2][4][1]=0.0072062; p3_emc_s_sdz[0][2][4][1]=-0.00828549; p4_emc_s_sdz[0][2][4][1]=0.00171263; 
    p0_emc_m_sdz[1][2][4][1]=-0.0785723; p1_emc_m_sdz[1][2][4][1]=0.00228236; p2_emc_m_sdz[1][2][4][1]=0.0971648; p3_emc_m_sdz[1][2][4][1]=0.106124; p4_emc_m_sdz[1][2][4][1]=-0.196011; 
    p0_emc_s_sdz[1][2][4][1]=-0.497499; p1_emc_s_sdz[1][2][4][1]=5.45217; p2_emc_s_sdz[1][2][4][1]=-1.74077; p3_emc_s_sdz[1][2][4][1]=-14.498; p4_emc_s_sdz[1][2][4][1]=14.3305; 
    p0_emc_m_sdz[0][2][5][1]=0.214458; p1_emc_m_sdz[0][2][5][1]=-0.316312; p2_emc_m_sdz[0][2][5][1]=0.192722; p3_emc_m_sdz[0][2][5][1]=-0.0593454; p4_emc_m_sdz[0][2][5][1]=0.007398; 
    p0_emc_s_sdz[0][2][5][1]=0.94691; p1_emc_s_sdz[0][2][5][1]=-0.177664; p2_emc_s_sdz[0][2][5][1]=0.180727; p3_emc_s_sdz[0][2][5][1]=-0.0563303; p4_emc_s_sdz[0][2][5][1]=0.00605638; 
    p0_emc_m_sdz[1][2][5][1]=3.5139; p1_emc_m_sdz[1][2][5][1]=-12.9192; p2_emc_m_sdz[1][2][5][1]=3.701; p3_emc_m_sdz[1][2][5][1]=31.6898; p4_emc_m_sdz[1][2][5][1]=-29.8591; 
    p0_emc_s_sdz[1][2][5][1]=0.986358; p1_emc_s_sdz[1][2][5][1]=0.0806045; p2_emc_s_sdz[1][2][5][1]=-0.924418; p3_emc_s_sdz[1][2][5][1]=-0.891709; p4_emc_s_sdz[1][2][5][1]=2.78113; 
    p0_emc_m_sdz[0][2][6][1]=0.246727; p1_emc_m_sdz[0][2][6][1]=-0.344533; p2_emc_m_sdz[0][2][6][1]=0.219016; p3_emc_m_sdz[0][2][6][1]=-0.062378; p4_emc_m_sdz[0][2][6][1]=0.00656395; 
    p0_emc_s_sdz[0][2][6][1]=0.759251; p1_emc_s_sdz[0][2][6][1]=0.148353; p2_emc_s_sdz[0][2][6][1]=-0.0452397; p3_emc_s_sdz[0][2][6][1]=0.0164685; p4_emc_s_sdz[0][2][6][1]=-0.00297517; 
    p0_emc_m_sdz[1][2][6][1]=-2.18126; p1_emc_m_sdz[1][2][6][1]=9.15437; p2_emc_m_sdz[1][2][6][1]=-2.58979; p3_emc_m_sdz[1][2][6][1]=-25.6012; p4_emc_m_sdz[1][2][6][1]=24.9017; 
    p0_emc_s_sdz[1][2][6][1]=1.72313; p1_emc_s_sdz[1][2][6][1]=-2.88627; p2_emc_s_sdz[1][2][6][1]=-0.114182; p3_emc_s_sdz[1][2][6][1]=7.47361; p4_emc_s_sdz[1][2][6][1]=-5.56597; 
    p0_emc_m_sdz[0][2][7][1]=0.12103; p1_emc_m_sdz[0][2][7][1]=-0.0592692; p2_emc_m_sdz[0][2][7][1]=0.000119556; p3_emc_m_sdz[0][2][7][1]=0.00249769; p4_emc_m_sdz[0][2][7][1]=0.000326077; 
    p0_emc_s_sdz[0][2][7][1]=0.890361; p1_emc_s_sdz[0][2][7][1]=-0.0791946; p2_emc_s_sdz[0][2][7][1]=0.0854149; p3_emc_s_sdz[0][2][7][1]=-0.0123657; p4_emc_s_sdz[0][2][7][1]=-0.000839558; 
    p0_emc_m_sdz[1][2][7][1]=0.656855; p1_emc_m_sdz[1][2][7][1]=-2.47423; p2_emc_m_sdz[1][2][7][1]=0.99358; p3_emc_m_sdz[1][2][7][1]=7.0091; p4_emc_m_sdz[1][2][7][1]=-7.40394; 
    p0_emc_s_sdz[1][2][7][1]=2.90816; p1_emc_s_sdz[1][2][7][1]=-7.80581; p2_emc_s_sdz[1][2][7][1]=1.81262; p3_emc_s_sdz[1][2][7][1]=20.5703; p4_emc_s_sdz[1][2][7][1]=-19.028; 
    p0_emc_m_sdz[0][2][8][1]=0.213963; p1_emc_m_sdz[0][2][8][1]=-0.21145; p2_emc_m_sdz[0][2][8][1]=0.077757; p3_emc_m_sdz[0][2][8][1]=-0.0117639; p4_emc_m_sdz[0][2][8][1]=0.000985816; 
    p0_emc_s_sdz[0][2][8][1]=0.89007; p1_emc_s_sdz[0][2][8][1]=-0.0156982; p2_emc_s_sdz[0][2][8][1]=0.00386806; p3_emc_s_sdz[0][2][8][1]=0.0255142; p4_emc_s_sdz[0][2][8][1]=-0.00694366; 
    p0_emc_m_sdz[1][2][8][1]=-7.40118; p1_emc_m_sdz[1][2][8][1]=29.6786; p2_emc_m_sdz[1][2][8][1]=-8.4194; p3_emc_m_sdz[1][2][8][1]=-79.6131; p4_emc_m_sdz[1][2][8][1]=76.1426; 
    p0_emc_s_sdz[1][2][8][1]=5.35034; p1_emc_s_sdz[1][2][8][1]=-18.5516; p2_emc_s_sdz[1][2][8][1]=6.50442; p3_emc_s_sdz[1][2][8][1]=49.903; p4_emc_s_sdz[1][2][8][1]=-49.7137; 
    p0_emc_m_sdz[0][2][9][1]=0.150254; p1_emc_m_sdz[0][2][9][1]=-0.0147349; p2_emc_m_sdz[0][2][9][1]=-0.109377; p3_emc_m_sdz[0][2][9][1]=0.0628467; p4_emc_m_sdz[0][2][9][1]=-0.00998031; 
    p0_emc_s_sdz[0][2][9][1]=0.871388; p1_emc_s_sdz[0][2][9][1]=0.0890504; p2_emc_s_sdz[0][2][9][1]=-0.0936912; p3_emc_s_sdz[0][2][9][1]=0.0608316; p4_emc_s_sdz[0][2][9][1]=-0.0111377; 
    p0_emc_m_sdz[1][2][9][1]=-1.4976; p1_emc_m_sdz[1][2][9][1]=5.86883; p2_emc_m_sdz[1][2][9][1]=-0.856292; p3_emc_m_sdz[1][2][9][1]=-14.4747; p4_emc_m_sdz[1][2][9][1]=11.6682; 
    p0_emc_s_sdz[1][2][9][1]=-0.207264; p1_emc_s_sdz[1][2][9][1]=3.99397; p2_emc_s_sdz[1][2][9][1]=-0.947321; p3_emc_s_sdz[1][2][9][1]=-9.5421; p4_emc_s_sdz[1][2][9][1]=8.46972; 



    //PC3 DPHI NEG 27
 
	
    p0_pc3_m_sdp[0][0][0][0]=1.50724; p1_pc3_m_sdp[0][0][0][0]=-3.10687; p2_pc3_m_sdp[0][0][0][0]=2.27255; p3_pc3_m_sdp[0][0][0][0]=-0.684225; p4_pc3_m_sdp[0][0][0][0]=0.0704865; 
    p0_pc3_s_sdp[0][0][0][0]=3.19365; p1_pc3_s_sdp[0][0][0][0]=-2.90682; p2_pc3_s_sdp[0][0][0][0]=0.775875; p3_pc3_s_sdp[0][0][0][0]=0.134008; p4_pc3_s_sdp[0][0][0][0]=-0.0478104; 
    p0_pc3_m_sdp[1][0][0][0]=-0.754865; p1_pc3_m_sdp[1][0][0][0]=1.49351; p2_pc3_m_sdp[1][0][0][0]=0.672389; p3_pc3_m_sdp[1][0][0][0]=-3.53677; p4_pc3_m_sdp[1][0][0][0]=2.44153; 
    p0_pc3_s_sdp[1][0][0][0]=3.55896; p1_pc3_s_sdp[1][0][0][0]=-12.6879; p2_pc3_s_sdp[1][0][0][0]=0.543465; p3_pc3_s_sdp[1][0][0][0]=40.4783; p4_pc3_s_sdp[1][0][0][0]=-32.7104; 
    p0_pc3_m_sdp[0][0][1][0]=0.24994; p1_pc3_m_sdp[0][0][1][0]=-0.600612; p2_pc3_m_sdp[0][0][1][0]=0.507235; p3_pc3_m_sdp[0][0][1][0]=-0.175972; p4_pc3_m_sdp[0][0][1][0]=0.0218857; 
    p0_pc3_s_sdp[0][0][1][0]=0.0707519; p1_pc3_s_sdp[0][0][1][0]=1.45047; p2_pc3_s_sdp[0][0][1][0]=-1.06742; p3_pc3_s_sdp[0][0][1][0]=0.320555; p4_pc3_s_sdp[0][0][1][0]=-0.0267308; 
    p0_pc3_m_sdp[1][0][1][0]=0.0400656; p1_pc3_m_sdp[1][0][1][0]=-0.0697289; p2_pc3_m_sdp[1][0][1][0]=-0.151664; p3_pc3_m_sdp[1][0][1][0]=-0.0456158; p4_pc3_m_sdp[1][0][1][0]=0.561213; 
    p0_pc3_s_sdp[1][0][1][0]=2.0978; p1_pc3_s_sdp[1][0][1][0]=-6.0378; p2_pc3_s_sdp[1][0][1][0]=1.32516; p3_pc3_s_sdp[1][0][1][0]=17.6264; p4_pc3_s_sdp[1][0][1][0]=-16.4903; 
    p0_pc3_m_sdp[0][0][2][0]=-0.131793; p1_pc3_m_sdp[0][0][2][0]=0.397148; p2_pc3_m_sdp[0][0][2][0]=-0.378754; p3_pc3_m_sdp[0][0][2][0]=0.137149; p4_pc3_m_sdp[0][0][2][0]=-0.0160479; 
    p0_pc3_s_sdp[0][0][2][0]=0.313028; p1_pc3_s_sdp[0][0][2][0]=0.544296; p2_pc3_s_sdp[0][0][2][0]=-0.0607743; p3_pc3_s_sdp[0][0][2][0]=-0.108784; p4_pc3_s_sdp[0][0][2][0]=0.0329065; 
    p0_pc3_m_sdp[1][0][2][0]=2.05773; p1_pc3_m_sdp[1][0][2][0]=-6.20652; p2_pc3_m_sdp[1][0][2][0]=0.00371946; p3_pc3_m_sdp[1][0][2][0]=15.3223; p4_pc3_m_sdp[1][0][2][0]=-12.1311; 
    p0_pc3_s_sdp[1][0][2][0]=-3.88464; p1_pc3_s_sdp[1][0][2][0]=18.2288; p2_pc3_s_sdp[1][0][2][0]=-6.18018; p3_pc3_s_sdp[1][0][2][0]=-48.3464; p4_pc3_s_sdp[1][0][2][0]=48.0579; 
    p0_pc3_m_sdp[0][0][3][0]=0.0382535; p1_pc3_m_sdp[0][0][3][0]=-0.0779565; p2_pc3_m_sdp[0][0][3][0]=0.0259755; p3_pc3_m_sdp[0][0][3][0]=0.00407029; p4_pc3_m_sdp[0][0][3][0]=-0.00164471; 
    p0_pc3_s_sdp[0][0][3][0]=-0.567604; p1_pc3_s_sdp[0][0][3][0]=2.75162; p2_pc3_s_sdp[0][0][3][0]=-1.97402; p3_pc3_s_sdp[0][0][3][0]=0.613129; p4_pc3_s_sdp[0][0][3][0]=-0.069073; 
    p0_pc3_m_sdp[1][0][3][0]=0.192293; p1_pc3_m_sdp[1][0][3][0]=-0.244636; p2_pc3_m_sdp[1][0][3][0]=-0.60332; p3_pc3_m_sdp[1][0][3][0]=-0.339948; p4_pc3_m_sdp[1][0][3][0]=1.66903; 
    p0_pc3_s_sdp[1][0][3][0]=1.35204; p1_pc3_s_sdp[1][0][3][0]=-0.470769; p2_pc3_s_sdp[1][0][3][0]=-2.16365; p3_pc3_s_sdp[1][0][3][0]=-1.65717; p4_pc3_s_sdp[1][0][3][0]=5.34859; 
    p0_pc3_m_sdp[0][0][4][0]=0.115517; p1_pc3_m_sdp[0][0][4][0]=-0.296885; p2_pc3_m_sdp[0][0][4][0]=0.246215; p3_pc3_m_sdp[0][0][4][0]=-0.0858946; p4_pc3_m_sdp[0][0][4][0]=0.010946; 
    p0_pc3_s_sdp[0][0][4][0]=0.402088; p1_pc3_s_sdp[0][0][4][0]=0.491201; p2_pc3_s_sdp[0][0][4][0]=-0.19733; p3_pc3_s_sdp[0][0][4][0]=0.0251773; p4_pc3_s_sdp[0][0][4][0]=0.0011804; 
    p0_pc3_m_sdp[1][0][4][0]=-2.04002; p1_pc3_m_sdp[1][0][4][0]=8.33566; p2_pc3_m_sdp[1][0][4][0]=-2.20389; p3_pc3_m_sdp[1][0][4][0]=-24.1768; p4_pc3_m_sdp[1][0][4][0]=23.7763; 
    p0_pc3_s_sdp[1][0][4][0]=3.17226; p1_pc3_s_sdp[1][0][4][0]=-10.2289; p2_pc3_s_sdp[1][0][4][0]=2.80673; p3_pc3_s_sdp[1][0][4][0]=28.1695; p4_pc3_s_sdp[1][0][4][0]=-26.8731; 
    p0_pc3_m_sdp[0][0][5][0]=0.120022; p1_pc3_m_sdp[0][0][5][0]=-0.320725; p2_pc3_m_sdp[0][0][5][0]=0.277779; p3_pc3_m_sdp[0][0][5][0]=-0.104643; p4_pc3_m_sdp[0][0][5][0]=0.0145195; 
    p0_pc3_s_sdp[0][0][5][0]=0.402251; p1_pc3_s_sdp[0][0][5][0]=0.534036; p2_pc3_s_sdp[0][0][5][0]=-0.239497; p3_pc3_s_sdp[0][0][5][0]=0.0382461; p4_pc3_s_sdp[0][0][5][0]=-0.000337998; 
    p0_pc3_m_sdp[1][0][5][0]=0.977205; p1_pc3_m_sdp[1][0][5][0]=-3.72303; p2_pc3_m_sdp[1][0][5][0]=0.948901; p3_pc3_m_sdp[1][0][5][0]=10.317; p4_pc3_m_sdp[1][0][5][0]=-10.0642; 
    p0_pc3_s_sdp[1][0][5][0]=1.78501; p1_pc3_s_sdp[1][0][5][0]=-4.59302; p2_pc3_s_sdp[1][0][5][0]=1.15846; p3_pc3_s_sdp[1][0][5][0]=13.5517; p4_pc3_s_sdp[1][0][5][0]=-13.1533; 
    p0_pc3_m_sdp[0][0][6][0]=0.260621; p1_pc3_m_sdp[0][0][6][0]=-0.649347; p2_pc3_m_sdp[0][0][6][0]=0.544684; p3_pc3_m_sdp[0][0][6][0]=-0.194115; p4_pc3_m_sdp[0][0][6][0]=0.0252981; 
    p0_pc3_s_sdp[0][0][6][0]=0.319767; p1_pc3_s_sdp[0][0][6][0]=0.722722; p2_pc3_s_sdp[0][0][6][0]=-0.394234; p3_pc3_s_sdp[0][0][6][0]=0.0900158; p4_pc3_s_sdp[0][0][6][0]=-0.00592972; 
    p0_pc3_m_sdp[1][0][6][0]=0.932549; p1_pc3_m_sdp[1][0][6][0]=-2.46555; p2_pc3_m_sdp[1][0][6][0]=-0.682616; p3_pc3_m_sdp[1][0][6][0]=5.44575; p4_pc3_m_sdp[1][0][6][0]=-2.91805; 
    p0_pc3_s_sdp[1][0][6][0]=-0.640901; p1_pc3_s_sdp[1][0][6][0]=5.74136; p2_pc3_s_sdp[1][0][6][0]=-2.67929; p3_pc3_s_sdp[1][0][6][0]=-15.6509; p4_pc3_s_sdp[1][0][6][0]=16.8505; 
    p0_pc3_m_sdp[0][0][7][0]=0.221983; p1_pc3_m_sdp[0][0][7][0]=-0.571623; p2_pc3_m_sdp[0][0][7][0]=0.493203; p3_pc3_m_sdp[0][0][7][0]=-0.176359; p4_pc3_m_sdp[0][0][7][0]=0.0226699; 
    p0_pc3_s_sdp[0][0][7][0]=0.0847294; p1_pc3_s_sdp[0][0][7][0]=1.44453; p2_pc3_s_sdp[0][0][7][0]=-1.11722; p3_pc3_s_sdp[0][0][7][0]=0.372629; p4_pc3_s_sdp[0][0][7][0]=-0.0413892; 
    p0_pc3_m_sdp[1][0][7][0]=0.823289; p1_pc3_m_sdp[1][0][7][0]=-3.66247; p2_pc3_m_sdp[1][0][7][0]=1.65659; p3_pc3_m_sdp[1][0][7][0]=9.45154; p4_pc3_m_sdp[1][0][7][0]=-9.8135; 
    p0_pc3_s_sdp[1][0][7][0]=0.629811; p1_pc3_s_sdp[1][0][7][0]=0.0644345; p2_pc3_s_sdp[1][0][7][0]=-0.519025; p3_pc3_s_sdp[1][0][7][0]=-0.435077; p4_pc3_s_sdp[1][0][7][0]=1.74063; 
    p0_pc3_m_sdp[0][0][8][0]=0.136664; p1_pc3_m_sdp[0][0][8][0]=-0.358476; p2_pc3_m_sdp[0][0][8][0]=0.330133; p3_pc3_m_sdp[0][0][8][0]=-0.124295; p4_pc3_m_sdp[0][0][8][0]=0.0169753; 
    p0_pc3_s_sdp[0][0][8][0]=-0.174131; p1_pc3_s_sdp[0][0][8][0]=2.20216; p2_pc3_s_sdp[0][0][8][0]=-1.85997; p3_pc3_s_sdp[0][0][8][0]=0.666048; p4_pc3_s_sdp[0][0][8][0]=-0.0802552; 
    p0_pc3_m_sdp[1][0][8][0]=-0.862351; p1_pc3_m_sdp[1][0][8][0]=2.20621; p2_pc3_m_sdp[1][0][8][0]=0.0618303; p3_pc3_m_sdp[1][0][8][0]=-4.5269; p4_pc3_m_sdp[1][0][8][0]=3.48435; 
    p0_pc3_s_sdp[1][0][8][0]=-1.14246; p1_pc3_s_sdp[1][0][8][0]=7.96257; p2_pc3_s_sdp[1][0][8][0]=-3.52983; p3_pc3_s_sdp[1][0][8][0]=-21.315; p4_pc3_s_sdp[1][0][8][0]=22.3604; 
    p0_pc3_m_sdp[0][0][9][0]=0.266982; p1_pc3_m_sdp[0][0][9][0]=-0.756984; p2_pc3_m_sdp[0][0][9][0]=0.748118; p3_pc3_m_sdp[0][0][9][0]=-0.294697; p4_pc3_m_sdp[0][0][9][0]=0.0403591; 
    p0_pc3_s_sdp[0][0][9][0]=-0.262996; p1_pc3_s_sdp[0][0][9][0]=2.4747; p2_pc3_s_sdp[0][0][9][0]=-2.12289; p3_pc3_s_sdp[0][0][9][0]=0.772114; p4_pc3_s_sdp[0][0][9][0]=-0.0957767; 
    p0_pc3_m_sdp[1][0][9][0]=-1.9263; p1_pc3_m_sdp[1][0][9][0]=5.36823; p2_pc3_m_sdp[1][0][9][0]=-0.382565; p3_pc3_m_sdp[1][0][9][0]=-10.7933; p4_pc3_m_sdp[1][0][9][0]=8.4489; 
    p0_pc3_s_sdp[1][0][9][0]=3.99794; p1_pc3_s_sdp[1][0][9][0]=-11.6892; p2_pc3_s_sdp[1][0][9][0]=1.79131; p3_pc3_s_sdp[1][0][9][0]=29.4567; p4_pc3_s_sdp[1][0][9][0]=-25.494; 
    p0_pc3_m_sdp[0][1][0][0]=0.178346; p1_pc3_m_sdp[0][1][0][0]=-0.312164; p2_pc3_m_sdp[0][1][0][0]=0.212274; p3_pc3_m_sdp[0][1][0][0]=-0.0625359; p4_pc3_m_sdp[0][1][0][0]=0.00708783; 
    p0_pc3_s_sdp[0][1][0][0]=-0.489677; p1_pc3_s_sdp[0][1][0][0]=3.14147; p2_pc3_s_sdp[0][1][0][0]=-2.65557; p3_pc3_s_sdp[0][1][0][0]=0.937673; p4_pc3_s_sdp[0][1][0][0]=-0.113772; 
    p0_pc3_m_sdp[1][1][0][0]=0.220819; p1_pc3_m_sdp[1][1][0][0]=-0.0909549; p2_pc3_m_sdp[1][1][0][0]=-0.383865; p3_pc3_m_sdp[1][1][0][0]=-0.293693; p4_pc3_m_sdp[1][1][0][0]=0.917885; 
    p0_pc3_s_sdp[1][1][0][0]=0.764073; p1_pc3_s_sdp[1][1][0][0]=0.321642; p2_pc3_s_sdp[1][1][0][0]=-0.252667; p3_pc3_s_sdp[1][1][0][0]=-0.613349; p4_pc3_s_sdp[1][1][0][0]=-0.00955972; 
    p0_pc3_m_sdp[0][1][1][0]=0.163939; p1_pc3_m_sdp[0][1][1][0]=-0.286958; p2_pc3_m_sdp[0][1][1][0]=0.172793; p3_pc3_m_sdp[0][1][1][0]=-0.0411873; p4_pc3_m_sdp[0][1][1][0]=0.00356811; 
    p0_pc3_s_sdp[0][1][1][0]=-0.475207; p1_pc3_s_sdp[0][1][1][0]=3.03423; p2_pc3_s_sdp[0][1][1][0]=-2.55677; p3_pc3_s_sdp[0][1][1][0]=0.905587; p4_pc3_s_sdp[0][1][1][0]=-0.109773; 
    p0_pc3_m_sdp[1][1][1][0]=5.65215; p1_pc3_m_sdp[1][1][1][0]=-21.4866; p2_pc3_m_sdp[1][1][1][0]=6.33928; p3_pc3_m_sdp[1][1][1][0]=56.2243; p4_pc3_m_sdp[1][1][1][0]=-54.8734; 
    p0_pc3_s_sdp[1][1][1][0]=0.484412; p1_pc3_s_sdp[1][1][1][0]=3.63081; p2_pc3_s_sdp[1][1][1][0]=-4.30712; p3_pc3_s_sdp[1][1][1][0]=-12.3361; p4_pc3_s_sdp[1][1][1][0]=17.3948; 
    p0_pc3_m_sdp[0][1][2][0]=0.0876597; p1_pc3_m_sdp[0][1][2][0]=-0.121197; p2_pc3_m_sdp[0][1][2][0]=0.0317867; p3_pc3_m_sdp[0][1][2][0]=0.00957317; p4_pc3_m_sdp[0][1][2][0]=-0.00313329; 
    p0_pc3_s_sdp[0][1][2][0]=-0.275635; p1_pc3_s_sdp[0][1][2][0]=2.56792; p2_pc3_s_sdp[0][1][2][0]=-2.1494; p3_pc3_s_sdp[0][1][2][0]=0.750697; p4_pc3_s_sdp[0][1][2][0]=-0.0890823; 
    p0_pc3_m_sdp[1][1][2][0]=1.9551; p1_pc3_m_sdp[1][1][2][0]=-7.31456; p2_pc3_m_sdp[1][1][2][0]=1.93008; p3_pc3_m_sdp[1][1][2][0]=19.448; p4_pc3_m_sdp[1][1][2][0]=-18.6593; 
    p0_pc3_s_sdp[1][1][2][0]=-5.17013; p1_pc3_s_sdp[1][1][2][0]=23.6872; p2_pc3_s_sdp[1][1][2][0]=-7.12653; p3_pc3_s_sdp[1][1][2][0]=-65.5768; p4_pc3_s_sdp[1][1][2][0]=64.6079; 
    p0_pc3_m_sdp[0][1][3][0]=0.129632; p1_pc3_m_sdp[0][1][3][0]=-0.290335; p2_pc3_m_sdp[0][1][3][0]=0.242255; p3_pc3_m_sdp[0][1][3][0]=-0.0933541; p4_pc3_m_sdp[0][1][3][0]=0.0135765; 
    p0_pc3_s_sdp[0][1][3][0]=0.372213; p1_pc3_s_sdp[0][1][3][0]=0.81142; p2_pc3_s_sdp[0][1][3][0]=-0.534994; p3_pc3_s_sdp[0][1][3][0]=0.146745; p4_pc3_s_sdp[0][1][3][0]=-0.0124458; 
    p0_pc3_m_sdp[1][1][3][0]=2.124; p1_pc3_m_sdp[1][1][3][0]=-7.57283; p2_pc3_m_sdp[1][1][3][0]=1.47119; p3_pc3_m_sdp[1][1][3][0]=19.6131; p4_pc3_m_sdp[1][1][3][0]=-17.7987; 
    p0_pc3_s_sdp[1][1][3][0]=-1.71748; p1_pc3_s_sdp[1][1][3][0]=10.1587; p2_pc3_s_sdp[1][1][3][0]=-3.88406; p3_pc3_s_sdp[1][1][3][0]=-27.2866; p4_pc3_s_sdp[1][1][3][0]=27.8821; 
    p0_pc3_m_sdp[0][1][4][0]=0.134002; p1_pc3_m_sdp[0][1][4][0]=-0.304681; p2_pc3_m_sdp[0][1][4][0]=0.239329; p3_pc3_m_sdp[0][1][4][0]=-0.0831755; p4_pc3_m_sdp[0][1][4][0]=0.0106821; 
    p0_pc3_s_sdp[0][1][4][0]=0.47528; p1_pc3_s_sdp[0][1][4][0]=0.608529; p2_pc3_s_sdp[0][1][4][0]=-0.385774; p3_pc3_s_sdp[0][1][4][0]=0.101238; p4_pc3_s_sdp[0][1][4][0]=-0.00853592; 
    p0_pc3_m_sdp[1][1][4][0]=-3.0059; p1_pc3_m_sdp[1][1][4][0]=12.5977; p2_pc3_m_sdp[1][1][4][0]=-4.02991; p3_pc3_m_sdp[1][1][4][0]=-34.4647; p4_pc3_m_sdp[1][1][4][0]=33.7086; 
    p0_pc3_s_sdp[1][1][4][0]=0.417837; p1_pc3_s_sdp[1][1][4][0]=0.534494; p2_pc3_s_sdp[1][1][4][0]=0.485654; p3_pc3_s_sdp[1][1][4][0]=0.0102367; p4_pc3_s_sdp[1][1][4][0]=-1.37182; 
    p0_pc3_m_sdp[0][1][5][0]=0.238572; p1_pc3_m_sdp[0][1][5][0]=-0.535415; p2_pc3_m_sdp[0][1][5][0]=0.401397; p3_pc3_m_sdp[0][1][5][0]=-0.125503; p4_pc3_m_sdp[0][1][5][0]=0.013992; 
    p0_pc3_s_sdp[0][1][5][0]=0.622206; p1_pc3_s_sdp[0][1][5][0]=0.324264; p2_pc3_s_sdp[0][1][5][0]=-0.197218; p3_pc3_s_sdp[0][1][5][0]=0.0484121; p4_pc3_s_sdp[0][1][5][0]=-0.00325442; 
    p0_pc3_m_sdp[1][1][5][0]=1.86338; p1_pc3_m_sdp[1][1][5][0]=-6.43172; p2_pc3_m_sdp[1][1][5][0]=1.17834; p3_pc3_m_sdp[1][1][5][0]=14.9604; p4_pc3_m_sdp[1][1][5][0]=-12.7328; 
    p0_pc3_s_sdp[1][1][5][0]=0.0713618; p1_pc3_s_sdp[1][1][5][0]=3.32752; p2_pc3_s_sdp[1][1][5][0]=-1.80965; p3_pc3_s_sdp[1][1][5][0]=-9.3823; p4_pc3_s_sdp[1][1][5][0]=10.3595; 
    p0_pc3_m_sdp[0][1][6][0]=0.102117; p1_pc3_m_sdp[0][1][6][0]=-0.318005; p2_pc3_m_sdp[0][1][6][0]=0.363282; p3_pc3_m_sdp[0][1][6][0]=-0.168204; p4_pc3_m_sdp[0][1][6][0]=0.0267984; 
    p0_pc3_s_sdp[0][1][6][0]=0.588603; p1_pc3_s_sdp[0][1][6][0]=0.49049; p2_pc3_s_sdp[0][1][6][0]=-0.411246; p3_pc3_s_sdp[0][1][6][0]=0.134109; p4_pc3_s_sdp[0][1][6][0]=-0.0125654; 
    p0_pc3_m_sdp[1][1][6][0]=-1.66066; p1_pc3_m_sdp[1][1][6][0]=8.83291; p2_pc3_m_sdp[1][1][6][0]=-4.62517; p3_pc3_m_sdp[1][1][6][0]=-25.6816; p4_pc3_m_sdp[1][1][6][0]=27.9501; 
    p0_pc3_s_sdp[1][1][6][0]=3.9773; p1_pc3_s_sdp[1][1][6][0]=-12.1593; p2_pc3_s_sdp[1][1][6][0]=2.41506; p3_pc3_s_sdp[1][1][6][0]=32.5429; p4_pc3_s_sdp[1][1][6][0]=-29.5932; 
    p0_pc3_m_sdp[0][1][7][0]=0.178609; p1_pc3_m_sdp[0][1][7][0]=-0.379392; p2_pc3_m_sdp[0][1][7][0]=0.27781; p3_pc3_m_sdp[0][1][7][0]=-0.0861795; p4_pc3_m_sdp[0][1][7][0]=0.00994203; 
    p0_pc3_s_sdp[0][1][7][0]=-0.0937994; p1_pc3_s_sdp[0][1][7][0]=2.21954; p2_pc3_s_sdp[0][1][7][0]=-1.95934; p3_pc3_s_sdp[0][1][7][0]=0.709692; p4_pc3_s_sdp[0][1][7][0]=-0.0861741; 
    p0_pc3_m_sdp[1][1][7][0]=-4.06065; p1_pc3_m_sdp[1][1][7][0]=15.5903; p2_pc3_m_sdp[1][1][7][0]=-3.8413; p3_pc3_m_sdp[1][1][7][0]=-41.5093; p4_pc3_m_sdp[1][1][7][0]=39.0415; 
    p0_pc3_s_sdp[1][1][7][0]=2.36588; p1_pc3_s_sdp[1][1][7][0]=-6.66248; p2_pc3_s_sdp[1][1][7][0]=1.84558; p3_pc3_s_sdp[1][1][7][0]=17.6022; p4_pc3_s_sdp[1][1][7][0]=-16.3308; 
    p0_pc3_m_sdp[0][1][8][0]=-0.360044; p1_pc3_m_sdp[0][1][8][0]=1.14803; p2_pc3_m_sdp[0][1][8][0]=-0.774639; p3_pc3_m_sdp[0][1][8][0]=0.123211; p4_pc3_m_sdp[0][1][8][0]=0.0100133; 
    p0_pc3_s_sdp[0][1][8][0]=0.100223; p1_pc3_s_sdp[0][1][8][0]=1.80422; p2_pc3_s_sdp[0][1][8][0]=-1.64577; p3_pc3_s_sdp[0][1][8][0]=0.617709; p4_pc3_s_sdp[0][1][8][0]=-0.0773846; 
    p0_pc3_m_sdp[1][1][8][0]=-6.512; p1_pc3_m_sdp[1][1][8][0]=13.4394; p2_pc3_m_sdp[1][1][8][0]=4.35178; p3_pc3_m_sdp[1][1][8][0]=-23.6233; p4_pc3_m_sdp[1][1][8][0]=12.4499; 
    p0_pc3_s_sdp[1][1][8][0]=5.06545; p1_pc3_s_sdp[1][1][8][0]=-16.2146; p2_pc3_s_sdp[1][1][8][0]=3.79669; p3_pc3_s_sdp[1][1][8][0]=41.674; p4_pc3_s_sdp[1][1][8][0]=-38.3058; 
    p0_pc3_m_sdp[0][1][9][0]=0.143491; p1_pc3_m_sdp[0][1][9][0]=-0.356375; p2_pc3_m_sdp[0][1][9][0]=0.322579; p3_pc3_m_sdp[0][1][9][0]=-0.118562; p4_pc3_m_sdp[0][1][9][0]=0.015543; 
    p0_pc3_s_sdp[0][1][9][0]=-0.129707; p1_pc3_s_sdp[0][1][9][0]=2.35699; p2_pc3_s_sdp[0][1][9][0]=-2.10847; p3_pc3_s_sdp[0][1][9][0]=0.780223; p4_pc3_s_sdp[0][1][9][0]=-0.097764; 
    p0_pc3_m_sdp[1][1][9][0]=-1.42394; p1_pc3_m_sdp[1][1][9][0]=4.31402; p2_pc3_m_sdp[1][1][9][0]=-0.338672; p3_pc3_m_sdp[1][1][9][0]=-8.69842; p4_pc3_m_sdp[1][1][9][0]=6.33719; 
    p0_pc3_s_sdp[1][1][9][0]=2.39878; p1_pc3_s_sdp[1][1][9][0]=-5.20686; p2_pc3_s_sdp[1][1][9][0]=-0.454538; p3_pc3_s_sdp[1][1][9][0]=12.9638; p4_pc3_s_sdp[1][1][9][0]=-9.07109; 

    //PC3 DPHI POS 
	
    p0_pc3_m_sdp[0][0][0][1]=-0.0794716; p1_pc3_m_sdp[0][0][0][1]=0.266844; p2_pc3_m_sdp[0][0][0][1]=-0.287715; p3_pc3_m_sdp[0][0][0][1]=0.122643; p4_pc3_m_sdp[0][0][0][1]=-0.0181796; 
    p0_pc3_s_sdp[0][0][0][1]=-0.0429104; p1_pc3_s_sdp[0][0][0][1]=1.85111; p2_pc3_s_sdp[0][0][0][1]=-1.58654; p3_pc3_s_sdp[0][0][0][1]=0.591143; p4_pc3_s_sdp[0][0][0][1]=-0.0753263; 
    p0_pc3_m_sdp[1][0][0][1]=1.30088; p1_pc3_m_sdp[1][0][0][1]=-4.34364; p2_pc3_m_sdp[1][0][0][1]=0.793074; p3_pc3_m_sdp[1][0][0][1]=11.0134; p4_pc3_m_sdp[1][0][0][1]=-10.1523; 
    p0_pc3_s_sdp[1][0][0][1]=-1.18633; p1_pc3_s_sdp[1][0][0][1]=7.77758; p2_pc3_s_sdp[1][0][0][1]=-2.56063; p3_pc3_s_sdp[1][0][0][1]=-21.6087; p4_pc3_s_sdp[1][0][0][1]=21.2709; 
    p0_pc3_m_sdp[0][0][1][1]=-0.00805557; p1_pc3_m_sdp[0][0][1][1]=0.187757; p2_pc3_m_sdp[0][0][1][1]=-0.255644; p3_pc3_m_sdp[0][0][1][1]=0.118961; p4_pc3_m_sdp[0][0][1][1]=-0.0183261; 
    p0_pc3_s_sdp[0][0][1][1]=-0.260629; p1_pc3_s_sdp[0][0][1][1]=2.43232; p2_pc3_s_sdp[0][0][1][1]=-2.09624; p3_pc3_s_sdp[0][0][1][1]=0.758592; p4_pc3_s_sdp[0][0][1][1]=-0.0920041; 
    p0_pc3_m_sdp[1][0][1][1]=1.78238; p1_pc3_m_sdp[1][0][1][1]=-5.3888; p2_pc3_m_sdp[1][0][1][1]=0.363523; p3_pc3_m_sdp[1][0][1][1]=12.0398; p4_pc3_m_sdp[1][0][1][1]=-9.25952; 
    p0_pc3_s_sdp[1][0][1][1]=0.0565093; p1_pc3_s_sdp[1][0][1][1]=2.63341; p2_pc3_s_sdp[1][0][1][1]=-1.00757; p3_pc3_s_sdp[1][0][1][1]=-7.26859; p4_pc3_s_sdp[1][0][1][1]=7.39408; 
    p0_pc3_m_sdp[0][0][2][1]=0.0100688; p1_pc3_m_sdp[0][0][2][1]=0.11751; p2_pc3_m_sdp[0][0][2][1]=-0.158526; p3_pc3_m_sdp[0][0][2][1]=0.0706435; p4_pc3_m_sdp[0][0][2][1]=-0.0103323; 
    p0_pc3_s_sdp[0][0][2][1]=0.176821; p1_pc3_s_sdp[0][0][2][1]=1.30861; p2_pc3_s_sdp[0][0][2][1]=-1.07048; p3_pc3_s_sdp[0][0][2][1]=0.363856; p4_pc3_s_sdp[0][0][2][1]=-0.0394517; 
    p0_pc3_m_sdp[1][0][2][1]=-0.915757; p1_pc3_m_sdp[1][0][2][1]=3.73263; p2_pc3_m_sdp[1][0][2][1]=-0.571406; p3_pc3_m_sdp[1][0][2][1]=-10.3819; p4_pc3_m_sdp[1][0][2][1]=9.2947; 
    p0_pc3_s_sdp[1][0][2][1]=2.04754; p1_pc3_s_sdp[1][0][2][1]=-6.00135; p2_pc3_s_sdp[1][0][2][1]=1.88501; p3_pc3_s_sdp[1][0][2][1]=17.4674; p4_pc3_s_sdp[1][0][2][1]=-17.2048; 
    p0_pc3_m_sdp[0][0][3][1]=0.314965; p1_pc3_m_sdp[0][0][3][1]=-0.597718; p2_pc3_m_sdp[0][0][3][1]=0.461128; p3_pc3_m_sdp[0][0][3][1]=-0.158812; p4_pc3_m_sdp[0][0][3][1]=0.0202902; 
    p0_pc3_s_sdp[0][0][3][1]=0.387128; p1_pc3_s_sdp[0][0][3][1]=0.7952; p2_pc3_s_sdp[0][0][3][1]=-0.642933; p3_pc3_s_sdp[0][0][3][1]=0.224768; p4_pc3_s_sdp[0][0][3][1]=-0.0269833; 
    p0_pc3_m_sdp[1][0][3][1]=1.67213; p1_pc3_m_sdp[1][0][3][1]=-5.47368; p2_pc3_m_sdp[1][0][3][1]=0.915334; p3_pc3_m_sdp[1][0][3][1]=13.4483; p4_pc3_m_sdp[1][0][3][1]=-11.8425; 
    p0_pc3_s_sdp[1][0][3][1]=-2.9524; p1_pc3_s_sdp[1][0][3][1]=14.0704; p2_pc3_s_sdp[1][0][3][1]=-4.0744; p3_pc3_s_sdp[1][0][3][1]=-38.0671; p4_pc3_s_sdp[1][0][3][1]=37.2968; 
    p0_pc3_m_sdp[0][0][4][1]=0.155546; p1_pc3_m_sdp[0][0][4][1]=-0.233324; p2_pc3_m_sdp[0][0][4][1]=0.163399; p3_pc3_m_sdp[0][0][4][1]=-0.0547798; p4_pc3_m_sdp[0][0][4][1]=0.00696502; 
    p0_pc3_s_sdp[0][0][4][1]=0.419542; p1_pc3_s_sdp[0][0][4][1]=0.723174; p2_pc3_s_sdp[0][0][4][1]=-0.576153; p3_pc3_s_sdp[0][0][4][1]=0.197228; p4_pc3_s_sdp[0][0][4][1]=-0.0232536; 
    p0_pc3_m_sdp[1][0][4][1]=-0.899056; p1_pc3_m_sdp[1][0][4][1]=3.47991; p2_pc3_m_sdp[1][0][4][1]=-0.649995; p3_pc3_m_sdp[1][0][4][1]=-8.38334; p4_pc3_m_sdp[1][0][4][1]=7.0647; 
    p0_pc3_s_sdp[1][0][4][1]=-0.360975; p1_pc3_s_sdp[1][0][4][1]=3.96556; p2_pc3_s_sdp[1][0][4][1]=-1.43697; p3_pc3_s_sdp[1][0][4][1]=-10.2957; p4_pc3_s_sdp[1][0][4][1]=10.5784; 
    p0_pc3_m_sdp[0][0][5][1]=0.26587; p1_pc3_m_sdp[0][0][5][1]=-0.483744; p2_pc3_m_sdp[0][0][5][1]=0.350297; p3_pc3_m_sdp[0][0][5][1]=-0.113905; p4_pc3_m_sdp[0][0][5][1]=0.0139303; 
    p0_pc3_s_sdp[0][0][5][1]=0.615964; p1_pc3_s_sdp[0][0][5][1]=0.246041; p2_pc3_s_sdp[0][0][5][1]=-0.184124; p3_pc3_s_sdp[0][0][5][1]=0.0678769; p4_pc3_s_sdp[0][0][5][1]=-0.00872454; 
    p0_pc3_m_sdp[1][0][5][1]=-0.629856; p1_pc3_m_sdp[1][0][5][1]=2.58821; p2_pc3_m_sdp[1][0][5][1]=-0.290623; p3_pc3_m_sdp[1][0][5][1]=-6.88961; p4_pc3_m_sdp[1][0][5][1]=5.79778; 
    p0_pc3_s_sdp[1][0][5][1]=-0.91526; p1_pc3_s_sdp[1][0][5][1]=5.4487; p2_pc3_s_sdp[1][0][5][1]=-1.06884; p3_pc3_s_sdp[1][0][5][1]=-12.9238; p4_pc3_s_sdp[1][0][5][1]=11.5412; 
    p0_pc3_m_sdp[0][0][6][1]=0.342043; p1_pc3_m_sdp[0][0][6][1]=-0.662301; p2_pc3_m_sdp[0][0][6][1]=0.486516; p3_pc3_m_sdp[0][0][6][1]=-0.155962; p4_pc3_m_sdp[0][0][6][1]=0.0183324; 
    p0_pc3_s_sdp[0][0][6][1]=0.352981; p1_pc3_s_sdp[0][0][6][1]=0.801861; p2_pc3_s_sdp[0][0][6][1]=-0.604755; p3_pc3_s_sdp[0][0][6][1]=0.200114; p4_pc3_s_sdp[0][0][6][1]=-0.0228374; 
    p0_pc3_m_sdp[1][0][6][1]=-0.42688; p1_pc3_m_sdp[1][0][6][1]=2.07165; p2_pc3_m_sdp[1][0][6][1]=-0.375077; p3_pc3_m_sdp[1][0][6][1]=-5.76302; p4_pc3_m_sdp[1][0][6][1]=5.05149; 
    p0_pc3_s_sdp[1][0][6][1]=1.18031; p1_pc3_s_sdp[1][0][6][1]=-2.47604; p2_pc3_s_sdp[1][0][6][1]=1.00946; p3_pc3_s_sdp[1][0][6][1]=7.48947; p4_pc3_s_sdp[1][0][6][1]=-7.79288; 
    p0_pc3_m_sdp[0][0][7][1]=0.172109; p1_pc3_m_sdp[0][0][7][1]=-0.246082; p2_pc3_m_sdp[0][0][7][1]=0.119817; p3_pc3_m_sdp[0][0][7][1]=-0.0212387; p4_pc3_m_sdp[0][0][7][1]=0.000714863; 
    p0_pc3_s_sdp[0][0][7][1]=0.205669; p1_pc3_s_sdp[0][0][7][1]=1.20078; p2_pc3_s_sdp[0][0][7][1]=-0.970923; p3_pc3_s_sdp[0][0][7][1]=0.329179; p4_pc3_s_sdp[0][0][7][1]=-0.0353643; 
    p0_pc3_m_sdp[1][0][7][1]=-1.74648; p1_pc3_m_sdp[1][0][7][1]=8.03119; p2_pc3_m_sdp[1][0][7][1]=-3.38816; p3_pc3_m_sdp[1][0][7][1]=-21.6313; p4_pc3_m_sdp[1][0][7][1]=22.2976; 
    p0_pc3_s_sdp[1][0][7][1]=2.49481; p1_pc3_s_sdp[1][0][7][1]=-7.50316; p2_pc3_s_sdp[1][0][7][1]=2.23422; p3_pc3_s_sdp[1][0][7][1]=20.5119; p4_pc3_s_sdp[1][0][7][1]=-19.7843; 
    p0_pc3_m_sdp[0][0][8][1]=0.056005; p1_pc3_m_sdp[0][0][8][1]=0.0129314; p2_pc3_m_sdp[0][0][8][1]=-0.122637; p3_pc3_m_sdp[0][0][8][1]=0.0804719; p4_pc3_m_sdp[0][0][8][1]=-0.0149193; 
    p0_pc3_s_sdp[0][0][8][1]=-0.155118; p1_pc3_s_sdp[0][0][8][1]=2.13151; p2_pc3_s_sdp[0][0][8][1]=-1.82746; p3_pc3_s_sdp[0][0][8][1]=0.663135; p4_pc3_s_sdp[0][0][8][1]=-0.0806587; 
    p0_pc3_m_sdp[1][0][8][1]=0.603889; p1_pc3_m_sdp[1][0][8][1]=-1.01985; p2_pc3_m_sdp[1][0][8][1]=-0.724333; p3_pc3_m_sdp[1][0][8][1]=2.12229; p4_pc3_m_sdp[1][0][8][1]=-0.951624; 
    p0_pc3_s_sdp[1][0][8][1]=2.9943; p1_pc3_s_sdp[1][0][8][1]=-9.29579; p2_pc3_s_sdp[1][0][8][1]=2.58034; p3_pc3_s_sdp[1][0][8][1]=25.4253; p4_pc3_s_sdp[1][0][8][1]=-24.4857; 
    p0_pc3_m_sdp[0][0][9][1]=-0.0674908; p1_pc3_m_sdp[0][0][9][1]=0.26627; p2_pc3_m_sdp[0][0][9][1]=-0.324248; p3_pc3_m_sdp[0][0][9][1]=0.14674; p4_pc3_m_sdp[0][0][9][1]=-0.0222126; 
    p0_pc3_s_sdp[0][0][9][1]=-0.314127; p1_pc3_s_sdp[0][0][9][1]=2.47001; p2_pc3_s_sdp[0][0][9][1]=-2.05817; p3_pc3_s_sdp[0][0][9][1]=0.7362; p4_pc3_s_sdp[0][0][9][1]=-0.0907725; 
    p0_pc3_m_sdp[1][0][9][1]=2.04619; p1_pc3_m_sdp[1][0][9][1]=-6.16926; p2_pc3_m_sdp[1][0][9][1]=0.163983; p3_pc3_m_sdp[1][0][9][1]=14.0895; p4_pc3_m_sdp[1][0][9][1]=-10.8188; 
    p0_pc3_s_sdp[1][0][9][1]=-0.346407; p1_pc3_s_sdp[1][0][9][1]=4.66769; p2_pc3_s_sdp[1][0][9][1]=-2.5912; p3_pc3_s_sdp[1][0][9][1]=-12.4653; p4_pc3_s_sdp[1][0][9][1]=13.9319; 
    p0_pc3_m_sdp[0][1][0][1]=-0.31135; p1_pc3_m_sdp[0][1][0][1]=1.14447; p2_pc3_m_sdp[0][1][0][1]=-1.217; p3_pc3_m_sdp[0][1][0][1]=0.502342; p4_pc3_m_sdp[0][1][0][1]=-0.0712238; 
    p0_pc3_s_sdp[0][1][0][1]=-0.341962; p1_pc3_s_sdp[0][1][0][1]=2.77274; p2_pc3_s_sdp[0][1][0][1]=-2.37977; p3_pc3_s_sdp[0][1][0][1]=0.857112; p4_pc3_s_sdp[0][1][0][1]=-0.104289; 
    p0_pc3_m_sdp[1][1][0][1]=1.66806; p1_pc3_m_sdp[1][1][0][1]=-6.09518; p2_pc3_m_sdp[1][1][0][1]=1.71283; p3_pc3_m_sdp[1][1][0][1]=16.4292; p4_pc3_m_sdp[1][1][0][1]=-16.1009; 
    p0_pc3_s_sdp[1][1][0][1]=3.86798; p1_pc3_s_sdp[1][1][0][1]=-13.2064; p2_pc3_s_sdp[1][1][0][1]=4.11441; p3_pc3_s_sdp[1][1][0][1]=36.1041; p4_pc3_s_sdp[1][1][0][1]=-35.0097; 
    p0_pc3_m_sdp[0][1][1][1]=0.123705; p1_pc3_m_sdp[0][1][1][1]=-0.0546854; p2_pc3_m_sdp[0][1][1][1]=-0.0558368; p3_pc3_m_sdp[0][1][1][1]=0.0409117; p4_pc3_m_sdp[0][1][1][1]=-0.00729088; 
    p0_pc3_s_sdp[0][1][1][1]=-0.541824; p1_pc3_s_sdp[0][1][1][1]=3.12513; p2_pc3_s_sdp[0][1][1][1]=-2.56209; p3_pc3_s_sdp[0][1][1][1]=0.880744; p4_pc3_s_sdp[0][1][1][1]=-0.103754; 
    p0_pc3_m_sdp[1][1][1][1]=0.7774; p1_pc3_m_sdp[1][1][1][1]=-2.98726; p2_pc3_m_sdp[1][1][1][1]=1.05845; p3_pc3_m_sdp[1][1][1][1]=8.98597; p4_pc3_m_sdp[1][1][1][1]=-9.40151; 
    p0_pc3_s_sdp[1][1][1][1]=-0.23547; p1_pc3_s_sdp[1][1][1][1]=4.55375; p2_pc3_s_sdp[1][1][1][1]=-2.62656; p3_pc3_s_sdp[1][1][1][1]=-13.6048; p4_pc3_s_sdp[1][1][1][1]=15.6825; 
    p0_pc3_m_sdp[0][1][2][1]=0.312814; p1_pc3_m_sdp[0][1][2][1]=-0.569457; p2_pc3_m_sdp[0][1][2][1]=0.450667; p3_pc3_m_sdp[0][1][2][1]=-0.164709; p4_pc3_m_sdp[0][1][2][1]=0.0220367; 
    p0_pc3_s_sdp[0][1][2][1]=-0.108947; p1_pc3_s_sdp[0][1][2][1]=2.01478; p2_pc3_s_sdp[0][1][2][1]=-1.58815; p3_pc3_s_sdp[0][1][2][1]=0.518105; p4_pc3_s_sdp[0][1][2][1]=-0.0554847; 
    p0_pc3_m_sdp[1][1][2][1]=-2.6845; p1_pc3_m_sdp[1][1][2][1]=10.0454; p2_pc3_m_sdp[1][1][2][1]=-2.06673; p3_pc3_m_sdp[1][1][2][1]=-26.0264; p4_pc3_m_sdp[1][1][2][1]=23.8479; 
    p0_pc3_s_sdp[1][1][2][1]=1.83848; p1_pc3_s_sdp[1][1][2][1]=-4.42752; p2_pc3_s_sdp[1][1][2][1]=0.56873; p3_pc3_s_sdp[1][1][2][1]=11.5654; p4_pc3_s_sdp[1][1][2][1]=-9.51017; 
    p0_pc3_m_sdp[0][1][3][1]=0.155537; p1_pc3_m_sdp[0][1][3][1]=-0.153932; p2_pc3_m_sdp[0][1][3][1]=0.0428423; p3_pc3_m_sdp[0][1][3][1]=0.00843553; p4_pc3_m_sdp[0][1][3][1]=-0.00391865; 
    p0_pc3_s_sdp[0][1][3][1]=0.259096; p1_pc3_s_sdp[0][1][3][1]=1.0698; p2_pc3_s_sdp[0][1][3][1]=-0.759388; p3_pc3_s_sdp[0][1][3][1]=0.220127; p4_pc3_s_sdp[0][1][3][1]=-0.0200796; 
    p0_pc3_m_sdp[1][1][3][1]=-2.00286; p1_pc3_m_sdp[1][1][3][1]=7.17384; p2_pc3_m_sdp[1][1][3][1]=-1.01165; p3_pc3_m_sdp[1][1][3][1]=-17.9306; p4_pc3_m_sdp[1][1][3][1]=15.5408; 
    p0_pc3_s_sdp[1][1][3][1]=2.37822; p1_pc3_s_sdp[1][1][3][1]=-6.74347; p2_pc3_s_sdp[1][1][3][1]=1.5041; p3_pc3_s_sdp[1][1][3][1]=17.978; p4_pc3_s_sdp[1][1][3][1]=-16.2924; 
    p0_pc3_m_sdp[0][1][4][1]=0.0451227; p1_pc3_m_sdp[0][1][4][1]=0.138638; p2_pc3_m_sdp[0][1][4][1]=-0.221733; p3_pc3_m_sdp[0][1][4][1]=0.106298; p4_pc3_m_sdp[0][1][4][1]=-0.01671; 
    p0_pc3_s_sdp[0][1][4][1]=0.434712; p1_pc3_s_sdp[0][1][4][1]=0.712125; p2_pc3_s_sdp[0][1][4][1]=-0.48476; p3_pc3_s_sdp[0][1][4][1]=0.133645; p4_pc3_s_sdp[0][1][4][1]=-0.011941; 
    p0_pc3_m_sdp[1][1][4][1]=-1.24518; p1_pc3_m_sdp[1][1][4][1]=3.75444; p2_pc3_m_sdp[1][1][4][1]=0.403488; p3_pc3_m_sdp[1][1][4][1]=-7.9098; p4_pc3_m_sdp[1][1][4][1]=4.84621; 
    p0_pc3_s_sdp[1][1][4][1]=-0.263431; p1_pc3_s_sdp[1][1][4][1]=3.9678; p2_pc3_s_sdp[1][1][4][1]=-1.51963; p3_pc3_s_sdp[1][1][4][1]=-10.9653; p4_pc3_s_sdp[1][1][4][1]=11.5519; 
    p0_pc3_m_sdp[0][1][5][1]=0.0351954; p1_pc3_m_sdp[0][1][5][1]=0.125872; p2_pc3_m_sdp[0][1][5][1]=-0.187904; p3_pc3_m_sdp[0][1][5][1]=0.0852949; p4_pc3_m_sdp[0][1][5][1]=-0.01273; 
    p0_pc3_s_sdp[0][1][5][1]=0.484138; p1_pc3_s_sdp[0][1][5][1]=0.624044; p2_pc3_s_sdp[0][1][5][1]=-0.451659; p3_pc3_s_sdp[0][1][5][1]=0.135734; p4_pc3_s_sdp[0][1][5][1]=-0.0136233; 
    p0_pc3_m_sdp[1][1][5][1]=1.25078; p1_pc3_m_sdp[1][1][5][1]=-5.32964; p2_pc3_m_sdp[1][1][5][1]=2.23558; p3_pc3_m_sdp[1][1][5][1]=14.8622; p4_pc3_m_sdp[1][1][5][1]=-15.4801; 
    p0_pc3_s_sdp[1][1][5][1]=-1.85559; p1_pc3_s_sdp[1][1][5][1]=9.73558; p2_pc3_s_sdp[1][1][5][1]=-2.63618; p3_pc3_s_sdp[1][1][5][1]=-25.3792; p4_pc3_s_sdp[1][1][5][1]=24.2456; 
    p0_pc3_m_sdp[0][1][6][1]=-0.0125455; p1_pc3_m_sdp[0][1][6][1]=0.274772; p2_pc3_m_sdp[0][1][6][1]=-0.334443; p3_pc3_m_sdp[0][1][6][1]=0.143055; p4_pc3_m_sdp[0][1][6][1]=-0.0207034; 
    p0_pc3_s_sdp[0][1][6][1]=0.313956; p1_pc3_s_sdp[0][1][6][1]=0.95973; p2_pc3_s_sdp[0][1][6][1]=-0.698707; p3_pc3_s_sdp[0][1][6][1]=0.207716; p4_pc3_s_sdp[0][1][6][1]=-0.0193291; 
    p0_pc3_m_sdp[1][1][6][1]=-0.936238; p1_pc3_m_sdp[1][1][6][1]=3.06368; p2_pc3_m_sdp[1][1][6][1]=-0.0712141; p3_pc3_m_sdp[1][1][6][1]=-6.94447; p4_pc3_m_sdp[1][1][6][1]=5.24744; 
    p0_pc3_s_sdp[1][1][6][1]=-0.760848; p1_pc3_s_sdp[1][1][6][1]=5.47548; p2_pc3_s_sdp[1][1][6][1]=-1.53309; p3_pc3_s_sdp[1][1][6][1]=-14.0372; p4_pc3_s_sdp[1][1][6][1]=13.4642; 
    p0_pc3_m_sdp[0][1][7][1]=-0.0224539; p1_pc3_m_sdp[0][1][7][1]=0.272416; p2_pc3_m_sdp[0][1][7][1]=-0.343679; p3_pc3_m_sdp[0][1][7][1]=0.152087; p4_pc3_m_sdp[0][1][7][1]=-0.0226588; 
    p0_pc3_s_sdp[0][1][7][1]=0.065845; p1_pc3_s_sdp[0][1][7][1]=1.70278; p2_pc3_s_sdp[0][1][7][1]=-1.42512; p3_pc3_s_sdp[0][1][7][1]=0.485589; p4_pc3_s_sdp[0][1][7][1]=-0.0532079; 
    p0_pc3_m_sdp[1][1][7][1]=1.53226; p1_pc3_m_sdp[1][1][7][1]=-6.50286; p2_pc3_m_sdp[1][1][7][1]=2.45791; p3_pc3_m_sdp[1][1][7][1]=17.6992; p4_pc3_m_sdp[1][1][7][1]=-17.7202; 
    p0_pc3_s_sdp[1][1][7][1]=-0.136515; p1_pc3_s_sdp[1][1][7][1]=2.16336; p2_pc3_s_sdp[1][1][7][1]=0.415187; p3_pc3_s_sdp[1][1][7][1]=-5.11184; p4_pc3_s_sdp[1][1][7][1]=3.5855; 
    p0_pc3_m_sdp[0][1][8][1]=0.148862; p1_pc3_m_sdp[0][1][8][1]=-0.278399; p2_pc3_m_sdp[0][1][8][1]=0.217905; p3_pc3_m_sdp[0][1][8][1]=-0.079647; p4_pc3_m_sdp[0][1][8][1]=0.0105287; 
    p0_pc3_s_sdp[0][1][8][1]=-0.316831; p1_pc3_s_sdp[0][1][8][1]=2.7475; p2_pc3_s_sdp[0][1][8][1]=-2.39283; p3_pc3_s_sdp[0][1][8][1]=0.858459; p4_pc3_s_sdp[0][1][8][1]=-0.103616; 
    p0_pc3_m_sdp[1][1][8][1]=0.314297; p1_pc3_m_sdp[1][1][8][1]=-1.53593; p2_pc3_m_sdp[1][1][8][1]=0.497732; p3_pc3_m_sdp[1][1][8][1]=4.79404; p4_pc3_m_sdp[1][1][8][1]=-4.42389; 
    p0_pc3_s_sdp[1][1][8][1]=3.74291; p1_pc3_s_sdp[1][1][8][1]=-12.1824; p2_pc3_s_sdp[1][1][8][1]=3.36542; p3_pc3_s_sdp[1][1][8][1]=33.3909; p4_pc3_s_sdp[1][1][8][1]=-32.0745; 
    p0_pc3_m_sdp[0][1][9][1]=-0.031049; p1_pc3_m_sdp[0][1][9][1]=0.123068; p2_pc3_m_sdp[0][1][9][1]=-0.12391; p3_pc3_m_sdp[0][1][9][1]=0.0418996; p4_pc3_m_sdp[0][1][9][1]=-0.00450689; 
    p0_pc3_s_sdp[0][1][9][1]=-0.202245; p1_pc3_s_sdp[0][1][9][1]=2.47953; p2_pc3_s_sdp[0][1][9][1]=-2.18823; p3_pc3_s_sdp[0][1][9][1]=0.801943; p4_pc3_s_sdp[0][1][9][1]=-0.0995165; 
    p0_pc3_m_sdp[1][1][9][1]=-0.198825; p1_pc3_m_sdp[1][1][9][1]=0.160108; p2_pc3_m_sdp[1][1][9][1]=0.496719; p3_pc3_m_sdp[1][1][9][1]=0.36725; p4_pc3_m_sdp[1][1][9][1]=-1.13424; 
    p0_pc3_s_sdp[1][1][9][1]=3.50368; p1_pc3_s_sdp[1][1][9][1]=-9.92834; p2_pc3_s_sdp[1][1][9][1]=1.68097; p3_pc3_s_sdp[1][1][9][1]=24.9609; p4_pc3_s_sdp[1][1][9][1]=-21.8905; 


    //PC3 DZ NEG  27

	
    p0_pc3_m_sdz[0][0][0][0]=-0.183204; p1_pc3_m_sdz[0][0][0][0]=0.205575; p2_pc3_m_sdz[0][0][0][0]=-0.0926581; p3_pc3_m_sdz[0][0][0][0]=0.0120739; p4_pc3_m_sdz[0][0][0][0]=0.00173202; 
    p0_pc3_s_sdz[0][0][0][0]=0.771688; p1_pc3_s_sdz[0][0][0][0]=-0.206331; p2_pc3_s_sdz[0][0][0][0]=0.253819; p3_pc3_s_sdz[0][0][0][0]=-0.088801; p4_pc3_s_sdz[0][0][0][0]=0.00902271; 
    p0_pc3_m_sdz[1][0][0][0]=2.23791; p1_pc3_m_sdz[1][0][0][0]=-11.7502; p2_pc3_m_sdz[1][0][0][0]=4.76945; p3_pc3_m_sdz[1][0][0][0]=32.0391; p4_pc3_m_sdz[1][0][0][0]=-30.9756; 
    p0_pc3_s_sdz[1][0][0][0]=1.12291; p1_pc3_s_sdz[1][0][0][0]=-0.343907; p2_pc3_s_sdz[1][0][0][0]=-1.64353; p3_pc3_s_sdz[1][0][0][0]=-1.09044; p4_pc3_s_sdz[1][0][0][0]=4.72913; 
    p0_pc3_m_sdz[0][0][1][0]=0.0416716; p1_pc3_m_sdz[0][0][1][0]=-0.176993; p2_pc3_m_sdz[0][0][1][0]=0.14907; p3_pc3_m_sdz[0][0][1][0]=-0.0496278; p4_pc3_m_sdz[0][0][1][0]=0.00615464; 
    p0_pc3_s_sdz[0][0][1][0]=0.695138; p1_pc3_s_sdz[0][0][1][0]=0.00726893; p2_pc3_s_sdz[0][0][1][0]=0.0555739; p3_pc3_s_sdz[0][0][1][0]=-0.0178156; p4_pc3_s_sdz[0][0][1][0]=0.00101783; 
    p0_pc3_m_sdz[1][0][1][0]=4.33401; p1_pc3_m_sdz[1][0][1][0]=-18.6906; p2_pc3_m_sdz[1][0][1][0]=5.92776; p3_pc3_m_sdz[1][0][1][0]=50.7305; p4_pc3_m_sdz[1][0][1][0]=-48.566; 
    p0_pc3_s_sdz[1][0][1][0]=4.08834; p1_pc3_s_sdz[1][0][1][0]=-15.5666; p2_pc3_s_sdz[1][0][1][0]=6.61002; p3_pc3_s_sdz[1][0][1][0]=47.7477; p4_pc3_s_sdz[1][0][1][0]=-51.6675; 
    p0_pc3_m_sdz[0][0][2][0]=-0.08636; p1_pc3_m_sdz[0][0][2][0]=0.168955; p2_pc3_m_sdz[0][0][2][0]=-0.137635; p3_pc3_m_sdz[0][0][2][0]=0.0475335; p4_pc3_m_sdz[0][0][2][0]=-0.00584836; 
    p0_pc3_s_sdz[0][0][2][0]=0.705827; p1_pc3_s_sdz[0][0][2][0]=-0.0165634; p2_pc3_s_sdz[0][0][2][0]=0.0596383; p3_pc3_s_sdz[0][0][2][0]=-0.0115988; p4_pc3_s_sdz[0][0][2][0]=-0.000656715; 
    p0_pc3_m_sdz[1][0][2][0]=1.1202; p1_pc3_m_sdz[1][0][2][0]=-6.54747; p2_pc3_m_sdz[1][0][2][0]=2.88804; p3_pc3_m_sdz[1][0][2][0]=21.2035; p4_pc3_m_sdz[1][0][2][0]=-22.168; 
    p0_pc3_s_sdz[1][0][2][0]=9.38911; p1_pc3_s_sdz[1][0][2][0]=-33.8414; p2_pc3_s_sdz[1][0][2][0]=8.7214; p3_pc3_s_sdz[1][0][2][0]=91.8999; p4_pc3_s_sdz[1][0][2][0]=-87.3684; 
    p0_pc3_m_sdz[0][0][3][0]=0.143181; p1_pc3_m_sdz[0][0][3][0]=-0.346618; p2_pc3_m_sdz[0][0][3][0]=0.304858; p3_pc3_m_sdz[0][0][3][0]=-0.114742; p4_pc3_m_sdz[0][0][3][0]=0.0157669; 
    p0_pc3_s_sdz[0][0][3][0]=0.759547; p1_pc3_s_sdz[0][0][3][0]=-0.138359; p2_pc3_s_sdz[0][0][3][0]=0.178522; p3_pc3_s_sdz[0][0][3][0]=-0.0659029; p4_pc3_s_sdz[0][0][3][0]=0.00823456; 
    p0_pc3_m_sdz[1][0][3][0]=2.78295; p1_pc3_m_sdz[1][0][3][0]=-10.3805; p2_pc3_m_sdz[1][0][3][0]=1.82616; p3_pc3_m_sdz[1][0][3][0]=27.8683; p4_pc3_m_sdz[1][0][3][0]=-24.8412; 
    p0_pc3_s_sdz[1][0][3][0]=0.87931; p1_pc3_s_sdz[1][0][3][0]=-0.0106366; p2_pc3_s_sdz[1][0][3][0]=-0.887747; p3_pc3_s_sdz[1][0][3][0]=-0.752718; p4_pc3_s_sdz[1][0][3][0]=2.42334; 
    p0_pc3_m_sdz[0][0][4][0]=-0.00314267; p1_pc3_m_sdz[0][0][4][0]=0.0403475; p2_pc3_m_sdz[0][0][4][0]=-0.0415576; p3_pc3_m_sdz[0][0][4][0]=0.0179592; p4_pc3_m_sdz[0][0][4][0]=-0.00268752; 
    p0_pc3_s_sdz[0][0][4][0]=0.795208; p1_pc3_s_sdz[0][0][4][0]=-0.222877; p2_pc3_s_sdz[0][0][4][0]=0.250444; p3_pc3_s_sdz[0][0][4][0]=-0.092608; p4_pc3_s_sdz[0][0][4][0]=0.0117982; 
    p0_pc3_m_sdz[1][0][4][0]=-2.69044; p1_pc3_m_sdz[1][0][4][0]=10.7865; p2_pc3_m_sdz[1][0][4][0]=-3.13776; p3_pc3_m_sdz[1][0][4][0]=-29.1215; p4_pc3_m_sdz[1][0][4][0]=28.2745; 
    p0_pc3_s_sdz[1][0][4][0]=9.98159; p1_pc3_s_sdz[1][0][4][0]=-38.2101; p2_pc3_s_sdz[1][0][4][0]=14.4994; p3_pc3_s_sdz[1][0][4][0]=99.2977; p4_pc3_s_sdz[1][0][4][0]=-99.9913; 
    p0_pc3_m_sdz[0][0][5][0]=-0.00382064; p1_pc3_m_sdz[0][0][5][0]=0.0322365; p2_pc3_m_sdz[0][0][5][0]=-0.043968; p3_pc3_m_sdz[0][0][5][0]=0.024836; p4_pc3_m_sdz[0][0][5][0]=-0.00444537; 
    p0_pc3_s_sdz[0][0][5][0]=0.755337; p1_pc3_s_sdz[0][0][5][0]=-0.154586; p2_pc3_s_sdz[0][0][5][0]=0.199349; p3_pc3_s_sdz[0][0][5][0]=-0.0726652; p4_pc3_s_sdz[0][0][5][0]=0.00883625; 
    p0_pc3_m_sdz[1][0][5][0]=0.197997; p1_pc3_m_sdz[1][0][5][0]=-0.275911; p2_pc3_m_sdz[1][0][5][0]=-0.667888; p3_pc3_m_sdz[1][0][5][0]=-0.321383; p4_pc3_m_sdz[1][0][5][0]=2.12846; 
    p0_pc3_s_sdz[1][0][5][0]=0.968977; p1_pc3_s_sdz[1][0][5][0]=-0.123362; p2_pc3_s_sdz[1][0][5][0]=-1.14498; p3_pc3_s_sdz[1][0][5][0]=-0.874138; p4_pc3_s_sdz[1][0][5][0]=3.16302; 
    p0_pc3_m_sdz[0][0][6][0]=0.0099156; p1_pc3_m_sdz[0][0][6][0]=-0.0195922; p2_pc3_m_sdz[0][0][6][0]=0.0103617; p3_pc3_m_sdz[0][0][6][0]=0.00153841; p4_pc3_m_sdz[0][0][6][0]=-0.00104043; 
    p0_pc3_s_sdz[0][0][6][0]=0.734035; p1_pc3_s_sdz[0][0][6][0]=-0.0851294; p2_pc3_s_sdz[0][0][6][0]=0.141294; p3_pc3_s_sdz[0][0][6][0]=-0.0496919; p4_pc3_s_sdz[0][0][6][0]=0.00519654; 
    p0_pc3_m_sdz[1][0][6][0]=-1.85663; p1_pc3_m_sdz[1][0][6][0]=7.99597; p2_pc3_m_sdz[1][0][6][0]=-2.5537; p3_pc3_m_sdz[1][0][6][0]=-22.4175; p4_pc3_m_sdz[1][0][6][0]=21.89; 
    p0_pc3_s_sdz[1][0][6][0]=5.75228; p1_pc3_s_sdz[1][0][6][0]=-20.7582; p2_pc3_s_sdz[1][0][6][0]=7.40382; p3_pc3_s_sdz[1][0][6][0]=56.58; p4_pc3_s_sdz[1][0][6][0]=-57.3389; 
    p0_pc3_m_sdz[0][0][7][0]=0.00273158; p1_pc3_m_sdz[0][0][7][0]=-0.0150032; p2_pc3_m_sdz[0][0][7][0]=0.00781038; p3_pc3_m_sdz[0][0][7][0]=0.00135792; p4_pc3_m_sdz[0][0][7][0]=-0.000785252; 
    p0_pc3_s_sdz[0][0][7][0]=0.706612; p1_pc3_s_sdz[0][0][7][0]=-0.0319645; p2_pc3_s_sdz[0][0][7][0]=0.104215; p3_pc3_s_sdz[0][0][7][0]=-0.0416381; p4_pc3_s_sdz[0][0][7][0]=0.00521074; 
    p0_pc3_m_sdz[1][0][7][0]=0.332052; p1_pc3_m_sdz[1][0][7][0]=-0.0169491; p2_pc3_m_sdz[1][0][7][0]=-0.417194; p3_pc3_m_sdz[1][0][7][0]=-0.563733; p4_pc3_m_sdz[1][0][7][0]=0.188988; 
    p0_pc3_s_sdz[1][0][7][0]=2.39647; p1_pc3_s_sdz[1][0][7][0]=-7.50842; p2_pc3_s_sdz[1][0][7][0]=2.88944; p3_pc3_s_sdz[1][0][7][0]=22.5073; p4_pc3_s_sdz[1][0][7][0]=-23.6292; 
    p0_pc3_m_sdz[0][0][8][0]=0.103004; p1_pc3_m_sdz[0][0][8][0]=-0.222825; p2_pc3_m_sdz[0][0][8][0]=0.179882; p3_pc3_m_sdz[0][0][8][0]=-0.0633723; p4_pc3_m_sdz[0][0][8][0]=0.00827874; 
    p0_pc3_s_sdz[0][0][8][0]=0.615494; p1_pc3_s_sdz[0][0][8][0]=0.150802; p2_pc3_s_sdz[0][0][8][0]=-0.0178963; p3_pc3_s_sdz[0][0][8][0]=-0.00606727; p4_pc3_s_sdz[0][0][8][0]=0.00105524; 
    p0_pc3_m_sdz[1][0][8][0]=0.363657; p1_pc3_m_sdz[1][0][8][0]=0.0804038; p2_pc3_m_sdz[1][0][8][0]=-0.327465; p3_pc3_m_sdz[1][0][8][0]=-0.663398; p4_pc3_m_sdz[1][0][8][0]=-0.459973; 
    p0_pc3_s_sdz[1][0][8][0]=1.66816; p1_pc3_s_sdz[1][0][8][0]=-2.94786; p2_pc3_s_sdz[1][0][8][0]=-0.539795; p3_pc3_s_sdz[1][0][8][0]=8.43874; p4_pc3_s_sdz[1][0][8][0]=-6.25994; 
    p0_pc3_m_sdz[0][0][9][0]=0.082444; p1_pc3_m_sdz[0][0][9][0]=-0.185688; p2_pc3_m_sdz[0][0][9][0]=0.130427; p3_pc3_m_sdz[0][0][9][0]=-0.0356906; p4_pc3_m_sdz[0][0][9][0]=0.00293127; 
    p0_pc3_s_sdz[0][0][9][0]=0.759997; p1_pc3_s_sdz[0][0][9][0]=-0.163351; p2_pc3_s_sdz[0][0][9][0]=0.219722; p3_pc3_s_sdz[0][0][9][0]=-0.0771996; p4_pc3_s_sdz[0][0][9][0]=0.00756031; 
    p0_pc3_m_sdz[1][0][9][0]=-7.42546; p1_pc3_m_sdz[1][0][9][0]=32.5234; p2_pc3_m_sdz[1][0][9][0]=-10.6011; p3_pc3_m_sdz[1][0][9][0]=-88.7792; p4_pc3_m_sdz[1][0][9][0]=85.0203; 
    p0_pc3_s_sdz[1][0][9][0]=0.500498; p1_pc3_s_sdz[1][0][9][0]=0.339698; p2_pc3_s_sdz[1][0][9][0]=0.0734676; p3_pc3_s_sdz[1][0][9][0]=-0.140662; p4_pc3_s_sdz[1][0][9][0]=0.0624986; 
    p0_pc3_m_sdz[0][1][0][0]=-0.012888; p1_pc3_m_sdz[0][1][0][0]=-0.0670482; p2_pc3_m_sdz[0][1][0][0]=0.0849712; p3_pc3_m_sdz[0][1][0][0]=-0.0397558; p4_pc3_m_sdz[0][1][0][0]=0.00724973; 
    p0_pc3_s_sdz[0][1][0][0]=0.549745; p1_pc3_s_sdz[0][1][0][0]=0.227078; p2_pc3_s_sdz[0][1][0][0]=-0.0300942; p3_pc3_s_sdz[0][1][0][0]=-0.00947235; p4_pc3_s_sdz[0][1][0][0]=0.00122707; 
    p0_pc3_m_sdz[1][1][0][0]=-0.726746; p1_pc3_m_sdz[1][1][0][0]=0.180209; p2_pc3_m_sdz[1][1][0][0]=1.18413; p3_pc3_m_sdz[1][1][0][0]=1.27446; p4_pc3_m_sdz[1][1][0][0]=-1.65375; 
    p0_pc3_s_sdz[1][1][0][0]=-0.876529; p1_pc3_s_sdz[1][1][0][0]=6.15692; p2_pc3_s_sdz[1][1][0][0]=-2.42412; p3_pc3_s_sdz[1][1][0][0]=-16.8618; p4_pc3_s_sdz[1][1][0][0]=17.8977; 
    p0_pc3_m_sdz[0][1][1][0]=-0.0276601; p1_pc3_m_sdz[0][1][1][0]=0.0116668; p2_pc3_m_sdz[0][1][1][0]=-0.00397107; p3_pc3_m_sdz[0][1][1][0]=-0.000250515; p4_pc3_m_sdz[0][1][1][0]=0.000587648; 
    p0_pc3_s_sdz[0][1][1][0]=0.456339; p1_pc3_s_sdz[0][1][1][0]=0.471449; p2_pc3_s_sdz[0][1][1][0]=-0.258091; p3_pc3_s_sdz[0][1][1][0]=0.0728409; p4_pc3_s_sdz[0][1][1][0]=-0.008469; 
    p0_pc3_m_sdz[1][1][1][0]=2.6391; p1_pc3_m_sdz[1][1][1][0]=-11.6784; p2_pc3_m_sdz[1][1][1][0]=3.46894; p3_pc3_m_sdz[1][1][1][0]=31.887; p4_pc3_m_sdz[1][1][1][0]=-29.752; 
    p0_pc3_s_sdz[1][1][1][0]=-3.7951; p1_pc3_s_sdz[1][1][1][0]=17.0078; p2_pc3_s_sdz[1][1][1][0]=-4.80291; p3_pc3_s_sdz[1][1][1][0]=-44.0293; p4_pc3_s_sdz[1][1][1][0]=42.3203; 
    p0_pc3_m_sdz[0][1][2][0]=0.0287227; p1_pc3_m_sdz[0][1][2][0]=-0.118109; p2_pc3_m_sdz[0][1][2][0]=0.113862; p3_pc3_m_sdz[0][1][2][0]=-0.046383; p4_pc3_m_sdz[0][1][2][0]=0.00694852; 
    p0_pc3_s_sdz[0][1][2][0]=0.475534; p1_pc3_s_sdz[0][1][2][0]=0.392339; p2_pc3_s_sdz[0][1][2][0]=-0.159165; p3_pc3_s_sdz[0][1][2][0]=0.0248623; p4_pc3_s_sdz[0][1][2][0]=-0.00047683; 
    p0_pc3_m_sdz[1][1][2][0]=1.3691; p1_pc3_m_sdz[1][1][2][0]=-6.94429; p2_pc3_m_sdz[1][1][2][0]=3.34715; p3_pc3_m_sdz[1][1][2][0]=20.1033; p4_pc3_m_sdz[1][1][2][0]=-21.3671; 
    p0_pc3_s_sdz[1][1][2][0]=-2.75255; p1_pc3_s_sdz[1][1][2][0]=12.8377; p2_pc3_s_sdz[1][1][2][0]=-3.51057; p3_pc3_s_sdz[1][1][2][0]=-31.9585; p4_pc3_s_sdz[1][1][2][0]=29.747; 
    p0_pc3_m_sdz[0][1][3][0]=0.0661586; p1_pc3_m_sdz[0][1][3][0]=-0.19961; p2_pc3_m_sdz[0][1][3][0]=0.205023; p3_pc3_m_sdz[0][1][3][0]=-0.0867337; p4_pc3_m_sdz[0][1][3][0]=0.0129981; 
    p0_pc3_s_sdz[0][1][3][0]=0.391541; p1_pc3_s_sdz[0][1][3][0]=0.595913; p2_pc3_s_sdz[0][1][3][0]=-0.322473; p3_pc3_s_sdz[0][1][3][0]=0.080116; p4_pc3_s_sdz[0][1][3][0]=-0.00733328; 
    p0_pc3_m_sdz[1][1][3][0]=1.19261; p1_pc3_m_sdz[1][1][3][0]=-5.09866; p2_pc3_m_sdz[1][1][3][0]=1.95519; p3_pc3_m_sdz[1][1][3][0]=12.3231; p4_pc3_m_sdz[1][1][3][0]=-11.7355; 
    p0_pc3_s_sdz[1][1][3][0]=0.400196; p1_pc3_s_sdz[1][1][3][0]=0.484219; p2_pc3_s_sdz[1][1][3][0]=0.398727; p3_pc3_s_sdz[1][1][3][0]=-0.0549553; p4_pc3_s_sdz[1][1][3][0]=-1.20988; 
    p0_pc3_m_sdz[0][1][4][0]=0.140462; p1_pc3_m_sdz[0][1][4][0]=-0.285744; p2_pc3_m_sdz[0][1][4][0]=0.22958; p3_pc3_m_sdz[0][1][4][0]=-0.0796997; p4_pc3_m_sdz[0][1][4][0]=0.0100837; 
    p0_pc3_s_sdz[0][1][4][0]=0.304378; p1_pc3_s_sdz[0][1][4][0]=0.756176; p2_pc3_s_sdz[0][1][4][0]=-0.441519; p3_pc3_s_sdz[0][1][4][0]=0.116531; p4_pc3_s_sdz[0][1][4][0]=-0.0111207; 
    p0_pc3_m_sdz[1][1][4][0]=0.697618; p1_pc3_m_sdz[1][1][4][0]=-2.95759; p2_pc3_m_sdz[1][1][4][0]=1.07474; p3_pc3_m_sdz[1][1][4][0]=9.63237; p4_pc3_m_sdz[1][1][4][0]=-10.4037; 
    p0_pc3_s_sdz[1][1][4][0]=0.50426; p1_pc3_s_sdz[1][1][4][0]=0.315935; p2_pc3_s_sdz[1][1][4][0]=0.0263723; p3_pc3_s_sdz[1][1][4][0]=-0.197659; p4_pc3_s_sdz[1][1][4][0]=0.0347115; 
    p0_pc3_m_sdz[0][1][5][0]=0.128251; p1_pc3_m_sdz[0][1][5][0]=-0.241795; p2_pc3_m_sdz[0][1][5][0]=0.199416; p3_pc3_m_sdz[0][1][5][0]=-0.0732761; p4_pc3_m_sdz[0][1][5][0]=0.00977776; 
    p0_pc3_s_sdz[0][1][5][0]=0.50375; p1_pc3_s_sdz[0][1][5][0]=0.352066; p2_pc3_s_sdz[0][1][5][0]=-0.156579; p3_pc3_s_sdz[0][1][5][0]=0.0284677; p4_pc3_s_sdz[0][1][5][0]=-0.000827638; 
    p0_pc3_m_sdz[1][1][5][0]=0.357809; p1_pc3_m_sdz[1][1][5][0]=-0.240061; p2_pc3_m_sdz[1][1][5][0]=-0.800058; p3_pc3_m_sdz[1][1][5][0]=-0.597613; p4_pc3_m_sdz[1][1][5][0]=1.83799; 
    p0_pc3_s_sdz[1][1][5][0]=6.80858; p1_pc3_s_sdz[1][1][5][0]=-22.9879; p2_pc3_s_sdz[1][1][5][0]=3.56855; p3_pc3_s_sdz[1][1][5][0]=63.1865; p4_pc3_s_sdz[1][1][5][0]=-56.2177; 
    p0_pc3_m_sdz[0][1][6][0]=-0.0114322; p1_pc3_m_sdz[0][1][6][0]=0.0156337; p2_pc3_m_sdz[0][1][6][0]=-0.00303756; p3_pc3_m_sdz[0][1][6][0]=-0.000958683; p4_pc3_m_sdz[0][1][6][0]=0.000186637; 
    p0_pc3_s_sdz[0][1][6][0]=0.477361; p1_pc3_s_sdz[0][1][6][0]=0.445885; p2_pc3_s_sdz[0][1][6][0]=-0.228696; p3_pc3_s_sdz[0][1][6][0]=0.051353; p4_pc3_s_sdz[0][1][6][0]=-0.00389897; 
    p0_pc3_m_sdz[1][1][6][0]=-0.132817; p1_pc3_m_sdz[1][1][6][0]=0.274977; p2_pc3_m_sdz[1][1][6][0]=0.580562; p3_pc3_m_sdz[1][1][6][0]=0.199065; p4_pc3_m_sdz[1][1][6][0]=-2.03541; 
    p0_pc3_s_sdz[1][1][6][0]=-5.18945; p1_pc3_s_sdz[1][1][6][0]=23.441; p2_pc3_s_sdz[1][1][6][0]=-7.699; p3_pc3_s_sdz[1][1][6][0]=-61.4215; p4_pc3_s_sdz[1][1][6][0]=60.3944; 
    p0_pc3_m_sdz[0][1][7][0]=0.0730323; p1_pc3_m_sdz[0][1][7][0]=-0.1853; p2_pc3_m_sdz[0][1][7][0]=0.17081; p3_pc3_m_sdz[0][1][7][0]=-0.0663007; p4_pc3_m_sdz[0][1][7][0]=0.00929449; 
    p0_pc3_s_sdz[0][1][7][0]=0.582589; p1_pc3_s_sdz[0][1][7][0]=0.224001; p2_pc3_s_sdz[0][1][7][0]=-0.0856703; p3_pc3_s_sdz[0][1][7][0]=0.0217239; p4_pc3_s_sdz[0][1][7][0]=-0.002999; 
    p0_pc3_m_sdz[1][1][7][0]=2.61836; p1_pc3_m_sdz[1][1][7][0]=-8.67902; p2_pc3_m_sdz[1][1][7][0]=1.417; p3_pc3_m_sdz[1][1][7][0]=21.8138; p4_pc3_m_sdz[1][1][7][0]=-19.9454; 
    p0_pc3_s_sdz[1][1][7][0]=4.35989; p1_pc3_s_sdz[1][1][7][0]=-13.604; p2_pc3_s_sdz[1][1][7][0]=2.7153; p3_pc3_s_sdz[1][1][7][0]=35.098; p4_pc3_s_sdz[1][1][7][0]=-31.3553; 
    p0_pc3_m_sdz[0][1][8][0]=0.125439; p1_pc3_m_sdz[0][1][8][0]=-0.285558; p2_pc3_m_sdz[0][1][8][0]=0.202398; p3_pc3_m_sdz[0][1][8][0]=-0.0554969; p4_pc3_m_sdz[0][1][8][0]=0.0048248; 
    p0_pc3_s_sdz[0][1][8][0]=0.671192; p1_pc3_s_sdz[0][1][8][0]=0.0420974; p2_pc3_s_sdz[0][1][8][0]=0.0467228; p3_pc3_s_sdz[0][1][8][0]=-0.0209436; p4_pc3_s_sdz[0][1][8][0]=0.0021702; 
    p0_pc3_m_sdz[1][1][8][0]=0.660213; p1_pc3_m_sdz[1][1][8][0]=-0.302505; p2_pc3_m_sdz[1][1][8][0]=-1.2685; p3_pc3_m_sdz[1][1][8][0]=-1.13457; p4_pc3_m_sdz[1][1][8][0]=2.34694; 
    p0_pc3_s_sdz[1][1][8][0]=5.82905; p1_pc3_s_sdz[1][1][8][0]=-21.6263; p2_pc3_s_sdz[1][1][8][0]=8.26616; p3_pc3_s_sdz[1][1][8][0]=58.4487; p4_pc3_s_sdz[1][1][8][0]=-59.6921; 
    p0_pc3_m_sdz[0][1][9][0]=0.0402272; p1_pc3_m_sdz[0][1][9][0]=-0.0718553; p2_pc3_m_sdz[0][1][9][0]=0.0232358; p3_pc3_m_sdz[0][1][9][0]=0.00386289; p4_pc3_m_sdz[0][1][9][0]=-0.0021608; 
    p0_pc3_s_sdz[0][1][9][0]=0.733853; p1_pc3_s_sdz[0][1][9][0]=-0.0977449; p2_pc3_s_sdz[0][1][9][0]=0.1569; p3_pc3_s_sdz[0][1][9][0]=-0.0542671; p4_pc3_s_sdz[0][1][9][0]=0.00495096; 
    p0_pc3_m_sdz[1][1][9][0]=0.019296; p1_pc3_m_sdz[1][1][9][0]=3.15124; p2_pc3_m_sdz[1][1][9][0]=-2.46944; p3_pc3_m_sdz[1][1][9][0]=-10.4041; p4_pc3_m_sdz[1][1][9][0]=10.4805; 
    p0_pc3_s_sdz[1][1][9][0]=-0.861773; p1_pc3_s_sdz[1][1][9][0]=6.60422; p2_pc3_s_sdz[1][1][9][0]=-3.36947; p3_pc3_s_sdz[1][1][9][0]=-16.0804; p4_pc3_s_sdz[1][1][9][0]=17.4888; 

	
    //PC3 DZ POS 
	
    p0_pc3_m_sdz[0][0][0][1]=-0.00531901; p1_pc3_m_sdz[0][0][0][1]=-0.11359; p2_pc3_m_sdz[0][0][0][1]=0.111431; p3_pc3_m_sdz[0][0][0][1]=-0.0411898; p4_pc3_m_sdz[0][0][0][1]=0.00624397; 
    p0_pc3_s_sdz[0][0][0][1]=0.710876; p1_pc3_s_sdz[0][0][0][1]=-0.0171809; p2_pc3_s_sdz[0][0][0][1]=0.088871; p3_pc3_s_sdz[0][0][0][1]=-0.0345833; p4_pc3_s_sdz[0][0][0][1]=0.00319605; 
    p0_pc3_m_sdz[1][0][0][1]=1.24521; p1_pc3_m_sdz[1][0][0][1]=-8.72301; p2_pc3_m_sdz[1][0][0][1]=4.54578; p3_pc3_m_sdz[1][0][0][1]=27.2751; p4_pc3_m_sdz[1][0][0][1]=-28.4469; 
    p0_pc3_s_sdz[1][0][0][1]=2.81492; p1_pc3_s_sdz[1][0][0][1]=-7.43529; p2_pc3_s_sdz[1][0][0][1]=1.51127; p3_pc3_s_sdz[1][0][0][1]=18.2493; p4_pc3_s_sdz[1][0][0][1]=-16.3831; 
    p0_pc3_m_sdz[0][0][1][1]=0.201596; p1_pc3_m_sdz[0][0][1][1]=-0.553751; p2_pc3_m_sdz[0][0][1][1]=0.461429; p3_pc3_m_sdz[0][0][1][1]=-0.163285; p4_pc3_m_sdz[0][0][1][1]=0.0214552; 
    p0_pc3_s_sdz[0][0][1][1]=0.592305; p1_pc3_s_sdz[0][0][1][1]=0.267638; p2_pc3_s_sdz[0][0][1][1]=-0.121453; p3_pc3_s_sdz[0][0][1][1]=0.0162315; p4_pc3_s_sdz[0][0][1][1]=0.00126722; 
    p0_pc3_m_sdz[1][0][1][1]=-1.20504; p1_pc3_m_sdz[1][0][1][1]=3.21293; p2_pc3_m_sdz[1][0][1][1]=-0.30316; p3_pc3_m_sdz[1][0][1][1]=-7.85885; p4_pc3_m_sdz[1][0][1][1]=7.75317; 
    p0_pc3_s_sdz[1][0][1][1]=0.913625; p1_pc3_s_sdz[1][0][1][1]=-0.00408505; p2_pc3_s_sdz[1][0][1][1]=-0.916814; p3_pc3_s_sdz[1][0][1][1]=-0.810511; p4_pc3_s_sdz[1][0][1][1]=2.3659; 
    p0_pc3_m_sdz[0][0][2][1]=0.0315229; p1_pc3_m_sdz[0][0][2][1]=-0.0773461; p2_pc3_m_sdz[0][0][2][1]=0.0239605; p3_pc3_m_sdz[0][0][2][1]=0.00570692; p4_pc3_m_sdz[0][0][2][1]=-0.00224382; 
    p0_pc3_s_sdz[0][0][2][1]=0.613566; p1_pc3_s_sdz[0][0][2][1]=0.235984; p2_pc3_s_sdz[0][0][2][1]=-0.11766; p3_pc3_s_sdz[0][0][2][1]=0.0247878; p4_pc3_s_sdz[0][0][2][1]=-0.00122322; 
    p0_pc3_m_sdz[1][0][2][1]=3.46898; p1_pc3_m_sdz[1][0][2][1]=-15.0187; p2_pc3_m_sdz[1][0][2][1]=5.01174; p3_pc3_m_sdz[1][0][2][1]=41.2532; p4_pc3_m_sdz[1][0][2][1]=-40.2396; 
    p0_pc3_s_sdz[1][0][2][1]=0.728685; p1_pc3_s_sdz[1][0][2][1]=0.199401; p2_pc3_s_sdz[1][0][2][1]=-0.404557; p3_pc3_s_sdz[1][0][2][1]=-0.523131; p4_pc3_s_sdz[1][0][2][1]=0.997749; 
    p0_pc3_m_sdz[0][0][3][1]=-0.0316535; p1_pc3_m_sdz[0][0][3][1]=0.138949; p2_pc3_m_sdz[0][0][3][1]=-0.171551; p3_pc3_m_sdz[0][0][3][1]=0.0787778; p4_pc3_m_sdz[0][0][3][1]=-0.011796; 
    p0_pc3_s_sdz[0][0][3][1]=0.602856; p1_pc3_s_sdz[0][0][3][1]=0.310964; p2_pc3_s_sdz[0][0][3][1]=-0.224423; p3_pc3_s_sdz[0][0][3][1]=0.0740733; p4_pc3_s_sdz[0][0][3][1]=-0.00842148; 
    p0_pc3_m_sdz[1][0][3][1]=-0.242292; p1_pc3_m_sdz[1][0][3][1]=0.111504; p2_pc3_m_sdz[1][0][3][1]=0.480553; p3_pc3_m_sdz[1][0][3][1]=0.445834; p4_pc3_m_sdz[1][0][3][1]=-0.872167; 
    p0_pc3_s_sdz[1][0][3][1]=2.22576; p1_pc3_s_sdz[1][0][3][1]=-6.29529; p2_pc3_s_sdz[1][0][3][1]=2.08351; p3_pc3_s_sdz[1][0][3][1]=17.2609; p4_pc3_s_sdz[1][0][3][1]=-16.8445; 
    p0_pc3_m_sdz[0][0][4][1]=0.0377786; p1_pc3_m_sdz[0][0][4][1]=-0.0821897; p2_pc3_m_sdz[0][0][4][1]=0.0808782; p3_pc3_m_sdz[0][0][4][1]=-0.0333965; p4_pc3_m_sdz[0][0][4][1]=0.00501038; 
    p0_pc3_s_sdz[0][0][4][1]=0.55231; p1_pc3_s_sdz[0][0][4][1]=0.459512; p2_pc3_s_sdz[0][0][4][1]=-0.370619; p3_pc3_s_sdz[0][0][4][1]=0.129572; p4_pc3_s_sdz[0][0][4][1]=-0.0149926; 
    p0_pc3_m_sdz[1][0][4][1]=-0.541446; p1_pc3_m_sdz[1][0][4][1]=2.5558; p2_pc3_m_sdz[1][0][4][1]=-1.1501; p3_pc3_m_sdz[1][0][4][1]=-8.00879; p4_pc3_m_sdz[1][0][4][1]=8.85047; 
    p0_pc3_s_sdz[1][0][4][1]=-3.07518; p1_pc3_s_sdz[1][0][4][1]=14.783; p2_pc3_s_sdz[1][0][4][1]=-4.33846; p3_pc3_s_sdz[1][0][4][1]=-37.5621; p4_pc3_s_sdz[1][0][4][1]=35.5174; 
    p0_pc3_m_sdz[0][0][5][1]=0.0426539; p1_pc3_m_sdz[0][0][5][1]=-0.108989; p2_pc3_m_sdz[0][0][5][1]=0.109282; p3_pc3_m_sdz[0][0][5][1]=-0.041301; p4_pc3_m_sdz[0][0][5][1]=0.00538373; 
    p0_pc3_s_sdz[0][0][5][1]=0.605544; p1_pc3_s_sdz[0][0][5][1]=0.339359; p2_pc3_s_sdz[0][0][5][1]=-0.288736; p3_pc3_s_sdz[0][0][5][1]=0.110226; p4_pc3_s_sdz[0][0][5][1]=-0.0144065; 
    p0_pc3_m_sdz[1][0][5][1]=0.603967; p1_pc3_m_sdz[1][0][5][1]=-2.58931; p2_pc3_m_sdz[1][0][5][1]=1.49763; p3_pc3_m_sdz[1][0][5][1]=6.76135; p4_pc3_m_sdz[1][0][5][1]=-7.95876; 
    p0_pc3_s_sdz[1][0][5][1]=-1.12001; p1_pc3_s_sdz[1][0][5][1]=7.73754; p2_pc3_s_sdz[1][0][5][1]=-2.88567; p3_pc3_s_sdz[1][0][5][1]=-21.1491; p4_pc3_s_sdz[1][0][5][1]=21.5108; 
    p0_pc3_m_sdz[0][0][6][1]=-0.00358831; p1_pc3_m_sdz[0][0][6][1]=-0.0269475; p2_pc3_m_sdz[0][0][6][1]=0.0604142; p3_pc3_m_sdz[0][0][6][1]=-0.0320099; p4_pc3_m_sdz[0][0][6][1]=0.00514137; 
    p0_pc3_s_sdz[0][0][6][1]=0.564007; p1_pc3_s_sdz[0][0][6][1]=0.428773; p2_pc3_s_sdz[0][0][6][1]=-0.324021; p3_pc3_s_sdz[0][0][6][1]=0.110753; p4_pc3_s_sdz[0][0][6][1]=-0.0134087; 
    p0_pc3_m_sdz[1][0][6][1]=-0.823306; p1_pc3_m_sdz[1][0][6][1]=4.46064; p2_pc3_m_sdz[1][0][6][1]=-2.31698; p3_pc3_m_sdz[1][0][6][1]=-13.0076; p4_pc3_m_sdz[1][0][6][1]=13.9309; 
    p0_pc3_s_sdz[1][0][6][1]=-1.76478; p1_pc3_s_sdz[1][0][6][1]=9.78768; p2_pc3_s_sdz[1][0][6][1]=-3.26641; p3_pc3_s_sdz[1][0][6][1]=-24.2454; p4_pc3_s_sdz[1][0][6][1]=23.386; 
    p0_pc3_m_sdz[0][0][7][1]=-0.0150011; p1_pc3_m_sdz[0][0][7][1]=-0.00897349; p2_pc3_m_sdz[0][0][7][1]=0.0505218; p3_pc3_m_sdz[0][0][7][1]=-0.0305695; p4_pc3_m_sdz[0][0][7][1]=0.00518377; 
    p0_pc3_s_sdz[0][0][7][1]=0.657202; p1_pc3_s_sdz[0][0][7][1]=0.156614; p2_pc3_s_sdz[0][0][7][1]=-0.0781884; p3_pc3_s_sdz[0][0][7][1]=0.0241368; p4_pc3_s_sdz[0][0][7][1]=-0.00295973; 
    p0_pc3_m_sdz[1][0][7][1]=-0.542818; p1_pc3_m_sdz[1][0][7][1]=4.10692; p2_pc3_m_sdz[1][0][7][1]=-3.13917; p3_pc3_m_sdz[1][0][7][1]=-11.6785; p4_pc3_m_sdz[1][0][7][1]=13.623; 
    p0_pc3_s_sdz[1][0][7][1]=2.12992; p1_pc3_s_sdz[1][0][7][1]=-5.55925; p2_pc3_s_sdz[1][0][7][1]=1.79334; p3_pc3_s_sdz[1][0][7][1]=14.4913; p4_pc3_s_sdz[1][0][7][1]=-14.2108; 
    p0_pc3_m_sdz[0][0][8][1]=0.0854863; p1_pc3_m_sdz[0][0][8][1]=-0.21318; p2_pc3_m_sdz[0][0][8][1]=0.188448; p3_pc3_m_sdz[0][0][8][1]=-0.0673681; p4_pc3_m_sdz[0][0][8][1]=0.00836013; 
    p0_pc3_s_sdz[0][0][8][1]=0.625507; p1_pc3_s_sdz[0][0][8][1]=0.261035; p2_pc3_s_sdz[0][0][8][1]=-0.198899; p3_pc3_s_sdz[0][0][8][1]=0.0764566; p4_pc3_s_sdz[0][0][8][1]=-0.0106672; 
    p0_pc3_m_sdz[1][0][8][1]=-0.629997; p1_pc3_m_sdz[1][0][8][1]=5.18278; p2_pc3_m_sdz[1][0][8][1]=-3.02096; p3_pc3_m_sdz[1][0][8][1]=-17.1689; p4_pc3_m_sdz[1][0][8][1]=18.4441; 
    p0_pc3_s_sdz[1][0][8][1]=3.70797; p1_pc3_s_sdz[1][0][8][1]=-13.402; p2_pc3_s_sdz[1][0][8][1]=5.53931; p3_pc3_s_sdz[1][0][8][1]=39.2451; p4_pc3_s_sdz[1][0][8][1]=-41.565; 
    p0_pc3_m_sdz[0][0][9][1]=0.0339263; p1_pc3_m_sdz[0][0][9][1]=-0.066078; p2_pc3_m_sdz[0][0][9][1]=0.04953; p3_pc3_m_sdz[0][0][9][1]=-0.0167149; p4_pc3_m_sdz[0][0][9][1]=0.00163502; 
    p0_pc3_s_sdz[0][0][9][1]=0.633234; p1_pc3_s_sdz[0][0][9][1]=0.233809; p2_pc3_s_sdz[0][0][9][1]=-0.146257; p3_pc3_s_sdz[0][0][9][1]=0.0495233; p4_pc3_s_sdz[0][0][9][1]=-0.00708171; 
    p0_pc3_m_sdz[1][0][9][1]=2.5402; p1_pc3_m_sdz[1][0][9][1]=-6.43288; p2_pc3_m_sdz[1][0][9][1]=-0.0169939; p3_pc3_m_sdz[1][0][9][1]=15.0455; p4_pc3_m_sdz[1][0][9][1]=-13.7137; 
    p0_pc3_s_sdz[1][0][9][1]=-5.54054; p1_pc3_s_sdz[1][0][9][1]=25.2143; p2_pc3_s_sdz[1][0][9][1]=-8.57194; p3_pc3_s_sdz[1][0][9][1]=-65.9238; p4_pc3_s_sdz[1][0][9][1]=64.9902; 
    p0_pc3_m_sdz[0][1][0][1]=-0.124429; p1_pc3_m_sdz[0][1][0][1]=0.118151; p2_pc3_m_sdz[0][1][0][1]=-0.0517708; p3_pc3_m_sdz[0][1][0][1]=0.0109668; p4_pc3_m_sdz[0][1][0][1]=-0.000497131; 
    p0_pc3_s_sdz[0][1][0][1]=0.220008; p1_pc3_s_sdz[0][1][0][1]=1.00046; p2_pc3_s_sdz[0][1][0][1]=-0.644762; p3_pc3_s_sdz[0][1][0][1]=0.185409; p4_pc3_s_sdz[0][1][0][1]=-0.0200499; 
    p0_pc3_m_sdz[1][1][0][1]=-1.75664; p1_pc3_m_sdz[1][1][0][1]=4.00585; p2_pc3_m_sdz[1][1][0][1]=-0.386933; p3_pc3_m_sdz[1][1][0][1]=-8.52601; p4_pc3_m_sdz[1][1][0][1]=8.78753; 
    p0_pc3_s_sdz[1][1][0][1]=-2.96355; p1_pc3_s_sdz[1][1][0][1]=13.7428; p2_pc3_s_sdz[1][1][0][1]=-3.22156; p3_pc3_s_sdz[1][1][0][1]=-37.2652; p4_pc3_s_sdz[1][1][0][1]=35.2676; 
    p0_pc3_m_sdz[0][1][1][1]=-0.0435052; p1_pc3_m_sdz[0][1][1][1]=0.0293046; p2_pc3_m_sdz[0][1][1][1]=-0.0270224; p3_pc3_m_sdz[0][1][1][1]=0.015064; p4_pc3_m_sdz[0][1][1][1]=-0.00263971; 
    p0_pc3_s_sdz[0][1][1][1]=0.176949; p1_pc3_s_sdz[0][1][1][1]=1.13329; p2_pc3_s_sdz[0][1][1][1]=-0.761542; p3_pc3_s_sdz[0][1][1][1]=0.21937; p4_pc3_s_sdz[0][1][1][1]=-0.02216; 
    p0_pc3_m_sdz[1][1][1][1]=0.330729; p1_pc3_m_sdz[1][1][1][1]=-3.06703; p2_pc3_m_sdz[1][1][1][1]=1.52251; p3_pc3_m_sdz[1][1][1][1]=9.73383; p4_pc3_m_sdz[1][1][1][1]=-9.58076; 
    p0_pc3_s_sdz[1][1][1][1]=1.90041; p1_pc3_s_sdz[1][1][1][1]=-5.6729; p2_pc3_s_sdz[1][1][1][1]=2.06529; p3_pc3_s_sdz[1][1][1][1]=17.3021; p4_pc3_s_sdz[1][1][1][1]=-17.7196; 
    p0_pc3_m_sdz[0][1][2][1]=0.0938448; p1_pc3_m_sdz[0][1][2][1]=-0.267818; p2_pc3_m_sdz[0][1][2][1]=0.22572; p3_pc3_m_sdz[0][1][2][1]=-0.0817891; p4_pc3_m_sdz[0][1][2][1]=0.0110693; 
    p0_pc3_s_sdz[0][1][2][1]=0.35671; p1_pc3_s_sdz[0][1][2][1]=0.789384; p2_pc3_s_sdz[0][1][2][1]=-0.530302; p3_pc3_s_sdz[0][1][2][1]=0.155614; p4_pc3_s_sdz[0][1][2][1]=-0.0160674; 
    p0_pc3_m_sdz[1][1][2][1]=-1.10224; p1_pc3_m_sdz[1][1][2][1]=3.74345; p2_pc3_m_sdz[1][1][2][1]=-0.98156; p3_pc3_m_sdz[1][1][2][1]=-9.43965; p4_pc3_m_sdz[1][1][2][1]=9.42196; 
    p0_pc3_s_sdz[1][1][2][1]=1.32106; p1_pc3_s_sdz[1][1][2][1]=-2.53572; p2_pc3_s_sdz[1][1][2][1]=0.193879; p3_pc3_s_sdz[1][1][2][1]=7.21107; p4_pc3_s_sdz[1][1][2][1]=-5.80547; 
    p0_pc3_m_sdz[0][1][3][1]=0.0796376; p1_pc3_m_sdz[0][1][3][1]=-0.199386; p2_pc3_m_sdz[0][1][3][1]=0.164947; p3_pc3_m_sdz[0][1][3][1]=-0.0573908; p4_pc3_m_sdz[0][1][3][1]=0.00727715; 
    p0_pc3_s_sdz[0][1][3][1]=0.463456; p1_pc3_s_sdz[0][1][3][1]=0.5858; p2_pc3_s_sdz[0][1][3][1]=-0.415539; p3_pc3_s_sdz[0][1][3][1]=0.135326; p4_pc3_s_sdz[0][1][3][1]=-0.0161039; 
    p0_pc3_m_sdz[1][1][3][1]=-0.864746; p1_pc3_m_sdz[1][1][3][1]=3.3825; p2_pc3_m_sdz[1][1][3][1]=-0.986745; p3_pc3_m_sdz[1][1][3][1]=-9.78175; p4_pc3_m_sdz[1][1][3][1]=9.90252; 
    p0_pc3_s_sdz[1][1][3][1]=-1.26904; p1_pc3_s_sdz[1][1][3][1]=8.07531; p2_pc3_s_sdz[1][1][3][1]=-3.1806; p3_pc3_s_sdz[1][1][3][1]=-21.6009; p4_pc3_s_sdz[1][1][3][1]=22.4102; 
    p0_pc3_m_sdz[0][1][4][1]=0.0460902; p1_pc3_m_sdz[0][1][4][1]=-0.0615878; p2_pc3_m_sdz[0][1][4][1]=0.033127; p3_pc3_m_sdz[0][1][4][1]=-0.00853857; p4_pc3_m_sdz[0][1][4][1]=0.00100252; 
    p0_pc3_s_sdz[0][1][4][1]=0.2379; p1_pc3_s_sdz[0][1][4][1]=1.02777; p2_pc3_s_sdz[0][1][4][1]=-0.71244; p3_pc3_s_sdz[0][1][4][1]=0.210772; p4_pc3_s_sdz[0][1][4][1]=-0.0216885; 
    p0_pc3_m_sdz[1][1][4][1]=1.93019; p1_pc3_m_sdz[1][1][4][1]=-8.12183; p2_pc3_m_sdz[1][1][4][1]=2.82191; p3_pc3_m_sdz[1][1][4][1]=22.9121; p4_pc3_m_sdz[1][1][4][1]=-23.2183; 
    p0_pc3_s_sdz[1][1][4][1]=0.261451; p1_pc3_s_sdz[1][1][4][1]=0.568648; p2_pc3_s_sdz[1][1][4][1]=0.713692; p3_pc3_s_sdz[1][1][4][1]=0.216226; p4_pc3_s_sdz[1][1][4][1]=-1.81285; 
    p0_pc3_m_sdz[0][1][5][1]=0.151851; p1_pc3_m_sdz[0][1][5][1]=-0.328433; p2_pc3_m_sdz[0][1][5][1]=0.278323; p3_pc3_m_sdz[0][1][5][1]=-0.0963288; p4_pc3_m_sdz[0][1][5][1]=0.0115138; 
    p0_pc3_s_sdz[0][1][5][1]=0.235106; p1_pc3_s_sdz[0][1][5][1]=1.11349; p2_pc3_s_sdz[0][1][5][1]=-0.851788; p3_pc3_s_sdz[0][1][5][1]=0.278029; p4_pc3_s_sdz[0][1][5][1]=-0.031695; 
    p0_pc3_m_sdz[1][1][5][1]=0.280023; p1_pc3_m_sdz[1][1][5][1]=-0.246362; p2_pc3_m_sdz[1][1][5][1]=-0.715987; p3_pc3_m_sdz[1][1][5][1]=-0.438117; p4_pc3_m_sdz[1][1][5][1]=1.94849; 
    p0_pc3_s_sdz[1][1][5][1]=2.23233; p1_pc3_s_sdz[1][1][5][1]=-6.703; p2_pc3_s_sdz[1][1][5][1]=1.76353; p3_pc3_s_sdz[1][1][5][1]=20.5108; p4_pc3_s_sdz[1][1][5][1]=-20.1418; 
    p0_pc3_m_sdz[0][1][6][1]=0.0349799; p1_pc3_m_sdz[0][1][6][1]=-0.0772402; p2_pc3_m_sdz[0][1][6][1]=0.0662499; p3_pc3_m_sdz[0][1][6][1]=-0.0228528; p4_pc3_m_sdz[0][1][6][1]=0.00267051; 
    p0_pc3_s_sdz[0][1][6][1]=0.490996; p1_pc3_s_sdz[0][1][6][1]=0.471239; p2_pc3_s_sdz[0][1][6][1]=-0.294747; p3_pc3_s_sdz[0][1][6][1]=0.0883369; p4_pc3_s_sdz[0][1][6][1]=-0.0102208; 
    p0_pc3_m_sdz[1][1][6][1]=1.34073; p1_pc3_m_sdz[1][1][6][1]=-4.4911; p2_pc3_m_sdz[1][1][6][1]=0.696261; p3_pc3_m_sdz[1][1][6][1]=11.8547; p4_pc3_m_sdz[1][1][6][1]=-11.0089; 
    p0_pc3_s_sdz[1][1][6][1]=2.30074; p1_pc3_s_sdz[1][1][6][1]=-6.28704; p2_pc3_s_sdz[1][1][6][1]=1.29704; p3_pc3_s_sdz[1][1][6][1]=17.0834; p4_pc3_s_sdz[1][1][6][1]=-15.3857; 
    p0_pc3_m_sdz[0][1][7][1]=0.149229; p1_pc3_m_sdz[0][1][7][1]=-0.308839; p2_pc3_m_sdz[0][1][7][1]=0.228417; p3_pc3_m_sdz[0][1][7][1]=-0.0688095; p4_pc3_m_sdz[0][1][7][1]=0.00715617; 
    p0_pc3_s_sdz[0][1][7][1]=0.347693; p1_pc3_s_sdz[0][1][7][1]=0.835137; p2_pc3_s_sdz[0][1][7][1]=-0.587934; p3_pc3_s_sdz[0][1][7][1]=0.182503; p4_pc3_s_sdz[0][1][7][1]=-0.0203838; 
    p0_pc3_m_sdz[1][1][7][1]=-3.38709; p1_pc3_m_sdz[1][1][7][1]=15.011; p2_pc3_m_sdz[1][1][7][1]=-5.10214; p3_pc3_m_sdz[1][1][7][1]=-42.0882; p4_pc3_m_sdz[1][1][7][1]=41.3146; 
    p0_pc3_s_sdz[1][1][7][1]=1.36183; p1_pc3_s_sdz[1][1][7][1]=-2.88278; p2_pc3_s_sdz[1][1][7][1]=0.50956; p3_pc3_s_sdz[1][1][7][1]=9.71687; p4_pc3_s_sdz[1][1][7][1]=-9.39656; 
    p0_pc3_m_sdz[0][1][8][1]=0.0119767; p1_pc3_m_sdz[0][1][8][1]=-0.0184483; p2_pc3_m_sdz[0][1][8][1]=0.0092554; p3_pc3_m_sdz[0][1][8][1]=0.000982517; p4_pc3_m_sdz[0][1][8][1]=-0.000935115; 
    p0_pc3_s_sdz[0][1][8][1]=0.404546; p1_pc3_s_sdz[0][1][8][1]=0.743073; p2_pc3_s_sdz[0][1][8][1]=-0.548818; p3_pc3_s_sdz[0][1][8][1]=0.179371; p4_pc3_s_sdz[0][1][8][1]=-0.0211503; 
    p0_pc3_m_sdz[1][1][8][1]=-3.90337; p1_pc3_m_sdz[1][1][8][1]=16.7591; p2_pc3_m_sdz[1][1][8][1]=-4.96344; p3_pc3_m_sdz[1][1][8][1]=-45.3509; p4_pc3_m_sdz[1][1][8][1]=42.3225; 
    p0_pc3_s_sdz[1][1][8][1]=0.942965; p1_pc3_s_sdz[1][1][8][1]=-0.21528; p2_pc3_s_sdz[1][1][8][1]=-1.35039; p3_pc3_s_sdz[1][1][8][1]=-0.93092; p4_pc3_s_sdz[1][1][8][1]=4.24153; 
    p0_pc3_m_sdz[0][1][9][1]=-0.00634648; p1_pc3_m_sdz[0][1][9][1]=0.00837075; p2_pc3_m_sdz[0][1][9][1]=-0.0156705; p3_pc3_m_sdz[0][1][9][1]=0.0114726; p4_pc3_m_sdz[0][1][9][1]=-0.00263512; 
    p0_pc3_s_sdz[0][1][9][1]=0.631441; p1_pc3_s_sdz[0][1][9][1]=0.193952; p2_pc3_s_sdz[0][1][9][1]=-0.0975447; p3_pc3_s_sdz[0][1][9][1]=0.0286645; p4_pc3_s_sdz[0][1][9][1]=-0.00410447; 
    p0_pc3_m_sdz[1][1][9][1]=2.58771; p1_pc3_m_sdz[1][1][9][1]=-6.42156; p2_pc3_m_sdz[1][1][9][1]=-0.376655; p3_pc3_m_sdz[1][1][9][1]=14.0334; p4_pc3_m_sdz[1][1][9][1]=-11.4761; 
    p0_pc3_s_sdz[1][1][9][1]=1.39304; p1_pc3_s_sdz[1][1][9][1]=-3.34334; p2_pc3_s_sdz[1][1][9][1]=0.472027; p3_pc3_s_sdz[1][1][9][1]=10.5142; p4_pc3_s_sdz[1][1][9][1]=-8.83269; 

  return 1;
}
//_____________________________________________________________________________________________________________________________
