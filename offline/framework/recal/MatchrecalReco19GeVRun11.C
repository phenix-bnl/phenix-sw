#include <MatchrecalReco19GeVRun11.h>
#include <getClass.h>
#include <PHGlobal.h>
#include <PHCompositeNode.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <RunHeader.h>
#include <iostream>

using namespace std;

//_____________________________________________________________________________________________________________________________
MatchrecalReco19GeVRun11::MatchrecalReco19GeVRun11(const string &name) : Recalibrator(name)
{
  baseclasses.insert("PHCentralTrackv23");
  baseclasses.insert("PHCentralTrackv24");
  return;
}
int MatchrecalReco19GeVRun11::isValidRun(const int RunNumber) const
{
  if(RunNumber>=341263 && RunNumber<=342345)return 1;
  else return 0;
}
//_____________________________________________________________________________________________________________________________
MatchrecalReco19GeVRun11::~MatchrecalReco19GeVRun11()
{

}
//_____________________________________________________________________________________________________________________________
int MatchrecalReco19GeVRun11::Init(PHCompositeNode *topNode)
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
int MatchrecalReco19GeVRun11::InitRun(PHCompositeNode *topNode)
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
  cout << " MatchrecalReco19GeVRun11::InitRun() :  run number = " << RunNumber << endl;
  cout << "---------------------------------------------------------------" << endl;
  return 0;
}

//_____________________________________________________________________________________________________________________________
int MatchrecalReco19GeVRun11::process_event(PHCompositeNode *topNode)
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
      pc3sdz   = GetFineTSval(iarm,cent,pt,pc3sdz);
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
int MatchrecalReco19GeVRun11::Zed(float zed)
{
  int ized=(int)(NZBN*(zed + 75.)/150.); 
  if(ized<0 || ized>=NZBN)return -9999;
  else  return ized;
}
//_____________________________________________________________________________________________________________________________


float MatchrecalReco19GeVRun11::GetFineTSval(int isub,int cent,float pt,float sdz)
{
	
  float A0=1.0;
  float A1=0.0;
  float A2=0.0;
	
  //! Only for PC3 sdz
  if(isub==0){ //! arm : 0
    A0 = 0.692922     +   0.197914/pt    + (-0.0524231)/pt/pt;
    A1 = 0.0339563    + (-0.105354)*pt   +   0.0681812*pt*pt;
    A2 = -0.000971425 +   0.00336999/pt  + (-0.00104471)/pt/pt;

  }else if(isub==1){//! arm : 1
    
    A0 =  0.674396       + 0.176918/pt   +(-0.0390975)/pt/pt;
    A1 = (-0.0222848)    + 0.0593779*pt  +(-0.0213859)*pt*pt;
    A2 = (-0.00242902)   + 0.0030689/pt  +(-0.000684575)/pt/pt;

  }
	
  float ftsdz = A0 + A1*cent + A2*cent*cent;
  sdz /= ftsdz;
	  
  return sdz;
}


float MatchrecalReco19GeVRun11::GetSval(int itype,int idet,int isub,int ized,int ich,float fpt,float fdval)
{
  if(fdval<-9000 || fpt<0)return -9999;

  float p0_m=0,p1_m=0,p2_m=0,p3_m=0,p4_m=0;
  float p0_s=1,p1_s=0,p2_s=0,p3_s=0,p4_s=0;
  
  float p0_abm=0,p1_abm=0,p2_abm=0,p3_abm=0,p4_abm=0;
  float p0_abs=1,p1_abs=0,p2_abs=0,p3_abs=0,p4_abs=0;
  
  int   ipt=0;
  if(fpt>=1.5)ipt=1;
  
  if(itype==0){ //! dphi
    
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
int MatchrecalReco19GeVRun11::InitPars(void)
{
  //EMC/Positive charge/dPhi/
  //Writing the data out for Pos 
	
  p0_emc_m_dp[0][0][1]=0.00117414; p1_emc_m_dp[0][0][1]=0.00979903; p2_emc_m_dp[0][0][1]=-0.00991997; p3_emc_m_dp[0][0][1]=0.00571169; p4_emc_m_dp[0][0][1]=-0.00130206; 
  p0_emc_s_dp[0][0][1]=0.00458052; p1_emc_s_dp[0][0][1]=-0.000880218; p2_emc_s_dp[0][0][1]=0.00158324; p3_emc_s_dp[0][0][1]=-0.00105175; p4_emc_s_dp[0][0][1]=0.000380809; 
  p0_emc_m_dp[0][1][1]=0.00160857; p1_emc_m_dp[0][1][1]=0.00959673; p2_emc_m_dp[0][1][1]=-0.0104745; p3_emc_m_dp[0][1][1]=0.00661807; p4_emc_m_dp[0][1][1]=-0.00145431; 
  p0_emc_s_dp[0][1][1]=0.00351123; p1_emc_s_dp[0][1][1]=0.00369592; p2_emc_s_dp[0][1][1]=-0.00576896; p3_emc_s_dp[0][1][1]=0.00390418; p4_emc_s_dp[0][1][1]=-0.000761084; 
  p0_emc_m_dp[0][2][1]=0.00292819; p1_emc_m_dp[0][2][1]=0.00502874; p2_emc_m_dp[0][2][1]=-0.00404725; p3_emc_m_dp[0][2][1]=0.00322403; p4_emc_m_dp[0][2][1]=-0.000771565; 
  p0_emc_s_dp[0][2][1]=0.00468459; p1_emc_s_dp[0][2][1]=-0.000666234; p2_emc_s_dp[0][2][1]=-0.000692645; p3_emc_s_dp[0][2][1]=0.00167452; p4_emc_s_dp[0][2][1]=-0.000449579; 
  p0_emc_m_dp[0][3][1]=0.00682421; p1_emc_m_dp[0][3][1]=-0.00819929; p2_emc_m_dp[0][3][1]=0.0123752; p3_emc_m_dp[0][3][1]=-0.00505985; p4_emc_m_dp[0][3][1]=0.000735933; 
  p0_emc_s_dp[0][3][1]=0.00654249; p1_emc_s_dp[0][3][1]=-0.00779249; p2_emc_s_dp[0][3][1]=0.00871403; p3_emc_s_dp[0][3][1]=-0.00322376; p4_emc_s_dp[0][3][1]=0.000413184; 
  p0_emc_m_dp[0][4][1]=0.00546116; p1_emc_m_dp[0][4][1]=-0.00141402; p2_emc_m_dp[0][4][1]=0.00331339; p3_emc_m_dp[0][4][1]=-0.000212187; p4_emc_m_dp[0][4][1]=-0.000144488; 
  p0_emc_s_dp[0][4][1]=0.0061028; p1_emc_s_dp[0][4][1]=-0.00760251; p2_emc_s_dp[0][4][1]=0.00976567; p3_emc_s_dp[0][4][1]=-0.00407879; p4_emc_s_dp[0][4][1]=0.000599446; 
  p0_emc_m_dp[0][5][1]=0.00298074; p1_emc_m_dp[0][5][1]=0.00249645; p2_emc_m_dp[0][5][1]=-9.93962e-05; p3_emc_m_dp[0][5][1]=0.00145548; p4_emc_m_dp[0][5][1]=-0.000485812; 
  p0_emc_s_dp[0][5][1]=0.00761113; p1_emc_s_dp[0][5][1]=-0.0105557; p2_emc_s_dp[0][5][1]=0.0116555; p3_emc_s_dp[0][5][1]=-0.00452394; p4_emc_s_dp[0][5][1]=0.00062518; 
  p0_emc_m_dp[0][6][1]=0.00391827; p1_emc_m_dp[0][6][1]=-0.00181039; p2_emc_m_dp[0][6][1]=0.00638783; p3_emc_m_dp[0][6][1]=-0.00264684; p4_emc_m_dp[0][6][1]=0.000380302; 
  p0_emc_s_dp[0][6][1]=0.00510006; p1_emc_s_dp[0][6][1]=-0.00126044; p2_emc_s_dp[0][6][1]=-0.000344479; p3_emc_s_dp[0][6][1]=0.00177357; p4_emc_s_dp[0][6][1]=-0.000535803; 
  p0_emc_m_dp[0][7][1]=0.00272821; p1_emc_m_dp[0][7][1]=0.00175496; p2_emc_m_dp[0][7][1]=0.00198731; p3_emc_m_dp[0][7][1]=-0.000460732; p4_emc_m_dp[0][7][1]=-4.05122e-05; 
  p0_emc_s_dp[0][7][1]=0.00738439; p1_emc_s_dp[0][7][1]=-0.00914568; p2_emc_s_dp[0][7][1]=0.00935028; p3_emc_s_dp[0][7][1]=-0.00343366; p4_emc_s_dp[0][7][1]=0.000476437; 
  p0_emc_m_dp[0][8][1]=0.00269399; p1_emc_m_dp[0][8][1]=0.00172545; p2_emc_m_dp[0][8][1]=0.00116595; p3_emc_m_dp[0][8][1]=0.000100178; p4_emc_m_dp[0][8][1]=-0.000212872; 
  p0_emc_s_dp[0][8][1]=0.00674833; p1_emc_s_dp[0][8][1]=-0.00718872; p2_emc_s_dp[0][8][1]=0.00747243; p3_emc_s_dp[0][8][1]=-0.00286802; p4_emc_s_dp[0][8][1]=0.000456264; 
  p0_emc_m_dp[0][9][1]=0.000360884; p1_emc_m_dp[0][9][1]=0.00892527; p2_emc_m_dp[0][9][1]=-0.00728765; p3_emc_m_dp[0][9][1]=0.00388212; p4_emc_m_dp[0][9][1]=-0.000908116; 
  p0_emc_s_dp[0][9][1]=0.00611682; p1_emc_s_dp[0][9][1]=-0.00460998; p2_emc_s_dp[0][9][1]=0.00442951; p3_emc_s_dp[0][9][1]=-0.00155371; p4_emc_s_dp[0][9][1]=0.000273301; 
  p0_emc_m_dp[1][0][1]=0.000932931; p1_emc_m_dp[1][0][1]=0.00584514; p2_emc_m_dp[1][0][1]=-0.00372443; p3_emc_m_dp[1][0][1]=0.000935409; p4_emc_m_dp[1][0][1]=-0.00023575; 
  p0_emc_s_dp[1][0][1]=0.00691014; p1_emc_s_dp[1][0][1]=-0.00496892; p2_emc_s_dp[1][0][1]=0.0045291; p3_emc_s_dp[1][0][1]=-0.00136484; p4_emc_s_dp[1][0][1]=0.000193084; 
  p0_emc_m_dp[1][1][1]=0.00283465; p1_emc_m_dp[1][1][1]=0.00163928; p2_emc_m_dp[1][1][1]=0.000125914; p3_emc_m_dp[1][1][1]=-0.000274958; p4_emc_m_dp[1][1][1]=-1.01776e-05; 
  p0_emc_s_dp[1][1][1]=0.00491296; p1_emc_s_dp[1][1][1]=0.00188584; p2_emc_s_dp[1][1][1]=-0.00383508; p3_emc_s_dp[1][1][1]=0.00302628; p4_emc_s_dp[1][1][1]=-0.00064241; 
  p0_emc_m_dp[1][2][1]=0.00466059; p1_emc_m_dp[1][2][1]=-0.00466057; p2_emc_m_dp[1][2][1]=0.00876736; p3_emc_m_dp[1][2][1]=-0.00480909; p4_emc_m_dp[1][2][1]=0.000868418; 
  p0_emc_s_dp[1][2][1]=0.00496686; p1_emc_s_dp[1][2][1]=0.0014725; p2_emc_s_dp[1][2][1]=-0.00277837; p3_emc_s_dp[1][2][1]=0.00226148; p4_emc_s_dp[1][2][1]=-0.00048133; 
  p0_emc_m_dp[1][3][1]=0.00348031; p1_emc_m_dp[1][3][1]=0.000777092; p2_emc_m_dp[1][3][1]=0.00191601; p3_emc_m_dp[1][3][1]=-0.00109034; p4_emc_m_dp[1][3][1]=0.000186193; 
  p0_emc_s_dp[1][3][1]=0.00709098; p1_emc_s_dp[1][3][1]=-0.00492056; p2_emc_s_dp[1][3][1]=0.00402009; p3_emc_s_dp[1][3][1]=-0.000736817; p4_emc_s_dp[1][3][1]=-9.26949e-06; 
  p0_emc_m_dp[1][4][1]=0.00364919; p1_emc_m_dp[1][4][1]=0.000404569; p2_emc_m_dp[1][4][1]=0.00300877; p3_emc_m_dp[1][4][1]=-0.00188886; p4_emc_m_dp[1][4][1]=0.000382155; 
  p0_emc_s_dp[1][4][1]=0.00486429; p1_emc_s_dp[1][4][1]=0.00330923; p2_emc_s_dp[1][4][1]=-0.00607844; p3_emc_s_dp[1][4][1]=0.00435909; p4_emc_s_dp[1][4][1]=-0.000913359; 
  p0_emc_m_dp[1][5][1]=0.00480873; p1_emc_m_dp[1][5][1]=-0.00319766; p2_emc_m_dp[1][5][1]=0.00732473; p3_emc_m_dp[1][5][1]=-0.00403612; p4_emc_m_dp[1][5][1]=0.000762788; 
  p0_emc_s_dp[1][5][1]=0.00388328; p1_emc_s_dp[1][5][1]=0.00684538; p2_emc_s_dp[1][5][1]=-0.0100555; p3_emc_s_dp[1][5][1]=0.00625589; p4_emc_s_dp[1][5][1]=-0.00124992; 
  p0_emc_m_dp[1][6][1]=0.00463499; p1_emc_m_dp[1][6][1]=-0.00175959; p2_emc_m_dp[1][6][1]=0.00478596; p3_emc_m_dp[1][6][1]=-0.0025199; p4_emc_m_dp[1][6][1]=0.000453396; 
  p0_emc_s_dp[1][6][1]=0.00731699; p1_emc_s_dp[1][6][1]=-0.00435074; p2_emc_s_dp[1][6][1]=0.00252635; p3_emc_s_dp[1][6][1]=0.000159992; p4_emc_s_dp[1][6][1]=-0.000177047; 
  p0_emc_m_dp[1][7][1]=0.00379978; p1_emc_m_dp[1][7][1]=0.00043139; p2_emc_m_dp[1][7][1]=0.00259407; p3_emc_m_dp[1][7][1]=-0.001805; p4_emc_m_dp[1][7][1]=0.000368377; 
  p0_emc_s_dp[1][7][1]=0.00643671; p1_emc_s_dp[1][7][1]=-0.00159775; p2_emc_s_dp[1][7][1]=-0.000416738; p3_emc_s_dp[1][7][1]=0.00142635; p4_emc_s_dp[1][7][1]=-0.000362168; 
  p0_emc_m_dp[1][8][1]=0.00353217; p1_emc_m_dp[1][8][1]=0.00215804; p2_emc_m_dp[1][8][1]=-0.000633584; p3_emc_m_dp[1][8][1]=0.000200182; p4_emc_m_dp[1][8][1]=-0.000112499; 
  p0_emc_s_dp[1][8][1]=0.00749372; p1_emc_s_dp[1][8][1]=-0.00512706; p2_emc_s_dp[1][8][1]=0.00403349; p3_emc_s_dp[1][8][1]=-0.00100375; p4_emc_s_dp[1][8][1]=0.00012037; 
  p0_emc_m_dp[1][9][1]=0.00360445; p1_emc_m_dp[1][9][1]=0.000906432; p2_emc_m_dp[1][9][1]=0.0016092; p3_emc_m_dp[1][9][1]=-0.00164023; p4_emc_m_dp[1][9][1]=0.000239722; 
  p0_emc_s_dp[1][9][1]=0.00658239; p1_emc_s_dp[1][9][1]=-0.00180799; p2_emc_s_dp[1][9][1]=-9.71194e-05; p3_emc_s_dp[1][9][1]=0.00112603; p4_emc_s_dp[1][9][1]=-0.000265956; 
  p0_emc_m_dp[2][0][1]=-0.00286179; p1_emc_m_dp[2][0][1]=0.00728763; p2_emc_m_dp[2][0][1]=-0.0070049; p3_emc_m_dp[2][0][1]=0.00308658; p4_emc_m_dp[2][0][1]=-0.000657836; 
  p0_emc_s_dp[2][0][1]=0.00528337; p1_emc_s_dp[2][0][1]=0.00275316; p2_emc_s_dp[2][0][1]=-0.00441454; p3_emc_s_dp[2][0][1]=0.00298265; p4_emc_s_dp[2][0][1]=-0.000567751; 
  p0_emc_m_dp[2][1][1]=0.000737209; p1_emc_m_dp[2][1][1]=-0.00374988; p2_emc_m_dp[2][1][1]=0.00674552; p3_emc_m_dp[2][1][1]=-0.00388972; p4_emc_m_dp[2][1][1]=0.000705802; 
  p0_emc_s_dp[2][1][1]=0.00655663; p1_emc_s_dp[2][1][1]=-0.00152983; p2_emc_s_dp[2][1][1]=1.15784e-06; p3_emc_s_dp[2][1][1]=0.0012165; p4_emc_s_dp[2][1][1]=-0.000343833; 
  p0_emc_m_dp[2][2][1]=0.001052; p1_emc_m_dp[2][2][1]=-0.00487858; p2_emc_m_dp[2][2][1]=0.00826479; p3_emc_m_dp[2][2][1]=-0.00437269; p4_emc_m_dp[2][2][1]=0.000781966; 
  p0_emc_s_dp[2][2][1]=0.00586041; p1_emc_s_dp[2][2][1]=0.000768137; p2_emc_s_dp[2][2][1]=-0.00252483; p3_emc_s_dp[2][2][1]=0.00229078; p4_emc_s_dp[2][2][1]=-0.000506664; 
  p0_emc_m_dp[2][3][1]=0.00131237; p1_emc_m_dp[2][3][1]=-0.00369141; p2_emc_m_dp[2][3][1]=0.0063039; p3_emc_m_dp[2][3][1]=-0.00322819; p4_emc_m_dp[2][3][1]=0.000591078; 
  p0_emc_s_dp[2][3][1]=0.0061229; p1_emc_s_dp[2][3][1]=2.31045e-05; p2_emc_s_dp[2][3][1]=-0.00233183; p3_emc_s_dp[2][3][1]=0.00268667; p4_emc_s_dp[2][3][1]=-0.000665848; 
  p0_emc_m_dp[2][4][1]=-0.000784964; p1_emc_m_dp[2][4][1]=0.00423187; p2_emc_m_dp[2][4][1]=-0.00336289; p3_emc_m_dp[2][4][1]=0.00182269; p4_emc_m_dp[2][4][1]=-0.000335128; 
  p0_emc_s_dp[2][4][1]=0.00498463; p1_emc_s_dp[2][4][1]=0.00373908; p2_emc_s_dp[2][4][1]=-0.00635883; p3_emc_s_dp[2][4][1]=0.00451981; p4_emc_s_dp[2][4][1]=-0.000963594; 
  p0_emc_m_dp[2][5][1]=0.00217307; p1_emc_m_dp[2][5][1]=-0.00682811; p2_emc_m_dp[2][5][1]=0.00957673; p3_emc_m_dp[2][5][1]=-0.00451475; p4_emc_m_dp[2][5][1]=0.000775094; 
  p0_emc_s_dp[2][5][1]=0.00493986; p1_emc_s_dp[2][5][1]=0.00294668; p2_emc_s_dp[2][5][1]=-0.00418781; p3_emc_s_dp[2][5][1]=0.0028739; p4_emc_s_dp[2][5][1]=-0.000588246; 
  p0_emc_m_dp[2][6][1]=-0.00118606; p1_emc_m_dp[2][6][1]=0.00532446; p2_emc_m_dp[2][6][1]=-0.00575048; p3_emc_m_dp[2][6][1]=0.00323909; p4_emc_m_dp[2][6][1]=-0.000605713; 
  p0_emc_s_dp[2][6][1]=0.00507771; p1_emc_s_dp[2][6][1]=0.00290201; p2_emc_s_dp[2][6][1]=-0.00499407; p3_emc_s_dp[2][6][1]=0.00356694; p4_emc_s_dp[2][6][1]=-0.000744699; 
  p0_emc_m_dp[2][7][1]=0.00101373; p1_emc_m_dp[2][7][1]=-0.00390592; p2_emc_m_dp[2][7][1]=0.00573939; p3_emc_m_dp[2][7][1]=-0.00262781; p4_emc_m_dp[2][7][1]=0.000412765; 
  p0_emc_s_dp[2][7][1]=0.0052141; p1_emc_s_dp[2][7][1]=0.00231815; p2_emc_s_dp[2][7][1]=-0.0044252; p3_emc_s_dp[2][7][1]=0.00327888; p4_emc_s_dp[2][7][1]=-0.000683292; 
  p0_emc_m_dp[2][8][1]=-0.0003343; p1_emc_m_dp[2][8][1]=-3.12888e-05; p2_emc_m_dp[2][8][1]=0.0005525; p3_emc_m_dp[2][8][1]=3.54385e-05; p4_emc_m_dp[2][8][1]=-0.000119683; 
  p0_emc_s_dp[2][8][1]=0.00446487; p1_emc_s_dp[2][8][1]=0.00352835; p2_emc_s_dp[2][8][1]=-0.0044476; p3_emc_s_dp[2][8][1]=0.00265493; p4_emc_s_dp[2][8][1]=-0.000474479; 
  p0_emc_m_dp[2][9][1]=-0.00234132; p1_emc_m_dp[2][9][1]=0.00516503; p2_emc_m_dp[2][9][1]=-0.00482599; p3_emc_m_dp[2][9][1]=0.00207609; p4_emc_m_dp[2][9][1]=-0.000482061; 
  p0_emc_s_dp[2][9][1]=0.00555523; p1_emc_s_dp[2][9][1]=-0.000170686; p2_emc_s_dp[2][9][1]=-0.000160124; p3_emc_s_dp[2][9][1]=0.000568272; p4_emc_s_dp[2][9][1]=-9.85723e-05; 
				
  //EMC/Negative charge/dPhi/
  //Writing the data out for Neg 
	
  p0_emc_m_dp[0][0][0]=0.000883293; p1_emc_m_dp[0][0][0]=-0.00283938; p2_emc_m_dp[0][0][0]=-0.000576057; p3_emc_m_dp[0][0][0]=0.000130885; p4_emc_m_dp[0][0][0]=0.000214314; 
  p0_emc_s_dp[0][0][0]=0.00751647; p1_emc_s_dp[0][0][0]=-0.0126192; p2_emc_s_dp[0][0][0]=0.0170839; p3_emc_s_dp[0][0][0]=-0.00889887; p4_emc_s_dp[0][0][0]=0.00168374; 
  p0_emc_m_dp[0][1][0]=-0.000157354; p1_emc_m_dp[0][1][0]=0.000738729; p2_emc_m_dp[0][1][0]=-0.00573016; p3_emc_m_dp[0][1][0]=0.00265794; p4_emc_m_dp[0][1][0]=-0.000309753; 
  p0_emc_s_dp[0][1][0]=0.00599549; p1_emc_s_dp[0][1][0]=-0.0064387; p2_emc_s_dp[0][1][0]=0.00829221; p3_emc_s_dp[0][1][0]=-0.00384781; p4_emc_s_dp[0][1][0]=0.000694867; 
  p0_emc_m_dp[0][2][0]=-0.00217932; p1_emc_m_dp[0][2][0]=0.00706784; p2_emc_m_dp[0][2][0]=-0.0131528; p3_emc_m_dp[0][2][0]=0.00611619; p4_emc_m_dp[0][2][0]=-0.000952916; 
  p0_emc_s_dp[0][2][0]=0.0107924; p1_emc_s_dp[0][2][0]=-0.0223259; p2_emc_s_dp[0][2][0]=0.0266007; p3_emc_s_dp[0][2][0]=-0.0123153; p4_emc_s_dp[0][2][0]=0.00202271; 
  p0_emc_m_dp[0][3][0]=0.00114514; p1_emc_m_dp[0][3][0]=-0.0038263; p2_emc_m_dp[0][3][0]=-0.000722641; p3_emc_m_dp[0][3][0]=5.30014e-05; p4_emc_m_dp[0][3][0]=5.37087e-05; 
  p0_emc_s_dp[0][3][0]=0.00883663; p1_emc_s_dp[0][3][0]=-0.0177397; p2_emc_s_dp[0][3][0]=0.0233767; p3_emc_s_dp[0][3][0]=-0.0117964; p4_emc_s_dp[0][3][0]=0.00210994; 
  p0_emc_m_dp[0][4][0]=-0.00242226; p1_emc_m_dp[0][4][0]=0.00713485; p2_emc_m_dp[0][4][0]=-0.0126342; p3_emc_m_dp[0][4][0]=0.00514478; p4_emc_m_dp[0][4][0]=-0.00068874; 
  p0_emc_s_dp[0][4][0]=0.0064298; p1_emc_s_dp[0][4][0]=-0.00930422; p2_emc_s_dp[0][4][0]=0.0133613; p3_emc_s_dp[0][4][0]=-0.00674492; p4_emc_s_dp[0][4][0]=0.00119941; 
  p0_emc_m_dp[0][5][0]=-0.00013396; p1_emc_m_dp[0][5][0]=-0.00349671; p2_emc_m_dp[0][5][0]=0.000280934; p3_emc_m_dp[0][5][0]=-0.000852162; p4_emc_m_dp[0][5][0]=0.000256408; 
  p0_emc_s_dp[0][5][0]=0.0119374; p1_emc_s_dp[0][5][0]=-0.0278325; p2_emc_s_dp[0][5][0]=0.0359537; p3_emc_s_dp[0][5][0]=-0.0183493; p4_emc_s_dp[0][5][0]=0.00330463; 
  p0_emc_m_dp[0][6][0]=-0.00181938; p1_emc_m_dp[0][6][0]=0.00345632; p2_emc_m_dp[0][6][0]=-0.00861088; p3_emc_m_dp[0][6][0]=0.00375047; p4_emc_m_dp[0][6][0]=-0.000557384; 
  p0_emc_s_dp[0][6][0]=0.00823521; p1_emc_s_dp[0][6][0]=-0.0154744; p2_emc_s_dp[0][6][0]=0.0211301; p3_emc_s_dp[0][6][0]=-0.0109267; p4_emc_s_dp[0][6][0]=0.00198848; 
  p0_emc_m_dp[0][7][0]=-0.00228426; p1_emc_m_dp[0][7][0]=0.00432105; p2_emc_m_dp[0][7][0]=-0.00850661; p3_emc_m_dp[0][7][0]=0.00346169; p4_emc_m_dp[0][7][0]=-0.000457854; 
  p0_emc_s_dp[0][7][0]=0.00706457; p1_emc_s_dp[0][7][0]=-0.0096464; p2_emc_s_dp[0][7][0]=0.0123198; p3_emc_s_dp[0][7][0]=-0.00585798; p4_emc_s_dp[0][7][0]=0.0010134; 
  p0_emc_m_dp[0][8][0]=-0.00045665; p1_emc_m_dp[0][8][0]=-0.00108006; p2_emc_m_dp[0][8][0]=-0.0026809; p3_emc_m_dp[0][8][0]=0.00110715; p4_emc_m_dp[0][8][0]=-7.50084e-05; 
  p0_emc_s_dp[0][8][0]=0.00667154; p1_emc_s_dp[0][8][0]=-0.00961526; p2_emc_s_dp[0][8][0]=0.0135474; p3_emc_s_dp[0][8][0]=-0.00712082; p4_emc_s_dp[0][8][0]=0.00136156; 
  p0_emc_m_dp[0][9][0]=-0.000110574; p1_emc_m_dp[0][9][0]=-0.00120229; p2_emc_m_dp[0][9][0]=-0.00308679; p3_emc_m_dp[0][9][0]=0.00181317; p4_emc_m_dp[0][9][0]=-0.00016286; 
  p0_emc_s_dp[0][9][0]=0.00420296; p1_emc_s_dp[0][9][0]=0.00083828; p2_emc_s_dp[0][9][0]=-0.0016722; p3_emc_s_dp[0][9][0]=0.00156589; p4_emc_s_dp[0][9][0]=-0.000313467; 
  p0_emc_m_dp[1][0][0]=0.000791596; p1_emc_m_dp[1][0][0]=0.00143541; p2_emc_m_dp[1][0][0]=-0.0030622; p3_emc_m_dp[1][0][0]=0.00162792; p4_emc_m_dp[1][0][0]=-0.000125857; 
  p0_emc_s_dp[1][0][0]=0.00786147; p1_emc_s_dp[1][0][0]=-0.00769383; p2_emc_s_dp[1][0][0]=0.00892675; p3_emc_s_dp[1][0][0]=-0.0042952; p4_emc_s_dp[1][0][0]=0.000822693; 
  p0_emc_m_dp[1][1][0]=0.000195302; p1_emc_m_dp[1][1][0]=0.00265246; p2_emc_m_dp[1][1][0]=-0.00410225; p3_emc_m_dp[1][1][0]=0.00153119; p4_emc_m_dp[1][1][0]=-0.000105269; 
  p0_emc_s_dp[1][1][0]=0.00935825; p1_emc_s_dp[1][1][0]=-0.0129503; p2_emc_s_dp[1][1][0]=0.0147412; p3_emc_s_dp[1][1][0]=-0.00679495; p4_emc_s_dp[1][1][0]=0.00118035; 
  p0_emc_m_dp[1][2][0]=0.000648378; p1_emc_m_dp[1][2][0]=0.00120959; p2_emc_m_dp[1][2][0]=-0.00299504; p3_emc_m_dp[1][2][0]=0.00103097; p4_emc_m_dp[1][2][0]=-7.66131e-05; 
  p0_emc_s_dp[1][2][0]=0.00830551; p1_emc_s_dp[1][2][0]=-0.00928195; p2_emc_s_dp[1][2][0]=0.0104262; p3_emc_s_dp[1][2][0]=-0.00460902; p4_emc_s_dp[1][2][0]=0.00076664; 
  p0_emc_m_dp[1][3][0]=0.00320741; p1_emc_m_dp[1][3][0]=-0.0076314; p2_emc_m_dp[1][3][0]=0.00741872; p3_emc_m_dp[1][3][0]=-0.00426448; p4_emc_m_dp[1][3][0]=0.000855818; 
  p0_emc_s_dp[1][3][0]=0.00878392; p1_emc_s_dp[1][3][0]=-0.0107437; p2_emc_s_dp[1][3][0]=0.0119174; p3_emc_s_dp[1][3][0]=-0.00512595; p4_emc_s_dp[1][3][0]=0.000806923; 
  p0_emc_m_dp[1][4][0]=-0.000552355; p1_emc_m_dp[1][4][0]=0.00498828; p2_emc_m_dp[1][4][0]=-0.00663398; p3_emc_m_dp[1][4][0]=0.00220879; p4_emc_m_dp[1][4][0]=-0.000228267; 
  p0_emc_s_dp[1][4][0]=0.00906687; p1_emc_s_dp[1][4][0]=-0.0122815; p2_emc_s_dp[1][4][0]=0.0150874; p3_emc_s_dp[1][4][0]=-0.00732639; p4_emc_s_dp[1][4][0]=0.00128995; 
  p0_emc_m_dp[1][5][0]=-3.71066e-05; p1_emc_m_dp[1][5][0]=0.00450607; p2_emc_m_dp[1][5][0]=-0.0073503; p3_emc_m_dp[1][5][0]=0.00319993; p4_emc_m_dp[1][5][0]=-0.000491998; 
  p0_emc_s_dp[1][5][0]=0.00869299; p1_emc_s_dp[1][5][0]=-0.00926807; p2_emc_s_dp[1][5][0]=0.0101092; p3_emc_s_dp[1][5][0]=-0.00436199; p4_emc_s_dp[1][5][0]=0.00070558; 
  p0_emc_m_dp[1][6][0]=0.00167957; p1_emc_m_dp[1][6][0]=-0.00183004; p2_emc_m_dp[1][6][0]=0.00127339; p3_emc_m_dp[1][6][0]=-0.00137676; p4_emc_m_dp[1][6][0]=0.000359986; 
  p0_emc_s_dp[1][6][0]=0.00639146; p1_emc_s_dp[1][6][0]=-0.00134154; p2_emc_s_dp[1][6][0]=0.000633625; p3_emc_s_dp[1][6][0]=0.000334702; p4_emc_s_dp[1][6][0]=-0.000118029; 
  p0_emc_m_dp[1][7][0]=0.000630365; p1_emc_m_dp[1][7][0]=0.00492666; p2_emc_m_dp[1][7][0]=-0.00876527; p3_emc_m_dp[1][7][0]=0.00455906; p4_emc_m_dp[1][7][0]=-0.000795625; 
  p0_emc_s_dp[1][7][0]=0.00746207; p1_emc_s_dp[1][7][0]=-0.00550931; p2_emc_s_dp[1][7][0]=0.00587463; p3_emc_s_dp[1][7][0]=-0.00235142; p4_emc_s_dp[1][7][0]=0.000362661; 
  p0_emc_m_dp[1][8][0]=0.00133246; p1_emc_m_dp[1][8][0]=0.0012857; p2_emc_m_dp[1][8][0]=-0.00241989; p3_emc_m_dp[1][8][0]=0.000805683; p4_emc_m_dp[1][8][0]=4.49509e-07; 
  p0_emc_s_dp[1][8][0]=0.00788135; p1_emc_s_dp[1][8][0]=-0.00685147; p2_emc_s_dp[1][8][0]=0.00762333; p3_emc_s_dp[1][8][0]=-0.00334535; p4_emc_s_dp[1][8][0]=0.000569195; 
  p0_emc_m_dp[1][9][0]=0.000713139; p1_emc_m_dp[1][9][0]=0.00490044; p2_emc_m_dp[1][9][0]=-0.00753879; p3_emc_m_dp[1][9][0]=0.00412823; p4_emc_m_dp[1][9][0]=-0.000609845; 
  p0_emc_s_dp[1][9][0]=0.00423544; p1_emc_s_dp[1][9][0]=0.00680057; p2_emc_s_dp[1][9][0]=-0.00944723; p3_emc_s_dp[1][9][0]=0.00535761; p4_emc_s_dp[1][9][0]=-0.000967349; 
  p0_emc_m_dp[2][0][0]=-0.00309564; p1_emc_m_dp[2][0][0]=-0.00223456; p2_emc_m_dp[2][0][0]=0.000397839; p3_emc_m_dp[2][0][0]=0.000313372; p4_emc_m_dp[2][0][0]=4.37187e-05; 
  p0_emc_s_dp[2][0][0]=0.00543917; p1_emc_s_dp[2][0][0]=0.00454517; p2_emc_s_dp[2][0][0]=-0.00628573; p3_emc_s_dp[2][0][0]=0.00358896; p4_emc_s_dp[2][0][0]=-0.000630686; 
  p0_emc_m_dp[2][1][0]=-0.0016131; p1_emc_m_dp[2][1][0]=-0.00755632; p2_emc_m_dp[2][1][0]=0.00735673; p3_emc_m_dp[2][1][0]=-0.00390885; p4_emc_m_dp[2][1][0]=0.000807539; 
  p0_emc_s_dp[2][1][0]=0.00874615; p1_emc_s_dp[2][1][0]=-0.00665468; p2_emc_s_dp[2][1][0]=0.00639575; p3_emc_s_dp[2][1][0]=-0.00238474; p4_emc_s_dp[2][1][0]=0.000373559; 
  p0_emc_m_dp[2][2][0]=-0.00274422; p1_emc_m_dp[2][2][0]=-0.00435648; p2_emc_m_dp[2][2][0]=0.00330602; p3_emc_m_dp[2][2][0]=-0.0020253; p4_emc_m_dp[2][2][0]=0.000455431; 
  p0_emc_s_dp[2][2][0]=0.00934455; p1_emc_s_dp[2][2][0]=-0.00920274; p2_emc_s_dp[2][2][0]=0.00972313; p3_emc_s_dp[2][2][0]=-0.00416471; p4_emc_s_dp[2][2][0]=0.000696246; 
  p0_emc_m_dp[2][3][0]=-0.00400709; p1_emc_m_dp[2][3][0]=0.00121896; p2_emc_m_dp[2][3][0]=-0.00443108; p3_emc_m_dp[2][3][0]=0.00211778; p4_emc_m_dp[2][3][0]=-0.000339073; 
  p0_emc_s_dp[2][3][0]=0.00770535; p1_emc_s_dp[2][3][0]=-0.00392609; p2_emc_s_dp[2][3][0]=0.0034247; p3_emc_s_dp[2][3][0]=-0.000904566; p4_emc_s_dp[2][3][0]=7.95812e-05; 
  p0_emc_m_dp[2][4][0]=0.000269245; p1_emc_m_dp[2][4][0]=-0.0120311; p2_emc_m_dp[2][4][0]=0.0101212; p3_emc_m_dp[2][4][0]=-0.00471163; p4_emc_m_dp[2][4][0]=0.000793877; 
  p0_emc_s_dp[2][4][0]=0.0101308; p1_emc_s_dp[2][4][0]=-0.0119502; p2_emc_s_dp[2][4][0]=0.01252; p3_emc_s_dp[2][4][0]=-0.00525863; p4_emc_s_dp[2][4][0]=0.000839508; 
  p0_emc_m_dp[2][5][0]=0.000216978; p1_emc_m_dp[2][5][0]=-0.0147081; p2_emc_m_dp[2][5][0]=0.0155873; p3_emc_m_dp[2][5][0]=-0.0081119; p4_emc_m_dp[2][5][0]=0.00145754; 
  p0_emc_s_dp[2][5][0]=0.00603218; p1_emc_s_dp[2][5][0]=-0.00010778; p2_emc_s_dp[2][5][0]=0.000316183; p3_emc_s_dp[2][5][0]=0.000103434; p4_emc_s_dp[2][5][0]=-2.19861e-05; 
  p0_emc_m_dp[2][6][0]=-0.00196014; p1_emc_m_dp[2][6][0]=-0.00640492; p2_emc_m_dp[2][6][0]=0.00559294; p3_emc_m_dp[2][6][0]=-0.00313375; p4_emc_m_dp[2][6][0]=0.000599403; 
  p0_emc_s_dp[2][6][0]=0.00857465; p1_emc_s_dp[2][6][0]=-0.00739005; p2_emc_s_dp[2][6][0]=0.0075803; p3_emc_s_dp[2][6][0]=-0.00301997; p4_emc_s_dp[2][6][0]=0.000458508; 
  p0_emc_m_dp[2][7][0]=-0.00160791; p1_emc_m_dp[2][7][0]=-0.00643436; p2_emc_m_dp[2][7][0]=0.00539094; p3_emc_m_dp[2][7][0]=-0.00289611; p4_emc_m_dp[2][7][0]=0.000573283; 
  p0_emc_s_dp[2][7][0]=0.00895641; p1_emc_s_dp[2][7][0]=-0.00988921; p2_emc_s_dp[2][7][0]=0.0112816; p3_emc_s_dp[2][7][0]=-0.00515339; p4_emc_s_dp[2][7][0]=0.000885452; 
  p0_emc_m_dp[2][8][0]=-0.003238; p1_emc_m_dp[2][8][0]=-0.000907253; p2_emc_m_dp[2][8][0]=-0.00110457; p3_emc_m_dp[2][8][0]=0.000601984; p4_emc_m_dp[2][8][0]=-4.24846e-05; 
  p0_emc_s_dp[2][8][0]=0.00593323; p1_emc_s_dp[2][8][0]=0.00071947; p2_emc_s_dp[2][8][0]=-0.00164899; p3_emc_s_dp[2][8][0]=0.00125934; p4_emc_s_dp[2][8][0]=-0.000218993; 
  p0_emc_m_dp[2][9][0]=-0.00400703; p1_emc_m_dp[2][9][0]=0.00328204; p2_emc_m_dp[2][9][0]=-0.00666603; p3_emc_m_dp[2][9][0]=0.0038571; p4_emc_m_dp[2][9][0]=-0.000578805; 
  p0_emc_s_dp[2][9][0]=0.00627313; p1_emc_s_dp[2][9][0]=-0.00099008; p2_emc_s_dp[2][9][0]=0.00045036; p3_emc_s_dp[2][9][0]=0.000275667; p4_emc_s_dp[2][9][0]=-5.57407e-05; 


  //EMC/Positive charge/dZ/
  //Writing the data out for Pos 
	
  p0_emc_m_dz[0][0][1]=-23.9678; p1_emc_m_dz[0][0][1]=74.4289; p2_emc_m_dz[0][0][1]=-77.7311; p3_emc_m_dz[0][0][1]=33.0772; p4_emc_m_dz[0][0][1]=-4.87481; 
  p0_emc_s_dz[0][0][1]=-11.0213; p1_emc_s_dz[0][0][1]=37.3078; p2_emc_s_dz[0][0][1]=-34.4604; p3_emc_s_dz[0][0][1]=13.2665; p4_emc_s_dz[0][0][1]=-1.77258; 
  p0_emc_m_dz[0][1][1]=-2.26614; p1_emc_m_dz[0][1][1]=7.06616; p2_emc_m_dz[0][1][1]=-7.28268; p3_emc_m_dz[0][1][1]=3.28226; p4_emc_m_dz[0][1][1]=-0.499947; 
  p0_emc_s_dz[0][1][1]=0.49861; p1_emc_s_dz[0][1][1]=5.1263; p2_emc_s_dz[0][1][1]=-4.10359; p3_emc_s_dz[0][1][1]=1.64551; p4_emc_s_dz[0][1][1]=-0.22763; 
  p0_emc_m_dz[0][2][1]=-1.38096; p1_emc_m_dz[0][2][1]=3.79374; p2_emc_m_dz[0][2][1]=-3.12265; p3_emc_m_dz[0][2][1]=1.19861; p4_emc_m_dz[0][2][1]=-0.158067; 
  p0_emc_s_dz[0][2][1]=1.86092; p1_emc_s_dz[0][2][1]=1.15686; p2_emc_s_dz[0][2][1]=-0.305046; p3_emc_s_dz[0][2][1]=0.151155; p4_emc_s_dz[0][2][1]=-0.0255514; 
  p0_emc_m_dz[0][3][1]=-1.78262; p1_emc_m_dz[0][3][1]=5.48981; p2_emc_m_dz[0][3][1]=-5.25153; p3_emc_m_dz[0][3][1]=2.18347; p4_emc_m_dz[0][3][1]=-0.315806; 
  p0_emc_s_dz[0][3][1]=1.70403; p1_emc_s_dz[0][3][1]=1.99153; p2_emc_s_dz[0][3][1]=-1.92098; p3_emc_s_dz[0][3][1]=1.13309; p4_emc_s_dz[0][3][1]=-0.204897; 
  p0_emc_m_dz[0][4][1]=-0.305691; p1_emc_m_dz[0][4][1]=2.87204; p2_emc_m_dz[0][4][1]=-3.73392; p3_emc_m_dz[0][4][1]=1.82671; p4_emc_m_dz[0][4][1]=-0.295617; 
  p0_emc_s_dz[0][4][1]=0.179229; p1_emc_s_dz[0][4][1]=5.99951; p2_emc_s_dz[0][4][1]=-5.72241; p3_emc_s_dz[0][4][1]=2.67717; p4_emc_s_dz[0][4][1]=-0.426003; 
  p0_emc_m_dz[0][5][1]=0.807891; p1_emc_m_dz[0][5][1]=-1.36598; p2_emc_m_dz[0][5][1]=1.00401; p3_emc_m_dz[0][5][1]=-0.35047; p4_emc_m_dz[0][5][1]=0.0396094; 
  p0_emc_s_dz[0][5][1]=1.34496; p1_emc_s_dz[0][5][1]=2.94405; p2_emc_s_dz[0][5][1]=-2.7356; p3_emc_s_dz[0][5][1]=1.43811; p4_emc_s_dz[0][5][1]=-0.243766; 
  p0_emc_m_dz[0][6][1]=0.979777; p1_emc_m_dz[0][6][1]=-1.85644; p2_emc_m_dz[0][6][1]=1.70439; p3_emc_m_dz[0][6][1]=-0.746055; p4_emc_m_dz[0][6][1]=0.105477; 
  p0_emc_s_dz[0][6][1]=2.15509; p1_emc_s_dz[0][6][1]=0.681519; p2_emc_s_dz[0][6][1]=-0.401347; p3_emc_s_dz[0][6][1]=0.32689; p4_emc_s_dz[0][6][1]=-0.063963; 
  p0_emc_m_dz[0][7][1]=2.46915; p1_emc_m_dz[0][7][1]=-5.71374; p2_emc_m_dz[0][7][1]=5.70734; p3_emc_m_dz[0][7][1]=-2.54643; p4_emc_m_dz[0][7][1]=0.381686; 
  p0_emc_s_dz[0][7][1]=3.01439; p1_emc_s_dz[0][7][1]=-1.48383; p2_emc_s_dz[0][7][1]=1.871; p3_emc_s_dz[0][7][1]=-0.641549; p4_emc_s_dz[0][7][1]=0.0821184; 
  p0_emc_m_dz[0][8][1]=2.50852; p1_emc_m_dz[0][8][1]=-5.47045; p2_emc_m_dz[0][8][1]=5.39742; p3_emc_m_dz[0][8][1]=-2.4103; p4_emc_m_dz[0][8][1]=0.355849; 
  p0_emc_s_dz[0][8][1]=2.79058; p1_emc_s_dz[0][8][1]=-0.36675; p2_emc_s_dz[0][8][1]=0.670439; p3_emc_s_dz[0][8][1]=-0.178903; p4_emc_s_dz[0][8][1]=0.0211739; 
  p0_emc_m_dz[0][9][1]=3.05675; p1_emc_m_dz[0][9][1]=-6.33528; p2_emc_m_dz[0][9][1]=5.93803; p3_emc_m_dz[0][9][1]=-2.59675; p4_emc_m_dz[0][9][1]=0.401864; 
  p0_emc_s_dz[0][9][1]=2.32389; p1_emc_s_dz[0][9][1]=0.814169; p2_emc_s_dz[0][9][1]=-0.0646423; p3_emc_s_dz[0][9][1]=-0.112599; p4_emc_s_dz[0][9][1]=0.0491412; 
  p0_emc_m_dz[1][0][1]=-2.75648; p1_emc_m_dz[1][0][1]=0.541923; p2_emc_m_dz[1][0][1]=0.759507; p3_emc_m_dz[1][0][1]=-0.453021; p4_emc_m_dz[1][0][1]=0.0574612; 
  p0_emc_s_dz[1][0][1]=2.94676; p1_emc_s_dz[1][0][1]=0.0614707; p2_emc_s_dz[1][0][1]=0.0669767; p3_emc_s_dz[1][0][1]=0.100685; p4_emc_s_dz[1][0][1]=-0.0121303; 
  p0_emc_m_dz[1][1][1]=-1.99093; p1_emc_m_dz[1][1][1]=-0.66079; p2_emc_m_dz[1][1][1]=1.85516; p3_emc_m_dz[1][1][1]=-0.901847; p4_emc_m_dz[1][1][1]=0.136125; 
  p0_emc_s_dz[1][1][1]=2.85587; p1_emc_s_dz[1][1][1]=-0.47045; p2_emc_s_dz[1][1][1]=0.67412; p3_emc_s_dz[1][1][1]=-0.0319194; p4_emc_s_dz[1][1][1]=-0.026156; 
  p0_emc_m_dz[1][2][1]=-2.45581; p1_emc_m_dz[1][2][1]=2.35161; p2_emc_m_dz[1][2][1]=-1.49988; p3_emc_m_dz[1][2][1]=0.519527; p4_emc_m_dz[1][2][1]=-0.0671068; 
  p0_emc_s_dz[1][2][1]=3.28841; p1_emc_s_dz[1][2][1]=-2.2506; p2_emc_s_dz[1][2][1]=2.60636; p3_emc_s_dz[1][2][1]=-0.868169; p4_emc_s_dz[1][2][1]=0.0974688; 
  p0_emc_m_dz[1][3][1]=-1.0305; p1_emc_m_dz[1][3][1]=0.345409; p2_emc_m_dz[1][3][1]=0.211738; p3_emc_m_dz[1][3][1]=-0.201908; p4_emc_m_dz[1][3][1]=0.0423969; 
  p0_emc_s_dz[1][3][1]=2.76944; p1_emc_s_dz[1][3][1]=-0.118568; p2_emc_s_dz[1][3][1]=-0.1713; p3_emc_s_dz[1][3][1]=0.494348; p4_emc_s_dz[1][3][1]=-0.122833; 
  p0_emc_m_dz[1][4][1]=0.41674; p1_emc_m_dz[1][4][1]=-1.79434; p2_emc_m_dz[1][4][1]=1.77768; p3_emc_m_dz[1][4][1]=-0.724346; p4_emc_m_dz[1][4][1]=0.10353; 
  p0_emc_s_dz[1][4][1]=3.17368; p1_emc_s_dz[1][4][1]=-2.0574; p2_emc_s_dz[1][4][1]=2.35659; p3_emc_s_dz[1][4][1]=-0.751216; p4_emc_s_dz[1][4][1]=0.0825538; 
  p0_emc_m_dz[1][5][1]=0.456052; p1_emc_m_dz[1][5][1]=1.03623; p2_emc_m_dz[1][5][1]=-1.78739; p3_emc_m_dz[1][5][1]=0.884844; p4_emc_m_dz[1][5][1]=-0.143683; 
  p0_emc_s_dz[1][5][1]=2.8833; p1_emc_s_dz[1][5][1]=-0.440737; p2_emc_s_dz[1][5][1]=0.273347; p3_emc_s_dz[1][5][1]=0.243518; p4_emc_s_dz[1][5][1]=-0.075971; 
  p0_emc_m_dz[1][6][1]=1.71332; p1_emc_m_dz[1][6][1]=-0.984102; p2_emc_m_dz[1][6][1]=0.214284; p3_emc_m_dz[1][6][1]=-0.0120521; p4_emc_m_dz[1][6][1]=-0.00495064; 
  p0_emc_s_dz[1][6][1]=2.41568; p1_emc_s_dz[1][6][1]=0.370931; p2_emc_s_dz[1][6][1]=0.162185; p3_emc_s_dz[1][6][1]=0.030334; p4_emc_s_dz[1][6][1]=-0.014801; 
  p0_emc_m_dz[1][7][1]=5.37723; p1_emc_m_dz[1][7][1]=-9.26849; p2_emc_m_dz[1][7][1]=7.851; p3_emc_m_dz[1][7][1]=-3.02902; p4_emc_m_dz[1][7][1]=0.416818; 
  p0_emc_s_dz[1][7][1]=-7.0232; p1_emc_s_dz[1][7][1]=28.3005; p2_emc_s_dz[1][7][1]=-27.6688; p3_emc_s_dz[1][7][1]=11.4133; p4_emc_s_dz[1][7][1]=-1.64511; 
  p0_emc_m_dz[1][8][1]=2.56608; p1_emc_m_dz[1][8][1]=0.721226; p2_emc_m_dz[1][8][1]=-2.3237; p3_emc_m_dz[1][8][1]=1.1088; p4_emc_m_dz[1][8][1]=-0.167453; 
  p0_emc_s_dz[1][8][1]=3.55836; p1_emc_s_dz[1][8][1]=-1.88704; p2_emc_s_dz[1][8][1]=2.45321; p3_emc_s_dz[1][8][1]=-0.938783; p4_emc_s_dz[1][8][1]=0.126229; 
  p0_emc_m_dz[1][9][1]=4.09095; p1_emc_m_dz[1][9][1]=-2.61086; p2_emc_m_dz[1][9][1]=1.01048; p3_emc_m_dz[1][9][1]=-0.328485; p4_emc_m_dz[1][9][1]=0.0625295; 
  p0_emc_s_dz[1][9][1]=1.72852; p1_emc_s_dz[1][9][1]=3.75462; p2_emc_s_dz[1][9][1]=-3.50405; p3_emc_s_dz[1][9][1]=1.63208; p4_emc_s_dz[1][9][1]=-0.254861; 
  p0_emc_m_dz[2][0][1]=-0.352409; p1_emc_m_dz[2][0][1]=-1.02157; p2_emc_m_dz[2][0][1]=3.16593; p3_emc_m_dz[2][0][1]=-1.55103; p4_emc_m_dz[2][0][1]=0.217987; 
  p0_emc_s_dz[2][0][1]=5.03633; p1_emc_s_dz[2][0][1]=-6.76954; p2_emc_s_dz[2][0][1]=7.65336; p3_emc_s_dz[2][0][1]=-3.14753; p4_emc_s_dz[2][0][1]=0.45803; 
  p0_emc_m_dz[2][1][1]=-0.337117; p1_emc_m_dz[2][1][1]=0.204804; p2_emc_m_dz[2][1][1]=1.65666; p3_emc_m_dz[2][1][1]=-0.941222; p4_emc_m_dz[2][1][1]=0.154067; 
  p0_emc_s_dz[2][1][1]=4.39667; p1_emc_s_dz[2][1][1]=-4.59513; p2_emc_s_dz[2][1][1]=5.02485; p3_emc_s_dz[2][1][1]=-1.89631; p4_emc_s_dz[2][1][1]=0.25099; 
  p0_emc_m_dz[2][2][1]=0.575625; p1_emc_m_dz[2][2][1]=-0.495595; p2_emc_m_dz[2][2][1]=1.75515; p3_emc_m_dz[2][2][1]=-0.865489; p4_emc_m_dz[2][2][1]=0.134618; 
  p0_emc_s_dz[2][2][1]=3.76328; p1_emc_s_dz[2][2][1]=-2.96492; p2_emc_s_dz[2][2][1]=3.43558; p3_emc_s_dz[2][2][1]=-1.29786; p4_emc_s_dz[2][2][1]=0.17441; 
  p0_emc_m_dz[2][3][1]=1.30811; p1_emc_m_dz[2][3][1]=-0.80781; p2_emc_m_dz[2][3][1]=1.8683; p3_emc_m_dz[2][3][1]=-0.95598; p4_emc_m_dz[2][3][1]=0.153876; 
  p0_emc_s_dz[2][3][1]=3.11506; p1_emc_s_dz[2][3][1]=-1.77227; p2_emc_s_dz[2][3][1]=2.3479; p3_emc_s_dz[2][3][1]=-0.830661; p4_emc_s_dz[2][3][1]=0.101704; 
  p0_emc_m_dz[2][4][1]=2.43304; p1_emc_m_dz[2][4][1]=-2.12682; p2_emc_m_dz[2][4][1]=2.68704; p3_emc_m_dz[2][4][1]=-1.25802; p4_emc_m_dz[2][4][1]=0.198382; 
  p0_emc_s_dz[2][4][1]=2.78085; p1_emc_s_dz[2][4][1]=-1.0338; p2_emc_s_dz[2][4][1]=1.62938; p3_emc_s_dz[2][4][1]=-0.521546; p4_emc_s_dz[2][4][1]=0.0561657; 
  p0_emc_m_dz[2][5][1]=2.70013; p1_emc_m_dz[2][5][1]=-0.145726; p2_emc_m_dz[2][5][1]=-0.190284; p3_emc_m_dz[2][5][1]=0.127638; p4_emc_m_dz[2][5][1]=-0.0245773; 
  p0_emc_s_dz[2][5][1]=2.60298; p1_emc_s_dz[2][5][1]=-0.519634; p2_emc_s_dz[2][5][1]=1.12363; p3_emc_s_dz[2][5][1]=-0.353088; p4_emc_s_dz[2][5][1]=0.0385593; 
  p0_emc_m_dz[2][6][1]=3.38888; p1_emc_m_dz[2][6][1]=-0.541099; p2_emc_m_dz[2][6][1]=-0.057474; p3_emc_m_dz[2][6][1]=0.0523944; p4_emc_m_dz[2][6][1]=-0.00842011; 
  p0_emc_s_dz[2][6][1]=3.08556; p1_emc_s_dz[2][6][1]=-1.76653; p2_emc_s_dz[2][6][1]=2.05109; p3_emc_s_dz[2][6][1]=-0.62127; p4_emc_s_dz[2][6][1]=0.0636422; 
  p0_emc_m_dz[2][7][1]=3.64753; p1_emc_m_dz[2][7][1]=0.515233; p2_emc_m_dz[2][7][1]=-1.55204; p3_emc_m_dz[2][7][1]=0.712698; p4_emc_m_dz[2][7][1]=-0.106725; 
  p0_emc_s_dz[2][7][1]=2.8563; p1_emc_s_dz[2][7][1]=-0.46106; p2_emc_s_dz[2][7][1]=0.400528; p3_emc_s_dz[2][7][1]=0.172232; p4_emc_s_dz[2][7][1]=-0.0641204; 
  p0_emc_m_dz[2][8][1]=4.55773; p1_emc_m_dz[2][8][1]=-0.750517; p2_emc_m_dz[2][8][1]=-0.839727; p3_emc_m_dz[2][8][1]=0.523212; p4_emc_m_dz[2][8][1]=-0.0854611; 
  p0_emc_s_dz[2][8][1]=3.72588; p1_emc_s_dz[2][8][1]=-2.92145; p2_emc_s_dz[2][8][1]=3.25791; p3_emc_s_dz[2][8][1]=-1.16845; p4_emc_s_dz[2][8][1]=0.149252; 
  p0_emc_m_dz[2][9][1]=1.71543; p1_emc_m_dz[2][9][1]=8.10645; p2_emc_m_dz[2][9][1]=-9.56118; p3_emc_m_dz[2][9][1]=4.00636; p4_emc_m_dz[2][9][1]=-0.559041; 
  p0_emc_s_dz[2][9][1]=-7.61908; p1_emc_s_dz[2][9][1]=29.6566; p2_emc_s_dz[2][9][1]=-28.619; p3_emc_s_dz[2][9][1]=11.6479; p4_emc_s_dz[2][9][1]=-1.65456; 
	
  //EMC/Negative charge/dZ/
  //Writing the data out for Neg 
	
  p0_emc_m_dz[0][0][0]=11.9042; p1_emc_m_dz[0][0][0]=-32.7752; p2_emc_m_dz[0][0][0]=31.4096; p3_emc_m_dz[0][0][0]=-12.0848; p4_emc_m_dz[0][0][0]=1.63409; 
  p0_emc_s_dz[0][0][0]=-8.16015; p1_emc_s_dz[0][0][0]=29.9489; p2_emc_s_dz[0][0][0]=-27.6753; p3_emc_s_dz[0][0][0]=10.5783; p4_emc_s_dz[0][0][0]=-1.39083; 
  p0_emc_m_dz[0][1][0]=-6.179; p1_emc_m_dz[0][1][0]=17.1661; p2_emc_m_dz[0][1][0]=-16.2375; p3_emc_m_dz[0][1][0]=6.71881; p4_emc_m_dz[0][1][0]=-0.968716; 
  p0_emc_s_dz[0][1][0]=-3.92583; p1_emc_s_dz[0][1][0]=19.3909; p2_emc_s_dz[0][1][0]=-18.9022; p3_emc_s_dz[0][1][0]=7.74858; p4_emc_s_dz[0][1][0]=-1.0987; 
  p0_emc_m_dz[0][2][0]=-7.4175; p1_emc_m_dz[0][2][0]=22.1837; p2_emc_m_dz[0][2][0]=-22.0787; p3_emc_m_dz[0][2][0]=9.27685; p4_emc_m_dz[0][2][0]=-1.35309; 
  p0_emc_s_dz[0][2][0]=-7.71724; p1_emc_s_dz[0][2][0]=28.7686; p2_emc_s_dz[0][2][0]=-27.3672; p3_emc_s_dz[0][2][0]=10.9628; p4_emc_s_dz[0][2][0]=-1.53445; 
  p0_emc_m_dz[0][3][0]=0.842423; p1_emc_m_dz[0][3][0]=-1.44318; p2_emc_m_dz[0][3][0]=1.39902; p3_emc_m_dz[0][3][0]=-0.414658; p4_emc_m_dz[0][3][0]=0.0422668; 
  p0_emc_s_dz[0][3][0]=-1.02175; p1_emc_s_dz[0][3][0]=10.186; p2_emc_s_dz[0][3][0]=-9.94946; p3_emc_s_dz[0][3][0]=4.25366; p4_emc_s_dz[0][3][0]=-0.622893; 
  p0_emc_m_dz[0][4][0]=8.91709; p1_emc_m_dz[0][4][0]=-21.8203; p2_emc_m_dz[0][4][0]=19.6076; p3_emc_m_dz[0][4][0]=-7.37116; p4_emc_m_dz[0][4][0]=0.991552; 
  p0_emc_s_dz[0][4][0]=-2.63593; p1_emc_s_dz[0][4][0]=14.3608; p2_emc_s_dz[0][4][0]=-13.6896; p3_emc_s_dz[0][4][0]=5.62086; p4_emc_s_dz[0][4][0]=-0.796175; 
  p0_emc_m_dz[0][5][0]=0.121575; p1_emc_m_dz[0][5][0]=-0.235674; p2_emc_m_dz[0][5][0]=0.743027; p3_emc_m_dz[0][5][0]=-0.587991; p4_emc_m_dz[0][5][0]=0.117873; 
  p0_emc_s_dz[0][5][0]=1.91769; p1_emc_s_dz[0][5][0]=1.02259; p2_emc_s_dz[0][5][0]=-0.257812; p3_emc_s_dz[0][5][0]=0.139676; p4_emc_s_dz[0][5][0]=-0.0195145; 
  p0_emc_m_dz[0][6][0]=-0.592011; p1_emc_m_dz[0][6][0]=1.74427; p2_emc_m_dz[0][6][0]=-1.08956; p3_emc_m_dz[0][6][0]=0.0873472; p4_emc_m_dz[0][6][0]=0.0207056; 
  p0_emc_s_dz[0][6][0]=-5.81897; p1_emc_s_dz[0][6][0]=22.5833; p2_emc_s_dz[0][6][0]=-21.0047; p3_emc_s_dz[0][6][0]=8.38729; p4_emc_s_dz[0][6][0]=-1.17638; 
  p0_emc_m_dz[0][7][0]=1.39566; p1_emc_m_dz[0][7][0]=-2.58009; p2_emc_m_dz[0][7][0]=2.3345; p3_emc_m_dz[0][7][0]=-1.128; p4_emc_m_dz[0][7][0]=0.173936; 
  p0_emc_s_dz[0][7][0]=2.65793; p1_emc_s_dz[0][7][0]=-0.27502; p2_emc_s_dz[0][7][0]=0.796414; p3_emc_s_dz[0][7][0]=-0.299978; p4_emc_s_dz[0][7][0]=0.0477848; 
  p0_emc_m_dz[0][8][0]=1.38959; p1_emc_m_dz[0][8][0]=-2.15872; p2_emc_m_dz[0][8][0]=1.63351; p3_emc_m_dz[0][8][0]=-0.812657; p4_emc_m_dz[0][8][0]=0.125847; 
  p0_emc_s_dz[0][8][0]=2.66396; p1_emc_s_dz[0][8][0]=0.222589; p2_emc_s_dz[0][8][0]=0.250443; p3_emc_s_dz[0][8][0]=-0.107908; p4_emc_s_dz[0][8][0]=0.025119; 
  p0_emc_m_dz[0][9][0]=0.925017; p1_emc_m_dz[0][9][0]=-0.550864; p2_emc_m_dz[0][9][0]=-0.0604351; p3_emc_m_dz[0][9][0]=-0.151979; p4_emc_m_dz[0][9][0]=0.0535726; 
  p0_emc_s_dz[0][9][0]=3.00396; p1_emc_s_dz[0][9][0]=0.129683; p2_emc_s_dz[0][9][0]=-0.299012; p3_emc_s_dz[0][9][0]=0.285977; p4_emc_s_dz[0][9][0]=-0.0436077; 
  p0_emc_m_dz[1][0][0]=1.5252; p1_emc_m_dz[1][0][0]=-9.61442; p2_emc_m_dz[1][0][0]=9.64605; p3_emc_m_dz[1][0][0]=-3.66108; p4_emc_m_dz[1][0][0]=0.473749; 
  p0_emc_s_dz[1][0][0]=3.5326; p1_emc_s_dz[1][0][0]=-0.86745; p2_emc_s_dz[1][0][0]=0.60476; p3_emc_s_dz[1][0][0]=-0.0618545; p4_emc_s_dz[1][0][0]=0.000541488; 
  p0_emc_m_dz[1][1][0]=-4.14968; p1_emc_m_dz[1][1][0]=6.93111; p2_emc_m_dz[1][1][0]=-6.44539; p3_emc_m_dz[1][1][0]=2.75107; p4_emc_m_dz[1][1][0]=-0.40754; 
  p0_emc_s_dz[1][1][0]=2.32541; p1_emc_s_dz[1][1][0]=1.55068; p2_emc_s_dz[1][1][0]=-1.19273; p3_emc_s_dz[1][1][0]=0.502769; p4_emc_s_dz[1][1][0]=-0.0678776; 
  p0_emc_m_dz[1][2][0]=-2.0119; p1_emc_m_dz[1][2][0]=2.28299; p2_emc_m_dz[1][2][0]=-2.1998; p3_emc_m_dz[1][2][0]=1.09154; p4_emc_m_dz[1][2][0]=-0.176581; 
  p0_emc_s_dz[1][2][0]=2.0447; p1_emc_s_dz[1][2][0]=2.43751; p2_emc_s_dz[1][2][0]=-2.37779; p3_emc_s_dz[1][2][0]=1.07369; p4_emc_s_dz[1][2][0]=-0.158151; 
  p0_emc_m_dz[1][3][0]=-1.51386; p1_emc_m_dz[1][3][0]=2.29638; p2_emc_m_dz[1][3][0]=-2.02494; p3_emc_m_dz[1][3][0]=0.872154; p4_emc_m_dz[1][3][0]=-0.129097; 
  p0_emc_s_dz[1][3][0]=1.7595; p1_emc_s_dz[1][3][0]=2.74729; p2_emc_s_dz[1][3][0]=-2.61428; p3_emc_s_dz[1][3][0]=1.20552; p4_emc_s_dz[1][3][0]=-0.183087; 
  p0_emc_m_dz[1][4][0]=-0.00107724; p1_emc_m_dz[1][4][0]=-0.132639; p2_emc_m_dz[1][4][0]=0.0612555; p3_emc_m_dz[1][4][0]=0.0261863; p4_emc_m_dz[1][4][0]=-0.00624044; 
  p0_emc_s_dz[1][4][0]=1.98466; p1_emc_s_dz[1][4][0]=2.20573; p2_emc_s_dz[1][4][0]=-2.15401; p3_emc_s_dz[1][4][0]=1.04935; p4_emc_s_dz[1][4][0]=-0.164203; 
  p0_emc_m_dz[1][5][0]=-0.239354; p1_emc_m_dz[1][5][0]=2.34205; p2_emc_m_dz[1][5][0]=-2.1431; p3_emc_m_dz[1][5][0]=0.769617; p4_emc_m_dz[1][5][0]=-0.100058; 
  p0_emc_s_dz[1][5][0]=0.618919; p1_emc_s_dz[1][5][0]=5.59932; p2_emc_s_dz[1][5][0]=-4.76082; p3_emc_s_dz[1][5][0]=1.86514; p4_emc_s_dz[1][5][0]=-0.255299; 
  p0_emc_m_dz[1][6][0]=0.893173; p1_emc_m_dz[1][6][0]=0.679077; p2_emc_m_dz[1][6][0]=-0.555974; p3_emc_m_dz[1][6][0]=0.0871217; p4_emc_m_dz[1][6][0]=-0.00102091; 
  p0_emc_s_dz[1][6][0]=3.64604; p1_emc_s_dz[1][6][0]=-2.58719; p2_emc_s_dz[1][6][0]=2.93586; p3_emc_s_dz[1][6][0]=-1.13213; p4_emc_s_dz[1][6][0]=0.155908; 
  p0_emc_m_dz[1][7][0]=-6.16091; p1_emc_m_dz[1][7][0]=21.2367; p2_emc_m_dz[1][7][0]=-19.8327; p3_emc_m_dz[1][7][0]=7.51209; p4_emc_m_dz[1][7][0]=-1.01399; 
  p0_emc_s_dz[1][7][0]=-7.6026; p1_emc_s_dz[1][7][0]=28.8954; p2_emc_s_dz[1][7][0]=-26.9425; p3_emc_s_dz[1][7][0]=10.5787; p4_emc_s_dz[1][7][0]=-1.46344; 
  p0_emc_m_dz[1][8][0]=3.54954; p1_emc_m_dz[1][8][0]=-3.29913; p2_emc_m_dz[1][8][0]=2.86491; p3_emc_m_dz[1][8][0]=-1.28299; p4_emc_m_dz[1][8][0]=0.19409; 
  p0_emc_s_dz[1][8][0]=3.2806; p1_emc_s_dz[1][8][0]=-0.478943; p2_emc_s_dz[1][8][0]=0.844024; p3_emc_s_dz[1][8][0]=-0.300304; p4_emc_s_dz[1][8][0]=0.0384657; 
  p0_emc_m_dz[1][9][0]=3.3074; p1_emc_m_dz[1][9][0]=-1.71294; p2_emc_m_dz[1][9][0]=1.32645; p3_emc_m_dz[1][9][0]=-0.741457; p4_emc_m_dz[1][9][0]=0.145451; 
  p0_emc_s_dz[1][9][0]=3.66944; p1_emc_s_dz[1][9][0]=-0.953101; p2_emc_s_dz[1][9][0]=1.07997; p3_emc_s_dz[1][9][0]=-0.404329; p4_emc_s_dz[1][9][0]=0.0665275; 
  p0_emc_m_dz[2][0][0]=-1.609; p1_emc_m_dz[2][0][0]=4.26272; p2_emc_m_dz[2][0][0]=-3.87974; p3_emc_m_dz[2][0][0]=1.78148; p4_emc_m_dz[2][0][0]=-0.296704; 
  p0_emc_s_dz[2][0][0]=1.77201; p1_emc_s_dz[2][0][0]=3.67623; p2_emc_s_dz[2][0][0]=-2.83421; p3_emc_s_dz[2][0][0]=1.00497; p4_emc_s_dz[2][0][0]=-0.115572; 
  p0_emc_m_dz[2][1][0]=-0.452832; p1_emc_m_dz[2][1][0]=2.08826; p2_emc_m_dz[2][1][0]=-2.00097; p3_emc_m_dz[2][1][0]=1.04748; p4_emc_m_dz[2][1][0]=-0.178305; 
  p0_emc_s_dz[2][1][0]=2.22542; p1_emc_s_dz[2][1][0]=2.1098; p2_emc_s_dz[2][1][0]=-1.43423; p3_emc_s_dz[2][1][0]=0.545859; p4_emc_s_dz[2][1][0]=-0.0712263; 
  p0_emc_m_dz[2][2][0]=0.188518; p1_emc_m_dz[2][2][0]=1.18178; p2_emc_m_dz[2][2][0]=-0.8805; p3_emc_m_dz[2][2][0]=0.469446; p4_emc_m_dz[2][2][0]=-0.0809233; 
  p0_emc_s_dz[2][2][0]=2.58806; p1_emc_s_dz[2][2][0]=0.882957; p2_emc_s_dz[2][2][0]=-0.435584; p3_emc_s_dz[2][2][0]=0.206608; p4_emc_s_dz[2][2][0]=-0.0274628; 
  p0_emc_m_dz[2][3][0]=0.369553; p1_emc_m_dz[2][3][0]=2.54745; p2_emc_m_dz[2][3][0]=-2.47085; p3_emc_m_dz[2][3][0]=1.11318; p4_emc_m_dz[2][3][0]=-0.171796; 
  p0_emc_s_dz[2][3][0]=2.71608; p1_emc_s_dz[2][3][0]=0.362429; p2_emc_s_dz[2][3][0]=-0.108981; p3_emc_s_dz[2][3][0]=0.160743; p4_emc_s_dz[2][3][0]=-0.0306228; 
  p0_emc_m_dz[2][4][0]=1.84302; p1_emc_m_dz[2][4][0]=-0.410706; p2_emc_m_dz[2][4][0]=0.673403; p3_emc_m_dz[2][4][0]=-0.319857; p4_emc_m_dz[2][4][0]=0.0498618; 
  p0_emc_s_dz[2][4][0]=1.45487; p1_emc_s_dz[2][4][0]=3.81728; p2_emc_s_dz[2][4][0]=-3.60593; p3_emc_s_dz[2][4][0]=1.6355; p4_emc_s_dz[2][4][0]=-0.247691; 
  p0_emc_m_dz[2][5][0]=2.93991; p1_emc_m_dz[2][5][0]=-1.60906; p2_emc_m_dz[2][5][0]=1.96455; p3_emc_m_dz[2][5][0]=-0.993161; p4_emc_m_dz[2][5][0]=0.163001; 
  p0_emc_s_dz[2][5][0]=2.36263; p1_emc_s_dz[2][5][0]=0.736855; p2_emc_s_dz[2][5][0]=-0.27162; p3_emc_s_dz[2][5][0]=0.189169; p4_emc_s_dz[2][5][0]=-0.0309755; 
  p0_emc_m_dz[2][6][0]=3.16189; p1_emc_m_dz[2][6][0]=-0.579978; p2_emc_m_dz[2][6][0]=0.61771; p3_emc_m_dz[2][6][0]=-0.347211; p4_emc_m_dz[2][6][0]=0.0557086; 
  p0_emc_s_dz[2][6][0]=3.26757; p1_emc_s_dz[2][6][0]=-1.33201; p2_emc_s_dz[2][6][0]=1.44276; p3_emc_s_dz[2][6][0]=-0.460564; p4_emc_s_dz[2][6][0]=0.0592703; 
  p0_emc_m_dz[2][7][0]=3.72889; p1_emc_m_dz[2][7][0]=-0.807001; p2_emc_m_dz[2][7][0]=0.922885; p3_emc_m_dz[2][7][0]=-0.564487; p4_emc_m_dz[2][7][0]=0.0977539; 
  p0_emc_s_dz[2][7][0]=3.13245; p1_emc_s_dz[2][7][0]=-0.80055; p2_emc_s_dz[2][7][0]=0.870763; p3_emc_s_dz[2][7][0]=-0.183828; p4_emc_s_dz[2][7][0]=0.0121661; 
  p0_emc_m_dz[2][8][0]=5.41417; p1_emc_m_dz[2][8][0]=-4.10571; p2_emc_m_dz[2][8][0]=3.82811; p3_emc_m_dz[2][8][0]=-1.68003; p4_emc_m_dz[2][8][0]=0.252542; 
  p0_emc_s_dz[2][8][0]=2.65646; p1_emc_s_dz[2][8][0]=0.729422; p2_emc_s_dz[2][8][0]=-0.227522; p3_emc_s_dz[2][8][0]=0.0880824; p4_emc_s_dz[2][8][0]=-0.00675055; 
  p0_emc_m_dz[2][9][0]=4.14911; p1_emc_m_dz[2][9][0]=-0.307384; p2_emc_m_dz[2][9][0]=0.592996; p3_emc_m_dz[2][9][0]=-0.558977; p4_emc_m_dz[2][9][0]=0.127453; 
  p0_emc_s_dz[2][9][0]=-9.12255; p1_emc_s_dz[2][9][0]=32.9849; p2_emc_s_dz[2][9][0]=-30.6566; p3_emc_s_dz[2][9][0]=11.9782; p4_emc_s_dz[2][9][0]=-1.64172; 

  //PC3/Positive charge/dPhi/
  //Writing the data out for Pos 
	
  p0_pc3_m_dp[0][0][0][1]=0.0020479; p1_pc3_m_dp[0][0][0][1]=-0.00331319; p2_pc3_m_dp[0][0][0][1]=0.00381193; p3_pc3_m_dp[0][0][0][1]=-0.00249911; p4_pc3_m_dp[0][0][0][1]=0.000351088; 
  p0_pc3_m_dp[1][0][0][1]=0.0020479; p1_pc3_m_dp[1][0][0][1]=-0.00331319; p2_pc3_m_dp[1][0][0][1]=0.00381193; p3_pc3_m_dp[1][0][0][1]=-0.00249911; p4_pc3_m_dp[1][0][0][1]=0.000351088; 
  p0_pc3_s_dp[0][0][1]=0.000443057; p1_pc3_s_dp[0][0][1]=0.00562327; p2_pc3_s_dp[0][0][1]=-0.00686311; p3_pc3_s_dp[0][0][1]=0.00410079; p4_pc3_s_dp[0][0][1]=-0.000752254; 
  p0_pc3_m_dp[0][0][1][1]=0.00078266; p1_pc3_m_dp[0][0][1][1]=0.00121252; p2_pc3_m_dp[0][0][1][1]=-0.00103181; p3_pc3_m_dp[0][0][1][1]=7.95155e-05; p4_pc3_m_dp[0][0][1][1]=-3.74245e-05; 
  p0_pc3_m_dp[1][0][1][1]=0.00078266; p1_pc3_m_dp[1][0][1][1]=0.00121252; p2_pc3_m_dp[1][0][1][1]=-0.00103181; p3_pc3_m_dp[1][0][1][1]=7.95155e-05; p4_pc3_m_dp[1][0][1][1]=-3.74245e-05; 
  p0_pc3_s_dp[0][1][1]=0.00294973; p1_pc3_s_dp[0][1][1]=-0.00220508; p2_pc3_s_dp[0][1][1]=0.0018689; p3_pc3_s_dp[0][1][1]=4.64494e-05; p4_pc3_s_dp[0][1][1]=-0.000104881; 
  p0_pc3_m_dp[0][0][2][1]=0.00181256; p1_pc3_m_dp[0][0][2][1]=-0.000933793; p2_pc3_m_dp[0][0][2][1]=0.00113877; p3_pc3_m_dp[0][0][2][1]=-0.000602817; p4_pc3_m_dp[0][0][2][1]=6.9999e-05; 
  p0_pc3_m_dp[1][0][2][1]=0.00181256; p1_pc3_m_dp[1][0][2][1]=-0.000933793; p2_pc3_m_dp[1][0][2][1]=0.00113877; p3_pc3_m_dp[1][0][2][1]=-0.000602817; p4_pc3_m_dp[1][0][2][1]=6.9999e-05; 
  p0_pc3_s_dp[0][2][1]=0.00260074; p1_pc3_s_dp[0][2][1]=-0.00117641; p2_pc3_s_dp[0][2][1]=0.00130991; p3_pc3_s_dp[0][2][1]=-7.81486e-05; p4_pc3_s_dp[0][2][1]=-3.16638e-05; 
  p0_pc3_m_dp[0][0][3][1]=0.00284688; p1_pc3_m_dp[0][0][3][1]=-0.00324525; p2_pc3_m_dp[0][0][3][1]=0.00336433; p3_pc3_m_dp[0][0][3][1]=-0.00135562; p4_pc3_m_dp[0][0][3][1]=0.0001818; 
  p0_pc3_m_dp[1][0][3][1]=0.00284688; p1_pc3_m_dp[1][0][3][1]=-0.00324525; p2_pc3_m_dp[1][0][3][1]=0.00336433; p3_pc3_m_dp[1][0][3][1]=-0.00135562; p4_pc3_m_dp[1][0][3][1]=0.0001818; 
  p0_pc3_s_dp[0][3][1]=0.0031865; p1_pc3_s_dp[0][3][1]=-0.00305301; p2_pc3_s_dp[0][3][1]=0.00351952; p3_pc3_s_dp[0][3][1]=-0.00115726; p4_pc3_s_dp[0][3][1]=0.0001457; 
  p0_pc3_m_dp[0][0][4][1]=0.00142915; p1_pc3_m_dp[0][0][4][1]=0.000286326; p2_pc3_m_dp[0][0][4][1]=0.000293172; p3_pc3_m_dp[0][0][4][1]=-0.000174604; p4_pc3_m_dp[0][0][4][1]=2.91448e-05; 
  p0_pc3_m_dp[1][0][4][1]=0.00142915; p1_pc3_m_dp[1][0][4][1]=0.000286326; p2_pc3_m_dp[1][0][4][1]=0.000293172; p3_pc3_m_dp[1][0][4][1]=-0.000174604; p4_pc3_m_dp[1][0][4][1]=2.91448e-05; 
  p0_pc3_s_dp[0][4][1]=0.00361455; p1_pc3_s_dp[0][4][1]=-0.0042986; p2_pc3_s_dp[0][4][1]=0.00487339; p3_pc3_s_dp[0][4][1]=-0.00170645; p4_pc3_s_dp[0][4][1]=0.000212091; 
  p0_pc3_m_dp[0][0][5][1]=0.00135414; p1_pc3_m_dp[0][0][5][1]=0.000339105; p2_pc3_m_dp[0][0][5][1]=-0.00041911; p3_pc3_m_dp[0][0][5][1]=0.000349023; p4_pc3_m_dp[0][0][5][1]=-7.03282e-05; 
  p0_pc3_m_dp[1][0][5][1]=0.00135414; p1_pc3_m_dp[1][0][5][1]=0.000339105; p2_pc3_m_dp[1][0][5][1]=-0.00041911; p3_pc3_m_dp[1][0][5][1]=0.000349023; p4_pc3_m_dp[1][0][5][1]=-7.03282e-05; 
  p0_pc3_s_dp[0][5][1]=0.00219941; p1_pc3_s_dp[0][5][1]=-0.000339291; p2_pc3_s_dp[0][5][1]=0.00138998; p3_pc3_s_dp[0][5][1]=-0.000456897; p4_pc3_s_dp[0][5][1]=5.40542e-05; 
  p0_pc3_m_dp[0][0][6][1]=0.000908287; p1_pc3_m_dp[0][0][6][1]=0.0014496; p2_pc3_m_dp[0][0][6][1]=-0.00150449; p3_pc3_m_dp[0][0][6][1]=0.000687039; p4_pc3_m_dp[0][0][6][1]=-0.000112149; 
  p0_pc3_m_dp[1][0][6][1]=0.000908287; p1_pc3_m_dp[1][0][6][1]=0.0014496; p2_pc3_m_dp[1][0][6][1]=-0.00150449; p3_pc3_m_dp[1][0][6][1]=0.000687039; p4_pc3_m_dp[1][0][6][1]=-0.000112149; 
  p0_pc3_s_dp[0][6][1]=0.00271316; p1_pc3_s_dp[0][6][1]=-0.00172121; p2_pc3_s_dp[0][6][1]=0.00259336; p3_pc3_s_dp[0][6][1]=-0.000918265; p4_pc3_s_dp[0][6][1]=0.000125941; 
  p0_pc3_m_dp[0][0][7][1]=0.0015805; p1_pc3_m_dp[0][0][7][1]=5.45411e-05; p2_pc3_m_dp[0][0][7][1]=-0.000736151; p3_pc3_m_dp[0][0][7][1]=0.000457078; p4_pc3_m_dp[0][0][7][1]=-0.000107641; 
  p0_pc3_m_dp[1][0][7][1]=0.0015805; p1_pc3_m_dp[1][0][7][1]=5.45411e-05; p2_pc3_m_dp[1][0][7][1]=-0.000736151; p3_pc3_m_dp[1][0][7][1]=0.000457078; p4_pc3_m_dp[1][0][7][1]=-0.000107641; 
  p0_pc3_s_dp[0][7][1]=0.00285525; p1_pc3_s_dp[0][7][1]=-0.00131722; p2_pc3_s_dp[0][7][1]=0.00133694; p3_pc3_s_dp[0][7][1]=-6.254e-05; p4_pc3_s_dp[0][7][1]=-3.82189e-05; 
  p0_pc3_m_dp[0][0][8][1]=0.00303106; p1_pc3_m_dp[0][0][8][1]=-0.00443913; p2_pc3_m_dp[0][0][8][1]=0.0039549; p3_pc3_m_dp[0][0][8][1]=-0.00181691; p4_pc3_m_dp[0][0][8][1]=0.000232666; 
  p0_pc3_m_dp[1][0][8][1]=0.00303106; p1_pc3_m_dp[1][0][8][1]=-0.00443913; p2_pc3_m_dp[1][0][8][1]=0.0039549; p3_pc3_m_dp[1][0][8][1]=-0.00181691; p4_pc3_m_dp[1][0][8][1]=0.000232666; 
  p0_pc3_s_dp[0][8][1]=0.000292938; p1_pc3_s_dp[0][8][1]=0.00715303; p2_pc3_s_dp[0][8][1]=-0.00831113; p3_pc3_s_dp[0][8][1]=0.0044414; p4_pc3_s_dp[0][8][1]=-0.00075742; 
  p0_pc3_m_dp[0][0][9][1]=0.00346061; p1_pc3_m_dp[0][0][9][1]=-0.00588792; p2_pc3_m_dp[0][0][9][1]=0.00535003; p3_pc3_m_dp[0][0][9][1]=-0.00278201; p4_pc3_m_dp[0][0][9][1]=0.000356764; 
  p0_pc3_m_dp[1][0][9][1]=0.00346061; p1_pc3_m_dp[1][0][9][1]=-0.00588792; p2_pc3_m_dp[1][0][9][1]=0.00535003; p3_pc3_m_dp[1][0][9][1]=-0.00278201; p4_pc3_m_dp[1][0][9][1]=0.000356764; 
  p0_pc3_s_dp[0][9][1]=0.000676859; p1_pc3_s_dp[0][9][1]=0.00642745; p2_pc3_s_dp[0][9][1]=-0.00821267; p3_pc3_s_dp[0][9][1]=0.00472728; p4_pc3_s_dp[0][9][1]=-0.000844191; 
  p0_pc3_m_dp[0][1][0][1]=0.000584835; p1_pc3_m_dp[0][1][0][1]=-0.0045312; p2_pc3_m_dp[0][1][0][1]=0.00472804; p3_pc3_m_dp[0][1][0][1]=-0.00277246; p4_pc3_m_dp[0][1][0][1]=0.00037864; 
  p0_pc3_m_dp[1][1][0][1]=0.000584835; p1_pc3_m_dp[1][1][0][1]=-0.0045312; p2_pc3_m_dp[1][1][0][1]=0.00472804; p3_pc3_m_dp[1][1][0][1]=-0.00277246; p4_pc3_m_dp[1][1][0][1]=0.00037864; 
  p0_pc3_s_dp[1][0][1]=-0.000306534; p1_pc3_s_dp[1][0][1]=0.00964183; p2_pc3_s_dp[1][0][1]=-0.010465; p3_pc3_s_dp[1][0][1]=0.00535948; p4_pc3_s_dp[1][0][1]=-0.00091261; 
  p0_pc3_m_dp[0][1][1][1]=-0.000338939; p1_pc3_m_dp[0][1][1][1]=-0.0014671; p2_pc3_m_dp[0][1][1][1]=0.00162627; p3_pc3_m_dp[0][1][1][1]=-0.00101632; p4_pc3_m_dp[0][1][1][1]=0.000125287; 
  p0_pc3_m_dp[1][1][1][1]=-0.000338939; p1_pc3_m_dp[1][1][1][1]=-0.0014671; p2_pc3_m_dp[1][1][1][1]=0.00162627; p3_pc3_m_dp[1][1][1][1]=-0.00101632; p4_pc3_m_dp[1][1][1][1]=0.000125287; 
  p0_pc3_s_dp[1][1][1]=-7.33411e-05; p1_pc3_s_dp[1][1][1]=0.00824833; p2_pc3_s_dp[1][1][1]=-0.00849976; p3_pc3_s_dp[1][1][1]=0.0043115; p4_pc3_s_dp[1][1][1]=-0.000730314; 
  p0_pc3_m_dp[0][1][2][1]=-0.000354611; p1_pc3_m_dp[0][1][2][1]=-0.00126129; p2_pc3_m_dp[0][1][2][1]=0.00124113; p3_pc3_m_dp[0][1][2][1]=-0.000528898; p4_pc3_m_dp[0][1][2][1]=4.18835e-05; 
  p0_pc3_m_dp[1][1][2][1]=-0.000354611; p1_pc3_m_dp[1][1][2][1]=-0.00126129; p2_pc3_m_dp[1][1][2][1]=0.00124113; p3_pc3_m_dp[1][1][2][1]=-0.000528898; p4_pc3_m_dp[1][1][2][1]=4.18835e-05; 
  p0_pc3_s_dp[1][2][1]=0.00280138; p1_pc3_s_dp[1][2][1]=-0.000943653; p2_pc3_s_dp[1][2][1]=0.00136339; p3_pc3_s_dp[1][2][1]=-7.99196e-05; p4_pc3_s_dp[1][2][1]=-5.05846e-05; 
  p0_pc3_m_dp[0][1][3][1]=0.000325034; p1_pc3_m_dp[0][1][3][1]=-0.00267384; p2_pc3_m_dp[0][1][3][1]=0.00245505; p3_pc3_m_dp[0][1][3][1]=-0.000869394; p4_pc3_m_dp[0][1][3][1]=9.80175e-05; 
  p0_pc3_m_dp[1][1][3][1]=0.000325034; p1_pc3_m_dp[1][1][3][1]=-0.00267384; p2_pc3_m_dp[1][1][3][1]=0.00245505; p3_pc3_m_dp[1][1][3][1]=-0.000869394; p4_pc3_m_dp[1][1][3][1]=9.80175e-05; 
  p0_pc3_s_dp[1][3][1]=0.00158367; p1_pc3_s_dp[1][3][1]=0.00161235; p2_pc3_s_dp[1][3][1]=-0.000498178; p3_pc3_s_dp[1][3][1]=0.000410851; p4_pc3_s_dp[1][3][1]=-8.56283e-05; 
  p0_pc3_m_dp[0][1][4][1]=0.000987073; p1_pc3_m_dp[0][1][4][1]=-0.00446008; p2_pc3_m_dp[0][1][4][1]=0.0043399; p3_pc3_m_dp[0][1][4][1]=-0.00160163; p4_pc3_m_dp[0][1][4][1]=0.000206894; 
  p0_pc3_m_dp[1][1][4][1]=0.000987073; p1_pc3_m_dp[1][1][4][1]=-0.00446008; p2_pc3_m_dp[1][1][4][1]=0.0043399; p3_pc3_m_dp[1][1][4][1]=-0.00160163; p4_pc3_m_dp[1][1][4][1]=0.000206894; 
  p0_pc3_s_dp[1][4][1]=0.00207827; p1_pc3_s_dp[1][4][1]=-0.000174554; p2_pc3_s_dp[1][4][1]=0.00157263; p3_pc3_s_dp[1][4][1]=-0.000544692; p4_pc3_s_dp[1][4][1]=6.06665e-05; 
  p0_pc3_m_dp[0][1][5][1]=-0.000654972; p1_pc3_m_dp[0][1][5][1]=0.000491339; p2_pc3_m_dp[0][1][5][1]=-0.000194161; p3_pc3_m_dp[0][1][5][1]=4.89051e-05; p4_pc3_m_dp[0][1][5][1]=-6.50299e-07; 
  p0_pc3_m_dp[1][1][5][1]=-0.000654972; p1_pc3_m_dp[1][1][5][1]=0.000491339; p2_pc3_m_dp[1][1][5][1]=-0.000194161; p3_pc3_m_dp[1][1][5][1]=4.89051e-05; p4_pc3_m_dp[1][1][5][1]=-6.50299e-07; 
  p0_pc3_s_dp[1][5][1]=0.00152827; p1_pc3_s_dp[1][5][1]=0.00183185; p2_pc3_s_dp[1][5][1]=-0.000494693; p3_pc3_s_dp[1][5][1]=0.00025157; p4_pc3_s_dp[1][5][1]=-3.94763e-05; 
  p0_pc3_m_dp[0][1][6][1]=0.00227433; p1_pc3_m_dp[0][1][6][1]=-0.00761439; p2_pc3_m_dp[0][1][6][1]=0.00735888; p3_pc3_m_dp[0][1][6][1]=-0.00290865; p4_pc3_m_dp[0][1][6][1]=0.000402158; 
  p0_pc3_m_dp[1][1][6][1]=0.00227433; p1_pc3_m_dp[1][1][6][1]=-0.00761439; p2_pc3_m_dp[1][1][6][1]=0.00735888; p3_pc3_m_dp[1][1][6][1]=-0.00290865; p4_pc3_m_dp[1][1][6][1]=0.000402158; 
  p0_pc3_s_dp[1][6][1]=0.0024453; p1_pc3_s_dp[1][6][1]=-0.000289433; p2_pc3_s_dp[1][6][1]=0.000985712; p3_pc3_s_dp[1][6][1]=-0.000161686; p4_pc3_s_dp[1][6][1]=1.59245e-06; 
  p0_pc3_m_dp[0][1][7][1]=-0.000154771; p1_pc3_m_dp[0][1][7][1]=-0.00206474; p2_pc3_m_dp[0][1][7][1]=0.00230757; p3_pc3_m_dp[0][1][7][1]=-0.00110933; p4_pc3_m_dp[0][1][7][1]=0.000152654; 
  p0_pc3_m_dp[1][1][7][1]=-0.000154771; p1_pc3_m_dp[1][1][7][1]=-0.00206474; p2_pc3_m_dp[1][1][7][1]=0.00230757; p3_pc3_m_dp[1][1][7][1]=-0.00110933; p4_pc3_m_dp[1][1][7][1]=0.000152654; 
  p0_pc3_s_dp[1][7][1]=0.00135659; p1_pc3_s_dp[1][7][1]=0.00355748; p2_pc3_s_dp[1][7][1]=-0.00361798; p3_pc3_s_dp[1][7][1]=0.00204101; p4_pc3_s_dp[1][7][1]=-0.000353998; 
  p0_pc3_m_dp[0][1][8][1]=0.00155162; p1_pc3_m_dp[0][1][8][1]=-0.00754399; p2_pc3_m_dp[0][1][8][1]=0.00739681; p3_pc3_m_dp[0][1][8][1]=-0.00329183; p4_pc3_m_dp[0][1][8][1]=0.000457708; 
  p0_pc3_m_dp[1][1][8][1]=0.00155162; p1_pc3_m_dp[1][1][8][1]=-0.00754399; p2_pc3_m_dp[1][1][8][1]=0.00739681; p3_pc3_m_dp[1][1][8][1]=-0.00329183; p4_pc3_m_dp[1][1][8][1]=0.000457708; 
  p0_pc3_s_dp[1][8][1]=0.00165934; p1_pc3_s_dp[1][8][1]=0.00313642; p2_pc3_s_dp[1][8][1]=-0.00404278; p3_pc3_s_dp[1][8][1]=0.00261652; p4_pc3_s_dp[1][8][1]=-0.000489878; 
  p0_pc3_m_dp[0][1][9][1]=0.000322442; p1_pc3_m_dp[0][1][9][1]=-0.00503944; p2_pc3_m_dp[0][1][9][1]=0.00528619; p3_pc3_m_dp[0][1][9][1]=-0.00293312; p4_pc3_m_dp[0][1][9][1]=0.000405068; 
  p0_pc3_m_dp[1][1][9][1]=0.000322442; p1_pc3_m_dp[1][1][9][1]=-0.00503944; p2_pc3_m_dp[1][1][9][1]=0.00528619; p3_pc3_m_dp[1][1][9][1]=-0.00293312; p4_pc3_m_dp[1][1][9][1]=0.000405068; 
  p0_pc3_s_dp[1][9][1]=0.000536043; p1_pc3_s_dp[1][9][1]=0.00713295; p2_pc3_s_dp[1][9][1]=-0.00892947; p3_pc3_s_dp[1][9][1]=0.00505669; p4_pc3_s_dp[1][9][1]=-0.000899813; 
    
  //PC3/Negative charge/dPhi/
  //Writing the data out for Neg 
	
  p0_pc3_m_dp[0][0][0][0]=-0.00211966; p1_pc3_m_dp[0][0][0][0]=0.00918156; p2_pc3_m_dp[0][0][0][0]=-0.00908686; p3_pc3_m_dp[0][0][0][0]=0.00442123; p4_pc3_m_dp[0][0][0][0]=-0.000602122; 
  p0_pc3_m_dp[1][0][0][0]=-0.00211966; p1_pc3_m_dp[1][0][0][0]=0.00918156; p2_pc3_m_dp[1][0][0][0]=-0.00908686; p3_pc3_m_dp[1][0][0][0]=0.00442123; p4_pc3_m_dp[1][0][0][0]=-0.000602122; 
  p0_pc3_s_dp[0][0][0]=0.000115594; p1_pc3_s_dp[0][0][0]=0.00768364; p2_pc3_s_dp[0][0][0]=-0.00947977; p3_pc3_s_dp[0][0][0]=0.00510206; p4_pc3_s_dp[0][0][0]=-0.000865583; 
  p0_pc3_m_dp[0][0][1][0]=-3.26938e-05; p1_pc3_m_dp[0][0][1][0]=0.00295064; p2_pc3_m_dp[0][0][1][0]=-0.00288905; p3_pc3_m_dp[0][0][1][0]=0.00137899; p4_pc3_m_dp[0][0][1][0]=-0.000158684; 
  p0_pc3_m_dp[1][0][1][0]=-3.26938e-05; p1_pc3_m_dp[1][0][1][0]=0.00295064; p2_pc3_m_dp[1][0][1][0]=-0.00288905; p3_pc3_m_dp[1][0][1][0]=0.00137899; p4_pc3_m_dp[1][0][1][0]=-0.000158684; 
  p0_pc3_s_dp[0][1][0]=-0.00060005; p1_pc3_s_dp[0][1][0]=0.00895742; p2_pc3_s_dp[0][1][0]=-0.00990051; p3_pc3_s_dp[0][1][0]=0.00490274; p4_pc3_s_dp[0][1][0]=-0.000788156; 
  p0_pc3_m_dp[0][0][2][0]=-2.73444e-05; p1_pc3_m_dp[0][0][2][0]=0.00216574; p2_pc3_m_dp[0][0][2][0]=-0.00180581; p3_pc3_m_dp[0][0][2][0]=0.000602426; p4_pc3_m_dp[0][0][2][0]=-3.89189e-05; 
  p0_pc3_m_dp[1][0][2][0]=-2.73444e-05; p1_pc3_m_dp[1][0][2][0]=0.00216574; p2_pc3_m_dp[1][0][2][0]=-0.00180581; p3_pc3_m_dp[1][0][2][0]=0.000602426; p4_pc3_m_dp[1][0][2][0]=-3.89189e-05; 
  p0_pc3_s_dp[0][2][0]=0.000857188; p1_pc3_s_dp[0][2][0]=0.00371524; p2_pc3_s_dp[0][2][0]=-0.00355104; p3_pc3_s_dp[0][2][0]=0.00173874; p4_pc3_s_dp[0][2][0]=-0.000255671; 
  p0_pc3_m_dp[0][0][3][0]=0.000325925; p1_pc3_m_dp[0][0][3][0]=0.000963766; p2_pc3_m_dp[0][0][3][0]=-0.00064847; p3_pc3_m_dp[0][0][3][0]=2.65184e-05; p4_pc3_m_dp[0][0][3][0]=3.12257e-05; 
  p0_pc3_m_dp[1][0][3][0]=0.000325925; p1_pc3_m_dp[1][0][3][0]=0.000963766; p2_pc3_m_dp[1][0][3][0]=-0.00064847; p3_pc3_m_dp[1][0][3][0]=2.65184e-05; p4_pc3_m_dp[1][0][3][0]=3.12257e-05; 
  p0_pc3_s_dp[0][3][0]=0.000981962; p1_pc3_s_dp[0][3][0]=0.00297651; p2_pc3_s_dp[0][3][0]=-0.0023752; p3_pc3_s_dp[0][3][0]=0.00107127; p4_pc3_s_dp[0][3][0]=-0.000139951; 
  p0_pc3_m_dp[0][0][4][0]=0.000838267; p1_pc3_m_dp[0][0][4][0]=-0.000154763; p2_pc3_m_dp[0][0][4][0]=4.16049e-05; p3_pc3_m_dp[0][0][4][0]=-0.000225789; p4_pc3_m_dp[0][0][4][0]=6.19171e-05; 
  p0_pc3_m_dp[1][0][4][0]=0.000838267; p1_pc3_m_dp[1][0][4][0]=-0.000154763; p2_pc3_m_dp[1][0][4][0]=4.16049e-05; p3_pc3_m_dp[1][0][4][0]=-0.000225789; p4_pc3_m_dp[1][0][4][0]=6.19171e-05; 
  p0_pc3_s_dp[0][4][0]=0.00180497; p1_pc3_s_dp[0][4][0]=0.000733848; p2_pc3_s_dp[0][4][0]=-0.000337307; p3_pc3_s_dp[0][4][0]=0.000344512; p4_pc3_s_dp[0][4][0]=-5.91419e-05; 
  p0_pc3_m_dp[0][0][5][0]=-0.00367378; p1_pc3_m_dp[0][0][5][0]=0.0133178; p2_pc3_m_dp[0][0][5][0]=-0.014188; p3_pc3_m_dp[0][0][5][0]=0.00589863; p4_pc3_m_dp[0][0][5][0]=-0.000851996; 
  p0_pc3_m_dp[1][0][5][0]=-0.00367378; p1_pc3_m_dp[1][0][5][0]=0.0133178; p2_pc3_m_dp[1][0][5][0]=-0.014188; p3_pc3_m_dp[1][0][5][0]=0.00589863; p4_pc3_m_dp[1][0][5][0]=-0.000851996; 
  p0_pc3_s_dp[0][5][0]=-0.0164633; p1_pc3_s_dp[0][5][0]=0.0624895; p2_pc3_s_dp[0][5][0]=-0.0699868; p3_pc3_s_dp[0][5][0]=0.0312017; p4_pc3_s_dp[0][5][0]=-0.00470115; 
  p0_pc3_m_dp[0][0][6][0]=-0.00038058; p1_pc3_m_dp[0][0][6][0]=0.00256041; p2_pc3_m_dp[0][0][6][0]=-0.00201655; p3_pc3_m_dp[0][0][6][0]=0.000560068; p4_pc3_m_dp[0][0][6][0]=-4.47431e-05; 
  p0_pc3_m_dp[1][0][6][0]=-0.00038058; p1_pc3_m_dp[1][0][6][0]=0.00256041; p2_pc3_m_dp[1][0][6][0]=-0.00201655; p3_pc3_m_dp[1][0][6][0]=0.000560068; p4_pc3_m_dp[1][0][6][0]=-4.47431e-05; 
  p0_pc3_s_dp[0][6][0]=0.00168449; p1_pc3_s_dp[0][6][0]=0.000972135; p2_pc3_s_dp[0][6][0]=-7.24812e-05; p3_pc3_s_dp[0][6][0]=-1.10988e-05; p4_pc3_s_dp[0][6][0]=3.23679e-05; 
  p0_pc3_m_dp[0][0][7][0]=0.000287774; p1_pc3_m_dp[0][0][7][0]=0.00149952; p2_pc3_m_dp[0][0][7][0]=-0.00121972; p3_pc3_m_dp[0][0][7][0]=0.00046232; p4_pc3_m_dp[0][0][7][0]=-3.79096e-05; 
  p0_pc3_m_dp[1][0][7][0]=0.000287774; p1_pc3_m_dp[1][0][7][0]=0.00149952; p2_pc3_m_dp[1][0][7][0]=-0.00121972; p3_pc3_m_dp[1][0][7][0]=0.00046232; p4_pc3_m_dp[1][0][7][0]=-3.79096e-05; 
  p0_pc3_s_dp[0][7][0]=0.000817896; p1_pc3_s_dp[0][7][0]=0.00430492; p2_pc3_s_dp[0][7][0]=-0.00416698; p3_pc3_s_dp[0][7][0]=0.0019675; p4_pc3_s_dp[0][7][0]=-0.000284978; 
  p0_pc3_m_dp[0][0][8][0]=-0.00154853; p1_pc3_m_dp[0][0][8][0]=0.00739461; p2_pc3_m_dp[0][0][8][0]=-0.00701757; p3_pc3_m_dp[0][0][8][0]=0.0031068; p4_pc3_m_dp[0][0][8][0]=-0.000425053; 
  p0_pc3_m_dp[1][0][8][0]=-0.00154853; p1_pc3_m_dp[1][0][8][0]=0.00739461; p2_pc3_m_dp[1][0][8][0]=-0.00701757; p3_pc3_m_dp[1][0][8][0]=0.0031068; p4_pc3_m_dp[1][0][8][0]=-0.000425053; 
  p0_pc3_s_dp[0][8][0]=-0.000966148; p1_pc3_s_dp[0][8][0]=0.0109121; p2_pc3_s_dp[0][8][0]=-0.0122281; p3_pc3_s_dp[0][8][0]=0.0059079; p4_pc3_s_dp[0][8][0]=-0.000933151; 
  p0_pc3_m_dp[0][0][9][0]=-0.0021769; p1_pc3_m_dp[0][0][9][0]=0.0102595; p2_pc3_m_dp[0][0][9][0]=-0.0103234; p3_pc3_m_dp[0][0][9][0]=0.00506799; p4_pc3_m_dp[0][0][9][0]=-0.000716052; 
  p0_pc3_m_dp[1][0][9][0]=-0.0021769; p1_pc3_m_dp[1][0][9][0]=0.0102595; p2_pc3_m_dp[1][0][9][0]=-0.0103234; p3_pc3_m_dp[1][0][9][0]=0.00506799; p4_pc3_m_dp[1][0][9][0]=-0.000716052; 
  p0_pc3_s_dp[0][9][0]=-0.000771505; p1_pc3_s_dp[0][9][0]=0.0112305; p2_pc3_s_dp[0][9][0]=-0.0134423; p3_pc3_s_dp[0][9][0]=0.0068476; p4_pc3_s_dp[0][9][0]=-0.00112987; 
  p0_pc3_m_dp[0][1][0][0]=-0.00186592; p1_pc3_m_dp[0][1][0][0]=0.000881992; p2_pc3_m_dp[0][1][0][0]=-0.000719728; p3_pc3_m_dp[0][1][0][0]=0.000976143; p4_pc3_m_dp[0][1][0][0]=-9.59136e-05; 
  p0_pc3_m_dp[1][1][0][0]=-0.00186592; p1_pc3_m_dp[1][1][0][0]=0.000881992; p2_pc3_m_dp[1][1][0][0]=-0.000719728; p3_pc3_m_dp[1][1][0][0]=0.000976143; p4_pc3_m_dp[1][1][0][0]=-9.59136e-05; 
  p0_pc3_s_dp[1][0][0]=-0.000563373; p1_pc3_s_dp[1][0][0]=0.0116959; p2_pc3_s_dp[1][0][0]=-0.0136452; p3_pc3_s_dp[1][0][0]=0.00691681; p4_pc3_s_dp[1][0][0]=-0.00115232; 
  p0_pc3_m_dp[0][1][1][0]=-0.00288892; p1_pc3_m_dp[0][1][1][0]=0.00309129; p2_pc3_m_dp[0][1][1][0]=-0.00267398; p3_pc3_m_dp[0][1][1][0]=0.00123259; p4_pc3_m_dp[0][1][1][0]=-0.000130184; 
  p0_pc3_m_dp[1][1][1][0]=-0.00288892; p1_pc3_m_dp[1][1][1][0]=0.00309129; p2_pc3_m_dp[1][1][1][0]=-0.00267398; p3_pc3_m_dp[1][1][1][0]=0.00123259; p4_pc3_m_dp[1][1][1][0]=-0.000130184; 
  p0_pc3_s_dp[1][1][0]=0.0043045; p1_pc3_s_dp[1][1][0]=-0.00311089; p2_pc3_s_dp[1][1][0]=0.00190424; p3_pc3_s_dp[1][1][0]=0.00013697; p4_pc3_s_dp[1][1][0]=-0.000122386; 
  p0_pc3_m_dp[0][1][2][0]=-0.00315688; p1_pc3_m_dp[0][1][2][0]=0.00298387; p2_pc3_m_dp[0][1][2][0]=-0.00230575; p3_pc3_m_dp[0][1][2][0]=0.000718544; p4_pc3_m_dp[0][1][2][0]=-3.9348e-05; 
  p0_pc3_m_dp[1][1][2][0]=-0.00315688; p1_pc3_m_dp[1][1][2][0]=0.00298387; p2_pc3_m_dp[1][1][2][0]=-0.00230575; p3_pc3_m_dp[1][1][2][0]=0.000718544; p4_pc3_m_dp[1][1][2][0]=-3.9348e-05; 
  p0_pc3_s_dp[1][2][0]=0.00371743; p1_pc3_s_dp[1][2][0]=-0.0020534; p2_pc3_s_dp[1][2][0]=0.0014316; p3_pc3_s_dp[1][2][0]=4.87117e-06; p4_pc3_s_dp[1][2][0]=-5.47841e-05; 
  p0_pc3_m_dp[0][1][3][0]=-0.00293552; p1_pc3_m_dp[0][1][3][0]=0.00200112; p2_pc3_m_dp[0][1][3][0]=-0.00129117; p3_pc3_m_dp[0][1][3][0]=0.000210335; p4_pc3_m_dp[0][1][3][0]=1.87918e-05; 
  p0_pc3_m_dp[1][1][3][0]=-0.00293552; p1_pc3_m_dp[1][1][3][0]=0.00200112; p2_pc3_m_dp[1][1][3][0]=-0.00129117; p3_pc3_m_dp[1][1][3][0]=0.000210335; p4_pc3_m_dp[1][1][3][0]=1.87918e-05; 
  p0_pc3_s_dp[1][3][0]=0.00280482; p1_pc3_s_dp[1][3][0]=-0.000529595; p2_pc3_s_dp[1][3][0]=0.000827429; p3_pc3_s_dp[1][3][0]=-0.000146203; p4_pc3_s_dp[1][3][0]=2.04651e-05; 
  p0_pc3_m_dp[0][1][4][0]=-0.00157941; p1_pc3_m_dp[0][1][4][0]=-0.00258761; p2_pc3_m_dp[0][1][4][0]=0.00377537; p3_pc3_m_dp[0][1][4][0]=-0.00209491; p4_pc3_m_dp[0][1][4][0]=0.000362043; 
  p0_pc3_m_dp[1][1][4][0]=-0.00157941; p1_pc3_m_dp[1][1][4][0]=-0.00258761; p2_pc3_m_dp[1][1][4][0]=0.00377537; p3_pc3_m_dp[1][1][4][0]=-0.00209491; p4_pc3_m_dp[1][1][4][0]=0.000362043; 
  p0_pc3_s_dp[1][4][0]=0.00278871; p1_pc3_s_dp[1][4][0]=-0.000643195; p2_pc3_s_dp[1][4][0]=0.00114879; p3_pc3_s_dp[1][4][0]=-0.000359895; p4_pc3_s_dp[1][4][0]=5.68308e-05; 
  p0_pc3_m_dp[0][1][5][0]=-0.000442001; p1_pc3_m_dp[0][1][5][0]=-0.00536167; p2_pc3_m_dp[0][1][5][0]=0.00715516; p3_pc3_m_dp[0][1][5][0]=-0.00368871; p4_pc3_m_dp[0][1][5][0]=0.00061329; 
  p0_pc3_m_dp[1][1][5][0]=-0.000442001; p1_pc3_m_dp[1][1][5][0]=-0.00536167; p2_pc3_m_dp[1][1][5][0]=0.00715516; p3_pc3_m_dp[1][1][5][0]=-0.00368871; p4_pc3_m_dp[1][1][5][0]=0.00061329; 
  p0_pc3_s_dp[1][5][0]=0.00153189; p1_pc3_s_dp[1][5][0]=0.00357871; p2_pc3_s_dp[1][5][0]=-0.00318414; p3_pc3_s_dp[1][5][0]=0.00145702; p4_pc3_s_dp[1][5][0]=-0.000212832; 
  p0_pc3_m_dp[0][1][6][0]=-0.00169222; p1_pc3_m_dp[0][1][6][0]=-0.00101634; p2_pc3_m_dp[0][1][6][0]=0.00249091; p3_pc3_m_dp[0][1][6][0]=-0.00159902; p4_pc3_m_dp[0][1][6][0]=0.000300101; 
  p0_pc3_m_dp[1][1][6][0]=-0.00169222; p1_pc3_m_dp[1][1][6][0]=-0.00101634; p2_pc3_m_dp[1][1][6][0]=0.00249091; p3_pc3_m_dp[1][1][6][0]=-0.00159902; p4_pc3_m_dp[1][1][6][0]=0.000300101; 
  p0_pc3_s_dp[1][6][0]=0.00295421; p1_pc3_s_dp[1][6][0]=-0.00101243; p2_pc3_s_dp[1][6][0]=0.00163068; p3_pc3_s_dp[1][6][0]=-0.00063074; p4_pc3_s_dp[1][6][0]=0.000110118; 
  p0_pc3_m_dp[0][1][7][0]=-0.00290009; p1_pc3_m_dp[0][1][7][0]=0.00263533; p2_pc3_m_dp[0][1][7][0]=-0.00120719; p3_pc3_m_dp[0][1][7][0]=8.11836e-05; p4_pc3_m_dp[0][1][7][0]=6.0421e-05; 
  p0_pc3_m_dp[1][1][7][0]=-0.00290009; p1_pc3_m_dp[1][1][7][0]=0.00263533; p2_pc3_m_dp[1][1][7][0]=-0.00120719; p3_pc3_m_dp[1][1][7][0]=8.11836e-05; p4_pc3_m_dp[1][1][7][0]=6.0421e-05; 
  p0_pc3_s_dp[1][7][0]=0.000324628; p1_pc3_s_dp[1][7][0]=0.00768887; p2_pc3_s_dp[1][7][0]=-0.00810444; p3_pc3_s_dp[1][7][0]=0.00376465; p4_pc3_s_dp[1][7][0]=-0.000569326; 
  p0_pc3_m_dp[0][1][8][0]=-0.00313979; p1_pc3_m_dp[0][1][8][0]=0.00363162; p2_pc3_m_dp[0][1][8][0]=-0.00253519; p3_pc3_m_dp[0][1][8][0]=0.00100545; p4_pc3_m_dp[0][1][8][0]=-9.35029e-05; 
  p0_pc3_m_dp[1][1][8][0]=-0.00313979; p1_pc3_m_dp[1][1][8][0]=0.00363162; p2_pc3_m_dp[1][1][8][0]=-0.00253519; p3_pc3_m_dp[1][1][8][0]=0.00100545; p4_pc3_m_dp[1][1][8][0]=-9.35029e-05; 
  p0_pc3_s_dp[1][8][0]=0.000427507; p1_pc3_s_dp[1][8][0]=0.00819278; p2_pc3_s_dp[1][8][0]=-0.00981851; p3_pc3_s_dp[1][8][0]=0.00502223; p4_pc3_s_dp[1][8][0]=-0.000822656; 
  p0_pc3_m_dp[0][1][9][0]=-0.00378857; p1_pc3_m_dp[0][1][9][0]=0.00612159; p2_pc3_m_dp[0][1][9][0]=-0.00548163; p3_pc3_m_dp[0][1][9][0]=0.00281218; p4_pc3_m_dp[0][1][9][0]=-0.000359975; 
  p0_pc3_m_dp[1][1][9][0]=-0.00378857; p1_pc3_m_dp[1][1][9][0]=0.00612159; p2_pc3_m_dp[1][1][9][0]=-0.00548163; p3_pc3_m_dp[1][1][9][0]=0.00281218; p4_pc3_m_dp[1][1][9][0]=-0.000359975; 
  p0_pc3_s_dp[1][9][0]=-0.000150936; p1_pc3_s_dp[1][9][0]=0.00998231; p2_pc3_s_dp[1][9][0]=-0.0121796; p3_pc3_s_dp[1][9][0]=0.00632924; p4_pc3_s_dp[1][9][0]=-0.00105892; 

  //PC3/Positive charge/dZ/
  //Writing the data out for Pos 
	
  p0_pc3_m_dz[0][0][0][1]=0.518608; p1_pc3_m_dz[0][0][0][1]=0.21798; p2_pc3_m_dz[0][0][0][1]=-0.251102; p3_pc3_m_dz[0][0][0][1]=0.114294; p4_pc3_m_dz[0][0][0][1]=-0.0392871; 
  p0_pc3_m_dz[1][0][0][1]=0.518608; p1_pc3_m_dz[1][0][0][1]=0.21798; p2_pc3_m_dz[1][0][0][1]=-0.251102; p3_pc3_m_dz[1][0][0][1]=0.114294; p4_pc3_m_dz[1][0][0][1]=-0.0392871; 
  p0_pc3_s_dz[0][0][1]=1.60686; p1_pc3_s_dz[0][0][1]=-0.470253; p2_pc3_s_dz[0][0][1]=1.00112; p3_pc3_s_dz[0][0][1]=-0.385574; p4_pc3_s_dz[0][0][1]=0.0651207; 
  p0_pc3_m_dz[0][0][1][1]=0.442524; p1_pc3_m_dz[0][0][1][1]=0.431016; p2_pc3_m_dz[0][0][1][1]=-0.507251; p3_pc3_m_dz[0][0][1][1]=0.281599; p4_pc3_m_dz[0][0][1][1]=-0.0573548; 
  p0_pc3_m_dz[1][0][1][1]=0.442524; p1_pc3_m_dz[1][0][1][1]=0.431016; p2_pc3_m_dz[1][0][1][1]=-0.507251; p3_pc3_m_dz[1][0][1][1]=0.281599; p4_pc3_m_dz[1][0][1][1]=-0.0573548; 
  p0_pc3_s_dz[0][1][1]=2.26456; p1_pc3_s_dz[0][1][1]=-2.47998; p2_pc3_s_dz[0][1][1]=3.06071; p3_pc3_s_dz[0][1][1]=-1.20732; p4_pc3_s_dz[0][1][1]=0.169472; 
  p0_pc3_m_dz[0][0][2][1]=0.102051; p1_pc3_m_dz[0][0][2][1]=1.24666; p2_pc3_m_dz[0][0][2][1]=-1.1752; p3_pc3_m_dz[0][0][2][1]=0.477994; p4_pc3_m_dz[0][0][2][1]=-0.0706645; 
  p0_pc3_m_dz[1][0][2][1]=0.102051; p1_pc3_m_dz[1][0][2][1]=1.24666; p2_pc3_m_dz[1][0][2][1]=-1.1752; p3_pc3_m_dz[1][0][2][1]=0.477994; p4_pc3_m_dz[1][0][2][1]=-0.0706645; 
  p0_pc3_s_dz[0][2][1]=1.18057; p1_pc3_s_dz[0][2][1]=0.7251; p2_pc3_s_dz[0][2][1]=-0.259513; p3_pc3_s_dz[0][2][1]=0.203318; p4_pc3_s_dz[0][2][1]=-0.0412743; 
  p0_pc3_m_dz[0][0][3][1]=0.167487; p1_pc3_m_dz[0][0][3][1]=0.958094; p2_pc3_m_dz[0][0][3][1]=-0.893619; p3_pc3_m_dz[0][0][3][1]=0.351691; p4_pc3_m_dz[0][0][3][1]=-0.0504631; 
  p0_pc3_m_dz[1][0][3][1]=0.167487; p1_pc3_m_dz[1][0][3][1]=0.958094; p2_pc3_m_dz[1][0][3][1]=-0.893619; p3_pc3_m_dz[1][0][3][1]=0.351691; p4_pc3_m_dz[1][0][3][1]=-0.0504631; 
  p0_pc3_s_dz[0][3][1]=1.73032; p1_pc3_s_dz[0][3][1]=-0.76844; p2_pc3_s_dz[0][3][1]=1.1732; p3_pc3_s_dz[0][3][1]=-0.368685; p4_pc3_s_dz[0][3][1]=0.0392675; 
  p0_pc3_m_dz[0][0][4][1]=0.804582; p1_pc3_m_dz[0][0][4][1]=-0.584238; p2_pc3_m_dz[0][0][4][1]=0.40369; p3_pc3_m_dz[0][0][4][1]=-0.125671; p4_pc3_m_dz[0][0][4][1]=0.0136096; 
  p0_pc3_m_dz[1][0][4][1]=0.804582; p1_pc3_m_dz[1][0][4][1]=-0.584238; p2_pc3_m_dz[1][0][4][1]=0.40369; p3_pc3_m_dz[1][0][4][1]=-0.125671; p4_pc3_m_dz[1][0][4][1]=0.0136096; 
  p0_pc3_s_dz[0][4][1]=1.95723; p1_pc3_s_dz[0][4][1]=-1.61051; p2_pc3_s_dz[0][4][1]=2.11409; p3_pc3_s_dz[0][4][1]=-0.811364; p4_pc3_s_dz[0][4][1]=0.110654; 
  p0_pc3_m_dz[0][0][5][1]=0.242139; p1_pc3_m_dz[0][0][5][1]=1.09377; p2_pc3_m_dz[0][0][5][1]=-1.39893; p3_pc3_m_dz[0][0][5][1]=0.636072; p4_pc3_m_dz[0][0][5][1]=-0.0987857; 
  p0_pc3_m_dz[1][0][5][1]=0.242139; p1_pc3_m_dz[1][0][5][1]=1.09377; p2_pc3_m_dz[1][0][5][1]=-1.39893; p3_pc3_m_dz[1][0][5][1]=0.636072; p4_pc3_m_dz[1][0][5][1]=-0.0987857; 
  p0_pc3_s_dz[0][5][1]=1.50988; p1_pc3_s_dz[0][5][1]=0.129705; p2_pc3_s_dz[0][5][1]=0.0821923; p3_pc3_s_dz[0][5][1]=0.136212; p4_pc3_s_dz[0][5][1]=-0.0390232; 
  p0_pc3_m_dz[0][0][6][1]=0.256242; p1_pc3_m_dz[0][0][6][1]=0.802892; p2_pc3_m_dz[0][0][6][1]=-0.796161; p3_pc3_m_dz[0][0][6][1]=0.293853; p4_pc3_m_dz[0][0][6][1]=-0.038842; 
  p0_pc3_m_dz[1][0][6][1]=0.256242; p1_pc3_m_dz[1][0][6][1]=0.802892; p2_pc3_m_dz[1][0][6][1]=-0.796161; p3_pc3_m_dz[1][0][6][1]=0.293853; p4_pc3_m_dz[1][0][6][1]=-0.038842; 
  p0_pc3_s_dz[0][6][1]=1.76833; p1_pc3_s_dz[0][6][1]=-0.936849; p2_pc3_s_dz[0][6][1]=1.42855; p3_pc3_s_dz[0][6][1]=-0.514337; p4_pc3_s_dz[0][6][1]=0.0667531; 
  p0_pc3_m_dz[0][0][7][1]=0.281529; p1_pc3_m_dz[0][0][7][1]=1.0692; p2_pc3_m_dz[0][0][7][1]=-1.14768; p3_pc3_m_dz[0][0][7][1]=0.437327; p4_pc3_m_dz[0][0][7][1]=-0.0579669; 
  p0_pc3_m_dz[1][0][7][1]=0.281529; p1_pc3_m_dz[1][0][7][1]=1.0692; p2_pc3_m_dz[1][0][7][1]=-1.14768; p3_pc3_m_dz[1][0][7][1]=0.437327; p4_pc3_m_dz[1][0][7][1]=-0.0579669; 
  p0_pc3_s_dz[0][7][1]=1.49776; p1_pc3_s_dz[0][7][1]=-0.0519641; p2_pc3_s_dz[0][7][1]=0.51872; p3_pc3_s_dz[0][7][1]=-0.147892; p4_pc3_s_dz[0][7][1]=0.0169716; 
  p0_pc3_m_dz[0][0][8][1]=0.619256; p1_pc3_m_dz[0][0][8][1]=0.255709; p2_pc3_m_dz[0][0][8][1]=-0.324795; p3_pc3_m_dz[0][0][8][1]=0.0891018; p4_pc3_m_dz[0][0][8][1]=-0.00312492; 
  p0_pc3_m_dz[1][0][8][1]=0.619256; p1_pc3_m_dz[1][0][8][1]=0.255709; p2_pc3_m_dz[1][0][8][1]=-0.324795; p3_pc3_m_dz[1][0][8][1]=0.0891018; p4_pc3_m_dz[1][0][8][1]=-0.00312492; 
  p0_pc3_s_dz[0][8][1]=1.15453; p1_pc3_s_dz[0][8][1]=0.96706; p2_pc3_s_dz[0][8][1]=-0.492322; p3_pc3_s_dz[0][8][1]=0.265905; p4_pc3_s_dz[0][8][1]=-0.0418961; 
  p0_pc3_m_dz[0][0][9][1]=1.00107; p1_pc3_m_dz[0][0][9][1]=-0.678645; p2_pc3_m_dz[0][0][9][1]=0.64282; p3_pc3_m_dz[0][0][9][1]=-0.303468; p4_pc3_m_dz[0][0][9][1]=0.0679656; 
  p0_pc3_m_dz[1][0][9][1]=1.00107; p1_pc3_m_dz[1][0][9][1]=-0.678645; p2_pc3_m_dz[1][0][9][1]=0.64282; p3_pc3_m_dz[1][0][9][1]=-0.303468; p4_pc3_m_dz[1][0][9][1]=0.0679656; 
  p0_pc3_s_dz[0][9][1]=2.07373; p1_pc3_s_dz[0][9][1]=-2.23776; p2_pc3_s_dz[0][9][1]=3.02435; p3_pc3_s_dz[0][9][1]=-1.26778; p4_pc3_s_dz[0][9][1]=0.193912; 
  p0_pc3_m_dz[0][1][0][1]=2.37405; p1_pc3_m_dz[0][1][0][1]=-0.780738; p2_pc3_m_dz[0][1][0][1]=0.968251; p3_pc3_m_dz[0][1][0][1]=-0.375845; p4_pc3_m_dz[0][1][0][1]=0.0265662; 
  p0_pc3_m_dz[1][1][0][1]=2.37405; p1_pc3_m_dz[1][1][0][1]=-0.780738; p2_pc3_m_dz[1][1][0][1]=0.968251; p3_pc3_m_dz[1][1][0][1]=-0.375845; p4_pc3_m_dz[1][1][0][1]=0.0265662; 
  p0_pc3_s_dz[1][0][1]=2.16654; p1_pc3_s_dz[1][0][1]=-2.31024; p2_pc3_s_dz[1][0][1]=3.02901; p3_pc3_s_dz[1][0][1]=-1.23049; p4_pc3_s_dz[1][0][1]=0.184653; 
  p0_pc3_m_dz[0][1][1][1]=2.26828; p1_pc3_m_dz[0][1][1][1]=-0.551523; p2_pc3_m_dz[0][1][1][1]=0.7028; p3_pc3_m_dz[0][1][1][1]=-0.278722; p4_pc3_m_dz[0][1][1][1]=0.0337423; 
  p0_pc3_m_dz[1][1][1][1]=2.26828; p1_pc3_m_dz[1][1][1][1]=-0.551523; p2_pc3_m_dz[1][1][1][1]=0.7028; p3_pc3_m_dz[1][1][1][1]=-0.278722; p4_pc3_m_dz[1][1][1][1]=0.0337423; 
  p0_pc3_s_dz[1][1][1]=1.81703; p1_pc3_s_dz[1][1][1]=-1.23305; p2_pc3_s_dz[1][1][1]=1.89953; p3_pc3_s_dz[1][1][1]=-0.745003; p4_pc3_s_dz[1][1][1]=0.10621; 
  p0_pc3_m_dz[0][1][2][1]=2.16335; p1_pc3_m_dz[0][1][2][1]=-0.811755; p2_pc3_m_dz[0][1][2][1]=0.922178; p3_pc3_m_dz[0][1][2][1]=-0.335609; p4_pc3_m_dz[0][1][2][1]=0.0390922; 
  p0_pc3_m_dz[1][1][2][1]=2.16335; p1_pc3_m_dz[1][1][2][1]=-0.811755; p2_pc3_m_dz[1][1][2][1]=0.922178; p3_pc3_m_dz[1][1][2][1]=-0.335609; p4_pc3_m_dz[1][1][2][1]=0.0390922; 
  p0_pc3_s_dz[1][2][1]=2.06256; p1_pc3_s_dz[1][2][1]=-1.78971; p2_pc3_s_dz[1][2][1]=2.39214; p3_pc3_s_dz[1][2][1]=-0.946069; p4_pc3_s_dz[1][2][1]=0.13618; 
  p0_pc3_m_dz[0][1][3][1]=2.45803; p1_pc3_m_dz[0][1][3][1]=-1.68113; p2_pc3_m_dz[0][1][3][1]=1.43542; p3_pc3_m_dz[0][1][3][1]=-0.480734; p4_pc3_m_dz[0][1][3][1]=0.0557593; 
  p0_pc3_m_dz[1][1][3][1]=2.45803; p1_pc3_m_dz[1][1][3][1]=-1.68113; p2_pc3_m_dz[1][1][3][1]=1.43542; p3_pc3_m_dz[1][1][3][1]=-0.480734; p4_pc3_m_dz[1][1][3][1]=0.0557593; 
  p0_pc3_s_dz[1][3][1]=1.7505; p1_pc3_s_dz[1][3][1]=-0.984436; p2_pc3_s_dz[1][3][1]=1.67838; p3_pc3_s_dz[1][3][1]=-0.676192; p4_pc3_s_dz[1][3][1]=0.0976812; 
  p0_pc3_m_dz[0][1][4][1]=1.31632; p1_pc3_m_dz[0][1][4][1]=0.767682; p2_pc3_m_dz[0][1][4][1]=-0.629001; p3_pc3_m_dz[0][1][4][1]=0.234269; p4_pc3_m_dz[0][1][4][1]=-0.031278; 
  p0_pc3_m_dz[1][1][4][1]=1.31632; p1_pc3_m_dz[1][1][4][1]=0.767682; p2_pc3_m_dz[1][1][4][1]=-0.629001; p3_pc3_m_dz[1][1][4][1]=0.234269; p4_pc3_m_dz[1][1][4][1]=-0.031278; 
  p0_pc3_s_dz[1][4][1]=1.8873; p1_pc3_s_dz[1][4][1]=-1.356; p2_pc3_s_dz[1][4][1]=1.95836; p3_pc3_s_dz[1][4][1]=-0.742604; p4_pc3_s_dz[1][4][1]=0.0988815; 
  p0_pc3_m_dz[0][1][5][1]=1.38071; p1_pc3_m_dz[0][1][5][1]=0.783918; p2_pc3_m_dz[0][1][5][1]=-1.04098; p3_pc3_m_dz[0][1][5][1]=0.462763; p4_pc3_m_dz[0][1][5][1]=-0.0702243; 
  p0_pc3_m_dz[1][1][5][1]=1.38071; p1_pc3_m_dz[1][1][5][1]=0.783918; p2_pc3_m_dz[1][1][5][1]=-1.04098; p3_pc3_m_dz[1][1][5][1]=0.462763; p4_pc3_m_dz[1][1][5][1]=-0.0702243; 
  p0_pc3_s_dz[1][5][1]=1.81768; p1_pc3_s_dz[1][5][1]=-1.22765; p2_pc3_s_dz[1][5][1]=1.97874; p3_pc3_s_dz[1][5][1]=-0.812669; p4_pc3_s_dz[1][5][1]=0.116991; 
  p0_pc3_m_dz[0][1][6][1]=2.2757; p1_pc3_m_dz[0][1][6][1]=-1.89245; p2_pc3_m_dz[0][1][6][1]=1.71268; p3_pc3_m_dz[0][1][6][1]=-0.696078; p4_pc3_m_dz[0][1][6][1]=0.0994738; 
  p0_pc3_m_dz[1][1][6][1]=2.2757; p1_pc3_m_dz[1][1][6][1]=-1.89245; p2_pc3_m_dz[1][1][6][1]=1.71268; p3_pc3_m_dz[1][1][6][1]=-0.696078; p4_pc3_m_dz[1][1][6][1]=0.0994738; 
  p0_pc3_s_dz[1][6][1]=1.10828; p1_pc3_s_dz[1][6][1]=0.71639; p2_pc3_s_dz[1][6][1]=0.0607623; p3_pc3_s_dz[1][6][1]=-0.0224829; p4_pc3_s_dz[1][6][1]=0.00332518; 
  p0_pc3_m_dz[0][1][7][1]=0.860812; p1_pc3_m_dz[0][1][7][1]=1.97338; p2_pc3_m_dz[0][1][7][1]=-2.21795; p3_pc3_m_dz[0][1][7][1]=0.90279; p4_pc3_m_dz[0][1][7][1]=-0.125286; 
  p0_pc3_m_dz[1][1][7][1]=0.860812; p1_pc3_m_dz[1][1][7][1]=1.97338; p2_pc3_m_dz[1][1][7][1]=-2.21795; p3_pc3_m_dz[1][1][7][1]=0.90279; p4_pc3_m_dz[1][1][7][1]=-0.125286; 
  p0_pc3_s_dz[1][7][1]=1.19231; p1_pc3_s_dz[1][7][1]=0.781228; p2_pc3_s_dz[1][7][1]=-0.315792; p3_pc3_s_dz[1][7][1]=0.238784; p4_pc3_s_dz[1][7][1]=-0.0450231; 
  p0_pc3_m_dz[0][1][8][1]=1.71931; p1_pc3_m_dz[0][1][8][1]=-0.707296; p2_pc3_m_dz[0][1][8][1]=0.418197; p3_pc3_m_dz[0][1][8][1]=-0.174632; p4_pc3_m_dz[0][1][8][1]=0.0325142; 
  p0_pc3_m_dz[1][1][8][1]=1.71931; p1_pc3_m_dz[1][1][8][1]=-0.707296; p2_pc3_m_dz[1][1][8][1]=0.418197; p3_pc3_m_dz[1][1][8][1]=-0.174632; p4_pc3_m_dz[1][1][8][1]=0.0325142; 
  p0_pc3_s_dz[1][8][1]=1.29965; p1_pc3_s_dz[1][8][1]=0.334704; p2_pc3_s_dz[1][8][1]=0.353104; p3_pc3_s_dz[1][8][1]=-0.153242; p4_pc3_s_dz[1][8][1]=0.028817; 
  p0_pc3_m_dz[0][1][9][1]=1.06206; p1_pc3_m_dz[0][1][9][1]=1.12081; p2_pc3_m_dz[0][1][9][1]=-1.3524; p3_pc3_m_dz[0][1][9][1]=0.530454; p4_pc3_m_dz[0][1][9][1]=-0.0500526; 
  p0_pc3_m_dz[1][1][9][1]=1.06206; p1_pc3_m_dz[1][1][9][1]=1.12081; p2_pc3_m_dz[1][1][9][1]=-1.3524; p3_pc3_m_dz[1][1][9][1]=0.530454; p4_pc3_m_dz[1][1][9][1]=-0.0500526; 
  p0_pc3_s_dz[1][9][1]=1.6053; p1_pc3_s_dz[1][9][1]=-0.760077; p2_pc3_s_dz[1][9][1]=1.45963; p3_pc3_s_dz[1][9][1]=-0.572796; p4_pc3_s_dz[1][9][1]=0.0866757; 

  //PC3/Negative charge/dZ/
  //Writing the data out for Neg 
  p0_pc3_m_dz[0][0][0][0]=0.213393; p1_pc3_m_dz[0][0][0][0]=1.24954; p2_pc3_m_dz[0][0][0][0]=-0.993726; p3_pc3_m_dz[0][0][0][0]=0.415372; p4_pc3_m_dz[0][0][0][0]=-0.0829096; 
  p0_pc3_m_dz[1][0][0][0]=0.213393; p1_pc3_m_dz[1][0][0][0]=1.24954; p2_pc3_m_dz[1][0][0][0]=-0.993726; p3_pc3_m_dz[1][0][0][0]=0.415372; p4_pc3_m_dz[1][0][0][0]=-0.0829096; 
  p0_pc3_s_dz[0][0][0]=0.805437; p1_pc3_s_dz[0][0][0]=1.79333; p2_pc3_s_dz[0][0][0]=-1.30349; p3_pc3_s_dz[0][0][0]=0.556294; p4_pc3_s_dz[0][0][0]=-0.0693807; 
  p0_pc3_m_dz[0][0][1][0]=1.28047; p1_pc3_m_dz[0][0][1][0]=-1.60894; p2_pc3_m_dz[0][0][1][0]=1.61277; p3_pc3_m_dz[0][0][1][0]=-0.574109; p4_pc3_m_dz[0][0][1][0]=0.066832; 
  p0_pc3_m_dz[1][0][1][0]=1.28047; p1_pc3_m_dz[1][0][1][0]=-1.60894; p2_pc3_m_dz[1][0][1][0]=1.61277; p3_pc3_m_dz[1][0][1][0]=-0.574109; p4_pc3_m_dz[1][0][1][0]=0.066832; 
  p0_pc3_s_dz[0][1][0]=1.3621; p1_pc3_s_dz[0][1][0]=0.822402; p2_pc3_s_dz[0][1][0]=-0.748534; p3_pc3_s_dz[0][1][0]=0.46084; p4_pc3_s_dz[0][1][0]=-0.0769435; 
  p0_pc3_m_dz[0][0][2][0]=0.359203; p1_pc3_m_dz[0][0][2][0]=0.768792; p2_pc3_m_dz[0][0][2][0]=-0.665028; p3_pc3_m_dz[0][0][2][0]=0.311784; p4_pc3_m_dz[0][0][2][0]=-0.0522744; 
  p0_pc3_m_dz[1][0][2][0]=0.359203; p1_pc3_m_dz[1][0][2][0]=0.768792; p2_pc3_m_dz[1][0][2][0]=-0.665028; p3_pc3_m_dz[1][0][2][0]=0.311784; p4_pc3_m_dz[1][0][2][0]=-0.0522744; 
  p0_pc3_s_dz[0][2][0]=0.747367; p1_pc3_s_dz[0][2][0]=2.19142; p2_pc3_s_dz[0][2][0]=-1.85646; p3_pc3_s_dz[0][2][0]=0.827357; p4_pc3_s_dz[0][2][0]=-0.119517; 
  p0_pc3_m_dz[0][0][3][0]=1.3224; p1_pc3_m_dz[0][0][3][0]=-2.10093; p2_pc3_m_dz[0][0][3][0]=2.11985; p3_pc3_m_dz[0][0][3][0]=-0.845709; p4_pc3_m_dz[0][0][3][0]=0.116611; 
  p0_pc3_m_dz[1][0][3][0]=1.3224; p1_pc3_m_dz[1][0][3][0]=-2.10093; p2_pc3_m_dz[1][0][3][0]=2.11985; p3_pc3_m_dz[1][0][3][0]=-0.845709; p4_pc3_m_dz[1][0][3][0]=0.116611; 
  p0_pc3_s_dz[0][3][0]=0.729459; p1_pc3_s_dz[0][3][0]=2.49306; p2_pc3_s_dz[0][3][0]=-2.28262; p3_pc3_s_dz[0][3][0]=1.03177; p4_pc3_s_dz[0][3][0]=-0.151431; 
  p0_pc3_m_dz[0][0][4][0]=-0.313156; p1_pc3_m_dz[0][0][4][0]=2.05746; p2_pc3_m_dz[0][0][4][0]=-1.68603; p3_pc3_m_dz[0][0][4][0]=0.594287; p4_pc3_m_dz[0][0][4][0]=-0.0767664; 
  p0_pc3_m_dz[1][0][4][0]=-0.313156; p1_pc3_m_dz[1][0][4][0]=2.05746; p2_pc3_m_dz[1][0][4][0]=-1.68603; p3_pc3_m_dz[1][0][4][0]=0.594287; p4_pc3_m_dz[1][0][4][0]=-0.0767664; 
  p0_pc3_s_dz[0][4][0]=1.08612; p1_pc3_s_dz[0][4][0]=1.02185; p2_pc3_s_dz[0][4][0]=-0.62254; p3_pc3_s_dz[0][4][0]=0.309011; p4_pc3_s_dz[0][4][0]=-0.047506; 
  p0_pc3_m_dz[0][0][5][0]=1.12008; p1_pc3_m_dz[0][0][5][0]=-1.66309; p2_pc3_m_dz[0][0][5][0]=1.6365; p3_pc3_m_dz[0][0][5][0]=-0.709039; p4_pc3_m_dz[0][0][5][0]=0.105453; 
  p0_pc3_m_dz[1][0][5][0]=1.12008; p1_pc3_m_dz[1][0][5][0]=-1.66309; p2_pc3_m_dz[1][0][5][0]=1.6365; p3_pc3_m_dz[1][0][5][0]=-0.709039; p4_pc3_m_dz[1][0][5][0]=0.105453; 
  p0_pc3_s_dz[0][5][0]=0.986597; p1_pc3_s_dz[0][5][0]=1.5017; p2_pc3_s_dz[0][5][0]=-1.10654; p3_pc3_s_dz[0][5][0]=0.498107; p4_pc3_s_dz[0][5][0]=-0.0717646; 
  p0_pc3_m_dz[0][0][6][0]=0.803358; p1_pc3_m_dz[0][0][6][0]=-0.675278; p2_pc3_m_dz[0][0][6][0]=0.64961; p3_pc3_m_dz[0][0][6][0]=-0.289987; p4_pc3_m_dz[0][0][6][0]=0.0428128; 
  p0_pc3_m_dz[1][0][6][0]=0.803358; p1_pc3_m_dz[1][0][6][0]=-0.675278; p2_pc3_m_dz[1][0][6][0]=0.64961; p3_pc3_m_dz[1][0][6][0]=-0.289987; p4_pc3_m_dz[1][0][6][0]=0.0428128; 
  p0_pc3_s_dz[0][6][0]=1.66226; p1_pc3_s_dz[0][6][0]=-0.399208; p2_pc3_s_dz[0][6][0]=0.75458; p3_pc3_s_dz[0][6][0]=-0.261094; p4_pc3_s_dz[0][6][0]=0.0378311; 
  p0_pc3_m_dz[0][0][7][0]=-0.217683; p1_pc3_m_dz[0][0][7][0]=2.30497; p2_pc3_m_dz[0][0][7][0]=-2.25055; p3_pc3_m_dz[0][0][7][0]=0.835507; p4_pc3_m_dz[0][0][7][0]=-0.10883; 
  p0_pc3_m_dz[1][0][7][0]=-0.217683; p1_pc3_m_dz[1][0][7][0]=2.30497; p2_pc3_m_dz[1][0][7][0]=-2.25055; p3_pc3_m_dz[1][0][7][0]=0.835507; p4_pc3_m_dz[1][0][7][0]=-0.10883; 
  p0_pc3_s_dz[0][7][0]=1.15228; p1_pc3_s_dz[0][7][0]=1.08381; p2_pc3_s_dz[0][7][0]=-0.649237; p3_pc3_s_dz[0][7][0]=0.279224; p4_pc3_s_dz[0][7][0]=-0.0342873; 
  p0_pc3_m_dz[0][0][8][0]=0.796757; p1_pc3_m_dz[0][0][8][0]=-0.0874852; p2_pc3_m_dz[0][0][8][0]=-0.234681; p3_pc3_m_dz[0][0][8][0]=0.115322; p4_pc3_m_dz[0][0][8][0]=-0.0142363; 
  p0_pc3_m_dz[1][0][8][0]=0.796757; p1_pc3_m_dz[1][0][8][0]=-0.0874852; p2_pc3_m_dz[1][0][8][0]=-0.234681; p3_pc3_m_dz[1][0][8][0]=0.115322; p4_pc3_m_dz[1][0][8][0]=-0.0142363; 
  p0_pc3_s_dz[0][8][0]=1.41224; p1_pc3_s_dz[0][8][0]=0.371435; p2_pc3_s_dz[0][8][0]=-0.0380742; p3_pc3_s_dz[0][8][0]=0.081496; p4_pc3_s_dz[0][8][0]=-0.0126011; 
  p0_pc3_m_dz[0][0][9][0]=0.417005; p1_pc3_m_dz[0][0][9][0]=0.974639; p2_pc3_m_dz[0][0][9][0]=-1.10014; p3_pc3_m_dz[0][0][9][0]=0.405915; p4_pc3_m_dz[0][0][9][0]=-0.0326918; 
  p0_pc3_m_dz[1][0][9][0]=0.417005; p1_pc3_m_dz[1][0][9][0]=0.974639; p2_pc3_m_dz[1][0][9][0]=-1.10014; p3_pc3_m_dz[1][0][9][0]=0.405915; p4_pc3_m_dz[1][0][9][0]=-0.0326918; 
  p0_pc3_s_dz[0][9][0]=1.49792; p1_pc3_s_dz[0][9][0]=0.119141; p2_pc3_s_dz[0][9][0]=0.109862; p3_pc3_s_dz[0][9][0]=0.0517791; p4_pc3_s_dz[0][9][0]=-0.00594081; 
  p0_pc3_m_dz[0][1][0][0]=2.88534; p1_pc3_m_dz[0][1][0][0]=-1.93793; p2_pc3_m_dz[0][1][0][0]=1.60389; p3_pc3_m_dz[0][1][0][0]=-0.537872; p4_pc3_m_dz[0][1][0][0]=0.0417752; 
  p0_pc3_m_dz[1][1][0][0]=2.88534; p1_pc3_m_dz[1][1][0][0]=-1.93793; p2_pc3_m_dz[1][1][0][0]=1.60389; p3_pc3_m_dz[1][1][0][0]=-0.537872; p4_pc3_m_dz[1][1][0][0]=0.0417752; 
  p0_pc3_s_dz[1][0][0]=1.33918; p1_pc3_s_dz[1][0][0]=0.368415; p2_pc3_s_dz[1][0][0]=0.0741558; p3_pc3_s_dz[1][0][0]=0.0176581; p4_pc3_s_dz[1][0][0]=0.00568756; 
  p0_pc3_m_dz[0][1][1][0]=2.82172; p1_pc3_m_dz[0][1][1][0]=-1.78746; p2_pc3_m_dz[0][1][1][0]=1.38594; p3_pc3_m_dz[0][1][1][0]=-0.426924; p4_pc3_m_dz[0][1][1][0]=0.0400503; 
  p0_pc3_m_dz[1][1][1][0]=2.82172; p1_pc3_m_dz[1][1][1][0]=-1.78746; p2_pc3_m_dz[1][1][1][0]=1.38594; p3_pc3_m_dz[1][1][1][0]=-0.426924; p4_pc3_m_dz[1][1][1][0]=0.0400503; 
  p0_pc3_s_dz[1][1][0]=1.48812; p1_pc3_s_dz[1][1][0]=0.0963786; p2_pc3_s_dz[1][1][0]=0.275119; p3_pc3_s_dz[1][1][0]=-0.0551381; p4_pc3_s_dz[1][1][0]=0.00979024; 
  p0_pc3_m_dz[0][1][2][0]=1.69844; p1_pc3_m_dz[0][1][2][0]=0.35919; p2_pc3_m_dz[0][1][2][0]=-0.356589; p3_pc3_m_dz[0][1][2][0]=0.19488; p4_pc3_m_dz[0][1][2][0]=-0.037953; 
  p0_pc3_m_dz[1][1][2][0]=1.69844; p1_pc3_m_dz[1][1][2][0]=0.35919; p2_pc3_m_dz[1][1][2][0]=-0.356589; p3_pc3_m_dz[1][1][2][0]=0.19488; p4_pc3_m_dz[1][1][2][0]=-0.037953; 
  p0_pc3_s_dz[1][2][0]=0.938897; p1_pc3_s_dz[1][2][0]=1.5674; p2_pc3_s_dz[1][2][0]=-1.06787; p3_pc3_s_dz[1][2][0]=0.44451; p4_pc3_s_dz[1][2][0]=-0.055786; 
  p0_pc3_m_dz[0][1][3][0]=1.62559; p1_pc3_m_dz[0][1][3][0]=0.124911; p2_pc3_m_dz[0][1][3][0]=-0.043997; p3_pc3_m_dz[0][1][3][0]=-0.00706547; p4_pc3_m_dz[0][1][3][0]=0.00391414; 
  p0_pc3_m_dz[1][1][3][0]=1.62559; p1_pc3_m_dz[1][1][3][0]=0.124911; p2_pc3_m_dz[1][1][3][0]=-0.043997; p3_pc3_m_dz[1][1][3][0]=-0.00706547; p4_pc3_m_dz[1][1][3][0]=0.00391414; 
  p0_pc3_s_dz[1][3][0]=1.65156; p1_pc3_s_dz[1][3][0]=-0.431005; p2_pc3_s_dz[1][3][0]=0.870529; p3_pc3_s_dz[1][3][0]=-0.31462; p4_pc3_s_dz[1][3][0]=0.0480965; 
  p0_pc3_m_dz[0][1][4][0]=1.69814; p1_pc3_m_dz[0][1][4][0]=-0.1416; p2_pc3_m_dz[0][1][4][0]=0.209265; p3_pc3_m_dz[0][1][4][0]=-0.118053; p4_pc3_m_dz[0][1][4][0]=0.0213735; 
  p0_pc3_m_dz[1][1][4][0]=1.69814; p1_pc3_m_dz[1][1][4][0]=-0.1416; p2_pc3_m_dz[1][1][4][0]=0.209265; p3_pc3_m_dz[1][1][4][0]=-0.118053; p4_pc3_m_dz[1][1][4][0]=0.0213735; 
  p0_pc3_s_dz[1][4][0]=1.79393; p1_pc3_s_dz[1][4][0]=-0.854268; p2_pc3_s_dz[1][4][0]=1.24426; p3_pc3_s_dz[1][4][0]=-0.468924; p4_pc3_s_dz[1][4][0]=0.0703112; 
  p0_pc3_m_dz[0][1][5][0]=2.41421; p1_pc3_m_dz[0][1][5][0]=-2.25506; p2_pc3_m_dz[0][1][5][0]=2.27335; p3_pc3_m_dz[0][1][5][0]=-0.992417; p4_pc3_m_dz[0][1][5][0]=0.149761; 
  p0_pc3_m_dz[1][1][5][0]=2.41421; p1_pc3_m_dz[1][1][5][0]=-2.25506; p2_pc3_m_dz[1][1][5][0]=2.27335; p3_pc3_m_dz[1][1][5][0]=-0.992417; p4_pc3_m_dz[1][1][5][0]=0.149761; 
  p0_pc3_s_dz[1][5][0]=0.82661; p1_pc3_s_dz[1][5][0]=2.17421; p2_pc3_s_dz[1][5][0]=-1.8234; p3_pc3_s_dz[1][5][0]=0.819671; p4_pc3_s_dz[1][5][0]=-0.120597; 
  p0_pc3_m_dz[0][1][6][0]=1.85475; p1_pc3_m_dz[0][1][6][0]=-0.691878; p2_pc3_m_dz[0][1][6][0]=0.697467; p3_pc3_m_dz[0][1][6][0]=-0.287372; p4_pc3_m_dz[0][1][6][0]=0.0398005; 
  p0_pc3_m_dz[1][1][6][0]=1.85475; p1_pc3_m_dz[1][1][6][0]=-0.691878; p2_pc3_m_dz[1][1][6][0]=0.697467; p3_pc3_m_dz[1][1][6][0]=-0.287372; p4_pc3_m_dz[1][1][6][0]=0.0398005; 
  p0_pc3_s_dz[1][6][0]=1.91803; p1_pc3_s_dz[1][6][0]=-1.00542; p2_pc3_s_dz[1][6][0]=1.28695; p3_pc3_s_dz[1][6][0]=-0.437687; p4_pc3_s_dz[1][6][0]=0.059086; 
  p0_pc3_m_dz[0][1][7][0]=1.51343; p1_pc3_m_dz[0][1][7][0]=0.0272326; p2_pc3_m_dz[0][1][7][0]=-0.0281119; p3_pc3_m_dz[0][1][7][0]=0.00417078; p4_pc3_m_dz[0][1][7][0]=0.000904741; 
  p0_pc3_m_dz[1][1][7][0]=1.51343; p1_pc3_m_dz[1][1][7][0]=0.0272326; p2_pc3_m_dz[1][1][7][0]=-0.0281119; p3_pc3_m_dz[1][1][7][0]=0.00417078; p4_pc3_m_dz[1][1][7][0]=0.000904741; 
  p0_pc3_s_dz[1][7][0]=0.928009; p1_pc3_s_dz[1][7][0]=1.62765; p2_pc3_s_dz[1][7][0]=-1.0905; p3_pc3_s_dz[1][7][0]=0.445503; p4_pc3_s_dz[1][7][0]=-0.0554634; 
  p0_pc3_m_dz[0][1][8][0]=2.06146; p1_pc3_m_dz[0][1][8][0]=-1.67153; p2_pc3_m_dz[0][1][8][0]=1.73848; p3_pc3_m_dz[0][1][8][0]=-0.762013; p4_pc3_m_dz[0][1][8][0]=0.120448; 
  p0_pc3_m_dz[1][1][8][0]=2.06146; p1_pc3_m_dz[1][1][8][0]=-1.67153; p2_pc3_m_dz[1][1][8][0]=1.73848; p3_pc3_m_dz[1][1][8][0]=-0.762013; p4_pc3_m_dz[1][1][8][0]=0.120448; 
  p0_pc3_s_dz[1][8][0]=1.80941; p1_pc3_s_dz[1][8][0]=-1.04795; p2_pc3_s_dz[1][8][0]=1.57015; p3_pc3_s_dz[1][8][0]=-0.606704; p4_pc3_s_dz[1][8][0]=0.0889484; 
  p0_pc3_m_dz[0][1][9][0]=1.83765; p1_pc3_m_dz[0][1][9][0]=-1.04268; p2_pc3_m_dz[0][1][9][0]=1.199; p3_pc3_m_dz[0][1][9][0]=-0.542222; p4_pc3_m_dz[0][1][9][0]=0.102436; 
  p0_pc3_m_dz[1][1][9][0]=1.83765; p1_pc3_m_dz[1][1][9][0]=-1.04268; p2_pc3_m_dz[1][1][9][0]=1.199; p3_pc3_m_dz[1][1][9][0]=-0.542222; p4_pc3_m_dz[1][1][9][0]=0.102436; 
  p0_pc3_s_dz[1][9][0]=1.87746; p1_pc3_s_dz[1][9][0]=-1.35534; p2_pc3_s_dz[1][9][0]=1.94181; p3_pc3_s_dz[1][9][0]=-0.811892; p4_pc3_s_dz[1][9][0]=0.133199; 
   
  //-------------------------------------------------------------------------------------
  //After Burn Sigmaized variables
  /////-----------------------------------------

  //*** EMC/Pos FOR 19 GeV

	
  p0_emc_m_sdp[0][0][0][1]=-1.47547; p1_emc_m_sdp[0][0][0][1]=3.67657; p2_emc_m_sdp[0][0][0][1]=-3.10101; p3_emc_m_sdp[0][0][0][1]=1.03005; p4_emc_m_sdp[0][0][0][1]=-0.109444; 
  p0_emc_s_sdp[0][0][0][1]=0.651199; p1_emc_s_sdp[0][0][0][1]=-0.192789; p2_emc_s_sdp[0][0][0][1]=0.569099; p3_emc_s_sdp[0][0][0][1]=-0.29557; p4_emc_s_sdp[0][0][0][1]=0.0412986; 
  p0_emc_m_sdp[1][0][0][1]=0.402606; p1_emc_m_sdp[1][0][0][1]=-0.525112; p2_emc_m_sdp[1][0][0][1]=-1.35792; p3_emc_m_sdp[1][0][0][1]=-0.798854; p4_emc_m_sdp[1][0][0][1]=3.89009; 
  p0_emc_s_sdp[1][0][0][1]=1.5127; p1_emc_s_sdp[1][0][0][1]=-0.462579; p2_emc_s_sdp[1][0][0][1]=-2.58085; p3_emc_s_sdp[1][0][0][1]=-2.17047; p4_emc_s_sdp[1][0][0][1]=6.2885; 
  p0_emc_m_sdp[0][0][1][1]=-3.14575; p1_emc_m_sdp[0][0][1][1]=8.3876; p2_emc_m_sdp[0][0][1][1]=-7.64656; p3_emc_m_sdp[0][0][1][1]=2.83388; p4_emc_m_sdp[0][0][1][1]=-0.359094; 
  p0_emc_s_sdp[0][0][1][1]=-0.611375; p1_emc_s_sdp[0][0][1][1]=3.5712; p2_emc_s_sdp[0][0][1][1]=-3.1451; p3_emc_s_sdp[0][0][1][1]=1.12706; p4_emc_s_sdp[0][0][1][1]=-0.136199; 
  p0_emc_m_sdp[1][0][1][1]=-3.43829; p1_emc_m_sdp[1][0][1][1]=13.8738; p2_emc_m_sdp[1][0][1][1]=-4.56632; p3_emc_m_sdp[1][0][1][1]=-36.9954; p4_emc_m_sdp[1][0][1][1]=36.3394; 
  p0_emc_s_sdp[1][0][1][1]=8.70975; p1_emc_s_sdp[1][0][1][1]=-31.9177; p2_emc_s_sdp[1][0][1][1]=8.04754; p3_emc_s_sdp[1][0][1][1]=92.4704; p4_emc_s_sdp[1][0][1][1]=-89.8628; 
  p0_emc_m_sdp[0][0][2][1]=-2.14791; p1_emc_m_sdp[0][0][2][1]=5.86358; p2_emc_m_sdp[0][0][2][1]=-5.43155; p3_emc_m_sdp[0][0][2][1]=2.04709; p4_emc_m_sdp[0][0][2][1]=-0.264238; 
  p0_emc_s_sdp[0][0][2][1]=-0.158629; p1_emc_s_sdp[0][0][2][1]=2.46775; p2_emc_s_sdp[0][0][2][1]=-2.22384; p3_emc_s_sdp[0][0][2][1]=0.8094; p4_emc_s_sdp[0][0][2][1]=-0.0985556; 
  p0_emc_m_sdp[1][0][2][1]=5.73876; p1_emc_m_sdp[1][0][2][1]=-23.6304; p2_emc_m_sdp[1][0][2][1]=7.38086; p3_emc_m_sdp[1][0][2][1]=63.6122; p4_emc_m_sdp[1][0][2][1]=-61.4493; 
  p0_emc_s_sdp[1][0][2][1]=4.86386; p1_emc_s_sdp[1][0][2][1]=-13.1602; p2_emc_s_sdp[1][0][2][1]=-1.5961; p3_emc_s_sdp[1][0][2][1]=35.5925; p4_emc_s_sdp[1][0][2][1]=-25.7301; 
  p0_emc_m_sdp[0][0][3][1]=0.249276; p1_emc_m_sdp[0][0][3][1]=-0.430951; p2_emc_m_sdp[0][0][3][1]=0.268954; p3_emc_m_sdp[0][0][3][1]=-0.0348502; p4_emc_m_sdp[0][0][3][1]=-0.00849073; 
  p0_emc_s_sdp[0][0][3][1]=0.447079; p1_emc_s_sdp[0][0][3][1]=0.681995; p2_emc_s_sdp[0][0][3][1]=-0.478194; p3_emc_s_sdp[0][0][3][1]=0.135175; p4_emc_s_sdp[0][0][3][1]=-0.0118079; 
  p0_emc_m_sdp[1][0][3][1]=-1.8909; p1_emc_m_sdp[1][0][3][1]=7.25146; p2_emc_m_sdp[1][0][3][1]=-2.38557; p3_emc_m_sdp[1][0][3][1]=-18.9005; p4_emc_m_sdp[1][0][3][1]=19.1653; 
  p0_emc_s_sdp[1][0][3][1]=-2.86877; p1_emc_s_sdp[1][0][3][1]=16.8918; p2_emc_s_sdp[1][0][3][1]=-8.5032; p3_emc_s_sdp[1][0][3][1]=-49.0702; p4_emc_s_sdp[1][0][3][1]=54.5438; 
  p0_emc_m_sdp[0][0][4][1]=-0.396122; p1_emc_m_sdp[0][0][4][1]=1.18036; p2_emc_m_sdp[0][0][4][1]=-1.05351; p3_emc_m_sdp[0][0][4][1]=0.365041; p4_emc_m_sdp[0][0][4][1]=-0.0398295; 
  p0_emc_s_sdp[0][0][4][1]=0.947222; p1_emc_s_sdp[0][0][4][1]=-0.54564; p2_emc_s_sdp[0][0][4][1]=0.465709; p3_emc_s_sdp[0][0][4][1]=-0.151604; p4_emc_s_sdp[0][0][4][1]=0.015664; 
  p0_emc_m_sdp[1][0][4][1]=10.0586; p1_emc_m_sdp[1][0][4][1]=-41.8705; p2_emc_m_sdp[1][0][4][1]=12.8826; p3_emc_m_sdp[1][0][4][1]=123.272; p4_emc_m_sdp[1][0][4][1]=-125.101; 
  p0_emc_s_sdp[1][0][4][1]=-6.71056; p1_emc_s_sdp[1][0][4][1]=33.9277; p2_emc_s_sdp[1][0][4][1]=-17.645; p3_emc_s_sdp[1][0][4][1]=-95.6825; p4_emc_s_sdp[1][0][4][1]=107.911; 
  p0_emc_m_sdp[0][0][5][1]=-1.12469; p1_emc_m_sdp[0][0][5][1]=3.07197; p2_emc_m_sdp[0][0][5][1]=-2.73915; p3_emc_m_sdp[0][0][5][1]=0.951845; p4_emc_m_sdp[0][0][5][1]=-0.104462; 
  p0_emc_s_sdp[0][0][5][1]=0.667112; p1_emc_s_sdp[0][0][5][1]=0.314066; p2_emc_s_sdp[0][0][5][1]=-0.270956; p3_emc_s_sdp[0][0][5][1]=0.0985848; p4_emc_s_sdp[0][0][5][1]=-0.0126211; 
  p0_emc_m_sdp[1][0][5][1]=-0.214824; p1_emc_m_sdp[1][0][5][1]=0.271256; p2_emc_m_sdp[1][0][5][1]=0.651259; p3_emc_m_sdp[1][0][5][1]=0.328437; p4_emc_m_sdp[1][0][5][1]=-1.85169; 
  p0_emc_s_sdp[1][0][5][1]=-7.41206; p1_emc_s_sdp[1][0][5][1]=31.905; p2_emc_s_sdp[1][0][5][1]=-9.04377; p3_emc_s_sdp[1][0][5][1]=-85.6746; p4_emc_s_sdp[1][0][5][1]=82.6671; 
  p0_emc_m_sdp[0][0][6][1]=0.347616; p1_emc_m_sdp[0][0][6][1]=-0.833436; p2_emc_m_sdp[0][0][6][1]=0.733946; p3_emc_m_sdp[0][0][6][1]=-0.271453; p4_emc_m_sdp[0][0][6][1]=0.0355815; 
  p0_emc_s_sdp[0][0][6][1]=-0.0748126; p1_emc_s_sdp[0][0][6][1]=2.37727; p2_emc_s_sdp[0][0][6][1]=-2.1742; p3_emc_s_sdp[0][0][6][1]=0.795801; p4_emc_s_sdp[0][0][6][1]=-0.0976896; 
  p0_emc_m_sdp[1][0][6][1]=-2.21984; p1_emc_m_sdp[1][0][6][1]=8.42607; p2_emc_m_sdp[1][0][6][1]=-1.52807; p3_emc_m_sdp[1][0][6][1]=-25.1913; p4_emc_m_sdp[1][0][6][1]=24.466; 
  p0_emc_s_sdp[1][0][6][1]=-7.87062; p1_emc_s_sdp[1][0][6][1]=35.7967; p2_emc_s_sdp[1][0][6][1]=-13.4658; p3_emc_s_sdp[1][0][6][1]=-94.0636; p4_emc_s_sdp[1][0][6][1]=94.4657; 
  p0_emc_m_sdp[0][0][7][1]=-0.127306; p1_emc_m_sdp[0][0][7][1]=0.336078; p2_emc_m_sdp[0][0][7][1]=-0.283097; p3_emc_m_sdp[0][0][7][1]=0.0939241; p4_emc_m_sdp[0][0][7][1]=-0.00954449; 
  p0_emc_s_sdp[0][0][7][1]=0.373619; p1_emc_s_sdp[0][0][7][1]=1.04681; p2_emc_s_sdp[0][0][7][1]=-0.892434; p3_emc_s_sdp[0][0][7][1]=0.315639; p4_emc_s_sdp[0][0][7][1]=-0.038434; 
  p0_emc_m_sdp[1][0][7][1]=-1.04156; p1_emc_m_sdp[1][0][7][1]=3.77489; p2_emc_m_sdp[1][0][7][1]=-1.06839; p3_emc_m_sdp[1][0][7][1]=-10.746; p4_emc_m_sdp[1][0][7][1]=11.1064; 
  p0_emc_s_sdp[1][0][7][1]=-3.34054; p1_emc_s_sdp[1][0][7][1]=13.8221; p2_emc_s_sdp[1][0][7][1]=-0.694088; p3_emc_s_sdp[1][0][7][1]=-37.2641; p4_emc_s_sdp[1][0][7][1]=31.6221; 
  p0_emc_m_sdp[0][0][8][1]=-0.816166; p1_emc_m_sdp[0][0][8][1]=1.95855; p2_emc_m_sdp[0][0][8][1]=-1.60907; p3_emc_m_sdp[0][0][8][1]=0.521164; p4_emc_m_sdp[0][0][8][1]=-0.0541233; 
  p0_emc_s_sdp[0][0][8][1]=0.91889; p1_emc_s_sdp[0][0][8][1]=-0.509514; p2_emc_s_sdp[0][0][8][1]=0.543002; p3_emc_s_sdp[0][0][8][1]=-0.199475; p4_emc_s_sdp[0][0][8][1]=0.0231929; 
  p0_emc_m_sdp[1][0][8][1]=-2.41832; p1_emc_m_sdp[1][0][8][1]=8.72821; p2_emc_m_sdp[1][0][8][1]=-2.10439; p3_emc_m_sdp[1][0][8][1]=-22.151; p4_emc_m_sdp[1][0][8][1]=20.7204; 
  p0_emc_s_sdp[1][0][8][1]=15.3975; p1_emc_s_sdp[1][0][8][1]=-53.8382; p2_emc_s_sdp[1][0][8][1]=7.5054; p3_emc_s_sdp[1][0][8][1]=148.331; p4_emc_s_sdp[1][0][8][1]=-131.935; 
  p0_emc_m_sdp[0][0][9][1]=-0.807084; p1_emc_m_sdp[0][0][9][1]=1.96296; p2_emc_m_sdp[0][0][9][1]=-1.61376; p3_emc_m_sdp[0][0][9][1]=0.500931; p4_emc_m_sdp[0][0][9][1]=-0.0443544; 
  p0_emc_s_sdp[0][0][9][1]=0.775652; p1_emc_s_sdp[0][0][9][1]=0.0145172; p2_emc_s_sdp[0][0][9][1]=0.0280896; p3_emc_s_sdp[0][0][9][1]=-0.0133884; p4_emc_s_sdp[0][0][9][1]=0.00142906; 
  p0_emc_m_sdp[1][0][9][1]=-1.85231; p1_emc_m_sdp[1][0][9][1]=9.30201; p2_emc_m_sdp[1][0][9][1]=-4.39254; p3_emc_m_sdp[1][0][9][1]=-30.5199; p4_emc_m_sdp[1][0][9][1]=33.3147; 
  p0_emc_s_sdp[1][0][9][1]=4.06334; p1_emc_s_sdp[1][0][9][1]=-14.585; p2_emc_s_sdp[1][0][9][1]=5.51855; p3_emc_s_sdp[1][0][9][1]=41.9517; p4_emc_s_sdp[1][0][9][1]=-43.485; 
  p0_emc_m_sdp[0][1][0][1]=-0.186991; p1_emc_m_sdp[0][1][0][1]=0.205022; p2_emc_m_sdp[0][1][0][1]=-0.246318; p3_emc_m_sdp[0][1][0][1]=0.09666; p4_emc_m_sdp[0][1][0][1]=-0.0079733; 
  p0_emc_s_sdp[0][1][0][1]=-0.939906; p1_emc_s_sdp[0][1][0][1]=3.92558; p2_emc_s_sdp[0][1][0][1]=-2.81241; p3_emc_s_sdp[0][1][0][1]=0.820427; p4_emc_s_sdp[0][1][0][1]=-0.0840453; 
  p0_emc_m_sdp[1][1][0][1]=-0.10039; p1_emc_m_sdp[1][1][0][1]=-0.103823; p2_emc_m_sdp[1][1][0][1]=-0.0632022; p3_emc_m_sdp[1][1][0][1]=0.0630345; p4_emc_m_sdp[1][1][0][1]=0.342052; 
  p0_emc_s_sdp[1][1][0][1]=8.34402; p1_emc_s_sdp[1][1][0][1]=-31.6693; p2_emc_s_sdp[1][1][0][1]=10.0088; p3_emc_s_sdp[1][1][0][1]=94.044; p4_emc_s_sdp[1][1][0][1]=-95.975; 
  p0_emc_m_sdp[0][1][1][1]=0.0790079; p1_emc_m_sdp[0][1][1][1]=-0.606268; p2_emc_m_sdp[0][1][1][1]=0.521806; p3_emc_m_sdp[0][1][1][1]=-0.175688; p4_emc_m_sdp[0][1][1][1]=0.0201714; 
  p0_emc_s_sdp[0][1][1][1]=0.169498; p1_emc_s_sdp[0][1][1][1]=1.66646; p2_emc_s_sdp[0][1][1][1]=-1.52674; p3_emc_s_sdp[0][1][1][1]=0.584948; p4_emc_s_sdp[0][1][1][1]=-0.0761118; 
  p0_emc_m_sdp[1][1][1][1]=5.80282; p1_emc_m_sdp[1][1][1][1]=-25.399; p2_emc_m_sdp[1][1][1][1]=10.0812; p3_emc_m_sdp[1][1][1][1]=68.5287; p4_emc_m_sdp[1][1][1][1]=-70.1035; 
  p0_emc_s_sdp[1][1][1][1]=4.9187; p1_emc_s_sdp[1][1][1][1]=-14.7143; p2_emc_s_sdp[1][1][1][1]=3.10172; p3_emc_s_sdp[1][1][1][1]=34.6298; p4_emc_s_sdp[1][1][1][1]=-30.2111; 
  p0_emc_m_sdp[0][1][2][1]=1.83085; p1_emc_m_sdp[0][1][2][1]=-5.42424; p2_emc_m_sdp[0][1][2][1]=5.07621; p3_emc_m_sdp[0][1][2][1]=-1.92792; p4_emc_m_sdp[0][1][2][1]=0.251694; 
  p0_emc_s_sdp[0][1][2][1]=-0.972713; p1_emc_s_sdp[0][1][2][1]=4.7417; p2_emc_s_sdp[0][1][2][1]=-4.11664; p3_emc_s_sdp[0][1][2][1]=1.42941; p4_emc_s_sdp[0][1][2][1]=-0.168291; 
  p0_emc_m_sdp[1][1][2][1]=-6.16084; p1_emc_m_sdp[1][1][2][1]=22.9117; p2_emc_m_sdp[1][1][2][1]=-4.76562; p3_emc_m_sdp[1][1][2][1]=-61.7485; p4_emc_m_sdp[1][1][2][1]=56.1499; 
  p0_emc_s_sdp[1][1][2][1]=-4.52105; p1_emc_s_sdp[1][1][2][1]=21.4273; p2_emc_s_sdp[1][1][2][1]=-6.31561; p3_emc_s_sdp[1][1][2][1]=-58.2563; p4_emc_s_sdp[1][1][2][1]=56.3427; 
  p0_emc_m_sdp[0][1][3][1]=0.179044; p1_emc_m_sdp[0][1][3][1]=-0.930142; p2_emc_m_sdp[0][1][3][1]=0.834158; p3_emc_m_sdp[0][1][3][1]=-0.29215; p4_emc_m_sdp[0][1][3][1]=0.0352494; 
  p0_emc_s_sdp[0][1][3][1]=0.624827; p1_emc_s_sdp[0][1][3][1]=0.235975; p2_emc_s_sdp[0][1][3][1]=-0.0577737; p3_emc_s_sdp[0][1][3][1]=-0.0298961; p4_emc_s_sdp[0][1][3][1]=0.0116629; 
  p0_emc_m_sdp[1][1][3][1]=3.28735; p1_emc_m_sdp[1][1][3][1]=-11.3662; p2_emc_m_sdp[1][1][3][1]=0.731737; p3_emc_m_sdp[1][1][3][1]=25.8597; p4_emc_m_sdp[1][1][3][1]=-19.4539; 
  p0_emc_s_sdp[1][1][3][1]=7.8578; p1_emc_s_sdp[1][1][3][1]=-25.1302; p2_emc_s_sdp[1][1][3][1]=5.09576; p3_emc_s_sdp[1][1][3][1]=57.2095; p4_emc_s_sdp[1][1][3][1]=-47.9548; 
  p0_emc_m_sdp[0][1][4][1]=0.742433; p1_emc_m_sdp[0][1][4][1]=-2.35741; p2_emc_m_sdp[0][1][4][1]=2.16605; p3_emc_m_sdp[0][1][4][1]=-0.791747; p4_emc_m_sdp[0][1][4][1]=0.0986867; 
  p0_emc_s_sdp[0][1][4][1]=-0.272059; p1_emc_s_sdp[0][1][4][1]=2.7489; p2_emc_s_sdp[0][1][4][1]=-2.42813; p3_emc_s_sdp[0][1][4][1]=0.878792; p4_emc_s_sdp[0][1][4][1]=-0.108594; 
  p0_emc_m_sdp[1][1][4][1]=-5.1104; p1_emc_m_sdp[1][1][4][1]=20.7399; p2_emc_m_sdp[1][1][4][1]=-7.1302; p3_emc_m_sdp[1][1][4][1]=-55.6665; p4_emc_m_sdp[1][1][4][1]=54.9418; 
  p0_emc_s_sdp[1][1][4][1]=-6.92535; p1_emc_s_sdp[1][1][4][1]=32.9915; p2_emc_s_sdp[1][1][4][1]=-13.1931; p3_emc_s_sdp[1][1][4][1]=-89.0945; p4_emc_s_sdp[1][1][4][1]=91.5173; 
  p0_emc_m_sdp[0][1][5][1]=0.316751; p1_emc_m_sdp[0][1][5][1]=-0.934336; p2_emc_m_sdp[0][1][5][1]=0.913595; p3_emc_m_sdp[0][1][5][1]=-0.36268; p4_emc_m_sdp[0][1][5][1]=0.0498625; 
  p0_emc_s_sdp[0][1][5][1]=0.144104; p1_emc_s_sdp[0][1][5][1]=1.77208; p2_emc_s_sdp[0][1][5][1]=-1.66903; p3_emc_s_sdp[0][1][5][1]=0.645843; p4_emc_s_sdp[0][1][5][1]=-0.0842809; 
  p0_emc_m_sdp[1][1][5][1]=6.02578; p1_emc_m_sdp[1][1][5][1]=-22.6122; p2_emc_m_sdp[1][1][5][1]=6.09991; p3_emc_m_sdp[1][1][5][1]=56.6032; p4_emc_m_sdp[1][1][5][1]=-53.3455; 
  p0_emc_s_sdp[1][1][5][1]=-10.6893; p1_emc_s_sdp[1][1][5][1]=45.4869; p2_emc_s_sdp[1][1][5][1]=-12.1057; p3_emc_s_sdp[1][1][5][1]=-125.539; p4_emc_s_sdp[1][1][5][1]=120.527; 
  p0_emc_m_sdp[0][1][6][1]=1.19411; p1_emc_m_sdp[0][1][6][1]=-3.25501; p2_emc_m_sdp[0][1][6][1]=3.112; p3_emc_m_sdp[0][1][6][1]=-1.21685; p4_emc_m_sdp[0][1][6][1]=0.164023; 
  p0_emc_s_sdp[0][1][6][1]=0.299876; p1_emc_s_sdp[0][1][6][1]=1.34946; p2_emc_s_sdp[0][1][6][1]=-1.21402; p3_emc_s_sdp[0][1][6][1]=0.443891; p4_emc_s_sdp[0][1][6][1]=-0.0544544; 
  p0_emc_m_sdp[1][1][6][1]=4.94193; p1_emc_m_sdp[1][1][6][1]=-19.2513; p2_emc_m_sdp[1][1][6][1]=4.7287; p3_emc_m_sdp[1][1][6][1]=54.2904; p4_emc_m_sdp[1][1][6][1]=-52.5026; 
  p0_emc_s_sdp[1][1][6][1]=8.33775; p1_emc_s_sdp[1][1][6][1]=-29.9167; p2_emc_s_sdp[1][1][6][1]=7.90357; p3_emc_s_sdp[1][1][6][1]=83.0158; p4_emc_s_sdp[1][1][6][1]=-80.0022; 
  p0_emc_m_sdp[0][1][7][1]=1.34317; p1_emc_m_sdp[0][1][7][1]=-3.45622; p2_emc_m_sdp[0][1][7][1]=3.20754; p3_emc_m_sdp[0][1][7][1]=-1.23255; p4_emc_m_sdp[0][1][7][1]=0.16439; 
  p0_emc_s_sdp[0][1][7][1]=0.133062; p1_emc_s_sdp[0][1][7][1]=1.82017; p2_emc_s_sdp[0][1][7][1]=-1.66671; p3_emc_s_sdp[0][1][7][1]=0.621634; p4_emc_s_sdp[0][1][7][1]=-0.0779266; 
  p0_emc_m_sdp[1][1][7][1]=5.92848; p1_emc_m_sdp[1][1][7][1]=-23.3551; p2_emc_m_sdp[1][1][7][1]=7.41526; p3_emc_m_sdp[1][1][7][1]=63.1082; p4_emc_m_sdp[1][1][7][1]=-62.8726; 
  p0_emc_s_sdp[1][1][7][1]=3.69293; p1_emc_s_sdp[1][1][7][1]=-12.9792; p2_emc_s_sdp[1][1][7][1]=6.67774; p3_emc_s_sdp[1][1][7][1]=34.8674; p4_emc_s_sdp[1][1][7][1]=-38.566; 
  p0_emc_m_sdp[0][1][8][1]=-0.164709; p1_emc_m_sdp[0][1][8][1]=0.577583; p2_emc_m_sdp[0][1][8][1]=-0.498369; p3_emc_m_sdp[0][1][8][1]=0.156335; p4_emc_m_sdp[0][1][8][1]=-0.0133696; 
  p0_emc_s_sdp[0][1][8][1]=0.756164; p1_emc_s_sdp[0][1][8][1]=0.0712645; p2_emc_s_sdp[0][1][8][1]=-0.0135435; p3_emc_s_sdp[0][1][8][1]=-0.00206332; p4_emc_s_sdp[0][1][8][1]=0.00107789; 
  p0_emc_m_sdp[1][1][8][1]=0.306614; p1_emc_m_sdp[1][1][8][1]=-0.192886; p2_emc_m_sdp[1][1][8][1]=-0.664519; p3_emc_m_sdp[1][1][8][1]=-0.501571; p4_emc_m_sdp[1][1][8][1]=1.48874; 
  p0_emc_s_sdp[1][1][8][1]=1.3285; p1_emc_s_sdp[1][1][8][1]=-0.413011; p2_emc_s_sdp[1][1][8][1]=-2.25911; p3_emc_s_sdp[1][1][8][1]=-1.70179; p4_emc_s_sdp[1][1][8][1]=6.72923; 
  p0_emc_m_sdp[0][1][9][1]=0.295893; p1_emc_m_sdp[0][1][9][1]=-0.689481; p2_emc_m_sdp[0][1][9][1]=0.596212; p3_emc_m_sdp[0][1][9][1]=-0.210458; p4_emc_m_sdp[0][1][9][1]=0.0257633; 
  p0_emc_s_sdp[0][1][9][1]=0.595392; p1_emc_s_sdp[0][1][9][1]=0.449246; p2_emc_s_sdp[0][1][9][1]=-0.286264; p3_emc_s_sdp[0][1][9][1]=0.0638376; p4_emc_s_sdp[0][1][9][1]=-0.000733353; 
  p0_emc_m_sdp[1][1][9][1]=4.16882; p1_emc_m_sdp[1][1][9][1]=-17.7529; p2_emc_m_sdp[1][1][9][1]=6.21202; p3_emc_m_sdp[1][1][9][1]=53.4339; p4_emc_m_sdp[1][1][9][1]=-55.7496; 
  p0_emc_s_sdp[1][1][9][1]=7.1599; p1_emc_s_sdp[1][1][9][1]=-25.6809; p2_emc_s_sdp[1][1][9][1]=7.53; p3_emc_s_sdp[1][1][9][1]=71.266; p4_emc_s_sdp[1][1][9][1]=-69.7043; 
  p0_emc_m_sdp[0][2][0][1]=-2.05285; p1_emc_m_sdp[0][2][0][1]=5.56264; p2_emc_m_sdp[0][2][0][1]=-5.12556; p3_emc_m_sdp[0][2][0][1]=1.92839; p4_emc_m_sdp[0][2][0][1]=-0.249716; 
  p0_emc_s_sdp[0][2][0][1]=0.0530055; p1_emc_s_sdp[0][2][0][1]=2.08675; p2_emc_s_sdp[0][2][0][1]=-1.88453; p3_emc_s_sdp[0][2][0][1]=0.693954; p4_emc_s_sdp[0][2][0][1]=-0.0854584; 
  p0_emc_m_sdp[1][2][0][1]=0.0864955; p1_emc_m_sdp[1][2][0][1]=-0.0347746; p2_emc_m_sdp[1][2][0][1]=-0.180494; p3_emc_m_sdp[1][2][0][1]=-0.220774; p4_emc_m_sdp[1][2][0][1]=0.169342; 
  p0_emc_s_sdp[1][2][0][1]=-9.64639; p1_emc_s_sdp[1][2][0][1]=42.261; p2_emc_s_sdp[1][2][0][1]=-11.1911; p3_emc_s_sdp[1][2][0][1]=-120.853; p4_emc_s_sdp[1][2][0][1]=117.403; 
  p0_emc_m_sdp[0][2][1][1]=1.36063; p1_emc_m_sdp[0][2][1][1]=-3.87858; p2_emc_m_sdp[0][2][1][1]=3.81624; p3_emc_m_sdp[0][2][1][1]=-1.50724; p4_emc_m_sdp[0][2][1][1]=0.201625; 
  p0_emc_s_sdp[0][2][1][1]=0.28206; p1_emc_s_sdp[0][2][1][1]=1.50855; p2_emc_s_sdp[0][2][1][1]=-1.37311; p3_emc_s_sdp[0][2][1][1]=0.508059; p4_emc_s_sdp[0][2][1][1]=-0.0623357; 
  p0_emc_m_sdp[1][2][1][1]=1.8978; p1_emc_m_sdp[1][2][1][1]=-7.01172; p2_emc_m_sdp[1][2][1][1]=0.613483; p3_emc_m_sdp[1][2][1][1]=19.1052; p4_emc_m_sdp[1][2][1][1]=-16.0151; 
  p0_emc_s_sdp[1][2][1][1]=0.898406; p1_emc_s_sdp[1][2][1][1]=0.0938217; p2_emc_s_sdp[1][2][1][1]=-0.765287; p3_emc_s_sdp[1][2][1][1]=-0.671382; p4_emc_s_sdp[1][2][1][1]=2.50447; 
  p0_emc_m_sdp[0][2][2][1]=1.518; p1_emc_m_sdp[0][2][2][1]=-4.18701; p2_emc_m_sdp[0][2][2][1]=3.97331; p3_emc_m_sdp[0][2][2][1]=-1.52445; p4_emc_m_sdp[0][2][2][1]=0.199501; 
  p0_emc_s_sdp[0][2][2][1]=0.255238; p1_emc_s_sdp[0][2][2][1]=1.65899; p2_emc_s_sdp[0][2][2][1]=-1.58692; p3_emc_s_sdp[0][2][2][1]=0.616406; p4_emc_s_sdp[0][2][2][1]=-0.0801804; 
  p0_emc_m_sdp[1][2][2][1]=-0.00445944; p1_emc_m_sdp[1][2][2][1]=-0.101289; p2_emc_m_sdp[1][2][2][1]=-0.140581; p3_emc_m_sdp[1][2][2][1]=0.0267097; p4_emc_m_sdp[1][2][2][1]=0.719773; 
  p0_emc_s_sdp[1][2][2][1]=0.663888; p1_emc_s_sdp[1][2][2][1]=0.385273; p2_emc_s_sdp[1][2][2][1]=-0.00716628; p3_emc_s_sdp[1][2][2][1]=-0.251872; p4_emc_s_sdp[1][2][2][1]=0.2156; 
  p0_emc_m_sdp[0][2][3][1]=0.945372; p1_emc_m_sdp[0][2][3][1]=-2.70079; p2_emc_m_sdp[0][2][3][1]=2.59907; p3_emc_m_sdp[0][2][3][1]=-0.997012; p4_emc_m_sdp[0][2][3][1]=0.128929; 
  p0_emc_s_sdp[0][2][3][1]=0.449671; p1_emc_s_sdp[0][2][3][1]=1.18126; p2_emc_s_sdp[0][2][3][1]=-1.19695; p3_emc_s_sdp[0][2][3][1]=0.487821; p4_emc_s_sdp[0][2][3][1]=-0.0660289; 
  p0_emc_m_sdp[1][2][3][1]=-11.5984; p1_emc_m_sdp[1][2][3][1]=46.1815; p2_emc_m_sdp[1][2][3][1]=-13.8311; p3_emc_m_sdp[1][2][3][1]=-125.98; p4_emc_m_sdp[1][2][3][1]=123.336; 
  p0_emc_s_sdp[1][2][3][1]=-4.77176; p1_emc_s_sdp[1][2][3][1]=23.1522; p2_emc_s_sdp[1][2][3][1]=-9.24582; p3_emc_s_sdp[1][2][3][1]=-60.7375; p4_emc_s_sdp[1][2][3][1]=62.8209; 
  p0_emc_m_sdp[0][2][4][1]=-0.574811; p1_emc_m_sdp[0][2][4][1]=1.36086; p2_emc_m_sdp[0][2][4][1]=-1.14728; p3_emc_m_sdp[0][2][4][1]=0.409605; p4_emc_m_sdp[0][2][4][1]=-0.0510496; 
  p0_emc_s_sdp[0][2][4][1]=0.246847; p1_emc_s_sdp[0][2][4][1]=1.64464; p2_emc_s_sdp[0][2][4][1]=-1.59355; p3_emc_s_sdp[0][2][4][1]=0.62629; p4_emc_s_sdp[0][2][4][1]=-0.0826185; 
  p0_emc_m_sdp[1][2][4][1]=-4.80227; p1_emc_m_sdp[1][2][4][1]=18.7835; p2_emc_m_sdp[1][2][4][1]=-5.33609; p3_emc_m_sdp[1][2][4][1]=-52.2278; p4_emc_m_sdp[1][2][4][1]=51.2777; 
  p0_emc_s_sdp[1][2][4][1]=2.93913; p1_emc_s_sdp[1][2][4][1]=-7.19226; p2_emc_s_sdp[1][2][4][1]=1.09341; p3_emc_s_sdp[1][2][4][1]=17.0279; p4_emc_s_sdp[1][2][4][1]=-14.4179; 
  p0_emc_m_sdp[0][2][5][1]=1.06988; p1_emc_m_sdp[0][2][5][1]=-3.12455; p2_emc_m_sdp[0][2][5][1]=3.05068; p3_emc_m_sdp[0][2][5][1]=-1.18996; p4_emc_m_sdp[0][2][5][1]=0.157209; 
  p0_emc_s_sdp[0][2][5][1]=0.256878; p1_emc_s_sdp[0][2][5][1]=1.65797; p2_emc_s_sdp[0][2][5][1]=-1.56348; p3_emc_s_sdp[0][2][5][1]=0.594998; p4_emc_s_sdp[0][2][5][1]=-0.0765086; 
  p0_emc_m_sdp[1][2][5][1]=-1.09621; p1_emc_m_sdp[1][2][5][1]=4.00258; p2_emc_m_sdp[1][2][5][1]=-0.545749; p3_emc_m_sdp[1][2][5][1]=-12.2691; p4_emc_m_sdp[1][2][5][1]=11.2794; 
  p0_emc_s_sdp[1][2][5][1]=-2.3812; p1_emc_s_sdp[1][2][5][1]=12.8573; p2_emc_s_sdp[1][2][5][1]=-3.66953; p3_emc_s_sdp[1][2][5][1]=-33.6147; p4_emc_s_sdp[1][2][5][1]=31.2853; 
  p0_emc_m_sdp[0][2][6][1]=-0.92215; p1_emc_m_sdp[0][2][6][1]=2.31103; p2_emc_m_sdp[0][2][6][1]=-2.08801; p3_emc_m_sdp[0][2][6][1]=0.785536; p4_emc_m_sdp[0][2][6][1]=-0.102143; 
  p0_emc_s_sdp[0][2][6][1]=0.346725; p1_emc_s_sdp[0][2][6][1]=1.51062; p2_emc_s_sdp[0][2][6][1]=-1.47567; p3_emc_s_sdp[0][2][6][1]=0.574751; p4_emc_s_sdp[0][2][6][1]=-0.0750363; 
  p0_emc_m_sdp[1][2][6][1]=4.11821; p1_emc_m_sdp[1][2][6][1]=-17.2444; p2_emc_m_sdp[1][2][6][1]=5.61417; p3_emc_m_sdp[1][2][6][1]=48.5975; p4_emc_m_sdp[1][2][6][1]=-49.0631; 
  p0_emc_s_sdp[1][2][6][1]=0.955556; p1_emc_s_sdp[1][2][6][1]=0.200072; p2_emc_s_sdp[1][2][6][1]=-0.702821; p3_emc_s_sdp[1][2][6][1]=-0.844133; p4_emc_s_sdp[1][2][6][1]=1.70296; 
  p0_emc_m_sdp[0][2][7][1]=0.219317; p1_emc_m_sdp[0][2][7][1]=-0.900188; p2_emc_m_sdp[0][2][7][1]=0.977607; p3_emc_m_sdp[0][2][7][1]=-0.400352; p4_emc_m_sdp[0][2][7][1]=0.0543891; 
  p0_emc_s_sdp[0][2][7][1]=0.356949; p1_emc_s_sdp[0][2][7][1]=1.38374; p2_emc_s_sdp[0][2][7][1]=-1.28598; p3_emc_s_sdp[0][2][7][1]=0.487275; p4_emc_s_sdp[0][2][7][1]=-0.0629243; 
  p0_emc_m_sdp[1][2][7][1]=0.0592584; p1_emc_m_sdp[1][2][7][1]=-0.167673; p2_emc_m_sdp[1][2][7][1]=-0.348055; p3_emc_m_sdp[1][2][7][1]=-0.191801; p4_emc_m_sdp[1][2][7][1]=0.855408; 
  p0_emc_s_sdp[1][2][7][1]=-2.83562; p1_emc_s_sdp[1][2][7][1]=15.5895; p2_emc_s_sdp[1][2][7][1]=-5.3419; p3_emc_s_sdp[1][2][7][1]=-45.0025; p4_emc_s_sdp[1][2][7][1]=46.0139; 
  p0_emc_m_sdp[0][2][8][1]=-1.06069; p1_emc_m_sdp[0][2][8][1]=2.59303; p2_emc_m_sdp[0][2][8][1]=-2.35525; p3_emc_m_sdp[0][2][8][1]=0.892546; p4_emc_m_sdp[0][2][8][1]=-0.116123; 
  p0_emc_s_sdp[0][2][8][1]=0.233567; p1_emc_s_sdp[0][2][8][1]=1.65996; p2_emc_s_sdp[0][2][8][1]=-1.54994; p3_emc_s_sdp[0][2][8][1]=0.584963; p4_emc_s_sdp[0][2][8][1]=-0.0726349; 
  p0_emc_m_sdp[1][2][8][1]=2.55275; p1_emc_m_sdp[1][2][8][1]=-9.83486; p2_emc_m_sdp[1][2][8][1]=2.14956; p3_emc_m_sdp[1][2][8][1]=25.4353; p4_emc_m_sdp[1][2][8][1]=-23.2843; 
  p0_emc_s_sdp[1][2][8][1]=-1.70789; p1_emc_s_sdp[1][2][8][1]=8.87622; p2_emc_s_sdp[1][2][8][1]=-1.17923; p3_emc_s_sdp[1][2][8][1]=-21.2858; p4_emc_s_sdp[1][2][8][1]=17.3851; 
  p0_emc_m_sdp[0][2][9][1]=-0.453026; p1_emc_m_sdp[0][2][9][1]=0.889953; p2_emc_m_sdp[0][2][9][1]=-0.650571; p3_emc_m_sdp[0][2][9][1]=0.176992; p4_emc_m_sdp[0][2][9][1]=-0.00956338; 
  p0_emc_s_sdp[0][2][9][1]=0.989939; p1_emc_s_sdp[0][2][9][1]=-0.347082; p2_emc_s_sdp[0][2][9][1]=0.260926; p3_emc_s_sdp[0][2][9][1]=-0.0719577; p4_emc_s_sdp[0][2][9][1]=0.00716597; 
  p0_emc_m_sdp[1][2][9][1]=3.1371; p1_emc_m_sdp[1][2][9][1]=-12.2795; p2_emc_m_sdp[1][2][9][1]=2.67301; p3_emc_m_sdp[1][2][9][1]=33.0519; p4_emc_m_sdp[1][2][9][1]=-30.2226; 
  p0_emc_s_sdp[1][2][9][1]=3.26854; p1_emc_s_sdp[1][2][9][1]=-9.58232; p2_emc_s_sdp[1][2][9][1]=2.40525; p3_emc_s_sdp[1][2][9][1]=26.872; p4_emc_s_sdp[1][2][9][1]=-25.5367; 

  //****** ReFitZedParam ******** NegCharge for 19 GeV
  //Writing the data out for Neg 
	
  p0_emc_m_sdp[0][0][0][0]=0.412862; p1_emc_m_sdp[0][0][0][0]=-1.09351; p2_emc_m_sdp[0][0][0][0]=0.955122; p3_emc_m_sdp[0][0][0][0]=-0.335974; p4_emc_m_sdp[0][0][0][0]=0.0385033; 
  p0_emc_s_sdp[0][0][0][0]=2.05035; p1_emc_s_sdp[0][0][0][0]=-3.33073; p2_emc_s_sdp[0][0][0][0]=2.93273; p3_emc_s_sdp[0][0][0][0]=-1.00374; p4_emc_s_sdp[0][0][0][0]=0.112842; 
  p0_emc_m_sdp[1][0][0][0]=0.616819; p1_emc_m_sdp[1][0][0][0]=-0.746912; p2_emc_m_sdp[1][0][0][0]=-1.93375; p3_emc_m_sdp[1][0][0][0]=-1.00198; p4_emc_m_sdp[1][0][0][0]=5.7688; 
  p0_emc_s_sdp[1][0][0][0]=6.47285; p1_emc_s_sdp[1][0][0][0]=-24.8972; p2_emc_s_sdp[1][0][0][0]=7.38208; p3_emc_s_sdp[1][0][0][0]=79.1387; p4_emc_s_sdp[1][0][0][0]=-80.6986; 
  p0_emc_m_sdp[0][0][1][0]=0.319526; p1_emc_m_sdp[0][0][1][0]=-0.756655; p2_emc_m_sdp[0][0][1][0]=0.570391; p3_emc_m_sdp[0][0][1][0]=-0.179347; p4_emc_m_sdp[0][0][1][0]=0.0202752; 
  p0_emc_s_sdp[0][0][1][0]=1.05638; p1_emc_s_sdp[0][0][1][0]=-1.02266; p2_emc_s_sdp[0][0][1][0]=1.03958; p3_emc_s_sdp[0][0][1][0]=-0.373494; p4_emc_s_sdp[0][0][1][0]=0.0422126; 
  p0_emc_m_sdp[1][0][1][0]=5.03223; p1_emc_m_sdp[1][0][1][0]=-22.6771; p2_emc_m_sdp[1][0][1][0]=8.18925; p3_emc_m_sdp[1][0][1][0]=71.3098; p4_emc_m_sdp[1][0][1][0]=-74.7105; 
  p0_emc_s_sdp[1][0][1][0]=6.48923; p1_emc_s_sdp[1][0][1][0]=-21.9911; p2_emc_s_sdp[1][0][1][0]=5.77797; p3_emc_s_sdp[1][0][1][0]=55.9351; p4_emc_s_sdp[1][0][1][0]=-52.2014; 
  p0_emc_m_sdp[0][0][2][0]=-0.770627; p1_emc_m_sdp[0][0][2][0]=1.8955; p2_emc_m_sdp[0][0][2][0]=-1.6361; p3_emc_m_sdp[0][0][2][0]=0.57049; p4_emc_m_sdp[0][0][2][0]=-0.06526; 
  p0_emc_s_sdp[0][0][2][0]=1.26341; p1_emc_s_sdp[0][0][2][0]=-1.31929; p2_emc_s_sdp[0][0][2][0]=1.12227; p3_emc_s_sdp[0][0][2][0]=-0.368078; p4_emc_s_sdp[0][0][2][0]=0.0388967; 
  p0_emc_m_sdp[1][0][2][0]=0.430232; p1_emc_m_sdp[1][0][2][0]=-0.489005; p2_emc_m_sdp[1][0][2][0]=-1.33257; p3_emc_m_sdp[1][0][2][0]=-0.794925; p4_emc_m_sdp[1][0][2][0]=3.74615; 
  p0_emc_s_sdp[1][0][2][0]=0.548339; p1_emc_s_sdp[1][0][2][0]=0.241146; p2_emc_s_sdp[1][0][2][0]=-0.232317; p3_emc_s_sdp[1][0][2][0]=-0.297739; p4_emc_s_sdp[1][0][2][0]=1.60282; 
  p0_emc_m_sdp[0][0][3][0]=-0.109423; p1_emc_m_sdp[0][0][3][0]=0.0911367; p2_emc_m_sdp[0][0][3][0]=-0.0145721; p3_emc_m_sdp[0][0][3][0]=-0.00325822; p4_emc_m_sdp[0][0][3][0]=0.000965382; 
  p0_emc_s_sdp[0][0][3][0]=1.69912; p1_emc_s_sdp[0][0][3][0]=-2.65615; p2_emc_s_sdp[0][0][3][0]=2.43965; p3_emc_s_sdp[0][0][3][0]=-0.864725; p4_emc_s_sdp[0][0][3][0]=0.0997405; 
  p0_emc_m_sdp[1][0][3][0]=0.247906; p1_emc_m_sdp[1][0][3][0]=-0.366029; p2_emc_m_sdp[1][0][3][0]=-0.857668; p3_emc_m_sdp[1][0][3][0]=-0.370174; p4_emc_m_sdp[1][0][3][0]=2.64048; 
  p0_emc_s_sdp[1][0][3][0]=-8.88; p1_emc_s_sdp[1][0][3][0]=32.7833; p2_emc_s_sdp[1][0][3][0]=-2.06292; p3_emc_s_sdp[1][0][3][0]=-87.0761; p4_emc_s_sdp[1][0][3][0]=72.7965; 
  p0_emc_m_sdp[0][0][4][0]=0.0393288; p1_emc_m_sdp[0][0][4][0]=-0.128371; p2_emc_m_sdp[0][0][4][0]=0.13732; p3_emc_m_sdp[0][0][4][0]=-0.063955; p4_emc_m_sdp[0][0][4][0]=0.0103474; 
  p0_emc_s_sdp[0][0][4][0]=1.6271; p1_emc_s_sdp[0][0][4][0]=-2.29385; p2_emc_s_sdp[0][0][4][0]=1.98045; p3_emc_s_sdp[0][0][4][0]=-0.671477; p4_emc_s_sdp[0][0][4][0]=0.0752775; 
  p0_emc_m_sdp[1][0][4][0]=2.07967; p1_emc_m_sdp[1][0][4][0]=0.331698; p2_emc_m_sdp[1][0][4][0]=-7.5596; p3_emc_m_sdp[1][0][4][0]=-20.268; p4_emc_m_sdp[1][0][4][0]=37.138; 
  p0_emc_s_sdp[1][0][4][0]=24.9577; p1_emc_s_sdp[1][0][4][0]=-81.0445; p2_emc_s_sdp[1][0][4][0]=-56.8816; p3_emc_s_sdp[1][0][4][0]=415.6; p4_emc_s_sdp[1][0][4][0]=-345.703; 
  p0_emc_m_sdp[0][0][5][0]=0.437146; p1_emc_m_sdp[0][0][5][0]=-0.957072; p2_emc_m_sdp[0][0][5][0]=0.7483; p3_emc_m_sdp[0][0][5][0]=-0.245806; p4_emc_m_sdp[0][0][5][0]=0.027786; 
  p0_emc_s_sdp[0][0][5][0]=2.37995; p1_emc_s_sdp[0][0][5][0]=-3.99824; p2_emc_s_sdp[0][0][5][0]=3.25829; p3_emc_s_sdp[0][0][5][0]=-1.06137; p4_emc_s_sdp[0][0][5][0]=0.11387; 
  p0_emc_m_sdp[1][0][5][0]=-1.44891; p1_emc_m_sdp[1][0][5][0]=1.59148; p2_emc_m_sdp[1][0][5][0]=4.13667; p3_emc_m_sdp[1][0][5][0]=2.39368; p4_emc_m_sdp[1][0][5][0]=-11.1898; 
  p0_emc_s_sdp[1][0][5][0]=8.39509; p1_emc_s_sdp[1][0][5][0]=-28.8432; p2_emc_s_sdp[1][0][5][0]=5.66471; p3_emc_s_sdp[1][0][5][0]=80.4746; p4_emc_s_sdp[1][0][5][0]=-75.8802; 
  p0_emc_m_sdp[0][0][6][0]=0.461817; p1_emc_m_sdp[0][0][6][0]=-1.36165; p2_emc_m_sdp[0][0][6][0]=1.40612; p3_emc_m_sdp[0][0][6][0]=-0.594875; p4_emc_m_sdp[0][0][6][0]=0.0884613; 
  p0_emc_s_sdp[0][0][6][0]=1.17491; p1_emc_s_sdp[0][0][6][0]=-0.713048; p2_emc_s_sdp[0][0][6][0]=0.250128; p3_emc_s_sdp[0][0][6][0]=0.0853323; p4_emc_s_sdp[0][0][6][0]=-0.0401062; 
  p0_emc_m_sdp[1][0][6][0]=4.23256; p1_emc_m_sdp[1][0][6][0]=-16.674; p2_emc_m_sdp[1][0][6][0]=3.98812; p3_emc_m_sdp[1][0][6][0]=48.1631; p4_emc_m_sdp[1][0][6][0]=-46.2593; 
  p0_emc_s_sdp[1][0][6][0]=1.16191; p1_emc_s_sdp[1][0][6][0]=-0.22974; p2_emc_s_sdp[1][0][6][0]=-1.6545; p3_emc_s_sdp[1][0][6][0]=-1.39347; p4_emc_s_sdp[1][0][6][0]=4.22094; 
  p0_emc_m_sdp[0][0][7][0]=-0.155455; p1_emc_m_sdp[0][0][7][0]=0.331378; p2_emc_m_sdp[0][0][7][0]=-0.271856; p3_emc_m_sdp[0][0][7][0]=0.0967893; p4_emc_m_sdp[0][0][7][0]=-0.0109113; 
  p0_emc_s_sdp[0][0][7][0]=1.29231; p1_emc_s_sdp[0][0][7][0]=-1.29423; p2_emc_s_sdp[0][0][7][0]=1.03811; p3_emc_s_sdp[0][0][7][0]=-0.320582; p4_emc_s_sdp[0][0][7][0]=0.0319738; 
  p0_emc_m_sdp[1][0][7][0]=0.314226; p1_emc_m_sdp[1][0][7][0]=-0.306503; p2_emc_m_sdp[1][0][7][0]=-0.903125; p3_emc_m_sdp[1][0][7][0]=-0.586913; p4_emc_m_sdp[1][0][7][0]=2.47623; 
  p0_emc_s_sdp[1][0][7][0]=11.6398; p1_emc_s_sdp[1][0][7][0]=-41.8415; p2_emc_s_sdp[1][0][7][0]=9.97739; p3_emc_s_sdp[1][0][7][0]=112.12; p4_emc_s_sdp[1][0][7][0]=-104.794; 
  p0_emc_m_sdp[0][0][8][0]=0.645366; p1_emc_m_sdp[0][0][8][0]=-1.65838; p2_emc_m_sdp[0][0][8][0]=1.35294; p3_emc_m_sdp[0][0][8][0]=-0.429548; p4_emc_m_sdp[0][0][8][0]=0.0461155; 
  p0_emc_s_sdp[0][0][8][0]=2.1833; p1_emc_s_sdp[0][0][8][0]=-3.44597; p2_emc_s_sdp[0][0][8][0]=2.84155; p3_emc_s_sdp[0][0][8][0]=-0.932839; p4_emc_s_sdp[0][0][8][0]=0.102305; 
  p0_emc_m_sdp[1][0][8][0]=-11.8521; p1_emc_m_sdp[1][0][8][0]=45.9761; p2_emc_m_sdp[1][0][8][0]=-11.2755; p3_emc_m_sdp[1][0][8][0]=-124.194; p4_emc_m_sdp[1][0][8][0]=116.942; 
  p0_emc_s_sdp[1][0][8][0]=-48.0537; p1_emc_s_sdp[1][0][8][0]=193.027; p2_emc_s_sdp[1][0][8][0]=-60.5217; p3_emc_s_sdp[1][0][8][0]=-506.176; p4_emc_s_sdp[1][0][8][0]=492.871; 
  p0_emc_m_sdp[0][0][9][0]=0.0427274; p1_emc_m_sdp[0][0][9][0]=-0.198572; p2_emc_m_sdp[0][0][9][0]=0.188692; p3_emc_m_sdp[0][0][9][0]=-0.0484089; p4_emc_m_sdp[0][0][9][0]=0.000651466; 
  p0_emc_s_sdp[0][0][9][0]=0.784478; p1_emc_s_sdp[0][0][9][0]=0.0815018; p2_emc_s_sdp[0][0][9][0]=-0.169448; p3_emc_s_sdp[0][0][9][0]=0.10036; p4_emc_s_sdp[0][0][9][0]=-0.0144796; 
  p0_emc_m_sdp[1][0][9][0]=-13.2665; p1_emc_m_sdp[1][0][9][0]=54.7018; p2_emc_m_sdp[1][0][9][0]=-16.8677; p3_emc_m_sdp[1][0][9][0]=-157.084; p4_emc_m_sdp[1][0][9][0]=157.085; 
  p0_emc_s_sdp[1][0][9][0]=-6.5109; p1_emc_s_sdp[1][0][9][0]=30.3034; p2_emc_s_sdp[1][0][9][0]=-9.50263; p3_emc_s_sdp[1][0][9][0]=-83.4055; p4_emc_s_sdp[1][0][9][0]=81.349; 
  p0_emc_m_sdp[0][1][0][0]=-0.00393091; p1_emc_m_sdp[0][1][0][0]=-0.279242; p2_emc_m_sdp[0][1][0][0]=0.0859636; p3_emc_m_sdp[0][1][0][0]=0.0258167; p4_emc_m_sdp[0][1][0][0]=-0.0103407; 
  p0_emc_s_sdp[0][1][0][0]=0.815802; p1_emc_s_sdp[0][1][0][0]=-0.0934828; p2_emc_s_sdp[0][1][0][0]=0.06811; p3_emc_s_sdp[0][1][0][0]=0.0161393; p4_emc_s_sdp[0][1][0][0]=-0.0113814; 
  p0_emc_m_sdp[1][1][0][0]=-3.03134; p1_emc_m_sdp[1][1][0][0]=10.8873; p2_emc_m_sdp[1][1][0][0]=-0.978681; p3_emc_m_sdp[1][1][0][0]=-30.3125; p4_emc_m_sdp[1][1][0][0]=25.28; 
  p0_emc_s_sdp[1][1][0][0]=7.17269; p1_emc_s_sdp[1][1][0][0]=-28.124; p2_emc_s_sdp[1][1][0][0]=12.2075; p3_emc_s_sdp[1][1][0][0]=78.7261; p4_emc_s_sdp[1][1][0][0]=-84.0685; 
  p0_emc_m_sdp[0][1][1][0]=0.197801; p1_emc_m_sdp[0][1][1][0]=-0.941951; p2_emc_m_sdp[0][1][1][0]=0.797984; p3_emc_m_sdp[0][1][1][0]=-0.268795; p4_emc_m_sdp[0][1][1][0]=0.0313788; 
  p0_emc_s_sdp[0][1][1][0]=0.685802; p1_emc_s_sdp[0][1][1][0]=0.486194; p2_emc_s_sdp[0][1][1][0]=-0.715628; p3_emc_s_sdp[0][1][1][0]=0.399955; p4_emc_s_sdp[0][1][1][0]=-0.0734662; 
  p0_emc_m_sdp[1][1][1][0]=3.52903; p1_emc_m_sdp[1][1][1][0]=-13.233; p2_emc_m_sdp[1][1][1][0]=1.92813; p3_emc_m_sdp[1][1][1][0]=36.2416; p4_emc_m_sdp[1][1][1][0]=-32.877; 
  p0_emc_s_sdp[1][1][1][0]=-3.39036; p1_emc_s_sdp[1][1][1][0]=12.9972; p2_emc_s_sdp[1][1][1][0]=2.20018; p3_emc_s_sdp[1][1][1][0]=-33.3422; p4_emc_s_sdp[1][1][1][0]=21.5963; 
  p0_emc_m_sdp[0][1][2][0]=0.0189003; p1_emc_m_sdp[0][1][2][0]=-0.399981; p2_emc_m_sdp[0][1][2][0]=0.240549; p3_emc_m_sdp[0][1][2][0]=-0.052835; p4_emc_m_sdp[0][1][2][0]=0.00407246; 
  p0_emc_s_sdp[0][1][2][0]=1.09855; p1_emc_s_sdp[0][1][2][0]=-0.784923; p2_emc_s_sdp[0][1][2][0]=0.616378; p3_emc_s_sdp[0][1][2][0]=-0.18022; p4_emc_s_sdp[0][1][2][0]=0.0159754; 
  p0_emc_m_sdp[1][1][2][0]=0.719388; p1_emc_m_sdp[1][1][2][0]=-0.881438; p2_emc_m_sdp[1][1][2][0]=-2.32915; p3_emc_m_sdp[1][1][2][0]=-1.45903; p4_emc_m_sdp[1][1][2][0]=6.01373; 
  p0_emc_s_sdp[1][1][2][0]=3.41348; p1_emc_s_sdp[1][1][2][0]=-13.0134; p2_emc_s_sdp[1][1][2][0]=6.29379; p3_emc_s_sdp[1][1][2][0]=40.6382; p4_emc_s_sdp[1][1][2][0]=-45.0287; 
  p0_emc_m_sdp[0][1][3][0]=0.0877597; p1_emc_m_sdp[0][1][3][0]=-0.521742; p2_emc_m_sdp[0][1][3][0]=0.191053; p3_emc_m_sdp[0][1][3][0]=0.0703397; p4_emc_m_sdp[0][1][3][0]=-0.0350074; 
  p0_emc_s_sdp[0][1][3][0]=0.155382; p1_emc_s_sdp[0][1][3][0]=1.52607; p2_emc_s_sdp[0][1][3][0]=-1.37195; p3_emc_s_sdp[0][1][3][0]=0.54038; p4_emc_s_sdp[0][1][3][0]=-0.0775398; 
  p0_emc_m_sdp[1][1][3][0]=-0.0754667; p1_emc_m_sdp[1][1][3][0]=-0.27559; p2_emc_m_sdp[1][1][3][0]=-0.344142; p3_emc_m_sdp[1][1][3][0]=0.0374254; p4_emc_m_sdp[1][1][3][0]=1.42971; 
  p0_emc_s_sdp[1][1][3][0]=-7.26264; p1_emc_s_sdp[1][1][3][0]=32.6982; p2_emc_s_sdp[1][1][3][0]=-10.6694; p3_emc_s_sdp[1][1][3][0]=-88.0305; p4_emc_s_sdp[1][1][3][0]=86.2714; 
  p0_emc_m_sdp[0][1][4][0]=0.301691; p1_emc_m_sdp[0][1][4][0]=-0.926772; p2_emc_m_sdp[0][1][4][0]=0.639811; p3_emc_m_sdp[0][1][4][0]=-0.176642; p4_emc_m_sdp[0][1][4][0]=0.0174446; 
  p0_emc_s_sdp[0][1][4][0]=2.48337; p1_emc_s_sdp[0][1][4][0]=-3.9063; p2_emc_s_sdp[0][1][4][0]=3.0989; p3_emc_s_sdp[0][1][4][0]=-0.989322; p4_emc_s_sdp[0][1][4][0]=0.105812; 
  p0_emc_m_sdp[1][1][4][0]=-18.0472; p1_emc_m_sdp[1][1][4][0]=72.643; p2_emc_m_sdp[1][1][4][0]=-22.0204; p3_emc_m_sdp[1][1][4][0]=-200.796; p4_emc_m_sdp[1][1][4][0]=197.115; 
  p0_emc_s_sdp[1][1][4][0]=-16.6741; p1_emc_s_sdp[1][1][4][0]=66.4942; p2_emc_s_sdp[1][1][4][0]=-10.5685; p3_emc_s_sdp[1][1][4][0]=-197.038; p4_emc_s_sdp[1][1][4][0]=184.223; 
  p0_emc_m_sdp[0][1][5][0]=-0.21219; p1_emc_m_sdp[0][1][5][0]=0.560245; p2_emc_m_sdp[0][1][5][0]=-0.498168; p3_emc_m_sdp[0][1][5][0]=0.162451; p4_emc_m_sdp[0][1][5][0]=-0.0156458; 
  p0_emc_s_sdp[0][1][5][0]=1.36464; p1_emc_s_sdp[0][1][5][0]=-1.29726; p2_emc_s_sdp[0][1][5][0]=0.996032; p3_emc_s_sdp[0][1][5][0]=-0.30805; p4_emc_s_sdp[0][1][5][0]=0.0311241; 
  p0_emc_m_sdp[1][1][5][0]=13.3331; p1_emc_m_sdp[1][1][5][0]=-44.893; p2_emc_m_sdp[1][1][5][0]=-7.89396; p3_emc_m_sdp[1][1][5][0]=153.253; p4_emc_m_sdp[1][1][5][0]=-128.665; 
  p0_emc_s_sdp[1][1][5][0]=-4.88829; p1_emc_s_sdp[1][1][5][0]=23.7353; p2_emc_s_sdp[1][1][5][0]=-0.234845; p3_emc_s_sdp[1][1][5][0]=-94.7094; p4_emc_s_sdp[1][1][5][0]=92.087; 
  p0_emc_m_sdp[0][1][6][0]=0.356124; p1_emc_m_sdp[0][1][6][0]=-0.815776; p2_emc_m_sdp[0][1][6][0]=0.602221; p3_emc_m_sdp[0][1][6][0]=-0.145441; p4_emc_m_sdp[0][1][6][0]=0.00201973; 
  p0_emc_s_sdp[0][1][6][0]=0.799972; p1_emc_s_sdp[0][1][6][0]=0.00820886; p2_emc_s_sdp[0][1][6][0]=0.0480217; p3_emc_s_sdp[0][1][6][0]=-0.0583201; p4_emc_s_sdp[0][1][6][0]=0.01708; 
  p0_emc_m_sdp[1][1][6][0]=-2.81442; p1_emc_m_sdp[1][1][6][0]=9.38883; p2_emc_m_sdp[1][1][6][0]=-1.18122; p3_emc_m_sdp[1][1][6][0]=-19.9452; p4_emc_m_sdp[1][1][6][0]=15.0756; 
  p0_emc_s_sdp[1][1][6][0]=-6.59826; p1_emc_s_sdp[1][1][6][0]=30.0301; p2_emc_s_sdp[1][1][6][0]=-8.99307; p3_emc_s_sdp[1][1][6][0]=-85.8523; p4_emc_s_sdp[1][1][6][0]=86.8754; 
  p0_emc_m_sdp[0][1][7][0]=-0.857332; p1_emc_m_sdp[0][1][7][0]=2.23563; p2_emc_m_sdp[0][1][7][0]=-1.90119; p3_emc_m_sdp[0][1][7][0]=0.622943; p4_emc_m_sdp[0][1][7][0]=-0.0627112; 
  p0_emc_s_sdp[0][1][7][0]=0.723884; p1_emc_s_sdp[0][1][7][0]=0.3555; p2_emc_s_sdp[0][1][7][0]=-0.397287; p3_emc_s_sdp[0][1][7][0]=0.17028; p4_emc_s_sdp[0][1][7][0]=-0.0233132; 
  p0_emc_m_sdp[1][1][7][0]=-0.0563574; p1_emc_m_sdp[1][1][7][0]=0.0569309; p2_emc_m_sdp[1][1][7][0]=0.134312; p3_emc_m_sdp[1][1][7][0]=0.0757613; p4_emc_m_sdp[1][1][7][0]=-0.282394; 
  p0_emc_s_sdp[1][1][7][0]=-2.82721; p1_emc_s_sdp[1][1][7][0]=10.6858; p2_emc_s_sdp[1][1][7][0]=3.95445; p3_emc_s_sdp[1][1][7][0]=-29.6863; p4_emc_s_sdp[1][1][7][0]=17.5678; 
  p0_emc_m_sdp[0][1][8][0]=0.543241; p1_emc_m_sdp[0][1][8][0]=-1.29411; p2_emc_m_sdp[0][1][8][0]=1.12184; p3_emc_m_sdp[0][1][8][0]=-0.391508; p4_emc_m_sdp[0][1][8][0]=0.0444396; 
  p0_emc_s_sdp[0][1][8][0]=0.853813; p1_emc_s_sdp[0][1][8][0]=-0.0584386; p2_emc_s_sdp[0][1][8][0]=0.0342891; p3_emc_s_sdp[0][1][8][0]=0.00300294; p4_emc_s_sdp[0][1][8][0]=-0.00326225; 
  p0_emc_m_sdp[1][1][8][0]=7.7644; p1_emc_m_sdp[1][1][8][0]=-32.2586; p2_emc_m_sdp[1][1][8][0]=12.0993; p3_emc_m_sdp[1][1][8][0]=89.0844; p4_emc_m_sdp[1][1][8][0]=-91.7712; 
  p0_emc_s_sdp[1][1][8][0]=-20.2626; p1_emc_s_sdp[1][1][8][0]=83.4824; p2_emc_s_sdp[1][1][8][0]=-20.3682; p3_emc_s_sdp[1][1][8][0]=-237.098; p4_emc_s_sdp[1][1][8][0]=227.282; 
  p0_emc_m_sdp[0][1][9][0]=-0.413707; p1_emc_m_sdp[0][1][9][0]=1.18803; p2_emc_m_sdp[0][1][9][0]=-1.11515; p3_emc_m_sdp[0][1][9][0]=0.41748; p4_emc_m_sdp[0][1][9][0]=-0.053401; 
  p0_emc_s_sdp[0][1][9][0]=-0.198946; p1_emc_s_sdp[0][1][9][0]=2.74354; p2_emc_s_sdp[0][1][9][0]=-2.51003; p3_emc_s_sdp[0][1][9][0]=0.934384; p4_emc_s_sdp[0][1][9][0]=-0.116553; 
  p0_emc_m_sdp[1][1][9][0]=-4.49659; p1_emc_m_sdp[1][1][9][0]=17.4757; p2_emc_m_sdp[1][1][9][0]=-5.25821; p3_emc_m_sdp[1][1][9][0]=-44.8997; p4_emc_m_sdp[1][1][9][0]=43.2693; 
  p0_emc_s_sdp[1][1][9][0]=-8.55718; p1_emc_s_sdp[1][1][9][0]=35.8123; p2_emc_s_sdp[1][1][9][0]=-7.11956; p3_emc_s_sdp[1][1][9][0]=-98.5925; p4_emc_s_sdp[1][1][9][0]=90.4984; 
  p0_emc_m_sdp[0][2][0][0]=0.28979; p1_emc_m_sdp[0][2][0][0]=-0.922319; p2_emc_m_sdp[0][2][0][0]=0.804553; p3_emc_m_sdp[0][2][0][0]=-0.293933; p4_emc_m_sdp[0][2][0][0]=0.0387789; 
  p0_emc_s_sdp[0][2][0][0]=0.336552; p1_emc_s_sdp[0][2][0][0]=1.32405; p2_emc_s_sdp[0][2][0][0]=-1.22512; p3_emc_s_sdp[0][2][0][0]=0.468849; p4_emc_s_sdp[0][2][0][0]=-0.0594935; 
  p0_emc_m_sdp[1][2][0][0]=1.92992; p1_emc_m_sdp[1][2][0][0]=-7.31746; p2_emc_m_sdp[1][2][0][0]=0.692362; p3_emc_m_sdp[1][2][0][0]=21.8164; p4_emc_m_sdp[1][2][0][0]=-19.6391; 
  p0_emc_s_sdp[1][2][0][0]=-7.09531; p1_emc_s_sdp[1][2][0][0]=30.7576; p2_emc_s_sdp[1][2][0][0]=-8.0594; p3_emc_s_sdp[1][2][0][0]=-83.1711; p4_emc_s_sdp[1][2][0][0]=79.6804; 
  p0_emc_m_sdp[0][2][1][0]=0.531038; p1_emc_m_sdp[0][2][1][0]=-1.46456; p2_emc_m_sdp[0][2][1][0]=1.12496; p3_emc_m_sdp[0][2][1][0]=-0.316439; p4_emc_m_sdp[0][2][1][0]=0.0236676; 
  p0_emc_s_sdp[0][2][1][0]=0.616665; p1_emc_s_sdp[0][2][1][0]=0.425409; p2_emc_s_sdp[0][2][1][0]=-0.2948; p3_emc_s_sdp[0][2][1][0]=0.0950331; p4_emc_s_sdp[0][2][1][0]=-0.0116236; 
  p0_emc_m_sdp[1][2][1][0]=0.254929; p1_emc_m_sdp[1][2][1][0]=-0.551065; p2_emc_m_sdp[1][2][1][0]=-1.16683; p3_emc_m_sdp[1][2][1][0]=-0.445239; p4_emc_m_sdp[1][2][1][0]=3.8719; 
  p0_emc_s_sdp[1][2][1][0]=0.900123; p1_emc_s_sdp[1][2][1][0]=0.0672289; p2_emc_s_sdp[1][2][1][0]=-0.847808; p3_emc_s_sdp[1][2][1][0]=-0.777901; p4_emc_s_sdp[1][2][1][0]=2.48113; 
  p0_emc_m_sdp[0][2][2][0]=0.502832; p1_emc_m_sdp[0][2][2][0]=-1.49084; p2_emc_m_sdp[0][2][2][0]=1.23007; p3_emc_m_sdp[0][2][2][0]=-0.391372; p4_emc_m_sdp[0][2][2][0]=0.0404597; 
  p0_emc_s_sdp[0][2][2][0]=1.24799; p1_emc_s_sdp[0][2][2][0]=-1.1021; p2_emc_s_sdp[0][2][2][0]=0.950521; p3_emc_s_sdp[0][2][2][0]=-0.312453; p4_emc_s_sdp[0][2][2][0]=0.0327824; 
  p0_emc_m_sdp[1][2][2][0]=0.218428; p1_emc_m_sdp[1][2][2][0]=-0.409937; p2_emc_m_sdp[1][2][2][0]=-0.914561; p3_emc_m_sdp[1][2][2][0]=-0.451025; p4_emc_m_sdp[1][2][2][0]=2.69854; 
  p0_emc_s_sdp[1][2][2][0]=-0.283953; p1_emc_s_sdp[1][2][2][0]=5.89177; p2_emc_s_sdp[1][2][2][0]=-3.36699; p3_emc_s_sdp[1][2][2][0]=-19.4649; p4_emc_s_sdp[1][2][2][0]=22.8632; 
  p0_emc_m_sdp[0][2][3][0]=-0.425149; p1_emc_m_sdp[0][2][3][0]=0.730094; p2_emc_m_sdp[0][2][3][0]=-0.560885; p3_emc_m_sdp[0][2][3][0]=0.169788; p4_emc_m_sdp[0][2][3][0]=-0.0130821; 
  p0_emc_s_sdp[0][2][3][0]=0.374204; p1_emc_s_sdp[0][2][3][0]=0.972317; p2_emc_s_sdp[0][2][3][0]=-0.713703; p3_emc_s_sdp[0][2][3][0]=0.21542; p4_emc_s_sdp[0][2][3][0]=-0.021405; 
  p0_emc_m_sdp[1][2][3][0]=-4.30193; p1_emc_m_sdp[1][2][3][0]=19.597; p2_emc_m_sdp[1][2][3][0]=-8.02993; p3_emc_m_sdp[1][2][3][0]=-61.5864; p4_emc_m_sdp[1][2][3][0]=66.5179; 
  p0_emc_s_sdp[1][2][3][0]=6.83951; p1_emc_s_sdp[1][2][3][0]=-20.8363; p2_emc_s_sdp[1][2][3][0]=2.68455; p3_emc_s_sdp[1][2][3][0]=49.2653; p4_emc_s_sdp[1][2][3][0]=-39.9027; 
  p0_emc_m_sdp[0][2][4][0]=0.0350032; p1_emc_m_sdp[0][2][4][0]=-0.440068; p2_emc_m_sdp[0][2][4][0]=0.427908; p3_emc_m_sdp[0][2][4][0]=-0.142566; p4_emc_m_sdp[0][2][4][0]=0.0132785; 
  p0_emc_s_sdp[0][2][4][0]=1.0456; p1_emc_s_sdp[0][2][4][0]=-0.596357; p2_emc_s_sdp[0][2][4][0]=0.529215; p3_emc_s_sdp[0][2][4][0]=-0.182134; p4_emc_s_sdp[0][2][4][0]=0.0190766; 
  p0_emc_m_sdp[1][2][4][0]=-3.69342; p1_emc_m_sdp[1][2][4][0]=13.4951; p2_emc_m_sdp[1][2][4][0]=-1.81809; p3_emc_m_sdp[1][2][4][0]=-39.0873; p4_emc_m_sdp[1][2][4][0]=35.3381; 
  p0_emc_s_sdp[1][2][4][0]=1.21596; p1_emc_s_sdp[1][2][4][0]=0.00892516; p2_emc_s_sdp[1][2][4][0]=-1.38037; p3_emc_s_sdp[1][2][4][0]=-1.48751; p4_emc_s_sdp[1][2][4][0]=2.91923; 
  p0_emc_m_sdp[0][2][5][0]=0.810045; p1_emc_m_sdp[0][2][5][0]=-2.18576; p2_emc_m_sdp[0][2][5][0]=1.78524; p3_emc_m_sdp[0][2][5][0]=-0.545108; p4_emc_m_sdp[0][2][5][0]=0.0460892; 
  p0_emc_s_sdp[0][2][5][0]=0.385214; p1_emc_s_sdp[0][2][5][0]=1.05611; p2_emc_s_sdp[0][2][5][0]=-0.869683; p3_emc_s_sdp[0][2][5][0]=0.282574; p4_emc_s_sdp[0][2][5][0]=-0.0297989; 
  p0_emc_m_sdp[1][2][5][0]=-4.68979; p1_emc_m_sdp[1][2][5][0]=17.7251; p2_emc_m_sdp[1][2][5][0]=-4.1638; p3_emc_m_sdp[1][2][5][0]=-48.92; p4_emc_m_sdp[1][2][5][0]=46.6676; 
  p0_emc_s_sdp[1][2][5][0]=-8.47024; p1_emc_s_sdp[1][2][5][0]=39.3365; p2_emc_s_sdp[1][2][5][0]=-12.1346; p3_emc_s_sdp[1][2][5][0]=-115.938; p4_emc_s_sdp[1][2][5][0]=116.015; 
  p0_emc_m_sdp[0][2][6][0]=0.229045; p1_emc_m_sdp[0][2][6][0]=-0.705339; p2_emc_m_sdp[0][2][6][0]=0.550142; p3_emc_m_sdp[0][2][6][0]=-0.154826; p4_emc_m_sdp[0][2][6][0]=0.0107209; 
  p0_emc_s_sdp[0][2][6][0]=0.849861; p1_emc_s_sdp[0][2][6][0]=-0.024057; p2_emc_s_sdp[0][2][6][0]=0.0183288; p3_emc_s_sdp[0][2][6][0]=0.00152497; p4_emc_s_sdp[0][2][6][0]=-0.00208446; 
  p0_emc_m_sdp[1][2][6][0]=-6.98527; p1_emc_m_sdp[1][2][6][0]=27.6451; p2_emc_m_sdp[1][2][6][0]=-8.3113; p3_emc_m_sdp[1][2][6][0]=-73.967; p4_emc_m_sdp[1][2][6][0]=71.813; 
  p0_emc_s_sdp[1][2][6][0]=1.19483; p1_emc_s_sdp[1][2][6][0]=-0.228465; p2_emc_s_sdp[1][2][6][0]=-1.71285; p3_emc_s_sdp[1][2][6][0]=-1.28838; p4_emc_s_sdp[1][2][6][0]=5.08983; 
  p0_emc_m_sdp[0][2][7][0]=0.380839; p1_emc_m_sdp[0][2][7][0]=-1.08114; p2_emc_m_sdp[0][2][7][0]=0.89464; p3_emc_m_sdp[0][2][7][0]=-0.288841; p4_emc_m_sdp[0][2][7][0]=0.0298245; 
  p0_emc_s_sdp[0][2][7][0]=1.07614; p1_emc_s_sdp[0][2][7][0]=-0.374569; p2_emc_s_sdp[0][2][7][0]=0.152764; p3_emc_s_sdp[0][2][7][0]=0.0154932; p4_emc_s_sdp[0][2][7][0]=-0.0135165; 
  p0_emc_m_sdp[1][2][7][0]=-0.0371893; p1_emc_m_sdp[1][2][7][0]=-0.056344; p2_emc_m_sdp[1][2][7][0]=-0.0635006; p3_emc_m_sdp[1][2][7][0]=0.00656248; p4_emc_m_sdp[1][2][7][0]=0.374455; 
  p0_emc_s_sdp[1][2][7][0]=0.580221; p1_emc_s_sdp[1][2][7][0]=0.88988; p2_emc_s_sdp[1][2][7][0]=1.04719; p3_emc_s_sdp[1][2][7][0]=0.122869; p4_emc_s_sdp[1][2][7][0]=-5.03834; 
  p0_emc_m_sdp[0][2][8][0]=-0.270563; p1_emc_m_sdp[0][2][8][0]=0.54568; p2_emc_m_sdp[0][2][8][0]=-0.500052; p3_emc_m_sdp[0][2][8][0]=0.186573; p4_emc_m_sdp[0][2][8][0]=-0.0234747; 
  p0_emc_s_sdp[0][2][8][0]=0.36485; p1_emc_s_sdp[0][2][8][0]=1.17213; p2_emc_s_sdp[0][2][8][0]=-1.0207; p3_emc_s_sdp[0][2][8][0]=0.364191; p4_emc_s_sdp[0][2][8][0]=-0.0436496; 
  p0_emc_m_sdp[1][2][8][0]=-0.658077; p1_emc_m_sdp[1][2][8][0]=0.630743; p2_emc_m_sdp[1][2][8][0]=1.82515; p3_emc_m_sdp[1][2][8][0]=1.14436; p4_emc_m_sdp[1][2][8][0]=-5.14447; 
  p0_emc_s_sdp[1][2][8][0]=8.2531; p1_emc_s_sdp[1][2][8][0]=-29.6695; p2_emc_s_sdp[1][2][8][0]=8.22818; p3_emc_s_sdp[1][2][8][0]=82.4881; p4_emc_s_sdp[1][2][8][0]=-80.1236; 
  p0_emc_m_sdp[0][2][9][0]=-0.351346; p1_emc_m_sdp[0][2][9][0]=0.732935; p2_emc_m_sdp[0][2][9][0]=-0.590722; p3_emc_m_sdp[0][2][9][0]=0.177898; p4_emc_m_sdp[0][2][9][0]=-0.0145445; 
  p0_emc_s_sdp[0][2][9][0]=0.948977; p1_emc_s_sdp[0][2][9][0]=-0.193815; p2_emc_s_sdp[0][2][9][0]=0.091423; p3_emc_s_sdp[0][2][9][0]=-0.00587906; p4_emc_s_sdp[0][2][9][0]=-0.00109307; 
  p0_emc_m_sdp[1][2][9][0]=-13.5605; p1_emc_m_sdp[1][2][9][0]=53.4424; p2_emc_m_sdp[1][2][9][0]=-16.6564; p3_emc_m_sdp[1][2][9][0]=-139.356; p4_emc_m_sdp[1][2][9][0]=135.425; 
  p0_emc_s_sdp[1][2][9][0]=8.06772; p1_emc_s_sdp[1][2][9][0]=-28.453; p2_emc_s_sdp[1][2][9][0]=8.42535; p3_emc_s_sdp[1][2][9][0]=76.3265; p4_emc_s_sdp[1][2][9][0]=-74.6466; 

  //EMC/DZ/NEG/19GEV
 	
  p0_emc_m_sdz[0][0][0][0]=0.355687; p1_emc_m_sdz[0][0][0][0]=-1.29679; p2_emc_m_sdz[0][0][0][0]=1.51795; p3_emc_m_sdz[0][0][0][0]=-0.687969; p4_emc_m_sdz[0][0][0][0]=0.104589; 
  p0_emc_s_sdz[0][0][0][0]=1.16122; p1_emc_s_sdz[0][0][0][0]=-0.676249; p2_emc_s_sdz[0][0][0][0]=0.647086; p3_emc_s_sdz[0][0][0][0]=-0.220054; p4_emc_s_sdz[0][0][0][0]=0.0233665; 
  p0_emc_m_sdz[1][0][0][0]=103.654; p1_emc_m_sdz[1][0][0][0]=-402.585; p2_emc_m_sdz[1][0][0][0]=79.6397; p3_emc_m_sdz[1][0][0][0]=1127.13; p4_emc_m_sdz[1][0][0][0]=-1038.55; 
  p0_emc_s_sdz[1][0][0][0]=47.3783; p1_emc_s_sdz[1][0][0][0]=-159.915; p2_emc_s_sdz[1][0][0][0]=-107.356; p3_emc_s_sdz[1][0][0][0]=831.968; p4_emc_s_sdz[1][0][0][0]=-703.372; 
  p0_emc_m_sdz[0][0][1][0]=0.0923266; p1_emc_m_sdz[0][0][1][0]=0.155125; p2_emc_m_sdz[0][0][1][0]=-0.46854; p3_emc_m_sdz[0][0][1][0]=0.278613; p4_emc_m_sdz[0][0][1][0]=-0.0475672; 
  p0_emc_s_sdz[0][0][1][0]=0.282204; p1_emc_s_sdz[0][0][1][0]=1.4866; p2_emc_s_sdz[0][0][1][0]=-1.13433; p3_emc_s_sdz[0][0][1][0]=0.364899; p4_emc_s_sdz[0][0][1][0]=-0.0426157; 
  p0_emc_m_sdz[1][0][1][0]=-30.1119; p1_emc_m_sdz[1][0][1][0]=107.046; p2_emc_m_sdz[1][0][1][0]=-11.2775; p3_emc_m_sdz[1][0][1][0]=-274.6; p4_emc_m_sdz[1][0][1][0]=228.458; 
  p0_emc_s_sdz[1][0][1][0]=1.63755; p1_emc_s_sdz[1][0][1][0]=-1.94281; p2_emc_s_sdz[1][0][1][0]=-4.56645; p3_emc_s_sdz[1][0][1][0]=-1.23311; p4_emc_s_sdz[1][0][1][0]=16.8149; 
  p0_emc_m_sdz[0][0][2][0]=-0.407751; p1_emc_m_sdz[0][0][2][0]=1.09512; p2_emc_m_sdz[0][0][2][0]=-1.04021; p3_emc_m_sdz[0][0][2][0]=0.419058; p4_emc_m_sdz[0][0][2][0]=-0.0599075; 
  p0_emc_s_sdz[0][0][2][0]=1.26431; p1_emc_s_sdz[0][0][2][0]=-0.901338; p2_emc_s_sdz[0][0][2][0]=0.671579; p3_emc_s_sdz[0][0][2][0]=-0.164502; p4_emc_s_sdz[0][0][2][0]=0.00998344; 
  p0_emc_m_sdz[1][0][2][0]=-155.713; p1_emc_m_sdz[1][0][2][0]=623.945; p2_emc_m_sdz[1][0][2][0]=-217.667; p3_emc_m_sdz[1][0][2][0]=-1580.93; p4_emc_m_sdz[1][0][2][0]=1552.03; 
  p0_emc_s_sdz[1][0][2][0]=-39.8272; p1_emc_s_sdz[1][0][2][0]=142.089; p2_emc_s_sdz[1][0][2][0]=-15.3864; p3_emc_s_sdz[1][0][2][0]=-356.699; p4_emc_s_sdz[1][0][2][0]=295.842; 
  p0_emc_m_sdz[0][0][3][0]=0.0585653; p1_emc_m_sdz[0][0][3][0]=-0.0840336; p2_emc_m_sdz[0][0][3][0]=0.0214924; p3_emc_m_sdz[0][0][3][0]=0.00487749; p4_emc_m_sdz[0][0][3][0]=-0.00170556; 
  p0_emc_s_sdz[0][0][3][0]=0.284481; p1_emc_s_sdz[0][0][3][0]=1.35428; p2_emc_s_sdz[0][0][3][0]=-1.01988; p3_emc_s_sdz[0][0][3][0]=0.330725; p4_emc_s_sdz[0][0][3][0]=-0.0388006; 
  p0_emc_m_sdz[1][0][3][0]=185.685; p1_emc_m_sdz[1][0][3][0]=-702.296; p2_emc_m_sdz[1][0][3][0]=-8.71824; p3_emc_m_sdz[1][0][3][0]=2404.08; p4_emc_m_sdz[1][0][3][0]=-2161.95; 
  p0_emc_s_sdz[1][0][3][0]=14.1022; p1_emc_s_sdz[1][0][3][0]=-61.384; p2_emc_s_sdz[1][0][3][0]=10.0888; p3_emc_s_sdz[1][0][3][0]=249.792; p4_emc_s_sdz[1][0][3][0]=-259.852; 
  p0_emc_m_sdz[0][0][4][0]=-0.0391138; p1_emc_m_sdz[0][0][4][0]=-0.417758; p2_emc_m_sdz[0][0][4][0]=0.826298; p3_emc_m_sdz[0][0][4][0]=-0.450993; p4_emc_m_sdz[0][0][4][0]=0.0745487; 
  p0_emc_s_sdz[0][0][4][0]=0.760998; p1_emc_s_sdz[0][0][4][0]=0.514381; p2_emc_s_sdz[0][0][4][0]=-0.57329; p3_emc_s_sdz[0][0][4][0]=0.243031; p4_emc_s_sdz[0][0][4][0]=-0.0330259; 
  p0_emc_m_sdz[1][0][4][0]=-0.926129; p1_emc_m_sdz[1][0][4][0]=-0.213583; p2_emc_m_sdz[1][0][4][0]=0.65629; p3_emc_m_sdz[1][0][4][0]=1.07992; p4_emc_m_sdz[1][0][4][0]=-0.367118; 
  p0_emc_s_sdz[1][0][4][0]=16.3199; p1_emc_s_sdz[1][0][4][0]=-65.3877; p2_emc_s_sdz[1][0][4][0]=24.6457; p3_emc_s_sdz[1][0][4][0]=180.048; p4_emc_s_sdz[1][0][4][0]=-184.097; 
  p0_emc_m_sdz[0][0][5][0]=0.11311; p1_emc_m_sdz[0][0][5][0]=-0.187578; p2_emc_m_sdz[0][0][5][0]=0.0607477; p3_emc_m_sdz[0][0][5][0]=0.0137989; p4_emc_m_sdz[0][0][5][0]=-0.00677445; 
  p0_emc_s_sdz[0][0][5][0]=0.963197; p1_emc_s_sdz[0][0][5][0]=-0.0965353; p2_emc_s_sdz[0][0][5][0]=0.0342283; p3_emc_s_sdz[0][0][5][0]=0.0108235; p4_emc_s_sdz[0][0][5][0]=-0.00334608; 
  p0_emc_m_sdz[1][0][5][0]=-8.34729; p1_emc_m_sdz[1][0][5][0]=34.5055; p2_emc_m_sdz[1][0][5][0]=-11.283; p3_emc_m_sdz[1][0][5][0]=-97.8286; p4_emc_m_sdz[1][0][5][0]=99.3322; 
  p0_emc_s_sdz[1][0][5][0]=-12.6857; p1_emc_s_sdz[1][0][5][0]=55.7119; p2_emc_s_sdz[1][0][5][0]=-12.9757; p3_emc_s_sdz[1][0][5][0]=-171.134; p4_emc_s_sdz[1][0][5][0]=167.408; 
  p0_emc_m_sdz[0][0][6][0]=0.65952; p1_emc_m_sdz[0][0][6][0]=-1.42011; p2_emc_m_sdz[0][0][6][0]=1.06087; p3_emc_m_sdz[0][0][6][0]=-0.328284; p4_emc_m_sdz[0][0][6][0]=0.0356379; 
  p0_emc_s_sdz[0][0][6][0]=1.13362; p1_emc_s_sdz[0][0][6][0]=-0.390386; p2_emc_s_sdz[0][0][6][0]=0.159473; p3_emc_s_sdz[0][0][6][0]=0.0187535; p4_emc_s_sdz[0][0][6][0]=-0.0113201; 
  p0_emc_m_sdz[1][0][6][0]=-71.273; p1_emc_m_sdz[1][0][6][0]=276.048; p2_emc_m_sdz[1][0][6][0]=-91.1473; p3_emc_m_sdz[1][0][6][0]=-678.64; p4_emc_m_sdz[1][0][6][0]=655.108; 
  p0_emc_s_sdz[1][0][6][0]=62.2197; p1_emc_s_sdz[1][0][6][0]=-240.922; p2_emc_s_sdz[1][0][6][0]=53.0937; p3_emc_s_sdz[1][0][6][0]=706.056; p4_emc_s_sdz[1][0][6][0]=-678.524; 
  p0_emc_m_sdz[0][0][7][0]=0.0960176; p1_emc_m_sdz[0][0][7][0]=-0.116724; p2_emc_m_sdz[0][0][7][0]=-0.0257348; p3_emc_m_sdz[0][0][7][0]=0.0608179; p4_emc_m_sdz[0][0][7][0]=-0.0143815; 
  p0_emc_s_sdz[0][0][7][0]=1.40056; p1_emc_s_sdz[0][0][7][0]=-1.24352; p2_emc_s_sdz[0][0][7][0]=1.01315; p3_emc_s_sdz[0][0][7][0]=-0.309529; p4_emc_s_sdz[0][0][7][0]=0.0318587; 
  p0_emc_m_sdz[1][0][7][0]=-12.8598; p1_emc_m_sdz[1][0][7][0]=48.6505; p2_emc_m_sdz[1][0][7][0]=-13.8545; p3_emc_m_sdz[1][0][7][0]=-118.993; p4_emc_m_sdz[1][0][7][0]=110.782; 
  p0_emc_s_sdz[1][0][7][0]=-8.01741; p1_emc_s_sdz[1][0][7][0]=40.3855; p2_emc_s_sdz[1][0][7][0]=-16.9042; p3_emc_s_sdz[1][0][7][0]=-115.998; p4_emc_s_sdz[1][0][7][0]=121.456; 
  p0_emc_m_sdz[0][0][8][0]=0.0635913; p1_emc_m_sdz[0][0][8][0]=-0.122516; p2_emc_m_sdz[0][0][8][0]=0.0477457; p3_emc_m_sdz[0][0][8][0]=0.0081571; p4_emc_m_sdz[0][0][8][0]=-0.0046619; 
  p0_emc_s_sdz[0][0][8][0]=0.0999618; p1_emc_s_sdz[0][0][8][0]=1.83374; p2_emc_s_sdz[0][0][8][0]=-1.43048; p3_emc_s_sdz[0][0][8][0]=0.467108; p4_emc_s_sdz[0][0][8][0]=-0.0526562; 
  p0_emc_m_sdz[1][0][8][0]=-11.947; p1_emc_m_sdz[1][0][8][0]=47.3379; p2_emc_m_sdz[1][0][8][0]=-12.1317; p3_emc_m_sdz[1][0][8][0]=-131.079; p4_emc_m_sdz[1][0][8][0]=124.501; 
  p0_emc_s_sdz[1][0][8][0]=8.40442; p1_emc_s_sdz[1][0][8][0]=-26.7608; p2_emc_s_sdz[1][0][8][0]=-2.27018; p3_emc_s_sdz[1][0][8][0]=89.5339; p4_emc_s_sdz[1][0][8][0]=-77.2771; 
  p0_emc_m_sdz[0][0][9][0]=-0.287714; p1_emc_m_sdz[0][0][9][0]=0.692892; p2_emc_m_sdz[0][0][9][0]=-0.62863; p3_emc_m_sdz[0][0][9][0]=0.233757; p4_emc_m_sdz[0][0][9][0]=-0.030519; 
  p0_emc_s_sdz[0][0][9][0]=1.52678; p1_emc_s_sdz[0][0][9][0]=-1.49721; p2_emc_s_sdz[0][0][9][0]=1.2422; p3_emc_s_sdz[0][0][9][0]=-0.406623; p4_emc_s_sdz[0][0][9][0]=0.0460994; 
  p0_emc_m_sdz[1][0][9][0]=1.72999; p1_emc_m_sdz[1][0][9][0]=-0.0084883; p2_emc_m_sdz[1][0][9][0]=-6.2962; p3_emc_m_sdz[1][0][9][0]=-13.7778; p4_emc_m_sdz[1][0][9][0]=27.2454; 
  p0_emc_s_sdz[1][0][9][0]=13.7182; p1_emc_s_sdz[1][0][9][0]=-25.1064; p2_emc_s_sdz[1][0][9][0]=-51.7245; p3_emc_s_sdz[1][0][9][0]=136.367; p4_emc_s_sdz[1][0][9][0]=-66.8811; 
  p0_emc_m_sdz[0][1][0][0]=-0.636713; p1_emc_m_sdz[0][1][0][0]=1.31767; p2_emc_m_sdz[0][1][0][0]=-1.09903; p3_emc_m_sdz[0][1][0][0]=0.388685; p4_emc_m_sdz[0][1][0][0]=-0.0495133; 
  p0_emc_s_sdz[0][1][0][0]=0.937012; p1_emc_s_sdz[0][1][0][0]=-0.038909; p2_emc_s_sdz[0][1][0][0]=0.0232336; p3_emc_s_sdz[0][1][0][0]=0.000177529; p4_emc_s_sdz[0][1][0][0]=-0.000509485; 
  p0_emc_m_sdz[1][1][0][0]=-22.8466; p1_emc_m_sdz[1][1][0][0]=83.2384; p2_emc_m_sdz[1][1][0][0]=35.2081; p3_emc_m_sdz[1][1][0][0]=-396.305; p4_emc_m_sdz[1][1][0][0]=351.671; 
  p0_emc_s_sdz[1][1][0][0]=-110.721; p1_emc_s_sdz[1][1][0][0]=392.375; p2_emc_s_sdz[1][1][0][0]=217.815; p3_emc_s_sdz[1][1][0][0]=-1931.14; p4_emc_s_sdz[1][1][0][0]=1654.55; 
  p0_emc_m_sdz[0][1][1][0]=0.018987; p1_emc_m_sdz[0][1][1][0]=-0.313462; p2_emc_m_sdz[0][1][1][0]=0.377131; p3_emc_m_sdz[0][1][1][0]=-0.179191; p4_emc_m_sdz[0][1][1][0]=0.0299265; 
  p0_emc_s_sdz[0][1][1][0]=1.09303; p1_emc_s_sdz[0][1][1][0]=-0.363176; p2_emc_s_sdz[0][1][1][0]=0.232416; p3_emc_s_sdz[0][1][1][0]=-0.0566689; p4_emc_s_sdz[0][1][1][0]=0.00579967; 
  p0_emc_m_sdz[1][1][1][0]=-8.07548; p1_emc_m_sdz[1][1][1][0]=24.2268; p2_emc_m_sdz[1][1][1][0]=7.83368; p3_emc_m_sdz[1][1][1][0]=-81.0307; p4_emc_m_sdz[1][1][1][0]=62.773; 
  p0_emc_s_sdz[1][1][1][0]=-25.0521; p1_emc_s_sdz[1][1][1][0]=115.93; p2_emc_s_sdz[1][1][1][0]=-34.9644; p3_emc_s_sdz[1][1][1][0]=-376.467; p4_emc_s_sdz[1][1][1][0]=386.64; 
  p0_emc_m_sdz[0][1][2][0]=-1.10029; p1_emc_m_sdz[0][1][2][0]=2.76249; p2_emc_m_sdz[0][1][2][0]=-2.56295; p3_emc_m_sdz[0][1][2][0]=0.992761; p4_emc_m_sdz[0][1][2][0]=-0.136158; 
  p0_emc_s_sdz[0][1][2][0]=1.10784; p1_emc_s_sdz[0][1][2][0]=-0.394486; p2_emc_s_sdz[0][1][2][0]=0.186858; p3_emc_s_sdz[0][1][2][0]=0.00352527; p4_emc_s_sdz[0][1][2][0]=-0.00850342; 
  p0_emc_m_sdz[1][1][2][0]=-2.93713; p1_emc_m_sdz[1][1][2][0]=15.199; p2_emc_m_sdz[1][1][2][0]=-8.8817; p3_emc_m_sdz[1][1][2][0]=-49.7135; p4_emc_m_sdz[1][1][2][0]=58.5049; 
  p0_emc_s_sdz[1][1][2][0]=1.12412; p1_emc_s_sdz[1][1][2][0]=-0.0797636; p2_emc_s_sdz[1][1][2][0]=-1.32377; p3_emc_s_sdz[1][1][2][0]=-0.991843; p4_emc_s_sdz[1][1][2][0]=3.55466; 
  p0_emc_m_sdz[0][1][3][0]=-0.32583; p1_emc_m_sdz[0][1][3][0]=0.678688; p2_emc_m_sdz[0][1][3][0]=-0.687908; p3_emc_m_sdz[0][1][3][0]=0.295023; p4_emc_m_sdz[0][1][3][0]=-0.043909; 
  p0_emc_s_sdz[0][1][3][0]=0.835582; p1_emc_s_sdz[0][1][3][0]=0.166739; p2_emc_s_sdz[0][1][3][0]=-0.148382; p3_emc_s_sdz[0][1][3][0]=0.0671327; p4_emc_s_sdz[0][1][3][0]=-0.0105813; 
  p0_emc_m_sdz[1][1][3][0]=0.315921; p1_emc_m_sdz[1][1][3][0]=-0.534898; p2_emc_m_sdz[1][1][3][0]=-1.49107; p3_emc_m_sdz[1][1][3][0]=-0.796946; p4_emc_m_sdz[1][1][3][0]=4.92783; 
  p0_emc_s_sdz[1][1][3][0]=-8.1688; p1_emc_s_sdz[1][1][3][0]=46.0876; p2_emc_s_sdz[1][1][3][0]=-27.8465; p3_emc_s_sdz[1][1][3][0]=-125.515; p4_emc_s_sdz[1][1][3][0]=142.622; 
  p0_emc_m_sdz[0][1][4][0]=-0.318055; p1_emc_m_sdz[0][1][4][0]=0.547934; p2_emc_m_sdz[0][1][4][0]=-0.326348; p3_emc_m_sdz[0][1][4][0]=0.0697005; p4_emc_m_sdz[0][1][4][0]=-0.00354897; 
  p0_emc_s_sdz[0][1][4][0]=0.907277; p1_emc_s_sdz[0][1][4][0]=-0.0173176; p2_emc_s_sdz[0][1][4][0]=0.0148569; p3_emc_s_sdz[0][1][4][0]=0.00106434; p4_emc_s_sdz[0][1][4][0]=-0.000575929; 
  p0_emc_m_sdz[1][1][4][0]=13.9388; p1_emc_m_sdz[1][1][4][0]=-57.3649; p2_emc_m_sdz[1][1][4][0]=20.2059; p3_emc_m_sdz[1][1][4][0]=153.594; p4_emc_m_sdz[1][1][4][0]=-153.562; 
  p0_emc_s_sdz[1][1][4][0]=-17.7619; p1_emc_s_sdz[1][1][4][0]=70.6604; p2_emc_s_sdz[1][1][4][0]=-10.4034; p3_emc_s_sdz[1][1][4][0]=-208.217; p4_emc_s_sdz[1][1][4][0]=192.374; 
  p0_emc_m_sdz[0][1][5][0]=-0.1069; p1_emc_m_sdz[0][1][5][0]=0.458016; p2_emc_m_sdz[0][1][5][0]=-0.425184; p3_emc_m_sdz[0][1][5][0]=0.156136; p4_emc_m_sdz[0][1][5][0]=-0.0206961; 
  p0_emc_s_sdz[0][1][5][0]=1.12461; p1_emc_s_sdz[0][1][5][0]=-0.640183; p2_emc_s_sdz[0][1][5][0]=0.607164; p3_emc_s_sdz[0][1][5][0]=-0.230945; p4_emc_s_sdz[0][1][5][0]=0.0312474; 
  p0_emc_m_sdz[1][1][5][0]=-33.3882; p1_emc_m_sdz[1][1][5][0]=135.782; p2_emc_m_sdz[1][1][5][0]=-43.1748; p3_emc_m_sdz[1][1][5][0]=-367.076; p4_emc_m_sdz[1][1][5][0]=360.343; 
  p0_emc_s_sdz[1][1][5][0]=-19.9183; p1_emc_s_sdz[1][1][5][0]=76.8855; p2_emc_s_sdz[1][1][5][0]=-13.0504; p3_emc_s_sdz[1][1][5][0]=-205.05; p4_emc_s_sdz[1][1][5][0]=182.856; 
  p0_emc_m_sdz[0][1][6][0]=-0.0198469; p1_emc_m_sdz[0][1][6][0]=0.153357; p2_emc_m_sdz[0][1][6][0]=-0.157961; p3_emc_m_sdz[0][1][6][0]=0.0584029; p4_emc_m_sdz[0][1][6][0]=-0.007176; 
  p0_emc_s_sdz[0][1][6][0]=0.988058; p1_emc_s_sdz[0][1][6][0]=-0.126604; p2_emc_s_sdz[0][1][6][0]=0.0531857; p3_emc_s_sdz[0][1][6][0]=0.00780567; p4_emc_s_sdz[0][1][6][0]=-0.00420978; 
  p0_emc_m_sdz[1][1][6][0]=0.616576; p1_emc_m_sdz[1][1][6][0]=-0.702881; p2_emc_m_sdz[1][1][6][0]=-1.79606; p3_emc_m_sdz[1][1][6][0]=-0.975906; p4_emc_m_sdz[1][1][6][0]=5.0039; 
  p0_emc_s_sdz[1][1][6][0]=-4.75924; p1_emc_s_sdz[1][1][6][0]=22.1417; p2_emc_s_sdz[1][1][6][0]=-7.48817; p3_emc_s_sdz[1][1][6][0]=-54.3545; p4_emc_s_sdz[1][1][6][0]=52.7845; 
  p0_emc_m_sdz[0][1][7][0]=0.0445899; p1_emc_m_sdz[0][1][7][0]=0.14158; p2_emc_m_sdz[0][1][7][0]=-0.255014; p3_emc_m_sdz[0][1][7][0]=0.140116; p4_emc_m_sdz[0][1][7][0]=-0.0244824; 
  p0_emc_s_sdz[0][1][7][0]=1.5465; p1_emc_s_sdz[0][1][7][0]=-1.41625; p2_emc_s_sdz[0][1][7][0]=1.00964; p3_emc_s_sdz[0][1][7][0]=-0.264157; p4_emc_s_sdz[0][1][7][0]=0.0199827; 
  p0_emc_m_sdz[1][1][7][0]=28.4992; p1_emc_m_sdz[1][1][7][0]=-95.6622; p2_emc_m_sdz[1][1][7][0]=-17.1235; p3_emc_m_sdz[1][1][7][0]=347.734; p4_emc_m_sdz[1][1][7][0]=-303.758; 
  p0_emc_s_sdz[1][1][7][0]=-56.9991; p1_emc_s_sdz[1][1][7][0]=220.22; p2_emc_s_sdz[1][1][7][0]=-41.4817; p3_emc_s_sdz[1][1][7][0]=-601.523; p4_emc_s_sdz[1][1][7][0]=546.608; 
  p0_emc_m_sdz[0][1][8][0]=-0.377802; p1_emc_m_sdz[0][1][8][0]=1.11824; p2_emc_m_sdz[0][1][8][0]=-1.00081; p3_emc_m_sdz[0][1][8][0]=0.373151; p4_emc_m_sdz[0][1][8][0]=-0.0503301; 
  p0_emc_s_sdz[0][1][8][0]=0.88146; p1_emc_s_sdz[0][1][8][0]=0.0657168; p2_emc_s_sdz[0][1][8][0]=-0.0302805; p3_emc_s_sdz[0][1][8][0]=-0.00270235; p4_emc_s_sdz[0][1][8][0]=0.00338672; 
  p0_emc_m_sdz[1][1][8][0]=12.2729; p1_emc_m_sdz[1][1][8][0]=-47.4669; p2_emc_m_sdz[1][1][8][0]=11.822; p3_emc_m_sdz[1][1][8][0]=129.661; p4_emc_m_sdz[1][1][8][0]=-123.22; 
  p0_emc_s_sdz[1][1][8][0]=2.77185; p1_emc_s_sdz[1][1][8][0]=-8.87908; p2_emc_s_sdz[1][1][8][0]=4.82227; p3_emc_s_sdz[1][1][8][0]=24.5423; p4_emc_s_sdz[1][1][8][0]=-27.3329; 
  p0_emc_m_sdz[0][1][9][0]=-0.456847; p1_emc_m_sdz[0][1][9][0]=1.27444; p2_emc_m_sdz[0][1][9][0]=-1.08703; p3_emc_m_sdz[0][1][9][0]=0.382408; p4_emc_m_sdz[0][1][9][0]=-0.0482836; 
  p0_emc_s_sdz[0][1][9][0]=1.16182; p1_emc_s_sdz[0][1][9][0]=-0.565734; p2_emc_s_sdz[0][1][9][0]=0.438673; p3_emc_s_sdz[0][1][9][0]=-0.129315; p4_emc_s_sdz[0][1][9][0]=0.0130424; 
  p0_emc_m_sdz[1][1][9][0]=-0.12532; p1_emc_m_sdz[1][1][9][0]=0.412803; p2_emc_m_sdz[1][1][9][0]=0.765118; p3_emc_m_sdz[1][1][9][0]=0.159604; p4_emc_m_sdz[1][1][9][0]=-2.92852; 
  p0_emc_s_sdz[1][1][9][0]=-18.136; p1_emc_s_sdz[1][1][9][0]=75.6131; p2_emc_s_sdz[1][1][9][0]=-20.8206; p3_emc_s_sdz[1][1][9][0]=-209.195; p4_emc_s_sdz[1][1][9][0]=202.862; 
  p0_emc_m_sdz[0][2][0][0]=0.464349; p1_emc_m_sdz[0][2][0][0]=-1.11612; p2_emc_m_sdz[0][2][0][0]=0.933133; p3_emc_m_sdz[0][2][0][0]=-0.335032; p4_emc_m_sdz[0][2][0][0]=0.0439909; 
  p0_emc_s_sdz[0][2][0][0]=0.925546; p1_emc_s_sdz[0][2][0][0]=0.0648648; p2_emc_s_sdz[0][2][0][0]=-0.111159; p3_emc_s_sdz[0][2][0][0]=0.0661098; p4_emc_s_sdz[0][2][0][0]=-0.0107466; 
  p0_emc_m_sdz[1][2][0][0]=-7.09963; p1_emc_m_sdz[1][2][0][0]=26.4986; p2_emc_m_sdz[1][2][0][0]=-4.85693; p3_emc_m_sdz[1][2][0][0]=-73.0047; p4_emc_m_sdz[1][2][0][0]=67.3511; 
  p0_emc_s_sdz[1][2][0][0]=-6.89147; p1_emc_s_sdz[1][2][0][0]=29.785; p2_emc_s_sdz[1][2][0][0]=-8.48171; p3_emc_s_sdz[1][2][0][0]=-73.2496; p4_emc_s_sdz[1][2][0][0]=68.7937; 
  p0_emc_m_sdz[0][2][1][0]=0.182711; p1_emc_m_sdz[0][2][1][0]=-0.531898; p2_emc_m_sdz[0][2][1][0]=0.501077; p3_emc_m_sdz[0][2][1][0]=-0.193893; p4_emc_m_sdz[0][2][1][0]=0.0267186; 
  p0_emc_s_sdz[0][2][1][0]=0.636466; p1_emc_s_sdz[0][2][1][0]=0.64735; p2_emc_s_sdz[0][2][1][0]=-0.475112; p3_emc_s_sdz[0][2][1][0]=0.143188; p4_emc_s_sdz[0][2][1][0]=-0.0139997; 
  p0_emc_m_sdz[1][2][1][0]=-24.8774; p1_emc_m_sdz[1][2][1][0]=101.539; p2_emc_m_sdz[1][2][1][0]=-34.7804; p3_emc_m_sdz[1][2][1][0]=-274.756; p4_emc_m_sdz[1][2][1][0]=275.379; 
  p0_emc_s_sdz[1][2][1][0]=-11.5548; p1_emc_s_sdz[1][2][1][0]=48.0151; p2_emc_s_sdz[1][2][1][0]=-12.9883; p3_emc_s_sdz[1][2][1][0]=-125.592; p4_emc_s_sdz[1][2][1][0]=119.138; 
  p0_emc_m_sdz[0][2][2][0]=0.00871784; p1_emc_m_sdz[0][2][2][0]=-0.077485; p2_emc_m_sdz[0][2][2][0]=0.107872; p3_emc_m_sdz[0][2][2][0]=-0.056612; p4_emc_m_sdz[0][2][2][0]=0.0100081; 
  p0_emc_s_sdz[0][2][2][0]=0.566415; p1_emc_s_sdz[0][2][2][0]=0.741285; p2_emc_s_sdz[0][2][2][0]=-0.547406; p3_emc_s_sdz[0][2][2][0]=0.166486; p4_emc_s_sdz[0][2][2][0]=-0.0161185; 
  p0_emc_m_sdz[1][2][2][0]=6.74132; p1_emc_m_sdz[1][2][2][0]=-24.8158; p2_emc_m_sdz[1][2][2][0]=4.10901; p3_emc_m_sdz[1][2][2][0]=64.3255; p4_emc_m_sdz[1][2][2][0]=-55.44; 
  p0_emc_s_sdz[1][2][2][0]=-2.57049; p1_emc_s_sdz[1][2][2][0]=17.0703; p2_emc_s_sdz[1][2][2][0]=-9.33151; p3_emc_s_sdz[1][2][2][0]=-49.1043; p4_emc_s_sdz[1][2][2][0]=55.5185; 
  p0_emc_m_sdz[0][2][3][0]=0.177317; p1_emc_m_sdz[0][2][3][0]=-0.441143; p2_emc_m_sdz[0][2][3][0]=0.391317; p3_emc_m_sdz[0][2][3][0]=-0.15044; p4_emc_m_sdz[0][2][3][0]=0.0213009; 
  p0_emc_s_sdz[0][2][3][0]=1.15316; p1_emc_s_sdz[0][2][3][0]=-0.540972; p2_emc_s_sdz[0][2][3][0]=0.370077; p3_emc_s_sdz[0][2][3][0]=-0.0916475; p4_emc_s_sdz[0][2][3][0]=0.0075907; 
  p0_emc_m_sdz[1][2][3][0]=-6.1071; p1_emc_m_sdz[1][2][3][0]=25.2465; p2_emc_m_sdz[1][2][3][0]=-8.19063; p3_emc_m_sdz[1][2][3][0]=-72.3827; p4_emc_m_sdz[1][2][3][0]=73.4766; 
  p0_emc_s_sdz[1][2][3][0]=-2.3061; p1_emc_s_sdz[1][2][3][0]=14.855; p2_emc_s_sdz[1][2][3][0]=-5.33718; p3_emc_s_sdz[1][2][3][0]=-48.2175; p4_emc_s_sdz[1][2][3][0]=51.2119; 
  p0_emc_m_sdz[0][2][4][0]=0.0736254; p1_emc_m_sdz[0][2][4][0]=-0.229359; p2_emc_m_sdz[0][2][4][0]=0.205116; p3_emc_m_sdz[0][2][4][0]=-0.0768607; p4_emc_m_sdz[0][2][4][0]=0.0106948; 
  p0_emc_s_sdz[0][2][4][0]=0.880748; p1_emc_s_sdz[0][2][4][0]=0.0271051; p2_emc_s_sdz[0][2][4][0]=-0.0367252; p3_emc_s_sdz[0][2][4][0]=0.0270336; p4_emc_s_sdz[0][2][4][0]=-0.00505592; 
  p0_emc_m_sdz[1][2][4][0]=0.0702174; p1_emc_m_sdz[1][2][4][0]=-0.0684657; p2_emc_m_sdz[1][2][4][0]=-0.225009; p3_emc_m_sdz[1][2][4][0]=-0.208692; p4_emc_m_sdz[1][2][4][0]=0.399532; 
  p0_emc_s_sdz[1][2][4][0]=-12.3007; p1_emc_s_sdz[1][2][4][0]=48.5895; p2_emc_s_sdz[1][2][4][0]=-7.34623; p3_emc_s_sdz[1][2][4][0]=-133.814; p4_emc_s_sdz[1][2][4][0]=120.489; 
  p0_emc_m_sdz[0][2][5][0]=0.0397136; p1_emc_m_sdz[0][2][5][0]=0.106116; p2_emc_m_sdz[0][2][5][0]=-0.140826; p3_emc_m_sdz[0][2][5][0]=0.0578123; p4_emc_m_sdz[0][2][5][0]=-0.00888749; 
  p0_emc_s_sdz[0][2][5][0]=1.17747; p1_emc_s_sdz[0][2][5][0]=-0.62893; p2_emc_s_sdz[0][2][5][0]=0.424216; p3_emc_s_sdz[0][2][5][0]=-0.102312; p4_emc_s_sdz[0][2][5][0]=0.00784575; 
  p0_emc_m_sdz[1][2][5][0]=0.94235; p1_emc_m_sdz[1][2][5][0]=-1.20632; p2_emc_m_sdz[1][2][5][0]=-2.94927; p3_emc_m_sdz[1][2][5][0]=-1.33004; p4_emc_m_sdz[1][2][5][0]=9.53635; 
  p0_emc_s_sdz[1][2][5][0]=-9.96804; p1_emc_s_sdz[1][2][5][0]=41.5048; p2_emc_s_sdz[1][2][5][0]=-7.5703; p3_emc_s_sdz[1][2][5][0]=-117.213; p4_emc_s_sdz[1][2][5][0]=107.607; 
  p0_emc_m_sdz[0][2][6][0]=0.16011; p1_emc_m_sdz[0][2][6][0]=-0.15065; p2_emc_m_sdz[0][2][6][0]=0.0410081; p3_emc_m_sdz[0][2][6][0]=0.0083888; p4_emc_m_sdz[0][2][6][0]=-0.00373514; 
  p0_emc_s_sdz[0][2][6][0]=0.752859; p1_emc_s_sdz[0][2][6][0]=0.310726; p2_emc_s_sdz[0][2][6][0]=-0.283387; p3_emc_s_sdz[0][2][6][0]=0.114159; p4_emc_s_sdz[0][2][6][0]=-0.0148209; 
  p0_emc_m_sdz[1][2][6][0]=-5.37112; p1_emc_m_sdz[1][2][6][0]=20.0959; p2_emc_m_sdz[1][2][6][0]=-3.95799; p3_emc_m_sdz[1][2][6][0]=-52.2121; p4_emc_m_sdz[1][2][6][0]=47.0697; 
  p0_emc_s_sdz[1][2][6][0]=0.042974; p1_emc_s_sdz[1][2][6][0]=1.10536; p2_emc_s_sdz[1][2][6][0]=1.84915; p3_emc_s_sdz[1][2][6][0]=0.790974; p4_emc_s_sdz[1][2][6][0]=-4.98272; 
  p0_emc_m_sdz[0][2][7][0]=-0.0679219; p1_emc_m_sdz[0][2][7][0]=0.414709; p2_emc_m_sdz[0][2][7][0]=-0.447215; p3_emc_m_sdz[0][2][7][0]=0.184443; p4_emc_m_sdz[0][2][7][0]=-0.0263894; 
  p0_emc_s_sdz[0][2][7][0]=0.869281; p1_emc_s_sdz[0][2][7][0]=-0.0462533; p2_emc_s_sdz[0][2][7][0]=0.10903; p3_emc_s_sdz[0][2][7][0]=-0.051585; p4_emc_s_sdz[0][2][7][0]=0.00855595; 
  p0_emc_m_sdz[1][2][7][0]=10.7575; p1_emc_m_sdz[1][2][7][0]=-43.0567; p2_emc_m_sdz[1][2][7][0]=14.0369; p3_emc_m_sdz[1][2][7][0]=117.183; p4_emc_m_sdz[1][2][7][0]=-116.881; 
  p0_emc_s_sdz[1][2][7][0]=-2.44047; p1_emc_s_sdz[1][2][7][0]=15.1329; p2_emc_s_sdz[1][2][7][0]=-7.77848; p3_emc_s_sdz[1][2][7][0]=-45.2502; p4_emc_s_sdz[1][2][7][0]=52.1572; 
  p0_emc_m_sdz[0][2][8][0]=-0.209197; p1_emc_m_sdz[0][2][8][0]=0.742794; p2_emc_m_sdz[0][2][8][0]=-0.692161; p3_emc_m_sdz[0][2][8][0]=0.259889; p4_emc_m_sdz[0][2][8][0]=-0.0348923; 
  p0_emc_s_sdz[0][2][8][0]=0.810327; p1_emc_s_sdz[0][2][8][0]=0.238425; p2_emc_s_sdz[0][2][8][0]=-0.243117; p3_emc_s_sdz[0][2][8][0]=0.10877; p4_emc_s_sdz[0][2][8][0]=-0.0155928; 
  p0_emc_m_sdz[1][2][8][0]=-9.96914; p1_emc_m_sdz[1][2][8][0]=39.1071; p2_emc_m_sdz[1][2][8][0]=-12.9112; p3_emc_m_sdz[1][2][8][0]=-97.4166; p4_emc_m_sdz[1][2][8][0]=94.8366; 
  p0_emc_s_sdz[1][2][8][0]=-0.191776; p1_emc_s_sdz[1][2][8][0]=1.70941; p2_emc_s_sdz[1][2][8][0]=2.98192; p3_emc_s_sdz[1][2][8][0]=0.887936; p4_emc_s_sdz[1][2][8][0]=-9.50322; 
  p0_emc_m_sdz[0][2][9][0]=0.587675; p1_emc_m_sdz[0][2][9][0]=-1.11838; p2_emc_m_sdz[0][2][9][0]=0.848241; p3_emc_m_sdz[0][2][9][0]=-0.279141; p4_emc_m_sdz[0][2][9][0]=0.0328982; 
  p0_emc_s_sdz[0][2][9][0]=1.1911; p1_emc_s_sdz[0][2][9][0]=-0.808534; p2_emc_s_sdz[0][2][9][0]=0.679142; p3_emc_s_sdz[0][2][9][0]=-0.194755; p4_emc_s_sdz[0][2][9][0]=0.0157171; 
  p0_emc_m_sdz[1][2][9][0]=9.32722; p1_emc_m_sdz[1][2][9][0]=0.577042; p2_emc_m_sdz[1][2][9][0]=-32.2816; p3_emc_m_sdz[1][2][9][0]=-77.4985; p4_emc_m_sdz[1][2][9][0]=144.691; 
  p0_emc_s_sdz[1][2][9][0]=-3.09155; p1_emc_s_sdz[1][2][9][0]=0.630294; p2_emc_s_sdz[1][2][9][0]=14.5187; p3_emc_s_sdz[1][2][9][0]=33.8493; p4_emc_s_sdz[1][2][9][0]=-65.7588; 

  
  //EMC/DZ/POS/19 GeV
  
	
  p0_emc_m_sdz[0][0][0][1]=-3.58997; p1_emc_m_sdz[0][0][0][1]=7.75493; p2_emc_m_sdz[0][0][0][1]=-5.97059; p3_emc_m_sdz[0][0][0][1]=1.95434; p4_emc_m_sdz[0][0][0][1]=-0.231683; 
  p0_emc_s_sdz[0][0][0][1]=0.844786; p1_emc_s_sdz[0][0][0][1]=0.189225; p2_emc_s_sdz[0][0][0][1]=-0.0706982; p3_emc_s_sdz[0][0][0][1]=-0.00934466; p4_emc_s_sdz[0][0][0][1]=0.00803549; 
  p0_emc_m_sdz[1][0][0][1]=-260.095; p1_emc_m_sdz[1][0][0][1]=965.885; p2_emc_m_sdz[1][0][0][1]=-241.339; p3_emc_m_sdz[1][0][0][1]=-2366.9; p4_emc_m_sdz[1][0][0][1]=2156.38; 
  p0_emc_s_sdz[1][0][0][1]=28.5791; p1_emc_s_sdz[1][0][0][1]=-106.259; p2_emc_s_sdz[1][0][0][1]=18.6429; p3_emc_s_sdz[1][0][0][1]=333.613; p4_emc_s_sdz[1][0][0][1]=-323.996; 
  p0_emc_m_sdz[0][0][1][1]=0.0156286; p1_emc_m_sdz[0][0][1][1]=-0.0397543; p2_emc_m_sdz[0][0][1][1]=0.0922787; p3_emc_m_sdz[0][0][1][1]=-0.0761746; p4_emc_m_sdz[0][0][1][1]=0.0173207; 
  p0_emc_s_sdz[0][0][1][1]=0.908653; p1_emc_s_sdz[0][0][1][1]=0.177733; p2_emc_s_sdz[0][0][1][1]=-0.221198; p3_emc_s_sdz[0][0][1][1]=0.0990026; p4_emc_s_sdz[0][0][1][1]=-0.0132808; 
  p0_emc_m_sdz[1][0][1][1]=8.94983; p1_emc_m_sdz[1][0][1][1]=-36.0043; p2_emc_m_sdz[1][0][1][1]=13.8205; p3_emc_m_sdz[1][0][1][1]=85.526; p4_emc_m_sdz[1][0][1][1]=-82.9526; 
  p0_emc_s_sdz[1][0][1][1]=1.54204; p1_emc_s_sdz[1][0][1][1]=-0.443027; p2_emc_s_sdz[1][0][1][1]=-2.27747; p3_emc_s_sdz[1][0][1][1]=-1.46263; p4_emc_s_sdz[1][0][1][1]=6.91393; 
  p0_emc_m_sdz[0][0][2][1]=0.217619; p1_emc_m_sdz[0][0][2][1]=-0.549402; p2_emc_m_sdz[0][0][2][1]=0.466569; p3_emc_m_sdz[0][0][2][1]=-0.158524; p4_emc_m_sdz[0][0][2][1]=0.0189537; 
  p0_emc_s_sdz[0][0][2][1]=0.519469; p1_emc_s_sdz[0][0][2][1]=1.081; p2_emc_s_sdz[0][0][2][1]=-0.936986; p3_emc_s_sdz[0][0][2][1]=0.328496; p4_emc_s_sdz[0][0][2][1]=-0.0383334; 
  p0_emc_m_sdz[1][0][2][1]=-9.49902; p1_emc_m_sdz[1][0][2][1]=38.7195; p2_emc_m_sdz[1][0][2][1]=-11.2177; p3_emc_m_sdz[1][0][2][1]=-110.593; p4_emc_m_sdz[1][0][2][1]=109.498; 
  p0_emc_s_sdz[1][0][2][1]=-8.35507; p1_emc_s_sdz[1][0][2][1]=33.7543; p2_emc_s_sdz[1][0][2][1]=-3.68995; p3_emc_s_sdz[1][0][2][1]=-96.7478; p4_emc_s_sdz[1][0][2][1]=87.3181; 
  p0_emc_m_sdz[0][0][3][1]=-0.434812; p1_emc_m_sdz[0][0][3][1]=1.3347; p2_emc_m_sdz[0][0][3][1]=-1.36272; p3_emc_m_sdz[0][0][3][1]=0.560259; p4_emc_m_sdz[0][0][3][1]=-0.0796206; 
  p0_emc_s_sdz[0][0][3][1]=1.05899; p1_emc_s_sdz[0][0][3][1]=-0.179693; p2_emc_s_sdz[0][0][3][1]=0.0523986; p3_emc_s_sdz[0][0][3][1]=0.0100045; p4_emc_s_sdz[0][0][3][1]=-0.00260333; 
  p0_emc_m_sdz[1][0][3][1]=-16.0048; p1_emc_m_sdz[1][0][3][1]=63.4293; p2_emc_m_sdz[1][0][3][1]=-18.3207; p3_emc_m_sdz[1][0][3][1]=-170.875; p4_emc_m_sdz[1][0][3][1]=165.469; 
  p0_emc_s_sdz[1][0][3][1]=-4.69504; p1_emc_s_sdz[1][0][3][1]=20.5114; p2_emc_s_sdz[1][0][3][1]=-1.26485; p3_emc_s_sdz[1][0][3][1]=-57.4494; p4_emc_s_sdz[1][0][3][1]=47.3779; 
  p0_emc_m_sdz[0][0][4][1]=-0.364721; p1_emc_m_sdz[0][0][4][1]=1.07366; p2_emc_m_sdz[0][0][4][1]=-1.07906; p3_emc_m_sdz[0][0][4][1]=0.440101; p4_emc_m_sdz[0][0][4][1]=-0.0614838; 
  p0_emc_s_sdz[0][0][4][1]=0.121201; p1_emc_s_sdz[0][0][4][1]=1.95163; p2_emc_s_sdz[0][0][4][1]=-1.66209; p3_emc_s_sdz[0][0][4][1]=0.572499; p4_emc_s_sdz[0][0][4][1]=-0.0669685; 
  p0_emc_m_sdz[1][0][4][1]=9.63671; p1_emc_m_sdz[1][0][4][1]=-25.309; p2_emc_m_sdz[1][0][4][1]=-4.03992; p3_emc_m_sdz[1][0][4][1]=40.9151; p4_emc_m_sdz[1][0][4][1]=-15.4606; 
  p0_emc_s_sdz[1][0][4][1]=-45.6872; p1_emc_s_sdz[1][0][4][1]=175.751; p2_emc_s_sdz[1][0][4][1]=-24.2752; p3_emc_s_sdz[1][0][4][1]=-515.128; p4_emc_s_sdz[1][0][4][1]=471.399; 
  p0_emc_m_sdz[0][0][5][1]=0.267275; p1_emc_m_sdz[0][0][5][1]=-0.60577; p2_emc_m_sdz[0][0][5][1]=0.464259; p3_emc_m_sdz[0][0][5][1]=-0.157292; p4_emc_m_sdz[0][0][5][1]=0.0198563; 
  p0_emc_s_sdz[0][0][5][1]=0.598692; p1_emc_s_sdz[0][0][5][1]=0.826587; p2_emc_s_sdz[0][0][5][1]=-0.720265; p3_emc_s_sdz[0][0][5][1]=0.250121; p4_emc_s_sdz[0][0][5][1]=-0.0286145; 
  p0_emc_m_sdz[1][0][5][1]=0.731579; p1_emc_m_sdz[1][0][5][1]=-0.866905; p2_emc_m_sdz[1][0][5][1]=-2.23932; p3_emc_m_sdz[1][0][5][1]=-1.264; p4_emc_m_sdz[1][0][5][1]=6.33107; 
  p0_emc_s_sdz[1][0][5][1]=-5.08022; p1_emc_s_sdz[1][0][5][1]=24.1317; p2_emc_s_sdz[1][0][5][1]=-5.46756; p3_emc_s_sdz[1][0][5][1]=-70.283; p4_emc_s_sdz[1][0][5][1]=66.8347; 
  p0_emc_m_sdz[0][0][6][1]=0.0165754; p1_emc_m_sdz[0][0][6][1]=0.0158443; p2_emc_m_sdz[0][0][6][1]=-0.0916261; p3_emc_m_sdz[0][0][6][1]=0.0549995; p4_emc_m_sdz[0][0][6][1]=-0.00923172; 
  p0_emc_s_sdz[0][0][6][1]=1.27779; p1_emc_s_sdz[0][0][6][1]=-0.80372; p2_emc_s_sdz[0][0][6][1]=0.63177; p3_emc_s_sdz[0][0][6][1]=-0.190578; p4_emc_s_sdz[0][0][6][1]=0.0203724; 
  p0_emc_m_sdz[1][0][6][1]=-0.980332; p1_emc_m_sdz[1][0][6][1]=0.912428; p2_emc_m_sdz[1][0][6][1]=2.63119; p3_emc_m_sdz[1][0][6][1]=1.76978; p4_emc_m_sdz[1][0][6][1]=-6.58091; 
  p0_emc_s_sdz[1][0][6][1]=17.6454; p1_emc_s_sdz[1][0][6][1]=-66.5603; p2_emc_s_sdz[1][0][6][1]=18.1305; p3_emc_s_sdz[1][0][6][1]=186.49; p4_emc_s_sdz[1][0][6][1]=-181.7; 
  p0_emc_m_sdz[0][0][7][1]=-0.397218; p1_emc_m_sdz[0][0][7][1]=0.999022; p2_emc_m_sdz[0][0][7][1]=-0.917756; p3_emc_m_sdz[0][0][7][1]=0.358968; p4_emc_m_sdz[0][0][7][1]=-0.0510966; 
  p0_emc_s_sdz[0][0][7][1]=0.7599; p1_emc_s_sdz[0][0][7][1]=0.364188; p2_emc_s_sdz[0][0][7][1]=-0.243268; p3_emc_s_sdz[0][0][7][1]=0.0639733; p4_emc_s_sdz[0][0][7][1]=-0.00431806; 
  p0_emc_m_sdz[1][0][7][1]=-0.934798; p1_emc_m_sdz[1][0][7][1]=1.00422; p2_emc_m_sdz[1][0][7][1]=2.68314; p3_emc_m_sdz[1][0][7][1]=1.54153; p4_emc_m_sdz[1][0][7][1]=-7.34713; 
  p0_emc_s_sdz[1][0][7][1]=1.24729; p1_emc_s_sdz[1][0][7][1]=-0.163476; p2_emc_s_sdz[1][0][7][1]=-1.67691; p3_emc_s_sdz[1][0][7][1]=-1.32413; p4_emc_s_sdz[1][0][7][1]=4.58081; 
  p0_emc_m_sdz[0][0][8][1]=-0.441224; p1_emc_m_sdz[0][0][8][1]=1.05049; p2_emc_m_sdz[0][0][8][1]=-0.934333; p3_emc_m_sdz[0][0][8][1]=0.358241; p4_emc_m_sdz[0][0][8][1]=-0.049668; 
  p0_emc_s_sdz[0][0][8][1]=0.838917; p1_emc_s_sdz[0][0][8][1]=0.289188; p2_emc_s_sdz[0][0][8][1]=-0.293249; p3_emc_s_sdz[0][0][8][1]=0.105583; p4_emc_s_sdz[0][0][8][1]=-0.010355; 
  p0_emc_m_sdz[1][0][8][1]=-21.4508; p1_emc_m_sdz[1][0][8][1]=82.4082; p2_emc_m_sdz[1][0][8][1]=-21.9448; p3_emc_m_sdz[1][0][8][1]=-215.431; p4_emc_m_sdz[1][0][8][1]=203.521; 
  p0_emc_s_sdz[1][0][8][1]=36.7006; p1_emc_s_sdz[1][0][8][1]=-134.628; p2_emc_s_sdz[1][0][8][1]=13.1167; p3_emc_s_sdz[1][0][8][1]=418.047; p4_emc_s_sdz[1][0][8][1]=-385.409; 
  p0_emc_m_sdz[0][0][9][1]=-0.647027; p1_emc_m_sdz[0][0][9][1]=1.56633; p2_emc_m_sdz[0][0][9][1]=-1.39435; p3_emc_m_sdz[0][0][9][1]=0.515955; p4_emc_m_sdz[0][0][9][1]=-0.0672747; 
  p0_emc_s_sdz[0][0][9][1]=1.50265; p1_emc_s_sdz[0][0][9][1]=-1.53088; p2_emc_s_sdz[0][0][9][1]=1.30258; p3_emc_s_sdz[0][0][9][1]=-0.421554; p4_emc_s_sdz[0][0][9][1]=0.0456144; 
  p0_emc_m_sdz[1][0][9][1]=2.99404; p1_emc_m_sdz[1][0][9][1]=-14.8041; p2_emc_m_sdz[1][0][9][1]=10.388; p3_emc_m_sdz[1][0][9][1]=39.2309; p4_emc_m_sdz[1][0][9][1]=-48.8381; 
  p0_emc_s_sdz[1][0][9][1]=-1.4218; p1_emc_s_sdz[1][0][9][1]=2.78636; p2_emc_s_sdz[1][0][9][1]=6.36741; p3_emc_s_sdz[1][0][9][1]=3.32315; p4_emc_s_sdz[1][0][9][1]=-17.654; 
  p0_emc_m_sdz[0][1][0][1]=-0.253389; p1_emc_m_sdz[0][1][0][1]=0.265723; p2_emc_m_sdz[0][1][0][1]=-0.0539032; p3_emc_m_sdz[0][1][0][1]=-0.023016; p4_emc_m_sdz[0][1][0][1]=0.00627495; 
  p0_emc_s_sdz[0][1][0][1]=0.503336; p1_emc_s_sdz[0][1][0][1]=0.877967; p2_emc_s_sdz[0][1][0][1]=-0.594246; p3_emc_s_sdz[0][1][0][1]=0.181761; p4_emc_s_sdz[0][1][0][1]=-0.0204164; 
  p0_emc_m_sdz[1][1][0][1]=-0.864996; p1_emc_m_sdz[1][1][0][1]=-0.112303; p2_emc_m_sdz[1][1][0][1]=3.16135; p3_emc_m_sdz[1][1][0][1]=7.74351; p4_emc_m_sdz[1][1][0][1]=-14.8259; 
  p0_emc_s_sdz[1][1][0][1]=52.6918; p1_emc_s_sdz[1][1][0][1]=-164.223; p2_emc_s_sdz[1][1][0][1]=-160.016; p3_emc_s_sdz[1][1][0][1]=947.219; p4_emc_s_sdz[1][1][0][1]=-771.607; 
  p0_emc_m_sdz[0][1][1][1]=-0.353643; p1_emc_m_sdz[0][1][1][1]=0.624893; p2_emc_m_sdz[0][1][1][1]=-0.447254; p3_emc_m_sdz[0][1][1][1]=0.148869; p4_emc_m_sdz[0][1][1][1]=-0.019415; 
  p0_emc_s_sdz[0][1][1][1]=0.741669; p1_emc_s_sdz[0][1][1][1]=0.369722; p2_emc_s_sdz[0][1][1][1]=-0.260105; p3_emc_s_sdz[0][1][1][1]=0.0808326; p4_emc_s_sdz[0][1][1][1]=-0.00788359; 
  p0_emc_m_sdz[1][1][1][1]=-5.59298; p1_emc_m_sdz[1][1][1][1]=19.4783; p2_emc_m_sdz[1][1][1][1]=-3.48644; p3_emc_m_sdz[1][1][1][1]=-46.7314; p4_emc_m_sdz[1][1][1][1]=40.2835; 
  p0_emc_s_sdz[1][1][1][1]=14.341; p1_emc_s_sdz[1][1][1][1]=-59.1197; p2_emc_s_sdz[1][1][1][1]=21.7145; p3_emc_s_sdz[1][1][1][1]=176.744; p4_emc_s_sdz[1][1][1][1]=-183.516; 
  p0_emc_m_sdz[0][1][2][1]=-0.218802; p1_emc_m_sdz[0][1][2][1]=0.341366; p2_emc_m_sdz[0][1][2][1]=-0.213855; p3_emc_m_sdz[0][1][2][1]=0.058045; p4_emc_m_sdz[0][1][2][1]=-0.00572659; 
  p0_emc_s_sdz[0][1][2][1]=0.92964; p1_emc_s_sdz[0][1][2][1]=-0.0199732; p2_emc_s_sdz[0][1][2][1]=0.0240982; p3_emc_s_sdz[0][1][2][1]=0.00359399; p4_emc_s_sdz[0][1][2][1]=-0.00217877; 
  p0_emc_m_sdz[1][1][2][1]=-4.04809; p1_emc_m_sdz[1][1][2][1]=16.3799; p2_emc_m_sdz[1][1][2][1]=-4.75342; p3_emc_m_sdz[1][1][2][1]=-45.0468; p4_emc_m_sdz[1][1][2][1]=43.1034; 
  p0_emc_s_sdz[1][1][2][1]=3.3274; p1_emc_s_sdz[1][1][2][1]=-13.6254; p2_emc_s_sdz[1][1][2][1]=7.53251; p3_emc_s_sdz[1][1][2][1]=50.0853; p4_emc_s_sdz[1][1][2][1]=-59.6434; 
  p0_emc_m_sdz[0][1][3][1]=0.267031; p1_emc_m_sdz[0][1][3][1]=-0.955817; p2_emc_m_sdz[0][1][3][1]=0.906709; p3_emc_m_sdz[0][1][3][1]=-0.337251; p4_emc_m_sdz[0][1][3][1]=0.0434358; 
  p0_emc_s_sdz[0][1][3][1]=0.484317; p1_emc_s_sdz[0][1][3][1]=0.981409; p2_emc_s_sdz[0][1][3][1]=-0.732834; p3_emc_s_sdz[0][1][3][1]=0.236814; p4_emc_s_sdz[0][1][3][1]=-0.0276763; 
  p0_emc_m_sdz[1][1][3][1]=-10.2024; p1_emc_m_sdz[1][1][3][1]=41.5053; p2_emc_m_sdz[1][1][3][1]=-14.6354; p3_emc_m_sdz[1][1][3][1]=-113.458; p4_emc_m_sdz[1][1][3][1]=115.405; 
  p0_emc_s_sdz[1][1][3][1]=10.9739; p1_emc_s_sdz[1][1][3][1]=-40.0337; p2_emc_s_sdz[1][1][3][1]=12.3858; p3_emc_s_sdz[1][1][3][1]=102.314; p4_emc_s_sdz[1][1][3][1]=-97.2058; 
  p0_emc_m_sdz[0][1][4][1]=0.282477; p1_emc_m_sdz[0][1][4][1]=-0.572974; p2_emc_m_sdz[0][1][4][1]=0.34656; p3_emc_m_sdz[0][1][4][1]=-0.0669028; p4_emc_m_sdz[0][1][4][1]=0.00105171; 
  p0_emc_s_sdz[0][1][4][1]=1.30222; p1_emc_s_sdz[0][1][4][1]=-0.894611; p2_emc_s_sdz[0][1][4][1]=0.700497; p3_emc_s_sdz[0][1][4][1]=-0.208912; p4_emc_s_sdz[0][1][4][1]=0.020576; 
  p0_emc_m_sdz[1][1][4][1]=1.95517; p1_emc_m_sdz[1][1][4][1]=-8.8367; p2_emc_m_sdz[1][1][4][1]=3.87496; p3_emc_m_sdz[1][1][4][1]=25.0423; p4_emc_m_sdz[1][1][4][1]=-26.1748; 
  p0_emc_s_sdz[1][1][4][1]=-2.18724; p1_emc_s_sdz[1][1][4][1]=14.123; p2_emc_s_sdz[1][1][4][1]=-4.96075; p3_emc_s_sdz[1][1][4][1]=-44.4529; p4_emc_s_sdz[1][1][4][1]=46.6221; 
  p0_emc_m_sdz[0][1][5][1]=0.293279; p1_emc_m_sdz[0][1][5][1]=-0.453409; p2_emc_m_sdz[0][1][5][1]=0.289231; p3_emc_m_sdz[0][1][5][1]=-0.0866385; p4_emc_m_sdz[0][1][5][1]=0.0100976; 
  p0_emc_s_sdz[0][1][5][1]=0.938276; p1_emc_s_sdz[0][1][5][1]=-0.163475; p2_emc_s_sdz[0][1][5][1]=0.242867; p3_emc_s_sdz[0][1][5][1]=-0.108485; p4_emc_s_sdz[0][1][5][1]=0.0164084; 
  p0_emc_m_sdz[1][1][5][1]=-7.29081; p1_emc_m_sdz[1][1][5][1]=29.4954; p2_emc_m_sdz[1][1][5][1]=-8.68984; p3_emc_m_sdz[1][1][5][1]=-81.4438; p4_emc_m_sdz[1][1][5][1]=80.0127; 
  p0_emc_s_sdz[1][1][5][1]=5.48941; p1_emc_s_sdz[1][1][5][1]=-18.4136; p2_emc_s_sdz[1][1][5][1]=5.11924; p3_emc_s_sdz[1][1][5][1]=51.1182; p4_emc_s_sdz[1][1][5][1]=-49.7365; 
  p0_emc_m_sdz[0][1][6][1]=0.051157; p1_emc_m_sdz[0][1][6][1]=-0.053058; p2_emc_m_sdz[0][1][6][1]=0.00467442; p3_emc_m_sdz[0][1][6][1]=0.00427979; p4_emc_m_sdz[0][1][6][1]=-0.000470804; 
  p0_emc_s_sdz[0][1][6][1]=1.23429; p1_emc_s_sdz[0][1][6][1]=-0.703131; p2_emc_s_sdz[0][1][6][1]=0.548628; p3_emc_s_sdz[0][1][6][1]=-0.161336; p4_emc_s_sdz[0][1][6][1]=0.0163635; 
  p0_emc_m_sdz[1][1][6][1]=-10.1854; p1_emc_m_sdz[1][1][6][1]=41.3476; p2_emc_m_sdz[1][1][6][1]=-12.648; p3_emc_m_sdz[1][1][6][1]=-113.955; p4_emc_m_sdz[1][1][6][1]=111.981; 
  p0_emc_s_sdz[1][1][6][1]=-4.01638; p1_emc_s_sdz[1][1][6][1]=20.5224; p2_emc_s_sdz[1][1][6][1]=-7.50329; p3_emc_s_sdz[1][1][6][1]=-53.9636; p4_emc_s_sdz[1][1][6][1]=53.847; 
  p0_emc_m_sdz[0][1][7][1]=0.307308; p1_emc_m_sdz[0][1][7][1]=-0.579701; p2_emc_m_sdz[0][1][7][1]=0.432521; p3_emc_m_sdz[0][1][7][1]=-0.148019; p4_emc_m_sdz[0][1][7][1]=0.0189403; 
  p0_emc_s_sdz[0][1][7][1]=0.838497; p1_emc_s_sdz[0][1][7][1]=0.0339043; p2_emc_s_sdz[0][1][7][1]=0.0483465; p3_emc_s_sdz[0][1][7][1]=-0.00112719; p4_emc_s_sdz[0][1][7][1]=-0.0054869; 
  p0_emc_m_sdz[1][1][7][1]=7.35768; p1_emc_m_sdz[1][1][7][1]=-26.1538; p2_emc_m_sdz[1][1][7][1]=10.5036; p3_emc_m_sdz[1][1][7][1]=51.6337; p4_emc_m_sdz[1][1][7][1]=-50.3188; 
  p0_emc_s_sdz[1][1][7][1]=-1.58687; p1_emc_s_sdz[1][1][7][1]=3.24874; p2_emc_s_sdz[1][1][7][1]=6.82773; p3_emc_s_sdz[1][1][7][1]=3.33469; p4_emc_s_sdz[1][1][7][1]=-18.5817; 
  p0_emc_m_sdz[0][1][8][1]=0.193878; p1_emc_m_sdz[0][1][8][1]=-0.309499; p2_emc_m_sdz[0][1][8][1]=0.24456; p3_emc_m_sdz[0][1][8][1]=-0.0994408; p4_emc_m_sdz[0][1][8][1]=0.0157463; 
  p0_emc_s_sdz[0][1][8][1]=0.744292; p1_emc_s_sdz[0][1][8][1]=0.402554; p2_emc_s_sdz[0][1][8][1]=-0.332344; p3_emc_s_sdz[0][1][8][1]=0.130613; p4_emc_s_sdz[0][1][8][1]=-0.0178471; 
  p0_emc_m_sdz[1][1][8][1]=-0.11445; p1_emc_m_sdz[1][1][8][1]=0.278215; p2_emc_m_sdz[1][1][8][1]=0.594226; p3_emc_m_sdz[1][1][8][1]=0.264861; p4_emc_m_sdz[1][1][8][1]=-1.83739; 
  p0_emc_s_sdz[1][1][8][1]=3.99295; p1_emc_s_sdz[1][1][8][1]=-12.0465; p2_emc_s_sdz[1][1][8][1]=2.42183; p3_emc_s_sdz[1][1][8][1]=34.3933; p4_emc_s_sdz[1][1][8][1]=-32.2341; 
  p0_emc_m_sdz[0][1][9][1]=0.414894; p1_emc_m_sdz[0][1][9][1]=-0.793701; p2_emc_m_sdz[0][1][9][1]=0.612687; p3_emc_m_sdz[0][1][9][1]=-0.209159; p4_emc_m_sdz[0][1][9][1]=0.0259648; 
  p0_emc_s_sdz[0][1][9][1]=1.09375; p1_emc_s_sdz[0][1][9][1]=-0.509254; p2_emc_s_sdz[0][1][9][1]=0.472535; p3_emc_s_sdz[0][1][9][1]=-0.154965; p4_emc_s_sdz[0][1][9][1]=0.0167198; 
  p0_emc_m_sdz[1][1][9][1]=12.708; p1_emc_m_sdz[1][1][9][1]=-51.7529; p2_emc_m_sdz[1][1][9][1]=18.7486; p3_emc_m_sdz[1][1][9][1]=138.822; p4_emc_m_sdz[1][1][9][1]=-140.923; 
  p0_emc_s_sdz[1][1][9][1]=-1.38478; p1_emc_s_sdz[1][1][9][1]=9.36417; p2_emc_s_sdz[1][1][9][1]=-3.73639; p3_emc_s_sdz[1][1][9][1]=-25.0455; p4_emc_s_sdz[1][1][9][1]=26.2356; 
  p0_emc_m_sdz[0][2][0][1]=-0.176072; p1_emc_m_sdz[0][2][0][1]=0.317918; p2_emc_m_sdz[0][2][0][1]=-0.199652; p3_emc_m_sdz[0][2][0][1]=0.055732; p4_emc_m_sdz[0][2][0][1]=-0.00638149; 
  p0_emc_s_sdz[0][2][0][1]=1.22867; p1_emc_s_sdz[0][2][0][1]=-0.531326; p2_emc_s_sdz[0][2][0][1]=0.282323; p3_emc_s_sdz[0][2][0][1]=-0.0277819; p4_emc_s_sdz[0][2][0][1]=-0.00441471; 
  p0_emc_m_sdz[1][2][0][1]=0.239437; p1_emc_m_sdz[1][2][0][1]=-0.350926; p2_emc_m_sdz[1][2][0][1]=-0.817476; p3_emc_m_sdz[1][2][0][1]=-0.329995; p4_emc_m_sdz[1][2][0][1]=2.77379; 
  p0_emc_s_sdz[1][2][0][1]=-3.52793; p1_emc_s_sdz[1][2][0][1]=17.5714; p2_emc_s_sdz[1][2][0][1]=-5.93941; p3_emc_s_sdz[1][2][0][1]=-45.2065; p4_emc_s_sdz[1][2][0][1]=44.8035; 
  p0_emc_m_sdz[0][2][1][1]=-0.117942; p1_emc_m_sdz[0][2][1][1]=0.198084; p2_emc_m_sdz[0][2][1][1]=-0.174088; p3_emc_m_sdz[0][2][1][1]=0.0821917; p4_emc_m_sdz[0][2][1][1]=-0.0144592; 
  p0_emc_s_sdz[0][2][1][1]=0.966207; p1_emc_s_sdz[0][2][1][1]=-0.0765028; p2_emc_s_sdz[0][2][1][1]=0.0441314; p3_emc_s_sdz[0][2][1][1]=0.00518078; p4_emc_s_sdz[0][2][1][1]=-0.00318447; 
  p0_emc_m_sdz[1][2][1][1]=0.520309; p1_emc_m_sdz[1][2][1][1]=-0.543742; p2_emc_m_sdz[1][2][1][1]=-1.43832; p3_emc_m_sdz[1][2][1][1]=-0.845489; p4_emc_m_sdz[1][2][1][1]=3.77592; 
  p0_emc_s_sdz[1][2][1][1]=1.47863; p1_emc_s_sdz[1][2][1][1]=-0.441917; p2_emc_s_sdz[1][2][1][1]=-2.2669; p3_emc_s_sdz[1][2][1][1]=-1.56496; p4_emc_s_sdz[1][2][1][1]=6.69957; 
  p0_emc_m_sdz[0][2][2][1]=-0.100976; p1_emc_m_sdz[0][2][2][1]=0.192639; p2_emc_m_sdz[0][2][2][1]=-0.150785; p3_emc_m_sdz[0][2][2][1]=0.0672788; p4_emc_m_sdz[0][2][2][1]=-0.0120982; 
  p0_emc_s_sdz[0][2][2][1]=1.03934; p1_emc_s_sdz[0][2][2][1]=-0.269435; p2_emc_s_sdz[0][2][2][1]=0.189665; p3_emc_s_sdz[0][2][2][1]=-0.0353125; p4_emc_s_sdz[0][2][2][1]=0.00047977; 
  p0_emc_m_sdz[1][2][2][1]=-3.77125; p1_emc_m_sdz[1][2][2][1]=14.6455; p2_emc_m_sdz[1][2][2][1]=-3.56543; p3_emc_m_sdz[1][2][2][1]=-40.5242; p4_emc_m_sdz[1][2][2][1]=38.7325; 
  p0_emc_s_sdz[1][2][2][1]=3.0466; p1_emc_s_sdz[1][2][2][1]=-10.3166; p2_emc_s_sdz[1][2][2][1]=4.33038; p3_emc_s_sdz[1][2][2][1]=34.861; p4_emc_s_sdz[1][2][2][1]=-38.3586; 
  p0_emc_m_sdz[0][2][3][1]=-0.0522194; p1_emc_m_sdz[0][2][3][1]=0.225943; p2_emc_m_sdz[0][2][3][1]=-0.292323; p3_emc_m_sdz[0][2][3][1]=0.147804; p4_emc_m_sdz[0][2][3][1]=-0.0251399; 
  p0_emc_s_sdz[0][2][3][1]=1.05688; p1_emc_s_sdz[0][2][3][1]=-0.310884; p2_emc_s_sdz[0][2][3][1]=0.237225; p3_emc_s_sdz[0][2][3][1]=-0.0552848; p4_emc_s_sdz[0][2][3][1]=0.00313781; 
  p0_emc_m_sdz[1][2][3][1]=-2.85127; p1_emc_m_sdz[1][2][3][1]=11.154; p2_emc_m_sdz[1][2][3][1]=-2.20856; p3_emc_m_sdz[1][2][3][1]=-31.8327; p4_emc_m_sdz[1][2][3][1]=29.5153; 
  p0_emc_s_sdz[1][2][3][1]=-0.169205; p1_emc_s_sdz[1][2][3][1]=6.6598; p2_emc_s_sdz[1][2][3][1]=-5.06701; p3_emc_s_sdz[1][2][3][1]=-20.9789; p4_emc_s_sdz[1][2][3][1]=26.1361; 
  p0_emc_m_sdz[0][2][4][1]=-0.712261; p1_emc_m_sdz[0][2][4][1]=1.60654; p2_emc_m_sdz[0][2][4][1]=-1.34314; p3_emc_m_sdz[0][2][4][1]=0.486545; p4_emc_m_sdz[0][2][4][1]=-0.064486; 
  p0_emc_s_sdz[0][2][4][1]=1.13519; p1_emc_s_sdz[0][2][4][1]=-0.663488; p2_emc_s_sdz[0][2][4][1]=0.664816; p3_emc_s_sdz[0][2][4][1]=-0.252322; p4_emc_s_sdz[0][2][4][1]=0.0331951; 
  p0_emc_m_sdz[1][2][4][1]=7.46791; p1_emc_m_sdz[1][2][4][1]=-29.4777; p2_emc_m_sdz[1][2][4][1]=6.59972; p3_emc_m_sdz[1][2][4][1]=84.0437; p4_emc_m_sdz[1][2][4][1]=-79.8233; 
  p0_emc_s_sdz[1][2][4][1]=4.42241; p1_emc_s_sdz[1][2][4][1]=-11.7936; p2_emc_s_sdz[1][2][4][1]=1.20867; p3_emc_s_sdz[1][2][4][1]=28.3296; p4_emc_s_sdz[1][2][4][1]=-23.0753; 
  p0_emc_m_sdz[0][2][5][1]=0.275101; p1_emc_m_sdz[0][2][5][1]=-0.492547; p2_emc_m_sdz[0][2][5][1]=0.334202; p3_emc_m_sdz[0][2][5][1]=-0.107076; p4_emc_m_sdz[0][2][5][1]=0.0133233; 
  p0_emc_s_sdz[0][2][5][1]=0.997946; p1_emc_s_sdz[0][2][5][1]=-0.10294; p2_emc_s_sdz[0][2][5][1]=0.0472847; p3_emc_s_sdz[0][2][5][1]=0.00857543; p4_emc_s_sdz[0][2][5][1]=-0.00418999; 
  p0_emc_m_sdz[1][2][5][1]=-16.6603; p1_emc_m_sdz[1][2][5][1]=64.8325; p2_emc_m_sdz[1][2][5][1]=-16.0963; p3_emc_m_sdz[1][2][5][1]=-176.081; p4_emc_m_sdz[1][2][5][1]=166.53; 
  p0_emc_s_sdz[1][2][5][1]=8.96912; p1_emc_s_sdz[1][2][5][1]=-32.8935; p2_emc_s_sdz[1][2][5][1]=10.3192; p3_emc_s_sdz[1][2][5][1]=89.8734; p4_emc_s_sdz[1][2][5][1]=-87.96; 
  p0_emc_m_sdz[0][2][6][1]=0.315622; p1_emc_m_sdz[0][2][6][1]=-0.512003; p2_emc_m_sdz[0][2][6][1]=0.328193; p3_emc_m_sdz[0][2][6][1]=-0.0898533; p4_emc_m_sdz[0][2][6][1]=0.00892501; 
  p0_emc_s_sdz[0][2][6][1]=0.715605; p1_emc_s_sdz[0][2][6][1]=0.403355; p2_emc_s_sdz[0][2][6][1]=-0.268081; p3_emc_s_sdz[0][2][6][1]=0.0911942; p4_emc_s_sdz[0][2][6][1]=-0.0115765; 
  p0_emc_m_sdz[1][2][6][1]=2.78012; p1_emc_m_sdz[1][2][6][1]=-12.3208; p2_emc_m_sdz[1][2][6][1]=5.66822; p3_emc_m_sdz[1][2][6][1]=35.7607; p4_emc_m_sdz[1][2][6][1]=-39.0807; 
  p0_emc_s_sdz[1][2][6][1]=-3.0668; p1_emc_s_sdz[1][2][6][1]=17.6567; p2_emc_s_sdz[1][2][6][1]=-7.31048; p3_emc_s_sdz[1][2][6][1]=-48.7563; p4_emc_s_sdz[1][2][6][1]=50.2373; 
  p0_emc_m_sdz[0][2][7][1]=0.0686579; p1_emc_m_sdz[0][2][7][1]=0.0660356; p2_emc_m_sdz[0][2][7][1]=-0.118206; p3_emc_m_sdz[0][2][7][1]=0.0417338; p4_emc_m_sdz[0][2][7][1]=-0.00325853; 
  p0_emc_s_sdz[0][2][7][1]=1.20813; p1_emc_s_sdz[0][2][7][1]=-0.778506; p2_emc_s_sdz[0][2][7][1]=0.667826; p3_emc_s_sdz[0][2][7][1]=-0.209218; p4_emc_s_sdz[0][2][7][1]=0.0223174; 
  p0_emc_m_sdz[1][2][7][1]=-0.488364; p1_emc_m_sdz[1][2][7][1]=0.644817; p2_emc_m_sdz[1][2][7][1]=1.56881; p3_emc_m_sdz[1][2][7][1]=0.846207; p4_emc_m_sdz[1][2][7][1]=-4.31156; 
  p0_emc_s_sdz[1][2][7][1]=4.7355; p1_emc_s_sdz[1][2][7][1]=-12.8004; p2_emc_s_sdz[1][2][7][1]=1.03329; p3_emc_s_sdz[1][2][7][1]=29.4851; p4_emc_s_sdz[1][2][7][1]=-22.2357; 
  p0_emc_m_sdz[0][2][8][1]=0.0786514; p1_emc_m_sdz[0][2][8][1]=0.0154518; p2_emc_m_sdz[0][2][8][1]=-0.0316802; p3_emc_m_sdz[0][2][8][1]=-0.00138212; p4_emc_m_sdz[0][2][8][1]=0.00334483; 
  p0_emc_s_sdz[0][2][8][1]=0.97565; p1_emc_s_sdz[0][2][8][1]=-0.113683; p2_emc_s_sdz[0][2][8][1]=0.0612007; p3_emc_s_sdz[0][2][8][1]=0.00805879; p4_emc_s_sdz[0][2][8][1]=-0.00423088; 
  p0_emc_m_sdz[1][2][8][1]=4.76703; p1_emc_m_sdz[1][2][8][1]=-18.3709; p2_emc_m_sdz[1][2][8][1]=4.78542; p3_emc_m_sdz[1][2][8][1]=51.8994; p4_emc_m_sdz[1][2][8][1]=-50.598; 
  p0_emc_s_sdz[1][2][8][1]=-7.63383; p1_emc_s_sdz[1][2][8][1]=33.7849; p2_emc_s_sdz[1][2][8][1]=-10.6144; p3_emc_s_sdz[1][2][8][1]=-89.3907; p4_emc_s_sdz[1][2][8][1]=87.6946; 
  p0_emc_m_sdz[0][2][9][1]=0.340837; p1_emc_m_sdz[0][2][9][1]=-0.415971; p2_emc_m_sdz[0][2][9][1]=0.128168; p3_emc_m_sdz[0][2][9][1]=0.0173458; p4_emc_m_sdz[0][2][9][1]=-0.00854319; 
  p0_emc_s_sdz[0][2][9][1]=1.37593; p1_emc_s_sdz[0][2][9][1]=-1.1968; p2_emc_s_sdz[0][2][9][1]=0.981167; p3_emc_s_sdz[0][2][9][1]=-0.289879; p4_emc_s_sdz[0][2][9][1]=0.0257107; 
  p0_emc_m_sdz[1][2][9][1]=27.7954; p1_emc_m_sdz[1][2][9][1]=-90.9137; p2_emc_m_sdz[1][2][9][1]=-5.4326; p3_emc_m_sdz[1][2][9][1]=281.118; p4_emc_m_sdz[1][2][9][1]=-243.373; 
  p0_emc_s_sdz[1][2][9][1]=-24.5795; p1_emc_s_sdz[1][2][9][1]=93.8898; p2_emc_s_sdz[1][2][9][1]=-17.5667; p3_emc_s_sdz[1][2][9][1]=-246.895; p4_emc_s_sdz[1][2][9][1]=223.297; 

	
  //PC3/DPHI/19GEV/NEG
  
  p0_pc3_m_sdp[0][0][0][0]=-0.151012; p1_pc3_m_sdp[0][0][0][0]=0.450816; p2_pc3_m_sdp[0][0][0][0]=-0.467625; p3_pc3_m_sdp[0][0][0][0]=0.192076; p4_pc3_m_sdp[0][0][0][0]=-0.0277125; 
  p0_pc3_s_sdp[0][0][0][0]=-0.0020763; p1_pc3_s_sdp[0][0][0][0]=1.77379; p2_pc3_s_sdp[0][0][0][0]=-1.52882; p3_pc3_s_sdp[0][0][0][0]=0.57449; p4_pc3_s_sdp[0][0][0][0]=-0.0737822; 
  p0_pc3_m_sdp[1][0][0][0]=-1.22655; p1_pc3_m_sdp[1][0][0][0]=6.08276; p2_pc3_m_sdp[1][0][0][0]=-2.04502; p3_pc3_m_sdp[1][0][0][0]=-16.3859; p4_pc3_m_sdp[1][0][0][0]=15.2612; 
  p0_pc3_s_sdp[1][0][0][0]=8.38379; p1_pc3_s_sdp[1][0][0][0]=-27.5545; p2_pc3_s_sdp[1][0][0][0]=2.29196; p3_pc3_s_sdp[1][0][0][0]=76.8617; p4_pc3_s_sdp[1][0][0][0]=-66.306; 
  p0_pc3_m_sdp[0][0][1][0]=-0.0802968; p1_pc3_m_sdp[0][0][1][0]=0.241571; p2_pc3_m_sdp[0][0][1][0]=-0.232837; p3_pc3_m_sdp[0][0][1][0]=0.089039; p4_pc3_m_sdp[0][0][1][0]=-0.0118579; 
  p0_pc3_s_sdp[0][0][1][0]=-0.275701; p1_pc3_s_sdp[0][0][1][0]=2.48019; p2_pc3_s_sdp[0][0][1][0]=-2.1289; p3_pc3_s_sdp[0][0][1][0]=0.77218; p4_pc3_s_sdp[0][0][1][0]=-0.0941779; 
  p0_pc3_m_sdp[1][0][1][0]=-4.7002; p1_pc3_m_sdp[1][0][1][0]=18.3034; p2_pc3_m_sdp[1][0][1][0]=-4.58312; p3_pc3_m_sdp[1][0][1][0]=-52.0933; p4_pc3_m_sdp[1][0][1][0]=51.3126; 
  p0_pc3_s_sdp[1][0][1][0]=5.65966; p1_pc3_s_sdp[1][0][1][0]=-22.7242; p2_pc3_s_sdp[1][0][1][0]=10.6982; p3_pc3_s_sdp[1][0][1][0]=66.9249; p4_pc3_s_sdp[1][0][1][0]=-73.6515; 
  p0_pc3_m_sdp[0][0][2][0]=0.028275; p1_pc3_m_sdp[0][0][2][0]=0.0414923; p2_pc3_m_sdp[0][0][2][0]=-0.112318; p3_pc3_m_sdp[0][0][2][0]=0.065497; p4_pc3_m_sdp[0][0][2][0]=-0.0114631; 
  p0_pc3_s_sdp[0][0][2][0]=0.276699; p1_pc3_s_sdp[0][0][2][0]=1.14171; p2_pc3_s_sdp[0][0][2][0]=-0.965498; p3_pc3_s_sdp[0][0][2][0]=0.341859; p4_pc3_s_sdp[0][0][2][0]=-0.0388702; 
  p0_pc3_m_sdp[1][0][2][0]=4.05101; p1_pc3_m_sdp[1][0][2][0]=-15.4725; p2_pc3_m_sdp[1][0][2][0]=3.64808; p3_pc3_m_sdp[1][0][2][0]=44.2375; p4_pc3_m_sdp[1][0][2][0]=-42.8872; 
  p0_pc3_s_sdp[1][0][2][0]=0.740578; p1_pc3_s_sdp[1][0][2][0]=0.126209; p2_pc3_s_sdp[1][0][2][0]=-0.647192; p3_pc3_s_sdp[1][0][2][0]=-0.750356; p4_pc3_s_sdp[1][0][2][0]=1.84888; 
  p0_pc3_m_sdp[0][0][3][0]=0.00146843; p1_pc3_m_sdp[0][0][3][0]=0.074166; p2_pc3_m_sdp[0][0][3][0]=-0.108853; p3_pc3_m_sdp[0][0][3][0]=0.0575497; p4_pc3_m_sdp[0][0][3][0]=-0.0100358; 
  p0_pc3_s_sdp[0][0][3][0]=0.797581; p1_pc3_s_sdp[0][0][3][0]=-0.25557; p2_pc3_s_sdp[0][0][3][0]=0.284659; p3_pc3_s_sdp[0][0][3][0]=-0.104552; p4_pc3_s_sdp[0][0][3][0]=0.0137638; 
  p0_pc3_m_sdp[1][0][3][0]=7.76808; p1_pc3_m_sdp[1][0][3][0]=-28.3915; p2_pc3_m_sdp[1][0][3][0]=5.34632; p3_pc3_m_sdp[1][0][3][0]=74.9874; p4_pc3_m_sdp[1][0][3][0]=-68.0442; 
  p0_pc3_s_sdp[1][0][3][0]=537.878; p1_pc3_s_sdp[1][0][3][0]=-4115.77; p2_pc3_s_sdp[1][0][3][0]=11692.8; p3_pc3_s_sdp[1][0][3][0]=-14613.8; p4_pc3_s_sdp[1][0][3][0]=6785.56; 
  p0_pc3_m_sdp[0][0][4][0]=-0.0380145; p1_pc3_m_sdp[0][0][4][0]=0.183782; p2_pc3_m_sdp[0][0][4][0]=-0.202814; p3_pc3_m_sdp[0][0][4][0]=0.0911347; p4_pc3_m_sdp[0][0][4][0]=-0.0143598; 
  p0_pc3_s_sdp[0][0][4][0]=0.512581; p1_pc3_s_sdp[0][0][4][0]=0.382391; p2_pc3_s_sdp[0][0][4][0]=-0.182723; p3_pc3_s_sdp[0][0][4][0]=0.0331642; p4_pc3_s_sdp[0][0][4][0]=-0.000402421; 
  p0_pc3_m_sdp[1][0][4][0]=-6.04837; p1_pc3_m_sdp[1][0][4][0]=23.7622; p2_pc3_m_sdp[1][0][4][0]=-6.86891; p3_pc3_m_sdp[1][0][4][0]=-65.6485; p4_pc3_m_sdp[1][0][4][0]=64.9868; 
  p0_pc3_s_sdp[1][0][4][0]=3.28079; p1_pc3_s_sdp[1][0][4][0]=-10.4956; p2_pc3_s_sdp[1][0][4][0]=2.69047; p3_pc3_s_sdp[1][0][4][0]=28.9948; p4_pc3_s_sdp[1][0][4][0]=-27.2265; 
  p0_pc3_m_sdp[0][0][5][0]=-0.683611; p1_pc3_m_sdp[0][0][5][0]=1.40242; p2_pc3_m_sdp[0][0][5][0]=-0.845424; p3_pc3_m_sdp[0][0][5][0]=0.196142; p4_pc3_m_sdp[0][0][5][0]=-0.0161028; 
  p0_pc3_s_sdp[0][0][5][0]=-1.53827; p1_pc3_s_sdp[0][0][5][0]=5.08006; p2_pc3_s_sdp[0][0][5][0]=-3.71815; p3_pc3_s_sdp[0][0][5][0]=1.13157; p4_pc3_s_sdp[0][0][5][0]=-0.124486; 
  p0_pc3_m_sdp[1][0][5][0]=270.265; p1_pc3_m_sdp[1][0][5][0]=-942.614; p2_pc3_m_sdp[1][0][5][0]=177.948; p3_pc3_m_sdp[1][0][5][0]=2249.02; p4_pc3_m_sdp[1][0][5][0]=-1963.41; 
  p0_pc3_s_sdp[1][0][5][0]=4.53888; p1_pc3_s_sdp[1][0][5][0]=-2.92535; p2_pc3_s_sdp[1][0][5][0]=-9.14424; p3_pc3_s_sdp[1][0][5][0]=-6.55182; p4_pc3_s_sdp[1][0][5][0]=20.7631; 
  p0_pc3_m_sdp[0][0][6][0]=0.221962; p1_pc3_m_sdp[0][0][6][0]=-0.485408; p2_pc3_m_sdp[0][0][6][0]=0.36736; p3_pc3_m_sdp[0][0][6][0]=-0.108562; p4_pc3_m_sdp[0][0][6][0]=0.0103777; 
  p0_pc3_s_sdp[0][0][6][0]=0.708336; p1_pc3_s_sdp[0][0][6][0]=0.0184142; p2_pc3_s_sdp[0][0][6][0]=0.0201708; p3_pc3_s_sdp[0][0][6][0]=0.00260745; p4_pc3_s_sdp[0][0][6][0]=-0.00184505; 
  p0_pc3_m_sdp[1][0][6][0]=0.304124; p1_pc3_m_sdp[1][0][6][0]=-0.216714; p2_pc3_m_sdp[1][0][6][0]=-0.683545; p3_pc3_m_sdp[1][0][6][0]=-0.495855; p4_pc3_m_sdp[1][0][6][0]=1.52643; 
  p0_pc3_s_sdp[1][0][6][0]=-2.59903; p1_pc3_s_sdp[1][0][6][0]=11.315; p2_pc3_s_sdp[1][0][6][0]=-1.16671; p3_pc3_s_sdp[1][0][6][0]=-27.5309; p4_pc3_s_sdp[1][0][6][0]=22.071; 
  p0_pc3_m_sdp[0][0][7][0]=-0.0900418; p1_pc3_m_sdp[0][0][7][0]=0.200145; p2_pc3_m_sdp[0][0][7][0]=-0.15854; p3_pc3_m_sdp[0][0][7][0]=0.0547722; p4_pc3_m_sdp[0][0][7][0]=-0.00724313; 
  p0_pc3_s_sdp[0][0][7][0]=0.226186; p1_pc3_s_sdp[0][0][7][0]=1.16681; p2_pc3_s_sdp[0][0][7][0]=-0.909235; p3_pc3_s_sdp[0][0][7][0]=0.29651; p4_pc3_s_sdp[0][0][7][0]=-0.030626; 
  p0_pc3_m_sdp[1][0][7][0]=0.18402; p1_pc3_m_sdp[1][0][7][0]=-0.225305; p2_pc3_m_sdp[1][0][7][0]=-0.541539; p3_pc3_m_sdp[1][0][7][0]=-0.256036; p4_pc3_m_sdp[1][0][7][0]=1.60918; 
  p0_pc3_s_sdp[1][0][7][0]=4.86879; p1_pc3_s_sdp[1][0][7][0]=-16.8913; p2_pc3_s_sdp[1][0][7][0]=4.62245; p3_pc3_s_sdp[1][0][7][0]=46.6306; p4_pc3_s_sdp[1][0][7][0]=-44.7991; 
  p0_pc3_m_sdp[0][0][8][0]=0.0131894; p1_pc3_m_sdp[0][0][8][0]=-0.0687237; p2_pc3_m_sdp[0][0][8][0]=0.0357392; p3_pc3_m_sdp[0][0][8][0]=0.00310842; p4_pc3_m_sdp[0][0][8][0]=-0.00371824; 
  p0_pc3_s_sdp[0][0][8][0]=-0.358914; p1_pc3_s_sdp[0][0][8][0]=2.5751; p2_pc3_s_sdp[0][0][8][0]=-2.13318; p3_pc3_s_sdp[0][0][8][0]=0.753512; p4_pc3_s_sdp[0][0][8][0]=-0.0911355; 
  p0_pc3_m_sdp[1][0][8][0]=-2.04686; p1_pc3_m_sdp[1][0][8][0]=9.47312; p2_pc3_m_sdp[1][0][8][0]=-4.98783; p3_pc3_m_sdp[1][0][8][0]=-21.9901; p4_pc3_m_sdp[1][0][8][0]=22.6847; 
  p0_pc3_s_sdp[1][0][8][0]=5.76348; p1_pc3_s_sdp[1][0][8][0]=-17.7276; p2_pc3_s_sdp[1][0][8][0]=2.12155; p3_pc3_s_sdp[1][0][8][0]=44.9145; p4_pc3_s_sdp[1][0][8][0]=-38.0052; 
  p0_pc3_m_sdp[0][0][9][0]=-0.243414; p1_pc3_m_sdp[0][0][9][0]=0.558884; p2_pc3_m_sdp[0][0][9][0]=-0.517579; p3_pc3_m_sdp[0][0][9][0]=0.201697; p4_pc3_m_sdp[0][0][9][0]=-0.028034; 
  p0_pc3_s_sdp[0][0][9][0]=-0.713483; p1_pc3_s_sdp[0][0][9][0]=3.29964; p2_pc3_s_sdp[0][0][9][0]=-2.66755; p3_pc3_s_sdp[0][0][9][0]=0.928735; p4_pc3_s_sdp[0][0][9][0]=-0.112491; 
  p0_pc3_m_sdp[1][0][9][0]=2.26879; p1_pc3_m_sdp[1][0][9][0]=-8.79068; p2_pc3_m_sdp[1][0][9][0]=3.23263; p3_pc3_m_sdp[1][0][9][0]=23.3986; p4_pc3_m_sdp[1][0][9][0]=-24.5547; 
  p0_pc3_s_sdp[1][0][9][0]=2.80762; p1_pc3_s_sdp[1][0][9][0]=-6.67002; p2_pc3_s_sdp[1][0][9][0]=0.143944; p3_pc3_s_sdp[1][0][9][0]=16.4652; p4_pc3_s_sdp[1][0][9][0]=-13.2959; 
  p0_pc3_m_sdp[0][1][0][0]=0.167705; p1_pc3_m_sdp[0][1][0][0]=-0.193042; p2_pc3_m_sdp[0][1][0][0]=0.0840514; p3_pc3_m_sdp[0][1][0][0]=-0.0238417; p4_pc3_m_sdp[0][1][0][0]=0.00329203; 
  p0_pc3_s_sdp[0][1][0][0]=-0.70371; p1_pc3_s_sdp[0][1][0][0]=3.53517; p2_pc3_s_sdp[0][1][0][0]=-2.91826; p3_pc3_s_sdp[0][1][0][0]=1.01229; p4_pc3_s_sdp[0][1][0][0]=-0.121194; 
  p0_pc3_m_sdp[1][1][0][0]=-0.36101; p1_pc3_m_sdp[1][1][0][0]=0.524729; p2_pc3_m_sdp[1][1][0][0]=1.20553; p3_pc3_m_sdp[1][1][0][0]=0.609937; p4_pc3_m_sdp[1][1][0][0]=-3.35928; 
  p0_pc3_s_sdp[1][1][0][0]=15.1859; p1_pc3_s_sdp[1][1][0][0]=-55.1857; p2_pc3_s_sdp[1][1][0][0]=14.6185; p3_pc3_s_sdp[1][1][0][0]=142.293; p4_pc3_s_sdp[1][1][0][0]=-133.554; 
  p0_pc3_m_sdp[0][1][1][0]=0.109044; p1_pc3_m_sdp[0][1][1][0]=-0.0624351; p2_pc3_m_sdp[0][1][1][0]=-0.0323182; p3_pc3_m_sdp[0][1][1][0]=0.0285895; p4_pc3_m_sdp[0][1][1][0]=-0.00558541; 
  p0_pc3_s_sdp[0][1][1][0]=-0.283232; p1_pc3_s_sdp[0][1][1][0]=2.30736; p2_pc3_s_sdp[0][1][1][0]=-1.68127; p3_pc3_s_sdp[0][1][1][0]=0.485483; p4_pc3_s_sdp[0][1][1][0]=-0.0400441; 
  p0_pc3_m_sdp[1][1][1][0]=4.57242; p1_pc3_m_sdp[1][1][1][0]=-18.3996; p2_pc3_m_sdp[1][1][1][0]=6.51471; p3_pc3_m_sdp[1][1][1][0]=49.0536; p4_pc3_m_sdp[1][1][1][0]=-49.1252; 
  p0_pc3_s_sdp[1][1][1][0]=5.66796; p1_pc3_s_sdp[1][1][1][0]=-19.4093; p2_pc3_s_sdp[1][1][1][0]=5.49416; p3_pc3_s_sdp[1][1][1][0]=47.9057; p4_pc3_s_sdp[1][1][1][0]=-44.4282; 
  p0_pc3_m_sdp[0][1][2][0]=0.198689; p1_pc3_m_sdp[0][1][2][0]=-0.391112; p2_pc3_m_sdp[0][1][2][0]=0.341948; p3_pc3_m_sdp[0][1][2][0]=-0.135659; p4_pc3_m_sdp[0][1][2][0]=0.0191756; 
  p0_pc3_s_sdp[0][1][2][0]=-0.201046; p1_pc3_s_sdp[0][1][2][0]=2.28572; p2_pc3_s_sdp[0][1][2][0]=-1.84767; p3_pc3_s_sdp[0][1][2][0]=0.626842; p4_pc3_s_sdp[0][1][2][0]=-0.0717567; 
  p0_pc3_m_sdp[1][1][2][0]=-4.6858; p1_pc3_m_sdp[1][1][2][0]=20.2673; p2_pc3_m_sdp[1][1][2][0]=-7.79735; p3_pc3_m_sdp[1][1][2][0]=-55.1323; p4_pc3_m_sdp[1][1][2][0]=56.3004; 
  p0_pc3_s_sdp[1][1][2][0]=-2.24391; p1_pc3_s_sdp[1][1][2][0]=10.5893; p2_pc3_s_sdp[1][1][2][0]=-1.77665; p3_pc3_s_sdp[1][1][2][0]=-27.6971; p4_pc3_s_sdp[1][1][2][0]=24.5492; 
  p0_pc3_m_sdp[0][1][3][0]=-0.0471185; p1_pc3_m_sdp[0][1][3][0]=0.203077; p2_pc3_m_sdp[0][1][3][0]=-0.208266; p3_pc3_m_sdp[0][1][3][0]=0.0888799; p4_pc3_m_sdp[0][1][3][0]=-0.0137499; 
  p0_pc3_s_sdp[0][1][3][0]=0.263624; p1_pc3_s_sdp[0][1][3][0]=1.12096; p2_pc3_s_sdp[0][1][3][0]=-0.82251; p3_pc3_s_sdp[0][1][3][0]=0.25552; p4_pc3_s_sdp[0][1][3][0]=-0.0270165; 
  p0_pc3_m_sdp[1][1][3][0]=-8.06856; p1_pc3_m_sdp[1][1][3][0]=32.9476; p2_pc3_m_sdp[1][1][3][0]=-8.57696; p3_pc3_m_sdp[1][1][3][0]=-93.6473; p4_pc3_m_sdp[1][1][3][0]=90.1513; 
  p0_pc3_s_sdp[1][1][3][0]=248.496; p1_pc3_s_sdp[1][1][3][0]=-1890.88; p2_pc3_s_sdp[1][1][3][0]=5356.62; p3_pc3_s_sdp[1][1][3][0]=-6684.48; p4_pc3_s_sdp[1][1][3][0]=3102.54; 
  p0_pc3_m_sdp[0][1][4][0]=-0.314469; p1_pc3_m_sdp[0][1][4][0]=0.915812; p2_pc3_m_sdp[0][1][4][0]=-0.867405; p3_pc3_m_sdp[0][1][4][0]=0.347482; p4_pc3_m_sdp[0][1][4][0]=-0.0502673; 
  p0_pc3_s_sdp[0][1][4][0]=0.821383; p1_pc3_s_sdp[0][1][4][0]=-0.137065; p2_pc3_s_sdp[0][1][4][0]=0.169099; p3_pc3_s_sdp[0][1][4][0]=-0.0676219; p4_pc3_s_sdp[0][1][4][0]=0.00953766; 
  p0_pc3_m_sdp[1][1][4][0]=4.23429; p1_pc3_m_sdp[1][1][4][0]=-18.4409; p2_pc3_m_sdp[1][1][4][0]=8.2776; p3_pc3_m_sdp[1][1][4][0]=50.7725; p4_pc3_m_sdp[1][1][4][0]=-54.1113; 
  p0_pc3_s_sdp[1][1][4][0]=-12.8691; p1_pc3_s_sdp[1][1][4][0]=51.7262; p2_pc3_s_sdp[1][1][4][0]=-10.9663; p3_pc3_s_sdp[1][1][4][0]=-142.553; p4_pc3_s_sdp[1][1][4][0]=133.018; 
  p0_pc3_m_sdp[0][1][5][0]=-0.130663; p1_pc3_m_sdp[0][1][5][0]=0.462025; p2_pc3_m_sdp[0][1][5][0]=-0.494281; p3_pc3_m_sdp[0][1][5][0]=0.225388; p4_pc3_m_sdp[0][1][5][0]=-0.0369551; 
  p0_pc3_s_sdp[0][1][5][0]=0.487991; p1_pc3_s_sdp[0][1][5][0]=0.671812; p2_pc3_s_sdp[0][1][5][0]=-0.474671; p3_pc3_s_sdp[0][1][5][0]=0.131308; p4_pc3_s_sdp[0][1][5][0]=-0.0106277; 
  p0_pc3_m_sdp[1][1][5][0]=4.48026; p1_pc3_m_sdp[1][1][5][0]=-19.8709; p2_pc3_m_sdp[1][1][5][0]=7.30658; p3_pc3_m_sdp[1][1][5][0]=58.5776; p4_pc3_m_sdp[1][1][5][0]=-59.8745; 
  p0_pc3_s_sdp[1][1][5][0]=1.9407; p1_pc3_s_sdp[1][1][5][0]=-5.62655; p2_pc3_s_sdp[1][1][5][0]=2.81105; p3_pc3_s_sdp[1][1][5][0]=14.5325; p4_pc3_s_sdp[1][1][5][0]=-15.5065; 
  p0_pc3_m_sdp[0][1][6][0]=-0.204693; p1_pc3_m_sdp[0][1][6][0]=0.619703; p2_pc3_m_sdp[0][1][6][0]=-0.589251; p3_pc3_m_sdp[0][1][6][0]=0.235674; p4_pc3_m_sdp[0][1][6][0]=-0.0341837; 
  p0_pc3_s_sdp[0][1][6][0]=0.413885; p1_pc3_s_sdp[0][1][6][0]=0.947944; p2_pc3_s_sdp[0][1][6][0]=-0.76341; p3_pc3_s_sdp[0][1][6][0]=0.254978; p4_pc3_s_sdp[0][1][6][0]=-0.0290736; 
  p0_pc3_m_sdp[1][1][6][0]=-0.692734; p1_pc3_m_sdp[1][1][6][0]=0.904978; p2_pc3_m_sdp[1][1][6][0]=2.26455; p3_pc3_m_sdp[1][1][6][0]=1.20666; p4_pc3_m_sdp[1][1][6][0]=-6.71363; 
  p0_pc3_s_sdp[1][1][6][0]=9.70387; p1_pc3_s_sdp[1][1][6][0]=-35.4887; p2_pc3_s_sdp[1][1][6][0]=7.03279; p3_pc3_s_sdp[1][1][6][0]=103.428; p4_pc3_s_sdp[1][1][6][0]=-97.1384; 
  p0_pc3_m_sdp[0][1][7][0]=0.340322; p1_pc3_m_sdp[0][1][7][0]=-0.860684; p2_pc3_m_sdp[0][1][7][0]=0.786976; p3_pc3_m_sdp[0][1][7][0]=-0.300778; p4_pc3_m_sdp[0][1][7][0]=0.0402024; 
  p0_pc3_s_sdp[0][1][7][0]=-0.175885; p1_pc3_s_sdp[0][1][7][0]=2.36763; p2_pc3_s_sdp[0][1][7][0]=-2.00378; p3_pc3_s_sdp[0][1][7][0]=0.703794; p4_pc3_s_sdp[0][1][7][0]=-0.083842; 
  p0_pc3_m_sdp[1][1][7][0]=-0.0121306; p1_pc3_m_sdp[1][1][7][0]=0.0996646; p2_pc3_m_sdp[1][1][7][0]=0.172639; p3_pc3_m_sdp[1][1][7][0]=0.0663731; p4_pc3_m_sdp[1][1][7][0]=-0.492305; 
  p0_pc3_s_sdp[1][1][7][0]=0.924585; p1_pc3_s_sdp[1][1][7][0]=-0.201054; p2_pc3_s_sdp[1][1][7][0]=-1.17529; p3_pc3_s_sdp[1][1][7][0]=-0.774114; p4_pc3_s_sdp[1][1][7][0]=3.42977; 
  p0_pc3_m_sdp[0][1][8][0]=0.134268; p1_pc3_m_sdp[0][1][8][0]=-0.326896; p2_pc3_m_sdp[0][1][8][0]=0.290995; p3_pc3_m_sdp[0][1][8][0]=-0.11024; p4_pc3_m_sdp[0][1][8][0]=0.0140097; 
  p0_pc3_s_sdp[0][1][8][0]=-0.209176; p1_pc3_s_sdp[0][1][8][0]=2.49796; p2_pc3_s_sdp[0][1][8][0]=-2.15322; p3_pc3_s_sdp[0][1][8][0]=0.770947; p4_pc3_s_sdp[0][1][8][0]=-0.0932978; 
  p0_pc3_m_sdp[1][1][8][0]=-9.65914; p1_pc3_m_sdp[1][1][8][0]=41.4009; p2_pc3_m_sdp[1][1][8][0]=-16.0595; p3_pc3_m_sdp[1][1][8][0]=-114.628; p4_pc3_m_sdp[1][1][8][0]=118.43; 
  p0_pc3_s_sdp[1][1][8][0]=-11.0972; p1_pc3_s_sdp[1][1][8][0]=45.9676; p2_pc3_s_sdp[1][1][8][0]=-13.9896; p3_pc3_s_sdp[1][1][8][0]=-120.217; p4_pc3_s_sdp[1][1][8][0]=117.046; 
  p0_pc3_m_sdp[0][1][9][0]=-0.0393246; p1_pc3_m_sdp[0][1][9][0]=0.186285; p2_pc3_m_sdp[0][1][9][0]=-0.228468; p3_pc3_m_sdp[0][1][9][0]=0.0945953; p4_pc3_m_sdp[0][1][9][0]=-0.0129996; 
  p0_pc3_s_sdp[0][1][9][0]=-0.408625; p1_pc3_s_sdp[0][1][9][0]=2.95162; p2_pc3_s_sdp[0][1][9][0]=-2.55013; p3_pc3_s_sdp[0][1][9][0]=0.919019; p4_pc3_s_sdp[0][1][9][0]=-0.112598; 
  p0_pc3_m_sdp[1][1][9][0]=-7.7752; p1_pc3_m_sdp[1][1][9][0]=33.9321; p2_pc3_m_sdp[1][1][9][0]=-13.4329; p3_pc3_m_sdp[1][1][9][0]=-94.2346; p4_pc3_m_sdp[1][1][9][0]=97.3067; 
  p0_pc3_s_sdp[1][1][9][0]=-4.54481; p1_pc3_s_sdp[1][1][9][0]=20.9839; p2_pc3_s_sdp[1][1][9][0]=-6.99459; p3_pc3_s_sdp[1][1][9][0]=-55.1927; p4_pc3_s_sdp[1][1][9][0]=54.5593; 


  //PC3/DPHI/NEG/19GeV
  
  p0_pc3_m_sdp[0][0][0][1]=0.220859; p1_pc3_m_sdp[0][0][0][1]=-0.538907; p2_pc3_m_sdp[0][0][0][1]=0.464051; p3_pc3_m_sdp[0][0][0][1]=-0.162626; p4_pc3_m_sdp[0][0][0][1]=0.0205963; 
  p0_pc3_s_sdp[0][0][0][1]=-0.171011; p1_pc3_s_sdp[0][0][0][1]=2.44294; p2_pc3_s_sdp[0][0][0][1]=-2.22655; p3_pc3_s_sdp[0][0][0][1]=0.832203; p4_pc3_s_sdp[0][0][0][1]=-0.104259; 
  p0_pc3_m_sdp[1][0][0][1]=2.2993; p1_pc3_m_sdp[1][0][0][1]=-11.0932; p2_pc3_m_sdp[1][0][0][1]=5.27797; p3_pc3_m_sdp[1][0][0][1]=31.7258; p4_pc3_m_sdp[1][0][0][1]=-33.7901; 
  p0_pc3_s_sdp[1][0][0][1]=3.79743; p1_pc3_s_sdp[1][0][0][1]=-10.616; p2_pc3_s_sdp[1][0][0][1]=1.53801; p3_pc3_s_sdp[1][0][0][1]=24.5814; p4_pc3_s_sdp[1][0][0][1]=-19.9887; 
  p0_pc3_m_sdp[0][0][1][1]=-0.358471; p1_pc3_m_sdp[0][0][1][1]=1.03467; p2_pc3_m_sdp[0][0][1][1]=-1.04239; p3_pc3_m_sdp[0][0][1][1]=0.436342; p4_pc3_m_sdp[0][0][1][1]=-0.0639689; 
  p0_pc3_s_sdp[0][0][1][1]=-0.511133; p1_pc3_s_sdp[0][0][1][1]=3.1667; p2_pc3_s_sdp[0][0][1][1]=-2.76893; p3_pc3_s_sdp[0][0][1][1]=0.998137; p4_pc3_s_sdp[0][0][1][1]=-0.119897; 
  p0_pc3_m_sdp[1][0][1][1]=5.87057; p1_pc3_m_sdp[1][0][1][1]=-22.7279; p2_pc3_m_sdp[1][0][1][1]=6.00286; p3_pc3_m_sdp[1][0][1][1]=62.0584; p4_pc3_m_sdp[1][0][1][1]=-59.6991; 
  p0_pc3_s_sdp[1][0][1][1]=2.37501; p1_pc3_s_sdp[1][0][1][1]=-7.77373; p2_pc3_s_sdp[1][0][1][1]=3.43324; p3_pc3_s_sdp[1][0][1][1]=21.6086; p4_pc3_s_sdp[1][0][1][1]=-22.6999; 
  p0_pc3_m_sdp[0][0][2][1]=0.126147; p1_pc3_m_sdp[0][0][2][1]=-0.362118; p2_pc3_m_sdp[0][0][2][1]=0.316443; p3_pc3_m_sdp[0][0][2][1]=-0.111968; p4_pc3_m_sdp[0][0][2][1]=0.0143444; 
  p0_pc3_s_sdp[0][0][2][1]=-0.109596; p1_pc3_s_sdp[0][0][2][1]=1.89948; p2_pc3_s_sdp[0][0][2][1]=-1.46893; p3_pc3_s_sdp[0][0][2][1]=0.469936; p4_pc3_s_sdp[0][0][2][1]=-0.0489946; 
  p0_pc3_m_sdp[1][0][2][1]=4.48802; p1_pc3_m_sdp[1][0][2][1]=-16.455; p2_pc3_m_sdp[1][0][2][1]=3.32353; p3_pc3_m_sdp[1][0][2][1]=41.9637; p4_pc3_m_sdp[1][0][2][1]=-37.6045; 
  p0_pc3_s_sdp[1][0][2][1]=2.67032; p1_pc3_s_sdp[1][0][2][1]=-7.08614; p2_pc3_s_sdp[1][0][2][1]=-0.396828; p3_pc3_s_sdp[1][0][2][1]=19.4952; p4_pc3_s_sdp[1][0][2][1]=-14.4359; 
  p0_pc3_m_sdp[0][0][3][1]=-0.158504; p1_pc3_m_sdp[0][0][3][1]=0.295395; p2_pc3_m_sdp[0][0][3][1]=-0.242896; p3_pc3_m_sdp[0][0][3][1]=0.0934147; p4_pc3_m_sdp[0][0][3][1]=-0.013271; 
  p0_pc3_s_sdp[0][0][3][1]=0.172005; p1_pc3_s_sdp[0][0][3][1]=1.19154; p2_pc3_s_sdp[0][0][3][1]=-0.885538; p3_pc3_s_sdp[0][0][3][1]=0.280332; p4_pc3_s_sdp[0][0][3][1]=-0.030422; 
  p0_pc3_m_sdp[1][0][3][1]=4.73061; p1_pc3_m_sdp[1][0][3][1]=-18.9967; p2_pc3_m_sdp[1][0][3][1]=5.10886; p3_pc3_m_sdp[1][0][3][1]=50.9312; p4_pc3_m_sdp[1][0][3][1]=-48.0372; 
  p0_pc3_s_sdp[1][0][3][1]=6.09221; p1_pc3_s_sdp[1][0][3][1]=-21.8399; p2_pc3_s_sdp[1][0][3][1]=5.75032; p3_pc3_s_sdp[1][0][3][1]=60.5109; p4_pc3_s_sdp[1][0][3][1]=-57.847; 
  p0_pc3_m_sdp[0][0][4][1]=-0.070492; p1_pc3_m_sdp[0][0][4][1]=0.0785229; p2_pc3_m_sdp[0][0][4][1]=-0.0513388; p3_pc3_m_sdp[0][0][4][1]=0.019306; p4_pc3_m_sdp[0][0][4][1]=-0.00284747; 
  p0_pc3_s_sdp[0][0][4][1]=0.158933; p1_pc3_s_sdp[0][0][4][1]=1.18029; p2_pc3_s_sdp[0][0][4][1]=-0.858806; p3_pc3_s_sdp[0][0][4][1]=0.271608; p4_pc3_s_sdp[0][0][4][1]=-0.0304039; 
  p0_pc3_m_sdp[1][0][4][1]=0.190483; p1_pc3_m_sdp[1][0][4][1]=-0.106645; p2_pc3_m_sdp[1][0][4][1]=-0.397899; p3_pc3_m_sdp[1][0][4][1]=-0.358291; p4_pc3_m_sdp[1][0][4][1]=0.696492; 
  p0_pc3_s_sdp[1][0][4][1]=-3.09097; p1_pc3_s_sdp[1][0][4][1]=13.8771; p2_pc3_s_sdp[1][0][4][1]=-3.60698; p3_pc3_s_sdp[1][0][4][1]=-35.1957; p4_pc3_s_sdp[1][0][4][1]=33.025; 
  p0_pc3_m_sdp[0][0][5][1]=0.0773024; p1_pc3_m_sdp[0][0][5][1]=-0.355169; p2_pc3_m_sdp[0][0][5][1]=0.377217; p3_pc3_m_sdp[0][0][5][1]=-0.154841; p4_pc3_m_sdp[0][0][5][1]=0.0220008; 
  p0_pc3_s_sdp[0][0][5][1]=0.383711; p1_pc3_s_sdp[0][0][5][1]=0.65795; p2_pc3_s_sdp[0][0][5][1]=-0.414828; p3_pc3_s_sdp[0][0][5][1]=0.112762; p4_pc3_s_sdp[0][0][5][1]=-0.0102219; 
  p0_pc3_m_sdp[1][0][5][1]=2.25217; p1_pc3_m_sdp[1][0][5][1]=-8.17411; p2_pc3_m_sdp[1][0][5][1]=1.77552; p3_pc3_m_sdp[1][0][5][1]=20.6363; p4_pc3_m_sdp[1][0][5][1]=-19.1958; 
  p0_pc3_s_sdp[1][0][5][1]=2.30127; p1_pc3_s_sdp[1][0][5][1]=-6.73189; p2_pc3_s_sdp[1][0][5][1]=1.89952; p3_pc3_s_sdp[1][0][5][1]=19.1377; p4_pc3_s_sdp[1][0][5][1]=-18.3351; 
  p0_pc3_m_sdp[0][0][6][1]=0.0923887; p1_pc3_m_sdp[0][0][6][1]=-0.356923; p2_pc3_m_sdp[0][0][6][1]=0.350223; p3_pc3_m_sdp[0][0][6][1]=-0.136459; p4_pc3_m_sdp[0][0][6][1]=0.018834; 
  p0_pc3_s_sdp[0][0][6][1]=0.305858; p1_pc3_s_sdp[0][0][6][1]=0.918054; p2_pc3_s_sdp[0][0][6][1]=-0.685089; p3_pc3_s_sdp[0][0][6][1]=0.220512; p4_pc3_s_sdp[0][0][6][1]=-0.0243426; 
  p0_pc3_m_sdp[1][0][6][1]=2.69222; p1_pc3_m_sdp[1][0][6][1]=-9.4817; p2_pc3_m_sdp[1][0][6][1]=1.82145; p3_pc3_m_sdp[1][0][6][1]=22.9336; p4_pc3_m_sdp[1][0][6][1]=-20.381; 
  p0_pc3_s_sdp[1][0][6][1]=1.82539; p1_pc3_s_sdp[1][0][6][1]=-5.23459; p2_pc3_s_sdp[1][0][6][1]=2.30865; p3_pc3_s_sdp[1][0][6][1]=13.7018; p4_pc3_s_sdp[1][0][6][1]=-14.1881; 
  p0_pc3_m_sdp[0][0][7][1]=0.0817999; p1_pc3_m_sdp[0][0][7][1]=-0.39624; p2_pc3_m_sdp[0][0][7][1]=0.449506; p3_pc3_m_sdp[0][0][7][1]=-0.19359; p4_pc3_m_sdp[0][0][7][1]=0.0287025; 
  p0_pc3_s_sdp[0][0][7][1]=-0.179723; p1_pc3_s_sdp[0][0][7][1]=2.1412; p2_pc3_s_sdp[0][0][7][1]=-1.72339; p3_pc3_s_sdp[0][0][7][1]=0.573213; p4_pc3_s_sdp[0][0][7][1]=-0.0628855; 
  p0_pc3_m_sdp[1][0][7][1]=3.62303; p1_pc3_m_sdp[1][0][7][1]=-13.8146; p2_pc3_m_sdp[1][0][7][1]=2.85801; p3_pc3_m_sdp[1][0][7][1]=37.6478; p4_pc3_m_sdp[1][0][7][1]=-34.9422; 
  p0_pc3_s_sdp[1][0][7][1]=2.20475; p1_pc3_s_sdp[1][0][7][1]=-6.68706; p2_pc3_s_sdp[1][0][7][1]=1.62995; p3_pc3_s_sdp[1][0][7][1]=19.2206; p4_pc3_s_sdp[1][0][7][1]=-17.7335; 
  p0_pc3_m_sdp[0][0][8][1]=-0.198734; p1_pc3_m_sdp[0][0][8][1]=0.317164; p2_pc3_m_sdp[0][0][8][1]=-0.17919; p3_pc3_m_sdp[0][0][8][1]=0.0409488; p4_pc3_m_sdp[0][0][8][1]=-0.00245641; 
  p0_pc3_s_sdp[0][0][8][1]=-0.587592; p1_pc3_s_sdp[0][0][8][1]=3.15381; p2_pc3_s_sdp[0][0][8][1]=-2.62934; p3_pc3_s_sdp[0][0][8][1]=0.919558; p4_pc3_s_sdp[0][0][8][1]=-0.109507; 
  p0_pc3_m_sdp[1][0][8][1]=1.60574; p1_pc3_m_sdp[1][0][8][1]=-7.89161; p2_pc3_m_sdp[1][0][8][1]=3.14274; p3_pc3_m_sdp[1][0][8][1]=23.5566; p4_pc3_m_sdp[1][0][8][1]=-24.4687; 
  p0_pc3_s_sdp[1][0][8][1]=1.28418; p1_pc3_s_sdp[1][0][8][1]=-2.84036; p2_pc3_s_sdp[1][0][8][1]=1.58848; p3_pc3_s_sdp[1][0][8][1]=7.69074; p4_pc3_s_sdp[1][0][8][1]=-9.04694; 
  p0_pc3_m_sdp[0][0][9][1]=-0.106892; p1_pc3_m_sdp[0][0][9][1]=0.0499601; p2_pc3_m_sdp[0][0][9][1]=0.0969469; p3_pc3_m_sdp[0][0][9][1]=-0.0681763; p4_pc3_m_sdp[0][0][9][1]=0.0116813; 
  p0_pc3_s_sdp[0][0][9][1]=-0.7632; p1_pc3_s_sdp[0][0][9][1]=3.59176; p2_pc3_s_sdp[0][0][9][1]=-3.01; p3_pc3_s_sdp[0][0][9][1]=1.06331; p4_pc3_s_sdp[0][0][9][1]=-0.128847; 
  p0_pc3_m_sdp[1][0][9][1]=1.9247; p1_pc3_m_sdp[1][0][9][1]=-9.13483; p2_pc3_m_sdp[1][0][9][1]=3.41885; p3_pc3_m_sdp[1][0][9][1]=26.1557; p4_pc3_m_sdp[1][0][9][1]=-26.495; 
  p0_pc3_s_sdp[1][0][9][1]=0.610218; p1_pc3_s_sdp[1][0][9][1]=0.262027; p2_pc3_s_sdp[1][0][9][1]=-0.18983; p3_pc3_s_sdp[1][0][9][1]=-0.456517; p4_pc3_s_sdp[1][0][9][1]=0.0995641; 
  p0_pc3_m_sdp[0][1][0][1]=0.27566; p1_pc3_m_sdp[0][1][0][1]=-0.530872; p2_pc3_m_sdp[0][1][0][1]=0.418609; p3_pc3_m_sdp[0][1][0][1]=-0.146998; p4_pc3_m_sdp[0][1][0][1]=0.0190715; 
  p0_pc3_s_sdp[0][1][0][1]=-0.143104; p1_pc3_s_sdp[0][1][0][1]=2.37218; p2_pc3_s_sdp[0][1][0][1]=-2.12653; p3_pc3_s_sdp[0][1][0][1]=0.797407; p4_pc3_s_sdp[0][1][0][1]=-0.10112; 
  p0_pc3_m_sdp[1][1][0][1]=-4.59996; p1_pc3_m_sdp[1][1][0][1]=18.8442; p2_pc3_m_sdp[1][1][0][1]=-5.9331; p3_pc3_m_sdp[1][1][0][1]=-50.3868; p4_pc3_m_sdp[1][1][0][1]=49.2261; 
  p0_pc3_s_sdp[1][1][0][1]=-0.783247; p1_pc3_s_sdp[1][1][0][1]=5.04848; p2_pc3_s_sdp[1][1][0][1]=0.474176; p3_pc3_s_sdp[1][1][0][1]=-14.1699; p4_pc3_s_sdp[1][1][0][1]=10.5355; 
  p0_pc3_m_sdp[0][1][1][1]=0.362146; p1_pc3_m_sdp[0][1][1][1]=-0.677891; p2_pc3_m_sdp[0][1][1][1]=0.450457; p3_pc3_m_sdp[0][1][1][1]=-0.127042; p4_pc3_m_sdp[0][1][1][1]=0.0132247; 
  p0_pc3_s_sdp[0][1][1][1]=-0.351858; p1_pc3_s_sdp[0][1][1][1]=2.79542; p2_pc3_s_sdp[0][1][1][1]=-2.41285; p3_pc3_s_sdp[0][1][1][1]=0.865413; p4_pc3_s_sdp[0][1][1][1]=-0.104735; 
  p0_pc3_m_sdp[1][1][1][1]=2.13783; p1_pc3_m_sdp[1][1][1][1]=-7.85014; p2_pc3_m_sdp[1][1][1][1]=1.65134; p3_pc3_m_sdp[1][1][1][1]=22.353; p4_pc3_m_sdp[1][1][1][1]=-21.184; 
  p0_pc3_s_sdp[1][1][1][1]=4.27645; p1_pc3_s_sdp[1][1][1][1]=-11.5534; p2_pc3_s_sdp[1][1][1][1]=0.3796; p3_pc3_s_sdp[1][1][1][1]=28.0463; p4_pc3_s_sdp[1][1][1][1]=-21.9508; 
  p0_pc3_m_sdp[0][1][2][1]=0.21953; p1_pc3_m_sdp[0][1][2][1]=-0.468178; p2_pc3_m_sdp[0][1][2][1]=0.356922; p3_pc3_m_sdp[0][1][2][1]=-0.118241; p4_pc3_m_sdp[0][1][2][1]=0.0146399; 
  p0_pc3_s_sdp[0][1][2][1]=-0.0371748; p1_pc3_s_sdp[0][1][2][1]=1.85708; p2_pc3_s_sdp[0][1][2][1]=-1.39894; p3_pc3_s_sdp[0][1][2][1]=0.415508; p4_pc3_s_sdp[0][1][2][1]=-0.0363037; 
  p0_pc3_m_sdp[1][1][2][1]=0.756322; p1_pc3_m_sdp[1][1][2][1]=-3.11741; p2_pc3_m_sdp[1][1][2][1]=2.24187; p3_pc3_m_sdp[1][1][2][1]=7.8547; p4_pc3_m_sdp[1][1][2][1]=-10.1519; 
  p0_pc3_s_sdp[1][1][2][1]=-4.03787; p1_pc3_s_sdp[1][1][2][1]=17.6785; p2_pc3_s_sdp[1][1][2][1]=-3.82072; p3_pc3_s_sdp[1][1][2][1]=-48.8056; p4_pc3_s_sdp[1][1][2][1]=46.5773; 
  p0_pc3_m_sdp[0][1][3][1]=0.178477; p1_pc3_m_sdp[0][1][3][1]=-0.405956; p2_pc3_m_sdp[0][1][3][1]=0.291589; p3_pc3_m_sdp[0][1][3][1]=-0.08789; p4_pc3_m_sdp[0][1][3][1]=0.0096363; 
  p0_pc3_s_sdp[0][1][3][1]=0.273667; p1_pc3_s_sdp[0][1][3][1]=1.18044; p2_pc3_s_sdp[0][1][3][1]=-0.924651; p3_pc3_s_sdp[0][1][3][1]=0.288234; p4_pc3_s_sdp[0][1][3][1]=-0.0285615; 
  p0_pc3_m_sdp[1][1][3][1]=-2.39402; p1_pc3_m_sdp[1][1][3][1]=9.969; p2_pc3_m_sdp[1][1][3][1]=-3.05411; p3_pc3_m_sdp[1][1][3][1]=-29.2813; p4_pc3_m_sdp[1][1][3][1]=29.3794; 
  p0_pc3_s_sdp[1][1][3][1]=3.34259; p1_pc3_s_sdp[1][1][3][1]=-10.3401; p2_pc3_s_sdp[1][1][3][1]=2.5034; p3_pc3_s_sdp[1][1][3][1]=29.0978; p4_pc3_s_sdp[1][1][3][1]=-27.8054; 
  p0_pc3_m_sdp[0][1][4][1]=-0.100165; p1_pc3_m_sdp[0][1][4][1]=0.284234; p2_pc3_m_sdp[0][1][4][1]=-0.313539; p3_pc3_m_sdp[0][1][4][1]=0.138512; p4_pc3_m_sdp[0][1][4][1]=-0.0210874; 
  p0_pc3_s_sdp[0][1][4][1]=0.674714; p1_pc3_s_sdp[0][1][4][1]=0.243103; p2_pc3_s_sdp[0][1][4][1]=-0.164656; p3_pc3_s_sdp[0][1][4][1]=0.0407617; p4_pc3_s_sdp[0][1][4][1]=-0.00202425; 
  p0_pc3_m_sdp[1][1][4][1]=-2.59468; p1_pc3_m_sdp[1][1][4][1]=10.4174; p2_pc3_m_sdp[1][1][4][1]=-4.055; p3_pc3_m_sdp[1][1][4][1]=-28.0108; p4_pc3_m_sdp[1][1][4][1]=29.1437; 
  p0_pc3_s_sdp[1][1][4][1]=-6.63531; p1_pc3_s_sdp[1][1][4][1]=31.2361; p2_pc3_s_sdp[1][1][4][1]=-12.8666; p3_pc3_s_sdp[1][1][4][1]=-84.5615; p4_pc3_s_sdp[1][1][4][1]=88.5009; 
  p0_pc3_m_sdp[0][1][5][1]=0.0640114; p1_pc3_m_sdp[0][1][5][1]=-0.196838; p2_pc3_m_sdp[0][1][5][1]=0.158731; p3_pc3_m_sdp[0][1][5][1]=-0.0495056; p4_pc3_m_sdp[0][1][5][1]=0.00518196; 
  p0_pc3_s_sdp[0][1][5][1]=0.266047; p1_pc3_s_sdp[0][1][5][1]=1.1769; p2_pc3_s_sdp[0][1][5][1]=-0.913679; p3_pc3_s_sdp[0][1][5][1]=0.286661; p4_pc3_s_sdp[0][1][5][1]=-0.0300111; 
  p0_pc3_m_sdp[1][1][5][1]=8.59228; p1_pc3_m_sdp[1][1][5][1]=-33.2165; p2_pc3_m_sdp[1][1][5][1]=9.80227; p3_pc3_m_sdp[1][1][5][1]=87.1844; p4_pc3_m_sdp[1][1][5][1]=-84.5514; 
  p0_pc3_s_sdp[1][1][5][1]=0.761161; p1_pc3_s_sdp[1][1][5][1]=0.210845; p2_pc3_s_sdp[1][1][5][1]=-0.410498; p3_pc3_s_sdp[1][1][5][1]=-0.544225; p4_pc3_s_sdp[1][1][5][1]=1.01454; 
  p0_pc3_m_sdp[0][1][6][1]=-0.147602; p1_pc3_m_sdp[0][1][6][1]=0.398972; p2_pc3_m_sdp[0][1][6][1]=-0.425942; p3_pc3_m_sdp[0][1][6][1]=0.192029; p4_pc3_m_sdp[0][1][6][1]=-0.0304162; 
  p0_pc3_s_sdp[0][1][6][1]=0.534825; p1_pc3_s_sdp[0][1][6][1]=0.574904; p2_pc3_s_sdp[0][1][6][1]=-0.457637; p3_pc3_s_sdp[0][1][6][1]=0.1413; p4_pc3_s_sdp[0][1][6][1]=-0.01248; 
  p0_pc3_m_sdp[1][1][6][1]=5.25307; p1_pc3_m_sdp[1][1][6][1]=-22.4402; p2_pc3_m_sdp[1][1][6][1]=7.13052; p3_pc3_m_sdp[1][1][6][1]=63.6725; p4_pc3_m_sdp[1][1][6][1]=-62.9772; 
  p0_pc3_s_sdp[1][1][6][1]=2.79007; p1_pc3_s_sdp[1][1][6][1]=-8.06676; p2_pc3_s_sdp[1][1][6][1]=1.96789; p3_pc3_s_sdp[1][1][6][1]=20.8281; p4_pc3_s_sdp[1][1][6][1]=-18.8702; 
  p0_pc3_m_sdp[0][1][7][1]=0.200208; p1_pc3_m_sdp[0][1][7][1]=-0.510032; p2_pc3_m_sdp[0][1][7][1]=0.429461; p3_pc3_m_sdp[0][1][7][1]=-0.150245; p4_pc3_m_sdp[0][1][7][1]=0.0191134; 
  p0_pc3_s_sdp[0][1][7][1]=0.0649695; p1_pc3_s_sdp[0][1][7][1]=1.83379; p2_pc3_s_sdp[0][1][7][1]=-1.63925; p3_pc3_s_sdp[0][1][7][1]=0.594116; p4_pc3_s_sdp[0][1][7][1]=-0.0712386; 
  p0_pc3_m_sdp[1][1][7][1]=-1.94515; p1_pc3_m_sdp[1][1][7][1]=7.83628; p2_pc3_m_sdp[1][1][7][1]=-2.41018; p3_pc3_m_sdp[1][1][7][1]=-19.8519; p4_pc3_m_sdp[1][1][7][1]=18.695; 
  p0_pc3_s_sdp[1][1][7][1]=-1.15761; p1_pc3_s_sdp[1][1][7][1]=6.30093; p2_pc3_s_sdp[1][1][7][1]=-0.957623; p3_pc3_s_sdp[1][1][7][1]=-13.9682; p4_pc3_s_sdp[1][1][7][1]=11.3253; 
  p0_pc3_m_sdp[0][1][8][1]=0.121521; p1_pc3_m_sdp[0][1][8][1]=-0.319591; p2_pc3_m_sdp[0][1][8][1]=0.320953; p3_pc3_m_sdp[0][1][8][1]=-0.138591; p4_pc3_m_sdp[0][1][8][1]=0.0220033; 
  p0_pc3_s_sdp[0][1][8][1]=-0.0607129; p1_pc3_s_sdp[0][1][8][1]=2.22326; p2_pc3_s_sdp[0][1][8][1]=-2.01005; p3_pc3_s_sdp[0][1][8][1]=0.736362; p4_pc3_s_sdp[0][1][8][1]=-0.0893431; 
  p0_pc3_m_sdp[1][1][8][1]=-2.74547; p1_pc3_m_sdp[1][1][8][1]=10.8905; p2_pc3_m_sdp[1][1][8][1]=-2.94668; p3_pc3_m_sdp[1][1][8][1]=-30.1789; p4_pc3_m_sdp[1][1][8][1]=28.8966; 
  p0_pc3_s_sdp[1][1][8][1]=0.645074; p1_pc3_s_sdp[1][1][8][1]=0.322537; p2_pc3_s_sdp[1][1][8][1]=-0.108358; p3_pc3_s_sdp[1][1][8][1]=-0.368136; p4_pc3_s_sdp[1][1][8][1]=0.150654; 
  p0_pc3_m_sdp[0][1][9][1]=0.0384845; p1_pc3_m_sdp[0][1][9][1]=-0.0669241; p2_pc3_m_sdp[0][1][9][1]=0.06944; p3_pc3_m_sdp[0][1][9][1]=-0.0338697; p4_pc3_m_sdp[0][1][9][1]=0.00635383; 
  p0_pc3_s_sdp[0][1][9][1]=-0.332175; p1_pc3_s_sdp[0][1][9][1]=3.0036; p2_pc3_s_sdp[0][1][9][1]=-2.74353; p3_pc3_s_sdp[0][1][9][1]=1.0171; p4_pc3_s_sdp[0][1][9][1]=-0.126589; 
  p0_pc3_m_sdp[1][1][9][1]=1.92116; p1_pc3_m_sdp[1][1][9][1]=-7.14736; p2_pc3_m_sdp[1][1][9][1]=1.69644; p3_pc3_m_sdp[1][1][9][1]=18.2525; p4_pc3_m_sdp[1][1][9][1]=-16.6097; 
  p0_pc3_s_sdp[1][1][9][1]=-3.33217; p1_pc3_s_sdp[1][1][9][1]=16.6899; p2_pc3_s_sdp[1][1][9][1]=-5.16626; p3_pc3_s_sdp[1][1][9][1]=-48.1666; p4_pc3_s_sdp[1][1][9][1]=48.7083; 


  //PC3/DZ/NEG/19

  p0_pc3_m_sdz[0][0][0][0]=-0.0280335; p1_pc3_m_sdz[0][0][0][0]=-0.0220574; p2_pc3_m_sdz[0][0][0][0]=0.059206; p3_pc3_m_sdz[0][0][0][0]=-0.0387542; p4_pc3_m_sdz[0][0][0][0]=0.00861625; 
  p0_pc3_s_sdz[0][0][0][0]=0.679053; p1_pc3_s_sdz[0][0][0][0]=0.154726; p2_pc3_s_sdz[0][0][0][0]=-0.0393122; p3_pc3_s_sdz[0][0][0][0]=-0.0109787; p4_pc3_s_sdz[0][0][0][0]=0.00407338; 
  p0_pc3_m_sdz[1][0][0][0]=5.96619; p1_pc3_m_sdz[1][0][0][0]=-23.9867; p2_pc3_m_sdz[1][0][0][0]=2.54335; p3_pc3_m_sdz[1][0][0][0]=75.3345; p4_pc3_m_sdz[1][0][0][0]=-67.8438; 
  p0_pc3_s_sdz[1][0][0][0]=0.739555; p1_pc3_s_sdz[1][0][0][0]=0.307985; p2_pc3_s_sdz[1][0][0][0]=-0.209295; p3_pc3_s_sdz[1][0][0][0]=-0.477158; p4_pc3_s_sdz[1][0][0][0]=-0.00309115; 
  p0_pc3_m_sdz[0][0][1][0]=0.0135767; p1_pc3_m_sdz[0][0][1][0]=-0.04264; p2_pc3_m_sdz[0][0][1][0]=0.0142322; p3_pc3_m_sdz[0][0][1][0]=0.00497827; p4_pc3_m_sdz[0][0][1][0]=-0.00197482; 
  p0_pc3_s_sdz[0][0][1][0]=0.427475; p1_pc3_s_sdz[0][0][1][0]=0.690632; p2_pc3_s_sdz[0][0][1][0]=-0.465278; p3_pc3_s_sdz[0][0][1][0]=0.131642; p4_pc3_s_sdz[0][0][1][0]=-0.0122363; 
  p0_pc3_m_sdz[1][0][1][0]=18.8959; p1_pc3_m_sdz[1][0][1][0]=-73.7154; p2_pc3_m_sdz[1][0][1][0]=19.0894; p3_pc3_m_sdz[1][0][1][0]=193.308; p4_pc3_m_sdz[1][0][1][0]=-180.576; 
  p0_pc3_s_sdz[1][0][1][0]=3.26729; p1_pc3_s_sdz[1][0][1][0]=-15.1654; p2_pc3_s_sdz[1][0][1][0]=10.2333; p3_pc3_s_sdz[1][0][1][0]=48.2208; p4_pc3_s_sdz[1][0][1][0]=-57.7411; 
  p0_pc3_m_sdz[0][0][2][0]=0.0119767; p1_pc3_m_sdz[0][0][2][0]=-0.0324419; p2_pc3_m_sdz[0][0][2][0]=0.00494645; p3_pc3_m_sdz[0][0][2][0]=0.00350312; p4_pc3_m_sdz[0][0][2][0]=-0.00057279; 
  p0_pc3_s_sdz[0][0][2][0]=0.761483; p1_pc3_s_sdz[0][0][2][0]=-0.0953586; p2_pc3_s_sdz[0][0][2][0]=0.198987; p3_pc3_s_sdz[0][0][2][0]=-0.0982335; p4_pc3_s_sdz[0][0][2][0]=0.0154963; 
  p0_pc3_m_sdz[1][0][2][0]=-3.37347; p1_pc3_m_sdz[1][0][2][0]=12.2122; p2_pc3_m_sdz[1][0][2][0]=-0.637799; p3_pc3_m_sdz[1][0][2][0]=-38.6967; p4_pc3_m_sdz[1][0][2][0]=35.8658; 
  p0_pc3_s_sdz[1][0][2][0]=-1.17569; p1_pc3_s_sdz[1][0][2][0]=2.40216; p2_pc3_s_sdz[1][0][2][0]=5.31682; p3_pc3_s_sdz[1][0][2][0]=2.54058; p4_pc3_s_sdz[1][0][2][0]=-15.2821; 
  p0_pc3_m_sdz[0][0][3][0]=0.112349; p1_pc3_m_sdz[0][0][3][0]=-0.360118; p2_pc3_m_sdz[0][0][3][0]=0.367626; p3_pc3_m_sdz[0][0][3][0]=-0.149093; p4_pc3_m_sdz[0][0][3][0]=0.021162; 
  p0_pc3_s_sdz[0][0][3][0]=0.592112; p1_pc3_s_sdz[0][0][3][0]=0.234055; p2_pc3_s_sdz[0][0][3][0]=-0.0366149; p3_pc3_s_sdz[0][0][3][0]=-0.0314525; p4_pc3_s_sdz[0][0][3][0]=0.00930106; 
  p0_pc3_m_sdz[1][0][3][0]=-21.5958; p1_pc3_m_sdz[1][0][3][0]=83.8884; p2_pc3_m_sdz[1][0][3][0]=-23.2798; p3_pc3_m_sdz[1][0][3][0]=-225.409; p4_pc3_m_sdz[1][0][3][0]=217.887; 
  p0_pc3_s_sdz[1][0][3][0]=-18.9521; p1_pc3_s_sdz[1][0][3][0]=72.3286; p2_pc3_s_sdz[1][0][3][0]=-13.1836; p3_pc3_s_sdz[1][0][3][0]=-192.488; p4_pc3_s_sdz[1][0][3][0]=174.662; 
  p0_pc3_m_sdz[0][0][4][0]=0.142987; p1_pc3_m_sdz[0][0][4][0]=-0.300361; p2_pc3_m_sdz[0][0][4][0]=0.249712; p3_pc3_m_sdz[0][0][4][0]=-0.0922476; p4_pc3_m_sdz[0][0][4][0]=0.0126349; 
  p0_pc3_s_sdz[0][0][4][0]=0.281943; p1_pc3_s_sdz[0][0][4][0]=1.12974; p2_pc3_s_sdz[0][0][4][0]=-0.8279; p3_pc3_s_sdz[0][0][4][0]=0.246264; p4_pc3_s_sdz[0][0][4][0]=-0.0240236; 
  p0_pc3_m_sdz[1][0][4][0]=2.83378; p1_pc3_m_sdz[1][0][4][0]=-9.34729; p2_pc3_m_sdz[1][0][4][0]=2.09724; p3_pc3_m_sdz[1][0][4][0]=26.5634; p4_pc3_m_sdz[1][0][4][0]=-27.8236; 
  p0_pc3_s_sdz[1][0][4][0]=-20.0244; p1_pc3_s_sdz[1][0][4][0]=74.9166; p2_pc3_s_sdz[1][0][4][0]=-8.92605; p3_pc3_s_sdz[1][0][4][0]=-207.019; p4_pc3_s_sdz[1][0][4][0]=183.272; 
  p0_pc3_m_sdz[0][0][5][0]=0.0381872; p1_pc3_m_sdz[0][0][5][0]=-0.025036; p2_pc3_m_sdz[0][0][5][0]=-0.0441172; p3_pc3_m_sdz[0][0][5][0]=0.045987; p4_pc3_m_sdz[0][0][5][0]=-0.0102642; 
  p0_pc3_s_sdz[0][0][5][0]=0.62817; p1_pc3_s_sdz[0][0][5][0]=0.310785; p2_pc3_s_sdz[0][0][5][0]=-0.212462; p3_pc3_s_sdz[0][0][5][0]=0.0583819; p4_pc3_s_sdz[0][0][5][0]=-0.00415759; 
  p0_pc3_m_sdz[1][0][5][0]=-18.3657; p1_pc3_m_sdz[1][0][5][0]=71.8139; p2_pc3_m_sdz[1][0][5][0]=-19.3101; p3_pc3_m_sdz[1][0][5][0]=-193.028; p4_pc3_m_sdz[1][0][5][0]=184.225; 
  p0_pc3_s_sdz[1][0][5][0]=-8.98537; p1_pc3_s_sdz[1][0][5][0]=36.7288; p2_pc3_s_sdz[1][0][5][0]=-6.78885; p3_pc3_s_sdz[1][0][5][0]=-102.39; p4_pc3_s_sdz[1][0][5][0]=94.6149; 
  p0_pc3_m_sdz[0][0][6][0]=-0.163002; p1_pc3_m_sdz[0][0][6][0]=0.349464; p2_pc3_m_sdz[0][0][6][0]=-0.281031; p3_pc3_m_sdz[0][0][6][0]=0.0993914; p4_pc3_m_sdz[0][0][6][0]=-0.012915; 
  p0_pc3_s_sdz[0][0][6][0]=0.763961; p1_pc3_s_sdz[0][0][6][0]=0.00652044; p2_pc3_s_sdz[0][0][6][0]=0.0163884; p3_pc3_s_sdz[0][0][6][0]=0.00225746; p4_pc3_s_sdz[0][0][6][0]=-0.00174673; 
  p0_pc3_m_sdz[1][0][6][0]=-4.80504; p1_pc3_m_sdz[1][0][6][0]=20.5396; p2_pc3_m_sdz[1][0][6][0]=-8.06422; p3_pc3_m_sdz[1][0][6][0]=-55.6118; p4_pc3_m_sdz[1][0][6][0]=56.9117; 
  p0_pc3_s_sdz[1][0][6][0]=3.79668; p1_pc3_s_sdz[1][0][6][0]=-12.4618; p2_pc3_s_sdz[1][0][6][0]=4.31194; p3_pc3_s_sdz[1][0][6][0]=35.7135; p4_pc3_s_sdz[1][0][6][0]=-36.9897; 
  p0_pc3_m_sdz[0][0][7][0]=0.220897; p1_pc3_m_sdz[0][0][7][0]=-0.584785; p2_pc3_m_sdz[0][0][7][0]=0.530448; p3_pc3_m_sdz[0][0][7][0]=-0.20123; p4_pc3_m_sdz[0][0][7][0]=0.0274918; 
  p0_pc3_s_sdz[0][0][7][0]=1.02829; p1_pc3_s_sdz[0][0][7][0]=-0.630763; p2_pc3_s_sdz[0][0][7][0]=0.54298; p3_pc3_s_sdz[0][0][7][0]=-0.182845; p4_pc3_s_sdz[0][0][7][0]=0.0218308; 
  p0_pc3_m_sdz[1][0][7][0]=0.621336; p1_pc3_m_sdz[1][0][7][0]=-0.257901; p2_pc3_m_sdz[1][0][7][0]=-1.12117; p3_pc3_m_sdz[1][0][7][0]=-1.06088; p4_pc3_m_sdz[1][0][7][0]=1.7021; 
  p0_pc3_s_sdz[1][0][7][0]=-2.98084; p1_pc3_s_sdz[1][0][7][0]=15.9759; p2_pc3_s_sdz[1][0][7][0]=-6.58924; p3_pc3_s_sdz[1][0][7][0]=-41.3531; p4_pc3_s_sdz[1][0][7][0]=41.9455; 
  p0_pc3_m_sdz[0][0][8][0]=-0.0475365; p1_pc3_m_sdz[0][0][8][0]=-0.0302751; p2_pc3_m_sdz[0][0][8][0]=0.108047; p3_pc3_m_sdz[0][0][8][0]=-0.0582177; p4_pc3_m_sdz[0][0][8][0]=0.00913005; 
  p0_pc3_s_sdz[0][0][8][0]=0.601017; p1_pc3_s_sdz[0][0][8][0]=0.294018; p2_pc3_s_sdz[0][0][8][0]=-0.150397; p3_pc3_s_sdz[0][0][8][0]=0.0330493; p4_pc3_s_sdz[0][0][8][0]=-0.00202039; 
  p0_pc3_m_sdz[1][0][8][0]=0.668428; p1_pc3_m_sdz[1][0][8][0]=-0.58261; p2_pc3_m_sdz[1][0][8][0]=-1.70204; p3_pc3_m_sdz[1][0][8][0]=-1.13486; p4_pc3_m_sdz[1][0][8][0]=4.21802; 
  p0_pc3_s_sdz[1][0][8][0]=15.9215; p1_pc3_s_sdz[1][0][8][0]=-62.0683; p2_pc3_s_sdz[1][0][8][0]=22.5542; p3_pc3_s_sdz[1][0][8][0]=163.903; p4_pc3_s_sdz[1][0][8][0]=-164.682; 
  p0_pc3_m_sdz[0][0][9][0]=-0.219231; p1_pc3_m_sdz[0][0][9][0]=0.362799; p2_pc3_m_sdz[0][0][9][0]=-0.239815; p3_pc3_m_sdz[0][0][9][0]=0.0726458; p4_pc3_m_sdz[0][0][9][0]=-0.0090167; 
  p0_pc3_s_sdz[0][0][9][0]=0.779955; p1_pc3_s_sdz[0][0][9][0]=-0.107045; p2_pc3_s_sdz[0][0][9][0]=0.175494; p3_pc3_s_sdz[0][0][9][0]=-0.0711354; p4_pc3_s_sdz[0][0][9][0]=0.00860481; 
  p0_pc3_m_sdz[1][0][9][0]=1.12207; p1_pc3_m_sdz[1][0][9][0]=-0.982636; p2_pc3_m_sdz[1][0][9][0]=-2.90938; p3_pc3_m_sdz[1][0][9][0]=-2.00327; p4_pc3_m_sdz[1][0][9][0]=7.35788; 
  p0_pc3_s_sdz[1][0][9][0]=20.2133; p1_pc3_s_sdz[1][0][9][0]=-75.0703; p2_pc3_s_sdz[1][0][9][0]=14.4043; p3_pc3_s_sdz[1][0][9][0]=214.931; p4_pc3_s_sdz[1][0][9][0]=-200.503; 
  p0_pc3_m_sdz[0][1][0][0]=0.0370207; p1_pc3_m_sdz[0][1][0][0]=-0.189309; p2_pc3_m_sdz[0][1][0][0]=0.19812; p3_pc3_m_sdz[0][1][0][0]=-0.0774088; p4_pc3_m_sdz[0][1][0][0]=0.0108122; 
  p0_pc3_s_sdz[0][1][0][0]=0.681095; p1_pc3_s_sdz[0][1][0][0]=0.211637; p2_pc3_s_sdz[0][1][0][0]=-0.179993; p3_pc3_s_sdz[0][1][0][0]=0.0695059; p4_pc3_s_sdz[0][1][0][0]=-0.00916964; 
  p0_pc3_m_sdz[1][1][0][0]=-16.7928; p1_pc3_m_sdz[1][1][0][0]=61.5189; p2_pc3_m_sdz[1][1][0][0]=-15.1339; p3_pc3_m_sdz[1][1][0][0]=-157.333; p4_pc3_m_sdz[1][1][0][0]=147.889; 
  p0_pc3_s_sdz[1][1][0][0]=-32.8455; p1_pc3_s_sdz[1][1][0][0]=134.787; p2_pc3_s_sdz[1][1][0][0]=-41.8463; p3_pc3_s_sdz[1][1][0][0]=-364.095; p4_pc3_s_sdz[1][1][0][0]=357.051; 
  p0_pc3_m_sdz[0][1][1][0]=-0.176727; p1_pc3_m_sdz[0][1][1][0]=0.331699; p2_pc3_m_sdz[0][1][1][0]=-0.238068; p3_pc3_m_sdz[0][1][1][0]=0.0723792; p4_pc3_m_sdz[0][1][1][0]=-0.00777511; 
  p0_pc3_s_sdz[0][1][1][0]=0.815437; p1_pc3_s_sdz[0][1][1][0]=-0.13771; p2_pc3_s_sdz[0][1][1][0]=0.158279; p3_pc3_s_sdz[0][1][1][0]=-0.0541603; p4_pc3_s_sdz[0][1][1][0]=0.0060618; 
  p0_pc3_m_sdz[1][1][1][0]=-12.9318; p1_pc3_m_sdz[1][1][1][0]=52.2849; p2_pc3_m_sdz[1][1][1][0]=-18.6225; p3_pc3_m_sdz[1][1][1][0]=-143.492; p4_pc3_m_sdz[1][1][1][0]=146.761; 
  p0_pc3_s_sdz[1][1][1][0]=-1.2966; p1_pc3_s_sdz[1][1][1][0]=2.27587; p2_pc3_s_sdz[1][1][1][0]=5.55932; p3_pc3_s_sdz[1][1][1][0]=3.34569; p4_pc3_s_sdz[1][1][1][0]=-15.1695; 
  p0_pc3_m_sdz[0][1][2][0]=0.0988383; p1_pc3_m_sdz[0][1][2][0]=-0.246671; p2_pc3_m_sdz[0][1][2][0]=0.208013; p3_pc3_m_sdz[0][1][2][0]=-0.0777532; p4_pc3_m_sdz[0][1][2][0]=0.010905; 
  p0_pc3_s_sdz[0][1][2][0]=0.741265; p1_pc3_s_sdz[0][1][2][0]=0.0136423; p2_pc3_s_sdz[0][1][2][0]=0.0417986; p3_pc3_s_sdz[0][1][2][0]=-0.0206075; p4_pc3_s_sdz[0][1][2][0]=0.00321268; 
  p0_pc3_m_sdz[1][1][2][0]=0.373283; p1_pc3_m_sdz[1][1][2][0]=-0.681165; p2_pc3_m_sdz[1][1][2][0]=-1.49052; p3_pc3_m_sdz[1][1][2][0]=-0.535987; p4_pc3_m_sdz[1][1][2][0]=5.34744; 
  p0_pc3_s_sdz[1][1][2][0]=-6.64246; p1_pc3_s_sdz[1][1][2][0]=27.6498; p2_pc3_s_sdz[1][1][2][0]=-0.280432; p3_pc3_s_sdz[1][1][2][0]=-92.6685; p4_pc3_s_sdz[1][1][2][0]=84.3275; 
  p0_pc3_m_sdz[0][1][3][0]=-0.139877; p1_pc3_m_sdz[0][1][3][0]=0.318657; p2_pc3_m_sdz[0][1][3][0]=-0.252324; p3_pc3_m_sdz[0][1][3][0]=0.0831892; p4_pc3_m_sdz[0][1][3][0]=-0.00969848; 
  p0_pc3_s_sdz[0][1][3][0]=0.581214; p1_pc3_s_sdz[0][1][3][0]=0.376781; p2_pc3_s_sdz[0][1][3][0]=-0.252815; p3_pc3_s_sdz[0][1][3][0]=0.0825105; p4_pc3_s_sdz[0][1][3][0]=-0.0102132; 
  p0_pc3_m_sdz[1][1][3][0]=10.1179; p1_pc3_m_sdz[1][1][3][0]=-40.4256; p2_pc3_m_sdz[1][1][3][0]=13.14; p3_pc3_m_sdz[1][1][3][0]=105.3; p4_pc3_m_sdz[1][1][3][0]=-102.462; 
  p0_pc3_s_sdz[1][1][3][0]=-13.3133; p1_pc3_s_sdz[1][1][3][0]=54.784; p2_pc3_s_sdz[1][1][3][0]=-12.5567; p3_pc3_s_sdz[1][1][3][0]=-152.523; p4_pc3_s_sdz[1][1][3][0]=143.837; 
  p0_pc3_m_sdz[0][1][4][0]=0.113574; p1_pc3_m_sdz[0][1][4][0]=-0.207605; p2_pc3_m_sdz[0][1][4][0]=0.137687; p3_pc3_m_sdz[0][1][4][0]=-0.0340666; p4_pc3_m_sdz[0][1][4][0]=0.00221513; 
  p0_pc3_s_sdz[0][1][4][0]=0.701938; p1_pc3_s_sdz[0][1][4][0]=0.0437373; p2_pc3_s_sdz[0][1][4][0]=0.0212539; p3_pc3_s_sdz[0][1][4][0]=0.00086578; p4_pc3_s_sdz[0][1][4][0]=-0.00238418; 
  p0_pc3_m_sdz[1][1][4][0]=4.80806; p1_pc3_m_sdz[1][1][4][0]=-18.6815; p2_pc3_m_sdz[1][1][4][0]=5.4115; p3_pc3_m_sdz[1][1][4][0]=51.8895; p4_pc3_m_sdz[1][1][4][0]=-52.1876; 
  p0_pc3_s_sdz[1][1][4][0]=-7.48106; p1_pc3_s_sdz[1][1][4][0]=28.0117; p2_pc3_s_sdz[1][1][4][0]=-2.84826; p3_pc3_s_sdz[1][1][4][0]=-70.0652; p4_pc3_s_sdz[1][1][4][0]=58.5599; 
  p0_pc3_m_sdz[0][1][5][0]=-0.0940049; p1_pc3_m_sdz[0][1][5][0]=0.307385; p2_pc3_m_sdz[0][1][5][0]=-0.301959; p3_pc3_m_sdz[0][1][5][0]=0.129067; p4_pc3_m_sdz[0][1][5][0]=-0.019944; 
  p0_pc3_s_sdz[0][1][5][0]=0.622054; p1_pc3_s_sdz[0][1][5][0]=0.297387; p2_pc3_s_sdz[0][1][5][0]=-0.19326; p3_pc3_s_sdz[0][1][5][0]=0.0550946; p4_pc3_s_sdz[0][1][5][0]=-0.00486024; 
  p0_pc3_m_sdz[1][1][5][0]=14.6638; p1_pc3_m_sdz[1][1][5][0]=-58.2924; p2_pc3_m_sdz[1][1][5][0]=17.3022; p3_pc3_m_sdz[1][1][5][0]=152.163; p4_pc3_m_sdz[1][1][5][0]=-145.217; 
  p0_pc3_s_sdz[1][1][5][0]=-5.74306; p1_pc3_s_sdz[1][1][5][0]=24.5762; p2_pc3_s_sdz[1][1][5][0]=-8.23766; p3_pc3_s_sdz[1][1][5][0]=-61.2311; p4_pc3_s_sdz[1][1][5][0]=60.559; 
  p0_pc3_m_sdz[0][1][6][0]=-0.0517057; p1_pc3_m_sdz[0][1][6][0]=0.0941371; p2_pc3_m_sdz[0][1][6][0]=-0.0669642; p3_pc3_m_sdz[0][1][6][0]=0.0249554; p4_pc3_m_sdz[0][1][6][0]=-0.00373597; 
  p0_pc3_s_sdz[0][1][6][0]=0.622795; p1_pc3_s_sdz[0][1][6][0]=0.273795; p2_pc3_s_sdz[0][1][6][0]=-0.16116; p3_pc3_s_sdz[0][1][6][0]=0.049843; p4_pc3_s_sdz[0][1][6][0]=-0.00616714; 
  p0_pc3_m_sdz[1][1][6][0]=-4.30029; p1_pc3_m_sdz[1][1][6][0]=16.4656; p2_pc3_m_sdz[1][1][6][0]=-5.05687; p3_pc3_m_sdz[1][1][6][0]=-42.0856; p4_pc3_m_sdz[1][1][6][0]=41.3916; 
  p0_pc3_s_sdz[1][1][6][0]=-3.83836; p1_pc3_s_sdz[1][1][6][0]=15.3435; p2_pc3_s_sdz[1][1][6][0]=-1.88032; p3_pc3_s_sdz[1][1][6][0]=-31.8369; p4_pc3_s_sdz[1][1][6][0]=22.9884; 
  p0_pc3_m_sdz[0][1][7][0]=-0.0322509; p1_pc3_m_sdz[0][1][7][0]=0.0445155; p2_pc3_m_sdz[0][1][7][0]=-0.0126723; p3_pc3_m_sdz[0][1][7][0]=-0.00320677; p4_pc3_m_sdz[0][1][7][0]=0.00137855; 
  p0_pc3_s_sdz[0][1][7][0]=0.550333; p1_pc3_s_sdz[0][1][7][0]=0.399727; p2_pc3_s_sdz[0][1][7][0]=-0.212475; p3_pc3_s_sdz[0][1][7][0]=0.0445565; p4_pc3_s_sdz[0][1][7][0]=-0.0023985; 
  p0_pc3_m_sdz[1][1][7][0]=6.0576; p1_pc3_m_sdz[1][1][7][0]=-24.2118; p2_pc3_m_sdz[1][1][7][0]=7.02102; p3_pc3_m_sdz[1][1][7][0]=69.8253; p4_pc3_m_sdz[1][1][7][0]=-70.0199; 
  p0_pc3_s_sdz[1][1][7][0]=-0.542982; p1_pc3_s_sdz[1][1][7][0]=1.94357; p2_pc3_s_sdz[1][1][7][0]=3.71769; p3_pc3_s_sdz[1][1][7][0]=1.40895; p4_pc3_s_sdz[1][1][7][0]=-11.2548; 
  p0_pc3_m_sdz[0][1][8][0]=-0.0595687; p1_pc3_m_sdz[0][1][8][0]=0.18158; p2_pc3_m_sdz[0][1][8][0]=-0.208727; p3_pc3_m_sdz[0][1][8][0]=0.0968533; p4_pc3_m_sdz[0][1][8][0]=-0.015436; 
  p0_pc3_s_sdz[0][1][8][0]=0.750739; p1_pc3_s_sdz[0][1][8][0]=0.0055132; p2_pc3_s_sdz[0][1][8][0]=0.0183929; p3_pc3_s_sdz[0][1][8][0]=0.00234962; p4_pc3_s_sdz[0][1][8][0]=-0.00174069; 
  p0_pc3_m_sdz[1][1][8][0]=-7.83943; p1_pc3_m_sdz[1][1][8][0]=30.9311; p2_pc3_m_sdz[1][1][8][0]=-9.24208; p3_pc3_m_sdz[1][1][8][0]=-77.5338; p4_pc3_m_sdz[1][1][8][0]=72.2823; 
  p0_pc3_s_sdz[1][1][8][0]=-16.0769; p1_pc3_s_sdz[1][1][8][0]=63.5347; p2_pc3_s_sdz[1][1][8][0]=-12.7446; p3_pc3_s_sdz[1][1][8][0]=-173.356; p4_pc3_s_sdz[1][1][8][0]=159.736; 
  p0_pc3_m_sdz[0][1][9][0]=0.0538908; p1_pc3_m_sdz[0][1][9][0]=-0.115762; p2_pc3_m_sdz[0][1][9][0]=0.04643; p3_pc3_m_sdz[0][1][9][0]=0.00701599; p4_pc3_m_sdz[0][1][9][0]=-0.00456887; 
  p0_pc3_s_sdz[0][1][9][0]=0.615052; p1_pc3_s_sdz[0][1][9][0]=0.203074; p2_pc3_s_sdz[0][1][9][0]=-0.0356692; p3_pc3_s_sdz[0][1][9][0]=-0.00930687; p4_pc3_s_sdz[0][1][9][0]=0.00178753; 
  p0_pc3_m_sdz[1][1][9][0]=-9.66842; p1_pc3_m_sdz[1][1][9][0]=40.7457; p2_pc3_m_sdz[1][1][9][0]=-14.6531; p3_pc3_m_sdz[1][1][9][0]=-110.873; p4_pc3_m_sdz[1][1][9][0]=111.436; 
  p0_pc3_s_sdz[1][1][9][0]=8.54022; p1_pc3_s_sdz[1][1][9][0]=-33.8505; p2_pc3_s_sdz[1][1][9][0]=11.8646; p3_pc3_s_sdz[1][1][9][0]=98.9053; p4_pc3_s_sdz[1][1][9][0]=-100.804; 


  //PC3/DZ/POS/19GeV
  
  p0_pc3_m_sdz[0][0][0][1]=-0.0979281; p1_pc3_m_sdz[0][0][0][1]=0.156889; p2_pc3_m_sdz[0][0][0][1]=-0.102498; p3_pc3_m_sdz[0][0][0][1]=0.0231019; p4_pc3_m_sdz[0][0][0][1]=-3.85897e-05; 
  p0_pc3_s_sdz[0][0][0][1]=0.748218; p1_pc3_s_sdz[0][0][0][1]=0.0499552; p2_pc3_s_sdz[0][0][0][1]=-0.00567289; p3_pc3_s_sdz[0][0][0][1]=-0.00180835; p4_pc3_s_sdz[0][0][0][1]=-0.000446024; 
  p0_pc3_m_sdz[1][0][0][1]=2.14936; p1_pc3_m_sdz[1][0][0][1]=-12.907; p2_pc3_m_sdz[1][0][0][1]=7.46845; p3_pc3_m_sdz[1][0][0][1]=39.9685; p4_pc3_m_sdz[1][0][0][1]=-44.8183; 
  p0_pc3_s_sdz[1][0][0][1]=-11.2724; p1_pc3_s_sdz[1][0][0][1]=45.2217; p2_pc3_s_sdz[1][0][0][1]=-12.217; p3_pc3_s_sdz[1][0][0][1]=-114.626; p4_pc3_s_sdz[1][0][0][1]=107.98; 
  p0_pc3_m_sdz[0][0][1][1]=0.138098; p1_pc3_m_sdz[0][0][1][1]=-0.350659; p2_pc3_m_sdz[0][0][1][1]=0.269388; p3_pc3_m_sdz[0][0][1][1]=-0.090758; p4_pc3_m_sdz[0][0][1][1]=0.0118422; 
  p0_pc3_s_sdz[0][0][1][1]=0.80879; p1_pc3_s_sdz[0][0][1][1]=-0.0634069; p2_pc3_s_sdz[0][0][1][1]=0.0335381; p3_pc3_s_sdz[0][0][1][1]=0.00453111; p4_pc3_s_sdz[0][0][1][1]=-0.00274674; 
  p0_pc3_m_sdz[1][0][1][1]=-2.76507; p1_pc3_m_sdz[1][0][1][1]=8.92707; p2_pc3_m_sdz[1][0][1][1]=-0.0220821; p3_pc3_m_sdz[1][0][1][1]=-26.5663; p4_pc3_m_sdz[1][0][1][1]=23.929; 
  p0_pc3_s_sdz[1][0][1][1]=6.69406; p1_pc3_s_sdz[1][0][1][1]=-22.6937; p2_pc3_s_sdz[1][0][1][1]=6.33192; p3_pc3_s_sdz[1][0][1][1]=58.1402; p4_pc3_s_sdz[1][0][1][1]=-55.5515; 
  p0_pc3_m_sdz[0][0][2][1]=0.0475867; p1_pc3_m_sdz[0][0][2][1]=-0.221582; p2_pc3_m_sdz[0][0][2][1]=0.227937; p3_pc3_m_sdz[0][0][2][1]=-0.0922927; p4_pc3_m_sdz[0][0][2][1]=0.0131459; 
  p0_pc3_s_sdz[0][0][2][1]=0.501054; p1_pc3_s_sdz[0][0][2][1]=0.677876; p2_pc3_s_sdz[0][0][2][1]=-0.514389; p3_pc3_s_sdz[0][0][2][1]=0.151369; p4_pc3_s_sdz[0][0][2][1]=-0.0136593; 
  p0_pc3_m_sdz[1][0][2][1]=-4.80482; p1_pc3_m_sdz[1][0][2][1]=19.223; p2_pc3_m_sdz[1][0][2][1]=-5.37564; p3_pc3_m_sdz[1][0][2][1]=-54.2979; p4_pc3_m_sdz[1][0][2][1]=53.0594; 
  p0_pc3_s_sdz[1][0][2][1]=9.47819; p1_pc3_s_sdz[1][0][2][1]=-33.5518; p2_pc3_s_sdz[1][0][2][1]=7.51302; p3_pc3_s_sdz[1][0][2][1]=92.2212; p4_pc3_s_sdz[1][0][2][1]=-86.1041; 
  p0_pc3_m_sdz[0][0][3][1]=0.0388949; p1_pc3_m_sdz[0][0][3][1]=-0.133982; p2_pc3_m_sdz[0][0][3][1]=0.145017; p3_pc3_m_sdz[0][0][3][1]=-0.0647548; p4_pc3_m_sdz[0][0][3][1]=0.0103588; 
  p0_pc3_s_sdz[0][0][3][1]=0.40884; p1_pc3_s_sdz[0][0][3][1]=0.831257; p2_pc3_s_sdz[0][0][3][1]=-0.603349; p3_pc3_s_sdz[0][0][3][1]=0.176752; p4_pc3_s_sdz[0][0][3][1]=-0.0172213; 
  p0_pc3_m_sdz[1][0][3][1]=-3.3678; p1_pc3_m_sdz[1][0][3][1]=12.7475; p2_pc3_m_sdz[1][0][3][1]=-2.16429; p3_pc3_m_sdz[1][0][3][1]=-36.5395; p4_pc3_m_sdz[1][0][3][1]=33.6201; 
  p0_pc3_s_sdz[1][0][3][1]=14.4098; p1_pc3_s_sdz[1][0][3][1]=-55.8433; p2_pc3_s_sdz[1][0][3][1]=17.8371; p3_pc3_s_sdz[1][0][3][1]=155.635; p4_pc3_s_sdz[1][0][3][1]=-154.858; 
  p0_pc3_m_sdz[0][0][4][1]=0.177113; p1_pc3_m_sdz[0][0][4][1]=-0.352105; p2_pc3_m_sdz[0][0][4][1]=0.244422; p3_pc3_m_sdz[0][0][4][1]=-0.069461; p4_pc3_m_sdz[0][0][4][1]=0.00677991; 
  p0_pc3_s_sdz[0][0][4][1]=0.517389; p1_pc3_s_sdz[0][0][4][1]=0.685674; p2_pc3_s_sdz[0][0][4][1]=-0.566878; p3_pc3_s_sdz[0][0][4][1]=0.201789; p4_pc3_s_sdz[0][0][4][1]=-0.0252851; 
  p0_pc3_m_sdz[1][0][4][1]=5.71945; p1_pc3_m_sdz[1][0][4][1]=-23.1873; p2_pc3_m_sdz[1][0][4][1]=7.87714; p3_pc3_m_sdz[1][0][4][1]=59.9735; p4_pc3_m_sdz[1][0][4][1]=-58.6439; 
  p0_pc3_s_sdz[1][0][4][1]=-0.0987587; p1_pc3_s_sdz[1][0][4][1]=1.1912; p2_pc3_s_sdz[1][0][4][1]=2.0533; p3_pc3_s_sdz[1][0][4][1]=0.753738; p4_pc3_s_sdz[1][0][4][1]=-5.85792; 
  p0_pc3_m_sdz[0][0][5][1]=0.0790252; p1_pc3_m_sdz[0][0][5][1]=-0.263049; p2_pc3_m_sdz[0][0][5][1]=0.290094; p3_pc3_m_sdz[0][0][5][1]=-0.123543; p4_pc3_m_sdz[0][0][5][1]=0.0183825; 
  p0_pc3_s_sdz[0][0][5][1]=0.414146; p1_pc3_s_sdz[0][0][5][1]=0.807623; p2_pc3_s_sdz[0][0][5][1]=-0.579586; p3_pc3_s_sdz[0][0][5][1]=0.164677; p4_pc3_s_sdz[0][0][5][1]=-0.014578; 
  p0_pc3_m_sdz[1][0][5][1]=1.45497; p1_pc3_m_sdz[1][0][5][1]=-6.34134; p2_pc3_m_sdz[1][0][5][1]=2.07483; p3_pc3_m_sdz[1][0][5][1]=18.2324; p4_pc3_m_sdz[1][0][5][1]=-17.9326; 
  p0_pc3_s_sdz[1][0][5][1]=1.35577; p1_pc3_s_sdz[1][0][5][1]=-0.463787; p2_pc3_s_sdz[1][0][5][1]=-2.31965; p3_pc3_s_sdz[1][0][5][1]=-1.71672; p4_pc3_s_sdz[1][0][5][1]=6.8847; 
  p0_pc3_m_sdz[0][0][6][1]=-0.00808736; p1_pc3_m_sdz[0][0][6][1]=-0.0208956; p2_pc3_m_sdz[0][0][6][1]=0.0566852; p3_pc3_m_sdz[0][0][6][1]=-0.0315916; p4_pc3_m_sdz[0][0][6][1]=0.00529733; 
  p0_pc3_s_sdz[0][0][6][1]=0.685893; p1_pc3_s_sdz[0][0][6][1]=0.279206; p2_pc3_s_sdz[0][0][6][1]=-0.230264; p3_pc3_s_sdz[0][0][6][1]=0.0821948; p4_pc3_s_sdz[0][0][6][1]=-0.0102775; 
  p0_pc3_m_sdz[1][0][6][1]=-5.72329; p1_pc3_m_sdz[1][0][6][1]=23.2397; p2_pc3_m_sdz[1][0][6][1]=-7.61129; p3_pc3_m_sdz[1][0][6][1]=-59.9454; p4_pc3_m_sdz[1][0][6][1]=57.8787; 
  p0_pc3_s_sdz[1][0][6][1]=-5.25343; p1_pc3_s_sdz[1][0][6][1]=25.3404; p2_pc3_s_sdz[1][0][6][1]=-9.62399; p3_pc3_s_sdz[1][0][6][1]=-67.4972; p4_pc3_s_sdz[1][0][6][1]=68.1365; 
  p0_pc3_m_sdz[0][0][7][1]=-0.00767274; p1_pc3_m_sdz[0][0][7][1]=-0.0952781; p2_pc3_m_sdz[0][0][7][1]=0.165583; p3_pc3_m_sdz[0][0][7][1]=-0.0840167; p4_pc3_m_sdz[0][0][7][1]=0.0135035; 
  p0_pc3_s_sdz[0][0][7][1]=0.535736; p1_pc3_s_sdz[0][0][7][1]=0.63515; p2_pc3_s_sdz[0][0][7][1]=-0.509466; p3_pc3_s_sdz[0][0][7][1]=0.166335; p4_pc3_s_sdz[0][0][7][1]=-0.0179943; 
  p0_pc3_m_sdz[1][0][7][1]=6.2784; p1_pc3_m_sdz[1][0][7][1]=-25.3797; p2_pc3_m_sdz[1][0][7][1]=8.80107; p3_pc3_m_sdz[1][0][7][1]=69.9249; p4_pc3_m_sdz[1][0][7][1]=-71.3209; 
  p0_pc3_s_sdz[1][0][7][1]=-10.7079; p1_pc3_s_sdz[1][0][7][1]=46.2933; p2_pc3_s_sdz[1][0][7][1]=-14.0767; p3_pc3_s_sdz[1][0][7][1]=-127.034; p4_pc3_s_sdz[1][0][7][1]=124.65; 
  p0_pc3_m_sdz[0][0][8][1]=-0.118325; p1_pc3_m_sdz[0][0][8][1]=0.136967; p2_pc3_m_sdz[0][0][8][1]=-0.0240539; p3_pc3_m_sdz[0][0][8][1]=-0.0117131; p4_pc3_m_sdz[0][0][8][1]=0.00286167; 
  p0_pc3_s_sdz[0][0][8][1]=0.781947; p1_pc3_s_sdz[0][0][8][1]=-0.0167728; p2_pc3_s_sdz[0][0][8][1]=0.0729786; p3_pc3_s_sdz[0][0][8][1]=-0.0448474; p4_pc3_s_sdz[0][0][8][1]=0.00832517; 
  p0_pc3_m_sdz[1][0][8][1]=6.11677; p1_pc3_m_sdz[1][0][8][1]=-23.6202; p2_pc3_m_sdz[1][0][8][1]=7.16028; p3_pc3_m_sdz[1][0][8][1]=63.3002; p4_pc3_m_sdz[1][0][8][1]=-62.7074; 
  p0_pc3_s_sdz[1][0][8][1]=-8.51402; p1_pc3_s_sdz[1][0][8][1]=36.4559; p2_pc3_s_sdz[1][0][8][1]=-10.1416; p3_pc3_s_sdz[1][0][8][1]=-98.6724; p4_pc3_s_sdz[1][0][8][1]=95.0642; 
  p0_pc3_m_sdz[0][0][9][1]=-0.25487; p1_pc3_m_sdz[0][0][9][1]=0.50949; p2_pc3_m_sdz[0][0][9][1]=-0.390668; p3_pc3_m_sdz[0][0][9][1]=0.132067; p4_pc3_m_sdz[0][0][9][1]=-0.0168795; 
  p0_pc3_s_sdz[0][0][9][1]=0.753398; p1_pc3_s_sdz[0][0][9][1]=0.0493835; p2_pc3_s_sdz[0][0][9][1]=-0.00284053; p3_pc3_s_sdz[0][0][9][1]=-0.00159277; p4_pc3_s_sdz[0][0][9][1]=-0.000634213; 
  p0_pc3_m_sdz[1][0][9][1]=0.119988; p1_pc3_m_sdz[1][0][9][1]=0.256529; p2_pc3_m_sdz[1][0][9][1]=0.253755; p3_pc3_m_sdz[1][0][9][1]=-0.220143; p4_pc3_m_sdz[1][0][9][1]=-1.80683; 
  p0_pc3_s_sdz[1][0][9][1]=-3.42349; p1_pc3_s_sdz[1][0][9][1]=15.1802; p2_pc3_s_sdz[1][0][9][1]=-2.23617; p3_pc3_s_sdz[1][0][9][1]=-41.7885; p4_pc3_s_sdz[1][0][9][1]=38.0845; 
  p0_pc3_m_sdz[0][1][0][1]=0.0100418; p1_pc3_m_sdz[0][1][0][1]=-0.0664636; p2_pc3_m_sdz[0][1][0][1]=0.0436445; p3_pc3_m_sdz[0][1][0][1]=-0.0118936; p4_pc3_m_sdz[0][1][0][1]=0.00188985; 
  p0_pc3_s_sdz[0][1][0][1]=0.483192; p1_pc3_s_sdz[0][1][0][1]=0.572256; p2_pc3_s_sdz[0][1][0][1]=-0.380112; p3_pc3_s_sdz[0][1][0][1]=0.114283; p4_pc3_s_sdz[0][1][0][1]=-0.0130688; 
  p0_pc3_m_sdz[1][1][0][1]=-6.73218; p1_pc3_m_sdz[1][1][0][1]=25.0213; p2_pc3_m_sdz[1][1][0][1]=-6.46762; p3_pc3_m_sdz[1][1][0][1]=-68.4999; p4_pc3_m_sdz[1][1][0][1]=67.1513; 
  p0_pc3_s_sdz[1][1][0][1]=-2.38159; p1_pc3_s_sdz[1][1][0][1]=10.7562; p2_pc3_s_sdz[1][1][0][1]=-1.49827; p3_pc3_s_sdz[1][1][0][1]=-26.145; p4_pc3_s_sdz[1][1][0][1]=22.2026; 
  p0_pc3_m_sdz[0][1][1][1]=-0.0325476; p1_pc3_m_sdz[0][1][1][1]=0.075176; p2_pc3_m_sdz[0][1][1][1]=-0.113907; p3_pc3_m_sdz[0][1][1][1]=0.0577685; p4_pc3_m_sdz[0][1][1][1]=-0.00903219; 
  p0_pc3_s_sdz[0][1][1][1]=0.445563; p1_pc3_s_sdz[0][1][1][1]=0.802125; p2_pc3_s_sdz[0][1][1][1]=-0.625961; p3_pc3_s_sdz[0][1][1][1]=0.207575; p4_pc3_s_sdz[0][1][1][1]=-0.024314; 
  p0_pc3_m_sdz[1][1][1][1]=-3.02757; p1_pc3_m_sdz[1][1][1][1]=10.827; p2_pc3_m_sdz[1][1][1][1]=-3.00821; p3_pc3_m_sdz[1][1][1][1]=-30.2297; p4_pc3_m_sdz[1][1][1][1]=31.0742; 
  p0_pc3_s_sdz[1][1][1][1]=-4.0743; p1_pc3_s_sdz[1][1][1][1]=17.9441; p2_pc3_s_sdz[1][1][1][1]=-3.37055; p3_pc3_s_sdz[1][1][1][1]=-49.7329; p4_pc3_s_sdz[1][1][1][1]=46.1778; 
  p0_pc3_m_sdz[0][1][2][1]=-0.122678; p1_pc3_m_sdz[0][1][2][1]=0.286424; p2_pc3_m_sdz[0][1][2][1]=-0.255517; p3_pc3_m_sdz[0][1][2][1]=0.0921449; p4_pc3_m_sdz[0][1][2][1]=-0.0114344; 
  p0_pc3_s_sdz[0][1][2][1]=0.314825; p1_pc3_s_sdz[0][1][2][1]=1.13261; p2_pc3_s_sdz[0][1][2][1]=-0.904156; p3_pc3_s_sdz[0][1][2][1]=0.301746; p4_pc3_s_sdz[0][1][2][1]=-0.035345; 
  p0_pc3_m_sdz[1][1][2][1]=-0.495082; p1_pc3_m_sdz[1][1][2][1]=0.292353; p2_pc3_m_sdz[1][1][2][1]=1.09095; p3_pc3_m_sdz[1][1][2][1]=0.904357; p4_pc3_m_sdz[1][1][2][1]=-2.37958; 
  p0_pc3_s_sdz[1][1][2][1]=0.717344; p1_pc3_s_sdz[1][1][2][1]=0.151386; p2_pc3_s_sdz[1][1][2][1]=-0.477455; p3_pc3_s_sdz[1][1][2][1]=-0.435114; p4_pc3_s_sdz[1][1][2][1]=1.87451; 
  p0_pc3_m_sdz[0][1][3][1]=-0.115536; p1_pc3_m_sdz[0][1][3][1]=0.293312; p2_pc3_m_sdz[0][1][3][1]=-0.261832; p3_pc3_m_sdz[0][1][3][1]=0.0948558; p4_pc3_m_sdz[0][1][3][1]=-0.0118993; 
  p0_pc3_s_sdz[0][1][3][1]=0.53107; p1_pc3_s_sdz[0][1][3][1]=0.56229; p2_pc3_s_sdz[0][1][3][1]=-0.440208; p3_pc3_s_sdz[0][1][3][1]=0.154938; p4_pc3_s_sdz[0][1][3][1]=-0.019709; 
  p0_pc3_m_sdz[1][1][3][1]=-0.553426; p1_pc3_m_sdz[1][1][3][1]=0.265231; p2_pc3_m_sdz[1][1][3][1]=1.0975; p3_pc3_m_sdz[1][1][3][1]=0.959962; p4_pc3_m_sdz[1][1][3][1]=-2.15696; 
  p0_pc3_s_sdz[1][1][3][1]=-4.80223; p1_pc3_s_sdz[1][1][3][1]=21.7157; p2_pc3_s_sdz[1][1][3][1]=-6.16792; p3_pc3_s_sdz[1][1][3][1]=-57.305; p4_pc3_s_sdz[1][1][3][1]=54.7617; 
  p0_pc3_m_sdz[0][1][4][1]=0.0626517; p1_pc3_m_sdz[0][1][4][1]=-0.0974955; p2_pc3_m_sdz[0][1][4][1]=0.061471; p3_pc3_m_sdz[0][1][4][1]=-0.0195323; p4_pc3_m_sdz[0][1][4][1]=0.00271643; 
  p0_pc3_s_sdz[0][1][4][1]=0.390982; p1_pc3_s_sdz[0][1][4][1]=0.874962; p2_pc3_s_sdz[0][1][4][1]=-0.678053; p3_pc3_s_sdz[0][1][4][1]=0.223923; p4_pc3_s_sdz[0][1][4][1]=-0.0261569; 
  p0_pc3_m_sdz[1][1][4][1]=8.45769; p1_pc3_m_sdz[1][1][4][1]=-31.0923; p2_pc3_m_sdz[1][1][4][1]=5.39482; p3_pc3_m_sdz[1][1][4][1]=84.6709; p4_pc3_m_sdz[1][1][4][1]=-76.8372; 
  p0_pc3_s_sdz[1][1][4][1]=4.41801; p1_pc3_s_sdz[1][1][4][1]=-16.0393; p2_pc3_s_sdz[1][1][4][1]=7.61858; p3_pc3_s_sdz[1][1][4][1]=42.964; p4_pc3_s_sdz[1][1][4][1]=-46.3455; 
  p0_pc3_m_sdz[0][1][5][1]=0.0886275; p1_pc3_m_sdz[0][1][5][1]=-0.179955; p2_pc3_m_sdz[0][1][5][1]=0.176173; p3_pc3_m_sdz[0][1][5][1]=-0.074118; p4_pc3_m_sdz[0][1][5][1]=0.0111749; 
  p0_pc3_s_sdz[0][1][5][1]=0.653128; p1_pc3_s_sdz[0][1][5][1]=0.375889; p2_pc3_s_sdz[0][1][5][1]=-0.371628; p3_pc3_s_sdz[0][1][5][1]=0.154415; p4_pc3_s_sdz[0][1][5][1]=-0.0214742; 
  p0_pc3_m_sdz[1][1][5][1]=-0.0532166; p1_pc3_m_sdz[1][1][5][1]=0.274968; p2_pc3_m_sdz[1][1][5][1]=0.500349; p3_pc3_m_sdz[1][1][5][1]=0.0515797; p4_pc3_m_sdz[1][1][5][1]=-2.28084; 
  p0_pc3_s_sdz[1][1][5][1]=-1.32061; p1_pc3_s_sdz[1][1][5][1]=11.4745; p2_pc3_s_sdz[1][1][5][1]=-6.91898; p3_pc3_s_sdz[1][1][5][1]=-37.7947; p4_pc3_s_sdz[1][1][5][1]=44.739; 
  p0_pc3_m_sdz[0][1][6][1]=-0.155654; p1_pc3_m_sdz[0][1][6][1]=0.466782; p2_pc3_m_sdz[0][1][6][1]=-0.461605; p3_pc3_m_sdz[0][1][6][1]=0.189779; p4_pc3_m_sdz[0][1][6][1]=-0.0278824; 
  p0_pc3_s_sdz[0][1][6][1]=0.850926; p1_pc3_s_sdz[0][1][6][1]=-0.177579; p2_pc3_s_sdz[0][1][6][1]=0.169316; p3_pc3_s_sdz[0][1][6][1]=-0.0612845; p4_pc3_s_sdz[0][1][6][1]=0.00831365; 
  p0_pc3_m_sdz[1][1][6][1]=3.37381; p1_pc3_m_sdz[1][1][6][1]=-13.6414; p2_pc3_m_sdz[1][1][6][1]=4.14824; p3_pc3_m_sdz[1][1][6][1]=36.4554; p4_pc3_m_sdz[1][1][6][1]=-35.2331; 
  p0_pc3_s_sdz[1][1][6][1]=3.09235; p1_pc3_s_sdz[1][1][6][1]=-9.7566; p2_pc3_s_sdz[1][1][6][1]=3.70493; p3_pc3_s_sdz[1][1][6][1]=28.5768; p4_pc3_s_sdz[1][1][6][1]=-30.3386; 
  p0_pc3_m_sdz[0][1][7][1]=0.0925291; p1_pc3_m_sdz[0][1][7][1]=-0.288691; p2_pc3_m_sdz[0][1][7][1]=0.311065; p3_pc3_m_sdz[0][1][7][1]=-0.132618; p4_pc3_m_sdz[0][1][7][1]=0.0196989; 
  p0_pc3_s_sdz[0][1][7][1]=0.86148; p1_pc3_s_sdz[0][1][7][1]=-0.185361; p2_pc3_s_sdz[0][1][7][1]=0.181958; p3_pc3_s_sdz[0][1][7][1]=-0.0713354; p4_pc3_s_sdz[0][1][7][1]=0.0101769; 
  p0_pc3_m_sdz[1][1][7][1]=6.16699; p1_pc3_m_sdz[1][1][7][1]=-24.2203; p2_pc3_m_sdz[1][1][7][1]=8.17145; p3_pc3_m_sdz[1][1][7][1]=65.1257; p4_pc3_m_sdz[1][1][7][1]=-65.8425; 
  p0_pc3_s_sdz[1][1][7][1]=-0.590418; p1_pc3_s_sdz[1][1][7][1]=5.11968; p2_pc3_s_sdz[1][1][7][1]=-2.07422; p3_pc3_s_sdz[1][1][7][1]=-9.30949; p4_pc3_s_sdz[1][1][7][1]=8.17011; 
  p0_pc3_m_sdz[0][1][8][1]=-0.0608463; p1_pc3_m_sdz[0][1][8][1]=0.159321; p2_pc3_m_sdz[0][1][8][1]=-0.144156; p3_pc3_m_sdz[0][1][8][1]=0.0558238; p4_pc3_m_sdz[0][1][8][1]=-0.00788336; 
  p0_pc3_s_sdz[0][1][8][1]=0.473265; p1_pc3_s_sdz[0][1][8][1]=0.678904; p2_pc3_s_sdz[0][1][8][1]=-0.517771; p3_pc3_s_sdz[0][1][8][1]=0.170401; p4_pc3_s_sdz[0][1][8][1]=-0.0197276; 
  p0_pc3_m_sdz[1][1][8][1]=5.72714; p1_pc3_m_sdz[1][1][8][1]=-21.0232; p2_pc3_m_sdz[1][1][8][1]=4.63349; p3_pc3_m_sdz[1][1][8][1]=55.791; p4_pc3_m_sdz[1][1][8][1]=-52.3544; 
  p0_pc3_s_sdz[1][1][8][1]=-7.16126; p1_pc3_s_sdz[1][1][8][1]=31.0059; p2_pc3_s_sdz[1][1][8][1]=-7.13843; p3_pc3_s_sdz[1][1][8][1]=-86.6495; p4_pc3_s_sdz[1][1][8][1]=81.7367; 
  p0_pc3_m_sdz[0][1][9][1]=-0.18795; p1_pc3_m_sdz[0][1][9][1]=0.40431; p2_pc3_m_sdz[0][1][9][1]=-0.322031; p3_pc3_m_sdz[0][1][9][1]=0.106648; p4_pc3_m_sdz[0][1][9][1]=-0.012813; 
  p0_pc3_s_sdz[0][1][9][1]=0.888827; p1_pc3_s_sdz[0][1][9][1]=-0.144435; p2_pc3_s_sdz[0][1][9][1]=0.0472066; p3_pc3_s_sdz[0][1][9][1]=0.00887037; p4_pc3_s_sdz[0][1][9][1]=-0.00397292; 
  p0_pc3_m_sdz[1][1][9][1]=11.8522; p1_pc3_m_sdz[1][1][9][1]=-43.0281; p2_pc3_m_sdz[1][1][9][1]=8.57487; p3_pc3_m_sdz[1][1][9][1]=114.849; p4_pc3_m_sdz[1][1][9][1]=-106.982; 
  p0_pc3_s_sdz[1][1][9][1]=-6.43329; p1_pc3_s_sdz[1][1][9][1]=27.8647; p2_pc3_s_sdz[1][1][9][1]=-8.48524; p3_pc3_s_sdz[1][1][9][1]=-71.141; p4_pc3_s_sdz[1][1][9][1]=68.6943; 
  
  return 1;
}
//_____________________________________________________________________________________________________________________________
