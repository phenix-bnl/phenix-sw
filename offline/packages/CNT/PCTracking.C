#include "PCTracking.h"
#include "EmcClusterLocalExt.h"
#include "PadCluster.h"
#include "PHCentralTrack.h"
#include "CrkHit.h" 
#include "CrkPID.hh"
#include "dCrkHitWrapper.h" //CrkPID uses this structure as input */
//INCLUDECHECKER: Removed this line: #include "PHLine.h" 
#include "PHGeometry.h"

using namespace std;

CrkPID *d_crkpid = 0;
dCrkHitWrapper *d_crkhitw = 0;

void find_pctrk(PadCluster *pc1,
		PadCluster *pc2,
		EmcClusterLocalExt *emc,
		vector<pctrk> &vpctrk)
{
  int npc1 = pc1->get_PadNCluster();
  int npc2 = pc2->get_PadNCluster();
  int nemc = emc->get_EmcNCluster();

  PHPoint pc1xyz, emcxyz, pc2xyz;
  float dphi, dz;

  for(int ipc1=0;ipc1<npc1;ipc1++) {
    if(pc1->get_xyz(ipc1,0)>0) {//in West Arm
      PHPoint pc1xyz(pc1->get_xyz(ipc1,0),
		     pc1->get_xyz(ipc1,1),
		     pc1->get_xyz(ipc1,2));
      for(int iemc=0;iemc<nemc;iemc++) {
	if(emc->get_xyz(iemc,0)>0 &&
	   emc->get_ecore(iemc)>0.15) {//in West Arm
	  PHPoint emcxyz(emc->get_xyz(iemc,0),
			 emc->get_xyz(iemc,1),
			 emc->get_xyz(iemc,2));
	  PHLine pc1emc(pc1xyz,emcxyz);
	  for(int ipc2=0;ipc2<npc2;ipc2++) {
	    PHPoint pc2xyz(pc2->get_xyz(ipc2,0),
			   pc2->get_xyz(ipc2,1),
			   pc2->get_xyz(ipc2,2));
	    PHPoint pc2proj = PHGeometry::closestApproachLinePoint(pc1emc,pc2xyz);
	    dz = pc2xyz.getZ() - pc2proj.getZ();
	    float dx = pc2xyz.getX() - pc2proj.getX();
	    float dy = pc2xyz.getY() - pc2proj.getY();
	    if(fabs(dz)<10 && fabs(dx)<10 && fabs(dy)<10) {
	      float phi      = atan2(pc2xyz.getY(),pc2xyz.getX());
	      float phi_proj = atan2(pc2proj.getY(),pc2proj.getX());
	      dphi = phi - phi_proj;
	      //	    if(fabs(dz)<8 && fabs(dphi)<0.015) {
	      if(fabs(dz)<5 && fabs(dphi)<0.012) {
		PHLine pc1pc2(pc1xyz,pc2xyz); //this is better to measure alpha
		PHVector dir = pc1pc2.getDirection();
		float pc1phi = atan2(pc1xyz.getY(),pc1xyz.getX());
		float alpha  = pc1phi - atan2(dir.getY(),dir.getX());
		if(fabs(alpha)<0.25&&fabs(alpha)>0.01) {
		  vpctrk.push_back(pctrk(dphi,dz,pc1phi,alpha,pc1xyz,pc2xyz,emcxyz,emc,iemc));
		}
	      }
	    }
	  }
	}
      }
    }
  }
}

void remove_duplicate(vector<pctrk> &vpctrk_in, PHCentralTrack *particle, vector<pctrk> &vpctrk_out)
{
  //search for the "same" track in particle and in vpctrk
  //
  int npctrk =vpctrk_in.size();
  int npart = particle->get_npart();

  //STEP1: removed ghost tracks in PC-EMC tracking.
  //This is caused by the following situation:
  //A single track somehow causes two near-by clusters in PC1 or EMC, or there is a background
  //hit in PC1 or EMC that is close to real PC-EMC track. In this situation, we have (A) 2 near-by hit
  //in PC and single hit in EMC, and one of the PC hit is a backgournd, or (B) 1 PC hit and 2 near-by
  //EMC clusters, one of which is a backgound. In these situation, we have two tracks reconstructed, but
  //only one of them is REAL and the other one is a ghost caused by the real track.
  //In the following code, I remove such ghost tracks in vpctrk_in. (Mark one of them as "ipart = -1".
  if(npctrk>=2) {
    for(int ipctrk=0;ipctrk<npctrk;ipctrk++)
      for(int jpctrk=ipctrk+1;jpctrk<npctrk;jpctrk++) {
	if((!vpctrk_in[ipctrk].ghost)&&(!vpctrk_in[jpctrk].ghost)) {
	  float pc1dphi = vpctrk_in[ipctrk].pc1phi - vpctrk_in[jpctrk].pc1phi;
	  float pc1dz   = vpctrk_in[ipctrk].pc1xyz.getZ()
	    - vpctrk_in[jpctrk].pc1xyz.getZ();
	  float emcdphi = atan2(vpctrk_in[ipctrk].emcxyz.getY(),
				vpctrk_in[ipctrk].emcxyz.getX())
	                - atan2(vpctrk_in[jpctrk].emcxyz.getY(),
				vpctrk_in[jpctrk].emcxyz.getX());
	  float emcdz   = vpctrk_in[ipctrk].emcxyz.getZ()
	                - vpctrk_in[jpctrk].emcxyz.getZ();
	  if((fabs(pc1dphi)<0.07&&fabs(pc1dz)<20&&fabs(emcdphi)<0.001&&fabs(emcdz)<0.1)||
	     (fabs(pc1dphi)<0.001&&fabs(pc1dz)<0.1&&fabs(emcdphi)<0.04&&fabs(emcdz)<20)){
	    //Chose one of the track as "real" and mark the other as "ghost"
	    //Use dphi and dz (note...they are dphi and dz at PC2 relative to the straight
	    //line connecting PC1 and EMC) to judge which one is real.
	    static const float sigma_dphi=0.0045;
	    static const float sigma_dz  =2.1;
	    float sphi_i = vpctrk_in[ipctrk].dphi/sigma_dphi;
	    float sz_i   = vpctrk_in[ipctrk].dz/sigma_dz;
	    float sphi_j = vpctrk_in[jpctrk].dphi/sigma_dphi;
	    float sz_j   = vpctrk_in[jpctrk].dz/sigma_dz;
	    if(sphi_i*sphi_i + sz_i*sz_i > sphi_j*sphi_j + sz_j*sz_j) {
	      vpctrk_in[ipctrk].ghost = true;
	    } else {
	      vpctrk_in[jpctrk].ghost = true;
	    }
	  }
	}
      }
  }


  for(int ipctrk=0;ipctrk<npctrk;ipctrk++) {
    if(!vpctrk_in[ipctrk].ghost) {
      float pc1phi = vpctrk_in[ipctrk].pc1phi;
      float pc1z   = vpctrk_in[ipctrk].pc1xyz.getZ();
      float emcx = vpctrk_in[ipctrk].emcxyz.getX();
      float emcy = vpctrk_in[ipctrk].emcxyz.getY();
      float emcz = vpctrk_in[ipctrk].emcxyz.getZ();
      float emcphi = atan2(emcy,emcx);
      
      for(int ipart=0;ipart<npart;ipart++) {
	if(particle->get_pemcx(ipart)>0&&
	   fabs(particle->get_ecore(ipart))<5.0){//associated to West EMC
	  float ppc1x = particle->get_ppc1x(ipart);
	  float ppc1y = particle->get_ppc1y(ipart);
	  float ppc1z = particle->get_ppc1z(ipart);
	  float pemcx = particle->get_pemcx(ipart);
	  float pemcy = particle->get_pemcy(ipart);
	  float pemcz = particle->get_pemcz(ipart);

	  float pc1dphi = pc1phi - atan2(ppc1y,ppc1x);
	  float pc1dz   = pc1z   - ppc1z;
	  float emcdphi = emcphi - atan2(pemcy,pemcx);
	  float emcdz   = emcz   - pemcz;

	  //If PC track and DC track shares the same PC1 cluster, they are
	  //for most case the same track.
	  //I applies a cut only in EMC phi position, since
	  //DC z reconstruction seems to be wrong sometimes.
	  //
	  if(fabs(pc1dphi)<0.005&&fabs(pc1dz)<1&&
	     fabs(emcdphi)<0.05) {
	    vpctrk_in[ipctrk].ipart   = ipart;
	    vpctrk_in[ipctrk].pc1dphi = pc1dphi;
	    vpctrk_in[ipctrk].pc1dz   = pc1dz;
	    vpctrk_in[ipctrk].emcdphi = emcdphi;
	    vpctrk_in[ipctrk].emcdz   = emcdz;
	  }
	}
      }
    }
  }
  vpctrk_out=vpctrk_in; //at this point, no filter.
}

void fill_particle(PHCentralTrack *particle, int itrk, pctrk &pc_trk)
{
  float Rref = 220.;

  float pc1x = pc_trk.pc1xyz.getX();
  float pc1y = pc_trk.pc1xyz.getY();
  float pc1z = pc_trk.pc1xyz.getZ();
  float pc1r = sqrt(pc1x*pc1x+pc1y*pc1y);
  float pc2x = pc_trk.pc2xyz.getX();
  float pc2y = pc_trk.pc2xyz.getY();
  float pc2z = pc_trk.pc2xyz.getZ();
  float pc2r = sqrt(pc2x*pc2x+pc2y*pc2y);
  float emcx = pc_trk.emcxyz.getX();
  float emcy = pc_trk.emcxyz.getY();
  float emcz = pc_trk.emcxyz.getZ();
  float emcr = sqrt(emcx*emcx+emcy*emcy);

  // Note that alpha, z, etc are from PC1 position 
  // Translate alpha/beta to momentum value.
  // comparison with DC calculated mom shows that
  // the resolution of (mom(PC)/mom(DC)) is 6%
  // at 1 GeV/c

  float dz_dr = (emcz - pc1z)/(emcr - pc1r);
  float zed   = pc1z + dz_dr*(Rref - pc1r);
  float alpha = pc_trk.alpha;
  float phi   = pc_trk.pc1phi;
  float beta  = atan2(pc2r-pc1r,pc2z-pc1z);
  float the0  = 1.025*beta-0.0365;
  float phi0  = phi+1.804*alpha;
  float beta0 = beta - 1.571;
  float mom   = (0.085*(1.0+0.24*alpha*alpha)/fabs(alpha))/(1-0.574*beta0*beta0);
  int charge = 1;
  if(alpha>0) charge = -1;
  float px = mom*sin(the0)*cos(phi0);
  float py = mom*sin(the0)*sin(phi0);
  float pz = mom*cos(the0);
 
  particle->set_px(itrk,px);
  particle->set_py(itrk,py);
  particle->set_pz(itrk,pz);
  particle->set_charge(itrk,charge);

  //quality code has the following meaning
  // 128 == found in PC-EMC, but not found in DC
  // 129 == found in both PC-EMC and DC, and
  //        DC is associated with the same EMC as PCtrack
  // 131 == found in both PC-EMC and DC, but
  //        DC is not associated with the same EMC as PCtrack
  // 132 == ghost (in PC tracking)
  // Here we set it to default(128) or ghost(132).
  // DC overlapping(129) is handled later.
  particle->set_quality(itrk,128);
  if(pc_trk.ghost) particle->set_quality(itrk,132);

  particle->set_zed(itrk,zed);
  particle->set_phi(itrk,phi);        //At PC1
  particle->set_alpha(itrk,alpha);
  particle->set_beta(itrk,beta);
  particle->set_mom(itrk,mom);
  particle->set_the0(itrk,the0);
  particle->set_phi0(itrk,phi0);
  particle->set_status(itrk,-999);
  particle->set_alpha1(itrk,-999);
  particle->set_alpha2(itrk,-999);
  particle->set_nx1hits(itrk,-999);
  particle->set_nx2hits(itrk,-999);
  particle->set_nx1x2fit(itrk,-999);
  particle->set_alphaf(itrk,-999);
  particle->set_ppc1x(itrk,pc1x);
  particle->set_ppc1y(itrk,pc1y);
  particle->set_ppc1z(itrk,pc1z);
  particle->set_ppc2x(itrk,pc2x);
  particle->set_ppc2y(itrk,pc2y);
  particle->set_ppc2z(itrk,pc2z);
  particle->set_ptecx(itrk,-999);
  particle->set_ptecy(itrk,-999);
  particle->set_ptecz(itrk,-999);
  particle->set_ppc3x(itrk,-999);//temp
  particle->set_ppc3y(itrk,-999);//temp
  particle->set_ppc3z(itrk,-999);//temp
  particle->set_pemcx(itrk,emcx);
  particle->set_pemcy(itrk,emcy);
  particle->set_pemcz(itrk,emcz);
  particle->set_ptofx(itrk,-999);
  particle->set_ptofy(itrk,-999);
  particle->set_ptofz(itrk,-999);
  particle->set_pltof(itrk,-999);
  particle->set_plemc(itrk,-999);// I have to calculate this later...

  // EMC variables
  EmcClusterLocalExt *emc=pc_trk.emc;
  int iemc = pc_trk.iemc;
  particle->set_sect(itrk,emc->get_sector(iemc));
  int emcindex = emc->get_index(iemc);
  particle->set_ysect(itrk,(emcindex/100)%100);
  particle->set_zsect(itrk,(emcindex%100));
  particle->set_ecorr(itrk,emc->get_ecorr(iemc));
  particle->set_ecore(itrk,emc->get_ecore(iemc));
  particle->set_temc(itrk,emc->get_tofcorr(iemc));
  particle->set_prob(itrk,emc->get_prob_photon(iemc));
  particle->set_ecent(itrk,emc->get_ecent(iemc));
  particle->set_twrhit(itrk,emc->get_twrhit(iemc));
  particle->set_e9(itrk,emc->get_e9(iemc));
  particle->set_re9(itrk,emc->get_re9(iemc));
  particle->set_emcchi2(itrk,emc->get_chi2(iemc));
  particle->set_deadmap(itrk,emc->get_deadmap(iemc));
  particle->set_warnmap(itrk,emc->get_warnmap(iemc));
  particle->set_emce(itrk,emc->get_e(iemc));

  //Swap variables of EMC --- I set as -999 for a moment.
  particle->set_secorr(itrk,-999);
  particle->set_secore(itrk,-999);
  particle->set_stemc(itrk,-999);
  particle->set_sprob(itrk,-999);
  particle->set_stwrhit(itrk,-999);
  particle->set_emcchi2(itrk,-999);

  //All TOF variables should be -999 (East Arm!)
  particle->set_slat(itrk,-999);
  particle->set_ttof(itrk,-999);
  particle->set_etof(itrk,-999);
  particle->set_sttof(itrk,-999);
  particle->set_setof(itrk,-999);

  //RICH varialbes
  particle->set_n0(itrk,pc_trk.n0);
  particle->set_npe0(itrk,pc_trk.npe0);
  particle->set_n1(itrk,-999);
  particle->set_npe1(itrk,-999);
  particle->set_chi2(itrk,pc_trk.chi2);
  particle->set_disp(itrk,-999);
  particle->set_tcrk(itrk,-999);
  particle->set_cross_phi(itrk,-999);
  particle->set_cross_z(itrk,-999);
  particle->set_center_phi(itrk,-999);
  particle->set_center_z(itrk,-999);
  particle->set_sn0(itrk,-999);  
  particle->set_snpe0(itrk,-999);  
  particle->set_sn1(itrk,-999);  
  particle->set_schi2(itrk,-999);  
  particle->set_sdisp(itrk,-999);  
  particle->set_stcrk(itrk,-999);  

  // All TEC variables are of course -999 (East arm!)
  particle->set_tecdedx1(itrk,-999);
  particle->set_tecdedx2(itrk,-999);

  // derivation from projection points
  // Since PC3 and EMC are *USED* as end points of the PC
  // tracking, they are by definition set to ZERO
  particle->set_pc2sdphi(itrk,pc_trk.dphi);
  particle->set_pc2sdz(itrk,pc_trk.dz);
  particle->set_pc3sdphi(itrk,0.0);
  particle->set_pc3sdz(itrk,0.0);
  particle->set_emcsdphi(itrk,0.0);
  particle->set_emcsdz(itrk,0.0);
  particle->set_tofsdphi(itrk,-999);
  particle->set_tofsdz(itrk,-999);
  //
  particle->set_emcsdphi_e(itrk,0.0);
  particle->set_emcsdz_e(itrk,0.0);

  // if it is also found by DC
  int ipart = pc_trk.ipart;
  if(ipart>=0) { //store DC track variables in un-used fields...
    // If associated DC tracks are found, the quality is changed to 129 or 131
    // 129 means that the DC track and PC track shares the same EMC.
    // 131 means that the DC track points to a different EMC cluster or none.
    // Note that the PCtrack-DC track association cuts in (PC1 position) and
    // EMC phi position is farely tight. Threfore, quality 131 means (in my
    // opinion) that the associated DC *FAILED* to find a correct EMC cluster
    // due to a very poor Z resolution in pp collision.
    //
    // Also, the associated DC track's particle number is stored in status.
    //
    particle->set_quality(itrk,129);
    if(particle->get_emcid(ipart) != pc_trk.iemc) particle->set_quality(itrk,131);
    particle->set_status(itrk,ipart);
    particle->set_pc3sdphi(itrk,pc_trk.pc1dphi); //temporary use
    particle->set_pc3sdz(itrk,pc_trk.pc1dz);     //temporary use
    particle->set_emcsdphi(itrk,pc_trk.emcdphi);
    particle->set_emcsdz(itrk,pc_trk.emcdz);
    particle->set_alphaf(itrk,particle->get_alpha(ipart));
    particle->set_alpha1(itrk,particle->get_the0(ipart));
    particle->set_alpha2(itrk,particle->get_phi0(ipart));
    particle->set_ptecx(itrk,particle->get_beta(ipart));
    particle->set_ptecy(itrk,particle->get_mom(ipart));
    particle->set_ptecz(itrk,particle->get_zed(ipart));
    particle->set_ptofx(itrk,particle->get_phi(ipart));
  }
}

void initialize_crkpid(int run) {
  d_crkpid = new CrkPID(run);
  d_crkhitw = new dCrkHitWrapper("dCrkHit",2000);
  d_crkpid->SetCrkHit(d_crkhitw);
}

void associate_to_RICH(vector<pctrk>&vpctrk, CrkHit *d_crk) {
  int ntrk = vpctrk.size();
  if(ntrk >0) {
    int ncrkhit = d_crk->get_CrkNHit();
    //    cout << "ncrkhit="<<ncrkhit<<endl;
    if(ncrkhit<=0) return;

    //Load PMT hits information into dcrkhitw
    dCrkHit *hit = d_crkhitw->TableData();
    for(int i=0;i<ncrkhit;i++) {
      hit[i].pmt  = d_crk->get_pmt(i);
      hit[i].npe  = d_crk->get_npe(i);
      hit[i].time = d_crk->get_time(i);
    }
    d_crkhitw->SetRowCount(ncrkhit);

    //Since d_crkpid has already been linked to d_crkhitw,
    //it is ready now. 

    CrkPIDout ring;
    for(vector<pctrk>::iterator it=vpctrk.begin();it!=vpctrk.end();++it) {
      d_crkpid->AssociateTrack(PHLine(it->pc1xyz,it->pc2xyz),&ring);
      it->n0 = ring.npmt0;
      it->npe0 = ring.npe0;
      it->chi2 = ring.chi2;
    }
  }
}
