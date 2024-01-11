#include <iostream>
#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TSystem.h>
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TFile.h"
#include "TGraphErrors.h"
#include "TPaveText.h"
#include "utiMatch.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TTree.h"

#include <BbcOut.h>
#include "BbcRaw.h"
#include "CglTrack.h"  
#include "PHTrackOut.h"
#include "DchTrack.h"
#include "EmcClusterLocalExt.h"
#include "EventHeader.h"
#include "RunHeader.h"
#include "PadCluster.h"
#include "TrigLvl1.h"
#include "VtxOut.h"
#include "ZdcOut.h"

#include "CrkRing.h"
#include "CrkHit.h"

#include "Bbc.hh"
#include "Zdc.hh"
#include "PHIODataNode.h"
#include "PHCompositeNode.h"
#include "PHNodeIOManager.h"
#include "PHNodeReset.h"
#include "PHGlobal.h"
#include "PHTrig.h"
#include "TrigLvl1.h"
#include "Lvl2DecisionOut.h"
#include "PHCentralTrack.h"
#include "utiMatch.h"
#include "utiReactionPlane.h"
#include "Simplicity.h"

void Simplicity(const char *microdstfile,int seg)
{
  
  
  //gSystem->Load("libmicroDST");
  //gSystem->Load("/phenix/workarea/sleckey/AllCode/fresh/install/lib/libuti.so"); 
  utiMatch *m=new utiMatch;
  //cout << m->d_PC2_z_match(3,7) << endl;  

  ////////////////////////////


  int ndet=4;   // number of subsamples interested in BBCN, BBCS, and BBC and CNT total
  //idet 1=bn,bs,bb,cnt
   
  int nhar=2;    //ihar=0  is directed , ihar=2 --> elliptic 
  int nmul=20;     // centrality binning 
  int nzps=5; // z-vertex binning
  short ADCarray[128];
  short TDCarray[128];

  // Microdst stuff
  PHNodeIOManager *dstFile;  
  PHIODataNode<PHObject> *BBCNode;
  PHIODataNode<PHObject> *ZDCNode;
  PHIODataNode<PHObject> *VTXNode;
  PHIODataNode<PHObject> *CGLNode;
  PHIODataNode<PHObject> *TRGNode;
  PHIODataNode<PHObject> *BBCRawNode;
  PHIODataNode<PHObject> *DCHNode;
  PHIODataNode<PHObject> *PC2Node;
  PHIODataNode<PHObject> *PC3Node;
  PHIODataNode<PHObject> *EMCNode;
  PHIODataNode<PHObject> *PROJNode;

  BbcOut             *bbcout   =0;
  ZdcOut             *zdcout   =0;
  VtxOut             *vtxout   =0;
  CglTrack           *cgltrack =0;
  TrigLvl1           *trigg    =0;
  BbcRaw             *bbcraw   =0;
  DchTrack           *dchtrack =0;
  PadCluster         *pc2      =0;
  PadCluster         *pc3      =0;
  EmcClusterLocalExt *emc      =0;
  PHTrackOut         *proj     =0;

  // Establish tree iterator
  //  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  //PHCompositeNode* dstNode = new PHCompositeNode("DST");
  //topNode->addNode(dstNode);
  //PHNodeIterator iter(topNode);
  

  //////////////////////////////////////////////////////////////
  // Get Run Number in rather silly way that works.
  //////////////////////////////////////////////////////////////
  
  TFile *myfile;
  myfile = new TFile(microdstfile);
  TTree *runtree = (TTree *) myfile->Get("T1");
  TBranch *br=runtree->GetBranch("RUN/RunHeader");
  RunHeader *runhead=0;
  br->SetAddress(&runhead);
  runtree->GetEntry(0);
  Int_t run= runhead->get_RunNumber();
  cout << "Run Number =" << run << endl; 
  //////////////////////////////////////////////////////////////////////////
  Char_t outputname[30];
  sprintf(outputname,"%i_%i.root",run,seg);

  TFile *OutputRootFile = new TFile(outputname,"RECREATE");
  TNtuple *Reac = new TNtuple("Reac","Reac","pt:phi:cent:PsiBBCN1:PsiBBCS1:PsiBBC1:PsiBBCN2:PsiBBCS2:PsiBBC2:PsiCNT1:PsiCNT2:track:event:segment:ntracks");


  //////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////
  int pass=1;    //
  
  utiReactionPlane *rp = new utiReactionPlane();
  rp->readGeoCal();
  rp->readCorre(run, nzps, nmul, nhar, ndet);
  rp->readOffst(run, nzps, nmul, nhar, ndet);
  rp->readTable(run, nzps, nmul, nhar,ndet);
  rp->makeHisto(nzps,nmul, nhar, ndet);
  rp->clrPlane(nhar,ndet);    
  

  int tevent=0;
  dstFile = new PHNodeIOManager(microdstfile, PHReadOnly);
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHNodeIterator iter(topNode);
      
  cout << pass << endl;
      
  while((dstFile->read(dstNode))){
    tevent++;
    if((tevent%1000)==0) cout<< "Process : "<<tevent<<" "<<" " /* << run*/ << " " <<  pass << endl;
        
    ////////////////////////////////////////////////////////////////
    // Get My Nodes
    ////////////////////////////////////////////////////////////////
    
    BBCNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","BbcOut");
    if(BBCNode) bbcout = (BbcOut *)(BBCNode->getData());

    ZDCNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","ZdcOut");
    if(ZDCNode) zdcout = (ZdcOut *)(ZDCNode->getData());


    VTXNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","VtxOut");
    if(VTXNode) vtxout = (VtxOut *)(VTXNode->getData());

    CGLNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","CglTrack");
    if(CGLNode) cgltrack = (CglTrack *)(CGLNode->getData());

    TRGNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","TrigLvl1");
    if(TRGNode) trigg = (TrigLvl1 *)(TRGNode->getData());

    BBCRawNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","BbcRaw");
    if(BBCRawNode) bbcraw = (BbcRaw *)(BBCRawNode->getData());

    DCHNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","DchTrack");
    if(DCHNode) dchtrack = (DchTrack *)(DCHNode->getData());


    PC2Node = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","Pc2Cluster");
    if(PC2Node) pc2 = (PadCluster *)(PC2Node->getData());

    PC3Node = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","Pc3Cluster");
    if(PC3Node) pc3 = (PadCluster *)(PC3Node->getData());

    
    EMCNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","EmcClusterLocalExt");
    if(EMCNode) emc = (EmcClusterLocalExt *)(DCHNode->getData());
    

    PROJNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","PHTrackOut");
    if(PROJNode) proj = (PHTrackOut *)(PROJNode->getData());

    
    ////////////////////////////////////////////////////////////////
    // Get Event styles variables
    ////////////////////////////////////////////////////////////////
    
    
    
    float charge = (bbcout->get_ChargeSum(Bbc::North)) + (bbcout->get_ChargeSum(Bbc::South));
    float bbczps = vtxout->get_BbcVertex();
    int   ntrack = cgltrack->get_CglNTrack();
    int   trigger = trigg->get_lvl1_trigraw();
    
    // Event Selection
    /*    if (fabs(bbczps)>50 || trigger>100000000) continue;
    if (charge>50.0+ntrack*7.0 || charge<-50.0+ntrack*2.0) continue;
    if (ntrack<5) continue;
    */
    for(short ipmt=0; ipmt<128; ipmt++){
      ADCarray[ipmt]=bbcraw->get_Pmt(ipmt); 
      TDCarray[ipmt]=bbcraw->get_Tdc0(ipmt); 
    }
    
    int imul = (int)(nmul*charge/1700.0);  //
    if (imul<0) imul=0;
    if (imul>nmul-1) imul=nmul-1;
    int izps = (int)(nzps*(bbczps+50.0)/100.0);
    if (izps<0) izps=0;
    if (izps>nzps-1) izps=nzps-1;
    
    //  izps and imul is the current event class
    
    rp->calBbcPlane(imul,izps,ADCarray,TDCarray,nhar,pass); // -2 -> first pass
    rp->clrPlane(nhar,ndet);
    
    for(int it=0; it<ntrack; it++){    // looping over tracks    
      
      float ptot=fabs(dchtrack->get_momentum(it));
      float phi=fabs(dchtrack->get_phi0(it));
      float theta=dchtrack->get_theta0(it);
      if (theta>-1000 && phi>-1000 && ptot<10) {
	//float ptra=ptot*sin(theta);
	float eta=-log(tan(theta/2.0));
	if (fabs(eta)>1.0) cout << "error wrong eta " 
				<< eta << " " << theta << " " <<  phi << endl;
	float wt=1;
	float pm=1;
	if (eta>0) pm=-1;
	rp->filHitPlane(0,  phi,wt,pm,nhar);
	float phj=dchtrack->get_zed(it)+80.0;
	int iphi=(int)(phj*20.0/160.0);
	int ibbc=iphi%4+1;
	rp->filHitPlane(ibbc,phi,wt,pm,nhar);
      }
    }
  
    rp->calPlane(imul,izps,ndet,nhar,pass);
    
    
    //////////////////////////////////////////
    //////////////////////////////////////////
    
    
    float bbce1 = bbcout->get_ChargeSum(Bbc::North);
    float bbce2 = bbcout->get_ChargeSum(Bbc::South);
    float zdce1 = zdcout->get_Energy(1); //North 
    float zdce2 = zdcout->get_Energy(0); //South
    
    
    //////////////////////////////
    // utiCentralty class doesn't work in macro
    /////////////////////////////
    
    
    
    float phiCut[94];
    
    float bbcMax = 1700;
    float zdcMax = 4500;
    float bbcCenter = 0.15;
    
    phiCut[0] = 1.99998;
    phiCut[1] = 1.29966;
    phiCut[2] = 1.2239;
    phiCut[3] = 1.15502;
    phiCut[4] = 1.09154;
    phiCut[5] = 1.03066;
    phiCut[6] = 0.97202;
    phiCut[7] = 0.91638;
    phiCut[8] = 0.86558;
    phiCut[9] = 0.81794;
    phiCut[10] = 0.77178;
    phiCut[11] = 0.7297;
    phiCut[12] = 0.68818;
    phiCut[13] = 0.64842;
    phiCut[14] = 0.61222;
    phiCut[15] = 0.57718;
    phiCut[16] = 0.54386;
    phiCut[17] = 0.51166;
    phiCut[18] = 0.48274;
    phiCut[19] = 0.45478;
    phiCut[20] = 0.42818;
    phiCut[21] = 0.40366;
    phiCut[22] = 0.38002;
    phiCut[23] = 0.35674;
    phiCut[24] = 0.33462;
    phiCut[25] = 0.31438;
    phiCut[26] = 0.29458;
    phiCut[27] = 0.27542;
    phiCut[28] = 0.2559;
    phiCut[29] = 0.23758;
    phiCut[30] = 0.2207;
    phiCut[31] = 0.20422;
    phiCut[32] = 0.1879;
    phiCut[33] = 0.1725;
    phiCut[34] = 0.15842;
    phiCut[35] = 0.14386;
    phiCut[36] = 0.13042;
    phiCut[37] = 0.1175;
    phiCut[38] = 0.10494;
    phiCut[39] = 0.09242;
    phiCut[40] = 0.08034;
    phiCut[41] = 0.06898;
    phiCut[42] = 0.05738;
    phiCut[43] = 0.04686;
    phiCut[44] = 0.03718;
    phiCut[45] = 0.0273;
    phiCut[46] = 0.01766;
    phiCut[47] = 0.00786;
    phiCut[48] = -0.00162;
    phiCut[49] = -0.01034;
    phiCut[50] = -0.01874;
    phiCut[51] = -0.02674;
    phiCut[52] = -0.03494;
    phiCut[53] = -0.04238;
    phiCut[54] = -0.04994;
    phiCut[55] = -0.0571;
    phiCut[56] = -0.0639;
    phiCut[57] = -0.0705;
    phiCut[58] = -0.07674;
    phiCut[59] = -0.08322;
    phiCut[60] = -0.08966;
    phiCut[61] = -0.09598;
    phiCut[62] = -0.10202;
    phiCut[63] = -0.10794;
    phiCut[64] = -0.11362;
    phiCut[65] = -0.11946;
    phiCut[66] = -0.12494;
    phiCut[67] = -0.13026;
    phiCut[68] = -0.13554;
    phiCut[69] = -0.14102;
    phiCut[70] = -0.14638;
    phiCut[71] = -0.1517;
    phiCut[72] = -0.15706;
    phiCut[73] = -0.16234;
    phiCut[74] = -0.16798;
    phiCut[75] = -0.17386;
    phiCut[76] = -0.17982;
    phiCut[77] = -0.1861;
    phiCut[78] = -0.19266;
    phiCut[79] = -0.19986;
    phiCut[80] = -0.20766;
    phiCut[81] = -0.21646;
    phiCut[82] = -0.22658;
    phiCut[83] = -0.23818;
    phiCut[84] = -0.2523;
    phiCut[85] = -0.2675;
    phiCut[86] = -0.28674;
    phiCut[87] = -0.3085;
    phiCut[88] = -0.33402;
    phiCut[89] = -0.36666;
    phiCut[90] = -0.41802;
    phiCut[91] = -0.52014;
    phiCut[92] = -0.85538;
    phiCut[93] = -1.4957;
    
    float bbce = bbce1 + bbce2;
    float zdce = zdce1 + zdce2;
    float BBC0 = bbcCenter * bbcMax;
    
    int cent = -1;
    
    if (zdce1 != 0 && zdce2 != 0 && bbce1 != 0 && bbce2 != 0)
      {
	float phi_bbczdc = atan2((bbce - BBC0) / bbcMax, zdce / zdcMax);
	for (int j = 0; j<93; j++)
	  {
	    if (phi_bbczdc > phiCut[j + 1] && phi_bbczdc < phiCut[j])
	      cent = j + 1;
	  }
      }
    
    
    
    Float_t PsiBBCN1=rp->getBbcPlane(0,0);
    Float_t PsiBBCS1=rp->getBbcPlane(1,0);
    Float_t PsiBBC1 =rp->getBbcPlane(2,0);
    Float_t PsiBBCN2=rp->getBbcPlane(0,1);
    Float_t PsiBBCS2=rp->getBbcPlane(1,1);
    Float_t PsiBBC2 =rp->getBbcPlane(2,1);
    Float_t PsiCNT1=rp->getHitPlane(0,0);
    Float_t PsiCNT2=rp->getHitPlane(0,1);
    

    //matching variables

    float ppc2y, phiM, phiP, zM, zP, rawphi, pc2sdphi, pc2sdz, ppc3y, rawz, pc3sdphi, pc3sdz, /*pemcy, beta,emcsdphi, emcsdz,*/naz,brzo,naphi,brphi;
    //  int arm,sect;
    float ptot;

    int ipc2;
    float ppc2x;
    int ipc3;
    float ppc3x;
    int iemc;
    float pemcx;
      

    for(int it=0; it<ntrack; it++){    // looping over tracks    
  
      //  cout << "it " << it << endl;
      ipc2=cgltrack->get_pc2clusid(it);
      ppc2x=proj->get_projectionPc2(it,0);
      ipc3=cgltrack->get_pc3clusid(it);
      ppc3x=proj->get_projectionPc3(it,0);
      iemc=cgltrack->get_emcclusid(it);
      pemcx=proj->get_projectionEmc(it,0);
      
      //cout << m->d_PC2_z_match(3,7) << endl;  
      if (((ipc2>=0&& ppc2x>-999)/*||(iemc>=0&& pemcx>-999)*/)&&(ipc3>=0&& ppc3x>-999)) {
	
	ptot=fabs(dchtrack->get_momentum(it));
	
	///////////
	// matching
	/////////////
	//if (ipc2>=0&& ppc2x>-999) {
	  ppc2y=proj->get_projectionPc2(it,1);
	  phiM=atan2(pc2->get_xyz(ipc2,1),pc2->get_xyz(ipc2,0));
	  phiP=atan2(ppc2y,ppc2x);
	  zM=pc2->get_xyz(ipc2,2);
	  zP=proj->get_projectionPc2(it,2);
	  rawphi=phiM-phiP;
	  rawz=zM-zP;
	  
	  //cout << m->d_PC2_z_match(3,7) << endl;  
	  pc2sdphi= m->d_PC2_phi_match(ptot,rawphi);
	  pc2sdz=m->d_PC2_z_match(ptot,rawz);
	  //} 
	
	
	  //	if (ipc3>=0&& ppc3x>-999) {
	  
	  ppc3y=proj->get_projectionPc3(it,1);
	  phiM=atan2(pc3->get_xyz(ipc3,1),pc3->get_xyz(ipc3,0));
	  phiP=atan2(ppc3y,ppc3x);
	  rawphi=phiM-phiP;
	  zM=pc3->get_xyz(ipc3,2);
	  zP=proj->get_projectionPc3(it,2);
	  rawz=zM-zP;
	  
	  if (ppc3x>0) {
	    pc3sdphi=m->d_PC3w_phi_match(ptot,rawphi);
	    pc3sdz=m->d_PC3w_z_match(ptot,rawz);
	  } else {
	    pc3sdphi=m->d_PC3e_phi_match(ptot,rawphi);
	    pc3sdz=m->d_PC3e_z_match(ptot,rawz);
	  }
	  
	  //	}
	/*
	if (iemc>=0&& pemcx>-999 ) {
	  
	  pemcy=proj->get_projectionEmc(it,1);
	  
	  cout << " before get " << iemc << " " << ntrack << " " << pemcx << endl;
	  // phiM=atan2(emc->get_xyz(iemc,1),emc->get_xyz(iemc,0));
	  cout << " after get " << endl;
	  phiP=atan2(pemcy,pemcx);
	  
	  beta=dchtrack->get_beta(it);
	  arm=emc->get_arm(iemc);
	  sect=emc->get_sector(iemc);
	  
	  rawphi=phiM-phiP;
	  zM=emc->get_xyz(iemc,2);
	  zP=proj->get_projectionEmc(it,2);
	  rawz=zM-zP;
	  
	  if (arm==0) {
	    emcsdphi=0;//m->d_PBSCw_phi_match(ptot,rawdphi);
	    emcsdz=0;//m->d_PBSCw_z_match(ptot,beta,rawdz,2);
	  } else {
	    if ((arm==1)&&(sect>1)) {
	      emcsdphi=0;//m->d_PBSCe_phi_match(ptot,rawdphi);
	      emcsdz=0;//m->d_PBSCe_z_match(ptot,beta,rawdz,2);
	    } else {
	      emcsdphi=0;//m->d_PBGL_phi_match(ptot,rawdphi);
	      emcsdz=0;//m->d_PBGL_z_match(ptot,beta,rawdz,2);
	    }
	  }
	  
	}
	*/
	naz   = 1.482*(pc2sdz-1.03*pc3sdz);
	brzo   = 0.524*(1.03*pc2sdz+pc3sdz);
	naphi = 0.6604*(pc2sdphi-1.137*pc3sdphi);
	brphi = 0.6604*(1.137*pc2sdphi+pc3sdphi);
	
	
	
	////////////////////////
	///////////////////////
	
		
	if (((naz<2)&&(brzo<2)&&(naphi<2)&&(brphi<2))/*||((emcsdphi<2)&&(emcsdpz<2)&&(pc3sdphi<2)&&(pc3sdz<2)*/) {
	  
	  
	  float phi=dchtrack->get_phi0(it);
	  float theta=dchtrack->get_theta0(it);
	  float ptra1=ptot*sin(theta);
	  

	  if (theta>0) 
	    Reac->Fill(ptra1,phi,cent,PsiBBCN1,PsiBBCS1,PsiBBC1,PsiBBCN2,PsiBBCS2,PsiBBC2,PsiCNT1,PsiCNT2,it,tevent,seg,ntrack);
	  
	}
      }


    }
    

  }
  
  
  
  
  
//    cout <<  pass << ' '  << run << ' ' << nzps << ' ' << nmul << ' ' << nhar << ' ' << ndet << endl;   
  cout << microdstfile << endl;
  dstFile->closeFile();
  delete dstFile;
  delete topNode;
  rp->closePlane(pass,run, nzps,  nmul,nhar, ndet);
  rp->deleteHisto(nzps, nmul, nhar, ndet);
  delete rp;
  
    
    OutputRootFile->Write();
    
    
}

  

//.x /phenix/workarea/sleckey/AllCode/fresh/src/offline/packages/uti/wrk/Simplicity.C("/phenix/data34/phnxreco/run2_v03_burn1/uuDST/uuDST_run2_v03_burn1-0000033693-0000.root",0);
