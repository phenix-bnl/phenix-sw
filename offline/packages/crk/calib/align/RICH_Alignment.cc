#include "RICH_Alignment.h"

#include <fstream>
using namespace std;

#include "math.h"

#include "TCanvas.h"
#include "TChain.h"
#include "TH1.h"
#include "TF2.h"

#include "PHCompositeNode.h"
#include "PHNodeIOManager.h"
#include "PHIODataNode.h"

#include "PHGeometry.h"
#include "PHSphereSection.h"
#include "PHPoint.h"
#include "PHLine.h"

#include "PHTrackOut.h"
#include "CglTrack.h"
#include "DchTrack.h"
#include "CrkHit.h"
#include "PadCluster.h"
#include "EmcClusterLocalExt.h"

#include "emcClusterContainer.h"
#include "emcClusterContent.h"

#include "dEmcClusterLocalExtWrapper.h"

using namespace PHGeometry;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<CglTrack> CglTrackNode_t;
typedef PHIODataNode<PHTrackOut> PHTrackNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;
typedef PHIODataNode<PadCluster> PadClusterNode_t;
typedef PHIODataNode<dEmcClusterLocalExtWrapper> EmcClusterLocalExtNode_t;
typedef PHIODataNode<emcClusterContainer> emcClusterContainerNode_t;
typedef PHIODataNode<CrkHit> CrkHitNode_t;



ClassImp(RICH_Alignment)   // ROOT Class Implementation

RICH_Alignment::RICH_Alignment(){

  TROOT("RICH_Alignment","RICH_Alignment");

  i_Eval = 0;

  SetMaxent(-1);
  SetEventOffset(0);
  SetDSTLvl(0);
  UsePAD(0);
  UseSurvey(0);
  SetVerbosity(0);
  VerifByPhi(1);

  SetAlignmentFile("alignment.dat");

  memset(f_dz,0,sizeof(f_dz)); 
  memset(f_dphi,0,sizeof(f_dphi));
  memset(f_dzfinal,0,sizeof(f_dzfinal)); 
  memset(f_dphifinal,0,sizeof(f_dphifinal));

  phtrk = new PHTrackOut();
  cgl = new CglTrack();
  dch = new DchTrack();
  
  emcClusters = new emcClusterContainer();
  emccontent   = new emcClusterContent();


}


RICH_Alignment::RICH_Alignment(char *dstin){
  TROOT("RICH_Alignment","RICH_Alignment");

  i_Eval = 0;

  SetMaxent(-1);
  SetEventOffset(0);
  SetDSTLvl(0);
  UsePAD(0);
  UseSurvey(0);
  SetVerbosity(0);
  VerifByPhi(1);

  SetAlignmentFile("alignment.dat");

  memset(f_dz,0,sizeof(f_dz)); 
  memset(f_dphi,0,sizeof(f_dphi));
  memset(f_dzfinal,0,sizeof(f_dzfinal)); 
  memset(f_dphifinal,0,sizeof(f_dphifinal));

  memset(i_hotPMT,0,sizeof(i_hotPMT)); 
  i_nhotPMT=0;

  SetDST(dstin);

  phtrk = new PHTrackOut();
  cgl = new CglTrack();
  dch = new DchTrack();

  emcClusters = new emcClusterContainer();
  emccontent   = new emcClusterContent();

}


Int_t RICH_Alignment::SetDST(char *dstin){
  topNode =  new PHCompositeNode("TOP");
  dstNode =  new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  iman = new PHNodeIOManager(dstin, PHReadOnly);

  return 1;
}


void RICH_Alignment::ImportHotPMTList(char *hotlistf){
  ifstream if_hot(hotlistf);
  while(!if_hot.eof()){
    if_hot >> i_hotPMT[i_nhotPMT];
    i_nhotPMT++;
  }
  if_hot.close();
}


void RICH_Alignment::SetBranches(){
  return;
}

void RICH_Alignment::ProcessTracks(){

  int trkid=-1,pc1id=-1,pc3id=-1,emcid=-1,phtrkid=-1;

  PHPoint cross;
  
  int this_trk=0,ntrk=0,arm=0,side=0,panel=0,pmtid=0,tnpmt=0,npmt=0;
  double path,npe;
  PHPoint pstart,pend;

  Float_t v_ref[3],b_ref[3];
  Float_t posx[1000],posy[1000],posz[1000],posr[1000],posphi[1000],this_npe[1000];

  tracks = new TTree("tracks","Electron candidate track and PMT hit info");

  tracks->Branch("ntrk",&this_trk,"ntrk/I");
  tracks->Branch("arm",&arm,"arm/I");
  tracks->Branch("side",&side,"side/I");
  tracks->Branch("panel",&panel,"panel/I");
  tracks->Branch("v_ref",&v_ref,"v_ref[3]/F");
  tracks->Branch("b_ref",&b_ref,"b_ref[3]/F");
  tracks->Branch("npmt",&npmt,"npmt/I");
  tracks->Branch("posx",&posx,"posx[npmt]/F");
  tracks->Branch("posy",&posy,"posy[npmt]/F");
  tracks->Branch("posz",&posz,"posz[npmt]/F");
  tracks->Branch("posr",&posr,"posr[npmt]/F");
  tracks->Branch("posphi",&posphi,"posphi[npmt]/F");
  tracks->Branch("npe",&this_npe,"npe[npmt]/F");

  this_trk=0;
  npmt=0;

  PHPoint pmt_pos;
  PHLine ref;

  SetupCGO();

  float ecorr;
  if(i_Verb > 0)
    cout << "Setting up Branches...  " << endl;

  PHCompositeNode *dummy;
  dummy = iman->read(dstNode);
  int ievt=0;   

  while(dummy && 
	(i_MaxEnt == -1 || ievt != i_MaxEnt + i_Offset)){

    if(i_Verb > 1)
      if(ievt % 1000 ==0)
	cout << ievt << " end" << endl;
    ievt++;

    if(ievt <= i_Offset)
      continue;
    
    dch = NULL;
  PHTypedNodeIterator<DchTrack> dchiter(dstNode);
  DchTrackNode_t *DchTrackNode = dchiter.find("DchTrack");
  if (DchTrackNode)
    {
      dch = DchTrackNode->getData();
    }
  if (!dch)
    {
      cout << PHWHERE << "ERROR: Node DchTrack not found." << endl;
      return;
    }

  phtrk = NULL;
  PHTypedNodeIterator<PHTrackOut> phiter(dstNode);
  PHTrackNode_t *PHTrackNode = phiter.find("PHTrackOut");
  if (PHTrackNode)
    {
      phtrk = PHTrackNode->getData();
    }
  if (!phtrk)
    {
      cout << PHWHERE << "ERROR: Node PHTrackOut not found." << endl;
      return;
    }

  cgl = 0;
  PHTypedNodeIterator<CglTrack> cgltrackmicroiter(dstNode);
  CglTrackNode_t *CglTrackMicroNode = cgltrackmicroiter.find("CglTrack");
  if (CglTrackMicroNode)
    {
      cgl = CglTrackMicroNode->getData();
    }
  if (!cgl)
    {
      cout << "UDST Node CglTrack not found" << endl;
      exit(1); //FM
    }

  dpc1 = NULL;
  PHTypedNodeIterator<PadCluster> pciter(dstNode);
  PadClusterNode_t *Pc1Node = pciter.find("Pc1Cluster");
  if (Pc1Node)
    {
      dpc1 = Pc1Node->getData();
    }
  if (!dpc1)
    {
      cout << PHWHERE << "ERROR: Node dPc1Cluster not found." << endl;
      return;
    }

  dpc3 = NULL;
  PadClusterNode_t *Pc3Node = pciter.find("Pc3Cluster");
  if (Pc3Node)
    {
      dpc3 = Pc3Node->getData();
    }
  if (!dpc3)
    {
      cout << PHWHERE << "ERROR: Node dPc3Cluster not found." << endl;
      return;
    }

  dchit = NULL;
  PHTypedNodeIterator<CrkHit> crkhiter(dstNode);
  CrkHitNode_t *CrkHitNode = crkhiter.find("CrkHit");
  if (CrkHitNode)
    {
      dchit = CrkHitNode->getData();
    }
  if (!dchit)
    {
      cout << PHWHERE << "ERROR: Node dCrkHit not found." << endl;
      return;
    }

  demc= 0;
  emcClusters = 0;

  PHTypedNodeIterator<emcClusterContainer> eeiter(topNode);
  emcClusterContainerNode_t* EmcCLNode = eeiter.find("emcClusterContainer");
  if ( !EmcCLNode )
    {
      // Try old object instead
      PHTypedNodeIterator<dEmcClusterLocalExtWrapper> eeiterold(topNode);
      EmcClusterLocalExtNode_t *EmcCLENode =
        eeiterold.find("dEmcClusterLocalExt");
      if (EmcCLENode)
        {
          demc = EmcCLENode->getData();
        }
    }
  else
    {
      emcClusters = EmcCLNode->getData();
    }
  if (!demc && !emcClusters)
    {
      cout << PHWHERE << "ERROR: none of emcClusterContainer and "
	   << "dEmcClusterLocalExt found."
	   << endl;
      return;
    }


    ntrk = cgl->get_CglNTrack();

    if(i_Verb > 1)
      if(ievt % 1000 == 1)
	cout << "Track entries for this event is " << ntrk << endl;

    for(int itrk=0;itrk<ntrk;itrk++){
      phtrkid = cgl->get_id(itrk);
      trkid = cgl->get_dctracksid(itrk);
      pc1id = cgl->get_pc1clusid(itrk);
      pc3id = cgl->get_pc3clusid(itrk);
      emcid = cgl->get_emcclusid(itrk);
  // cout << emcid << " " << pc1id << " " << trkid << " " << phtrkid << endl;
      if(emcid > -1 && trkid > -1){	  
	ecorr = emcClusters->findCluster(emcid)->ecore();

	if(ecorr > 0 && 
	   fabs(ecorr/dch->get_momentum(trkid)) > 0.4 &&
	   fabs(ecorr/dch->get_momentum(trkid)) < 1.8){
	  if(i_UsePAD==1 && pc3id > -1 && pc1id >-1){	      // Using PAD
	    if(fabs(atan2(dpc3->get_xyz(pc3id,1),dpc3->get_xyz(pc3id,0)) -
		    atan2(phtrk->get_projectionPc3(phtrkid,1),
			 phtrk->get_projectionPc3(phtrkid,0))) < 0.2 ){
	      pstart.setX(dpc1->get_xyz(pc1id,0));
	      pstart.setY(dpc1->get_xyz(pc1id,1));
	      pstart.setZ(dpc1->get_xyz(pc1id,2));
	      
	      pend.setX(dpc3->get_xyz(pc3id,0));
	      pend.setY(dpc3->get_xyz(pc3id,1));
	      pend.setZ(dpc3->get_xyz(pc3id,2));
	    }
	    else
	      continue;   // skip this track
	  }
	  else{  	    // Using DC track projection
	    pstart.setX(phtrk->get_projectionPc1(phtrkid,0));
	    pstart.setY(phtrk->get_projectionPc1(phtrkid,1));
	    pstart.setZ(phtrk->get_projectionPc1(phtrkid,2));
	    
	    pend.setX(phtrk->get_projectionPc3(phtrkid,0));
	    pend.setY(phtrk->get_projectionPc3(phtrkid,1));
	    pend.setZ(phtrk->get_projectionPc3(phtrkid,2));
	  }
	}
	else
	  continue;   // skip this track
      	
	
	PHLine trkline(pstart,pend);
	
	arm = pstart.getX() > 0 ? 0 : 1;
	
	// Reflect line made
	ref = cgo->Reflect(arm, trkline, side, panel, path);
	
	v_ref[0] = ref.getDirection().getX();
	v_ref[1] = ref.getDirection().getY();
	v_ref[2] = ref.getDirection().getZ();
	
	b_ref[0] = ref.getBasepoint().getX();
	b_ref[1] = ref.getBasepoint().getY();
	b_ref[2] = ref.getBasepoint().getZ();
	
	if (ref.length() > 0.) {
	  // If reflect line could be made, make cross point to PMT array.
	  cross = cgo->HitArray (arm, ref, side);
	  
	  if (  cross == PHPoint() )
	    ;
	  else{   // If Hit to PMT array, Loop for each PMT hits. 
	    
	    //	    tnpmt = dchit->RowCount();
	    tnpmt = dchit->get_CrkNHit();
	  
	  for (int ihit = 0; ihit < tnpmt; ihit ++) {
	    
	      pmtid = dchit->get_pmt(ihit);
	      npe = dchit->get_npe(ihit);

	    
	    if(npe > 8.)
	      continue;     // Reject PMT noises
	    	    
	    // Reject Hot PMT's
	    for(int ihpmt=0;ihpmt<i_nhotPMT;ihpmt++){
	      if(pmtid==i_hotPMT[ihpmt])
		npe=0;
	    }

	    if(npe>0.1)
	      if(cgo->IdToArm(pmtid) == arm){ // Take PMT in the same sector
		// Get PMT position
		pmt_pos = cgo->GetPmtPosition(cgo->IdToArm(pmtid),
					      cgo->IdToSide(pmtid),
					      cgo->IdToSm(pmtid),
					      cgo->IdToPmt(pmtid));
		
		posx[npmt]=pmt_pos.getX();
		posy[npmt]=pmt_pos.getY();
		posz[npmt]=pmt_pos.getZ();
		posr[npmt]=sqrt(posx[npmt]*posx[npmt] + 
			      posy[npmt]*posy[npmt]);
		posphi[npmt]= (posy[npmt] / fabs(posy[npmt])) * 
		  acos(posx[npmt]/posr[npmt]);
		this_npe[npmt]=npe;
		npmt++;
		
	      } // If same sector
	    
	  }  // for each PMT hits
	  
	  tracks->Fill();
	  this_trk++;
	  npmt=0;
	  
	  } // If reflected lines crosses to PMT array
	}  // If reflected line exists    
      } // If associated EMC cluster and Pad cluster exists
    } // For each tracks
      dummy = iman->read(dstNode);
  }  //For each event
  
  if(i_Eval==1){
    tf_eval->cd();
    tracks->Write();
  }

}

void RICH_Alignment::ProcessRings(Int_t iloop){
  
  int ntrk,arm,side,panel,this_npmt;

  Float_t *v_ref = new Float_t[3];
  Float_t *b_ref = new Float_t[3];
  Float_t *posx = new Float_t[1000]; 
  Float_t *posy = new Float_t[1000];
  Float_t *posz = new Float_t[1000];
  Float_t *posr = new Float_t[1000];
  Float_t *posphi = new Float_t[1000];
  Float_t *this_npe = new Float_t[1000];
  
  tracks->SetBranchAddress("arm",&arm);
  tracks->SetBranchAddress("side",&side);
  tracks->SetBranchAddress("panel",&panel);
  tracks->SetBranchAddress("v_ref",v_ref);
  tracks->SetBranchAddress("b_ref",b_ref);
  tracks->SetBranchAddress("npmt",&this_npmt);
  tracks->SetBranchAddress("posx",posx);
  tracks->SetBranchAddress("posy",posy);
  tracks->SetBranchAddress("posz",posz);
  tracks->SetBranchAddress("posr",posr);
  tracks->SetBranchAddress("posphi",posphi);
  tracks->SetBranchAddress("npe",this_npe);
  
  ntrk = (Int_t) tracks->GetEntries();

  TCanvas *tc_printed[4];

  TH2F *h_ring[2][2][24];
  TH2F *h_ringtot=NULL,*h_ringinit=NULL;
  TH1F *h_R=NULL;
  
  Text_t drawn[100],tsect[10];

  if(iloop == 20){
    h_ringtot = new TH2F("h_ringtot","Accumulated for all panels for QA"
			 ,60,-15,15,60,-15,15);
    h_R = new TH1F("h_R","Distance from ring center",150,0,30);
  }
  if(iloop == -1)
    h_ringinit = new TH2F("h_ringinit",
			  "Accumulated for all panels with non-aligned"
			  ,60,-15,15,60,-15,15);

  for(int iarm=0;iarm<2;iarm++){
    for(int iside=0;iside<2;iside++){
      switch(iarm *2 + iside){
      case 0:    sprintf(tsect,"WS"); break;
      case 1:    sprintf(tsect,"WN"); break;
      case 2:    sprintf(tsect,"ES"); break;
      case 3:    sprintf(tsect,"EN"); break;
      }
      if(iloop == 20){
	tc_printed[iarm*2+iside] = new TCanvas(tsect,tsect,10,10,600,400);
      }
      for(int ip=0;ip<24;ip++){
	sprintf(drawn,"%s_%d_%d",tsect,ip,iloop);
	h_ring[iarm][iside][ip] = new TH2F(drawn,drawn,60,-15,15,60,-15,15);
      } 
    }
  }


  if(i_Verb>0 && iloop ==-1)
    cout << "Total entry picked up is " << ntrk << endl;
  if(i_Verb>0 )
    cout << iloop << "th loop" << endl;

  for(int itrk=0;itrk<ntrk;itrk++){
    tracks->GetEntry(itrk);
    // Make frame of Reflected line.
    // Z is the reflected line. And X is directed to beam axis.
    
    PHVector newZ(v_ref[0],v_ref[1],v_ref[2]);
    newZ.normalize();
    PHVector newY = newZ.cross(PHVector(0,0,1));
    newY.normalize();
    PHVector newX = newY.cross(newZ);
    newX.normalize();

    //   Align Z and Phi axis to positive
    if(newX.getZ()<0)
      newX = -newX;
    if(i_VerifByPhi==0&&iloop!=20)
      if(newY.getY()<0)
	newY = -newY;
    newZ = newX.cross(newY);

    PHPoint base(b_ref[0],b_ref[1],b_ref[2]);    
    PHFrame fr_ref(base,newX,newY,newZ);

    for(int ipmt=0;ipmt<this_npmt;ipmt++){
      //    Set mirror alignment
      PHPoint pmt_pos(posx[ipmt],posy[ipmt],posz[ipmt]);

      if(iloop > -1){
	if(i_VerifByPhi){
	  pmt_pos.setX(posr[ipmt] * cos(posphi[ipmt] - 
					 f_dphi[arm][side][panel][iloop]));
	  pmt_pos.setY(posr[ipmt] * sin(posphi[ipmt] - 
					 f_dphi[arm][side][panel][iloop]));
	  pmt_pos.setZ(posz[ipmt] - f_dz[arm][side][panel][iloop]);
	}
	else{
	  PHPoint dzdphi(0,f_dphi[arm][side][panel][iloop],
			 f_dz[arm][side][panel][iloop]);
	  pmt_pos = pmt_pos - dzdphi;
	}
      }
      
      if(iloop == 20){
	pmt_pos.setX(posr[ipmt] * cos(posphi[ipmt] - 
				      f_dphifinal[arm][side][panel]));
	pmt_pos.setY(posr[ipmt] * sin(posphi[ipmt] - 
				      f_dphifinal[arm][side][panel]));
	pmt_pos.setZ(posz[ipmt] - f_dzfinal[arm][side][panel]);
      }
      
      PHPoint pmt_pos_trans = transformPoint(PHFrame(),pmt_pos,fr_ref);

      h_ring[arm][side][panel]->Fill(pmt_pos_trans.getX(),
				     pmt_pos_trans.getY());
      if(iloop == 20){
	h_ringtot->Fill(pmt_pos_trans.getX(),
			pmt_pos_trans.getY());
	h_R->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
		       pmt_pos_trans.getY()*pmt_pos_trans.getY()));
      }
      if(iloop == -1)
	h_ringinit->Fill(pmt_pos_trans.getX(),
			pmt_pos_trans.getY());
    }

  }

  if(i_Verb>1)
    cout << iloop << "th loop Fill end" << endl;

  Double_t z,phi;
  TGraph *tg,*guide_in=NULL,*guide_out=NULL;
  float gxin[31],gyin[31],gxout[31],gyout[31];

  if(iloop==20){
    for(int i=0;i<31;i++){
      gxin[i] = 3.4 * cos ((float)i * M_PI / 15.);
      gyin[i] = 3.4 * sin ((float)i * M_PI / 15.);
      gxout[i] = 8.4 * cos ((float)i * M_PI / 15.);
      gyout[i] = 8.4 * sin ((float)i * M_PI / 15.);
    }
    guide_in = new TGraph(31,gxin,gyin);
    guide_out = new TGraph(31,gxout,gyout);

    tf_eval->cd();
    h_ringtot->Write();
    h_R->Write();
  }
  if(iloop==-1 && i_Eval==1){
    tf_eval->cd();
    h_ringinit->Write();
  }

  for(int ic=0;ic<4;ic++){
    if(iloop==20)
      tc_printed[ic]->Divide(6,4);
    for(int ip=0;ip<24;ip++){
      if(iloop==20){
	tc_printed[ic]->cd(ip+1);
	h_ring[ic / 2][ic % 2][ip]->Draw("col");
	guide_in->Draw("CP");
	guide_out->Draw("CP");
	if(i_Eval==1){
	  CalcError(h_ring[ic/2][ic%2][ip],ic/2,ic%2,ip);
	  tf_eval->cd();
	  //	  h_ring[ic / 2][ic % 2][ip]->Write();
	}
      }
      else{
	tg = FindCent(h_ring[ic / 2][ic % 2][ip]);
	tg->GetPoint(8,z,phi);
	if(iloop == -1){
	  f_dz[ic / 2][ic % 2][ip][iloop+1] = z;
	  if(i_VerifByPhi)
	    f_dphi[ic / 2][ic % 2][ip][iloop+1] = phi / 263.5; 
	  else
	    f_dphi[ic / 2][ic % 2][ip][iloop+1] = phi; 
	}
	else if(iloop+1 > -1 && iloop + 1 < 20){
	  f_dz[ic / 2][ic % 2][ip][iloop+1] = 
	    f_dz[ic / 2][ic % 2][ip][iloop] + z;
	  if(i_VerifByPhi)
	    f_dphi[ic / 2][ic % 2][ip][iloop+1] = 
	      f_dphi[ic / 2][ic % 2][ip][iloop] + (phi / 263.5); 
	  else
	    f_dphi[ic / 2][ic % 2][ip][iloop+1] = 
	      f_dphi[ic / 2][ic % 2][ip][iloop] + phi;
	}

	if(i_Eval==1){
	  tf_eval->cd();
	  sprintf(drawn,"g_%s",h_ring[ic / 2][ic % 2][ip]->GetName());
	  //	  tg->Write(drawn);
	  //	  h_ring[ic / 2][ic % 2][ip]->Write();
	}

      }
            
    } // panel loop

    if(iloop==20 && i_Eval==1)
      tc_printed[ic]->Print();

  } // Arm + Sector loop

  TTree *result=NULL;
  Float_t dz,dphi,errdz,errdphi;
  int ia,is,ip;
  if(iloop == 20 && i_Eval==1){
    result = new TTree("result","Alignment result");
    result->Branch("arm",&ia,"arm/I");
    result->Branch("side",&is,"side/I");
    result->Branch("panel",&ip,"panel/I");
    result->Branch("dz",&dz,"dz/F");
    result->Branch("dphi",&dphi,"dphi/F");
    result->Branch("errdz",&errdz,"errdz/F");
    result->Branch("errdphi",&errdphi,"errdphi/F");

    for(ia=0;ia<2;ia++)
      for(is=0;is<2;is++)
	for(ip=0;ip<24;ip++){
	  dz = f_dzfinal[ia][is][ip]; dphi = f_dphifinal[ia][is][ip];
	  errdz = f_errdz[ia][is][ip]; errdphi = f_errdphi[ia][is][ip];
	  if(i_Eval==1)
	    result->Fill();
	}
    tf_eval->cd();
    result->Write();
  }
    
  for(int ic=0;ic<4;ic++)
    for(int ip=0;ip<24;ip++)
      delete h_ring[ic / 2][ic % 2][ip];

  delete [] v_ref;
  delete [] b_ref;
  delete [] posx;
  delete [] posy;
  delete [] posz;
  delete [] posr;
  delete [] posphi;
  delete [] this_npe;
  
}


TGraph* RICH_Alignment::FindCent(TH2* hist){

  float NBINS = hist->GetNbinsX(); 
  float XMAX = hist->GetXaxis()->GetXmax();
  float XMIN = hist->GetXaxis()->GetXmin();
  float SCAN_W = 2.;
  float binw = (XMAX - XMIN) / (float)NBINS;
  Int_t xmin,xmax;
  float z[15],phi[15];
  Text_t gname[150];

  xmin = (Int_t) ((-SCAN_W -XMIN )/ binw);
  xmax = (Int_t) ((SCAN_W -XMIN )/ binw);

  sprintf(gname,"x%d_%s",0,hist->GetName());
  TH1D *x = hist->ProjectionX(gname,xmin,xmax);
  x->SetAxisRange(-10.,0);
  z[0] = x->GetMean();
  x->SetAxisRange(0.,10.);
  z[0] += x->GetMean();
  z[0] /=2.;


  xmin = (Int_t)((-XMIN -z[0] -SCAN_W)/ binw);
  xmax = (Int_t)((-XMIN -z[0] +SCAN_W)/ binw);

  sprintf(gname,"phi%d_%s",0,hist->GetName());
  TH1D *y = hist->ProjectionY(gname,xmin,xmax);
  y->SetAxisRange(-10.,0);
  phi[0] = y->GetMean();
  y->SetAxisRange(0.,10.);
  phi[0] += y->GetMean();
  phi[0] /=2.;
  delete x,y;
  for(int ig=1;ig<10;ig++){
    xmin = (Int_t) ((-SCAN_W -XMIN -phi[ig-1])/ binw);
    xmax = (Int_t) ((SCAN_W -XMIN -phi[ig-1])/ binw);

    sprintf(gname,"x%d_%s",ig,hist->GetName());
    x = hist->ProjectionX(gname,xmin,xmax);
    x->SetAxisRange(z[ig-1]-10.,z[ig-1]);
    z[ig] = x->GetMean();
    x->SetAxisRange(z[ig-1],z[ig-1]+10.);
    z[ig] += x->GetMean();
    z[ig] /=2.;


    xmin = (Int_t)((-XMIN -z[ig-1] -SCAN_W)/ binw);
    xmax = (Int_t)((-XMIN -z[ig-1] +SCAN_W)/ binw);

    sprintf(gname,"phi%d_%s",ig,hist->GetName());
    y = hist->ProjectionY(gname,xmin,xmax);
    y->SetAxisRange(phi[ig-1]-10.,phi[ig-1]);
    phi[ig] = y->GetMean();
    y->SetAxisRange(phi[ig-1],phi[ig-1]+10.);
    phi[ig] += y->GetMean();
    phi[ig] /=2.;
    delete x,y;
  }

  TGraph *out = new TGraph(10,z,phi);
  return out;
}


void RICH_Alignment::CalcError(TH2* hist,int arm,int side,int panel){

  float NBINS = hist->GetNbinsX(); 
  float XMAX = hist->GetXaxis()->GetXmax();
  float XMIN = hist->GetXaxis()->GetXmin();
  float SCAN_W = 2.;
  float binw = (XMAX - XMIN) / (float)NBINS;
  Int_t xmin,xmax;
  float err;
  float rms,N;
  Text_t gname[150];

  xmin = (Int_t) ((-SCAN_W -XMIN )/ binw);
  xmax = (Int_t) ((SCAN_W -XMIN )/ binw);

  sprintf(gname,"zerr_%s",hist->GetName());
  TH1D *x = hist->ProjectionX(gname,xmin,xmax);
  x->SetAxisRange(-10.,0);
  rms = x->GetRMS();
  N = x->Integral();
  if(N>0)
    err = rms*rms/N;
  else
    err = 0;
  x->SetAxisRange(0.,10.);
  rms = x->GetRMS();
  N = x->Integral();
  if(N>0)
    err += rms*rms/N;
  else
    err += 0;
  f_errdz[arm][side][panel] = sqrt(err)/2.;

  sprintf(gname,"phierr_%s",hist->GetName());
  TH1D *y = hist->ProjectionY(gname,xmin,xmax);
  y->SetAxisRange(-10.,0);
  rms = y->GetRMS();
  N = y->Integral();
  if(N>0)
    err = rms*rms/N;
  else
    err = 0;
  y->SetAxisRange(0.,10.);
  rms = y->GetRMS();
  N = y->Integral();
  if(N>0)
    err += rms*rms/N;
  else
    err += 0;
  f_errdphi[arm][side][panel] = (sqrt(err)/2.) / 263.5;
}


void RICH_Alignment::Calc(Int_t fixed, Int_t mean){
  int ia,is,ip;

  memset(f_dzfinal,0,sizeof(f_dzfinal)); 
  memset(f_dphifinal,0,sizeof(f_dphifinal));

  for(ia=0;ia<2;ia++)
    for(is=0;is<2;is++)
      for(ip=0;ip<24;ip++){
	for(int i=0;i<mean;i++){
	  f_dzfinal[ia][is][ip] += f_dz[ia][is][ip][fixed + i + 1];
	  f_dphifinal[ia][is][ip] += f_dphi[ia][is][ip][fixed + i + 1];
	}
	f_dzfinal[ia][is][ip] = f_dzfinal[ia][is][ip] / (Float_t) mean;
	f_dphifinal[ia][is][ip] = f_dphifinal[ia][is][ip] / (Float_t) mean;

	if(!i_VerifByPhi){
	  f_dphifinal[ia][is][ip] = f_dphifinal[ia][is][ip] / 263.5;
	}

	if(i_Verb>1){
	  cout << ia << " " << is << " " << ip << " " 
	       << f_dzfinal[ia][is][ip] << " " 
	       << f_dphifinal[ia][is][ip] <<endl;
	}	  
      }
}


void RICH_Alignment::Write(){
  cout << "Writing alignment result on " << c_alignmentf << endl;
  ofstream fout(c_alignmentf);
  for(int ia=0;ia<2;ia++)
    for(int is=0;is<2;is++)
      for(int ip=0;ip<24;ip++){
	fout.width(3);
	fout << ia;
	fout.width(3);
	fout << is;
	fout.width(3);
	fout << ip;
	fout.width(9);
	fout.precision(3);
	fout << f_dzfinal[ia][is][ip];
	fout.width(10);
	fout.precision(4);
	fout << f_dphifinal[ia][is][ip] << endl;
      }
  fout.close();
}

void RICH_Alignment::Write(char *align){
  cout << "Writing alignment result on " << align << endl;
  ofstream fout(align);
  for(int ia=0;ia<2;ia++)
    for(int is=0;is<2;is++)
      for(int ip=0;ip<24;ip++){
	fout.width(3);
	fout << ia;
	fout.width(3);
	fout << is;
	fout.width(3);
	fout << ip;
	fout.width(9);
	fout.precision(3);
	fout << f_dzfinal[ia][is][ip];
	fout.width(10);
	fout.precision(4);
	fout << f_dphifinal[ia][is][ip] << endl;
      }
  fout.close();
}


void RICH_Alignment::Verify(){
  ProcessRings(20);
}


void RICH_Alignment::Process(Int_t fixed, Int_t mean){

  if(i_Verb>0 )
    cout << "Process DST Entries" << endl;
  ProcessTracks();

  for(int i=-1;i<fixed+mean;i++){
  if(i_Verb>0 )
    cout << "Find ring center. " << i << "th try" << endl;
    ProcessRings(i);
  }
  cout << "Calculate final ring center positions." << endl;
  Calc(fixed,mean);
  Write();

  if(i_Eval > 0){
    Verify();
  }
}

void RICH_Alignment::ImportOutput(char *evalf,char *alignf){

  int arm,side,panel;
  float dz,dphi;
  int cnt = 0;

  TFile *evalimp = new TFile(evalf);
  tracks = (TTree*) evalimp->Get("tracks");
  ifstream fdzdphi(alignf);
  while(!fdzdphi.eof() && cnt<96){
    fdzdphi >> arm >> side >> panel >> dz >> dphi;
    f_dzfinal[arm][side][panel] = dz;
    f_dphifinal[arm][side][panel] = dphi;
    cnt++;
  }

  fdzdphi.close();
}

