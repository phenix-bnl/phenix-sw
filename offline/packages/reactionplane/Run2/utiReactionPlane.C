#include "BbcOut.h"
#include "BbcRaw.h"
#include "CglTrack.h"  
#include "PHTrackOut.h"
#include "DchTrack.h"
#include "EventHeader.h"
#include "RunHeader.h"
#include "TrigLvl1.h"
#include "VtxOut.h"
#include "Bbc.hh"
#include "Zdc.hh"
#include "PHIODataNode.h"
#include "PHCompositeNode.h"
#include "PHNodeIOManager.h"
#include "PHNodeReset.h"
#include "PHGlobal.h"
#include "TriggerHelper.h"
#include "TrigLvl1.h"
#include "Lvl2DecisionOut.h"
#include "PHCentralTrack.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include "PdbDouble.hh"
#include "utiCentrality.h"
#include "utiReactionPlane.h"

#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;
using namespace PhUtilities;

utiReactionPlane* utiReactionPlane::_instance =0;
utiReactionPlane* utiReactionPlane::instance()
{
  if(!_instance)
    _instance = new utiReactionPlane();
  return _instance;
}

utiReactionPlane::utiReactionPlane()
{
  cout << "info. utiReactionPlane::utiReactionPlane, created" << endl;


  CheckByOneZ=(1==0);
  CheckByOneCent=(1==0);

  //Default numbers
  ncent=15;
  nzps=7;
  ndet=8;   // number of subsamples interested in BBCN, BBCS, and BBC toatl  
  nhar=2;    //ihar=0  is directed , ihar=2 --> elliptic 

  int idet,ihar;
  for (idet=0; idet<ndet; idet++) {
    for (ihar=0; ihar<nhar; ihar++) {
      psi[idet][ihar]=-99;
    }
  }

  sprintf(incalibname,"%s","utiReactionPlane.table");
  sprintf(outcalibname,"%s","utiReactionPlane.table");
  //makeHisto();
}

utiReactionPlane::~utiReactionPlane()
{
  deleteHisto();
  cout << "info. utiReactionPlane::~utiReactionPlane, deleted" << endl;
}

int utiReactionPlane::GetCentralityBin(int cent){

  Int_t MostPeripheral=100;
  Int_t MostCentral=0;

  int bin = (int) ((ncent-1)*cent/(MostPeripheral-MostCentral));
  if (bin>(ncent-1)) bin=ncent-1;
  if (bin<0) bin=0;
  
  return bin;
}

int utiReactionPlane::GetZBin(float z){
  //-40 to 40
  Float_t MostNorth=40;
  Float_t MostSouth=-40;

  int bin = (int) (((nzps-1)*(z-MostSouth))/(MostNorth-MostSouth));
  if (bin>(nzps-1)) bin=(nzps-1);
  if (bin<0) bin=0;
    return bin;
}


void utiReactionPlane::SetParamTable(int runs, int zbins, int centb, int dets, int hars)
{
  runset=runs;
  nzps=zbins;
  ncent=centb;
  ndet=dets;
  nhar=hars;
}

//read calibrations
void utiReactionPlane::readTable(int run, int DB)
{

  int dummy;

  cout << "info. utiReactionPlane::read GeoCal" << endl;

  if (DB==0) {//file 
    ifstream calFile,geoFile;
    calFile.open("bbccal.dat");
    geoFile.open("bbcgeo.dat");

    if(!calFile){
      cout<<"bbccal.dat does not exist"<<endl;
      return;
    }

    if(!geoFile){
      cout<<"bbcgeo.dat does not exist"<<endl;
      return;
    }
    int itmp;
    for (int ipmt=0; ipmt<128; ipmt++) {
      calFile >> pede[ipmt] >> oflw[ipmt] >> slw1[ipmt] >> slw2[ipmt]
	      >> gain[ipmt] >> tgai[ipmt] >> qgai[ipmt];
      geoFile >> itmp >> xpos[ipmt] >> ypos[ipmt] >> zpos[ipmt];
    }
    calFile.close();
    geoFile.close();

    cout << "reading from Table " << incalibname <<endl;
    ifstream ifs;
    ifs.open(incalibname);
    if(!ifs){
      cout<<incalibname<<" does not exist"<<endl;
      
    } else {
 
      ifs >> dummy;

      if (dummy!=123321) {
	cout << " This table is not compatible with utiReactionPlane.C " << endl;
	cout << " Please check " << endl;
	exit(1);
      }

      ifs >> runset;
      ifs >> nzps;
      ifs >> ncent;
      ifs >> ndet;
      ifs >> nhar;
      
      //gain correction
      for (int iz=0; iz<nzps; iz++) {
	for (int im=0; im<ncent; im++) {
	  for (int ip=0; ip<128; ip++) {
	    ifs >> fac[im][iz][ip];
	}
	}
      }

      //offset correction
      for (int iz=0; iz<nzps; iz++) {
	for (int im=0; im<ncent; im++) {
	  for (int ih=0; ih<nhar; ih++) {
	    for (int ib=0; ib<ndet; ib++) {
	      ifs>> ofsx[ib][ih][im][iz] >> ofsy[ib][ih][im][iz];
	    }
	  }
	}
      }
      //flatting correction
      for (int iz=0; iz<nzps; iz++) {
	for (int im=0; im<ncent; im++) {
	  for (int ih=0; ih<nhar; ih++) {
	    for (int ib=0; ib<ndet; ib++) {
	      for (int ia=0; ia<nOrd; ia++) {
		ifs >> costable[ib][ih][im][iz][ia] >> sintable[ib][ih][im][iz][ia];
	      }
	    }
	  }
	}
      }
    }
  } else {
    cout << "reading from DB" << endl;


    if (run>32552) 
      {
	run = 32552;
	cout << " near or past end of Au Au using defualt RP calibrations from run 32552-33694 " << endl; 
      }
    PdbBankManager *bc = PdbBankManager::instance();
    bc->getApplication()->startRead();
    PdbBankID bid=0;
    PdbCalBank *cc = bc->fetchBank("PdbDoubleBank",bid,"sean.reac.2",27808/*cal*/);
    PdbCalBank *cd = bc->fetchBank("PdbDoubleBank",bid,"sean.reac.2",33694/*geo*/);
    cout << " loaded first" << endl;

    if ((cc)&&(cd)) {
      int whereamical=0;
      int whereamigeo=0;
      //float itmp;

      for (int ipmt=0; ipmt<128; ipmt++) {
	//cal
	pede[ipmt]=((PdbDouble &) cc->getEntry(whereamical++)).getValue();
	oflw[ipmt]=((PdbDouble &) cc->getEntry(whereamical++)).getValue();
	slw1[ipmt]=((PdbDouble &) cc->getEntry(whereamical++)).getValue();
	slw2[ipmt]=((PdbDouble &) cc->getEntry(whereamical++)).getValue();
	gain[ipmt]=((PdbDouble &) cc->getEntry(whereamical++)).getValue();
	tgai[ipmt]=((PdbDouble &) cc->getEntry(whereamical++)).getValue();
	qgai[ipmt]=((PdbDouble &) cc->getEntry(whereamical++)).getValue();
	// geo
	//itmp      =((PdbDouble &) cd->getEntry(whereamigeo++)).getValue();
	xpos[ipmt]=((PdbDouble &) cd->getEntry(whereamigeo++)).getValue();
	ypos[ipmt]=((PdbDouble &) cd->getEntry(whereamigeo++)).getValue();
	zpos[ipmt]=((PdbDouble &) cd->getEntry(whereamigeo++)).getValue();
      }
      bc->getApplication()->commit();
      delete cc;
      delete cd;      
    } else {
      cout << "Error Reading DB utiReactionPlane::readTable(int run, int DB), run="  << run << endl;
      cout << "PdbCalBank *cc = bc->fetchBank(\"PdbDoubleBank\",0,\"sean.reac.2\",27808/*cal*/); " << endl;
      cout << "PdbCalBank *cd = bc->fetchBank(\"PdbDoubleBank\",0,\"sean.reac.2\",33694/*geo*/); " << endl;
      exit(1);
    }


    

    PdbBankManager *b = PdbBankManager::instance();
    b->getApplication()->startRead();
    PdbCalBank *c = b->fetchBank("PdbDoubleBank",bid,"sean.reac.1",run);
 
    if (c) {

      int whereami =0;
      
      //gain correction
      dummy=(int) ((PdbDouble &) c->getEntry(whereami++)).getValue();

      if (dummy!=123321) {
	cout << "rp DB format problem" << endl;
	exit(1);
      }
      runset = (int) ((PdbDouble &) c->getEntry(whereami++)).getValue();
      nzps = (int) ((PdbDouble &) c->getEntry(whereami++)).getValue();
      ncent= (int) ((PdbDouble &) c->getEntry(whereami++)).getValue();
      ndet= (int) ((PdbDouble &) c->getEntry(whereami++)).getValue();
      nhar= (int) ((PdbDouble &) c->getEntry(whereami++)).getValue();

      for (int iz=0; iz<nzps; iz++) {
	for (int im=0; im<ncent; im++) {
	  for (int ip=0; ip<128; ip++) {
	    fac[im][iz][ip] = ((PdbDouble &) c->getEntry(whereami++)).getValue();
	  }
	}
      }
      //offset correction
      for (int iz=0; iz<nzps; iz++) {
	for (int im=0; im<ncent; im++) {
	  for (int ih=0; ih<nhar; ih++) {
	    for (int ib=0; ib<ndet; ib++) {
	      ofsx[ib][ih][im][iz] = ((PdbDouble &) c->getEntry(whereami++)).getValue();
	      ofsy[ib][ih][im][iz] = ((PdbDouble &) c->getEntry(whereami++)).getValue();
	    }
	  }
	}
      }
      //flatting correction
      for (int iz=0; iz<nzps; iz++) {
	for (int im=0; im<ncent; im++) {
	  for (int ih=0; ih<nhar; ih++) {
	    for (int ib=0; ib<ndet; ib++) {
	      for (int ia=0; ia<nOrd; ia++) {
		costable[ib][ih][im][iz][ia]= ((PdbDouble &) c->getEntry(whereami++)).getValue();
		sintable[ib][ih][im][iz][ia]= ((PdbDouble &) c->getEntry(whereami++)).getValue();
	      }
	    }
	  }
	}
      }
      b->getApplication()->commit();    
      delete c;
    } else {
      cout << "Error Reading DB utiReactionPlane::readTable(int run, int DB), run="  << run << endl;
      cout << " PdbCalBank *c = b->fetchBank(\"PdbDoubleBank\",0,\"sean.reac.1\",run); " << endl;
      exit(1);
    }
      

  }

  // reformat geo info
  float rad;
  int ioff;
  int nrad[14];
  for (int i=0; i<14; i++) nrad[i]=0;
  for (int ipmt=0; ipmt<128; ipmt++) {
    phipos[ipmt] = atan2(ypos[ipmt],xpos[ipmt]);      
    //cout << ipmt << ' ' <<  phipos[ipmt] << endl;
    ioff=0; 
    if (ipmt>63) ioff=7; 
    rad=sqrt(xpos[ipmt]*xpos[ipmt]+ypos[ipmt]*ypos[ipmt]);
    if (rad<80.0)       {irad[ipmt]=0+ioff; nrad[0+ioff]++;}
    else if (rad<90.0)  {irad[ipmt]=1+ioff; nrad[1+ioff]++;}
    else if (rad<100.0) {irad[ipmt]=2+ioff; nrad[2+ioff]++;}
    else if (rad<110.0) {irad[ipmt]=3+ioff; nrad[3+ioff]++;}
    else if (rad<120.0) {irad[ipmt]=4+ioff; nrad[4+ioff]++;}
    else if (rad<126.0) {irad[ipmt]=5+ioff; nrad[5+ioff]++;}
    else                {irad[ipmt]=6+ioff; nrad[6+ioff]++;}
  }
}

//write calibrations
void utiReactionPlane::writeTable(int run, int DB){
  ofstream ofs;
  ofs.open(outcalibname);
  cout << "checking into file" <<outcalibname<< endl;
  //gain correction

  ofs << 123321 << endl;
  ofs << run << endl;
  ofs << nzps << endl;
  ofs << ncent << endl;
  ofs << ndet << endl;
  ofs << nhar << endl;

  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<ncent; im++) {
      for (int ip=0; ip<128; ip++) {
	ofs << fac[im][iz][ip] << endl;
      }
    }
  }
  //offset correction
  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<ncent; im++) {
      for (int ih=0; ih<nhar; ih++) {
	for (int ib=0; ib<ndet; ib++) {
	  ofs << ofsx[ib][ih][im][iz] << " "
	      << ofsy[ib][ih][im][iz] << endl;
	}
      }
    }
  }
  //flatting correction
  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<ncent; im++) {
      for (int ih=0; ih<nhar; ih++) {
	for (int ib=0; ib<ndet; ib++) {
	  for (int ia=0; ia<nOrd; ia++) {
	    ofs << costable[ib][ih][im][iz][ia] << " "
		<< sintable[ib][ih][im][iz][ia] << endl;
	  }
	}
      }
    }
  }
  ofs.close();

  if(DB==1){//DB
    Char_t command[100];
    sprintf(command,"setDouble -d \"checkining\" sean.reac.1 %i 0 %s",run,outcalibname);
    gSystem->Exec(command);
  }  
}

void utiReactionPlane::calculateReac(PHCompositeNode *topNode, int pass){

  short adc[128];
  short tdc[128];
  //  topNode->Print();
  int idet,ihar;


  for (idet=0; idet<ndet; idet++) {
    for (ihar=0; ihar<nhar; ihar++) {
      psi[idet][ihar]=-99;
    }
  }
  
 

  //set up pointers
  PHIODataNode<PHObject>*Node;
  BbcOut      *bbcout   =0;
  ZdcOut      *zdcout   =0;
  VtxOut      *vtxout   =0;
  CglTrack    *cgltrack =0;
  BbcRaw      *bbcraw   =0;
  DchTrack    *dchtrack =0;

  PHNodeIterator iter(topNode);

  Node = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","BbcOut");
  if(Node) bbcout = (BbcOut *)(Node->getData());
  Node = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","ZdcOut");
  if(Node) zdcout = (ZdcOut *)(Node->getData());
  Node = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","VtxOut");
  if(Node) vtxout = (VtxOut *)(Node->getData());
  Node = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","CglTrack");
  if(Node) cgltrack = (CglTrack *)(Node->getData());
  Node = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","BbcRaw");
  if(Node) bbcraw = (BbcRaw *)(Node->getData());
  Node = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode","DchTrack");
  if(Node) dchtrack = (DchTrack *)(Node->getData());  

  if(!bbcraw  ) { cerr  << PHWHERE <<" BbcRaw node not found " << endl; return ;}
  if(!cgltrack) { cerr  << PHWHERE <<" BbcRaw node not found " << endl; return ;}
  if(!bbcout  ) { cerr  << PHWHERE <<" BbcRaw node not found " << endl; return ;}
  if(!vtxout  ) { cerr  << PHWHERE <<" BbcRaw node not found " << endl; return ;}
  if(!vtxout  ) { cerr  << PHWHERE <<" BbcRaw node not found " << endl; return ;}


  float charge,psiObs,finalpsi;
  int ipmt;

  chargesum = (bbcout->get_ChargeSum(Bbc::North)) + (bbcout->get_ChargeSum(Bbc::South));
  ntrack = cgltrack->get_CglNTrack();

  TriggerHelper myTriggerHelper(topNode);   //isEventMinBias().... for HUMANS
   
  //first minbias.....
  minbias=myTriggerHelper.IsEventMinBias();
  float bbczps=vtxout->get_BbcVertex();

  cent=0;

  if(pass!=3){
    if ((fabs(bbczps)>50)||(!(minbias==1))) return; // || (!minbias)) return;
    if (chargesum>(50.0+ntrack*7.0) || chargesum<(-50.0+ntrack*2.0)) return;
    if (ntrack<5) return;
  }

  for(ipmt=0; ipmt<128; ipmt++){
    adc[ipmt]=bbcraw->get_Adc(ipmt); 
    tdc[ipmt]=bbcraw->get_Tdc0(ipmt); 
  } 

  cent = PhUtilities::getCentralityByClock(bbcout, zdcout);
  z=bbczps;


  icent = GetCentralityBin(cent);
  izps  = GetZBin(bbczps);
  //cout << icent << endl;
  if (pass==0) { // fill gain corrections(bbc only right now)
    for (ipmt=0; ipmt<128; ipmt++) {
      if ((tdc[ipmt]<oflw[ipmt]-10)){
	charge=(adc[ipmt]-pede[ipmt])*qgai[ipmt]/gain[ipmt];
	Hpmt[icent][izps]->Fill(ipmt,charge);
	Hrad[icent][izps]->Fill(irad[ipmt],charge);
      }
    } 
  }

  if (pass>0){
    //    cout << "pass>0" << endl;
    InitializeWeights();
    CalcWeightedSummandBBC(icent,izps,bbcraw);
    CalcWeightedSumandCNT(icent,izps,dchtrack);
  }
  
  if(pass==1){//fill offset correction for all detectors
    for (idet=0; idet<ndet; idet++) {
      for (ihar=0; ihar<nhar; ihar++) {
	if (sumw[idet][ihar]!=0) {
	  //	  cout << " fill histo" << endl;
	  Hsum[idet][ihar][icent][izps]->Fill(1,sumx[idet][ihar]);
	  Hsum[idet][ihar][icent][izps]->Fill(2,sumy[idet][ihar]);
	}
      }
    }
  }

  if (pass==2) {//fill flatting correction for all detectors
    for (idet=0; idet<ndet; idet++) {
      for (ihar=0; ihar<nhar; ihar++) {
	
	if (sumw[idet][ihar]!=0) {
	  psiObs=OffsetCorrected(sumy[idet][ihar],
				 sumx[idet][ihar],
				 ofsy[idet][ihar][icent][izps],
				 ofsx[idet][ihar][icent][izps],
				 ihar+1);
	  FillFlatteningHistograms(psiObs,idet,ihar,icent,izps);
	}
      }
    }
  }

  if(pass==3){

    if (CheckByOneZ) {
      izps--;
      if (izps<0) izps=nzps-1;
    }
    if (CheckByOneCent) {
      icent--;
      if (icent<0) icent=ncent-1;
    }
    

    //calculate reaction plane
    for (idet=0; idet<ndet; idet++) {
      for (ihar=0; ihar<nhar; ihar++) {
	if (sumw[idet][ihar]!=0) {
	  psiObs=OffsetCorrected(sumy[idet][ihar],
				 sumx[idet][ihar],
				 ofsy[idet][ihar][icent][izps],
				 ofsx[idet][ihar][icent][izps],
				 ihar+1);
	  psiobs[idet][ihar]=psiObs;
	  if ((ihar==1)&&fabs(psiObs*2)>(acos(-1.0)+0.01)) { cout << "psiObs=" << psiObs << endl; exit(1); }
 
	  //if (Hphi) Hphi[idet][ihar][icent][izps]->Fill(psiObs);
	  finalpsi=Flattened(psiObs,idet,ihar,icent,izps);
	  psi[idet][ihar]=finalpsi;
	  //if (Hphj) Hphj[idet][ihar][icent][izps]->Fill(psi[idet][ihar]);
	} else {
	  psi[idet][ihar]=-10000;
	}
      }
    }
  }
}

float utiReactionPlane::Flattened(float phi, int idet,int ihar,int icent,int izps){
  // float ans;
  float Delta=0;
  float AVGSIN,AVGCOS;
  int i;
  for (i=1; i<(nOrd+1); i++) {
    AVGSIN=sintable[idet][ihar][icent][izps][i-1];
    AVGCOS=costable[idet][ihar][icent][izps][i-1];
    Delta+=(-AVGSIN*cos(phi*(ihar+1)*i)+AVGCOS*sin(phi*(ihar+1)*i))/i;
  }
  //ans=atan2(sin(phi+2*Delta/(ihar+1)),cos(phi+2*Delta/(ihar+1)))/(ihar+1); 
  //if ((ihar+1)==2) ans=atan2(sin(ans*2),fabs(cos(2*ans)));
  return phi+2*Delta/(ihar+1);
}

void utiReactionPlane::FillFlatteningHistograms(float psiObs,int idet, int ihar, int icent, int izps){
  int i;
  for (i=0; i<nOrd; i++) {
    Hprc[idet][ihar][icent][izps]->Fill(i+1,cos((ihar+1)*psiObs*(i+1)));
    Hprs[idet][ihar][icent][izps]->Fill(i+1,sin((ihar+1)*psiObs*(i+1)));
  }
}


float utiReactionPlane::OffsetCorrected(float y, float x, float cy, float cx, int har){
  return atan2(y-cy,x-cx)/har;
}

void utiReactionPlane::InitializeWeights() {
  int idet,ihar;
  for (idet=0; idet<ndet; idet++) {
    for (ihar=0; ihar<nhar; ihar++) {
      sumx[idet][ihar] = 0;
      sumy[idet][ihar] = 0;
      sumw[idet][ihar] = 0;
    }
  }
}

void utiReactionPlane::CalcWeightedSummandBBC(int icent,int izps, BbcRaw *bbcraw){
  int ipmt;
  float adc, tdc;
  float corcharge;
  float wh;
  int ihar;
  //int MAXpmt=-1;
  //float MAXwh=0;
  for (ipmt=0; ipmt<128; ipmt++) {
    adc=bbcraw->get_Adc(ipmt); 
    tdc=bbcraw->get_Tdc0(ipmt);
    if (tdc<oflw[ipmt]-10) {
      charge[ipmt]=(adc-pede[ipmt])*qgai[ipmt]/gain[ipmt];
      ccharge[ipmt]=charge[ipmt]*fac[icent][izps][ipmt];
      corcharge=ccharge[ipmt];
      float phk= phipos[ipmt];
      for (ihar=0; ihar<nhar; ihar++) {
	//     South(<64) North 
	//pm    +1          -1
	//idet   0           1
	// ihar=0->wt    c           -c
	// ihar=1->wt    c	        c  
	if(ihar%2==0&&ipmt>=64) wh = -corcharge;
	else wh = corcharge;
	if (ipmt<64) {
	  //south
	  sumx[0][ihar]+=wh*cos(((float)(ihar+1))*phk);
	  sumy[0][ihar]+=wh*sin(((float)(ihar+1))*phk);
	  sumw[0][ihar]+=wh;  
	} else {
	  //north
	  sumx[1][ihar]+=wh*cos(((float)(ihar+1))*phk);
	  sumy[1][ihar]+=wh*sin(((float)(ihar+1))*phk);
	  sumw[1][ihar]+=wh;  
	}
	//both
	sumx[2][ihar]+=wh*cos(((float)(ihar+1))*phk);
	sumy[2][ihar]+=wh*sin(((float)(ihar+1))*phk);
	sumw[2][ihar]+=wh;  
      }
    }
  }
  //  cout << MAXpmt << " "  << MAXwh << " " << phipos[MAXpmt] << endl;
  
}

void utiReactionPlane::CalcWeightedSumandCNT(int icent,int izps, DchTrack * dchtrack){
  int ntrack=dchtrack->get_DchNTrack();
  int it,quality,iphi,index,ihar;
  float wh,ptot,phi,theta;
  for(it=0; it<ntrack; it++){
    ptot=dchtrack->get_momentum(it);
    phi=dchtrack->get_phi0(it);
    theta=dchtrack->get_theta0(it);
    quality=dchtrack->get_quality(it);
    
    if (theta>-1000 && phi>-1000 && ptot<5&&(quality>31)) {
      
      iphi=(int)((dchtrack->get_zed(it)+80.0)*20.0/160.0);  
      index=iphi%4+4;
	
      for (ihar=0; ihar<nhar; ihar++) {
	if (ihar%2==0&&theta<M_PI_2) wh = -1;
	else wh = 1;
	sumx[3][ihar] += wh*cos((float)(ihar+1)*phi);
	sumy[3][ihar] += wh*sin((float)(ihar+1)*phi);
	sumw[3][ihar] += wh;  
	
	sumx[index][ihar] += wh*cos((float)(ihar+1)*phi);
	sumy[index][ihar] += wh*sin((float)(ihar+1)*phi);
	sumw[index][ihar] += wh;  
      }
    }
  }
}




void utiReactionPlane::finishpass(int pass){//for calibration only
  if(pass==0){
    float norm;
    for (int iz=0; iz<nzps; iz++) {
      for (int im=0; im<ncent; im++) {
	for (int ip=0; ip<128; ip++) {
	  fac[im][iz][ip]=Hpmt[im][iz]->GetBinContent(ip+1);
	  norm           =Hrad[im][iz]->GetBinContent(irad[ip]+1);
	  if (fac[im][iz][ip]!=0) { 
	    fac[im][iz][ip]=norm/fac[im][iz][ip];
	  } else {
	    fac[im][iz][ip]=1.0;
 
	  }
	}
      }
    }    
  }
  if(pass==1){
    for (int iz=0; iz<nzps; iz++) {
      for (int im=0; im<ncent; im++) {
	for (int ih=0; ih<nhar; ih++) {
	  for (int ib=0; ib<ndet; ib++) {
            ofsx[ib][ih][im][iz]=Hsum[ib][ih][im][iz]->GetBinContent(1);
            ofsy[ib][ih][im][iz]=Hsum[ib][ih][im][iz]->GetBinContent(2);
	  }
	}
      }
    }
  }
  if(pass==2){
    for (int iz=0; iz<nzps; iz++) {
      for (int im=0; im<ncent; im++) {
	for (int ih=0; ih<nhar; ih++) {
	  for (int ib=0; ib<ndet; ib++) {
	    for (int ia=0; ia<nOrd; ia++) {
	      costable[ib][ih][im][iz][ia]=Hprc[ib][ih][im][iz]->GetBinContent(ia+1);
	      sintable[ib][ih][im][iz][ia]=Hprs[ib][ih][im][iz]->GetBinContent(ia+1);
	    }
	  }
	}
      }
    }
  }
}
void utiReactionPlane::makeHisto()
{
  cout << "info. utiReactionPlane::makeHisto" << endl;
  // hfile = new TFile("reactionPlane.root","recreate");
  // hfile->cd();
  char name[20];
  float pi=acos(-1.0);
  for (int iz=0; iz<nzps; iz++) { 
    for (int im=0; im<ncent; im++) { 
      for (int ih=0; ih<nhar; ih++) { 
        for (int ib=0; ib<ndet; ib++) {
          sprintf(name,"phi%d%d%d%d",ib,ih,im,iz);
          Hphi[ib][ih][im][iz] = new TH1F(name,name, 50,-pi,pi);
          sprintf(name,"phj%d%d%d%d",ib,ih,im,iz);
          Hphj[ib][ih][im][iz] = new TH1F(name,name, 50,-pi,pi);
          sprintf(name,"prc%d%d%d%d",ib,ih,im,iz);
          Hprc[ib][ih][im][iz] = 
          new TProfile(name,name,nOrd,0.5,nOrd+0.5,-1.1,1.1);
          sprintf(name,"prs%d%d%d%d",ib,ih,im,iz);
          Hprs[ib][ih][im][iz] = 
          new TProfile(name,name,nOrd,0.5,nOrd+0.5,-1.1,1.1);
          sprintf(name,"sum%d%d%d%d",ib,ih,im,iz);
          Hsum[ib][ih][im][iz] = 
          new TProfile(name,name,2,0.5,2.5,-4000,4000);
        }
      }
      sprintf(name,"pmt%d%d",im,iz);
      Hpmt[im][iz] = new TProfile(name,name,128,-0.5,127.5,-100.0,100.0);
      sprintf(name,"rad%d%d",im,iz);
      Hrad[im][iz] = new TProfile(name,name,14,-0.5,13.5,-100.0,100.0);
    }
  }
  int nbin=(ncent)*nzps;
  for (int ih=0; ih<ncent; ih++) { 
    for (int iz=0; iz<nzps; iz++) { 
      sprintf(name,"cor%d%d",iz,ih);
      Hcor[ih][iz] = new TH2F(name,name, 50,-pi,pi,50,-pi,pi);
    }
    sprintf(name,"rsp%d",ih);
    Hrsp[ih] = new TH2F    (name,name, nbin,-0.5,nbin-0.5,50,-pi,pi);
    sprintf(name,"rsc%d",ih);
    Hrsc[ih] = new TProfile(name,name, nbin,-0.5,nbin-0.5,-1.1,1.1);
    sprintf(name,"rss%d",ih);
    Hrss[ih] = new TProfile(name,name, nbin,-0.5,nbin-0.5,-1.1,1.1);
  }
}
void utiReactionPlane::deleteHisto(){
 cout << "info. utiReactionPlane::deleteHisto" << endl;
  for (int iz=0; iz<nzps; iz++) { 
    for (int im=0; im<ncent; im++) { 
      for (int ih=0; ih<nhar; ih++) { 
        for (int ib=0; ib<ndet; ib++) {
          if(Hphi[ib][ih][im][iz]) delete Hphi[ib][ih][im][iz];
          if(Hphj[ib][ih][im][iz]) delete Hphj[ib][ih][im][iz];
          if(Hprc[ib][ih][im][iz]) delete Hprc[ib][ih][im][iz];
          if(Hprs[ib][ih][im][iz]) delete Hprs[ib][ih][im][iz];
          if(Hsum[ib][ih][im][iz]) delete Hsum[ib][ih][im][iz];
        }
      }
      if(Hpmt[im][iz]) delete Hpmt[im][iz];
      if(Hrad[im][iz]) delete Hrad[im][iz];
    }
  }
  for (int ih=0; ih<ncent; ih++) { 
    for (int iz=0; iz<nzps; iz++) { 
      if(Hcor[ih][iz]) delete Hcor[ih][iz];
    }
    if(Hrsp[ih]) delete  Hrsp[ih];
    if(Hrsc[ih]) delete  Hrsc[ih];
    if(Hrss[ih]) delete  Hrss[ih];
  }

}
float utiReactionPlane::getReacPlane(int idet, int ihar){
  float ans;
  ans=psi[idet][ihar];
  if (ihar==1) {
    ans=psi[idet][ihar];
    if (ans>(M_PI_2)) ans=ans-M_PI;
    if (ans<(-M_PI_2)) ans=ans+M_PI;
  }
  return ans;
}
float utiReactionPlane::getObsReacPlane(int idet, int ihar){
  return psiobs[idet][ihar];
}
void utiReactionPlane::setincalibname(const char*name){
  sprintf(incalibname,"%s",name);
}
void utiReactionPlane::setoutcalibname(const char*name){
  sprintf(outcalibname,"%s",name);
}


void utiReactionPlane::setOffByOneZ(bool yesno){
  CheckByOneZ=yesno;
}



void utiReactionPlane::setOffByOneCent(bool yesno){
  CheckByOneCent=yesno;
}


void utiReactionPlane::savetofile(const char*name){
  TFile* f = new TFile(name,"RECREATE");
  for (int iz=0; iz<nzps; iz++) { 
    for (int im=0; im<ncent; im++) { 
      for (int ih=0; ih<nhar; ih++) { 
        for (int ib=0; ib<ndet; ib++) {
          Hphi[ib][ih][im][iz]->Write();
          Hphj[ib][ih][im][iz]->Write();
          Hprc[ib][ih][im][iz]->Write();
          Hprs[ib][ih][im][iz]->Write();
          Hsum[ib][ih][im][iz]->Write();
        }
      }
      Hpmt[im][iz]->Write();
      Hrad[im][iz]->Write();
    }
  }
  for (int ih=0; ih<ncent; ih++) { 
    for (int iz=0; iz<nzps; iz++) { 
      Hcor[ih][iz]->Write();
    }
    Hrsp[ih]->Write();
    Hrsc[ih]->Write();
    Hrss[ih]->Write();
  }
  f->Close();
}

int utiReactionPlane::GetNCent()
{
  return ncent;
}

int utiReactionPlane::GetCent()
{
  return cent;
}

float utiReactionPlane::GetZ()
{
  return z;
}


float utiReactionPlane::GetCharge()
{
  return chargesum;
}

bool utiReactionPlane::GetMinBias()
{
  return minbias;
}

int utiReactionPlane::GetNtrack()
{
  return ntrack;
}

int utiReactionPlane::GetNZps()
{
  return nzps;
}

float utiReactionPlane::GetFactor(int izps, int icent, int ipmt)
{
  return fac[icent][izps][ipmt];
}

float utiReactionPlane::GetCharge(int izps, int icent, int ipmt)
{
  return Hpmt[icent][izps]->GetBinContent(ipmt+1);
}

	
void utiReactionPlane::FillEPass1(float marray[400])
{
   int e;
  int m=0;
  marray[m++]=izps;
  marray[m++]=icent;
  marray[m++]=z;
  marray[m++]=cent;
  for (e=0; e<128; e++) {
    marray[m++]=charge[e];
  }
  for (e=0; e<128; e++) {
    marray[m++]=ccharge[e];
  }
  marray[m++]=sumx[2][1];
  marray[m++]=sumy[2][1];
  marray[m++]=sumw[2][1];
  marray[m++]=sumx[1][1];
  marray[m++]=sumy[1][1];
  marray[m++]=sumw[1][1];
  marray[m++]=sumx[0][1];
  marray[m++]=sumy[0][1];
  marray[m++]=sumw[0][1];

}

float utiReactionPlane::GetWeightedSummandX(int idet, int ihar)
{
  return sumx[idet][ihar];
}


float utiReactionPlane::GetWeightedSummandY(int idet, int ihar)
{
  return sumy[idet][ihar];
}

