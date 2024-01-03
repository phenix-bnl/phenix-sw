#include <fstream>
#include "emCalibMaster.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"

#include "TMath.h"

#include "phool.h"
#include "TOAD.h"
#include "getClass.h"
#include "PHGlobal.h"
#include "Fun4AllReturnCodes.h"
#include "RunHeader.h"

#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "EmcIndexer.h"

using namespace findNode;

emCalibMaster::emCalibMaster()
{
  fT0=0;
  fVZ=0;

  for(int i=0; i!=8; ++i) {
    fEMCTOF_s0[i] = 0;
    fEMCTOF_s1[i] = 0;
    fEMCTOF_s2[i] = 0;
    fEMCSector[i]=0;
  }
  for(int i=0; i!=24768; ++i) {
    fEMCTOF_lc[i] = -99999;
    for(int j=0; j!=3; ++j) {
      fEMCTOF_t0[i][j] = 0;
      if(j<2) fEMCTOF_t1[i][j] = 0;
    }
  }
  for(int i=0; i<8; i++)
    for(int j=0; j<48; j++)
      for(int k=0;k<96; k++)
	fEMCDead[i][j][k]=0;

  fEMCCont = NULL;
  fEMTCont = NULL;

}


void emCalibMaster::InitTables(PHCompositeNode *topNode) {

  // std::cout<<"\n I am inside InitTables";
  RunHeader *header = getClass<RunHeader>(topNode,"RunHeader");
  if(!header) std::cout << "can't find RunHeader" << std::endl;
  int runnumber=header->get_RunNumber();
  fEMTCont = getClass<emcTowerContainer>(topNode, "emcHitContainer");
  fEMCCont = getClass<emcClusterContainer>(topNode, "emcClusterContainer");

  if( runnumber >= 454774 && runnumber <= 455639) {
    Read_EMC_Files("Run16dAuEmcalDeadMap.txt","run_vs_ratiopersector.dat",runnumber,true);
    Read_EMCTOF_Tables(runnumber);
  }
}

int emCalibMaster::LoadGlobals(PHCompositeNode *topNode) {
  PHGlobal *phg = getClass<PHGlobal>(topNode,"PHGlobal");
  if(!phg) return DISCARDEVENT;
  fVZ = phg->getBbcZVertex();
  fT0 = phg->getBbcTimeZero();
  return EVENT_OK;
}


emCalibMaster::~emCalibMaster() {
}

//========================
void emCalibMaster::Read_EMCTOF_Tables(int run,TString sel) {
  TOAD toad_loader("Run16dAuPi0Photon");
  std::ifstream fin;
  //std::cout<<"\n I am inside Read_EMCTOF_Tables";
  //======= RUNxRUN =======
  TString table1 = toad_loader.location( Form("table1_TOF%s.txt",sel.Data()) );
  // std::cout<<"\n Name of the FILE ..."<<table1.Data();
  fin.open( table1.Data() );
  int tmp;
  float ftmp;
  for(;;) {
    fin >> tmp;
    if(!fin.good()) break;
    if(tmp==run) {
      fin >> fEMCTOF_s0[0] >> fEMCTOF_s0[1] >> fEMCTOF_s0[2] >> fEMCTOF_s0[3];
      fin >> fEMCTOF_s0[4] >> fEMCTOF_s0[5] >> fEMCTOF_s0[6] >> fEMCTOF_s0[7];
      fin >> fEMCTOF_s1[0] >> fEMCTOF_s1[1] >> fEMCTOF_s1[2] >> fEMCTOF_s1[3];
      fin >> fEMCTOF_s1[4] >> fEMCTOF_s1[5] >> fEMCTOF_s1[6] >> fEMCTOF_s1[7];
      break;
    } else {
      fin >> ftmp >> ftmp >> ftmp >> ftmp >> ftmp >> ftmp >> ftmp >> ftmp;
      fin >> ftmp >> ftmp >> ftmp >> ftmp >> ftmp >> ftmp >> ftmp >> ftmp;
    }
  }
  fin.close();


  std::ifstream fin2;
  //Extra Calibration, which fixes the run dependance to the timing issue for every sector. Similar to fEMCTOF_s1. 
  TString table3 = toad_loader.location( Form("tableNR_TOF%s.txt",sel.Data()) );
  // std::cout<<"\n Name of the FILE ..."<<table1.Data();
  fin2.open( table3.Data() );
  int tmp2;
  float ftmp2;
  for(;;) {
    fin2 >> tmp2;
    if(!fin.good()) break;
    if(tmp2==run) {
      fin2 >> fEMCTOF_s2[0] >> fEMCTOF_s2[1] >> fEMCTOF_s2[2] >> fEMCTOF_s2[3];
      fin2 >> fEMCTOF_s2[4] >> fEMCTOF_s2[5] >> fEMCTOF_s2[6] >> fEMCTOF_s2[7];
      //  std::cout<<"********\n\n"<<run<<"\t"<<fEMCTOF_s2[0]<<"\t"<<fEMCTOF_s2[1]<<"\t"<<fEMCTOF_s2[2]<<"\t"<<fEMCTOF_s2[3]<<"\t"<<fEMCTOF_s2[4]<<"\t"<<fEMCTOF_s1[0]<<"\n\n**********\n\n";
      break;
    } else {
      fin2 >> ftmp2 >> ftmp2 >> ftmp2 >> ftmp2 >> ftmp2 >> ftmp2 >> ftmp2 >> ftmp2;
      }
  }
  fin2.close();

 
  // std::cout << "TOF sector calib: for run " << run << std::endl;
  for(int i=0; i!=8; ++i) std::cout << " " << Form("%.1f",fEMCTOF_s0[i]);
  std::cout << " |";
  for(int i=0; i!=8; ++i) std::cout << " " << Form("%.1f",fEMCTOF_s1[i]);
  std::cout << std::endl;

  //======= TOWERID =======
  TString table2 = toad_loader.location( Form("table2_TOF%s.txt",sel.Data()) );
  fin.open( table2.Data() );
  int k;
  for(;;) {
    fin >> k;
    if(!fin.good()) break;
    fin >> fEMCTOF_t0[k][0] >> fEMCTOF_t0[k][1] >> fEMCTOF_t0[k][2];
    fin >> fEMCTOF_t1[k][0] >> fEMCTOF_t1[k][1] >> fEMCTOF_lc[k];
  }
  fin.close();
}
//========================
void emCalibMaster::Read_EMC_Files(TString deadmap, TString gainmap, int trun, bool rvc) {
  TOAD toad_loader("Run16dAuPi0Photon");
  
  //======= DEADMAP =======
  TString dead = toad_loader.location( deadmap.Data() );
  std::ifstream fdead( dead.Data() );
  if(!fdead.is_open()) exit(0);
  int sect = 0, ypos = 0, zpos = 0, status = 0;
  while(fdead >> sect >> ypos >> zpos >> status) {
    int sector = sect;
    
    if(rvc) { // needed for dAu since it was done with Veronica's convention
      if(sect>3) {
        sector = 7 - sect;
      }
    }
    fEMCDead[sector][ypos][zpos] = status;
  }
  fdead.close();

  //======= GAINMAP =======
  TString gain = toad_loader.location( gainmap.Data() );
  std::ifstream fgain( gain.Data() );
  if(!fgain.is_open()) exit(0);
  int run;
  float s[8];
  while(fgain >> run >>s[0]>>s[1]>>s[2]>>s[3]>>s[4]>>s[5]>>s[6]>>s[7]) {
    if(trun==run) {
     
      fEMCSector[0] = s[0];
      fEMCSector[1] = s[1];
      fEMCSector[2] = s[2];
      fEMCSector[3] = s[3];
      fEMCSector[4] = s[4];
      fEMCSector[5] = s[5];
      fEMCSector[6] = s[6];
      fEMCSector[7] = s[7];
    }
  }

  return;
}
//========================
float emCalibMaster::NLC_EMC_PbSc(float ene) {
  if(ene<0.01) return 0;
  if(ene>100.) return 0;
  return 0.003+(1-0.010/ene);
}

//========================
float emCalibMaster::NLC_EMC_PbGl(float ene) {
  if(ene<0.01) return 0;
  if(ene>100.) return 0;
  return 0.021+(1-0.020/ene);
}
//========================
float emCalibMaster::ECoreCorr(emcClusterContent *emc) {
  if(!emc) return 0;
  int arm = emc->arm(); // in EMC arm=0 is (+x) and arm=1 is (-x)
  int mysector = arm*4+emc->sector(); // 0-3 PbSc || 4-5 PbGl || 6-7 PbSc
  float ecore_ori = emc->ecore();
  bool isPbSc = true;
  if(mysector==4||mysector==5) isPbSc = false;
  float ecore;
  if(isPbSc)
    ecore = ecore_ori/NLC_EMC_PbSc(ecore_ori) * fEMCSector[mysector];
  else
    ecore = ecore_ori/NLC_EMC_PbGl(ecore_ori) * fEMCSector[mysector];
  return ecore;
}
//========================
float emCalibMaster::TOF(emcClusterContent *emc,float len) {
  float tof = -99999;
  if(!emc) return tof;
  if(!fEMTCont) return tof;
  const double c = 29.979245829979; //[cm/ns]
  emcTowerContent *emt;
  int tid = emc->towerid(0);
  int isc, iz, iy;
  EmcIndexer::decodeTowerId(tid,isc,iz,iy);
  float res = fEMCTOF_t1[tid][1];
  if(res>0.2&&res<8.2) tof = 99999;
  if(tof>0) {
    float TDC=0;
    float ADC=0;
    for(uint dumb=0; dumb!=fEMTCont->size(); ++dumb) {
      emt = fEMTCont->getTower(dumb);
      if( tid!=emt->towerid() ) continue;
      TDC = emt->TDC();
      ADC = emt->ADC();
      break;
    }
    //int ifem, ichn;
    //EmcIndexer::PXPXSM144CH(tid,ifem,ichn);
    //float LC = fCDH->getCalibration(ifem,"LCTofs")->getValueFast(ichn,0);
    //LC =((LC>25.&&LC<65.)?LC:40.0)/1000.;
    float LC = fEMCTOF_lc[tid];
    if(len<0) { //ok, recomputing (expensive)
      float x = emc->x(); // in cm
      float y = emc->y(); // in cm
      float z = emc->z()-fVZ; // in cm
      len = TMath::Sqrt( x*x + y*y + z*z );
    }
    float LIGHT=len/c; // ns
    float TDCCOR = TDC -fEMCTOF_t0[tid][1]/ADC -fEMCTOF_t0[tid][2]/ADC/ADC;
    TDCCOR = TDCCOR -fEMCTOF_t0[tid][0] +2000 -fEMCTOF_s0[isc];
    tof=-LC*TDCCOR -LIGHT -fT0 -fEMCTOF_s1[isc] -fEMCTOF_t1[tid][0] -fEMCTOF_s2[isc];
  }
  return tof;
}
//========================
float emCalibMaster::GetECore(int id) {
  float ecore = -1;
  if(!fEMCCont) return ecore;
  emcClusterContent *emc;
  int nclu = fEMCCont->size();
  for(int i=0; i!=nclu; ++i) {
    emc = fEMCCont->getCluster(i);
    if(emc->id()!=id) continue;
    ecore = ECoreCorr(emc);
    break;
  }
  return ecore;
}
//========================
float emCalibMaster::GetETOF(int id, float len, int &tid) {
  float etof = -99999;
  tid = -99999;
  if(!fEMCCont) return etof;
  emcClusterContent *emc;
  int nclu = fEMCCont->size();
  for(int i=0; i!=nclu; ++i) {
    emc = fEMCCont->getCluster(i);
    if(emc->id()!=id) continue;
    etof = TOF(emc,len);
    tid = emc->towerid(0);
    break;
  }
  return etof;
}

/*
USAGE
=====

emcClusterContent *emc;
int nclu = fEMCCont->size();

for(int i=0; i!=nclu; ++i) {
  emc = fEMCCont->getCluster(i);
  int arm = emc->arm(); // in EMC arm=0 is (+x) and arm=1 is (-x)
  int sect = emc->sector(); // in EMC sector goes from 0 to 3 bottom to top in y
  int mysector = arm*4+sect; // 0-3 PbSc || 4-5 PbGl || 6-7 PbSc               
  float x = emc->x(); // in cm
  float y = emc->y(); // in cm
  float z = emc->z()-fVZ; // in cm
  float len = TMath::Sqrt( x*x + y*y + z*z );
  float tof = TOF(emc,len);
*/
