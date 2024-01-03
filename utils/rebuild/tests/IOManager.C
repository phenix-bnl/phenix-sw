// This macro handle setting all of the output nodes for the various types 
// of output files.

#include <stdio.h> 
#include <time.h> 
#include <string>
#include <vector>

vector<string> Output_Files;
vector<string> Output_Types;

string OUTPUT;
string EXTN;
string FILE;

void addCommon(Fun4AllDstOutputManager *manager)
{
  manager->AddEventSelector("PADREMOVE");
  manager->AddNode("EventHeader");
  manager->AddNode("Sync");
  manager->AddNode("TrigLvl1");
  manager->AddNode("PreviousEvent");

  manager->AddNode("L2Decision");
  manager->AddNode("Lvl2OutArray");
}

void MakeOutput(const char* path="./",const char* dir="", const char* NUM="", const char *file ="")
{
  string PATH(path);
  string DIR(dir);
  string FILE(file);
  string NUMBER(NUM);
  string SYST="_run5CuCu_62GeV_pro70";
  string EXTN=".root";
  OUTPUT = PATH + DIR + FILE + SYST + NUM + EXTN;

  Output_Files.push_back(OUTPUT);
  Output_Types.push_back(FILE);

  char cmd[1000];
  sprintf(cmd,"mkdir -p %s",(PATH+DIR).c_str());
  gSystem->Exec(cmd);
}


void DST_EVE_NodeAdd(Fun4AllDstOutputManager *manager)
{
  manager->AddNode("T0Out");
  manager->AddNode("VtxOut");
  manager->AddNode("BbcOut");
  manager->AddNode("BbcRaw");
  manager->AddNode("ZdcOut");
  manager->AddNode("ZdcRaw");
  manager->AddNode("SmdOut");
  manager->AddNode("fclOutNorth");
  manager->AddNode("fclOutSouth");
  manager->AddNode("fclRawNorth");
  manager->AddNode("fclRawSouth");
}

void DST_EVE_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_EVE_MinBias")
{

  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_EVE_MinBias_Manager  = new Fun4AllDstOutputManager("DST_EVE_MinBias_OUT",OUTPUT.c_str());

  dst_EVE_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_EVE_MinBias_Manager);
  DST_EVE_NodeAdd(dst_EVE_MinBias_Manager);
  se->registerOutputManager(dst_EVE_MinBias_Manager);
}



void DST_EVE_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_EVE_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_EVE_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_EVE_OtherTrig_OUT",OUTPUT.c_str());

  dst_EVE_OtherTrig_Manager->AddEventSelector("OTHERS");
  addCommon(dst_EVE_OtherTrig_Manager);
  DST_EVE_NodeAdd(dst_EVE_OtherTrig_Manager);
  se->registerOutputManager(dst_EVE_OtherTrig_Manager);
}


void DST_DCHHit_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_DCHHit_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_DCHHit_MinBias_Manager  = new Fun4AllDstOutputManager("DST_DCHHit_MinBias_OUT",OUTPUT.c_str());

  dst_DCHHit_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_DCHHit_MinBias_Manager);
  dst_DCHHit_MinBias_Manager->AddNode("DchHitLineTable");

  se->registerOutputManager(dst_DCHHit_MinBias_Manager);
}



void DST_DCHHit_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_DCHHit_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_DCHHit_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_DCHHit_OtherTrig_OUT",OUTPUT.c_str());

  dst_DCHHit_OtherTrig_Manager->AddEventSelector("OTHERS");
  addCommon(dst_DCHHit_OtherTrig_Manager);
  dst_DCHHit_OtherTrig_Manager->AddNode("DchHitLineTable");

  se->registerOutputManager(dst_DCHHit_OtherTrig_Manager);
}



void DST_DCHTrk_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_DCHTrk_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_DCHTrk_MinBias_Manager  = new Fun4AllDstOutputManager("DST_DCHTrk_MinBias_OUT",OUTPUT.c_str());

  dst_DCHTrk_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_DCHTrk_MinBias_Manager);
  dst_DCHTrk_MinBias_Manager->AddNode("DchTrack");

  se->registerOutputManager(dst_DCHTrk_MinBias_Manager);
}

void DST_DCHTrk_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_DCHTrk_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_DCHTrk_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_DCHTrk_OtherTrig_OUT",OUTPUT.c_str());

  dst_DCHTrk_OtherTrig_Manager->AddEventSelector("OTHERS");
  addCommon(dst_DCHTrk_OtherTrig_Manager);
  dst_DCHTrk_OtherTrig_Manager->AddNode("DchTrack");
  se->registerOutputManager(dst_DCHTrk_OtherTrig_Manager);
}

void DST_PAD_NodeAdd(Fun4AllDstOutputManager *manager)
{
  manager->AddNode("Pc1Cluster");
  manager->AddNode("Pc2Cluster");
  manager->AddNode("Pc3Cluster");
  manager->AddNode("Pc1Raw");
  manager->AddNode("Pc2Raw");
  manager->AddNode("Pc3Raw");
}

void DST_PAD_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_PAD_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_PAD_MinBias_Manager  = new Fun4AllDstOutputManager("DST_PAD_MinBias_OUT",OUTPUT.c_str());

  dst_PAD_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_PAD_MinBias_Manager);
  DST_PAD_NodeAdd(dst_PAD_MinBias_Manager);
  se->registerOutputManager(dst_PAD_MinBias_Manager);
}


void DST_PAD_OtherTrig_IOManager(const char* path="./", const char* NUM="", const char *file = "DST_PAD_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_PAD_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_PAD_OtherTrig_OUT",OUTPUT.c_str());

  dst_PAD_OtherTrig_Manager->AddEventSelector("OTHERS");
  addCommon(dst_PAD_OtherTrig_Manager);
  DST_PAD_NodeAdd(dst_PAD_OtherTrig_Manager);
  se->registerOutputManager(dst_PAD_OtherTrig_Manager);
}

void DST_CRK_NodeAdd(Fun4AllDstOutputManager *manager)
{
  manager->AddNode("CrkHit");
  manager->AddNode("CrkRing");
  manager->AddNode("CrkRingBack");
}

void DST_CRK_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_CRK_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_CRK_MinBias_Manager  = new Fun4AllDstOutputManager("DST_CRK_MinBias_OUT",OUTPUT.c_str());

  dst_CRK_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_CRK_MinBias_Manager);
  DST_CRK_NodeAdd(dst_CRK_MinBias_Manager);
  se->registerOutputManager(dst_CRK_MinBias_Manager);
}

void DST_CRK_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_CRK_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_CRK_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_CRK_OtherTrig_OUT",OUTPUT.c_str());

  addCommon(dst_CRK_OtherTrig_Manager);
  DST_CRK_NodeAdd(dst_CRK_OtherTrig_Manager);

  se->registerOutputManager(dst_CRK_OtherTrig_Manager);
}


void DST_TEC_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_TEC_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_TEC_MinBias_Manager  = new Fun4AllDstOutputManager("DST_TEC_MinBias_OUT",OUTPUT.c_str());

  dst_TEC_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_TEC_MinBias_Manager);
  dst_TEC_MinBias_Manager->AddNode("TecOut");

  se->registerOutputManager(dst_TEC_MinBias_Manager);
}

void DST_TEC_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_TEC_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_TEC_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_TEC_OtherTrig_OUT",OUTPUT.c_str());

  dst_TEC_OtherTrig_Manager->AddEventSelector("OTHERS");
  addCommon(dst_TEC_OtherTrig_Manager);
  dst_TEC_OtherTrig_Manager->AddNode("TecOut");

  se->registerOutputManager(dst_TEC_OtherTrig_Manager);
}


void DST_TOF_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_TOF_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_TOF_MinBias_Manager  = new Fun4AllDstOutputManager("DST_TOF_MinBias_OUT",OUTPUT.c_str());

  dst_TOF_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_TOF_MinBias_Manager);
  dst_TOF_MinBias_Manager->AddNode("TofOut");

  se->registerOutputManager(dst_TOF_MinBias_Manager);
}


void DST_TOF_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_TOF_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_TOF_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_TOF_OtherTrig_OUT",OUTPUT.c_str());

  dst_TOF_OtherTrig_Manager->AddEventSelector("OTHERS");
  addCommon(dst_TOF_OtherTrig_Manager);
  dst_TOF_OtherTrig_Manager->AddNode("TofOut");

  se->registerOutputManager(dst_TOF_OtherTrig_Manager);
}


void DST_AERO_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_AERO_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_AERO_MinBias_Manager  = new Fun4AllDstOutputManager("DST_AERO_MinBias_OUT",OUTPUT.c_str());

  dst_AERO_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_AERO_MinBias_Manager);
  dst_AERO_MinBias_Manager->AddNode("AccRaw");

  se->registerOutputManager(dst_AERO_MinBias_Manager);
}


void DST_AERO_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_AERO_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_AERO_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_AERO_OtherTrig_OUT",OUTPUT.c_str());

  dst_AERO_OtherTrig_Manager->AddEventSelector("OTHERS");
  addCommon(dst_AERO_OtherTrig_Manager);
  dst_AERO_OtherTrig_Manager->AddNode("AccRaw");

  se->registerOutputManager(dst_AERO_OtherTrig_Manager);
}



void DST_EMCTwr_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_EMCTwr_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_EMCTwr_MinBias_Manager  = new Fun4AllDstOutputManager("DST_EMCTwr_MinBias_OUT",OUTPUT.c_str());

  dst_EMCTwr_MinBias_Manager->AddEventSelector("MB");
  addCommon(dst_EMCTwr_MinBias_Manager);
  dst_EMCTwr_MinBias_Manager->AddNode("emcTowerContainer");

  se->registerOutputManager(dst_EMCTwr_MinBias_Manager);
}


void DST_EMCTwr_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_EMCTwr_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_EMCTwr_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_EMCTwr_OtherTrig_OUT",OUTPUT.c_str());

  dst_EMCTwr_OtherTrig_Manager->AddEventSelector("OTHERS");
  addCommon(dst_EMCTwr_OtherTrig_Manager);
  dst_EMCTwr_OtherTrig_Manager->AddNode("emcTowerContainer");

  se->registerOutputManager(dst_EMCTwr_OtherTrig_Manager);
}

void DST_CGL_NodeAdd(Fun4AllDstOutputManager *manager)
{
  addCommon(manager);
  manager->AddNode("CglTrack");
  manager->AddNode("CglTrackBack");
  manager->AddNode("PHDchTrackOut");
  manager->AddNode("PHTrackOut");
  manager->AddNode("PHTrackOutBack");
}
  
void DST_CGL_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_CGL_MinBias")
{
  MakeOutput(path,"DST_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_CGL_MinBias_Manager  = new Fun4AllDstOutputManager("DST_CGL_MinBias_OUT",OUTPUT.c_str());

  dst_CGL_MinBias_Manager->AddEventSelector("MB");
  DST_CGL_NodeAdd(dst_CGL_MinBias_Manager);

  se->registerOutputManager(dst_CGL_MinBias_Manager);
}


void DST_CGL_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_CGL_OtherTrig")
{
  MakeOutput(path,"DST_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_CGL_OtherTrig_Manager  = new Fun4AllDstOutputManager("DST_CGL_OtherTrig_OUT",OUTPUT.c_str());

  dst_CGL_OtherTrig_Manager->AddEventSelector("OTHERS");
  DST_CGL_NodeAdd(dst_CGL_OtherTrig_Manager);

  se->registerOutputManager(dst_CGL_OtherTrig_Manager);
}

void CNT_NodeAdd(Fun4AllDstOutputManager *manager)
{
  addCommon(manager);
  manager->AddNode("PHCentralTrack");
  manager->AddNode("AccCluster");
  manager->AddNode("PHGlobal");
}

void CNT_MinBias_IOManager(const char* path="./", const char* NUM="", const char *file = "CNT_MinBias")
{
  MakeOutput(path,"CNT_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *cntMinBiasManager  = new Fun4AllDstOutputManager("CNT_MinBias",OUTPUT.c_str());

  cntMinBiasManager->AddEventSelector("MB");
  CNT_NodeAdd(cntMinBiasManager);

  se->registerOutputManager(cntMinBiasManager);
}


void CNT_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "CNT_OtherTrig")
{
  MakeOutput(path,"CNT_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *cntOtherTrigManager  = new Fun4AllDstOutputManager("CNT_OtherTrig",OUTPUT.c_str());

  cntOtherTrigManager->AddEventSelector("OTHERS");
  CNT_NodeAdd(cntOtherTrigManager);

  se->registerOutputManager(cntOtherTrigManager);
}

void EWG_NodeAdd(Fun4AllDstOutputManager *manager)
{
  addCommon(manager);
  manager->AddNode("EWGCentralTrack");
  manager->AddNode("PHGlobal");
}

void EWG_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "EWG_MinBias" )
{
  MakeOutput(path,"EWG_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();  
  Fun4AllDstOutputManager *ewgMinBiasManager  = new Fun4AllDstOutputManager("EWG_MinBias",OUTPUT.c_str());

  ewgMinBiasManager->AddEventSelector("MB");
  EWG_NodeAdd(ewgMinBiasManager);
  se->registerOutputManager(ewgMinBiasManager);
}

void compactEWG_NodeAdd(Fun4AllDstOutputManager *manager)
{
  addCommon(manager);
  manager->AddNode("cEWGCentralTrack");
  manager->AddNode("PHGlobal");
}

void compactEWG_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "compactEWG_MinBias")
{
  MakeOutput(path,"cEWG_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *compewgMinBiasManager  = new Fun4AllDstOutputManager("compactEWG_MinBias",OUTPUT.c_str());

  compewgMinBiasManager->AddEventSelector("MB");
  compactEWG_NodeAdd(compewgMinBiasManager);

  se->registerOutputManager(compewgMinBiasManager);
}

void HWG_NodeAdd(Fun4AllDstOutputManager *manager)
{
  addCommon(manager);
  manager->AddNode("HWGCentralTrack");
  manager->AddNode("PHGlobal");
}  

void HWG_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "HWG_MinBias")
{
  MakeOutput(path,"HWG_MB/",NUM,file);
 
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *hwgMinBiasManager  = new Fun4AllDstOutputManager("HWG_MinBias",OUTPUT.c_str());

  hwgMinBiasManager->AddEventSelector("MB");
  HWG_NodeAdd(hwgMinBiasManager);

  se->registerOutputManager(hwgMinBiasManager);
}

void PWG_NodeAdd(Fun4AllDstOutputManager *manager)
{
  addCommon(manager);
  manager->AddNode("emcClusterContainer");
  manager->AddNode("PHGlobal");
}  

void PWG_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "PWG_MinBias")
{
  MakeOutput(path,"PWG_MB/",NUM,file);
  
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *pwgMinBiasManager  = new Fun4AllDstOutputManager("PWG_MinBias",OUTPUT.c_str());

  pwgMinBiasManager->AddEventSelector("MB");
  PWG_NodeAdd(pwgMinBiasManager);

  se->registerOutputManager(pwgMinBiasManager);
}

void PWG_OtherTrig_IOManager(const char* path="./", const char* NUM="",const char *file = "PWG_OtherTrig")
{
  MakeOutput(path,"PWG_OT/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *pwgOtherTrigManager  = new Fun4AllDstOutputManager("PWG_OtherTrig",OUTPUT.c_str());

  pwgOtherTrigManager->AddEventSelector("OTHERS");
  PWG_NodeAdd(pwgOtherTrigManager);
  se->registerOutputManager(pwgOtherTrigManager);
}


void MWG_NodeAdd(Fun4AllDstOutputManager *manager)
{
  addCommon(manager);
  manager->AddNode("PHMuoTracks00");
  manager->AddNode("PHGlobal");
} 

void MWG_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "MWG_MinBias")
{
  MakeOutput(path,"MWG_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *mwgMinBiasManager  = new Fun4AllDstOutputManager("MWG_MinBias",OUTPUT.c_str());

  mwgMinBiasManager->AddEventSelector("MB");
  MWG_NodeAdd(mwgMinBiasManager);
  se->registerOutputManager(mwgMinBiasManager);
}


void HardpDST_NodeAdd(Fun4AllDstOutputManager *manager)
{
  addCommon(manager);
  manager->AddNode("EvPhCglList");
  manager->AddNode("EvPhPhotonList");
  manager->AddNode("TrPhCglList");
  manager->AddNode("TrPhPhotonList");
  manager->AddNode("PHGlobal");
}

void HardpDST_MinBias_IOManager(const char* path="./", const char* NUM="",const char *file = "Hard_MinBias")
{
  MakeOutput(path,"Hard_MB/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *HardMinBiasManager  = new Fun4AllDstOutputManager("Hard_MinBias",OUTPUT.c_str());
  HardMinBiasManager->AddEventSelector("MB");
  HardpDST_NodeAdd(HardMinBiasManager);

  se->registerOutputManager(HardMinBiasManager);
}

void KAL_MinBias_IOManager(const char* path="./", const char* NUM="", const char *file = "KAL")
{
  MakeOutput(path,"Kalman/",NUM,file);

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *Manager  = new Fun4AllDstOutputManager("Kalman",  OUTPUT.c_str());

  Manager->AddNode("PHCentralTrack");
  Manager->AddNode("KalFitOut");
  Manager->AddNode("PHGlobal");
  Manager->AddNode("TrigLvl1");
  Manager->AddNode("EventHeader");
  Manager->AddNode("Sync");

  se->registerOutputManager(Manager);
}


void RunSummary(const char* path="./", const char* NUM="", const char *file = "RunSummary")
{
  string PATH(path);
  string FILE(file);
  string NUMBER(NUM);
  string SYST="_run5CuCu_62GeV_pro70";
  string EXTN=".txt";
  string OUTPUT = FILE + SYST + NUM + EXTN;

  ofstream fp_out;
  fp_out.open(OUTPUT.c_str(), ios::out);
  fp_out << OUTPUT << endl;
  for (unsigned int i=0; i<Output_Files.size(); i++)
    {
      fp_out << Output_Files[i] << endl;
    }
  fp_out.close();

}

//________________________________________________________________________
void QA_IOManager(const char* path="./", const char* NUM="", const char *file = "qaRoot")
{
  MakeOutput(path,"qaRoot/",NUM,file);

  QAEnd(OUTPUT.c_str());
}

//________________________________________________________________________
void Mu_DST_IOManager(const char* path="./", const char* NUM="", const char *file = "Mu_DST_mutoo_ntuple")
{
  MakeOutput(path,"Mu_DST/",NUM,file);

  char cmd2[1000];
  sprintf(cmd2,"mv %s %s",(FILE + EXTN).c_str(),OUTPUT.c_str());
  gSystem->Exec(cmd2);

}

//________________________________________________________________________
void MuidEff_IOManager(const char* path="./", const char* NUM="", const char *file = "muid_effic_ntuple")
{

  MakeOutput(path,"Muid_eff/",NUM,file);

  char cmd2[1000];
  sprintf(cmd2,"mv %s %s",(FILE + EXTN).c_str(),OUTPUT.c_str());
  gSystem->Exec(cmd2);

}
