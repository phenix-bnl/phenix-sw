/*-----------------------------------------------------------------

o EMC Tower? Module? Tile?
  In this code, a tower means a PMT and a module means 2x2=4 PMTs. 
  A tile consists of 6x6=36 modules.
  The towerkey follows the PISA convention.

  towerkey = arm*100000+sector*10000+iy*100+iz; 

  - arm=0 for west, arm=1 for east arm. 
  - For each arm, bottom sector is 0 and the number increases as 
    to upwards (sector=0,1,2,3)
  - iy is the row number of the tower, it stars from 0 and 
    increases upwards wheresas iz is the column number of the 
    tower which starts from 0. It increases from left to right
    for both arms. For the different types of EMC supermodules
    iy and iz take the values;

    PbSc; iy; 0,1,....35	PbGl; iy;0,1,...77
          iz; 0,1,....71	      iz;0,1,...95

-------------------------------------------------------------------*/

#include "ERTSimulator.h"
#include "ErtUtils.h"
#include "dErtFemDataWrapper.h"
#include "dErtFemData.h"
#include "ErtOut.h"
#include "ErtOutv1.h"

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHTimeStamp.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbErtDecode.hh>
#include <PdbErtSMEff.hh>
#include <PdbErtSMMask.hh>
#include <PdbParameter.hh>
#include <PdbCalBank.hh>
#include <PdbBankList.hh>
#include <PdbBankListIterator.hh>

#include <dEmcRawDataWrapper.h>
#include <dCrkHitWrapper.h>
#include <dCrkRawWrapper.h>
#include <dCrkRawHitParWrapper.h>
#include <dCrkCalWrapper.h>
#include <dCrkHit.h>
#include <CrkHit.h>
#include <emcTowerContainer.h>
#include <emcTowerContent.h>
#include <EmcIndexer.h>


#include <TRandom.h>
#include <cmath>
#include <algorithm>
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>

using namespace std;

typedef PHIODataNode<dErtFemDataWrapper> dErtFemDataNode_t;
typedef PHIODataNode<dEmcRawDataWrapper> dEmcRawDataNode_t;
typedef PHIODataNode<emcTowerContainer> emcTowerContainerNode_t;
typedef PHIODataNode<dCrkHitWrapper> dCrkHitWrapperNode_t;
typedef PHIODataNode<CrkHit> CrkHitNode_t;
typedef PHIODataNode<dCrkRawWrapper> dCrkRawWrapperNode_t;
typedef PHIODataNode<dCrkRawHitParWrapper> dCrkRawHitParWrapperNode_t;
typedef PHIODataNode<dCrkCalWrapper> dCrkCalWrapperNode_t;
typedef PHIODataNode<ErtOut> ErtOutNode_t;

ERTSimulator::ERTSimulator()
{
  NodeName = "ErtOut";

  rdm = new TRandom;

  for(int imod=0; imod<N_MODULE; imod++){ EMCModuleDepositE[imod]=0; }
  for(int i=0; i<N_CRKTT; i++){ CrkTriggerTileNPE[i]=0; }

  emcthreshold=0;
  crkthreshold=0;

  for(int emctt=0; emctt<N_EMCTT; emctt++) {
    for(int crktt=0; crktt<N_CRKTT; crktt++) {
      TriggerMatch[emctt][crktt]=0;
    }
  }

  for(int iarm = 0; iarm<2; iarm++) {
    for(int isect = 0; isect<4; isect++) {
      for(int iy = 0; iy<48; iy++) {
	for(int iz = 0; iz<96; iz++) {
	  gain[iarm][isect][iy][iz] = 1.0;
          //... EMC gain parameter initialization
	}
      }
    }
  }

  gain_additional = 1.0;

  ertsmeff = new PdbErtSMEff();

  //... SM bit position
  SMCoordModeAPBGL = new int[N_BIT];
  SMCoordModeBPBGL = new int[N_BIT];
  SMCoordModeAPBSC = new int[N_BIT];
  SMCoordModeBPBSC = new int[N_BIT];
  SMCoordModeARICH = new int[N_BIT];
  SMCoordModeBRICH = new int[N_BIT];

  SMBitPosPBSC = new int[N_SM];
  SMBitPosPBGL = new int[N_SM];
  SMBitPosRICH = new int[N_SM];


  for(int i = 0; i<N_SM; i++) {
    SMBitPosPBSC[i] = -1;
    SMBitPosPBGL[i] = -1;
    SMBitPosRICH[i] = -1;
  }

  //... Read Data Base ..
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PdbCalBank *ertBank;
  if (application->startRead())
    {
      PHTimeStamp tStart = PHTimeStamp(2001, 1, 3, 0, 0, 0);
      PHTimeStamp tStop  = PHTimeStamp(2003, 5, 1, 0, 0, 0); //2003/5/1

      PdbBankID bankID(0);
      const char *calibname = "calib.ErtBLT.map";

      PdbErtDecode *ertdbdecode;

      PdbBankList bankList;
      ertBank = bankManager->fetchBank("PdbErtDecodeBank", bankID, calibname, 40925);

      ertBank->printHeader();
      ertBank->print();
      ertdbdecode = (PdbErtDecode*) & (ertBank->getEntry(0));
      //... 4x4A ..
      for(int k = 0; k < 2; k++)
	{
	  for(int j = 0; j < 4; j++)
	    {
	      wordEMC4x4ACoord[j][k][0] = ertdbdecode->get_word4x4ACoord_ROC(j, k);
	      wordEMC4x4ACoord[j][k][1] = ertdbdecode->get_word4x4ACoord_WORD(j, k);
	    }
	}

      //... 4x4B ..
      for(int k = 0; k < 2; k++)
	{
	  for(int j = 0; j < 4; j++)
	    {
	      wordEMC4x4BCoord[j][k][0] = ertdbdecode->get_word4x4BCoord_ROC(j, k);
	      wordEMC4x4BCoord[j][k][1] = ertdbdecode->get_word4x4BCoord_WORD(j, k);
	    }
	}

      //... 4x4C ..
      for(int k = 0; k < 2; k++)
	{
	  for(int j = 0; j < 4; j++)
	    {
	      wordEMC4x4CCoord[j][k][0] = ertdbdecode->get_word4x4CCoord_ROC(j, k);
	      wordEMC4x4CCoord[j][k][1] = ertdbdecode->get_word4x4CCoord_WORD(j, k);
	    }
	}

      //..  2x2 ....
      for(int k = 0; k < 2; k++)
	{
	  for(int j = 0; j < 4; j++)
	    {
	      wordEMC2x2_Coord[j][k][0] = ertdbdecode->get_word2x2Coord_ROC(j, k);
	      wordEMC2x2_Coord[j][k][1] = ertdbdecode->get_word2x2Coord_WORD(j, k);
	    }
	}

      //..  RICH ....
      for(int k = 0; k < 2; k++)
	{
	  for(int j = 0; j < 4; j++)
	    {
	      wordRICH4x5Coord[j][k][0] = ertdbdecode->get_word4x5Coord_ROC(j, k);
	      wordRICH4x5Coord[j][k][1] = ertdbdecode->get_word4x5Coord_WORD(j, k);
	    }
	}


      for(int i = 0; i < 16; i++)
	{
	  SMCoordModeAPBGL[i] = ertdbdecode->get_SMCoordModeAPBGL(i);
	}
      for(int i = 0; i < 16; i++)
	{
	  SMCoordModeBPBGL[i] = ertdbdecode->get_SMCoordModeBPBGL(i);
	}
      for(int i = 0; i < 16; i++)
	{
	  SMCoordModeAPBSC[i] = ertdbdecode->get_SMCoordModeAPBSC(i);

	}
      for(int i = 0; i < 16; i++)
	{
	  SMCoordModeBPBSC[i] = ertdbdecode->get_SMCoordModeBPBSC(i);
	}
      for(int i = 0; i < 16; i++)
	{
	  SMCoordModeARICH[i] = ertdbdecode->get_SMCoordModeARICH(i);
	}
      for(int i = 0; i < 16; i++)
	{
	  SMCoordModeBRICH[i] = ertdbdecode->get_SMCoordModeBRICH(i);

	}
      application->abort();
      cout << "ERTSimulator: Reading application data finished successfully." << endl;
    }
  else
    {
      cout << "ERTSimulator error: failed to start application for update" << endl;
      exit(0);

    }







  for(int i = 0; i<N_BIT; i++) {
    if(SMCoordModeAPBSC[i]!=-1){ SMBitPosPBSC[SMCoordModeAPBSC[i]] = i; }
   SMBitPosPBGL[SMCoordModeAPBGL[i]] = i;
   SMBitPosRICH[SMCoordModeARICH[i]] = i;

   if(SMCoordModeBPBSC[i]!=-1){ SMBitPosPBSC[SMCoordModeBPBSC[i]] = i; }
   SMBitPosPBGL[SMCoordModeBPBGL[i]] = i;
   SMBitPosRICH[SMCoordModeBRICH[i]] = i;
 }


 for(int i = 0; i<16; i++){ bitMask[i] = (int)pow(2.0,i); }

}



ERTSimulator::~ERTSimulator()
{

  delete [] SMCoordModeAPBGL;
  delete [] SMCoordModeBPBGL;
  delete [] SMCoordModeAPBSC;
  delete [] SMCoordModeBPBSC;
  delete [] SMCoordModeARICH;
  delete [] SMCoordModeBRICH;
  
  delete [] SMBitPosPBSC;
  delete [] SMBitPosPBGL;
  delete [] SMBitPosRICH;

}



void ERTSimulator::SetRunNumber(int input)
{ 
  runNumber = input; 
  cout << "ERTSimulator: Set Run Number = " << runNumber << endl; 
}

void ERTSimulator::SetNodeName(const char *node_name)
{ 
  NodeName = node_name;
  cout << "ERTSimulator: Set NodeName = " << NodeName << endl; 
}

bool ERTSimulator::FetchSMEff(int run_number)
{
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();  
    std::cout<<" ErtSMEff:: Opening FD in update mode.."<<std::endl;
    if (application->startRead()) {
	//PHTimeStamp tStart = PHTimeStamp(2001,1,3,0,0,0);
	PdbBankID bankID(0);
	const char *calibname = "calib.ert.smeff";
	PdbCalBank *ertBank;
	ertBank = bankManager->fetchBank("PdbErtSMEffBank",bankID,calibname,run_number);
	if( ertBank )
	{
	    ertBank->printHeader();
	    ertBank->print();
	    ertsmeff = (PdbErtSMEff*)&(ertBank->getEntry(0));
	    application->abort();
	}
    }
    else
    {
	std::cout<<" ErtSMEff::Error!! failed to start application for update" << std::endl;
	exit(0);
    }
    return true;
}

void ERTSimulator::FetchSMEff_fromfile()
{
  ifstream fp("sm_eff.dat");
  if (!fp)
    {
      cout << "ERTSimulator: Efficiency file not found. Using default." << endl;
      return;
    }
  string name;
  short iarm,isect,ism;
  float eff;
  while (fp >> name >> iarm >> isect >> ism >> eff)
    {
      iarm = 1 - iarm; // using DCH arm index
      if (name=="4x4a" || name =="4x4A")
	ertsmeff->Set(iarm,isect,ism,0,eff);
      if (name=="4x4b" || name=="4x4B")
	ertsmeff->Set(iarm,isect,ism,1,eff);
      if (name=="4x4c" || name=="4x4C")
	ertsmeff->Set(iarm,isect,ism,2,eff);
      if (name=="2x2")
	ertsmeff->Set(iarm,isect,ism,3,eff);
      if (name=="RICH" || name=="rich")
	ertsmeff->Set(iarm,isect,ism,4,eff);
    }
  cout << "ERTSImulator: SM efficiencies loaded." << endl;
}

/*
//<<<<<<<<<<<<<<<<  Reading Mask File <<<<<<<<<<<<<<<<<<<<<<<
void ERTSimulator::FetchSMMask(int run_number)
{
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();  
    std::cout<<" ErtSimulator:: Opening ErtSMMask in Fetch mode.."<<std::endl;
    if (application->startRead()) {
	//PHTimeStamp tStart = PHTimeStamp(2001,1,3,0,0,0);
	PdbBankID bankID;
	const char *calibname = "calib.ert.smmask";
	PdbCalBank *ertBank;
	PdbErtSMMask *ertsmmask;
	ertBank = bankManager->fetchBank("PdbErtSMMaskBank",bankID,calibname,run_number);
	if( ertBank )
	{
	    ertBank->printHeader();
	    ertBank->print();
	    ertsmmask = (PdbErtSMMask*)&(ertBank->getEntry(0));
	    int iarm,isector,ism,itrig;
	    //---------------------------------------------------
	    iarm = NARM;
	    while( iarm-- ){
		isector = NSECTOR;
		while( isector-- ){
		    ism = NSM;
		    while( ism-- ){
			itrig = NTRIGTYP;
			while( itrig-- ){
			    if (ertsmmask->Get(iarm,isector,ism,itrig))
				ertsmeff->Set(iarm,isector,ism,itrig,0.0);
			}
		    }
		}
	    }
	    //---------------------------------------------------	    
	    application->abort();
	}
    }
    else
    {
	cout<<" ErtSimulator::Failed to fetch Masks from database" << std::endl;
	exit(0);
    }
}
*/
void ERTSimulator::FetchSMMask_fromfile()
{
  ifstream fp("mask.dat");
  if (!fp)
  {
    cout<<"ERTSimulator error: mask.dat not found, exit now "<<endl;
    exit(9);
  }

  int iarm;
  int isect;
  int ism;
  string name = "";

  while ( fp >> name >> iarm >> isect >> ism)
    {
      if (name=="4x4a")
	ertsmeff->Set(iarm,isect,ism,0,0.0);
      if (name=="4x4b")
	ertsmeff->Set(iarm,isect,ism,1,0.0);
      if (name=="4x4c")
	  ertsmeff->Set(iarm,isect,ism,2,0.0);
      if (name=="2x2")
	  ertsmeff->Set(iarm,isect,ism,3,0.0);
      if (name=="RICH" || name=="rich")
	  ertsmeff->Set(iarm,isect,ism,4,0.0);
    }
  fp.close();

  cout<< "ERTSimulator: Reading of ERT tile masks finished successfully." <<endl;  

}

void ERTSimulator::FetchSector(int run_number)
{
    isGaussNoise = true;
    PdbParameter* ertsector = 0;
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();
    PdbCalBank *ertBank = 0;

    // Loop over chambers
    for (int index = 0; index < 8; index++)
    {
	int arm = index/4;
	int sect = index%4;
	if (application->startRead())
        {
	    PdbBankID bankID;
	    bankID.setInternalValue(index);
	    ertBank = bankManager->fetchBank("PdbParameterBank", bankID, "ert.sector.pars", run_number);
	    if (ertBank)
	    {
		// fetch thresholds
		ertsector = (PdbParameter*)&(ertBank->getEntry(0));
		emc4x4Athres[arm][sect] = ertsector->getParameter();
		ertsector = (PdbParameter*)&(ertBank->getEntry(1));
		emc4x4Bthres[arm][sect] = ertsector->getParameter();
		ertsector = (PdbParameter*)&(ertBank->getEntry(2));
		emc4x4Cthres[arm][sect] = ertsector->getParameter();
		ertsector = (PdbParameter*)&(ertBank->getEntry(3));
		emc2x2_thres[arm][sect] = ertsector->getParameter();
		ertsector = (PdbParameter*)&(ertBank->getEntry(4));
		crk4x5_thres[arm][sect] = ertsector->getParameter();
		// fetch noise
		ertsector = (PdbParameter*)&(ertBank->getEntry(5));
		noise_4x4Sigma[arm][sect] = ertsector->getParameter();
		ertsector = (PdbParameter*)&(ertBank->getEntry(6));
		noise_2x2Sigma[arm][sect] = ertsector->getParameter();
		ertsector = (PdbParameter*)&(ertBank->getEntry(7));
		noise_crkSigma[arm][sect] = ertsector->getParameter();
	    }
	    else
	    {
		cerr << "ErtSimulator:: Couldn't fetch ERTSector database information" << endl;
		exit(0);
	    }
	}
    }
}

void ERTSimulator::AddPMT(int pmt,float npe) 
{
  int crktt=ErtUtils::get_RICH_TrigTile_FromPMT(pmt);
  if(crktt>=0){
    CrkTriggerTileNPE[crktt]+=npe;
    IsCrkTileHit[crktt] = true;
  }

}

float ERTSimulator::GetEMCTTMaxSum_no_noise(int emctt,int emcopt)
{
  int ModulesInEmcTile[36];
  ErtUtils::get_EMC_Module_FromTrigTile(emctt,ModulesInEmcTile);
  int moduleID;
  float MaxSum=0.0;

  switch (emcopt) {
    case 0:
      // 2 by 2 non overlapping
      for(int i=0;i<36;i++) {
        moduleID=ModulesInEmcTile[i];
        MaxSum=max(MaxSum,EMCModuleDepositE[moduleID]);
      }
      break;
    case 1:
      // 4 by 4 overlapping
      float sumA16;
      int neighbours[4];
      for(int i=0;i<36;i++) {
        moduleID=ModulesInEmcTile[i];
        ErtUtils::get_EMC_Module_Neighbors(moduleID,neighbours);
        sumA16=0.0;
        for(int j=0;j<4;j++) {
          if (neighbours[j]!=-1) {
	    //.. no connect bewteen sector
            int arm_tmp, sect_tmp, arm, sect;
	    int dummy;
	    int tmp = ErtUtils::get_EMC_TrigTile_FromModule(neighbours[j]);
	    ErtUtils::get_EMC_ArmSectSide_FromTrigTile(arm_tmp,sect_tmp,dummy,tmp);
	    ErtUtils::get_EMC_ArmSectSide_FromTrigTile(arm,sect,dummy,emctt);
	    if(arm_tmp==arm && sect_tmp==sect)
	      sumA16+=EMCModuleDepositE[neighbours[j]];
	  }
	}
	 MaxSum=max(MaxSum,sumA16);
      }
      break;
    default:
      cout << "ERTSimulator error: GetTriggerBit: " << emcopt 
	   << " unknown emc option." << endl;
      return 0;
  } // End of switch
  return MaxSum;
}



float ERTSimulator::GetCRKTTMaxSum(int crktt,int crkopt, int arm, int sect)
{
  float tmp_noise;
  float MaxSum=0.0;

  switch (crkopt) {
    case 0:
      // Non Overlapping
      MaxSum=CrkTriggerTileNPE[crktt];
      tmp_noise = rdm->Gaus(0, noise_crkSigma[arm][sect]);
      MaxSum += tmp_noise;
      break;
    case 1:
      // Overlapping
      int crkneighbours[4];
      float sumA4=0.0;
      ErtUtils::get_RICH_TrigTile_Neighbors(crktt,crkneighbours);
      for(int i=0;i<4;i++) {
        crktt=crkneighbours[i];
        sumA4+=CrkTriggerTileNPE[crktt];
      }
      MaxSum=sumA4;
      break;
  } // End of switch
  return MaxSum;
}

void ERTSimulator::ClearforCRK(){

  for(int arm=0; arm<2; arm++){
    for(int sector=0; sector<4; sector++){
      for(int sm=0; sm<32; sm++){
        RICH4x5[arm][sector][sm] = false;
      }
    }
  }

  for(int i = 0; i<N_CRKTT; i++) {
    IsCrkTileHit[i] = false;
  }
  
  for(int i=0; i<N_CRKTT; i++){//256
    CrkTriggerTileNPE[i] = 0;
  }
}



void ERTSimulator::ClearforEMC(){

  for(int arm=0; arm<2; arm++){
    for(int sector=0; sector<4; sector++){
      for(int sm=0; sm<32; sm++){
        EMC4x4A[arm][sector][sm] = false;
        EMC4x4B[arm][sector][sm] = false;
        EMC4x4C[arm][sector][sm] = false;
        EMC_2x2[arm][sector][sm] = false;
      }
    }
  }

  for(int i = 0; i<N_EMCTT; i++) {
    IsTileHit[i] = false;
    TileMax2x2E[i] = -9999;
    TileMax4x4E[i] = -9999;
  }
  for(int i=0; i<36; i++) {
    ModulesInEMCTile[i] = -9999; 
  }

  for(int i=0; i<N_MODULE; i++){//172
    EMCModuleDepositE[i] = 0;
  }
}



PHBoolean ERTSimulator::EventLoopforCRK(PHCompositeNode* topNode)
{

   int arm, sector, smID;
   
   ClearforCRK();   //.. reset all value at for each event 

   PHNodeIterator it(topNode);

   //.. get CrkHit table ..
   CrkHit *crkhit = NULL;
   PHTypedNodeIterator<CrkHit> crkhit_iter(topNode);
   PHIODataNode <CrkHit> *CrkHitNode = crkhit_iter.find("CrkHit"); 
   if(CrkHitNode) {
      crkhit = CrkHitNode->getData();
   } else {
      cout << "ERTSimulator error: could not find dCrkHitNode table." << endl;
      return 0;
   }

   for(int ihit =0; ihit<(int)(crkhit->get_CrkNHit()); ihit++) {
     int pmt = crkhit->get_pmt(ihit);
     float npe = crkhit->get_npe(ihit);

     AddPMT(pmt, npe);//npe in a tile 
   }

   for(int crktt =0; crktt<N_CRKTT; crktt++) {
     if(!IsCrkTileHit[crktt]){ continue; }//.. neglect 0-npe tiles

     ErtUtils::get_RICH_smID_FromTrigTile(arm, sector, smID, crktt);

     // apply a gaussian noise first ...
     float tmp = rdm->Gaus(0, noise_crkSigma[arm][sector]);
     CrkTriggerTileNPE[crktt] += tmp; 

     if (rdm->Uniform(0,1) < ertsmeff->Get(arm,sector,smID,4))
     {
	 if(CrkTriggerTileNPE[crktt] > crk4x5_thres[arm][sector]){
	     RICH4x5[arm][sector][smID] = true;
/*
  cout << "RICH4x5-----------------" << endl;
         cout << "arm     = " << arm        << endl;
         cout << "sector  = " << sector     << endl;
         cout << "smID    = " << smID       << endl;
         cout << "npe     = " << CrkTriggerTileNPE[crktt] << endl;
         cout << "------------------------" << endl;
*/
	 }
     }
   }
   
   return True;
}




PHBoolean ERTSimulator::EventLoopforEMC(PHCompositeNode* topNode)
{

   int arm, sector, smID;
   
   ClearforEMC();   //.. reset all value at for each event 

   PHNodeIterator it(topNode);



/*<<<<<<<<<<<<<<<<<<<<<<<<<< EMC2x2 deposit enegy <<<<<<<<<<<<<<<<<<<<<<<<<<<*/

   //.. get dErtFemData table ..
   dErtFemDataNode_t  *dertfemdatanode = 
     static_cast<dErtFemDataNode_t *>(it.findFirst("PHIODataNode",
	                                           "dErtFemData"));
   if(!dertfemdatanode){
      cout<< "ERTSimulator error: could not find dErtFemData table."<<endl;
   }

   //.. get emcTowerContainer table ..
   emcTowerContainer *emctowercontainer = 0;
   emcTowerContainerNode_t *emctowercontainernode =
     static_cast<emcTowerContainerNode_t *>(it.findFirst("PHIODataNode",
							 "emcTowerContainer"));     
   if(emctowercontainernode){
     emctowercontainer = emctowercontainernode->getData();
   } else {
     cout<< "ERTSimulator error: could not find dErtFemData table."<<endl;
     return 0;
   }

   //transfer tower information into trigger simulation class .. 
   for(int itower =0; itower<(int)(emctowercontainer->size()); itower++)
     {
       emcTowerContent* tower = emctowercontainer->getTower(itower);
       long  towerid = tower->TowerID();

     //Deposit Energy; ecal
       float ecal = tower->Energy();

       AddTower(towerid, ecal);
   }
 
   for(int emctt=0; emctt<N_EMCTT; emctt++) {

     if(!IsTileHit[emctt]){ continue; }//.. neglect 0-energy tiles

     ErtUtils::get_EMC_smID_FromTrigTile(arm, sector, smID, emctt);

     //... get maximum 2x2 module energy in a tile ....
     ErtUtils::get_EMC_Module_FromTrigTile(emctt,ModulesInEMCTile);
     //emctt -> ModulesInEMCTile[36]

     //Selection a maximum energy of all 36 modules.
     for(int i=0;i<36;i++) {
        moduleid = ModulesInEMCTile[i];
        TileMax2x2E[emctt] =
            max(TileMax2x2E[emctt],EMCModuleDepositE[moduleid]);
     }
     //......add noise ....
     float tmp_noise = 0;
     if(isGaussNoise)
     { tmp_noise = rdm->Gaus(0, noise_2x2Sigma[arm][sector]); }
     else if(isContNoise){ tmp_noise = noise2x2[arm][sector]; }


     TileMax2x2E[emctt] += tmp_noise;//Add noise effect



/*<<<<<<<<<<<<<<<<<<<<<<<<<< EMC4x4 deposit enegy <<<<<<<<<<<<<<<<<<<<<<<<<<<*/
     //....  4x4 overlapping ....
     for(int i=0;i<36;i++){
        moduleid = ModulesInEMCTile[i];
        ErtUtils::get_EMC_Module_Neighbors(moduleid,neighbours);
        sumA16=0.0;
        for(int j=0;j<4;j++){
          if (neighbours[j]!=-1){
            //
            //.. Kensuke said no connection between superModule in PBSC
            //.. for RUN2 by mistake 
            if(emctt<108) { //.. PBSC

              if(ErtUtils::get_EMC_TrigTile_FromModule(neighbours[j]) == emctt) 
                 sumA16+=EMCModuleDepositE[neighbours[j]];

            } else {//.. PBGL, no connect bewteen sector

              int arm_tmp, sect_tmp, arm, sect, dummy;

              int tmp = ErtUtils::get_EMC_TrigTile_FromModule(neighbours[j]);
              ErtUtils::get_EMC_ArmSectSide_FromTrigTile(arm_tmp, sect_tmp, dummy, tmp);
              ErtUtils::get_EMC_ArmSectSide_FromTrigTile(arm, sect, dummy, emctt);

              if(arm_tmp==arm && sect_tmp==sect) 
                 sumA16+=EMCModuleDepositE[neighbours[j]];
            }
          }
        } 
        TileMax4x4E[emctt]=max(TileMax4x4E[emctt],sumA16);
     }
     //......add noise ....
     tmp_noise = 0;
     if(isGaussNoise){ tmp_noise = rdm->Gaus(0, noise_4x4Sigma[arm][sector]); }
     else if(isContNoise){ tmp_noise = noise4x4[arm][sector]; }

     TileMax4x4E[emctt] += tmp_noise;
     



/*<<<<<<<<<<<<<<<<<<<<<< trigger by threshold energy <<<<<<<<<<<<<<<<<<<<<<<<*/
     //....  calculate the value of word ....
     if (rdm->Uniform(0,1) < ertsmeff->Get(arm,sector,smID,0))
     {
	 if(TileMax4x4E[emctt] > emc4x4Athres[arm][sector]) {
	     EMC4x4A[arm][sector][smID] = true;
	 }
     } 

     if (rdm->Uniform(0,1) < ertsmeff->Get(arm,sector,smID,1))
     {
	 if(TileMax4x4E[emctt] > emc4x4Bthres[arm][sector]) {
	     EMC4x4B[arm][sector][smID] = true;
	 }
     }

     if (rdm->Uniform(0,1) < ertsmeff->Get(arm,sector,smID,2))
     {
	 if(TileMax4x4E[emctt] > emc4x4Cthres[arm][sector]) {
	     EMC4x4C[arm][sector][smID] = true;
	 }
     }

     if (rdm->Uniform(0,1) < ertsmeff->Get(arm,sector,smID,3))
     {
	 if(TileMax2x2E[emctt] > emc2x2_thres[arm][sector]){ 
	     EMC_2x2[arm][sector][smID] = true;
	     /*
	 cout << "EMC2x2------------------" << endl; 
	 cout << "arm         = " << arm    << endl;
	 cout << "sector      = " << sector << endl;
	 cout << "emctt       = " << emctt  << endl; 
	 cout << "TileMax2x2E = " << TileMax2x2E[emctt] << endl;
	 cout << "------------------------" << endl; 
*/
	 }
     }
   }//Loop of emctt
   
   
   return True;
}


// transfer dEmcCalibTower information into trigger class
void ERTSimulator::AddTower(int towerid, float DepositE)
{
  
  int arm, sect, iy, iz;
  EmcIndexer::TowerLocation(towerid,arm,sect,iy,iz);

  float unCalDepositE = DepositE * gain[arm][sect][iy][iz] * gain_additional;
  //float unCalDepositE = DepositE;

  int mid = ErtUtils::get_EMC_Module_FromTowerID(towerid);
  EMCModuleDepositE[mid] += unCalDepositE;
  if(ErtUtils::get_EMC_TrigTile_FromModule(mid)!=-1){ IsTileHit[ErtUtils::get_EMC_TrigTile_FromModule(mid)] = true; }
 
}



void ERTSimulator::GetRocWordBitPacket(int arm,int sector,int trgType,int sm,
                      int &roc,int &word,int &packet,int &bitEMC,int &bitRICH)
{
  GetBitPosition(arm, sector, sm, bitEMC, bitRICH);
  packet = GetPacket(arm);
  GetRocWord(arm, sector, trgType, sm, roc, word);
}


void  ERTSimulator::GetRocWord(int arm,int sector,int trgType,int sm,int &roc,int &word)
{
//--------------------------------
//  determine the side of the SM

  int sideEMC, sideRICH;

  if(arm==1) { //__ PBSC
    if((sm/3)%2==0)  sideEMC = 0;  //__ south
    else   sideEMC = 1;  //__ north
  } else {
    if((sm/3)%2==0)  sideEMC = 1;  //__ north
    else   sideEMC = 0;  //__ south
  }

  if(arm==1 && (sector==0 || sector==1)) { //__PBGL
    if((sm/4)%2==0){ sideEMC = 0; } //__ south
    else { sideEMC = 1; } //__ south
  }

  if(arm==1) { //__ RICH
    if((sm/4)%2==0){ sideRICH = 0; } //__ south
    else { sideRICH = 1; } //__ north
  } else {
    if((sm/4)%2==0){  sideRICH = 1; } //__ north
    else { sideRICH = 0; } //__ south
  }

  if(trgType==0) { //__ 4x4A
    roc  = wordEMC4x4ACoord[sector][sideEMC][0];
    word = wordEMC4x4ACoord[sector][sideEMC][1];
  }

  if(trgType==1) { //__ 4x4B
    roc  = wordEMC4x4BCoord[sector][sideEMC][0];
    word = wordEMC4x4BCoord[sector][sideEMC][1];
  }

  if(trgType==2) { //__ 4x4C
    roc  = wordEMC4x4CCoord[sector][sideEMC][0];
    word = wordEMC4x4CCoord[sector][sideEMC][1];
  }

  if(trgType==3) { //__ 2x2
    roc  = wordEMC2x2_Coord[sector][sideEMC][0];
    word = wordEMC2x2_Coord[sector][sideEMC][1];
  }

  if(trgType==4) { //__ 4x5 RICH
    roc  = wordRICH4x5Coord[sector][sideRICH][0];
    word = wordRICH4x5Coord[sector][sideRICH][1];
   }
}


int  ERTSimulator::GetPacket(int arm)
{
  if(arm==1) return 14201; //__ east arm
  if(arm==0) return 14200; //__ west arm
  return -1;
}



void ERTSimulator::GetBitPosition(int arm,int sector,int sm,int &bitEMC,int &bitRICH)
{
  int* SMBitPos;

  if(arm==1 && (sector==0||sector==1)){ SMBitPos = SMBitPosPBGL; } 
  else { SMBitPos = SMBitPosPBSC; }

    bitEMC = SMBitPos[sm];
    bitRICH = SMBitPosRICH[sm];
}



void ERTSimulator::Decode(PHCompositeNode *topNode)
{
  Reset();

  PHNodeIterator it(topNode);
  dErtFemDataWrapper *dErtFemData = 0;
  dErtFemDataNode_t  *dertfemdatanode =
    static_cast<dErtFemDataNode_t *>(it.findFirst("PHIODataNode","dErtFemData"));

  if(dertfemdatanode){
    dErtFemData = dertfemdatanode->getData();
  } else {
    cout << "ERTSimulator error: could not find table dErtFemData." << endl;
    // scan build fix: we crash in the next line when this is a null pointer
    // so we might as well exit to stop getting mails about this
    exit(1);
  }

  int packetid, iroc, iword;
  long ivalue;
  for(int i = 0; i<(int)(dErtFemData->RowCount()); i++){
    if(dErtFemData->get_crate(i) == 0){ packetid = 14200; }//West arm
                                 else { packetid = 14201; }//East arm
    iroc   = dErtFemData->get_Roc(i);
    iword  = dErtFemData->get_word(i);
    ivalue = dErtFemData->get_Value(i);

    SetPacketData(packetid, iroc, iword, ivalue);
   /* 
    cout << "=======================" << endl;
    cout << " packetid = " << packetid << endl;
    cout << " iroc     = " << iroc << endl;
    cout << " iword    = " << iword << endl;
    cout << "=======================" << endl;
   */ 
  }

  Calculate();
}



void ERTSimulator::SetPacketData(int packetID, int roc, int word, long value)
{
  int arm;
  if(packetID == EASTPACKETID){ arm  = 1; }
  else if(packetID == WESTPACKETID){ arm  = 0; }
  else {
    std::cout << "ERTSimulator error: packetID is unknown."<<endl;
    return;
  }

  PacketData[arm][roc][word] = value;
}


/**********************************************************************

  Loop Valuables;
  arm
  --sector
  ----side .... N_SIDE = 0 (South), 1 (North);
  ------bit ..... 16 bits
        if(PacketData[arm][roc][word])
        trigmode 0-4

**********************************************************************/
void ERTSimulator::Calculate()
{
  int *emcsmcoord, *richsmcoord;
 
  for(int arm = 0; arm<N_ARM; arm++){
   for(int sector = 0; sector<N_SECTOR; sector++){
     for(int side = 0; side<N_SIDE; side++) { //0; South, 1; North

       if(arm==1) {
         emcsmcoord  = side? SMCoordModeBPBSC : SMCoordModeAPBSC;
         richsmcoord = side? SMCoordModeBRICH : SMCoordModeARICH;
       } else {
         emcsmcoord  = side? SMCoordModeAPBSC : SMCoordModeBPBSC;
         richsmcoord = side? SMCoordModeARICH : SMCoordModeBRICH;
       }

       if(arm ==1 && (sector == 0 || sector == 1)){  //__PBGL
         emcsmcoord = side? SMCoordModeBPBGL : SMCoordModeAPBGL;
       }


         int roc1  = wordEMC4x4ACoord[sector][side][0];
         int word1 = wordEMC4x4ACoord[sector][side][1];
         int roc2  = wordEMC4x4BCoord[sector][side][0];
         int word2 = wordEMC4x4BCoord[sector][side][1];
         int roc3  = wordEMC4x4CCoord[sector][side][0];
         int word3 = wordEMC4x4CCoord[sector][side][1];
         int roc4  = wordEMC2x2_Coord[sector][side][0];
         int word4 = wordEMC2x2_Coord[sector][side][1];
         int roc5  = wordRICH4x5Coord[sector][side][0];
         int word5 = wordRICH4x5Coord[sector][side][1];

         for(int bit = 0; bit<16; bit++) {
           //...  4x4A ...
           FillRawBit(arm, sector, side,  roc1, word1, bit, emcsmcoord, 0);

           //...  4x4B ...
           FillRawBit(arm, sector, side,  roc2, word2, bit, emcsmcoord, 1);

           //...  4x4C ...
           FillRawBit(arm, sector, side,  roc3, word3, bit, emcsmcoord, 2);

           //...  2x2 ...
           FillRawBit(arm,  sector, side, roc4, word4, bit, emcsmcoord, 3);

           //...  4x5 RICH ...
           FillRawBit(arm,  sector, side, roc5, word5, bit, richsmcoord, 4);
        }
      }
    }
  }
}



void ERTSimulator::FillRawBit(int arm,int sector,int side,int roc,int word,int bit,int* smcoord, int tileType)
{
  if((PacketData[arm][roc][word] & bitMask[bit])==bitMask[bit]) 
  {

     int sm = smcoord[bit]; 
     if(sm==-1) return;  //__ non-existing SM

     if(tileType==0) { //__ 4x4A __

       Bit4x4AHalfSectEMC[arm][sector][side] = 1;
       Bit4x4ASMEMC[arm][sector][sm] = 1;

     } else if(tileType==1) { //__ 4x4B __

       Bit4x4BHalfSectEMC[arm][sector][side] = 1;
       Bit4x4BSMEMC[arm][sector][sm] = 1;


     } else if(tileType==2) { //__ 4x4C __

       Bit4x4CHalfSectEMC[arm][sector][side] = 1;
       Bit4x4CSMEMC[arm][sector][sm] = 1;

     } else if(tileType==3) { //__ 2x2 __

       Bit2x2HalfSectEMC[arm][sector][side] = 1;
       Bit2x2SMEMC[arm][sector][sm] = 1;
/*
       cout << "=====================" << endl;
       cout << " EMC2x2 Trigger "      << endl; 
       cout << " arm    = " << arm     << endl; 
       cout << " sector = " << sector  << endl; 
       cout << " sm     = " << sm      << endl;
       cout << "===================="  << endl;
*/
     } else if(tileType==4) { //__  RICH 4x5 __

       Bit4x5HalfSectRICH[arm][sector][side] = 1;
       Bit4x5SMRICH[arm][sector][sm] = 1;
/*
       cout << "=====================" << endl;
       cout << " RICH4x5 Trigger "     << endl; 
       cout << " arm    = " << arm     << endl; 
       cout << " sector = " << sector  << endl; 
       cout << " sm     = " << sm      << endl;
       cout << "===================="  << endl;
*/
     }
  }
}



void ERTSimulator::Reset()
{
  for(int arm = 0; arm<N_ARM; arm++){
    for(int sector = 0; sector<N_SECTOR; sector++){
      for(int side = 0; side<N_SIDE; side++){
        Bit4x4AHalfSectEMC[arm][sector][side] = 0;
        Bit4x4BHalfSectEMC[arm][sector][side] = 0;
        Bit4x4CHalfSectEMC[arm][sector][side] = 0;
        Bit2x2HalfSectEMC[arm][sector][side] = 0;
        Bit4x5HalfSectRICH[arm][sector][side] = 0;
      }

      for(int sm = 0; sm<N_SM; sm++){
        Bit4x4ASMEMC[arm][sector][sm] = 0;
        Bit4x4BSMEMC[arm][sector][sm] = 0;
        Bit4x4CSMEMC[arm][sector][sm] = 0;
        Bit2x2SMEMC[arm][sector][sm] = 0;
      }

      for(int sm = 0; sm<N_SM; sm++){
        Bit4x5SMRICH[arm][sector][sm] = 0;
      }
    }
  }

   for (int arm = 0; arm < N_ARM; arm++){
     for (int roc = 0; roc < N_ROC; roc++){
       for(int word=0;word < N_WORD;word++){
         PacketData[arm][roc][word]=0;
       }
     }
   }

}



void ERTSimulator::DecisionMaking(PHCompositeNode *topNode)
{
  float EMCMaxE[2][4][2];

  int arm, sect, side, smID;

  for(arm = 0; arm<2; arm++){
    for(sect = 0; sect<4; sect++){
      for(side = 0; side<2; side++){
        EMCMaxE[arm][sect][side] = 0;
      }
    }
  }
  //... EMCal 2x2 tile sum ...
  for(int emctt = 0; emctt<N_EMCTT; emctt++){
    ErtUtils::get_EMC_ArmSectSide_FromTrigTile(arm, sect, side, emctt);//emctt->arm, sect, side
    ErtUtils::get_EMC_ArmSectSide_FromTrigTile(arm, sect, smID, emctt);//emctt->smID
    bitEMC2x2Tile[emctt] = false;
   
    if(EMC_Mask[arm][sect][smID][3]<0) {
      float E = GetEMCTTMaxSum_no_noise(emctt, 0) 
              + rdm->Gaus(0, noise_2x2Sigma[arm][sect]);
      if(E>emc2x2_thres[arm][sect]){bitEMC2x2Tile[emctt] = true;}
      if(EMCMaxE[arm][sect][side] < E){EMCMaxE[arm][sect][side] = E;}
    }
  }


  //... CRK4x5 trigger bit ...
  for(int crktt = 0; crktt<N_CRKTT; crktt++)
    {
      ErtUtils::get_RICH_ArmSectSide_FromTrigTile(arm, sect, side, crktt);//crktt->arm, sect, side
      ErtUtils::get_RICH_smID_FromTrigTile(arm, sect, smID, crktt);//crktt->arm,sect,smID
      bitCRK4x5Tile[arm][sect][smID] = false;
      
      if (rdm->Uniform(0,1) < ertsmeff->Get(arm,sect,smID,4))
      {
	  if(GetCRKTTMaxSum(crktt, 0, arm, sect) > crk4x5_thres[arm][sect])
	  {
	      bitCRK4x5Tile[arm][sect][smID] = true;
	  }
      }
    }
}



void ERTSimulator::DstStore(PHCompositeNode *topNode)
{

 ErtOut *ertout=0;

 PHTypedNodeIterator<ErtOut>    ertiter(topNode);
 ErtOutNode_t *ErtOutNode = ertiter.find(NodeName.Data());
 if (ErtOutNode){
   ertout = ErtOutNode->getData();
 }else{
   cout << "ERTSimulator: " << NodeName.Data() << " Node missing" << endl;
 }


  if(ertout){
    ertout->Clear();
    for(int arm=0; arm<2; arm++){
      for(int sector=0; sector<4; sector++){
        for(int sm=0; sm<32; sm++){

         if(EMC4x4A[arm][sector][sm]){ ertout->set_ERTbit(0,arm,sector,sm); }
         if(EMC4x4B[arm][sector][sm]){ ertout->set_ERTbit(1,arm,sector,sm); }
         if(EMC4x4C[arm][sector][sm]){ ertout->set_ERTbit(2,arm,sector,sm); }
         if(EMC_2x2[arm][sector][sm]){ ertout->set_ERTbit(3,arm,sector,sm);

	   /*           cout << "EMC2x2 hit check ----------------------------------" << endl;
           cout << " Arm    = "    << arm    << "\t" 
                << " Sector = "    << sector << "\t" 
                << " SM     = "    << sm     << endl;
           cout << "--------------------------------------------------" << endl;
	   */
         }

         if(RICH4x5[arm][sector][sm]){ ertout->set_ERTbit(4,arm,sector,sm);
	   /*
           cout << "RICH4x5 hit check --------------------------------" << endl;
           cout << " Arm    = "    << arm    << "\t" 
                << " Sector = "    << sector << "\t"
                << " SM     = "    << sm     << endl;
           cout << "--------------------------------------------------" << endl;
	   */
        }

      }
    }
  }

  }

}
