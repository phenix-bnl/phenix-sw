#include <Lvl2FilterMon.h>
#include <Event.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <L2DecisionHelper.h> // offline/packages/lvl2
#include <TrigRunLvl1.h>
#include <TrigLvl1.h>
#include <TrigRunLvl2.h>
#include <Lvl2OutArray.h>
#include <L2AuAuElectronCandidate.h>
#include <L2AuAuElectronInvMass.h>
#include <L2MuiPairs.h>
#include <recoConsts.h>

#include <getClass.h>
#include <iomanip>
#include <cstdlib>
#include <cstring>

#include <TH1F.h>
#include <TH2F.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TLeaf.h>

#include <L2MuiTracks.h>
#include <L2MutrPairPrimitive.h>
#include <L2MutrTrackPrimitive.h>
#include <L2MuonTrigFilter.h>

using namespace std;

typedef PHIODataNode<Lvl2OutArray>	Lvl2OutArrayNode_t; 

Lvl2FilterMon::Lvl2FilterMon(const char *name): SubsysReco(name)
{
  nevt = 0;
  runnumber = 0;
  esq = -1;
  done=0;
  got_names=0;

  if(!strcmp(name,"ATPLVL2"))
    {
      sprintf(lvl2decisionnodename,"L2Decision");
      sprintf(lvl2trigrunnodename,"TrigRunLvl2");
      sprintf(lvl2outarraynodename,"Lvl2OutArray");
    }
  else
    {
      sprintf(lvl2decisionnodename,"L2DecisionCal");
      sprintf(lvl2trigrunnodename,"TrigRunLvl2Cal");
      sprintf(lvl2outarraynodename,"Lvl2OutArrayCal");
    }

 cout << "Lvl2FilterMon will read " << name << " info from " << lvl2trigrunnodename 
      << " and " << lvl2decisionnodename <<  " and " << lvl2outarraynodename << endl;
}

int Lvl2FilterMon::Init(PHCompositeNode *topNode)
{
  for(int ilvl1=0;ilvl1<32;ilvl1++)
    {
      Lvl1Scaled[ilvl1]=0;
      for(int ilvl2=0;ilvl2<32;ilvl2++)
	{
	  Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1]=0;	
	  Lvl2FiredLvl1Scaled[ilvl2][ilvl1]=0;	
	  Lvl2ErrorLvl1Scaled[ilvl2][ilvl1]=0;		  
	}
    }

  for (int lvl2trigbit=0;lvl2trigbit<32;lvl2trigbit++)
    {
      Lvl2Name[lvl2trigbit] = "";
    }

  // Get the flags that are needed in process event
  recoConsts *rc = recoConsts::instance();
  real_data_flag = rc->get_IntFlag("LVL2_REAL_DATA");

  return 0;
}
  
void Lvl2FilterMon::identify(ostream& out) const
{
  cout << "LVL2FILTERMON" << endl;
  return;
}

void Lvl2FilterMon::SetSequenceNumber(int inesq)
{
  cout << "Lvl2FilterMon: Setting sequence number to " << inesq << endl;
  esq=inesq;
  return;
}


int Lvl2FilterMon::process_event(PHCompositeNode *topNode)
{
  // verbosity is defined in SubsysReco

  nevt++;
  if(nevt%1 == 0 && verbosity>0)
    {
      cout << "Lvl2FilterMon: Nevts = " << nevt << endl;
    } 
 
  PHNodeIterator iter(topNode);
  
  Event *evt = findNode::getClass<Event>(topNode,"PRDF");
  
      if (!evt)
	{
	  cout << PHWHERE << "NULL Event Pointer" << endl;
	  return -1;
	}  

  // If this is not a data event, skip it
  if( evt->getEvtType() != 1 )
    {
      cout << "Lvl2FilterMon: Not a data event, skip it - event type = " 
	   << evt->getEvtType() << endl;
      nEvtsNotData++;
      return 0;
    }
  
  
  ///////////////////////////////////////////////////////////////////////
  // find the dstNode  
  //////////////////////////////////////////////////////////////////////

  //PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "Lvl2FilterMon::process_event: No DST node, do nothing and return!"
	   << endl;
      return 1;
    }

  ///////////////////////////////////////////////////////////////////////
  // find the runNode  
  //////////////////////////////////////////////////////////////////////

  PHCompositeNode *runNode = NULL;
  runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if(!runNode)
    {
      cout << "Lvl2FilterMon::process_event: No RUN node, do nothing and return!"
	   << endl;
      return 1;
    }

  /////////////////////////////////////////////////////////////////////////
  ///// naming the output root file 
  ///////////////////////////////////////////////////////////////////////
  if(runnumber == 0)
    {
      runnumber = evt->getRunNumber();
      
      if(verbosity>0)
	cout << " runnumber = " << runnumber << endl;
      
      // If esq has not been set using the method , then we set it here to the event 
      // sequence number for the first event. Thus if we run on different file segments
      // from the same run, we always get a unique histogram file name.

      if(esq < 0)
	{
	  cout << "Lvl2FilterMon: Will use sequence number of 1st event in file name" << endl;
	  esq = evt->getEvtSequence();
	}
      else
	{
	  cout << "Lvl2FilterMon: Will use the sequence number supplied by calling macro in file name" << endl; 
	}
      cout << "  -- event sequence = " << esq << endl;
      
      // If the sequence number is small enough for the standard 4 field format, use that
      char filename[100];
      if(esq < 10000)
	sprintf(filename,"Lvl2FilterMon-%010u-%04d.root",runnumber,esq);
      else
	sprintf(filename,"Lvl2FilterMon-%010u-%d.root",runnumber,esq);

      hfile = new TFile(filename,"RECREATE","Lvl2 monitor histograms");
      

      //=================
      //Create Histograms
      //=================
 
      // rejectplot should contain all Lvl1/Lvl2 trig combinations, presently 10 for Run 5
      rejectplot = new TH1F("rejectplot"," Rejection plot",10,-0.5,9.5);
      zvertplot = new TH1F("zvertplot","zvertplot",100,-100,100);
      momvsenergy = new TH2F("momvsenergy","momvsenergy",100,0,3,100,0,3);
      pc3yvsz_east = new TH2F("pc3yvsz_east","pc3yvsz_east",100,-300,300,100,-300,450);
      pc3yvsz_west = new TH2F("pc3yvsz_west","pc3yvsz_west",100,-300,300,100,-300,450);
      massvspt = new TH2F("massvspt","massvspt",100,0,5,100,1.0,4.0);
      eoverpvsnpe = new TH2F("eoverpvsnpe","eoverpvsnpe",100,0,30,100,0,5);
      eoverpvsp = new TH2F("eoverpvsp","eoverpvsp",100,0,5,100,0,5);
      richyvsz_east = new TH2F("richyvsz_east","richyvsz_east",100,-300,300,100,-200,250);
      richyvsz_west = new TH2F("richyvsz_west","richyvsz_west",100,-300,300,100,-200,250);
      pc1yvsz_east = new TH2F("pc1yvsz_east","pc1yvsz_east",100,-150,150,100,-200,250);
      pc1yvsz_west = new TH2F("pc1yvsz_west","pc1yvsz_west",100,-150,150,100,-200,250);
      emcyvsz_east = new TH2F("emcyvsz_east","emcyvsz_east",100,-300,300,100,-300,450);
      emcyvsz_west = new TH2F("emcyvsz_west","emcyvsz_west",100,-300,300,100,-300,450);
      trkringdist=new  TH1F("trkringdist","Ring track distance",100,0.0,20.0);
      
      //MuidTrack
      muidN_depth = new TH1F("muidN_depth","muidN_depth",20,1,5);  //is the L2 decision working?
      muidN_slope = new TH1F("muidN_slope","muidN_slope",100,0,2);  //is the L2 decision working?
      muidN_panel = new TH1F("muidN_panel","muidN_panel",18,-0.5,8.5);
      muidN_trkpevt = new TH1F("muidN_trkpevt","muidN_trkpevt",100,-1,49);
      muidN_hitpevt = new TH1F("muidN_hitpevt","muidN_hitpevt",100,-1,49);
      muidN_hid = new TH1F("muidN_hid","muidN_hid",100,-1,49);
      muidN_vid = new TH1F("muidN_vid","muidN_vid",100,-1,49);
      muidN_pmin = new TH1F("muidN_pmin","muidN_pmin",100,-1,49); 
      muidS_depth = new TH1F("muidS_depth","muidS_depth",20,1,5);  //is the L2 decision working?
      muidS_slope = new TH1F("muidS_slope","muidS_slope",100,0,2);  //is the L2 decision working?
      muidS_panel = new TH1F("muidS_panel","muidS_panel",18,-0.5,8.5);
      muidS_trkpevt = new TH1F("muidS_trkpevt","muidS_trkpevt",100,-1,49);
      muidS_hitpevt = new TH1F("muidS_hitpevt","muidS_hitpevt",100,-1,49);
      muidS_hid = new TH1F("muidS_hid","muidS_hid",100,-1,49);
      muidS_vid = new TH1F("muidS_vid","muidS_vid",100,-1,49);
      muidS_pmin = new TH1F("muidS_pmin","muidS_pmin",100,-1,49);      

      //MuidPair
      muidN_openangle = new TH1F("muidN_openangle","muidN_openangle",100,0,2);  //is the L2 decision working?
      muidN_pairpevt = new TH1F("muidN_pairpevt","muidN_pairpevt",100,-1,49);
      muidN_mass = new TH1F("muidN_mass","muidN_mass",100,-0.5,9.5);
      muidS_openangle = new TH1F("muidS_openangle","muidS_openangle",100,0,2);  //is the L2 decision working?
      muidS_pairpevt = new TH1F("muidS_pairpevt","muidS_pairpevt",100,-1,49);
      muidS_mass = new TH1F("muidS_mass","muidS_mass",100,-0.5,9.5);

      //MutrTrack
      mutrN_trkpevt = new TH1F("mutrN_trkpevt","mutrN_trkpevt",100,-1,49);
      mutrN_E = new TH1F("mutrN_E","mutrN_E",100,0,50);
      mutrN_ptot = new TH1F("mutrN_ptot","mutrN_ptot",100,0,50);
      mutrN_depth = new TH1F("mutrN_depth","mutrN_depth",20,1,5);
      mutrN_sign = new TH1F("mutrN_sign","mutrN_sign",4,-2,2);
      mutrN_theta_muid = new TH1F("mutrN_theta_muid","mutrN_theta_muid",30,-1,2);
      mutrN_theta_st3 = new TH1F("mutrN_theta_st3","mutrN_theta_st3",30,-1,2);
      mutrN_theta_st2 = new TH1F("mutrN_theta_st2","mutrN_theta_st2",30,-1,2);
      mutrN_phi_muid = new TH1F("mutrN_phi_muid","mutrN_phi_muid",20,-5,5);
      mutrN_phi_st3 = new TH1F("mutrN_phi_st3","mutrN_phi_st3",20,-5,5);
      mutrN_phi_st2 = new TH1F("mutrN_phi_st2","mutrN_phi_st2",20,-5,5);
      mutrS_trkpevt = new TH1F("mutrS_trkpevt","mutrS_trkpevt",100,-1,49);
      mutrS_ptot = new TH1F("mutrS_ptot","mutrS_ptot",100,0,50);
      mutrS_E = new TH1F("mutrS_E","mutrS_E",100,0,50);
      mutrS_depth = new TH1F("mutrS_depth","mutrS_depth",20,1,5);
      mutrS_sign = new TH1F("mutrS_sign","mutrS_sign",4,-2,2);
      mutrS_theta_muid = new TH1F("mutrS_theta_muid","mutrS_theta_muid",30,-1,2);
      mutrS_theta_st3 = new TH1F("mutrS_theta_st3","mutrS_theta_st3",30,-1,2);
      mutrS_theta_st2 = new TH1F("mutrS_theta_st2","mutrS_theta_st2",30,-1,2);
      mutrS_phi_muid = new TH1F("mutrS_phi_muid","mutrS_phi_muid",20,-5,5);
      mutrS_phi_st3 = new TH1F("mutrS_phi_st3","mutrS_phi_st3",20,-5,5);
      mutrS_phi_st2 = new TH1F("mutrS_phi_st2","mutrS_phi_st2",20,-5,5);

      //MutrPair
      mutrN_mass = new TH1F("mutrN_mass","mutrN_mass",100,-0.5,9.5);  //is the L2 decision working?
      mutrN_openangle = new TH1F("mutrN_openangle","mutrN_openangle",100,0,2);  //is the L2 decision working?
      mutrN_pairpevt = new TH1F("mutrN_pairpevt","mutrN_pairpevt",100,-1,49);
      mutrS_mass = new TH1F("mutrS_mass","mutrS_mass",100,-0.5,9.5);  //is the L2 decision working?
      mutrS_openangle = new TH1F("mutrS_openangle","mutrS_openangle",100,0,2);  //is the L2 decision working?
      mutrS_pairpevt = new TH1F("mutrS_pairpevt","mutrS_pairpevt",100,-1,49);

      //MuonTrigFilter
      L2MuonTrig_bbc_charge_sum= new TH1F("L2MuonTrig_bbc_charge_sum","L2MuonTrig_bbc_charge_sum",200,0,2000);
      L2MuonTrig_muid_nhitsS= new TH1F("L2MuonTrig_muid_nhitsS","L2MuonTrig_muid_nhitsS",100,0,100);
      L2MuonTrig_muid_nhitsN= new TH1F("L2MuonTrig_muid_nhitsN","L2MuonTrig_muid_nhitsN",100,0,100);
      L2MuonTrig_accepted= new TH1F("L2MuonTrig_accepted","L2MuonTrig_accepted",4,0,2);
      L2MuonTrig_nhitsS_bbc= new TH2F("L2MuonTrig_nhitsS_bbc","L2MuonTrig_nhitsS_bbc",50,0,50,600,0,600);
      L2MuonTrig_nhitsN_bbc= new TH2F("L2MuonTrig_nhitsN_bbc","L2MuonTrig_nhitsN_bbc",50,0,50,600,0,600);
      L2MuonTrig_nhitsNS_bbc= new TH2F("L2MuonTrig_nhitsNS_bbc","L2MuonTrig_nhitsNS_bbc",50,0,50,600,0,600);
      
      //==========================
      //Setup Trigger Stats Tree
      //==========================
      trgstats=new TTree("trgstats","TriggerStatistics");
      
      b=trgstats->Branch("RunInfo",&RunNum,"RunNum/I:FirstEvtNum/I");
      b->GetLeaf("FirstEvtNum")->SetAddress(&FirstEvtNum);
      
      b=trgstats->Branch("TrgInfo",&nEvtsTotal,"nEvtsTotal/I:nEvtsRawRej/I:nEvtsNotData/I:nEvtsGood/I");
      b->GetLeaf("nEvtsRawRej")->SetAddress(&nEvtsRawRej);
      b->GetLeaf("nEvtsNotData")->SetAddress(&nEvtsNotData);
      b->GetLeaf("nEvtsGood")->SetAddress(&nEvtsGood);

      b=trgstats->Branch("LL1Trgs",nLL1Trg,"nEmpty0/F:nClock/F:nBBCLL1/F:nBBCLL1NoVtxCut/F:nZDCLL1wide/F:nZDCLL1narrow/F:nBBCLL1_ZDCNZDCS/F:nERT_Gamma3/F:nERTLL1_2x2BBCLL1/F:nERTLL1_4x4aBBCLL1/F:nERTLL1_4x4c/F:nERTLL1_4x4bBBCLL1/F:nERTLL1_4x4cBBCLL1/F:nERTLL1_EBBCLL1/F:nMUIDLL1_N1DBBCLL1/F:nMUIDLL1_S1DBBCLL1/F:nMUIDLL1_N1SBBCLL1/F:nMUIDLL1_S1SBBCLL1/F:nMUIDLL1_N1D1SBBCLL1/F:nMUIDLL1_S1D1SBBCLL1/F:nMUIDN_1DBBCLL1/F:nMUIDS_1DBBCLL1/F:nY+B+Y+B-/F:nY+B-Y-B+/F:nY0B0/F:nRPC1andRPC2/F:nRPC1orRPC2/F:nUNUSED/F:nPPGPedestal/F:nPPGTestPulse/F:nPPGLaser/F:nNoise/F");

      //nL2EmcHighPtTile[nLL1Trg][nL2Exec,nL2Acc]
      b=trgstats->Branch("L2EmcHighPtTileTrigger",nL2EmcHighPtTile,"nEmpty0[2]/F:nClock[2]/F:nBBCLL1[2]/F:nBBCLL1NoVtxCut[2]/F:nZDCLL1wide[2]/F:nZDCLL1narrow[2]/F:nBBCLL1_ZDCNZDCS[2]/F:nERT_Gamma3[2]/F:nERTLL1_2x2BBCLL1[2]/F:nERTLL1_4x4aBBCLL1[2]/F:nERTLL1_4x4c[2]/F:nERTLL1_4x4bBBCLL1[2]/F:nERTLL1_4x4cBBCLL1[2]/F:nERTLL1_EBBCLL1[2]/F:nMUIDLL1_N1DBBCLL1[2]/F:nMUIDLL1_S1DBBCLL1[2]/F:nMUIDLL1_N1SBBCLL1[2]/F:nMUIDLL1_S1SBBCLL1[2]/F:nMUIDLL1_N1D1SBBCLL1[2]/F:nMUIDLL1_S1D1SBBCLL1[2]/F:nMUIDN_1DBBCLL1[2]/F:nMUIDS_1DBBCLL1[2]/F:nY+B+Y+B-[2]/F:nY+B-Y-B+[2]/F:nY0B0[2]/F:nRPC1andRPC2[2]/F:nRPC1orRPC2[2]/F:nUNUSED[2]/F:nPPGPedestal[2]/F:nPPGTestPulse[2]/F:nPPGLaser[2]/F:nNoise[2]/F"); 

      b=trgstats->Branch("L2AuAuDiElectronTrigger",nL2AuAuDiElectron,"nEmpty0[2]/F:nClock[2]/F:nBBCLL1[2]/F:nBBCLL1NoVtxCut[2]/F:nZDCLL1wide[2]/F:nZDCLL1narrow[2]/F:nBBCLL1_ZDCNZDCS[2]/F:nERT_Gamma3[2]/F:nERTLL1_2x2BBCLL1[2]/F:nERTLL1_4x4aBBCLL1[2]/F:nERTLL1_4x4c[2]/F:nERTLL1_4x4bBBCLL1[2]/F:nERTLL1_4x4cBBCLL1[2]/F:nERTLL1_EBBCLL1[2]/F:nMUIDLL1_N1DBBCLL1[2]/F:nMUIDLL1_S1DBBCLL1[2]/F:nMUIDLL1_N1SBBCLL1[2]/F:nMUIDLL1_S1SBBCLL1[2]/F:nMUIDLL1_N1D1SBBCLL1[2]/F:nMUIDLL1_S1D1SBBCLL1[2]/F:nMUIDN_1DBBCLL1[2]/F:nMUIDS_1DBBCLL1[2]/F:nY+B+Y+B-[2]/F:nY+B-Y-B+[2]/F:nY0B0[2]/F:nRPC1andRPC2[2]/F:nRPC1orRPC2[2]/F:nUNUSED[2]/F:nPPGPedestal[2]/F:nPPGTestPulse[2]/F:nPPGLaser[2]/F:nNoise[2]/F");

      b=trgstats->Branch("L2MutrDimuonNorthTrigger",nL2MutrDimuonNorth,"nEmpty0[2]/F:nClock[2]/F:nBBCLL1[2]/F:nBBCLL1NoVtxCut[2]/F:nZDCLL1wide[2]/F:nZDCLL1narrow[2]/F:nBBCLL1_ZDCNZDCS[2]/F:nERT_Gamma3[2]/F:nERTLL1_2x2BBCLL1[2]/F:nERTLL1_4x4aBBCLL1[2]/F:nERTLL1_4x4c[2]/F:nERTLL1_4x4bBBCLL1[2]/F:nERTLL1_4x4cBBCLL1[2]/F:nERTLL1_EBBCLL1[2]/F:nMUIDLL1_N1DBBCLL1[2]/F:nMUIDLL1_S1DBBCLL1[2]/F:nMUIDLL1_N1SBBCLL1[2]/F:nMUIDLL1_S1SBBCLL1[2]/F:nMUIDLL1_N1D1SBBCLL1[2]/F:nMUIDLL1_S1D1SBBCLL1[2]/F:nMUIDN_1DBBCLL1[2]/F:nMUIDS_1DBBCLL1[2]/F:nY+B+Y+B-[2]/F:nY+B-Y-B+[2]/F:nY0B0[2]/F:nRPC1andRPC2[2]/F:nRPC1orRPC2[2]/F:nUNUSED[2]/F:nPPGPedestal[2]/F:nPPGTestPulse[2]/F:nPPGLaser[2]/F:nNoise[2]/F");

      b=trgstats->Branch("L2MutrDimuonSouthTrigger",nL2MutrDimuonSouth,"nEmpty0[2]/F:nClock[2]/F:nBBCLL1[2]/F:nBBCLL1NoVtxCut[2]/F:nZDCLL1wide[2]/F:nZDCLL1narrow[2]/F:nBBCLL1_ZDCNZDCS[2]/F:nERT_Gamma3[2]/F:nERTLL1_2x2BBCLL1[2]/F:nERTLL1_4x4aBBCLL1[2]/F:nERTLL1_4x4c[2]/F:nERTLL1_4x4bBBCLL1[2]/F:nERTLL1_4x4cBBCLL1[2]/F:nERTLL1_EBBCLL1[2]/F:nMUIDLL1_N1DBBCLL1[2]/F:nMUIDLL1_S1DBBCLL1[2]/F:nMUIDLL1_N1SBBCLL1[2]/F:nMUIDLL1_S1SBBCLL1[2]/F:nMUIDLL1_N1D1SBBCLL1[2]/F:nMUIDLL1_S1D1SBBCLL1[2]/F:nMUIDN_1DBBCLL1[2]/F:nMUIDS_1DBBCLL1[2]/F:nY+B+Y+B-[2]/F:nY+B-Y-B+[2]/F:nY0B0[2]/F:nRPC1andRPC2[2]/F:nRPC1orRPC2[2]/F:nUNUSED[2]/F:nPPGPedestal[2]/F:nPPGTestPulse[2]/F:nPPGLaser[2]/F:nNoise[2]/F");

      b=trgstats->Branch("L2EMuTrigger",nL2EMu,"nEmpty0[2]/F:nClock[2]/F:nBBCLL1[2]/F:nBBCLL1NoVtxCut[2]/F:nZDCLL1wide[2]/F:nZDCLL1narrow[2]/F:nBBCLL1_ZDCNZDCS[2]/F:nERT_Gamma3[2]/F:nERTLL1_2x2BBCLL1[2]/F:nERTLL1_4x4aBBCLL1[2]/F:nERTLL1_4x4c[2]/F:nERTLL1_4x4bBBCLL1[2]/F:nERTLL1_4x4cBBCLL1[2]/F:nERTLL1_EBBCLL1[2]/F:nMUIDLL1_N1DBBCLL1[2]/F:nMUIDLL1_S1DBBCLL1[2]/F:nMUIDLL1_N1SBBCLL1[2]/F:nMUIDLL1_S1SBBCLL1[2]/F:nMUIDLL1_N1D1SBBCLL1[2]/F:nMUIDLL1_S1D1SBBCLL1[2]/F:nMUIDN_1DBBCLL1[2]/F:nMUIDS_1DBBCLL1[2]/F:nY+B+Y+B-[2]/F:nY+B-Y-B+[2]/F:nY0B0[2]/F:nRPC1andRPC2[2]/F:nRPC1orRPC2[2]/F:nUNUSED[2]/F:nPPGPedestal[2]/F:nPPGTestPulse[2]/F:nPPGLaser[2]/F:nNoise[2]/F");

    }


  //////////////////////////////////////////////////////////////////////////
  // Trigger selection
  //////////////////////////////////////////////////////////////////////////
  
  // Level 1
  // These guys should all have been placed on the run and dst nodes already by head/trig
    TrigRunLvl1 *trRunLvl1 = findNode::getClass<TrigRunLvl1>(runNode,"TrigRunLvl1");
  TrigLvl1 *trLvl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  if (!trLvl1)
    {
      cout <<PHWHERE << " No TrigLvl1 object, return and do nothing" 
	   << endl;
      return 0;
    }


  // Level 2
  // Get the level 2 trigger setup

  TrigRunLvl2 *trigRunLvl2 = findNode::getClass<TrigRunLvl2>(runNode,lvl2trigrunnodename);
  if (!trigRunLvl2)
    {
      cout << " No TrigRunLvl2 object " << lvl2trigrunnodename << " - return and do nothing" 
	   << endl;
      return 0;
    }
  
  Lvl2DecisionOut *trigLvl2 = findNode::getClass<Lvl2DecisionOut>(topNode,lvl2decisionnodename);
  if (!trigLvl2)
    {
      cout <<PHWHERE << " No Lvl2DecisionOut object " << lvl2decisionnodename << " do nothing and return" 
	   << endl;
      return 0;
    }

  // Get a L2DecisionHelper object, initialize it with trigLvl2 and trigRunLvl2
  L2DecisionHelper *l2dh = new L2DecisionHelper(trigLvl2, trigRunLvl2);
  if(!l2dh)
    {
      cout << PHWHERE
	   << " No L2decisionHelper object, do nothing and return" << endl; 
      return 0;
    }

  // get the Lvl1 and Lvl2 names only once
  if(got_names==0)
    {
      got_names=1;

      if(verbosity>1)
	cout << "Lvl2FilterMon: Lvl1 algorithm names are:" << endl;
      for(UINT ilvl1=0;ilvl1<32;ilvl1++)
	{
	  Lvl1Name[ilvl1]=trRunLvl1->get_lvl1_trig_name_bybit(ilvl1);
	  
	  if(verbosity>1)
	    cout << " Lvl1 name = " <<  Lvl1Name[ilvl1] << endl;	  
	}
      
      if(verbosity>1)
	 cout << "Lvl2FilterMon: Lvl2 algorithm names are:" << endl;
      for(UINT ilvl2=0;ilvl2<32;ilvl2++)
	{
	  int lvl2bit = trigRunLvl2->get_lvl2_trig_bit(ilvl2);
	  if (strcmp(Lvl2Name[lvl2bit], "")==0)
	    {
	      Lvl2Name[lvl2bit]=trigRunLvl2->get_lvl2_trig_name(ilvl2);
	      
	      if(verbosity>1)
		cout << "  ilvl2 = " << ilvl2 << " Lvl2 bit = " 
		     << trigRunLvl2->get_lvl2_trig_bit(ilvl2) 
		     << " Lvl2 name = " <<  Lvl2Name[lvl2bit] << endl;
	    }
	}
    }

  recoConsts *rc = recoConsts::instance();
  
  Lvl2DecisionOut *l2decisionOut =l2dh->get_lvl2DecisionOut();
  
  if(verbosity>1)
    {
      cout << "Lvl2FilterMon: dump of Lvl2DecisionOut: " << endl;
      l2decisionOut->dump(cout);
    }

  for (int ilvl1 = 0; ilvl1 < 32; ilvl1++)
    {
      
      // Get the Lvl1 trigger decision - we want scaled, right?
      
      // This is a Kluge for Run 4 simulated PRDF's where the trLvl1 has not been filled
      // Note that the level 1 bit that level 2 triggers are associated with is hard coded to 2 for sims

      if( (trLvl1->get_lvl1_trigscaled_bit(ilvl1) && real_data_flag == 1) ||
	  (ilvl1==2 && !(real_data_flag == 1)) ) 
	{
	  if(verbosity>1)
	    cout << " Lvl1:  bit " << ilvl1 << " name " 
		 << trRunLvl1->get_lvl1_trig_name_bybit(ilvl1) 
		 << " fired " <<endl;
	  
	  Lvl1Scaled[ilvl1]++;
	  
	  // Get the LVl2 trigger decisions for this Lvl1
	  if(verbosity>1)
	    cout << "Lvl2FilterMon: Get Lvl2 trigger decisions" << endl;
	  
	  for (int ilvl2 = 0; ilvl2 < 32; ilvl2++)
	    {

	      Lvl2Decision decis1 
		= l2decisionOut->getLvl1AlgorithmDecision(ilvl1, ilvl2);

	      if (decis1.isEmpty() == false)
		{
		  // then ilvl2 was associated with ilvl1 
		  // get the Lvl2 decision
		  
		  if(decis1.didErrorOccur())
		    cout << "Lvl2FilterMon: Error occurred for ilvl2 = " 
			 << ilvl2 << endl;

		  if(decis1.wasExecuted())
		    {
		      // This is our denominator for rejection
		      
		      Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1]++;
		      
		      if(decis1.wasAlgorithmAccepted())
			{
			  // Lvl2 algorithm succeeded on this Lvl1 bit 
			  // This is the numerator for rejection
			  
			  Lvl2FiredLvl1Scaled[ilvl2][ilvl1]++;

			  if(verbosity>1)
			    {
			      cout << "  nevt " << nevt << " ilvl1 " << ilvl1 
				   << " ilvl2 " << ilvl2 << " lvl2 name " 
				   << Lvl2Name[ilvl2] 
				   << " lvl2 trig bit " << trigRunLvl2->get_lvl2_trig_bit(ilvl2) << endl;
			      cout << " isEmpty = " << decis1.isEmpty() << endl 
				   << " wasExecuted = " << decis1.wasExecuted() << endl
				   << "  wasAccepted = " << decis1.wasAccepted() 
				   << endl
				   << " wasAlgorithmAccepted = " 
				   << decis1.wasAlgorithmAccepted() << endl
				   << " didErrorOccur = " << decis1.didErrorOccur() 
				   << endl
				   << " acepptEvent = " << decis1.acceptEvent() << endl
				   << endl;
			    }
			  
			} //end acc loop

		      if(decis1.didErrorOccur())
			{

			  // This does not work - is there no Lvl2 info for 
			  // events that were accepted on errors?

			  // Lvl2 algorithm accepted on this Lvl1 bit
			  // because it had an error
			  
			  Lvl2ErrorLvl1Scaled[ilvl2][ilvl1]++;
			} //end err loop 
		    }  //end exec loop 
		} 
	      if (ilvl2==0){
		nL2EmcHighPtTile[ilvl1][0] = Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1];
		nL2EmcHighPtTile[ilvl1][1] = Lvl2FiredLvl1Scaled[ilvl2][ilvl1];
	      }
	      else {
		if(ilvl2==7){
		  nL2AuAuDiElectron[ilvl1][0] = Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1];
		  nL2AuAuDiElectron[ilvl1][1] = Lvl2FiredLvl1Scaled[ilvl2][ilvl1];
		}
		else {
		  if(ilvl2==8){
		  nL2MutrDimuonNorth[ilvl1][0] = Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1];
		  nL2MutrDimuonNorth[ilvl1][1] = Lvl2FiredLvl1Scaled[ilvl2][ilvl1];
		  }
		  else {
		    if(ilvl2==9){
		      nL2MutrDimuonSouth[ilvl1][0] = Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1];
		      nL2MutrDimuonSouth[ilvl1][1] = Lvl2FiredLvl1Scaled[ilvl2][ilvl1];
		    }
		    else {
		      if(ilvl2==15){
			nL2EMu[ilvl1][0] = Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1];
			nL2EMu[ilvl1][1] = Lvl2FiredLvl1Scaled[ilvl2][ilvl1];
		      }
		      else continue;
		    }
		  }
		}
	      }
	    } // end lvl2 loop 
	}

      //if (ilvl1!=24)
      //cout<<"Counter =  "<<ilvl1<<", Lvl1Name= "<<Lvl1Name[ilvl1]<<endl;

    } // end lvl1 loop 

  delete l2dh;
  
  //===========================
  //Make primitive histograms
  //===========================
  // The level 2 primitive data subnodes in the DST node are accessed through a Lvl2OutArray object

  Lvl2OutArray *lvl2outarray = 0;
  PHTypedNodeIterator<Lvl2OutArray> lvl2iter(dstNode);
  Lvl2OutArrayNode_t *Lvl2OutNode = lvl2iter.find(lvl2outarraynodename);
  if (Lvl2OutNode) lvl2outarray = Lvl2OutNode->getData();  
  if (!lvl2outarray) {
    cout << PHWHERE << "failed to get Lvl2OutArray object " << lvl2outarraynodename << endl;
    return False;
  }

  ///////////////////////////////////////////////////////////////////
  // Electron candidates
  ///////////////////////////////////////////////////////////////////

  if (lvl2outarray->GetPrimitive( "L2AuAuElectronCandidate"))
  {
    L2AuAuElectronCandidateRBP electron(0);
    
    int nelec = electron->Candidate.size();
    
    //cout << " Found " << nelec << " electron candidates" << endl;

    for(int icand=0;icand<nelec;icand++)
      {
	// Histogram ptotpc3 vs energy
	momvsenergy->Fill(electron->Candidate[icand].energy,electron->Candidate[icand].ptotpc3);
	
	// Histogram Zvertex
	zvertplot->Fill(electron->Candidate[icand].zvertex);
	
	// Histogram ypc3 vs zpc3
	if(electron->Candidate[icand].xpc3 > 0)
	  pc3yvsz_east->Fill(electron->Candidate[icand].zpc3,electron->Candidate[icand].ypc3);
	else 
	  pc3yvsz_west->Fill(electron->Candidate[icand].zpc3,electron->Candidate[icand].ypc3);

	if(electron->Candidate[icand].xpc1 > 0)
	  pc1yvsz_east->Fill(electron->Candidate[icand].zpc1,electron->Candidate[icand].ypc1);
	else 
	  pc1yvsz_west->Fill(electron->Candidate[icand].zpc1,electron->Candidate[icand].ypc1);

	if(electron->Candidate[icand].xemc > 0)
	  emcyvsz_east->Fill(electron->Candidate[icand].zemc,electron->Candidate[icand].yemc);
	else 
	  emcyvsz_west->Fill(electron->Candidate[icand].zemc,electron->Candidate[icand].yemc);

	if(electron->Candidate[icand].xrich > 0)
	  richyvsz_east->Fill(electron->Candidate[icand].zrich,electron->Candidate[icand].yrich);
	else 
	  richyvsz_west->Fill(electron->Candidate[icand].zrich,electron->Candidate[icand].yrich);

	float eoverp = electron->Candidate[icand].energy / electron->Candidate[icand].ptotpc3;
	eoverpvsnpe->Fill(electron->Candidate[icand].npe,eoverp);
	eoverpvsp->Fill(electron->Candidate[icand].ptotpc3,eoverp);

	// Look at the distance from the track to the ring for likely electrons
	if(electron->Candidate[icand].npe>8.0 && eoverp > 1.0 &&  electron->Candidate[icand].ptotpc3>1.0)
	   trkringdist->Fill(electron->Candidate[icand].TrkRingDist);
     }
  }

  ////////////////////////////////////////////////////////////////////
  // Electron pairs
  ////////////////////////////////////////////////////////////////////
  
  
  if (lvl2outarray->GetPrimitive("L2AuAuElectronInvMass"))
    {                                                                                
      L2AuAuElectronInvMassRBP epairlvl2(0);
      
      int npair = epairlvl2->ElectronPair.size();
      
      //cout << " Found " << npair << " electron pairs" << endl;    
      for(int ipair=0;ipair<npair;ipair++)
	{
	  // Histogram mass vs pT
	  massvspt->Fill(epairlvl2->ElectronPair[ipair].Mass,epairlvl2->ElectronPair[ipair].Pt);
	}
    }
  
  
  ////////////////////////////////////////////////////////////////////
  // Muons
  ////////////////////////////////////////////////////////////////////
  //SOUTH ARM=0, NORTH ARM=1
  int armon[2]={0,0};
  if (trLvl1->get_lvl1_trigscaled_bit(19)) armon[0]=1;
  if (trLvl1->get_lvl1_trigscaled_bit(18)) armon[1]=1;
  //lvl2outarray->dump_info();
  
  if (lvl2outarray->GetPrimitive("L2MuiPairs"))
    {
      int narm = 2;
      int npairpevt_muidN = 0;
      int npairpevt_muidS = 0;

      for(int iarm=0; iarm<narm; iarm++)
	{
	  if (armon[iarm]==0) continue;
	  L2MuiPairsRBP muipairs(iarm); //south (iarm=0)
	  //if (! muipairs.wasRead() )
	  //{
	  //  cout<< "L2 muipairs was not read: iarm= "<<iarm<<endl;
	  //  continue;
	  //}

	  int nmuipairs = muipairs->mupairs.size();
	  //cout << "L2MuiPairs has " << nmuipairs << " entries " << endl;

	  if(iarm==0) npairpevt_muidS = nmuipairs;      
	  else npairpevt_muidN = nmuipairs;
	  for(int npair=0; npair<nmuipairs; npair++)
	    { 
	      if(iarm==0) {
		muidS_openangle->Fill(muipairs->mupairs[npair].openangle);
		muidS_mass->Fill(muipairs->mupairs[npair].mass);
	      }
	      else {
		muidN_openangle->Fill(muipairs->mupairs[npair].openangle);
		muidN_mass->Fill(muipairs->mupairs[npair].mass);
	      }
	    }
	} //end arm loop
      muidS_pairpevt->Fill(npairpevt_muidS);
      muidN_pairpevt->Fill(npairpevt_muidN);

    }

  if (lvl2outarray->GetPrimitive("L2MuiTracks"))
    {
      int narm = 2;
      int ntrkpevt_muidN = 0;
      int ntrkpevt_muidS = 0;
      int nhitpevt_muidN = 0;
      int nhitpevt_muidS = 0;

      for(int iarm=0; iarm<narm; iarm++)
	{
	  if (armon[iarm]==0) continue;
	  L2MuiTracksRBP muitracks(iarm);
	  //if (! muitracks.wasRead() )
	  //{
	  //  cout<< "L2 muitracks was not read: iarm= "<<iarm<<endl;
	  //  continue;
	  //}

	  int nmuitracks = muitracks->tracks.size();
	  //cout << "L2MuiTracks has " << nmuitracks << " arm = "<<iarm<<" entries " << endl;

	  if(iarm==0) ntrkpevt_muidS = nmuitracks;      
	  else ntrkpevt_muidN = nmuitracks;
	  
	  for(int ntrack=0; ntrack<nmuitracks; ntrack++)
	    { 
	      if(iarm==0) {
		muidS_panel->Fill(muitracks->tracks[ntrack].panel);
		nhitpevt_muidS += muitracks->tracks[ntrack].hitsum;
		muidS_depth->Fill(muitracks->tracks[ntrack].depth);
		muidS_slope->Fill(muitracks->tracks[ntrack].slope);
		muidS_hid->Fill(muitracks->tracks[ntrack].hid);
		muidS_vid->Fill(muitracks->tracks[ntrack].vid);
		muidS_pmin->Fill(muitracks->tracks[ntrack].pmin);
	      }
	      else {
		muidN_panel->Fill(muitracks->tracks[ntrack].panel);
		nhitpevt_muidN += muitracks->tracks[ntrack].hitsum;
		muidN_depth->Fill(muitracks->tracks[ntrack].depth);
		muidN_slope->Fill(muitracks->tracks[ntrack].slope);
		muidN_hid->Fill(muitracks->tracks[ntrack].hid);
		muidN_vid->Fill(muitracks->tracks[ntrack].vid);
		muidN_pmin->Fill(muitracks->tracks[ntrack].pmin);
	      }
	    }
	}
    
      muidS_trkpevt->Fill(ntrkpevt_muidS);
      muidS_hitpevt->Fill(nhitpevt_muidS);
      muidN_trkpevt->Fill(ntrkpevt_muidN);
      muidN_hitpevt->Fill(nhitpevt_muidN);
    }
  

  if (lvl2outarray->GetPrimitive("L2MutrPairPrimitive"))
    {

      int narm = 2;
      int npairpevt_mutrN = 0;
      int npairpevt_mutrS = 0;

      for(int iarm=0; iarm<narm; iarm++)
	{
	  if (armon[iarm]==0) continue;
	  L2MutrPairPrimitiveRBP mutrpair(iarm);
	  //if (! mutrpair.wasRead() )
	  //{
	  //  cout<< "L2 mutrpair was not read: iarm= "<<iarm<<endl;
	  //  continue;
	  //}

	  int nmutrpair = mutrpair->pairlist.size();
	  //cout << "L2MutrPair has " << nmutrpair << " entries " << endl;

	  if(iarm==0) npairpevt_mutrS = nmutrpair;      
	  else npairpevt_mutrN = nmutrpair;

	  for(int npair=0; npair<nmutrpair; npair++)
	    { 
	      if(iarm==0) {
		mutrS_mass->Fill(mutrpair->pairlist[npair].mass);
		mutrS_openangle->Fill(mutrpair->pairlist[npair].open_angle);
	      }
	      else {
		mutrN_mass->Fill(mutrpair->pairlist[npair].mass);
		mutrN_openangle->Fill(mutrpair->pairlist[npair].open_angle);
	      }
	    } //end pair loop
	} //end arm loop
      mutrS_pairpevt->Fill(npairpevt_mutrS);
      mutrN_pairpevt->Fill(npairpevt_mutrN);
    }

  if (lvl2outarray->GetPrimitive("L2MutrTrackPrimitive"))
    {
      int narm = 2;
      int ntrkpevt_mutrN = 0;
      int ntrkpevt_mutrS = 0;
      float mutrN_px = 0;
      float mutrN_py = 0;
      float mutrN_pz = 0;
      float imutrN_ptot = 0;
      float mutrS_px = 0;
      float mutrS_py = 0;
      float mutrS_pz = 0;
      float imutrS_ptot = 0;

      for(int iarm=0; iarm<narm; iarm++)
	{
	  if (armon[iarm]==0) continue;
	  L2MutrTrackPrimitiveRBP mutrtracks(iarm);
	  //if (! mutrtracks.wasRead() )
	  //{
	  //  cout<< "L2 mutrtracks was not read: iarm= "<<iarm<<endl;
	  //  continue;
	  //}

	  int nmutrtracks = mutrtracks->tracklist.size();
	  //cout << "L2MutrTracks has " << nmutrtracks << " arm = "<<iarm<<" entries " << endl;

	  if(iarm==0) ntrkpevt_mutrS = nmutrtracks;      
	  else ntrkpevt_mutrN = nmutrtracks;
	  
	  for(int ntrack=0; ntrack<nmutrtracks; ntrack++)
	    { 
	      if(iarm==0) {
		mutrS_px = mutrtracks->tracklist[ntrack].p.getX();
		mutrS_py = mutrtracks->tracklist[ntrack].p.getY();
		mutrS_pz = mutrtracks->tracklist[ntrack].p.getZ();
		imutrS_ptot = sqrt(mutrS_px*mutrS_px+mutrS_py*mutrS_py+mutrS_pz*mutrS_pz);
		mutrS_ptot->Fill(imutrS_ptot);
		mutrS_E->Fill(mutrtracks->tracklist[ntrack].E);
		mutrS_depth->Fill(mutrtracks->tracklist[ntrack].depth);
		mutrS_sign->Fill(mutrtracks->tracklist[ntrack].sign);
		mutrS_theta_muid->Fill(mutrtracks->tracklist[ntrack].theta_muid);
		mutrS_theta_st2->Fill(mutrtracks->tracklist[ntrack].theta_st2);
		mutrS_theta_st3->Fill(mutrtracks->tracklist[ntrack].theta_st3);
		mutrS_phi_muid->Fill(mutrtracks->tracklist[ntrack].phi_muid);
		mutrS_phi_st2->Fill(mutrtracks->tracklist[ntrack].phi_st2);
		mutrS_phi_st3->Fill(mutrtracks->tracklist[ntrack].phi_st3);	      }
	      else {
		mutrN_px = mutrtracks->tracklist[ntrack].p.getX();
		mutrN_py = mutrtracks->tracklist[ntrack].p.getY();
		mutrN_pz = mutrtracks->tracklist[ntrack].p.getZ();
		imutrN_ptot = sqrt(mutrN_px*mutrN_px+mutrN_py*mutrN_py+mutrN_pz*mutrN_pz);
		mutrN_ptot->Fill(imutrN_ptot);
		mutrN_E->Fill(mutrtracks->tracklist[ntrack].E);
		mutrN_depth->Fill(mutrtracks->tracklist[ntrack].depth);
		mutrN_sign->Fill(mutrtracks->tracklist[ntrack].sign);
		mutrN_theta_muid->Fill(mutrtracks->tracklist[ntrack].theta_muid);
		mutrN_theta_st2->Fill(mutrtracks->tracklist[ntrack].theta_st2);
		mutrN_theta_st3->Fill(mutrtracks->tracklist[ntrack].theta_st3);
		mutrN_phi_muid->Fill(mutrtracks->tracklist[ntrack].phi_muid);
		mutrN_phi_st2->Fill(mutrtracks->tracklist[ntrack].phi_st2);
		mutrN_phi_st3->Fill(mutrtracks->tracklist[ntrack].phi_st3);	      }
	    }
	}
      mutrS_trkpevt->Fill(ntrkpevt_mutrS);
      mutrN_trkpevt->Fill(ntrkpevt_mutrN);
    }


  if (lvl2outarray->GetPrimitive("L2MuonTrigFilter"))
    {
      L2MuonTrigFilterRBP muontrigfilter(0);
      
      L2MuonTrig_bbc_charge_sum->Fill(muontrigfilter->bbc_charge_sum);
      L2MuonTrig_muid_nhitsS->Fill(muontrigfilter->muid_nhits0);
      L2MuonTrig_muid_nhitsN->Fill(muontrigfilter->muid_nhits1);
      L2MuonTrig_accepted->Fill(muontrigfilter->accepted);
      L2MuonTrig_nhitsS_bbc->Fill((float)muontrigfilter->muid_nhits0,muontrigfilter->bbc_charge_sum);
      L2MuonTrig_nhitsN_bbc->Fill((float)muontrigfilter->muid_nhits1,muontrigfilter->bbc_charge_sum);
      float nhits_sum = (float)muontrigfilter->muid_nhits0+(float)muontrigfilter->muid_nhits1;
      L2MuonTrig_nhitsNS_bbc->Fill(nhits_sum,muontrigfilter->bbc_charge_sum);
    }
  
  if(verbosity>0)
    cout << "Leaving Lvl2FilterMon:: process_event" << endl;

  return 0;
}

int Lvl2FilterMon::BeginRun(const int runno) 
{
  runnumber=runno;
  return 0;
}

int Lvl2FilterMon::EndRun(const int runno) 
{
  cout << endl << "Lvl2FilterMon::EndRun: Total events = " << nevt
       << endl << endl;

  // Trigger rejection statistics

  // Write rejection summary to the log file and to a histogram
  // There can be multiple lvl1 triggers per Lvl2 trigger. 

  int algcounter = 0;

  //possibly should count from 1-33
  for (int ilvl1=0;ilvl1<32;ilvl1++)
    {
      //for TrgStats tree
      nLL1Trg[ilvl1] = Lvl1Scaled[ilvl1];

      if(Lvl1Scaled[ilvl1])
	{
	  cout << endl << "L2: " << Lvl1Name[ilvl1] 
	       << " fired " << Lvl1Scaled[ilvl1] << " times" << endl;
	  cout << setw(3) << "L2:" << setw(27)<< "Lvl2 Algorithm name" << setw(12) << "Executed" 
	       << setw(12) << "Accepted" << setw(12) << "Errors" 
	       << setw(12) << "Rejection" << endl;

	  cout << setw(3) << "L2:" << setw(27)<< "-------------------" << setw(12) << "--------" 
	       << setw(12) << "--------" << setw(12) << "------" 
	       << setw(12) << "---------" << endl;


	  for(int ilvl2=0;ilvl2<32;ilvl2++)
	    {
	      if(Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1] > 0)
		{
		  cout << setw(3) << "L2:" << setw(27) << Lvl2Name[ilvl2]
		       << setw(12) << Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1] 
		       << setw(12) << Lvl2FiredLvl1Scaled[ilvl2][ilvl1]
		       << setw(12) << Lvl2ErrorLvl1Scaled[ilvl2][ilvl1];
		  float rejection = -1;
		  float rejection_error = -1;
		  
		  // counts Lvl1/Lvl2 combinations

		  algcounter++;
		  
		  if(Lvl2FiredLvl1Scaled[ilvl2][ilvl1] > 0)
		    {
		      rejection = Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1] / 
			Lvl2FiredLvl1Scaled[ilvl2][ilvl1];
		      float term = 1.0 / Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1] +
			1.0 / Lvl2FiredLvl1Scaled[ilvl2][ilvl1];
		      
		      rejection_error = rejection * sqrt(term);
		    }
		  else
		    {
		      //rejection = 10E+31;
		      rejection = 9999;
		      rejection_error = rejection * sqrt(1.0 / Lvl2ExecutedLvl1Scaled[ilvl2][ilvl1]);
		    }

		  cout << setw(12) << rejection << endl;
		  
		  rejectplot->SetBinContent(algcounter,rejection);
		  rejectplot->SetBinError(algcounter,rejection_error);
		}
	    }
	}  
    }


  cout << endl << "Lvl2 trigger totals:" << endl;
  cout << setw(12) << "Lvl2 Algo" << setw(36) << "Name" << setw(12) << "Fired" 
       << setw(12) << "errors" << endl;
  
  for (int i2=0;i2<32;i2++)
    {
      float tmp_fired = 0;
      float tmp_error = 0;
      
      for(int i1=0;i1<32;i1++)
	{
	  tmp_fired = tmp_fired + Lvl2FiredLvl1Scaled[i2][i1];
	  tmp_error = tmp_error + Lvl2ErrorLvl1Scaled[i2][i1];
	}
      cout << setw(12) << i2 << setw(36) << Lvl2Name[i2] << setw(12) << tmp_fired << setw(12) << tmp_error << endl;
    }
  
  //Fill TrgStatsTree
  RunNum = runnumber;
  FirstEvtNum = esq;
  nEvtsTotal = nevt;
  nEvtsRawRej = 0;
  nEvtsGood = nEvtsTotal-nEvtsRawRej-nEvtsNotData;
  if(trgstats)  trgstats->Fill();
  else {
    cout<<"trgstats object doesn't exist"<<endl;
    return 1;

  }

  //write histograms to a file
  hfile->Write();

  // Write a victory message to a text file in the form of the number of events processed
  // This can be used to flag success of the filtering job
  //AAB: change victory file to a root file that contains the trigger stats

  char filename[100];
  if(esq < 10000)  sprintf(filename,"filter-succeeded-%010u-%04d.root",runnumber,esq);
  else  sprintf(filename,"filter-succeeded-%010u-%d.root",runnumber,esq);

  printf("Saving trgstats to file %s\n",filename);
  TFile vicfile(filename,"RECREATE");
  vicfile.cd();
  if (trgstats) trgstats->Write();
  else {
    cout<<"trgstats object doesn't exist"<<endl;
    return 1;
  }

  vicfile.Close();

  return 0;
}
