/*
 * QAMui.C
 * $Id: QAMui.C,v 1.4 2017/07/15 15:19:06 phnxbld Exp $
 *
 * Book and fill mui histograms.
 */

#include <PHIODataNode.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllServer.h>
#include <TMutNode.h>

#include "QAMui.h"
#include "QADefs.h"


#include <BbcOut.h>
#include <TMuiHVTable.hh>
#include <PHTimeStamp.h>
#include <PdbBankID.hh>
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <TrigRunLvl1.h>
#include <VtxOut.h>
#include <TMuiRoadMapO.h>
#include <TMui1DRoadMapO.h>
#include <TMuiHitMapO.h>
#include <TMuiClusterMapO.h>

// skip the run header stuff?
#include <RunHeader.h>
#include <EventHeader.h>
#include <TMutTrkMap.h>
#include <iostream>

using namespace std;

// Here we select which triggers will be studied
// This may change from Run-to-Run: the selection below is for Run-5
// (Run4 only has min-bias trigger and run3 see bellow).

// Description for the triggers
// * acts as a wildcard in triggerhelper selection string

const char *trigstring[] = 
{ "MUIDLL1_S1S&BBCLL1", "MUIDLL1_S1D&BBCLL1","MUIDLL1_S1D1S&BBCLL1", // the same triggers for each arm
  "MUIDLL1_N1S&BBCLL1", "MUIDLL1_N1D&BBCLL1","MUIDLL1_N1D1S&BBCLL1"}; // least restrictive trigger first

//  Need to figure out the trigger strings for Run4
const int trigarm[QAMui::N_TRIGGERS] = { 0, 0, 0, 1, 1, 1}; 

//________________________________________________________
int QAMui::InitRun(PHCompositeNode *topNode)
{
  // limits for the histograms, when there are more than one with same binning
  // we only care about the selected triggers.
  int nbits = N_TRIGGERS; 
  int nroads = 50;
  int nhits = 200;

  Fun4AllServer *se = Fun4AllServer::instance();

  try
    {
      TMutNode<TMuiHitMapO>::find_node(topNode, "TMuiHitMapO");
      TMutNode<TMutTrkMap>::find_node(topNode, "TMutTrkMap");
      TMutNode<TMuiRoadMapO>::find_node(topNode, "TMuiRoadMapO");
    }
  catch ( exception& e)
    {
      se->unregisterSubsystem(this);
      return 0;
    }
  
  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
  {
    hm = new Fun4AllHistoManager(HistoManagerName);
    se->registerHistoManager(hm);
  }
  
  muiRunNumber = new TH1F("muiRunNumber","Run Number",1,-0.5,0.5);
  hm->registerHisto(muiRunNumber);
  muiDataType = new TH1F("muiDataType","Data type: -1 (unknown), 0 (run2_AuAu), 1(run2_pp), 2(run3_pp), 3(dA), 4(AuAu-Run4_200GeV), 5(AuAu-Run4_63GeV), 6(PP_Run4), 7(CuCu_Run5), 8(PP_Run5)", 
    1,-0.5,0.5);
  hm->registerHisto(muiDataType);
  muiMinBias = new TH1F("muiMinBias","Number of Min Bias Events Examined", 
    1,-0.5,0.5);
  hm->registerHisto(muiMinBias);
  muiTriggerBit = new TH1F("muiTriggerBit","Trigger Bit",
    nbits, -0.5, nbits-0.5);
  hm->registerHisto(muiTriggerBit);
  muiTriggerBit_scaled = new TH1F("muiTriggerBit_scaled","Trigger Bit Scaled",
    nbits, -0.5, nbits-0.5);
  hm->registerHisto(muiTriggerBit_scaled);
  muiTriggerBit_live = new TH1F("muiTriggerBit_live","Trigger Bit Live",
    nbits, -0.5, nbits-0.5);
  hm->registerHisto(muiTriggerBit_live);
  
  char id[128], title[128];
  for (int trig = 0; trig<N_TRIGGERS; trig++)
  {	        
    sprintf(id,"muiNHits[%d]", trig);
    sprintf(title,"Number of MuID Hits - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiNHits[trig] = new TH1F(id, title,
      nhits, -0.5, nhits-0.5);
    hm->registerHisto(muiNHits[trig]);
    sprintf(id,"muiNRoads[%d]", trig);
    sprintf(title,"Number of MuID Roads - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiNRoads[trig] = new TH1F(id, title,
      nroads, -0.5, nroads-0.5);
    hm->registerHisto(muiNRoads[trig]);
    
    sprintf(id,"muiNRoads_NoGhost[%d]", trig);
    sprintf(title,"Number of MuID Roads_NoGhost - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiNRoads_NoGhost[trig] = new TH1F(id, title,
      nroads, -0.5, nroads-0.5);
    hm->registerHisto(muiNRoads_NoGhost[trig]);
    
    sprintf(id,"muiNRoads_Golden[%d]", trig);
    sprintf(title,"Number of MuID Roads_Golden - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiNRoads_Golden[trig] = new TH1F(id, title,
      nroads, -0.5, nroads-0.5);
    hm->registerHisto(muiNRoads_Golden[trig]);
    
    sprintf(id,"muiNGolden_per_Road[%d]", trig);
    sprintf(title,"Number of MuID Golden_per_Road - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiNGolden_per_Road[trig] = new TH1F(id, title,
      51,-0.01,1.01);  
    hm->registerHisto(muiNGolden_per_Road[trig]);
    
    sprintf(id,"muiRoadTrack_MatchPos[%d]", trig);
    sprintf(title,"Road-Track Match Position - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiRoadTrack_MatchPos[trig] = new TH1F(id, title,
      100,0.,100.);
    hm->registerHisto(muiRoadTrack_MatchPos[trig]);
    
    sprintf(id,"muiLastPlane[%d]", trig);
    sprintf(title,"Depth of Road in the MUID - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiLastPlane[trig] = new TH1F(id, title,
      6,-0.5,5.5);
    hm->registerHisto(muiLastPlane[trig]);
    
    sprintf(id,"muiMaxHits[%d]", trig);
    sprintf(title,"Max Hits in the MUID - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiMaxHits[trig] = new TH1F(id, title,
      21,-0.5,20.5);
    hm->registerHisto(muiMaxHits[trig]);
    
    sprintf(id,"muiFitQuality[%d]", trig);
    sprintf(title,"Fit Quality in the MUID - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiFitQuality[trig] = new TH1F(id, title,
      26,-0.5,25.5);
    hm->registerHisto(muiFitQuality[trig]);
    
    sprintf(id,"muiBBCZVertex[%d]", trig);
    sprintf(title,"ZVertex from BBC - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiBBCZVertex[trig] = new TH1F(id, title,
      100,-150.,150.);
    hm->registerHisto(muiBBCZVertex[trig]);
    
    sprintf(id,"muiChainHits[%d]", trig);
    sprintf(title,"Hits in HV Chain - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiChainHits[trig] = new TH1F(id, title,
      360, -0.5, 359.5);
    hm->registerHisto(muiChainHits[trig]);
    
    sprintf(id,"muiRoadGap0[%d]", trig);
    sprintf(title,"MuID Road Intersection with First MuID gap - %s", 
      trigstring[trig]);
    muiRoadGap0[trig] = new TH2F(id, title,
      144,-604.8,604.8,
      144,-604.8,604.8);
    hm->registerHisto(muiRoadGap0[trig]);
    
    sprintf(id,"muiRoadRefPos[%d]", trig);
    sprintf(title,"MuID Road Intersection with Vertex Plane - %s", 
      trigstring[trig]);
    muiRoadRefPos[trig] = new TH2F(id, title,
      80,-200.0,200.0,
      80,-200.0,200.0);
    hm->registerHisto(muiRoadRefPos[trig]);
    
    sprintf(id,"muiP_Depth[%d]", trig);
    sprintf(title,"Momentum vs Depth - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiP_Depth[trig] = new TH2F(id, title,
      6,-0.5,5.5,
      30,0.0,15.0);
    hm->registerHisto(muiP_Depth[trig]);      
    
    sprintf(id,"muiNHits_BBCch[%d]", trig);
    sprintf(title,"MUID Occupancy vs BBC Charge - %s Arm %d", trigstring[trig], trigarm[trig]);
    muiNHits_BBCch[trig] = new TH2F(id, title,
      200,0.0,400,
      220,0.0,2200.0);	
    hm->registerHisto(muiNHits_BBCch[trig]);
  }  
  
  // moved the database init part here to avoid reading from Objy 
  // when it's not allowed (leads to graceful exiting) 
  if( !_hv_map )
  {
    cout << "QAMui::InitRun - initializing HV map" << endl;
    PdbBankID bankID(12200);
    PHTimeStamp fSearchTime(1997,1,1,1,0,2); //Obviously not correlated to a real run
    _hv_map = TMuiHVTable::Table();
    if(_hv_map->fetch(fSearchTime, "map.mui.mui_highvoltage", bankID))
    {
      
      //	cout << "HV map filled from database." << endl;
      _hv_map->commit();
      
    } else	{
      
      cout << "Initialization of MuID HV map from database failed!" << endl;
      _hv_map->commit();
      return 0;
      
    }
    
  }
  
  // all histograms are booked.. we're done 
  return 0;
}

//________________________________________________________
int QAMui::process_event(PHCompositeNode *top_node)
{
     
  // We'll let the TriggerHelper keep track of which bits are which
  // instead of hardcoding in the values (as was previously done)
  //TriggerHelper fTrigHelper( top_node );  
  TriggerHelper fTrigHelper;  
  
  // try load level1 trigger
  try {
    
    TMutNode<TrigRunLvl1>::find_io_node( top_node, "TrigRunLvl1" );
    TMutNode<TrigLvl1>::find_io_node( top_node, "TrigLvl1" );
    _has_trigger = true;
    fTrigHelper.setNodes( top_node );
    
  } catch( exception& e ) { 
    _has_trigger = false;
    static int count(0);
    if( count <= 10 )
    {
      count++;
      cout << "QAMui::process_event - no TrigLvl1 object." << endl;
      if( count == 10 ) { cout << "QAMui::process_event - message disabled." << endl; }
    }
    
  }
  
  if( (!_has_trigger) || fTrigHelper.IsEventMinBias()) 
  { muiMinBias->Fill(0.0); }

  // we'll get the runnumber and datatype the first chance we get
  static bool first( true );
  if( first ) 
  {
    
    first = false;
    
    PHIODataNode<RunHeader>::iterator fRunHeader(top_node);
    if(fRunHeader.find("RunHeader")==0)
    cout << "QAMui::MuiHistFill - RunHeader Not Found." << endl;
  
    int runNumber = 0;
    runNumber=(*fRunHeader).get_RunNumber();
    muiRunNumber->Fill(0.0,runNumber);
    cout << "QAMui::MuiHistFill - Run Number Muid = "<<runNumber<<endl;  
  
    // we'll store the datatype also
    // this will be useful later on when we do cuts
    // Au-Au: 0, p-p:1, dAu: 2
    int datatype = -1; 
    
    if( _has_trigger )
    {
      if(fTrigHelper.IsRun2_AuAu()) datatype = 0;
      else if(fTrigHelper.IsRun2_PP()) datatype = 1;
      else if(fTrigHelper.IsRun3_PP()) datatype = 2;
      else if(fTrigHelper.IsRun3_dAu()) datatype = 3;
      else if(fTrigHelper.IsRun4_AuAu_200GeV()) datatype = 4;
      else if(fTrigHelper.IsRun4_AuAu_63GeV()) datatype = 5;
      else if(fTrigHelper.IsRun4_PP()) datatype = 6;
      else if(fTrigHelper.IsRun5_CuCu()) datatype = 7;
      else if(fTrigHelper.IsRun5_PP()) datatype = 8;
    }
    
    muiDataType->Fill(0.0,datatype); // datatype is the weight of the entry
    cout << "QAMui::MuiHistFill - Data type = " << datatype << endl;  
  }
  
  //! fill histograms related to TMuiHitMapO
  fill_mui_hits_hists( top_node, fTrigHelper);
  
  //! fill histograms related to TMutTrkMap
  fill_mui_tracks_hists( top_node, fTrigHelper );
  
  //! fill histograms related to TMuiRoadMapO
  fill_mui_roads_hists( top_node, fTrigHelper );

  
  return 0;  
}

//_____________________________________________________________________________
void QAMui::fill_mui_hits_hists( PHCompositeNode* top_node, TriggerHelper& fTrigHelper ) 
{
  
  try {
    
    //
    // Mui hit map
    //
    int NumHits[N_ARMS] = {0};
    
    TMuiHitMapO* hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");
    TMuiHitMapO::const_iterator hit_iter = hit_map->range();
    while(TMuiHitMapO::const_pointer hit_ptr = hit_iter.next()){
      /*
      hit_var[0] = hit_ptr->get()->get_arm();
      hit_var[1] = hit_ptr->get()->get_plane();
      hit_var[2] = hit_ptr->get()->get_panel();
      hit_var[3] = hit_ptr->get()->get_orientation();
      hit_var[4] = hit_ptr->get()->get_twopack();
      hit_var[5] = hit_ptr->get()->get_associated<TMutTrk>().count();
      */
      NumHits[hit_ptr->get()->get_arm()]++;    
      NumHits[2]++;
    }

    /*
    // try load VtxOut node
    bool has_vtxOut( true );
    PHIODataNode<VtxOut>::iterator fVtxOut(top_node);
    if( !fVtxOut.find("VtxOut") ) {
      has_vtxOut = false;
      cout << "QAMui::fill_mui_roads_hists - VtxOut Not Found." << endl;
    }
    */
    
    bool has_BbcOut( true );
    PHIODataNode<BbcOut>::iterator fBbcOut(top_node);
    if( !fBbcOut.find("BbcOut") ) { has_BbcOut = false; }

    if( _has_trigger )
    {
      for( short trig = 0; trig < N_TRIGGERS; trig++) 
      {
        short trigval = fTrigHelper.trigLive(trigstring[trig]);        
        if( !trigval ) continue;
        
        muiNHits[trig]->Fill( (float) NumHits[trigarm[trig]]);
        
        if( !has_BbcOut ) continue;
        muiNHits_BBCch[trig]->Fill( (float) NumHits[trigarm[trig]],
          (*fBbcOut).get_ChargeSum(0)+
          (*fBbcOut).get_ChargeSum(1));
        muiBBCZVertex[trig]->Fill((*fBbcOut).get_VertexPoint());
      }	    
    }
    
  } catch( std::exception& e ) { cout << e.what() << endl; }
  
  return;
  
}


//_____________________________________________________________________________
void QAMui::fill_mui_tracks_hists( PHCompositeNode* top_node, TriggerHelper& fTrigHelper ) 
{
  
  try {
    
    int NumRoads_NoGhosts[N_ARMS] = {0};
    TMutTrkMap*  trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
    TMutTrkMap::const_iterator trk_iter = trk_map->range();
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
    {
      TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
      while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next())
      {
        if(road_ptr->get()->get_ghost_flag()==0 && road_iter.count()==1)
        {
          NumRoads_NoGhosts[road_ptr->get()->get_arm()]++; 
          NumRoads_NoGhosts[2]++; 
        } 
      }
    }
    
    if( _has_trigger )
    {
      for( short trig = 0; trig < N_TRIGGERS; trig++) 
      {
        short trigval = fTrigHelper.trigLive(trigstring[trig]);
        if( !trigval ) continue;
        muiNRoads_NoGhost[trig]->Fill(NumRoads_NoGhosts[trigarm[trig]]);
      }
    }
    
  } catch( std::exception& e ) { cout << e.what() << endl; }
  
  return;
  
}

//_____________________________________________________________________________
void QAMui::fill_mui_roads_hists( PHCompositeNode* top_node, TriggerHelper& fTrigHelper ) {
  
  try {
    
    int NumRoads[N_ARMS] = {0};
    int NumRoads_Golden[N_ARMS] = {0};
    float road_var[100] = {0};
    float hit_var[100] = {0};

    for (int i=0; i<100; i++)road_var[i] = 0;
    TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
    TMuiRoadMapO::const_iterator road_iter = road_map->range();
    while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next())
    {
      
      /*
      road_var[0] = road_ptr->get()->get_arm();
      road_var[1] = road_ptr->get()->get_gap0_point().getX();
      road_var[2] = road_ptr->get()->get_gap0_point().getY();
      road_var[3] = road_ptr->get()->get_gap0_point().getZ();
      road_var[4] = road_ptr->get()->get_depth();
      road_var[5] = road_ptr->get()->get_associated<TMutTrk>().count();
      road_var[6] = road_ptr->get()->get_golden();
      road_var[7] = road_ptr->get()->get_max_hit_plane(); // get_max_hits ??????
      road_var[8] = road_ptr->get()->get_road_quality(); // get_fit_quality ??????
      */
      //road_ptr->get()->print(cout);
  
      if(road_ptr->get()->get_golden()==1) {
  	    NumRoads_Golden[road_ptr->get()->get_arm()]++; // arm is 0 or 1
  	    NumRoads_Golden[2]++; // both arms
      }
      
      NumRoads[road_ptr->get()->get_arm()]++; // arm is 0 or 1 
      NumRoads[2]++;
  
    }
    
    float Golden_per_road[N_ARMS] = {0};
    if(NumRoads[2]>0)
    {
      for( Int_t arm = 0; arm<N_ARMS; arm++)
      { Golden_per_road[arm] = (float)NumRoads_Golden[arm]/(float)NumRoads[2]; }
    }
  
    if( _has_trigger )
    {
      
      // count all triggers we are interested in
      PHIODataNode<TrigLvl1>::iterator fTtrig1(top_node);
      if( !fTtrig1.find("TrigLvl1") ) 
      {
        cout<<"QAMui::fill_mui_roads_hists - TrigLvl1 not found." << endl;
      } 
      
      PHIODataNode<TrigRunLvl1>::iterator fTtrigRun1(top_node);
      if( !fTtrigRun1.find("TrigRunLvl1") ) {
        cout<<"QAMui::fill_mui_roads_hists - TrigRunLvl1 not found." << endl;
      }

      for( short trig = 0; trig < N_TRIGGERS; trig++) 
      {
        
        short itrig = fTrigHelper.getLevel1BitNumber(trigstring[trig]); 
        if( (*fTtrig1).get_lvl1_trigscaled_bit(itrig) )
        { muiTriggerBit_scaled->Fill(trig); }
      
        if( (*fTtrig1).get_lvl1_triglive_bit(itrig) == true)
        { muiTriggerBit_live->Fill(trig); }
             
        if(((*fTtrigRun1).get_lvl1_trig_scale_down_bybit(itrig) == 0 && 
          (*fTtrig1).get_lvl1_triglive_bit(itrig) == true)||
          ((*fTtrigRun1).get_lvl1_trig_scale_down_bybit(itrig) > 0 && 
          (*fTtrig1).get_lvl1_trigscaled_bit(itrig) == true))
        { muiTriggerBit->Fill(trig); }
      }

      for( short trig = 0; trig < N_TRIGGERS; trig++) 
      {
        short trigval = fTrigHelper.trigLive(trigstring[trig]);
      
        if( !trigval ) continue;
        
        muiNRoads[trig]->Fill(NumRoads[trigarm[trig]]);
        muiNRoads_Golden[trig]->Fill(NumRoads_Golden[trigarm[trig]]);
        muiNGolden_per_Road[trig]->Fill(Golden_per_road[trigarm[trig]]);
	
        /////
        ///// road track association
        /////
        
        float X0 =0.;
        float Y0= 0.;
        float Z0= 0.;
        float X3 = 0.;
        float Y3 = 0.;
        float Z3 = 0.;
        float dxdz0 = 0.;
        float dydz0 = 0.;
        float roadTrack_matchPosition = 0.;
        
        TMutTrkMap*  trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
        TMutTrkMap::const_iterator trk_iter = trk_map->range();
        while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){
          const TMutTrkPar* local_trk_par = trk_ptr->get()->get_trk_par_vtx();
          // Get Track par at vtx and fill the momentum.
          //
          TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
          while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next())
          {
            
            // We consider only tracks having one and only one associated roads // 
            if(road_ptr->get()->get_ghost_flag()==0 && road_iter.count()==1 &&
              (trigarm[trig] == 2 || trigarm[trig] == trk_ptr->get()->get_arm()) ) 
            {
              muiP_Depth[trig]->Fill(road_ptr->get()->get_depth(),fabs(local_trk_par->get_pz())); 
              
              //calulate track-road match position
              // it is the distance between z-position of projected road at st-3 and z-position of
              // track at st-3 (equivalent to DS3 variable defined in offline/analysis/MWGana/tools/DS3.h)
              
              //coordinates of road at gap0
              X0 = road_ptr->get()->get_gap0_point().getX();
              Y0 = road_ptr->get()->get_gap0_point().getY();
              Z0 = road_ptr->get()->get_gap0_point().getZ();
              
              //coordinates of track at Station-3
              X3 = trk_ptr->get()->get_trk_par_station( MUTOO::Station3 )->get_x();
              Y3 = trk_ptr->get()->get_trk_par_station( MUTOO::Station3 )->get_y();
              Z3 = trk_ptr->get()->get_trk_par_station( MUTOO::Station3 )->get_z();
              
              //slopes of road (roads are straighe line fits)
              dxdz0 = road_ptr->get()->get_fit_par().get_dxdz(); 
              dydz0 = road_ptr->get()->get_fit_par().get_dydz(); 
              
              roadTrack_matchPosition = sqrt( MUTOO::SQUARE(X3 - X0 -dxdz0*(Z3-Z0)) + 
                MUTOO::SQUARE(Y3 - Y0 -dydz0*(Z3-Z0)) );
              
              muiRoadTrack_MatchPos[trig]->Fill(roadTrack_matchPosition);
            }
          }
        } 

        for (int i=0; i<100; i++)road_var[i] = 0;
        TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
        TMuiRoadMapO::const_iterator road_iter = road_map->range();
        while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next())
        {
          road_var[0] = road_ptr->get()->get_arm();
          road_var[1] = road_ptr->get()->get_gap0_point().getX();
          road_var[2] = road_ptr->get()->get_gap0_point().getY();
          road_var[3] = road_ptr->get()->get_gap0_point().getZ();
          road_var[4] = road_ptr->get()->get_depth();
          road_var[5] = road_ptr->get()->get_associated<TMutTrk>().count();
          road_var[6] = road_ptr->get()->get_golden();
          road_var[7] = road_ptr->get()->get_max_hit_plane(); // get_max_hits ??????
          road_var[8] = road_ptr->get()->get_road_quality(); 
          
          if (trigarm[trig] == 2 || trigarm[trig] == road_ptr->get()->get_arm())
          {
            muiRoadRefPos[trig]->Fill(road_ptr->get()->get_fit_par().get_x(), 
              road_ptr->get()->get_fit_par().get_y());
            muiRoadGap0[trig]->Fill(road_var[1],road_var[2]);
            muiLastPlane[trig]->Fill(road_var[4]);
            
            muiMaxHits[trig]->Fill(road_var[7]);
            muiFitQuality[trig]->Fill(road_var[8]);
          }
        }  
        
        for (int i=0; i<100; i++) { hit_var[i] = 0; }
        TMuiHitMapO* hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");
        TMuiHitMapO::iterator hit_iter = hit_map->range();
        
        while(TMuiHitMapO::const_pointer hit_ptr = hit_iter.next())
        {
          if (trigarm[trig] == 2 || trigarm[trig] == hit_ptr->get()->get_arm()) 
          {
            hit_var[0] = hit_ptr->get()->get_arm();
            hit_var[1] = hit_ptr->get()->get_plane();
            hit_var[2] = hit_ptr->get()->get_panel();
            hit_var[3] = hit_ptr->get()->get_orientation();
            hit_var[4] = hit_ptr->get()->get_twopack();
            hit_var[5] = hit_ptr->get()->get_associated<TMutTrk>().count();
            
            short chain( 0 ), mainframe( 0 ), slot( 0 ), channel( 0 ), twopackLo( 0 ), twopackHi( 0 );
            _hv_map->HardwareAddress((int)hit_var[0],(int)hit_var[3],(int)hit_var[1],
              (int)hit_var[2],0,(int)hit_var[4],chain,mainframe,
              slot,channel,twopackLo,twopackHi);
            float hash = (((hit_var[0]*5+hit_var[1])*6+hit_var[2])*2+hit_var[3])*3+chain/2; 
            muiChainHits[trig]->Fill(hash);
          }
        }
      } 
    }
  } catch( std::exception &e ) { cout << e.what() << endl; }
  
  return;
}
