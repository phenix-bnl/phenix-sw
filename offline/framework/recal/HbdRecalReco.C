//
// Wrapper class to produce HbdCellList from HbdMiniCellList
//   T. Sakaguchi, July 14, 2009
//
#include "HbdRecalReco.h"
#include <V24maker.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHGlobal.h>
#include <HbdMiniToFullCell.h>
#include <HbdMiniCellList.h>
#include <hbdAdcCalib.hh>
#include <HbdWisClusterizer.h>
#include <HbdCellListAnalyzer.h>
#include <phool.h>
#include <getClass.h>
#include <recoConsts.h>

#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>

using namespace std;

HbdRecalReco::HbdRecalReco(const string &name): Recalibrator(name)
{
  // This is put on the node tree by ReadBackCompactCNT for Run 10

  baseclasses.insert("PHCentralTrack");

  hbdconv = 0;
  calib = 0;
  cana = 0;
  v24maker = 0;
  verbosity = 0;
  clst_flag = 0;

  calib = new hbdAdcCalib();
}

HbdRecalReco::~HbdRecalReco()
{
  delete hbdconv;
  delete cana;
  delete calib;
  delete v24maker;
  return;
}


int
HbdRecalReco::isValidRun(const int runno) const
{
  
  //Run-9
  if (runno > BEGIN_OF_RUN9 && runno < BEGIN_OF_RUN10)
    {
      return 1;
    }
  
  //Run-10
  if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV)
    {
      
      calib->fetch(runno);
      
     
      // Check if the run is calibrated
      if ( calib->isRunCalibrated() ){
	cout << "HbdRecalReco: Gain Calibration for run " << runno << "........OK"  << endl;
	return 1;
      }
      else{
	cout << PHWHERE<< "HbdRecalReco: Gain Calibration for run " << runno << " does not exist!"  << endl;
	return 0;
      } 
    }
  
  return 0;
}


int HbdRecalReco::InitRun(PHCompositeNode *topNode)
{

  recoConsts *rc = recoConsts::instance();
  mc_flag = rc->get_IntFlag("HBD_MC",0);
  skip_flag=0;
  
  if (mc_flag==0)
    {
      HbdMiniCellList* mincell = NULL;
      mincell = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
      if(!mincell) {
	std::cerr << "No HbdMiniCellList!::InitRun"<< std::endl;
	std::cerr << "We don't process HBD Recal::InitRun"<< std::endl;
	skip_flag=1;
	return 0;
      }
      
      hbdconv = new HbdMiniToFullCell(); 
      //rc->set_IntFlag("HBD_CHANBYCHAN",1); //This is determined in HbdMiniToFullCell	 
      //rc->set_IntFlag("HBD_MODBYMOD", 1);  //This is determined in HbdMiniToFullCell	 
      hbdconv->Init(topNode);
      if(0!=hbdconv->InitRun(topNode)){
	skip_flag=1;
	return -1;
      }
    }

   // Set the default Clusterizer
   int runno = rc->get_IntFlag("RUNNUMBER");
   //Run-9
   if (runno > BEGIN_OF_RUN9 && runno < BEGIN_OF_RUN10)
     { //Weizmann Clusterizer
       clst_flag = rc->get_IntFlag("HBD_CLUSTERIZER", 1); 
     }
   //Run-10
   else if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV)
     { //MinPad Clusterizer
       clst_flag = rc->get_IntFlag("HBD_CLUSTERIZER", 3); 
     }
   else 
     { //Weizmann Clusterizer
       clst_flag = rc->get_IntFlag("HBD_CLUSTERIZER", 1); 
     }

   if (clst_flag ==1){
   // Hbd Clusterizer
   if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV){  
      cana = new HbdCellListAnalyzer();	
      cana->setMCFlag(mc_flag);
   }
   wisclusters.setNodeName(inputnodename);
   wisclusters.Init(topNode);
   }
   else if (clst_flag ==2)
   {
   // V24
   v24maker = new V24maker(inputnodename.c_str(), "PHCentralTrackV24");
   v24maker->InitRun(topNode);

   }
   else if (clst_flag == 3){
   // MinPadClusteriser
   if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV){
      cana = new HbdCellListAnalyzer();
      cana->setMCFlag(mc_flag);
   }
   minpadclusters.Verbosity(verbosity);
   minpadclusters.Init(topNode); 
   }
   else if (clst_flag == 4){
   // Underlying event subtraction
   if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV){
      cana = new HbdCellListAnalyzer();
      cana->setMCFlag(mc_flag);
   }
   // Weizmann Clusterizer
   wisclusters.setNodeName(inputnodename);
   wisclusters.Init(topNode);
   // MinPad Clusterizer
   minpadclusters.Verbosity(verbosity);
   minpadclusters.Init(topNode);
   }
   else if (clst_flag == 0)
   {
     cout << PHWHERE << " no clustering done" << endl;
     skip_flag = 1;
   }
   else
   {
     std::cerr << "HBD_CLUSTERIZER flag not recognized. Clustering aborted!" << std::endl;
     return -1;

   }
   return 0;
}


int HbdRecalReco::process_event(PHCompositeNode *topNode)
{
   //
   // If there is no HbdMiniCell, skip process
   //
   if(skip_flag) return 0;

   PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
   if(!global) cout<<PHWHERE<<"HbdRecalReco:: No PHGlobal!"<<endl;
   int centrality = global->getCentrality();

   recoConsts *rc = recoConsts::instance();
   int runno = rc->get_IntFlag("RUNNUMBER");

   if (verbosity){
     cout << "HbdRecalReco::Centrality: " << centrality << endl;
   }

   if (mc_flag==0)
     {
       if(0!=hbdconv->process_event(topNode)){
	 std::cerr << "Error in HbdMiniToFullCell::process_event"<< std::endl;
	 return ABORTEVENT;
       }
     }

   if (clst_flag ==1)
     {
       //Expand and subtraction (Only for Run-10)
       if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV){
         if(0!=cana->Process(topNode)){
           std::cerr << "Error in HbdCellListAnalyzer::process_event"<< std::endl;
           return ABORTEVENT;
         }
       }
       if(0!=wisclusters.process_event(topNode)){
	 std::cerr << "Error in HbdWisClusterizer::process_event"<< std::endl;
	 return ABORTEVENT;
       }
     }
   else if (clst_flag ==2)
     {
       
       if(0!=v24maker->process_event(topNode)){
	 std::cerr << "Error in V24maker::process_event" << std::endl;
	 return -1; 
       }
       
     }
   else if (clst_flag ==3)
     {
       //Expand and subtraction (Only for Run-10))
       if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV){
         if(0!=cana->Process(topNode)){
  	   std::cerr << "Error in HbdCellListAnalyzer::process_event"<< std::endl;
	   return ABORTEVENT;
         }
       }
       if(0!=minpadclusters.process_event(topNode)){
	 std::cerr << "Error in HbdMinPadClusterizer::process_event"<< std::endl;
	 return ABORTEVENT;
       }
     }
   else if (clst_flag ==4)
     {
       //Expand and subtraction (Only for Run-10))
       if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV){
         if(0!=cana->Process(topNode)){
           std::cerr << "Error in HbdCellListAnalyzer::process_event"<< std::endl;
           return ABORTEVENT;
         }
       }
       if(0!=wisclusters.process_event(topNode)){
         std::cerr << "Error in HbdWisClusterizer::process_event"<< std::endl;
         return ABORTEVENT;
       }
       if(0!=minpadclusters.process_event(topNode)){
         std::cerr << "Error in HbdMinPadClusterizer::process_event"<< std::endl;
         return ABORTEVENT;
       }
     }
   else
   {
     std::cerr << "Value for HBD_CLUSTERIZER not implemented!"<< std::endl;
     return -1;
   }

   return 0;
}

int HbdRecalReco::End(PHCompositeNode *topNode)
{

  if (clst_flag==3 || clst_flag==4){	
    minpadclusters.End(topNode);
  }

  return 0;
}
