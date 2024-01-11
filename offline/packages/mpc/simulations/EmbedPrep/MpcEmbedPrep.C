//General PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
#include <Fun4AllReturnCodes.h>
#include <PHDataNode.h>
#include <PHIODataNode.h>
#include <recoConsts.h>

//MPC
#include <MpcMap.h>
#include <MpcCalib.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcTowerContentV1.h>
#include <mpcSimTowerContentV1.h>
#include <MpcRandom.h>
#include <mpcNoiseContainer.h>
//END_MPC

// analysis header file
#include <MpcEmbedPrep.h>



//  Root histogram types
#include <TLorentzVector.h>
#include <TVector3.h>
#include <TTree.h>
#include <TFile.h>

using namespace std;
using namespace findNode; //namespace findnode is where getclass< > resides

int MpcEmbedPrep::CalibrateSimTowers(int arm)
{
  
  //this function goes through and creates mpcSimTowerContent objects for channels where none exist
  //this is necessary for proper embedding
  //it does not apply cuts to the data

  //first make sure we are dealing with mpcSimTowerContent
  int type = mpctow->get_type();
  if(type < 11 || type >= 100){
    static int nwarn = 0;
    if(nwarn < 5){
      cout << PHWHERE << "cannot calibrate towers...mpcTowerContent object is not of type MpcSimTowerContentVXXX\n";
    }
    nwarn++;
    return 0;
  }
  
  bool is_calibrated[576] = {0};
  
  for(unsigned int itow=0;itow<mpctow->size();itow++){
    mpcTowerContent* tow = mpctow->getTower(itow);
    int ch = tow->get_ch();
    if(arm == 0 && ch > 287) continue; 
    if(arm == 1 && ch < 288) continue; 
    is_calibrated[ch] = 1;
  }
  
  //now we go through and create 0 energy towers
  
  mpcSimTowerContentV1 temp_towercontent;
  for(int ich=0;ich<576;ich++){
    if(arm == 0 && ich >= 288) break;
    if(arm == 1 && ich < 288){ ich = 288;}
    int ix = mpcmap->getGridX(ich);
    if(ix < 0) continue;
    if(is_calibrated[ich]) continue;
    
    temp_towercontent.set(ich,0.0,0.0);
    mpctow->addTower( temp_towercontent );
    is_calibrated[ich] = 1;
  }
  
  return 1;
}

int MpcEmbedPrep::CalibrateRealTowers(int arm)
{
  //this function goes through and calibrates each tower.
  //it does not apply cuts to the data

  //first make sure we are dealing with mpcSimTowerContent
  int type = mpctow->get_type();
  if(type < 1 || type >= 10){
    static int nwarn = 0;
    if(nwarn < 5){
      cout << PHWHERE << "cannot calibrate towers...mpcTowerContent object is not of type MpcTowerContentVXXX\n";
    }
    nwarn++;
    return 0;
  }
  
  MpcRandom* mpcrand = MpcRandom::instance();
  bool is_calibrated[576] = {0};
  
  
  for(unsigned int itow=0;itow<mpctow->size();itow++){
    mpcTowerContent* tow = mpctow->getTower(itow);
    int ch = tow->get_ch();
    if(arm == 0 && ch > 287) continue; 
    if(arm == 1 && ch < 288) continue; 
    is_calibrated[ch] = 1;
  }
  
  //now we go through and generate noisy towers
  mpcTowerContentV1 temp_towercontent;
  for(int ich=0;ich<576;ich++){
    if(arm == 0 && ich >= 288) break;
    if(arm == 1 && ich < 288){ ich = 288;}
    
    int ix = mpcmap->getGridX(ich);
    if(ix < 0) continue;
    if(is_calibrated[ich]) continue;
    
    //generate random noise and see if it passes cuts
    
    
    for(int itr=0;itr<100;itr++){
      //try 100 times...if it fails Houston we have a problem.
      
      //float noise_energy = mpcrand->Gaus( 0., term_noise );
      float noise_energy = mpcrand->Gaus( 0., GetNoise(ich,rms_adc_tics) );
      
      //now we have to make sure that higain adc < 33
      //the 1.05 is just for consistency w/ simulations
      if( PassAdcCuts(1.05*noise_energy,ich) ){
	continue;
      }
      if(itr == 99){
	cout << PHWHERE << "Could not add noise to tower " << ich << " after 100 tries" << endl;
	cout << "consider doing something\n";
      }
      
      temp_towercontent.set(ich,0.0,1.05*noise_energy);
      mpctow->addTower( temp_towercontent );
      is_calibrated[ich] = 1;
      break;
    }
  }
  
  return 1;
}


float MpcEmbedPrep::GetNoise(int ch, float adc_tics)
{
  if(adc_tics < 0){
    MpcRandom* mpcrand = MpcRandom::instance();
    return mpcrand->Gaus( 0., term_noise );
  }
  
  double gain = mpccalib->get_adc_gain(ch);
  float ledcorr = mpccalib->get_led( ch );
  if(ledcorr <=0) ledcorr = 1;
  gain = gain/ledcorr;
  float noise_level = adc_tics*gain;
  return noise_level/1.05;
}

int MpcEmbedPrep::PassAdcCuts(float energy, int ch)
{
  double gain = mpccalib->get_adc_gain(ch);
  float ledcorr = mpccalib->get_led( ch );
  if(ledcorr <=0) ledcorr = 1;
  gain = gain/ledcorr;
  float hilo_ratio = mpccalib->get_hilo_ratio(ch);
  
  //  MpcRandom *mpcrand = MpcRandom::instance();
  int post_amu = mpctow->get_post_amu();
  if(post_amu< 0) post_amu = 0;
  if(post_amu> 63) post_amu = 63;
  
  int pre_amu = mpctow->get_pre_amu();
  if(pre_amu< 0) pre_amu = 0;
  if(pre_amu> 63) pre_amu = 63;
  
  float hiped = mpccalib->get_ped_hgpost(ch,post_amu) - mpccalib->get_ped_hgpre(ch,pre_amu);
  
  float adc = energy/gain*hilo_ratio + hiped;
  if(adc < 33. ) return 0;
  else return 1;
}


MpcEmbedPrep::MpcEmbedPrep(int arm, float t_noise, float t_calib,
			   float adc_tics) : SubsysReco("MPCEMBEDPREP")
{
  rms_adc_tics = adc_tics;
  term_noise = t_noise;
  term_calib = t_calib;  //guess is that it is 0.08
  term_stochastic = 0.02626128;
  mpc_arm = arm;

  //  MpcRandom* mpcrand = MpcRandom::instance();

  //if the detector is miscalibrated, it is systematically miscalibrated
  //for the entire run
  //  for(int ich=0;ich<576;ich++){
  //calib_array[ich] = mpcrand->Gaus( 0.,term_calib );
  //}
 }


MpcEmbedPrep::~MpcEmbedPrep()
{
}

int MpcEmbedPrep::InitRun(PHCompositeNode *topNode)
{
  cout << "Beginning InitRun()\n";
  //MPC
  mpcmap = getClass<MpcMap>(topNode, "MpcMap");
  mpccalib = getClass<MpcCalib>(topNode, "MpcCalib");
  //END_MPC
  
  recoConsts* rc = recoConsts::instance();
  runnum = rc->get_IntFlag("RUNNUMBER",0);
  
  if(runnum>=254945 && runnum < 260000) {
    daflag = 0; 
    cout << "In Run8 p+p";
  }
  else if(runnum < 254000 && runnum > 246000){
    daflag = 1;
    cout << "In Run8 d+Au";
  }

  cout << "end InitRun()";
  return 0;
}

int MpcEmbedPrep::Init(PHCompositeNode *topNode)
{
  cout << "Beginning Init()\n";

  cout << "Ending Init()\n";
  return 0;
}

// ================ process_event ==============

int MpcEmbedPrep::process_event(PHCompositeNode *topNode)
{
  static int ncalls = 0;
  ncalls++;
  if (ncalls % 5000 == 0 && verbosity) cout << "MpcEmbedPrep Ncalls = " << ncalls << endl;

  
  //Make sure that we have consistent calibs across different subsysreco objects
  mpcNoiseContainer* mpcnoise = mpcNoiseContainer::instance(term_calib);
  mpcnoise->reset_calib(term_calib); //this randomizes the calib term

  mpcraw = getClass<mpcRawContainer>(topNode,"MpcRaw");
  mpctow = getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  
  if(ncalls < 3 && (mpcraw == 0 || mpctow == 0) ){
    cout << "MpcEmbedPrep::GetMpcNodes()\t" << PHWHERE << endl;
    cout << "\ttow: " << (unsigned int)mpctow 
	 << "\traw: " << (unsigned int)mpcraw 
	 << endl;
  }
  
  if(mpctow == 0){ 
    cout << "no towers for this event...aborting run\n";
    return -2;
  }

  if(mpctow->get_type() > 10 && mpctow->get_type() < 100)
    CalibrateSimTowers(mpc_arm);
  else
    CalibrateRealTowers(mpc_arm);
  
  
  return EVENT_OK;
}
// ================================================== End ====
int MpcEmbedPrep::End(PHCompositeNode *topNode)
{
  cout << "begin End()";
  cout << "end End()";
  return 0;

}

