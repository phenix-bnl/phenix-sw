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
#include <CalibSimTowers.h>



//  Root histogram types
#include <TLorentzVector.h>
#include <TVector3.h>
#include <TTree.h>
#include <TFile.h>

using namespace std;
using namespace findNode; //namespace findnode is where getclass< > resides

int CalibSimTowers::CalibrateSimTowers(int arm)
{
  //this function goes through and calibrates each tower.
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

  MpcRandom* mpcrand = MpcRandom::instance();
  mpcNoiseContainer* mpcnoise = mpcNoiseContainer::instance(term_calib);
  
  bool is_calibrated[576] = {0};
  
  //here we set energy_noise
  for(unsigned int itow=0;itow<mpctow->size();itow++){
    mpcTowerContent* tow = mpctow->getTower(itow);
    int ch = tow->get_ch();
    if(arm == 0 && ch > 287) continue; 
    if(arm == 1 && ch < 288) continue; 
    
    //noiseless energy
    float energy = tow->get_energy(0);
    float sigmaTotal = 0;
    float sigmaStochastic = 0;
    if(energy > 0) 
      sigmaStochastic = mpcrand->Gaus( 0.,term_stochastic )*sqrt(energy);
    
    //float sigmaNoise = 0.00+mpcrand->Gaus( 0., term_noise );
    float sigmaNoise = 0.00+mpcrand->Gaus( 0., GetNoise(ch,rms_adc_tics) );
    float sigmaCalib = mpcnoise->get_calib(ch)*energy; 

    sigmaTotal = sigmaStochastic+sigmaNoise+sigmaCalib;
    tow->set_noise(sigmaTotal);
    is_calibrated[ch] = 1;
  }
  
  //now we go through and generate noisy towers
      
  int ntot = 0;
  int ngood = 0;
  mpcSimTowerContentV1 temp_towercontent;
  for(int ich=0;ich<576;ich++){
    if(arm == 0 && ich >= 288) break;
    if(arm == 1 && ich < 288){ ich = 288;}
    
    int ix = mpcmap->getGridX(ich);
    if(ix < 0) continue;
    if(is_calibrated[ich]) continue;
    
    if(ich > 287) ntot++;
    //generate random noise and see if it passes cuts
    //float noise_energy = 0.00+mpcrand->Gaus( 0., term_noise );
    float noise_energy = 0.00+mpcrand->Gaus( 0., GetNoise(ich,rms_adc_tics) );
    
    
    if( PassAdcCuts(noise_energy*1.05, ich) ){
      if(ich > 287) ngood++;
      //      if(ich > 287) cout << "ngood is: " << ngood << ", " << noise_energy << endl;
      temp_towercontent.set(ich,0.0,0.0);
      mpctow->addTower( temp_towercontent );
      unsigned int size = mpctow->size();
      mpcTowerContent* tc = mpctow->getTower(size-1);

      //make sure you set this afterwards otherwise funny stuff happens
      tc->set_noise(noise_energy);
      
      /*
	int test_ch = tc->get_ch();
      
      float test_e = tc->get_energy(1);
      float test_e0 = tc->get_energy(0);
      float test_e2 = tc->get_energy(1);
      
      cout << "size, energy, ch, ich, e0, e2: " 
	   << size << ", " << test_e << ", " 
	   << test_ch << ", " << ich  << ", " 
	   << test_e0 << ", " << test_e2 << endl;
      */
    }
    
    is_calibrated[ich] = 1;
  }
  // cout << "ngood/ntot: " << ngood << "  /  " << ntot << endl;
  
  
  return 1;
}


float CalibSimTowers::GetNoise(int ch, float adc_tics)
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



int CalibSimTowers::PassAdcCuts(float energy, int ch)
{
  double gain = mpccalib->get_adc_gain(ch);
  float ledcorr = mpccalib->get_led( ch );
  if(ledcorr <=0) ledcorr = 1;
  gain = gain/ledcorr;
  float hilo_ratio = mpccalib->get_hilo_ratio(ch);
  
  int post_amu = mpctow->get_post_amu();
  if(post_amu< 0) post_amu = 0;
  if(post_amu> 63) post_amu = 63;

  int pre_amu = mpctow->get_pre_amu();
  if(pre_amu< 0) pre_amu = 0;
  if(pre_amu> 63) pre_amu = 63;
  
  float hiped = mpccalib->get_ped_hgpost(ch,post_amu) - mpccalib->get_ped_hgpre(ch,pre_amu);
      
  float adc = energy/gain*hilo_ratio + hiped;
  if(adc < 33.) return 0;
  else return 1;
}


CalibSimTowers::CalibSimTowers(int arm, float t_noise, float t_calib,
			       float adc_tics,const char* infile) : SubsysReco("CALIBSIMTOWERS")
{
  rms_adc_tics = adc_tics;
  fname = infile;
  amu_file = TFile::Open(fname.Data());
  amu_tree = (TTree*) amu_file->Get("amu_tree");
  amu_tree->SetBranchAddress("mpc_post_amu",&mpc_post_amu);
  amu_tree->SetBranchAddress("mpc_pre_amu",&mpc_pre_amu);

  term_noise = t_noise;
  term_calib = t_calib;  //guess is that it is 0.08
  term_stochastic = 0.02626128;
  mpc_arm = arm;

  //  MpcRandom* mpcrand = MpcRandom::instance();

  //if the detector is miscalibrated, it is systematically miscalibrated
  //for the entire run
  //  for(int ich=0;ich<576;ich++){
  //    calib_array[ich] = mpcrand->Gaus( 0.,term_calib );
  //  }
  //  mpcnoise = MpcNoiseContainer::instance(); //initializes values
 }


CalibSimTowers::~CalibSimTowers()
{
}

int CalibSimTowers::InitRun(PHCompositeNode *topNode)
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

int CalibSimTowers::Init(PHCompositeNode *topNode)
{
  cout << "Beginning Init()\n";

  cout << "Ending Init()\n";
  return 0;
}

// ================ process_event ==============

int CalibSimTowers::process_event(PHCompositeNode *topNode)
{

  static int ncalls = 0;
  int amu_calls = ncalls%500000;

  ncalls++;
  if (ncalls % 5000 == 0 && verbosity) cout << "CalibSimTowers Ncalls = " << ncalls << endl;
  
  mpcNoiseContainer* mpcnoise = mpcNoiseContainer::instance(term_calib);
  if(mpcnoise->reset_status() == 0){
    mpcnoise->reset_calib(term_calib);
    mpcnoise->set_reset_status(0); //this randomizes the calib term
  }


  
  //  Fun4AllServer* se = Fun4AllServer::instance(); //how to get embedded nodes
  //PHCompositeNode* mixNode = 0;
  //if(mix_mb) mixNode= se->topNode("MIXN");

  mpcraw = getClass<mpcRawContainer>(topNode,"MpcRaw");
  mpctow = getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  
  
  if(ncalls < 3 && (mpcraw == 0 || mpctow == 0) ){
    cout << "CalibSimTowers::GetMpcNodes()\t" << PHWHERE << endl;
    cout << "\ttow: " << (unsigned int)mpctow 
	 << "\traw: " << (unsigned int)mpcraw 
	 << endl;
  }
  
  if(mpctow == 0){ 
    cout << "no towers for this event...aborting run\n";
    return -2;
  }
  
  float tow_type = mpctow->get_type();

  if(tow_type > 10 && tow_type < 100){
    
    mpc_post_amu = -999;
    mpc_pre_amu = -999;
    amu_tree->GetEntry(amu_calls);
    if(mpc_post_amu < 0 || mpc_pre_amu < 0){
      cout << "post, pre amu not found in ttree or tree contains bad data\n";
      mpc_post_amu = 0;
      mpc_pre_amu = 0;
    }
    
    mpctow->set_amu(0,mpc_pre_amu,mpc_post_amu);
    
    CalibrateSimTowers(mpc_arm);
    
  }
  
  //now we go through and clean up those towers that don't pass the adc cuts
  
  for(unsigned int itow=0;itow < mpctow->size();itow++){
    mpcTowerContent* tow = mpctow->getTower(itow);
    int ch = tow->get_ch();
    if(mpc_arm == 0 && ch > 287) continue; 
    if(mpc_arm == 1 && ch < 288) continue; 
    
    //noiseless energy
    float energy = tow->get_energy(1);
    if( !PassAdcCuts(energy,ch) ){
      tow->set_energy(0);
      if(tow_type > 10 && tow_type < 100) tow->set_noise(0);
    }
  }
  
  //types are:
  //10+V# for simTowerContentV#
  //0+V# for mpcTowerContentV#
  
  
  
  return EVENT_OK;
}
// ================================================== End ====
int CalibSimTowers::End(PHCompositeNode *topNode)
{
  cout << "begin End()\n";
  cout << "end End()\n";
  return 0;
  
}

