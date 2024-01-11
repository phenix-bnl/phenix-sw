#include "mMpcExDigitizeHits.h"
#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "recoConsts.h"
#include "PHCompositeNode.h"
#include "getClass.h"
#include "TMpcExCalibContainer.h"
#include "TMpcExCalib.h"
#include "TMpcExGeaHit.h"
#include "TMpcExGeaHitContainer.h"
#include "TMpcExHit.h"
#include "TMpcExHitSet.h"
#include "MpcExConstants.h"
#include "MpcExMapper.h"
#include "MpcExPISAEventHeader.h"
#include <TRandom3.h>
#include <cmath>
#include <iostream>

#include "TFile.h"
#include "TH2.h"

mMpcExDigitizeHits::mMpcExDigitizeHits() : SubsysReco("MMPCEXDIGITIZEHITS") {
  _dice = new TRandom3(0);

  recoConsts *myrc = recoConsts::instance();
  makeHisto = myrc->get_IntFlag("MpcExDigitizeHitsHisto",0x0); 
  outputfile = NULL;
  _histo_low = NULL;
  _histo_high = NULL;
  _histo_low_dcm = NULL;
  _histo_high_dcm = NULL;
}

mMpcExDigitizeHits::~mMpcExDigitizeHits(){
  delete _dice;
  if(makeHisto)
    delete outputfile;
}

int mMpcExDigitizeHits::Init(PHCompositeNode *topNode)
{

  if(makeHisto){
    outputfile = new TFile("MpcExDigitizeHitsHisto.root","RECREATE");
    _histo_low = new TH2D("low_adc_bykey","Low gain ADC by key",49152,-0.5,49151.5,256,0.,256.0); 
    _histo_high = new TH2D("high_adc_bykey","High gain ADC by key",49152,-0.5,49151.5,256,0.,256.0); 
    _histo_low_dcm = new TH2D("low_adc_bykey_dcm","Low gain ADC by key (DCM cut)",49152,-0.5,49151.5,256,0.,256.0); 
    _histo_high_dcm = new TH2D("high_adc_bykey_dcm","High gain ADC by key (DCM cut)",49152,-0.5,49151.5,256,0.,256.0); 
  }

  return 0; 

}

int mMpcExDigitizeHits::process_event(PHCompositeNode *topNode){

  //These are the "Gea Hits" at first that will get turned into digitized hits
  TMpcExGeaHitContainer *geahits = findNode::getClass<TMpcExGeaHitContainer>(topNode, "TMpcExGeaHitContainer");
  if(geahits == NULL) {
    std::cout << "No TMpcExGeaHitContainer! ... You need to load mMpcExCreateNodeTree" << std::endl; 
    return ABORTRUN;
  }

  //this is the real data pedestals and mip peak positions
  TMpcExCalibContainer *calibMap = findNode::getClass<TMpcExCalibContainer>(topNode,"TMpcExCalibContainer");
  if(!calibMap){
    std::cout << "No TMpcExCalibContainer! ... You need to load mMpcExCreateNodeTree" << std::endl; 
    return ABORTRUN;
  }

  //this will be the digitized hits
  MpcExRawHit *rawhits = findNode::getClass<MpcExRawHit>(topNode,"MpcExRawHit");
  if(rawhits == NULL){
    std::cout << "No MpcExRawHit! ... You need to load mMpcExCreateNodeTree" << std::endl; 
    return ABORTRUN;
  }

  // pisaheader to store saturated minipad information
  int nLowSat[2] = {0,0}; 
  float eLowSat[2] = {0.0,0.0}; 
  MpcExPISAEventHeader *pisaheader = findNode::getClass<MpcExPISAEventHeader>(topNode,"MpcExPISAEventHeader");
  if (pisaheader == NULL) {
    std::cout<<PHWHERE<<" I could not find MpcExPISAEventHeader"<<std::endl;
    std::cout<<PHWHERE<<" Make sure that you are reading a simulated PISAEvent file"<<std::endl;
    //return ABORTRUN;
  } 

  MpcExMapper *mapper = MpcExMapper::instance();

  //I'm going to keep an internal std::set of the hits as I 
  //manipulate them. This saves lots of time since we need 
  //remove hits at the end or periodically for some reason
  std::set<TMpcExHit*,TMpcExHitSort::SortByKey> hit_list;

  //  MpcExMapper *mapper = MpcExMapper::instance();
  //----------
  //Digitize the GEANT energy
  //----------
  for(unsigned int ghit=0; ghit<geahits->size(); ghit++){

    TMpcExGeaHit *geahit = geahits->getHit(ghit); 


    if(geahit == NULL){
      std::cout<<PHWHERE<<" Something is seriously wrong. I cannot get the GEANT hit #"<<ghit<<std::endl;
      std::cout<<PHWHERE<<"BAILING"<<std::endl;
      return ABORTRUN;
    }

    unsigned int key = geahit->key();
    
    //add cutoff 
    TMpcExCalib *calib = calibMap->get(key);
    if(geahit->e()*1000000.0 < calib->get_mip_corr_cutoff_position()) continue;
   
    TMpcExHit *hit = new TMpcExHit(key);
    
    float adclo = 0.0;
    float adchi = 0.0;
    float esat = 0.0;

    float geant_smeared_energy = geahit->e()*1000000.0*_dice->Gaus(1.0,calib->get_smear()); 

    if((hit->layer()==0) || (hit->layer()==1))
      adclo = MpcExConstants::make_adc(MpcExConstants::FIXED_MIP_L01/MpcExConstants::FIXED_HL_RATIO,
				       MpcExConstants::FIXED_LOW_PEDESTAL,
				       MpcExConstants::FIXED_LOW_PEDESTAL_WIDTH,
				       geant_smeared_energy,
				       _dice,
				       &esat);
    else
      adclo = MpcExConstants::make_adc(MpcExConstants::FIXED_MIP_L27/MpcExConstants::FIXED_HL_RATIO,
				       MpcExConstants::FIXED_LOW_PEDESTAL,
				       MpcExConstants::FIXED_LOW_PEDESTAL_WIDTH,
				       geant_smeared_energy,
				       _dice,
				       &esat);

    if(makeHisto) _histo_low->Fill(key,adclo);
    if(esat>0.0) {
      unsigned int iarm = mapper->get_arm(key); 
      nLowSat[iarm]++; 
      eLowSat[iarm] += esat;
    }

    if((hit->layer()==0) || (hit->layer()==1))
      adchi = MpcExConstants::make_adc(MpcExConstants::FIXED_MIP_L01,
				       MpcExConstants::FIXED_HIGH_PEDESTAL,
				       MpcExConstants::FIXED_HIGH_PEDESTAL_WIDTH,
				       geant_smeared_energy,
				       _dice,
				       &esat);
    else
      adchi = MpcExConstants::make_adc(MpcExConstants::FIXED_MIP_L27,
				       MpcExConstants::FIXED_HIGH_PEDESTAL,
				       MpcExConstants::FIXED_HIGH_PEDESTAL_WIDTH,
				       geant_smeared_energy,
				       _dice,
				       &esat);

    if(makeHisto) _histo_high->Fill(key,adchi);

    hit->set_low(adclo);
    hit->set_high(adchi);
    
    hit->set_status_low(TMpcExHit::ADC);
    hit->set_status_high(TMpcExHit::ADC);

    hit_list.insert(hit);

  }//hit loop

  //OK, now we can make a "zero suppression" based on the DCM threshold

  std::set<TMpcExHit*>::iterator hitr = hit_list.begin();
  std::vector<unsigned int> raw;
  for(; hitr!=hit_list.end(); ++hitr){
    TMpcExHit *digihit = *hitr;

    unsigned short threshold_low = MpcExConstants::FIXED_LOW_PEDESTAL + 
      MpcExConstants::NORTH_DCM_LOW_SIGMA_CUT*MpcExConstants::FIXED_LOW_PEDESTAL_WIDTH; 
    unsigned short threshold_high = MpcExConstants::FIXED_HIGH_PEDESTAL + 
      MpcExConstants::NORTH_DCM_HIGH_SIGMA_CUT*MpcExConstants::FIXED_HIGH_PEDESTAL_WIDTH; 
    if(digihit->arm()==0) {
      threshold_low = MpcExConstants::FIXED_LOW_PEDESTAL + 
	MpcExConstants::SOUTH_DCM_LOW_SIGMA_CUT*MpcExConstants::FIXED_LOW_PEDESTAL_WIDTH; 
      threshold_high = MpcExConstants::FIXED_HIGH_PEDESTAL + 
	MpcExConstants::SOUTH_DCM_HIGH_SIGMA_CUT*MpcExConstants::FIXED_HIGH_PEDESTAL_WIDTH;
    }

    //turn this into the bit packed raw hit if hit would have passed the DCM threshold
    unsigned short adclow = 0; 
    unsigned short adchigh = 0; 
    if(digihit->low()>threshold_low){
      float low = digihit->low();
      if(low<0) low = 0;
      adclow = low; 
      // histogram the ADC's
      if(makeHisto) _histo_low_dcm->Fill(digihit->key(),adclow);
    }
    if(digihit->high()>threshold_high){
      float high = digihit->high();
      if(high<0) high = 0;
      adchigh = high; 
      if(makeHisto) _histo_high_dcm->Fill(digihit->key(),adchigh);
    }

    // Add the hit if one of the two gains passes threshold
    if( (adchigh>0) || (adclow>0) ){
      unsigned short adc = (adchigh << 8) + adclow;
      unsigned int val = digihit->key() << 16;
      val |= adc;
      raw.push_back(val);
    }

    delete digihit;

  }//loop over hits
  hit_list.clear();

  //add everything to the vector
  rawhits->fillfromvector(raw);

  if(pisaheader){
    pisaheader->setnLowSat(0,nLowSat[0]); 
    pisaheader->seteLowSat(0,eLowSat[0]/1000000.0); //convert from keV to GeV 
    pisaheader->setnLowSat(1,nLowSat[1]); 
    pisaheader->seteLowSat(1,eLowSat[1]/1000000.0); //convert from keV to GeV
  }
  
  //std::cout << nLowSat[0] << " " << eLowSat[0] << std::endl; 
  //std::cout << nLowSat[1] << " " << eLowSat[1] << std::endl << std::endl; 

  return EVENT_OK;
}

int mMpcExDigitizeHits::End(PHCompositeNode *topNode)
{

  if(makeHisto){
    outputfile->Write();
    outputfile->Close();
  }
  
  return 0;
}

