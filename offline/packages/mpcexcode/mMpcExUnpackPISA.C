#include "mMpcExUnpackPISA.h"
#include "PHCompositeNode.h"
#include "Fun4AllReturnCodes.h"
#include "recoConsts.h"
#include "getClass.h"
#include "TMpcExGeaHit.h"
#include "TMpcExGeaHitContainer.h"
#include "NCCPisaHit.h"
#include "MPCEXABSPisaHit.h"
#include "MPCFPLTPisaHit.h"
#include "NCCSnglPisaHit.h"
#include "MPCEXABSSnglPisaHit.h"
#include "MPCFPLTSnglPisaHit.h"
#include "MpcExConstants.h"
#include "MpcExMapper.h"
#include "MpcExPISAEventHeader.h"
#include "MpcExPISAEventHeaderv1.h"
#include "MpcExPISAEventHeaderv2.h"


#include "TFile.h"
#include "TH2.h"


mMpcExUnpackPISA::mMpcExUnpackPISA() : SubsysReco("MMPCEXUNPACKPISA") {

  recoConsts *myrc = recoConsts::instance();
  makeHisto = myrc->get_IntFlag("MpcExUnpackPisaHisto",0x0); 
  outputfile = NULL;
  _histo = NULL;
  _histoD = NULL;
}

mMpcExUnpackPISA::~mMpcExUnpackPISA(){
  if(makeHisto)
    delete outputfile;
}


int mMpcExUnpackPISA::Init(PHCompositeNode *topNode)
{

  if(makeHisto){
    outputfile = new TFile("MpcExUnpackPisaHisto.root","RECREATE");
    _histo = new TH2D("geant_edep_bykey","GEANT energy deposit (keV) by key",49152,-0.5,49151.5,256,0.,1000.0); 
    _histoD = new TH2D("geant_edep_bykey_long","GEANT energy deposit (keV) by key",49152,-0.5,49151.5,256,0.,2000.0); 
  }

  return 0; 

}


int mMpcExUnpackPISA::process_event(PHCompositeNode *topNode){

  NCCPisaHit *pisahits = findNode::getClass<NCCPisaHit>(topNode,"NCCPisaHit");
  if (pisahits == NULL) {
    std::cout<<PHWHERE<<" I could not find NCCPisaHit"<<std::endl;
    std::cout<<PHWHERE<<" Make sure that you are reading a simulated PISAEvent file"<<std::endl;
    return ABORTRUN;
  } 

  MPCEXABSPisaHit *absorberhits = findNode::getClass<MPCEXABSPisaHit>(topNode,"MPCEXABSPisaHit");
  if (absorberhits == NULL) {
    std::cout<<PHWHERE<<" I could not find MPCEXABSPisaHit"<<std::endl;
    std::cout<<PHWHERE<<" Make sure that you are reading a simulated PISAEvent file"<<std::endl;
    //return ABORTRUN;
  } 

  MPCFPLTPisaHit *frontplatehits = findNode::getClass<MPCFPLTPisaHit>(topNode,"MPCFPLTPisaHit");
  if (frontplatehits == NULL) {
    std::cout<<PHWHERE<<" I could not find MPCFPLTPisaHit"<<std::endl;
    std::cout<<PHWHERE<<" Make sure that you are reading a simulated PISAEvent file"<<std::endl;
    //return ABORTRUN;
  }

  MpcExPISAEventHeader *pisaheader = findNode::getClass<MpcExPISAEventHeader>(topNode,"MpcExPISAEventHeader");
  if (pisaheader == NULL) {
    std::cout<<PHWHERE<<" I could not find MpcExPISAEventHeader"<<std::endl;
    std::cout<<PHWHERE<<" Make sure that you are reading a simulated PISAEvent file"<<std::endl;
    //return ABORTRUN;
  } 

  TMpcExGeaHitContainer *geahits = findNode::getClass<TMpcExGeaHitContainer>(topNode,"TMpcExGeaHitContainer");
  if(geahits == NULL){
    std::cout<<PHWHERE<<" I could not find TMpcExGeaHitContainer"<<std::endl;
    std::cout<<PHWHERE<<" Make sure that you load mMpcExCreateNodeTree before this module"<<std::endl;
    std::cout<<PHWHERE<<" and that SIMULATION_FLAG exists in recoConsts."<<std::endl;
    return ABORTRUN;
  }

  MpcExMapper *mapper = MpcExMapper::instance();

  ////////////////////////////////////////////
  // - Numbering scheme -volume copy numbers (number(2-5))
  // - (2) FOC 1/2 - positive and negative z
  // - (3) Subassembly(1x2 sensors) - iver+100*ihor
  // - (4) Sampling cells: 1- 3 - S0(preshower)
  // - 4-12 - S1 (EMC1)
  // - 13-21 - S2 (EMC2)
  // - (5) Silicon: cell kind + Top/Bottom*10 + cell layer*100
  // - layer = 3/9 in strip cells, 5 - EM, 6 - Had cells
  // usually the same numbers will be written to output structure as "numbv"
  // to allow tower energies combined - here are the modifications made in GUSTEP
  // numbv(1) = number(1)*10+segm *** FOC and Segment
  // if(iswit(4).eq.1) then
  // numbv(2) = cell + 100*(iy + 1000*ix) *** cell and pixel within subassembly
  // else
  // numbv(2) = 100*(iy + 1000*ix) *** pixel within subassembly
  // endif
  // numbv(3) = hv *** subassembly address iver+100*ihor
  // numbv(4) = cell kind + Top/Bottom*10 + cell layer*100
  // in all sets except the first two (really sensitive) ix and iy = 99
  // whenever the energy in absorbers is stored - it goes into tower 99/99 in appropriate component 
  ////////////////////////////////////////////
  
  float abs_energy[2] = {0.0,0.0}; 
  float si_energy[2] = {0.0,0.0}; 
  float deadAreaEnergy[2] = {0.0,0.0}; 
  float fplt_energy[2] = {0.0,0.0}; 
  float side_leakage[2] = {0.0,0.0};
  float back_leakage[2] = {0.0,0.0}; 
  float inner_energy[2] = {0.0,0.0}; 
  int abs_nhits[2] = {0,0}; 
  int fplt_nhits[2] = {0,0}; 
  int si_nhits[2] = {0,0}; 
  int dead_nhits[2] = {0,0}; 
  int side_nhits[2] = {0,0}; 
  int back_nhits[2] = {0,0}; 
  int inner_nhits[2] = {0,0}; 

  if (absorberhits) {
    
    int fNMpcExAbsHits = absorberhits->GetnHit();
    for (int ihit=0 ; ihit<fNMpcExAbsHits ; ihit++)
    {
      MPCEXABSSnglPisaHit *single_hit = absorberhits->GetHit(ihit);
      abs_energy[single_hit->GetIarm()] += single_hit->GetDedx()/1000000.; //convert from keV to GeV
      abs_nhits[single_hit->GetIarm()]++;
    }

    //std::cout << fNMpcExAbsHits << " " << abs_nhits[0] << " " << abs_nhits[1] << std::endl;  

  }

  if (frontplatehits) {
    
    int fNMpcFPLTHits = frontplatehits->GetnHit();
    for (int ihit=0 ; ihit<fNMpcFPLTHits ; ihit++)
    {
      MPCFPLTSnglPisaHit *single_hit = frontplatehits->GetHit(ihit);      
      if(single_hit->GetIncc()<10){
        fplt_energy[single_hit->GetIarm()] += single_hit->GetDedx()/1000000.; //convert from keV to GeV
        fplt_nhits[single_hit->GetIarm()]++;
      }
      else if((single_hit->GetIncc()>10)&& (single_hit->GetIncc()<=20)){ // inner Al plates
        inner_energy[single_hit->GetIarm()] += single_hit->GetDedx()/1.0e6; //convert from keV to GeV
        inner_nhits[single_hit->GetIarm()]++;
      }
      else if((single_hit->GetIncc()>20)&& (single_hit->GetIncc()<=30)){ // back leakage
        back_leakage[single_hit->GetIarm()] += single_hit->GetDedx()/1.0e6; //convert from keV to GeV
        back_nhits[single_hit->GetIarm()]++;
      }
      else if((single_hit->GetIncc()>30)&& (single_hit->GetIncc()<=40)){ // side leakage
        side_leakage[single_hit->GetIarm()] += single_hit->GetDedx()/1.0e6; //convert from keV to GeV
        side_nhits[single_hit->GetIarm()]++;
      }
    }

    //std::cout << fNMpcFPLTHits << " " << fplt_nhits[0] << " " << fplt_nhits[1] << std::endl;  

  }

  int fNMpcExHits = pisahits->GetnHit();
  for (int ihit=0 ; ihit<fNMpcExHits ; ihit++)
    {
      NCCSnglPisaHit *single_hit = pisahits->GetHit(ihit);
      // North/South calorimeters
      // numbv(1) = ((((detId*10+segm)*10+layer)*10+xy)*100+sen_X)*100+sen_Y
      // numbv(2) = 1000*ix+iy
      int sid = single_hit->GetSENID();
      int tid = single_hit->GetTWRID()%1000000;//above this is the detId (N|S)
      int arm = 2-single_hit->GetIncc();
      //int segment = ((int)sid/1000000)%10 - 1 ;
      int layer = (((int)sid/100000)%10)-1;
      if(layer < 0 || layer >= MpcExConstants::NLAYERS){
	std::cout<<PHWHERE<<" Something is seriously wrong -- The layer of the GEANT hit is not valid: "<<layer<<std::endl;
	std::cout<<PHWHERE<<" BAILING."<<std::endl;
	return ABORTRUN;
      }
      int xy = (((int)sid/10000)%10)-1;
      //int ssKind = (xy)? 1 : 2;
      int sen_x = ((int)sid/100)%100;
      int sen_y = (int)sid%100;
      int xLoc = (int)tid/1000; if(sen_x!=99) xLoc--;
      int yLoc = (int)tid%1000; if(sen_y!=99) yLoc--;
      if(sen_x!=99&&sen_y!=99) {sen_x--; sen_y--;}

      //need to make sure not to read beyond array boundaries later
      static const unsigned short NSX = 6;
      static const unsigned short NSY = 6;
      if(sen_x<0 || sen_x>=NSX || sen_y<0 || sen_y>=NSY){
	std::cout<<PHWHERE<<" Serious problem: the sensor indices (sen_x,sen_y) = ("<<sen_x<<","<<sen_y<<") are out of bounds."<<std::endl;
	std::cout<<PHWHERE<<" BAILING!"<<std::endl;
	return ABORTRUN;
      }

      if( (xLoc>127) || (yLoc>127) ){
	// This is a dead area hit - sum up the energy 
	// for accounting. 
	deadAreaEnergy[single_hit->GetIarm()] += single_hit->GetDedx()/1000000.; //convert from keV to GeV
	dead_nhits[single_hit->GetIarm()]++; 
	continue; 
      }
      else{
        si_energy[single_hit->GetIarm()] += single_hit->GetDedx()/1000000.; //convert from keV to GeV
	si_nhits[single_hit->GetIarm()]++; 
      }

      //Convert into minipad(X,Y)
      // JGL 4/16/2015
      // Must convert into local micromodule coordinates
      // GEANT uses lower LH of micromodule as reference, 
      // real world uses lower RH corner. Modules rotates
      // 90 degrees CCW for y layers
      int minipad_x = -1;
      int minipad_y = -1;
      if(xy==0) {//x-orientation strips
	minipad_x = 31 - xLoc%32;
	minipad_y = xLoc/32; 
      }
      if(xy==1) {//y-orientation strips
	minipad_y = 31 - yLoc%32;
	minipad_x = 3 - yLoc/32; 
      }
      if(minipad_x<0 || minipad_y<0){
	std::cout<<PHWHERE<<" Something is seriously wrong. The local minipad coordinates are invalid: (mx,my) = ("<<minipad_x<<","<<minipad_y<<")"<<std::endl;
	std::cout<<PHWHERE<<"BAILING"<<std::endl;
	return ABORTRUN;
      }

      // JGL 4/16/2015
      // fix up rotation for bottom carrier boards
      if(sen_y<=2){
	if(xy==0) {
	  minipad_x = 31 - minipad_x; 
	  minipad_y = 3 - minipad_y; 
	}
	if(xy==1) {
	  minipad_y = 31 - minipad_y; 
	  minipad_x = 3 - minipad_x; 
	}
      }

      //////////////////////////////////////////
      //
      //for this key business we need:
      //arm, packet, chipmap
      //
      //and chipmap is created from
      //chain, chip, and channel
      //
      //////////////////////////////////////////

      //covert mx/my to lx/ly
      unsigned short lx = minipad_x;
      unsigned short ly = minipad_y;
      if((layer%2)==1){
	lx = minipad_y;
	ly = minipad_x;
      }

      //arm is already given 
      //and packet are easy calculations or look up
      unsigned short topbottom = sen_y<=2; //0=top, 1=bottom
      unsigned short packet = MpcExConstants::PACKET_IN_LAYER_TOPBOTTOM[topbottom][layer];

      //for the chain we need another lookup
      //this is for even layers
      //the odd layers are += 2 on valid sensors
      //this works in both arms because the sense of
      //the meaning of sen_x switches to go -x to +x
      //in the south arm and +x to -x in the north arm
      //the readout is swapped accordingly
      //the 0xffff indicate the that a sensor does not 
      //exist at that location
      static const unsigned short chain_sx_sy[NSY][NSX] =
	//left                                //right
	{{0xffff,1     ,0      ,0     ,0xffff,0xffff}, //bottom
	 {1     ,1     ,1      ,0     ,0     ,0xffff},
	 {1     ,1     ,0xffff,0xffff ,0     ,0     },
	 {0     ,0     ,0xffff,0xffff ,1     ,1     },
	 {0     ,0     ,1     ,1      ,1     ,0xffff},
	 {0xffff,0     ,0     ,1      ,0xffff,0xffff}}; //top
      int chain = chain_sx_sy[sen_y][sen_x];
      if(chain == 0xffff){
	std::cout<<" Something is wrong: attempting to grab a sensor that doesn't exist at (arm,sx,sy): ("<<arm<<","<<sen_x<<","<<sen_y<<")"<<std::endl;
      } else {
	//add 2 to the chain in the odd layers
	if((layer%2)==1){
	  chain += 2;
	}
      }

      //for the chip, we calculate the sensor within the 
      //chain readout, double it and then add one 
      //when it lx=>16
      //this works in both arms because the sense of
      //the meaning of sen_x switches to go -x to +x
      //in the south arm and +x to -x in the north arm
      //the readout is swapped accordingly
      static unsigned short sensor_in_chain[NSY][NSX] = {
	//left                              //right
	{0xffff,0     ,0     ,1     ,0xffff,0xffff}, //bottom
	{3     ,2     ,1     ,2     ,3     ,0xffff},
	{5     ,4     ,0xffff,0xffff,4     ,5     },
	{5     ,4     ,0xffff,0xffff,4     ,5     },
	{3     ,2     ,1     ,2     ,3     ,0xffff},
	{0xffff,1     ,0     ,0     ,0xffff,0xffff}  //top
      };
      unsigned short sensor = sensor_in_chain[sen_y][sen_x];
      unsigned short chip = sensor;
      if(sensor == 0xffff){
	std::cout<<" Something is wrong: attempting to grab a sensor that doesn't exist at (sx,sy): ("<<sen_x<<","<<sen_y<<")"<<std::endl;
      } else {
	chip = 2*sensor;
	//need to add one for sensors with lx>=16
	if(lx>15){
	  chip+=1;
	}
      }

      //what follows is a LUT that is the reverse
      //of the following LUTs from Nikki and Norbert
      //chip => 0==even 1==odd
      //0<=lx<32, 0<=ly<4
      //lx,ly should be related to mx,my from earlier
      /**
      //input is rocbond, chip0 is the odd chips, chip1 are the even chips in the packet
      int LocChip0Lx[64] = {
	0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15};
      int LocChip1Lx[64] = {
	16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,27,27,27,27,28,28,28,28,29,29,29,29,30,30,30,30,31,31,31,31};
      int LocChip0Ly[64] = {
	2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0}
      int LocChip1Ly[64] = {
	2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0,2,3,1,0};
       */
      static const unsigned short rocbond_from_chip_lx_ly[MpcExConstants::NCHIPS_PER_MODULE][MpcExConstants::NLX_PER_MODULE][MpcExConstants::NLY_PER_MODULE] = {{{3,2,0,1},{7,6,4,5},{11,10,8,9},{15,14,12,13},{19,18,16,17},{23,22,20,21},{27,26,24,25},{31,30,28,29},{35,34,32,33},{39,38,36,37},{43,42,40,41},{47,46,44,45},{51,50,48,49},{55,54,52,53},{59,58,56,57},{63,62,60,61},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff}},{{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{0xffff,0xffff,0xffff,0xffff},{3,2,0,1},{7,6,4,5},{11,10,8,9},{15,14,12,13},{19,18,16,17},{23,22,20,21},{27,26,24,25},{31,30,28,29},{35,34,32,33},{39,38,36,37},{43,42,40,41},{47,46,44,45},{51,50,48,49},{55,54,52,53},{59,58,56,57},{63,62,60,61}}};
      unsigned short chipnum = chip%2;
      unsigned short rocbond = rocbond_from_chip_lx_ly[chipnum][lx][ly];
      if(rocbond == 0xffff){
	std::cout<<"Something is seriously wrong -- trying to get the rocbond for (chip,lx,ly) = ("<<chipnum<<","<<lx<<","<<ly<<")"<<std::endl;
      }

      //generate the key for this minipad
      unsigned short chipmap = mapper->get_chipmap(chain,chip,rocbond);
      unsigned int key = mapper->generate_key(arm,packet,chipmap);

      //grab the hit by key, if it doesn't exist, add it to the container
      TMpcExGeaHit *geahit = geahits->get_hit_by_key(key);
      if(geahit == NULL){
	geahit = new TMpcExGeaHit(key);
	geahit->set_e(0.0); //explicitly clear energy
	geahits->addHit(geahit);
      }

      //add the energy to this hit
      float energy = single_hit->GetDedx()/1000000.; //convert from keV to GeV
      geahit->set_e(geahit->e()+energy);

      // add to the list of tracks contributing to this minipad

      geahit->add_contributor(single_hit->GetMctrack(),energy); 
      
      //      std::cout<<arm<<" "<<layer<<" "<<sen_x<<" "<<sen_y<<" "<<minipad_x<<" "<<minipad_y<<" "<<packet<<" "<<chain<<" "<<chip<<" "<<rocbond<<" "<<chipmap<<" "<<key<<" "<<geahit->low()<<" "<<geahit->high()<<std::endl;

    }//loop over pisahits


  // Loop over the geahits and fill the histogram

  if(makeHisto){
     for(unsigned int ghit=0; ghit<geahits->size(); ghit++){
       TMpcExGeaHit *geahit = geahits->getHit(ghit); 
       _histo->Fill(geahit->key(),geahit->e()*1.0e6); 
       _histoD->Fill(geahit->key(),geahit->e()*1.0e6); 
     }
  }

  // Finish up the PISA header entries

  //std::cout << "Back and Side Leakage" << std::endl; 

  if(pisaheader){    
    for(int iarm=0; iarm<2; iarm++){ 

      if((si_energy[iarm] + abs_energy[iarm] + deadAreaEnergy[iarm] + inner_energy[iarm])>0.0) 
        pisaheader->setSamplingFraction(iarm, 
					si_energy[iarm]/(si_energy[iarm] + abs_energy[iarm] + deadAreaEnergy[iarm] + inner_energy[iarm]) ); 
      else
	pisaheader->setSamplingFraction(iarm,0.0); 

      pisaheader->setGEANTEnergy(iarm, si_energy[iarm] + abs_energy[iarm] + deadAreaEnergy[iarm] + inner_energy[iarm]); 
      pisaheader->setABSEnergy(iarm, abs_energy[iarm]); 
      pisaheader->setDeadAreaEnergy(iarm, deadAreaEnergy[iarm]); 
      pisaheader->setSiEnergy(iarm, si_energy[iarm]); 
      pisaheader->setFPLTEnergy(iarm, fplt_energy[iarm]); 
      pisaheader->setABSnHits(iarm,abs_nhits[iarm]); 
      pisaheader->setSinHits(iarm,si_nhits[iarm]); 
      pisaheader->setDeadnHits(iarm,dead_nhits[iarm]); 
      pisaheader->setFPLTnHits(iarm,fplt_nhits[iarm]); 
      pisaheader->setnLowSat(iarm,0);   // to be filled later
      pisaheader->seteLowSat(iarm,0.0); // to be filled later

      pisaheader->setBackLeakage(iarm,back_leakage[iarm]); 
      pisaheader->setSideLeakage(iarm,side_leakage[iarm]); 
      pisaheader->setBacknHits(iarm,back_nhits[iarm]); 
      pisaheader->setSidenHits(iarm,side_nhits[iarm]); 
      pisaheader->setInnerEnergy(iarm,inner_energy[iarm]); 
      pisaheader->setInnernHits(iarm,inner_nhits[iarm]); 

      //std::cout << fplt_nhits[iarm] << " " << fplt_energy[iarm] << std::endl;
      //std::cout << back_nhits[iarm] << " " << back_leakage[iarm] << " " << side_nhits[iarm] << " " << side_leakage[iarm] << std::endl;
      //std::cout << inner_nhits[iarm] << " " << inner_energy[iarm] << std::endl;

    }
  }

  return EVENT_OK;
}

int mMpcExUnpackPISA::End(PHCompositeNode *topNode)
{

  if(makeHisto){
    outputfile->Write();
    outputfile->Close();
  }

  return 0;
}

