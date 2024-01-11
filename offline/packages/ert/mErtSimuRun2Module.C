#include "mErtSimuRun2Module.h"
#include "PHIODataNode.h"
#include "dEmcRawDataWrapper.h"
#include "dErtFemDataWrapper.h"
#include "dCrkHitWrapper.h"
#include "dCrkRawWrapper.h"
#include "dCrkRawHitParWrapper.h"
#include "dCrkCalWrapper.h"
#include "ErtSMMask.h"

#include "TRandom.h"

#include <algorithm>
#include <iostream>

using namespace std;

typedef PHIODataNode<dErtFemDataWrapper> dErtFemDataNode_t;
typedef PHIODataNode<dEmcRawDataWrapper> dEmcRawDataNode_t;
typedef PHIODataNode<dCrkHitWrapper> dCrkHitWrapperNode_t;
typedef PHIODataNode<dCrkRawWrapper> dCrkRawWrapperNode_t;
typedef PHIODataNode<dCrkRawHitParWrapper> dCrkRawHitParWrapperNode_t;
typedef PHIODataNode<dCrkCalWrapper> dCrkCalWrapperNode_t;

//...................................
mErtSimuRun2Module::mErtSimuRun2Module(int runNumber)
{
   Reset();
   rndm = new TRandom();
   isGaussNoise = false;
   isContNoise = false;
   isRealGainVar = false;
   isGausGainVar = false;
   isConstGainVar = false;

   //.. set runNumber for trigger mask
   cout<<"...... running on runNumber: "<<runNumber<<"......"<<endl;
   SMMask=new ErtSMMask(runNumber); 

   //..........................
   for(int iarm = 0; iarm<2; iarm++) 
     for(int isect = 0; isect<4; isect++) {
       	thres2x2[iarm][isect] = 0;
       	thres4x4A[iarm][isect] = 0;
	thres4x4B[iarm][isect] = 0;
	thres4x4C[iarm][isect] = 10000; //.. not implemented in RUN2
	noise2x2[iarm][isect] = 0;
	noise2x2Sigma[iarm][isect] = 0;
	noise4x4[iarm][isect] = 0;
	noise4x4Sigma[iarm][isect] = 0;
     }

    gain_additional = 1;
}
//...........................................
void mErtSimuRun2Module::GetGainVariation()
{ 

  //... assign everthing thing to 1 ... 
  for(int iarm = 0; iarm<nArm; iarm++)
    for(int isect = 0; isect<4; isect++) 
      for(int iy = 0; iy<48; iy++) 
         for(int iz = 0; iz<96; iz++) {
	    gain[iarm][isect][iy][iz] = 1;
         } 

   if(isRealGainVar) {

       cout<<" ++++ applying realistic gain variation ++++"<<endl;
       cout<<" Warning: this ability is disabled temperally "<<endl;
       cout<<" every twoer is set to 1"<<endl;
       cout<<" please send to: xiewei@rcf.rhic.bnl.gov for instructions"<<endl;

       /*
      FILE *fp;
      if((fp = fopen("ertEMCalGainVarRUN2.dat", "r")) == NULL) { 
         cout<<" ertEMCalGainVarRUN2.dat is not exist, exiting .."<<endl;
         exit(9);
      }
      int arm, sect, iy, iz;
      float tmp;
      for(int ii = 0; ii<24768; ii++) {
        fscanf(fp, "%d%d%d%d%f", &arm, &sect, &iy, &iz, &tmp); 
        gain[arm][sect][iy][iz] = tmp;
      }
      fclose(fp);
      */

   } else if(isGausGainVar) {
       cout<<" ++++ applying gaussian gain variation ++++"<<endl;
      cout<<" gainSigma = "<<gainSigma<<endl;
      for(int iarm = 0; iarm<2; iarm++) 
        for(int isect = 0; isect<4; isect++) 
          for(int iy = 0; iy<48; iy++)
            for(int iz = 0; iz<96; iz++) {
              gain[iarm][isect][iy][iz] = rndm->Gaus(1, gainSigma);
            } 
   } else if(isConstGainVar) {
       cout<<" ++++ applying constant gain variation ++++"<<endl;
       for(int iarm = 0; iarm<2; iarm++)
        for(int isect = 0; isect<4; isect++)
          for(int iy = 0; iy<48; iy++)
            for(int iz = 0; iz<96; iz++)
              gain[iarm][isect][iy][iz] = constGain[iarm][isect];
   }
}
//...................................
void mErtSimuRun2Module::Reset()
{//.. reset to initial value at the beginning of each event 


  for(int i = 0; i<NumofEmcTT; i++) {
    IsTileHit[i] = false;
    TileMax2x2E[i] = -9999;
    TileMax4x4E[i] = -9999;
  }

  for(int i = 0; i<NumofCrkTT; i++) {
    IsCrkTileHit[i] = false;
  }

  for(int iarm = 0; iarm<nArm; iarm++) 
     for(int iroc = 0; iroc<nRoc; iroc++) 
        for(int iword = 0; iword<NWORD; iword++) {
	   if(iroc == 0 || iroc == 2 || iroc == 4 || iroc == 5 ||
	      iroc == 7 || iroc == 8 || iroc == 9 || iroc == 11||
              iroc == 13 ||  iroc == 14 || iroc == 16 || iroc == 18 ||
              (iroc == 6 && (iword==0 || iword==1)) ||
	      (iroc == 15 && (iword==0 || iword==1)) ||
	      (iroc == 17 && (iword==5 || iword==2)) ||
	      (iroc == 19 && (iword==5 || iword==2))) 
	         initBit[iarm][iroc][iword] = 0xffff;
           else 
		 initBit[iarm][iroc][iword] = 0; 
        }

  for(int i=0; i<NumofModules; i++)
    EmcModuleDepositE[i] = 0;

  for(int i=0; i<NumofCrkTT; i++)
    CrkTriggerTileNPE[i] = 0;

}
//...................................
void mErtSimuRun2Module::AddTower(int towerkey, float DepositE)
{//.. transfer dEmcCalibTower information into trigger class

  int arm = towerkey/100000;
  int sect = (towerkey%100000)/10000;
  int iy = (towerkey/100)%100;
  int iz = towerkey%100;

  float unCalDepositE = DepositE * gain[arm][sect][iy][iz] * gain_additional;
  //cout<<" arm = "<<arm<<" sect = "<<sect<<" iy = "<<iy<<" iz = "<<iz<<" gain = "<<gain[arm][sect][iy][iz]<<" gain_additional = "<<gain_additional<<endl; 
  
  int mid =GetModuleFromTower(towerkey);
  EmcModuleDepositE[mid]+=unCalDepositE;
  if(GetEmcttFromModule(mid)!=-1) 
     IsTileHit[GetEmcttFromModule(mid)] = true;
}

//...................................
int mErtSimuRun2Module::GetModuleFromTower(int towerkey)
{//.. get module ID from Tower Key

  int iz=towerkey%100;
  towerkey/=100;
  int iy=towerkey%100;
  towerkey/=100;
  int sector=towerkey%10;
  towerkey/=10;
  int arm=towerkey;

  if (0==arm && sector<4 && iy<36 && iz<72)
	    return (int) iz/2+(iy/2)*36+sector*648;

  if (1==arm && sector<2 && iy<48 && iz<96) 
	    return (int) iz/2+(iy/2)*48+sector*1152+2592;

  if (1==arm && sector<4 && iy<36 && iz<72)
	    return (int) iz/2+(iy/2)*36+(sector-2)*648+4896;

  //cout<<" mErtSimuRun2Module::GetModuleFromTower ==> towerkey = "<<towerkey<< "goes beyond the range. arm = "<<arm<<" sector = "<<sector<<" iy = "<<iy<<" iz = "<<iz<<endl;
  return -1;
}

//...................................
void mErtSimuRun2Module::GetModuleNeighbours(int moduleid,int *neighbour)
{//.. get 4 neighbours including the module itself to do 4x4 overlapping 

  neighbour[0]=moduleid;
  neighbour[1]=neighbour[2]=neighbour[3]=-1;

  if (moduleid>=0 && moduleid<2592) {
     bool notup=((moduleid/36+1)%18);
     bool notleft=(moduleid%36);
     if (notleft) neighbour[1]=moduleid-1;
     if (notup && notleft) neighbour[2]=moduleid+35;
     if (notup) neighbour[3]=moduleid+36;
     return;
  }

  if (moduleid>=2592 && moduleid<4896) {
     bool notup=(((moduleid-2592)/48+1)%24);
     bool notleft=((moduleid-2592)%48);
     if (notleft) neighbour[1]=moduleid-1;
     if (notup && notleft) neighbour[2]=moduleid+47;
     if (notup) neighbour[3]=moduleid+48;
     return;
  }

  if (moduleid>=4896 && moduleid<6192) {
      bool notup=(((moduleid-4896)/36+1)%18);
      bool notleft=((moduleid-4896)%36);
      if (notleft) neighbour[1]=moduleid-1;
      if (notup && notleft) neighbour[2]=moduleid+35;
      if (notup) neighbour[3]=moduleid+36;
      return;
  }
}

//......................................
int mErtSimuRun2Module::GetEmcttFromModule(int moduleid)
{//.. get trigger tile ID from module id 

  if (moduleid>=0 && moduleid<2592) 
    return (int) (35-moduleid%36)/6+((moduleid/36)/6)*6;

  if (moduleid>=2592 && moduleid<4896) { 
    moduleid-=2592;
    return (int) (moduleid%48)/6-((moduleid/48)/6)*8+164;
  }
      
  if (moduleid>=4896 && moduleid<6192) {
    moduleid-=4896;
    return (int) (moduleid%36)/6-((moduleid/36)/6)*6+102;
  }

  cout<<"mErtSimuRun2Module::GetEmcttFromModule... ID of the module = "<<moduleid<<" goes beyond the range "<<endl;
  return -1;
}
//
//......................................
void mErtSimuRun2Module::GetEmcttModules(int emctt,int *moduleids)
{//.. get 36 module ID of a EMCal trigger tile  

  int moduleid0 = -1 ,nrow = 0;

  if (emctt>=0 && emctt<72) {
     nrow=36;
     moduleid0=(5-emctt%6)*6+(emctt/6)*216;
  }
  if (emctt>=72 && emctt<108) {
    nrow=36;
    moduleid0=(17-emctt/6)*216+(emctt%6)*6+4896;
  }
  if (emctt>=108 && emctt<172) {
    nrow=48;
    moduleid0=((emctt+4)%8)*6+(20-(emctt-4)/8)*288+2592;
  }
 
  if(emctt>=172) {
    cout<<"mErtSimuRun2Module::GetEmcttModules=> emctt = "<<emctt<<" goes beyond the range"<<endl;
    for(int i=0;i<6;i++)
      for(int j=0;j<6;j++)
	moduleids[i+6*j]= -1;

    return;
  }

  for(int i=0;i<6;i++)
    for(int j=0;j<6;j++)
       moduleids[i+6*j]=moduleid0+i+nrow*j;
}

//
//......................................
void mErtSimuRun2Module::GetSMidFromEMCTrgTile(int &arm, int &sector, int &smID, int emctt)
{ //  convert ERT EMCal simulated trigger tile to
  //  the hardware supermodule position

   if(emctt<72) {//...west arm

      if(emctt>=0  && emctt<18)        sector = 0; 
      else if (emctt>=18 && emctt<36)  sector = 1;
      else if (emctt>=36 && emctt<54)  sector = 2;
      else if (emctt>=54 && emctt<72)  sector = 3;

      arm = 0; //.. west arm 

      smID = ((emctt - 18*sector)/6 + 1)*6 - (emctt - 18*sector)%6 - 1;   

   } else if(emctt<172) { //.. east arm

     if(emctt>=72 && emctt<90) 		sector = 3;
     else if(emctt>=90 && emctt<108)	sector = 2;
     else if(emctt>=108 && emctt<140)	sector = 1;
     else if(emctt>=140 && emctt<172)	sector = 0;

     arm = 1; //.. east arm 

     if(sector<2) {
        
         smID = (emctt - 140 + sector*32) + 24 - (emctt - 140 + sector*32)/8*16;

     } else if(sector <4) {

         smID = (emctt - 126 + sector*18) + 12 - (emctt - 126 + sector*18)/6*12;
        
     } else {
        cout<<"+++  sector should not >= 4 , something is wrong +++ "<<endl;
     }

   } else {
      cout<<"++++ emctt = "<<emctt<<" goes beyond the range"<<endl; 
   }
}

//.....................................
PHBoolean mErtSimuRun2Module::event(PHCompositeNode* topNode)
{//.. calculate trigger bit for run-2 configuration ..

   int arm, sector, smID, roc, word, packet, bitEMC, bitRICH;
   
   Reset();   //.. reset all value at for each event 

   PHNodeIterator it(topNode);

   //
   //**********  Deal with EMCal first ********** 
   // 

   //.. get dEmcRawDatat able ..
   dEmcRawDataWrapper* dEmcRawData = 0;

   dEmcRawDataNode_t *demcrawdatanode = static_cast<dEmcRawDataNode_t *>(it.findFirst("PHIODataNode","dEmcRawData"));
   if(demcrawdatanode) {
      dEmcRawData = demcrawdatanode->getData();
   } else {
      cout<<" mErtSimuRun2Module: could not find table dEmcRawData"<<endl;
    // scan build fix: we crash in the next line when this is a null pointer
    // so we might as well exit to stop getting mails about this
    exit(1);
    }

   //.. get dErtFemData table ..
   //
   dErtFemDataWrapper* dErtFemData = 0;

   dErtFemDataNode_t *dertfemdatanode = static_cast<dErtFemDataNode_t *>(it.findFirst("PHIODataNode","dErtFemData"));
   if(dertfemdatanode) {
      dErtFemData = dertfemdatanode->getData();
   } else {
      cout<<" mErtSimuRun2Module: could not find table dErtFemData"<<endl;
   }

   //.. transfer tower information into trigger simulation class .. 
   //

   for(unsigned int itower =0; itower<dEmcRawData->RowCount(); itower++) {

     long swkey = dEmcRawData->get_swkey(itower); 
     short adclopost =dEmcRawData->get_adclopost(itower); 
     short adclopre  = dEmcRawData->get_adclopre(itower);
     short adchipost = dEmcRawData->get_adchipost(itower);
     short adchipre  =dEmcRawData->get_adchipre(itower);

     float ecal = GetEcal(adclopost, adclopre, adchipost, adchipre);
     AddTower(swkey, ecal);
   }
 
   for(int emctt=0; emctt<NumofEmcTT; emctt++) {

     if(!IsTileHit[emctt]) continue; //.. neglect 0-energy tiles

     GetSMidFromEMCTrgTile(arm, sector, smID, emctt);

     //... get maximum 2x2 module energy in a tile ....
     //
     GetEmcttModules(emctt,ModulesInEmcTile);

     for(int i=0;i<36;i++) {
        moduleid=ModulesInEmcTile[i];
        TileMax2x2E[emctt] =
            max(TileMax2x2E[emctt],EmcModuleDepositE[moduleid]);
     }
     //......add noise ....
     float tmp_noise = 0;
     if(isGaussNoise) 
        tmp_noise = rndm->Gaus(0, noise2x2Sigma[arm][sector]);
     else if(isContNoise)
        tmp_noise = noise2x2[arm][sector];

     //cout<<" 2x2 tmp_noise = "<<tmp_noise<<" TileMax2x2E[emctt] = "<<TileMax2x2E[emctt]<<" isGaussNoise = "<<isGaussNoise<<" noise2x2Sigma[arm][sector] = "<<noise2x2Sigma[arm][sector]<<" isContNoise = "<<isContNoise<<" tmp_noise = "<<tmp_noise<<endl;

     TileMax2x2E[emctt] += tmp_noise;

     //....  4x4 overlapping ....
     //
     for(int i=0;i<36;i++) {
        moduleid=ModulesInEmcTile[i];
        GetModuleNeighbours(moduleid,neighbours);
        sumA16=0.0;
        for(int j=0;j<4;j++) {
          if (neighbours[j]!=-1) {
            //
            //.. Kensuke said no connection between superModule in PBSC
            //.. for RUN2 by mistake 
            if(emctt<108) { //.. PBSC

              if(GetEmcttFromModule(neighbours[j]) == emctt) 
                 sumA16+=EmcModuleDepositE[neighbours[j]];

            } else {//.. PBGL, no connect bewteen sector

              int arm_tmp, sect_tmp, arm, sect;

              int tmp = GetEmcttFromModule(neighbours[j]);
              GetSectFromEmctt(tmp, arm_tmp, sect_tmp);
              GetSectFromEmctt(emctt, arm, sect);

              if(arm_tmp==arm && sect_tmp==sect) 
                 sumA16+=EmcModuleDepositE[neighbours[j]];
            }
          }
        } 
        TileMax4x4E[emctt]=max(TileMax4x4E[emctt],sumA16);
     }
     //......add noise ....
     tmp_noise = 0;
     if(isGaussNoise) 
        tmp_noise = rndm->Gaus(0, noise4x4Sigma[arm][sector]);
     else if(isContNoise)
        tmp_noise = noise4x4[arm][sector];

     //cout<<" 4x4 tmp_noise = "<<tmp_noise<<" TileMax4x4E[emctt] = "<<TileMax4x4E[emctt]<<" noise4x4Sigma[arm][sector] = "<<noise4x4Sigma[arm][sector]<<endl;
     TileMax4x4E[emctt] += tmp_noise;

     //....  calculate the value of word ....
     //
     if(TileMax4x4E[emctt] > thres4x4A[arm][sector]) {
        DECODE.GetRocWordBitPacket(arm, sector, 0, smID, 
                                roc, word, packet, bitEMC, bitRICH);
        if(!SMMask->Get(arm,sector,smID,0))
            initBit[arm][roc][word] |= 1<< bitEMC;
     } 

     if(TileMax4x4E[emctt] > thres4x4B[arm][sector]) {
        DECODE.GetRocWordBitPacket(arm, sector, 1, smID, 
                                roc, word, packet, bitEMC, bitRICH);
	if(!SMMask->Get(arm,sector,smID,1))
            initBit[arm][roc][word] |= 1<< bitEMC;
     }

     if(TileMax4x4E[emctt] > thres4x4C[arm][sector]) {
        DECODE.GetRocWordBitPacket(arm, sector, 2, smID, 
                                roc, word, packet, bitEMC, bitRICH);
	if(!SMMask->Get(arm,sector,smID,2))
            initBit[arm][roc][word] |= 1<< bitEMC;
     }

     if(TileMax2x2E[emctt] > thres2x2[arm][sector]) {
      //cout<<"2x2 trigger arm= = "<<arm<<" sector = "<<sector<<" thres = "<<thres2x2[arm][sector]<<endl;
        DECODE.GetRocWordBitPacket(arm, sector, 3, smID, 
                                roc, word, packet, bitEMC, bitRICH);
	if(!SMMask->Get(arm,sector,smID,3))
            initBit[arm][roc][word] |= 1<< bitEMC;
     }

     //cout<<" arm = "<<arm<<" sector= "<<sector<<" thres2x2 = "<<thres2x2[arm][sector]<<" thres4x4B = "<<thres4x4B[arm][sector]<<" thres4x4A = "<<thres4x4A[arm][sector]<<endl;

   }

   //
   // ***********  now deal with RICH *************
   //

   //.. get dCrkRawHit able ..
   dCrkRawWrapper* dcrkraw = 0;

   dCrkRawWrapperNode_t *dcrkrawhitNode = static_cast<dCrkRawWrapperNode_t *>(it.findFirst("PHIODataNode","dCrkRaw"));
   if(dcrkrawhitNode) {
      dcrkraw = dcrkrawhitNode->getData();
   } else {
      cout<<" mErtSimuRun2Module: could not find table dCrkRaw"<<endl;
   }

   //.. get dCrkRawHit able ..
   dCrkCalWrapper* dcrkcal = 0;

   dCrkCalWrapperNode_t *dcrkcalnode = static_cast<dCrkCalWrapperNode_t *>(it.findFirst("PHIODataNode","dCrkCal"));
   if(dcrkcalnode) {
      dcrkcal = dcrkcalnode->getData();
   } else {
      cout<<" mErtSimuRun2Module: could not find table dCrkCal"<<endl;
   }

   //.. get dCrkRawHit table ..
   dCrkRawHitParWrapper* dcrkpar = 0;

   dCrkRawHitParWrapperNode_t *dcrkparnode = static_cast<dCrkRawHitParWrapperNode_t *>(it.findFirst("PHIODataNode","dCrkRawHitPar"));
   if(dcrkparnode) {
      dcrkpar = dcrkparnode->getData();
   } else {
      cout<<" mErtSimuRun2Module: could not find table dCrkRawHitPar"<<endl;
   }

   for(unsigned int iraw =0; iraw<dcrkraw->RowCount(); iraw++) {
     int pmt = dcrkraw->get_pmt(iraw);
     float min_p = dcrkpar->get_min_pe(0);
     float npe = getNPE(iraw, min_p, dcrkraw, dcrkcal);
     AddPMT(pmt, npe);
   }

   for(int crktt =0; crktt <NumofCrkTT; crktt ++) {

     if(!IsCrkTileHit[crktt]) continue; //.. neglect 0-npe tiles

     GetSMidFromCrkTrgTile(arm, sector, smID, crktt);
     DECODE.GetRocWordBitPacket(arm, sector, 4, smID, roc, word, packet, bitEMC, bitRICH);

     // apply a gaussian noise first ...
     //
     float tmp = rndm->Gaus(0, crkNoiseSigma);
     CrkTriggerTileNPE[crktt] += tmp; 
     if(CrkTriggerTileNPE[crktt] > crkThres) 
          initBit[arm][roc][word] |= 1<<bitRICH;
   }

   //... now fill the dErtFemData table ..
   //
   int icount=0; 
   for(int iarm = 0; iarm<nArm; iarm++) 
     for(int iroc = 0; iroc<nRoc; iroc++) 
       for(int iword = 0; iword<NWORD; iword++) {
          dErtFemData->set_id(icount, icount);
          dErtFemData->set_crate(icount, iarm);
          dErtFemData->set_Roc(icount, iroc);
          dErtFemData->set_word(icount, iword);
          dErtFemData->set_Value(icount, initBit[iarm][iroc][iword]);
          icount++;
       }

   dErtFemData->SetRowCount(icount);

   return True;
}

//.....................................
float mErtSimuRun2Module::GetEcal(short adclopost, short adclopre, short adchipost,short adchipre)
{
   float e_lo =  adclopost - adclopre ;
   float e_hi =  adchipost - adchipre ;
   e_lo = e_lo * 0.001 ;
   e_hi = e_hi * 0.008 ;
   if(e_lo < 0.0) e_lo = - e_lo;
   if(e_hi < 0.0) e_hi = - e_hi;
            
   float e = (e_lo > e_hi) ? e_lo : e_hi;    
 
   return e;
}

//...........................
void mErtSimuRun2Module::GetSMidFromCrkTrgTile(int &arm, int &sector, int &smID, int crktt)
{
  if(crktt<128) { //.. west arm
    arm = 0;
    sector = crktt/32;
    smID = 15 + (((crktt/8)%4)/2 + sector)*32 - crktt;
  } else if (crktt<256) { //.. east arm
    arm = 1;
    sector = 7 - crktt/32;
    smID = crktt - (((crktt/8)%4)/2 + 3 - sector)*32 - 112;
  } else {
    cout<<"error:  crktt = "<<crktt<<" goes beyond range "<<endl;
  }
}
//....................................
void mErtSimuRun2Module::ApplyConstantNoise(int arm, int sect, float in2x2, float in4x4) 
{
  isContNoise = true;
  ApplyConst2x2TileNoise(arm, sect, in2x2);
  ApplyConst4x4TileNoise(arm, sect, in4x4);
}
//....................................
void mErtSimuRun2Module::ApplyGausNoise(int arm, int sect, float in2x2, float in4x4)
{
   isGaussNoise = true;

   ApplyGaus2x2TileNoise(arm, sect, in2x2);
   ApplyGaus4x4TileNoise(arm, sect, in4x4);
}
//....................................
void mErtSimuRun2Module::ApplyThreshold(int arm, int sect, float in2x2, float in4x4a, float in4x4b)
{
  ApplyConst2x2TileThreshold(arm, sect, in2x2);
  ApplyConst4x4aTileThreshold(arm, sect,in4x4a);
  ApplyConst4x4bTileThreshold(arm, sect, in4x4b);
}
//....................................
void  mErtSimuRun2Module::GetSectFromEmctt(int emctt_in, int &arm, int &sect)
{
  if(emctt_in>=0 && emctt_in<18) {
    arm = 0; 
    sect = 0;	
  } else if(emctt_in>=18 && emctt_in<36) {
    arm = 0; 
    sect = 1;	
  } else if(emctt_in>=36 && emctt_in<54) {
    arm = 0; 
    sect = 2;	
  } else if(emctt_in>=54 && emctt_in<72) {
    arm = 0; 
    sect = 3;	
  } else if(emctt_in>=72 && emctt_in<90) {
    arm = 1; 
    sect = 3;	
  } else if(emctt_in>=90 && emctt_in<108) {
    arm = 1; 
    sect = 2;	
  } else if(emctt_in>=108 && emctt_in<140) {
    arm = 1; 
    sect = 1;	
  } else if(emctt_in>=140 && emctt_in<172) {
    arm = 1; 
    sect = 0;	
  }
}
//......................................
int mErtSimuRun2Module::GetCrkttFromPMT(int pmt) 
{
  int crktt = -1;
  int crk_sector = pmt/1280;
  int npmt = pmt%1280;
  
  if (0==crk_sector) {
    crktt=(15-npmt%16)/4+((npmt/16)/5)*8;
  }
  if (1==crk_sector) {
    crktt=(npmt%16)/4+((npmt/16)/5)*8+4;
  }
  if (2==crk_sector) {
    crktt=(15-npmt%16)/4+((79-npmt/16)/5)*8+128;
  }
  if (3==crk_sector) {
    crktt=(npmt%16)/4+((79-npmt/16)/5)*8+132;
  }
  return(crktt);

}
//.............................................
void mErtSimuRun2Module::AddPMT(int pmt,float npe) {
  int crktt=GetCrkttFromPMT(pmt);
  if(crktt>=0) { 
    CrkTriggerTileNPE[crktt]+=npe;
    IsCrkTileHit[crktt] = true;
  }
}
//....................................
float mErtSimuRun2Module::getNPE( int iraw, float min_p, dCrkRawWrapper* dcrkraw, dCrkCalWrapper* dcrkcal)
{
  int pmt = dcrkraw->get_pmt(iraw);
  float npe =0;

  if( dcrkcal->get_adc_gain(pmt) > 0) {
    npe =  (dcrkraw->get_adc(iraw) - dcrkcal->get_adc_ped(pmt))/dcrkcal->get_adc_gain(pmt);
    if(npe < min_p) npe = min_p;
  } else { 
    npe = 100.; 
  }

  return npe;
}
