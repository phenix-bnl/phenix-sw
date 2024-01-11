///////////////////////////////////////////////////////////////
//   simulation for RUN2 ERT trigger configuration 
//
//   based on D. galanak's private lvl1 package
//
//   problem report to: xiewei@rcf2.rhic.bnl.gov 
//
//  Note: Here, a trigger tile, i.e.a tile, is the refered as a SM 
//        and it numbering convention is specifically defined for 
//        the trigger simulation         

#ifndef __mErtSimuRun2Module_H__
#define __mErtSimuRun2Module_H__
#include "phool.h"
#include "PHCompositeNode.h"


#include "EMCalRichDecode.h"

//#include "ErtSMMask.h"

class ErtSMMask;
class dCrkCalWrapper;
class dCrkRawWrapper;
class PHCompositeNode;
class TRandom;

#define NumofModules 6192
#define NumofEmcTT 172
#define NumofCrkTT 256
#define nArm 2
#define nSect 4
#define nRoc 20
#define NWORD 6

class mErtSimuRun2Module
{

  private:

    float EmcModuleDepositE[NumofModules]; 
    float CrkTriggerTileNPE[NumofCrkTT];
    float thres2x2[nArm][nSect];                  
    float thres4x4A[nArm][nSect];
    float thres4x4B[nArm][nSect];
    float thres4x4C[nArm][nSect];
    float noise2x2[nArm][nSect];                  
    float noise2x2Sigma[nArm][nSect];
    float noise4x4[nArm][nSect];                  
    float noise4x4Sigma[nArm][nSect];

    int   ModulesInEmcTile[36];
    int   moduleid;
    float sumA16;
    int   neighbours[4];
    float TileMax2x2E[NumofEmcTT]; //.. max 2x2 energy in a trigger tile
    float TileMax4x4E[NumofEmcTT]; //.. max 4x4 ....

    bool  IsTileHit[NumofEmcTT];
    bool  IsCrkTileHit[NumofCrkTT];
    EMCalRichDecode DECODE;
    long   initBit[nArm][nRoc][NWORD];  //.. set to 0 at the beginning

    ErtSMMask* SMMask; 

    float gain[nArm][nSect][48][96];  //.. gain(normalize to 1)for each channel
    float gain_additional;  //.. additional gain varition on top of gain[][][];
    float gainSigma;  //.. sigma of gaussian gain 
    float constGain[nArm][nSect];  //.. constant gain value;

    TRandom *rndm;

    bool  isGaussNoise;
    bool  isContNoise;
    bool  isRealGainVar;
    bool  isGausGainVar;
    bool  isConstGainVar;

    float crkThres;
    float crkNoiseSigma;

  private:

    void      GetSectFromEmctt(int emctt_in, int &arm, int &sect);
			//.. get arm, sect from emctt 
    int       GetModuleFromTower(int towerkey);  
	                //.. moduleID of a tower
    void      GetModuleNeighbours(int moduleid,int *neighbour); 
	                //..4x4 overlap neightbour
    int       GetEmcttFromModule(int moduleid); //.. SM trigger tile from module
    void      GetEmcttModules(int emctt,int *moduleids); 
	                //.. modules from a SM trigger tile.

    void      GetSMidFromEMCTrgTile(int &arm, int &sector,int &smID, int emctt);
			//.. get SM id from simulation SM EMCal trigger tile
    void      GetSMidFromCrkTrgTile(int &arm, int &sector,int &smID, int crktt);
			//.. get SM id from simulation SM CRK trigger tile

    void      AddTower(int towerkey, float DepositE);
    void      Reset();
    
    float     GetEcal(short adclopost, short adclopre,
                      short adchipost,short adchipre);
  			//.. get dEmcCalibTower energy

    float     getNPE(int iraw, float min_p, dCrkRawWrapper* dcrkraw, dCrkCalWrapper* dcrkcal);  //... get RICH NPE

    //...  applying gaussian noise .......
    void      ApplyGaus2x2TileNoise(int arm, int sect, float sigma) 
              { noise2x2Sigma[arm][sect] = sigma;} 
    void      ApplyGaus4x4TileNoise(int arm, int sect, float sigma)
              { noise4x4Sigma[arm][sect] = sigma;} 

    //...  applying constant noise .......
    void      ApplyConst2x2TileNoise(int arm, int sect, float input) 
              {noise2x2[arm][sect] = input;}  
    void      ApplyConst4x4TileNoise(int arm, int sect, float input)
              {noise4x4[arm][sect] = input;}  

    //... apply constant threshold  ..........
    void      ApplyConst2x2TileThreshold(int arm, int sect, float input) 
              {thres2x2[arm][sect] = input;}
    void      ApplyConst4x4aTileThreshold(int arm, int sect, float input) 
              {thres4x4A[arm][sect] = input;}
    void      ApplyConst4x4bTileThreshold(int arm, int sect, float input) 
              {thres4x4B[arm][sect] = input;}

    int      GetCrkttFromPMT(int pmt);
    void     AddPMT(int pmt,float npe);
  public:

    mErtSimuRun2Module(int runNumber);
    virtual ~mErtSimuRun2Module() {;}

    PHBoolean event(PHCompositeNode*);

    //...  applying gain variation .......
    void      ApplyRealGainVar() {isRealGainVar = true;} 
    void      ApplyGausGainVar(float sigma) 
              {gainSigma = sigma; isGausGainVar = true;}
    void      ApplyConstGainVar(int arm, int sect, float input) 
	      {constGain[arm][sect] = input; isConstGainVar = true;}
    void      ApplyAdditionalGain(float input) {gain_additional = input;}
       

    //... get gain variation .....
    void      GetGainVariation();

    //.. apply noise ...
    void      ApplyConstantNoise(int arm, int sect, float in2x2, float in4x4);
    void      ApplyGausNoise(int arm, int sect, float in2x2, float in4x4);

    void     ApplyCrkGausNoise(float sigma_noise)
             {crkNoiseSigma = sigma_noise;}
    void     SetCrkThreshold(float threshold) {crkThres = threshold;}

    //.. apply constant threshold ...
    void ApplyThreshold(int arm, int sect, float in2x2, float in4x4a, float in4x4b);

};
#endif /*__mErtSimuRun2Module_H__*/
