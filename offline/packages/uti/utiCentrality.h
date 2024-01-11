
#ifndef __UTICENTRALITY_H__
#define __UTICENTRALITY_H__

class BbcOut;
class ZdcOut;
class PHCompositeNode;
class FclOut;
class PHGlobal;

// Centrality utilities embedded in a namespace.  

namespace PhUtilities {

//-----------------------------------------
// These functions were used for run2 AuAu
//-----------------------------------------

 int getCentralityByClock(BbcOut* bbc, ZdcOut* zdc);

 int getCentralityByClock(PHCompositeNode* topNode);

 int getCentralityByClock(float bbcen, float bbces, float zdcen, float zdces);

 int getCentralityByPerp(BbcOut* bbc, ZdcOut* zdc);

 int getCentralityByPerp(PHCompositeNode* topNode);

 int getCentralityByPerp(float bbcen, float bbces, float zdcen, float zdces);

//-----------------------------------------
// These functions are for run4 AuAu
//-----------------------------------------

 int getCentralityByClockRun4(float bbc1, float bbc2, float zdc1, float zdc2, int runno);
 int getCentralityByClockRun4(PHCompositeNode* topNode);
 int getCentralityByPerpRun4(float bbc1, float bbc2, float zdc1, float zdc2, int runno);
 int getCentralityByPerpRun4(PHCompositeNode* topNode);

//---------------------------------------------
// These functions are for run5 CuCu - j.nagle
//---------------------------------------------

 int getCentralityByBBCRun5CuCu(float bbc1, float bbc2, float zdc1, float zdc2, float zvertex, int runno);
 int getCentralityByBBCRun5CuCu(PHCompositeNode* topNode);

 //-------------------
 //centrality for d-Au
 //-------------------

 //Users, use these functions
 int getdAuCentrality(FclOut* fclSouth, ZdcOut* zdc);
 int getdAuCentrality(FclOut* fclSouth, BbcOut* bbc);
 int getdAuCentrality(BbcOut* bbc, ZdcOut* zdc);
 int getdAuCentrality(FclOut* fcl, BbcOut* bbc, ZdcOut* zdc);

 //functions used by the d Au centrality functions
 int getdAuCentralityFclZdc(float fclEnergyS, float zdcEnergyN, float zdcEnergyS);
 int getdAuCentralityFclBbc(float fclEnergyS, float bbcEnergyN, float bbcEnergyS);
 int getdAuCentralityBbcZdc(float bbcEnergyN, float bbcEnergyS, float zdcEnergyN, float zdcEnergyS);
 int getdAuCentralityFclBbcZdc(float fclEnergyS, float bbcEnergyN, float bbcEnergyS, float zdcEnergyN, float zdcEnergyS);
}
#endif 

