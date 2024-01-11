#include <iostream> 
#include <cassert>
#include <cstdio>
#include <utility>
#include <vector>
#include <tr1/unordered_map>
#include "EmcIndexer.h"
#include "PbScIndexer.h"
#include "PbGlIndexer.h"
// 
////////////////////////////////////////////////////////////////////////////////// 
// E.Kistenev         6/10/99 
// send comments to kistenev@bnl.gov 
//                    6/11/99  conversion from PHNX SM number to Sector Number
//                             and SM144 Number within the sector is included 
//////////////////////////////////////////////////////////////////////////////////

using namespace std;

// **********************************************************************

/// Internally used by EmcIndexer
struct emcTowerLocation {
  int arm,sector,yrow,zrow; 
};

vector<string> EmcIndexer::fEmcSectorIdent ;

//_____________________________________________________________________________
int 
EmcIndexer::EmcSectorNumber(const char * SectorId)
{ 
  if (fEmcSectorIdent.empty()) {
    fEmcSectorIdent.push_back("W0") ;
    fEmcSectorIdent.push_back("W1") ;
    fEmcSectorIdent.push_back("W2") ;
    fEmcSectorIdent.push_back("W3") ;
    fEmcSectorIdent.push_back("E2") ;
    fEmcSectorIdent.push_back("E3") ;
    fEmcSectorIdent.push_back("E0") ;
    fEmcSectorIdent.push_back("E1") ;
    fEmcSectorIdent.push_back("NONE") ;
  }

  int SectorNumber=0;

  while (SectorNumber<8 && fEmcSectorIdent[SectorNumber]!=SectorId) {
    SectorNumber++ ;
  }

  if(SectorNumber>7) { 
    return -1;
  } 
  else { 
    return SectorNumber; 
  }
}

//_____________________________________________________________________________
const char* 
EmcIndexer::EmcSectorId(int SectorNumber)
{ 
  if (fEmcSectorIdent.empty()) {
    EmcIndexer::EmcSectorNumber("W0") ;
  }

  return fEmcSectorIdent[SectorNumber].c_str() ;
}

//_____________________________________________________________________________
int EmcIndexer::SoftwareKey(int TowerId)
{
  static vector<int> softkeys ;
  static const int PXMAX = 24768 ;

  if (softkeys.size()==0) {

    softkeys.resize(PXMAX) ;

    int ItemId ;
    
    int iS ;   // sector number (0-7)
    int iSMT ; // tower_index number within Sector
    int iarm ; // arm:0=West, 1=East;
    int iy ;   // row number of the tower_index within a sector (bottom=>row=0)
    int iz ;   /* column number of the tower_index within a sector. 
		  column=0 for
		  lower bottom left tower_index, 
		  when sector is viewed from back 
		  (i.e. you are looking at the electronics).
	       */
    int swkey ; /* software key = 
		    100000 * iarm +
		    10000 * iS +
		    100 * iy +
		    iz
		 */

    for ( ItemId = 0 ; ItemId < PXMAX ; ItemId++ ) {

      EmcIndexer::iPXiSiST(ItemId,iS,iSMT) ; 

      iarm = ((iS<4)?0:1) ;
      if (iS<6) {
	iy = iSMT/72 ;
	iz = iSMT%72 ;
      }
      else {
	iy = iSMT/96 ;
	iz = iSMT%96 ; 
      }
      
      if(iS>=4){
	// change sector numbers to comply with PHENIX geography
	iS = ((iS -=6)<0)? iS+4 : iS;
      }
      swkey = SoftwareKey(iarm,iS,iy,iz) ;
      softkeys[ItemId] = swkey ;
    }
  }

  int rv ;
  
  if (TowerId<0 || static_cast<unsigned int>(TowerId) >= softkeys.size()) 
    {
      rv = 0 ;
    }
  else 
    {
      rv = softkeys[TowerId] ;
    }
  return rv ;
}

//_____________________________________________________________________________
int
EmcIndexer::SoftwareKey(int arm, int sector_in_arm, int yrow, int zrow)
{
  return 100000*arm + 10000*sector_in_arm + 100*yrow + zrow ;
}

//_____________________________________________________________________________
void 
EmcIndexer::PXSM144_iSiSM144(int PXSM144, int &iS, int &iSM144)
{
  //  PHENIX SM144 (1-172) -> Sector, SM NUmber(Sector SM)	
  if(PXSM144<108){
    iS    = PXSM144/18;
    iSM144= PXSM144%18;
  } else {
    iS    = 6 + (PXSM144-108)/32;
    iSM144= (PXSM144-108)%32;
  }
}


//_____________________________________________________________________________
int  
EmcIndexer::iSiSM144_PXSM144(int iS, int iSM144)
{
  //  Sector, SM NUmber(Sector SM) -> PHENIX SM144 (1-172) 
  if(iS<6){
    return iS*18+iSM144;
  } else {
    return 108 + (iS-6)*32+iSM144;
  }
}


//_____________________________________________________________________________
int 
EmcIndexer::iSiSMiSMTiST(int  SectorNumber, int  SMNumber, int  SMTower)
{
  // SectorNumber,SMNumber,SMTower -> Sector Tower
  if(SMTower<0) return SMTower;
  if(SectorNumber<6){
    PbScIndexer * gPbSc = PbScIndexer::buildPbScIndexer();
    return gPbSc->SMiSMTiST(SMNumber ,SMTower);
  } else {
    PbGlIndexer * gPbGl = PbGlIndexer::buildPbGlIndexer();
    return gPbGl->SMiSMTiST(SMNumber ,SMTower);
  }
}

//_____________________________________________________________________________
int 
EmcIndexer::iSiSMiSMTiPX(int SectorNumber, int SMNumber, int SMTower)
{
  // SectorNumber,SMNumber,SMTower -> PHENIX Tower
  if(SMTower<0) return SMTower;
  if(SectorNumber<6){
    PbScIndexer * gPbSc = PbScIndexer::buildPbScIndexer();
    return 2592*SectorNumber + gPbSc->SMiSMTiST(SMNumber ,SMTower);
  } else {
    PbGlIndexer * gPbGl = PbGlIndexer::buildPbGlIndexer();
    return 15552 + 4608*(SectorNumber-6) + gPbGl->SMiSMTiST(SMNumber ,SMTower);
  }
}

//_____________________________________________________________________________
void 
EmcIndexer::iSiSTiSMiSMT(const int iS, const int iST, int & iSM, int & iSMT)
{
  // SectorNumber, SectorTower -> SM NUmber, SM Tower
  int x, y; 
  if(iS<6){
    PbScIndexer * gPbSc = PbScIndexer::buildPbScIndexer();
    gPbSc->iST_SMInd(iST, iSM, iSMT, x, y);
  } else {
    PbGlIndexer * gPbGl = PbGlIndexer::buildPbGlIndexer();
    gPbGl->iST_SMInd(iST, iSM, iSMT, x, y);
  }
}

//_____________________________________________________________________________
int 
EmcIndexer::PXSM144iCH_iPX(const int PXSM144, const int iCH)
{
  /// SM144 (EMC Scope), FEM Channel -> PHENIX EMC Tower.
  static vector<vector<int> > ifem_vs_ichannel_to_towerID ;

  static int nfem = MaxNumberOfFEMs() ;
  static int nchannels = 144 ;

  // assertion is probably not user friendly, but this
  // method is meant to be fast, so we'd like to have
  // as few if as possible.
  // SO : CHECK THAT YOUR INPUT ARGUMENTS ARE CORRECT !
  assert(PXSM144>=0 && PXSM144<nfem) ;
  assert(iCH>=0 && iCH<nchannels) ;

  if (ifem_vs_ichannel_to_towerID.empty()) {
    
    ifem_vs_ichannel_to_towerID.resize(nfem) ;

    int ifem, ichannel ;    
    int iS, iSM144, iSMT ;

    for (ifem=0;ifem<nfem;ifem++) {

      ifem_vs_ichannel_to_towerID[ifem].resize(nchannels) ;

      for (ichannel=0;ichannel<nchannels;ichannel++) {
	
	PXSM144_iSiSM144(ifem, iS, iSM144);
	iSMT = iCHiSMT(ichannel);
	ifem_vs_ichannel_to_towerID[ifem][ichannel] = 
	  iSiSMiSMTiPX(iS, iSM144, iSMT);
      }
    }
  }

  return ifem_vs_ichannel_to_towerID[PXSM144][iCH] ;
}

//_____________________________________________________________________________
int 
EmcIndexer::iCHiSMT(int  iCH) 
{ 
  //  This is a very preliminary version which may well change at 
  //  a later time. It looks a bit complicated but it will be used
  //  only to establish look-up-tables for individual EmcFeeCrates
  int ASIC  = iCH/24;          //  2 tower wide columns
  int preamp = (iCH%24)/4;      //  2 tower wide rows counted 
                                    //  from above (0 at the top)
  int input  = iCH%4;
  //  convert input into x/y for cells within 'module' scope
  int xc, yc;
  yc = input/2;
  xc = 1-input%2;
  return 12*((5-preamp)*2+yc)+ASIC*2+xc;
}
       
//_____________________________________________________________________________
int 
EmcIndexer::iCHiSMT(const int Channel, const bool ASIC24)
{

  //   FEE Channel , ASIC Readout Flag -> i(SM Tower) 
  //   FIX for 144/192 channels switching

  int chPerBoard = (ASIC24)? 24 : 32;
  int ASIC   = Channel/chPerBoard;          //  2 tower wide columns
  int ASICCh = Channel%chPerBoard;
  if(!ASIC24) {
    if((ASICCh>11&&ASICCh<16)||ASICCh>27) return -1;
    if(ASICCh>=16) ASICCh -=4;
  }
  int preamp = ASICCh/4;            //  2 tower wide rows counted 
  //  from above (0 at the top)
  int input  = Channel%4;
  //  convert input into x/y for cells within 'module' scope
  int xc, yc;
  yc = input/2;
  xc = 1-input%2;
  return 12*((5-preamp)*2+yc)+ASIC*2+xc;
} 

//_____________________________________________________________________________
int   
EmcIndexer::iSMTiCH(const int iSMT, bool ASIC24)
{
  //   i(SM Tower), ASIC Readout Flag ->  FEE Channel 
  //   FIX for 144/192 channels switching
  int x, y;
  iSM144TxySM144T(iSMT, x, y);
  int ASIC = x/2;
  int ym = y/2;
  int preamp = 5-ym;
  int xc = x%2;
  int yc = y%2;
  int cell = 2*yc+(1-xc);
  if(ASIC24) {return ASIC*24+preamp*4+cell;
  } else {
    if(preamp<3) {return ASIC*32+preamp*4+cell;
    } else { return ASIC*32+4+preamp*4+cell;}
  }
}

//_____________________________________________________________________________
int 
EmcIndexer::absFEMCHiPX(const int absFEMCH)
{
  int FEM, iS, iCH, iSM, iSMT;
  FEM      = absFEMCH/192;
  iCH      = absFEMCH%192;
  iSMT     = iCHiSMT(iCH, false); 
  if(iSMT<0) return iSMT;
  if(FEM<108){
    iS     = FEM/18;
    iSM    = FEM%18;
  } else {
    iS     = 6+(FEM-108)/32;
    iSM    = (FEM-108)%32;
  }
  return iSiSMiSMTiPX(iS, iSM, iSMT);
}

//_____________________________________________________________________________
int 
EmcIndexer::iPXabsFEMCH(const int iPX)
{
  int iS,iST, iCH, iSM, iSMT;
  bool ASIC24 = false;
  EmcIndexer::iPXiSiST(iPX, iS, iST);
  iSiSTiSMiSMT(iS, iST, iSM, iSMT);
  iCH = iSMTiCH(iSMT, ASIC24);
  if(iS<6){
    return 192*(iS*18+iSM)+iCH;
  } else {
    return 192*(108+32*(iS-6)+iSM)+iCH;
  }
}

//_____________________________________________________________________________
void 
EmcIndexer::iCH192ASICi32(const int iCH192, int & ASIC, int & iCH32)
{
  ASIC = iCH192/32;
  iCH32 = iCH192%32;
}

//_____________________________________________________________________________
void 
EmcIndexer::iSM144TxySM144T(const int iSMT, int & xsmt, int & ysmt)   
{ 
  ysmt = iSMT/12; 
  xsmt = iSMT - ysmt*12; 
}

//_____________________________________________________________________________
void       
EmcIndexer::iSM144TtoHWIndexes(int iSMT, bool ASIC24, int & QB, int &Cell, 
			       int &ASIC, int &Preamp, int &Input, 
			       int &FEMChannel)
{
  int x, y;
  iSM144TxySM144T(iSMT, x, y);
  ASIC   = x/2;
  int ym = y/2;
  Preamp = 5-ym;
  QB     = ym*6 + ASIC;
  int xc = x%2;
  int yc = y%2;
  Cell   = 2*yc+(1-xc);
  Input  = Preamp*4 + Cell;
  if(ASIC24) {
    FEMChannel = ASIC*24+Preamp*4+Cell;
  } else {
    if(Preamp<3) {
      FEMChannel = ASIC*32+Preamp*4+Cell;
    } else { 
      FEMChannel = ASIC*32+4+Preamp*4+Cell;
    }
  }
}

//_____________________________________________________________________________
void 
EmcIndexer::findItemIdentity(const int ItemId, const bool ASIC24, 
			     const bool EMCalMapStyle, int & iS, int & iSM, 
			     int & iSMT, int &iFEM, int &iCH192, int &ASIC, 
			     int &ASICCh)
{	
  // convert channel number from DataMap into Crate/ASIC/Channel 
  if(EMCalMapStyle) {
    // numbering in the space of PHENIX Towers 
    int  iST;
    EmcIndexer::iPXiSiST(ItemId, iS, iST);
    EmcIndexer::iSiSTiSMiSMT(iS , iST , iSM , iSMT);
    iCH192 = iSMTiCH(iSMT, 0);
    iFEM = (iS>5)? 108+(iS-6)*32 : iS*18;
    iFEM += iSM;
  } else {
    // numbering in the space of PHENIX FEM Channels (absFEM) 
    iFEM   = ItemId/192;
    iCH192 = ItemId%192;
    PXSM144_iSiSM144(iFEM, iS, iSM);
    iSMT   = iCHiSMT(iCH192,ASIC24);
  }
  iCH192ASICi32(iCH192 , ASIC, ASICCh);
}

//_____________________________________________________________________________
bool 
EmcIndexer::IsValid(int iS, int x, int y){
  if(x<0||y<0) return false;
  if(iS<6){
    if(x>=72||y>=36) return false;
  } else {
    if(x>=96||y>=48) return false;
  }
  return true;
}

//***************************************************************************

int  EmcIndexer::iSiSTiPX(int iS, int iST){
  return ((iS<6)? 2592*iS+iST : 15552+4608*(iS-6)+iST);
} 

// **************************************************************************
void EmcIndexer::iPXiSiST(const int iPX, int & iS, int & iST)
{
  if(iPX<15552){
// PbSc Calorimeter
    iS  = iPX/2592;
    iST = iPX%2592;
  } else {
// PbGl Calorimeter
    iS  = 6+(iPX-15552)/4608;
    iST = (iPX-15552)%4608;
  }
}

// **************************************************************************
void EmcIndexer::iPXiSiSMiSMT(int iPX, int &iS, int &iSM, int &iSMT){
  int iST, ix, iy;
  if(iPX<15552){
// PbSc Calorimeter
    iS  = iPX/2592;
    iST = iPX%2592;
    ix  = iST%72;
    iy  = iST/72;
    iSM = iS*18 + (iy/12)*6+(ix/12);
  } else {
// PbGl Calorimeter
    iS  = 6+(iPX-15552)/4608;
    iST = (iPX-15552)%4608;
    ix  = iST%96;
    iy  = iST/96;
    iSM = 108+(iS-6)*32 + (iy/12)*8+(ix/12);
  }
  iSMT = (iy%12)*12+ix%12;
}
//***************************************************************************

void  EmcIndexer::iSTxyST(int iS, int iST, int & x, int & y){
  y = iST/((iS<6)? 72 : 96);
  x = iST%((iS<6)? 72 : 96);
} 

//***************************************************************************

int  EmcIndexer::xySTiST(int iS, int x,int y){
  return ((iS<6)? 72*y+x : 96*y+x);
}

//***************************************************************************

int  EmcIndexer::getTowerId(int iS, int x,int y){
  return ((iS<6)? 2592*iS+72*y+x : 15552+4608*(iS-6)+96*y+x);
}

//***************************************************************************

void EmcIndexer::decodeTowerId(int TowerId, int & iS, int & x,int & y){
  int iST;
  iPXiSiST(TowerId, iS, iST);
  iSTxyST(iS, iST, x, y);
}

//_____________________________________________________________________________
bool
EmcIndexer::TowerLocation(int towerID, int& arm, int& sector_in_arm,
			  int& yrow, int& zrow)
{
    if (towerID >= 24768) {
      arm = 0;
      sector_in_arm = 0;
      yrow = 0;
      zrow = 0;
      return false;
    }

    int iST;

    iPXiSiST(towerID, sector_in_arm, iST);
    iSTxyST(sector_in_arm, iST, zrow, yrow);
    arm = (sector_in_arm<4) ? 0 : 1;
    if (arm==1) {
      sector_in_arm -= 6;
      if (sector_in_arm<0) sector_in_arm += 4;
    }

    return true;
}

//_____________________________________________________________________________
//int 
//EmcIndexer::TowerID(int arm, int sector_in_arm, int yrow, int zrow)
//{
//}

//_____________________________________________________________________________
void EmcIndexer::PXPXSM144CH(int PX, int& PXSM144, int& CH) 
{
  static vector<pair<int,int> > fPXPXSM144CH ; 
  static const int PXMAX = 24768 ;

  if (PX<0 || PX>=PXMAX) {
    PXSM144 = -1 ;
    CH = -1 ;
  }

  else {

    if (fPXPXSM144CH.empty()) {

      // Allocate on first call.

      fPXPXSM144CH.resize(PXMAX) ;
      int i ;
      int iS, iST, iSM, iSMT ;
      for (i=0;i<PXMAX;i++) {
	iPXiSiST(i,iS,iST) ;
	iSiSTiSMiSMT(iS,iST,iSM,iSMT);
	fPXPXSM144CH[i].first = iSiSM144_PXSM144(iS,iSM) ;
	fPXPXSM144CH[i].second = iSMTiCH(iSMT) ;
      }
    }

    pair<int,int>& thepair = fPXPXSM144CH[PX] ;

    PXSM144 = thepair.first ;
    CH = thepair.second ;
  }
}


//_____________________________________________________________________________
int 
EmcIndexer::sectorOfflineToOnline(int arm, int offline_sector)
{
  //   ONLINE #  OFFLINE ARM #  OFFLINE SECTOR #
  //W0    0          0             0       PbSc
  //W1    1          0             1       PbSc 
  //W2    2          0             2       PbSc
  //W3    3          0             3       PbSc 
  //E2    4          1             2       PbSc
  //E3    5          1             3       PbSc 
  //E0    6          1             0       PbGl
  //E1    7          1             1       PbGl 

// ///////////////////////////////////////////////////////////////////////////

  int online_sector = -1 ;


  if ( arm==0 ) {
    online_sector = offline_sector ;
  }
  else {
    online_sector = ( offline_sector >= 2 ) ? offline_sector+2 
      : offline_sector+6 ;  
  }
  return online_sector ;
}

// ///////////////////////////////////////////////////////////////////////////

/// Tells if an absolute tower id is a PbSc reference.
bool EmcIndexer::isPbScReference(int TowerId)
{
	
	if(TowerId<24768) return false;

	int absFEM=((TowerId-24768)%96)/12+172;
	return ( absFEM>=172 && absFEM<=175 );
	
}

/// Tells if an absolute tower id is a PbGl reference.
bool EmcIndexer::isPbGlReference(int TowerId)
{
	
	if(TowerId<24768) return false;
	
	int absFEM=((TowerId-24768)%96)/12+172;
	return (absFEM>=176 && absFEM<=179 );
}

//_____________________________________________________________________________
void 
EmcIndexer::sectorOnlineToOffline(int sectorOnline, int& arm,
                                  int& offline_sector)
{
  if ( sectorOnline < 0 || sectorOnline > 8 ) 
    {
      arm=-1;
      offline_sector=-1;
    }

  if ( sectorOnline < 4 ) 
    {
      arm = 0;
      offline_sector = sectorOnline;
    }
  else if ( sectorOnline < 6 )
    {
      arm = 1;
      offline_sector = sectorOnline-2;
    }
  else
    {
      arm = 1;
      offline_sector = sectorOnline-6;
    }
}

//_____________________________________________________________________________
void 
EmcIndexer::decodeSoftwareKey(int key, int& arm, int& sector, 
                              int& yrow, int& zrow)
{
  arm    = key / 100000; 
  sector = ( key - arm * 100000 ) / 10000; 
  yrow   = ( key - arm * 100000 - sector * 10000 ) / 100; 
  zrow   = key - arm * 100000 - sector * 10000 - yrow * 100; 
}

//_____________________________________________________________________________
int
EmcIndexer::TowerID(int arm, int sector_in_arm, int yrow, int zrow)
{
  int iS = sectorOfflineToOnline(arm,sector_in_arm);
  int iST = xySTiST(iS,zrow,yrow);
  return iSiSTiPX(iS,iST);
}

//_____________________________________________________________________________
int
EmcIndexer::TowerID(int softwarekey)
{
  int arm,sector,yrow,zrow;
  EmcIndexer::decodeSoftwareKey(softwarekey,arm,sector,yrow,zrow);
  return TowerID(arm,sector,yrow,zrow);
}
