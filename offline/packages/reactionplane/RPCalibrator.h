#ifndef __RPCALIBRATOR_H__
#define __RPCALIBRATOR_H__

#include <SubsysReco.h>
#include "RpConst.h"

#include <string>

class PreviousEvent;
class RpSumXYObject;
class ReactionPlaneObject;

class Fun4AllHistoManager;
class ReactionPlaneCalibv1;

class TFile;
class TProfile;
class TH1;
class TH2;
class TH3;

class RPCalibrator: public SubsysReco 
{
  public:
    enum CALIBFLAG { FLAG_RECENT = 0, FLAG_FLAT = 1, FLAG_NOCALIB = 2 };

  public:
    RPCalibrator(const char* output = "RPCalibrator");
    virtual ~RPCalibrator();
    
    virtual int  Init(   PHCompositeNode *topNode);
    virtual int  InitRun(PHCompositeNode *topNode);
    virtual int  process_event(PHCompositeNode *topNode);
    virtual int  End(PHCompositeNode *topNode);

    virtual int  Reset(     PHCompositeNode *topNode) { return 0; }
    virtual int  ResetEvent(PHCompositeNode *topNode) { return 0; }
//--    virtual void Print(const char *what) const { return; }

    virtual void setRecalFlag(int flag) { m_RecalFlag = flag; }
//--    virtual void setMasterRecalFlag(int flag) { m_MasterRecalFlag = flag; }
//--    virtual void setVnFlag(int flag) { m_VnFlag = flag; }
//--    virtual void setEvSkipFlag(int flag) { m_EvSkipFlag = flag; }
  
  protected:
    int    CreateNodeTree(PHCompositeNode *topNode) { return 0; }

    int m_RecalFlag;       // 0 : generate re-centering parameters
                           // 1 : generate flattening parameters
                           // 2 : use caliblation parameters
//--    int m_MasterRecalFlag; // 0 : master recalibrator module doesn't run
//--                           // 1 : run master recalibrator module
//--    int m_VnFlag;          // 0 : Resolution data for Vn is not created
//--                           // 1 : Resolution data is generated.
//--    int m_EvSkipFlag;
  
  private:
    static const unsigned int NHAR   = RP::NHAR4;
    static const unsigned int NHAR2D = 2;
    static const unsigned int NDET   = 7; // SVX, SEG, MPC, BBC, SMD, CNT, FVTX


    std::string OutputFileName;
    int         m_RunNumber;
    int         m_EventNumber;
  
    ReactionPlaneCalibv1* rpcalibv1;
    Fun4AllHistoManager*  HistoManager;
  
    void  initHisto();
    bool  isBadTick(PreviousEvent *peve);

    bool  fillAllRawQvector(const int detid, const int icent, const int iz, RpSumXYObject *sumxy);
    bool  fillRawQvector(const int detid, const unsigned int ikind, const unsigned int ihar, 
                         const int icent, const int iz, RpSumXYObject* sumxy);


    bool  fillAllFlatpar(const int detid, const int icent, const int iz, RpSumXYObject *sumxy, ReactionPlaneObject *rp);
    bool  fillFlatpar( const int detid, const unsigned int ikind, const unsigned int ihar,
                       const int icent, const int iz, RpSumXYObject *sumxy, ReactionPlaneObject *rp);


//--    int   EventSkip();

    // event
    TH2* m_centzv;
    TH2* m_centzv_svx;
    
    // recentering & flattening
    // ndet= Svx,     Seg, MPC, BBC, SMD, CNT
    // kind= 12+1+12   9    3    3    3    5
    TProfile** m_rawq  [RP::NMUL3][RP::NZPS2][RP::NHAR4][NDET]; // Raw Q vector to get recentering parameters
    TH3**      m_rawqxy[RP::NMUL3][NHAR2D][NDET];    // Raw Qx vs Qy vs iz

    // flattening
    TProfile** m_flatpar[RP::NMUL3][RP::NZPS2][RP::NHAR4][NDET]; // Raw RP to get flattening parameters
    TH1**      m_rawrp[RP::NMUL3][RP::NZPS2][RP::NHAR4][NDET];   // Raw RP to get flattening parameters


/*
    //Master Re-calib
    TH1*      FL_Psi1D[RP::NMUL3][RP::NZPS2][ndet3];
    TProfile* Vtx_Bbc_Reso[ndet3][3][2];
    TProfile* Cnt_Reso[ndet3][2];
    TProfile* Cor_S_N[3][2];
    //TEST

//--    TH2*      cntdphidz[10][RP::NZPS2];
//--    TH2*      cnttrack[10][RP::NZPS2];
    //--TProfile* Fvtx_S_N[ndet3][2];
    TProfile* Cntvn_Ob[10][ndet3][2];
*/
};

#endif

