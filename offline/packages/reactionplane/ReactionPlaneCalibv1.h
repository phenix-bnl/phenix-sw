#ifndef __REACTIONPLANECALIBV1_H__
#define __REACTIONPLANECALIBV1_H__

#include "RpConst.h"

#include <string>
#include <map>
#include <vector>


class PdbCalBank;
class PdbApplication;
class PHCompositeNode;
class PHGlobal;
class PHTimeStamp;

class ReactionPlaneCalibv1
{
  public:
    typedef unsigned int uint;

  public:
    ReactionPlaneCalibv1();
    virtual ~ReactionPlaneCalibv1();

    virtual void Reset();

    virtual void Verbosity(const int val)    { m_verbosity = val; }

    virtual bool isCalibrationOK()           { return m_isCalibOK; }
    virtual bool isUsedQvectorForAnalysis(const int detid, const uint ikind);
    virtual void setUsedQvectorForAnalysis(const int detid, std::vector<int> vKind);

    virtual void setCalibrationOK(const bool flag) { m_isCalibOK = flag; }

    virtual void test();
  
    // Utility function to get centrality and z-vertex bin
    virtual int GetCentrality(PHCompositeNode *topNode);
    virtual int GetCentralityBin(const float cent);

    virtual int GetZVertexBin(PHCompositeNode *topNode, float& zvertex);
    virtual int GetZVertexBin(const float bbcz);
    virtual int GetZVertexBinSvx(const float vertex);
    
    // temporally function until centrality is available
    virtual int GetCentralityBin(PHCompositeNode *topNode, float& bbcchargesum);
    
    ////////////////
    virtual float Recentering(const int detid, 
                              const uint ikind, const uint ihar, const uint icent, const uint iz, 
                              const float qxy, const float qw, const int xy);
    virtual float Flattening( const int detid, 
                              const uint ikind, const uint ihar, const uint icent, const uint iz, 
                              const float psi);
    
    // Fetch calibration constatns
    bool Fetch(const int runNumber); 
    bool Fetch(const int detid, const int runNumber); 
    bool Fetch(const int detid, const PHTimeStamp& tstart);
    bool Fetch(const char* filename);
    
    // Update parameters to DB
    void Update(const int beginrun, const int endrun);
    void Update(const int detid, const int beginrun, const int endrun);
    void Update(const int detid, const PHTimeStamp& tstart, const PHTimeStamp& tstop);
    
    // Write parameter to file
    void Write(const char* filename);
    
    // Get/Set calibration parameters
    float GetSumXmean( const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz);
    float GetSumXsigma(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz);
    float GetSumYmean( const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz);
    float GetSumYsigma(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz);
    void  SetSumXmean( const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const float val);
    void  SetSumXsigma(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const float val);
    void  SetSumYmean( const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const float val);
    void  SetSumYsigma(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const float val);
    
    float GetFlatCos(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord);
    float GetFlatSin(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord);
    void  SetFlatCos(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord, const float val);
    void  SetFlatSin(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord, const float val);


    void  SetEnableDetByRunNumber(const int runnumber);
    void  SetEnableDetArray(const std::vector<int>& array, const bool flag);
    void  SetEnableDetAll(const bool flag);
    void  clearEnableDetArray() { m_enableArray.clear(); }

    bool  GetEnableDet(const int detid);
    const std::vector<int>& GetEnableDetArray() { return m_enableArray; }

    uint  GetNDet()                     {return NDET; } // N registered detector
    uint  GetNEnableDet()               {return m_enableArray.size(); } // N enabled detector
    void  PrintDet(const bool onlyenable=true);

    uint  GetDetNkind (const int detid) {return GetDetConst(detid, 0); }
    uint  GetDetNhar  (const int detid) {return GetDetConst(detid, 1); }
    uint  GetDetNcent (const int detid) {return GetDetConst(detid, 2); }
    uint  GetDetNz    (const int detid) {return GetDetConst(detid, 3); }
    uint  GetDetNorder(const int detid) {return GetDetConst(detid, 4); }
    std::string 
          GetDetName  (const int detid);

    bool IsSumUpdated (const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz);
    bool IsFlatUpdated(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord);
    
    
    // convert from idx to detid
    int  getDetId (const int idx);
    int  getDetIdx(const int detid);

    // config parameter
    void recoverCalibConfig(const char *name, 
                            int& icorrect, int& detid, int& ikind, int& ihar, 
                            int& icent, int& iz, int& ipar);

    void calcCalibConfig(const int icorrect, 
                         const int detid, const int ikind, const int ihar, 
                         const int icent, const int iz, const int ipar, 
                         char* name);
   
  private:
    // util funcs for Database I/F
    // Back ID
    //  1 =  Run4, Run5
    //  2 =  Run7 -
    int  GetBankID(int runNumber, const int detid) const;

    bool Fetch(PdbCalBank* rpBank);
    bool Update(PdbCalBank* rpBank);
    

    // util funcs for handling rpCalData
    uint GetDetConst(const int detid, const int ispecies);
    void setEnableDet(const int detid, const bool flag);
    void createEnableDetMap();

    void printIDtoIDXMap();
    void printIDXtoIDMap();
    
  private:
    int         m_verbosity;
    std::string m_databasename;
    bool        m_isCalibOK; 

    ///////////////////////
    // new parameter
    static const uint NDET     = 7;
    static const uint NKINDMAX = 70; //; //53+12;

    struct RpCalibData {
      RpCalibData(const uint id    = 0,
                  const uint nkind = NKINDMAX, 
                  const uint nhar  = RP::NHAR3, 
                  const uint ncent = RP::NMUL3, 
                  const uint nz    = RP::NZPS2,
                  const uint nord  = RP::NORD3,
                  const std::string& name = "N/A",
                  const bool         enable=false) :
         m_id(id), m_nkind(nkind), m_nhar(nhar), m_ncent(ncent), m_nz(nz), m_nord(nord), 
         m_name(name), m_enable(enable)
         { init(); }

      virtual ~RpCalibData(){}

      virtual void init();
      virtual void reset();
      virtual void print();

      virtual bool  isEnable() { return m_enable; }
      virtual void  setEnable(bool flag) { m_enable = flag; }

      virtual bool  isUsed( const uint ikind) { return (ikind<m_nkind) ? m_used[ikind] : false; }
      virtual void  setUsed(const uint ikind, const bool flag) { if(ikind<m_nkind) m_used[ikind] = flag; }
      virtual void  setUsedAll(const bool flag);
      virtual void  setUsedByArray(const uint size, const int *array, const bool flag);
   
      virtual float getSumXmean( const uint ikind, const uint ihar, const uint icent, const uint iz)
                                { return getSumXYmeansigma(0, ikind, ihar, icent, iz); }
      virtual float getSumXsigma(const uint ikind, const uint ihar, const uint icent, const uint iz)  
                                { return getSumXYmeansigma(1, ikind, ihar, icent, iz); }
      virtual float getSumYmean (const uint ikind, const uint ihar, const uint icent, const uint iz)  
                                { return getSumXYmeansigma(2, ikind, ihar, icent, iz); }
      virtual float getSumYsigma(const uint ikind, const uint ihar, const uint icent, const uint iz)  
                                { return getSumXYmeansigma(3, ikind, ihar, icent, iz); }
      virtual void  setSumXmean (const uint ikind, const uint ihar, const uint icent, const uint iz, const float val)
                                {        setSumXYmeansigma(0, ikind, ihar, icent, iz, val); }
      virtual void  setSumXsigma(const uint ikind, const uint ihar, const uint icent, const uint iz, const float val)
                                {        setSumXYmeansigma(1, ikind, ihar, icent, iz, val); }
      virtual void  setSumYmean (const uint ikind, const uint ihar, const uint icent, const uint iz, const float val)
                                {        setSumXYmeansigma(2, ikind, ihar, icent, iz, val); }
      virtual void  setSumYsigma(const uint ikind, const uint ihar, const uint icent, const uint iz, const float val)
                                {        setSumXYmeansigma(3, ikind, ihar, icent, iz, val); }

      virtual float getFlatCos(  const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord)
                                { return getFlatCosSin(0, ikind, ihar, icent, iz, iord); }
      virtual float getFlatSin(  const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord)
                                { return getFlatCosSin(1, ikind, ihar, icent, iz, iord); }
      virtual void  setFlatCos(  const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord, const float val)
                                {        setFlatCosSin(0, ikind, ihar, icent, iz, iord, val); }
      virtual void  setFlatSin(  const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord, const float val)
                                {        setFlatCosSin(1, ikind, ihar, icent, iz, iord, val); }

      virtual float getSumXYmeansigma(const uint idx, 
                                      const uint ikind, 
                                      const uint ihar, 
                                      const uint icent, 
                                      const uint iz);

      virtual void  setSumXYmeansigma(const uint idx, 
                                      const uint ikind, 
                                      const uint ihar, 
                                      const uint icent, 
                                      const uint iz,
                                      const float val);

      virtual float getFlatCosSin(const uint idx, 
                                  const uint ikind, 
                                  const uint ihar, 
                                  const uint icent, 
                                  const uint iz,
                                  const uint iord);

      virtual void  setFlatCosSin(const uint idx, 
                                  const uint ikind, 
                                  const uint ihar, 
                                  const uint icent, 
                                  const uint iz,
                                  const uint iord,
                                  const float val);

      virtual bool isSumUpdated(const uint ikind, 
                                const uint ihar, 
                                const uint icent, 
                                const uint iz);
 
      virtual bool isFlatUpdated(const uint ikind, 
                                 const uint ihar, 
                                 const uint icent, 
                                 const uint iz,
                                 const uint iord);

      virtual bool isValidRange(const uint ikind, 
                                const uint ihar, 
                                const uint icent, 
                                const uint iz);
 
      virtual bool isValidRange(const uint ikind, 
                                const uint ihar, 
                                const uint icent, 
                                const uint iz,
                                const uint iord);


      uint        m_id;
      uint        m_nkind;
      uint        m_nhar;
      uint        m_ncent;
      uint        m_nz;
      uint        m_nord;
      std::string m_name;

      bool        m_enable;

      bool  m_used[NKINDMAX];

      bool  m_SumUpdate[NKINDMAX][RP::NHAR3][RP::NMUL3][RP::NZPS2];
      float m_SumXmean [NKINDMAX][RP::NHAR3][RP::NMUL3][RP::NZPS2];
      float m_SumXsigma[NKINDMAX][RP::NHAR3][RP::NMUL3][RP::NZPS2];
      float m_SumYmean [NKINDMAX][RP::NHAR3][RP::NMUL3][RP::NZPS2];
      float m_SumYsigma[NKINDMAX][RP::NHAR3][RP::NMUL3][RP::NZPS2];

      bool  m_FlatUpdate[NKINDMAX][RP::NHAR3][RP::NMUL3][RP::NZPS2][RP::NORD3];
      float m_FlatCos   [NKINDMAX][RP::NHAR3][RP::NMUL3][RP::NZPS2][RP::NORD3];
      float m_FlatSin   [NKINDMAX][RP::NHAR3][RP::NMUL3][RP::NZPS2][RP::NORD3];
    };

    //RpCalibData m_rpCalData[NDET];
    RpCalibData* m_rpCalData[NDET];
    
    std::vector<int>   m_vIDXtoID; // conversion map <detid, detidx>
    std::map<int, int> m_vIDtoIDX; // reverse map <detid, detidx>

    std::vector<int>   m_enableArray;
};

#endif
