#ifndef __MVD_RP_PARAMS_H__
#define __MVD_RP_PARAMS_H__

#include <string>
#include <map>

#include "PdbMvdDeadchannel.hh"
#include "PdbMvdRp.hh"

class MvdRpParams{
  
public:
  virtual ~MvdRpParams();
  
  /// returns the instance associated with the given run number.
  /// It is guaranteed to have been initialized from the db.
  static MvdRpParams &instance(int iRun);
  
  /// Set all channels good
  void init();
  
  /// Get constants from database.
  void fetch(int runnumber);
  
  /** Store constants in database.
      @param beginrun the first run for which this recal data is valid.
      @param endrun the last run for which this recal data is valid.
      -1 means infinity.
  */
  void update(int beginrun, int endrun=-1, bool bMap=true, bool bRp=true);
  
  PdbMvdRp *getRp(){ return &m_rp; }
  // 0<=i<24
  PdbMvdDeadchannel *getDeadchannel(int i){ 
    if(0>i || i>23) return NULL;
    return (m_aDc + i);
  }
  
  bool setRp(PdbMvdRp &rp){
    m_rp = rp;
    return true;
  }
	bool setDeadchannel(int i, PdbMvdDeadchannel &dc){
	  if(0>i || i>23) return false;
	  m_aDc[i] = dc;
	  return true;
	}

  void set_channelDead(int iCol, int iRow, int iZ, bool bDead);
  bool get_channelDead(int iCol, int iRow, int iZ);
  
  /// return the last runnumber that was "fetch()"ed, or 0 if none
  int get_runNumber(){ return m_iRun; }
  
  int get_nDeadS(){ 
    if (m_nDeadS<0) calcDead();
    return m_nDeadS; 
  }
  int get_nDeadN(){ 
    if (m_nDeadN<0) calcDead();
    return m_nDeadN; 
  }
  
protected:
  
  MvdRpParams();
  static MvdRpParams *__instance;
  
  void calcDead();
  
  static std::map<int, MvdRpParams> paramMap;
  
  // the dead channel map from the database for this run;
  // indexes 0-23 correspond to packets 2097-2120.
  PdbMvdDeadchannel m_aDc[24];
  // the RP parameters for this run;
  PdbMvdRp          m_rp;
  
  std::string dcName;
  std::string rpName; 
  
  int m_iRun;
  int m_nDeadS;
  int m_nDeadN;
};

#endif
