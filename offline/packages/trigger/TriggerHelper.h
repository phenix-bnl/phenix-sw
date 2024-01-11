/*  ////////////////////////
// TriggerHelper:
//    analysis object implementation:
//  general utility object providing  information spanning both lvl1 and lvl2   
//  such as whether an event is minbias 
//
//    analysis object:  to be used with dst and microDST storage 
//    objects.
//
//
//    Uses the L2Decision helper object    
//   
//    for doc on that option see 
//    
//    See www.phenix.bnl.gov/WWW/evb/lvl2/l2offline.html
//
//
//    
//    author: jfrantz  
//
//////////////////////////
*/

#ifndef __TriggerHelper_H
#define __TriggerHelper_H

#include <string>
#include <vector>

class Lvl2DecisionOut;
class L2DecisionHelper;
class PHCompositeNode;
class TrigLvl1;
class TrigRunLvl1;
class TrigRunLvl2;

// this data structure may contain info
// retrieved during the course of a 
// IsMinBias request -- for future use

class TriggerHelper
{
 public:

  /*
    The three different options are for convienience:  it will
    retrieve objects for you from the node tree or if you 
    have your own objects you can use them */

  //  TriggerHelper(DstContent* ezdstTopNode);//interface is no longer
  // supported as of 03/2006--no one should use ezdst anymore: only fun4all

  //! default constructor
  TriggerHelper();

  //! constructor from top node
  TriggerHelper(PHCompositeNode *topNode);
  
  //! direct constructor from external nodes
  TriggerHelper(
		TrigLvl1 *trigL1,
		TrigRunLvl1 *trigRunL1, 
		Lvl2DecisionOut *trigL2, 
		TrigRunLvl2 *trigRunL2
		);

  //! trigger names initialization
  void initialize();
	
  class IsMinBiasInfo 
    {
    public:
      IsMinBiasInfo() { IsMinBias = false;}
      bool IsMinBias;
      unsigned int whichLvl1Accepted; 
      unsigned int whichLvl2Accepted; 
    };

  //! stores local pointer to requested nodes
  void setNodes(PHCompositeNode * topNode);

  void dump_info(IsMinBiasInfo* info = NULL) const;    

  IsMinBiasInfo __info; // this is a temporary hack to fix memory leak
  // associated with new'ing _lastMinBiasInfo -- now that pointer
  // will refer to this member variable

  bool IsEventMinBias(IsMinBiasInfo* info = NULL) const;    
  bool IsEventMinBias_noZDCrequired(IsMinBiasInfo * info = NULL);
  bool IsEventMinBias_requireZDC(IsMinBiasInfo * info = NULL);
  bool IsEventClockMinBias(IsMinBiasInfo * info = NULL);

  bool IsRun2_AuAu() const;
  bool IsRun2_PP() const;
  bool IsRun3_dAu() const;
  bool IsRun3_PP() const;
  bool IsRun4_AuAu_63GeV() const;
  bool IsRun4_AuAu_200GeV() const;
  bool IsRun4_PP() const;
  bool IsRun5_CuCu() const;
  bool IsRun5_PP() const;
  bool IsRun6_PP() const;
  bool IsRun7_AuAu_200GeV() const;
  bool IsRun8_dAu_200GeV() const;
  bool IsRun8_pp_200GeV() const;
  bool IsRun9_pp_500GeV() const;
  bool IsRun9_pp_200GeV() const;

  // old methods supporting backwards compatibility
  bool IsRunAuAu() { return (IsRun7_AuAu_200GeV() ||
			      IsRun4_AuAu_63GeV()  || 
			      IsRun4_AuAu_200GeV() ||  
			      IsRun2_AuAu());       }

  bool IsRunPP() { return   (IsRun2_PP() ||
			      IsRun3_PP() ||
			      IsRun4_PP() ||
			      IsRun5_PP() ||
			      IsRun6_PP() );         } 

  bool IsRun_dAu_Run3()  { return IsRun3_dAu(); }			 


  bool checkDataNoFill() const;
  bool IsLevel2RejectOn();

  enum RawScaledLive { l1Raw, l1Scaled, l1Live };
  bool returnL1RawScaledLive(const char * TriggerName, RawScaledLive rsl) const;
  
	bool didLevel1TriggerFire(std::string TriggerName) const
		{ return returnL1RawScaledLive(TriggerName.c_str(), l1Live); }

	bool didLevel1TriggerFireRaw(std::string TriggerName) const
		{ return returnL1RawScaledLive(TriggerName.c_str(), l1Raw); }

	bool didLevel1TriggerGetScaled(std::string TriggerName) const
		{ return returnL1RawScaledLive(TriggerName.c_str(), l1Scaled); }
  
  //same methods as above but less typing
  bool trigLive(std::string lvl1TriggerName) const
    { return didLevel1TriggerFire(lvl1TriggerName);}
	
  bool trigScaled(std::string lvl1TriggerName) const
    { return didLevel1TriggerGetScaled(lvl1TriggerName);}
	
  bool trigRaw(std::string lvl1TriggerName) const
    { return didLevel1TriggerFireRaw(lvl1TriggerName);}
  
  enum L1What {l1Scaledown = 0, l1BitMask, l1BitNumber, l1Rate, l1Enabled};
	
  int getLevel1TriggerProperty(const char * TriggerName, L1What property);
  
  bool isLevel1TriggerEnabled(std::string TriggerName);  

  unsigned int getLevel1Scaledown(std::string TriggerName)
		{ return getLevel1TriggerProperty(TriggerName.c_str(), l1Scaledown); }

  unsigned int getLevel1BitMask(std::string TriggerName)
		{ return getLevel1TriggerProperty(TriggerName.c_str(), l1BitMask); }

  unsigned int getLevel1BitNumber(std::string TriggerName)
		{ return getLevel1TriggerProperty(TriggerName.c_str(), l1BitNumber); }

	
  //float getLevel1InitialScalerRate(char * TriggerName); 
  // getters
  TrigRunLvl1 * get_trigRunLvl1() 
	{return _trigRunLvl1;}
	
  TrigLvl1 * get_trigLvl1() 
	{return  _trigLvl1;}
	
  L2DecisionHelper * get_l2DecisionHelper() 
	{return _l2DecisionHelper;}
  
	//! destructor
  virtual ~TriggerHelper();

  // pointers from the previous object
  // do NOT modify these ever:  Add new members with new names 
  static TrigRunLvl2 * _lastTrigRunLvl2;
  static TrigRunLvl1 * _lastTrigRunLvl1;
  static TrigLvl1 * _lastTrigLvl1;

  static bool _isFirst;

  // verbosity level of error messages
  static int _verbosity;

  // verbosity level for error messages
  // 0 = no error messages,  1 = once per file with NO level1 errors,
  // 2 = once per file with ALL level1 errors (DEFAULT),   3 = display all messages
  static void set_verbosity(int verbosity);
  static int get_verbosity() {return _verbosity;}

 private:

  void initializeFromComponents(
				TrigLvl1* trigL1, 
				TrigRunLvl1 * trigRunL1, 
				Lvl2DecisionOut * trigL2, 
				TrigRunLvl2* trigRunL2
				);

  static const unsigned int maxLvl1Trigs = 32;

  // names for Run 2 for finding strings in trigger names
  // DO NOT modify the following 22 lines ever (again):  
  //  for new runs, add new members with new names these (e.g. _bbcNm_auau_run10) 
  // these 22 lines are for run2 through run4 AuAu  (NOT run4 pp!!!)
  static const int _maxNum_TriggerDesc_auau = 1;
  static const int _maxNum_TriggerDesc_pp = 2;

  std::vector<std::string> _TriggerDesc_auau;
  std::vector<std::string> _TriggerDesc_pp;

  // do NOT modify these ever:  for new runs, add new members with new names: see above/below
  static const int _maxNum_bbcNames_auau = 5;
  static const int _maxNum_zdcNames_auau = 1;
  static const int _maxNum_clockNames_pp = 1;
  static const int _maxNum_bbcntcNames_pp = 3;
  static const int _maxNum_rejectNames_pp = 2;
  static const int _maxNum_bbcNames_run5_cucu = 1;
  static const int _maxNum_bbcNames_run5_pp   = 1;
  static const int _maxNum_bbcNames_run6_pp   = 1;
  static const int _maxNum_bbcNames_run7_auau = 1;
  static const int _maxNum_bbcNames_run8_dau = 1;
  static const int _maxNum_bbcNames_run8_pp = 1;
  static const int _maxNum_bbcNames_run9_pp200gev = 1;
  static const int _maxNum_bbcNames_run9_pp500gev = 1;

  // do NOT modify these ever:  for new runs, add new members with new names: see above/below
  std::vector<std::string> _bbcNames_auau;
  std::vector<std::string> _zdcNames_auau;
  std::vector<std::string> _clockNames_pp;
  std::vector<std::string> _bbcntcNames_pp;
  std::vector<std::string> _rejectNames_pp;
  std::vector<std::string> _bbcNames_run5_cucu;
  std::vector<std::string> _bbcNames_run5_pp;
  std::vector<std::string> _bbcNames_run6_pp;
  std::vector<std::string> _bbcNames_run7_auau;
  std::vector<std::string> _bbcNames_run8_dau;
  std::vector<std::string> _bbcNames_run8_pp;

  // do NOT modify these ever:  for new runs, add new members with new names: see above/below
  std::string _minBiasName_dau;
  std::string _minBiasName_dau_zdc;
  // do NOT modify the previous 22 lines ever (again):  
  //  for new runs, add new members with new names these (e.g. _bbcNm_auau_run10) 
  // these 22 lines are for  run2 through run4 AuAu  (NOT run4 pp!!!)

  std::vector<std::string> _bbcNames_run9_pp500gev;
  std::vector<std::string> _bbcNames_run9_pp200gev;

  // do NOT modify these ever:  for new runs, add new members with new names 
  TrigRunLvl1 * _trigRunLvl1;
  IsMinBiasInfo * _lastMinBiasInfo;  
  TrigLvl1 * _trigLvl1;
  L2DecisionHelper * _l2DecisionHelper;

  bool trigMatchNames(const char * triggerNameInput, 
		      const char * triggerNameToMatch) const;
  

  // ******** new error handler features ********
  bool displayError(TrigRunLvl2 * trigRunL2) const;

  bool displayError(TrigRunLvl1 * trigRunL1) const;

  bool displayError(TrigLvl1 * trigL1) const;
};

#endif





