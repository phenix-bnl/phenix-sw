/* ////////////////////////
// TriggerHelper:
//    analysis object implementation:
//  general utility object providing  information spanning both lvl1 and lvl2   
//  such as whether an event is minbias 
//   
//    author: jfrantz and j.nagle
//
//    chp: use printf to write to stdout, cout is redirected by the
//         message system in 1008 and leads to lots of useless blabbering
//         for the monitoring shift crew
//////////////////////////
*/

#include "TriggerHelper.h"

#include <TRegexp.h>
#include <PHIODataNode.h>
#include <L2DecisionHelper.h>
#include <Lvl2DecisionOut.h>
#include <TrigLvl1.h>
#include <TrigRunLvl1.h>

#include <iostream>
using namespace std;

// Initialize our static members here
//
TrigRunLvl2 * TriggerHelper::_lastTrigRunLvl2 = 0;
TrigRunLvl1 * TriggerHelper::_lastTrigRunLvl1 = 0;
TrigLvl1 * TriggerHelper::_lastTrigLvl1 = 0;

// initialize verbosity to once-per-file messages with level1 errors as default
int TriggerHelper::_verbosity = 2;

// true only for first instantiation
bool TriggerHelper::_isFirst = true;


//! default constructor
TriggerHelper::TriggerHelper()
{
  initialize();
}

//! constructor from top node
TriggerHelper::TriggerHelper(PHCompositeNode *topNode)
{ 
  initialize();
  setNodes( topNode ); 
}

//! direct constructor from external nodes
TriggerHelper::TriggerHelper(
			     TrigLvl1 *trigL1,
			     TrigRunLvl1 *trigRunL1, 
			     Lvl2DecisionOut *trigL2, 
			     TrigRunLvl2 *trigRunL2)
{
  initialize();
  initializeFromComponents(trigL1, 
			   trigRunL1, 
			   trigL2, 
			   trigRunL2);
}


void TriggerHelper::initialize() 
{

  //variable initialization
  _trigRunLvl1 = 0; 
  _trigLvl1= 0;
  _l2DecisionHelper = 0; 
  _lastMinBiasInfo = &__info;

  // do NOT modify the following 16 lines ever (again):  
  //  for new runs, add new members with new names these (e.g. _bbcNm_auau_run10) 
  // these 16 lines are for  run2-run4 AuAu  (NOT run4 pp!!!)
  _TriggerDesc_auau.push_back( "BigPartition" );
  _TriggerDesc_pp.push_back(  "PPLL" );
  _TriggerDesc_pp.push_back(  "PPL" );
  _bbcNames_auau.push_back( "BBCLL1>=2" ); 
  _bbcNames_auau.push_back( "BBLL1" );     
  _bbcNames_auau.push_back( "BBLL1>=2" ); 
  _bbcNames_auau.push_back( "BBCLL1>=1" );
  _bbcNames_auau.push_back( "BBCLL1" );    //                                      
  _zdcNames_auau.push_back( "ZDCNS" );     // please never modify any of these
  _clockNames_pp.push_back( "Clock" );     //                                  
  _bbcntcNames_pp.push_back( "BBCLL1>=1||NTCw" );
  _bbcntcNames_pp.push_back( "BBCLL1>=1||NTCW" );   
  _bbcntcNames_pp.push_back( "BBCLL1>=1||NTC" );
  _rejectNames_pp.push_back( "ERT" );
  _rejectNames_pp.push_back( "MUID" );

  _minBiasName_dau = "BBCLL1>=1";
  _minBiasName_dau_zdc = "BBCLL1&ZDCNS";
  // do NOT modify the previous 16 lines ever (again):  
  //  for new runs, add new members with new names these  (e.g. _bbcNm_auau_run10) 
  // these 16 lines are for  run2-run4 AuAu  (NOT run4 pp!!!)

  // add for Run-5 cucu
  _bbcNames_run5_cucu.push_back("BBCLL1(>0 tubes)");
  // add for Run-5 pp
  _bbcNames_run5_pp.push_back( "BBCLL1(>0 tubes)");
  // add for Run-6 pp
  _bbcNames_run6_pp.push_back( "BBCLL1(>0 tubes)");
  // add for Run-7 AuAu
  _bbcNames_run7_auau.push_back("BBCLL1(>1 tubes)");
  // add for Run-8 dAu
  _bbcNames_run8_dau.push_back("BBCLL1(>0 tubes)");
  // add for Run-8 pp
  _bbcNames_run8_pp.push_back("BBCLL1(>0 tubes)");
  // add for Run-9 pp 500 GeV
  _bbcNames_run9_pp500gev.push_back("BBCLL1(noVertexCut)");
  // add for Run-9 pp 200 GeV
  _bbcNames_run9_pp200gev.push_back("BBCLL1(noVertexCut)");
  return;
}

//_________________________________________________
void TriggerHelper::dump_info(IsMinBiasInfo* /*info*/) const {

  if ( !_trigRunLvl1 && displayError(_trigRunLvl1) ) 
    {
      // TriggerHelper needs at the very least TrigRunLvl1
      // otherwise it is completely useless
      cout << "ERROR: Unable to run TriggerHelper::dump_info" << endl;  
      return;
    }

  printf("\n===============================================================================\n");
  printf("RUN LEVEL1 TRIGGER OBJECT INFORMATION (TriggerHelper::dump_info Method Output)\n");
  printf("===============================================================================\n\n");
  printf("  Trigger Description       = %s\n",_trigRunLvl1->get_lvl1_trigger_description());
  printf("  Trigger Version           = %u\n",_trigRunLvl1->get_lvl1_trigger_version());
  printf("  Partition Name            = %s\n",_trigRunLvl1->get_lvl1_partition_name());
  printf("  BBCLL1  Description       = %s\n",_trigRunLvl1->get_lvl1_bbcll1_description());
  printf("  BBCLL1  Version           = %u\n\n",_trigRunLvl1->get_lvl1_bbcll1_version());

  for (int i = 0; i < 32; i++)
    {
      printf("    Lvl1 Name               = %s\n",_trigRunLvl1->get_lvl1_trig_name(i));
      printf("      Index=%d Bit=%u",i,_trigRunLvl1->get_lvl1_trig_bit(i));
      printf(" Enable=%u",_trigRunLvl1->get_lvl1_trigger_enable(i));
      printf(" ScaleDown=%u\n",_trigRunLvl1->get_lvl1_trig_scale_down(i));
    }

  // perhaps it would be good to print the run number and which run type and what the
  // IsEventMinBias selection is set to (in this code)

  printf("\nThis run number = %d\n",_trigRunLvl1->get_run_number());
  // make sure for backward compatibility that this set of if - else
  // if statements are in cronological order
  if ( IsRun9_pp_200GeV()) 
    {
      printf("This is Run9_pp_200GeV\n");
      for (int i=0;i<_maxNum_bbcNames_run9_pp200gev;i++) {
	printf("  IsEventMinBias condition requires %s\n", _bbcNames_run9_pp200gev[i].c_str());
      }
    }
  else if ( IsRun9_pp_500GeV()) 
    {
      printf("This is Run9_pp_500GeV\n");
      for (int i=0;i<_maxNum_bbcNames_run9_pp500gev;i++) {
	printf("  IsEventMinBias condition requires %s\n", _bbcNames_run9_pp500gev[i].c_str());
      }
    }
  else if ( IsRun8_pp_200GeV()) 
    {
      printf("This is Run8_pp_200GeV\n");
      for (int i=0;i<_maxNum_bbcNames_run8_pp;i++) {
	printf("  IsEventMinBias condition requires %s\n", _bbcNames_run8_pp[i].c_str());
      }
    }
  else  if (IsRun8_dAu_200GeV()) 
    {
      printf("This is Run8_dAu_200GeV\n");
      for (int i=0;i<_maxNum_bbcNames_run8_dau;i++) {
	printf("  IsEventMinBias condition requires %s\n", _bbcNames_run8_dau[i].c_str());
      }
    }
  else if (IsRun7_AuAu_200GeV()) 
    {
      printf("This is Run7_AuAu_200GeV\n");
      for (int i=0;i<_maxNum_bbcNames_run7_auau;i++) {
	printf("  IsEventMinBias condition requires %s and (ZDCNS || ZDCLL1wide)\n", _bbcNames_run7_auau[i].c_str());
      }
    }
  else if (IsRun6_PP()) 
    {
      printf("This is Run6_pp_200GeV\n");
      for (int i=0;i<_maxNum_bbcNames_run6_pp;i++) {
	printf("  IsEventMinBias condition requires %s\n", _bbcNames_run6_pp[i].c_str());
      }
    }
  else if (IsRun5_PP()) 
      printf("This is Run5_pp_200GeV\n");
  else if (IsRun5_CuCu()) 
      printf("This is Run5_CuCu_200GeV\n");
  else if (IsRun4_PP()) 
      printf("This is Run4_pp_200GeV\n");
  else if (IsRun4_AuAu_63GeV())
      printf("This is Run4_AuAu_63eV\n");
  else if (IsRun4_AuAu_200GeV())
      printf("This is Run4_AuAu_200GeV\n");
  else if (IsRun3_dAu()||IsRun3_PP())
    printf("This is Run3_dAu_200GeV or Run3_pp_200GeV\n");
  else if (IsRun2_AuAu()) 
    printf("This is Run2_AuAu_200GeV\n");
  else if (IsRun2_PP()) 
      printf("This is Run2_pp_200GeV\n");
  
  printf("\n===============================================================================\n\n");

  return;

}

//_________________________________________________
void TriggerHelper::initializeFromComponents(TrigLvl1* trigL1, 
					     TrigRunLvl1 * trigRunL1, 
					     Lvl2DecisionOut * trigL2, 
					     TrigRunLvl2* trigRunL2)
{
  _trigLvl1 = trigL1;
  _trigRunLvl1 = trigRunL1;

  if ( !_trigRunLvl1 && displayError(_trigRunLvl1) ) 
    {
      // TriggerHelper needs at the very least TrigRunLvl1
      // otherwise it is completely useless
      cout << "ERROR: Unable to initialize TriggerHelper" << endl;  
      return;
    }

  if( !_trigLvl1 && displayError(_trigLvl1) ) 
    {
      // with no trigLvl1 node, TriggerHelper cannot access event information
      printf("TriggerHelper:: no lvl1 event node found.  For Level1, only Run\n");
      printf("\tTrigger Configuration information will be available\n");
    }
      

  if( !trigRunL2 && displayError(trigRunL2) )
    {
      printf("TriggerHelper ::  Lvl2 Run Information not available\n");
      return;
    }

  //L2DecisionHelper::set_verbosity(_verbosity);
  if ( !_l2DecisionHelper )
    {
      _l2DecisionHelper = new L2DecisionHelper(trigL2, trigRunL2);
    }
  else
    {
      _l2DecisionHelper->initializeFromComponents(trigL2, trigRunL2);
    }

  _lastTrigRunLvl2 = trigRunL2;
  _lastTrigRunLvl1 = _trigRunLvl1;
  _lastTrigLvl1 = _trigLvl1;
  _isFirst = false;
  return;
}

//_________________________________________________
void TriggerHelper::setNodes(PHCompositeNode * topNode)
{

  PHTypedNodeIterator<TrigLvl1> trig_uditer(topNode);
  PHIODataNode<TrigLvl1> * TrigLvl1Node = trig_uditer.find("TrigLvl1");
  if (TrigLvl1Node)
    {
      _trigLvl1 = TrigLvl1Node->getData();
    }
  
  PHTypedNodeIterator<TrigRunLvl1> run_uditer(topNode);
  PHIODataNode<TrigRunLvl1> * TrigRunLvl1Node = run_uditer.find("TrigRunLvl1");
  if (TrigRunLvl1Node)
    {
      _trigRunLvl1 = TrigRunLvl1Node->getData();
    }
  
  Lvl2DecisionOut * trigLvl2 = 0;
  TrigRunLvl2 * trigRunLvl2 = 0;

  PHTypedNodeIterator<Lvl2DecisionOut> trig2_uditer(topNode);
  PHIODataNode<Lvl2DecisionOut> * TrigLvl2Node = trig2_uditer.find("L2Decision");
  if (TrigLvl2Node) 
    {
      trigLvl2 = TrigLvl2Node->getData();
    }
  
  PHTypedNodeIterator<TrigRunLvl2> run_2uditer(topNode);
  PHIODataNode<TrigRunLvl2> * TrigRunLvl2Node = run_2uditer.find("TrigRunLvl2");
  if (TrigRunLvl2Node)
    {
      trigRunLvl2 = TrigRunLvl2Node->getData();
    }
  
    initializeFromComponents(_trigLvl1, _trigRunLvl1, trigLvl2, trigRunLvl2);
  

}

//_________________________________________________
TriggerHelper::~TriggerHelper()
{
  if( _l2DecisionHelper ) delete _l2DecisionHelper;
}


//_________________________________________________
bool TriggerHelper::IsEventMinBias(IsMinBiasInfo* info) const
{

  if( !info ) info = _lastMinBiasInfo;  bool answer = false;   // initialize answer to IsEventMinBias to false
  // check that we have all the neccessary objects
  if ( !( _trigRunLvl1 && _trigLvl1 ) && ( displayError(_trigRunLvl1) || displayError(_trigLvl1) ) )
    {
    std::cout << "TriggerHelper Error::  TrigRunLvl1 and/or TrigLvl1 Objects not found" << std::endl;
    return false; // objects not found
  }

  if(checkDataNoFill() && ( displayError(_trigRunLvl1) && displayError(_trigLvl1) ) ) {
    std::cout << "ERROR:  TriggerHelper: Data base information not filled into TrigRun Objects" << std::endl;
    std::cout << "        TriggerHelper: All minimum bias calls will return false." << std::endl;
  }

  // make sure for backward compatibility that this set of if - else
  // if statements are in cronological order
  if ( IsRun9_pp_200GeV()) 
    {
      bool fired = false;
      if ( trigScaled(_bbcNames_run9_pp200gev[0].c_str()) ) 
	fired = true;
      return fired;      
    }
  else if ( IsRun9_pp_500GeV()) 
    {
      bool fired = false;
      if ( trigScaled(_bbcNames_run9_pp500gev[0].c_str()) ) 
	fired = true;
      return fired;      
    }
  else if ( IsRun8_pp_200GeV()) 
    {
      bool fired = false;
      if ( trigScaled(_bbcNames_run8_pp[0].c_str()) ) 
	fired = true;
      return fired;      
    }
  else  if (IsRun8_dAu_200GeV()) 
    {
      bool fired = false;
      if ( trigScaled(_bbcNames_run8_dau[0].c_str()) ) 
	fired = true;
      return fired;      
    }

  else if (IsRun7_AuAu_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run7_auau[0].c_str()) && 
	  (trigLive("ZDCNS") || trigLive("ZDCLL1wide"))) 
	fired = true;
      return fired;      
    }

  else if (IsRun6_PP()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run6_pp[0].c_str() )) fired = true;
      return fired;      
    }

  else if (IsRun5_PP()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run5_pp[0].c_str() )) fired = true;
      return fired;      
    }

  else if (IsRun5_CuCu()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run5_cucu[0].c_str() )) fired = true;
      return fired;
    }
  else if (IsRun4_PP()) 
    {
      
      return (trigScaled("BBCLL1"));
      
    }
  else if (IsRun4_AuAu_63GeV())
    { 
      bool fired = false;
      if(trigScaled( _bbcNames_auau[4].c_str() )) fired = true;
      return fired; 
    }
  else if (IsRun4_AuAu_200GeV())
    {
      // if scaled is set, then live should always be set
      bool fired = false;
      if (trigScaled(_bbcNames_auau[4].c_str()) && 
	  (trigLive("ZDCNS") || trigLive("ZDCLL1wide"))) 
	fired = true;
      return fired;       
    }
  else if (IsRun3_dAu()||IsRun3_PP())
    {
      // if scaled is set, then live should always be set
      return trigScaled(_minBiasName_dau.c_str());



    }
  else if (IsRun2_AuAu()) 
    {
    
      // Currently we define MinBias for Au-Au as firing both BBCLL1>=2 and ZDCNS
      // First we must determine if it is a Au-Au run or Proton-Proton run
      // In Run 2 all Au-Au Level-1 trigger configurations are "BigPartition"
      // qand all p-p configurations are "PPLL"
      
      // Live bit will be set for all triggers that fired.
      // But event may not have been taken for that trigger
      // Scaled bit set means it was. (0 scaledown will always fire scaled)
      // However we don't care.  If the live bit for a trigger that 
      // had one of the various names we gave the minbias trigger
      // it will be considered minbias, from the level1 standpoint
      // We must still consider what was done at level2.
      
      // Find fired level-1 triggers - first check for BBCLL1
      for (UINT i=0;i<maxLvl1Trigs;i++)
			if (_trigLvl1->get_lvl1_triglive_bit(i) == true) { 

	  		TString firedString = _trigRunLvl1->get_lvl1_trig_name_bybit(i);
	  		// bad form for hard coding which names must contain versus exactly match
    		// also note that CompareTo returns an integer, while Contains returns a boolean
				if (firedString.Contains(_bbcNames_auau[0].c_str())) answer = true;
					
				// j = 3 was the max for run2: (do not look at e.g. _bbcNames[3].  That's for run4
				for (int j = 1; j < 3; j++) 
	    	if (firedString.CompareTo(_bbcNames_auau[j].c_str()) == 0) answer = true;
      }     
			 
      if (answer == true) 
			{
				  
	// then also check for ZDCNS trigger
	bool zdcanswer = false;
	for (UINT i=0;i<maxLvl1Trigs;i++) {
	  if (_trigLvl1->get_lvl1_triglive_bit(i) == true) { 
	    TString firedString = _trigRunLvl1->get_lvl1_trig_name_bybit(i);
	    // bad form for hard coding which names must contain versus exactly match
	    // also note that CompareTo returns an integer, while Contains returns a boolean
	    for (int j = 0; j < _maxNum_zdcNames_auau; j++) {
	      if (firedString.CompareTo(_zdcNames_auau[j].c_str()) == 0) zdcanswer = true;
	    }
	  }
	}
	if (zdcanswer == false) answer = false;
	if (zdcanswer == true)  answer = true;
      }
      
      if (answer == false) return false;   // no minimum bias level-1 trigger
      
      // if there is a level1 min bias trigger, then check for lvl2 information
      if( !_l2DecisionHelper ) return true;  // lvl2 was not run
      return _l2DecisionHelper->IsLvl2MinBias();

      


    }
  else if (IsRun2_PP()) 
    {
    // - Live bit will be set for all triggers that fired,
    //     but event may not have been taken for that trigger
    // - Scaled bit set means it was. (0 scaledown will always fire -
    //     however there was a bug/feature where if scaledown=0, then
    //     only the live trigger bit counts.
    // - Thus check the scaledown first, then check either the scale bit
    //     or the live bit appropriately to determine if min.bias.
    // - There was no level2 rejection during any proton-proton running (Run 2)

    // just check for BBCLL1>=1 || NTCw = current definition of minimum bias

    for (UINT i=0;i<maxLvl1Trigs;i++) {
      // check for name match to desired selection list
      TString firedString = _trigRunLvl1->get_lvl1_trig_name_bybit(i);
      for (int j=0; j<_maxNum_bbcntcNames_pp; j++) {
        if (firedString.Contains(_bbcntcNames_pp[j].c_str()) == true) {	  

	  bool temp_answer = true;
	  // check if the name contains either ERT or MUID in combinations (ie. not minbias)
	  // I checked PPLL versions 44-48 and one can just check against the strings "ERT" and "MUID"
	  for (int k=0; k<_maxNum_rejectNames_pp; k++) {
	    if (firedString.Contains(_rejectNames_pp[k].c_str()) == true) temp_answer = false;
	  }

	  if (temp_answer) {
            // if either (scaledown=0 and live bit set) or (scaledown>0 and scale bit set)
	    if ((_trigRunLvl1->get_lvl1_trig_scale_down_bybit(i) == 0 &&
	         _trigLvl1->get_lvl1_triglive_bit(i) == true)
	        ||
	        (_trigRunLvl1->get_lvl1_trig_scale_down_bybit(i) >  0 &&
	         _trigLvl1->get_lvl1_trigscaled_bit(i) == true)) {

	      return true;  // found level-1 min.bias, no bad string, and taken for this reason
	    }
	  }
	}
      }
    } // end of loop over all 32 level1 triggers
    
    return false;

    }
  else if( displayError(_trigRunLvl1) || displayError(_trigLvl1) )
    {
      // if somehow neither Proton-Proton or Au-Au
      std::cout << "TriggerHelper::Error -- could not locate run period"  << std::endl;
    }
  
  return false;
  
}

//_________________________________________________
bool TriggerHelper::IsEventMinBias_noZDCrequired(IsMinBiasInfo* info) {

  // this method only has meaning for the Au-Au running and removes the ZDC requirement

  if ( !info ) info = _lastMinBiasInfo;
  bool answer = false;   // initialize answer to false
  // check that we have all the neccessary objects
  if ( !( _trigRunLvl1  && _trigLvl1 ) && ( displayError(_trigRunLvl1) || displayError(_trigLvl1) ))
    {
      std::cout << "TriggerHelper Error::  TrigRunLvl1 and/or TrigLvl1 Objects not found" << std::endl;
      return false; // objects not found
    }
  
  if(checkDataNoFill() && ( displayError(_trigRunLvl1) && displayError(_trigLvl1) ) ) {
    std::cout << "ERROR:  TriggerHelper: Data base information not filled into TrigRun Objects" << std::endl;
    std::cout << "        TriggerHelper: All minimum bias calls will return false." << std::endl;
  }
  

  if (IsRun9_pp_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run9_pp200gev[0].c_str()))
	fired = true;
      return fired;      
    }

  else if (IsRun9_pp_500GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run9_pp500gev[0].c_str()))
	fired = true;
      return fired;      
    }

  else if (IsRun8_pp_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run8_pp[0].c_str()))
	fired = true;
      return fired;      
    }

  else if (IsRun8_dAu_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run8_dau[0].c_str()))
	fired = true;
      return fired;      
    }

  else if (IsRun7_AuAu_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run7_auau[0].c_str()))
	fired = true;
      return fired;      
    }

  else if (IsRun6_PP()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run6_pp[0].c_str() )) fired = true;
      return fired;      
    }

  else if (IsRun5_PP()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run5_pp[0].c_str() )) fired = true;
      return fired;
    }
  else if (IsRun5_CuCu()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run5_cucu[0].c_str() )) fired = true;
      return fired;
    }
  else if (IsRun4_PP()) 
    {

      return trigScaled("BBCLL1");

    }
  else if (IsRun4_AuAu_63GeV() || IsRun4_AuAu_200GeV())
    {
      return (trigScaled(_bbcNames_auau[4].c_str()) ||
	      trigScaled(_bbcNames_auau[3].c_str())    );
      
    }
  else if (IsRun3_dAu()||IsRun3_PP())
    {
      // if scaled is set, then live should always be set
      return trigScaled(_minBiasName_dau.c_str());



    }
  else if (IsRun2_AuAu()) 
    {
    
    // Find fired level-1 triggers - first check for BBCLL1
    for (UINT i=0;i<maxLvl1Trigs;i++) {
      if (_trigLvl1->get_lvl1_triglive_bit(i) == true) { 
	  TString firedString = _trigRunLvl1->get_lvl1_trig_name_bybit(i);
          if (firedString.Contains(_bbcNames_auau[0].c_str())) answer = true;
          for (int j = 1; j < 3; j++) {
	    // j = 3 was the max for run2: (do not look at e.g. _bbcNames[3].  That's for run4
  	    if (firedString.CompareTo(_bbcNames_auau[j].c_str()) == 0) answer = true;
	  }
      }
    }  
    
    if (answer == false) return false;   // no minimum bias level-1 trigger
  
    // if there is a level1 min bias trigger, then check for lvl2 information
    if( !_l2DecisionHelper ) return true;  // lvl2 was not run
    return _l2DecisionHelper->IsLvl2MinBias();
    }

  return false;

}

//_________________________________________________
bool TriggerHelper::IsEventMinBias_requireZDC(IsMinBiasInfo* /*info*/) {

  if (IsRun9_pp_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run9_pp200gev[0].c_str()) &&  trigLive("ZDCLL1wide") )
	fired = true;
      return fired;      
    }
  else if (IsRun9_pp_500GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run9_pp500gev[0].c_str()) &&  trigLive("ZDCLL1wide") )
	fired = true;
      return fired;      
    }

  else if (IsRun8_pp_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run8_pp[0].c_str()) &&  trigLive("ZDCNS") )
	fired = true;
      return fired;      
    }

  else if (IsRun8_dAu_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run8_dau[0].c_str()) && trigLive("ZDCNS") )
	fired = true;
      return fired;      
    }

  else if (IsRun7_AuAu_200GeV()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_auau[4].c_str()) && 
	  (trigLive("ZDCNS") || trigLive("ZDCLL1wide"))) 
	fired = true;
      return fired;      
    }

  else if (IsRun6_PP()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run6_pp[0].c_str() )  && trigLive("ZDCNS")) fired = true;
      return fired;      
    }

  else if (IsRun5_PP()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run5_pp[0].c_str() ) && trigLive("ZDCNS")) fired = true;
      return fired;

    }
  else if (IsRun5_CuCu()) 
    {
      bool fired = false;
      if (trigScaled(_bbcNames_run5_cucu[0].c_str() ) && trigLive("ZDCNS")) fired = true;
      return fired;

    }
  else if (IsRun4_PP())
    {

      return (trigScaled("BBCLL1") && trigLive("ZDCNS"));
      
    }
  else if (IsRun3_dAu()) 
    {
      // if scaled is set, then live should always be set
      return trigScaled(_minBiasName_dau_zdc.c_str());
    }  
  else if (IsRun2_AuAu()) 
    {
      return IsEventMinBias();
    }
    
  // else not implemented for all run periods
  cout << PHWHERE << " isEventMinBias_requireZDC cannot be used for the current run period: returning false" << endl;
  return false;
  
}

//_________________________________________________
bool TriggerHelper::IsEventClockMinBias(IsMinBiasInfo* info) {

  // This method only has meaning for run 2 proton-proton data

  if( !info ) info = _lastMinBiasInfo;
  // check that we have all the neccessary objects

  if(checkDataNoFill() && ( displayError(_trigRunLvl1) && displayError(_trigLvl1) ) ) {
    std::cout << "ERROR:  TriggerHelper: Data base information not filled into TrigRun Objects" << std::endl;
    std::cout << "        TriggerHelper: All minimum bias calls will return false." << std::endl;
  }

  if ( !( _trigRunLvl1 && _trigLvl1 ) && ( displayError(_trigRunLvl1) || displayError(_trigLvl1) ) )
    {
      std::cout << "TriggerHelper Error::  TrigRunLvl1 and/or TrigLvl1 Objects not found" << std::endl;
      return false; // objects not found
    }

  if (IsRun2_PP() || IsRun3_dAu()) {

    for (UINT i=0;i<maxLvl1Trigs;i++) {
      TString firedString = _trigRunLvl1->get_lvl1_trig_name_bybit(i);
      for (int j=0; j<_maxNum_clockNames_pp; j++) {
        if (firedString.Contains(_clockNames_pp[j].c_str()) == true) {	  
          // if either (scaledown=0 and live bit set) or (scaledown>0 and scale bit set)
	  if ((_trigRunLvl1->get_lvl1_trig_scale_down_bybit(i) == 0 &&
	       _trigLvl1->get_lvl1_triglive_bit(i) == true)
	      ||
	      (_trigRunLvl1->get_lvl1_trig_scale_down_bybit(i) >  0 &&
	       _trigLvl1->get_lvl1_trigscaled_bit(i) == true)) {
	    // then event was taken for this reason
	    return true;
	  }
	}
      }
    } // end of loop over all 32 level-1 triggers

    // if it got this far, then must be false
    return false;

  }

  // if event is anything but run 2 proton-proton return false
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun9_pp_200GeV() const
{
  
  if (!_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun9_pp_200GeV:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  // as of this date 4/15/2009 we do not know the last run number (j.nagle)
  if (_trigRunLvl1->get_run_number() > 280243 && 
      _trigRunLvl1->get_run_number() < 291581)
    return true;

  return false;

}


//_________________________________________________
bool TriggerHelper::IsRun9_pp_500GeV() const
{
  
  if (!_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun9_pp_500GeV:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() >= 275899 &&
      _trigRunLvl1->get_run_number() <= 280242) 
    return true;

  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun8_pp_200GeV() const
{
  
  if (!_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun8_pp_200GeV:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() >= 253701 &&
      _trigRunLvl1->get_run_number() <= 275898 ) 
    return true;

  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun8_dAu_200GeV() const
{
  
  if (!_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun8_dAu_200GeV:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() >= 245796 &&
      _trigRunLvl1->get_run_number() <= 253701) 
    return true;

  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun7_AuAu_200GeV() const
{
  
  if (!_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun7_AuAu_200GeV:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if ( _trigRunLvl1->get_run_number() >= 227016 && 
       _trigRunLvl1->get_run_number() <= 240121 ) 
    return true;

  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun6_PP() const
{
  
  if (!_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun6_PP:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() >= 189579 && _trigRunLvl1->get_run_number() <= 206495) return true;
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun5_PP() const
{
  
  if (!_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun5_PP:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  // j.nagle - this run is still in progress
  if (_trigRunLvl1->get_run_number() >= 163682 && _trigRunLvl1->get_run_number() < 189579) return true;
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun5_CuCu() const
{

  if( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun5_CuCu:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  // j.nagle - 05-17-2005 - note that same minimum bias definition for CuCu 200 GeV, 62 GeV and injection running
  if (_trigRunLvl1->get_run_number() < 163682 && _trigRunLvl1->get_run_number() > 146999) 
    {
      return true;
    } 
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun3_dAu() const
{
  
  if( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun3_dAu:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() > 50000 && _trigRunLvl1->get_run_number() <=83059 ) return true;
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun3_PP() const
{
  
  if( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun3_PP:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() >=83478 && _trigRunLvl1->get_run_number() <=92446 ) return true;
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun4_AuAu_63GeV() const
{
  
  if ( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun3_dAu:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() >= 122304 &&_trigRunLvl1->get_run_number() <=123564) return true;
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun4_AuAu_200GeV() const
{
  
  if ( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun3_dAu:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() >= 105287 &&_trigRunLvl1->get_run_number() <=122223) return true;
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun4_PP() const
{
  
  if ( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun4_PP:: no trigRunLvl1 Object" << std::endl;
    return false;
  }  

  if (_trigRunLvl1->get_run_number() >= 124000 && _trigRunLvl1->get_run_number() <= 130553) return true;
  return false;

}

//_________________________________________________
bool TriggerHelper::IsRun2_AuAu() const 
	{
  // if Level-1 Trigger Description contains given TString then Au-Au
  if ( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRunAuAu - no TrigRunLvl1 Object found." << std::endl;
    return false;
  }
  TString triggerDesc = _trigRunLvl1->get_lvl1_trigger_description();
  for (int j=0; j < _maxNum_TriggerDesc_auau; j++) {
    if (triggerDesc.Contains(_TriggerDesc_auau[j].c_str())) return true;
  }
  return false;
}

//_________________________________________________
bool TriggerHelper::IsRun2_PP() const
{
  // if Level-1 Trigger Description contains given TString then proton-proton
  if( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: IsRun2_PP - no TrigRunLvl1 Object found." << std::endl;
    return false;
  }
  TString triggerDesc = _trigRunLvl1->get_lvl1_trigger_description();
  for (int j=0; j < _maxNum_TriggerDesc_pp; j++) {
    if (triggerDesc.Contains(_TriggerDesc_pp[j].c_str())) return true;
  }
  return false;
}


//_________________________________________________
bool TriggerHelper::checkDataNoFill() const
{
  if ( !_trigRunLvl1 && displayError(_trigRunLvl1) ) {
    std::cout << "ERROR: checkDataNoFill - no TrigRunLvl1 Object found." << std::endl;
    return true;
  }
  TString triggerDesc = _trigRunLvl1->get_lvl1_trigger_description();
  if (triggerDesc.Length()==0) return true;
  return false;
}


//_________________________________________________
bool TriggerHelper::IsLevel2RejectOn() {
  if( !_l2DecisionHelper) return false;  // if no lvl2 run then no reject on
  return _l2DecisionHelper->IsLevel2RejectOn();  


}


//_________________________________________________
bool TriggerHelper::trigMatchNames(const char * triggerNameInput, const char * triggerNameToMatch) const
{
  if (!strcmp(triggerNameInput,triggerNameToMatch))
    {
      return true;
    }
  return false;
}

//_________________________________________________
bool TriggerHelper::returnL1RawScaledLive(const char * TriggerName, RawScaledLive rsl) const
{
  // check that we have all the neccessary objects
  if ( !( _trigRunLvl1 && _trigLvl1 ) && ( displayError(_trigRunLvl1) || displayError(_trigLvl1) ) )
  {
    std::cout << "TriggerHelper Error::  TrigRunLvl1 and/or TrigLvl1 Objects not found" << std::endl;
    return false; // objects not found
  }

  
  // pointer to function
  bool (TrigLvl1::*trigfunction)(const unsigned int) const = 0;
  if (rsl == l1Raw) trigfunction = &TrigLvl1::get_lvl1_trigraw_bit;
  if (rsl == l1Scaled) trigfunction = &TrigLvl1::get_lvl1_trigscaled_bit;
  if (rsl == l1Live) trigfunction = &TrigLvl1::get_lvl1_triglive_bit;

  for (UINT i=0;i<maxLvl1Trigs;i++) 
    {
      if ((_trigLvl1->*trigfunction)(i) == true) 
	{ 
	  if (trigMatchNames((char *) TriggerName,  
			    (char *) _trigRunLvl1->get_lvl1_trig_name_bybit(i)))
	    return true;
	}// if      
    } //for
  return false;

}




//_________________________________________________
int TriggerHelper::getLevel1TriggerProperty(const char * TriggerName, L1What what)
{

  // check that we have all the neccessary objects
  if ( !_trigRunLvl1 && displayError(_trigRunLvl1) )
    {
      std::cout << "TriggerHelper Error::  TrigRunLvl1 Object not found" << std::endl;
      return -1; // objects not found
    }


  // pointer to member function
  unsigned int (TrigRunLvl1::*trigfunction)(const unsigned int) const;
  //    = &TrigRunLvl1::get_lvl1_trig_scale_down;

  if (what == l1BitMask)
    {
      trigfunction = &TrigRunLvl1::get_lvl1_trigger_enable;
    }
  else if (what == l1BitNumber)
    {
      trigfunction = &TrigRunLvl1::get_lvl1_trig_bit;
    }
  else if (what == l1Enabled )
    {
      trigfunction = &TrigRunLvl1::get_lvl1_trigger_enable;
    }
  else if (what == l1Scaledown)
    {
      trigfunction = &TrigRunLvl1::get_lvl1_trig_scale_down;
    }
  else
    {
      return -1;
    }

  for (UINT i = 0; i < maxLvl1Trigs; i++)
    {
      if ( trigMatchNames( TriggerName,
                           _trigRunLvl1->get_lvl1_trig_name(i)))
        {
          return (_trigRunLvl1->*trigfunction)(i);
        }
    }

  return -1;

}

//_________________________________________________
bool TriggerHelper::isLevel1TriggerEnabled(std::string TriggerName) 
{
  if (getLevel1TriggerProperty(TriggerName.c_str(), l1Enabled) != 0) return true;
  return false;
}

//=============================================================
//=============================================================

// ******** new error handler features ********
//_________________________________________________
void TriggerHelper::set_verbosity(int verbosity)
{
  // verbosity level for error messages
  // 0 = no error messages,  1 = once per file with no lvl1 errors,
  // 2 = once per file with all lvl1 errors (DEFAULT),   3 = display all messages
  if(verbosity > -1 && verbosity < 4)
    {
      _verbosity = verbosity;
      L2DecisionHelper::set_verbosity(verbosity);
    }

  // let the once-per-file messages with level1 errors be the default
  else _verbosity = 2;
}

//_________________________________________________
bool TriggerHelper::displayError(TrigRunLvl2 * trigRunL2) const
{
  if(_verbosity == 0) return false;  // never display messages
  if(_verbosity == 3) return true; // always display messages

  if(_verbosity == 1 || _verbosity == 2)
    {
      // check if current pointer is same as last one
      if(trigRunL2 == _lastTrigRunLvl2 && _isFirst == false)  return false;
    }

  // if they are not the same, display the messages
  return true;
}

//_________________________________________________
bool TriggerHelper::displayError(TrigRunLvl1 * trigRunL1) const
{
  if(_verbosity == 0) return false; // never display messages
  if(_verbosity == 2) return true; // typically we don't want to shut off level 1 errors
  if(_verbosity == 3) return true; // always display messages

  if(_verbosity == 1)
    {
      // check if current pointer is same as last one
      if(trigRunL1 == _lastTrigRunLvl1 && _isFirst == false) return false;
    }

  // if they are not the same, display the messages
  return true;
}

//_________________________________________________
bool TriggerHelper::displayError(TrigLvl1 * trigL1) const
{
  if(_verbosity == 0) return false; // never display messages
  if(_verbosity == 2) return true; // typically we don't want to shut off level 1 errors
  if(_verbosity == 3) return true; // always display messages

  if(_verbosity == 1)
    {
      // check if current pointer is same as last one
      if(trigL1 == _lastTrigLvl1 && _isFirst == false) return false;
    }

  // if they are not the same, display the messages
  return true;
}

/*
Notes:  there were some notes here from Run2 about exploring Steve Adlers 
mySql database that was used for Run2 work.  This can be accessed by looking at any
revisions previous to 03/29/2006.  
*/
