// L2DecisionHelper:
//    analysis object implementation:
//    author: jfrantz  
//    chp: use printf to write to stdout, cout is redirected by the
//         message system in 1008 and leads to lots of useless blabbering
//         for the monitoring shift crew
//

#include <L2DecisionHelper.h>
#include <Lvl2DecisionOutv1.h> //just for the Max enums
#include <PHIODataNode.h>
#include <Lvl2DecisionOutMicrov1.h>
#include <getClass.h>

#include <iostream>
using namespace std;
 
void L2DecisionHelper::initialize()
{
  _lvl2DecisionOut = NULL;
  _topNode = NULL;
  _microDstState = l2udstUndetermined;
  _dstMicroSave = NULL; 
  _lvl2RejectOn = -1; // means uninitialized  
}

L2DecisionHelper::L2DecisionHelper(PHCompositeNode * topNode)
{
  
  Lvl2DecisionOut * lvl2DecisionOut = findNode::getClass<Lvl2DecisionOut>(topNode,"L2Decision");
  TrigRunLvl2 * trigRunLvl2 = findNode::getClass<TrigRunLvl2>(topNode,"TrigRunLvl2");

  initializeFromComponents(lvl2DecisionOut, trigRunLvl2);
  
  _topNode = topNode;
}


void L2DecisionHelper::initializeFromComponents(Lvl2DecisionOut* trigL2, TrigRunLvl2* trigRunL2)
{
  initialize();
  _lvl2DecisionOut = trigL2;
  _trigRunLvl2 = trigRunL2;  
  
  if ( _trigRunLvl2 == NULL && displayError(_trigRunLvl2) )  
    {
      printf("ERROR: Unable to intialize L2DecisionHelper\n");
    }
  
  if (_lvl2DecisionOut == NULL && displayError(_lvl2DecisionOut) )
    {
      // Comment out for now, since this prints a message for every event
      // when reading from two topnodes, if there is no Lvl2 decision node
      /*
      printf("L2DecisionHelper:: Level2 decision event node not found.\n");
      printf("\tLevel2 may not have been turned on during this run.\n");
      printf("\tIf Level2 was on, only Run configuration information is available.\n");
      */
    }

  _lastTrigRunLvl2 = trigRunL2;
  _lastLvl2DecisionOut = trigL2;
  _isFirst = false;
}

TString L2DecisionHelper::getAlgorithmName(UINT ialg)
{
  TString nameString("ERROR ALGORITHM NAME NOT FOUND");

  if (_trigRunLvl2 != NULL) 
    {
    nameString = _trigRunLvl2->get_lvl2_trig_name(ialg);
    }
  else
    {  
      // this should be gotten from the RunbyRun object
      // but for year2 data it never changed
      if (ialg == 0) nameString = "L2EmcHighPtTileTrigger";
      else if (ialg == 1) nameString = "L2ChargedHighPtTrigger";
      else if (ialg == 2) nameString = "L2EmcHighPtTileTriggerRecut";
      else if (ialg == 3) nameString = "L2JPsiElectronTrigger";
      else if (ialg == 4) nameString = "L2JPsiElectronTriggerCentcut";
      else if (ialg == 5) nameString = "L2SingleElectronTriggerNoPC3Cut";
      else if (ialg == 6) nameString = "L2EmcHighPtTilePeriphTrigger";
      else if (ialg == 7) nameString = "L2MuDiMuonTrigger";
      else if (ialg == 8) nameString = "L2MuSingleMuonThetaCutTrigger";
      else if (ialg == 9) nameString = "L2MuSingleMuonPeriphTrigger";
      else if (ialg == 10) nameString = "L2SingleElectronTriggerRecut";
      else if (ialg == 11) nameString = "L2SingleElectronTriggerCentcut";
      else if (ialg == 12) nameString = "L2EMuTrigger";
      else if (ialg == 13) nameString = "L2PHIElectronTriggerCentcut";
      else if (ialg == 14) nameString = "L2EMuTriggerRecut";
      else if (ialg == 15) nameString = "L2ChargedHighPtTriggerRecut";
      else if (ialg == 16) nameString = "L2CoherentPeriphTrigger";
      else if (ialg == 17) nameString = "L2JPsiElectronTriggerReCut";
      else if (ialg == 18) nameString = "L2MuDiMuonThetaCutTrigger";
      else if (ialg == 19) nameString = "L2MuSingleMuonTrigger";
      else if (ialg == 20) nameString = "L2SingleElectronTrigger";
      else if (ialg == 21) nameString = "L2MuDiMuonPeriphTrigger";
      else if (ialg == 22) nameString = "L2ChargedHighPtTriggerEastNoTec";
      else if (ialg == 23) nameString = "L2ChargedHighPtTriggerEastNoTecRecut";
      else if (ialg == 24) nameString = "L2ChargedHighPtTriggerEastTec";
      else if (ialg == 25) nameString = "L2ChargedHighPtTriggerEastTecRecut";
      else if (ialg == 26) nameString = "L2ChargedHighPtTriggerWest";
      else if (ialg == 27) nameString = "L2ChargedHighPtTriggerWestRecut";      
    }

  return nameString;
}
 
// Lvl2Decision L2DecisionHelper::getFinalDecision()
// {
//   UINT finalBits;
//   if (_lvl2DecisionOut == NULL) finalBits = 0;
//   else finalBits = _lvl2DecisionOut->getFullDecision(); 
			      
//   Lvl2Decision final(finalBits);
//   return final;
// }

// these can be expensive on a microDST if _microDstState is not
// set to Yes
// Lvl2Decision L2DecisionHelper::getAlgorithmDecision(UINT ialg)
// {
//   Lvl2Decision retDec(0);

//   if (_lvl2DecisionOut == NULL) return retDec;
			  
//   Lvl2DecisionOut* decOut = _lvl2DecisionOut;  
//   if (_microDstState == l2udstYes)  // this will speed up the expensive
//     {                               // microDST getAlgDecision call
//                                     // see comments in .h file about _microDstState
//       if (_dstMicroSave == NULL) 
// 	{
// 	  Lvl2DecisionOutMicrov1 * udstPtr = 
// 	    dynamic_cast<Lvl2DecisionOutMicrov1*> (decOut);
// 	  _dstMicroSave = udstPtr->createDstObj();
// 	}
//       decOut = _dstMicroSave;
//     }
  
  
//   retDec = decOut->getAlgorithmDecision(ialg);
//   return retDec;
// }


// Lvl2Decision L2DecisionHelper::getAlgorithmDecision(char * algName) 
// {
//   for (UINT i = 0; i < Lvl2DecisionOutv1::MaxNumAlgorithms;  i++)
//     {
//       if ( getAlgorithmName(i).CompareTo(algName) == 0)
// 	return getAlgorithmDecision(i);
//     }
//   // if here must be unable to find algName
//   cout << "L2DecisionHelper:: Error! could not find alg Name" << endl;
//   Lvl2Decision nullDec(0);
//   return nullDec;

// }

bool L2DecisionHelper::IsLevel2RejectOn() {

  if (_lvl2RejectOn < 0)
    {
//       Lvl2Decision finalDecision = getFinalDecision();
//       // if finalDecision is empty the whole node probably is
//       if (finalDecision.isEmpty()) return false;
//       if (finalDecision.wasDisabled())
	{
	  _lvl2RejectOn = 0;
	  return false;
	}
//       else 
// 	{
// 	  _lvl2RejectOn = 1;
// 	  return true;
// 	}
    }
  else 
    {
      if (_lvl2RejectOn == 0) return false; 
      else return true;
    }
  return false;
}
 
bool L2DecisionHelper::IsLvl2MinBias()
{
  /*
    this method will return whether the lvl2 information
    (regardless of the level1 information which really is the
    most important determiner of minbias status).
    offline/packages/trigger/TriggerHelper.h should be used
    to make the true (full) determination of whether an event
    is minbias.
  */

  /*
    if lvl2 is not present assume lvl2 was not on (at all) 
    for this event.  Therefore the event must be minbias  
    since we got this far.
  */
  if (get_lvl2DecisionOut() == NULL) return true; 

  // by rights the above should also check
  // that "||  get_trigRunLvl2() == NULL)"  but
  // right now only l2decOut is used

  //check if any level2 FA was enforced or rejection was off.
//   Lvl2Decision finalDecision = getFinalDecision();
//   // if finalDecision is empty the whole node probably is
//   if (finalDecision.isEmpty()) return true;
//   if (finalDecision.wasDisabled() || finalDecision.wasForced())
    return true;

  // none of the level2 tests for minbias passed, return false
  return false; 

}

//=============================================================
//=============================================================

// ******** new error handler features ********

TrigRunLvl2 * L2DecisionHelper::_lastTrigRunLvl2 = NULL;
Lvl2DecisionOut * L2DecisionHelper::_lastLvl2DecisionOut = NULL;

// initialize verbosity to once-per-file messages as default
int L2DecisionHelper::_verbosity = 2;

// true only for first instantiation
bool L2DecisionHelper::_isFirst = true;

void L2DecisionHelper::set_verbosity(int verbosity)
{
  // verbosity level for error messages
  // 0 = no error messages,  1 = once per file with NO level1 errors (same as 2 for this class),
  // 2 = once per file with ALL level1 errors (DEFAULT),   3 = display all messages
  if(verbosity > -1 && verbosity < 4)
    {
      _verbosity = verbosity;
    }

  // let the once-per-file messages be the default
  else _verbosity = 2;
}

bool L2DecisionHelper::displayError(TrigRunLvl2 * trigRunL2)
{
  if(_verbosity == 0) return false;  // never display messages
  if(_verbosity == 3) return true; // always display messages

  if(_verbosity == 1 || _verbosity == 2)
    {
      // check if current pointer is same as last one
      if(trigRunL2 == _lastTrigRunLvl2 && _isFirst == false) return false;
    }

  // if they are not the same, display the messages
  return true;
}

bool L2DecisionHelper::displayError(Lvl2DecisionOut * trigL2)
{
  if(_verbosity == 0) return false; // never display messages
  if(_verbosity == 3) return true; // always display messages

  if(_verbosity == 1 || _verbosity == 2)
    {
      // check if current pointer is same as last one
      if(trigL2 == _lastLvl2DecisionOut && _isFirst == false) return false;
    }

  // if they are not the same, display the messages
  return true;
}



