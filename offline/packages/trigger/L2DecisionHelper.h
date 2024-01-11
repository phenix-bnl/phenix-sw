/*  ////////////////////////
// L2DecisionHelper:
//    analysis object:  to be used with dst and microDST storage 
//    objects.
//    Provides some nice utilities to make using
//    L2Decision objects easier.
//    
//    Also can be used as a wrapper for microDST and dst l2decision
//    objects.
//    
//    See www.phenix.bnl.gov/WWW/evb/lvl2/l2offline.html
//
//    Please refer to offline/packages/lvl2 for information about 
//    the class Lvl2DecisionOut.  ALSO PLEASE NOTE: when version
//    of Lvl2DecisionOutv1 -> v2 we may want to update this class's
//    implementation accordingly
//    
//    
//    author: jfrantz  
//
//////////////////////////
*/

#ifndef L2DecisionHelper_H
#define L2DecisionHelper_H

#include <Lvl2DecisionOut.h> 
#include <PHCompositeNode.h>
#include <phenixTypes.h>
#include <TString.h>
#include <TrigRunLvl2.h>


class L2DecisionHelper
{
///


 public:
	
  /* 
		 For just decoding the bits from a 
     UINT decision see example below
     The three different options are for convienience:  it will
     retrieve objects for you from the node tree or if you 
     have your own objects you can use them */
  L2DecisionHelper(PHCompositeNode * topNode);
	
	// direct constructor from input nodes
  L2DecisionHelper(Lvl2DecisionOut* trigL2, TrigRunLvl2* trigRunL2)
	{ initializeFromComponents(trigL2, trigRunL2); }
	
	// reinitialize internal nodes
  void initializeFromComponents(Lvl2DecisionOut*, TrigRunLvl2 *);

	
  //  L2DecisionHelper(DstContent* ezdstTopNode); removed dstcontent interface
  // --no longer supported--as with TrigHlpr. no more ezdst, use fun4all
  void initialize();

  // signifies whether this analysis context is being done with a 
  // microDST for sure as opposed to a dst
  enum MicroDstState  
  { l2udstNo = -1, l2udstUndetermined = 0, l2udstYes = 1};


  /*
    the following method will return whether the lvl2 information
    (regardless of the level1 information which really is the
    most important determiner of minbias status) is sufficient to
    make an event minbias.  It is meaningless without the level1 
    information but is provided for cases where a user determines
    the level1 minbias status independently.
    offline/packages/trigger/TriggerHelper.h should be used
    to make the true (full) determination of whether an event
    is minbias.
  */
  bool IsLvl2MinBias();


  /// Lvl2Decision example
/*   Lvl2Decision convertBitsToLvl2Decision(UINT decision)  */
/*     { */
/*       // see www.phenix.bnl.gov/evb/lvl2/l2offline.html  */
/*       // this is to provide an example of using the user  */
/*       // friendly Lvl2Decision class */
/*       // you should just directly create your own Lvl2Decision  */
/*       // variables */
/*       // in code (which will save typing) */
/*       Lvl2Decision aNewDecision(decision);   */
/*     return aNewDecision; */
/*     // just stuff the UINT decision into the  */
/*     // the Lvl2Decision's contructor.  */
/*     } */
  
  // perhaps later all methods on Lvl2DecisionOut could be implemented 
  // here but return a user-friendly Lvl2Decision instead

  TString getAlgorithmName(UINT ialg);

  /* these next two can be expensive on a microDST if 
     _microDstState is not set to Yes   */
/*   Lvl2Decision getAlgorithmDecision(char * algName);  */
/*   Lvl2Decision getAlgorithmDecision(UINT ialg); */
  
/*   Lvl2Decision getFinalDecision(); */

  bool IsLevel2RejectOn();

 private:

  Lvl2DecisionOut* _lvl2DecisionOut;
  PHCompositeNode* _topNode;
  TrigRunLvl2 * _trigRunLvl2;
  int _lvl2RejectOn;

  // these two variables are here because there are different kinds of versions
  // of Lvl2DecisionOut -- vX and MicrovX.  MicrovX is a "compressed" version
  // that requires more processing when certain methods are called.  Knowing something
  // about the two versions, these allow the user to tell L2DecHelper objects
  // that they know the (microDST) file they are using contains Lvl2DecisinOut objects 
  // of type MicrovX as opposed to vX, so that the this object will then create it's own
  // local vX version (stored in _dstMicroSave) and use that instead of the MicrovX version
  // so that the time spent in the uncompression is saved.  The user tells l2DecisionHelper this 
  // by setting doing   set_microDstState(l2udstYes)
  // However since this uncompression time is probably insigificant compared to the rest
  // of PHENIX processing, this should be considered overkill and could be removed anytime 
  // in the future.   Jfrantz 
  Lvl2DecisionOut* _dstMicroSave;
  MicroDstState _microDstState; 
///

 public:
  // getters/setters

  TrigRunLvl2* get_trigRunLvl2() {return _trigRunLvl2;}

  Lvl2DecisionOut* get_lvl2DecisionOut() {return _lvl2DecisionOut; }

  void set_lvl2DecisionOut(Lvl2DecisionOut* newDec) {_lvl2DecisionOut = newDec;}

  PHCompositeNode * get_topNode() {return _topNode;}
 
  void set_microDstState(MicroDstState val) { _microDstState = val;}
  // this will increase speed of getAlgorithm if using microDST

  virtual ~L2DecisionHelper() 
    {if (_dstMicroSave != NULL) delete _dstMicroSave;}
  

  // ******** new error handler features ********
 public:
  // pointers from the previous object
  static TrigRunLvl2 * _lastTrigRunLvl2;
  static Lvl2DecisionOut * _lastLvl2DecisionOut;
  static bool _isFirst;

  // verbosity level of error messages
  static int _verbosity;

  // verbosity level for error messages
  // 0 = no error messages,  1 = once per file with NO level1 errors (same as 2 for this class),
  // 2 = once per file with ALL level1 errors (DEFAULT),   3 = display all messages
  static void set_verbosity(int verbosity);
  static int get_verbosity() {return _verbosity;}

 private:
  bool displayError(TrigRunLvl2 * trigRunL2);
  bool displayError(Lvl2DecisionOut * trigL2);

};

#endif










