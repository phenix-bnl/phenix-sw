// simple example how to use ezdst
// This example prints out the Bbc/Zdc identify() and the list of vertices
// from VtxOut if you read in a Dst or the time of flight for the cgl tracks
// if you read in a micro dst and the identify() function of the micro dst 
// classes.
//
// Sorry about the static_casts<> but at some place one has to declare with what kind
// of object one deals.
//
// Note:
// You need only the include files which contain the classes you want to look 
// at.
// The strings which identify the class in getClass are the node names (branch
// names) on the dst/microdst

#include <strstream>
#include <iostream.h>
#include "anadst_trighelp.h"

#include "ezdst.h"

#include "ZdcOut.h"
#include "BbcOut.h"
#include "VtxOut.h"
#include "Lvl2DecisionOut.h"

#include "TrigLvl1.h"
#include "TrigRunLvl1.h"
#include "TrigRunLvl2.h"
#include "TrigRunLvl2v2.h"
#include "TrigRunLvl1v1.h"

#include "TriggerHelper.h"
#include "TriggerUtilities.h"
#include "L2DecisionHelper.h"
#include "Lvl2Decision.h"
#include "Lvl2Out.h"

static int numFiles = 0;
static int numEvents = 0;
static int numMinBias = 0;
static int numErtGam2 = 0;
static int numZdcns = 0; 

int anainit()
{
  numFiles++;
  numEvents = 0;
  numMinBias = 0;
  numErtGam2 = 0;
  numZdcns = 0; 

  return 0;
}

int anaexit()
{
  cout << "___thtest___ number of total events " << numEvents << endl;
  cout << "___thtest___ number of MinBias events " << numMinBias << endl;
  cout << "___thtest___ number of 'ERT_Gamma2' live events " << numErtGam2 << endl;
  cout << "___thtest___ number of 'ZDCNS' scaled events " << numZdcns << endl;

  return 0;
}

int process_event (DstContent *dst)
{
  numEvents++;
  
  TriggerHelper * myTH;

  myTH = new TriggerHelper(dst);

  //cout << "minBias: " <<  myTH.IsEventMinBias();
  
  //    cout << "before calling myTH.IsEventMinBias()" << endl;
  if(myTH->IsEventMinBias()) 
    {
      numMinBias++;
      //	cout << " this event is min bias" << endl;
    }
  
  //    cout << "minBiasZDC :" <<  myTH.IsEventMinBias_requireZDC() << endl;
  //    //  cout << "name " << mDHelper.getAlgorithmName(2) << myDec.wasDisabled();
  
  
  //  cout << "ERT Trigger? " << myTH.didLevel1TriggerFire("ERT_2x2") << endl;
  if (myTH->trigLive("ERT_Gamma2")) numErtGam2++;
  if (myTH->trigScaled("ZDCNS")) numZdcns++;
  

    ///////////////////////////////////// other test code: (different constructors, e.g.) 
  //TriggerHelper myTH(dst->get_topNode());
  //
  

  //  L2DecisionHelper * mDHelper = myTH.get_l2DecisionHelper(); 
  // L2DecisionHelper * mDHelper = new l2DecisionHelper(dst); 
  // L2DecisionHelper * mDHelper = new l2DecisionHelper(dst->get_topNode()); 
  //  L2DecisionHelper * mDHelper = new L2DecisionHelper(l2dec,runlevel2); 


  /*
  cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
  cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;


  cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
  cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
  */

  

//    if (level1) {
//      // if (init_done == 0) 
//      std::cout << "EVENT LEVEL1 OBJECT" << std::endl;
//      std::cout << "Trigger raw    = 0x" << hex << level1->get_lvl1_trigraw() << std::endl;
//      std::cout << "Trigger live   = 0x" << hex << level1->get_lvl1_triglive() << std::endl;
//      std::cout << "Trigger scaled = 0x" << hex << level1->get_lvl1_trigscaled() << std::endl;
//    }
 

 //   cout << dec;

//      if (runlevel1) {

//        runlevel1->dump_info(runlevel1);

//      }

  

  //  cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
  //cout << "%%% Level2 Info: " << endl;
  //cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;


//     if (myTH.trigLive("BBCLL1>=2(300-450Hz)") == false)  return 0;
    
//     Lvl2Decision fd = mDHelper->getFinalDecision();
  
//     cout << "Final Decision : disabled? " <<  fd.wasDisabled() << "\t acc: \t" << fd.wasAccepted() << "\t algacc: \t" << fd.wasAlgorithmAccepted() << "\t fa?:" << 
//       fd.wasForced() << " pres" << fd.wasPrescaled() << endl;
    
//     Lvl2Decision ld(0);


//      //    for (int lvl1bit=0; lvl2bit<32; lvl2bit++) {
//      for (int lvl2bit=0; lvl2bit<32; lvl2bit++) {
      
//        //if (runlevel2->get_lvl2_lvl1_prescale(lvl2bit, lvl1bit) != 0)
      
//        TString mstr = mDHelper->getAlgorithmName(lvl2bit);
//        if (mstr.Length() < 1) continue;
      
//        strstream mstr;
//        mstr << mstr.Data() << ends;
//        ld = mDHelper->getAlgorithmDecision(msstr.str());
//        if (ld.wasPrescaled())
//  	cout << "\tAlgor " << mstr.Data() << hex << ld.getValue() << dec << "\t acc: \t" << ld.wasAccepted() << "\t algacc: \t" << ld.wasAlgorithmAccepted() << "prescale" 
//  	     << ld.wasPrescaled() << endl;
      
//      }

	//}


//    cout << hex;
//    if (l2dec) {
//      l2dec->dump();
//    } else {
//      cout << "l2decision not found!" << endl;
//    }
//    cout << dec;




  
 /*
 if (runlevel2) {
   
    runlevel2->dump_info(runlevel2);
    
 }
 */


    ////////////////////////////////////////////////
    ////this is a test of TriggerUtitites: 
    ////////////////////////////////////////////////
    
    //    TriggerUtilities tu;

//    TrigRunLvl1v1  t1run;
//    TrigRunLvl2v2  t2run;

//    cout << "calling run trig objects" << endl;
//    tu.dbFillTrigRunObjects(65736, &t1run, &t2run);
//    cout << "done filling" << endl;
  
  //L2DecisionHelper::set_verbosity(2);
  //TriggerHelper::set_verbosity(2);

//    Lvl2DecisionOut* l2dec = static_cast<Lvl2DecisionOut*> (dst->getClass("L2Decision"));
//    TrigLvl1 *level1 = static_cast<TrigLvl1*> (dst->getClass("TrigLvl1"));
//    TrigRunLvl1 *runlevel1 = static_cast<TrigRunLvl1*> (dst->getClass("TrigRunLvl1"));
//    TrigRunLvl2 *runlevel2 = static_cast<TrigRunLvl2*> (dst->getClass("TrigRunLvl2"));
  
 
//    cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
//    cout <<"%%%%%%%%%%%%%    lvl2  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;

  
//    cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
//    cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;

//    runlevel2->dump_info(runlevel2);
  
//    cout << "\n>>>>>>>>>>copying>>>>>>>>>>>>>>\n" << endl;
//    TrigRunLvl2v2 ml2;
//    ml2.Copy(runlevel2);
//    ml2.dump_info(&ml2);

//    cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
//    cout <<"%%%%%%%%%%%%%    lvl1  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;

  
//    cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
//    cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;

//    runlevel1->dump_info(runlevel1);
  
//    cout << "\n>>>>>>>>>>copying>>>>>>>>>>>>>>\n" << endl;
//    TrigRunLvl1v1 ml1;
//    ml1.Copy(runlevel1);
//    ml1.dump_info(&ml1);  

//    cout <<"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"<< endl;
//    cout <<"%%%%%%%%%%%%%%%%%%%%%%%% done %%%%%%%%%%%%%%%%%%%%%%"<< endl;

    /////////////////////////////////////////////
    ////  end of trig util test
    /////////////////////////////////////////////
  

    return 0;
  
}













