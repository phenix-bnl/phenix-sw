#ifndef PHAFTERBURNER_H
#define PHAFTERBURNER_H

#include "PHCompositeNode.h"
#include "PHObject.h"

//
//  Hello AfterBurner user.  Although in an ideal world we would like  
//  all calibrations to be perfectly under control for every pass
//  of data, the real world frequently results in discovered mistakes 
//  that are frequently taken care of as so-called "after-burners".
//
//  This file contains a base class for PHENIX AfterBurners.  It is a purely 
//  virtual base class and is thereby used to strictly impose the interface
//  to which all AfterBurner authors must conform.
//
//  It is assumed that there are two important possible moments at which we
//  may apply these AfterBurners.  The first is dst->udst production.  The
//  second is udst->ndst production.  These stages have one format completely
//  in common:  the udst format.  Users are *required* to unpack the udst
//  data and apply corrections directly to the udst data.  This would be a 
//  last step in dst->udst framework and would be a first step in the udst->ndst
//  framework.
//
//  The user codes to two different methods.  The first is an initialize method.
//  This is called after the first event of a particular file has been unpacked.
//  The user should do whatever initialization is necessary which may, for example,
//  be run dependent (hence the wait for the first event so that the run number 
//  can be known).
//  
//  The second method which the user must code is called apply.  This one is feed 
//  the node which rests directly above the udst.  The user is then expected to 
//  repair the data in the udst node in accordance with their needs.  The details
//  of how these corrections are determined and applied is entirely up to the
//  user, however, the user should note one important feature of this framework
//  which may be of use. The AfterBurner object will become a "once-per-run" data
//  output member in the produced file (either udst or ndst).  If you choose to hold
//  your afterburner constants as member variables of the class which inherits from 
//  here, you will automatically achieve a permanent record of these constants without
//  applying any additional effort.
//                                  TKH 4-19-2002
//


class PHAfterBurner : public PHObject 
{
  
 public:
  PHAfterBurner() {}
  virtual ~PHAfterBurner() {}
  
  // These are purely virtual and must be overridden
  // in the inherited classes..
  // They should return true when all cuts are passed
  virtual void initialize(PHCompositeNode *topNode )=0;  //Feed the topnode to see everything.
  virtual void apply     (PHCompositeNode *udstNode)=0;  //Feed only the udst node since the corrections
                                                         //must operate purely on udst.
  
  ClassDef(PHAfterBurner,1)
    
    };
#endif
    

