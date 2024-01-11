//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDummyAddressObject.h                       
//                                                                
// Created by: Federica Ceretto                                     
//                                                                
// Description:                                                   
//                                                                
// Last update:                                           
//                                                                
//----------------------------------------------------------------

#include "PHDummyAddressObject.h" 
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

PHDummyAddressObject::PHDummyAddressObject()
{
}


PHDummyAddressObject::~PHDummyAddressObject()
{  
  DetectorIndex.clearAndDestroy();
  commit();
}

PHBoolean PHDummyAddressObject::setSoft(int iarm, int imod, int ismod)
{
  //----------------------------------------
  // set in one go all the Software Indices
  //----------------------------------------
  arm->setValue(iarm);  
  module->setValue(imod);
  superModule->setValue(ismod);

  //----------------------------------------
  // Upgrade the Hardware indices
  //----------------------------------------

  fromSoftToHard();  
}

PHBoolean PHDummyAddressObject::setHard(int iarm, int iboard , int ichannel)
{

  //----------------------------------------
  // set in one go all the Hardware Indices
  //----------------------------------------
  arm->setValue(iarm);
  board->setValue(iboard);;
  channel->setValue(ichannel);

  //----------------------------------------
  // Upgrade the Software indices
  //----------------------------------------
  fromHardToSoft();
}

PHBoolean PHDummyAddressObject::setGlobalIndex(PdbIndex* ind)
{
  //------------------------------------------------------------------------
  // setting the indeces  corresponding to a particular global index 
  //------------------------------------------------------------------------

  int ii = ind->getValue();
  return setGlobalIndex(ii);
}

PHBoolean
PHDummyAddressObject::setGlobalIndex(int ind)
{
  //------------------------------------------------------------------------
  // setting the indeces corresponding to a particular global index
  //------------------------------------------------------------------------

  int tmpValue;
  int iarm, ichannel, iboard;

  ichannel = (ind % channel->getNumberOf());
  channel->setValue(ichannel);
  tmpValue = ind / channel->getNumberOf();

  iboard = tmpValue % board->getNumberOf();
  board->setValue(iboard);

  tmpValue = tmpValue / board->getNumberOf();
  iarm = tmpValue % arm->getNumberOf();
  arm->setValue(iarm);

  setHard(iarm, iboard, ichannel);

  return True;
}

PdbIndex* PHDummyAddressObject::getDetectorIndex(int ind , PdbIndex* global) 
{
  //------------------------------------------
  // from global index to local one --> 
  // the software indices : only arm,plane,cell can be calculated  
  //------------------------------------------
    setGlobalIndex(global);
    switch(ind) {
    case ARM:
       return arm;
    case MODULE:
       return module;
    case  SUPERMODULE:
      return superModule;
    default:
      PHMessage("PHDummyAddressObject::getDetectorIndex",PHWarning, "Can not be calculated !!!");
      return 0;     
    }
    
}
PHBoolean PHDummyAddressObject::setDetectorIndex(int ind, PdbIndex* local) {

  //-----------------------------------------------------
  // setting a particular index to a certain value/range: 
  // Since the function does not update all the index together,
  // for setting a value is strongly suggested
  // to use functions which set in one go all the index together
  // This function is instead useful ONLY to set the range of one index
  //-----------------------------------------------------
 
   switch(ind) {
    case ARM:
       arm = local;
       break;
    case MODULE:
       module = local;
       break;
    case  SUPERMODULE:
      superModule = local;
      break;
    case  BOARD:
      board = local;
      break;
     case  CHANNEL:
      channel = local;
      break;
   default:
      PHMessage("PHDummyAddressObject::setDetectorIndex",PHWarning, " Index NOT Existing !!!");
       return False;     
    }
 
 return True;
}
							
void PHDummyAddressObject::initialize() 
{ 
  //-----------------------------------------------------------------------------------
  //  This function will be substitute later when the access to the database is ready !
  //  The values will be read from the database
  //-----------------------------------------------------------------------------------
 
  arm          = new PdbIndex(0, 1,0,"ARM");
  module       = new PdbIndex(0, 8,0,"MODULE");
  superModule  = new PdbIndex(0, 3,0,"SUPERMODULE");
  board        = new PdbIndex(0, 5,0,"BOARD");
  channel      = new PdbIndex(0, 5,0,"CHANNEL");

  int tmpMaxIndex = channel->getMax() + board->getMax()*channel->getNumberOf() +
                         arm->getMax()*channel->getNumberOf()*board->getNumberOf();
                              

  index   = new PdbIndex(0,tmpMaxIndex,0,"GLOBAL");

  DetectorIndex.append(arm);
  DetectorIndex.append(module);
  DetectorIndex.append(superModule);
  DetectorIndex.append(board);
  DetectorIndex.append(channel);
 
}

PHBoolean PHDummyAddressObject::fromSoftToHard()
{


  //----------------------------------
  // How to get from Software indices to Hardware ones
  //----------------------------------

  int iarm,imodule,isuperModule;
  iarm = arm->getValue();
  imodule = module->getValue();
  isuperModule = superModule->getValue();

  // surely can be done more cleverly
  
  if (((imodule%3)==0) && ((isuperModule%2)==0)) {
    board->setValue(0);
  } else if (((imodule%3)==0) && ((isuperModule%2)==1)) {
    board->setValue(3);
  }else if (((imodule%3)==1) && ((isuperModule%2)==0)) {
    board->setValue(1);
  } else if (((imodule%3)==1) && ((isuperModule%2)==1)) {
    board->setValue(4);
  }else if (((imodule%3)==2) && ((isuperModule%2)==0)) {
    board->setValue(2);
  } else if (((imodule%3)==2) && ((isuperModule%2)==1)) {
    board->setValue(5);
  }

  int tmpValue;
  if (isuperModule < 2) {
    if (imodule < 3) {
      tmpValue = 5;
    } else if (imodule < 6) {
      tmpValue = 4;
    } else if (imodule <9) {
      tmpValue = 3;
    }
  }else if (isuperModule < 4) {
    if (imodule < 3) {
      tmpValue = 2;
    } else if (imodule < 6) {
      tmpValue = 1;
    } else if (imodule <9) {
      tmpValue = 0;
    }
  }
  
  channel->setValue(tmpValue);

  	       
  index->setValue(channel->getValue() + board->getValue()*channel->getNumberOf() +
		  arm->getValue()*board->getNumberOf()*channel->getNumberOf());


}

PHBoolean PHDummyAddressObject::fromHardToSoft()
{
  //----------------------------------
  // How to get from Hardware indices to software ones
  //----------------------------------

  int iboard, ichannel, iarm;
  iarm = arm->getValue();
  iboard = board->getValue();
  ichannel = channel->getValue();
  
  /// do your calculation here !!!!

  int mod,ism;
  if (iboard <3 && ichannel <3) {
    ism = 2;
  }
  if (iboard >2 && ichannel >2) {
    ism = 1;
  }
  if (iboard <3 && ichannel >2) {
    ism = 0;
  }
  if (iboard >2 && ichannel <3) {
    ism = 3;
  }

  superModule->setValue(ism);



  // to be finish
  

  

  //---------------------------
  //  Calculate the global index
  //---------------------------

  index->setValue(channel->getValue() + board->getValue()*channel->getNumberOf() +
		  arm->getValue()*board->getNumberOf()*channel->getNumberOf());

 
}

PHBoolean PHDummyAddressObject::update(PHTimeStamp &Tstart,PHTimeStamp &Tstop,
                                   const char *calibname, PdbBankID bankID, char *descrip )
{
  initialize();
  if (committed == 1) {                       
    if(!application->startUpdate()) {
      PHMessage("PHDummyAddressObject",PHError, "Aborting ... Database not writable");
      application->abort();
    }else{
      committed = 0;
    }
  }
          
  addressBank = bankManager->createBank("PdbIndexBank",bankID,descrip,Tstart,Tstop,calibname);
  int length = getLength();  
  addressBank->setLength(length);
  addressBank->print();
  
  PdbIndex *pdbindex;
  for(int i=0; i < addressBank->getLength(); i++) {
    pdbindex = (PdbIndex*)& addressBank->getEntry(i);
    *pdbindex = *(DetectorIndex[i]);	
  }
  return True;
}

PHBoolean PHDummyAddressObject::fetch(PHTimeStamp &Tsearch, const char *name, PdbBankID bankID )
{
  if (committed == 1) {                       
    if(!application->startRead()) {
      PHMessage("PHDummyAddressObject",PHError, "Aborting ... Database not readable");
      application->abort();
    }else{
      committed = 0;
    }
  }
  DetectorIndex.clearAndDestroy();
  addressBank = bankManager->fetchBank("PdbIndexBank",bankID,name,Tsearch);
  addressBank->print();
  
  PdbIndex *thisIndex;
  PdbIndex *pdbindex;
  for(int i=0; i < addressBank->getLength(); i++) {
    pdbindex = (PdbIndex*)& addressBank->getEntry(i);
    pdbindex->print();
    thisIndex = new PdbIndex();
    *thisIndex = *pdbindex;
    DetectorIndex.append(thisIndex);
  }
  
  arm          = PHAddressObject::getDetectorIndex(0);
  module       = PHAddressObject::getDetectorIndex(1);
  superModule  = PHAddressObject::getDetectorIndex(2);
  board        = PHAddressObject::getDetectorIndex(3);
  channel      = PHAddressObject::getDetectorIndex(4);
 
  int tmpMaxIndex = arm->getNumberOf()*module->getNumberOf()*superModule->getNumberOf() -1 ;

  index   = new PdbIndex(0,tmpMaxIndex,0,"GLOBAL");
  
  return True;
  
}
