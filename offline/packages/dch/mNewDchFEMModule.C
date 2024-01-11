#include "DchAnaPar.h"

#include "mNewDchFEMModule.hh"

#include "dDchRawWrapper.h"
#include "dDchGhitRawWrapper.h"
#include "dDchFEMWrapper.h"
#include "dDchNibbleGhitWrapper.h"

#include "PHDchAddressObject.h"
#include "PHIODataNode.h"

#include "getClass.h"

#include <cstdlib>
#include <iostream>
using namespace std;

//__________________________________________________
mNewDchFEMModule::mNewDchFEMModule()
{
  // this object must _not_ be destructed at the end of the 
  // processing because it is not created by "new", but accessed via 
  // findNode::getClass. Its deletion must thus be left to fun4all.
  dchAddressObject = 0;
  return;
}

//__________________________________________________
PHBoolean mNewDchFEMModule::event(PHCompositeNode *root) 
{

  dDchRawWrapper *dDchRawWrap = findNode::getClass<dDchRawWrapper>(root,"dDchRaw");
  if (!dDchRawWrap)
  {
    cout << PHWHERE << "dDchRaw Node missing" << endl;
    exit(1);
  }
 
  dDchGhitRawWrapper *dDchGhitRawWrap = findNode::getClass<dDchGhitRawWrapper>(root,"dDchGhitRaw");
  if (!dDchRawWrap)
  {
    cout << PHWHERE << "dDchGhitRaw Node missing" << endl;
    exit(1);
  }
  
  dDchFEMWrapper *dDchFEMWrap = findNode::getClass<dDchFEMWrapper>(root,"dDchFEM");
  if (!dDchFEMWrap)
  {
    cout << PHWHERE << "dDchFEM Node missing" << endl;
    exit(1);
  }
  
  dDchNibbleGhitWrapper *dDchNibbleGhitWrap = findNode::getClass<dDchNibbleGhitWrapper>(root,"dDchNibbleGhit");
  if (!dDchNibbleGhitWrap)
  {
    cout << PHWHERE << "dDchNibbleGhit Node missing" << endl;
    exit(1);
  }
  
  
  dchAddressObject = findNode::getClass<PHDchAddressObject>(root,"DchDAO");
  
  dDchRaw_h = dDchRawWrap->TableHeader();
  dDchRaw = dDchRawWrap->TableData();
  dDchGhitRaw_h = dDchGhitRawWrap->TableHeader();
  dDchGhitRaw = dDchGhitRawWrap->TableData();
  dDchFEM_h = dDchFEMWrap->TableHeader();
  dDchFEM = dDchFEMWrap->TableData();
  dDchNibbleGhit_h= dDchNibbleGhitWrap->TableHeader();
  dDchNibbleGhit = dDchNibbleGhitWrap->TableData();
  
  dDchFEM_h.nok =0;
  initialize();
  insertRawHits();
  
  dDchRawWrap->SetRowCount(dDchRaw_h.nok);
  dDchGhitRawWrap->SetRowCount(dDchGhitRaw_h.nok);
  dDchFEMWrap->SetRowCount(dDchFEM_h.nok);
  dDchNibbleGhitWrap->SetRowCount(dDchNibbleGhit_h.nok);
  
  return True;
  
}

void 
mNewDchFEMModule::initialize()
{
  short j;  
  unsigned long Id;

  unsigned long Addr;
  
  // First fill up the array headers but have no hits at all
  short counter = 0;
    for (short iarm = 0; iarm < numberOfArms; iarm++) {
       Id = 0x300;
       Id+= iarm;
       for (short iside = 0; iside < numberOfSides; iside++) {
	   for (short ikey =0; ikey < numberOfKeyStones; ikey++) {
	       for (short ipair = 0; ipair < numberOfPairs; ipair++) {
		   Addr = 0x00000;
		   Addr = Addr + iside*0x1000 + ikey*0x10 + ipair;
		   dDchFEM[counter].CAV1 = 0xFFFFF;
		   dDchFEM[counter].det  = Id;
		   // event counter - 16bits: fill with an arbitrary number
		   dDchFEM[counter].Ecounter = 0x01234;
		   dDchFEM[counter].adr      = Addr;
		   dDchFEM[counter].Flag     = 0xFF333;
                   // beam counter - 8bits: fill with an arbitrary number
		   dDchFEM[counter].Bcounter = 0x00056;
		   dDchFEM[counter].usr1     = 0xFF444;
		   dDchFEM[counter].usr2     = 0xFF555;
                   // status word: fill with an arbitrary number
		   dDchFEM[counter].usr3     = 0x0;
		   dDchFEM[counter].usr4     = 0x0;
                   // XOR of all previous words except the CAV1 ---> should fix that later
		   dDchFEM[counter].parity   = 0x0;
		   dDchFEM[counter].CAV2     = 0;

		   j = 0;
		   for (short iwire = 0; iwire < numberOfChannels; iwire++) {
		       for (short iword = 0; iword < numberOfWordsForChannel; iword++) {
                          dDchFEM[counter].Word[j] = 0x0;
			  j++;
		       }
		   }
		   index[iarm][iside][ikey][ipair] = counter;
		   counter++;
		   dDchFEM_h.nok++;
		   
	       }
	   }
       }
    }   
}
 
void 
mNewDchFEMModule::insertRawHits()
{
 short geantHitId;
 for (int i = 0; i < dDchRaw_h.nok; i+=2) {

   dchAddressObject->setSoft(dDchRaw[i].arm,dDchRaw[i].side, dDchRaw[i].plane, dDchRaw[i].cell);
   short theArm  = dchAddressObject->getArm()->getValue();
   short theSide = dchAddressObject->getSide()->getValue();
   short theKeystone  = dchAddressObject->getKeystone()->getValue();
   short thePair = dchAddressObject->getPair()->getValue();
   theChannel = dchAddressObject->getChannel()->getValue();
   theCounter = index[theArm][theSide][theKeystone][thePair];
   
   geantHitId   = dDchGhitRaw[i].ghitid;
   insertEdges(dDchRaw[i].time, dDchRaw[i+1].time, geantHitId);
 }
}

short 
mNewDchFEMModule::insertEdges(const short leadingEdge, const short trailingEdge,const short geantHitId)
{
    // check if leading and trailing edges are in allowed range

  if (leadingEdge < 0 ||  trailingEdge < 0)
    {
      return 0;
    }
  if (leadingEdge > maxEdge || trailingEdge > maxEdge)
    {
      return 0;
    }
  if (leadingEdge > trailingEdge)
    {
      return 0;
    }

  short idummy;
  short status = 0;
  short nibble = leadingEdge/16;
  short tL     = leadingEdge - nibble*16;
  short value  = 0x10|tL;
  short theNibble = readData(nibble);
  
  switch(theNibble){
  case 0:
    writeData(nibble, value);
    addRelation(nibble,geantHitId);
    status++;
    break;
  case 0x01:
      break;
  default:
    if(theNibble&0x10) {// a leading edge was already here
      idummy = theNibble - value;
      if (idummy >= 0){
	writeData( nibble, value);
	replaceRelation(nibble,geantHitId);
	status++;
      }
    }else {  // we have a trailing edge here -> look who arrived first
      idummy = ((theNibble&0x07)*2)-(value&0x0f);
      if (idummy >=0) {
	writeData(nibble,0x01);
      }else { // trailing edge lost to discretization 
	writeData(nibble,value);
	replaceRelation(nibble,geantHitId);
	status++;
      }
    }
    break;
  }
  
  nibble   = trailingEdge/16;
  short tT = (trailingEdge - nibble*16)/2;
  value    = 0x08|tT;
  theNibble = readData(nibble);
  if(theNibble == 0) { // nothing there
    writeData(nibble,value);
    addRelation(nibble,geantHitId);
    status+= 10;
  }else{ // if earlier trailing edge ---> overwrite
    if(theNibble&0x08) {
      idummy = theNibble&0x07;
      if(((value&0x07)-idummy) >= 0) {
	writeData(nibble,value);
	replaceRelation(nibble,geantHitId);
	status+=10;
      }
    }
  }
  
  short lNibble = leadingEdge/16;
  short tNibble = trailingEdge/16;
  if ((tNibble- lNibble) > 1) {
    for (short i = lNibble+1; i < tNibble; i++) {
      writeData(i,0x01);
    }
  }
  
  return status;
}

void 
mNewDchFEMModule::writeData(const short nibble, const short value)
{
  
  short channel=theChannel;
  short timeSlice;
  short myNibble;
  unsigned long theWord = 0;
  unsigned long idummy = 0;

  if (nibble > 47) {
    cout << PHWHERE " this is a very late hit: it falls out of digitalization, limit 46, seen: " << nibble << endl;
  } else {
    timeSlice = (channel*12)+(nibble/4);
    idummy    = dDchFEM[theCounter].Word[timeSlice];
    myNibble  = nibble - ((nibble/4)*4);
    switch(myNibble){
      
    case 0:
      // 0000 0|000 00|00 000|1 1111 this is where we want the value
      theWord=value;
      //1111 1|111 11|11 111|0 0000 don't touch the bits in the other Nibbles
      idummy=idummy&0xfffe0;
      theWord=theWord|idummy;
      break;

    case 1:
      // and with 0000 0|000 00|11 111|0 0000 = 0x003e0
      theWord=value;
      theWord<<=5;
      //1111 1|111 11|00 000|1 1111 don't touch the bits in the other Nibbles
      idummy=idummy&0xffc1f;
      theWord=theWord|idummy;
      break;

    case 2:
      // 0000 0|111 11|00 000|0 0000 first put the value in the space we want
      theWord=value;
      theWord<<=10;
      //1111 1|000 00|11 111|1 1111 don't touch the bits in the other Nibbles
      idummy=(idummy&0xf83ff);
      theWord=theWord|idummy;
      break;
    case 3:
      // 1111 1|000 00|00 000|0 0000 this is where we want the value
      theWord=value;
      theWord<<=15;
      //0000 0|111 11|11 111|1 1111 don't touch the bits in the other Nibbles
      idummy=idummy&0x07fff;
      theWord=theWord|idummy;
      break;

    }
    dDchFEM[theCounter].Word[timeSlice]=theWord;
  }
}

short 
mNewDchFEMModule::readData(const short nibble) const
{
  short timeSlice = 0;
  short myNibble = 0;
  short theNibble = 0;
  unsigned long idummy;
  short channel=theChannel;
  
  timeSlice = channel*12 + (nibble/4);
  idummy    = dDchFEM[theCounter].Word[timeSlice];
  myNibble  = nibble - (nibble/4)*4;
  switch(myNibble){
    
  case 0:
    // and with 0000 0|000 00|00 000|1 1111 = 0x0001f
    theNibble=idummy&0x1f;
    break;
  case 1:
    // and with 0000 0|000 00|11 111|0 0000 = 0x003e
    idummy>>=5;
    theNibble=idummy&0x1f;
    break;
    
  case 2:
    // and with 0000 0|111 11|00 000|0 0000 = 0x07c00
    // now shift to get the nibble only
    idummy>>=10;
    theNibble=idummy&0x1f;
    break;
  case 3:
    // and with 1111 1|000 00|00 000|0 0000 = 0xf8000
    idummy>>=15;
    theNibble=idummy&0x1f;
    break;
  }
  return(theNibble);
}

void 
mNewDchFEMModule::print(const short counter) const
{
  short i; 
  cout << hex;
  cout << dDchFEM[counter].CAV1 << " " ;
  cout << dDchFEM[counter].det  << " " ;
  cout << dDchFEM[counter].Ecounter  << " " ;
  cout << dDchFEM[counter].adr  << " " ;
  cout << dDchFEM[counter].Flag  << " " ;
  cout << dDchFEM[counter].Bcounter  << " " ;
  cout << endl;
  
  i = 0;
  for (short ich = 0; ich < numberOfChannels; ich++) {
    for(short iword = 0; iword < numberOfWordsForChannel; iword++) {
      cout << dDchFEM[counter].Word[i]  << " " ;
      i++;
    }
    cout << endl;
  }
  
  cout << dDchFEM[counter].usr1  << " " ;
  cout << dDchFEM[counter].usr2  << " " ;
  cout << dDchFEM[counter].usr3  << " " ;
  cout << dDchFEM[counter].usr4  << " " ;
  cout << dDchFEM[counter].parity  << " " ;
  cout << dDchFEM[counter].CAV2  << " " ;
  cout << dec << endl;
  
}

void 
mNewDchFEMModule::addRelation(const short nibble, const short geantHitId)
{
  int i = dDchNibbleGhit_h.nok;
  dDchNibbleGhit[i].ghitid = geantHitId;
  dDchNibbleGhit[i].arm    = dchAddressObject->getArm()->getValue();
  dDchNibbleGhit[i].side   = dchAddressObject->getSide()->getValue();
  dDchNibbleGhit[i].key    = dchAddressObject->getKeystone()->getValue();
  dDchNibbleGhit[i].pair   = dchAddressObject->getPair()->getValue();
  dDchNibbleGhit[i].channel = theChannel;
  dDchNibbleGhit[i].Nibble  = nibble;
  dDchNibbleGhit_h.nok++;
}

void 
mNewDchFEMModule::replaceRelation(const short nibble, const short geantHitId)
{
  for (int i = 0; i < dDchNibbleGhit_h.nok; i++) {
    if (dDchNibbleGhit[i].arm  == dchAddressObject->getArm()->getValue()  &&
	dDchNibbleGhit[i].side == dchAddressObject->getSide()->getValue() &&
	dDchNibbleGhit[i].key  == dchAddressObject->getKeystone()->getValue() &&
	dDchNibbleGhit[i].pair ==  dchAddressObject->getPair()->getValue() &&
	dDchNibbleGhit[i].channel == theChannel &&
	dDchNibbleGhit[i].Nibble  == nibble) {
      
      dDchNibbleGhit[i].ghitid = geantHitId;
      return;
    }
  }
  cout << "DchFEM::replaceRelation  -> Error , previous entry missing !! " << endl;
}
