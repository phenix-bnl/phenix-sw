////////////////////////
//Lvl2DecisionOutMicrov1:
//    microDST Output object:
//    
//    
//    Class Implementation
//     author: jfrantz  
//
//
//
//
//
//////////////////////////

#include <Lvl2DecisionOut.h>
#include <Lvl2DecisionOutv1.h>
#include <Lvl2DecisionOutMicrov1.h>
#include <microL2DArrayWrapperv1.h>
#include <TClonesArray.h>
#include <iostream>

ClassImp(Lvl2DecisionOutMicrov1)

Lvl2DecisionOutMicrov1::Lvl2DecisionOutMicrov1() 
{
  _numLvl1Triggers = 0;
  _microL2DArrayWrappers = new TClonesArray("microL2DArrayWrapperv1",1);
  Clear();
}

Lvl2DecisionOutMicrov1::Lvl2DecisionOutMicrov1(Lvl2DecisionOut * inDstObj) 
{
  Lvl2DecisionOutMicrov1();
  fillSelfFromDstObj(inDstObj);
}

Lvl2DecisionOutMicrov1::~Lvl2DecisionOutMicrov1()
{
  Clear();
  delete _microL2DArrayWrappers;
  return;
}

void Lvl2DecisionOutMicrov1::Clear(Option_t*) {
  // set everything to 0
  _finalDecision = 0;
  _numLvl1Triggers = 0;

  for (int ilevel1=0; ilevel1<MICROL2MAXNUMLVL1TRIGGERSV1; ilevel1++)
    {
      _lvl1Decision[ilevel1] = 0;
    }

  _microL2DArrayWrappers->Clear();
  _microL2DArrayWrappers->Expand(1);
}

void Lvl2DecisionOutMicrov1::Reset() {
  Clear();
}

void Lvl2DecisionOutMicrov1::identify(std::ostream& out) const
{
  out << "identify: Lvl2DecisionOutMicrov1 object" << std::endl;
}

int Lvl2DecisionOutMicrov1::addMicroL2DArrayWrapper(microL2DArrayWrapperv1 *wrapper) const
{
  // check if no of entries is the size of TCLArray - size is preallocated do we have free entries?
  if (_microL2DArrayWrappers->GetEntries()==_microL2DArrayWrappers->GetSize()) 
    {
      // if no free entry, add one
      _microL2DArrayWrappers->Expand(_microL2DArrayWrappers->GetSize()+1); //
    }

  TClonesArray &array = *_microL2DArrayWrappers;
  // create new microL2DArrayWrappers at last entry of TCLArray
  new(array[_microL2DArrayWrappers->GetEntries()]) microL2DArrayWrapperv1(*wrapper);
  return _microL2DArrayWrappers->GetEntries();
}


//the next two functions are sort of inverses of each other
void Lvl2DecisionOutMicrov1::fillSelfFromDstObj(Lvl2DecisionOut * inDstObj) 
{
  // packs info from a dst object into self
  _finalDecision = inDstObj->getFullDecision();
  _numLvl1Triggers = inDstObj->getNumLevel1Triggers(); 
       

  for (UINT i = 0;i < MICROL2MAXNUMLVL1TRIGGERSV1; i++)
    {
       _lvl1Decision[i] = inDstObj->getLevel1TriggerDecision(i);

       // fill _microL2DArrayWrappers
       if (_lvl1Decision[i] != 0) 
	 {	   
	   _numLvl1Triggers++;
	
	   microL2DArrayWrapperv1* arrWrapper = 
	     new microL2DArrayWrapperv1();
	   // fill this array of lvl2 decisions and add to list
	   for (UINT j = 0;j < MICROL2MAXNUMALGORITHMSV1; j++)
	     { // fill
	       arrWrapper->set_theArrayVal(j,inDstObj->getLvl1AlgorithmDecision(i,j));
	     }

	   // insurance in case TClonesA ordering fails 
	   // ==> numAlgs +1 saves lvl1 index I'm not fully trusting 
	   // of the Add code that I can't understand
	   arrWrapper->set_theArrayVal((MICROL2MAXNUMALGORITHMSV1),i);

	   addMicroL2DArrayWrapper(arrWrapper);
	 }
    } // for i

}


Lvl2DecisionOut* Lvl2DecisionOutMicrov1::createDstObj() const
{
  // resurrects a "new"-ed dstObj from packed self data:
  // the pointer it returns should be "delete"-ed!!!

  Lvl2DecisionOut* dstObj = new Lvl2DecisionOutv1();
 
  dstObj->setFullDecision(_finalDecision);
  dstObj->setNumLevel1Triggers(_microL2DArrayWrappers->GetEntries());


  // loop through lvl1 trigs
  UINT iWrap = 0;
  for (UINT i = 0;i < MICROL2MAXNUMLVL1TRIGGERSV1; i++)
    {
      dstObj->setLevel1TriggerDecision(i, _lvl1Decision[i]);
      if (_lvl1Decision[i] != 0) 
	{	  
	  if (iWrap > (UINT)_microL2DArrayWrappers->GetEntries())
	    {
	      std::cout << "L2Decision microdst data corrupt!!!!" << std::endl;
	    }

	  microL2DArrayWrapperv1* arrWrapper = 
	    (microL2DArrayWrapperv1*) _microL2DArrayWrappers->At(iWrap);
	  // _micL2DAWrappers is a TClonesArray of OBJECT (not ptr)
	  // ArrayWrappers that contain arrays (i.e. a 2-D array) 

	  //check that order matches
	  if (i != arrWrapper->get_theArrayVal(MICROL2MAXNUMALGORITHMSV1)) 
	    {
	      std::cout << "L2Decision microdst data corrupt!!!!" <<
		"lvl1 order not perserved in TClonesArr" << std::endl;
	    }
	  
	  iWrap++;

	  // passed checks -- so fill Lvl2DecsionOut object
	  for (UINT j = 0;j < MICROL2MAXNUMALGORITHMSV1; j++)
	     { 
	       UINT decis = arrWrapper->get_theArrayVal(j);
	       dstObj->setLvl1AlgorithmDecision(i,j,decis);
	       // dst.getAlgDecis: l2 decis for any l1 it ran on
	       // if non-zero, answer must be same for all l1s
	       if (decis != 0) 
		 dstObj->setAlgorithmDecision(j,decis);
	     }
	}
    } // end loop through lvl1 triggers 

  // return the pointer to the DST object created
  return dstObj;
}

//------------------------------------------------------------------

/// Lvl2DecisionOut Functs (these must be defined since inherited from Lvl2DecisionOut


int  Lvl2DecisionOutMicrov1::locateLvl1L2DArray(UINT ilevel1)
{
  // returns the _microClusArray index for the given level1 
  //assuming the order in _microClus array is same as in _lvl1De 
  // returns -1 if not there
  int last_nonzero_entry = -1;
  for (UINT i = 0; i < MICROL2MAXNUMLVL1TRIGGERSV1; i++)
    {
      if (_lvl1Decision[i] != 0) last_nonzero_entry++;
      if (i == ilevel1) return last_nonzero_entry;
    }
  return -1; //error if here
}  

void	 Lvl2DecisionOutMicrov1::setFullDecision(UINT decision) 
{
  _finalDecision = decision;
}

UINT	 Lvl2DecisionOutMicrov1::getFullDecision()
{
  return _finalDecision;
}

void	 Lvl2DecisionOutMicrov1::setNumLevel1Triggers(UINT num)
{
  _numLvl1Triggers = num;
}

UINT Lvl2DecisionOutMicrov1::getNumLevel1Triggers()
{
  return _numLvl1Triggers;
}

void  Lvl2DecisionOutMicrov1::setLevel1TriggerDecision(UINT ilevel1, UINT decision)
{
      _lvl1Decision[ilevel1] = decision;
}

UINT	 Lvl2DecisionOutMicrov1::getLevel1TriggerDecision(UINT ilevel1)
{
  return _lvl1Decision[ilevel1];
}


UINT	 Lvl2DecisionOutMicrov1::getAlgorithmDecision(UINT ialg)
{
  Lvl2DecisionOut* dstObj = createDstObj();
  UINT theD = dstObj->getAlgorithmDecision(ialg);
  delete dstObj;
  return theD;
}

void	 Lvl2DecisionOutMicrov1::setLvl1AlgorithmDecision(UINT ilevel1, UINT ialg, UINT decision)
{
  // assume ordering is ok
  int microL2ArrIndex = locateLvl1L2DArray(ilevel1);
  if (microL2ArrIndex < 0 || microL2ArrIndex >= MICROL2MAXNUMLVL1TRIGGERSV1)
    return;
  microL2DArrayWrapperv1 * thisWrapper = (microL2DArrayWrapperv1 *) 
    _microL2DArrayWrappers->UncheckedAt(microL2ArrIndex);
  thisWrapper->set_theArrayVal(ialg, decision);

}

UINT   Lvl2DecisionOutMicrov1::getLvl1AlgorithmDecision(UINT ilevel1, UINT ialg)
{
  // assume ordering is ok
  int microL2ArrIndex = locateLvl1L2DArray(ilevel1);
  if (microL2ArrIndex < 0 || microL2ArrIndex >= MICROL2MAXNUMLVL1TRIGGERSV1)
    {
      std::cout << "faulty level2 storage access" << std::endl;
      return 99999;
    }

  microL2DArrayWrapperv1 * thisWrapper = (microL2DArrayWrapperv1 *) 
    _microL2DArrayWrappers->UncheckedAt(microL2ArrIndex);
  
  return thisWrapper->get_theArrayVal(ialg);
}


















