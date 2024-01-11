/*////////////////////////
//microL2Decision:
//    microDST Output object:
//    
//    
//    Class Implementation
//    
//    
//
//     author: jfrantz  
//    
//    
//    
//
//
//////////////////////////*/

#include <microL2Decision.h>
#include <iostream.h>
#include <TClonesArray.h>
#include <Lvl2DecisionOutv1.h>
#include <microL2DArrayWrapper.h>

ClassImp(microL2Decision)

microL2Decision::microL2Decision() 
{
  _numLvl1Triggers = 0;
  dstObj = NULL; 
  _microL2DArrayWrappers = new TClonesArray("microL2DArrayWrapper",1);
  Clear();
}

microL2Decision::microL2Decision(Lvl2DecisionOutv1 * inDstObj) 
{
  microL2Decision();
  fillSelfFromDstObj(inDstObj);
}

microL2Decision::~microL2Decision() {
  Clear();
}

void microL2Decision::Clear() {
  // set everything to 0
  _finalDecision = 0;
  _numLvl1Triggers = 0;

  for (int ilevel1=0; ilevel1<MicroL2MaxNumLvl1Triggers; ilevel1++)
    {
      _lvl1Decision[ilevel1] = 0;
    }

  _microL2DArrayWrappers->Clear();
  _microL2DArrayWrappers->Expand(0);
}

void microL2Decision::Reset() {
  Clear();
}

void microL2Decision::identify(ostream& out) const 
{
  out << "lvl2 decision information" << endl;
}

int microL2Decision::addMicroL2DArrayWrapper(microL2DArrayWrapper* wrapper) const
{

  if (_microL2DArrayWrappers->GetEntries()==_microL2DArrayWrappers->GetSize()) 
    {
      _microL2DArrayWrappers->Expand(_microL2DArrayWrappers->GetSize()+1);
    }

  TClonesArray &array = *_microL2DArrayWrappers;
  new(array[_microL2DArrayWrappers->GetEntries()]) microL2DArrayWrapper(*wrapper);
  // this is the most confusing line of code ^^^ I've ever seen
  // But I'm following straight from S. Lebedev's EMCal ClusterList
  // example --jfrantz

  return _microL2DArrayWrappers->GetEntries();
}

//the next two functions are sort of inverses of each other
void microL2Decision::fillSelfFromDstObj(Lvl2DecisionOutv1 * inDstObj) 
{
  // packs info from a dst object into self
  _finalDecision = inDstObj->getFullDecision();
  _numLvl1Triggers = inDstObj->getNumLevel1Triggers(); 
       

  for (UINT i = 0;i < MicroL2MaxNumLvl1Triggers; i++)
    {
       _lvl1Decision[i] = inDstObj->getLevel1TriggerDecision(i);

       // fill _microL2DArrayWrappers
       if (_lvl1Decision[i] != 0) 
	 {	   
	   _numLvl1Triggers++;
	
	   microL2DArrayWrapper* arrWrapper = 
	     new microL2DArrayWrapper();
	   // fill this array of lvl2 decisions and add to list
	   for (UINT j = 0;j < MicroL2MaxNumAlgorithms; j++)
	     { // fill
	       arrWrapper->theArray[j] =
		 inDstObj->getLvl1AlgorithmDecision(i,j);
	     }

	   // insurance in case TClonesA ordering fails 
	   // ==> +1 saves lvl1 index I'm not fully trusting 
	   // of the Add code that I can't understand
	   arrWrapper->theArray[MicroL2MaxNumAlgorithms +1] = i;

	   addMicroL2DArrayWrapper(arrWrapper);
	 }
    } // for i

}


void microL2Decision::createDstObj() 
{
  // resurrects dstObj from packed self data

  dstObj = new Lvl2DecisionOutv1();
 
  dstObj->setFullDecision(_finalDecision);
  dstObj->setNumLevel1Triggers(MicroL2MaxNumLvl1Triggers);


  // loop through lvl1 trigs
  UINT iWrap = 0;
  for (UINT i = 0;i < MicroL2MaxNumLvl1Triggers; i++)
    {
      dstObj->setLevel1TriggerDecision(i, _lvl1Decision[i]);
      if (_lvl1Decision != 0) 
	{	  
	  if (iWrap > _microL2DArrayWrappers->GetSize())
	    {
	      cout << "L2Decision microdst data corrupt!!!!" << endl;
	    }

	  microL2DArrayWrapper* arrWrapper = 
	    (microL2DArrayWrapper*) _microL2DArrayWrappers->At(iWrap++);
	  // _micL2DAWrappers is a TClonesArray of OBJECT (not ptr)
	  // ArrayWrappers that contain arrays (i.e. a 2-D array) 

	  //check that order matches
	  if (i != arrWrapper->theArray[MicroL2MaxNumAlgorithms + 1]) 
	    {
	      cout << "L2Decision microdst data corrupt!!!!" <<
		"lvl1 order not perserved in TClonesArr" << endl;
	    }
	  
	  // passed checks -- so fill Lvl2DecsionOut object
	  for (UINT j = 0;j < MicroL2MaxNumAlgorithms; j++)
	     { 
	       UINT decis = arrWrapper->theArray[j];
	       dstObj->setLvl1AlgorithmDecision(i,j,decis);
	       // dst.getAlgDecis: l2 decis for any l1 it ran on
	       // if non-zero, answer must be same for all l1s
	       if (decis != 0) 
		 dstObj->setAlgorithmDecision(j,decis);
	     }
	}
    } // end loop through lvl1 triggers 
}

//------------------------------------------------------------------









