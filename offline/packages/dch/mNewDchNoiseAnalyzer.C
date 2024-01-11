#include "PHDchNoiseObject.h"
#include "PHDchHistogrammer.hh"

#include "BbcOut.h"

#include "mNewDchNoiseAnalyzer.hh"


#include "PHIODataNode.h"

#include "RunToTime.hh"
#include "Event.h"

#include <iostream>
#include <sstream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<BbcOut> BbcOutNode_t;

mNewDchNoiseAnalyzer::mNewDchNoiseAnalyzer(int f, float noiseThreshold)
{
   flagBBC = f;  
   referenceTime = 0;
   pDchAddress   = 0;
   pDchGeometry  = 0;
   pDchNoise     = 0;
   pDchCalibration = 0;
   pDchHistogrammer = 0;
   eventCounter = 0;
   timeZeroBBC = 0;
   calibrationStage = 0;
   EventRunNumber = 0;

   for (short arm = 0; arm < numberOfArms; arm++) {
     for (short side = 0; side < numberOfSides; side++) {
       for (short plane = 0; plane < numberOfPlanes; plane++) {
	 for (short cell = 0; cell < numberOfCells; cell++) {
	    noiseFieldEdge0[arm][side][plane][cell] = 0;
	    noiseFieldEdge1[arm][side][plane][cell] = 0;
	 }
       }
     }
   }
 }

PHBoolean 
mNewDchNoiseAnalyzer::event(PHCompositeNode *topNode) 
{
  PHObjectNode_t *phob;
  PHNodeIterator i(topNode);
  
 PHDataNode<PHDchAddressObject>* pDchDaoNode;
 PHDataNode<PHDchGeometryObject>* pDchDgoNode;
 PHDataNode<PHDchCalibrationObject>* pDchDcoNode;
 PHDataNode<PHDchNoiseObject>* pDchDnoNode;
 PHDataNode<PHDchHistogrammer>* pDchHistoNode;

 pTopNode = topNode;

 phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchRawTablev1"));
 rawTable = dynamic_cast < DchRawTable * >(phob->getData ());
 if (!rawTable) {
   cout << "DchRawTablev1 not found "<< endl;
   return False;
 }

 // try to extract the information from the PRDF event 

 Event *eventPRDF;
 PHDataNode<Event>* eventNodePRDF;

 eventPRDF = 0;
 eventNodePRDF = (PHDataNode<Event>*)i.findFirst("PHDataNode", "PRDF");
 if (eventNodePRDF)
   {
     eventPRDF = eventNodePRDF->getData();
     if (eventPRDF)
       {
	 EventRunNumber = eventPRDF->getRunNumber();
       }
     else
       {
	 cout << "Event PRDF not found in node tree " << endl;
       }
   }
 else
   {
     cout << "eventPRDFNode  not found " << endl;
  }

 pDchHistoNode = (PHDataNode<PHDchHistogrammer>*)i.findFirst("PHDataNode","DchHisto");
 pDchHistogrammer = pDchHistoNode->getData();
 
 pDchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO");
 pDchAddress = pDchDaoNode->getData();
 
 pDchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode","DchDGO");
 pDchGeometry  = pDchDgoNode->getData();
 
 pDchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode","DchDCO");
 pDchCalibration = pDchDcoNode->getData();
 
 pDchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode","DchDNO");
 pDchNoise = pDchDnoNode->getData();
 
 timeZeroBBC = 0;
 
 if (analyzeTimeZeroBBC()) { // fill the noise Field if the BBC is there
   eventCounter++;
   fillNoiseField();
   return True;
 }else if(!flagBBC) {   // if bbc not there to fill the noise field you have to use the flag !!!
   eventCounter++;
   fillNoiseField();
   return True;
 }else{
   return False;
 }

}

PHBoolean mNewDchNoiseAnalyzer::callPAM(PHPointerList<PHNode>& pList)
{

  return True;

}

//---------------------------------------------------------------
//BBC
//---------------------------------------------------------------

PHBoolean mNewDchNoiseAnalyzer::analyzeTimeZeroBBC()
{
   BbcOut *bbcout = 0;
   PHTypedNodeIterator<BbcOut> bbciter(pTopNode);
   BbcOutNode_t *BbcOutNode = bbciter.find("BbcOut");
   if (BbcOutNode)     {
     bbcout = BbcOutNode->getData();
   }else{
     cout << "mNewDchNoiseAnalyserWARNING::analyzeTimeZeroBBC: BbcOut table not found "<< endl;
     return 0;
   }

   if (bbcout->isValid()) { 
     return True;
   }else {
     return False;
   }
   
}

//-------------------------------------------------------

PHBoolean mNewDchNoiseAnalyzer::analyzeReferenceChannel()
{
  
  int global,time;
  int totalRaws = rawTable->Entries();
  unsigned int arm,side;
  int cell;

  foundReferenceTime = False;
  referenceTime = -1;
  
  for (int i = 0; i < totalRaws; i++) {
    global = rawTable->getGlobal(i);
    arm    = rawTable->getArm(i);
    side   = rawTable->getSide(i);
    cell   = rawTable->getCell(i);
    time   = rawTable->getTime(i);
         
    if (side == NORTH && arm == WEST && cell == 81 && global == 12801) 
      { // west arm Reference Channel
	if (pDchHistogrammer) 
	  {
	    pDchHistogrammer->fillReferenceTime(time);
	  }
	if (!foundReferenceTime && time > 555 && time <580 ) 
	  {
	    referenceTime = time;
	    foundReferenceTime = True;
	  }
      }
  }
  return foundReferenceTime;
}

PHBoolean mNewDchNoiseAnalyzer::fillNoiseField()
{

  int totalRaws = rawTable->Entries();

  int edge;
  int arm, side, cell, plane;

  for (int i = 0; i < totalRaws; i++) {
    arm    = rawTable->getArm(i);
    side   = rawTable->getSide(i);
    plane  = rawTable->getPlane(i);
    cell   = rawTable->getCell(i);
    edge   = rawTable->getEdge(i);
           
    if (cell >= 0 && cell <=79) {  // cell bigger than 80 need to be eliminated
      if ( edge == 0) {
	noiseFieldEdge0[arm][side][plane][cell]++;
      }else {
	noiseFieldEdge1[arm][side][plane][cell]++;
      }
    }
  }
  return True;
}

PHBoolean mNewDchNoiseAnalyzer::fillNoiseNtuple()
{
  float ncountsPerEvent;
  if(pDchHistogrammer) {
    for (int a=0; a<2;a++) {
      for (int s=0; s<2;s++) {
	for (int p=0; p<40;p++) {
	  for (int c=0; c<80;c++) { 
	    ncountsPerEvent = (float)noiseFieldEdge0[a][s][p][c]/(float)eventCounter;

	    pDchHistogrammer->fillNoise(a,s,p,c,ncountsPerEvent,noiseFieldEdge1[a][s][p][c]);
	  }
	}
      }
    }
  }else {
    cout << "Histogrammer not found "<< endl;
  }
  return True;
}

PHBoolean mNewDchNoiseAnalyzer::writeFile(float threshold)
{
  int total = 0;
  int totalNoisy = 0;
  int totalDead  = 0;
  int globalindex = 0;
  float countsPerEvent;
  float totalCountsInChannel;
  int events = eventCounter;

  FILE *fp;
  FILE *dead;
  FILE *noisy;

  ostringstream string;
  string.str("DchNoise.Real");
  string << EventRunNumber;
  ostringstream string1;
  string1.str("DchNoise.Dead");
  string1 << EventRunNumber;
  ostringstream string2;
  string2.str("DchNoise.Noise");
  string2 << EventRunNumber;
  
  fp  = fopen(string.str().c_str(),"w");
  dead = fopen(string1.str().c_str(),"w");
  noisy  = fopen(string2.str().c_str(),"w");

  if (eventCounter > 0) {
  
    for (int s=0; s < numberOfSides; s++) {
      for (int a=0; a < numberOfArms;a++) {
	for (int p=0; p < numberOfPlanes;p++) {
	  for (int c=0; c< numberOfCells;c++) {
	    totalCountsInChannel = noiseFieldEdge0[a][s][p][c];
	    countsPerEvent = (float)(noiseFieldEdge0[a][s][p][c])/(float)(events);
	    // --------------------------------------------------------------
	    if (countsPerEvent > threshold ) {
	      pDchAddress->setSoft(a,s,p,c);
	      total++;
	      totalNoisy++;
	      globalindex = pDchAddress->getGlobalIndex()->getValue();
	      fprintf(fp,"%d  index  %d  counts %f noisy %d\n", 
		      total, globalindex, countsPerEvent, 1); // noisy
	      fprintf(noisy,"%d  index  %d  counts %f noisy %d\n", 
		      totalNoisy, globalindex, countsPerEvent, 1); // noisy
	      
	    }else if (totalCountsInChannel == 0 ) {
	      pDchAddress->setSoft(a,s,p,c);
	      total++;
	      totalDead++;
	      globalindex = pDchAddress->getGlobalIndex()->getValue();
	      fprintf(fp,"%d  index  %d  counts %f noisy %d\n", 
		      total, globalindex, countsPerEvent, 2); // dead 
	      fprintf(dead,"%d  index  %d  counts %f noisy %d\n", 
		      totalDead, globalindex, countsPerEvent, 2); // dead 
	      
	    }	  
	  }
	}
      }
    }
  }
  fclose(fp);
  fclose(dead);
  fclose(noisy);

  cout << "Finish to write the noisy channel to file "<< endl;

  return True;
 
}

PHBoolean mNewDchNoiseAnalyzer::writeCalibrationInDatabase(const char* fileName,int runStart, int runStop,float threshold)
{
  int total = 0;
  int globalindex = 0;
  float countsPerEvent;
  float totalCountsInChannel;
  int events = eventCounter;
 
  const char* descr="automatic from Noise Analyzer";
  
  PdbBankID bankID;
  bankID.setInternalValue(1);

  PHTimeStamp start;
  PHTimeStamp stop;
  RunToTime *rtt = RunToTime::instance();
  start = *(rtt->getBeginTime(runStart));
  stop  = *(rtt->getBeginTime(runStop ));
 
  pDchNoise->commit();
  pDchNoise->clearAndDestroy(); // clear old list and zero array 

  if (eventCounter > 0) {
  
    for (int s=0; s < numberOfSides; s++) {
      for (int a=0; a < numberOfArms;a++) {
	for (int p=0; p < numberOfPlanes;p++) {
	  for (int c=0; c< numberOfCells;c++) {
	    totalCountsInChannel = noiseFieldEdge0[a][s][p][c];
	    countsPerEvent = (float)(noiseFieldEdge0[a][s][p][c])/(float)(events);
	    // --------------------------------------------------------------
	    if (countsPerEvent > threshold || totalCountsInChannel == 0 ) { // noisy or dead
	      pDchAddress->setSoft(a,s,p,c);
	      total++;
	      globalindex = pDchAddress->getGlobalIndex()->getValue();

	      DchBasicNoise* basicNoise = new DchBasicNoise(globalindex,countsPerEvent,False);
	      pDchNoise->appendNoise(basicNoise);
	      pDchNoise->setNoisyDeadArray(globalindex,0); // dead
	      
	    }	  
	  }
	}
      }
    }
  }

  cout << "------------------------------------------------------- "<< endl;
  cout << "------------- DCH NOISE in the DB --------------------- "<< endl;
  cout << "------------------------------------------------------- "<< endl;
  cout << "EventRunNumber "<< EventRunNumber << endl;
  cout << "filename: " << fileName << endl;
  cout << "description : " << descr << endl;
  bankID.print();
  cout << "------------------------------------------------------- "<< endl;
  cout << "------------------------------------------------------- "<< endl;
  
  cout << "ATTENTION the  DATABASE is switched on"<< endl;
  pDchNoise->update(start,stop,fileName,bankID,descr);// attention
  pDchNoise->commit();

  pDchNoise->fetch(start,fileName,bankID);
  pDchNoise->commit();
  
  return True;

}



