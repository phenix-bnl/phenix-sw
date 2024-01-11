//
//  TKH--Updated to use new interface to 
//       calibration constants.
//            11-25-2001
//

#include "PHIODataNode.h"
#include "PHTable.hh"
#include "mNewDchDriftVelocityCalibrator.hh"
#include "PHDchGeometryObject.h"
#include "PHDchCalibrationObject.h"
#include "PHDchHistogrammer.hh"
#include "TF1.h"
#include "BbcCalib.hh"

#include "PHIODataNode.h"

#include "ZdcOut.h"
#include "BbcOut.h"
#include "T0Out.h"

#include "Event.h"

#include "RunToTime.hh"

#include <iostream>
#include <sstream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<BbcOut> BbcOutNode_t;
typedef PHIODataNode<ZdcOut> ZdcOutNode_t;
typedef PHIODataNode<T0Out> T0OutNode_t;

Double_t edgeFit(Double_t* , Double_t*);

mNewDchDriftVelocityCalibrator::mNewDchDriftVelocityCalibrator()
{
  dchGeometryObject  = 0;
  dchAddressObject  = 0;
  dchCalibrationObject =0;
  dchNoiseObject = 0;
  dchHistogrammer = 0;
  trackList = 0;
  intTimeBBC =0;
  intTimeZDC = 0;
  meanTimeBBC = 0;
  meanTimeZDC = 0;
  zdcEntries = 0;
  bbcEntries = 0;
  verbose = 0;
  leadingTrack = 0;
  leadingRaw = 0;
  trailingTrack = 0;
  trailingRaw = 0;
  calibrationStage = 0;

  for (int k=0; k<numberOfArms; k++) {
    fittedDrift[k] = 0;
    fittedT0[k] = 0;
    for (int l=0; l<7; l++) {
      startT0[l][k] = 0;
      startVD[l][k] = 0;
      finalT0[l][k] = 0;
      finalVD[l][k] = 0;
    }
  }
  zeroArray();
}

void 
mNewDchDriftVelocityCalibrator::zeroArray()
{
  for (int k=0; k<numberOfTimeBins; k++) {
    timeDistance0[k]=0;      // time is in  Bins (distance is in cm)  
    timeDistanceTrack0[k]=0; // time is in Bins(distance is in cm)  
    timeDistance1[k]=0;      // time is in  Bins (distance is in cm)  
    timeDistanceTrack1[k]=0; // time is in Bins(distance is in cm)  
 
    for (int l = 0; l<7; l++) {
      timeArray0[l][k] = 0;
      timeArray1[l][k] = 0;
      timeArrayScaled0[l][k] = 0;
      timeArrayScaled1[l][k] = 0;
    }
    
    integralTimeScaled0[k] = 0;
    integralTimeTrackScaled0[k] = 0;
    integralTimeScaled1[k] = 0;
    integralTimeTrackScaled1[k] = 0;

    distArrayScaled[k] =0;
    integralDistanceScaled[k] = 0;
   }
}

mNewDchDriftVelocityCalibrator::~mNewDchDriftVelocityCalibrator()
{
}

float 
mNewDchDriftVelocityCalibrator::getBbcTimeZeroOffset() 
{
  PHNodeIterator nodeIter(topNode);
  PHIODataNode<TObject> *BBCNode = (PHIODataNode<TObject>*)nodeIter.findFirst("PHIODataNode","BbcCalibPar");
  if (!BBCNode) {
    PHMessage("mNewDchCalibrator::getBbcTimeZeroOffset",PHWarning,"BbcCalibPar table not found T0 Offset not set");
    return 0;
  }
  BbcCalib* bbcCalib = (BbcCalib*)(BBCNode->getData());
  if (bbcCalib) {
    float bbcTimeZeroOffset = bbcCalib->getTZeroOff()->getCalibPar(0)->getPeakChannel();
    if (verbose) cout << "Time Zero OFFset is (ns): " << bbcTimeZeroOffset << endl;
    return bbcTimeZeroOffset;
  }else {
    return 0;
  }
}

PHBoolean 
mNewDchDriftVelocityCalibrator::getBBCandZDCInfo()
{
  BbcOut* bbcout =  0;
  ZdcOut* zdcout =  0;

   PHTypedNodeIterator<BbcOut>    bbciter(topNode);
   BbcOutNode_t *BbcOutNode = bbciter.find("BbcOut");
   if (BbcOutNode)     {
       bbcout = BbcOutNode->getData();
   }else{
     if (verbose) {
            PHMessage("mNewDchDriftVelocityCalibrator::getBBCandZDCInfo",
                      PHWarning,"BbcOut table not found ");
     }
     return False;
   }
 
   PHTypedNodeIterator<ZdcOut>    zdciter(topNode);
   ZdcOutNode_t *ZdcOutNode = zdciter.find("ZdcOut");
   if (ZdcOutNode){
     zdcout = ZdcOutNode->getData();
   }else{
     if (verbose) {
       PHMessage("mNewDchDriftVelocityCalibrator::getBBCandZDCInfo",
		 PHWarning,"ZdcOut table not found ");
     }
     return False;
   }
   
  float timeBBC = -9999;
  float timeZDC = -9999;
  float timeZDC1 = -9999;
  float timeZDC2 = -9999;

  getBbcTimeZeroOffset();
  if (bbcout) timeBBC =bbcout->get_TimeZero()+getBbcTimeZeroOffset();
  if (zdcout) {
    timeZDC = (zdcout->get_Timing(0) + zdcout->get_Timing(1))/2.;
    timeZDC1 = zdcout->get_Timing(0);
    timeZDC2 = zdcout->get_Timing(1);
  }

  if (verbose) cout << timeBBC << " timing " << timeZDC << endl;

  if (bbcout->isValid()) {
    intTimeBBC += timeBBC;
    bbcEntries++;
    meanTimeBBC = intTimeBBC/bbcEntries;
  }
  if (timeZDC> -100) {
    intTimeZDC += timeZDC;
    zdcEntries++;
    meanTimeZDC = intTimeZDC/zdcEntries;
  }

  if (dchHistogrammer && dchHistogrammer->flagInit) {
    dchHistogrammer->fillT0s(timeBBC,timeZDC,timeZDC1, timeZDC2);
  }else {
    cout << "Histogrammer not initialized "<< endl;
  }

  return True;
}

PHBoolean 
mNewDchDriftVelocityCalibrator::event(PHCompositeNode *root) 
{
  PHObjectNode_t* phob;
 topNode = root;  
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root);

 PHDataNode<PHDchAddressObject>* dchDaoNode;
 PHDataNode<PHDchGeometryObject>* dchDgoNode;
 PHDataNode<PHDchCalibrationObject>* dchDcoNode;
 PHDataNode<PHDchNoiseObject>* dchDnoNode;
 PHDataNode<PHDchHistogrammer>* dchHistoNode;

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchRawTablev1"));
 rawTable = dynamic_cast < DchRawTable * >(phob->getData ());
 if (!rawTable) {
   cout << "DchRawTablev1 not found "<< endl;
   return False;
 }

 phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchHitLineTablev1"));
 hitLineTable = dynamic_cast < DchHitLineTable * >(phob->getData ());
 if (!hitLineTable) {
   cout << "DchHitLineTablev1 not found "<< endl;
   return False;
 }
 
  PHDataNode<PHPointerList<DchTrackInfo> >* tmpTrackNode =
     (PHDataNode<PHPointerList<DchTrackInfo> >*)i.findFirst("PHDataNode","DchTrackInfo");
  if (tmpTrackNode) {
    trackList = tmpTrackNode->getData();
  }else {
    trackList = 0;
    PHMessage("mDchDriftVelocityCalibrator", PHWarning , "Need the Analyzer");
  }

  // try to extract the information from the PRDF event 
 
  PHTimeStamp* when = 0;
  Event *eventPRDF;
  PHDataNode<Event>* eventNodePRDF;

  eventPRDF = 0;
  eventNodePRDF = (PHDataNode<Event>*)i.findFirst("PHDataNode", "PRDF");
  if (eventNodePRDF)
    {
      eventPRDF = eventNodePRDF->getData();
      if (eventPRDF)
	{
	  if (verbose)
	    {
	      cout << "Run number " << eventPRDF->getRunNumber() << endl;
	    }
	  EventRunNumber = eventPRDF->getRunNumber();
	  when = new PHTimeStamp(eventPRDF->getTime());
	  if (when)
	    {
	      if (verbose)
		{
		  cout << "when TimeStamp: " << *(when) << endl;
		}
	      delete when;
	    }
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
    
  // find the Geometry Objects

  dchHistoNode = (PHDataNode<PHDchHistogrammer>*)i.findFirst("PHDataNode","DchHisto");
  dchHistogrammer = dchHistoNode->getData();
  
  dchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO");
  dchAddressObject = dchDaoNode->getData();
 
  dchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode","DchDGO");
  dchGeometryObject = dchDgoNode->getData();
  
  dchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode","DchDCO");
  dchCalibrationObject = dchDcoNode->getData();
     
  dchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode","DchDNO");
  dchNoiseObject = dchDnoNode->getData();

  getBBCandZDCInfo(); // information from ZDC and BBC
  return callPAM(nodes);
  
}

PHBoolean 
mNewDchDriftVelocityCalibrator::callPAM(PHPointerList<PHNode> &nl)
{
  //  Only will use event if t0Out->IsBBC
  T0Out *T0out = 0;
  //float time;
  PHTypedNodeIterator<T0Out> t0iter(topNode);
  T0OutNode_t *T0OutNode = t0iter.find("T0Out");
  if (T0OutNode){
    T0out = T0OutNode->getData();
  }else{
    PHMessage("mNewDchDVCalibrator: TimeDis on Hits",PHWarning,
	      "T0Out table not found ");
    return False; 
  }
  if (T0out->isBbcT0()){    
    if (hitLineTable) {
      initializeStartValues();
      timeDistributionOnHits();
    }
  } else {
  }
  
  return (T0out->isBbcT0());
}

void 
mNewDchDriftVelocityCalibrator::initializeStartValues()
{

  guessT0[EAST] = dchCalibrationObject->getNominalT0(EAST);
  guessVD[EAST] = dchCalibrationObject->getNominalDriftVelocity(EAST);
				     
  guessT0[WEST] = dchCalibrationObject->getNominalT0(WEST);
  guessVD[WEST] = dchCalibrationObject->getNominalDriftVelocity(WEST);

  for (int k=0; k<7; k++) {
    startT0[k][EAST] = finalT0[k][EAST];
    startVD[k][EAST] = finalVD[k][EAST];
    startT0[k][WEST] = finalT0[k][WEST];
    startVD[k][WEST] = finalVD[k][WEST];
  }

}

PHBoolean 
mNewDchDriftVelocityCalibrator::timeDistributionOnHits()
{
  unsigned int arm;
  int time,plane;
  int timeScaled[2];
  int timeScaledArm[2];
  float driftDistanceRef = dchGeometryObject->getDriftDistance(39);
  float driftDistance;
  int numberOfHits = hitLineTable->Entries();

  
  
  if (verbose) {
    cout <<"Mean BBC used for calibration: " << dchCalibrationObject->getBbcMean() << endl;
    cout <<"Mean ZDC used for calibration: " << dchCalibrationObject->getZdcMean() << endl;
    cout <<"Guess T0 (EAST) "<< guessT0[EAST] << " Guess VD : " << guessVD[EAST] << endl;
    cout <<"Guess T0 (WEST) "<< guessT0[WEST] << " Guess VD : " << guessVD[WEST] << endl;
  }
  int type = 0;
  for (int i=0; i < numberOfHits; i++) {
    arm = hitLineTable->getArm(i);
    plane = hitLineTable->getPlane(i);
    if (plane == 0 || plane ==11 || plane ==20 || plane ==31 ) continue;
    type = getWireType(plane); // return the module type (wire type from 1-6)
    
    driftDistance = dchGeometryObject->getDriftDistance(plane);
    time = hitLineTable->getTime1(i);
    
    timeScaledArm[arm] = (int)(((float)(time - startT0[0][arm])*(float)((driftDistanceRef)/(driftDistance)))+0.5); // nearest int value
    timeScaled[arm] = (int)(((float)(time - startT0[type][arm])*(float)((driftDistanceRef)/(driftDistance)))+0.5); // nearest int value
    
    if (arm == EAST) {
      if (time>-1 && time < numberOfTimeBins) {
	timeArray0[type][time]+= 1.;// modules
	if (type == 1 || type == 4) timeArray0[0][time]+= 1.;// everything
	if (timeScaled[arm] > -1 && timeScaled[arm] < numberOfTimeBins) {
	  timeArrayScaled0[type][(timeScaled[arm])]+=1.;
	  if (type ==1 || type ==4) timeArrayScaled0[0][(timeScaledArm[arm])]+=1.;
	  }else {
	  }
      }
    }else if (arm == WEST) {
      if (time>-1 && time < numberOfTimeBins) {
	timeArray1[type][time]+= 1.;
	if (type == 1 || type == 4) timeArray1[0][time]+= 1.;
	if (timeScaled[arm] > -1 && timeScaled[arm] < numberOfTimeBins) {
	  timeArrayScaled1[type][(timeScaled[arm])]+= 1.;
	  if (type == 1 || type == 4) timeArrayScaled1[0][(timeScaledArm[arm])]+= 1.;
	}else {
	}
      }     
    }		
    
  }

  return True;   
} 

PHBoolean 
mNewDchDriftVelocityCalibrator::timeDistributionOnTracks()
{
  int arm;
  int time,plane,timeScaled;
  float driftDistanceRef = dchGeometryObject->getDriftDistance(39);
  float driftDistance;
  float guessT0[2];
  DchRawInfo* rawInfo;
  DchHitInfo* hitInfo;
  DchTrackInfo* trackInfo;
  //----------------------------------------------------------------------------------------------
  // Need the Calibrator to transfrom the time to distance and check the proportional Reagion !!!
  //----------------------------------------------------------------------------------------------

  guessT0[EAST] = dchCalibrationObject->getNominalT0(EAST);
  guessT0[WEST] = dchCalibrationObject->getNominalT0(WEST);

  int numberOfTracks = trackList->length();
  for (int i=0; i < numberOfTracks; i++) {
     trackInfo = (*trackList)[i];
     if (trackInfo) {
       for (unsigned int hit=0; hit < trackInfo->getHitInfoList().length(); hit++) {
	 hitInfo =  trackInfo->getHitInfo(hit);
	 if (hitInfo) {
	   rawInfo = hitInfo->getRaw1();
           plane = rawInfo->getPlane();
	   arm = hitInfo->getArm() ;
           driftDistance = dchGeometryObject->getDriftDistance(plane);
           if (arm == 0) {
	     time = rawInfo->getTime();
	     timeScaled = (int)(((float)(time-guessT0[arm])*(float)((driftDistanceRef)/(driftDistance)))+0.5); // nearest int value           
	     timeArrayTrack0[time]+=1.;
	     if (timeScaled < numberOfTimeBins) {
	       timeArrayTrackScaled0[timeScaled]+=1.;
	     }
	   }else {
	     time = rawInfo->getTime();
	     timeScaled = (int)(((float)(time-guessT0[arm])*(float)((driftDistanceRef)/(driftDistance)))+0.5); // nearest int value           
	     timeArrayTrack1[time]+=1.;
	     if (timeScaled < numberOfTimeBins) {
	       timeArrayTrackScaled1[timeScaled]+=1.;
	     }
	   }
	 }
       }
     }
  }
  return True;
}

PHBoolean 
mNewDchDriftVelocityCalibrator::generateDistanceDistribution()
{
 
  float driftDistanceRef = dchGeometryObject->getDriftDistance(39);
  float driftDistance;
    
  for(int i=0; i <numberOfTimeBins; i++) {
    driftDistance = (i)*driftDistanceRef/(numberOfTimeBins); // check !!! 
    if (driftDistance < propWidth) {
      distArrayScaled[i]     = distArrayScaled[i] + 2.;      // add 2 if in the proportional region
    }else{
      distArrayScaled[i]+=1.;                            // add 1 if not in the proprotional region
    }
  }
  return True;
}

PHBoolean 
mNewDchDriftVelocityCalibrator::integrate()
{
  //--------------------------------------------------------------------
  // only for last event : Normalize the distributions and fill them
  // -------------------------------------------------------------------

  int i,k;
  float totalTimeArray0 = 0;
  float totalTimeArray1 = 0;
  float totalTimeArrayScaled0 = 0;
  float totalTimeArrayScaled1 = 0;
  
  for (k=0; k<7; k++) {
    // integrate and normalize the arrays  
    totalTimeArray0 = 0;
    totalTimeArray1 = 0;
    totalTimeArrayScaled0 = 0;
    totalTimeArrayScaled1 = 0;

    for (i=0; i < numberOfTimeBins; i++) {

      totalTimeArray0 += timeArray0[k][i];
      totalTimeArrayScaled0 += timeArrayScaled0[k][i];

      totalTimeArray1 += timeArray1[k][i];
      totalTimeArrayScaled1 += timeArrayScaled1[k][i];
    }
 
    for (i=0;i < numberOfTimeBins; i++) {
      if (totalTimeArray0 > 0) timeArray0[k][i] = timeArray0[k][i]/totalTimeArray0;
      if (totalTimeArray1 > 0) timeArray1[k][i] = timeArray1[k][i]/totalTimeArray1;
      if (totalTimeArrayScaled0 > 0) timeArrayScaled0[k][i] = timeArrayScaled0[k][i]/totalTimeArrayScaled0;
      if (totalTimeArrayScaled1 > 0) timeArrayScaled1[k][i] = timeArrayScaled1[k][i]/totalTimeArrayScaled1;
    }
  }
  if (verbose) cout << "Distance -Time relation not active "<< endl;
  if (dchHistogrammer && dchHistogrammer->flagInit) { 
    fillHistogramms(); // filling the histramms to perf  
  }
  zeroArray(); // zero the arrays 
  return True;
}

PHBoolean 
mNewDchDriftVelocityCalibrator::fitTimeDistanceRelation(int minTime,int maxTime,double T0,double VD)
{
  cout << "not uptodate "<< endl;

  return False;
}

PHBoolean 
mNewDchDriftVelocityCalibrator::calculateCalibrationFromEdges(short arm,
							      int minTimel, int maxTimel, 
							      int minTimet, int maxTimet,
							      short mod)
{

  float driftDistanceRef = dchGeometryObject->getDriftDistance(39);
  int numberOfParameters = 4;
  float halfGuessLeading = minTimel + (maxTimel - minTimel)/2.;
  float halfGuessTrailing= minTimet + (maxTimet - minTimet)/2.; 

  if (verbose) {
    cout << "Guess Leading T0 " << halfGuessLeading << endl;
    cout << "Guess Trailing T0 "<< halfGuessTrailing << endl;
  }
  binSize = dchCalibrationObject->getBinSize(); // general calibration  
  
  if (calibrationStage > 1 && dchHistogrammer) {
    delete dchHistogrammer->getLeadingRaw(arm,mod);
    delete dchHistogrammer->getTrailingRaw(arm,mod);
  }
  delete trailingTrack;
  delete leadingTrack;

  if (dchHistogrammer && dchHistogrammer->flagInit) {
    Double_t parRt[4];
    Double_t parRl[4];

    TH1F    *timeDistributionScaled      = dchHistogrammer->getTimeDistributionScaled(arm,mod);
    TH1F    *timeDistribution            = dchHistogrammer->getTimeDistribution(arm,mod);

    char buff[256];   
    // Fitting the trailing edge for Raw hits
    sprintf(buff,"trailingRaw%d%d",arm,mod);
    trailingRaw   = new TF1(buff,edgeFit,minTimet,maxTimet,numberOfParameters);
    trailingRaw->SetParameters(halfGuessTrailing, 20.,0.001,0.);
    trailingRaw->SetParNames("Half height","slope","Total","Noise");
    timeDistributionScaled->Fit(buff,"R0");
    trailingRaw->SetLineColor(kRed);
    for (int k = 0; k < numberOfParameters; k++) { 
      parRt[k] = trailingRaw->GetParameter(k);
    }

    //fitting the leading edge for Raw Hits
    sprintf(buff,"leadingRaw%d%d",arm,mod);
    leadingRaw   = new TF1(buff,edgeFit,minTimel,maxTimel,numberOfParameters);
    leadingRaw->SetParameters(halfGuessLeading,-3.,0.001,0.);
    leadingRaw->SetParNames("Half height","slope","Total","Noise");
    timeDistribution->Fit(buff,"R0");
    leadingRaw->SetLineColor(kRed);
    for (int k = 0; k < numberOfParameters; k++) { 
      parRl[k] = leadingRaw->GetParameter(k);
    }
 
    halfampl[mod][arm] = parRl[0];
    halfampt[mod][arm] = parRt[0];
    slopel[mod][arm] = parRl[1];
    slopet[mod][arm] = parRt[1];
    ampl[mod][arm] = parRl[2];
    ampt[mod][arm] = parRt[2];
    noisel[mod][arm] = parRl[3];
    noiset[mod][arm] = parRt[3];

    float leadingTimeRaw    = parRl[0];
    float trailingTimeRaw   = parRt[0];
    float T0Raw   = leadingTimeRaw;
    float VDRaw = 0;
    if (trailingTimeRaw > 0) {
      VDRaw   = driftDistanceRef/(trailingTimeRaw);
    }

    dchHistogrammer->setLeadingRaw(arm,mod,leadingRaw);
    dchHistogrammer->setTrailingRaw(arm,mod,trailingRaw);
    
    finalT0[mod][arm] = T0Raw;
    finalVD[mod][arm] = VDRaw;

    printCalibration(arm,mod);
    
  } // if histogrammer
  return True;
}

void 
mNewDchDriftVelocityCalibrator::printCalibration(short arm, short mod) 
{
  float driftDistanceRef = dchGeometryObject->getDriftDistance(39);
   
  if (verbose) {
    cout << "ARM: " << arm << " Module: " << mod << endl;
    cout << "T0 for RAW (bins) :    " << finalT0[mod][arm] << endl;
    cout << "T0 for Raw (ns)  :     " << finalT0[mod][arm]*binSize << endl;
    cout << "VD for Raw (cm/Bins):  " << finalVD[mod][arm]  << endl;
    cout << "VD for Raw (cm/ns):    " << finalVD[mod][arm]/binSize << endl;  // BinSize
    cout << "TF for Raw (bins):     " << driftDistanceRef/finalVD[mod][arm] << endl;
    cout << "driftDistanceRef :     " << driftDistanceRef << endl; 
  }
  char buff[30]; 
  sprintf(buff,"fitting%d%d",arm,mod);
  cout << "Filename for output is : " << buff << endl;   

  FILE* a = fopen(buff,"w");
  fprintf(a, "ARM  %d   Module %d \n",arm,mod);
  fprintf(a,"T0 from RAW(bins): %f \n ",finalT0[mod][arm]);
  fprintf(a,"T0 from RAW(ns)  : %f \n ",finalT0[mod][arm]*binSize);
  fprintf(a,"Falling T from RAW(bins): %f \n ",driftDistanceRef/finalVD[mod][arm]);
  fprintf(a,"driftDistanceRef : %f \n ",driftDistanceRef);

  fprintf(a,"VD from RAW(cm/bins): %f \n ",finalVD[mod][arm]);
  fprintf(a,"VD from RAW(cm/ns): %f \n ",finalVD[mod][arm]/binSize);
  fprintf(a,"Arm %d Module %d  Half %f Slope %f Tot %f Noise %f \n", 
	  arm, mod, halfampl[mod][arm], slopel[mod][arm], ampl[mod][arm], noisel[mod][arm]);
  fprintf(a,"Arm %d Module %d  Half %f Slope %f Tot %f Noise %f \n",
	  arm, mod, halfampt[mod][arm], slopet[mod][arm], ampt[mod][arm], noiset[mod][arm]);
  fprintf(a,"Mean BBC : %f \n", meanTimeBBC);
  fprintf(a,"Mean ZDC : %f \n", meanTimeZDC);
  fclose(a);
  
}

Double_t 
edgeFit(Double_t* x , Double_t* par )
{
  Double_t den = 0;
  Double_t fitval = 0;
  if (par[1]) {
    den = (exp((x[0] - par[0])/par[1]) + 1);  // denominator
    fitval = par[3] + par[2]/den;
  }
  return fitval;		
}

PHBoolean 
mNewDchDriftVelocityCalibrator::fillHistogramms()
{
  int i,mod,arm;
  TH1F* hist, *histScaled; 
  
  if (dchHistogrammer && dchHistogrammer->flagInit) {
    for (arm=0; arm<2; arm++) { // loop over arms
      for (mod=0; mod<7; mod++) {  // loop over modules
	 histScaled = dchHistogrammer->getTimeDistributionScaled(arm,mod);
	 hist       = dchHistogrammer->getTimeDistribution(arm,mod);
	 for (i=0; i< numberOfTimeBins; i++) { // loop over timebins
	   if (arm == 0) {
	     hist->SetBinContent(i+1,timeArray0[mod][i]);
	     histScaled->SetBinContent(i+1,timeArrayScaled0[mod][i]);
	   }else {
	     hist->SetBinContent(i+1,timeArray1[mod][i]);
	     histScaled->SetBinContent(i+1,timeArrayScaled1[mod][i]);
	   }
	 }	
      }
    }
    return True;
   }else {
    return False;  
  }
}

int 
mNewDchDriftVelocityCalibrator::getWireType(int plane)
{
  if (plane <12) {
    return 1;  // X1 wire
  }else if (plane <16) {
    return 2; // U1 wire
  }else if (plane <20) {
    return 3; // V1 wire
  }else if (plane <32) {
    return 4; // X2 wire
  }else if (plane <36) {
    return 5; // U2 wire
  }else if (plane <40) {
    return 6; // V2 wire
  }

  return 0;
}

void 
mNewDchDriftVelocityCalibrator::writeFile()
{

  float t0East = finalT0[0][0];
  float t0West = finalT0[0][1];
  float vdEast = finalVD[0][0];
  float vdWest = finalVD[0][1];

  dchCalibrationObject->setRunNumber(EventRunNumber);
  dchCalibrationObject->setBbcMean(meanTimeBBC);
  dchCalibrationObject->setZdcMean(meanTimeZDC);
  dchCalibrationObject->setBbcCounts(bbcEntries);
  dchCalibrationObject->setZdcCounts(zdcEntries);

  ostringstream fname;
  fname << "DchCalibration.Real" << EventRunNumber;
  FILE* a = fopen(fname.str().c_str(),"w");

  fprintf(a, "8 headerParameters \n");
  fprintf(a, "%d runNumber\n",dchCalibrationObject->getRunNumber());
  fprintf(a, "%f  bbcMean \n",dchCalibrationObject->getBbcMean() );
  fprintf(a, "%f  zdcMean \n",dchCalibrationObject->getZdcMean() );
  fprintf(a, "%d  bbcCounts \n", dchCalibrationObject->getBbcCounts());
  fprintf(a, "%d  zdcCounts \n", dchCalibrationObject->getZdcCounts());
  fprintf(a, "2  nChannels \n");
  fprintf(a, "%f binSize \n",  dchCalibrationObject->getBinSize());

  fprintf(a, "%f t0East \n",  t0East);
  fprintf(a, "%f vdEast \n",  vdEast);
			      
  fprintf(a, "%f t0West \n",  t0West);
  fprintf(a, "%f vdWest \n",  vdWest);

  fprintf(a, "%f t0East used \n", dchCalibrationObject->getNominalT0(EAST));
  fprintf(a, "%f vdEast used \n", dchCalibrationObject->getNominalDriftVelocity(EAST));

  fprintf(a, "%f t0West used \n", dchCalibrationObject->getNominalT0(WEST));
  fprintf(a, "%f vdWest used \n", dchCalibrationObject->getNominalDriftVelocity(WEST));

  fclose(a);

}

PHBoolean 
mNewDchDriftVelocityCalibrator::writeCalibrationInDatabase(const char* fileName,
							   int runStart, int runStop)
{

  const char* descr="automatic from Calibrator";
  
  PdbBankID bankID;
  bankID.setInternalValue(1);

  PHTimeStamp start;
  PHTimeStamp stop;
  RunToTime *rtt = RunToTime::instance();
  start = *(rtt->getBeginTime(runStart));
  stop  = *(rtt->getBeginTime(runStop ));

  dchCalibrationObject->commit();

  float t0East = finalT0[0][0];
  float t0West = finalT0[0][1];
  float vdEast = finalVD[0][0];
  float vdWest = finalVD[0][1];

  float arm0t0old = dchCalibrationObject->getNominalT0(EAST);
  float arm1t0old = dchCalibrationObject->getNominalT0(WEST);
  float arm0vdold = dchCalibrationObject->getNominalDriftVelocity(EAST);
  float arm1vdold = dchCalibrationObject->getNominalDriftVelocity(WEST);

  dchCalibrationObject->setNominalT0(EAST,t0East);
  dchCalibrationObject->setNominalDriftVelocity(EAST,vdEast);
  dchCalibrationObject->setNominalT0(WEST,t0West);
  dchCalibrationObject->setNominalDriftVelocity(WEST,vdWest);

  unsigned int arm;
  float delta_t0, delta_vd;
  PdbIndex* index = dchAddressObject->getGlobalIndex();
  for (int glo = 0; glo < index->getNumberOf(); glo++) {
    dchAddressObject->setGlobalIndex(glo);
    arm = dchAddressObject->getArm()->getValue();
    if (arm == EAST) {
      delta_t0 = dchCalibrationObject->getExplicitT0(index) - arm0t0old;
      delta_vd = dchCalibrationObject->getExplicitDriftVelocity(index) -arm0vdold;
    }else {			       
      delta_t0 = dchCalibrationObject->getExplicitT0(index) - arm1t0old;
      delta_vd = dchCalibrationObject->getExplicitDriftVelocity(index) -arm1vdold;
    }
    dchCalibrationObject->setExplicitCalibration(index, t0East+delta_t0, vdEast+delta_vd);

    if (glo == 0 || glo ==3200 || glo ==6400 || glo ==9600) {
    }
  }

  dchCalibrationObject->setRunNumber(EventRunNumber);
  dchCalibrationObject->setBbcMean(meanTimeBBC);
  dchCalibrationObject->setZdcMean(meanTimeZDC);
  dchCalibrationObject->setBbcCounts(bbcEntries);
  dchCalibrationObject->setZdcCounts(zdcEntries);

  FILE* a = fopen("DchCalibration.txt","w");

  fprintf(a, "8 headerParameters \n");
  fprintf(a, "%d runNumber\n",dchCalibrationObject->getRunNumber());
  fprintf(a, "%f  bbcMean \n",dchCalibrationObject->getBbcMean() );
  fprintf(a, "%f  zdcMean \n",dchCalibrationObject->getZdcMean() );
  fprintf(a, "%d  bbcCounts \n", dchCalibrationObject->getBbcCounts());
  fprintf(a, "%d  zdcCounts \n", dchCalibrationObject->getZdcCounts());
  fprintf(a, "2  nChannels \n");
  fprintf(a, "%f binSize \n",  dchCalibrationObject->getBinSize());
  
  fprintf(a, "%f t0East \n",  dchCalibrationObject->getNominalT0(EAST));
  fprintf(a, "%f vdEast \n",  dchCalibrationObject->getNominalDriftVelocity(EAST));
  fprintf(a, "%f t0West \n",  dchCalibrationObject->getNominalT0(WEST));
  fprintf(a, "%f vdWest \n",  dchCalibrationObject->getNominalDriftVelocity(WEST) );

  fclose(a);

  // print on screen 
  cout << "------------------------------------------------------- "<< endl;
  cout << "---------- DCH  CALIBRATION in the DB ----------------- "<< endl;
  cout << "------------------------------------------------------- "<< endl;
  cout << "EventRunNumber "<< EventRunNumber << endl;
  cout << "filename: " << fileName << endl;
  cout << "description : " << descr << endl;
  bankID.print();

  cout << " Run number : " << dchCalibrationObject->getRunNumber() << endl;
  cout << " BBC MEan : " << dchCalibrationObject->getBbcMean() << endl;
  cout << " ZDC MEan : " << dchCalibrationObject->getZdcMean() << endl;
  cout << " BBC Counts : " << dchCalibrationObject->getBbcCounts() << endl;
  cout << " ZDC Counts : " << dchCalibrationObject->getZdcCounts() << endl;
  cout << " Bin Size : " << dchCalibrationObject->getBinSize() << endl;

  cout << "T0 EAST : "<<  dchCalibrationObject->getNominalT0(EAST) << endl;
  cout << "VD EAST : "<<  dchCalibrationObject->getNominalDriftVelocity(EAST) << endl;

  cout << "T0 WEST : "<<  dchCalibrationObject->getNominalT0(WEST) << endl;
  cout << "VD WEST : "<<  dchCalibrationObject->getNominalDriftVelocity(WEST) << endl;
  
  cout << "------------------------------------------------------- "<< endl;
  cout << "------------------------------------------------------- "<< endl;

  cout << "ATTENTION the  DATABASE is switched on"<< endl;
  dchCalibrationObject->update(start,stop,fileName,bankID,descr);// attention
  dchCalibrationObject->commit();

  dchCalibrationObject->fetch(start,fileName,bankID);
  dchCalibrationObject->commit();

  return True;
}
