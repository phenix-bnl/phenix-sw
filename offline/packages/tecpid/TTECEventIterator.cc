#include "TTECEventIterator.hh"

#include "gsl/gsl_math.h"
#include "fileEventiterator.h"
#include "recoConsts.h"
#include "TecOutV7.hh"

#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject>            PHObjectNode_t;
typedef PHIODataNode<TObject>             ObjectNode_t;
typedef PHIODataNode<TecOutV1>            TecOutNode_t;

ClassImp(TTECEventIterator)

/**
 * Create a new iterator for the given PRDF file.
 *
 * @param prdfFileName Name of the file to load data from.
 * @param runNumber The run number.
 * @param isSimulation Is this simulated data?
 * @param useObjy Should info be fetched from the database?
 */
TTECEventIterator::TTECEventIterator(char* prdfFileName,
				     Int_t runNumber, 
				     Bool_t isSimulation,
				     Bool_t useObjy) 
  : TQObject()
{
  fPrdfFileName = new char[strlen(prdfFileName) + 1];
  strcpy(fPrdfFileName, prdfFileName);

  fIsSimulation = isSimulation;
  fUseObjy = useObjy;
  fVerbosity = 12;

  fIsPastLast = kFALSE;

  fEventNumber = 0;
  fRunNumber = runNumber;

  fHasDonePHOOLInit = kFALSE;  
  fHasDoneFirstEventInit = kFALSE;

  fTecAddress = 0;
  fTecGeometry = 0;
  fTecCalibration = 0;
  fMainIter = 0;
  fEventIter = 0;

  for (int i = 0; i < 8; i++) 
    {
      fEvents[i] = 0;
    }
}

///Clean up.
TTECEventIterator::~TTECEventIterator()
{
  for (int i = 0; i < 8; i++)
    {
      delete fEvents[i];
    }
  delete [] fPrdfFileName;
  CleanUpPHOOL();
}

//static methods

void 
TTECEventIterator::GetMinMax(int *MinBin, int *MaxBin,
			     TecCalibrationObject * TCO)
{

  // Prepare arrays
  //Could memset be used here?
  for (int i = TECMAXINDEX; i < TECMAXINDEX*2; i++) 
    {
      MinBin[i] = 999;
      MaxBin[i] = -999;
    }

  // Find first and last time bins from Calibration Object
  for (int i = 0; i < TECMAXINDEX; i++) 
    {
      MinBin[i] = TCO->getFirstTimeBin (i);
      MaxBin[i] = TCO->getLastTimeBin (i);
    }
}

void 
TTECEventIterator::GetMidphi(float* MidPhi,
			     float *midphi,
			     float *sinalpha,
			     TecGeometryObject * TGO)
{

  // Calculate phi for the center of each Tec chamber. EAST arm only

  //Could memset be used here?
  for (int i = 0; i < TECMAXINDEX; i++) 
    { 
      MidPhi[i] = 0.0; 
      sinalpha[i] = 0.0;
    }
  for (int i = 0; i < TECMAXSECT*2; i++) 
    {
      midphi[i] = 0.0;
    }

  for (int isect = 0; isect < TECMAXSECT; isect++)
    {
      for (int iplane = 0; iplane < TECMAXPLANE; iplane++)
        {
          for (int iside = 0; iside < TECMAXSIDE; iside++)
            {
              int index =
                iside + iplane * TECMAXSIDE +
                isect * TECMAXSIDE * TECMAXPLANE;
              PHPanel tecpanel = TGO->getTecPanel (index);
              PHVector normal = tecpanel.getNormal ();
              float x = normal.getX ();
              float y = normal.getY ();
              if (x != 0) 
		{
                  MidPhi[index] = M_PI + atan (y / x);
		}
              else 
		{
                  MidPhi[index] = M_PI;
		}
              sinalpha[index] = -sin(MidPhi[index]);
            }
        }
    }

  //Calculate phi for the center of each Tec sector/side. 
  //Take average for all 12 planes.
  for (int isect = 0; isect < TECMAXSECT; isect++) 
    {
      
      midphi[isect]=0.;
      
      for (int iside = 0; iside < TECMAXSIDE; iside++) 
	{
	  for (int iplane = 0; iplane < TECMAXPLANE; iplane++) 
	    {
	      int index = iside + iplane*TECMAXSIDE 
		+ isect*TECMAXSIDE*TECMAXPLANE;
	      midphi[isect] += MidPhi[index];
	    }
	}
      midphi[isect]=midphi[isect]/(TECMAXPLANE*TECMAXSIDE);
      midphi[isect] = M_PI - midphi[isect];
    
    }
}

void 
TTECEventIterator::CalcXYFromBin (float relativebin, 
				  float Xwire, float Ywire, 
				  float Sinus, 
				  float& Xpos, float& Ypos)
{
  float Cosinus = sqrt (1.0 - Sinus * Sinus);

  // East Arm only !!!
  Xpos =
    Xwire + (TecGeometryObject::get_ywidth () - 0.3) * Cosinus * relativebin;
  Ypos =
    Ywire + (TecGeometryObject::get_ywidth () - 0.3) * Sinus * relativebin;

}


///Set up the PHOOL classes.
void 
TTECEventIterator::InitializePHOOL ()
{

  //Set up the node tree.

  fTopNode = new PHCompositeNode("TOP");

  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  fTopNode->addNode(parNode);

  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  fTopNode->addNode(dcmNode);

  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  fTopNode->addNode(dstNode);

  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  fTopNode->addNode(tecNode);

  fMainIter = new PHNodeIterator(fTopNode);

  //Set up input and output files.

  fEvent = 0;

  fMainIter->addNode(new PHDataNode<Event>(fEvent, "PRDF"));

  fEventIter = new fileEventiterator(fPrdfFileName);

  //Set up the modules.

  fMTecUnpack = new mTecUnpackModule();

  fMTecCalib = new mTecCalibModule();

  fMTecHoughTrack = new mTecHoughTrackModule();

  fTecAddress = new TecAddressObject();
  fTecGeometry = new TecGeometryObject();
  fTecCalibration = new TecCalibrationObject();

  //Initialize the tables.

  PHIODataNode<TObject>* TecDetGeoNode = new PHIODataNode<TObject>(fTecGeometry,"TecGeometry");
  parNode->addNode(TecDetGeoNode);

  PHDataNode<TecCalibrationObject>* TecCalibNode = new PHDataNode<TecCalibrationObject>(fTecCalibration,"TecCalibration");
  parNode->addNode(TecCalibNode);

  fDTecOut = new TecOutV1();
  PHIODataNode<PHObject>* TecOutV1Node = new PHIODataNode<PHObject>(fDTecOut,"TecOutV1","PHObject");
  tecNode->addNode(TecOutV1Node);

  TecOut *tecoutshort = new TecOutV7();
  PHObjectNode_t *TecOutShortNode = new PHObjectNode_t(tecoutshort, "TecOutShort", "PHObject");
  dstNode->addNode(TecOutShortNode);

  TecOut *tecout = new TecOutV7();
  PHObjectNode_t *TecOutNode = new PHObjectNode_t(tecout, "TecOut", "PHObject");
  dstNode->addNode(TecOutNode);

// This node is necessary to split TecHits and TecTracks for output to separate streams.
// TecOut node will contain only tracks, TecHitOut node will contain only hits.
// SL 28/07/2003
  TecOut *techitout = new TecOutV7();
  PHObjectNode_t *TecHitOutNode = new PHObjectNode_t(techitout, "TecHitOut", "PHObject");
  dstNode->addNode(TecHitOutNode);

//  fDTecOut = new TecOutV1();
//  PHIODataNode<PHObject>* TecOutNode = new
//                       PHIODataNode<PHObject>((PHObject*)fDTecOut,"TecOutV1","PHObject");
//  tecNode->addNode(TecOutNode);

  fMainIter->cd();

  fHasDonePHOOLInit = kTRUE;
}

///Clean up the memory from the PHOOL objects.
void 
TTECEventIterator::CleanUpPHOOL ()
{
  if (fHasDonePHOOLInit) 
    {
      delete fTecAddress;
      fTecAddress = 0;
      
      delete fTecGeometry;
      fTecGeometry = 0;
      
      delete fTecCalibration;
      fTecCalibration = 0;
      
      delete fMainIter;
      fMainIter = 0;
      
      delete fEventIter;
      fEventIter = 0;
    }
  
  fHasDonePHOOLInit = kFALSE;
  fHasDoneFirstEventInit = kFALSE;
  fEventNumber = 0;
  fIsPastLast = kFALSE;
}

/**
 * Move onto the next TEC event from the PRDF file.
 * The Get methods return whatever event the iterator
 * is currently at. kTRUE is returned if the data
 * for the next event was retrieved without error.
 * kFALSE is returned if there are no more events
 * or if there was an error in retrieving the event.
 * 
 * @return kTRUE if the next event was fetched.
 */
Bool_t 
TTECEventIterator::MoveToNext()
{
  return MoveTo(fEventNumber + 1);
}

/**
 * Move to the specified event.
 * 
 * @param eventNumber The number of the event to load.
 * @return kTRUE if the specified event was fetched without error.
 */
Bool_t 
TTECEventIterator::MoveTo (Int_t targetNumber)
{

  if (!fHasDonePHOOLInit)
    {
      InitializePHOOL();
    }

  //Clean out the fEvents array, deleting any events
  //that have not been retrieved by the caller.
  for (int i = 0; i < 8; i++) 
    {
      delete fEvents[i];
      fEvents[i] = 0;
    }

  if (targetNumber <= 0 && fVerbosity > 0) 
    {
      cerr << "ERROR! targetNumber <= 0 passed to " <<
	"TTECEventIterator::MoveTo(Int_t)" << endl;
      return kFALSE;
    }
  
  if (fIsPastLast && targetNumber >= fEventNumber)
    {
      return kFALSE;
    }

  if (targetNumber <= fEventNumber) 
    {
      CleanUpPHOOL();
      InitializePHOOL();
    }

  while (fEventNumber < targetNumber) 
    {
      
      fEvent = fEventIter->getNextEvent();
      
      if (!fEvent) 
	{
	  if (fVerbosity > 0) 
	    {
	      cerr << "TTECEventIterator error:" << endl;
	      cerr << "  Expected at least " << targetNumber <<
		" events, but found only " << fEventNumber << endl;
	    }
	  
	  fIsPastLast = kTRUE;
	  
	  fEventNumber++;
	  
	  return kFALSE;
	}

      fEventNumber++;
      
      // Point the data node to the new event.
      fMainIter->cd();
      ((PHDataNode<Event>*)(fMainIter->findFirst("PHDataNode", "PRDF")))->setData(fEvent);
      
      if (fVerbosity > 5) 
	{
	  cout << "Fetched event " << fEventNumber << endl;
	}

      recoConsts *rc = recoConsts::instance();
      int runNumber = 0;
      if(rc->FlagExist("RUNNUMBER")) {
        runNumber = rc->get_IntFlag("RUNNUMBER");
      }
      else {
        cerr << "Set Run Number in your macro using: " << endl 
             << "recoConsts *rc = recoConsts::instance();" << endl 
             << "rc->set_IntFlag(RUNNUMBER,80312);" << endl;
	exit(-1);
      }

      if (!fIsSimulation) 
	{
	  runNumber = fRunNumber;
	}

    // First event initializations
    if (!fHasDoneFirstEventInit) 
      {
	fHasDoneFirstEventInit = kTRUE;
	
	//Set search time.
	PHTimeStamp Tsearch(2001, 8, 10, 0, 0, 0);
	//PHTimeStamp TimeStamp;
	
	//if (runNumber < 20000) 
	//  {
	//    lookup_timeStamp("runtotime.txt",&TimeStamp,runNumber);
	//  }
	//else 
	//  {
	//    TimeStamp = Tsearch;
	//  }

	if (fIsSimulation) 
	  {
	    fTecAddress->setTimeStamp(Tsearch);
	    fTecGeometry->setTimeStamp(Tsearch);
	    fTecCalibration->setTimeStamp(Tsearch);
	    fTecAddress->UseSimulationDatabase();
	    fTecGeometry->UseSimulationDatabase();
	    fTecCalibration->UseSimulationDatabase();
	    
	    if (fVerbosity > 0) { cout << " Analyzing simulated run." << endl; }
	  }
	else 
	  {
	    //fTecAddress->setTimeStamp(TimeStamp);
	    //fTecGeometry->setTimeStamp(TimeStamp);
	    //fTecCalibration->setTimeStamp(TimeStamp);
	    fTecAddress->setRunNumber(runNumber);
	    fTecGeometry->setRunNumber(runNumber);
	    fTecCalibration->setRunNumber(runNumber);
	    
	    if (fVerbosity > 0) 
	      {
		cout << " Analyzing run # " << runNumber;
		cout << " with TimeStamp:  " << fTecAddress->getTimeStamp() << endl;
	      }
	  }
	
	if (fUseObjy) 
	  {
	    // Fetch info from the database
	    fTecAddress->Fetch();
	    fTecGeometry->Fetch();
	    fTecCalibration->Fetch();
	  }
	else 
	  {
	    fTecAddress->FetchFromFile("tecmap_database_run00.txt");
	    
	    // Fetch retracted arms geometry
	    fTecGeometry->FetchFromFile("tecgeom_database_run00_00.dat");
	    fTecGeometry->Shift(44.0, 0.0, 0.0);
	    fTecCalibration->FetchAbsGainFromFile("tecabsgain_database.txt");
	    fTecCalibration->FetchRelGainFromFile("tecrelgain_database_0.txt");
	    fTecCalibration->FetchTimingGainFromFile("tectimecalib_database.txt");
	  }
      } //end first event initialization
    
    if (fVerbosity > 10) 
      {
	cout << "Calling mTecUnpack" << endl;
      }
    fMTecUnpack->set_Verbose(5);
    fMTecUnpack->event(fTopNode, fTecAddress);
  
    if (fVerbosity > 10) 
      {
	cout << "Calling mTecCalib" << endl;
      }
    fMTecCalib->set_Verbose(5);
    fMTecCalib->event(fTopNode, fTecAddress, fTecCalibration);

    if (fVerbosity > 10) 
      {
	cout << "Calling mTecHoughTrack" << endl;
      }
    fMTecHoughTrack->event(fTopNode, fTecGeometry, fTecCalibration);
    if (fVerbosity > 10) 
      {
      }

    //Reset all data for this event.
    fMainIter->cd();

    if (fEventNumber == targetNumber) 
      {
	LoadData();
      }

    if (fMainIter->cd("DST")) 
      {
	fMainIter->forEach(fReset);
	fMainIter->cd();
      }
    if (fMainIter->cd("DCM")) 
      {
	fMainIter->forEach(fReset);
	fMainIter->cd();
      }
    if (fMainIter->cd("GEA")) 
      {
	fMainIter->forEach(fReset);
	fMainIter->cd();
      }
    if (fMainIter->cd("TEC")) 
      {
	fMainIter->forEach(fReset);
	fMainIter->cd();
      }
    
    //Add some space at the end.
    if (fVerbosity > 0)
      {
	cout << endl;
      }
    
    } //end of loop
  
  return kTRUE;
}

Bool_t 
TTECEventIterator::LoadData ()
{
  PHNodeIterator iii(fTopNode);

  //Create the event models.
  for (int side = 0; side < 2; side++) 
    {
      for (int sector = 0; sector < 4; sector++) 
	{
	  int index = 4 * side + sector;
	  fEvents[index] = new TTECSectorSideEvent(sector, side,
						   fPrdfFileName,
						   fEventNumber,
						   fIsSimulation);
	} 
    }
  
  if (fVerbosity > 0) 
    {
      cout << "Number of Tracks = " << fDTecOut->getNTracks() << endl;
      cout << "Number of Hits = " << fDTecOut->getNHits() << endl;
    }
  
  int iarm, isect, iside, iplane, iwire;
  double x1, x2, y1, y2;
  
  int MinBin[96], MaxBin[96], myindex;
  float midphi[8], Xwire, Ywire;
  float relativebin, difference1, difference2;
  float MidPhi[TECMAXINDEX];
  float SinAlpha[TECMAXINDEX];

  //TCO ==> fTecCalibration
  GetMinMax(MinBin, MaxBin, fTecCalibration);

  //TGO ==> fTecGeometry
  GetMidphi(MidPhi, midphi, SinAlpha, fTecGeometry);

  for (int i = 0; i < (int) fDTecOut->getNHits(); i++) 
    {
      iarm = 0;
      isect = fDTecOut->getHitSector(i);
      iplane = fDTecOut->getHitPlane(i);
      iside = fDTecOut->getHitSide(i);
      
      iwire = fDTecOut->getHitWire(i);
      myindex = iarm * 48 + isect * 12 + iplane * 2 + iside;
      
      //TGO ==> fTecGeometry
      
      Xwire = fTecGeometry->getGlobalX(isect*TECMAXSIDE*TECMAXPLANE+iplane*TECMAXSIDE+iside, iwire);
      Ywire = fTecGeometry->getGlobalY(isect*TECMAXSIDE*TECMAXPLANE+iplane*TECMAXSIDE+iside, iwire);
      
      TECHit_t hit;
	  float amplitude = fDTecOut->getHitADC(i);
	  float timebin = fDTecOut->getHitTimeBin(i);
	  
	  difference1 = MaxBin[myindex] - MinBin[myindex];
	  difference2 = timebin - MinBin[myindex];
	  relativebin = difference2 / difference1;
	  float Xpos, Ypos;
	  CalcXYFromBin(relativebin, Xwire, Ywire,
			SinAlpha[myindex],
			Xpos, Ypos);
	  
	  if ((iside == 0 || iside == 1) && isect >= 0 && isect <= 3) 
	    {
	      //Add the hit info to the appropriate event model.
	      int eventIndex = 4 * iside + isect;
	      hit.x = Xpos;
	      hit.y = Ypos;
	      hit.amplitude = amplitude;
	      
	      fEvents[eventIndex]->AddHit(hit);
	    }
    }
  
  //Get tracks
  for (int i = 0; i < (int) fDTecOut->getNTracks(); i++) 
    {
      x1 = fDTecOut->getTrackXin(i);
      y1 = fDTecOut->getTrackYin(i);
      x2 = fDTecOut->getTrackXout(i);
      y2 = fDTecOut->getTrackYout(i);
      int side = fDTecOut->getTrackSide(i);

      TECTrack_t track;
      track.x1 = x1;
      track.y1 = y1;
      track.x2 = x2;
      track.y2 = y2;
      
      for (int sector = 0; sector < 4; sector++) 
	{
	  int eventIndex = 4 * side + sector;
	  fEvents[eventIndex]->AddTrack(track);
	}
      
    }
  
  return kTRUE;
}

/**
 * Get the specified side and sector of the 
 * TEC event.
 * <p>
 * Null is returned if there was an error in retrieving
 * the event, if there are no more events, or if this 
 * side-sector has been gotten before (in this iteration).
 * <p>
 * The caller is responsible for deleting the memory
 * of the returned object.
 * <p>
 * This method will return a particular sector-side only once.
 * After a sector-side has been gotten once, subsequent calls
 * to this method will return null.
 * <p>
 * The result of calling this method with an invalid sector
 * and side pair is undefined.
 * 
 * @param sector 0, 1, 2, 3.
 * @param side 0 for south, 1 for north.
 * @return The specified side and sector of the TEC event. 
 */
TTECSectorSideEvent* 
TTECEventIterator::GetEvent(Int_t sector, Int_t side)
{
  if ((side != 0 && side != 1) || sector < 0 || sector > 3) 
    {
      return 0;
    }

  int index = 4 * side + sector;
  TTECSectorSideEvent* event = fEvents[index];
  fEvents[index] = 0;

  return event;
}

int TTECEventIterator::lookup_timeStamp(char *fname, PHTimeStamp *timestp, int run)
{
  int itest = 0;
  ifstream file(fname);

  // Read in these parameters
  int luRun, luYear, luMonth, luDay, luHour, luMinute;
  int luSecond, luFraction, luType;

  if (!file)
    {
      cout << PHWHERE << "Cannot open timestamp look-up file"
	   << fname << endl;
      timestp->set(2000, 1, 0, 0, 0, 0, 0);
      return -1;
    }  // !file

  while (!itest)
    {

      // Read in all information on the line.
      file >> luRun;
      file >> luYear;
      file >> luMonth;
      file >> luDay;
      file >> luHour;
      file >> luMinute;
      file >> luSecond;
      file >> luFraction;
      file >> luType;

      // Have we reached the end of the file?
      if (file.eof())
        itest = 1;

      // Do a quick sanity check
      if (luType < 0 || luType > 2)
        itest = 2;

      // Only continue if this entry is a TimeStamp entry (luType = 0)
      if (luType == 0 && itest == 0)
        {

          // Do the run numbers match?  If they do, then this is the entry
          // we want.  Store it and leave.
          if (luRun == run)
            {

              itest = 3;
              timestp->set(luYear, luMonth, luDay, luHour, luMinute,
                           luSecond, luFraction);

              return 0;
            }  // luRun==run

        } // luType==0

    }  // itest

  if (itest == 1)
    cout << PHWHERE << "No run-to-time match found"
    << endl;
  timestp->set(2000, 1, 0, 0, 0, 0, 0);
  return -1;
}
