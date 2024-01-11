//
//  TKH--Updated to use new calibration constant interface.
//       This routine is PURELY MonteCarlo and uses the
//       "Nominal" Routines.
//                           11-25-2001
//
#define __histostuff__ 0

#include "mNewDchSimulator.hh" 
#include <PHDchGeometryObject.h> 
#include <PHDchCalibrationObject.h> 
#include <PHDchNoiseObject.h> 

#include "dcghitWrapper.h" 
#include "dDchFastSimParWrapper.h"  
#include "dDchHitWrapper.h" 
#include "dDchGhitHitsWrapper.h" 
#include "dDchGhitRawWrapper.h" 
 
#include <PHGeometry.h>
#include <PHCompositeNode.h> 
#include <PHIODataNode.h>
#include <PHTable.hh> 

#include <TRandom3.h>

#include <iostream> 
#include <cmath>
using namespace std;

using namespace PHGeometry;
 
typedef PHIODataNode<dcghitWrapper> dcghitNode_t; 
typedef PHIODataNode<dDchFastSimParWrapper> dDchFastSimParNode_t; 
typedef PHIODataNode<dDchRawWrapper> dDchRawNode_t; 
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t; 
typedef PHIODataNode<dDchGhitHitsWrapper> dDchGhitHitsNode_t; 
typedef PHIODataNode<dDchGhitRawWrapper> dDchGhitRawNode_t; 
typedef PHIODataNode<PHTable> TableNode_t; 
 
mNewDchSimulator::mNewDchSimulator(const int iseed):
  dchGeometryObject(NULL),
  dchAddressObject(NULL),
  dchCalibrationObject(NULL),
  dchNoiseObject(NULL),
  dchHistogrammer(NULL),
  DriftWidth(0.15),
  BackDriftWidth(0.1),
  BackDriftShift(0.3),
  BackDriftCut(0.2),
  RProp(0.3),
  PanFlag(1),
  BackEffFlag(1),
  EffFlag(1),
  ResFlag(1),
  MergeFlag(1),
  AddNoiseFlag(1)
{
  rand = gsl_rng_alloc(gsl_rng_mt19937);
  if (iseed < 0)
    {
      gsl_rng_set (rand, TRandom3(0).GetSeed()); // set seed
    }
  else
    {
      gsl_rng_set (rand, iseed); // set seed
    }
  BackInclAngle = M_PI * 45. / 180.; // 45 deg so far
  setSmearConstants(0.01,0.005,0.01,0.5);   // default - no smearing
} 
 
mNewDchSimulator::~mNewDchSimulator()
{
  gsl_rng_free (rand);
} 
 
PHBoolean mNewDchSimulator::event(PHCompositeNode *root) 
{ 
  PHPointerList<PHNode> nodes; 
  PHNodeIterator i(root), *j; 
  PHNode *n; 
  TableNode_t *d; 
  PHTable *w; 
  PHCompositeNode *geaNode = 0;
  PHCompositeNode *parNode = 0;
  PHCompositeNode *dchNode = 0;
  PHCompositeNode *dstNode = 0;
  PHCompositeNode *outNode = 0; 
  PHCompositeNode *doNode = 0;
  
#if __histostuff__ == 1
  PHCompositeNode *histoNode;
  PHDataNode<PHDchHistogrammer>*      dchHistoNode; 
#endif

  PHDataNode<PHDchAddressObject>*     dchDaoNode; 
  PHDataNode<PHDchGeometryObject>*    dchDgoNode; 
  PHDataNode<PHDchCalibrationObject>* dchDcoNode;
  PHDataNode<PHDchNoiseObject>*       dchDnoNode; 
 
  // cout << "initializing FastSim" << endl; (excess output line removed, 5/29/2005 CFM, use iDebug check)
  
  parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR")); 
  if (!parNode) { 
    parNode = new PHCompositeNode("PAR"); 
    root->addNode(parNode); 
  } 
  
  doNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCHDO")); 
  if (!doNode) { 
    doNode = new PHCompositeNode("DCHDO"); 
    dchNode->addNode(doNode); 
    PHMessage("mDchFastSimulator::event",PHError,"NO Detector Objects !!"); 
  } 
  
  geaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "GEA")); 
  if (!geaNode) { 
    geaNode = new PHCompositeNode("GEA"); 
    root->addNode(geaNode); 
  } 
  
  dchNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCH")); 
  if (!dchNode) { 
    dchNode = new PHCompositeNode("DCH"); 
    root->addNode(dchNode); 
  } 
  
  dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST")); 
  if (!dstNode) { 
    dstNode = new PHCompositeNode("DST"); 
    root->addNode(dstNode); 
  } 
  
#if __histostuff__ == 1
  
  // Make a node for the histogrammer
  histoNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "HISTO"));
  if (!histoNode) {
    histoNode = new PHCompositeNode("HISTO");
    root->addNode(histoNode);
  }
  
  dchHistoNode = (PHDataNode<PHDchHistogrammer>*)i.findFirst("PHDataNode","DchHisto");
  if (!dchHistoNode) {
    dchHistogrammer = new PHDchHistogrammer();
    dchHistogrammer->initializeFile();
    dchHistogrammer->initializeDefault();
    dchHistoNode = new PHDataNode<PHDchHistogrammer>(dchHistogrammer,"DchHisto");
    histoNode->addNode(dchHistoNode);
  }else{
    cout << "histogrammer exists already" << endl;
    dchHistogrammer = dchHistoNode->getData();
    dchHistogrammer->initializeFile();
  }
  
#endif
  
  // Insert code here to navigate node hierarchy and find 
  // or create specific nodes to pass to physics module... 
  
  outNode = geaNode; 
  n = i.findFirst("PHIODataNode", "dcghit"); 
  if (!n) { 
    cout << "ERROR:  'in' parameter dcghit not found" << endl; 
    w = new dcghitWrapper("dcghit", 10); 
    if (!w) return 1; 
    n = new TableNode_t(w,"dcghit"); 
    outNode->addNode(n); 
  } 
  nodes.append(n); 
  
  outNode = parNode; 
  n = i.findFirst("PHIODataNode", "dDchFastSimPar"); 
  if (!n) { 
    cout << "ERROR:  'inout' parameter dDchFastSimPar not found" << endl; 
    w = new dDchFastSimParWrapper("dDchFastSimPar", 1); 
    if (!w) return 1; 
    n = new TableNode_t(w,"dDchFastSimPar"); 
    outNode->addNode(n); 
  } 
  nodes.append(n); 
  
  outNode = dchNode; 
  j = new PHNodeIterator(outNode); 
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchRaw")))) { 
    w = new dDchRawWrapper("dDchRaw", 60000); 
    if (!w) return 1; 
    d = new TableNode_t(w,"dDchRaw"); 
    outNode->addNode(d); 
  } 
  delete j; 
  nodes.append(d); 
  
  outNode = dstNode; 
  j = new PHNodeIterator(outNode); 
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchHit")))) { 
    w = new dDchHitWrapper("dDchHit", 60000); 
    if (!w) return 1; 
    d = new TableNode_t(w,"dDchHit"); 
    outNode->addNode(d); 
  } 
  delete j; 
  nodes.append(d); 
  
  outNode = dchNode; 
  j = new PHNodeIterator(outNode); 
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchGhitHits")))) { 
    w = new dDchGhitHitsWrapper("dDchGhitHits", 60000); 
    if (!w) return 1; 
    d = new TableNode_t(w,"dDchGhitHits"); 
    outNode->addNode(d); 
  } 
  delete j; 
  nodes.append(d); 
  
  outNode = dchNode; 
  j = new PHNodeIterator(outNode); 
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchGhitRaw")))) { 
    w = new dDchGhitRawWrapper("dDchGhitRaw", 60000); 
    if (!w) return 1; 
    d = new TableNode_t(w,"dDchGhitRaw"); 
    outNode->addNode(d); 
  } 
  delete j; 
  nodes.append(d); 
  
  dchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO"); 
  if (!dchDaoNode) { 
    PHMessage("mNewDchSimulator::event",PHError,"DAO missing in the tree"); 
  }else{ 
    dchAddressObject = dchDaoNode->getData(); 
  } 
  
  dchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode","DchDGO"); 
  if (!dchDgoNode) { 
    PHMessage("mNewDchSimulator::event",PHError,"DGO missing in the tree"); 
  }else{ 
    dchGeometryObject = dchDgoNode->getData(); 
  } 
  
  dchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode","DchDCO"); 
  if (!dchDcoNode) { 
    PHMessage("mNewDchSimulator::event",PHError,"DCO missing in the tree"); 
  }else{ 
    dchCalibrationObject = dchDcoNode->getData();
  } 
  
  dchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode","DchDNO"); 
  if (!dchDnoNode) { 
    PHMessage("mNewDchSimulator::event",PHError,"DNO missing in the tree"); 
  }else{ 
    dchNoiseObject = dchDnoNode->getData(); 
  } 
  initialize();
  
  return  callPAM(nodes); 
}
 
PHBoolean mNewDchSimulator::callPAM(PHPointerList<PHNode> &nl) 
{ 
   
  //---------------------------------------------------------------------------- 
  // This module reads in the dcghit table, simulates the drift chamber response 
  // and creates the dDchHit and dDchRaw tables.  The dDchHit table can be 
  // directly read by the Tracker and the dDchRaw table is used to simulate 
  // the electronics chain by passing it to dDchFEM. 
  //---------------------------------------------------------------------------- 
 
  TABLE_HEAD_ST      dcghit_h; 
  DCGHIT_ST         *dcghit; 
  TABLE_HEAD_ST      dDchFastSimPar_h; 
  TABLE_HEAD_ST      dDchRaw_h; 
  DDCHRAW_ST        *dDchRaw; 
  TABLE_HEAD_ST      dDchHit_h; 
  DDCHHIT_ST        *dDchHit; 
  TABLE_HEAD_ST      dDchGhitHits_h; 
  DDCHGHITHITS_ST   *dDchGhitHits; 
  TABLE_HEAD_ST      dDchGhitRaw_h; 
  DDCHGHITRAW_ST    *dDchGhitRaw; 

  dcghitNode_t*         n1 = static_cast<dcghitNode_t*>(nl[0]); 
  dDchFastSimParNode_t* n2 = static_cast<dDchFastSimParNode_t*>(nl[1]); 
  dDchRawNode_t*        n3 = static_cast<dDchRawNode_t*>(nl[2]); 
  dDchHitNode_t*        n4 = static_cast<dDchHitNode_t*>(nl[3]); 
  dDchGhitHitsNode_t*   n5 = static_cast<dDchGhitHitsNode_t*>(nl[4]); 
  dDchGhitRawNode_t*    n6 = static_cast<dDchGhitRawNode_t*>(nl[5]); 

  dcghit_h         = n1->getData()->TableHeader(); 
  dcghit           = n1->getData()->TableData(); 
  dDchFastSimPar_h = n2->getData()->TableHeader(); 
  dDchRaw_h        = n3->getData()->TableHeader(); 
  dDchRaw          = n3->getData()->TableData(); 
  dDchHit_h        = n4->getData()->TableHeader(); 
  dDchHit          = n4->getData()->TableData(); 
  dDchGhitHits_h   = n5->getData()->TableHeader(); 
  dDchGhitHits     = n5->getData()->TableData(); 
  dDchGhitRaw_h    = n6->getData()->TableHeader(); 
  dDchGhitRaw      = n6->getData()->TableData(); 

  int mc_hits; 
  mc_hits = dcghit_h.nok;
  
  int idraw1,idraw2;
  short edge; 
  int ihit; 
  int hitcounter = 0; 
  int rawcounter = 0; 
  long drift_time;

  // Zero nibble array
  for (int ta = 0; ta<2; ta++) {
    for (int ts = 0; ts<2; ts++) {
      for (int tp = 0; tp<40; tp++) {
	for (int tc = 0; tc<80; tc++) {
	  for (int tn = 0; tn<48; tn++) {
	    nibbleArray[ta][ts][tp][tc][tn] = -1;
	  }
	}
      }
    }
  }

  Glob_dv_cor = gsl_ran_gaussian (rand, Glob_dv_sig);

  //----------------------------------- 
  // main loop over GEANT hits 
  //------------------------------------ 
  for (ihit = 0; ihit < mc_hits; ihit++)
    { 
      
      // Obtain arm information
      if (dcghit[ihit].xyzinglo[0] < 0)	
	{
	  arm = EAST;
	}
      else 
	{
	  arm = WEST; 
	}
      plane = dcghit[ihit].plane; 

      // Decode cell number
      if (arm == WEST)  cell = dcghit[ihit].cell; 
      else              cell = numberOfCells - dcghit[ihit].cell - 1;
      
      // Decode side (only works if frames are not shifted in Z
      if (dcghit[ihit].xyzinglo[2] < 0) side = SOUTH; 
      else                              side = NORTH; 
      
      // No UV back efficient hits
      dist_b1 = -10.;
      dist_b2 = -10.;
      
      // Set hard<->soft relations
      dchAddressObject->setSoft(arm,side,plane,cell);
      globalIndex = dchAddressObject->getGlobalIndex()->getValue();
      if (!dchNoiseObject->status(globalIndex)) continue;
     
      // --------------------------------------------------------- //

      // Cell coordinates, only needed to get alpha for the track
      //
      //                  X_out / "track"
      //  ---------------------0------
      //  | Drift cell  *     /      |
      //  -------------------0--------
      //               X_in /
      //
      //  in   below refers to the entry of the GEANT Volume
      //  out  below refers to the exit from the Volume
      //  For the sake of uniformity consider track always panetrate from the center out
      //  and in global CS in_radius is always smaller then out_radius

      
      PHPoint InPoint (dcghit[ihit].xyzinloc[0]*DriftSign[arm][plane][cell],dcghit[ihit].xyzinloc[1],dcghit[ihit].xyzinloc[2]);
      PHPoint OutPoint(dcghit[ihit].xyzoutloc[0]*DriftSign[arm][plane][cell],dcghit[ihit].xyzoutloc[1],dcghit[ihit].xyzoutloc[2]);
      PHPoint GloInPointGeant(dcghit[ihit].xyzinglo[0],dcghit[ihit].xyzinglo[1],dcghit[ihit].xyzinglo[2]);
      
      if (distancePointToPoint(InPoint,OutPoint)<0.05) continue; // track length is too short to be registered
      
      PHPoint LocalHit = (InPoint+OutPoint)*0.5;
      double ZHit = LocalHit.getZ();                              // Zed of the hit. The same in local and global
	
      // ----------------------------------------------------------- //      
      //  Calculating Global coordinates of the hit in PISA geometry
      
      PHVector WireLinePISA(wireNorthPointPISA[arm][plane][cell]-wireSouthPointPISA[arm][plane][cell]);
      WireLinePISA.normalize();
      
      PHPoint WirePointPISA = (wireNorthPointPISA[arm][plane][cell]+wireSouthPointPISA[arm][plane][cell])*0.5;
      
      PHVector LocalDriftPISA = (wireNorthDriftPISA[arm][plane][cell]+wireSouthDriftPISA[arm][plane][cell])*0.5;
      LocalDriftPISA.normalize();

      PHVector CrossPISA = WireLinePISA.cross(LocalDriftPISA)*DriftSign[arm][plane][cell];
      CrossPISA.normalize();

      // GlobalIn is GEANT volume entry point. The same as GloInPoinGeant with 5 micron accuracy 
      PHPoint  GlobalIn( WirePointPISA.getX() + LocalDriftPISA.getX()*InPoint.getX() + CrossPISA.getX()*InPoint.getY(),
			 WirePointPISA.getY() + LocalDriftPISA.getY()*InPoint.getX() + CrossPISA.getY()*InPoint.getY(),
			       InPoint.getZ() + LocalDriftPISA.getZ()*InPoint.getX() + CrossPISA.getZ()*InPoint.getY());
      
      // Global point of GEANT volume exit
      PHPoint  GlobalOut(WirePointPISA.getX() + LocalDriftPISA.getX()*OutPoint.getX() + CrossPISA.getX()*OutPoint.getY(),
			 WirePointPISA.getY() + LocalDriftPISA.getY()*OutPoint.getX() + CrossPISA.getY()*OutPoint.getY(),
			      OutPoint.getZ() + LocalDriftPISA.getZ()*OutPoint.getX() + CrossPISA.getZ()*OutPoint.getY());
      
      // Track segment crossing the drift cell
      PHLine Track(GlobalIn,GlobalOut);
     
      // ----------------------------------------------------------- //        
      // From this point on use ASCII file geometry

      PHVector WireLine(wireNorthPoint[arm][plane][cell]-wireSouthPoint[arm][plane][cell]);
      WireLine.normalize();
      
      double ZNorth = wireNorthPoint[arm][plane][cell].getZ();
      double ZSouth = wireSouthPoint[arm][plane][cell].getZ();
      
      double Delta = (ZHit-ZNorth)/(ZSouth-ZNorth);
      PHPoint WirePoint = wireNorthPoint[arm][plane][cell] +(wireSouthPoint[arm][plane][cell]-wireNorthPoint[arm][plane][cell])*Delta;
      
      PHVector LocalDrift = wireNorthDrift[arm][plane][cell] +(wireSouthDrift[arm][plane][cell]-wireNorthDrift[arm][plane][cell])*Delta;
      LocalDrift.normalize();

      PHVector Cross = WireLine.cross(LocalDrift)*DriftSign[arm][plane][cell];
      Cross.setZ(0.0);
      Cross.normalize();
     
      // Creating planes that define Drift cell
      PHPoint LowerPlanePoint(WirePoint.getX() - Cross.getX()*DriftWidth,
			      WirePoint.getY() - Cross.getY()*DriftWidth, 0.);
      PHPlane LowerPlane(LowerPlanePoint,Cross);

      PHPoint UpperPlanePoint(WirePoint.getX() + Cross.getX()*DriftWidth,
			      WirePoint.getY() + Cross.getY()*DriftWidth, 0.);
      PHPlane UpperPlane(UpperPlanePoint,Cross);
      
      // Real cell crossing points for the track
      PHPoint RealIn, RealOut;
      if (!intersectionLinePlane(Track, LowerPlane, RealIn)) continue;
      if (!intersectionLinePlane(Track, UpperPlane, RealOut)) continue;
      
      // Real hit position
      PHPoint  GeaHit    = (RealIn+RealOut)*0.5;     
      
      PHLine   Wire(wireNorthPoint[arm][plane][cell],wireSouthPoint[arm][plane][cell]);
      
      // Ideal distance to the wire 
      dist = distanceLinePoint(Wire, GeaHit);
      
      if (dist>3.) continue;

      PHPoint WirePoint2 = closestApproachLinePoint(Wire,GeaHit); // For UV hits this is the only way to determine back drift
      PHVector DistVect = GeaHit-WirePoint2;
      double DistDir = DistVect.getY()*LocalDrift.getY();
      // Calculating main parameters of the track
    
      if (arm == WEST)
	  {
	      tmpangle = atan2((Track.getDirection()).getY(),(Track.getDirection()).getX());
	      phi = atan2(GeaHit.getY(),GeaHit.getX());
	  }
      else
	  {
	      tmpangle = Pi-atan2((Track.getDirection()).getY(),-(Track.getDirection()).getX());
	      phi = Pi-atan2(GeaHit.getY(),-(GeaHit.getX()));
	  }

      alpha = phi - tmpangle;
      if ( alpha> Pi/2.) alpha = Pi - alpha;
      if ( alpha< -Pi/2.) alpha = -Pi - alpha;      
	   
      // DistSign is negative if the drift comes from Back side 
      if (DistDir<0.) DistSign = -1 ; else DistSign = 1;

      // Implementing Pan model
      width = (short) getWidth();

      double L = dist*cos(alpha);               // distance of the closest approach
      if (L<RProp) 
	dist = L;
      else 
	{
	  double H = RProp*sin(alpha);
	  if (H<DriftWidth) dist -= RProp*(1./cos(alpha)-1.);
	  else dist -= sqrt(RProp*RProp-DriftWidth*DriftWidth)+DriftWidth*tan(alpha)-RProp;
	}
  
      double Lproj = L*cos(alpha);
      
      if ((Lproj>BackDriftCut && L<RProp && DistSign == -1) ||
	  (L>=RProp && DistSign == -1)) 
	{ 
	  // Back Efficiency applied here
	  if (!BackEffFlag || !BackEfficient())  continue;
	  if ((plane>11 && plane<21) || plane>31) SimulateUVBackDrift(Wire,WirePoint,Cross*DriftSign[arm][plane][cell],Track);
	} 
      else
	{
	  // Efficiensy applied here
	  if (EffFlag && !Efficient())  continue;
	}
      
      if (ResFlag) dist+=getResolution();

      if (dist_b1<0. && dist_b2<0.)
	{
	  drift_time = (long) dchCalibrationObject->transformDistanceToNominalTime(dist,edge,
										   gsl_ran_gaussian(rand,SW_t0_sig),
										   (1.+ Glob_dv_cor
										    +Key_dv_cor[arm][plane][cell/4]
										    +gsl_ran_gaussian(rand,SW_dv_sig)));       
	  
	  // Add TOF correction
	  drift_time += (long) ((dcghit[ihit].tof*1000-8.465)/dchCalibrationObject->getBinSize()); //8.465 - average TOF for HIJING mean bias
	  
	  // store data in STAF tables now:  raw data, hit, and MC reference 
	  if (hitcounter < dDchHit_h.maxlen && rawcounter < dDchRaw_h.maxlen) 
	    { 
	      // leading edge raw data 
	      dDchRaw_h.nok++; 
	      dDchRaw[rawcounter].id           = rawcounter; 
	      dDchRaw[rawcounter].global       = globalIndex; 
	      dDchRaw[rawcounter].arm          = arm;  
	      dDchRaw[rawcounter].plane        = plane;  
	      dDchRaw[rawcounter].cell         = cell;  
	      dDchRaw[rawcounter].side         = side;  
	      dDchRaw[rawcounter].edge         = 0;  
	      dDchRaw[rawcounter].time         = drift_time;
	      idraw1                           = rawcounter;
	      dDchGhitRaw[rawcounter].ghitid   = dcghit[ihit].id; 
	      dDchGhitRaw[rawcounter].rawid    = rawcounter; 
	      dDchGhitRaw_h.nok++; 
	      rawcounter ++; 
	      
	      // trailing edge raw data 
	      dDchRaw_h.nok++;  
	      dDchRaw[rawcounter].id           = rawcounter; 
	      dDchRaw[rawcounter].global       = globalIndex; 
	      dDchRaw[rawcounter].arm          = arm;  
	      dDchRaw[rawcounter].plane        = plane;   
	      dDchRaw[rawcounter].cell         = cell;  
	      dDchRaw[rawcounter].side         = side;  
	      dDchRaw[rawcounter].edge         = 1;  
	      dDchRaw[rawcounter].time         = drift_time+width;  
	      idraw2                           = rawcounter; 
	      dDchGhitRaw[rawcounter].ghitid   = dcghit[ihit].id; 
	      dDchGhitRaw[rawcounter].rawid    = rawcounter; 
	      dDchGhitRaw_h.nok++; 
	      rawcounter ++; 
              
	      dDchHit_h.nok++; 
	      dDchHit[hitcounter].id           = hitcounter; 
	      dDchHit[hitcounter].arm          = arm ; 
	      dDchHit[hitcounter].plane        = plane;  
	      dDchHit[hitcounter].cell         = cell ; 
	      dDchHit[hitcounter].side         = side; 
	      dDchHit[hitcounter].distance     = dist;
	      dDchHit[hitcounter].time1        = drift_time;
	      dDchHit[hitcounter].time2        = drift_time+width;
	      
	      dDchHit[hitcounter].width        = width;  
	      dDchHit[hitcounter].idraw1       = idraw1; 
	      dDchHit[hitcounter].idraw2       = idraw2; 
	      dDchHit[hitcounter].idmirror     = 0; 
	      
	      PHLine line;                 // need sign of distance 
	      line = dchGeometryObject->transformDistanceToLine (arm,plane,cell,dist*DistSign); 
	      dDchHit[hitcounter].xyz[0]  = line.getBasepoint().getX(); 
	      dDchHit[hitcounter].xyz[1]  = line.getBasepoint().getY(); 
	      dDchHit[hitcounter].xyz[2]  = line.getBasepoint().getZ(); 
	      dDchHit[hitcounter].vxyz[0] = line.getDirection().getX(); 
	      dDchHit[hitcounter].vxyz[1] = line.getDirection().getY(); 
	      dDchHit[hitcounter].vxyz[2] = line.getDirection().getZ(); 
	     
	      dDchGhitHits[hitcounter].ghitid = dcghit[ihit].id; 
	      dDchGhitHits[hitcounter].hitsid = hitcounter; 
	      dDchGhitHits_h.nok++; 
	      
	      hitcounter ++; 
	      
	    } 
	  else 
	    {
	      cout << " DCHit table exeeded the limits " <<endl; 
	    } 
	  
	  if (MergeFlag) 
	    {
	      MergeHits(hitcounter-1,&dDchRaw_h,dDchRaw);
	    }
	}
      else
	{
	  for ( int b_cntr = 0; b_cntr <2; b_cntr++)
	    {
	      if (b_cntr == 0 ) dist = dist_b1; else dist = dist_b2;

	      if (dist>3.) continue;

	      if (ResFlag) dist +=getResolution();
	      
	      width = getWidth();
	      
	      drift_time = (long) dchCalibrationObject->transformDistanceToNominalTime(dist,edge,
										       gsl_ran_gaussian(rand,SW_t0_sig),
										       (1.+ Glob_dv_cor
											+Key_dv_cor[arm][plane][cell/4]
											+gsl_ran_gaussian(rand,SW_dv_sig)));              
	      
	      // Add TOF correction
	      drift_time += (long) ((dcghit[ihit].tof*1000-8.465)/dchCalibrationObject->getBinSize()); //8.465 - average TOF for HIJING mean bias
	      
	      // store data in STAF tables now:  raw data, hit, and MC reference 
	      if (hitcounter < dDchHit_h.maxlen && rawcounter < dDchRaw_h.maxlen) 
		{ 
		  // leading edge raw data 
		  dDchRaw_h.nok++; 
		  dDchRaw[rawcounter].id           = rawcounter; 
		  dDchRaw[rawcounter].global       = globalIndex; 
		  dDchRaw[rawcounter].arm          = arm;  
		  dDchRaw[rawcounter].plane        = plane;  
		  dDchRaw[rawcounter].cell         = cell;  
		  dDchRaw[rawcounter].side         = side;  
		  dDchRaw[rawcounter].edge         = 0;  
		  dDchRaw[rawcounter].time         = drift_time;
		  idraw1                           = rawcounter;
		  dDchGhitRaw[rawcounter].ghitid   = dcghit[ihit].id; 
		  dDchGhitRaw[rawcounter].rawid    = rawcounter; 
		  dDchGhitRaw_h.nok++; 
		  rawcounter ++; 
		  
		  // trailing edge raw data 
		  dDchRaw_h.nok++;  
		  dDchRaw[rawcounter].id           = rawcounter; 
		  dDchRaw[rawcounter].global       = globalIndex; 
		  dDchRaw[rawcounter].arm          = arm;  
		  dDchRaw[rawcounter].plane        = plane;   
		  dDchRaw[rawcounter].cell         = cell;  
		  dDchRaw[rawcounter].side         = side;  
		  dDchRaw[rawcounter].edge         = 1;  
		  dDchRaw[rawcounter].time         = drift_time+width;  
		  idraw2                           = rawcounter; 
		  dDchGhitRaw[rawcounter].ghitid   = dcghit[ihit].id; 
		  dDchGhitRaw[rawcounter].rawid    = rawcounter; 
		  dDchGhitRaw_h.nok++; 
		  rawcounter ++; 
		  
		  dDchHit_h.nok++; 
		  dDchHit[hitcounter].id           = hitcounter; 
		  dDchHit[hitcounter].arm          = arm ; 
		  dDchHit[hitcounter].plane        = plane;  
		  dDchHit[hitcounter].cell         = cell ; 
		  dDchHit[hitcounter].side         = side; 
		  dDchHit[hitcounter].distance     = dist;
		  dDchHit[hitcounter].time1        = drift_time;
		  dDchHit[hitcounter].time2        = drift_time+width;
		  
		  dDchHit[hitcounter].width        = width;  
		  dDchHit[hitcounter].idraw1       = idraw1; 
		  dDchHit[hitcounter].idraw2       = idraw2; 
		  dDchHit[hitcounter].idmirror     = 0; 
		  
		  PHLine line;                 // need sign of distance 
		  line = dchGeometryObject->transformDistanceToLine (arm,plane,cell,dist*DistSign); 
		  dDchHit[hitcounter].xyz[0]  = line.getBasepoint().getX(); 
		  dDchHit[hitcounter].xyz[1]  = line.getBasepoint().getY(); 
		  dDchHit[hitcounter].xyz[2]  = line.getBasepoint().getZ(); 
		  
		  dDchHit[hitcounter].vxyz[0] = line.getDirection().getX(); 
		  dDchHit[hitcounter].vxyz[1] = line.getDirection().getY(); 
		  dDchHit[hitcounter].vxyz[2] = line.getDirection().getZ(); 
		
		  dDchGhitHits[hitcounter].ghitid = dcghit[ihit].id; 
		  dDchGhitHits[hitcounter].hitsid = hitcounter; 
		  dDchGhitHits_h.nok++; 
		  
		  hitcounter ++; 
		  
		} 
	      else 
		{
		  cout << " DCHit table exeeded the limits " <<endl; 
		} 
	      
	      if (MergeFlag) 
		{
		  MergeHits(hitcounter-1,&dDchRaw_h,dDchRaw);
		}	  
	    }
	}

      if (AddNoiseFlag)
	{
	  //  Going to add random electronic noise with exponential width distribution

	}
    }       
  
  n1->getData()->SetRowCount(dcghit_h.nok); 
  n2->getData()->SetRowCount(dDchFastSimPar_h.nok); 
  n3->getData()->SetRowCount(dDchRaw_h.nok); 
  n4->getData()->SetRowCount(dDchHit_h.nok); 
  n5->getData()->SetRowCount(dDchGhitHits_h.nok); 
  n6->getData()->SetRowCount(dDchGhitRaw_h.nok);
  
  return True; 
} 

void mNewDchSimulator::initialize()
{ 
  short p,c,s;
  unsigned int a;
  double del1;

  double dphicell =  0.019634556;                // Angle between adjusent cells /* 90/80*Pi/180.*/
 
  PHVector Zaxis(0.,0.,1.);
  PHVector tmp1, tmp2;
  
  for (a=0 ;a<2 ;a++){
    for (p=0 ;p<40; p++){
      for (c=0 ;c<80; c++){

	//  Wire geometry information from ASCII file
	wireNorthPoint[a][p][c] = dchGeometryObject->getWireBasepointNorth(a,p,c);
	wireSouthPoint[a][p][c] = dchGeometryObject->getWireBasepointSouth(a,p,c);
	wireNorthDrift[a][p][c] = dchGeometryObject->getWireDriftDirectionNorth(a,p,c);
	wireSouthDrift[a][p][c] = dchGeometryObject->getWireDriftDirectionSouth(a,p,c);
	wireNorthDrift[a][p][c].normalize();
	wireSouthDrift[a][p][c].normalize();

	//  Wire information used in PISA detector VOLUMES
	wireNorthPointPISA[a][p][c] = wireNorthPoint[a][p][c];
	wireSouthPointPISA[a][p][c] = wireSouthPoint[a][p][c];
	wireNorthDriftPISA[a][p][c] = wireNorthDrift[a][p][c];
	wireSouthDriftPISA[a][p][c] = wireSouthDrift[a][p][c];
	
    	if (p<12 ||(p>19&&p<32)) del1 = cos(dphicell/2);           // For X wires radial correction for Cathode wires as Cell reference
  	else   del1 = cos(dphicell*0.83);                          // For UV wires empiric radial correction from best match of global hits

	// Radial corrections applied 
  	wireNorthPointPISA[a][p][c].setX(wireNorthPointPISA[a][p][c].getX()*del1);
  	wireNorthPointPISA[a][p][c].setY(wireNorthPointPISA[a][p][c].getY()*del1);
  	wireSouthPointPISA[a][p][c].setX(wireSouthPointPISA[a][p][c].getX()*del1);
  	wireSouthPointPISA[a][p][c].setY(wireSouthPointPISA[a][p][c].getY()*del1);
	
	// Recalculation of ideal drift directions ( By defenition perpendicular to the wire )
	tmp1 = Zaxis.cross(wireNorthPointPISA[a][p][c]);
	tmp2 = Zaxis.cross(wireSouthPointPISA[a][p][c]);
	
	// Check the direction of Drift Vectors. Should coinside with direction from ASCII file
	if (tmp1.getX()*wireNorthDriftPISA[a][p][c].getX()<0.) wireNorthDriftPISA[a][p][c] = tmp1*(-1);
	else wireNorthDriftPISA[a][p][c] = tmp1;
	if (tmp2.getX()*wireSouthDriftPISA[a][p][c].getX()<0.) wireSouthDriftPISA[a][p][c] = tmp2*(-1);
	else wireSouthDriftPISA[a][p][c] = tmp2;
	
	wireNorthDriftPISA[a][p][c].normalize();
	wireSouthDriftPISA[a][p][c].normalize();
	
	// Fill relative drift direction sign
	if (wireNorthDrift[a][p][c].getY()>0) s = 1; else s = -1;
	if ( a == WEST )  s *= -1;
	
	DriftSign[a][p][c] = s;
      }
    }
  }
}

int mNewDchSimulator::getWidth()
{
  
  double tmpwidth = 0.;
  double w_par_east[4] = {45.75, 0.4675, 67.43, 13.31};
  double w_par_west[4] = {41.6 , 0.3708, 64.01, 10.64};
  double sigma_w_par_east[4] = {7.349, 0.1721, 4.818, 11.58};
  double sigma_w_par_west[4] = {7.877, 0.313 , 4.47 , 11.7 };
 
  double wid_mean, wid_sigma;

  if (arm == EAST) 
    {
      wid_mean = w_par_east[0] + w_par_east[1]*dist*dist + w_par_east[2]*exp(-dist*w_par_east[3]);
      wid_sigma = sigma_w_par_east[0] + sigma_w_par_east[1]*dist*dist + sigma_w_par_east[2]*exp(-dist*sigma_w_par_east[3]);
    }
  else
    {
      wid_mean = w_par_west[0] + w_par_west[1]*dist*dist + w_par_west[2]*exp(-dist*w_par_west[3]);
      wid_sigma = sigma_w_par_west[0] + sigma_w_par_west[1]*dist*dist + sigma_w_par_west[2]*exp(-dist*sigma_w_par_west[3]);
    }
  
  while (tmpwidth<20.)
    {
      tmpwidth = wid_mean + gsl_ran_gaussian(rand,wid_sigma);
    }
  
  return (int) tmpwidth;
}

float mNewDchSimulator::getResolution()
{
  return gsl_ran_gaussian(rand,0.01);
}


PHBoolean mNewDchSimulator::BackEfficient()
{
  double backeffX = 0.07;   //  7%  average Back efficiency
  double backeffUV = 0.85;  //  85% average UV back efficiency

  if ( plane<12 || (plane>19 &&plane<32 )) 
    {
      if (gsl_rng_uniform(rand)<(backeffX*dchNoiseObject->getEfficiency(globalIndex))) return True; // Back eff should be proportional to front efficiency
      else return False;
    } 
  else
    {
      if (gsl_rng_uniform(rand)<backeffUV) return True;
      else return False;
    }
}

PHBoolean mNewDchSimulator::Efficient()
{
  double sweffUV = 0.95;

  if (plane<12 || (plane>19 &&plane<32 ))
    {
      if (gsl_rng_uniform(rand)<dchNoiseObject->getEfficiency(globalIndex)) return True;
      else return False;
    }
  else
    {
     if (gsl_rng_uniform(rand)<sweffUV) return True;
     else return False; 
    }
}

// Hit merging routine
// Basic idea fill the nibblearray with the hitid of the hit and if the new one falls  
// to previously occupied nibble - modify the previouce hit and remove the current.
// The same logic as in FEMmodule merging routine.
// One nibble can store only one edge

void mNewDchSimulator::MergeHits(int CurId, TABLE_HEAD_ST *dDchRaw_h,DDCHRAW_ST *dDchRaw)
{
  int StartId;                                                                   // ID of the hit that gives leading time for merged hit
  int EndId;                                                                     // ID of the hit that gives trailing time for merged hit
  int PrevId;                                                                    // Running ID of the hit stored in the current nibble
  int StartTime;                                                                 // Storage for the time of the merged hit leading edge
  int LNibble = (int) floor((float)dDchRaw[2*CurId].time/16.);                   // Leading edge nibble
  int TNibble = (int) floor((float)dDchRaw[2*CurId+1].time/16.);                 // Trailing edje nibble
  int loop, LN, TN;
  
  StartId = CurId;
  EndId   = CurId;
  StartTime = dDchRaw[2*CurId].time;
  
  if (LNibble>=0 && LNibble<48) 
    PrevId = nibbleArray[arm][side][plane][cell][LNibble];       // read previouse information from the nibble array
  else 
    PrevId = -1;
  
  // The was a previously stored hit, check whether that hit overlap with the current
  if (PrevId != -1 && dDchRaw[2*PrevId].time < dDchRaw[2*CurId].time && dDchRaw[2*PrevId+1].time > dDchRaw[2*CurId].time)
    { 
      StartId = PrevId;                                     // Previouse hit started earlier, it is going to be a leading edge
      StartTime = dDchRaw[2*PrevId].time;                   // Store time as it may change later 
    } 
  
  for (loop = LNibble+1; loop<TNibble; loop++)
    {
      if (loop>=0 && loop<48) 
	PrevId = nibbleArray[arm][side][plane][cell][loop];
      else 
	PrevId = -1;
      
      if (PrevId != -1) dDchRaw[2*PrevId].time = -10;       // If in the continuation of the current pulse other pulse found
                                                            // remove it. Negative time removes the hit from reconstruction 
                                                            // both in FEMmodule and Candidatory 
    }
  
  if (TNibble>=0 && TNibble<48)                             // Check the trailing edge of the current pulse
    PrevId = nibbleArray[arm][side][plane][cell][TNibble];
  else 
    PrevId = -1;
  
  if (PrevId != -1)                                         // If it merged with the other hit 
    {
      if (dDchRaw[2*PrevId].time != -10 && dDchRaw[2*CurId+1].time > dDchRaw[2*PrevId].time)  // Hit's leading edge is in the same nibble
	{
	  dDchRaw[2*PrevId].time = -10;                     // remove if it starts before the current trailing edge
	  EndId = PrevId;
	}
      
      if (dDchRaw[2*PrevId].time == -10 && dDchRaw[2*PrevId+1].time > dDchRaw[2*CurId+1].time)  // Was already removed
	EndId = PrevId;
    }
  
  if (dDchRaw[2*StartId].time == -10) dDchRaw[2*StartId].time = StartTime;   // If Leading edge removed, restore it - this is our merged hit
                                                                             // othervise StartId is CurId
  if (StartId != CurId)  dDchRaw[2*CurId].time = -10;                        // If leading edge is from different hit, remove current hit
  
  LN = (int) floor((float)dDchRaw[2*StartId].time/16.);                      // Merged Leading edge nibble
  TN = (int) floor((float)dDchRaw[2*EndId+1].time/16.);                      // Merged Trailing edje nibble
  for (loop = LN; loop<=TN; loop++)
    {
      if (loop>=0 && loop<48) 
	nibbleArray[arm][side][plane][cell][loop] = StartId;
    }
  
}

void mNewDchSimulator::SimulateUVBackDrift(const PHLine &Wire, const PHPoint &W, const PHVector &C, const PHLine &T)
{
  
  double cos_a = cos(BackInclAngle);
  double sin_a = sin(BackInclAngle);

  PHVector Cross2(C.getX()*cos_a-C.getY()*sin_a,                    // Region defining vector, rotated -45
		  C.getX()*sin_a+C.getY()*cos_a, 0.);
  PHVector Cross1(C.getX()*cos_a+C.getY()*sin_a,                    
		 -C.getX()*sin_a+C.getY()*cos_a, 0.);
  
  PHPoint LowerPlanePoint1(W.getX() - Cross1.getX()*BackDriftWidth,
			   W.getY() - Cross1.getY()*BackDriftWidth, 0.);
  PHPlane LowerPlane1(LowerPlanePoint1,Cross1);
 
  PHPoint UpperPlanePoint1(W.getX() + Cross1.getX()*BackDriftWidth,
			   W.getY() + Cross1.getY()*BackDriftWidth, 0.);
  PHPlane UpperPlane1(UpperPlanePoint1,Cross1);
  
  PHPoint LowerPlanePoint2(W.getX() - Cross2.getX()*BackDriftWidth,
			   W.getY() - Cross2.getY()*BackDriftWidth, 0.);
  PHPlane LowerPlane2(LowerPlanePoint2,Cross2);
  
  PHPoint UpperPlanePoint2(W.getX() + Cross2.getX()*BackDriftWidth,
			   W.getY() + Cross2.getY()*BackDriftWidth, 0.);
  PHPlane UpperPlane2(UpperPlanePoint2,Cross2);
  
  // Real cell crossing points for the track
  PHPoint RealIn1(0.,0.,0.);
  PHPoint RealOut1(0.,0.,0.);
  PHPoint RealIn2(0.,0.,0.);
  PHPoint RealOut2(0.,0.,0.);
 
  PHBoolean intin1, intout1, intin2, intout2;
  
  intin1  = intersectionLinePlane(T, LowerPlane1, RealIn1);
  intout1 = intersectionLinePlane(T, UpperPlane1, RealOut1);
  intin2  = intersectionLinePlane(T, LowerPlane2, RealIn2);
  intout2 = intersectionLinePlane(T, UpperPlane2, RealOut2);
  
  double din1 = 100.; 
  double dout1 = 100.;
  double din2 = 100.; 
  double dout2 = 100.;
    
  if (intin1)  din1  = distanceLinePoint(Wire, RealIn1);
  if (intout1) dout1 = distanceLinePoint(Wire, RealOut1);
  if (intin2)  din2  = distanceLinePoint(Wire, RealIn2);
  if (intout2) dout2 = distanceLinePoint(Wire, RealOut2);
  
  PHPoint LowerPlanePoint3(W.getX() + C.getX()*(BackDriftShift - BackDriftWidth),
			   W.getY() + C.getY()*(BackDriftShift - BackDriftWidth), 0.);
  PHPlane LowerPlane3(LowerPlanePoint3,C);
  
  PHPoint UpperPlanePoint3(W.getX() + C.getX()*(BackDriftShift + BackDriftWidth),
			   W.getY() + C.getY()*(BackDriftShift + BackDriftWidth), 0.);
  PHPlane UpperPlane3(UpperPlanePoint3,C);
  
  PHPoint LowerPlanePoint4(W.getX() + C.getX()*(-BackDriftShift - BackDriftWidth),
			   W.getY() + C.getY()*(-BackDriftShift - BackDriftWidth), 0.);
  PHPlane LowerPlane4(LowerPlanePoint4,C);
  
  PHPoint UpperPlanePoint4(W.getX() + C.getX()*(-BackDriftShift + BackDriftWidth),
			   W.getY() + C.getY()*(-BackDriftShift + BackDriftWidth), 0.);
  PHPlane UpperPlane4(UpperPlanePoint4,C);
    
  // Real cell crossing points for the track
  PHPoint RealIn3(0.,0.,0.);
  PHPoint RealOut3(0.,0.,0.);
  PHPoint RealIn4(0.,0.,0.);
  PHPoint RealOut4(0.,0.,0.);
 
  PHBoolean intin3, intout3, intin4, intout4;

  intin3  = intersectionLinePlane(T, LowerPlane3, RealIn3);
  intout3 = intersectionLinePlane(T, UpperPlane3, RealOut3);
  intin4  = intersectionLinePlane(T, LowerPlane4, RealIn4);
  intout4 = intersectionLinePlane(T, UpperPlane4, RealOut4);
  
  double din3 = 100.; 
  double dout3 = 100.;
  double din4 = 100.; 
  double dout4 = 100.;
    
  if (intin3)  din3  = distanceLinePoint(Wire, RealIn3);
  if (intout3) dout3 = distanceLinePoint(Wire, RealOut3);
  if (intin4)  din4  = distanceLinePoint(Wire, RealIn4);
  if (intout4) dout4 = distanceLinePoint(Wire, RealOut4);

  double distin1, distout1,distin2,distout2;  
  double D = BackDriftShift - BackDriftWidth;
  double H1 = (D*cos_a + BackDriftWidth)/sin_a;
  double L1 = sqrt(D*D + H1*H1);

  if (din1>L1) 
    { 
      din1 = L1;
      distin1 = sqrt(din3*din3-D*D)-sqrt(din1*din1-D*D) + din1; 
    }
  else 
    distin1 = din1;

  if (dout2>L1) 
    { 
      dout2 = L1;
      distout2 = sqrt(dout4*dout4-D*D)-sqrt(dout2*dout2-D*D) + dout2; 
    }
  else 
    distout2 = dout2;
  
  D = BackDriftShift + BackDriftWidth;
  double H2 = (D*cos_a - BackDriftWidth)/sin_a;
  double L2 = sqrt(D*D + H2*H2);
   
  if (dout1>L2) 
    { 
      dout1 = L2;
      distout1 = sqrt(dout3*dout3-D*D)-sqrt(dout1*dout1-D*D) + dout1; 
    }
  else 
    distout1 = dout1;

  if (din2>L2) 
    { 
      din2 = L2;
      distin2 = sqrt(din4*din4-D*D)-sqrt(din2*din2-D*D) + din2; 
    }
  else 
    distin2 = din2;

  if (distin1<distout1) 
    dist_b1 = distin1;
  else 
    dist_b1 = distout1;

  if (distin2<distout2) 
    dist_b2 = distin2;
  else 
    dist_b2 = distout2;
}

void mNewDchSimulator::setSmearConstants(const double G_dv_sig, const double K_dv_sig, const double W_dv_sig, const double W_t0_sig) 
{
  short a,p,k;
  
  Glob_dv_sig = G_dv_sig;
  Key_dv_sig  = K_dv_sig;
  SW_dv_sig   = W_dv_sig;
  SW_t0_sig   = W_t0_sig;
  for (a=0 ;a<2 ;a++){
    for (p=0 ;p<40; p++){
      for (k=0 ;k<20; k++){
	Key_dv_cor[a][p][k] = gsl_ran_gaussian(rand,Key_dv_sig);
      }
    }
  }
}
