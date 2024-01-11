//
//  TKH--Updated to use new calibration constant interface.
//       This routine is PURELY MonteCarlo and uses the
//       "Nominal" Routines.
//                           11-25-2001
//
#define __histostuff__ 0

#include <iostream> 
#include <fstream>
#include <list> 
#include <gsl/gsl_const.h>
#include <PHIODataNode.h>

#include "mNewDchFastSimulator.hh" 
#include "PHDchGeometryObject.h" 
#include "PHDchCalibrationObject.h" 
#include "PHDchNoiseObject.h" 

#include "dcghitWrapper.h" 
#include "dDchFastSimParWrapper.h"  
#include "dDchRawWrapper.h" 
#include "dDchHitWrapper.h" 
#include "dDchGhitHitsWrapper.h" 
#include "dDchGhitRawWrapper.h" 
 
//INCLUDECHECKER: Removed this line: #include "table_header.h" 
#include "PHGeometry.h"

#include "DchSimPar.h" 
#include "utiPrototype.hh" 

using namespace PHGeometry;
using namespace std;
 
typedef PHIODataNode<dcghitWrapper> dcghitNode_t; 
typedef PHIODataNode<dDchFastSimParWrapper> dDchFastSimParNode_t; 
typedef PHIODataNode<dDchRawWrapper> dDchRawNode_t; 
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t; 
typedef PHIODataNode<dDchGhitHitsWrapper> dDchGhitHitsNode_t; 
typedef PHIODataNode<dDchGhitRawWrapper> dDchGhitRawNode_t; 
 
typedef PHIODataNode<PHTable> TableNode_t; 
 
// speed of light 29979 cm/us  
static const float C = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e6;

mNewDchFastSimulator::mNewDchFastSimulator() 
{
  PropRegion = 0.25;
  BackdriftCutoff = 0.20;
  WidthofDriftChannel = 0.15;
  mirrorHits = 0;

} 
 
mNewDchFastSimulator::~mNewDchFastSimulator(){} 
 
PHBoolean mNewDchFastSimulator::event(PHCompositeNode *root) 
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
 
  initialize(root);
  // cout << "initializing FastSim" << endl;  (excess output line removed, CFM 5/29/2005, use iDebug check)
  
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
     if (!w) { 
       return 1; 
     } 
     n = new TableNode_t(w,"dcghit"); 
     outNode->addNode(n); 
  } 
  nodes.append(n); 
 
  outNode = parNode; 
  n = i.findFirst("PHIODataNode", "dDchFastSimPar"); 
  if (!n) { 
    cout << "ERROR:  'inout' parameter dDchFastSimPar not found" << endl; 
     w = new dDchFastSimParWrapper("dDchFastSimPar", 1); 
     if (!w) { 
       return 1; 
     } 
     n = new TableNode_t(w,"dDchFastSimPar"); 
     outNode->addNode(n); 
  } 
  nodes.append(n); 
 
  outNode = dchNode; 
  j = new PHNodeIterator(outNode); 
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchRaw")))) { 
     w = new dDchRawWrapper("dDchRaw", 60000); 
     if (!w) { 
       return 1; 
     } 
     d = new TableNode_t(w,"dDchRaw"); 
     outNode->addNode(d); 
  } 
  delete j; 
  nodes.append(d); 
 
  outNode = dstNode; 
  j = new PHNodeIterator(outNode); 
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchHit")))) { 
     w = new dDchHitWrapper("dDchHit", 60000); 
     if (!w) { 
       return 1; 
     } 
     d = new TableNode_t(w,"dDchHit"); 
     outNode->addNode(d); 
  } 
  delete j; 
  nodes.append(d); 
 
  outNode = dchNode; 
  j = new PHNodeIterator(outNode); 
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchGhitHits")))) { 
     w = new dDchGhitHitsWrapper("dDchGhitHits", 60000); 
     if (!w) { 
       return 1; 
     } 
     d = new TableNode_t(w,"dDchGhitHits"); 
     outNode->addNode(d); 
  } 
  delete j; 
  nodes.append(d); 
 
  outNode = dchNode; 
  j = new PHNodeIterator(outNode); 
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchGhitRaw")))) { 
     w = new dDchGhitRawWrapper("dDchGhitRaw", 60000); 
     if (!w) { 
       return 1; 
     } 
     d = new TableNode_t(w,"dDchGhitRaw"); 
     outNode->addNode(d); 
  } 
  delete j; 
  nodes.append(d); 
 
  dchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO"); 
  if (!dchDaoNode) { 
     PHMessage("mNewDchFastSimulator::event",PHError,"DAO missing in the tree"); 
  }else{ 
    dchAddressObject = dchDaoNode->getData(); 
  } 
 
  dchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode","DchDGO"); 
  if (!dchDgoNode) { 
    PHMessage("mNewDchFastSimulator::event",PHError,"DGO missing in the tree"); 
  }else{ 
    dchGeometryObject = dchDgoNode->getData(); 
  } 
 
  dchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode","DchDCO"); 
  if (!dchDcoNode) { 
    PHMessage("mNewDchFastSimulator::event",PHError,"DCO missing in the tree"); 
  }else{ 
    dchCalibrationObject = dchDcoNode->getData();
  } 
   
  dchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode","DchDNO"); 
  if (!dchDnoNode) { 
    PHMessage("mNewDchFastSimulator::event",PHError,"DNO missing in the tree"); 
  }else{ 
    dchNoiseObject = dchDnoNode->getData(); 
  } 

  if(mirrorHits <= 0) cout <<" DchFastSim: MirrorHits switched off!" << endl;
  return callPAM(nodes); 
} 
 
PHBoolean mNewDchFastSimulator::callPAM(PHPointerList<PHNode> &nl) 
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
  DDCHFASTSIMPAR_ST *dDchFastSimPar; 
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
  dDchFastSimPar   = n2->getData()->TableData(); 
  dDchRaw_h        = n3->getData()->TableHeader(); 
  dDchRaw          = n3->getData()->TableData(); 
  dDchHit_h        = n4->getData()->TableHeader(); 
  dDchHit          = n4->getData()->TableData(); 
  dDchGhitHits_h   = n5->getData()->TableHeader(); 
  dDchGhitHits     = n5->getData()->TableData(); 
  dDchGhitRaw_h    = n6->getData()->TableHeader(); 
  dDchGhitRaw      = n6->getData()->TableData(); 
  
  long irndm = dDchFastSimPar[0].randseed; 
   
  DchSimPar   simulationPar(&dDchFastSimPar_h, dDchFastSimPar); 
 
  float driftVelocity = dchCalibrationObject->getNominalDriftVelocity(EAST) /
                        dchCalibrationObject->getBinSize();
   
  // setup pointer list used for hit merging 
  list<short> fired[numberOfArms][numberOfPlanes][numberOfCells][numberOfSides]; 
  short new_hit; 

  // keep copy of dcghit localy 
  int mc_hits; 
  mc_hits = dcghit_h.nok;
  short plane,cell,side;
  unsigned int arm;
  short globalIndex;
  short idraw,idraw1,idraw2;
  short edge; 
  float tof;
   
  //----------------------------------- 
  // main loop over GEANT hits 
  //------------------------------------ 
  short ihit; 
  short hitcounter = 0; 
  short rawcounter = 0; 
  short proc_hits = 0;
  short raw_hits = 0;

  for (ihit = 0; ihit < mc_hits; ihit++){ 
    // set flag to store hit (1 == store) 
    new_hit = 1; 
     
    // check for efficiency first 

    if (dcghit[ihit].xyzinglo[0] < 0){
      arm = EAST;
    }
    else{
      arm = WEST; 
    }

    plane = dcghit[ihit].plane; 

    if (arm == WEST){
      cell = dcghit[ihit].cell; 
    } 
    else{ 
      cell = numberOfCells - dcghit[ihit].cell - 1;
    }
    
    if (dcghit[ihit].xyzinglo[2] < 0){ // positive and negative z are separate 
      side = SOUTH; 
    } 
    else{ 
      side = NORTH; 
    }   

    dchAddressObject->setSoft(arm,side,plane,cell);
    globalIndex = dchAddressObject->getGlobalIndex()->getValue();

    // get information about drift cell in cell coordinates
    //=========================================================

    float Zmax = getZmax(arm, plane);                    // z-coordinate of north end of wire
    float DeltaX = getDeltaX(arm, plane);                // x-coordinate of north end of wire
    float northy = (float) getnorth_y(arm,plane);
    float northz = (float) getnorth_z(arm,plane);
    float sign = (float) getsign(arm, plane);            

    PHPoint NorthLocal, SouthLocal;
    NorthLocal.setX( DeltaX );
    NorthLocal.setZ( Zmax );
    SouthLocal.setX( -DeltaX );
    SouthLocal.setZ( -Zmax ); 

    PHLine wireLine( SouthLocal, NorthLocal );
    PHVector wireLineDirection = wireLine.getDirection(); 
   
    // get information about hit in cell coordinates
    //==================================================

    float xin,yin,zin;  
    float xout,yout,zout;  

        xin = dcghit[ihit].xyzinloc[0];
        yin = dcghit[ihit].xyzinloc[1];
        zin = dcghit[ihit].xyzinloc[2];

        xout = dcghit[ihit].xyzoutloc[0];
        yout = dcghit[ihit].xyzoutloc[1];
        zout = dcghit[ihit].xyzoutloc[2];

	// Width vs. alpha is not being calculated temporary  SB  05/14/01 
	
	// this is the width due to alpha
	float alpha_width = 0.; 

    float z = 0.5*(zin + zout);      // z-coordinate of hit

    // Point of entering the drift cell
    // also available in global coordinates

    // Defining entrance and exit points of the hit
     
    if( xin == xout && yin == yout && zin == zout) continue;  
  
    PHPoint in(xin,yin,zin);
    PHPoint out(xout,yout,zout); 

    PHLine track(in, out);
    PHVector track_dir = track.getDirection();
    track_dir.normalize();

    float track_x, track_y;
    track_x = track_dir.getX();
    track_y = track_dir.getY();

    if( track_dir.getX() == 0. && track_dir.getY() == 0. && track_dir.getZ() == 0. )
       { cout << "track_dir is 000 " << endl; }

    PHVector driftDirection;  // in cell coordinates
    {
      float temp1 = northy*( 2.* z / Zmax); // y-component at z of hit 
      PHVector temp2( sqrt(1 - (temp1*temp1) - (northz*northz)) , temp1 , northz);  //direction of drift at z of hit 
      driftDirection = temp2;
    }  //mark

    // implement pan-modell
    //=====================
   
        float C_Radius =  getPropRegion();
	dchGeometryObject->setDistanceToDriftRegion( C_Radius );
        float L_Cutoff =  getBackdriftCutoff();
        float CH_Width =  getWidthOfDriftChannel() ;  // width from middle to margin of channel

    // normal vector on driftregion confining planes

    PHVector CH_Vec = wireLineDirection.cross(driftDirection);
    CH_Vec.normalize();

    float chvec_x, chvec_y;
    chvec_x = CH_Vec.getX();
    chvec_y = CH_Vec.getY();
     
    PHVector gap;

    gap.setX( CH_Width * CH_Vec.getX() );
    gap.setY( CH_Width * CH_Vec.getY() );
    gap.setZ( CH_Width * CH_Vec.getZ() );

    float point_x = (float) gap.getX();
    float point_y = (float) gap.getY();
    float point_z = (float) gap.getZ();
 
    //  driftregion confining planes
         
    PHPoint   CH_UPPoint;
    PHPoint CH_DOWNPoint;

    CH_UPPoint.setX(point_x);
    CH_UPPoint.setY(point_y);
    CH_UPPoint.setZ(point_z);

    CH_DOWNPoint.setX( -point_x);
    CH_DOWNPoint.setY( -point_y);
    CH_DOWNPoint.setZ( -point_z);
     
    PHPlane CH_UPPlane( CH_UPPoint, gap);
    PHPlane CH_DOWNPlane( CH_DOWNPoint, gap);

    PHPlane CH_MIDPlane;
    {
      PHPoint origin( 0., 0., 0.);
      PHPlane temp1 ( origin, gap);
      CH_MIDPlane = temp1;
    } 

    // Calculate alpha

    float alpha = 400.;

    if (track_x == 0 && track_y == 0 ){
      alpha = 0;
    }
    else{
      PHVector CH_Vec_flat(chvec_x, chvec_y, 0.), track_dir_flat(track_x, track_y, 0.);
      PHAngle temp = angle( track_dir_flat,CH_Vec_flat );
      alpha = temp.degree();
    }
    if ( chvec_x < track_x){
      alpha = - alpha;
    }

    // Calculate drift distance to wire

    PHPoint nearest_Point;
    {
      PHAngle wire_track = track_dir.angle(wireLineDirection);  //check if track and wire are parallel
      if ( wire_track  == (double) Pi || (double) wire_track == 0.){
	PHVector ez( 0., 0., 1.);
	PHPoint origin( 0.,0.,0.);
	PHPlane Base( origin, ez);
     	intersectionLinePlane( track, Base, nearest_Point );
      }
      else{
      	nearest_Point = closestApproachLineLine(wireLine, track);
      }
    }
        
    float line_dist = (float) distanceLinePoint(wireLine, nearest_Point);
    float dist;
     
    float region = 0;

    if (line_dist < C_Radius ){
      region=1;
      dist = line_dist;
    }
    else{
      PHPoint cross1, cross2, cross3;
      intersectionLinePlane(track, CH_UPPlane, cross1);
      intersectionLinePlane(track, CH_DOWNPlane, cross2);
      intersectionLinePlane(track, CH_MIDPlane, cross3);

      dist = (float) distanceLinePoint( wireLine, cross1);
      region=2;
      nearest_Point = cross1;
      float regist;
      regist = (float) distanceLinePoint( wireLine, cross2);
      if ( regist < dist){ 
	dist = regist; 
	region=3;
	nearest_Point = cross2;
      }
      regist = (float) distanceLinePoint( wireLine, cross3);
      if ( regist < dist){
	dist = regist;
	region = 4;
	nearest_Point = cross3;
      }
    }

    // look down for dist dependent efficiency

    float effi_dist = 1;
    float mean_effi = dchNoiseObject->getEfficiency(globalIndex);
    if( plane<12 ) effi_dist = mean_effi  - ((dist-0.8) * 0.0092564);
    else if(plane > 19 && plane < 32) effi_dist = mean_effi - ((dist-0.8) * 0.00997485);

    if ( (plane>11 && plane<20) || plane>31 ) effi_dist = 0.95;
    
    if (utiRandom(&irndm) > effi_dist ) continue; 

    float x_drift = nearest_Point.getX() - z*DeltaX/Zmax;
    if (x_drift > 2.5) 
      {
	cout << " FastSim: x_drift = " << x_drift <<" probably too large!" <<  endl;
	cout << "   arm: " << arm << endl;
	cout << "   plane: " << plane << endl;
	cout << "   cell: " << cell << endl;
	cout << "   dist: " << dist << endl;
	cout << "   z: " << z << endl;
	cout << "   line_dist: " << line_dist << endl;
	cout << "   xin: " << xin << endl;
	cout << "   xout: " << xout << endl;
	cout << "   yin: " << yin << endl;
	cout << "   yout: " << yout << endl;
	cout << "   zin: " << zin << endl;
	cout << "   zout: " << zout << endl;
	cout << "   DeltaX: " << DeltaX << endl;
	cout << "   Zmax: " << Zmax << endl;
	cout << "   Region: "<< region << endl;
	cout << "   nearest_Point.getX(): " << nearest_Point.getX() << endl;
	nearest_Point.print();
      }

    float x_dist = (xin + xout)*0.5 - z*DeltaX/Zmax;
    if (x_dist > 2.5) cout << " FastSim: x_dist = " << x_dist <<" probably too large!" <<  endl;

    tof   = dcghit[ihit].tof; 
    float sdist,width; 

    // check if particle produces a signal in chamber 
    //  
    //   a) hit is in proportional region  
    //  
    //   b) back drift protection was inefficient 
    //  
    //   c) positive drift direction and positive drift distance 
    // 
    //   d) negatice drift direction and negative drift distance 
    // 
    // if it produced a signal add resolution 
      
    if (fabs(dist) > C_Radius ){ // not in proportional region 
      if (utiRandom(&irndm) > simulationPar.getBackWireInefficiency()){  // back drift protection 
        if (x_drift < 0 && sign > 0) continue; // wrong side jump out 
        if (x_drift > 0 && sign < 0) continue; // wrong side jump out 
      }
      float temp = simulationPar.getDriftResolution(dist);
      utiGaussian(&dist,&temp,&irndm, &dist); 
    } 
    else{ // in proportional region 
      if ( ( x_drift * sign) < 0 && fabs(x_drift) > L_Cutoff ){
	PHPoint temp( (-1.* (float) sign) * L_Cutoff,0.,0.);
	PHVector ex(1., 0., 0.);
	PHPlane L_Plane( temp, ex);
	intersectionLinePlane(track, L_Plane, temp);
	float tempdist = distanceLinePoint(wireLine, temp);
	x_drift = (sign * L_Cutoff);  //only true for X-wires!!!
	if (xin == xout ) 
	  {
	    tempdist = fabs(xin);     //only true for X-wires!!!
	    x_drift = xin;
	  }

	if ( tempdist < C_Radius && fabs(x_drift) <= L_Cutoff ){
	  dist = fabs(tempdist);
	  region = 5;
	}
	else { dist = 999.; continue; }
	}    
	       
      float temp = simulationPar.getDriftResolution(dist);
      utiGaussian(&dist,&temp,&irndm, &dist); 
    }
      
    float temp1 = simulationPar.getWidthOfHit(plane, dist, C_Radius);  
    float temp2 = simulationPar.getSigmaWidthOfHit(plane, dist,C_Radius); 

    utiGaussian(&temp1, &temp2, &irndm, &width); 

    //check if X-wire and adjust width to length of drift cell
    if(plane < 12 || ( plane > 19 && plane < 32) )
      {
	if( (dist + width > length[arm][plane] ) && (length[arm][plane] > dist)  )
	  {
	    width = length[arm][plane] - dist;
	  }
      }

    if (width < simulationPar.getWidthCut())  continue; // lower cutoff on width 

    sdist = dist/fabs(dist);        // need sign later to create line  

    if (width < alpha_width) width = alpha_width;
   
     // this GEANT hit produced a signal in the chamber 
      
    if (simulationPar.getCorrectToF()) { 
      // correct for TOF: C speed of light 29979 cm/us  
      float correction = (tof - dchGeometryObject->getRadiusAtZ0(plane)/C)*driftVelocity; 
      dist = dist + correction;
    } 

    if (simulationPar.getMergeTrackFlag()) { 
      if (!fired[arm][plane][cell][side].empty() && simulationPar.getMergeTrackFlag()) {   
	list<short>::const_iterator it;
	short hitID;
 	for (it = fired[arm][plane][cell][side].begin();
	     it != fired[arm][plane][cell][side].end(); it++) {
	  hitID = *it;
          float old_dist  = dDchHit[hitID].distance; 
          float old_width = dDchHit[hitID].width; 
            
          // new hit is leading edge                              
          if ( dist < old_dist && old_dist < fabs(dist)+width) { 
            // replace leading edge in hit table 
            dDchHit[hitID].distance = dist;  
            dDchGhitHits[hitID].ghitid = dcghit[ihit].id; 
            // get corresponding raw data and change  
            idraw = dDchHit[hitID].idraw1; 
            edge  = 0; 
            dDchRaw[idraw].time = dchCalibrationObject->transformDistanceToNominalTime(dist,edge); 
            dDchGhitRaw[idraw].ghitid = dcghit[ihit].id; 
              
            new_hit = 0;           // do not store this hit again 
              
            // old hit trailing edge 
            if ( old_dist+old_width > dist+width) { 
              dDchHit[hitID].width = old_dist+old_width-dist;              
              // new  hit also trailing edge 
            } else {  
              dDchHit[hitID].width = width; 
              // get corresponding raw data and change  
              idraw = dDchHit[hitID].idraw2; 
              edge  = 1; 
              dDchRaw[idraw].time = dchCalibrationObject->transformDistanceToNominalTime(dist+width,edge); 
              dDchGhitRaw[idraw].ghitid = dcghit[ihit].id; 
            }                      
            // old hit gives leading edge  
          } else if ( old_dist < dist && dist < old_dist+old_width) { 
            // new hit trailing edge 
            if ( dist+width > old_dist+old_width) { 
              dDchHit[hitID].width = dist+width - dDchHit[hitID].distance; 
              // get corresponding raw data and change  
              idraw = dDchHit[hitID].idraw2; 
              edge  = 1; 
              dDchRaw[idraw].time = dchCalibrationObject->transformDistanceToNominalTime(dist+width,edge); 
              dDchGhitRaw[idraw].ghitid = dcghit[ihit].id; 
            }  
            // old hit trailing edge --> no action  
            new_hit = 0;        // do not store this hit again 
          } 
        } 
      } 
    } // end of Merge Track

    float drift_time;
    drift_time = (float) dchCalibrationObject->transformDistanceToNominalTime(dist,edge); 
 
    // new hit created 
    // 
    // store data in STAF tables now:  raw data, hit, and MC reference 
    // 
    if (new_hit == 1 && (hitcounter <= dDchHit_h.maxlen-1)  
        && (rawcounter <= dDchRaw_h.maxlen-1)) { 
      // leading edge raw data 
      dDchRaw_h.nok++; 
      dDchRaw[rawcounter].id        = rawcounter; 
      dDchRaw[rawcounter].global    = globalIndex; 
      dDchRaw[rawcounter].arm       = arm;  
      dDchRaw[rawcounter].plane     = plane;  
      dDchRaw[rawcounter].cell      = cell;  
      dDchRaw[rawcounter].side      = side;  
      edge                          = 0; 
      dDchRaw[rawcounter].edge      = edge;  
      long leading_time             = dchCalibrationObject->transformDistanceToNominalTime(dist,edge); 
      dDchRaw[rawcounter].time      = leading_time;
      idraw1                        = rawcounter; 
      dDchGhitRaw[rawcounter].ghitid = dcghit[ihit].id; 
      dDchGhitRaw[rawcounter].rawid = rawcounter; 
      dDchGhitRaw_h.nok++; 
      rawcounter ++; 
      raw_hits = rawcounter;

      // trailing edge raw data 
      dDchRaw_h.nok++;  
      dDchRaw[rawcounter].id        = rawcounter; 
      dDchRaw[rawcounter].global    = globalIndex; 
      dDchRaw[rawcounter].arm       = arm;  
      dDchRaw[rawcounter].plane     = plane;   
      dDchRaw[rawcounter].cell      = cell;  
      dDchRaw[rawcounter].side      = side;  
      edge                          = 1; 
      dDchRaw[rawcounter].edge      = edge;  
      float d = dist+width; 
      long trailing_time            = dchCalibrationObject->transformDistanceToNominalTime(d,edge); 
      if (trailing_time-leading_time <3) trailing_time = leading_time+3; 
      dDchRaw[rawcounter].time      = trailing_time;  
      idraw2                        = rawcounter; 
      dDchGhitRaw[rawcounter].ghitid = dcghit[ihit].id; 
      dDchGhitRaw[rawcounter].rawid = rawcounter; 
      dDchGhitRaw_h.nok++; 
      rawcounter ++; 
              
      dDchHit_h.nok++; 
      dDchHit[hitcounter].id        = hitcounter; 
      dDchHit[hitcounter].arm       = arm ; 
      dDchHit[hitcounter].plane     = plane;  
      dDchHit[hitcounter].cell      = cell ; 
      dDchHit[hitcounter].side      = side; 
      dDchHit[hitcounter].distance  = dist;
      dDchHit[hitcounter].time1     = (long)drift_time;
      dDchHit[hitcounter].time2     = (long)99999.;
      
      dDchHit[hitcounter].width     = width;  
      dDchHit[hitcounter].idraw1    = idraw1; 
      dDchHit[hitcounter].idraw2    = idraw2; 
      dDchHit[hitcounter].idmirror  = -1; 
        
      PHLine line;                 // need sign of distance 
      line = dchGeometryObject->transformDistanceToLine (arm,plane,cell,sdist*dist); 
      dDchHit[hitcounter].xyz[0]  = line.getBasepoint().getX(); 
      dDchHit[hitcounter].xyz[1]  = line.getBasepoint().getY(); 
      dDchHit[hitcounter].xyz[2]  = line.getBasepoint().getZ(); 
      dDchHit[hitcounter].vxyz[0] = line.getDirection().getX(); 
      dDchHit[hitcounter].vxyz[1] = line.getDirection().getY(); 
      dDchHit[hitcounter].vxyz[2] = line.getDirection().getZ(); 
        
      fired[arm][plane][cell][side].push_back(dDchHit[hitcounter].id); 
        
      dDchGhitHits[hitcounter].ghitid = dcghit[ihit].id; 
      dDchGhitHits[hitcounter].hitsid = hitcounter; 
      dDchGhitHits_h.nok++; 
        
      hitcounter ++; 
      proc_hits = hitcounter;

      if ( fabs(dist) < C_Radius && mirrorHits > 0 ){
	// for backdrift protection is taken care above
        // hit in proportional region 
        // add second copy to hit list 
        // but line constructed with -distance  
        dDchHit[hitcounter-1].idmirror  = hitcounter; 
        dDchHit[hitcounter].idmirror    = hitcounter-1; 
          
        dDchHit_h.nok++; 
        dDchHit[hitcounter].id        = hitcounter; 
        dDchHit[hitcounter].arm       = arm ; 
        dDchHit[hitcounter].plane     = plane;  
        dDchHit[hitcounter].cell      = cell ; 
        dDchHit[hitcounter].side      = side; 
        dDchHit[hitcounter].distance  = dist; 
	
        dDchHit[hitcounter].width     = width;  
        dDchHit[hitcounter].idraw1    = idraw1; 
        dDchHit[hitcounter].idraw2    = idraw2; 
          
        PHLine line;                         // need sign of distance 
        line = dchGeometryObject->transformDistanceToLine(arm,plane,cell,-sdist*dist); 
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
    } else if (new_hit == 1) { 
      cout << " DCHit table is full " << ihit << endl; 
    } 

#if __histostuff__ == 1

    float array[31];
    float dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy15, dummy16, dummy17, dummy18, dummy19, dummy20, dummy21, dummy22, dummy23, dummy24, dummy25, dummy26, dummy27, dummy28, dummy29, dummy30, dummy31; 

     dummy1 = x_dist;
     dummy2 = dist;
     dummy3 = drift_time;
     dummy4 = width;
     dummy5 = raw_time;   
     dummy6 = 1.;
     dummy7 = region;
     dummy8 = point_z;
     dummy9 = DeltaX;
     dummy10 = Zmax;
     dummy11 = z;
     dummy12 = raw_dist;
     dummy13 = chvec_y;
     dummy14 = chvec_z;
     dummy15 = xin;
     dummy16 = dist;
     dummy17 = xglo;
     dummy18 = yglo;
     dummy19 = zglo;
     dummy20 = xin;
     dummy21 = yin;
     dummy22 = zin;
     dummy23 = xout;
     dummy24 = yout;
     dummy25 = zout;
     dummy26 = dist;
     dummy27 = sign;
     dummy28 = sign;
     dummy29 = plane;
     dummy30 = arm;
     dummy31 = line_dist;

     int m = 0;
  
  array[m++] = dummy1;
  array[m++] = dummy2;
  array[m++] = dummy3;
  array[m++] = dummy4;
  array[m++] = dummy5;
  array[m++] = dummy6;
  array[m++] = dummy7;
  array[m++] = dummy8;
  array[m++] = dummy9;
  array[m++] = dummy10;
  array[m++] = dummy11;
  array[m++] = dummy12;
  array[m++] = dummy13;
  array[m++] = dummy14;
  array[m++] = dummy15;
  array[m++] = dummy16;
  array[m++] = dummy17;
  array[m++] = dummy18;
  array[m++] = dummy19;
  array[m++] = dummy20;
  array[m++] = dummy21;
  array[m++] = dummy22;
  array[m++] = dummy23;
  array[m++] = dummy24;
  array[m++] = dummy25;
  array[m++] = dummy26;
  array[m++] = dummy27;
  array[m++] = dummy28;
  array[m++] = dummy29;
  array[m++] = dummy30;
  array[m++] = dummy31;

     dchHistogrammer->fillSimulation(array);

#endif

  }       
 
  n1->getData()->SetRowCount(dcghit_h.nok); 
  n2->getData()->SetRowCount(dDchFastSimPar_h.nok); 
  n3->getData()->SetRowCount(dDchRaw_h.nok); 
  n4->getData()->SetRowCount(dDchHit_h.nok); 
  n5->getData()->SetRowCount(dDchGhitHits_h.nok); 
  n6->getData()->SetRowCount(dDchGhitRaw_h.nok);
      
  cout << "number of hits: " << hitcounter << endl;
  cout << "raw_hits: " << raw_hits << endl;
  cout << "proc_hits: " << proc_hits << endl;

  return True; 
  
} 

int mNewDchFastSimulator::initialize(PHCompositeNode* root){ 
  PHNodeIterator it(root);
  PHDchGeometryObject*    initGeometryObject = 0;
  PHDataNode<PHDchGeometryObject>*    initDgoNode = 0; 

  initDgoNode = (PHDataNode<PHDchGeometryObject>*)it.findFirst("PHDataNode","DchDGO"); 
  if (!initDgoNode) { 
    PHMessage("mNewDchFastSimulator::event",PHError,"DGO missing in the tree"); 
  }else{ 
    initGeometryObject = initDgoNode->getData(); 
    } 
  
  // Parameters of pan modell
    cout <<"Parameter: "<< PropRegion << "  " << BackdriftCutoff << "  " << WidthofDriftChannel << endl;

  for (unsigned int  armxx=0 ; armxx<=1 ; armxx++){
    for (int  planexx=0 ;planexx<=39; planexx++){
      // get the wire in global coordinates
      PdbDchWire GloWire = initGeometryObject->getWire(armxx,planexx,1);
      PHPoint NP = GloWire.getNorthPoint();
      PHPoint SP = GloWire.getSouthPoint();

      // computing length of drift cell
      // only valid for X-wires!!!
      {
	float tmp1, tmp2;
	tmp1 = NP.getX();
	tmp2 = NP.getY();
	float radius = sqrt( (tmp1 * tmp1) + (tmp2 * tmp2) );
	length[armxx][planexx] = radius * 9.81779246679/1000. ;
      }

      float sx, sy, nx, ny;
      sx = SP.getX(); 
      sy = SP.getY(); 
      nx = NP.getX(); 
      ny = NP.getY(); 
	  
      PHVector gloDriftNorth = GloWire.getNorthDrift();
      PHVector gloDriftSouth = GloWire.getSouthDrift();
      gloDriftNorth.normalize();
      gloDriftSouth.normalize();
      PHVector n_cross_s = gloDriftNorth.cross( gloDriftSouth );
		 
      // sign_view == 1/-1 means, the cell looks in +x/-x direction
      // drift-direction defined from wire to particle!
      float sign_view = (float) 2.*( (armxx == EAST) - 0.5) *  (gloDriftNorth.getY()/fabs(gloDriftNorth.getY()));
      sign[armxx][planexx] = sign_view;

      // sign_Phi_north == 1/-1 means driftvector north is in anti-clockwise/clockwise turn direction from
      // driftvector south watched from north

      float sign_Phi_north = 0. ;  
      
      if ( n_cross_s.getZ() != 0.) // check if UV wire
	{
	  sign_Phi_north = (float) (- n_cross_s.getZ()/fabs(n_cross_s.getZ()) );
	}
      
      
      // Calculate the half angle between gloDriftNorth and gloDriftSouth
      
      float DeltaPhi;
      if ( gloDriftNorth.getX() == gloDriftSouth.getX() && 
	   gloDriftNorth.getY() == gloDriftSouth.getY() && 
	   gloDriftNorth.getZ() == gloDriftSouth.getZ() ){
	DeltaPhi = 0.;
      }
      else{
       PHAngle DeltaPhiDummy = gloDriftNorth.angle( gloDriftSouth );
       DeltaPhi = sign_Phi_north * ((float) DeltaPhiDummy.degree()) *
	 M_PI / (180.0 * 2.0);
      }

      // drift vectors in cell coordinates
      PHVector cellDriftNorth;
      PHVector cellDriftSouth;
      {

        float x_flat = cos(DeltaPhi);
	float y_flat = sin(DeltaPhi);
	float z_drift = gloDriftNorth.getZ();
	float x_drift = sqrt( (x_flat * x_flat) * (1-(z_drift * z_drift))  )  ;
	float y_drift = sqrt( (y_flat * y_flat) * (1-(z_drift * z_drift))  )  ;
	  
	PHVector temp1(  - sign_view * x_drift ,- sign_view * sign_Phi_north * y_drift , z_drift);
	cellDriftNorth = temp1;
	PHVector temp2(  - sign_view * x_drift ,  sign_view * sign_Phi_north * y_drift , z_drift);
	cellDriftSouth = temp2;
      }
	 
      // to be changed to vectors
      north_x[armxx][planexx] = cellDriftNorth.getX();
      north_y[armxx][planexx] = cellDriftNorth.getY();
      north_z[armxx][planexx] = cellDriftNorth.getZ();

      Phi[armxx][planexx] = DeltaPhi;

      Zmax[armxx][planexx] = sqrt( (NP.getX()-SP.getX())*(NP.getX()-SP.getX())+
				   (NP.getY()-SP.getY())*(NP.getY()-SP.getY())+
				   (NP.getZ()-SP.getZ())*(NP.getZ()-SP.getZ()))/2 ;
      DX[armxx][planexx] = 0;
      if ( nx != sx  || ny != sy ){
	DX[armxx][planexx] = sqrt((nx - sx)*(nx - sx) +( ny - sy)*(ny - sy))/2;
	if ( (armxx == EAST &&  ny < sy) || (armxx == WEST && ny > sy) ){
	  DX[armxx][planexx] = -DX[armxx][planexx];
	}
      }
    }	  
  }

  return 1;
}

int mNewDchFastSimulator::print( int armxx, int planemin, int planemax ){
  for ( int planexx=planemin;  planexx<=planemax; planexx++){
    cout << "   plane = " << planexx << endl;
    cout << "      DeltaX = " <<DX[armxx][planexx]<< endl;
    cout << "      Zmax   = " <<Zmax[armxx][planexx]<< endl;
    cout << "      Side   = " <<sign[armxx][planexx]<< endl;
  }
  return 1;
}

int mNewDchFastSimulator::print(){
  cout << "calling print()..." ;
  ofstream fout("wires.local.dat");
  for ( int armxx=0; armxx<=1; armxx++){
    for ( int planexx=0; planexx<40; planexx++){
      fout << " arm = " <<  armxx << "   plane = " << planexx << endl;
      fout << "      DeltaX = " <<DX[armxx][planexx]<< endl;
      fout << "      Zmax   = " <<Zmax[armxx][planexx]<< endl;
      fout << "      Side   = " <<sign[armxx][planexx]<< endl;
    }
  }
  fout.close();
  cout << "writing wires.local.dat" << endl;
  return 1;
}




