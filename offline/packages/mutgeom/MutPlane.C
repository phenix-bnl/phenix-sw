// $Id: MutPlane.C,v 1.70 2016/03/21 18:26:18 shlim Exp $

/*!
   \file    MutPlane.C
   \brief   Muon tracker Plane (cathode/anode)
   \author  Douglas Fields, Nicki Bruner, Hugo Pereira
   \version $Revision: 1.70 $
   \date    $Date: 2016/03/21 18:26:18 $
*/

#include <MutDCMChannelMap.h>
#include <MutStrip.h>
#include <MutWire.h>
#include <TMutDatabaseCntrl.h>

#include <PdbCalBank.hh>
#include <PdbCoordinate.hh>
#include <PdbMutDCMMap.hh>

#include <PHGeometry.h>

#include <cassert>
#include <vector>
#include <sstream>
#include <iostream>
#include <fstream>

using namespace PHGeometry;
using namespace std;
using namespace MUTGEOM;

//____________________________________________________________________
MutPlane::MutPlane(const MutArm* Arm,
  const MutStation* Station,
  const MutOctant* Octant,
  const MutHalfOctant* HalfOctant,
  const MutGap* Gap,
  const PlaneNumber& PlaneNum):
  f_pArm(Arm),
  f_pStation(Station),
  f_pOctant(Octant),
  f_pHalfOctant(HalfOctant),
  f_pGap(Gap),
  fPlaneNum(PlaneNum)
{

  /*  Determine Plane's global position and normal vector from Gap
      position/vector and known configuration.  The Wire plane and Gap
      share position/vector; translate Cathode 1 and Cathode2 positions
      plus or minus gapWidth/2.
      The translation of plane positions wrt the gap position is
      adjusted based on the gap's normal vector.

      This is also a convenient place to set the number of elements.
  */
  fGlobalPosition=f_pGap->getGlobalPosition();
  fGlobalVector=f_pGap->getGlobalVector();

  //Gap width is total cathode to cathode width.
  double gapWidth = f_pGap->getGapThickness();

  planeOffset= fGlobalVector * (-1.0 * gapWidth/2 * (fPlaneNum-1));
  fGlobalPosition=fGlobalPosition + planeOffset;

  // alocate number of elemetents for "wire" planes
  if( fPlaneNum == Wire )
  {

    name = "Wire";
    switch(getStation()) {

      case Station1:
      NumElements = 94;  //counting only sense wires, not calib wires
      break;

      case Station2:
      if(getArm() == North) {
        if(getOctant()%2) NumElements = 186;
        else NumElements = 176;
      } else {
        if(getOctant()%2) NumElements = 147;
        else NumElements = 138;
      }
      break;

      case Station3:
      if(getArm() == North) NumElements = 320;
      else NumElements = 221;
      break;
    }

    f_pMutWires.resize(NumElements,0);

  } else {

    // set plane name
    if( fPlaneNum == Cathode1 ) name = "Cathode 1";
    else if( fPlaneNum == Cathode2 ) name = "Cathode 2";
    else {
      ostringstream what;
      what << "MutPlane::MutPlane - invalid plane number " << fPlaneNum;
      throw runtime_error( what.str() );
    }

    // allocate number of elements
    switch(getStation()) {
      case Station1:
      NumElements = 55; //This will be reset again as strips coords are read in.
      break;

      case Station2:
      if(getArm() == North) NumElements = 96;
      else NumElements = 80;
      break;

      case Station3:
      if(getArm() == North) {
        if(fPlaneNum==Cathode1) NumElements = 158;
        else NumElements = 160;
      } else NumElements = 120;
      break;
    }

    f_pMutStrips.resize(NumElements,0);
  }

  /* Create Wire or Strip Objects and store pointers to them.

  The positions of the first wires for each station are input and
  the positions of each succeeding wire are calculated from the
  position of the first.  The station 1 first wire positions are for the
  first wires in an entire Octant, so the second halfOctant wire
  positions are calculated using an offset.
  */

  if(fPlaneNum == Wire) {

    // create wires
    switch( getStation() )
    {

      case Station1:
      createSt1Wires();
      break;

      case Station2:
      createSt2Wires();
      break;

      case Station3:
      createSt3Wires();
      break;

      default:
      throw std::runtime_error( "MutPlane - invalid station" );
      break;
    }

    // assign invalid strip spacing
    stripPerpSpacing = -1.0;

  } else {

    // create strips
    switch( getStation() )
    {

      case Station1:
      if(f_pArm->readFromDB && !TMutDatabaseCntrl::get_database_access( "use_local_st1_autocad_file" )) fetchSt1Strips();
      else createSt1Strips();
      break;

      case Station2:
      createSt2Strips();
      break;

      case Station3:
      createSt3Strips();
      break;

      default:
      throw std::runtime_error( "MutPlane - invalid station" );
      break;
    }

    // get DCM channels and map to wires
    if( f_pArm->readFromDB && !TMutDatabaseCntrl::get_database_access( "use_local_dcm_map_file" ) ) fetchDCMChannels();
    else getDCMChannels();

    // strip spacing
    setStripSpacing();

  }
}

//____________________________________________________________________
MutPlane::~MutPlane()
{

  for( unsigned int i=0; i < f_pMutWires.size(); i++ ) if( f_pMutWires[i] ) delete f_pMutWires[i];
  for( unsigned int i=0; i < f_pMutStrips.size(); i++ ) if( f_pMutStrips[i] ) delete f_pMutStrips[i];
  f_pMutWires.clear();
  f_pMutStrips.clear();

}

//____________________________________________________________________
void MutPlane::AddWire(const int& WireNum, MutWire* wire)
{

  // check index overflow
  if( WireNum >= int(f_pMutWires.size()) )
  {
    ostringstream what;
    what << "MutPlane::AddWire - invalid index " << WireNum << " vs " << f_pMutWires.size();
    throw runtime_error( what.str() );
  }

  // check if wire already alocated
  if( f_pMutWires[WireNum] )
  {
    ostringstream what;
    what << "MutPlane::AddWire - wire index " << WireNum << " already created";
    throw runtime_error( what.str() );
  }

  // alocate wire
  f_pMutWires[WireNum] = wire;
  return;

}

//____________________________________________________________________
void MutPlane::AddStrip(const int& StripNum, MutStrip* strip)
{

  if( StripNum >= int(f_pMutStrips.size()) )
  {
    ostringstream what;
    what << "MutPlane::AddStrip - invalid index " << StripNum << " vs " << f_pMutStrips.size();
    throw runtime_error( what.str() );
  }

  // check if wire already alocated
  if( f_pMutStrips[StripNum] )
  {
    ostringstream what;
    what << "MutPlane::AddStrip - strip index " << StripNum << " already created";
    throw runtime_error( what.str() );
  }

  f_pMutStrips[StripNum] = strip;
  return;

}

//____________________________________________________________________
void MutPlane::createSt1Wires()
{
  //First create wires in the Octant frame.
  // spacing is 1.084cm along the pin strip
  double wireSpacing = f_pStation->getWireSpacing();

  PHPoint wire1Begin, wire1End;      // position of wire 1 wrt pin1 for

  // South HalfOct 1/Oct1,3,5,7 or
  // South HalfOct 0/Oct2,4,6,8
  PHPoint wire1Bend;  // point where wire bends around the 22.5 degree rib

  wire1Begin.setX(82.206);// wrt pin1 Oct 1, HalfO 1
  wire1Begin.setY(2.139);// wrt pin1 Oct 1, HalfO 1

  wire1End.setX(90.547);// wrt pin1 Oct 1, HalfO 1
  wire1End.setY(19.522);// wrt pin1 Oct 1, HalfO 1

  wire1Bend.setX(83.605);
  wire1Bend.setY(9.129);

  //Actual wire1 positions are determined from wire1Begin, wire1Bend,
  //and wire1End according to halfOctants
  PHPoint HOBegin = wire1Begin;
  PHPoint HOEnd = wire1Bend;

  //The wire begin and end positions must advance along the direction of
  //the rib to which they are attached.
  PHVector beginRibDirection(1,0,0);
  PHVector endRibDirection(cos(M_PI/8),sin(M_PI/8),0);
  PHFrame globalFrame;

  //There are sign flips below based on the way the Octant reference
  //frames are defined.

  if(getOctant()%2) {  //Octants 2,4,6,8 (fOctantNums 1,3,5,7)

    wire1Begin.setX(wire1Begin.getX() * -1.0);
    wire1Bend.setX(wire1Bend.getX() * -1.0);
    wire1End.setX(wire1End.getX() * -1.0);
    HOBegin = wire1Begin;
    HOEnd = wire1Bend;
    if((getHalfOctant()&&getArm()==South)||(!getHalfOctant()&&getArm()==North)){
      HOBegin = wire1Bend;
      HOEnd = wire1End;
      beginRibDirection = endRibDirection;
      endRibDirection.setX(cos(Pi/4));
      endRibDirection.setY(sin(Pi/4));
    }

  }
  else { //Octants 1,3,5,7
    if((getHalfOctant()&&getArm()==South)||(!getHalfOctant()&&getArm()==North)){
      beginRibDirection.setX(beginRibDirection.getX() * -1.0);
      endRibDirection.setX(endRibDirection.getX() * -1.0);
    }
    else {

      HOBegin = wire1Bend;
      HOEnd = wire1End;
      beginRibDirection = endRibDirection;
      beginRibDirection.setX(beginRibDirection.getX() * -1.0);
      endRibDirection.setX(-cos(Pi/4));
      endRibDirection.setY(sin(Pi/4));

    }
  }
  for (int j=0; j<NumElements; j++) {

    MutWire* p = new MutWire(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,f_pGap,this,j);
    PHPoint wireBegin = HOBegin + beginRibDirection * (j * wireSpacing);
    PHPoint wireEnd = HOEnd + endRibDirection * (j * wireSpacing);

    wireBegin = rotateAndTranslate(f_pOctant->octFrame,wireBegin,globalFrame);
    wireEnd = rotateAndTranslate(f_pOctant->octFrame,wireEnd,globalFrame);

    wireBegin = wireBegin + planeOffset + f_pGap->getGapOffset();
    wireEnd = wireEnd + planeOffset + f_pGap->getGapOffset();

    //flip begin and end positions for oct 0,2,4,6 to
    //have consistent definition.
    //Begin to end is in counterclockwise direction.
    if((getOctant()%2&&getArm()==South) || (getOctant()%2==0&&getArm()==North))
      p->SetGlobalGeom(wireBegin, wireEnd);
    else p->SetGlobalGeom(wireEnd, wireBegin);

    // register wire
    AddWire( j, p );

  }
}

//____________________________________________________________________
void MutPlane::createSt2Wires()
{

    //First create wires in the Octant frame.
  double wireSpacing = f_pStation->getWireSpacing(); // 1.0196cm along the pin strip

  PHVector beginRibDirection(0,1,0);
  PHVector endRibDirection(sin(M_PI/8),cos(M_PI/8),0);
  PHVector wireDirection(-cos(M_PI/16),sin(M_PI/16),0);
  PHFrame globalFrame;

  PHPoint wire1Begin(0,8.5986,0);

  PHPoint xmaxPt;
  if(getArm()==North) {
    //some North parameters have their signs reversed only to be reversed again.
    //This is to incorporate the north arm with as little code change as possible.
    endRibDirection.setX(endRibDirection.getX()*-1.0);
    wireDirection.setX(wireDirection.getX()*-1.0);

    if(getOctant()%2) {
      xmaxPt.setX(93.512);
      xmaxPt.setY(25.346);
    } else {
      xmaxPt.setX(89.566);
      xmaxPt.setY(26.064);
    }
  } else {
    if(getOctant()%2) {
      xmaxPt.setX(-79.066);
      xmaxPt.setY(21.920);
    } else {
      xmaxPt.setX(-75.555);
      xmaxPt.setY(21.303);
    }
  }

  if(getHalfOctant()) {
    xmaxPt.setX(xmaxPt.getX()*-1.0);
    endRibDirection.setX(endRibDirection.getX()*-1.0);
    wireDirection.setX(wireDirection.getX()*-1.0);
  }

  PHLine endRib(xmaxPt, endRibDirection);

  for (int j=0; j<NumElements; j++)
  {

    MutWire* p = new MutWire(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,f_pGap,this, j);
    PHPoint wireBegin = wire1Begin + beginRibDirection * ((NumElements-1-j) *  wireSpacing);
    PHLine wire(wireBegin, wireDirection);
    PHPoint wireEnd = closestApproachLineLine(wire,endRib);

    wireBegin = rotateAndTranslate(f_pOctant->octFrame,wireBegin,globalFrame);
    wireEnd = rotateAndTranslate(f_pOctant->octFrame,wireEnd,globalFrame);

    wireBegin = wireBegin + planeOffset + f_pGap->getGapOffset();
    wireEnd = wireEnd + planeOffset + f_pGap->getGapOffset();

    /*
      flip begin and end positions for half-octant 0 to
      have consistent definition.
      Begin to end is in counterclockwise direction.
    */
    if(getHalfOctant()) p->SetGlobalGeom(wireBegin, wireEnd);
    else p->SetGlobalGeom(wireEnd, wireBegin);

    AddWire( j, p );

  }
}

//____________________________________________________________________
void MutPlane::createSt3Wires()
{

  bool use_geometry_fix( TMutDatabaseCntrl::get_database_access( "use_st3_geometry_fix" ) );
  static bool first( true );
  if( first )
  { 
    first = false;
    cout << "MutPlane::createSt3Wires - use_geometry_fix: " << (use_geometry_fix ? "true":"false" ) << endl;
  }
  
  // need to retrieve the same half-octant and octant-frames that are used to define 
  // station3 wires, so that anode and cathode planes are parallel.
  PHFrame half_octant_frame;
  PHFrame octant_frame;
  getSt3SurveyFrame( half_octant_frame, octant_frame );
    
  // need to set proper axis direction for half octant frame
  if( getArm() == South ) { half_octant_frame = PHFrame( half_octant_frame.getOrigin(), -half_octant_frame.getU(), half_octant_frame.getV(), -half_octant_frame.getW() ); }
  else { half_octant_frame = PHFrame( half_octant_frame.getOrigin(), half_octant_frame.getU(), -half_octant_frame.getV(), -half_octant_frame.getW() ); }
    
  PHVector translation;
  PHMatrix rotation;
  frames2MatrixAndVector( half_octant_frame, octant_frame, rotation, translation );
  
  // rotation between octant axis to half-octant axis, inside half-octant frame
  // this is needed because the way the wires are defined match the octant axis, although they
  // should be set in the half octant frame axis system
  // this does not depend on any survey data
  PHMatrix wire_rotation;
  if( ( getArm() == South && getHalfOctant() ) || ( getArm() == North && !getHalfOctant() ) )
  {
    
    wire_rotation = PHMatrix( 
      PHVector( cos( M_PI/16), +sin(M_PI/16), 0 ), 
      PHVector( -sin( M_PI/16), cos(M_PI/16), 0 ), 
      PHVector( 0, 0, 1 ) );
    
  } else {
    
    wire_rotation = PHMatrix( 
      PHVector( cos( M_PI/16), -sin(M_PI/16), 0 ), 
      PHVector( +sin( M_PI/16), cos(M_PI/16), 0 ), 
      PHVector( 0, 0, 1 ) );
    
  }    
  
  // translation between octant origin and half octant origin, inside half-octant frame
  PHFrame globalFrame;
  PHPoint wire_origin = rotateAndTranslate( globalFrame, octant_frame.getOrigin(), half_octant_frame);
  wire_origin.setZ(0);

  // wire spacing is 1.01958cm along the pin strip
  double wireSpacing = f_pStation->getWireSpacing();

  // first wire position with respect to pin1
  // in octant frame
  PHPoint wire1Begin, wire1Bend, wire1End;
  if( getArm() == South )
  {
    
    wire1Begin = PHPoint( 44.783, 223.56, 0 );
    wire1Bend = PHPoint( 76.11, 217.33, 0 );
    wire1End = PHPoint( 107.45, 223.56, 0 );
    
  } else {

    wire1Begin = PHPoint( 137.3166, 317.1014, 0 );
    wire1Bend = PHPoint( 101.93, 310.0726, 0);
    wire1End = PHPoint( 66.5473, 317.1107, 0 );

  }

  //the 22.5 degree rib
  PHVector BeginRibDirection(sin(M_PI/8),cos(M_PI/8),0);
  PHVector yAxis(0,1,0);
  PHVector EndRibDirection = yAxis;

  if(getArm()==North) BeginRibDirection.setX(BeginRibDirection.getX()*-1.0);

  if(getHalfOctant()) {
    
    EndRibDirection = BeginRibDirection;
    EndRibDirection.setX(EndRibDirection.getX()*-1.0);
    BeginRibDirection = yAxis;
    wire1Begin=wire1Bend;
    
  } else wire1End=wire1Bend;

  for (int j=0; j<NumElements; j++)
  {

    MutWire* p = new MutWire(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,f_pGap,this,j);
    PHPoint wireBegin = wire1Begin - BeginRibDirection * (j * wireSpacing);
    PHPoint wireEnd = wire1End - EndRibDirection * (j * wireSpacing);

    if( use_geometry_fix )
    {
      
      // first transform from octant coordinates to half_octant coordinates,
      // in half-octant frame
      wireBegin = transformPoint( wire_rotation, wire_origin, wireBegin );
      wireEnd = transformPoint( wire_rotation, wire_origin, wireEnd );
      
      // transform from half-octant frame to octant frame
      wireBegin = transformPoint(rotation,translation,wireBegin);
      wireEnd = transformPoint(rotation,translation,wireEnd);
      
    }

    // transform from octant frame to global frame
    wireBegin = rotateAndTranslate(f_pOctant->octFrame,wireBegin,globalFrame);
    wireEnd = rotateAndTranslate(f_pOctant->octFrame,wireEnd,globalFrame);
    
    // add necessary z offsets
    wireBegin = wireBegin + planeOffset + f_pGap->getGapOffset();
    wireEnd = wireEnd + planeOffset + f_pGap->getGapOffset();
    
    // assign position to wires
    p->SetGlobalGeom(wireBegin, wireEnd);
    
    // and save in array
    AddWire( j, p );

  }
}

//____________________________________________________________________
void MutPlane::createSt1Strips()
{
  /* Determining the number of elements in a Station 1 strip plane is trickier
     than for a wire plane since there are different configurations.
     The Station 1 strip positions are read in from a file created from an
     AutoCad drawing.  Get file for strip configurations and determine the
     number of elements based on # of file entries.
  */
  string fileString;
  string directory("/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/");


  fileString = directory + getSt1AutocadFile() + ".dat";

  static const int bufsize = 256;
  char line[bufsize];
  int num; // strip number from file or db
  int northNum = -1; // to reverse the strip numbering for the North Arm
  double a,b,c,d,e,f; //data place holders
  PHFrame globalFrame;

  ifstream s(fileString.c_str());
  if (!s){
    cout<<"Error opening geometry file "<<fileString.c_str()<<". \n";
    return;
  }


  //The north arm strip numbering is reversed from south.
  //So see how many strips are in the file and change the input index accordingly.
  if(getArm()==North)
  { while(s.getline(line,bufsize,'\n')) northNum++; }

  //Create a new strip for each strip position read from the file.

  s.close();
  ifstream s2(fileString.c_str());

  while(s2.getline(line,bufsize,'\n')){
    istringstream stringbuf(line);
    stringbuf >> num >> a >> b >> c >> d >> e >> f;

    if(getArm()==North) num = northNum - num;
    MutStrip* p = new MutStrip(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,f_pGap,this, num);

    /*
      Geometry files give strip begin and end positions wrt pin1 in x,y.
      File positions are in cm.
      Then convert these to global coordinates.
    */

    PHPoint begin(a,b,c);
    PHPoint end(d,e,f);
    begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
    end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
    begin = begin + planeOffset + f_pGap->getGapOffset();
    end = end + planeOffset + f_pGap->getGapOffset();
    p->SetGlobalGeom(begin, end);

    AddStrip( num, p );
  }

  NumElements = num+1;
  if(getArm()==North) NumElements = northNum+1;
  s2.close();

  //
  f_pMutStrips.resize(NumElements,0);

}

//____________________________________________________________________
void MutPlane::createSt2Strips()
{
  /* All station 2 strips begin at the same position in y (2.29) and most
     end at the same position in x (+/- 0.25), in the octant reference frame.
     "endLine" defines the termination point for every strip.
     Create the strips in a local octant frame and then translate/rotate to
     the global frame.
     The local frame used to create the strips is that same one used in the
     etching process - the outer radial edge of the octant defines the x-axis.
     The y-axis is the octant centerline.  So straight strips are at an
     angle of 11.25 degrees to the centerline.
     Station 2 differs from the others stations in that its stereo planes are
     cathode 2.
     From IP: straight (11.25 degrees from centerline - perp to wires)
     stereo   (3.75 degrees from centerline)
     straight (11.25 degrees from centerline)
     stereo   (7.5 degrees from centerline)
     straight (11.25 degrees from centerline)
     stereo   (0 degrees from centerline)

  */

  //1cm center to center on a line parallel
  //to the top.
  double stripSpacing = 1.0; 
  if(getArm()==North) stripSpacing = -1.0;

  PHVector xDirection(1,0,0);
  PHVector beginRibDirection(0,1,0);
  PHVector endRibDirection(sin(M_PI/8),cos(M_PI/8),0);
  PHVector stripAngle(sin(M_PI/16), cos(M_PI/16), 0); //11.25 degrees
  PHFrame globalFrame;

  PHPoint endFirstStrip(0.50,2.29,0);//first strip end point in cm
  PHPoint begin;                        //each strip's begin point
  PHPoint end;                        //each strip's end point
  PHPoint begin2(-0.250,2.29,0);           //edge of center rib.
  //will be filled later with strip alternate end point.

  PHPoint ymaxPt;     //distance to inner radius of station 2
  PHPoint xmaxPt; //point on the rib defining edge of active area

  if(fPlaneNum==Cathode2){
    switch(getGap()){
    case Gap1:
      stripAngle.setX(sin(0.0654498)); // (Pi/48 or 3.75 degrees)
      stripAngle.setY(cos(0.0654498));
      break;
    case Gap2:
      stripAngle.setX(sin(0.1308996)); // (Pi/24 or 7.5 degrees)
      stripAngle.setY(cos(0.1308996));
      break;
    case Gap3:
      stripAngle = beginRibDirection;
      break;
    }
  }

  if(getArm()==North) {
    //some North parameters have their signs reversed only to be reversed again.
    //This is to incorporate the north arm with as little code change as possible.
    endRibDirection.setX(endRibDirection.getX()*-1.0);
    stripAngle.setX(stripAngle.getX()*-1.0);
    begin2.setX(begin2.getX()*-1.0);
    endFirstStrip.setX(endFirstStrip.getX()*-1.0);
    if(getOctant()%2) {
      ymaxPt.setY(194.993);
      xmaxPt.setX(93.512);
      xmaxPt.setY(25.346);
    } else {
      ymaxPt.setY(186.260);
      xmaxPt.setX(89.566);
      xmaxPt.setY(26.064);
    }
  } else {
    if(getOctant()%2) {
      ymaxPt.setY(155.043);
      xmaxPt.setX(-79.066);
      xmaxPt.setY(21.920);
    } else {
      ymaxPt.setY(145.949);
      xmaxPt.setX(-75.555);
      xmaxPt.setY(21.303);
    }
  }

  if(getHalfOctant()) {
    xmaxPt.setX(xmaxPt.getX()*-1.0);
    endRibDirection.setX(endRibDirection.getX()*-1.0);
    stripAngle.setX(stripAngle.getX()*-1.0);
    begin2.setX(begin2.getX()*-1.0);
  } else {
    endFirstStrip.setX(endFirstStrip.getX()*-1.0 -stripSpacing*(NumElements-1));
  }

  PHLine beginRib(begin2, beginRibDirection);
  PHLine endRib(xmaxPt, endRibDirection);
  PHLine yMax(ymaxPt, xDirection);

  for (int j=0; j<NumElements; j++) {
    MutStrip* p = new MutStrip(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,f_pGap,this,j);

    end = endFirstStrip + xDirection*(j*stripSpacing);

    PHLine stripLine(end,stripAngle);
    begin = closestApproachLineLine(stripLine,endRib);
    if(begin.getY()>ymaxPt.getY()) begin = closestApproachLineLine(stripLine,yMax);

    if(stripAngle.getX()!=beginRibDirection.getX()) {
      begin2 = closestApproachLineLine(stripLine,beginRib);
      if(begin.getY()>begin2.getY()) begin=begin2;
    }

    begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
    end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
    begin = begin + planeOffset + f_pGap->getGapOffset();
    end = end + planeOffset + f_pGap->getGapOffset();
    p->SetGlobalGeom(begin, end);

    AddStrip( j, p );

  }
}

//____________________________________________________________________
void MutPlane::createSt3Strips()
{

  /*  Station 3 strip positions must be determined from survey positions.
      One groove in the copper plane was surveyed on each South arm stereo
      half-octant.  The South straight planes and all North arm planes had
      two surveyed grooves.
      These grooves lie between one readout and one non-readout strip.
      The coordinate system used in the surveys is based on alignment
      pins in the half-octants and is different for each cathode plane
      and half-octant.  Lastly, the grooves were not surveyed on their
      begin and end positions. Strip positions are determined from lines
      defined by the grooves terminated at the edges of the active area.

      To be compatible with pisa geometry reads, the strip position
      conversion goes from half-octant reference frame to octant
      reference frame to global coordinates.
      Strip surveys are in files which have been converted to cm
      and stored in the database.

      steps:
      1) read in the survey data
      2) create the half-octant reference frame and establish the edges
      of the active area.
      3) create the strips.  This is treated differently for each plane.
      4) transform strips to global coordinates.
  */

  // translates the half octant frame into the octant frame
  // (for later translation from octant frame to global frame)
  // all strip locations are defined in this half-octant frame
  PHFrame half_octant_frame;
  PHFrame octant_frame;
  getSt3SurveyFrame( half_octant_frame, octant_frame );

  PHVector translation;
  PHMatrix rotation;
  frames2MatrixAndVector( half_octant_frame, octant_frame, rotation, translation );

  /////////////////////////////////////////////
  double surveyData[13];
  if(f_pArm->readFromDB) {
    if(!fetchSt3StripSrvyData(surveyData)) return;
  } else {
    if(!fileReadSt3StripSrvyData(surveyData)) return;
  }

  double pin2x =surveyData[0];
  double line1x1 =surveyData[5];
  double line1y1 =surveyData[6];
  double line1x2 =surveyData[7];
  double line1y2 =surveyData[8];

  double line2x1 =0.0;
  double line2x2 =0.0;
  double line2y1 =0.0;
  double line2y2 =0.0;

  if(fPlaneNum==Cathode2 && getArm()==South) 
  {
    // 0 degree planes have more survey data.
    line1x2 =surveyData[6];
    line2x1 =surveyData[7];
    line2x2 =surveyData[8];
    line1y1 =surveyData[9];
    line1y2 =surveyData[10];
    line2y1 =surveyData[11];
    line2y2 =surveyData[12];
  }

  if(getArm()==North)
  {
    line1x1 =surveyData[0];
    line1y1 =surveyData[1];
    line1x2 =surveyData[4];
    line1y2 =surveyData[5];
    line2x1 =surveyData[2];
    line2y1 =surveyData[3];
    line2x2 =surveyData[6];
    line2y2 =surveyData[7];
    pin2x = 1.0;
  }

  //Step 2: ------------------------------------------------

  // These are the surveyed points in the grooves.
  PHPoint firstx1y1(line1x1,line1y1,0);
  PHPoint firstx2y2(line1x2,line1y2,0);
  PHPoint secondx1y1(line2x1,line2y1,0);
  PHPoint secondx2y2(line2x2,line2y2,0);

  // These lines are the surveyed grooves.
  PHLine surveyLine1(firstx1y1,firstx2y2);
  PHLine surveyLine2(secondx2y2, secondx1y1);

  //Define the half-octant reference frame used in the groove surveys.
  PHPoint pin2(pin2x,0,0);
  PHPoint halfOctFrameOrigin;
  PHVector halfOctFramexAxis = pin2 - halfOctFrameOrigin;
  halfOctFramexAxis.normalize();
  PHLine xAxisLine(halfOctFrameOrigin, pin2);

  //These points and vectors define the edges of the active area.
  double ymax = 229.2;
  if(getArm()==North) ymax = -324.54;

  PHVector Rib1Direction(sin(M_PI/16), cos(M_PI/16),0);
  PHVector Rib2Direction(-sin(M_PI/16), cos(M_PI/16),0);
  PHPoint edgePoint0(-1.578,ymax,0); // point on edge of active area
                                     // for Half-octant 0
  PHPoint edgePoint1(57.578,ymax,0);
  PHPoint ribPoint0(32.11,228.5,0); // point on rib for Half-octant 0
  PHPoint ribPoint1(23.87,228.5,0); // same point in the halfOct 1 frame
  if(getArm()==North) {
    edgePoint0.setX(-36.21);
    edgePoint1.setX(36.21);
    ribPoint0.setY(ymax);
    ribPoint1.setY(ymax);
    ribPoint0.setX(-71.36);
    ribPoint1.setX(71.36);
  }

  PHLine Rib1(edgePoint0, Rib1Direction);
  PHLine Rib2(ribPoint0, Rib2Direction);
  PHPoint ymaxPt(0,ymax,0);

  //used to cut off strips at the inner radii
  PHLine yMaxLine(ymaxPt,halfOctFramexAxis);
  PHLine readoutPadLine = xAxisLine;
  PHPoint padHeight(0.0,2.0,0.0);
  if(getArm()==North) padHeight.setY(-2.0);
  readoutPadLine.setBasepoint(padHeight);

  // Step 3: Create the strips ----------------------------------------
  for (int j=0; j<NumElements; j++) {
    MutStrip* p = new MutStrip(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,f_pGap,this,j);
    AddStrip( j, p );
  }

  //Step 4 ------------------------------------------------------------
  /*  Fill strip positions and convert to global coordinates.
      There are two options for converting to global coordinates: 1) convert
      from half-octant frame to octant frame, then from octant frame to
      global, as with the other stations, or 2) make a half-octant frame
      with respect to global coordinates, from the octant survey, and convert
      directly from half-octant frame to global.  We are going with #1 to
      make the use of pisa geometry as input easier (possible?).
  */
  PHFrame globalFrame;
  double stripSpacing = 1.0;

  if(getArm()==South && fPlaneNum==Cathode2) {

    /*First, disable channels that don't physically exist.
      These are:
      - the last readout channel (actually last readout and last two
      ground channels) in Cathodes 2, half-octant 0.
      - the first two readout channels (actually first two readout and first
      ground channels) in Cathodes 2, half-octant 1.
    */
    if(getHalfOctant()) {
      f_pMutStrips[0]->setChannelToDead();
      f_pMutStrips[1]->setChannelToDead();
    }
    else f_pMutStrips[NumElements-1]->setChannelToDead();

    /*
      Shift from surveyed groove to readout strip.
      The closest strips are:
      90 (line2) and 74 (line1) for Cathode2 HalfOctant0
      45 (line1) and 29 (line2) for Cathode2 HalfOctant1
      There is a shift of 0.25 cm in the -x dir to get to these strips.

      The above details are true in general, the exceptions are:
      Octant 0 HalfOct 0 Gap2 Cathode2 strip 90 is +0.25cm from survey groove.
      Octant 0 HalfOct 1 Gap2 Cathode2 strip 29 is +0.25cm from survey groove.
      Octant 0 HalfOct 1 Gap1 Cathode2 strip 45 is +0.75cm from survey groove.
    */
    int line1indx = 74;
    int line2indx = 90;
    if(getHalfOctant()) {
      line1indx = 45;
      line2indx = 29;
    }

    surveyLine1.setBasepoint(surveyLine1.getBasepoint()-halfOctFramexAxis*0.25*stripSpacing);
    surveyLine2.setBasepoint(surveyLine2.getBasepoint()-halfOctFramexAxis*0.25*stripSpacing);

    if(!getOctant()){

      //shift to 45
      if(getHalfOctant()&&getGap()==Gap1)
      surveyLine1.setBasepoint(surveyLine1.getBasepoint()+halfOctFramexAxis*1.0*stripSpacing);

      //shift to 29
      if(getHalfOctant()&&getGap()==Gap2)
      surveyLine2.setBasepoint(surveyLine2.getBasepoint()+halfOctFramexAxis*0.5*stripSpacing);

      //shift to 90
      if(!getHalfOctant()&&getGap()==Gap2)
      surveyLine2.setBasepoint(surveyLine2.getBasepoint()+halfOctFramexAxis*0.5*stripSpacing);

    }

    //Each strip will have the surveyLines' angle.  We will adjust the
    //base point for each strip.
    PHLine strip=surveyLine1;
    PHPoint begin1, begin2, begin, end;

    //Don't know where Cathode2 lamination joint is - assume it is close
    //to line2.

    for (int j=30; j<NumElements; j++) 
    {
      int indx = j;
      if(!getHalfOctant()) 
      {

        //make strips 0-89
        indx = NumElements-j-1;

      } else {

        //make strips 30-119
        Rib1.setBasepoint(ribPoint1);
        Rib2.setBasepoint(edgePoint1);

      }
      strip.setBasepoint(surveyLine1.getBasepoint()+halfOctFramexAxis*((indx-line1indx)*stripSpacing));
      begin1 = closestApproachLineLine(strip,Rib1);
      begin2 = closestApproachLineLine(strip,Rib2);
      if(begin1.getY()>begin2.getY()) begin=begin2;
      else begin=begin1;
      end = closestApproachLineLine(strip,readoutPadLine);
      if(begin.getY()>ymax) begin = closestApproachLineLine(strip,yMaxLine);

      //Convert from half-octant frame to octant frame.
      begin = transformPoint(rotation,translation,begin);
      end = transformPoint(rotation,translation,end);

      //Convert from octant frame.
      begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
      end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
      begin = begin + planeOffset + f_pGap->getGapOffset();
      end = end + planeOffset + f_pGap->getGapOffset();
      f_pMutStrips[indx]->SetGlobalGeom(begin, end);

    }

    strip=surveyLine2;
    for (int j=0; j<30; j++) {
      int indx = j;
      if(!getHalfOctant()) {

        //make strips 90-119
        indx = line2indx+j;
        strip.setBasepoint(surveyLine2.getBasepoint()+halfOctFramexAxis*((indx-line2indx)*stripSpacing));
        begin = closestApproachLineLine(strip,Rib2);

      } else {

        //make strips 0-29
        strip.setBasepoint(surveyLine2.getBasepoint()+halfOctFramexAxis*((indx-line2indx)*stripSpacing));
        begin = closestApproachLineLine(strip,Rib1);

      }

      end = closestApproachLineLine(strip,readoutPadLine);
      if(begin.getY()>ymax) begin = closestApproachLineLine(strip,yMaxLine);

      //Convert from half-octant frame to octant frame.
      begin = transformPoint(rotation,translation,begin);
      end = transformPoint(rotation,translation,end);

      //Convert from octant frame.
      begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
      end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
      begin = begin + planeOffset + f_pGap->getGapOffset();
      end = end + planeOffset + f_pGap->getGapOffset();
      f_pMutStrips[indx]->SetGlobalGeom(begin, end);
    }
  }

  if(getArm()==South && fPlaneNum==Cathode1) {
    /*First, disable channels that don't physically exist.
      These are:
      - the last readout channel (actually last readout and ground channels)
      in Cathodes 1, half-octant 0.
      - the first readout channel (actually first readout and ground channels)
      in Cathodes 1, half-octant 1.
    */
    if(getHalfOctant()) f_pMutStrips[0]->setChannelToDead();
    else f_pMutStrips[NumElements-1]->setChannelToDead();

    /*  Shift from surveyed groove to readout strip.
  The closest strips are:
  31 for Cathode1 HalfOctant0
  88 for Cathode1 HalfOctant1
  There is a shift of 0.25 cm in the -x direction to get to these strips.

  The above details are true in general, the exceptions are:
  Octant 0 HalfOct 1 Gap2 Cathode1 strip 88 is +1.75cm from groove.
  Octant 2 HalfOct 1 Gap2 Cathode1 strip 88 is -0.75cm from survey groove.
  Octant 1 HalfOct 1 Gap1 Cathode1 strip 88 is +0.25cm in x direction
  from survey groove.
    */
    int line1indx = 31;
    if(getHalfOctant()) line1indx = 88;

    surveyLine1.setBasepoint(surveyLine1.getBasepoint()-
           halfOctFramexAxis*0.25*stripSpacing);
    if(getOctant()==0 && getHalfOctant() && getGap()==Gap2){//move to 88
      surveyLine1.setBasepoint(surveyLine1.getBasepoint()+
             halfOctFramexAxis*2.0*stripSpacing);
    }
    if(getOctant()==2 && getHalfOctant() && getGap()==Gap2){//move to 88
      surveyLine1.setBasepoint(surveyLine1.getBasepoint()-
             halfOctFramexAxis*0.5*stripSpacing);
    }
    if(getOctant()==1 && getHalfOctant() && getGap()==Gap1){//move to 88
      surveyLine1.setBasepoint(surveyLine1.getBasepoint()+
             halfOctFramexAxis*0.5*stripSpacing);
    }

    PHLine strip=surveyLine1;
    PHPoint begin, end;

    for (int j=0; j<NumElements; j++) {
      strip.setBasepoint(surveyLine1.getBasepoint()-halfOctFramexAxis*
       ((j-line1indx)*stripSpacing));
      if(getHalfOctant()) {
  begin = closestApproachLineLine(strip,Rib2);
      } else {
  Rib1.setBasepoint(ribPoint1);
  Rib2.setBasepoint(edgePoint1);
  begin = closestApproachLineLine(strip,Rib1);
      }
      end = closestApproachLineLine(strip,readoutPadLine);

      if(begin.getY()>ymax)
  begin = closestApproachLineLine(strip,yMaxLine);

      //Convert from half-octant frame to octant frame.
      begin = transformPoint(rotation,translation,begin);
      end = transformPoint(rotation,translation,end);
      //Convert from octant frame.
      begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
      end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
      begin = begin + planeOffset + f_pGap->getGapOffset();
      end = end + planeOffset + f_pGap->getGapOffset();
      f_pMutStrips[j]->SetGlobalGeom(begin, end);
    }
  }

  //  NORTH *****************
  if(getArm()==North) {
    /* The North arm has two groove surveys for every plane.
       line1Num and line2Num or the groove numbers of the two groove surveys.
       They differ by half-octant and plane.

       These strips were connected to adjacent, unetched patches of copper and
       should be disabled:
       cathode 2 half-octant 0 strip 159
       cathode 2 half-octant 1 strip 159
       cathode 1 half-octant 1 strip 0
    */

    //These are for half0 Cathode1
    int line1Num = 117;
    int line2Num = 32;

    if((getHalfOctant() && fPlaneNum==Cathode1) ||
       (!getHalfOctant() && fPlaneNum==Cathode2)) {
      Rib1.setBasepoint(ribPoint1);
      Rib2.setBasepoint(edgePoint1);
    }
    if(getHalfOctant() && fPlaneNum==Cathode1) {
      line1Num = 40;
      line2Num = 125;
      if(f_pMutStrips[0]) f_pMutStrips[0]->setChannelToDead();
    }
    if(getHalfOctant() && fPlaneNum==Cathode2) {
      line1Num = 49;
      line2Num = 62;
      if(f_pMutStrips[159]) f_pMutStrips[159]->setChannelToDead();
    }
    if(!getHalfOctant() && fPlaneNum==Cathode2) {
      line1Num = 109;
      line2Num = 97;
      if(f_pMutStrips[159]) f_pMutStrips[159]->setChannelToDead();
    }

    int sign = -1;  //which direction to propagate strips in the local frame.
    if(fPlaneNum==Cathode1) {
      sign = 1;
      stripSpacing = 1.01959;
    }

    //shift from surveyed line to strip center.
    surveyLine1.setBasepoint(surveyLine1.getBasepoint()+
           halfOctFramexAxis*0.25*stripSpacing*sign);
    surveyLine2.setBasepoint(surveyLine2.getBasepoint()+
           halfOctFramexAxis*0.25*stripSpacing);

    PHLine strip=surveyLine1;
    PHPoint begin;
    PHPoint end;

    if(getHalfOctant()){
      for (int j=0; j<line1Num+1; j++) {

  strip.setBasepoint(surveyLine1.getBasepoint() +
         halfOctFramexAxis*stripSpacing*(line1Num -j)*sign);

  if(fPlaneNum==Cathode1)
    begin = closestApproachLineLine(strip,Rib1);
  else
    begin = closestApproachLineLine(strip,Rib2);
  if(begin.getY()<ymax)
    begin = closestApproachLineLine(strip,yMaxLine);

  end = closestApproachLineLine(strip,readoutPadLine);
  //Convert from half-octant frame to octant frame.
  begin = transformPoint(rotation,translation,begin);
  end = transformPoint(rotation,translation,end);
  //Convert from octant frame.
  begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
  end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
  begin = begin + planeOffset + f_pGap->getGapOffset();
  end = end + planeOffset + f_pGap->getGapOffset();
  f_pMutStrips[j]->SetGlobalGeom(begin, end);
      }
      strip=surveyLine2;
      for (int j=line1Num+1; j<NumElements; j++) {

  strip.setBasepoint(surveyLine2.getBasepoint() +
         halfOctFramexAxis*stripSpacing*(line2Num -j)*sign);

  if(fPlaneNum==Cathode2) {
    begin = closestApproachLineLine(strip,Rib2);
    PHPoint begin2 = closestApproachLineLine(strip,Rib1);
    if(abs(begin.getY())>abs(begin2.getY())) begin=begin2;
  }
  else
    begin = closestApproachLineLine(strip,Rib1);

  if(begin.getY()<ymax)
    begin = closestApproachLineLine(strip,yMaxLine);

  end = closestApproachLineLine(strip,readoutPadLine);
  //Convert from half-octant frame to octant frame.
  begin = transformPoint(rotation,translation,begin);
  end = transformPoint(rotation,translation,end);
  //Convert from octant frame.
  begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
  end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
  begin = begin + planeOffset + f_pGap->getGapOffset();
  end = end + planeOffset + f_pGap->getGapOffset();

  f_pMutStrips[j]->SetGlobalGeom(begin, end);
      }
    }
    else { //half-octant 0
      for (int j=line1Num; j<NumElements; j++) {
  strip.setBasepoint(surveyLine1.getBasepoint() +
         halfOctFramexAxis*stripSpacing*(line1Num -j)*sign);
  if(fPlaneNum==Cathode2)
    begin = closestApproachLineLine(strip,Rib1);
  else
    begin = closestApproachLineLine(strip,Rib2);
  if(begin.getY()<ymax)
    begin = closestApproachLineLine(strip,yMaxLine);

  end = closestApproachLineLine(strip,readoutPadLine);

  //Convert from half-octant frame to octant frame.
  begin = transformPoint(rotation,translation,begin);
  end = transformPoint(rotation,translation,end);
  //Convert from octant frame.
  begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
  end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
  begin = begin + planeOffset + f_pGap->getGapOffset();
  end = end + planeOffset + f_pGap->getGapOffset();
  f_pMutStrips[j]->SetGlobalGeom(begin, end);
      }
      strip=surveyLine2;
      for (int j=0; j<line1Num; j++) {
  strip.setBasepoint(surveyLine2.getBasepoint() +
         halfOctFramexAxis*stripSpacing*(line2Num -j)*sign);
  if(fPlaneNum==Cathode2){
    begin = closestApproachLineLine(strip,Rib1);
    PHPoint begin2 = closestApproachLineLine(strip,Rib2);
    if(abs(begin.getY())>abs(begin2.getY())) begin=begin2;
  }
  else
    begin = closestApproachLineLine(strip,Rib2);

  if(begin.getY()<ymax)
    begin = closestApproachLineLine(strip,yMaxLine);

  end = closestApproachLineLine(strip,readoutPadLine);

  //Convert from half-octant frame to octant frame.
  begin = transformPoint(rotation,translation,begin);
  end = transformPoint(rotation,translation,end);
  //Convert from octant frame.
  begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
  end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
  begin = begin + planeOffset + f_pGap->getGapOffset();
  end = end + planeOffset + f_pGap->getGapOffset();

  f_pMutStrips[j]->SetGlobalGeom(begin, end);
      }
    }
  }
}

//____________________________________________________________________
void MutPlane::fetchSt1Strips()
{
  PdbBankID bankID=0;
  string bankString = "survey.mut.St1Strip" + getSt1AutocadFile();
  if(getArm()==North) bankString = "survey.mut.St1StripNorth" + getSt1AutocadFile();
  PHTimeStamp Tsearch = f_pArm->getArmTimeStamp();

  if(!fetch( "PdbCoordinateBank", Tsearch, bankString.c_str(), bankID)){
    cout << "MutPlane::fetchSt1Strips - fetch failed.\n";
    cout << "MutPlane::fetchSt1Strips - Station 1 strips have not been created!\n";
    return;
  }

  NumElements=getBankLength();
  f_pMutStrips.resize(NumElements,0);

  PHFrame globalFrame;
  for (int strip=0; strip<NumElements; strip++)
  {

    PdbCoordinate *DBStripPosition = (PdbCoordinate*)&geometryBank->getEntry(strip);

    PHPoint begin( DBStripPosition->getParameter(0), DBStripPosition->getParameter(1), DBStripPosition->getParameter(2));
    begin = rotateAndTranslate(f_pOctant->octFrame,begin,globalFrame);
    begin = begin + planeOffset + f_pGap->getGapOffset();

    PHPoint end( DBStripPosition->getParError(0), DBStripPosition->getParError(1), DBStripPosition->getParError(2));
    end = rotateAndTranslate(f_pOctant->octFrame,end,globalFrame);
    end = end + planeOffset + f_pGap->getGapOffset();

    // create strip, set geometry, store in array
    MutStrip* p = new MutStrip(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,f_pGap,this,strip);
    p->SetGlobalGeom(begin,end);
    AddStrip(strip, p);

  }

  if(!commit()) cout<<"Error in MutPlane::fetchSt1Strips commit.\n";

}


//____________________________________________________________________
void MutPlane::updateSt1Strips(PHTimeStamp &Tstart, PHTimeStamp &Tstop)
{
  PdbBankID bankID=0;
  string bankString = "survey.mut.St1Strip" + getSt1AutocadFile();
  if(getArm()==North) bankString = "survey.mut.St1StripNorth" + getSt1AutocadFile();
  const char *descrip = "St1 strip positions in a plane";

  if(!update( "PdbCoordinateBank", Tstart, Tstop, bankString.c_str(), bankID, descrip)){
    cout << "MutPlane::updateSt1Strips - update failed.\n";
    cout << "MutPlane::updateSt1Strips - Station 1 strips have not been stored in DB.\n";
    return;
  }

  geometryBank->setLength(NumElements);

  string fileString;
  string directory("/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/");
  fileString = directory + getSt1AutocadFile() + ".dat";

  static const int bufsize = 256;
  char line[bufsize];
  int num; // strip number from file
  float a,b,c,d,e,f; //data place holders

  ifstream s(fileString.c_str());
  if (!s){
    cout<<"MutPlane::updateSt1Strips - Error opening geometry file "<<fileString.c_str()<<". \n";
    return;
  }

  PdbCoordinate *StripPosition;

  while(s.getline(line,bufsize,'\n')){
    istringstream stringbuf(line);
    stringbuf >> num >> a >> b >> c >> d >> e >> f;
    if(getArm()==North) num = NumElements - num - 1;

    StripPosition = (PdbCoordinate*)&geometryBank->getEntry(num);
    StripPosition->setAllParameters(a,b,c);
    StripPosition->setAllParErrors(d,e,f);
  }
  if(!commit()) cout<<"Error in MutPlane::updateSt1Strips writing "<<
      fileString.c_str()<<" to db.\n";
  else cout<<fileString.c_str()<<" written to "<<bankString.c_str()<<endl;
}

//____________________________________________________________________
bool MutPlane::fileReadSt3StripSrvyData(double surveyData[13])
{
  string fileString;
  string directory("/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/");

  fileString = directory + getSt3HalfOctSrvyFile() + ".dat";

  static const int bufsize = 256;
  char line[bufsize];
  double a,b,c,d,e; //data place holders

  ifstream s(fileString.c_str());
  if (!s){
    cout<<"MutPlane::fileReadSt3StripSrvyData - Error opening geometry file "<<fileString.c_str()<<". \n";
    return false;
  }

  s.getline(line,bufsize,'\n');

  //Now get the correct line in the file.
  for (int i=0; i<getOctant(); i++) s.getline(line,bufsize,'\n');
  if(fPlaneNum==Cathode2 && getArm()==South)
  {
    //South 0 degree plane files
    //have twice the lines.
    for (int i=0; i<getOctant(); i++)
    { s.getline(line,bufsize,'\n'); }

  }

  istringstream stringbuf(line);
  stringbuf
    >> surveyData[0]>>surveyData[1]>>surveyData[2]>>surveyData[3]
    >> surveyData[4]>>surveyData[5]>>surveyData[6]>>surveyData[7]
    >> surveyData[8];

  if(getArm()==North) surveyData[8] = 0.0;
  if(fPlaneNum==Cathode2 && getArm()==South)
  {

    //South 0 degree plane,
    //need to read next line.
    s.getline(line,bufsize,'\n');
    istringstream stringbf(line);
    stringbf
      >> a >> b >> c >> d >> e >> surveyData[9] >> surveyData[10]
      >> surveyData[11] >> surveyData[12];

  } else {
    surveyData[9] = surveyData[10]= surveyData[11] = surveyData[12] = 0.0;
  }
  s.close();
  return true;
}

//____________________________________________________________________
bool MutPlane::fetchSt3StripSrvyData(double surveyData[13])
{
  PdbBankID bankID=0;
  string bankString ="survey.mut." + getSt3HalfOctSrvyFile();
  PHTimeStamp Tsearch = f_pArm->getArmTimeStamp();

  if(!fetch( "PdbCoordinateBank", Tsearch, bankString.c_str(), bankID)){
    cout << " MutPlane::fetchSt3StripSrvyData - Error in MutPlane::fetchSt3StripSrvyData,  fetch failed.\n";
    return false;
  }

  PdbCoordinate *DBStripPosition;

  int i=3*getOctant();
  DBStripPosition = (PdbCoordinate*)&geometryBank->getEntry(i);
  surveyData[0] = DBStripPosition->getParameter(0);
  surveyData[1] = DBStripPosition->getParameter(1);
  surveyData[2] = DBStripPosition->getParameter(2);
  surveyData[3] = DBStripPosition->getParError(0);
  surveyData[4] = DBStripPosition->getParError(1);
  surveyData[5] = DBStripPosition->getParError(2);
  i++;
  DBStripPosition = (PdbCoordinate*)&geometryBank->getEntry(i);
  surveyData[6] = DBStripPosition->getParameter(0);
  surveyData[7] = DBStripPosition->getParameter(1);
  surveyData[8] = DBStripPosition->getParameter(2);
  surveyData[9] = DBStripPosition->getParError(0);
  surveyData[10] = DBStripPosition->getParError(1);
  surveyData[11] = DBStripPosition->getParError(2);
  i++;
  DBStripPosition = (PdbCoordinate*)&geometryBank->getEntry(i);
  surveyData[12] = DBStripPosition->getParameter(0);

  if(!commit()) {
    cout<< PHWHERE <<"Error in MutPlane::fetchSt3StripSrvyData commit.\n";
    return false;
  }

  else return true;

}

//____________________________________________________________________________
void MutPlane::updateSt3StripSrvyData(PHTimeStamp &Tstart, PHTimeStamp &Tstop)
{
  PdbBankID bankID=0;
  string bankString = "survey.mut." + getSt3HalfOctSrvyFile();
  const char *descrip = "St3 strip positions in a half-octant";

  if(!update( "PdbCoordinateBank", Tstart, Tstop, bankString.c_str(), bankID, descrip)){
    cout << "MutPlane::updateSt3StripSrvyData - Error in MutPlane::updateSt3Strips, update failed.\n";
    cout << "MutPlane::updateSt3StripSrvyData - Station 3 strips have not been stored in DB.\n";
    return;
  }

  geometryBank->setLength(24); //3 entries for each octant

  string fileString;
  string directory("/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/");
  fileString = directory + getSt3HalfOctSrvyFile() + ".dat";

  static const int bufsize = 256;
  char line[bufsize];
  
  //data place holders
  float a(0),b(0),c(0),d(0),e(0),f(0),g(0),h(0),k(0); 

  ifstream s(fileString.c_str());
  if (!s){
    cout<< "MutPlane::updateSt3StripSrvyData - Error opening geometry file "<<fileString.c_str()<<". \n";
    if(!commit()) cout <<"MutPlane::updateSt3StripSrvyData - Error closing the database.\n";
    return;
  }

  /*  
  There is a survey file for each plane.  Each file has one or two
  lines of data for each octant.  These two lines of data are stored
  in 3 PdbCoordinates.  So 24 entries.
  */
  
  PdbCoordinate *StripPosition;
  int index =0;

  while(s.getline(line,bufsize,'\n') && index<24)
  {
    istringstream stringbuf(line);
    stringbuf >> a >> b >> c >> d >> e >> f >> g >> h >> k;
    StripPosition = (PdbCoordinate*)&geometryBank->getEntry( index );
    StripPosition->setAllParameters(a,b,c);
    StripPosition->setAllParErrors(d,e,f);
    index++;
    StripPosition = (PdbCoordinate*)&geometryBank->getEntry( index );
    StripPosition->setAllParameters(g,h,k);

    if(fPlaneNum==Cathode2&&getArm()==South) {
      
      //South 0 degree plane
      //files have twice the lines.
      s.getline(line,bufsize,'\n');
      istringstream stringbf(line);
      stringbf >> a >> b >> c >> d >> e >> f >> g >> h >> k;
    } else {
      a = b = c = d = e = f = g = h = k = 0;
    }

    StripPosition->setAllParErrors(f,g,h);
    index++;
    StripPosition = (PdbCoordinate*)&geometryBank->getEntry( index );
    StripPosition->setAllParameters(k,0.0,0.0);
    index++;
  }
  s.close();
  if(!commit())
  { cout<<"Error in MutPlane::updateSt3StripSrvyData commit.\n"; }

}

//____________________________________________________________________
string MutPlane::getSt1AutocadFile( void )
{
  string autocadFile;
  if(getOctant()%2)
  {
    //Octants 2,4,6,8/fOctantNums 1,3,5,7
    switch(fPlaneNum)
    {
      case Cathode1:
      switch(getGap())
      {
        case Gap1:
        if((getArm()==South&&getHalfOctant())||(getArm()==North&&!getHalfOctant())) autocadFile = "091_1";
        else autocadFile = "091_0";
        break;

        case Gap2:
        if((getArm()==South&&getHalfOctant())||(getArm()==North&&!getHalfOctant())) autocadFile = "095_1";
        else autocadFile = "095_0";
        break;

        case Gap3:
        if((getArm()==South&&getHalfOctant())||(getArm()==North&&!getHalfOctant())) autocadFile = "089_1";
        else autocadFile = "089_0";
        break;

        default: break;

      }
      break;

      case Cathode2:
      if((getArm()==South&&getHalfOctant())||(getArm()==North&&!getHalfOctant())) autocadFile = "088_1";
      else autocadFile = "088_0";
      break;

      default: break;

    }

    //North Station 1 octant 8 gap 1cathode 1 is not 11.25 degrees, but 6.5.
    //This is an assembly error.
    if(getArm()==North&&getOctant()==7&&getGap()==Gap1&&fPlaneNum==Cathode1) {
      if(getHalfOctant()) autocadFile = "095_0";
      else autocadFile = "095_1";
    }

  } else {

    //Octants 1,3,5,7 (fOctantNums 0,2,4,6)
    switch(fPlaneNum)
    {

      case Cathode1:
      switch(getGap())
      {
        case Gap1:
        if((getArm()==South&&getHalfOctant())||(getArm()==North&&!getHalfOctant())) autocadFile = "092_1";
        else autocadFile = "092_0";
        break;

        case Gap2:
        if((getArm()==South&&getHalfOctant())||(getArm()==North&&!getHalfOctant())) autocadFile = "096_1";
        else autocadFile = "096_0";
        break;

        case Gap3:
        if((getArm()==South&&getHalfOctant())||(getArm()==North&&!getHalfOctant())) autocadFile = "090_1";
        else autocadFile = "090_0";
        break;

        default: break;
      }
      break;

      case Cathode2:
      if((getArm()==South&&getHalfOctant())||(getArm()==North&&!getHalfOctant())) autocadFile = "087_1";
      else autocadFile = "087_0";
      break;

      default: break;
    }
  }

  return autocadFile;

}

//____________________________________________________________________
string MutPlane::getSt3HalfOctSrvyFile( void )
{

  string surveyFile ="st3_halfoct_survey";
  if(getArm()==North) surveyFile += "North";
  switch(getGap())
  {

    case Gap1:
    switch(fPlaneNum)
    {

      case Cathode1:
      if((getHalfOctant()&&getArm()==South) || (!getHalfOctant()&&getArm()==North)) surveyFile +="_03";
      else surveyFile +="_06";
      break;

      case Cathode2:
      if((getHalfOctant()&&getArm()==South) || (!getHalfOctant()&&getArm()==North)) surveyFile +="_02A";
      else surveyFile +="_05A";
      break;

      default:
      cout << "MutPlane::getSt3HalfOctSrvyFile - incorrect plane number" << endl;
      assert(0);
      break;

    }
    break;

    case Gap2:
    switch(fPlaneNum)
    {
      case Wire:
      case Cathode1:
      if((getHalfOctant()&&getArm()==South) || (!getHalfOctant()&&getArm()==North)) surveyFile +="_02B";
      else surveyFile +="_05B";
      break;

      case Cathode2:
      if((getHalfOctant()&&getArm()==South) || (!getHalfOctant()&&getArm()==North)) surveyFile +="_01";
      else surveyFile +="_04";
      break;

      default:
      cout << "MutPlane::getSt3HalfOctSrvyFile - incorrect plane number" << endl;
      assert(0);
      break;

    }
    break;

    default:
    cout << "MutPlane::getSt3HalfOctSrvyFile - incorrect gap number" << endl;
    assert(0);
    break;

  }

  return surveyFile;
}

//____________________________________________________________________
vector<PHPoint> MutPlane::getSt3HalfOctFramePoints( void )
{

  vector<PHPoint> survey_pins;
  
  if(f_pArm->readFromDB)
  {

    PdbBankID bankID=0;
    PHTimeStamp Tsearch = f_pArm->getArmTimeStamp();
    const char *bank_name = "survey.mut.St3HalfOct";
    if(getArm()==North) bank_name = "survey.mut.St3NorthHalfOct";
    if(!fetch( "PdbCoordinateBank", Tsearch, bank_name, bankID)){
      cout 
        << "MutPlane::getSt3HalfOctFramePoints - fetch failed. "
        << "Station 3 strips will not been created."
        << endl;
      return survey_pins;
    }
    
    PdbCoordinate *surveyPosition;
    for(int i=8*getOctant(); i<8*(getOctant()+1); i++)
    {
      surveyPosition = (PdbCoordinate*)&geometryBank->getEntry(i);
      double a = surveyPosition->getParameter(0);
      double b = surveyPosition->getParameter(1);
      double c = surveyPosition->getParameter(2);

      //convert to cm
      survey_pins.push_back( PHPoint(a/10.0,b/10.0,c/10.0) );
    }
    
    if(!commit()) cout<<"MutPlane::getSt3HalfOctFramePoints - commit failed.\n";

  } else {

    const char *file = "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/St3OctantSurveys.dat";
    if(getArm()==North) file = "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/St3NOctantSurveys.dat";
    static const int bufsize = 256;
    char line[bufsize];
    char pinName[7];

    ifstream s(file);
    if (!s)
    {
      cout<<"MutPlane::getSt3HalfOctFramePoints - error opening file " <<file<<". \n";
      return survey_pins;
    }

    // skip trailing lines (?)
    for (int i=0; i<(9*getOctant()); i++) 
    { s.getline(line,bufsize,'\n'); }

    // read pin points
    for (int i=0; i<8; i++)
    {
      s.getline(line,bufsize,'\n');
      istringstream stringbuf(line);
      double a(0), b(0), c(0);
      stringbuf >> pinName >> a >> b >> c ;

      //convert to cm
      survey_pins.push_back( PHPoint(a/10.0,b/10.0,c/10.0) );
    }
  }
  
  return survey_pins;
  
}

#define NEW_IMPLEMENTATION
#ifdef NEW_IMPLEMENTATION
//____________________________________________________________________
// this new implementation of the getDCMChannels allows to read "full" maps, that allow
// for different mapping from one octant to the other. This is notably necessary for places 
// where cables have been swapped
void MutPlane::getDCMChannels( string tag )
{
   
  static bool first( true );
  if( first )
  {
    first = false;
    cout << "MutPlane::getDCMChannels - using new implementation" << endl;
  }
  
  string file( MutDCMChannelMap::bankName( getArm(), getStation(), tag ) );
  
  ifstream in( file.c_str() );
  
  // some printouts
  if( getOctant() == 0 && getHalfOctant() == 0 && getGap() == 0 && fPlaneNum == 0 )
  {
    if( !in )
    {
      cout << "MutPlane::getDCMChannels - arm: " << getArm() << " station: " << getStation() << " - cannot read file " << file << endl;
    } else cout << "MutPlane::getDCMChannels - arm: " << getArm() << " station: " << getStation() << " - reading " << file << endl;
  }
  
  if( !in ) return;

  static const int bufsize = 512;
  char line[bufsize];
  while( in.getline(line,bufsize,'\n'))
  {

    istringstream stringbuf(line);
    
    MutDCMChannelMap::ChannelId channel_id;
    
    stringbuf >> channel_id;
    if( stringbuf.rdstate() & ios::failbit ) continue;
    
    // check identifiers
    assert( getArm() == channel_id.arm && getStation() == channel_id.station );
    if( getOctant() != channel_id.octant || getHalfOctant() != channel_id.half_octant || getGap() != channel_id.gap || fPlaneNum != channel_id.plane ) continue;
    
    // check strip index
    assert( channel_id.strip < getNumElements() && f_pMutStrips[channel_id.strip] );
    
    // assign
    f_pMutStrips[channel_id.strip]->setDCMChannel(channel_id.channel);
    f_pMutStrips[channel_id.strip]->setPacket_ID(channel_id.packet);

  }

}

#else
//____________________________________________________________________
// this old implementation reads a unique mapping for each quadrant (in station1) and each octant (in stations 2 and 3)
// and applies it to all. It does not allow to handle swapped cable, which is why it has been obsoleted 
// and replaced by the "new" more general implementation
void MutPlane::getDCMChannels( std::string )
{
  
  static bool first( true );
  if( first )
  {
    first = false;
    cout << "MutPlane::getDCMChannels - using old implementation" << endl;
  }

  int DCMChannel =0;
  int pkt_id_Offset = 11001;
  int stripOctant=0;
  
  //map file name with dummy initialization
  const char *file= "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/a1_s1_map.dat";

  if(getArm()==North) pkt_id_Offset = 11171;

  switch(getStation())
  {

    case Station1:
    if(getArm()==North) 
    {
      
      file= "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/a2_s1_map.dat";
      if(getOctant()%2) stripOctant=1;
      
      //use integer divide to truncate the result
      pkt_id_Offset = pkt_id_Offset +10*(getOctant()/2);
    
    } else {
      
      file = "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/a1_s1_map.dat";
      if(getOctant()%2==0) stripOctant=1;
      if(getOctant()!=7) pkt_id_Offset = pkt_id_Offset +10*((getOctant()+1)/2);
    }
    break;

    case Station2:
    if(getArm()==North) {

      file = "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/a2_s2_map.dat";
      pkt_id_Offset = pkt_id_Offset + 40 + getOctant()*9;

    } else {

      file = "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/a1_s2_map.dat";
      pkt_id_Offset = pkt_id_Offset + 40 + getOctant()*8;

    }
    break;

    case Station3:
    if(getArm()==North)
    {

      file = "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/a2_s3_map.dat";
      pkt_id_Offset = pkt_id_Offset + 112 + getOctant()*10;

    } else {

      file = "/afs/rhic.bnl.gov/phenix/software/calibration/mutr_txt/a1_s3_map.dat";
      pkt_id_Offset = pkt_id_Offset + 104 + getOctant()*8;

    }
    
    break;

    default: break;

  }

  static const int bufsize = 256;
  char linebuf[bufsize];
  int pkt_id, octant, halfoct, gap, plane, strip;
  octant=0;

  ifstream s(file);
  if (!s){
    cout<<"Error opening file "<<file<<". \n";
    s.close();
    return;
  }

  int num=0;
  while(s.getline(linebuf,bufsize,'\n'))
  {

    istringstream stringbuf(linebuf);

    if(getStation()==Station1)
    {
      
      stringbuf >> pkt_id >> DCMChannel >> octant >> halfoct>> gap >> plane >> strip;
      
    } else {
      
      stringbuf >> pkt_id >> DCMChannel >> halfoct>> gap >> plane >> strip;
      
    }

    if (plane==fPlaneNum/2 && gap==getGap() && halfoct==getHalfOctant() && octant==stripOctant)
    {
      if(strip>-1 && strip<NumElements)
      {
        //to prevent crash.
        if(f_pMutStrips[strip])
        {
          f_pMutStrips[strip]->setDCMChannel(DCMChannel);
          f_pMutStrips[strip]->setPacket_ID(pkt_id+pkt_id_Offset);
          if(++num == NumElements) break;
        }
      }
    }
  }
  s.close();
}
#endif

//____________________________________________________________________
void MutPlane::fetchDCMChannels()
{
  
  PdbBankID bankID = 0;
  PHTimeStamp Tsearch = f_pArm->getArmTimeStamp();

  string bank_name( MutDCMChannelMap::bankName( getArm(), getStation() ) );
  if(!fetch( "PdbMutDCMMapBank", Tsearch, bank_name.c_str(), bankID )){
    cout << "Error in MutPlane::fetchDCMChannels, fetch failed.\n";
    cout << "Performing MuTr strip-to-DCM channel mapping from file.\n";
    getDCMChannels();
    return;
  }

  PdbMutDCMMap *DCMmap;

  //total #entries = #octants x #halfocts x #gaps x #cathode planes
  int numGaps = f_pHalfOctant->getNumberOfGaps();

  int plane_index = (fPlaneNum == Cathode1) ? 0:1;
  int index = plane_index + MUTGEOM::NumberOfCathodePlanes*( 
    getGap() + numGaps*( 
    getHalfOctant() + MUTGEOM::NumberOfHalfOctants*(
    getOctant() ) ) );
  
  //  int i= getOctant()*NumberOfHalfOctants*numGaps*NumberOfCathodePlanes
  // + getHalfOctant()*numGaps*NumberOfCathodePlanes
  // + getGap()*NumberOfCathodePlanes + fPlaneNum/2;

  DCMmap = (PdbMutDCMMap*)&geometryBank->getEntry( index );
  if(DCMmap->getOctant()!=getOctant()||
     DCMmap->getHalfOctant()!=getHalfOctant() ||
     DCMmap->getGap()!=getGap()||
     DCMmap->getPlane()!=fPlaneNum) 
  {
    cout<< "Error in MutPlane::fetchDCMChannels, retrieved wrong plane!\n";
    commit();
    cout << "Performing MuTr strip-to-DCM channel mapping from file.\n";
    getDCMChannels();
    return;
  }

  // dump
  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DCM_CHANNELS ) >= TMutDatabaseCntrl::MAX ) {
    MUTGEOM::PRINT( cout, "MutPlane::fetchDCMChannels" );
    cout << "MutPlane::fetchDCMChannels - arm station plane octant half gap plane strip dcm packet" << endl;
  }

  for(int strip=0; strip<NumElements; strip++) 
  {
    if(f_pMutStrips[strip])
    {

      // dump
      if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::DCM_CHANNELS) >= TMutDatabaseCntrl::MAX )
      {
        
        cout
          << getArm() << " " << getStation() << " " << getOctant()
          << " " << getHalfOctant() << " " << getGap() << " " << getPlane() << " " << strip
          << " " << DCMmap->getPacketID(strip) 
          << " " << DCMmap->getDCMChannel(strip) 
          << endl;
      }
      
      f_pMutStrips[strip]->setDCMChannel(DCMmap->getDCMChannel(strip));
      f_pMutStrips[strip]->setPacket_ID(DCMmap->getPacketID(strip));

    } else { cout << "MutPlane::fetchDCMChannels - invalid strip number" << endl; }
    
  }

  if(!commit()) cout<<"Error in MutPlane::fetchDCMChannels commit.\n";
  return;
}

//____________________________________________________________________
void MutPlane::print( void ) const
{

  char line[5120];
  static bool first( true );

  if( first )
  {
    first = false;
    sprintf( line,
      "%5s %5s %5s %5s %5s %5s %10s %10s %10s %10s %10s %10s %10s %10s",
      "arm", "sta", "oct", "half", "gap", "plane", "strip",
      "x1", "y1", "z1",
      "x2", "y2", "z2",
      "angle");
    cout << line << endl;
  }

  // loop over wires if any
  if( !f_pMutWires.empty() )
  {
    for( unsigned int i_wire = 0; i_wire < f_pMutWires.size(); i_wire++ )
    {

      MutWire* wire( f_pMutWires[i_wire] );
      if( !wire ) continue;

      PHPoint begin( wire->getGlobalPositionBegin() );
      PHPoint end( wire->getGlobalPositionEnd() );

      sprintf( line, "%5i %5i %5i %5i %5i %5i %10i  %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f",
        getArm(), getStation(), getOctant(), getHalfOctant(), getGap(), getPlane(), (int) i_wire,
        begin.getX(), begin.getY(), begin.getZ(),
        end.getX(), end.getY(), end.getZ()
        );
      cout << line << endl;

    }

    cout << endl;

  }

  // loop over strips if any
  if( !f_pMutStrips.empty() )
  {
    for( unsigned int i_strip = 0; i_strip < f_pMutStrips.size(); i_strip++ )
    {

      MutStrip* strip( f_pMutStrips[i_strip] );
      if( !strip ) continue;

      PHPoint begin( strip->getGlobalPositionBegin() );
      PHPoint end( strip->getGlobalPositionEnd() );
      const double s_angle = strip->getAngle();

      sprintf( line, "%5i %5i %5i %5i %5i %5i %10i  %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f",
        getArm(), getStation(), getOctant(), getHalfOctant(), getGap(), getPlane(), (int) i_strip,
        begin.getX(), begin.getY(), begin.getZ(),
        end.getX(), end.getY(), end.getZ(), s_angle
        );

      cout << line << endl;
    }

    cout << endl;
  }

  return;

}

//____________________________________________________________________
void MutPlane::printStripSpacing( void ) const
{

  char line[512];
  static bool first( true );

  if( first )
  {
    first = false;
    sprintf( line,
      "%5s %5s %5s %5s %5s %5s %10s",
      "arm", "sta", "oct", "half", "gap", "plane",
      "spacing" );
    cout << line << endl;
  }

  sprintf( line, "%5i %5i %5i %5i %5i %5i %10.5f",
    getArm(), getStation(), getOctant(), getHalfOctant(), getGap(), getPlane(),
    getStripSpacing() );

  cout << line << endl;

}

//____________________________________________________________________
void MutPlane::setStripSpacing()
{

  //get the strip width for use in the response chain.
  if(f_pMutStrips[1] && f_pMutStrips[2])
  {

    PHLine strip1(f_pMutStrips[1]->getGlobalPositionBegin(), f_pMutStrips[1]->getGlobalPositionEnd());
    PHLine strip2(f_pMutStrips[2]->getGlobalPositionBegin(), f_pMutStrips[2]->getGlobalPositionEnd());
    PHPlane myplane(strip2.getBasepoint(),strip2.getDirection());
    PHPoint cross;

    if(intersectionLinePlane(strip1, myplane, cross)) stripPerpSpacing = (PHVector(cross-strip2.getBasepoint())).length();
    else cout<<"MutPlane::setStripSpacing - invalid strip spacing.\n";

  } else { cout<<"MutPlane::setStripSpacing - strips not initialized.\n"; }

}

//____________________________________________________________________
void MutPlane::translate(const PHPoint &translation)
{

  //translate position of plane and its strips and wires.
  fGlobalPosition = fGlobalPosition + translation;
  if(fPlaneNum == Wire) {
    for (int j=0; j<NumElements; j++)
    if(f_pMutWires[j]) f_pMutWires[j]->translate(translation);
  } else {
    for (int j=0; j<NumElements; j++)
    if(f_pMutStrips[j]) f_pMutStrips[j]->translate(translation);
  }
}

//____________________________________________________________________
void MutPlane::rotate(float rotAngle, char axisLabel)
{
  //rotate position of plane and its strips and wires.

  rotateThis(rotAngle, axisLabel);

  //strips and wires must be rotated differently than other geom objects.

  PHVector axis(0,0,0);

  switch(axisLabel){
  case 'x':
    axis.setX(1);
    break;
  case 'y':
    axis.setY(1);
    break;
  case 'z':
    axis.setZ(1);
    break;
  default:
    cout<< axis <<" does not specify either x, y, or z.\n";
    cout<<"Rotation not performed.\n";
    return;
  }
  PHMatrix rotation = rotationMatrix(rotAngle, axis);
  PHVector tempV;
  PHPoint begin,end;

  if(fPlaneNum == Wire) {
    for (int j=0; j<NumElements; j++)
    if(f_pMutWires[j]) {
      tempV = f_pMutWires[j]->getGlobalPositionBegin();
      tempV = rotation * tempV;
      begin=tempV;
      tempV = f_pMutWires[j]->getGlobalPositionEnd();
      tempV = rotation * tempV;
      end=tempV;
      f_pMutWires[j]->SetGlobalGeom(begin,end);
    }
  } else {
    for (int j=0; j<NumElements; j++)
    if(f_pMutStrips[j]) {
      tempV = f_pMutStrips[j]->getGlobalPositionBegin();
      tempV = rotation * tempV;
      begin=tempV;
      tempV = f_pMutStrips[j]->getGlobalPositionEnd();
      tempV = rotation * tempV;
      end=tempV;
      f_pMutStrips[j]->SetGlobalGeom(begin,end);
    }
  }

}

//____________________________________________________________________
void MutPlane::XYExpansion(float expansionPercentage)
{
  /*  We want to be able to adjust for thermal expansions in the plane
      of the strip/wires.  For this, adjusting as a percentage of
      current size seems reasonable.  This percentage (not fraction) is
      passed as a parameter.

      First expand everything, with no fixed point on the plane.  Then
      translate the original position of the plane's inner radius.
  */

  PHPoint translation(0,0,0);
  PHPoint newBegin, newEnd;

  if(fPlaneNum == Wire) {
    if(f_pMutWires[0]) {
      newBegin = f_pMutWires[0]->getGlobalPositionBegin()* (1+expansionPercentage/100);
      newEnd = f_pMutWires[0]->getGlobalPositionEnd()* (1+expansionPercentage/100);

      //reset z positions
      newBegin.setZ(f_pMutWires[0]->getGlobalPositionBegin().getZ());
      newEnd.setZ(f_pMutWires[0]->getGlobalPositionEnd().getZ());

      if(getHalfOctant()) translation = f_pMutWires[0]->getGlobalPositionBegin() - newBegin;
      else translation = f_pMutWires[0]->getGlobalPositionEnd() - newEnd;

      f_pMutWires[0]->SetGlobalGeom(newBegin, newEnd);
    }
    for (int j=1; j<NumElements; j++)
    if(f_pMutWires[j]) {
      newBegin = f_pMutWires[j]->getGlobalPositionBegin()*(1+expansionPercentage/100);
      newEnd = f_pMutWires[j]->getGlobalPositionEnd()*(1+expansionPercentage/100);
      newBegin.setZ(f_pMutWires[j]->getGlobalPositionBegin().getZ());
      newEnd.setZ(f_pMutWires[j]->getGlobalPositionEnd().getZ());
      f_pMutWires[j]->SetGlobalGeom(newBegin, newEnd);
    }
  } else {
    //While expanding strips, find the strip that begins on the
    //plane's inner radius.
    double oldRadius = 999.0;
    PHPoint referencePoint(0,0,fGlobalPosition.getZ());

    for (int j=0; j<NumElements; j++)
    if(f_pMutStrips[j]) {
      newBegin = f_pMutStrips[j]->getGlobalPositionBegin()*(1+expansionPercentage/100);
      newEnd = f_pMutStrips[j]->getGlobalPositionEnd()*(1+expansionPercentage/100);

      //reset z positions
      newBegin.setZ(f_pMutStrips[j]->getGlobalPositionBegin().getZ());
      newEnd.setZ(f_pMutStrips[j]->getGlobalPositionEnd().getZ());
      double newRadius = distancePointToPoint(f_pMutStrips[j]->getGlobalPositionBegin(), referencePoint);
      if(newRadius<oldRadius) {
        oldRadius = newRadius;
        translation = f_pMutStrips[j]->getGlobalPositionBegin() - newBegin;
      }
      f_pMutStrips[j]->SetGlobalGeom(newBegin, newEnd);
    }
  }

  translate(translation);
}

//____________________________________________________________________
void MutPlane::transformToNewFrame(PHFrame oldFrame, PHFrame newFrame)
{
  fGlobalPosition = rotateAndTranslate(oldFrame, fGlobalPosition, newFrame);
  fGlobalVector = rotateAndTranslate(oldFrame, fGlobalVector, newFrame);

  PHPoint newBegin, newEnd;
  if(fPlaneNum == Wire) {
    for (int j=0; j<NumElements; j++)
    if(f_pMutWires[j])
    {
      newBegin = rotateAndTranslate(oldFrame, f_pMutWires[j]->getGlobalPositionBegin(), newFrame);
      newEnd = rotateAndTranslate(oldFrame, f_pMutWires[j]->getGlobalPositionEnd(), newFrame);
      f_pMutWires[j]->SetGlobalGeom(newBegin, newEnd);
    }
  } else {
    for (int j=0; j<NumElements; j++)
    if(f_pMutStrips[j]) {
      newBegin = rotateAndTranslate(oldFrame, f_pMutStrips[j]->getGlobalPositionBegin(), newFrame);
      newEnd = rotateAndTranslate(oldFrame, f_pMutStrips[j]->getGlobalPositionEnd(), newFrame);
      f_pMutStrips[j]->SetGlobalGeom(newBegin, newEnd);
    }
  }
}

//____________________________________________________________________
void MutPlane::updateElements(const PHPoint oldPlanePosition, const PHVector oldPlaneVector)
{
  PHVector translation = fGlobalPosition-oldPlanePosition;
  PHAngle rotAngle = angle(oldPlaneVector, fGlobalVector);
  PHMatrix rotation;

  //protection against a nan matrix.
  if(rotAngle>1.0e-5) rotation = rotationMatrix(rotAngle, fGlobalVector.cross(oldPlaneVector));

  if(fPlaneNum==Wire)
  for(int i=0; i<NumElements; i++) {
    PHPoint newBegin = transformPoint(rotation, translation,f_pMutWires[i]->getGlobalPositionBegin());
    PHPoint newEnd = transformPoint(rotation, translation,f_pMutWires[i]->getGlobalPositionEnd());
    f_pMutWires[i]->SetGlobalGeom(newBegin,newEnd);
  }
  else
  for(int i=0; i<NumElements; i++) {
    PHPoint newBegin = transformPoint(rotation, translation,f_pMutStrips[i]->getGlobalPositionBegin());
    PHPoint newEnd = transformPoint(rotation, translation,f_pMutStrips[i]->getGlobalPositionEnd());
    f_pMutStrips[i]->SetGlobalGeom(newBegin,newEnd);
  }

}

//________________________________________________________________________________________
void MutPlane::getSt3SurveyFrame( PHFrame& half_octant_frame, PHFrame& octant_frame )
{

  //Get the half-octant frame points in global coordinates.
  //All 8 survey points on an octant are retrieved in survey_pins[8].
  vector<PHPoint> survey_pins( getSt3HalfOctFramePoints() );
  assert( survey_pins.size() == 8 );
  
  // print all survey points
  if( TMutDatabaseCntrl::get_verbosity() >= TMutDatabaseCntrl::MAX && fPlaneNum == Wire )
  {

    cout << "MutPlane::getSt3SurveyFrame - ["
      << getArm() << ","
      << getStation() << ","
      << getOctant() << ","
      << getHalfOctant() << ","
      << getGap() << ","
      << fPlaneNum << "]"
      << endl;

    for( int i = 0; i<8; i++ )
    { cout << "survey_pins[" << i << "]: " << survey_pins[i] << endl; }

  }

  /*Get rotation and translation from half-octant to octant frame.
    This is used to redefine strip positions with respect to the Octant
    frame.  There are 4 different half-octant frames depending on half-octant number
    and plane number.
  */
  PHPoint origin(0,0,0);
  PHVector xAxis, yAxis, zAxis, temp;
  //The North strip surveys were done in completely different coordinate frames
  //from the South surveys.  The survey file lines were arranged to make use of
  //the same algorithms as much as possible.
  if(fPlaneNum==Cathode2) 
  {
    
    if(getHalfOctant()) 
    {
      xAxis = survey_pins[6] - survey_pins[7];
      origin = survey_pins[7];
      if(getArm()==North) 
      {
        PHLine y0Line(survey_pins[7],xAxis);
        origin = closestApproachLinePoint(y0Line, survey_pins[5]);
      }
      temp = survey_pins[4] - origin;
    
    } else {
     
      xAxis = survey_pins[3] - survey_pins[2];
      origin = survey_pins[2];
      if(getArm()==North) 
      {
        PHLine y0Line(survey_pins[2],xAxis);
        origin = closestApproachLinePoint(y0Line, survey_pins[1]);
      }
      temp = survey_pins[0] - origin;
      
    }
    
  } else {
    
    if(getHalfOctant()) 
    {
    
      xAxis = survey_pins[7] - survey_pins[6];
      origin = survey_pins[6];
      if(getArm()==North) 
      {
      
        PHLine y0Line(survey_pins[6],xAxis);
        origin = closestApproachLinePoint(y0Line, survey_pins[5]);
      }
      
      temp = survey_pins[4] - origin;
      
    } else {
      
      xAxis = survey_pins[2] - survey_pins[3];
      origin = survey_pins[3];
      if(getArm()==North) 
      {
        
        PHLine y0Line(survey_pins[3],xAxis);
        origin = closestApproachLinePoint(y0Line, survey_pins[1]);
      }
      
      temp = survey_pins[0] - origin;
    
    }
  }
  xAxis.normalize();
  temp.normalize();
  if(getArm()==North) temp = temp * -1.0;
  zAxis=xAxis.cross(temp);
  yAxis=zAxis.cross(xAxis);

  half_octant_frame = PHFrame(origin,xAxis,yAxis,zAxis);

  /* 
     For normal data, this information is already stored in MutOctant::octFrame.
     But when create that variable from PISAPar, we need to recreate this frame
     to properly move the strips.
     I'm reusing the yAxis and zAxis variables here.
  */
  PHVector OctXAxis = survey_pins[6] - survey_pins[2];
  OctXAxis.normalize();
  temp = survey_pins[0] - survey_pins[2];
  origin = survey_pins[2];
  
  if(getArm()==North) {
    OctXAxis = OctXAxis * -1.0;
    temp = survey_pins[4] - survey_pins[6];
    origin = survey_pins[6];
  }
  
  temp.normalize();
  zAxis=OctXAxis.cross(temp);
  yAxis=zAxis.cross(OctXAxis);
  octant_frame = PHFrame(origin,OctXAxis,yAxis,zAxis);

}
