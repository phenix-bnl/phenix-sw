#include <MutStrip.h>
#include <MutWire.h>
#include <MutPISAPara.h>
#include <TMutDatabaseCntrl.h>

#include <PHGeometry.h>
#include <PdbCoordinate.hh>
#include <PdbCalBank.hh>

#include <TFile.h>
#include <TCanvas.h>
#include <TGaxis.h>
#include <TPaveText.h>
#include <TPaveLabel.h>

#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;
using namespace PHGeometry;
using namespace MUTGEOM;

//__________________________________________________
MutOctant::MutOctant(const MutArm* Arm,
       const MutStation* Station,
       const OctantNumber& OctantNum)
  : f_pArm(Arm),
    f_pStation(Station),
    fOctantNum(OctantNum)
{

  switch(fOctantNum) {
    case Octant1: name = "Octant 1";
      break;
    case Octant2: name = "Octant 2";
      break;
    case Octant3: name = "Octant 3";
      break;
    case Octant4: name = "Octant 4";
      break;
    case Octant5: name = "Octant 5";
      break;
    case Octant6: name = "Octant 6";
      break;
    case Octant7: name = "Octant 7";
      break;
    case Octant8: name = "Octant 8";
      break;
  }

  // read octant geometry
  if(f_pArm->PISAPar) {

    // pisa geometry (?)
    getPisaGeom();

  } else if( TMutDatabaseCntrl::get_database_access( "use_local_octant_survey_file" ) ) {

    // read geometry from file
    GetGlobalGeom( TMutDatabaseCntrl::get_filename( "use_local_octant_survey_file" ) );

  } else {

    // read geometry from DB
    GetGlobalGeom();

  }

  // Create HalfOctant Objects and store pointers to them
  for (int j=0; j<NumberOfHalfOctants; j++)
  f_pMutHalfOctants[j] = new MutHalfOctant(f_pArm,f_pStation,this,HalfOctantNumber(j));

  fillCylCoordParams();
}


//__________________________________________________
MutOctant::~MutOctant()
{

  for(int i=0; i<NumberOfHalfOctants; i++)
  if (f_pMutHalfOctants[i]) delete f_pMutHalfOctants[i];

}

//__________________________________________________
void MutOctant::RefreshHalfOctant(const HalfOctantNumber& HalfOctantNum)
{

  if (f_pMutHalfOctants[HalfOctantNum]) {

    // The HalfOctant object exists - delete it and create a new one.
    delete f_pMutHalfOctants[HalfOctantNum];
    f_pMutHalfOctants[HalfOctantNum] = new MutHalfOctant(f_pArm,f_pStation,this,HalfOctantNum);

  } else cout << "Cannot refresh non-existant HalfOctant." << "\n";

}

//__________________________________________________
void MutOctant::GetGlobalGeom(const string& file)
{
  /* Octant survey positions are read in from a file.  The positions are
     in global coordinates, and located on the upstream frames.
   */
  static const int bufsize = 256;
  char linebuf[bufsize];
  int arm,sta,oct;
  double a,b,c; // x,y,z position of pin1
  double e,f,g; // x,y,z position of pin2
  double h,m,n; // x,y,z position of pin3
  PHVector xAxis, Pin3toPin1, yAxis;

  if( !file.empty() )
  {

    if( getStation() == 0 && fOctantNum == 0 )
    { cout << "MutOctant::GetGlobalGeom - reading geometry from file " << file << endl; }

    // open vile
    ifstream s( file.c_str() );
    if (!s)
    {

      if( getStation() == 0 && fOctantNum == 0 )
      { cout<<"MutOctant::GetGlobalGeom - Error opening file "<<file<<". \n"; }

      s.close();
      return;
    }

    bool found( false );
    while(s.getline(linebuf,bufsize,'\n'))
    {

      istringstream stringbuf(linebuf);
      stringbuf
        >> arm >> sta >> oct
        >> a >> b >> c
        >> e >> f >> g
        >> h >> m >> n;

      if( !(stringbuf.rdstate() & ios::failbit ) && arm==getArm() && sta==getStation() && oct==fOctantNum)
      {
        found = true;
        break;
      }

    }
    s.close();

    assert( found );

  } else {

    PdbBankID bankID=0;
    PHTimeStamp Tsearch = f_pArm->getArmTimeStamp();
    const char *bankName;
    if(getArm()==South) bankName = "survey.mut.SouthOctants";
    else bankName = "survey.mut.NorthOctants";

    if(!fetch( "PdbCoordinateBank", Tsearch, bankName, bankID)){
      cout << "Error in MutOctant::GetGlobalGeom, fetch failed.\n";
      cout << "Octant survey data has not been retrieved!\n";
      return;
    }

    if( getOctant() == 0 && getStation() == 0 ) {
      cout << "MutOctant::GetGlobalGeom" << endl;
      print_bank_summary();
    }

    PdbCoordinate *surveyPosition;

    //there are 3 coordinates for each octant
    int i=getStation()*24 + 3*fOctantNum;
    surveyPosition = (PdbCoordinate*)&geometryBank->getEntry(i);
    a = surveyPosition->getParameter(0);
    b = surveyPosition->getParameter(1);
    c = surveyPosition->getParameter(2);
    i++;
    surveyPosition = (PdbCoordinate*)&geometryBank->getEntry(i);
    e = surveyPosition->getParameter(0);
    f = surveyPosition->getParameter(1);
    g = surveyPosition->getParameter(2);
    i++;
    surveyPosition = (PdbCoordinate*)&geometryBank->getEntry(i);
    h = surveyPosition->getParameter(0);
    m = surveyPosition->getParameter(1);
    n = surveyPosition->getParameter(2);

    if(!commit()) cout<<"Error in MutOctant::GetGlobalGeom commit.\n";

  }

  // print values
  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::OCTANT_SURVEY ) >= TMutDatabaseCntrl::MAX )
  {
    cout
      << "MutOctant::GetGlobalGeom - "
      << getArm() << " " << getStation() << " " << fOctantNum << " "
      << a << " " << b << " " << c << " "
      << e << " " << f << " " << g << " "
      << h << " " << m << " " << n << endl;
  }

  //Convert mm to cm.
  fGlobalPosition.setX(a/10);
  fGlobalPosition.setY(b/10);
  fGlobalPosition.setZ(c/10);
  PHPoint Pin2(e/10,f/10,g/10);
  PHPoint Pin3(h/10,m/10,n/10);

  xAxis = (PHVector)(Pin2 - fGlobalPosition);
  xAxis.normalize();

  //Station 1 Octants 2,4,6,8 (fOctantNums 1,3,5,7)
  //need sign flipped for x-axis.
  if(getStation()==Station1 && fOctantNum%2) xAxis = xAxis * -1.0;

  Pin3toPin1 = (PHVector)(Pin3 - fGlobalPosition);
  Pin3toPin1.normalize();
  fGlobalVector = xAxis.cross(Pin3toPin1);
  fGlobalVector.normalize();

 /* Strip/wire positions are known within the frame of the octant plane.
    Define this frame here.  It is used to transform strip/wire positions
    to global coordinates.
 */
  yAxis = fGlobalVector.cross(xAxis);
  PHFrame frame(fGlobalPosition, xAxis, yAxis, fGlobalVector);
  octFrame = frame;

}


//__________________________________________________
void MutOctant::getPisaGeom()
{
  PHFrame globalFrame;
  double angles[6];
  for (int i = 0; i<6; i++)
    {
      angles[i] = NAN;
    }
  Float_t offsets[3];
  //temporary fix until arm numbering is reversed in pisa.
  int arm = 1;
  if(getArm()==North) arm = 0;

  if(getStation()==Station1){
    int quadrant = (fOctantNum-1)/2;
    if(fOctantNum==0) quadrant = 3;

    for(int i=0; i<6; i++) angles[i] = f_pArm->PISAPar->
         GetMutParaOneAngles(arm, quadrant, i)/180*Pi;
    for(int i=0; i<3; i++) offsets[i] = f_pArm->PISAPar->
                    GetMutParaOneOffsets(arm, quadrant, i);
    //Now convert x to OCTANT angles.
    if(!(fOctantNum%2)) angles[1] = angles[1] - Pi/2;
  }
  else if(getStation()==Station2){
    for(int i=0; i<6; i++) angles[i] = f_pArm->PISAPar->
                       GetMutParaTwoAngles(arm, getOctant(), i)/180*Pi;
    for(int i=0; i<3; i++) offsets[i] = f_pArm->PISAPar->
           GetMutParaTwoOffsets(arm, getOctant(), i);
    angles[1] = angles[1] + 5*Pi/8;
  }
  else if(getStation()==Station3){
    for(int i=0; i<6; i++) angles[i] = f_pArm->PISAPar->
                   GetMutParaThreeAngles(arm, getOctant(), i)/180*Pi;
    for(int i=0; i<3; i++) offsets[i] = f_pArm->PISAPar->
           GetMutParaThreeOffsets(arm, getOctant(), i);
    angles[1] = angles[1] + 5*Pi/8;
  }

  if(angles[1]>2*Pi) angles[1] = angles[1] - 2*Pi;
  //Calculate the y-axis rotation from the x-axis rotation.
  angles[3] = angles[1] + Pi/2;
  if(angles[3]>2*Pi) angles[3] = angles[3] - 2*Pi;

  /* Need to force precisions and do bounds checking here so the
     octFrame is orthogonal.
  */
  PHVector row1((float)cos(angles[1]), (float)sin(angles[1]), cos(angles[0]));
  PHVector row2((float)cos(angles[3]), (float)sin(angles[3]), cos(angles[2]));
  PHVector row3(sin(angles[4])*cos(angles[5]), sin(angles[4])*sin(angles[5]),
    cos(angles[4]));
  if(abs(row1.getX())<1.0e-14) row1.setX(0);
  if(abs(row1.getY())<1.0e-14) row1.setY(0);
  if(abs(row2.getX())<1.0e-14) row2.setX(0);
  if(abs(row2.getY())<1.0e-14) row2.setY(0);

  PHMatrix rotation(row1, row2, row3);
  PHVector translation;
  octFrame = MatrixAndVector2frames(globalFrame, rotation,
                 translation);

  double a[2][6] = {{-3.878, -3.878, 213.6408, 222.733, 317.7564, 317.7564},
                   {-3.878, -3.878, 252.2139, 261.0331, 421.0856, 421.0856}};
  double b[2][6] = {{115.008, -115.008, 0.0, 0.0, 76.1151, 76.1151},
                   {115.008, -115.008, 0.0, 0.0, 101.9532, 101.9532}};

  int station = getStation();
  if(getStation()==Station2 && getOctant()%2) station = 2;
  if(getStation()==Station3) station = 3;

  float Znominal = f_pArm->PISAPar->GetMutParaZpos(arm,station);
  if(getArm()==South) Znominal = -1.0 * Znominal;
  PHVector octPisaPosition(offsets[0],offsets[1],(offsets[2]+Znominal));

  int armIndx = getArm();
  int staIndx = getStation();
  fGlobalPosition = octPisaPosition -
    octFrame.getV() * a[armIndx][2*staIndx+getOctant()%2] -
    octFrame.getU() * b[armIndx][2*staIndx+getOctant()%2];

  octFrame.setOrigin(fGlobalPosition);
  fGlobalVector=octFrame.getW();
  //We want the orientation vector to pointout of the arm, toward the IP.
  if(getArm()==North) fGlobalVector = fGlobalVector*-1.0;
}


//__________________________________________________
void MutOctant::fillCylCoordParams()
{
  MutPlane *plane= f_pMutHalfOctants[0]->f_pMutGaps[Gap2]->f_pMutPlanes[Cathode2];

  PHSphPoint tempSphPoint(plane->f_pMutStrips[0]->getGlobalPositionBegin());
  beginPhi = tempSphPoint.getPhi();

  tempSphPoint=plane->f_pMutStrips[plane->getNumElements()/2]->getGlobalPositionBegin();
  thetaInner = tempSphPoint.getTheta();

  tempSphPoint=plane->f_pMutStrips[plane->getNumElements()-1]->getGlobalPositionEnd();
  thetaOuter = tempSphPoint.getTheta();

  plane =f_pMutHalfOctants[1]->f_pMutGaps[Gap1]->f_pMutPlanes[Cathode2];
  tempSphPoint=plane->f_pMutStrips[plane->getNumElements()-1]->getGlobalPositionBegin();
  endPhi = tempSphPoint.getPhi();

  /*  Inner and outer radii are defined as the middle of the inner wire and
      end of the outer wire.  This gives the greatest size to the active area.
      Find the middle of the inner and outer anode wires and shift.
  */
  plane = f_pMutHalfOctants[HalfOctant1]->f_pMutGaps[Gap2]->f_pMutPlanes[Wire];
  PHPoint innerWireMiddle = (plane->f_pMutWires[0]->getGlobalPositionBegin() +
    plane->f_pMutWires[0]->getGlobalPositionEnd())*0.5;
  PHPoint outerWireMiddle = (plane->f_pMutWires[plane->getNumElements()-1]->
           getGlobalPositionBegin() + plane->f_pMutWires[plane->
           getNumElements()-1]->getGlobalPositionEnd())*0.5;
  PHVector perpToWire=innerWireMiddle - outerWireMiddle;
  perpToWire.normalize();
  innerWireMiddle = innerWireMiddle + perpToWire*0.5*cos(Pi/8);

  PHCylPoint tempCylPoint(innerWireMiddle);
  innerRadius = tempCylPoint.getR();

  //The farthest point in R on the chamber in the wire plane is on a rib,
  //so use wire end instead of middle.
  tempCylPoint = plane->f_pMutWires[plane->getNumElements()-1]->
    getGlobalPositionEnd();
  outerRadius = tempCylPoint.getR();

}


//__________________________________________________
void MutOctant::translate(const PHPoint &translation)
{

  //Translate position of octant and its half-octants.
  fGlobalPosition = fGlobalPosition + translation;
  for (int j=0; j<NumberOfHalfOctants; j++)
  if(f_pMutHalfOctants[j]) f_pMutHalfOctants[j]->translate(translation);

}


//__________________________________________________
void MutOctant::rotate(float angle, char axisLabel)
{
  rotateThis(angle, axisLabel);
  for (int j=0; j<NumberOfHalfOctants; j++)
    if(f_pMutHalfOctants[j]) f_pMutHalfOctants[j]->rotate(angle, axisLabel);
}


//__________________________________________________
void MutOctant::XYExpansion(float expansionPercentage)
{
  /*  We want to be able to adjust for thermal expansions in the plane
      of the strip/wires.  For this, adjusting as a percentage of
      current size seems reasonable.  This percentage is passed as a
      parameter.
      The adjustment will be determined for each strip relative to the
      centerline of the octant.  Since the strips for station 1 aren't
      uniformly spaced, the adjustment must be determined for each strip
      based on its original distance from the strip closest to the
      centerline.
  */

  for (int j=0; j<NumberOfHalfOctants; j++) {
      if(f_pMutHalfOctants[j])
  f_pMutHalfOctants[j]->XYExpansion(expansionPercentage);
  }
}


//__________________________________________________
PHFrame MutOctant::getBodyCenteredFrame()
{
  /* Return a coordinate system centered on the chamber with y axis
     defined as the centerline of the chamber, x axis as lying in the
     plane of the chamber, and z axis = fGlobalVector.
     Use Gap2 anode to define the center of the chamber.  This will be
     imperfect for Station 3, but close enough.
  */


  MutPlane *middlePlane =f_pMutHalfOctants[HalfOctant1]->f_pMutGaps[Gap2]->
    f_pMutPlanes[Wire];
  int centerWire = middlePlane->getNumElements()/2;
  PHPoint frameOrigin = middlePlane->f_pMutWires[centerWire]->
                        getGlobalPositionEnd();

  PHVector yAxis = middlePlane->f_pMutWires.back()->getGlobalPositionEnd() -
       middlePlane->f_pMutWires.front()->getGlobalPositionEnd();
  yAxis.normalize();
  PHVector xAxis = yAxis.cross(fGlobalVector);
  PHVector zAxis = xAxis.cross(yAxis);

  PHFrame frame(frameOrigin,xAxis,yAxis,zAxis);
  return frame;
}


//__________________________________________________
void MutOctant::transformToNewFrame(PHFrame oldFrame, PHFrame newFrame)
{
  fGlobalPosition = rotateAndTranslate(oldFrame, fGlobalPosition, newFrame);
  fGlobalVector = rotateAndTranslate(oldFrame, fGlobalVector, newFrame);
  for (int j=0; j<NumberOfHalfOctants; j++) {
    if(f_pMutHalfOctants[j])
    f_pMutHalfOctants[j]->transformToNewFrame(oldFrame,newFrame);
  }
}


//__________________________________________________
void MutOctant::Draw(const char *filename)
{
  string labelString;
  double x1,y1,x2,y2,size;
  //dummy initializations
  x1=y1=x2=y2=size=0.0;

  switch(getStation()) {
  case Station1:
    size = 140;
    break;
  case Station2:
    size = 235;
    if(getArm()==North) size = 275.0;
    break;
  case Station3:
    size = 340;
    if(getArm()==North) size = 470.0;
    break;
  }

  switch(fOctantNum) {
    case Octant1: x1 = 0; y1 = -size/2; x2 = size; y2 = size/2;
      break;
    case Octant2: x1 = 0; y1 = 0; x2 = size; y2 = size;
      break;
    case Octant3: x1 = -size/2; y1 = 0; x2 = size/2; y2 = size;
      break;
    case Octant4: x1 = -size; y1 = 0; x2 = 0; y2 = size;
      break;
    case Octant5: x1 = -size; y1 = -size/2; x2 = 0; y2 = size/2;
      break;
    case Octant6: x1 = -size; y1 = -size; x2 = 0; y2 = 0;
      break;
    case Octant7: x1 = -size/2; y1 = -size; x2 = size/2; y2 = 0;
      break;
    case Octant8: x1 = 0; y1 = -size; x2 = size, y2 = 0;
      break;
  }

  TFile *file = new TFile(filename,"RECREATE");
  TGaxis *axis = new TGaxis(x1,y1*0.9,x2,y1*0.9,x1,x2,510,"");
  TPaveText *legend = new TPaveText((x2*0.5),(y2*0.8),x2,(y2*0.95));
  TText *t2=legend->AddText("Disabled Channels");
  t2->SetTextColor(2);

  //Draw Gap1, Cathode1

  TCanvas *canvas_Gap1Cathode1 = new TCanvas("canvas_Gap1Cathode1","MuTr Geometry Display",0,0,600,600);
  canvas_Gap1Cathode1->Range(x1,y1,x2,y2);
  labelString = name + " Gap1 Cathode1";
  TPaveLabel *title0 = new TPaveLabel(x1,(y2-size*0.1),(x1+size*0.35),y2,labelString.c_str());
  title0->SetFillColor(32);
  title0->Draw();
  axis->Draw();
  stripDraw(Gap1,Cathode1);
  file->Append(canvas_Gap1Cathode1);

  //Draw Gap1Cathode2

  TCanvas *canvas_Gap1Cathode2 = new TCanvas("canvas_Gap1Cathode2","MuTr Geometry Display",10,10,600,600);
  canvas_Gap1Cathode2->Range(x1,y1,x2,y2);
  labelString.assign(name);
  labelString.append(" Gap1 Cathode2");
  TPaveLabel *title1 = new TPaveLabel(x1,(y2-size*0.1),(x1+size*0.35),y2,labelString.c_str());
  title1->SetFillColor(32);
  title1->Draw();
  axis->Draw();
  stripDraw(Gap1,Cathode2);
  file->Append(canvas_Gap1Cathode2);

  //Draw Gap2, Cathode1

  TCanvas *canvas_Gap2Cathode1 = new TCanvas("canvas_Gap2Cathode1","MuTr Geometry Display",0,0,600,600);
  canvas_Gap2Cathode1->Range(x1,y1,x2,y2);
  labelString.assign(name);
  labelString.append(" Gap2 Cathode1");
  TPaveLabel *title2 = new TPaveLabel(x1,(y2-size*0.1),(x1+size*0.35),y2,labelString.c_str());
  title2->SetFillColor(32);
  title2->Draw();
  axis->Draw();
  stripDraw(Gap2,Cathode1);
  file->Append(canvas_Gap2Cathode1);

  //Draw Gap2 Cathode2

  TCanvas *canvas_Gap2Cathode2 = new TCanvas("canvas_Gap2Cathode2","MuTr Geometry Display",10,10,600,600);
  canvas_Gap2Cathode2->Range(x1,y1,x2,y2);
  labelString.assign(name);
  labelString.append(" Gap2 Cathode2");
  TPaveLabel *title3 = new TPaveLabel(x1,(y2-size*0.1),(x1+size*0.35),y2,labelString.c_str());
  title3->SetFillColor(32);
  title3->Draw();
  axis->Draw();
  stripDraw(Gap2,Cathode2);
  file->Append(canvas_Gap2Cathode2);

  //Draw Gap1 wire

  TCanvas *canvas_Gap1wire = new TCanvas("canvas_Gap1wire","MuTr Geometry Display",10,10,600,600);
  canvas_Gap1wire->Range(x1,y1,x2,y2);
  labelString.assign(name);
  labelString.append(" Gap1 wire");
  TPaveLabel *title4 = new TPaveLabel(x1,(y2-size*0.1),(x1+size*0.35),y2,labelString.c_str());
  title4->SetFillColor(32);
  title4->Draw();
  stripDraw(Gap1,Wire);
  file->Append(canvas_Gap1wire);

  //Draw Gap1 wire

  TCanvas *canvas_Gap2wire = new TCanvas("canvas_Gap2wire","MuTr Geometry Display",10,10,600,600);
  canvas_Gap2wire->Range(x1,y1,x2,y2);
  labelString.assign(name);
  labelString.append(" Gap2 wire");
  TPaveLabel *title5 = new TPaveLabel(x1,(y2-size*0.1),(x1+size*0.35),y2,labelString.c_str());
  title5->SetFillColor(32);
  title5->Draw();
  stripDraw(Gap2,Wire);
  file->Append(canvas_Gap2wire);

  file->Write();
  file->Close();

  delete axis;
  delete title0; delete title1; delete title2; delete title3; delete title4;
  delete title5; delete legend;
  delete canvas_Gap1Cathode1;
  delete canvas_Gap1Cathode2;
  delete canvas_Gap2Cathode1;
  delete canvas_Gap2Cathode2;
  delete canvas_Gap1wire;
  delete canvas_Gap2wire;
  delete file;

  cout<<name<<" canvases have been written to "<<filename<<endl;
}


//__________________________________________________
void MutOctant::stripDraw(const GapNumber Gap, const PlaneNumber Plane,
        const PHBoolean DrawDisabledOnly)
{
  //Draw the strips or wires for one plane

  TLine *strip = new TLine(0,0,1,0);
  PHPoint begin, end;

  for (int j=0; j<NumberOfHalfOctants; j++){
    if (!f_pMutHalfOctants[j]){
      cout<<"No halfOctant to draw.\n";
      break;
    }
    if (!f_pMutHalfOctants[j]->f_pMutGaps[Gap]){
      cout<<"No Gap to draw.\n";
      break;
    }
    MutPlane *pPlane =
      f_pMutHalfOctants[j]->f_pMutGaps[Gap]->f_pMutPlanes[Plane];
    if(!pPlane){
      cout<<"No plane to draw.\n";
      break;
    }

    for(int i=0; i<pPlane->getNumElements(); i++){
      PHBoolean drawThis = True;
      if(Plane==Wire){
  if(pPlane->f_pMutWires[i]) {
    begin = pPlane->f_pMutWires[i]->getGlobalPositionBegin();
    end = pPlane->f_pMutWires[i]->getGlobalPositionEnd();
    if(DrawDisabledOnly &&
       !pPlane->f_pMutWires[i]->ChannelIsDead()) drawThis=False;
    else if(pPlane->f_pMutWires[i]->ChannelIsDead())
      strip->SetLineColor(2);
  }
      }
      else{
  if(pPlane->f_pMutStrips[i]) {
    begin = pPlane->f_pMutStrips[i]->getGlobalPositionBegin();
    end = pPlane->f_pMutStrips[i]->getGlobalPositionEnd();
    if(DrawDisabledOnly &&
       !pPlane->f_pMutStrips[i]->ChannelIsDead()) drawThis=False;
    else if(pPlane->f_pMutStrips[i]->ChannelIsDead())
      strip->SetLineColor(2);
  }
      }
      if(drawThis) strip->DrawLine(begin.getX(),begin.getY(),
           end.getX(),end.getY());
      strip->SetLineColor(1);
    }
  }
  delete strip;
}








