// $Id: MutStation.C,v 1.51 2017/07/11 03:28:00 phnxbld Exp $
/*!
  \file MutStation.C
  \brief Describes an Station of the muon tracker system. 
  \author Douglas Fields, Nicki Bruner

*/

#include <MutDCMChannelMap.h>
#include <MutStrip.h>
#include <MutWire.h>

#include <PHGeometry.h>

#include <PdbMutDCMMap.hh>
#include <PdbCoordinate.hh>
#include <PdbCalBank.hh>

#include <TFile.h>
#include <TCanvas.h>
#include <TPaveLabel.h>
#include <TPaveText.h>
#include <TGaxis.h>

#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;
using namespace PHGeometry;
using namespace MUTGEOM;

//____________________________________________________________________
MutStation::MutStation(const MutArm* Arm,
 const StationNumber StationNum): 
  f_pArm(Arm),
  fStationNum(StationNum)
{
  SetGlobalGeom(0,0,0,0,0,0);

  NumberOfGaps=3;
  switch(fStationNum) 
  {
  
    case Station1: 
    name = "Station 1";
    wireSpacing = 1.084; //1.084 cm along rib.
    break;
    
    case Station2: 
    name = "Station 2";
    wireSpacing = 1.0196; //1.0196 cm along rib.
    break;
    
    case Station3: 
    name = "Station 3";
    wireSpacing = 1.01958; //1.01958 cm along rib.
    NumberOfGaps=2;
    break;
  
  }

  // Create Octant Objects and store pointers to them
  for (int j=0; j<NumberOfOctants; j++) 
  { f_pMutOctants[j] = new MutOctant(f_pArm,this,OctantNumber(j)); }
  
  GetGlobalGeom();

}

//____________________________________________________________________
MutStation::~MutStation()
{

  for(int i=0; i<NumberOfOctants; i++)
  if (f_pMutOctants[i]) delete f_pMutOctants[i];

}

//____________________________________________________________________
void MutStation::GetGlobalGeom()
{ //Define as the average of this Station's octants.

  int num = 0;

  for (int i=0; i<NumberOfOctants; i++){ 
    if (f_pMutOctants[i]) {
      fGlobalPosition =fGlobalPosition + f_pMutOctants[i]->getGlobalPosition();
      fGlobalVector = fGlobalVector + f_pMutOctants[i]->getGlobalVector();
      num++;
    }
  }
  fGlobalPosition = fGlobalPosition * (1.0/num);
  fGlobalVector = fGlobalVector * (1.0/num);
}

//____________________________________________________________________
void MutStation::RefreshOctant(const OctantNumber& OctantNum)
{

  if (f_pMutOctants[OctantNum]) {
    
    // The Octant object exists - delete it and create a new one.
    delete f_pMutOctants[OctantNum];
    f_pMutOctants[OctantNum] = new MutOctant(f_pArm,this,OctantNum);
  } else cout << "MutStation::RefreshOctant - Cannot refresh non-existant Octant." << "\n";
  
} 

//____________________________________________________________________
void MutStation::updateStripPositions(PHTimeStamp &Tstart, PHTimeStamp &Tstop)
{
 MutHalfOctant *halfOct = NULL;
 if(fStationNum==Station1) {
  /* All strip positions are the same from quadrant to quadrant. 
     So only update one quadrant's worth of strips.
  */
  for (int oct = 0; oct<2; oct++) {
    if(!f_pMutOctants[oct]) cout<<"The octant is missing!/n";
    for (int i=0; i<NumberOfHalfOctants; i++) { //half-octant loop
     halfOct = f_pMutOctants[oct]->f_pMutHalfOctants[i];
      for (int j=0; j<halfOct->getNumberOfGaps(); j++) { //gap loop
  halfOct->f_pMutGaps[j]
    ->f_pMutPlanes[Cathode1]->updateSt1Strips(Tstart,Tstop); 
      }
      //Only need one gap for cathode 2 strips.
      halfOct->f_pMutGaps[0]
  ->f_pMutPlanes[Cathode2]->updateSt1Strips(Tstart,Tstop); 
    }
  }
 }
 else if(fStationNum==Station3) {
   /* This survey data is organized by plane, not octant, so
      loop over planes in one octant to copy file into the db.
   */

   for (int j=0; j<2; j++) { //half-octant loop
     halfOct = f_pMutOctants[0]->f_pMutHalfOctants[j];
     for (int k=0; k< halfOct->getNumberOfGaps(); k++) {
       for (int l=Cathode1; l<NumberOfPlanes; l++) { 
   MutPlane *plane = halfOct->f_pMutGaps[k]->f_pMutPlanes[l];
   if(plane->getPlane()!=Wire) plane->updateSt3StripSrvyData(Tstart,Tstop); 
       }
     }
   }
 }

}
 
//____________________________________________________________________
void MutStation::updateSt3HalfOctFrame(PHTimeStamp &Tstart, PHTimeStamp &Tstop, const char *file)
{
  if(fStationNum!=Station3) {
    cout <<"This function is only for Station 3.\n No Action taken.\n";
    return;
  }

  ifstream s(file);
  if (!s){ 
    cout<<"Error in MutStation::updateSt3HalfOctFrame opening file "<<file<<".\n";
    return;
  }
  else { cout<<"Reading file "<<file<<endl;}

  PdbBankID bankID=0;
  const char *bankName = "survey.mut.St3HalfOct";
  if(getArm()==North) bankName = "survey.mut.St3NorthHalfOct";
  const char *descrip = "St3 half-octant frame positions";
  
  if(!update( "PdbCoordinateBank", Tstart, Tstop, bankName, bankID, descrip)){
    cout << "Error in MutStation::updateSt3HalfOctFrame, update failed.\n";
    cout << "Station 3 half-octant frame positions have not been stored in DB.\n";
    return;
  }

  int bufsize = 256;
  char linebuf[bufsize];
  char pinName[7];
  double a,b,c; //data place holders
  PdbCoordinate *pinPosition;
  int line =0;

  //8 entries for each octant
  geometryBank->setLength(64); 
  while(s.getline(linebuf,bufsize))
  {
    istringstream stringbuf(linebuf);
    stringbuf >> pinName >> a >> b >> c ;

    if(pinName[0]=='3') {
      pinPosition = (PdbCoordinate*)&geometryBank->getEntry(line);
      pinPosition->setAllParameters(a,b,c);
      line++;
    }
  }
  cout<<"Committed "<<line<<" entries into "<<bankName<<endl;

  if(!commit()) cout<<"Error in MutStation::updateSt3HalfOctFrame commit.\n";
}

//____________________________________________________________________
void MutStation::printDCMChannelMap( std::string tag ) const
{
  
  string file( MutDCMChannelMap::bankName( getArm(), fStationNum, tag ) );  
  ofstream out( file.c_str() );
  if( !out )
  {
    cout << "MutStation::printDCMChannelMap - arm: " << getArm() << " station: " << fStationNum << " - cannot write to file " << file << endl;
    return;
  } else cout << "MutStation::printDCMChannelMap - arm: " << getArm() << " station: " << fStationNum << " - writting to " << file << endl;
  
  // loop over octants, half octant, gap and cathode
  for( int i_octant = 0; i_octant < MUTGEOM::NumberOfOctants; i_octant++ )
  for( int i_half = 0; i_half < MUTGEOM::NumberOfHalfOctants; i_half++ )
  for( int i_gap = 0; i_gap < MUTGEOM::NumberOfGaps; i_gap++ )
  {
    // station 3 has only two gaps
    if( fStationNum == MUTGEOM::Station3 && i_gap == MUTGEOM::Gap3 ) continue;
    
    
    // loop over cathode
    for( int i_cathode = 0; i_cathode < MUTGEOM::NumberOfCathodePlanes; i_cathode++ )
    {
      
      // get correct cathode id
      int cathode_id( i_cathode ? MUTGEOM::Cathode1 : MUTGEOM::Cathode2 );
      MutPlane* plane_ptr =
        f_pMutOctants[i_octant]
        ->f_pMutHalfOctants[i_half]
        ->f_pMutGaps[i_gap]
        ->f_pMutPlanes[cathode_id];
      
      // loop over strips
      int num_elements( plane_ptr->getNumElements() );
      for( int i_strip = 0; i_strip < num_elements; i_strip++ )
      {
        MutStrip* strip_ptr = plane_ptr->f_pMutStrips[i_strip];
        assert( strip_ptr );
        out << MutDCMChannelMap::ChannelId( *strip_ptr ) << endl;
      }
    }
  }
  
  out.close();
  
}

//____________________________________________________________________
void MutStation::updateDCMChannelMap(PHTimeStamp &Tstart, PHTimeStamp &Tstop, string description )
{
  PdbBankID bankID = 0;

  string bank_name( MutDCMChannelMap::bankName( getArm(), fStationNum ) );
  
  cout << "MutStation::updateDCMChannelMap - bank name: " << bank_name << endl;
  if(!update( "PdbMutDCMMapBank", Tstart, Tstop, bank_name.c_str(), bankID, description.c_str() ) )
  {
    cout << "Error in MutStation::DCMChannelMap, update failed.\n";
    return;
  }

  //total #entries = #octants x #halfocts x #gaps x #cathode planes
  int totalEntries = NumberOfOctants * NumberOfHalfOctants * NumberOfGaps * NumberOfCathodePlanes;
  geometryBank->setLength(totalEntries); 

  PdbMutDCMMap *DCMmap;
  int index = 0;
  for(int oct=0; oct<NumberOfOctants; oct++)
  {
    for (int ho=0; ho<NumberOfHalfOctants; ho++) 
    { 
      
      MutHalfOctant *halfOct = f_pMutOctants[oct]->f_pMutHalfOctants[ho];
      for (int gap=0; gap<NumberOfGaps; gap++) 
      {
        for (int pla=Cathode1; pla<NumberOfPlanes; pla=pla+2) 
        { 
          MutPlane *plane = halfOct->f_pMutGaps[gap]->f_pMutPlanes[pla];
          DCMmap = (PdbMutDCMMap*)&geometryBank->getEntry( index );
          DCMmap->setAllIdentifiers(getArm(),fStationNum,oct,ho,gap,pla);
          for(int strip=0; strip<plane->getNumElements(); strip++)
          {
            DCMmap->setDCMChannel(strip, plane->f_pMutStrips[strip]->getDCMChannel()); 
            DCMmap->setPacketID(strip, plane->f_pMutStrips[strip]->getPacket_ID());
          }
          index++;
       }
      
      }
      
    }
    
  }
  
  if(!commit()) cout<<"Error in MutStation::updateDCMChannelMap commit.\n";
  
}

//____________________________________________________________________
void MutStation::translate(const PHPoint &translation)
{
  //Translate position of Station and its Octants.

  fGlobalPosition = fGlobalPosition + translation;
  for (int j=0; j<NumberOfOctants; j++) {
      if(f_pMutOctants[j]) f_pMutOctants[j]->translate(translation);
  }
}


//____________________________________________________________________
void MutStation::rotate(float angle, char axisLabel)
{
  rotateThis(angle, axisLabel);
  for (int j=0; j<NumberOfOctants; j++) 
      if(f_pMutOctants[j]) f_pMutOctants[j]->rotate(angle, axisLabel);
}


//____________________________________________________________________
void MutStation::XYExpansion(float expansionPercentage)
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
  for (int j=0; j<NumberOfOctants; j++) 
    if(f_pMutOctants[j]) f_pMutOctants[j]->XYExpansion(expansionPercentage);
}


//____________________________________________________________________
void MutStation::Draw(const char *filename, PHBoolean DrawDisabledOnly) 
{
  string labelString;
  const char *label;
  double x1, y1, x2, y2;
  x1 = 0; //dummy initialization

  switch(fStationNum) {
    case Station1: 
      x1 = -145.0; 
      break;
    case Station2: 
      x1 = -235.0; 
      if(getArm()==North) x1 = -275.0;
      break;
    case Station3: 
      x1 = -340.0; 
      if(getArm()==North) x1 = -470.0;
      break;
  }

  y1 = x1; x2 = -x1; y2 = x2;

  TFile *file = new TFile(filename,"RECREATE");

  TGaxis *axis = new TGaxis(x1,y1*0.9,x2,y1*0.9,x1,x2,510,"");

  TPaveText *legend = new TPaveText((x2*0.5),(y2*0.8),x2,(y2*0.95));

  if(!DrawDisabledOnly) legend->AddText("Active Channels");
  TText *t2=legend->AddText("Disabled Channels");
  t2->SetTextColor(2);

  //Draw Gap 1 Cathode 1

  labelString = name + " Gap 1, Cathode 1";
  label= labelString.c_str();
  TCanvas *canvas_Gap1Cathode1 = 
    new TCanvas("canvas_Gap1Cathode1",label,10,10,600,600);
  canvas_Gap1Cathode1->Range(x1,y1,x2,y2);
  TPaveLabel *title0 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  title0->SetFillColor(32);
  title0->Draw();
  legend->Draw();
  axis->Draw();
  for (int i=0; i<NumberOfOctants; i++) 
    f_pMutOctants[i]->stripDraw(Gap1,Cathode1,DrawDisabledOnly);
  file->Append(canvas_Gap1Cathode1);

  //Draw Gap 1 Cathode 2

  labelString = name + " Gap 1, Cathode 2";
  label= labelString.c_str();
  TCanvas *canvas_Gap1Cathode2 = new TCanvas("canvas_Gap1Cathode2",
          label,0,0,600,600);
  canvas_Gap1Cathode2->Range(x1,y1,x2,y2);
  TPaveLabel *title1 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  title1->SetFillColor(32);
  title1->Draw();
  legend->Draw();
  axis->Draw();
  for (int i=0; i<NumberOfOctants; i++) 
    f_pMutOctants[i]->stripDraw(Gap1,Cathode2,DrawDisabledOnly);
  file->Append(canvas_Gap1Cathode2);

  //Draw Gap 2 Cathode 1

  labelString = name + " Gap 2, Cathode 1";
  label= labelString.c_str();
  TCanvas *canvas_Gap2Cathode1 = 
    new TCanvas("canvas_Gap2Cathode1",label,10,10,600,600);
  canvas_Gap2Cathode1->Range(x1,y1,x2,y2);
  TPaveLabel *title2 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  title2->SetFillColor(32);
  title2->Draw();
  legend->Draw();
  axis->Draw();
  for (int i=0; i<NumberOfOctants; i++) 
    f_pMutOctants[i]->stripDraw(Gap2,Cathode1,DrawDisabledOnly);
  file->Append(canvas_Gap2Cathode1);

  //Draw Gap 2 Cathode 2

  labelString = name + " Gap 2, Cathode 2";
  label= labelString.c_str();
  TCanvas *canvas_Gap2Cathode2 = new TCanvas("canvas_Gap2Cathode2",
          label,0,0,600,600);
  canvas_Gap2Cathode2->Range(x1,y1,x2,y2);
  TPaveLabel *title3 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  title3->SetFillColor(32);
  title3->Draw();
  legend->Draw();
  axis->Draw();
  for (int i=0; i<NumberOfOctants; i++) 
    f_pMutOctants[i]->stripDraw(Gap2,Cathode2,DrawDisabledOnly);
  file->Append(canvas_Gap2Cathode2);

    //Draw Gap 3 Cathode 1

  labelString = name + " Gap 3, Cathode 1";
  label= labelString.c_str();
  TCanvas *canvas_Gap3Cathode1 = new TCanvas("canvas_Gap3Cathode1",
            label,10,10,600,600);
  canvas_Gap3Cathode1->Range(x1,y1,x2,y2);
  TPaveLabel *title4 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  if(fStationNum!=Station3){
    title4->SetFillColor(32);
    title4->Draw();
    legend->Draw();
    axis->Draw();
    for (int i=0; i<NumberOfOctants; i++) 
      f_pMutOctants[i]->stripDraw(Gap3,Cathode1,DrawDisabledOnly);
    file->Append(canvas_Gap3Cathode1);
  }    
    //Draw Gap 3 Cathode 2

  labelString = name + " Gap 3, Cathode 2";
  label= labelString.c_str();
  TCanvas *canvas_Gap3Cathode2 = new TCanvas("canvas_Gap3Cathode2",
              label,0,0,600,600);
  canvas_Gap3Cathode2->Range(x1,y1,x2,y2);
  TPaveLabel *title5 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  if(fStationNum!=Station3){
    title5->SetFillColor(32);
    title5->Draw();
    legend->Draw();
    axis->Draw();
    for (int i=0; i<NumberOfOctants; i++) 
      f_pMutOctants[i]->stripDraw(Gap3,Cathode2,DrawDisabledOnly);
    file->Append(canvas_Gap3Cathode2);
 }

  //Draw wire planes

  labelString = name + " Gap1 wire plane";
  label= labelString.c_str();
  TCanvas *canvas_Gap1wire = 
    new TCanvas("canvas_Gap1wire",label,10,10,600,600);
  canvas_Gap1wire->Range(x1,y1,x2,y2);
  TPaveLabel *title6 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  title6->SetFillColor(32);
  title6->Draw();
  legend->Draw();
  axis->Draw();
  for (int i=0; i<NumberOfOctants; i++) 
    f_pMutOctants[i]->stripDraw(Gap1,Wire,DrawDisabledOnly);
  file->Append(canvas_Gap1wire);

  labelString = name + " Gap2 wire plane";
  label= labelString.c_str();
  TCanvas *canvas_Gap2wire = 
    new TCanvas("canvas_Gap2wire",label,10,10,600,600);
  canvas_Gap2wire->Range(x1,y1,x2,y2);
  TPaveLabel *title7 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  title7->SetFillColor(32);
  title7->Draw();
  legend->Draw();
  axis->Draw();
  for (int i=0; i<NumberOfOctants; i++) 
    f_pMutOctants[i]->stripDraw(Gap2,Wire,DrawDisabledOnly);
  file->Append(canvas_Gap2wire);

  labelString = name + " Gap3 wire plane";
  label= labelString.c_str();
  TCanvas *canvas_Gap3wire = 
    new TCanvas("canvas_Gap3wire",label,10,10,600,600);
  canvas_Gap3wire->Range(x1,y1,x2,y2);
  TPaveLabel *title8 = new TPaveLabel(x1,(y2*0.9),(x1*0.2),y2,label);
  if(fStationNum!=Station3){
    title8->SetFillColor(32);
    title8->Draw();
    legend->Draw();
    axis->Draw();
    for (int i=0; i<NumberOfOctants; i++) 
      f_pMutOctants[i]->stripDraw(Gap3,Wire,DrawDisabledOnly);
    file->Append(canvas_Gap3wire);
  }

  file->Write();
  file->Close();

  delete axis;
  delete title0; delete title1; delete title2; delete title3;
  delete title4; delete title5; delete title6; delete title7;
  delete title8; delete legend; 
  delete canvas_Gap1Cathode1; delete canvas_Gap1Cathode2;
  delete canvas_Gap2Cathode1; delete canvas_Gap2Cathode2;
  delete canvas_Gap3Cathode1; delete canvas_Gap3Cathode2;
  delete canvas_Gap1wire; delete canvas_Gap2wire; delete canvas_Gap3wire;

  delete file;

  cout<<name<<" canvases have been written to "<<filename<<".\n";
}

//____________________________________________________________________
void MutStation::DrawDisabledChannels(const char *filename) 
{
  PHBoolean DrawDisabledOnly=True;
  Draw(filename, DrawDisabledOnly);
}


//____________________________________________________________________
int MutStation::convertPisaHitToChamber(PHPoint PisaCoordinate, 
      MutWire *ptrWire[1], double &wireIP,
      MutStrip *ptrStrip[2], double stripIP[2])
{
  /* 
     return errors:
     0: no error.
     1: half-octant not found.
     2: gap not found.
     3: wire not found.
     4: strip not found.
     5: station not found.
  */


  PHCylPoint PisaCylCoordinate;
  MutHalfOctant *pHitHalfOctant = 0;
  MutGap *pHitGap = 0;

  cartesianToCylindrical(PisaCoordinate, PisaCylCoordinate);
  double pisaPhi = PisaCylCoordinate.getPhi().getPhi();
  
  int j=0;
  while(!pHitHalfOctant && j<8) 
  {
    
    // Locate the Half-Octant by comparing phi positions.
    //PHAngle forces the phi range to be -pi/2 to 3pi/2.
    //changing to 0 to 2Pi makes comparisons easier.
    //..except for octant 0 since it spans 5.89 to 0.34 rads.
    double octBeginPhi = f_pMutOctants[j]->getBeginPhi();
    double octEndPhi = f_pMutOctants[j]->getEndPhi();
    if(j>0) {
      if(pisaPhi<0) pisaPhi += 2*M_PI;
      if(octBeginPhi<0) octBeginPhi += 2*M_PI;
      if(octEndPhi<0) octEndPhi += 2*M_PI;
    }
    
    //station 2 and 3 half-octants are symmetric
    double halfOctBoundaryPhi = (octEndPhi+octBeginPhi)/2.0;

    //station 1 half-octants are not.
    if(fStationNum==Station1) {
      MutPlane *pla = f_pMutOctants[j]->f_pMutHalfOctants[HalfOctant1]->f_pMutGaps[Gap1]->f_pMutPlanes[Cathode1];
      PHCylPoint SphPoint0(pla->f_pMutStrips.back()->getGlobalPositionEnd());
      halfOctBoundaryPhi = SphPoint0.getPhi().getPhi();
      if(j>0 && halfOctBoundaryPhi<0) halfOctBoundaryPhi += 2*M_PI;
    }
    
    if(pisaPhi >= octBeginPhi && pisaPhi <= octEndPhi) {
      
      //found hit octant
      if(pisaPhi <= halfOctBoundaryPhi) pHitHalfOctant = f_pMutOctants[j]->f_pMutHalfOctants[0];
      else pHitHalfOctant = f_pMutOctants[j]->f_pMutHalfOctants[1];
    }
    j++;
  }
  
  if(!pHitHalfOctant) return 1;
  
  for (int j=0; j<pHitHalfOctant->getNumberOfGaps(); j++) 
  {
    PHPoint gapPosition = pHitHalfOctant->f_pMutGaps[j]->getGlobalPosition();
    double zDiff = abs(PisaCoordinate.getZ() - gapPosition.getZ());
    
    // How close should pisa hit be to wire plane?
    double gapThickness=pHitHalfOctant->f_pMutGaps[j]->getGapThickness();
    if(zDiff < gapThickness) pHitGap = pHitHalfOctant->f_pMutGaps[j];
  }
  
  if(!pHitGap) return 2;

  //Find the nearest wire to hit.
  PHLine Wire0(
    pHitGap->f_pMutPlanes[1]->f_pMutWires[0]->getGlobalPositionBegin(),
    pHitGap->f_pMutPlanes[1]->f_pMutWires[0]->getGlobalPositionEnd());

  double hitToWire0 = distanceLinePoint(Wire0, PisaCoordinate);
  
  // distance between wires along ribs * rib direction
  size_t hitWireNum = (size_t) abs(hitToWire0/(wireSpacing * cos(M_PI/16)));  

  wireIP = hitToWire0 - hitWireNum*wireSpacing*cos(M_PI/16);
  if (wireIP>0.50*(wireSpacing * cos(M_PI/16))) 
  {
    hitWireNum++;
    wireIP=wireIP - wireSpacing*cos(M_PI/16);
  }

  if(hitWireNum>=pHitGap->f_pMutPlanes[1]->f_pMutWires.size()) return 3;
  
  // the check on whether MC hits are removed or not based on the wire being dead
  // should not be done here, but at a later stage, when converting MC hits to hits
  // during the response. This allows to get an efficiency that includes the dead wires
  // if(pHitGap->f_pMutPlanes[1]->f_pMutWires[hitWireNum]->ChannelIsDead()) return 3;

  ptrWire[0]= pHitGap->f_pMutPlanes[1]->f_pMutWires[hitWireNum];

  PHLine Wire(ptrWire[0]->getGlobalPositionBegin(), ptrWire[0]->getGlobalPositionEnd());


  //Find hit strips for both cathode planes.
  for(int i=Cathode1; i<=Cathode2; i=i+2)
  {
    PHLine Strip0( 
      pHitGap->f_pMutPlanes[i]->f_pMutStrips[0]->getGlobalPositionBegin(), 
      pHitGap->f_pMutPlanes[i]->f_pMutStrips[0]->getGlobalPositionEnd());

    double hitToStrip0 = distanceLinePoint(Strip0, PisaCoordinate);
    int hitStripNum = static_cast<int>( hitToStrip0/1 ); // about 1.0 cm between strips


    /* This gets us close, but the strip spacing varies by station and plane. So we iterate a little.
    */

    double hitToPrevious = 10000.0;
    int NumElements = pHitGap->f_pMutPlanes[i]->getNumElements();
    int min= hitStripNum<3 ? 0: hitStripNum-3;
    int max= hitStripNum<NumElements-4 ? hitStripNum+4 : NumElements;

    for (int j = min; j<max; j++) {

      MutStrip *pStripTemp = pHitGap->f_pMutPlanes[i]->f_pMutStrips[j];

      if(pStripTemp) {
        PHLine Strip(pStripTemp->getGlobalPositionBegin(), pStripTemp->getGlobalPositionEnd());

        double hitToStrip=distanceLinePoint(Strip, PisaCoordinate);
        if (abs(hitToStrip)<abs(hitToPrevious)) ptrStrip[(i/2)] = pStripTemp;
        hitToPrevious=hitToStrip;
      }
    }

    /* 
    To get the impact parameter of the hit on the nearest strip, we find the 
    point of closest approach on the strip.  Then find the distance to the 
    hit's projection on the plane.
    */

    if(ptrStrip[(i/2)]) 
    {
      PHLine Strip(ptrStrip[(i/2)]->getGlobalPositionBegin(),
      ptrStrip[(i/2)]->getGlobalPositionEnd());
      PHPoint PointOnHitStrip = closestApproachLinePoint(Strip, PisaCoordinate);
      PHLine HitProjection(PisaCoordinate,pHitGap->getGlobalVector());
      stripIP[(i/2)] = distanceLinePoint(HitProjection, PointOnHitStrip);

      PHCylPoint PointOnHitStripCyl;
      cartesianToCylindrical(PointOnHitStrip, PointOnHitStripCyl);
      if(PisaCylCoordinate.getPhi()<PointOnHitStripCyl.getPhi()) stripIP[(i/2)] = stripIP[(i/2)]*-1.0;

      // Make sure hit has not fallen outside edge of active area:
      if (abs(stripIP[(i/2)]) > ptrStrip[(i/2)]->getStripSpacing()/2.0) ptrStrip[(i/2)] = NULL;
      
    }
    
    //do we want to keep the other pointers here?
    if(!ptrStrip[(i/2)]) {  
      ptrWire[0] = NULL;
      ptrStrip[0] = NULL;
      ptrStrip[1] = NULL;
      return 4;
    }
  }
  return 0;
}


//____________________________________________________________________
void MutStation::getDCMRegions(const char *regionFile, const char *angleFile)
{
 double x1, x2, x3, x4, x5, y1, y2, y3, y4, y5;
 PHVector v1, v2, stripVector;

 ofstream s(regionFile);
 s<<"plane pktID  x1\t  y1\t   x2\t   y2\t   x3\t   y3\t   x4\t   y4 \n";

 ofstream t(angleFile);
 t<<"plane pktID  strip0x  strip0y vectorX vectorY\n";

 for(int k=0; k<NumberOfOctants; k++) {
   for(int h=0; h<NumberOfHalfOctants; h++) {
     for(int j=Cathode1; j<=Cathode2; j=j+2) {
       MutPlane *plane = f_pMutOctants[k]->f_pMutHalfOctants[h]->
   f_pMutGaps[0]->f_pMutPlanes[j];
       s<<j/2<<"/"<<j/2+2<<" ";
       s<<plane->f_pMutStrips[0]->getPacket_ID()<<" ";
       t<<j/2<<"/"<<j/2+2<<" ";
       t<<plane->f_pMutStrips[0]->getPacket_ID()<<" ";

       x1 =plane->f_pMutStrips[0]->getGlobalPositionEnd().getX();
       y1 =plane->f_pMutStrips[0]->getGlobalPositionEnd().getY();
       x2 =plane->f_pMutStrips[0]->getGlobalPositionBegin().getX();
       y2 =plane->f_pMutStrips[0]->getGlobalPositionBegin().getY();
       s<<x1<<" "<<y1<<" "<<x2<<" "<<y2<<" ";

       stripVector =plane->f_pMutStrips[0]->getGlobalPositionEnd() - 
     plane->f_pMutStrips[0]->getGlobalPositionBegin();
       stripVector.normalize();
       t<<x2<<" "<<y2<<" "<<stripVector.getX()<<" "<<stripVector.getY()<<endl;

       v1=plane->f_pMutStrips[1]->getGlobalPositionBegin() - 
     plane->f_pMutStrips[0]->getGlobalPositionBegin();
       v1.normalize();
       for(int i=31; i<plane->getNumElements(); i=i+32){
   x3 = plane->f_pMutStrips[i]->getGlobalPositionBegin().getX();
   y3 = plane->f_pMutStrips[i]->getGlobalPositionBegin().getY();
   x4 = plane->f_pMutStrips[i]->getGlobalPositionEnd().getX();
   y4 = plane->f_pMutStrips[i]->getGlobalPositionEnd().getY();

   v2=plane->f_pMutStrips[i]->getGlobalPositionBegin() - 
      plane->f_pMutStrips[i-31]->getGlobalPositionBegin();
   v2.normalize();
   int n=i;
   while(v1.dot(v2)<0.999 && n>=i-30) {
     n--;
     v2 = plane->f_pMutStrips[n]->getGlobalPositionBegin() - 
         plane->f_pMutStrips[i-31]->getGlobalPositionBegin();
     v2.normalize();
   }
   if(n!=i) {
     x5 = plane->f_pMutStrips[n]->getGlobalPositionBegin().getX();
     y5 = plane->f_pMutStrips[n]->getGlobalPositionBegin().getY();
     s<<x5<<" "<<y5<<" ";
   }
   s<<x3<<" "<<y3<<" ";
   s<<x4<<" "<<y4<<endl;
   x2 = plane->f_pMutStrips[i+1]->getGlobalPositionBegin().getX();
   y2 = plane->f_pMutStrips[i+1]->getGlobalPositionBegin().getY();
   x1 = plane->f_pMutStrips[i+1]->getGlobalPositionEnd().getX();
   y1 = plane->f_pMutStrips[i+1]->getGlobalPositionEnd().getY();
   s<<j/2<<"/"<<j/2+2<<" ";
   s<<plane->f_pMutStrips[i+1]->getPacket_ID()<<" ";
   s<<x1<<" "<<y1<<" ";
   s<<x2<<" "<<y2<<" ";

   t<<j/2<<"/"<<j/2+2<<" ";
   t<<plane->f_pMutStrips[i+1]->getPacket_ID()<<" ";
   stripVector =plane->f_pMutStrips[i+1]->getGlobalPositionEnd() - 
     plane->f_pMutStrips[i+1]->getGlobalPositionBegin();
   stripVector.normalize();
   t<<x2<<" "<<y2<<" "<<stripVector.getX()<<" "<<stripVector.getY()<<endl;

   v1=plane->f_pMutStrips[i+2]->getGlobalPositionBegin() -  plane->f_pMutStrips[i+1]->getGlobalPositionBegin();
   v1.normalize();
        }
       s<<plane->f_pMutStrips.back()->getGlobalPositionBegin().getX()<<" ";
       s<<plane->f_pMutStrips.back()->getGlobalPositionBegin().getY()<<" ";
       s<<plane->f_pMutStrips.back()->getGlobalPositionEnd().getX()<<" ";
       s<<plane->f_pMutStrips.back()->getGlobalPositionEnd().getY()<<endl;
     }
   }
 }
 s.close();
 t.close();
 cout<<"Wrote DCM regions for "<<name<<" to "<<regionFile<<endl;
 cout<<"Wrote Strip begin positions and angles for each DCM region for "<<name<<" to "<<angleFile<<endl;
}

