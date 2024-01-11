#include "RPCFULLGEOM.h"
#include "RpcGeom.h"
#include "RpcStrip_v1.h"
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <TMath.h>
#include <TLine.h>

using namespace std;

const void RpcStrip_v1::InitVal()

{
  arm = station = octant = halfoctant = Rseg = strip = 
    (int)RPCFULLGEOM::OUTOFRANGE;

  double oor = (double)RPCFULLGEOM::OUTOFRANGE;
  begin_x = oor; begin_y = oor; begin_z = oor;
  end_x = oor; end_y = oor; end_z = oor;
  mid_x = oor; mid_y = oor; mid_z = oor;

  db = RpcDBInfo::getInstance();
}

int RpcStrip_v1::IsEmpty() const
{
  int oor_int = (int)RPCFULLGEOM::OUTOFRANGE;
  if ( arm <= oor_int || station <= oor_int || octant <= oor_int
       || Rseg <= oor_int || strip <= oor_int ){
    return 1; 

    /*coverity
    //cout << "RpcStrip:: strip is empty" << endl;
    print();
    */
  }
  return 0;
}

int RpcStrip_v1::IsDead() 
{
  return db->isDead(this);
}

void RpcStrip_v1::SetStrip(double x, double y, int arm_index, int station_index, int R_segmentation)
{
  //From a given hit position, get the strip number...
  //In principle, can calculate the r_seg from the x,y position
  //BUT would need to include the possibility of TWO hits (for overlapping segments)

  int _arm,_station, _octant, _halfoctant, _Rseg, _strip;
  _Rseg = _strip = (int)RPCFULLGEOM::OUTOFRANGE;
  _Rseg = R_segmentation;
  
  double hit_x = x;
  double hit_y = y;

  _arm = arm_index;
  _station = station_index;
  
  float phi = atan2(hit_y,hit_x)*RPCFULLGEOM::RAD_TO_DEG;//Minus sign to get the right octant number
  if(phi>=360.) { phi-=360.; }
  if(phi<0.0)   { phi+=360.; }
  
  _octant = (int(phi+22.5))/45;//Crude interpretation may need fixing
  _halfoctant = (phi-45*_octant)>0 ? 1:0;//(RPCFULLGEOM::Left:RPCFULLGEOM::Right)
  if(_octant==8) { _octant = 0; } //otherwise (7,0) will appear as (8,0)
  
  //cout << "Detected: " << phi << " " << _octant << " " << _halfoctant << endl;
  //_octant = 4; _halfoctant=0;
  for(Int_t istrip=0 ; istrip<64 ; istrip++) {
    double thisdca = GetPointStripDCA(x,y,_arm,_station,_octant,_halfoctant,R_segmentation,istrip);
    if(thisdca<-998) { continue; } //Strip does not exist
    
    double stripwidth = RPCFULLGEOM::StripWidth_Outer[_station][_Rseg]/2.;
    if(istrip<32) {//Is an inner layer strip
      stripwidth = RPCFULLGEOM::StripWidth_Inner[_station][_Rseg]/2.;}
    stripwidth += 0.5001*RPCFULLGEOM::StripGap;
    
    if(thisdca<=stripwidth) { _strip=istrip; break; }}
  
  SetStrip(_arm,_station,_octant,_halfoctant,_Rseg,_strip);
  return;
}

void RpcStrip_v1::SetStrip(double x, double y, int arm_index, int station_index, int R_segmentation, int excludestrip)
{
  //From a given hit position, get the strip number...
  //In principle, can calculate the r_seg from the x,y position
  //BUT would need to include the possibility of TWO hits (for overlapping segments)

  int _arm,_station, _octant, _halfoctant, _Rseg, _strip;
  _Rseg = _strip = (int)RPCFULLGEOM::OUTOFRANGE;
  _Rseg = R_segmentation;
  
  double hit_x = x;
  double hit_y = y;

  _arm = arm_index;
  _station = station_index;
  
  float phi = atan2(hit_y,hit_x)*RPCFULLGEOM::RAD_TO_DEG;//Minus sign to get the right octant number
  if(phi>=360.) { phi-=360.; }
  if(phi<0.0)   { phi+=360.; }
  
  _octant = (int(phi+22.5))/45;//Crude interpretation may need fixing
  _halfoctant = (phi-45*_octant)>0 ? 1:0;//(RPCFULLGEOM::Left:RPCFULLGEOM::Right)
  if(_octant==8) { _octant = 0; } //otherwise (7,0) will appear as (8,0)
  
  //cout << "Detected: " << phi << " " << _octant << " " << _halfoctant << endl;
  //_octant = 4; _halfoctant=0;
  double lowdca = 10000;
  
  for(Int_t istrip=0 ; istrip<64 ; istrip++) {
    if(istrip==excludestrip) { continue; }
    double thisdca = GetPointStripDCA(x,y,_arm,_station,_octant,_halfoctant,R_segmentation,istrip);
    if(thisdca<-998) { continue; } //Strip does not exist
    if(lowdca>thisdca) { lowdca = thisdca; _strip = istrip; } }
  
  //Special case of long strips "on their own"
  //this selects the next strip away from the boundary
  if(excludestrip==37 && _Rseg==1) { _strip = 36; }
  if(excludestrip==36 && _Rseg==2) { _strip = 35; }
  SetStrip(_arm,_station,_octant,_halfoctant,_Rseg,_strip);
  return;
}

double RpcStrip_v1::GetPointStripDCA(double x, double y, int arm_index, int station_index, int octant, int halfoct, int R_segmentation, int strip)
{
  RPCFULLGEOM full(RPCFULLGEOM::ArmNumber(arm_index),RPCFULLGEOM::StationNumber(station_index),octant,halfoct,RPCFULLGEOM::RadialSegment(R_segmentation),strip);
  if(!full.checkStrip()) { return -999; }  //This strip does not exist
  double strip_x0 = full.GetBegin().getX();
  double strip_y0 = full.GetBegin().getY();
  double strip_x1 = full.GetEnd().getX();
  double strip_y1 = full.GetEnd().getY();
  
  full.Delete();
  //********************************
  //
  // Calculated from the vectors:
  // - A[(x0,y0),(x1,y1)] (strip end to strip begin)
  // - B[(x0,y0),(x,y)] (hit point to strip begin)
  // 
  // Next use: vA(dot)vB = |vA|*|vB|*cos(angle) to extract the angle
  //
  // DCA is then: dca = |vA|*sin(angle)
  //
  //********************************
  
  double stripwidth = RPCFULLGEOM::StripWidth_Outer[station_index][R_segmentation]/2.;
  if(strip<32) {//Is the inner layers
    stripwidth = RPCFULLGEOM::StripWidth_Inner[station_index][R_segmentation]/2.;}
  
  double xy_to_begin = sqrt(pow(strip_x0-strip_x1,2)+pow(strip_y0-strip_y1,2))*
    sqrt(pow(strip_x0-x,2)+pow(strip_y0-y,2));//|vA|*|vB|
  
  //acos((vA(dot)vB)/(|vA|*|vB|)):
  double fNumerator = ((strip_x0-strip_x1)*(strip_x0-x)+(strip_y0-strip_y1)*(strip_y0-y));
  //Condition stops xy_to_begin from being smaller than fNumerator
  //happens when numbers are too close together and ratio >1 (unphysical)
  if(fNumerator>xy_to_begin) { xy_to_begin = fNumerator+0.00001; }
  double angle = acos(fNumerator/xy_to_begin);
  
  //check that this hit is not outside the strip length
  double length_to_begin = sqrt(pow(strip_x0-x,2)+pow(strip_y0-y,2));
  double length_to_end   = sqrt(pow(strip_x1-x,2)+pow(strip_y1-y,2));
  double thislength      = sqrt(pow(RPCFULLGEOM::StripLength[station_index][R_segmentation][strip],2)+pow(stripwidth,2));
  //cout << istrip << "  " << thislength << " " << length_to_begin << " " << length_to_end << " " << _Rseg << endl;
  if(length_to_begin>thislength || length_to_end>thislength) { return -999; }
  
  //check if within half strip width (+gap/2)
  double thisdca = fabs(length_to_begin*sin(angle));
  
  //if(length_to_begin<thislength && length_to_end<thislength) {
  return thisdca; //}
  //  else {
  //return -thisdca-TMath::Min(length_to_begin,length_to_end); }
}

void RpcStrip_v1::SetStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip)
{
  //This should be RPCFULLGEOM::ArmNumber for example
  
  //CheckStrip(_arm,_station,_octant,_halfoctant,_Rseg,_strip);
  
  arm = _arm;
  station = _station;
  octant = _octant;
  halfoctant = _halfoctant;
  Rseg = _Rseg;
  strip = _strip;
  
  //if(db->isDead(this)) {
  //cout << "this is dead!" << endl; }
  return;
}

double RpcStrip_v1::GetStripWidth() const
{
  if(station<0 || station>2) {
    cout << "RpcStrip_v1::GetStripWidth() ... Station Unknown" << endl;
    return 0; }
  if(Rseg<0 || Rseg>2) {
    cout << "RpcStrip_v1::GetStripWidth() ... Radial Segment Unknown" << endl;
    return 0; }
  
  if(RpcStrip_v1::strip>=32) {//This is the outer layers
    return RPCFULLGEOM::StripWidth_Outer[station][Rseg];}
  else {
    return RPCFULLGEOM::StripWidth_Inner[station][Rseg];}  
}

double RpcStrip_v1::GetStripWidth(int _station, int _Rseg) const
{
  if(station<0 || station>2) {
    cout << "RpcStrip_v1::GetStripWidth(int _station, int _Rseg) ... Station Unknown" << endl;
    return 0; }
  if(Rseg<0 || Rseg>2) {
    cout << "RpcStrip_v1::GetStripWidth(int _station, int _Rseg) ... Radial Segment Unknown" << endl;
    return 0; }
  
  if(RpcStrip_v1::strip>=32) {//This is the outer layers
    return RPCFULLGEOM::StripWidth_Outer[_station][_Rseg];}
  else {
    return RPCFULLGEOM::StripWidth_Inner[_station][_Rseg];}  
}

double RpcStrip_v1::GetStripLength() const
{
  if(station<0 || station>2) {
    cout << "RpcStrip_v1::GetStripLength() ... Station Unknown" << endl;
    return 0; }
  if(Rseg<0 || Rseg>2) {
    cout << "RpcStrip_v1::GetStripLength() ... Radial Segment Unknown" << endl;
    return 0; }
  if(strip<0 || strip>=64) {
    cout << "RpcStrip_v1::GetStripLength() ... Strip Unknown" << endl;
    return 0; }

  return RPCFULLGEOM::StripLength[station][Rseg][strip];
}

PHPoint RpcStrip_v1::GetBegin()
{
  RPCFULLGEOM geom(RPCFULLGEOM::ArmNumber(arm),RPCFULLGEOM::StationNumber(station),octant,halfoctant,RPCFULLGEOM::RadialSegment(Rseg),strip);

  begin_x = geom.GetBegin().getX();
  begin_y = geom.GetBegin().getY();
  begin_z = geom.GetBegin().getZ();
  
  return geom.GetBegin();
}

PHPoint RpcStrip_v1::GetEnd()
{
  RPCFULLGEOM geom(RPCFULLGEOM::ArmNumber(arm),RPCFULLGEOM::StationNumber(station),octant,halfoctant,RPCFULLGEOM::RadialSegment(Rseg),strip);
  
  end_x = geom.GetEnd().getX();
  end_y = geom.GetEnd().getY();
  end_z = geom.GetEnd().getZ();

  return geom.GetEnd();
}

PHPoint RpcStrip_v1::GetMid()
{
  RPCFULLGEOM geom(RPCFULLGEOM::ArmNumber(arm),RPCFULLGEOM::StationNumber(station),octant,halfoctant,RPCFULLGEOM::RadialSegment(Rseg),strip);
  
  mid_x = geom.GetMid().getX();
  mid_y = geom.GetMid().getY();
  mid_z = geom.GetMid().getZ();

  return geom.GetMid();
} 

void RpcStrip_v1::CheckStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip)
{
  RPCFULLGEOM geom(RPCFULLGEOM::ArmNumber(_arm),
		     RPCFULLGEOM::StationNumber(_station),
		     _octant,_halfoctant,
		     RPCFULLGEOM::RadialSegment(Rseg),
		     _strip);
  
  if(!geom.checkStrip()){
    ostringstream what;
    what 
      << "RpcStrip_v1::CheckStrip - invalid strip please check: " << endl
      << " " << _arm << ", max is " << RPCFULLGEOM::NumberOfArms-1 << " " 
      << " " << _station << ", max is " << RPCFULLGEOM::NumberOfStations-1 << " "
      << " " << _octant << ", max is " << 7 << " "
      << " " << _halfoctant << ", max is " << 1 << " "
      << " " << _Rseg << ", max is " << 2 << " "
      << " " << _strip << ", max is " << 63 << " (but some missing!) ";
    RPCFULLGEOM::TRACE(what.str());
    exit(1);}
  
  return;
}
