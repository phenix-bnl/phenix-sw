#include "RPCPROTOGEOM.h"
#include "RpcGeom.h"
#include "RpcStrip_proto.h"
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <TMath.h>
#include <TLine.h>

using namespace std;

const void RpcStrip_proto::InitVal()

{
  arm = station = octant = halfoctant = Rseg = strip = 
    (int)RPCPROTOGEOM::OUTOFRANGE;

  double oor = (double)RPCPROTOGEOM::OUTOFRANGE;
  begin_x = oor; begin_y = oor; begin_z = oor;
  end_x = oor; end_y = oor; end_z = oor;
  mid_x = oor; mid_y = oor; mid_z = oor;
}

int RpcStrip_proto::IsEmpty() const
{
  int oor_int = (int)RPCPROTOGEOM::OUTOFRANGE;
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

void RpcStrip_proto::SetStrip(double x, double y, int arm_index, int station_index, int R_segmentation)
{
  //From a given hit position, get the strip number...
  //In principle, can calculate the r_seg from the x,y position
  //BUT would need to include the possibility of TWO hits (for overlapping segments)
  
  //Apply a tiny jitter to stop input x,y and geometry from precisely overlapping
  x += 0.00001;
  y += 0.00001;
  
  int _arm,_station, _octant, _halfoctant, _Rseg, _strip;
  _Rseg = _strip = (int)RPCPROTOGEOM::OUTOFRANGE;
  _Rseg = R_segmentation;
  
  /*clang
  double hit_x = x;
  double hit_y = y;
  */
  _arm = arm_index;
  _station = station_index;
  
  /*clang
  float phi = atan2(hit_y,hit_x)*RPCPROTOGEOM::RAD_TO_DEG;
  if(phi>=360.) { phi-=360.; }
  if(phi<0.0)   { phi+=360.; }
  */
  
  //_octant = (int(phi+22.5))/45;//Crude interpretation and not wholly correct for prototype, but see below (_octant and _halfoctant are fixed)
  //_halfoctant = (phi-45*_octant)>0 ? 1:0;//(RPCPROTOGEOM::Left:RPCPROTOGEOM::Right)
  
  //  cout << "Detected: " << _octant << " " << _halfoctant << endl;
  _octant = 4; _halfoctant=0;
  
  for(Int_t istrip=0 ; istrip<64 ; istrip++) {
    double thisdca = GetPointStripDCA(x,y,_arm,_station,_octant,_halfoctant,R_segmentation,istrip);
    if(thisdca<-998) { continue; } //Strip does not exist
    
    double stripwidth = RPCPROTOGEOM::StripWidth_Outer[_station][_Rseg]/2.;
    if(istrip<32) {//Is an inner layer strip
      stripwidth = RPCPROTOGEOM::StripWidth_Inner[_station][_Rseg]/2.;}
    stripwidth += 0.5001*RPCPROTOGEOM::StripGap;

    if(thisdca<=stripwidth) { _strip=istrip; break; }}
  
  SetStrip(_arm,_station,_octant,_halfoctant,_Rseg,_strip);
  return;
}

double RpcStrip_proto::GetPointStripDCA(double x, double y, int arm_index, int station_index, int octant, int halfoct, int R_segmentation, int strip)
{
  RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(arm_index),RPCPROTOGEOM::StationNumber(station_index),octant,halfoct,RPCPROTOGEOM::RadialSegment(R_segmentation),strip);
  if(!proto.checkStrip()) { return -999; }  //This strip does not exist
  double strip_x0 = proto.GetBegin().getX();
  double strip_y0 = proto.GetBegin().getY();
  double strip_x1 = proto.GetEnd().getX();
  double strip_y1 = proto.GetEnd().getY();
  
  proto.Delete();
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
  
  double stripwidth = RPCPROTOGEOM::StripWidth_Outer[station_index][R_segmentation]/2.;
  if(strip<32) {//Is the inner layers
    stripwidth = RPCPROTOGEOM::StripWidth_Inner[station_index][R_segmentation]/2.;}
  
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
  double thislength      = sqrt(pow(RPCPROTOGEOM::StripLength[station_index][R_segmentation][strip],2)+pow(stripwidth,2));
  //cout << istrip << "  " << thislength << " " << length_to_begin << " " << length_to_end << " " << _Rseg << endl;
  if(length_to_begin>thislength || length_to_end>thislength) { return -999; }
  
  //check if within half strip width (+gap/2)
  double thisdca = fabs(length_to_begin*sin(angle));
  
  return thisdca;
}

void RpcStrip_proto::SetStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip)
{
  //This should be RPCPROTOGEOM::ArmNumber for example
  
  //CheckStrip(_arm,_station,_octant,_halfoctant,_Rseg,_strip);
  
  arm = _arm;
  station = _station;
  octant = _octant;
  halfoctant = _halfoctant;
  Rseg = _Rseg;
  strip = _strip;
  
  return;
}

double RpcStrip_proto::GetStripWidth() const
{
  if(station<0 || station>2) {
    cout << "RpcStrip_proto::GetStripWidth() ... Station Unknown" << endl;
    return 0; }
  if(Rseg<0 || Rseg>2) {
    cout << "RpcStrip_proto::GetStripWidth() ... Radial Segment Unknown" << endl;
    return 0; }

  if(RpcStrip_proto::strip>=32) {//This is the outer layers
    return RPCPROTOGEOM::StripWidth_Outer[station][Rseg];}
  else {
    return RPCPROTOGEOM::StripWidth_Inner[station][Rseg];}  
}

double RpcStrip_proto::GetStripWidth(int _station, int _Rseg) const
{
  if(station<0 || station>2) {
    cout << "RpcStrip_proto::GetStripWidth(int _station, int _Rseg) ... Station Unknown" << endl;
    return 0; }
  if(Rseg<0 || Rseg>2) {
    cout << "RpcStrip_proto::GetStripWidth(int _station, int _Rseg) ... Radial Segment Unknown" << endl;
    return 0; }

  if(RpcStrip_proto::strip>=32) {//This is the outer layers
    return RPCPROTOGEOM::StripWidth_Outer[_station][_Rseg];}
  else {
    return RPCPROTOGEOM::StripWidth_Inner[_station][_Rseg];}  
}

PHPoint RpcStrip_proto::GetBegin()
{
  RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(arm),RPCPROTOGEOM::StationNumber(station),octant,halfoctant,RPCPROTOGEOM::RadialSegment(Rseg),strip);

  begin_x = proto.GetBegin().getX();
  begin_y = proto.GetBegin().getY();
  begin_z = proto.GetBegin().getZ();
  
  return proto.GetBegin();
}

PHPoint RpcStrip_proto::GetEnd()
{
  RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(arm),RPCPROTOGEOM::StationNumber(station),octant,halfoctant,RPCPROTOGEOM::RadialSegment(Rseg),strip);
  
  end_x = proto.GetEnd().getX();
  end_y = proto.GetEnd().getY();
  end_z = proto.GetEnd().getZ();

  return proto.GetEnd();
}

PHPoint RpcStrip_proto::GetMid()
{
  RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(arm),RPCPROTOGEOM::StationNumber(station),octant,halfoctant,RPCPROTOGEOM::RadialSegment(Rseg),strip);
  
  mid_x = proto.GetMid().getX();
  mid_y = proto.GetMid().getY();
  mid_z = proto.GetMid().getZ();

  return proto.GetMid();
} 

void RpcStrip_proto::CheckStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip)
{
  RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(_arm),
		     RPCPROTOGEOM::StationNumber(_station),
		     _octant,_halfoctant,
		     RPCPROTOGEOM::RadialSegment(Rseg),
		     _strip);
  
  if(!proto.checkStrip()){
    ostringstream what;
    what 
      << "RpcStrip_proto::CheckStrip - invalid strip please check: " << endl
      << " " << _arm << ", max is " << RPCPROTOGEOM::NumberOfArms-1 << " " 
      << " " << _station << ", max is " << RPCPROTOGEOM::NumberOfStations-1 << " "
      << " " << _octant << ", max is " << 7 << "(==4 inproto) "
      << " " << _halfoctant << ", max is " << 1 << " "
      << " " << _Rseg << ", max is " << 2 << " "
      << " " << _strip << ", max is " << 63 << " (but some missing!) ";
    RPCPROTOGEOM::TRACE(what.str());
    exit(1);}
  
  return;
}
