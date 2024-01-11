
#include "RPCGEOM.h"
#include "RpcGeom.h"
#include "RpcStrip.h"
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <TMath.h>
#include <TLine.h>

using namespace std;

const void RpcStrip::InitVal()
{
  arm = station = octant = halfoctant = Rseg = strip = (int)RPCGEOM::OUTOFRANGE;

  double oor = (double)RPCGEOM::OUTOFRANGE;
  begin_x = oor; begin_y = oor; begin_z = oor;
  end_x = oor; end_y = oor; end_z = oor;
  mid_x = oor; mid_y = oor; mid_z = oor;

}

int RpcStrip::IsEmpty() const
{
  int oor_int = (int)RPCGEOM::OUTOFRANGE;
  if ( arm <= oor_int || station <= oor_int || octant <= oor_int
       || Rseg <= oor_int || strip <= oor_int ){
    return 1; 

    /*coverity
      cout << "RpcStrip:: strip is empty" << endl;
      print();
    */
  }
  return 0;
}

void RpcStrip::SetStrip(double x, double y, int arm_index, int station_index, int R_segmentation)
{
  int _arm,_station, _octant, _halfoctant, _Rseg, _strip;
  _Rseg = _strip = (int)RPCGEOM::OUTOFRANGE;

  double hit_x = x;
  double hit_y = y;

  _arm = arm_index;
  _station = station_index;

  // Need to convert positions to local RPC coords.

  float phi = atan2(hit_y,hit_x)*RPCGEOM::RAD_TO_DEG; 
  if(phi>=360.0) phi-=360.0; 
  if(phi<0.0) phi+=360.0;  

  //get octant and half-octant

  _octant = int((phi+22.5)/45.);
  if(phi>(360-22.5)) _octant=0;

  _halfoctant = int(((phi+22.5)/22.5)-2.*_octant);
  if(phi>(360-22.5)) _halfoctant=0;

  int strip_phi_dir=(int)RPCGEOM::OUTOFRANGE;
  switch(_halfoctant)
    {
    case 0:
      strip_phi_dir = -1;
      break;
    case 1:
      strip_phi_dir = 1;
      break;
    }

  float octant_center_phi = (_octant*45.)*RPCGEOM::DEG_TO_RAD; 
  float strip_center_phi = (_octant*45.+strip_phi_dir*11.25)*RPCGEOM::DEG_TO_RAD;
  
  double x_strip_loc = hit_y*cos(strip_center_phi) - hit_x*sin(strip_center_phi);
  //THIS IS REMOVED 8/DEC?2014 to make the scan build happy ...
  /*
    double y_strip_loc = hit_x*cos(strip_center_phi) + hit_y*sin(strip_center_phi);
    if(y_strip_loc<0) y_strip_loc=-y_strip_loc;
  */

  //  double x_octant_loc = hit_y*cos(octant_center_phi) - hit_x*sin(octant_center_phi);
  double y_octant_loc = hit_x*cos(octant_center_phi) + hit_y*sin(octant_center_phi);
  if(y_octant_loc<0) y_octant_loc=-y_octant_loc;

  //get Rseg
  for(Int_t i=0;i<R_segmentation-1;i++){
    Int_t j=i+1;
    if((y_octant_loc>=RPCGEOM::R_bound[_station][i])&&(y_octant_loc<RPCGEOM::R_bound[_station][j])) _Rseg=i;
  }

  //get stripnum.  Strip numbers count up from straight edge of half octant, i.e. out from the center of each octant

  int strip_from_center = -999;
  int index_shift = -999;

  if((_Rseg<R_segmentation-1)&&(_Rseg>=0)){

    //get strip num from x strip loc

    //count up from 1 for outer half
    if(x_strip_loc>0){
      strip_from_center = (int)(x_strip_loc/GetStripWidth(_station,_Rseg));
      if(_halfoctant==0) index_shift = -1;
      if(_halfoctant==1) index_shift = 0;      
    }

    //count down from -1 for inner half
    if(x_strip_loc<=0){
      strip_from_center = (int)(x_strip_loc/GetStripWidth(_station,_Rseg));
      if(_halfoctant==0) index_shift = 0;
      if(_halfoctant==1) index_shift = -1;   
    }

    _strip =  GetNumStripsInner(_arm,_station,_octant,_halfoctant,_Rseg)+strip_phi_dir*(strip_from_center)+index_shift;
  }

  SetStrip(_arm,_station,_octant,_halfoctant,_Rseg,_strip);

  return;
}

void RpcStrip::SetStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip){

  //Not Checking right now since PISA geometry is bigger than actual geometry
  // CheckStrip(_arm,_station,_octant,_halfoctant,_Rseg,_strip);
  
  arm = _arm;
  station = _station;
  octant = _octant; halfoctant = _halfoctant;
  Rseg = _Rseg;
  strip = _strip;
  
  return;
}

PHPoint RpcStrip::GetBegin()
{
  CheckStrip(arm,station,octant,halfoctant,Rseg,strip);

  begin_z = RPCGEOM::station_z[arm][station];
  
  //Need to get local coords, then convert to global
  double y_octant_loc = RPCGEOM::R_bound[station][Rseg];

  //take care of zeroth strip and last strips

  //find number of strips in one half first 
  double strip_divs_1half = RPCGEOM::R_bound[station][Rseg+1]*tan(11.25*RPCGEOM::DEG_TO_RAD)*cos(11.25*RPCGEOM::DEG_TO_RAD)*(1./GetStripWidth(station,Rseg));

  //then the other
  double strip_divs_2half = RPCGEOM::R_bound[station][Rseg+1]*(tan(22.5*RPCGEOM::DEG_TO_RAD)-tan(11.25*RPCGEOM::DEG_TO_RAD))*cos(11.25*RPCGEOM::DEG_TO_RAD)*(1./GetStripWidth(station,Rseg));

  double first_strip_width = (strip_divs_1half - (int)strip_divs_1half)*GetStripWidth(station,Rseg);
  double last_strip_width = (strip_divs_2half - (int)strip_divs_2half)*GetStripWidth(station,Rseg); 

  int max_strip = GetNumStrips(arm,station,octant,halfoctant,Rseg);
  double x_octant_loc=RPCGEOM::OUTOFRANGE;
  double x_shift=(RPCGEOM::R_bound[station][Rseg+1]-RPCGEOM::R_bound[station][Rseg])*tan(11.25*RPCGEOM::DEG_TO_RAD);
  if(x_shift<0) x_shift=-x_shift;

  //first get x at strip end
  if(strip==0) x_octant_loc=0.5*first_strip_width;
  if(strip==max_strip-1)  x_octant_loc=(max_strip-2)*GetStripWidth(station,Rseg)+first_strip_width+0.5*last_strip_width;
  if((strip!=0)&&(strip!=(max_strip-1))) x_octant_loc=(strip-1+0.5)*GetStripWidth(station,Rseg)+first_strip_width;

  //now convert to x at strip begin
  x_octant_loc = x_octant_loc-x_shift;
  if(x_octant_loc<0){
    y_octant_loc=RPCGEOM::R_bound[station][Rseg+1]-((x_octant_loc+x_shift)/tan(11.25*RPCGEOM::DEG_TO_RAD));
    x_octant_loc=0;
  }

  //Need to take care of angled edges
  if(x_octant_loc>y_octant_loc*tan(22.5*RPCGEOM::DEG_TO_RAD)) y_octant_loc = x_octant_loc*(1./tan(22.5*RPCGEOM::DEG_TO_RAD));

  if(halfoctant==0) x_octant_loc=-x_octant_loc;

  float octant_center_phi = (octant*45.)*RPCGEOM::DEG_TO_RAD;

  double x = y_octant_loc*cos(octant_center_phi) - x_octant_loc*sin(octant_center_phi);
  double y = y_octant_loc*sin(octant_center_phi) + x_octant_loc*cos(octant_center_phi); 

  begin_x = x;
  begin_y = y;
  
  PHPoint begin(begin_x,begin_y,begin_z);
  
  return begin;
}

PHPoint RpcStrip::GetEnd()
{
  CheckStrip(arm,station,octant,halfoctant,Rseg,strip);

  end_z = RPCGEOM::station_z[arm][station];
 
  //Need to get local coords, then convert to global
  float y_octant_loc = RPCGEOM::R_bound[station][Rseg+1];

  //take care of zeroth strip

  //find number of strips in one half first 
  double strip_divs_1half = RPCGEOM::R_bound[station][Rseg+1]*tan(11.25*RPCGEOM::DEG_TO_RAD)*cos(11.25*RPCGEOM::DEG_TO_RAD)*(1./GetStripWidth(station,Rseg));

  //then the other
  double strip_divs_2half = RPCGEOM::R_bound[station][Rseg+1]*(tan(22.5*RPCGEOM::DEG_TO_RAD)-tan(11.25*RPCGEOM::DEG_TO_RAD))*cos(11.25*RPCGEOM::DEG_TO_RAD)*(1./GetStripWidth(station,Rseg));

  double first_strip_width = (strip_divs_1half - (int)strip_divs_1half)*GetStripWidth(station,Rseg);
  double last_strip_width = (strip_divs_2half - (int)strip_divs_2half)*GetStripWidth(station,Rseg); 

  int max_strip = GetNumStrips(arm,station,octant,halfoctant,Rseg);
  double x_octant_loc=RPCGEOM::OUTOFRANGE;

  //first get x at strip midline
  if(strip==0) x_octant_loc=0.5*first_strip_width;
  if(strip==max_strip-1)  x_octant_loc=(max_strip-2)*GetStripWidth(station,Rseg)+first_strip_width+0.5*last_strip_width;
  if((strip!=0)&&(strip!=(max_strip-1))) x_octant_loc=(strip-1+0.5)*GetStripWidth(station,Rseg)+first_strip_width;

  
  float octant_center_phi = (octant*45.)*RPCGEOM::DEG_TO_RAD; 
 
  if(halfoctant==0) x_octant_loc=-x_octant_loc;

  double x = y_octant_loc*cos(octant_center_phi) - x_octant_loc*sin(octant_center_phi);
  double y = y_octant_loc*sin(octant_center_phi) + x_octant_loc*cos(octant_center_phi); 

  end_x = x;
  end_y = y;

  PHPoint end(end_x,end_y,end_z);
  return end;
}

PHPoint RpcStrip::GetMid()
{
  mid_z = RPCGEOM::station_z[arm][station];

  PHPoint beg = GetBegin();
  PHPoint end = GetEnd();

  double x = 0.5*(beg.getX()+end.getX());
  double y = 0.5*(beg.getY()+end.getY());

  mid_x = x;
  mid_y = y;

  PHPoint mid(mid_x,mid_y,mid_z);
  return mid;
} 

int RpcStrip::GetNumStripsInner(int _arm,int _station,int _octant,int _halfoctant,int _Rseg)
{
  //Find number of strips in inner part of octant 
  double strip_divs_1half = RPCGEOM::R_bound[_station][_Rseg+1]*tan(11.25*RPCGEOM::DEG_TO_RAD)*cos(11.25*RPCGEOM::DEG_TO_RAD)*(1./GetStripWidth(_station,_Rseg));

  int num_strips_1half = (int)strip_divs_1half;

  //here we instrument partial strips.  If this changes, it may cause unintended damage to entire RpcStrip object
  if(strip_divs_1half>num_strips_1half) num_strips_1half = num_strips_1half+1;

  return num_strips_1half;
}

int RpcStrip::GetNumStripsOuter(int _arm,int _station,int _octant,int _halfoctant,int _Rseg)
{
  //Find number of strips in outer part of octant 
  double strip_divs_2half = RPCGEOM::R_bound[_station][_Rseg+1]*(tan(22.5*RPCGEOM::DEG_TO_RAD)-tan(11.25*RPCGEOM::DEG_TO_RAD))*cos(11.25*RPCGEOM::DEG_TO_RAD)*(1./GetStripWidth(_station,_Rseg));

  int num_strips_2half = (int)strip_divs_2half;

  //here we instrument partial strips.  If this changes, it may cause unintended damage to entire RpcStrip object
  if(strip_divs_2half>num_strips_2half) num_strips_2half = num_strips_2half+1;

  return num_strips_2half;
}

int RpcStrip::GetNumStrips(int _arm,int _station,int _octant,int _halfoctant,int _Rseg)
{

  int num_strips_1half = GetNumStripsInner(_arm,_station,_octant,_halfoctant,_Rseg);
  int num_strips_2half = GetNumStripsOuter(_arm,_station,_octant,_halfoctant,_Rseg);  

  int num_strips = num_strips_1half+num_strips_2half;

  return num_strips;
} 

void RpcStrip::CheckStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip)
{
  
  int max_arms =  RPCGEOM::NumberOfArms;
  if((_arm>max_arms-1)||(_arm<0)){
    ostringstream what;
    what 
      << "RpcStrip::CheckStrip - invalid arm index: got " << _arm
      << ", max is " << RPCGEOM::NumberOfArms;
    RPCGEOM::TRACE(what.str());
    exit(1);
  }
  int max_stations =  RPCGEOM::NumberOfStations;
  if((_station>max_stations-1)||(_station<0)){
    ostringstream what;
    what 
      << "RpcStrip::CheckStrip - invalid station index: got " << _station
      << ", max is " << RPCGEOM::NumberOfStations;
    RPCGEOM::TRACE(what.str());
    exit(1);
  }
  if((_octant>7)||(_octant<0)){
    ostringstream what;
    what 
      << "RpcStrip::CheckStrip - invalid octant index: got " << _octant 
      << ", max is 7";
    RPCGEOM::TRACE(what.str());
    exit(1);
  }
  if((_halfoctant>1)||(_halfoctant<0)){
    ostringstream what;
    what 
      << "RpcStrip::CheckStrip - invalid half octant index: got " << _halfoctant 
      << ", max is 1";
    RPCGEOM::TRACE(what.str());
    exit(1);
  }
  int max_seg=0;
  switch(_station)
    {
    case RPCGEOM::Station1 :
      max_seg = RPCGEOM::NumberofRBoundsSt1-1;
      break;
    case RPCGEOM::Station2 :
      max_seg = RPCGEOM::NumberofRBoundsSt2-1;
      break;
    case RPCGEOM::Station3 :
      max_seg = RPCGEOM::NumberofRBoundsSt3-1;
      break;
    }
  
  if((_Rseg>max_seg)||(_Rseg<0)){
    ostringstream what;
    what 
      << "RpcStrip::CheckStrip - invalid R segment: got " << _Rseg 
      << ", max is " << max_seg;
    RPCGEOM::TRACE(what.str());
    exit(1);
  } 
 
  int max_strips = GetNumStrips(_arm,_station,_octant,_halfoctant,_Rseg);
  if((_strip>max_strips)||(_strip<0)){
    ostringstream what;
    what 
      << "RpcStrip::CheckStrip - invalid strip index: got " << _strip 
      << ", max is " << max_strips;
    RPCGEOM::TRACE(what.str());
    exit(1);
  } 

 
  return;
}
