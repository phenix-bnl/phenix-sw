// $Id: RPCGEOM.cxx,v 1.5 2014/11/05 23:53:00 pinkenbu Exp $

/*!
  \file RPCGEOM.cxx
  \brief widely used utility functions and enumerations
  \author H. Pereira Da Costa
  \version $Revision: 1.5 $
  \date $Date: 2014/11/05 23:53:00 $
*/
#include "RPCGEOM.h"
#include <cmath>

ClassImp( RPCGEOM )

const Float_t RPCGEOM::RAD_TO_DEG = 180.0/M_PI;
const Float_t RPCGEOM::DEG_TO_RAD = M_PI/180.0;
const int RPCGEOM::OUTOFRANGE = -999;

//z-positions of stations
const double RPCGEOM::station_z[NumberOfArms][NumberOfStations] = 
  {{-148.73,-683.9,-906.3},               //South
   {148.73,683.9,906.3}};                 //North

//Radial Segmentation--North and South are taken to be symmetric.
const double RPCGEOM::R_bound[NumberOfStations][NumberofRBoundsSt1] =
  {{32.43,47.79,64.14,81.91,101.68},       //RPC1
   {149.11,219.77,294.94,376.65,467.54},   //RPC2
   {197.61,291.23,390.86,499.14,RPCGEOM::OUTOFRANGE}};    //RPC3

//strip widths
const double RPCGEOM::strip_size[NumberOfStations][NumberofRBoundsSt1-1] = 
  {{1.24,1.66,1.06,1.32},                  //RPC1
   {2.84,3.82,4.88,6.05},                  //RPC2
   {3.77,5.06,6.46,RPCGEOM::OUTOFRANGE}};                 //RPC3

//_______________________________________________
void RPCGEOM::PRINT(std::ostream& os, const std::string& message){
  const int max_col=80;
  if(!message.size()) {
    os << std::string(max_col,'-') << std::endl;
    return;
  }
  int fill = max_col - message.size() - 2;
  int pre = static_cast<int>(std::floor(fill/2.0));
  int post = fill - pre;
  os << std::string(pre,'-') << " ";
  os << message << " ";
  os << std::string(post,'-') << std::endl;	
}
