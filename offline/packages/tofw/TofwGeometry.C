//-------------------------------------------------------------------
// Implementation of class TofwGeometry
// Author : Shengli Huang (Vanderbilt University)   
//-------------------------------------------------------------------

#include "TofwGeometry.h"
#include "PHGeometry.h"
#include <iostream>
#include <fstream>

using namespace std;

//_____________________________________________________________________________
TofwGeometry::TofwGeometry()
{
  //------------TofwGeometry defalt constructor------------
  //Four Boxes
  //box 0 BS TS BN
  PHPoint box0_bs(tofw_radius, tofw_corner_y_0, -1.0*tofw_z_width);
  PHPoint box0_ts(tofw_radius, tofw_corner_y_1, -1.0*tofw_z_width);
  PHPoint box0_bn(tofw_radius, tofw_corner_y_0, 0);

  //box 1
  PHPoint box1_bs(tofw_radius, tofw_corner_y_0, 0.);
  PHPoint box1_ts(tofw_radius, tofw_corner_y_1, 0.);
  PHPoint box1_bn(tofw_radius, tofw_corner_y_0, tofw_z_width);

  //box2
  PHPoint box2_bs(tofw_corner_x_0, tofw_corner_y_2, -1.0*tofw_z_width);
  PHPoint box2_ts(tofw_corner_x_1, tofw_corner_y_3, -1.0*tofw_z_width);
  PHPoint box2_bn(tofw_corner_x_0, tofw_corner_y_2, 0.);

  //box3
  PHPoint box3_bs(tofw_corner_x_0, tofw_corner_y_2, 0.);
  PHPoint box3_ts(tofw_corner_x_1, tofw_corner_y_3, 0.);
  PHPoint box3_bn(tofw_corner_x_0, tofw_corner_y_2, tofw_z_width);

  BoxPanel[0] = PHPanel(box0_bs, box0_ts, box0_bn);
  BoxPanel[1] = PHPanel(box1_bs, box1_ts, box1_bn);
  BoxPanel[2] = PHPanel(box2_bs, box2_ts, box2_bn);
  BoxPanel[3] = PHPanel(box3_bs, box3_ts, box3_bn);

  isGeometryOK = -1;
  
  debug = 0;
}

//_____________________________________________________________________________
// Get Box Geometry
PHPanel TofwGeometry::getBoxGeo(int ibox)
{
  return BoxPanel[ibox];
}

PHPoint TofwGeometry::getStripCenter(int ibox, int ichamber, int istrip)
{
  float x= -9999;
  float y= pad_center_y[ibox];
  float z= -9999;
  
  //x
  if(ibox==0||ibox==1){
    x=pad_center_x[ibox];
  }else if(ibox>1&&ichamber<16){
    x=pad_center_x[2]-1.09;
  }else if(ibox>1&&ichamber>=16){
    x=pad_center_x[3]-1.09;
  }

  //y
  if(ibox==0||ibox==1){
    if(ichamber<16) y= pad_center_y[0]+2.46;
    else if(ichamber>=16) y= pad_center_y[1]+2.46;
  }else if(ibox==2||ibox==3){
    if(ichamber<16) y= pad_center_y[2]+2.63;
    else if(ichamber>=16) y= pad_center_y[3]+2.63;
  }

  //z
  if(ibox==0||ibox==2) {
    z = -1.0*pad_center_z[ichamber%16]; 
  }else{
    z = pad_center_z[15-ichamber%16];
  }
  if(istrip==0) z= z - 3.0*strip_gap_width/2.0;
  if(istrip==1) z= z - 1.0*strip_gap_width/2.0;
  if(istrip==2) z= z + 1.0*strip_gap_width/2.0;
  if(istrip==3) z= z + 3.0*strip_gap_width/2.0;

  z = z + 2.70;// global adjust 2.7 cm 
  PHPoint center_point(x,y,z);
  return center_point;

}

PHPoint TofwGeometry::getSimStripCenter(int ibox, int ichamber, int istrip)
{
  float x= -9999;
  float y= pad_center_y[ibox];
  float z= -9999;
  
  //x
  if(ibox==0||ibox==1){
    x=pad_center_x[ibox];
  }else if(ibox>1&&ichamber<16){
    x=pad_center_x[2];
  }else if(ibox>1&&ichamber>=16){
    x=pad_center_x[3];
  }

  //y
  if(ibox==0||ibox==1){
    if(ichamber<16) y= pad_center_y[0];
    else if(ichamber>=16) y= pad_center_y[1];
  }else if(ibox==2||ibox==3){
    if(ichamber<16) y= pad_center_y[2];
    else if(ichamber>=16) y= pad_center_y[3];
 
    y = y + 2.1;
  }

  //z
  if(ibox==0||ibox==2) {
    z = -1.0*pad_center_z[ichamber%16]; 
  }else{
    z = pad_center_z[15-ichamber%16];
  }
  if(istrip==0) z= z - 3.0*strip_gap_width/2.0;
  if(istrip==1) z= z - 1.0*strip_gap_width/2.0;
  if(istrip==2) z= z + 1.0*strip_gap_width/2.0;
  if(istrip==3) z= z + 3.0*strip_gap_width/2.0;

  //z = z + 2.70;// global adjust 2.7 cm 
  PHPoint center_point(x,y,z);
  return center_point;

}
