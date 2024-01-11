//describe the Acceptance of the MpcEx
//and MPC
//can decide wether a hit or shower is in 
//MpcEx or Mpc
//Liankun @03/07/2017

#ifndef __MPCEXACPT_H__
#define __MPCEXACPT_H__
#include <stddef.h>

class MpcExAcpt{
  public:
    virtual ~MpcExAcpt(){}
    static MpcExAcpt* instance();

  //get shortest distance from the mpcex edge to (x,y),alwalys > 0
  //in cm
  virtual double get_shortest_d(int arm,int layer,double x,double y); 
  //wehter the hit is on MpcEx 
  //for different layers
  virtual bool is_on_mpcex(int arm,int layer,double x,double y);

  virtual bool is_track_in_mpcex(int arm,double vertex,double hsx,double hsy);
  
  //wehter a shower is in MPCEX, with the shower radius hsrms in hough space
  virtual bool is_shower_in_mpcex(int arm,double vertex,double hsx,double hsy,double hsrms);
  
  virtual bool is_track_in_mpc(int arm,double vertex,double hsx,double hsy);
  //wether a shower is in MPC, with the shower radius hsrms in hough space
  virtual bool is_shower_in_mpc(int arm,double vertex,double hsx,double hsy,double hsrms);
  virtual bool is_on_mpc(int arm,double x,double y);
  
  //get shortest distance from the mpc edge to (x,y),alwalys > 0
  //in cm
  virtual double get_shortest_d_mpc(int arm,double x,double y);

  protected:
    MpcExAcpt();
    static MpcExAcpt* _instance;
    double _rmax[2][8][1080];
    double _rmin[2][8][1080];
    double _mpc_rmax[2][1080];
    double _mpc_rmin[2][1080];
    double _layer_dst[2][8];
    double _dphi;
    double _mpc_front_dst[2];
};

#endif //__MPCEXACPT_H__
