#ifndef _RpcDBInfo_h_
#define _RpcDBInfo_h_

/*!
	\file RpcDBInfo.h
	\brief provides an interface to the database/text file
	\author Richard Hollis
*/

#include<string>
#include<iostream>
#include<TObject.h>
#include <RPCFULLGEOM.h>

class RpcStrip;

//! widely used utility functions and enumerations
class RpcDBInfo : public TObject
{
 protected:
  static bool instantiated;
  static RpcDBInfo *dbinstance;

 public:
  RpcDBInfo(); // default constructor
  static RpcDBInfo *getInstance();
  
  void readFile(const std::string& fFile);
  void readDB( const int run );
  bool isDead(RpcStrip *strip);
  double getNoise(RpcStrip *strip);
  void reset();
  void killall();
  void flatnoise(double rate);
  
	//- Dead Channel Map
        int DeadChannel[RPCFULLGEOM::NumberOfArms][RPCFULLGEOM::NumberOfStations][8][2][3][64];
	//- Noise Rates (PER EVENT, not per event per cm2)
	double NoiseInChannel[RPCFULLGEOM::NumberOfArms][RPCFULLGEOM::NumberOfStations][8][2][3][64];

	ClassDef( RpcDBInfo,1 );
	
};
#endif
