#ifndef __RXNP_H__
#define __RXNP_H__

namespace RXNP
{
  // enum and const numbers
  //

  // enum for detector location
  enum LOCATION {SOUTH = 0, NORTH = 1};
  //module verbosity level
  enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};

  // number of arms
  const unsigned short NARM = 2;
  // number of rings
  //
  const unsigned short NRING = 2;
  // packet id
  const int RXNP_PACKID = 23001;
  // number of channels in total
  const unsigned short NCHANNEL_TOTAL = 48;
  // number of channels per arm
  const unsigned short NCHANNEL_PERARM = 24;
  // number of channels per ring
  const unsigned short NCHANNEL_PERRING = 12; 	
  // number of amu cells
  const unsigned short NAMU = 64;


  // Conversion factor from adc to nhits for posthigh - prehigh
  const int HIGHCONV = 100;
  // Conversion factor from adc to nhits for postlow - prelow
  const int LOWCONV = 10;
  // utility functions
  //
  // function return the location of the detector given a channel id
  //
  inline int get_location(int chanid) {
    if(chanid >= NCHANNEL_TOTAL || chanid < 0)
      return -1;
    else
      return chanid%NCHANNEL_PERARM;
  }

}

#endif // end define RXNP
