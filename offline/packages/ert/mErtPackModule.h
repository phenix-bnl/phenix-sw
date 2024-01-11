///////////////////////////////////////////////////////////////
//  pack FEM data into DCM data  
//
//   problem report to: xiewei@rcf2.rhic.bnl.gov 
//

#ifndef __mErtPackModule_H__
#define __mErtPackModule_H__
#include "phool.h"
#include "PHCompositeNode.h"

#define nPacket 2             //..  east arm: 14201, west arm: 14200
#define nWordPerPacket  135

class mErtPackModule
{

  private:

    int  wordValue[nPacket][nWordPerPacket];
    int  packetID[nPacket];

  public:

    mErtPackModule() {packetID[0] = 14200; packetID[1] = 14201;}
    virtual ~mErtPackModule(){}

    PHBoolean event(PHCompositeNode*);

};
#endif /*__mErtPackModule_H__*/
