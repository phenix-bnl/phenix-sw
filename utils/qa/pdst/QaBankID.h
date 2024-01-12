#ifndef QABANKID_H
#define QABANKID_H

//
//  Hello all
//      Although the BankID and Description field of
//  PdbCal objects are usually the private business 
//  of a subsystem, there is a small restriction placed
//  upon the many users of the qasummary database.  Our
//  design requires that the data is accessible as a 
//  "strip chart" via a web page, we need to ensure that 
//  all of us use unique BankId values.
//
//      This header file serves to provide an enumeration
//  of the actual BankID values.  Anyone adding an additional
//  bank *MUST* edit this file and add their enumeration to
//  the full list.
//
//                           7-31-2004
//

namespace QaBankID
{
  
  enum QABANKID 
    {
      BBC      =    1 ,
      DCH      =    2 ,
      EmCal    =    3 ,
      Tec      =    4 ,
      Crk      =    5 ,
      Pad      =    6 ,
      Zdc      =    7 ,
      Tof      =    8 ,
      FCal     =    9 ,
      MuTr     =   10 ,
      MuID     =   11 ,
      Mvd      =   12 ,
      Ntcp     =   13 ,
      Electron =   14 ,
      PhysicsQA=   15,
      MPC      =   16
    };
  
}

#endif
