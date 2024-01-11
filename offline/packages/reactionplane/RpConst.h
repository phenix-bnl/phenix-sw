
#ifndef __RPCONST_H__
#define __RPCONST_H__

namespace RP {

  static const int NDET = 17;
  static const int NHAR = 2;
  static const int NMUL = 20;
  static const int NZPS = 3;
  static const int NORD = 20;

  static const int NDET2 = 29;
  static const int NHAR2 = 2;
  static const int NMUL2 = 20;
  static const int NZPS2 = 12;
  static const int NORD2 = 20;

  static const int NDET3 = 27;
  static const int NDET3_1 = 88;
  static const int NDET3_2 = 39;
  static const int NDET3_3 = 69;
  static const int MDET  = 62;
  static const int NHAR3 =  6;
  static const int NMUL3 = 20;//5  //20;
  static const int NZPS3 = 10;//20;
  static const int NORD3 =  8;

  static const int NHAR4 = 4;

  // This is Run8 d+Au specific
  static const int NDET5 = 12;
  static const int NHAR5 = 2;
  static const int NMUL5 = 4;
  static const int NZPS5 = 12;
  static const int NORD5 = 8;

  static const int ID_BBC =  0;
  static const int ID_SMD =  3;
  static const int ID_MVD =  6;
  static const int ID_FCL =  9;
  static const int ID_CNT = 12;
  static const int ID_RXN = 17;
  static const int ID_MPC = 26;
  static const int ID_SVX = 29;
  static const int ID_SEG = 72;

  //FVTX
  static const int NFVT = 40; // 40 Qvector with 0.5 rapidity from 1.0~3.5 for each disc
  static const int ID_FVT = 91;//svx 62+29

  static const int SCHEMA_V3 = 3;

  int  calcIdCode(const int det, const int kind, const int nhar);
  void reverseIdCode(const int idcode, int& det, int& kind, int& nhar);
};

#endif
