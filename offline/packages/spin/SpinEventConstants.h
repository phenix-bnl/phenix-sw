#ifndef __SPINEVENTCONSTANTS_H__
#define __SPINEVENTCONSTANTS_H__

  //                                packet id in DATAEVENT
  const int GL1_packet_number     = 14001;  
  const int GL1P1_packet_number   = 14008;
  const int GL1P2_packet_number   = 14009;
  const int GL1PSum_packet_number = 14011;

  //                                          packet id in SCALEREVENT
  const int Scaler_packet_number            = 900;  //
  const int RingSpecBlue_packet_number      = 804;  // from CDEV, Spin Fill patter for Blue ring 
  const int RingSpecYellow_packet_number    = 803;  // from CDEV, Spin Fill patter for Yellow ring
  const int BeamCurrentBlue_packet_number   = 805;  // from CDEV, Wall current for Blue ring
  const int BeamCurrentYellow_packet_number = 806;  // from CDEV, Wall current for Yellow ring
  const int PolMetBlue_packet_number        = 808;  // from CDEV, CNI Polarimeter for Blue ring
  const int PolMetYellow_packet_number      = 809;  // from CDEV, CNI Polarimeter for Yellow ring

  // the other constatns 
  const int nCrossing           = 120;  // number of beam crossing 
  const int shiftCrossing       =   5;  // an expected constant to adjust RHIC crossing of blue beam as 0 at PHENIX IR
  const int shiftYellowCrossing =  40;  // relative shift value of yellow to blue

  const int nGL1PBoard = 2;
  const int nGL1PScaler = 4;

  // Granule disable  for V124
  //                                                                 3322 2222 2222 1111 1111 11
  //                                                                 1098 7654 3210 9876 5432 1098 7654 3210  // From afternoon or evening of Apr. 9
  static const unsigned int gdisablebitsYellowUp    = 0x00400000; // 0000 0000 0100 0000 0000 0000 0000 0000
  static const unsigned int gdisablebitsBlueUp      = 0x00800000; // 0000 0000 1000 0000 0000 0000 0000 0000
  static const unsigned int gdisablebitsYellowDown  = 0x01000000; // 0000 0001 0000 0000 0000 0000 0000 0000
  static const unsigned int gdisablebitsBlueDown    = 0x02000000; // 0000 0010 0000 0000 0000 0000 0000 0000
  static const unsigned int gdisablebitsYellowUnpol = 0x04000000; // 0000 0100 0000 0000 0000 0000 0000 0000
  static const unsigned int gdisablebitsBlueUnpol   = 0x08000000; // 0000 1000 0000 0000 0000 0000 0000 0000

  static const unsigned int gdisablebitsBlue        = 0x0a800000; // 0000 1010 1000 0000 0000 0000 0000 0000
  static const unsigned int gdisablebitsYellow      = 0x05400000; // 0000 0101 0100 0000 0000 0000 0000 0000

  

#endif
