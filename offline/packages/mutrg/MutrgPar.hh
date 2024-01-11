#ifndef __MUTRGPAR__
#define __MUTRGPAR__

namespace MutrgPar{
  const int NARM=2;
  const int ARM_SOUTH=0;
  const int ARM_NORTH=1;

  const int NSTATION=3;
  const int NOCTANT=8;
  const int NHALFOCTANT=2;

  const int MAX_NGAP=3;
  const int NGAP[NSTATION]={3,3,2};

  const int NCATHODE=2;

  const int MAX_NSTRIP_IN_OCTANT=320; // north, st=2
  //const int MAX_NSTRIP[NARM][NSTATION]={96,192,320};

  // temporary number for South
  // plane=1 is anode plane.
  //const int INSTALL_PLANE[NARM][NSTATION]={{2,0,2},{2,0,2}};
  const int INSTALL_PLANE[NSTATION]={2,0,2};

  // temporary number for South
  //const int INSTALL_CATHODE[NARM][NSTATION]={{1,0,1},{1,0,1}};
  const int INSTALL_CATHODE[NSTATION]={1,0,1};

  const bool IS_INSTALL_GAP[NSTATION][MAX_NGAP]={
    {true,true,true},{true,false,true},{true,true,false}};

  int NSTRIP_IN_OCTANT(int arm,int station,int octant);
  int NSTRIP_IN_OCTANT(int arm,int station,int octant,int gap,int cathode);

  int NSTRIP_IN_HALFOCTANT(int arm,int station,int octant,int halfoctant);

  extern bool IS_READ_NSTRIP_DB;
  int NSTRIP_IN_HALFOCTANT(int arm,int station,int octant,int halfoctant,
			   int gap,int cathode);
  int NSTRIP_IN_HALFOCTANT_DB(int arm,int station,int octant,int halfoctant,
			      int gap,int cathode);
  int NSTRIP_IN_HALFOCTANT_HC(int arm,int station,int octant,int halfoctant,
			      int gap,int cathode);

  const int NPACKET_ID=8;
  const int PACKET_ID[NPACKET_ID]={
    11510,11511,11512,11513,11500,11501,11502,11503};

  const int NOCTANT_IN_PACKET=2;
  const int MAX_NHITCLOCK=7;
  const unsigned short MAX_NHITCLOCK_MASK=0x7f;

  enum TrkDBVersion{
    TRKDV_UNKNOWN             =-1,
    TRKDV_RUN_MUTRG_RPC_0     =1000, // used during run (setting 0)
    TRKDV_V3_MUTRG_SG0        =2000, // MuTRG only (sg0)
    TRKDV_V3_MUTRG_SG1        =2001, // MuTRG only (sg1)
    TRKDV_V3_MUTRG_SG2        =2002, // MuTRG only (sg2)
    TRKDV_V3_RPC_CL0          =3000, // RPC only
    TRKDV_V3_MUTRG_SG1_RPC_CL0=4001  // MuTRG&RPC
  };
};

#endif /* __MUTRGPAR__ */
