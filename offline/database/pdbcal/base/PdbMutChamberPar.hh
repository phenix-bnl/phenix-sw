//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2013
//
//  Declaration of class PdbMutChamberPar
//
//  Purpose: Store parameters for mMutResponse and Landau parameters
//
//  Description:
//
//  Author: Cesar L. da Silva (slash@bnl.gov)
//-----------------------------------------------------------------------------
#ifndef __PDBMUTCHAMBERPAR_HH__
#define __PDBMUTCHAMBERPAR_HH__

#include <cstdio>
#include "PdbCalChan.hh"

class PdbMutChamberPar : public PdbCalChan {

public:
  PdbMutChamberPar();
  PdbMutChamberPar( const int iarm, const int istation, const int ioctant,
		       const int ihalfoctant, const int igap, const int iplane);
  PdbMutChamberPar( const PdbMutChamberPar& ref );
  PdbMutChamberPar& operator = (const PdbMutChamberPar& ref );
  ~PdbMutChamberPar();

  virtual void print() const;
  virtual void write(ostream& os) const;
  virtual void read(std::istream& is);

  int getArm    ()     const {return ArmNum;} 
  int getStation()     const {return StationNum;}
  int getOctant ()     const {return OctantNum;}
  int getHalfOctant () const {return HalfOctantNum;}
  int getGap ()        const {return GapNum;}
  int getPlane  ()     const {return PlaneNum;}
  float get_time_offset()     const { return time_offset;}
  float get_time_offset_rms() const { return time_offset_rms;}
  float get_pedestal()        const { return pedestal;}
  float get_Landau_offset()   const { return Landau_offset;}
  float get_Landau_scale()    const { return Landau_scale;}
  float get_Mathieson_cathode_coupling() const { return Mathieson_cathode_coupling;}
  float get_Mathieson_anode_coupling()   const { return Mathieson_anode_coupling;}
  float get_cathode_error()              const { return cathode_error;}
  float get_chamber_efficiency()         const { return chamber_efficiency;}
  //! chamber identifier 
  static int get_unique_ID(int iarm, int istation, int ioctant, int ihalfoc, int igap, int iplane)
  {
    return iplane + igap*2 + ihalfoc*2*3 + ioctant*2*3*2 + istation*2*3*2*8 + iarm*2*3*2*8*3;
  }

  //! return chamber id for this chamber
  int   get_unique_ID() const 
  {
    return get_unique_ID( ArmNum, StationNum, OctantNum, HalfOctantNum, GapNum, PlaneNum );
  }

  
  void setArm        (int temp){ ArmNum=temp;}
  void setStation    (int temp){ StationNum=temp;}
  void setOctant     (int temp){ OctantNum=temp;}
  void setHalfOctant (int temp){ HalfOctantNum=temp;}
  void setGap        (int temp){ GapNum=temp;}
  void setPlane      (int temp){ PlaneNum=temp;}
  void set_time_offset     ( float a ) { time_offset=a; }
  void set_time_offset_rms ( float a ) { time_offset_rms=a; }
  void set_pedestal        ( float a ) { pedestal=a; }
  void set_Landau_offset   ( float a ) { Landau_offset=a; }
  void set_Landau_scale    ( float a ) { Landau_scale=a; }
  void set_Mathieson_cathode_coupling ( float a ) { Mathieson_cathode_coupling=a; }
  void set_Mathieson_anode_coupling   ( float a ) { Mathieson_anode_coupling=a; }
  void set_cathode_error              ( float a ) { cathode_error=a; }
  void set_chamber_efficiency         ( float a ) { chamber_efficiency=a; }

private:
  int ArmNum; 
  int StationNum;
  int OctantNum;
  int HalfOctantNum;
  int GapNum;
  int PlaneNum;
  float time_offset;
  float time_offset_rms;
  float pedestal;
  float Landau_offset;
  float Landau_scale;
  float Mathieson_cathode_coupling;
  float Mathieson_anode_coupling;
  float cathode_error;
  float chamber_efficiency;

  ClassDef(PdbMutChamberPar,1);
};

#endif /* __PDBMUTCHAMBERPAR_HH__ */
