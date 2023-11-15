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

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <sstream>
#include "PdbMutChamberPar.hh"
using namespace std;

PdbMutChamberPar::PdbMutChamberPar() :
  ArmNum ( -999 ),
  StationNum ( -999 ),
  OctantNum ( -999 ),
  HalfOctantNum ( -999 ),
  GapNum ( -999 ),
  PlaneNum ( -999 ),
  time_offset ( -999 ),
  time_offset_rms ( -999 ),
  pedestal ( 0.0 ),
  Landau_offset ( -999 ),
  Landau_scale ( -999 ),
  Mathieson_cathode_coupling ( -999 ),
  Mathieson_anode_coupling ( -999 ),
  cathode_error ( -999 ),
  chamber_efficiency ( 0.98 )
{}

PdbMutChamberPar::PdbMutChamberPar( const int iarm, const int istation, const int ioctant,
				      const int ihalfoctant, const int igap, const int iplane ) :
  ArmNum ( iarm ),
  StationNum ( istation ),
  OctantNum ( ioctant ),
  HalfOctantNum ( ihalfoctant ),
  GapNum ( igap ),
  PlaneNum ( iplane ),
  time_offset ( -999 ),
  time_offset_rms ( -999 ),
  pedestal ( 0.0 ),
  Landau_offset ( -999 ),
  Landau_scale ( -999 ),
  Mathieson_cathode_coupling ( -999 ),
  Mathieson_anode_coupling ( -999 ),
  cathode_error ( -999 ),
  chamber_efficiency ( 0.98 )
{}

PdbMutChamberPar::~PdbMutChamberPar(){}

//______________________________________________________
PdbMutChamberPar::PdbMutChamberPar( const PdbMutChamberPar& ref )
{
  setArm( ref.getArm() );
  setStation( ref.getStation() );
  setOctant( ref.getOctant() );
  setHalfOctant( ref.getHalfOctant() );
  setGap( ref.getGap() );
  setPlane( ref.getPlane() );
  set_time_offset( ref.get_time_offset() );
  set_time_offset_rms( ref.get_time_offset_rms() );
  set_pedestal ( ref.get_pedestal() );
  set_Landau_offset ( ref.get_Landau_offset() );
  set_Landau_scale( ref.get_Landau_scale() );
  set_Mathieson_cathode_coupling (ref.get_Mathieson_cathode_coupling() );
  set_cathode_error (ref.get_cathode_error() );
  set_chamber_efficiency ( ref.get_chamber_efficiency() );
}

//_________________________________________________________________
PdbMutChamberPar& PdbMutChamberPar::operator = (const PdbMutChamberPar& ref )
{
  setArm( ref.getArm() );
  setStation( ref.getStation() );
  setOctant( ref.getOctant() );
  setHalfOctant( ref.getHalfOctant() );
  setGap( ref.getGap() );
  setPlane( ref.getPlane() );
  set_time_offset( ref.get_time_offset() );
  set_time_offset_rms( ref.get_time_offset_rms() );
  set_pedestal ( ref.get_pedestal() );
  set_Landau_offset ( ref.get_Landau_offset() );
  set_Landau_scale( ref.get_Landau_scale() );
  set_Mathieson_cathode_coupling (ref.get_Mathieson_cathode_coupling() );
  set_cathode_error (ref.get_cathode_error() );
  set_chamber_efficiency ( ref.get_chamber_efficiency() );

  return *this;
}

void
PdbMutChamberPar::print() const
{
  write(cout);
}

//______________________________________________________
void PdbMutChamberPar::write(ostream& os) const
{
  os << setiosflags(ios::fixed)
     << setw(1);
  os <<  ArmNum << " "
     << StationNum << " "
     << OctantNum << " "
     << HalfOctantNum << " "
     << GapNum << " "
     << PlaneNum << " ";
  os << setw(8) << setprecision(3)
     << time_offset << " "
     << time_offset_rms << " "
     << pedestal << " "
     << Landau_offset << " " << Landau_scale << " "
     << Mathieson_cathode_coupling << " "
     << Mathieson_anode_coupling << " ";
  os << cathode_error << " "
     << chamber_efficiency << endl;
}

//______________________________________________________
void PdbMutChamberPar::read(istream& is)
{
  // opposite of write method
  is >> ArmNum
     >> StationNum
     >> OctantNum
     >> HalfOctantNum
     >> GapNum
     >> PlaneNum
     >> time_offset
     >> time_offset_rms
     >> pedestal
     >> Landau_offset >> Landau_scale
     >> Mathieson_cathode_coupling
     >> Mathieson_anode_coupling
     >> cathode_error
     >> chamber_efficiency;
}

