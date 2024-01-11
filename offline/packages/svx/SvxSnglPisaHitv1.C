// $Id: SvxSnglPisaHitv1.C,v 1.2 2011/02/08 23:47:27 youzy Exp $

/*!
  \file SvxSnglPisaHitv1.C
  \brief Forward vertex phool interface to pisa hit
  \author Sasha Lebedev (lebedev@iastate.edu)
  \version $Revision: 1.2 $
  \date $Date: 2011/02/08 23:47:27 $
*/

#include "SvxSnglPisaHitv1.h"
ClassImp(SvxSnglPisaHitv1)

int SvxSnglPisaHitv1::SvxCount = 0;

//________________________________________________________
SvxSnglPisaHitv1::SvxSnglPisaHitv1()
{

  mctrack = -9999;
  idPart = -9999;
  xyzglobal[0] = -9999.;
  xyzglobal[1] = -9999.;
  xyzglobal[2] = -9999.;
  dele = -9999.;
  pmomxyz[0] = -9999.;
  pmomxyz[1] = -9999.;
  pmomxyz[2] = -9999.;
  timeOfFlight = -9999.;

  xyzlocalIn[0] = -9999.;
  xyzlocalIn[1] = -9999.;
  xyzlocalIn[2] = -9999.;
  xyzlocalOut[0] = -9999.;
  xyzlocalOut[1] = -9999.;
  xyzlocalOut[2] = -9999.;

  xyzglobalIn[0] = -9999.;
  xyzglobalIn[1] = -9999.;
  xyzglobalIn[2] = -9999.;
  xyzglobalOut[0] = -9999.;
  xyzglobalOut[1] = -9999.;
  xyzglobalOut[2] = -9999.;

  hitVolume[0] = -9999;
  hitVolume[1] = -9999;
  hitVolume[2] = -9999;
  hitVolume[3] = -9999;
  hitVolume[4] = -9999;
  hitVolume[5] = -9999;
  hitVolume[6] = -9999;
  hitVolume[7] = -9999;
  hitVolume[8] = -9999;
  track = -9999;
  layer = -9999;
  isubevent = -9999;
  nfile = -9999;

  return;
}

//________________________________________________________
SvxSnglPisaHitv1::SvxSnglPisaHitv1( const SvxSnglPisaHit *base_ptr)
{

  if( !base_ptr ) return;

  SetSvxCount( base_ptr->GetSvxCount() );

  SetMctrack( base_ptr->GetMctrack() );
  SetIdPart( base_ptr->GetIdPart() );
  SetXGlobal( base_ptr->GetXGlobal() );
  SetYGlobal( base_ptr->GetYGlobal() );
  SetZGlobal( base_ptr->GetZGlobal() );
  SetDele( base_ptr->GetDele() );

  SetPmomX( base_ptr->GetPmomX() );
  SetPmomY( base_ptr->GetPmomY() );
  SetPmomZ( base_ptr->GetPmomZ() );
  SetTof( base_ptr->GetTof() );

  SetXLocalIn( base_ptr->GetXLocalIn() );
  SetYLocalIn( base_ptr->GetYLocalIn() );
  SetZLocalIn( base_ptr->GetZLocalIn() );

  SetXLocalOut( base_ptr->GetXLocalOut() );
  SetYLocalOut( base_ptr->GetYLocalOut() );
  SetZLocalOut( base_ptr->GetZLocalOut() );

  SetXGlobalIn( base_ptr->GetXGlobalIn() );
  SetYGlobalIn( base_ptr->GetYGlobalIn() );
  SetZGlobalIn( base_ptr->GetZGlobalIn() );

  SetXGlobalOut( base_ptr->GetXGlobalOut() );
  SetYGlobalOut( base_ptr->GetYGlobalOut() );
  SetZGlobalOut( base_ptr->GetZGlobalOut() );

  for( unsigned int i=0; i<9; i++ )
  { SetHitVolume( i, base_ptr->GetHitVolume(i) ); }

  SetTrack( base_ptr->GetTrack() );
  SetLayer( base_ptr->GetLayer() );
  SetIsubevent( base_ptr->GetIsubevent() );
  SetNfile( base_ptr->GetNfile() );

  return;
}


//________________________________________________________
SvxSnglPisaHitv1::SvxSnglPisaHitv1( const SvxSnglPisaHit &base_ref)
{

  SetSvxCount( base_ref.GetSvxCount() );

  SetMctrack( base_ref.GetMctrack() );
  SetIdPart( base_ref.GetIdPart() );
  SetXGlobal( base_ref.GetXGlobal() );
  SetYGlobal( base_ref.GetYGlobal() );
  SetZGlobal( base_ref.GetZGlobal() );
  SetDele( base_ref.GetDele() );

  SetPmomX( base_ref.GetPmomX() );
  SetPmomY( base_ref.GetPmomY() );
  SetPmomZ( base_ref.GetPmomZ() );
  SetTof( base_ref.GetTof() );

  SetXLocalIn( base_ref.GetXLocalIn() );
  SetYLocalIn( base_ref.GetYLocalIn() );
  SetZLocalIn( base_ref.GetZLocalIn() );

  SetXLocalOut( base_ref.GetXLocalOut() );
  SetYLocalOut( base_ref.GetYLocalOut() );
  SetZLocalOut( base_ref.GetZLocalOut() );

  SetXGlobalIn( base_ref.GetXGlobalIn() );
  SetYGlobalIn( base_ref.GetYGlobalIn() );
  SetZGlobalIn( base_ref.GetZGlobalIn() );

  SetXGlobalOut( base_ref.GetXGlobalOut() );
  SetYGlobalOut( base_ref.GetYGlobalOut() );
  SetZGlobalOut( base_ref.GetZGlobalOut() );

  for( unsigned int i=0; i<9; i++ )
  { SetHitVolume( i, base_ref.GetHitVolume(i) ); }

  SetTrack( base_ref.GetTrack() );
  SetLayer( base_ref.GetLayer() );
  SetIsubevent( base_ref.GetIsubevent() );
  SetNfile( base_ref.GetNfile() );

  return;
}


