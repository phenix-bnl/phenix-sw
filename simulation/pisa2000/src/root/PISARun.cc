// $Id: PISARun.cc,v 1.7 2015/03/12 10:39:07 chiu Exp $

#include "PISARun.h"

/*!
  \file   PISARun.cc
  \brief  interface to pisa
  \author Charlie Maguire
  \version $Revision: 1.7 $
  \date    if( _subsystem_flag & $Date: 2015/03/12 10:39:07 $
*/

using namespace std;

//________________________________________
const float PISARun::RAD2DEG = 180.0/M_PI;

//_____________________________________________________________
void PISARun::PrintCounters( void ) const
{
  
  cout << endl << "PISARun::PrintCounters" << endl;
  for( CounterMap::const_iterator iter = _counters.begin(); iter != _counters.end(); iter++ )
  { 
    if( iter->second.accumulated_size() )
    { cout << iter->first << " " << iter->second << endl; }
  }
  cout << endl;
  
  return;
  
}

//_____________________________________________________________
void PISARun::AncAll( const int& ancflag )
{
  
  // check inputs
  if( _inputs.empty() ) return;
  PISAEvent::pointer pisaevent( _inputs[0]._pisa_event_pointer );
  
  // call ANC for all subsystems
  if( _subsystem_flag & AER ) AncAer(ancflag, pisaevent); 
  if( _subsystem_flag & BBC ) AncBbc(ancflag, pisaevent); 
  if( _subsystem_flag & CRK ) AncCrk(ancflag, pisaevent);
  if( _subsystem_flag & DCH ) AncDch(ancflag, pisaevent);
  if( _subsystem_flag & EMC ) AncEmcPad(ancflag, pisaevent);
  if( _subsystem_flag & FCL ) AncFcl(ancflag, pisaevent);
  if( _subsystem_flag & HBD ) AncHbd(ancflag, pisaevent);
  if( _subsystem_flag & VNC ) AncVnc(ancflag, pisaevent);
  if( _subsystem_flag & MPC ) AncMpc(ancflag, pisaevent);
  if( _subsystem_flag & MUP ) AncMuPC(ancflag, pisaevent);
  if( _subsystem_flag & MUI ) AncMui(ancflag, pisaevent);
  if( _subsystem_flag & MUT ) AncMut(ancflag, pisaevent);
  if( _subsystem_flag & NCC ) AncNCC(ancflag, pisaevent);
  if( _subsystem_flag & PAD ) AncPad(ancflag, pisaevent);
  if( _subsystem_flag & PRI ) AncPri(ancflag, pisaevent);
  if( _subsystem_flag & PYT ) AncPythia(ancflag, pisaevent);
  if( _subsystem_flag & RLT ) AncRLT(ancflag, pisaevent);
  if( _subsystem_flag & RXN ) AncRxn(ancflag, pisaevent);
  if( _subsystem_flag & SVX ) AncSvx(ancflag, pisaevent);
  if( _subsystem_flag & TEC ) AncTec(ancflag, pisaevent);
  if( _subsystem_flag & TFW ) AncTfw(ancflag, pisaevent);
  if( _subsystem_flag & TOF ) AncTof(ancflag, pisaevent);
  if( _subsystem_flag & ZDC ) AncZdc(ancflag, pisaevent);
  
}
