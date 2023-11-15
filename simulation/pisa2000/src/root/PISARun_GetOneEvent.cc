// $Id: PISARun_GetOneEvent.cc,v 1.11 2018/07/05 16:50:13 lajoie Exp $

/*!
  \file PISARun_GetOneEvent.cc
  \brief  Interface between PISA hits file and PHOOL
  \author Charlie Maguire
  \version $Revision: 1.11 $
  \date $Date: 2018/07/05 16:50:13 $
*/

/*!
	Interface between PISA hits file and PHOOL
	Original version developed in 1999 during STAF -> PHOOL transition
	First version was for reading subevents
	Second version was for reading full events, with MERGE files allowed

	Code is largely duplications one subsystem after another
	Could be hugely simplfied with some class structure instead
	This code would be obsolete when PISA writes out a PHOOL file directly
*/

#include <vector>
#include "PISARun.h"
#include "EmcPISAPara.h"
#include "MutPISAPara.h"

#include "PriPISAHitHelper.h"

using namespace std;

//_____________________________________________________________
void PISARun::HitsClear( void )
{ 
  
  KinPISAHit::KinClear();
  PriPISAHit::PriClear();
  BbcPISAHit::BbcClear();
  DchPISAHit::DchClear();

  EmcPISAHit::EmcClear();
  
  PadPISAHit::PadClear();
  CrkPISAHit::CrkClear();
  TecPISAHit::TecClear();
  TofPISAHit::TofClear();
  MutPISAHit::MutClear();
  MuiPISAHit::MuiClear();
  ZdcPISAHit::ZdcClear();
  SvxPISAHit::SvxClear();
  RxnPISAHit::RxnClear();
  FclPISAHit::FclClear();
  HbdPISAHit::HbdClear();
  AerPISAHit::AerClear();
  MuPCPISAHit::MuPCClear();
  RLTPISAHit::RLTClear();
  NCCPISAHit::NCCClear();
  MPCEXABSPISAHit::MPCEXABSClear();
  MPCFPLTPISAHit::MPCFPLTClear();
  MPCEXEntryPISAHit::MPCEXEntryClear();
  MpcPISAHit::MpcClear();
  VncPISAHit::VncClear();
  TfwPISAHit::TfwClear();
  
}

//__________________________________________________________________________________
// todo: change argument to pass a refence to the event index, rather than a pointer
bool PISARun::GetOneEvent( const int& event )
{
    
  // check number of files
  if( _inputs.empty() ) {
    cout << "PISARun::GetOneEvent - no input file " << endl;
    return false;
  }

  // dump number of files at first call
  static bool first( true );
  if( first )
  { 
    first = false;
    cout << "PISARun::GetOneEvent - number of input files = " <<  _inputs.size() << endl; 
  }
    
  // clear hit arrays
  /* 
  in priciple this should not be needed
  because already handled in Fun4AllPisaInputManager
  however since this is only clearing "std::vector"s, 
  it takes no overhead if the vectors are already empty.
  */
  HitsClear();
  
  // running maximum track id
  // get PISAEvent structure for all files in local array
  for( unsigned int i=0; i < _inputs.size(); i++)
  {    
    
    // load event for this TTree
    if( !_inputs[i]._ttree->GetEntry( event ) )
    {
      cout << "PISARun::GetOneEvent - unable to load event " << event << " from file " << _inputs[i]._filename << endl;
      return false;
    }
    
  }
  
  // set event header, get EMC and Mutr parameters
  for(  unsigned int i=0; i < _inputs.size(); i++)
  {
    
    // local storage of pisa event to shorten notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
    
    // store first file event header into static to facilitate access from calling classes
    // the SetEventHeader makes a copy of the pisaevent structure into a static object
    if( i == 0 ) PISAEventHeader::SetEventHeader( *pisaevent->GetHeader() ); 

    // subsystem parameters
    // they are initialized only once, when empty
    // should occur at first file, first event, provided that the detector was on in the simulations
    if( !EmcPISAPara::GetEmcParaCount() ) getEmcPar( pisaevent );
    if( !MutPISAPara::GetMutParaCount() ) getMutPar( pisaevent );
    
  }
  
  // fill full KinPISAHit array
  // this must be done first so that "true track id" (meaning: event-wise track unique id) is filled and track parents associated properly
  // before the detectors are processed
  
  _kin_helper.reset();
  for(  unsigned int i=0; i < _inputs.size(); i++)
  {
  
    // local storage of pisa event to shorten notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
    _kin_helper.add( *pisaevent->GetKinHits(), pisaevent->GetKinNhit(), i );
        
    // increment counter
    _counters["KinPISAHit"].add_count( pisaevent->GetKinNhit(), i );
    
  }
    
  // perform associations to "true track" ids
  _kin_helper.associate();
  // KinPISAHit::PrintAssociations();
  
  // fill other detectors
  for(  unsigned int i=0; i < _inputs.size(); i++)
  {

    // local storage of pisa event to shorten notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
    
    // generic subsystems
    getOneGenericEvent<PriPISAHit>(  2, *pisaevent->GetPriHits(), pisaevent->GetPriNhit(), i );
    getOneGenericEvent<BbcPISAHit>(  2, *pisaevent->GetBbcHits(), pisaevent->GetBbcNhit(), i );
    getOneGenericEvent<ZdcPISAHit>(  3, *pisaevent->GetZdcHits(), pisaevent->GetZdcNhit(), i );
    getOneGenericEvent<DchPISAHit>(  4, *pisaevent->GetDchHits(), pisaevent->GetDchNhit(), i );
    getOneGenericEvent<MpcPISAHit>( 11, *pisaevent->GetMpcHits(), pisaevent->GetMpcNhit(), i );
    getOneGenericEvent<CrkPISAHit>( 12, *pisaevent->GetCrkHits(), pisaevent->GetCrkNhit(), i );
    getOneGenericEvent<TecPISAHit>( 13, *pisaevent->GetTecHits(), pisaevent->GetTecNhit(), i );
    getOneGenericEvent<TfwPISAHit>( 26, *pisaevent->GetTfwHits(), pisaevent->GetTfwNhit(), i );
    getOneGenericEvent<TofPISAHit>( 14, *pisaevent->GetTofHits(), pisaevent->GetTofNhit(), i );
    getOneGenericEvent<SvxPISAHit>( 15, *pisaevent->GetSvxHits(), pisaevent->GetSvxNhit(), i );
    getOneGenericEvent<FclPISAHit>( 16, *pisaevent->GetFclHits(), pisaevent->GetFclNhit(), i );
    getOneGenericEvent<AerPISAHit>( 18, *pisaevent->GetAerHits(), pisaevent->GetAerNhit(), i );
    getOneGenericEvent<RxnPISAHit>( 19, *pisaevent->GetRxnHits(), pisaevent->GetRxnNhit(), i );
    getOneGenericEvent<HbdPISAHit>( 21, *pisaevent->GetHbdHits(), pisaevent->GetHbdNhit(), i );
    getOneGenericEvent<EmcPISAHit>( 22, *pisaevent->GetEmcHits(), pisaevent->GetEmcNhit(), i );
    getOneGenericEvent<MutPISAHit>( 23, *pisaevent->GetMutHits(), pisaevent->GetMutNhit(), i );
    getOneGenericEvent<MuiPISAHit>( 24, *pisaevent->GetMuiHits(), pisaevent->GetMuiNhit(), i );
  
  }
  
  // fill special detectors
  // need to pass all files at once for internal counting
  // first argument is the detector id
  getOnePadEvent( 7 );  
  getOneMuPCEvent( 8 );
  getOneRLTEvent( 9 );
  getOneNCCEvent( 10 );
  getOneMPCEXABSEvent( 10 );
  getOneMPCFPLTEvent( 10 );
  getOneMPCEXEntryEvent( 10 );
  
  // associate primary index to KinPISAHits
  PriPISAHitHelper().associate();
  
  return true;
  
}

//________________________________________________________
void PISARun::getEmcPar( PISAEvent::pointer pisaevent )
{

  for(int i_hit = 0; i_hit < pisaevent->GetEmcNpara(); i_hit++ )
  { EmcPISAPara::AddHit( *static_cast<EmcPISAPara*>(pisaevent->GetEmcParas()->UncheckedAt(i_hit) ) ); }
  
  if( EmcPISAPara::GetEmcParaCount() )
  { cout << "PISARun::getEmcPar - loaded " << EmcPISAPara::GetEmcParaCount() << " emc parameters" << endl; }
  
}
   
//________________________________________________________
void PISARun::getMutPar( PISAEvent::pointer pisaevent )
{

  for(int i_hit = 0; i_hit < pisaevent->GetMutNpara(); i_hit++ )
  { MutPISAPara::AddHit( *static_cast<MutPISAPara*>(pisaevent->GetMutParas()->UncheckedAt(i_hit) ) ); }
  
  if( MutPISAPara::GetMutParaCount() )
  { cout << "PISARun::getMutPar - loaded " << MutPISAPara::GetMutParaCount() << " mut parameters" << endl; }
  
}

//__________________________________________________________________________
bool PISARun::getOnePadEvent( int subsystem_id )
{

  if( _verbosity >= 1 ) cout << "PISARun::getOnePadEvent" << endl;
  
  // local storage to separate hits from different pad chambers
  vector< PadPISAHit > pc1_hits;
  vector< PadPISAHit > pc2_hits;
  vector< PadPISAHit > pc3_hits;
  
  for( unsigned int i=0; i < _inputs.size(); i++ )
  {
           
    // local storage of pisa event  for shorter notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
    
    // read hits
    for( int i_hit=0; i_hit < pisaevent->GetPadNhit(); i_hit++)
    { 
      
      PadPISAHit* tmp =  static_cast<PadPISAHit*>(pisaevent->GetPadHits()->UncheckedAt(i_hit ) );
      
      // update true track id
      int file(i);

      // get true_track index matching this unique ID
      int true_track( _kin_helper.findTrueTrack( file, tmp->GetIsubevent(), tmp->GetNtrack() ) );
      tmp->SetMctrack( true_track );
      tmp->SetNfile( file );
        
      // store in local array
      int index( tmp->GetIpc() );
      switch( index )
      {
        case 1: pc1_hits.push_back( *tmp ); break;
        case 2: pc2_hits.push_back( *tmp ); break;
        case 3: pc3_hits.push_back( *tmp ); break;
      
        default:
        cout << "PISARun::getOnePadEvent - invalid index" << index << endl;
        break;
        
      }
    }
        
  }
  
  // resize arrays and store counters
  PadPISAHit::SetPadCounts( pc1_hits.size(), pc2_hits.size(), pc3_hits.size() ); 
  _counters["PadPISAHit pc1"].add_count( pc1_hits.size() );
  _counters["PadPISAHit pc2"].add_count( pc2_hits.size() );
  _counters["PadPISAHit pc3"].add_count( pc3_hits.size() );

  // fill static storage
  for( unsigned int i = 0; i < pc1_hits.size(); i++ )
  { PadPISAHit::SetPC1HitEvt( i, pc1_hits[i] ); }
    
  for( unsigned int i = 0; i < pc2_hits.size(); i++ )
  { PadPISAHit::SetPC2HitEvt( i, pc2_hits[i] ); }

  for( unsigned int i = 0; i < pc3_hits.size(); i++ )
  { PadPISAHit::SetPC3HitEvt( i, pc3_hits[i] ); }

  return true;
  
}

//__________________________________________________________________________
bool PISARun::getOneMuPCEvent( int subsystem_id )
{  
  
  if( _verbosity >= 1 ) cout << "PISARun::getOneMuPCEvent" << endl;
 
  // local storage
  vector<MuPCPISAHit> pc1_hits;
  vector<MuPCPISAHit> pc2_hits;
  vector<MuPCPISAHit> pc3_hits;
  for( unsigned int i=0; i < _inputs.size(); i++ )
  {
    
    // local storage of pisa event  for shorter notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
        
    // read hits
    for( int i_hit=0; i_hit < pisaevent->GetMuPCNhit(); i_hit++)
    { 
      
      // read hit
      MuPCPISAHit* tmp =  static_cast<MuPCPISAHit*>(pisaevent->GetMuPCHits()->UncheckedAt(i_hit ) );
      
      // get true track id
      // update true track id
      int file(i);
      
      // get true_track index matching this unique ID
      int true_track( _kin_helper.findTrueTrack( file, tmp->GetIsubevent(), tmp->GetNtrack() ) );
      tmp->SetMctrack( true_track );
      tmp->SetNfile( file );
      
      int index( tmp->GetIpc() );
      switch( index )
      {
        case 1: pc1_hits.push_back( *tmp ); break;
        case 2: pc2_hits.push_back( *tmp ); break;
        case 3: pc3_hits.push_back( *tmp ); break;
      
        default:
        cout << "PISARun::getOneMuPCEvent - invalid index" << index << endl;
        break;
        
      }
    }
    
  }
  
  // resize arrays and store counters
  MuPCPISAHit::SetMuPCCounts( pc1_hits.size(), pc2_hits.size(), pc3_hits.size() ); 
  _counters["MuPCPISAHit pc1"].add_count( pc1_hits.size() );
  _counters["MuPCPISAHit pc2"].add_count( pc2_hits.size() );
  _counters["MuPCPISAHit pc3"].add_count( pc3_hits.size() );

  // fill static storage
  for( unsigned int i = 0; i < pc1_hits.size(); i++ )
  { MuPCPISAHit::SetMuPC1HitEvt( i, pc1_hits[i] ); }
    
  for( unsigned int i = 0; i < pc2_hits.size(); i++ )
  { MuPCPISAHit::SetMuPC2HitEvt( i, pc2_hits[i] ); }

  for( unsigned int i = 0; i < pc3_hits.size(); i++ )
  { MuPCPISAHit::SetMuPC3HitEvt( i, pc3_hits[i] ); }

  return true;
  
}

//__________________________________________________________________________
bool PISARun::getOneNCCEvent( int subsystem_id )
{

  if( _verbosity >= 1 ) cout << "PISARun::getOneNCCEvent" << endl;
  
  // local storage
  vector< NCCPISAHit > ncc1_hits;
  vector< NCCPISAHit > ncc2_hits;
  for( unsigned int i=0; i < _inputs.size(); i++ )
  {
    
    // local storage of pisa event  for shorter notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
       
    // read hits
    for( int i_hit=0; i_hit < pisaevent->GetNCCNhit(); i_hit++)
    { 
      
      // read hit
      NCCPISAHit* tmp =  static_cast<NCCPISAHit*>(pisaevent->GetNCCHits()->UncheckedAt(i_hit ) );

      // update true track id
      int file(i);

      // get true_track index matching this unique ID
      int true_track( _kin_helper.findTrueTrack( file, tmp->GetIsubevent(), tmp->GetNtrack() ) );
      tmp->SetMctrack( true_track );
      tmp->SetNfile( file );
     
      int index( tmp->GetIncc() );
      switch( index )
      {
        case 1: ncc1_hits.push_back( *tmp ); break;
        case 2: ncc2_hits.push_back( *tmp ); break;
      
        default:
        cout << "PISARun::getOneNCCEvent - invalid index" << index << endl;
        break;
        
      }
    }
    
  }
      
  // resize arrays and store counters
  NCCPISAHit::SetNCCCounts( ncc1_hits.size(), ncc2_hits.size() ); 
  _counters["NCCPISAHit ncc1"].add_count( ncc1_hits.size() );
  _counters["NCCPISAHit ncc2"].add_count( ncc2_hits.size() );

  // fill static storage
  for( unsigned int i = 0; i < ncc1_hits.size(); i++ )
  { NCCPISAHit::SetNCC1HitEvt( i, ncc1_hits[i] ); }

  for( unsigned int i = 0; i < ncc2_hits.size(); i++ )
  { NCCPISAHit::SetNCC2HitEvt( i, ncc2_hits[i] ); }

  return true;
}

//__________________________________________________________________________
bool PISARun::getOneMPCEXABSEvent( int subsystem_id )
{

  if( _verbosity >= 1 ) cout << "PISARun::getOneMPCEXABSEvent" << endl;
  
  // local storage
  vector< MPCEXABSPISAHit > ncc1_hits;
  vector< MPCEXABSPISAHit > ncc2_hits;
  for( unsigned int i=0; i < _inputs.size(); i++ )
  {
    
    // local storage of pisa event  for shorter notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
       
    // read hits
    for( int i_hit=0; i_hit < pisaevent->GetMPCEXABSNhit(); i_hit++)
    { 
      
      // read hit
      MPCEXABSPISAHit* tmp =  static_cast<MPCEXABSPISAHit*>(pisaevent->GetMPCEXABSHits()->UncheckedAt(i_hit ) );

      // update true track id
      int file(i);

      // get true_track index matching this unique ID
      int true_track( _kin_helper.findTrueTrack( file, tmp->GetIsubevent(), tmp->GetNtrack() ) );
      tmp->SetMctrack( true_track );
      tmp->SetNfile( file );
     
      int index( tmp->GetIncc() );
      switch( index )
      {
        case 1: ncc1_hits.push_back( *tmp ); break;
        case 2: ncc2_hits.push_back( *tmp ); break;
      
        default:
	  cout << "PISARun::getOneMPCEXABSEvent - invalid index = " << index << " i_hit = " << i_hit << endl;
        break;
        
      }
    }
    
  }
      
  // resize arrays and store counters
  MPCEXABSPISAHit::SetMPCEXABSCounts( ncc1_hits.size(), ncc2_hits.size() ); 
  _counters["MPCEXABSPISAHit ncc1"].add_count( ncc1_hits.size() );
  _counters["MPCEXABSPISAHit ncc2"].add_count( ncc2_hits.size() );

  // fill static storage
  for( unsigned int i = 0; i < ncc1_hits.size(); i++ )
    { MPCEXABSPISAHit::SetMPCEXABS1HitEvt( i, ncc1_hits[i] ); }

  for( unsigned int i = 0; i < ncc2_hits.size(); i++ )
    { MPCEXABSPISAHit::SetMPCEXABS2HitEvt( i, ncc2_hits[i] ); }

  return true;
}

//__________________________________________________________________________
bool PISARun::getOneMPCFPLTEvent( int subsystem_id )
{

  if( _verbosity >= 1 ) cout << "PISARun::getOneMPCFPLTEvent" << endl;
  
  // local storage
  vector< MPCFPLTPISAHit > ncc1_hits;
  vector< MPCFPLTPISAHit > ncc2_hits;
  for( unsigned int i=0; i < _inputs.size(); i++ )
  {
    
    // local storage of pisa event  for shorter notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
       
    // read hits
    for( int i_hit=0; i_hit < pisaevent->GetMPCFPLTNhit(); i_hit++)
    { 
      
      // read hit
      MPCFPLTPISAHit* tmp =  static_cast<MPCFPLTPISAHit*>(pisaevent->GetMPCFPLTHits()->UncheckedAt(i_hit ) );

      // update true track id
      int file(i);

      // get true_track index matching this unique ID
      int true_track( _kin_helper.findTrueTrack( file, tmp->GetIsubevent(), tmp->GetNtrack() ) );
      tmp->SetMctrack( true_track );
      tmp->SetNfile( file );
     
      //int index( tmp->GetIncc() );
      
      int index = tmp->GetIncc(); 
      if((index>=10) && (index<20) ) index = index%10; 
      if((index>=20) && (index<30) ) index = index%20; 
      if((index>=30) && (index<40) ) index = index%30; 

      switch( index )
      {
        case 1: ncc1_hits.push_back( *tmp ); break;
        case 2: ncc2_hits.push_back( *tmp ); break;
      
        default:
	  cout << "PISARun::getOneMPCFPLTEvent - invalid index = " << index << " i_hit = " << i_hit << endl;
        break;
        
      }
    }
    
  }
      
  // resize arrays and store counters
  MPCFPLTPISAHit::SetMPCFPLTCounts( ncc1_hits.size(), ncc2_hits.size() ); 
  _counters["MPCFPLTPISAHit ncc1"].add_count( ncc1_hits.size() );
  _counters["MPCRPLTPISAHit ncc2"].add_count( ncc2_hits.size() );

  // fill static storage
  for( unsigned int i = 0; i < ncc1_hits.size(); i++ )
    { MPCFPLTPISAHit::SetMPCFPLT1HitEvt( i, ncc1_hits[i] ); }

  for( unsigned int i = 0; i < ncc2_hits.size(); i++ )
    { MPCFPLTPISAHit::SetMPCFPLT2HitEvt( i, ncc2_hits[i] ); }

  return true;
}

//__________________________________________________________________________
bool PISARun::getOneMPCEXEntryEvent( int subsystem_id )
{

  if( _verbosity >= 1 ) cout << "PISARun::getOneMPCEXEntryEvent" << endl;
  
  // local storage
  vector< MPCEXEntryPISAHit > _hits;
  for( unsigned int i=0; i < _inputs.size(); i++ )
  {
    
    // local storage of pisa event  for shorter notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
       
    // read hits
    for( int i_hit=0; i_hit < pisaevent->GetMPCEXEntryNhit(); i_hit++)
    { 
      
      // read hit
      MPCEXEntryPISAHit* tmp =  static_cast<MPCEXEntryPISAHit*>(pisaevent->GetMPCEXEntryHits()->UncheckedAt(i_hit ) );

      // update true track id
      int file(i);

      // get true_track index matching this unique ID
      int true_track( _kin_helper.findTrueTrack( file, tmp->GetIsubevent(), tmp->GetNtrack() ) );
      tmp->SetMctrack( true_track );
      tmp->SetNfile( file );
     
      _hits.push_back( *tmp ); 

    }
    
  }
      
  // resize arrays and store counters
  MPCEXEntryPISAHit::SetMPCEXEntryCount( _hits.size() ); 
  _counters["MPCEXEntryPISAHit"].add_count( _hits.size() );

  // fill static storage
  for( unsigned int i = 0; i < _hits.size(); i++ )
    { MPCEXEntryPISAHit::SetMPCEXEntryHitEvt( i, _hits[i] ); }


  return true;
}

//__________________________________________________________________________
bool PISARun::getOneRLTEvent( int subsystem_id )
{
  
  if( _verbosity >= 1 ) cout << "PISARun::getOneRLTEvent" << endl;
  
  // local storage
  vector<RLTPISAHit> pc1_hits;
  vector<RLTPISAHit> pc2_hits;
  vector<RLTPISAHit> pc3_hits;  
  for( unsigned int i=0; i < _inputs.size(); i++ )
  {
    
    // local storage of pisa event  for shorter notations
    PISAEvent::pointer pisaevent = _inputs[i]._pisa_event_pointer;
        
    // read hits
    for( int i_hit=0; i_hit < pisaevent->GetrltNhit(); i_hit++)
    { 
      
      RLTPISAHit* tmp =  static_cast<RLTPISAHit*>(pisaevent->GetrltHits()->UncheckedAt(i_hit ) );
      
      // update true track id
      int file(i);

      // get true_track index matching this unique ID
      int true_track( _kin_helper.findTrueTrack( file, tmp->GetIsubevent(), tmp->GetNtrack() ) );
      tmp->SetMctrack( true_track );
      tmp->SetNfile( file );

      int index( tmp->GetIrpc() );
      switch( index )
      {
        case 1: pc1_hits.push_back( *tmp ); break;
        case 2: pc2_hits.push_back( *tmp ); break;
        case 3: pc3_hits.push_back( *tmp ); break;
      
        default:
        cout << "PISARun::getOneRLTEvent - invalid index" << index << endl;
        break;
        
      }
    }
    
  }
    
  // resize arrays and store counters
  RLTPISAHit::SetrltCounts( pc1_hits.size(), pc2_hits.size(), pc3_hits.size() ); 
  _counters["RLTPISAHit rpc1"].add_count( pc1_hits.size() );
  _counters["RLTPISAHit rpc2"].add_count( pc2_hits.size() );
  _counters["RLTPISAHit rpc3"].add_count( pc3_hits.size() );

  // fill static storage
  for( unsigned int i = 0; i < pc1_hits.size(); i++ )
  { RLTPISAHit::SetrltRPC1HitEvt( i, pc1_hits[i] ); }
    
  for( unsigned int i = 0; i < pc2_hits.size(); i++ )
  { RLTPISAHit::SetrltRPC2HitEvt( i, pc2_hits[i] ); }

  for( unsigned int i = 0; i < pc3_hits.size(); i++ )
  { RLTPISAHit::SetrltRPC3HitEvt( i, pc3_hits[i] ); }

  return true;
}
