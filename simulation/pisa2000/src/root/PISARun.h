#ifndef _PISARun_
#define _PISARun_

// $Id: PISARun.h,v 1.38 2018/07/05 16:50:13 lajoie Exp $

/*!
  \file   PISARun.h
  \brief  interface to pisa
  \author Charlie Maguire
  \version $Revision: 1.38 $
  \date    $Date: 2018/07/05 16:50:13 $
*/

#include <iostream>
#include <map>
#include <vector>
#include<boost/weak_ptr.hpp>

#include "PISAHitCounter.h"
#include "PISAEvent.h"
#include "KinPISAHit.h"
#include "KinPISAHitHelper.h"
#include "PriPISAHit.h"
#include "ZdcPISAHit.h"
#include "AerPISAHit.h"
#include "BbcPISAHit.h"
#include "DchPISAHit.h"
#include "PadPISAHit.h"
#include "CrkPISAHit.h"
#include "TecPISAHit.h"
#include "TfwPISAHit.h"
#include "TofPISAHit.h"
#include "SvxPISAHit.h"
#include "HbdPISAHit.h"
#include "MuPCPISAHit.h"
#include "FclPISAHit.h"
#include "RxnPISAHit.h"
#include "EmcPISAHit.h"
#include "MutPISAHit.h"
#include "MuiPISAHit.h"
#include "RLTPISAHit.h"
#include "VncPISAHit.h"
#include "MpcPISAHit.h"
#include "NCCPISAHit.h"
#include "MPCEXABSPISAHit.h"
#include "MPCFPLTPISAHit.h"
#include "MPCEXEntryPISAHit.h"
#include "rootAnc.h"

//! pisa interface
class PISARun
{

  public:

  //! bitwise subsystem flags
  enum SubsystemFlag
  {
    AER = 1<<0,
    BBC = 1<<1,
    CRK = 1<<2,
    DCH = 1<<3,
    EMC = 1<<4,
    FCL = 1<<5,     
    HBD = 1<<6,     
    MPC = 1<<7,     
    MUP = 1<<8,     
    MUI = 1<<9,     
    MUT = 1<<10,     
    NCC = 1<<12,     
    VNC = 1<<13,     
    PAD = 1<<14,     
    PRI = 1<<15,         
    PYT = 1<<16,     
    RLT = 1<<17,     
    RXN = 1<<18,     
    SVX = 1<<19,     
    TEC = 1<<20,     
    TFW = 1<<21,     
    TOF = 1<<22,     
    ZDC = 1<<23,
    
    //! correspond to all subsystem selected 
    /*! is equal to (1 << (max_subsystem+1)) - 1 */
    ALL = (1<<25) - 1
  };
  
  //! constructor
  PISARun( void ):
    _max_events(0),
    _subsystem_flag( ALL ),
    _verbosity( 0 )
  {}
  
  //! destructor
  virtual ~PISARun( void ) 
  {}
 
  //! subsystem flag
  void set_subsystem_flag( const unsigned long int& flag )
  { _subsystem_flag = flag; }
  
  //! subsystem flag
  const unsigned long int& get_subsystem_flag( void ) const 
  { return _subsystem_flag; }
  
  //! verbosity
  void set_verbosity( const int& verbosity )
  { _verbosity = verbosity; }
  
  //! add a file to the list of inputs
  /*! returns true on success */
  bool AddFile( const std::string& file_name );
  
  //! get number of stored inputs
  int GetNInputs( void ) const
  { return _inputs.size(); }
  
  //! get max number of events to be processed with this set of files
  int GetMaxEvents( void ) const
  { return _max_events; }
  
  //! reset inputs
  void Reset( void ) 
  { 
    _inputs.clear(); 
    _counters.clear();
    _max_events = 0;
  }
  
  //! fill detector hits from PISAEvent tables
  bool GetOneEvent( const int& event );
  
  //! pisa evaluation for all subsystems
  void AncAll(const int & ancflag );
  
  //! clear detector event structures
  void HitsClear();
    
  //! reset all counters
  void ResetCounters( void )
  { _counters.clear(); }
  
  //! print all counters
  void PrintCounters( void ) const;
  
  private:
  
  //! fill emc parameters
  void getEmcPar( PISAEvent::pointer );
  
  //! fill mut parameters
  void getMutPar( PISAEvent::pointer );
  
  //! fill one PAD event
  /*! it cannot be made generic because pad has different arrays per PC index */
  bool getOnePadEvent( int subsystem_id );
  
  //! fill one MuPC event
  /*! it cannot be made generic because pad has different arrays per PC index */
  bool getOneMuPCEvent( int subsystem_id );
   
  //! fill one NCC event
  /*! it cannot be made generic because ncc has different arrays per arm */		
  bool getOneNCCEvent( int subsystem_id );

  bool getOneMPCEXABSEvent( int subsystem_id );

  bool getOneMPCFPLTEvent( int subsystem_id );

  bool getOneMPCEXEntryEvent( int subsystem_id );

  //! fill one RLT event
  /*! it cannot be made generic because ncc has different arrays per arm */
  bool getOneRLTEvent( int subsystem_id );
  
  //! generic event filling
  /*! 
  the T class name passed as template argument correspond to a XXXPISAHit class
  that can be filled sequentially, using a AddHit method
  \param subsystem_id the subsystem ID needed to retrieve track association
  \param array the TClonesArray that contains this file hits of type T*
  \param array_size the size of the array
  \param file the file id (in case several files are merged
  */
  template< typename T > 
  void getOneGenericEvent( int subsystem_id, TClonesArray& array, int array_size, int file )
  {
    
    // some debugging information
    if( _verbosity >= 1 ) 
    { 
      std::cout 
        << "PISARun::getOneGenericEvent -"
        << " processing " << T().GetName()
        << " array_size: " << array_size 
        << std::endl; 
    }

    // increment counters
    _counters[T().GetName()].add_count( array_size, file );
    
    // look over hits in array
    for( int i_hit = 0; i_hit < array_size; i_hit++ )
    { 
      
      // retrieve object at index i, and check
      T* tmp =  static_cast<T*>(array.UncheckedAt(i_hit ) );
               
      // get KinPISAHit matching this unique ID
      int true_track( _kin_helper.findTrueTrack( file, tmp->GetIsubevent(), tmp->GetNtrack() ) );
      tmp->SetMctrack( true_track );
      tmp->SetNfile( file );
      T::AddHit( *tmp ); 

      if( true_track < 0 ) 
      {

        if( !( subsystem_id == 2 || subsystem_id == 12 ) ) {
        
          // print error message for all subsystem except PriPISAHit (some primary track never make it into a KinPISAHit)
          std::cout << "PISARun::getOneGenericEvent - unable to find matching kin hit for"
            << " class: " <<  T().GetName() 
            << " file: " << tmp->GetNfile() 
            << " event: " << tmp->GetIsubevent() 
            << " track: " << tmp->GetNtrack()
            << std::endl;
         }
      }
        
      // store object in static container
 
    }
        
    return;
    
  }
         
  //!@name ANC method used to fill basic ntuples from event structures
  //@{
  
  // some of the methods are still implemented as "C" functions
  // some have beed moved directly as PISARun class methods.
  // this is (not so important) work in progress 
  
  //! ZDC pisa evaluation
  void AncZdc(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! Aerogel pisa evaluation
  void AncAer(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! BBC pisa evaluation
  void AncBbc(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! Drift chambers pisa evaluation
  void AncDch(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! Pad chambers pisa evaluation
  void AncPad(const int & ancflag, PISAEvent::pointer pisaevent) 
  { rootAncPad(ancflag, pisaevent->GetHeader()); }
  
  //! Crk pisa evaluation
  void AncCrk(const int & ancflag, PISAEvent::pointer pisaevent)
  { rootAncCrk(ancflag, pisaevent->GetHeader()); }
  
  //! TEC pisa evaluation
  void AncTec(const int & ancflag, PISAEvent::pointer pisaevent) 
  { rootAncTec(ancflag, pisaevent->GetHeader()); }
  
  //! TOF west pisa evaluation
  void AncTfw(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! TOF pisa evaluation
  void AncTof(const int & ancflag, PISAEvent::pointer pisaevent) 
  { rootAncTof(ancflag, pisaevent->GetHeader()); }
  
  //! Svx (silicon vertex detector) pisa evaluation
  void AncSvx(const int & ancflag, PISAEvent::pointer pisaevent) 
  { rootAncSvx(ancflag, pisaevent->GetHeader()); }
  
  //! HBD (hadron blind detector) pisa evaluation
  void AncHbd(const int & ancflag, PISAEvent::pointer pisaevent) 
  { rootAncHbd(ancflag, pisaevent->GetHeader()); }
  
  //! Muon pad chambers pisa evaluation
  void AncMuPC(const int & ancflag, PISAEvent::pointer pisaevent) 
  { rootAncMuPC(ancflag, pisaevent->GetHeader()); }
  
  //! RLT pisa evaluation
  void AncRLT(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! NCC (nose cone calorimeter) pisa evaluation
  void AncNCC(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! VNC (nose cone calorimeter) pisa evaluation
  void AncVnc(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! fill Muon pad chambers PISA evaluation ntuple
  void AncMpc(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! FCL pisa evaluation
  void AncFcl(const int & ancflag, PISAEvent::pointer pisaevent) 
  { rootAncFcl(ancflag, pisaevent->GetHeader()); }
  
  //! Rxn (reaction plane detector) pisa evaluation
  void AncRxn(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! EMCal pisa evaluation
  void AncEmcPad(const int & ancflag, PISAEvent::pointer pisaevent)
  { rootAncEmcPad(ancflag, pisaevent->GetHeader()); }
  
  //! fill Muon Tracker PISA evaluation ntuple
  void AncMut(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! fill Muon Identifier PISA evaluation ntuple
  void AncMui(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! Primary particles pisa evaluation
  void AncPri(const int & ancflag, PISAEvent::pointer pisaevent);
  
  //! Pythia information pisa evaluation
  void AncPythia(const int & ancflag, PISAEvent::pointer pisaevent) 
  { rootAncPythia(ancflag, pisaevent->GetHeader()); }
  
  //@}

  //! convert radians into degrees
  static const float RAD2DEG;

  //! PISAInput internal container
  /*! it keeps track of the TFile, the TTree, and the associated PISAEvent structure */
  class PISAInput
  {

    public:
    
    //! filename
    std::string _filename;
    
    //! shared pointers to TFile
    typedef boost::shared_ptr<TFile> TFile_pointer;
    
    //! shared pointer to TFile
    TFile_pointer _tfile;
    
    //! associated "T" TTree
    TTree* _ttree;
    
    //! associated event structure
    PISAEvent::pointer _pisa_event_pointer;
    
  };
      
  //! inputs
  std::vector<PISAInput> _inputs;
  
  //! maximum number of events for the recorded set of files
  int _max_events;
  
  //! subsystem selection flag
  unsigned long int _subsystem_flag;
      
  //! hit counter map
  typedef std::map< std::string, PISAHitCounter > CounterMap;
  CounterMap _counters;
  
  //! verbosity
  int _verbosity;

  //! helper class
  KinPISAHitHelper _kin_helper;
  
  
};

#endif
