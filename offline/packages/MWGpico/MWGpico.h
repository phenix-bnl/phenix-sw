// $Id: MWGpico.h,v 1.79 2014/03/10 05:45:11 mwysocki Exp $

#ifndef __MWGpico_H__
#define __MWGpico_H__

/*!
  \file MWGpico.h
  \brief fun4all module to fill Muon picoDSTs from nanoDSTs
  \author Frederic Fleuret/Hugo Pereira
  \version $Revision: 1.79 $
  \date $Date: 2014/03/10 05:45:11 $
*/

#include <set>
#include <map>
#include <vector>
#include <iostream>
#include <SubsysReco.h>
#include <SpinDataEventOut.h>
#include <TH1.h>
#include <TH2.h>
#include <TTree.h>
#include <TNtuple.h>
#include <TrigLvl1v1.h>
#include <TString.h>

// LOCAL
#include "MWGCuts.h"
#include "MUTOO.h"
#include "MWGRunInfo.h"
#include <FvtxConeTracklets.h>
class EventHeader;
class headerWrapper;
class PHCompositeNode;
class PHGlobal;
class ReactionPlaneObject;
class PHInclusiveNanoCuts;
class PHMuoTracksOut;
class RunHeader;
class SpinDataEventOut;
class TMutTrkMap;
class TMutMCTrkMap;
class TMutMCHitMap;
class TMutHitMap;
class TMuiRoadMapO;
class TRpcHitMap;
class TRpcMuoTrkMap;
class TRpcTrkMap;
class TRpcHodoHitMap;
class TRpcCoordMap;
class TMCPrimaryMap;
class MutrgTrkArray;
class MutrgHitArray;
class TriggerHelper;
class BbcMultipleVtx;
class FvtxConeTracklets;

//! fun4all module to fill Muon picoDSTs from nanoDSTs
class MWGpico: public SubsysReco
{
 public:

  //! default constructor
  MWGpico();

  //! filled constructor
  MWGpico( const char* choice, const char* where="RCF");

  //! destructor
  virtual ~MWGpico();

  //! global initialization
  int Init(PHCompositeNode *topNode);

  //! Run initialization
  int InitRun(PHCompositeNode *topNode);

  //! event method
  int process_event(PHCompositeNode *topNode);

  //! global termination
  int End(PHCompositeNode *topNode);

  //! load list of nanoDST files to be processed
  void LoadNano(const char* choice, const char* where);

  //! set data type (pp, AuAu or dAu)
  void DataType( const char* type )
  { _type = type;}

  //! return size of nanoDSTs
  int NanoListSize()
  {return _nano_list.size();}

  //! return nanoDST file located at index i
  TString NanoFile(int i)
  {return _nano_list[i];}

  //! decide picoDST type
  enum MWGpicoDSTType
  {
    NONE,
    DIMUONS,
    DIMUONSOO,
    SNGMUONS,
    NEWSNGMUONS,
    EVTMIX,
    EVTMIXOO,
    HISTOGRAMS
  };

  //! picoDST type and associated file
  void MakePico( const char* pico, const char* rootfile)
  {

    if( !pico ) return;
    MUTOO::PRINT( std::cout, "MWGpico::MakePico" );
    std::string pico_string( pico );
    std::string type_name;
    MWGpicoDSTType type( NONE );
    if( pico_string == "dimuons" )         { type_name = "DIMUONS";     type = DIMUONS; }
    else if( pico_string == "dimuonsOO" )  { type_name = "DIMUONSOO";	 type = DIMUONSOO; }
    else if( pico_string == "sngmuons" )   { type_name = "SNGMUONS";	 type = SNGMUONS; }
    else if( pico_string == "newsngmuons" )   { type_name = "NEWSNGMUONS";	 type = NEWSNGMUONS; }
    else if( pico_string == "evtMix" )     { type_name = "EVTMIX";	 type = EVTMIX; }
    else if( pico_string == "evtMixOO" )   { type_name = "EVTMIXOO";	 type = EVTMIXOO; }
    else if( pico_string == "histograms" ) { type_name = "HISTOGRAMS";  type = HISTOGRAMS; }
    else {
      std::cout << "invalid picoDST type: " << pico << std::endl;
      std::cout << "doing nothing." << std::endl;
      type = NONE;
    }

    std::cout << "This method is obsolete" << std::endl;
    std::cout << "You should use the corresponding MWGpicoDSTType in place of the character string" << std::endl;
    std::cout << "namely: " << std::endl;
    std::cout << "   picoDST->MakePico( MWGpico::" << type_name << ", \"" << rootfile << "\")" << std::endl;
    std::cout << "   in place of picoDST->MakePico( \"" << pico << "\", \"" << rootfile << "\")" << std::endl;

    if( type != NONE ) _picos.insert( std::make_pair( type, rootfile ) );
    MUTOO::PRINT( std::cout, "**" );
  }

  //! picoDST type and associated file
  void MakePico( const MWGpicoDSTType& pico, const char* rootfile)
  { _picos.insert( std::make_pair( pico, rootfile ) ); }

//   //! instanciate mutr efficiency object
//   void MakeMutEffic()
//   { _muteffic = new MutEffic(); }

  //! run_info object
  const MWGRunInfo& run_info( void ) const
  { return _run_info; }

  //! retrieve associated cut object
  MWGCuts& Cuts( void )
  { return _cuts; }

  /*! \brief
    initialize cuts on nanoDST to select which events/dimuons
    are to be written to the pico
  */
  void InitCuts(const char* what)
  { _cuts.init( what ); }

  //! dump cuts to screen
  void PrintCuts()
  { _cuts.print(); }

  //! turn on golden muon cut (just for Run12pp right now)
  void set_golden_muons(bool b)
  {_use_golden_mass_cut = b;}

  //! do_dbinit flag
  //! defines nanoDST file list input file
  void set_nano_file( const char* file )
  { if( file ) _nano_file = file; }

  //! run number enumeration
  enum RunType {
    Unknown,
    RUN3,
    RUN4,
    RUN5,
    RUN6,
    RUN7,
    RUN8,
    RUN8pp,
    RUN9,
    RUN10,
    RUN11,
    RUN11pp,
    RUN12,
    RUN12pp,
    RUN12pp200,
    RUN12CuAu,
    RUN13
  };

  //! set run type
  void set_run_type( const RunType& runtype)
  {_runtype=runtype;}

  //! retrieve run_type
  RunType get_run_type() const
  { return _runtype;}

  //! retrieve run_type name
  std::string get_run_type_name( void ) const
  { return get_run_type_name( get_run_type() ); }

  //! retrieve run_type name
  std::string get_run_type_name( const RunType& run_type ) const
  {
    switch( run_type )
    {
      case RUN3: return "RUN3";
      case RUN4: return "RUN4";
      case RUN5: return "RUN5";
      case RUN6: return "RUN6";
      case RUN7: return "RUN7";
      case RUN8: return "RUN8";
      case RUN8pp: return "RUN8pp";
      case RUN9: return "RUN9";
      case RUN10: return "RUN10";
      case RUN11: return "RUN11";
      case RUN11pp: return "RUN11pp";
      case RUN12: return "RUN12";
      case RUN12pp: return "RUN12pp";
      case RUN12pp200: return "RUN12pp200";
      case RUN12CuAu: return "RUN12CuAu";
      case RUN13: return "RUN13";
      case Unknown: return "Unknown";
      default:
      {
        std::ostringstream what;
        what << "Unknown (" << run_type << ")";
        return what.str();
      }
    }
  }

  //! retrieve stop flag
  bool StopProcessing()
  { return _thestopflag;}

  //! fill list of nodes [OBSOLETE]
  void AccessNode( const char* accessNode)
  {
    std::cout << std::endl;
    std::cout << "MWGpico::AccessNode - this method has been made obsolete." << std::endl;
    std::cout << "MWGpico::AccessNode - MWGpico will try load all needed node and dump a warning" << std::endl;
    std::cout << "MWGpico::AccessNode - for the nodes that can't be found" << std::endl;
    std::cout << std::endl;
  }

  //! framework enumeration
  enum Framework {

    //! old framework (obsolete)
    MUT,

    //! new framework
    MUTOO

  };


  int _run_flag;
  void set_run_flag( const int run_flag)
  {_run_flag=run_flag;}


  bool _rpcnotracks_flag;
  void set_rpcnotracks_flag( const bool rpcnotracks_flag)
  {_rpcnotracks_flag=rpcnotracks_flag;}


  protected:

  //! retrieve nodes to be accessed from the nanoDST
  void GetNodes(PHCompositeNode *topNode);

  //! access/create new framework internal nodes
  int CreateNodeTree(PHCompositeNode *topNode);

  //! returns name of picoDST rootfile associated to picoDST type
  std::string get_picodst_rootfile(const MWGpicoDSTType& pico) const;

  //!@name dimuon ntuples
  //@{

  //! initialize ntuples based on run type
  void initialize_ntuples( void );

  //! book dimuon ntuple
  void BookDimuonsNtuple( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon B2B ntuple
  void BookDimuonsNtupleBackToBack( TNtuple *& dimuonsb2b, TString name, TString title );

  //! book dimuon ntuple (from run3 nanoDSTs)
  void BookDimuonsNtupleRun3( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run4 nanoDSTs)
  void BookDimuonsNtupleRun4( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run5 nanoDSTs)
  void BookDimuonsNtupleRun5( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run6 nanoDSTs)
  void BookDimuonsNtupleRun6( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run6 nanoDSTs)
  void BookDimuonsNtupleRun6BackToBack( TNtuple *& dimuonsb2b, TString name, TString title );

  //! book dimuon ntuple (for run7 nanoDSTs)
  void BookDimuonsNtupleRun7( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run8 nanoDSTs)
  void BookDimuonsNtupleRun8( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run8 nanoDSTs)
  void BookDimuonsNtupleRun8pp( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run8 nanoDSTs)
  void BookDimuonsNtupleRun8ppBackToBack( TNtuple *& dimuonsb2b, TString name, TString title );

  //! book dimuon ntuple (for run9 nanoDSTs)
  void BookDimuonsNtupleRun9( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run10 nanoDSTs)
  void BookDimuonsNtupleRun10( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run11 nanoDSTs)
  void BookDimuonsNtupleRun11( TNtuple *& dimuons, TString name, TString title );

  //! book dimuon ntuple (for run11 nanoDSTs, with trigger)
  void BookDimuonsNtupleRun11pp( TNtuple *& dimuons, TString name, TString title );
  void BookDimuonsNtupleRun11ppBackToBack( TNtuple *& dimuonsb2b, TString name, TString title );
  //! book dimuon ntuple (currently just copied from run11pp)
  void BookDimuonsNtupleRun12pp( TNtuple *& dimuons, TString name, TString title );
  void BookDimuonsNtupleRun12ppBackToBack( TNtuple *& dimuonsb2b, TString name, TString title );
  //! book dimuon ntuple (currently just copied from run11)
  void BookDimuonsNtupleRun12( TNtuple *& dimuons, TString name, TString title );


  //! book event mixing ntuple
  int BookEvtMixNtuple( TNtuple *& evtMix, TString name, TString title );

  //! book event mixing ntuple (for run3 nanoDSTs)
  void BookEvtMixNtupleRun3( TNtuple *& evtMix, TString name, TString title );

  //! book event mixing ntuple (for run4 nanoDSTs)
  void BookEvtMixNtupleRun4( TNtuple *& evtMix, TString name, TString title );

  //! fill dimuon ntuple
  int FillDimuons( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon B2B ntuple
  int FillDimuonsBackToBack( PHMuoTracksOut* &muo, TNtuple* dimuonsb2b );

  //! fill dimuon ntuple (from run3 nanoDSTs)
  int FillDimuonsRun3( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run4 nanoDSTs)
  int FillDimuonsRun4( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run5 nanoDSTs)
  int FillDimuonsRun5( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run5 nanoDSTs)
  int FillDimuonsRun5BackToBack( PHMuoTracksOut* &muo, TNtuple* dimuonsb2b );

  //! fill dimuon ntuple (for run6 nanoDSTs)
  int FillDimuonsRun6( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run5 nanoDSTs)
  int FillDimuonsRun6BackToBack( PHMuoTracksOut* &muo, TNtuple* dimuonsb2b );

  //! fill dimuon ntuple (for run7 nanoDSTs)
  int FillDimuonsRun7( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run8 nanoDSTs)
  int FillDimuonsRun8( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run8pp nanoDSTs)
  int FillDimuonsRun8pp( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run8 nanoDSTs)
  int FillDimuonsRun8ppBackToBack( PHMuoTracksOut* &muo, TNtuple* dimuonsb2b );

  //! fill dimuon ntuple (for run9 nanoDSTs)
  int FillDimuonsRun9( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run10 nanoDSTs)
  int FillDimuonsRun10( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run11 nanoDSTs)
  int FillDimuonsRun11( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! fill dimuon ntuple (for run11 nanoDSTs)
  int FillDimuonsRun11pp( PHMuoTracksOut* &muo, TNtuple* dimuons );
  int FillDimuonsRun11ppBackToBack( PHMuoTracksOut* &muo, TNtuple* dimuonsb2b );
  //! fill dimuon ntuple (currently jsut copied from run11pp)
  int FillDimuonsRun12pp( PHMuoTracksOut* &muo, TNtuple* dimuons );
  int FillDimuonsRun12ppBackToBack( PHMuoTracksOut* &muo, TNtuple* dimuonsb2b );

  //! fill dimuon ntuple (currently jsut copied from run11)
  int FillDimuonsRun12( PHMuoTracksOut* &muo, TNtuple* dimuons );

  //! store single muons for event mixing
  int StoreEvtMixMuons (PHMuoTracksOut* &muo);

  //! store single muons for event mixing (from run3 nanoDSTs)
  int StoreEvtMixMuonsRun3 (PHMuoTracksOut* &muo);

  //! store single muons for event mixing (from run3 nanoDSTs)
  int StoreEvtMixMuonsRun4 (PHMuoTracksOut* &muo);

  //! fill event mixing ntuple
  int FillEvtMix(TNtuple* evtMix);

  //! fill event mixing ntuple (from run3 nanoDSTs)
  int FillEvtMixRun3(TNtuple* evtMix);

  //! fill event mixing ntuple (from run4 nanoDSTs)
  int FillEvtMixRun4(TNtuple* evtMix);

  //@}

  //!@name single muons histograms/ntuple
  //@{

  //! book single muon ntuple
  void BookSngmuonsNtuple(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run3
  void BookSngmuonsNtupleRun3(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run4
  void BookSngmuonsNtupleRun4(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run5
  void BookSngmuonsNtupleRun5(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run6
  void BookSngmuonsNtupleRun6(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run7
  void BookSngmuonsNtupleRun7(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run8
  void BookSngmuonsNtupleRun8(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run8pp
  void BookSngmuonsNtupleRun8pp(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run9
  void BookSngmuonsNtupleRun9(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run12 pp 200 GeV
  void BookSngmuonsNtupleRun12pp200(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run12 CuAu                                                      
  void BookSngmuonsNtupleRun12CuAu(TNtuple*& sngmuons, TString m_name, TString m_title);

  //! book single muon ntuple: run13                                                     
  void BookSngmuonsNtupleRun13(TNtuple*& sngmuons, TString m_name, TString m_title);


  // book single muon evt ntuple
  void BookSngmuonsEvtNtuple(TNtuple*& sngvtx, TString v_name, TString v_title );

  // book single muon evt ntuple: run3
  void BookSngmuonsEvtNtupleRun3(TNtuple*& sngvtx, TString v_name, TString v_title );

  // book single muon evt ntuple: run4
  void BookSngmuonsEvtNtupleRun4(TNtuple*& sngvtx, TString v_name, TString v_title );

  // book single muon evt ntuple: run5
  void BookSngmuonsEvtNtupleRun5(TNtuple*& sngvtx, TString v_name, TString v_title );

  // book single muon evt ntuple: run6
  void BookSngmuonsEvtNtupleRun6(TNtuple*& sngvtx, TString v_name, TString v_title );

  // book single muon evt ntuple: run7
  void BookSngmuonsEvtNtupleRun7(TNtuple*& sngvtx, TString v_name, TString v_title );

  // book single muon evt ntuple: run8
  void BookSngmuonsEvtNtupleRun8(TNtuple*& sngvtx, TString v_name, TString v_title );

  // book single muon evt ntuple: run8pp
  void BookSngmuonsEvtNtupleRun8pp(TNtuple*& sngvtx, TString v_name, TString v_title );

  // book single muon evt ntuple: run9
  void BookSngmuonsEvtNtupleRun9(TNtuple*& sngvtx, TString v_name, TString v_title );

  //! book single muon ntuple: run12 pp 200 GeV
  void BookSngmuonsEvtNtupleRun12pp200(TNtuple*& sngvtx, TString v_name, TString v_title);

  //! book single muon ntuple: run12 CuAu                                                      
  void BookSngmuonsEvtNtupleRun12CuAu(TNtuple*& sngvtx, TString v_name, TString v_title);

  // book single muon evt ntuple: run13
  void BookSngmuonsEvtNtupleRun13(TNtuple*& sngvtx, TString v_name, TString v_title );


  //! fill single muon ntuple + evt ntuple
  int FillSngmuons(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx, bool golden=false);

  //! fill single muon ntuple + evt ntuple
  int FillSngmuonsNtp(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run3
  int FillSngmuonsNtpRun3(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run4
  int FillSngmuonsNtpRun4(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run5
  int FillSngmuonsNtpRun5(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run6
  int FillSngmuonsNtpRun6(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run7
  int FillSngmuonsNtpRun7(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run8
  int FillSngmuonsNtpRun8(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run8pp
  int FillSngmuonsNtpRun8pp(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run9
  int FillSngmuonsNtpRun9(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run12 pp 200 GeV
  int FillSngmuonsNtpRun12pp200(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx, bool golden=false);

  //! fill single muon ntuple + evt ntuple: run12 CuAu 
  int FillSngmuonsNtpRun12CuAu(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);

  //! fill single muon ntuple + evt ntuple: run13
  int FillSngmuonsNtpRun13(PHMuoTracksOut* &muo, TNtuple* sngmuons, TNtuple* sngvtx);


  //@}

  //!@name NEW single muons histograms/ntuple
  //@{

  //! book single muon ntuple
  void BookNewSngmuonsNtuple(TTree*& newsngmuons, TString m_name, TString m_title);
  //! book single muon ttree: run9
  void BookNewSngmuonsNtupleRun9(TTree*& newsngmuons, TString m_name, TString m_title);
  //! book single muon ttree: run11
  void BookNewSngmuonsNtupleRun11(TTree*& newsngmuons, TString m_name, TString m_title);
  //! book single muon ttree: run12
  void BookNewSngmuonsNtupleRun12(TTree*& newsngmuons, TString m_name, TString m_title);


  // book single muon evt ntuple
  void BookNewSngmuonsEvtNtuple(TNtuple*& newsngvtx, TString v_name, TString v_title );


  // book single muon evt ntuple: run9
  void BookNewSngmuonsEvtNtupleRun9(TNtuple*& newsngvtx, TString v_name, TString v_title );
  // book single muon evt ntuple: run11
  void BookNewSngmuonsEvtNtupleRun11(TNtuple*& newsngvtx, TString v_name, TString v_title );
  // book single muon evt ntuple: run12
  void BookNewSngmuonsEvtNtupleRun12(TNtuple*& newsngvtx, TString v_name, TString v_title );


  //! fill single muon ttree + evt ntuple
  int FillNewSngmuons(PHMuoTracksOut* &muo, TTree* newsngmuons, TNtuple* newsngvtx);

  //! fill single muon ntuple + evt ntuple
  int FillNewSngmuonsNtp(PHMuoTracksOut* &muo, TNtuple* newsngmuons, TNtuple* newsngvtx);

  //! fill single muon ntuple + evt ntuple: run9
  int FillNewSngmuonsNtpRun9(PHMuoTracksOut* &muo, TTree* newsngmuons, TNtuple* newsngvtx);

  //! fill single muon ntuple + evt ntuple: run11
  int FillNewSngmuonsNtpRun11(PHMuoTracksOut* &muo, TTree* newsngmuons, TNtuple* newsngvtx);
  //! fill single muon ntuple + evt ntuple: run12
  int FillNewSngmuonsNtpRun12(PHMuoTracksOut* &muo, TTree* newsngmuons, TNtuple* newsngvtx);

  //@}


  //!@name normalization histograms
  //@{

  //! create normalization histograms
  void BookHistograms( void );

  /*! \brief
    fill normalization histograms
    histograms have one entry per accepted events. They are used for normalization when
    calculating efficiencies
  */
  void FillHistograms( void );

  //@}

  //!@name utilities
  //@{

  //! returns number of muioo roads associated to a given track (new framework only)
  int GetNMuiooRoads( PHMuoTracksOut* &muo, int ipart );

  //! From possibly multiple roads to selected best road
  void MuiooRoadSelection( PHMuoTracksOut* &muo);

  //@}

  //!@name dumpers
  //@{

  //! dump most of the first nevent events
  void dump_events(PHGlobal* &evt, PHMuoTracksOut* &muo, int nevents = 1 );

  //! dump level1 trigger decision
  void dump_level1_triggers( void ) const;

  //@}

  private:

  //! returns true if ntuples for a given type have been booked
  bool Registered( MWGpico::MWGpicoDSTType type ) const
  { return _registered.find( type ) != _registered.end(); }

  //! register a given type to keep track of which ntuples have been booked
  void Register( MWGpico::MWGpicoDSTType type )
  { _registered.insert( type ); }

  //! associated cut object to select picoDST filling
  MWGCuts _cuts;

  //! run information from MWG nanoDSTs
  MWGRunInfo _run_info;

  //!@name configuration flags
  //@{

  //! data type (internal, depend on choice. Can be AuAu, pp, dAu)
  std::string _type;

  //! input configuration. input choice (pp, AuAu, simu, ...)
  std::string _choice;

  //! nanoDST build in location
  std::string _where;

  //! switch to select used framework
  Framework _framework;

  //! enumeration Run
  RunType _runtype;

  //@}

  //!@name required nodes
  //!@{

  //! top node
  PHCompositeNode* _top_node;

  //! run header
  RunHeader* run_header;

  //! run header
  EventHeader* event_header;

  //! event information
  PHGlobal *evt;

  //! RP information
  ReactionPlaneObject *rp;

  //! MC event header information
  headerWrapper* header;

  //! muon and dimuon information (old framework)
  PHMuoTracksOut *muo;

  //! muon and dimuon information (new framework)
  PHMuoTracksOut *muoo;

  //! trigger helper
  TriggerHelper *TrigHelp;

  //! TMutTrkMap
  TMutTrkMap * mut_trk_map;

  //! TMutMCTrkMap
  TMutMCTrkMap * mut_mctrk_map;

  //! TMutMCTrkMap
  TMutMCHitMap * mut_mchit_map;

  //! RpcHitMap
  TRpcHitMap * rpchit_map;

  //! RpcHodoHitMap
  TRpcHodoHitMap* rpchodohit_map;
  //! TMutHitMap 
  TMutHitMap* muthit_map1;

  //! TRpcMuoTrkMap
  TRpcMuoTrkMap* rpc_muotrk_map;


  MutrgTrkArray* fMutrgtrkArray;

  MutrgHitArray* fMutrgHitArray;

  TRpcCoordMap* rpccoord;  

  TRpcTrkMap* rpctrk_map2;
  
  BbcMultipleVtx* bbcm;

  FvtxConeTracklets * fvtx_cone;

  //! polariation information
  SpinDataEventOut *spin;

  //@}

  //!@name counters
  //@{

  //! total number of events counters
  int totEVT;

  //! accepted events counter
  int accEVT;

  //! accepted events counter
  int accEVT_backToBack;

  //! total number of dimuons counter
  int totDIMU;

  //! accepted dimuons counter
  int accDIMU;

  //! counter for dimuons with bad (not found) track
  int negDIMU;

  //! total single muons counter
  int totMU;

  //! accepterd single muons counter
  int accMU;

  //@}


  bool once;
  bool search_once;
  bool first;

  //!@name input
  //@{

  //! list of nanoDST files to be processed
  std::vector<TString> _nano_list;

  //! name of nanoDST file list input file
  std::string _nano_file;

  //@}

  //!@name output
  //@{

  //! picoDST type and file name
  typedef std::map< MWGpico::MWGpicoDSTType, std::string > FileMap;
  typedef std::set< MWGpico::MWGpicoDSTType > TypeSet;

  //! map between picoDST type and associated root file name
  FileMap _picos;

  //! stores picoDST types that have been booked
  TypeSet _registered;

  //! dimuon ntuple, old framework
  TNtuple *_dimuons;

  //! event mixing ntuple, old framework
  TNtuple *_evtMix;

  //! dimuon ntuple, new framework
  TNtuple *_dimuoos;

  //! dimuon ntuple, new framework
  TNtuple *_dimuoosb2b;

  //! event mixing ntuple, new framework
  TNtuple *_evtMixoo;

  //! Event mixing variables
  std::vector< std::vector<float> > MuInfos;

  //! stop filling event mixing ntuples when set to true
  bool _thestopflag;

  //! only accept muons that are in a dimuon pair w/ mass = 2.6-3.6 GeV/c^2
  bool _use_golden_mass_cut;

  //! centrality histogram
  TH1F* _centrality;

  //! zVtx histogram
  TH1F* _z_vertex;

  //! reaction plane
  TH1F* _reaction_plane;

  //! BBC Charge Calibrations:
  TH1F *_bbc_ch00;
  TH1F *_bbc_ch01;
  TH1F *_bbc_ch02;
  TH1F *_bbc_ch03;
  TH1F *_bbc_ch04;
  TH1F *_bbc_ch05;
  TH1F *_bbc_ch06;
  TH1F *_bbc_ch07;
  TH1F *_bbc_ch08;
  TH1F *_bbc_ch09;
  TH1F *_bbc_ch10;
  TH1F *_bbc_ch11;
  TH1F *_bbc_ch12;

  //! Monitoring Histograms
  TH2F *_bbc_NS;  
  TH2F *_zdc_NS;
  TH2F *_bbc_zdc;
  TH2F *_bbcN_zdcN;
  TH2F *_bbcS_zdcS;
  TH2F *_bbcN_zdcS;
  TH2F *_bbcS_zdcN;
  TH1F *_bbcCh_AllTriggers;

  //! single muon ntuple
  TNtuple *_sngmuons;

  //! single muon ttree
  TTree *_newsngmuons;


  //! single muon vertex/event ntuple
  TNtuple *_sngvtx;

  //! single muon vertex/event ntuple
  TNtuple *_newsngvtx;

//   //! mutr efficiency ntuple
//   MutEffic* _muteffic;

  //@}

  //! MC primary particles map
  TMCPrimaryMap* _mc_primary_map;

  //! level1 trigger helper
  TrigLvl1* _trig_lvl1;

};

#endif



