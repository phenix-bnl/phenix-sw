// $Id: MWGpico.C,v 1.118 2014/12/09 06:29:05 rseidl Exp $

/*!
  \file MWGpico.C
  \brief fun4all module to fill Muon picoDSTs from nanoDSTs
  \author Frederic Fleuret/Hugo Pereira
  \version $Revision: 1.118 $
  \date $Date: 2014/12/09 06:29:05 $
*/

#include <EventHeader.h>
#include <getClass.h>
#include <headerWrapper.h>
#include <PHGlobal.h>
#include <ReactionPlaneObject.h>
#include <PHMuoTracksOut.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <RunHeader.h>
#include <SpinDataEventOut.h>
#include <TriggerHelper.h>
#include <TFile.h>
#include <TMutTrkMap.h>
#include <TMutMCTrkMap.h>
#include<TMuiHitMapO.h>
#include <TMuiRoadMapO.h>
#include <TMutMCHitMap.h>
#include <TMCPrimaryMap.h>
#include <utiCentrality.h>
#include <FvtxConeTracklets.h>
#include <string>
#include <PHTFileServer.h>

#include <MuonUtil.h>

// local headers
#include <MUTOO.h>
#include <MWGVersion.h>
#include <Tools.h>

#include <MWGpico.h>

using namespace std;

//__________________________________________________
MWGpico::MWGpico():
  _framework( MUTOO ),
  _runtype( Unknown ),
  TrigHelp( 0 ),
  totEVT( 0 ),
  accEVT( 0 ),
  accEVT_backToBack( 0 ),
  totDIMU( 0 ),
  accDIMU( 0 ),
  negDIMU( 0 ),
  totMU( 0 ),
  accMU( 0 ),
  _nano_file( "file.txt" ),
  _use_golden_mass_cut( false ),
  _trig_lvl1( 0 )
{
  ThisName = "MWGpico";

  // initialize all ntuples and histograms to NULL
  _dimuons = 0;
  _dimuoos = 0;
  _dimuoosb2b = 0;
  _evtMix = 0;
  _evtMixoo = 0;
  _centrality = 0;
  _bbc_ch00 = 0;
  _bbc_ch01 = 0;
  _bbc_ch02 = 0;
  _bbc_ch03 = 0;
  _bbc_ch04 = 0;
  _bbc_ch05 = 0;
  _bbc_ch06 = 0;
  _bbc_ch07 = 0;
  _bbc_ch08 = 0;
  _bbc_ch09 = 0;
  _bbc_ch10 = 0;
  _bbc_ch11 = 0;
  _bbc_ch12 = 0;
  
  _bbc_NS = 0;
  _zdc_NS = 0;
  _bbc_zdc = 0;
  _bbcN_zdcN = 0;
  _bbcS_zdcS = 0;
  _bbcN_zdcS = 0;
  _bbcS_zdcN = 0;
  _bbcCh_AllTriggers = 0;

  _z_vertex = 0;
  _reaction_plane = 0;
  _sngmuons = 0;
  _newsngmuons = 0;
  _sngvtx = 0;
  _newsngvtx = 0;
  // _muteffic = 0;
  //  once = true;
  // search_once = true;

  _run_flag = -1;
  _rpcnotracks_flag = false;
  
  _cuts.init( "none" );
  return ;
}

//__________________________________________________
MWGpico::MWGpico( const char* choice, const char* where):
  _choice( choice ),
  _where( where ),
  _framework( MUTOO ),
  _runtype( Unknown ),
  TrigHelp( 0 ),
  totEVT( 0 ),
  accEVT( 0 ),
  accEVT_backToBack(0),
  totDIMU( 0 ),
  accDIMU( 0 ),
  negDIMU( 0 ),
  totMU( 0 ),
  accMU( 0 ),
  _nano_file( "file.txt" ),
  _use_golden_mass_cut( false ),
  _trig_lvl1( 0 )
{
  ThisName = "MWGpico";

  // initialize all ntuples and histograms to NULL
  _dimuons = 0;
  _dimuoos = 0;
  _dimuoosb2b = 0;
  _evtMix = 0;
  _evtMixoo = 0;
  _centrality = 0;
  _bbc_ch00 = 0;
  _bbc_ch01 = 0;
  _bbc_ch02 = 0;
  _bbc_ch03 = 0;
  _bbc_ch04 = 0;
  _bbc_ch05 = 0;
  _bbc_ch06 = 0;
  _bbc_ch07 = 0;
  _bbc_ch08 = 0;
  _bbc_ch09 = 0;
  _bbc_ch10 = 0;
  _bbc_ch11 = 0;
  _bbc_ch12 = 0;

  _bbc_NS = 0;  
  _zdc_NS = 0;
  _bbc_zdc = 0;
  _bbcN_zdcN = 0;
  _bbcS_zdcS = 0;
  _bbcN_zdcS = 0;
  _bbcS_zdcN = 0;
  _bbcCh_AllTriggers = 0;

  _z_vertex = 0;
  _reaction_plane = 0;
  _sngmuons = 0;
  _newsngmuons = 0;
  _sngvtx = 0;
  _newsngvtx = 0;
  // _muteffic = 0;
_run_flag = -1;
_rpcnotracks_flag = false;
 _cuts.init( "none" );
  return ;
}

//__________________________________________________
MWGpico::~MWGpico( void )
{
  if( TrigHelp ) delete TrigHelp;
  if( _dimuons ) delete _dimuons;
  if( _dimuoos ) delete _dimuoos;
  if( _dimuoosb2b ) delete _dimuoosb2b;
  if( _evtMix ) delete _evtMix;
  if( _evtMixoo ) delete _evtMixoo;
  if( _centrality ) delete _centrality;
  if( _bbc_ch00 ) delete _bbc_ch00;
  if( _bbc_ch01 ) delete _bbc_ch01;
  if( _bbc_ch02 ) delete _bbc_ch02;
  if( _bbc_ch03 ) delete _bbc_ch03;
  if( _bbc_ch04 ) delete _bbc_ch04;
  if( _bbc_ch05 ) delete _bbc_ch05;
  if( _bbc_ch06 ) delete _bbc_ch06;
  if( _bbc_ch07 ) delete _bbc_ch07;
  if( _bbc_ch08 ) delete _bbc_ch08;
  if( _bbc_ch09 ) delete _bbc_ch09;
  if( _bbc_ch10 ) delete _bbc_ch10;
  if( _bbc_ch11 ) delete _bbc_ch11;
  if( _bbc_ch12 ) delete _bbc_ch12;

  if( _bbc_NS )    delete _bbc_NS; 
  if( _zdc_NS )    delete _zdc_NS;
  if( _bbc_zdc ) delete _bbc_zdc;
  if( _bbcN_zdcN ) delete _bbcN_zdcN;
  if( _bbcS_zdcS ) delete _bbcS_zdcS;
  if( _bbcN_zdcS ) delete _bbcN_zdcS;
  if( _bbcS_zdcN ) delete _bbcS_zdcN;
  if( _bbcCh_AllTriggers ) delete _bbcCh_AllTriggers;

  if( _z_vertex ) delete _z_vertex;
  if( _reaction_plane ) delete _reaction_plane;
  if( _sngmuons ) delete _sngmuons;
  if( _newsngmuons ) delete _newsngmuons;
  if( _sngvtx ) delete _sngvtx;
  if( _newsngvtx ) delete _newsngvtx;
}

//__________________________________________________
int MWGpico::Init(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "MWGpico::Init" );

  //! load nanoDSTs
  LoadNano( _choice.c_str(), _where.c_str() );

  //! initilize global counters
  totEVT = 0;
  accEVT = 0;
  accEVT_backToBack = 0;
  totDIMU = 0;
  accDIMU = 0;
  totMU = 0;
  accMU=0;

  once = true;
  search_once = true;
  first = true;

  // initialize flag to stop processing the files
  _thestopflag = false;

  MUTOO::PRINT( cout, "**" );

  return 0;
}

//___________________________________________________________________
int MWGpico::CreateNodeTree(PHCompositeNode *top_node)
{
  try {

    // Instantiate nodes for muioo containers
    // it is instanciated locally only since used nowhere else
    PHNodeIterator nodeItr(top_node);
    PHCompositeNode *muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
    if(!muioo_node){

      muioo_node = new PHCompositeNode("MUIOO");
      top_node->addNode( muioo_node );

    }
  }	 catch (exception& e) { MUTOO::TRACE( e.what() ); }

  // Instantiate nodes for mutoo containers
  // it is instanciated locally only since used nowhere else
  PHNodeIterator nodeItr(top_node);
  PHCompositeNode *mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!mutoo_node){
    mutoo_node = new PHCompositeNode("MUTOO");
    top_node->addNode( mutoo_node );
  }

  try { TMutNode<TMutTrkMap>::find_node( mutoo_node, "TMutTrkMap" ); }
  catch (exception &e ) { MUTOO::TRACE( e.what() ); }

  try { MuonUtil::find_node<TMutMCTrkMap>( "TMutMCTrkMap" ); }
  catch (exception &e ) { MUTOO::TRACE( e.what() ); }

  try { MuonUtil::find_node<TMutMCHitMap>( "TMutMCHitMap" ); }
  catch (exception &e ) { MUTOO::TRACE( e.what() ); }

  try { MuonUtil::find_node<TMCPrimaryMap>( "TMCPrimaryMap" ); }
  catch (exception &e ) { MUTOO::TRACE( e.what() ); }

  return 0;
}

//______________________________________________________
int MWGpico::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "MWGPico::InitRun" );
  // Create Node Tree
  CreateNodeTree(top_node); // need to acess DST

  // make a TriggerHelper
  TrigHelp = new TriggerHelper(top_node);

  // nanoDST cuts
  PHInclusiveNanoCuts* nano_cuts(0);
  try { nano_cuts = TMutNode<PHInclusiveNanoCuts>::find_io_node(top_node,"MWGCuts"); }
  catch( exception &e )
  {
    nano_cuts = 0;
    cout << PHWHERE << "MWGpico::InitRun - PHInclusiveNanoCuts not found." << endl;
  }

  // RunHeader
  try { run_header = TMutNode<RunHeader>::find_io_node(top_node, "RunHeader"); }
  catch( exception &e )
  {
    run_header = 0;
    cout << "RunHeader not found." << endl;
    cout << "RunType: " << get_run_type_name() << endl;
  }

  if( run_header )
  {
    // use the run_header to check consistancy with the set_run_type flag
    int run_number( run_header->get_RunNumber() );
    cout << "RunNumber: " << run_number << endl;

    // if runtype is not set, try guess it based on run number
    if( get_run_type() == Unknown )
    {

      if( run_number < static_cast<int>(BEGIN_OF_RUN4) ) set_run_type( RUN3 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN5) ) set_run_type( RUN4 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN6) ) set_run_type( RUN5 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN7) ) set_run_type( RUN6 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN8) ) set_run_type( RUN7 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN9) ) set_run_type( RUN8 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN10) ) set_run_type( RUN9 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN11) ) set_run_type( RUN10 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN12) ) set_run_type( RUN11 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN13) ) set_run_type( RUN12 );
      else set_run_type( RUN13 );

      cout << "RunType selected automatically for run number " << run_number << ": " << get_run_type_name() << endl;
      cout << "if you want a different run type, add mwgpico->set_run_type( value ) to your macro, " << endl;
      cout << "with value one of RUN3, RUN4, RUN5, etc." << endl;

    } else {

      // check consistency between run number and selected run type
      bool consistent( true );

      if( run_number < static_cast<int>(BEGIN_OF_RUN3) ) consistent = false;
      else if( run_number < static_cast<int>(BEGIN_OF_RUN4) ) consistent = ( get_run_type() == RUN3 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN5) ) consistent = ( get_run_type() == RUN4 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN6) ) consistent = ( get_run_type() == RUN5 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN7) ) consistent = ( get_run_type() == RUN6 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN8) ) consistent = ( get_run_type() < RUN8pp );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN9) ) consistent = ( get_run_type() < RUN9 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN10) ) consistent = ( get_run_type() < RUN10 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN11) ) consistent = ( get_run_type() < RUN11 );   
      else if( run_number < static_cast<int>(BEGIN_OF_RUN12) ) consistent = ( get_run_type() < RUN12 );
      else if( run_number < static_cast<int>(BEGIN_OF_RUN13) ) consistent = ( get_run_type() < RUN13 );

      else consistent = ( get_run_type() >= RUN13 );

      if( !consistent )
      {
        cout << "WARNING: current run_number = " << run_number << endl;
        cout << "WARNING: is inconsistent with the _runtype selection (" << get_run_type_name() << ")" << endl;
      } else { cout << "run number: " << run_number << " RunType: " << get_run_type_name() << endl; }

    }

  }
  // initialize ntuples if needed
  initialize_ntuples();

  // update _run_info
  _run_info.update( run_header, nano_cuts );
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//______________________________________________________
int MWGpico::process_event(PHCompositeNode *top_node)
{
  if (first)
    GetNodes(top_node);
  totEVT++;

  // Dump as much events as the verbosity variable
  if(_dimuons && muo)	 {_framework = MUT; dump_events (evt, muo, (int)verbosity ); }
  if(_dimuoos && muoo) {_framework = MUTOO; dump_events (evt, muoo, (int)verbosity ); }

  // go from multiple road candidates in new framework to a single road
  // this has been made obsolete starting from RUN7 nanoDSTs.
  if (muoo && MWGVersion::get( muoo->GetName() ) < 10 )
  { MuiooRoadSelection(muoo); }

  // BLT decision
  if( _runtype == RUN3 || _runtype == RUN4 || _runtype == RUN5 )
  { Tools::LoadBLTDecision( top_node ); }

  // level2 decision
  if( _runtype == RUN4 || _runtype == RUN5 || _runtype == RUN7 || _runtype == RUN8 || _runtype ==RUN8pp ||  _runtype ==RUN9)
  {
    Tools::LoadL2Decision( top_node );
    Tools::LoadL2Primitives( top_node );
  }

  // LL1 decision
  if( ( _runtype == RUN5 || _runtype == RUN6 || _runtype == RUN8 || _runtype ==RUN8pp ||  _runtype ==RUN9 || _runtype ==RUN11 || _runtype == RUN11pp || _runtype ==RUN12 || _runtype ==RUN12pp || _runtype ==RUN12CuAu || _runtype == RUN13 ) )
  { Tools::LoadLL1Decision( top_node ); }

  if ( _dimuons && muo )
  {
    _framework = MUT;
    FillDimuons(muo, _dimuons);
  }

  if ( _evtMix && muo )
  {
    _framework = MUT;
    StoreEvtMixMuons (muo);
  }

  if ( _dimuoos && muoo )
  {
    _framework = MUTOO;
    FillDimuons(muoo, _dimuoos);
  }

  if ( _dimuoosb2b && muoo )
  {
    _framework = MUTOO;
    FillDimuonsBackToBack(muoo, _dimuoosb2b);
  }

  if ( _evtMixoo && muoo )
  {
    _framework = MUTOO;
    StoreEvtMixMuons (muoo);
  }

  if ( _sngmuons && muoo) FillSngmuons(muoo, _sngmuons, _sngvtx, _use_golden_mass_cut);
  if ( _newsngmuons && muoo) FillNewSngmuons(muoo, _newsngmuons, _newsngvtx);

  FillHistograms();

  // Beware, should mix mutoo and mut if done together !
  // cout << " test muteffic " << _muteffic << " muo " << muo << " muoo " << muoo << endl;
  // if ( _muteffic && muo && Cuts().pass_event_cuts(_top_node) ) _muteffic->Fill (muo);
  // if ( _muteffic && muoo && Cuts().pass_event_cuts(_top_node) ) _muteffic->Fill (muoo);

  return 0;
}

//______________________________________________________
int MWGpico::End(PHCompositeNode *top_node)
{

  MUTOO::PRINT( cout, "MWGPico::End" );

  // print run informations
  _run_info.print();

  // mutr efficiency
  // if (_muteffic) _muteffic->Dumptofile("muteffic.root");

  // close all files
  for( FileMap::iterator iter = _picos.begin(); iter != _picos.end(); iter ++ )
  {

    // for the HISTOGRAM type, do nothing
    if( iter->first == HISTOGRAMS ) continue;

    // change to TFile
    if( !PHTFileServer::get().cd( iter->second ) ) continue;

    // write RunInfo and Cuts (if not yet written)
    if( !gDirectory->FindKey( _run_info.GetName() ) ) _run_info.Write();
    //    if( !gDirectory->FindKey( _cuts.GetName() ) ) _cuts.Write();

    // write histograms (if not yet written)
    if( get_picodst_rootfile( HISTOGRAMS ).size() )
    {
      if( _centrality && !gDirectory->FindKey( _centrality->GetName() ) ) _centrality->Write();
      if( _bbc_ch00 && !gDirectory->FindKey( _bbc_ch00->GetName() ) ) _bbc_ch00->Write();
      if( _bbc_ch01 && !gDirectory->FindKey( _bbc_ch01->GetName() ) ) _bbc_ch01->Write();
      if( _bbc_ch02 && !gDirectory->FindKey( _bbc_ch02->GetName() ) ) _bbc_ch02->Write();
      if( _bbc_ch03 && !gDirectory->FindKey( _bbc_ch03->GetName() ) ) _bbc_ch03->Write();
      if( _bbc_ch04 && !gDirectory->FindKey( _bbc_ch04->GetName() ) ) _bbc_ch04->Write();
      if( _bbc_ch05 && !gDirectory->FindKey( _bbc_ch05->GetName() ) ) _bbc_ch05->Write();
      if( _bbc_ch06 && !gDirectory->FindKey( _bbc_ch06->GetName() ) ) _bbc_ch06->Write();
      if( _bbc_ch07 && !gDirectory->FindKey( _bbc_ch07->GetName() ) ) _bbc_ch07->Write();
      if( _bbc_ch08 && !gDirectory->FindKey( _bbc_ch08->GetName() ) ) _bbc_ch08->Write();
      if( _bbc_ch09 && !gDirectory->FindKey( _bbc_ch09->GetName() ) ) _bbc_ch09->Write();
      if( _bbc_ch10 && !gDirectory->FindKey( _bbc_ch10->GetName() ) ) _bbc_ch10->Write();
      if( _bbc_ch11 && !gDirectory->FindKey( _bbc_ch11->GetName() ) ) _bbc_ch11->Write();
      if( _bbc_ch12 && !gDirectory->FindKey( _bbc_ch12->GetName() ) ) _bbc_ch12->Write();
      if( _bbc_NS && !gDirectory->FindKey( _bbc_NS->GetName() ) ) _bbc_NS->Write();
      if( _zdc_NS && !gDirectory->FindKey( _zdc_NS->GetName() ) ) _zdc_NS->Write();
      if( _bbc_zdc && !gDirectory->FindKey( _bbc_zdc->GetName() ) ) _bbc_zdc->Write();
      if( _bbcN_zdcN && !gDirectory->FindKey( _bbcN_zdcN->GetName() ) ) _bbcN_zdcN->Write();
      if( _bbcS_zdcS && !gDirectory->FindKey( _bbcS_zdcS->GetName() ) ) _bbcS_zdcS->Write();
      if( _bbcN_zdcS && !gDirectory->FindKey( _bbcN_zdcS->GetName() ) ) _bbcN_zdcS->Write();
      if( _bbcS_zdcN && !gDirectory->FindKey( _bbcS_zdcN->GetName() ) ) _bbcS_zdcN->Write();
      if( _bbcCh_AllTriggers && !gDirectory->FindKey( _bbcCh_AllTriggers->GetName() ) ) _bbcCh_AllTriggers->Write();

      if( _z_vertex && !gDirectory->FindKey( _z_vertex->GetName() ) )     _z_vertex->Write();
      if( _reaction_plane && !gDirectory->FindKey( _reaction_plane->GetName() ) ) _reaction_plane->Write();
    }

    // close TFile
    PHTFileServer::get().write( iter->second );
  }

  // dump some statistics
  cout << accEVT << "/" << totEVT << " accepted events." << endl;
  cout << accEVT_backToBack << "/" << totEVT << " accepted events (back to back)." << endl;
  cout << accMU << "/" << totMU << " accepted single muons." << endl;
  cout << accDIMU << "/" << totDIMU << " accepted dimuons. (" << negDIMU << " dimuons with unassociated track)" << endl;

  return 0;
}

//______________________________________________________
void MWGpico::GetNodes(PHCompositeNode *top_node)
{

  //  static bool first = true;

  // store top node
  _top_node = top_node;

  //lvl1 trigger node
  try{ _trig_lvl1 = TMutNode<TrigLvl1>::find_io_node(top_node, "TrigLvl1"); }
  catch( exception &e )
  {
    _trig_lvl1 = 0;
    if( first ) cout<< PHWHERE << "TrigLvl1 does not exist"<<endl;
  }

  // event header
  try{ event_header = TMutNode<EventHeader>::find_io_node( top_node, "EventHeader" ); }
  catch( exception &e )
  {
    event_header = 0;
    if( first ) cout << PHWHERE << "MWGpico::GetNodes - EventHeader not in Node Tree" << endl;
  }

  // PHGlobal
  try{ evt = TMutNode<PHGlobal>::find_io_node(top_node, "PHGlobal" ); }
  catch( exception &e )
  {
    evt = 0;
    if( first ) cout << PHWHERE << "MWGpico::GetNodes - PHGlobal not in Node Tree" << endl;
  }

  // ReactionPlane
  try{ rp = TMutNode<ReactionPlaneObject>::find_io_node(top_node, "ReactionPlaneObject" ); }
  catch( exception &e )
  {
    rp = 0;
    if( first ) cout << PHWHERE << "MWGpico::GetNodes - ReactionPlaneObject not in Node Tree" << endl;
  }

  // PISA header
  try{ header = MuonUtil::find_io_node<headerWrapper>("header" ); }
  catch( exception &e )
  {
    header = 0;
    if( first ) cout << "MWGpico::GetNodes - header not in Node Tree" << endl;
  }

  // old framework MWG tracks
  try{ muo = TMutNode<PHMuoTracksOut>::find_io_node(top_node, "PHMuoTracks" ); }
  catch( exception &e )
  {
    muo = 0;
    if( first ) cout << "MWGpico::GetNodes - PHMuoTracks (old framework) not in Node Tree" << endl;
  }

  // new framework MWG tracks
  try{ muoo = TMutNode<PHMuoTracksOut>::find_io_node(top_node,"PHMuoTracksOO"); }
  catch( exception &e )
  {
    muoo = 0;
    if( first ) cout << "MWGpico::GetNodes - PHMuoTracksOO (new framework) not in Node Tree" << endl;
  }

  // spin information
  try{ spin = TMutNode<SpinDataEventOut>::find_io_node( top_node, "SpinDataEventOut" ); }
  catch( exception &e )
  {
    spin = 0;
    if( first ) cout << "MWGpico::GetNodes - SpinDataEventOut not in Node Tree" << endl;
  }

  // MUTR DST tracks (needed for single muon MC analysis  MXL
  try { mut_trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap"); }
  catch( exception &e )
  {
    mut_trk_map = 0;
    if( first ) cout << PHWHERE << "MWGpico::GetNodes - TMutTrkMap not in Node Tree" << endl;
  }

  // MUTR MC hits
  try { mut_mctrk_map = MuonUtil::find_node<TMutMCTrkMap>( "TMutMCTrkMap" ); }
  catch( exception &e )
  {
    mut_mctrk_map =0;
    if( first ) cout << PHWHERE << "MWGpico::GetNodes - TMutMCTrkMap not in Node Tree" << endl;
  }

  // MUTR MC hits
  // try { mut_mchit_map = TMutNode<TMutMCHitMap>::find_node(top_node,"TMutMCHitMap"); }
  try { mut_mchit_map = MuonUtil::find_node<TMutMCHitMap>("TMutMCHitMap"); }
  catch( exception &e )
  {
    mut_mchit_map =0;
    if( first ) cout << PHWHERE << "MWGpico::GetNodes - TMutMCHitMap not in Node Tree" << endl;
  }

  // MCPrimary
  try { _mc_primary_map = MuonUtil::find_node<TMCPrimaryMap>( "TMCPrimaryMap" ); }
  catch( exception &e )
  {
    _mc_primary_map =0;
    if( first ) cout << PHWHERE << "MWGpico::GetNodes - TMCPrimaryMap not in Node Tree" << endl;
  }


//RPCHitMap
  try { rpchit_map =  TMutNode<TRpcHitMap>::find_node( _top_node,"TRpcHitMap" );}
  catch( exception &e )
  {
     rpchit_map = 0;
     if( first ) cout << PHWHERE << "MWGpico::GetNodes - TRpcHitMap not in Node Tree" << endl;
//	 TRpcHitMap* hit_map =  TMutNode<TRpcHitMap>::find_node( _top_node,"TRpcHitMap" );
  }

//HodoHitMap
  try { rpchodohit_map =  TMutNode<TRpcHodoHitMap>::find_node( _top_node,"TRpcHodoHitMap" );}
  catch (exception &e ) {
     if( first ) cout << PHWHERE << "MWGpico::GetNodes - TRpcHodoHitMap not in Node Tree" << endl;
//     MUTOO::TRACE( e.what() );
     rpchodohit_map = NULL;}

//MutHitMap
  try { muthit_map1 =  TMutNode<TMutHitMap>::find_node( _top_node,"TMutHitMap" );}
  catch (exception &e) {
     if(first) cout << PHWHERE << "MWGpico::GetNodes - TMutHitMap not in Node Tree" << endl;
     muthit_map1 = NULL;
  }

//RPCMuoTrkMap
		  try { rpc_muotrk_map = TMutNode<TRpcMuoTrkMap>::find_node( _top_node, "TRpcMuoTrkMap" ); }
		  catch (exception &e ) {
     if(first) cout << PHWHERE << "MWGpico::GetNodes - TRpcMuoTrkMap not in Node Tree" << endl;
// MUTOO::TRACE( e.what() );
     rpc_muotrk_map = NULL;}

//MutrgHitArrary
	       try { fMutrgHitArray = TMutNode<MutrgHitArray>::find_io_node( _top_node, "MutrgHitArray" ); }
	       catch (exception &e ) {
     if(first) cout << PHWHERE << "MWGpico::GetNodes - MutrgHitArray not in Node Tree" << endl;
		  fMutrgHitArray = NULL;}

//MutrgTrkArray
	       try { fMutrgtrkArray = TMutNode<MutrgTrkArray>::find_io_node( _top_node, "MutrgTrkArray" ); }
	       catch (exception &e ) { //MUTOO::TRACE( e.what() );
		  if(first) cout << PHWHERE << "MWGpico::GetNodes - MutrgHitArray not in Node Tree" << endl;
		  fMutrgtrkArray = NULL;}

// RpcCoordMap
	       try { rpccoord = TMutNode<TRpcCoordMap>::find_node( _top_node,"TRpcCoordMap" ); }
	       catch (exception &e ) { //MUTOO::TRACE( e.what() );
		  if(first) cout << PHWHERE << "MWGpico::GetNodes - RpcCoord not in Node Tree" << endl;
		  rpccoord = NULL;}
	      
// RpcTrkMap
//	       printf("loading RpcTrkMap\n");
	       try { rpctrk_map2 = TMutNode<TRpcTrkMap>::find_node( _top_node, "TRpcTrkMap" ); }
	       catch (exception &e ) {// MUTOO::TRACE( e.what() );
		  if(first) cout << PHWHERE << "MWGpico::GetNodes - RpcTrkMap not in Node Tree" << endl;
	       rpctrk_map2 = NULL;}

// BbcMultipleVtx
//	       printf("loading bbcm\n");
	       try { bbcm = TMutNode<BbcMultipleVtx>::find_io_node( _top_node, "BbcMultipleVtx" ); }
	       catch (exception &e ) { //MUTOO::TRACE( e.what() );
		  if(first) cout << PHWHERE << "MWGpico::GetNodes - BbcMultVtx not in Node Tree" << endl;
		  bbcm = NULL;}


	       //	       printf("loading fvtx_cone\n");

	       const unsigned int muo_version = MWGVersion::get(muoo->ClassName());
	       //	       printf("MWG version %d\n",muo_version);
	       if (muo_version >= 15)
		 {
		   if (once)
		     {
		       once = false;
		       cout <<"mFillSingleMuonContainer::process_event - INFO - "
			    <<"Version "<<muo_version<<" of nDST is used in the TOP node. Fill cone observables as normal"
			    <<endl;
		     }
		 }
	       else  if (muo_version >= 13)
		 {
		     fvtx_cone = 0;
		   // directly load from FvtxConeTracklets
		   // this is intended as a fix for run12 510pp 1st production nDSTs
		   // this should not be used in other productions/analysis
 	
		     //	       printf("loading fvtx_cone\n");

		     if (search_once)
		     {
		       search_once = false;
		       
		       Fun4AllServer *se = Fun4AllServer::instance();
		       
		       fvtx_cone = dynamic_cast<FvtxConeTracklets *>(se->getSubsysReco( "FVTXCONETRACKLETS"));
		     }
		 }
	       else
		 fvtx_cone = 0;

	       //	       printf("after loading fvtx_cone\n");


  first = false;

  return;
}

//______________________________________________________
string MWGpico::get_picodst_rootfile( const MWGpico::MWGpicoDSTType& pico) const
{
  FileMap::const_iterator iter = _picos.find( pico );
  if( iter == _picos.end() ) return "";
  else return iter->second;
}


//_________________________________________________________
void MWGpico::initialize_ntuples( void )
{

  // initialize dimuons ntuples
  // old framework dimuon ntuple
  if( !( get_picodst_rootfile( DIMUONS ).empty() || Registered( DIMUONS ) ) )
  {
    PHTFileServer::get().open( get_picodst_rootfile( DIMUONS ).c_str(),"RECREATE");
    BookDimuonsNtuple( _dimuons, "dimuons","dimuons" );
    Register( DIMUONS );
  }

  // new framework dimuon ntuple
  if( !( get_picodst_rootfile( DIMUONSOO ).empty() || Registered( DIMUONSOO ) ) )
  {
    PHTFileServer::get().open( get_picodst_rootfile( DIMUONSOO ).c_str(),"RECREATE");
    BookDimuonsNtuple( _dimuoos, "dimuons","dimuoos" );
    BookDimuonsNtupleBackToBack( _dimuoosb2b, "dimuonsb2b","dimuoos" );
    Register( DIMUONSOO );
  }

  // old framework event mixing ntuple
  if( !( get_picodst_rootfile( EVTMIX ).empty() || Registered( EVTMIX ) ) )
  {
    PHTFileServer::get().open( get_picodst_rootfile( EVTMIX ).c_str(),"RECREATE");
    BookEvtMixNtuple( _evtMix, "evtMix", "evtMix" );
    Register( EVTMIX );
  }

  // new framework event mixing ntuple
  if( !( get_picodst_rootfile( EVTMIXOO ).empty() || Registered( EVTMIXOO ) ) )
  {
    PHTFileServer::get().open( get_picodst_rootfile( EVTMIXOO ).c_str(),"RECREATE");
    BookEvtMixNtuple( _evtMixoo, "evtMix", "evtMixoo" );
    Register( EVTMIXOO );
  }

  // histograms
  /*
    not that the histograms are not associated to a given TFile
    they are stored in any of the TFiles above when created
  */
  if( !( get_picodst_rootfile( HISTOGRAMS ).empty() || Registered( HISTOGRAMS ) ) )
  {
    gROOT->cd();
    BookHistograms();
    Register( HISTOGRAMS );
  }

  // initialize single muons ntuples
  if( !( get_picodst_rootfile( SNGMUONS ).empty() || Registered( SNGMUONS ) ) )
  {
    PHTFileServer::get().open( get_picodst_rootfile( SNGMUONS ).c_str(),"RECREATE");
    BookSngmuonsNtuple(_sngmuons, "sngmuons", "sngmuons");
    BookSngmuonsEvtNtuple(_sngvtx, "sngvtx", "vertex" );
    Register( SNGMUONS );
  }

  // initialize new single muons TTEE
  if( !( get_picodst_rootfile( NEWSNGMUONS ).empty() || Registered( NEWSNGMUONS ) ) )
  {
    //    cout << "initializing new muon ttree"<<endl;
    PHTFileServer::get().open( get_picodst_rootfile( NEWSNGMUONS ).c_str(),"RECREATE");
    BookNewSngmuonsNtuple(_newsngmuons, "newsngmuons", "newsngmuons");
    BookNewSngmuonsEvtNtuple(_newsngvtx, "newsngvtx", "vertex" );
    Register( NEWSNGMUONS );
  }

}

//______________________________________________________
void MWGpico::BookHistograms( void )
{
  if( !_centrality ) _centrality = new TH1F( "centrality", "centrality", 100, 0, 100 );

  if( !_bbc_ch00 ) _bbc_ch00 = new TH1F( "hBbcQtotal00", "bbcch00", 3000, 0, 3000 );
  if( !_bbc_ch01 ) _bbc_ch01 = new TH1F( "hBbcQtotal01", "bbcch01", 3000, 0, 3000 );
  if( !_bbc_ch02 ) _bbc_ch02 = new TH1F( "hBbcQtotal02", "bbcch02", 3000, 0, 3000 );
  if( !_bbc_ch03 ) _bbc_ch03 = new TH1F( "hBbcQtotal03", "bbcch03", 3000, 0, 3000 );
  if( !_bbc_ch04 ) _bbc_ch04 = new TH1F( "hBbcQtotal04", "bbcch04", 3000, 0, 3000 );
  if( !_bbc_ch05 ) _bbc_ch05 = new TH1F( "hBbcQtotal05", "bbcch05", 3000, 0, 3000 );
  if( !_bbc_ch06 ) _bbc_ch06 = new TH1F( "hBbcQtotal06", "bbcch06", 3000, 0, 3000 );
  if( !_bbc_ch07 ) _bbc_ch07 = new TH1F( "hBbcQtotal07", "bbcch07", 3000, 0, 3000 );
  if( !_bbc_ch08 ) _bbc_ch08 = new TH1F( "hBbcQtotal08", "bbcch08", 3000, 0, 3000 );
  if( !_bbc_ch09 ) _bbc_ch09 = new TH1F( "hBbcQtotal09", "bbcch09", 3000, 0, 3000 );
  if( !_bbc_ch10 ) _bbc_ch10 = new TH1F( "hBbcQtotal10", "bbcch10", 3000, 0, 3000 );
  if( !_bbc_ch11 ) _bbc_ch11 = new TH1F( "hBbcQtotal11", "bbcch11", 3000, 0, 3000 );
  if( !_bbc_ch12 ) _bbc_ch12 = new TH1F( "hBbcQtotal12", "bbcch12", 3000, 0, 3000 );

  if( !_bbc_NS ) _bbc_NS = new TH2F( "bbc_NS", "bbcNS", 175, 0, 1750 , 175, 0, 1750 );
  if( !_zdc_NS ) _zdc_NS = new TH2F( "zdc_NS", "zdcNS", 125, 0, 5000 , 125, 0, 5000 );
  if( !_bbc_zdc ) _bbc_zdc = new TH2F( "bbc_zdc", "bbc_zdc", 150, 0, 3000 , 200, 0, 8000 );
  if( !_bbcN_zdcN ) _bbcN_zdcN = new TH2F( "bbcN_zdcN", "bbcN_zdcN", 175, 0, 1750 , 125, 0, 5000 );
  if( !_bbcS_zdcS ) _bbcS_zdcS = new TH2F( "bbcS_zdcS", "bbcS_zdcS", 175, 0, 1750 , 125, 0, 5000 );
  if( !_bbcN_zdcS ) _bbcN_zdcS = new TH2F( "bbcN_zdcS", "bbcN_zdcS", 175, 0, 1750 , 125, 0, 5000 );
  if( !_bbcS_zdcN ) _bbcS_zdcN = new TH2F( "bbcS_zdcN", "bbcS_zdcN", 175, 0, 1750 , 125, 0, 5000 );
  if( !_bbcCh_AllTriggers ) _bbcCh_AllTriggers = new TH1F( "bbcCh_AllTriggers", "bbcCh_AllTriggers", 3000, 0, 3000 );


  if( !_z_vertex ) _z_vertex = new TH1F( "z_vertex", "z_vertex", 100, -50, 50 );
  if( !_reaction_plane ) _reaction_plane = new TH1F( "reaction_plane", "reaction_plane", 100, -1.8, 1.8 );
  return;
}

//______________________________________________________
void MWGpico::FillHistograms( void )
{

  if( verbosity >= 2 ) cout << "MWGpico::FillHistograms" << endl;
  if( !( evt && Cuts().pass_event_cuts(_top_node) ) ) return;

  // fill centrality histogram
  if( _centrality ) {

    if( _runtype == RUN4 ) _centrality->Fill( PhUtilities::getCentralityByClockRun4( _top_node ) );
    else if( _runtype == RUN5 || _runtype == RUN7 || _runtype == RUN8 ) _centrality->Fill( evt->getCentrality() );
    else if( _runtype == RUN10 || _runtype == RUN11 || _runtype == RUN12 || _runtype == RUN12CuAu) _centrality->Fill( evt->getCentrality() );
    else {
      static bool first( true );
      if( first ) cout << "MWGpico::FillHistograms - centrality histogram is not implemented properly for runType " << _runtype << endl;
      first = false;
    }

  }

  //fill centrality calibration variables
  if( _bbc_ch00 && evt &&
      TrigHelp->trigScaled("BBCLL1(>1 tubes)") // Require the 30cm trigger
      )
    {
      float bbcZ = evt->getBbcZVertex();
      float bbcQtot = evt->getBbcChargeN()+evt->getBbcChargeS();
      if(bbcZ>=-30  &&  bbcZ<30  ) { _bbc_ch00->Fill(bbcQtot); }
      if(bbcZ>=-30  &&  bbcZ<-25 ) { _bbc_ch01->Fill(bbcQtot); }
      if(bbcZ>=-25  &&  bbcZ<-20 ) { _bbc_ch02->Fill(bbcQtot); }
      if(bbcZ>=-20  &&  bbcZ<-15 ) { _bbc_ch03->Fill(bbcQtot); }
      if(bbcZ>=-15  &&  bbcZ<-10 ) { _bbc_ch04->Fill(bbcQtot); }
      if(bbcZ>=-10  &&  bbcZ<-5  ) { _bbc_ch05->Fill(bbcQtot); }
      if(bbcZ>=-5   &&  bbcZ<0   ) { _bbc_ch06->Fill(bbcQtot); }
      if(bbcZ>=0    &&  bbcZ<5   ) { _bbc_ch07->Fill(bbcQtot); }
      if(bbcZ>=5    &&  bbcZ<10  ) { _bbc_ch08->Fill(bbcQtot); }
      if(bbcZ>=10   &&  bbcZ<15  ) { _bbc_ch09->Fill(bbcQtot); }
      if(bbcZ>=15   &&  bbcZ<20  ) { _bbc_ch10->Fill(bbcQtot); }
      if(bbcZ>=20   &&  bbcZ<25  ) { _bbc_ch11->Fill(bbcQtot); }
      if(bbcZ>=25   &&  bbcZ<30  ) { _bbc_ch12->Fill(bbcQtot); }
      
    }

  //Some Monitoring Histograms
  if( _bbc_NS && evt &&
      TrigHelp->trigScaled("BBCLL1(>1 tubes) narrowvtx") // Require the 12cm trigger
      )
    {
      float bbcZ = evt->getBbcZVertex();
      if(bbcZ>=-10  &&  bbcZ<10  )
	{
	  _bbc_NS->Fill(evt->getBbcChargeN(),evt->getBbcChargeS()); 
	  _zdc_NS->Fill(evt->getZdcEnergyN(),evt->getZdcEnergyS()); 
	  _bbc_zdc->Fill(evt->getBbcChargeN()+evt->getBbcChargeS(),
			 evt->getZdcEnergyN()+evt->getZdcEnergyS());
	  _bbcN_zdcN->Fill(evt->getBbcChargeN(),evt->getZdcEnergyN());
	  _bbcS_zdcS->Fill(evt->getBbcChargeS(),evt->getZdcEnergyS());
	  _bbcN_zdcS->Fill(evt->getBbcChargeN(),evt->getZdcEnergyS());
	  _bbcS_zdcN->Fill(evt->getBbcChargeS(),evt->getZdcEnergyN());
	}
    }
  
  //For Event Normalisation
  
  if( _bbcCh_AllTriggers && evt )
    {
      float bbcZ = evt->getBbcZVertex();
      float bbcQtot = evt->getBbcChargeN()+evt->getBbcChargeS();
      if(bbcZ>=-30  &&  bbcZ<30 ) { _bbcCh_AllTriggers->Fill(bbcQtot); }
    }
  
  // fill z_vertex histogram
  if( _z_vertex ) {

    double BbcZVertex( -9999 );

    if( header && (_choice == "simu" || _choice == "simu_file") )
    {
    // try get vertex from PISA header
      if( verbosity >= 2 ) cout << "MWGpico::FillHistograms - using MC vertex" << endl;
      bool error( false );
      BbcZVertex = Tools::zVertexMC( header, error );
    } else {

      // try get vertex from PHGlobal
 
      if ( evt)
	BbcZVertex = evt->getBbcZVertex();
      if( verbosity >= 2 ) cout << "MWGpico::FillHistograms - using BBC vertex: " << BbcZVertex << endl;

    }

    // fill vertex histogram
    _z_vertex->Fill( BbcZVertex );

  }

  // reaction plane
  //if( _reaction_plane && rp && !((_choice == "simu" || _choice == "simu_file") ))
  if( _reaction_plane && rp )
    { /*_reaction_plane->Fill( rp->getRXNrp18() );*/ }

  return;

}

//______________________________________________________
void MWGpico::MuiooRoadSelection(PHMuoTracksOut* &muo)
{
  // From muioo tri-road info into the single road variables
  if( !muo ) return;

  for ( unsigned int ipart=0; ipart<muo->get_npart(); ipart++) {
    int iroad = Cuts().get_best_road_oo( ipart, muo );
    if( iroad < 0 ) continue;

    for (int igapcoor=0; igapcoor<5; igapcoor++)
    { muo->set_muID_gap0( igapcoor, ipart, muo->get_muIDOO_gap0(igapcoor,iroad,ipart)); }

    // store the chi-square for the best in the 1st slot (avoid confusion later)
    muo->set_muIDOOchi(0, ipart, muo->get_muIDOOchi(iroad, ipart));

    // set hit pattern
    int muioo( muo->get_muIDOOhits( iroad, ipart ) );
    muo->set_muIDhits( ipart, Tools::get_muid_hit_pattern( muioo ) );
  }

  return;
}

//______________________________________________________
int MWGpico::GetNMuiooRoads(PHMuoTracksOut* &muo, int i_part)
{
  if( !muo ) return 0;

  int out( 0 );
  for( int i_road=0; i_road<3; i_road++ )
  if( muo->get_muIDOOhits( i_road, i_part ) ) out++;

  return out;
}

//______________________________________________________
void MWGpico::dump_events(PHGlobal* &evt, PHMuoTracksOut* &muo, int nevents )
{

  // make local event counter for mut and mutoo !
  static int local_event[2]={0,0};
  static string _frameworkname[2]={"MUT","MUTOO"};

  local_event[_framework]++;
  if( local_event[_framework] > nevents ) return;

  ostringstream what;
  if (muo) { // check PHMuoTracks or PHMuoTracksOO exist
    int ndimu = muo->get_ndimu();
    int npart = muo->get_npart();
    what << "Dumping " << _frameworkname[_framework] << " event #" << local_event[_framework] << " with " << npart << " tracks & " << ndimu << " dimuons" ;
    MUTOO::PRINT( cout, what.str().c_str() );

    if( event_header ) cout << "Evt:" << event_header->get_EvtSequence();
    if( run_header ) cout << " Run: " <<	run_header->get_RunNumber();
    if( evt ) cout << " Bbc: " << evt->getBbcZVertex() << endl;

    for (int ipart=0; ipart<npart; ipart++) { // single particle loop
      cout << "Trak #" << ipart
	   << " Ch: " << muo->get_charge(ipart)
	   << " Px: " << muo->get_px(0,ipart)
	   << " Py: " << muo->get_py(0,ipart)
	   << " Pz: " << muo->get_pz(0,ipart)
	   << " Zv: " << muo->get_zpos(0,ipart)
	   << " Hits: " << muo->get_muTRhits(ipart)
	   << " Chi2: " << muo->get_chisquare(ipart) << endl;
      if (_framework==MUT)
	cout << "> Road Hits: " << muo->get_muIDhits(ipart)
	     << " X: " << muo->get_muID_gap0(0,ipart)
	     << " Y: " << muo->get_muID_gap0(1,ipart)
	     <<	" Z: " << muo->get_muID_gap0(2,ipart)
	     << " dX: " << muo->get_muID_gap0(3,ipart)
	     << " dY: " << muo->get_muID_gap0(4,ipart) << endl;
      if (_framework==MUTOO) for (int iroad=0; iroad<3; iroad++) { // internal road loop
	if (muo->get_muIDOOhits(iroad,ipart)!=0)
	  cout << "> Road " << iroad
	       << " Hits: "	<< muo->get_muIDOOhits(iroad,ipart)
	       << " X: " << muo->get_muIDOO_gap0(0,iroad,ipart)
	       << " Y: " << muo->get_muIDOO_gap0(1,iroad,ipart)
	       <<	" Z: " << muo->get_muIDOO_gap0(2,iroad,ipart)
	       << " dX: " << muo->get_muIDOO_gap0(3,iroad,ipart)
	       << " dY: " << muo->get_muIDOO_gap0(4,iroad,ipart) << endl;
      }
    }

    for (int idimu=0; idimu<ndimu; idimu++) { // dimuon loop
      if (_framework==MUT)
	cout << "Dimu #" << idimu
	     << " Trks: " << muo->get_ditrkIndex(0,idimu) << " & " << muo->get_ditrkIndex(1,idimu)
	     << " Mass: " << muo->get_dimass(idimu) << endl;
      if (_framework==MUTOO)
	cout << "Dimu #" << idimu
	     << " Trks: " << muo->get_ditrkIndex(0,idimu) << " & " << muo->get_ditrkIndex(1,idimu)
	     << " Mass: " << muo->get_dimass(idimu)
	     << " Z: " << muo->get_vtx_zpos(idimu)
	     << " Chi2: " << muo->get_vtx_chisquare(idimu) << endl;
    }
  }
}

//_________________________________________________________________
void MWGpico::dump_level1_triggers( void ) const
{
  if( !TrigHelp ) return;
  const char* trig_names[] =
  {
    "MUIDS_1D1S&BBCLL1",
    "MUIDN_1D1S&BBCLL1",
    "MUIDS_2DS&BBCLL1",
    "MUIDN_2DS&BBCLL1",
    "MUIDS_1D&BBCLL1",
    "MUIDN_1D&BBCLL1",
    "MUIDS_1S&BBCLL1",
    "MUIDN_1S&BBCLL1",
    0
  };

  for( unsigned int i=0; trig_names[i]; i++ )
  if( TrigHelp->didLevel1TriggerGetScaled( trig_names[i] ) )
  cout << "MWGpico::dump_l1_trigger - trigger " << trig_names[i] << " fired" << endl;
  return;
}
