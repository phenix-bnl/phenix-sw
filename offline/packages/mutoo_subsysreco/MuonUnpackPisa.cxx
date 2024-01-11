// $Id: MuonUnpackPisa.cxx,v 1.48 2019/06/07 16:50:17 slash Exp $
#include "MuonUnpackPisa.h"
#include "MuonUtil.h"

#include <recoConsts.h>

// MUTOO IOC includes
#include<TMCPrimaryMap.h>
#include<TMutHitMap.h>
#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<TMuiMCHitMapO.h>
#include<TMuiHitMapO.h>

// pisa mu/mui hit table
#include <mumhitsWrapper.h>
#include <munhitsWrapper.h>
#include <fkinWrapper.h>
#include <primaryWrapper.h>

#include <KinGetGEA.h>
#include <MutGetGEA.h>
#include <MuiGetGEA.h>

// MUTOO module.
#include <mMuiSlowSim.h>
#include <mMuiResponse.h>

#include <mMutSlowSim.h>
#include <mMutResponse.h>
#include <mMutCalibrate.h>
#include <mMutZeroSup.h>

// Include online runtime parameter.
#include<mMuiSlowSimPar.h>
#include<mMuiResponsePar.h>
#include<mMutSlowSimPar.h>
#include<mMutResponsePar.h>
#include<mMutZeroSupPar.h>

#include <getClass.h>
#include<RunHeader.h>

// MUTOO
#include <TMuiHVMask.h>
#include <TMutMathiesonPar.h>

#include<PHTimer.h>
#include<PHTimeServer.h>

#include <odbc++/connection.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>

#include<iostream>
#include <sstream>
#include<fstream>

using namespace odbc;
using namespace std;

//______________________________________________________
MuonUnpackPisa::MuonUnpackPisa( const char* name ) :
  MuonSubsysReco( name ),
  _timer( PHTimeServer::get()->insert_new( name ) ),
  _flags( NONE ),
  _check_bbc_rate( false )
{

  for(int i=0;i<18;i++) _mutr_correlation_width[i]=0.104;

  for(int i=0;i<1024;i++)
    {
      _mutr_base_efficiency[i]=0.98;
      _chamber_eff0[i]=0.98;
      _chamber_eff1[i]=0;
    }
}

//______________________________________________________
MuonUnpackPisa::~MuonUnpackPisa()
{}

//______________________________________________________
int MuonUnpackPisa::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );

  MUTOO::PRINT( cout, "MuonUnpackPisa::Init" );

  cout << "flags: " << _flags << endl;
  cout << "DO_RESPONSE: " << ( get_flag( DO_RESPONSE ) ? "true":"false" ) << endl;
  cout << "NO_ZERO_SUP: " << ( get_flag( NO_ZERO_SUP ) ? "true":"false" ) << endl;
  cout << "NO_CALIBRATE : " << ( get_flag( NO_CALIBRATE ) ? "true":"false" ) << endl;
  cout << "NO_CHARGE_SMEAR : " << ( get_flag( NO_CHARGE_SMEAR ) ? "true":"false" ) << endl;

  MUTOO::PRINT( cout, "**" );
  return 0;
}

//______________________________________________________
int MuonUnpackPisa::InitRun(PHCompositeNode *top_node)
{

  MUTOO::PRINT( cout, "MuonUnpackPisa::InitRun" );

  // initialize database
  MuonUtil::initialize_database( top_node );

  //extract the BBC rate of this run for chamber efficiency calculation
  if (_check_bbc_rate)
    {
      float bbcrate = get_bbcrate(top_node);
      for (int arm=0; arm<2; arm++)
	for(int station=0;station<MUTOO::NumberOfStations;station++)
	  for(int gap=0;gap<MUTOO::NumberOfGaps;gap++)
	    for(int pla=0;pla<MUTOO::NumberOfPlanes;pla++)
	      for (int octant=0; octant<MUTOO::MAX_OCTANT; octant++)
		for (int hoct=0; hoct<MUTOO::MAX_HALF_OCTANT; hoct++)
		  {
		    int index = pla + MUTOO::NumberOfPlanes*(hoct + MUTOO::MAX_HALF_OCTANT *(octant + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
		    float eff = _chamber_eff0[index] + _chamber_eff1[index]*bbcrate;
		    set_mutr_base_efficiency(arm, station, gap, pla, octant, hoct, eff);
		  }
    }

  // Create Node Tree
  CreateNodeTree(top_node);

  MUTOO::PRINT( cout, "**" );
  return 0;
}

//__________________________________________________________________________
int MuonUnpackPisa::CreateNodeTree(PHCompositeNode *top_node)
{

  // Instantiate nodes for mutoo containers
  {
    PHNodeIterator nodeItr(top_node);
    mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    if(!mutoo_node)
    {
      mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(mutoo_node);
    }
  }

  // Instantiate nodes for muioo containers
  {
    PHNodeIterator nodeItr(top_node);
    muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
    if(!muioo_node)
    {
      muioo_node = new PHCompositeNode("MUIOO");
      top_node->addNode(muioo_node);
    }
  }


  {
    PHNodeIterator nodeItr(top_node);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!dst_node)
    {
      dst_node = new PHCompositeNode("DST");
      top_node->addNode(dst_node);
    }
  }

  // Interface Object Containers (IOCS)
  // muid
  TMuiHitMapO* muihit_map = TMutNode<TMuiHitMapO>::new_node(muioo_node,"TMuiHitMapO");
  TMuiMCHitMapO* mc_muihit_map = TMutNode<TMuiMCHitMapO>::new_node(muioo_node,"TMuiMCHitMapO");

  // muon tracker
  TMCPrimaryMap* mc_primary_map = TMutNode<TMCPrimaryMap>::new_node(mutoo_node, "TMCPrimaryMap");
  TMutHitMap* hit_map = TMutNode<TMutHitMap>::new_node(mutoo_node, "TMutHitMap");
  TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::new_node(mutoo_node,"TMutMCTrkMap");
  TMutMCHitMap* mc_hit_map = TMutNode<TMutMCHitMap>::new_node(mutoo_node,"TMutMCHitMap");

  // Set up pisa takes needed for mut/muioo slowsim
  PHNodeIterator nodeIter(top_node);
  PHCompositeNode *geaNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "GEA"));

  mumhitsWrapper* mumhits = new mumhitsWrapper("mumhits", 60000);
  PHIODataNode<PHTable>* mumhitsNode = new PHIODataNode<PHTable>(mumhits, "mumhits");
  geaNode->addNode(mumhitsNode);

  munhitsWrapper* munhits = new munhitsWrapper("munhits", 60000);
  PHIODataNode<PHTable>* munhitsNode = new PHIODataNode<PHTable>(munhits, "munhits");
  geaNode->addNode(munhitsNode);

  fkinWrapper* fkins = new fkinWrapper("fkin", 60000);
  PHIODataNode<PHTable>* fkinNode = new PHIODataNode<PHTable>(fkins, "fkin");
  geaNode->addNode(fkinNode);

  primaryWrapper* primarys = new primaryWrapper("primary", 60000);
  PHIODataNode<PHTable>* primaryNode = new PHIODataNode<PHTable>(primarys, "primary");
  geaNode->addNode(primaryNode);

  // Make IOCs persistent here
  //
  mc_primary_map->make_persistant(dst_node,"TMCPrimary");
  mc_hit_map->make_persistant(dst_node,"TMutMCHit");
  mc_trk_map->make_persistant(dst_node,"TMutMCTrk");
  mc_muihit_map->make_persistant(dst_node,"TMuiMCHitO");

  if( get_flag( DO_RESPONSE ) )
  {
    cout << "MuonUnpackPisa::CreateNodeTree - response required, make hit map persistant. " << endl;
    muihit_map->make_persistant(dst_node,"TMuiHitO");
    hit_map->make_persistant(dst_node,"TMutHit");
  }

  // Module parameter tables
  // muid slow simulator
  mMuiSlowSimPar* mMuiSlowSim_par = TMutNode<mMuiSlowSimPar>::new_node(muioo_node,"mMuiSlowSimPar");
  mMuiSlowSim_par->set_verbosity(MUIOO::NONE);

  // muid response
  mMuiResponsePar* mMuiResponse_par = TMutNode<mMuiResponsePar>::new_node(muioo_node,"mMuiResponsePar");
  mMuiResponse_par->set_verbosity(MUIOO::NONE);
  mMuiResponse_par->set_use_hv_mask(true);

  // try load overriden efficiency from recoConsts
  if( recoConsts::instance()->FlagExist("MUIOO_TUBE_EFF") )
  {
    MUTOO::PRINT( cout, "MuonUnpackPisa::CreateNodeTree" );
    cout << "The use of setDoubleFlag( \"MUIOO_TUBE_EFF\", eff ) is obsolete" << endl;
    cout << "Better use: " << endl;
    cout << "  TMuiHVMask::set_mode( TMuiHVMask::FIXED_VALUE );" << endl;
    cout << "  TMuiHVMask::set_effic_twopack( eff );" << endl;
    cout << "in your macro." << endl;
    double efficiency = recoConsts::instance()->get_DoubleFlag( "MUIOO_TUBE_EFF", 1.0 );
    TMuiHVMask::set_mode( TMuiHVMask::FIXED_VALUE );
    TMuiHVMask::set_effic_twopack( efficiency );
  }

  // mutr slow simulator
  mMutSlowSimPar* mMutSlowSim_par = TMutNode<mMutSlowSimPar>::new_node(mutoo_node,"mMutSlowSimPar");
  mMutSlowSim_par->set_verbosity( MUTOO::NONE );
  for(int arm=0; arm<MUTOO::NumberOfArms; arm++)
  for(int station=0; station <MUTOO::NumberOfStations; station ++)
  for(int gap=0; gap < MUTOO::NumberOfGaps; gap++) {
    if(station ==2 && gap ==2 ) continue;
    mMutSlowSim_par->set_correlation_width( arm, station, gap, _mutr_correlation_width[gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm)]  );
  }

  // mutr response
  mMutResponsePar* mMutResponse_par = TMutNode<mMutResponsePar>::new_node(mutoo_node,"mMutResponsePar");
  mMutResponse_par->set_verbosity( MUTOO::NONE );
  if ( get_flag( ADD_NOISE ) ) mMutResponse_par->set_noise_flag( true );
  if( get_flag( NO_CHARGE_SMEAR ) ) mMutResponse_par->set_smear_q( false );

  for(int arm=0; arm<MUTOO::NumberOfArms; arm++)
  for(int station=0; station <MUTOO::NumberOfStations; station ++)
  for(int gap=0; gap < MUTOO::NumberOfGaps; gap++)
  for(int pla=0; pla < MUTOO::NumberOfPlanes; pla++)
  for(int oct=0; oct < MUTOO::MAX_OCTANT; oct++)
  for(int hoct=0; hoct < MUTOO::MAX_HALF_OCTANT; hoct++)
  {
    if(station == 2 && gap == 2) continue;
    int index =  pla + MUTOO::NumberOfPlanes*(hoct + MUTOO::MAX_HALF_OCTANT *(oct + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
    if( _mutr_base_efficiency[index]>=0 )
      mMutResponse_par->set_chamber_efficiency( arm, station, gap, pla, oct, hoct, _mutr_base_efficiency[index]  );
  }

  // zero suppression parameters.
  mMutZeroSupPar *mMutZeroSup_par = TMutNode<mMutZeroSupPar>::new_node( mutoo_node, "mMutZeroSupPar" );
  mMutZeroSup_par->set_mode( mMutZeroSupPar::DCM );

  // calibration
  mMutCalibratePar* mMutCalibrate_par = TMutNode<mMutCalibratePar>::new_node(mutoo_node,"mMutCalibratePar");
  mMutCalibrate_par->set_verbosity( MUTOO::NONE );
  mMutCalibrate_par->set_fit_type( mMutCalibratePar::AVERAGE );
  mMutCalibrate_par->set_t_sample(0,100);
  mMutCalibrate_par->set_t_sample(1,450);
  mMutCalibrate_par->set_t_sample(2,600);
  mMutCalibrate_par->set_t_sample(3,700);

  // dump parameters if needed.
  if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
  {

    mMutSlowSim_par->print();
    TMutMathiesonPar::print();

    if( get_flag( DO_RESPONSE ) )
    {
      mMutResponse_par->print();
      mMutZeroSup_par->print();
      mMutCalibrate_par->print();
    }
  }

  return 0;
}

//________________________________________________________________________________________
int MuonUnpackPisa::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();

  // Call MUTOO modules for track momentum reconstruction and vertex finding
  try {

    // load external vertex
    load_vertex_if_needed( top_node );

    // Fill mumhits table from PISA tree
    KinGetGEA(top_node);
    MutGetGEA(top_node);
    MuiGetGEA(top_node);

    // Call mutoo slow simulator and response
    // Be careful with the sequence now, mutr first
    // muid later, but it will fix soon.
    _mMutSlowSim_mod.event(top_node);
    _mMuiSlowSim_mod.event(top_node);

    // response modules
    if( get_flag( DO_RESPONSE ) )
    {
      _mMuiResponse_mod.event(top_node);
      _mMutResponse_mod.event(top_node);
      if( !get_flag( NO_ZERO_SUP ) ) _mMutZeroSup_mod.event( top_node );
      if( !get_flag( NO_CALIBRATE ) ) _mMutCalibrate_mod.event(top_node);
    }

    write_maps_if_needed();

  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }

  _timer.get()->stop();

  return 0;

}

//____________________________________________________________
int MuonUnpackPisa::End(PHCompositeNode* top_node)
{

  // print this module timer statistics
//   _timer.get()->print_stat();

  _mMutSlowSim_mod.print_summary();

  // dump zero suppression summary
  // this is usefull only if the module was used, when get_flag( DO_RESPONSE ) is set to true
  if( get_flag( DO_RESPONSE ) && !get_flag( NO_ZERO_SUP ) )
  { _mMutZeroSup_mod.print_summary(); }

  return 0;
}

float MuonUnpackPisa::get_bbcrate(PHCompositeNode* top_node) const
{
  // gtetting BBC rate (MHz) from database 
  float bbcrate = 0;
  RunHeader* runh = findNode::getClass<RunHeader>(top_node,"RunHeader");
  if (runh)
    {
      int runnumber = runh->get_RunNumber();
      Connection* con = 0;
      Statement* stmt = 0;
      ResultSet* rs = 0;
      try
	{
	  con = DriverManager::getConnection("daq", "phnxrc", "");
	}
      catch (SQLException& e)
	{
	  cout << PHWHERE
	       << " Exception caught during DriverManager::getConnection" << endl;
	  cout << e.getMessage() << endl;
	  return false;
	}
      stmt = con->createStatement();
      try
	{
	  string cmd = Form("select trigger.name,scalerupdatescaled*(scaledown+1)/extract(epoch from (run.ertimestamp-run.brtimestamp)) as bbcrate from trigger,run where trigger.runnumber=%d and run.runnumber=%d and trigger.name='BBCLL1(>0 tubes)'",runnumber,runnumber);
	  rs = stmt->executeQuery(cmd.c_str());
	}
      catch (SQLException& e)
	{
	  cout << e.getMessage() << endl;
	  cout << PHWHERE << "No database entries" << endl;
	  delete stmt;
	  delete con;
	  return false;
	}
      while (rs->next())
	{
	  bbcrate = rs->getFloat("bbcrate")/1e6;
	  cout << PHWHERE << " BBC rate used for MuTr chamber efficiency = " << bbcrate << " MHz" << endl;
	}
    }
  else
    cout << PHWHERE << "Could not find the run header" << endl;
  return bbcrate;
}

//______________________________________________________
void MuonUnpackPisa::set_bbcrate_dependent_efficiency( const std::string file )
{
  ifstream fin(file.c_str());
  int arm=-1, station=-1, gap=-1, plane=-1, octant=-1, half_octant=-1;
  float eff0=-999., eff1=-999.;
  cout << PHWHERE << "----- BBC rate dependent MuTr chamber efficiency ---" << endl;
  while (fin >> arm >> station >> gap >> plane >> octant >> half_octant >> eff0 >> eff1)
    {
      int index = plane + MUTOO::NumberOfPlanes*(half_octant + MUTOO::MAX_HALF_OCTANT *(octant + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
      _chamber_eff0[index] = eff0;
      _chamber_eff1[index] = eff1;
      cout << index << " " << eff0 << "+" << eff1 << "*bbcrate" << endl;
    }
  cout << "--------------------" << endl;
  fin.close();

  _check_bbc_rate = true;
}

//______________________________________________________
void MuonUnpackPisa::set_mutr_base_efficiency( const std::string file )
{
  float eff = 0.98;
  int index = -999;
  ifstream fin(file.c_str());
  cout << "MuonUnpackPisa::set_mutr_base_efficiency(" << file << ")" << endl;
  while (fin >> index >> eff)
    {
      _mutr_base_efficiency[index] = eff;
    }
  cout << "--------------------" << endl;
  fin.close();
}
