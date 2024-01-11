// $Id: RxnpAnaDst.cxx,v 1.12 2009/08/31 19:28:25 hpereira Exp $

/*!
  \file    RxnpAnaDst.cxx
  \ingroup supermodules 
  \brief   Rxnp reconstruction event loop
  \author  Chun Zhang
  \version $Revision: 1.12 $
  \date    $Date: 2009/08/31 19:28:25 $
*/
// ROOT
#include <TFile.h>
#include <TTree.h>
//
#include <PHCompositeNode.h>
#include <recoConsts.h>
#include <TMutNode.h>
#include <Fun4AllReturnCodes.h>

// ZDC, BBC, trig includes
#include <Bbc.hh>
#include <Zdc.hh>

#include <ZdcOutv2.h>
#include <BbcOutv1.h>
#include <TrigLvl1.h>
#include <SmdOut.h>
#include <BbcRaw.h>
#include <BbcCalib.hh>
#include <BbcGeo.hh>
#include <RunToTime.hh>
#include <PHMapManager.h>

#include <TRxnpRawScintMap.h>
#include <TRxnpScintMap.h>
#include <TRxnpRawXangMap.h>

#include <getClass.h>
#include <RunHeader.h>
#include <EventHeader.h>

#include "RxnpAnaDst.h"

using namespace std;

static BbcCalib* bbccalib = 0;
static BbcGeo* bbcgeo = 0;

//______________________________________________________
RxnpAnaDst::RxnpAnaDst(std::string outfile) : 
  MuonSubsysReco( "RxnpAnaDst" ),
  _timer( PHTimeServer::get()->insert_new("RxnpAnaDst") )
{ _outfile = outfile; }

//______________________________________________________
int RxnpAnaDst::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );
    
  MUTOO::PRINT( cout, "RxnpAnaDst::Init" );
  
  // Instantiate Rxnp analysis modules
  _anaOut = new TFile(_outfile.c_str(),"RECREATE");

  _anaTree = new TTree("anaTree", "Rxnp DST analysis tree");
  // set up branches
  //
  _anaTree->Branch("run", &_run, "run/i");
  _anaTree->Branch("evt", &_evt, "evt/i");

  _anaTree->Branch("arm",       _arm,       "arm[48]/s");
  _anaTree->Branch("ring",      _ring,      "ring[48]/s");
  _anaTree->Branch("scint",     _scint,     "scint[48]/s");
  _anaTree->Branch("chanid",    _chanid,    "chanid[48]/s");

  _anaTree->Branch("high_pre",  _high_pre,  "high_pre[48]/s");
  _anaTree->Branch("high_post", _high_post, "high_post[48]/s");
  _anaTree->Branch("low_pre",   _low_pre,   "low_pre[48]/s");
  _anaTree->Branch("low_post",  _low_post,  "low_post[48]/s");
  _anaTree->Branch("tdc",       _tdc,       "tdc[48]/s");
  _anaTree->Branch("amu_pre",   _amu_pre,   "amu_pre[48]/s");
  _anaTree->Branch("amu_post",  _amu_post,  "amu_post[48]/s");
  _anaTree->Branch("amu_tdc",   _amu_tdc,   "amu_tdc[48]/s");

  _anaTree->Branch("phi",       _phi,       "phi[48]/D");
  _anaTree->Branch("theta",     _theta,     "theta[48]/D");
  _anaTree->Branch("high_e",    _high_e,    "high_e[48]/D");
  _anaTree->Branch("low_e",     _low_e,     "low_e[48]/D");
  _anaTree->Branch("tof_c",     _tof_c,     "tof_c[48]/D");

  _anaTree->Branch("xang_1H",   _xang_1H,   "xang_1H[9]/D");
  _anaTree->Branch("xang_1L",   _xang_1L,   "xang_1L[9]/D");
  _anaTree->Branch("xang_2H",   _xang_2H,   "xang_2H[9]/D");
  _anaTree->Branch("xang_2L",   _xang_2L,   "xang_2L[9]/D");
  _anaTree->Branch("xang_3H",   _xang_3H,   "xang_3H[9]/D");
  _anaTree->Branch("xang_3L",   _xang_3L,   "xang_3L[9]/D");
  _anaTree->Branch("xang_4H",   _xang_4H,   "xang_4H[9]/D");
  _anaTree->Branch("xang_4L",   _xang_4L,   "xang_4L[9]/D");

  _anaTree->Branch("charge",    _charge,    "charge[3]/D");
  _anaTree->Branch("time",      _time,      "time[3]/D");
  _anaTree->Branch("nhit",      _nhit,      "nhit[3]/D");
  _anaTree->Branch("zvertex",   &_zvertex,  "zvertex/D");

  _anaTree->Branch("bbcv",     &_bbcZvertex,    "bbcv/D");
  _anaTree->Branch("bbct",     &_bbcTimeZero,   "bbct/D");
  _anaTree->Branch("bbcn",      _bbcNpmt,       "bbcn[2]/I");
  _anaTree->Branch("bbca",      _bbcCharge,     "bbca[2]/D");
  _anaTree->Branch("zdcv",     &_zdcZvertex,    "zdcv/D");
  _anaTree->Branch("zdct",     &_zdcTimeZero,   "zdct/D");
  _anaTree->Branch("zdce",      _zdcEnergy,     "zdce[2]/D");
  _anaTree->Branch("smde",      _smdEnergy,     "smde[2]/D");
  _anaTree->Branch("smdx",      _smdXpos,       "smdx[2]/D");
  _anaTree->Branch("smdy",      _smdYpos,       "smdy[2]/D");
  _anaTree->Branch("bbad",      _bbcAdc,        "bbad[128]/D");
  _anaTree->Branch("bbt0",      _bbcTdc0,       "bbt0[128]/D");
  _anaTree->Branch("bbt1",      _bbcTdc1,       "bbt1[128]/s");

  initTTree();

  _global = new TTree("global" , " global event info tree" );
  
  _global->Branch("run", &_run, "run/i");
  _global->Branch("evt", &_evt, "evt/i");

  _global->Branch("Rawtrig",&_trig_raw, "Rawtrig/i");
  _global->Branch("Livetrig",&_trig_live, "Livetrig/i");
  _global->Branch("Scaledtrig",&_trig_scaled, "Scaledtrig/i");
  _global->Branch("NtubeS", &_nbbc_tube_S, "NtubeS/i");
  _global->Branch("NtubeN", &_nbbc_tube_N, "NtubeN/i");
  _global->Branch("chargeS",&_bbc_q_S, "chargeS/D");
  _global->Branch("chargeN",&_bbc_q_N, "chargeN/D");
  _global->Branch("zdcES",&_zdc_e_S, "zdcES/D");
  _global->Branch("zdcEN",&_zdc_e_N, "zdcEN/D");
  _global->Branch("Bbczvtx", &_zvtx, "BbcZvtx/D"); 
  _global->Branch("Zdczvtx", &_zdc_zvtx, "ZdcZvtx/D"); 
  
  initGlobalTree();

  bbccalib = new BbcCalib();
  bbcgeo = new BbcGeo();

  MUTOO::PRINT( cout, "**" );
  return 0;
}

//______________________________________________________
int RxnpAnaDst::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "RxnpAnaDst::InitRun" );
  CreateNodeTree(top_node);
  MUTOO::PRINT( cout, "**" );
    // need run node too
  //.
  RunHeader* d_runhdr = findNode::getClass<RunHeader>(top_node,"RunHeader");
  if( !d_runhdr )
  {
    std::cerr << PHWHERE << " Can not find RunHeader :: you should have HeaderReco in your macro" << std::endl;  
    return ABORTRUN;
  }
 
  // run number
  int runnumber =  d_runhdr->get_RunNumber();
 
  // init the reaction plane calculation
  _mRxnpCalXang_mod.init_calcu(runnumber, 0);

  // Get TRxnpFEM and init it
  RunToTime* runTime = RunToTime::instance();
  
  PHTimeStamp* ts( runTime->getBeginTime(runnumber) );
  PHTimeStamp tstart = *ts;
  delete ts;

  int bbccalib_version = 4002;

  std::cout << "RxnpAnaDst::InitRun, run number= " << runnumber
  << " Version of BbcCalib = " << bbccalib_version << endl;

  bbccalib->restore(tstart, bbccalib_version);

  for (int ipmt=0; ipmt<128; ipmt++) {
    std::cout << ipmt 
      << " " << bbcgeo->getX(ipmt) << " "
      << bbcgeo->getY(ipmt) << " "
      << bbcgeo->getZ(ipmt) << std::endl;
  }

  return 0;
}

//______________________________________________________
int RxnpAnaDst::CreateNodeTree(PHCompositeNode *top_node)
{
  
  // get dst node
  //
  {
    PHNodeIterator nodeItr(top_node);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!dst_node) {
      // Error condition -- need input DST node
      cerr << "RxnpAnaDst::CreateNodeTree - could not find DST node.\n";
    }
  }
  // Instantiate nodes for Rxnp containers
  {
    PHNodeIterator nodeItr(top_node);
    rxnp_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RXNP"));
    if(!rxnp_node){
      rxnp_node = new PHCompositeNode("RXNP");
      top_node->addNode(rxnp_node);
    }
  }
  


  TMutNode<TRxnpRawScintMap>::new_dst_input_node(rxnp_node, "TRxnpRawScintMap", dst_node, "TRxnpRawScint");
  TMutNode<TRxnpScintMap>::new_dst_input_node(rxnp_node, "TRxnpScintMap", dst_node, "TRxnpScint");
  TMutNode<TRxnpRawXangMap>::new_dst_input_node(rxnp_node, "TRxnpRawXangMap", dst_node, "TRxnpRawXang");

  TMutNode<mRxnpCalXangPar>::new_node(rxnp_node,"mRxnpCalXangPar");

  // Interface Object Containers (IOCS)
  //recoConsts *rc = recoConsts::instance();
     
  return 0;
}

//______________________________________________________
int RxnpAnaDst::process_event(PHCompositeNode *top_node)
{
  
  _timer.get()->restart();
  
  // Call RXNP modules for track momentum reconstruction and vertex finding
  load_vertex_if_needed( top_node );
  try {
    
    PHMapManager::read(top_node);    

    mRxnpCalXangPar* calxang_par = TMutNode<mRxnpCalXangPar>::find_node(top_node, "mRxnpCalXangPar");
    calxang_par->set_iteration(_iter);

    //calculate Xang
    _mRxnpCalXang_mod.event(top_node);

    if (calxang_par->get_iteration()==2)
    {
      //Fill anaylsis at last iteration.
      // init TTree
      initTTree();
      initGlobalTree();
      // fill TTree
      fill_tree(top_node);
      fill_GlobalTree(top_node);
    }
    
  } catch (exception& e) {
    MUTOO::TRACE(e.what());
  }  

  write_maps_if_needed();
  
  _timer.get()->stop();

  return 0;
}

//______________________________________________________
void RxnpAnaDst::initTTree()
{
  _run       = 0;
  _evt       = 0;
  for (int i=0; i<48; i++) 
  {
    _arm[i]       = 9999;
    _ring[i]      = 9999;
    _scint[i]     = 9999;
    _chanid[i]    = 9999;
    _high_pre[i]  = 9999;
    _high_post[i] = 9999;
    _low_pre[i]   = 9999;
    _low_post[i]  = 9999;
    _tdc[i]       = 9999;
    _amu_pre[i]   = 9999;
    _amu_post[i]  = 9999;
    _amu_tdc[i]   = 9999;

    _phi[i]       = -9999.;
    _theta[i]     = -9999.;
    _high_e[i]    = -9999.;
    _low_e[i]     = -9999.;
    _tof_c[i]     = -9999.;
  }

  for (int i=0; i<9; i++) 
  {
    _xang_1H[i] = -9999.;
    _xang_1L[i] = -9999.;
    _xang_2H[i] = -9999.;
    _xang_2L[i] = -9999.;
    _xang_3H[i] = -9999.;
    _xang_3L[i] = -9999.;
    _xang_4H[i] = -9999.;
    _xang_4L[i] = -9999.;
  }

  for (int i=0; i<3; i++) 
  {
    _charge[i]  = -9999.;
    _time[i]    = -9999.;
    _nhit[i]    = -9999.;
  }
  _zvertex = -9999.;

  _bbcZvertex   = -9999.;
  _bbcTimeZero  = -9999.;
  _zdcZvertex   = -9999.;
  _zdcTimeZero  = -9999.;
  
  for (int i=0; i<2; i++) 
  {
    _bbcNpmt[i]   = -9999;
    _bbcCharge[i] = -9999.;
    _zdcEnergy[i] = -9999.;
    _smdEnergy[i] = -9999.;
    _smdXpos[i] = -9999.;
    _smdYpos[i] = -9999.;
  }
  
  for (int i=0; i<128; i++) 
  {
    _bbcAdc[i] = -9999.;
    _bbcTdc0[i] = -9999.;
    _bbcTdc1[i] = 9999;
  }
}

//______________________________________________________
void RxnpAnaDst::initGlobalTree()
{
  _evt = 0;
  _run = 0;
  
  _trig_raw = 0;
  _trig_live = 0;
  _trig_scaled = 0;
  _nbbc_tube_S = 0;
  _nbbc_tube_N = 0;
  _bbc_q_S = 0;
  _bbc_q_N = 0;
  
  _zdc_e_S = 0;
  _zdc_e_N = 0;

  _zvtx = -9999.0;
  _zdc_zvtx = -9999.0;
}


//______________________________________________________
void RxnpAnaDst::fill_tree(PHCompositeNode *node)
{

  TRxnpRawScintMap* raw_map = 0;
  TRxnpScintMap* cor_map = 0;
  
  try
  {
    raw_map = TMutNode<TRxnpRawScintMap>::find_node(node, "TRxnpRawScintMap");
    cor_map = TMutNode<TRxnpScintMap>::find_node(node, "TRxnpScintMap");
  } 
  catch(std::exception& e)
  { return; }

  BbcOut *bbcout = findNode::getClass<BbcOut>(node,"BbcOut");
  if (bbcout != 0) {
    _bbcZvertex   = bbcout->get_VertexPoint();
    _bbcTimeZero  = bbcout->get_TimeZero();
    _bbcNpmt[0]   = bbcout->get_nPmt(0);
    _bbcNpmt[1]   = bbcout->get_nPmt(1);
    _bbcCharge[0] = bbcout->get_ChargeSum(0);
    _bbcCharge[1] = bbcout->get_ChargeSum(1);
  }

  ZdcOut *zdcout = findNode::getClass<ZdcOut>(node,"ZdcOut");
  if (zdcout != 0) {
    _zdcZvertex   = zdcout->get_Zvertex();
    _zdcTimeZero  = zdcout->get_TimeZero();
    _zdcEnergy[0] = zdcout->get_Energy(0);
    _zdcEnergy[1] = zdcout->get_Energy(1);
  }

  SmdOut *smdout = findNode::getClass<SmdOut>(node,"SmdOut");
  if (smdout != 0) {
    _smdEnergy[0] = smdout->get_Energy(0);
    _smdEnergy[1] = smdout->get_Energy(1);
    _smdXpos[0]   = smdout->get_Xpos(0);
    _smdYpos[0]   = smdout->get_Ypos(0);
    _smdXpos[1]   = smdout->get_Xpos(1);
    _smdYpos[1]   = smdout->get_Ypos(1);
  }

  BbcRaw *bbcraw = findNode::getClass<BbcRaw>(node,"BbcRaw");
  if (bbcraw != 0) 
  {
    for (int ipmt=0; ipmt<128; ipmt++) 
    {
      short adc = bbcraw->get_Adc(ipmt);
      short tdc = bbcraw->get_Tdc0(ipmt);
      _bbcAdc[ipmt]  = bbccalib->getCharge(ipmt, adc);
      _bbcTdc0[ipmt] = bbccalib->getHitTime0(ipmt, tdc, adc);
      _bbcTdc1[ipmt] = bbcraw->get_Tdc1(ipmt);
    }
  }
  
  char name[80];
  for(unsigned short iarm = 0; iarm < 2; iarm++)
  {
    for(unsigned short iring = 0; iring < 2; iring++)
    {
      for(unsigned short iscint = 0; iscint < 12; iscint++)
      {
        int ii = iarm*24 + iring*12 + iscint;
        int ir = 0;
        TRxnpRawScintMap::const_iterator raw_iter = raw_map->get(iarm, iring, iscint);
        while(TRxnpRawScintMap::const_pointer raw_ptr= raw_iter.next())
        {
          _arm[ii]       = iarm;
          _ring[ii]      = iring;
          _scint[ii]     = iscint;
          _chanid[ii]    = raw_ptr->get()->get_chanid();
          _high_pre[ii]  = raw_ptr->get()->get_high_pre();
          _high_post[ii] = raw_ptr->get()->get_high_post();
          _low_pre[ii]   = raw_ptr->get()->get_low_pre();
          _low_post[ii]  = raw_ptr->get()->get_low_post();
          _tdc[ii]       = raw_ptr->get()->get_tdc();
          _amu_pre[ii]   = raw_ptr->get()->get_amu_pre();
          _amu_post[ii]  = raw_ptr->get()->get_amu_post();
          _amu_tdc[ii]   = raw_ptr->get()->get_amu_tdc();
          ir++;
        }
        int ic = 0;
        TRxnpScintMap::const_iterator cor_iter = cor_map->get(iarm, iring, iscint);
        while(TRxnpScintMap::const_pointer cor_ptr= cor_iter.next())
        {
          _phi[ii]       = cor_ptr->get()->get_phi();
          _theta[ii]     = cor_ptr->get()->get_theta();
          _high_e[ii]    = cor_ptr->get()->get_high_e();
          _low_e[ii]     = cor_ptr->get()->get_low_e();
          _tof_c[ii]     = cor_ptr->get()->get_tof();
          ic++;
        }
        if (ir>1 || ic>1)
        {
          sprintf(name,"RxnpAnaDst::fill_tree, error %d %d %d : %d %d",iarm,iring,iscint,ir,ic);
          MUTOO::PRINT( cout, name );
        }
      }
    }
  }
  
  for (int ii=0; ii<9; ii++) 
  {
    _xang_1H[ii] = _mRxnpCalXang_mod.getRxnPlane(ii, 0, 0);
    _xang_1L[ii] = _mRxnpCalXang_mod.getRxnPlane(ii, 0, 1);
    _xang_2H[ii] = _mRxnpCalXang_mod.getRxnPlane(ii, 1, 0);
    _xang_2L[ii] = _mRxnpCalXang_mod.getRxnPlane(ii, 1, 1);
    _xang_3H[ii] = _mRxnpCalXang_mod.getRxnPlane(ii, 2, 0);
    _xang_3L[ii] = _mRxnpCalXang_mod.getRxnPlane(ii, 2, 1);
    _xang_4H[ii] = _mRxnpCalXang_mod.getRxnPlane(ii, 3, 0);
    _xang_4L[ii] = _mRxnpCalXang_mod.getRxnPlane(ii, 3, 1);
  }
  
  for (int ii=0; ii<3; ii++) 
  {
    _charge[ii] = _mRxnpCalXang_mod.getGlobal(ii);
    _time[ii]   = _mRxnpCalXang_mod.getGlobal(ii+3);
    _nhit[ii]   = _mRxnpCalXang_mod.getGlobal(ii+7);
  }
  _zvertex = _mRxnpCalXang_mod.getGlobal(6);
  
  _anaTree->Fill();
  
}

//______________________________________________________
void RxnpAnaDst::fill_GlobalTree(PHCompositeNode *top_node)
{
  // get run and event info
  //
  RunHeader *runheader = findNode::getClass<RunHeader>(top_node, "RunHeader");
  EventHeader* eventheader = findNode::getClass<EventHeader>(top_node, "EventHeader");

  if(runheader) _run = runheader->get_RunNumber();
  if(eventheader) _evt = eventheader->get_EvtSequence();

  // get trigger infor
  //
  // mask for trigger lvl1
  // only good for April 2 20007  afternoon
  //
  int mask_BBCLL1andZDCLL1Wide = (0x04);
  int mask_BBCLL1NoVertex = (0x08);
  int mask_ZDCLL1Narrow = (0x20);
  int mask_ZDCLL1Wide = (0x10);
  int mask_BBCLL1 = (0x4000000); 
  int mask_ZDCNS =  (0x8000000); 

  TrigLvl1* triglvl1 = findNode::getClass<TrigLvl1>(top_node,"TrigLvl1");
  if(triglvl1)
  {
    int trig_raw = triglvl1->get_lvl1_trigraw();
    if(trig_raw & mask_ZDCNS) _trig_raw |= (0x01<<5);
    if(trig_raw & mask_BBCLL1andZDCLL1Wide) _trig_raw |= (0x01<<4);
    if(trig_raw & mask_ZDCLL1Wide) _trig_raw |= (0x01<<3);
    if(trig_raw & mask_ZDCLL1Narrow) _trig_raw |= (0x01<<2);
    if(trig_raw & mask_BBCLL1NoVertex) _trig_raw |= (0x01<<1);
    if(trig_raw & mask_BBCLL1) _trig_raw |= (0x01<<0);
    int trig_live = triglvl1->get_lvl1_triglive();
    if(trig_live & mask_ZDCNS) _trig_live |= (0x01<<5);
    if(trig_live & mask_BBCLL1andZDCLL1Wide) _trig_live |= (0x01<<4);
    if(trig_live & mask_ZDCLL1Wide) _trig_live |= (0x01<<3);
    if(trig_live & mask_ZDCLL1Narrow) _trig_live |= (0x01<<2);
    if(trig_live & mask_BBCLL1NoVertex) _trig_live |= (0x01<<1);
    if(trig_live & mask_BBCLL1) _trig_live |= (0x01<<0);
    int trig_scaled = triglvl1->get_lvl1_trigscaled();
    if(trig_scaled & mask_ZDCNS) _trig_scaled |= (0x01<<5);
    if(trig_scaled & mask_BBCLL1andZDCLL1Wide) _trig_scaled |= (0x01<<4);
    if(trig_scaled & mask_ZDCLL1Wide) _trig_scaled |= (0x01<<3);
    if(trig_scaled & mask_ZDCLL1Narrow) _trig_scaled |= (0x01<<2);
    if(trig_scaled & mask_BBCLL1NoVertex) _trig_scaled |= (0x01<<1);
    if(trig_scaled & mask_BBCLL1) _trig_scaled |= (0x01<<0);
  }
  
  // BBC information
  BbcOut *bbcout = findNode::getClass<BbcOut>(top_node, "BbcOut");
  
  if(bbcout)
  {
    _zvtx = bbcout->get_VertexPoint();
    _nbbc_tube_S = bbcout->get_nPmt(Bbc::South);
    _nbbc_tube_N = bbcout->get_nPmt(Bbc::North);
    _bbc_q_S = bbcout->get_ChargeSum(Bbc::South);
    _bbc_q_N = bbcout->get_ChargeSum(Bbc::North);
  }

  // ZDC information
  ZdcOut *zdcout = findNode::getClass<ZdcOut>(top_node, "ZdcOut");

  if(zdcout)
  {
    _zdc_zvtx = zdcout->get_Zvertex();
    _zdc_e_S = zdcout->get_Energy(Zdc::South);
    _zdc_e_N = zdcout->get_Energy(Zdc::North);
  }
    
  _global->Fill();
  
}


//______________________________________________________
int RxnpAnaDst::End(PHCompositeNode* top_node) 
{
  _anaOut->Write();
  _anaOut->Close();
  _mRxnpCalXang_mod.closeHisto();
  _timer.get()->print_stat();
  
  return 0;
}

