// $Id: PadVtxReco.C,v 1.12 2011/12/05 22:12:25 phnxbld Exp $

#include "PadVtxReco.h"

#include <PadVertexFunction.h>

#include <ZdcOut.h>
#include <BbcOut.h>
#include <VtxOut.h>
#include <VtxOrdercodes.h>
#include <T0Out.h>

#include <recoConsts.h>

#include <BbcCalibPar.hh>
#include <ZdcCalibPar.hh>
#include <PdbPmtPeak.hh>

#include <mEmcClusterizerv0.h>
#include <mEmcGeometryModule.h>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>

#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <gsl/gsl_const.h>

#include <iostream>
#include <string>


using namespace std;

static const float lightSpeed = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9; // cm/ns
// error values are set somewhat arbitrarily at the moment for what we reconstruct
static const float VTXERR_X = 1.;
static const float VTXERR_Y = 1.;

const float PadVtxReco::fgTowerThresholdPbSc = 0.010;
const float PadVtxReco::fgTowerThresholdPbGl = 0.014;
const float PadVtxReco::fgMinClusterEnergyPbSc = 0.015;
const float PadVtxReco::fgMinClusterEnergyPbGl = 0.060;

//
// The basic idea for this module is to try and reconstruct a vertex
// using PC + EMC if no vertex was previously found.
// If a vertex _is_ found, we calculate a t0 too, and redo EMC clustering
//

PadVtxReco::PadVtxReco(const string &name): 
  SubsysReco(name),
  padvtx(new PadVertexFunction()),
  fClusterizer(NULL)
{
  return ;
}

PadVtxReco::~PadVtxReco()
{
  delete padvtx;
  delete fClusterizer;
  return ;
}

int 
PadVtxReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  // Set Multiplicity Cut
  padvtx->set_max_pc3_hits( rc->get_IntFlag("PADVTX_PC3_MULT_CUT",20) );

  PHTimeStamp TimeStp = rc->get_TimeStamp();

  BbcCalibPar<PdbPmtPeak> bbctzero;
  int calib_version = rc->get_IntFlag("BBCCALIBVERSION");
  bbctzero.restore(TimeStp, "tzero", calib_version);
  bbc_t0_offset = bbctzero.getCalibPar(0)->getPeakChannel();

  ZdcCalibPar<PdbPmtPeak> zdctzero;
  zdctzero.restore(TimeStp, "tzero");
  zdc_t0_offset = zdctzero.getCalibPar(0)->getPeakChannel();

  /*DS
    cout << PHWHERE << " InitRun " 
    << " bbc_calib_version " << calib_version
    << " bbc_t0_offset " << bbc_t0_offset
    << " zdc_t0_offset " << zdc_t0_offset
    << endl;
  */

  std::auto_ptr<mEmcGeometryModule> geom(new mEmcGeometryModule()); // default is real geometry

  fClusterizer = new mEmcClusterizerv0(geom.get());

  fClusterizer->SetTowerThresholdPbSc(fgTowerThresholdPbSc);
  fClusterizer->SetTowerThresholdPbGl(fgTowerThresholdPbGl);
  fClusterizer->SetMinClusterEnergyPbSc(fgMinClusterEnergyPbSc);
  fClusterizer->SetMinClusterEnergyPbGl(fgMinClusterEnergyPbGl);

  return EVENT_OK;
}

int 
PadVtxReco::process_event(PHCompositeNode *topNode)
{
  int iret = EVENT_OK;
  /*
    check if the node containing the VtxOut object exists - note that here we don't
    need to create a VtxOutvX object (inheritance can be a pretty cool thing). 
  */

  VtxOut* vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
  if (!vtxout)
    {
      cout << PHWHERE << "VtxOut Node missing, returning" << endl;
      return ABORTRUN;
    }

  //---------------------------------------------------------------------------
  // OK, now we have VtxOut. Check if it thinks it has a good vertex
  //---------------------------------------------------------------------------
  int vtx_is_valid = vtxout->isValid();
  /* DS
     cout << PHWHERE << " INFO: vtx_is_valid " << vtx_is_valid 
     << " which_vtx " << vtxout->which_Vtx() 
     << endl;
  */
  bool found_regular_vertex = false;	// whether BB,ZDC,MVD vtx exists
  if (vtx_is_valid != 0 && ( string( vtxout->which_Vtx() ) != "DEFAULT" ) )
    { // we already have an ok vertex;
      found_regular_vertex = true;
    }

  // even if we have a good vertex,
  // try to get an pad vertex using Pad and others
  bool found_pad_vertex = false;

  double z0PC = 0;
  double z0PCerr = 0;
  Int_t fault = 0;
  fault = 0; // !=0 => verbosity on
  padvtx->event(topNode, &z0PC, &z0PCerr, &fault);
  /*DS
    cout << PHWHERE << " INFO: " 
    << " z0PC " << z0PC
    << " z0PCerr " << z0PCerr
    << " fault " << fault << endl;
  */
  float zvtx = 0;
  float zvtxerr = 0;

  // judge if we have found a credible vertex
  if ( fault == 0 && fabs(z0PC)<350.)
    {
      found_pad_vertex = true;
      zvtx = z0PC;
      zvtxerr = z0PCerr;
      //DS cout << PHWHERE << " INFO: found_pad_vertex " << endl;
    }
  //DS  cout << PHWHERE << " INFO: zvtx " << zvtx << endl;

  // set new values for vertex, with a default of 0
  if ( found_pad_vertex )
  {
    const char *name = "PAD";
    float vtxarr[3] = {0, 0, zvtx};
    float errarr[3] = {VTXERR_X, VTXERR_Y, zvtxerr};
    if (found_pad_vertex) vtxout->AddVtx(name, vtxarr, errarr, VTX::PADORDER);
    /*DS
    cout << PHWHERE << " INFO: new which_vtx " << vtxout->which_Vtx() 
    << " zvertex " << vtxout->get_ZVertex() 
    << endl;
    */
    
  }

  if (verbosity > 0)
  {
    cout << "******" << endl;
    vtxout->identify();
    cout << "******" << endl;
  }

  // if we got an ok pad vertex and no orig vertex,
  // we should try to get a t0 value too;
  // otherwise, we should just return, since we only
  // want to determine the pad t0 when it is necessary,
  // ie, when there is no regular vertex and there is
  // a pad vertex
  if (!found_pad_vertex || found_regular_vertex)
  {
    return iret;
  }

  // get t0 node
  T0Out* t0out = findNode::getClass<T0Out>(topNode, "T0Out");
  if (!t0out)
    {
      cout << PHWHERE << "T0Out Object missing, returning" << endl;
      return ABORTRUN;
    }

  // if we have made it this far, t0 should not have been found up-front
  // (no vertex was found for Bbc/Zdc), but now we have an alt. vertex
  // and should adjust t0 value..
  //DS cout << PHWHERE << " INFO: t0 " << t0out->get_T0() << endl;

  // get bbc and zdc info
  // we must have time info from one side, of either BBC or ZDC
  // - for the ultra-peripheral events, we are interested in ZDC events,
  // but I put in BBC too, and ask for BBC first (since it has better
  // resolution) to be a bit more general..
  float BbcTiming[2] = { -9999, -9999 };
  float ZdcTiming[2] = { -9999, -9999 };
  int BbcValidTime = 0;
  int ZdcValidTime = 0;
  int side = 0; // North or South

  BbcOut *bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (bbcout)
    {
      /*DS
	cout << PHWHERE << " INFO: bbc t0 " 
	<< bbcout->get_TimeZero() << endl;
      */
      for (side = 0; side < 2; side++)
        {
          BbcTiming[side] = bbcout->get_Timing(side);
          if (BbcTiming[side] > -999)
            {
              BbcValidTime |= (1 << side);
            }
        }
    }

  int ValidTime = 0;
  float Timing[2] = { -9999};
  float t0offset = 0;

  if (BbcValidTime > 0)
    { // bbc had valid time time info
      ValidTime = BbcValidTime;
      for (side = 0; side < 2; side++)
        {
          Timing[side] = BbcTiming[side];
        }
      t0offset = bbc_t0_offset;
    }
  else
    {
      // ask the zdc too
      ZdcOut *zdcout = findNode::getClass<ZdcOut>(topNode, "ZdcOut");
      if (zdcout)
        {
          /*DS
	    cout << PHWHERE << " INFO: zdc t0 " 
	    << zdcout->get_TimeZero() << endl;
          */
          for (side = 0; side < 2; side++)
            {
              ZdcTiming[side] = zdcout->get_Timing(side);
              if (ZdcTiming[side] > -999)
                {
                  ZdcValidTime |= (1 << side);
                }
            }
        }
      if (ZdcValidTime > 0)
        { // zdc had valid time time info
          ValidTime = ZdcValidTime;
          for (side = 0; side < 2; side++)
            {
              Timing[side] = ZdcTiming[side];
            }
          t0offset = zdc_t0_offset;
        }
    }
  /*DS
*/
  // tmp dump of values
/*
  cout << PHWHERE << " INFO: BBC time; " << BbcValidTime 
  << " south " << BbcTiming[0]
  << " north " << BbcTiming[1] << endl;

  cout << PHWHERE << " INFO: ZDC time; " << ZdcValidTime 
  << " south " << ZdcTiming[0]
  << " north " << ZdcTiming[1] << endl;

  cout << PHWHERE << " INFO: time; " << ValidTime 
  << " south " << Timing[0]
  << " north " << Timing[1] << endl;
*/

/*
  */ 
  // based on the found vertex, we calculate a t0
  float time_diff = 2 * zvtx / lightSpeed; // = South - North
  if (ValidTime == 1)
    {
      Timing[1] = Timing[0] - time_diff;
    }
  else if (ValidTime == 2)
    {
      Timing[0] = Timing[1] + time_diff;
    }
  else if (ValidTime == 3)
    {
      // on occasion the BBC timing has a valid vtx but not valid t0.
      // do nothing, we recalculate the t0 later
    }
  else
    {
      cerr << PHWHERE << " ERROR: ValidTime " << ValidTime
	   << " not valid time info on any one side of BBC/ZDC "
	   << " - bailing out " << endl;
      return iret;
    }

  float t0 = 0.5 * (Timing[0] + Timing[1]) + t0offset;
  float t0err = zvtxerr / lightSpeed;
  //DS  cout << PHWHERE << " INFO: t0 " << t0 << endl;

  // set new values for t0
  if (BbcValidTime > 0)
    {
      t0out->set_BbcT0(t0, t0err);
    }
  else
    {
      t0out->set_ZdcT0(t0, t0err);
    }
  /*DS
  cout << PHWHERE << " INFO: new t0 " << t0out->get_T0()
       << endl;
  // and then redo stuff that needs re-doing
  cout << PHWHERE << " INFO: redoing EMC clustering " << endl;
  */

  fClusterizer->process_event(topNode);

  return iret;
}

