// $Id: FvtxPrimVtxEval.C,v 1.4 2017/07/13 19:24:26 phnxbld Exp $

/*!
\file    FvtxPrimVtxEval.C
\brief   muon-fvtx nanoDST creation, using new framework input nodes
\author  Cesar Luiz da Silva
\version $Revision: 1.4 $
\date    $Date: 2017/07/13 19:24:26 $
*/

#include <Fun4AllReturnCodes.h>
#include <PHIODataNode.h>
#include <PHGeometry.h>
#include <MUTOO.h>

// FVTXOO
#include <TFvtxCoordMap.h>
#include <TFvtxTrkMap.h>
#include <TFvtxTrk.h>

#include <TFile.h>
#include <TNtuple.h>

#include <VtxOut.h>
#include <FvtxPrimVtxEval.h>
#include <FVTXOO.h>
#include <FvtxPrimVertex.h>

using namespace std;

//_______________________________________________________________
FvtxPrimVtxEval::FvtxPrimVtxEval(string primaryvtxfile):
  _fvtxoo_node(NULL),
  _ievt(0),
  _vtxntup(NULL),
  vtxout(NULL),
  _primaryvtxfile( primaryvtxfile )
{
  ThisName = "FvtxPrimEval";
  return;
}

//_______________________________________________________________
FvtxPrimVtxEval::~FvtxPrimVtxEval()
{ return; }

//_______________________________________________________________
int FvtxPrimVtxEval::Init(PHCompositeNode *top_node)
{
  _vtxfile = new TFile(_primaryvtxfile.c_str(),"recreate");
  _vtxntup = new TNtuple("vtxntup","primary vertex finder",
			 "ievt:"
			 "trueX:trueY:trueZ:"
			 "bbcZ:"
			 "vtxX:vtxY:vtxZ:vtxEX:vtxEY:vtxEZ:"
			 "fvtxX:fvtxY:fvtxZ:fvtxEX:fvtxEY:fvtxEZ:"
			 "ncrossings");
  return 0;
}

//_______________________________________________________________
int FvtxPrimVtxEval::InitRun(PHCompositeNode *top_node)
{
  return 0;
}

//_______________________________________________________________
int FvtxPrimVtxEval::process_event(PHCompositeNode *top_node)
{
  ++_ievt;

  // retrive nodes
  set_node_ptrs(top_node);

  PHPoint trueVTX    = PHPoint(vtxout->get_Vertex("SIM"));
  float   bbcZ       = vtxout->get_BbcVertex();
  PHPoint vtx        = PHPoint(vtxout->get_Vertex("SVX_PRECISE"));
  PHPoint vtx_error  = PHPoint(vtxout->get_VertexError("SVX_PRECISE"));
  PHPoint fvtx       = PHPoint(vtxout->get_Vertex("FVTX"));
  PHPoint fvtx_error = PHPoint(vtxout->get_VertexError("FVTX"));
  float f_ievt = _ievt;
  float trueVTX_X = trueVTX.getX();
  float trueVTX_Y = trueVTX.getY();
  float trueVTX_Z = trueVTX.getZ();
  float vtx_X = vtx.getX();
  float vtx_Y = vtx.getY();
  float vtx_Z = vtx.getZ();
  float vtx_error_X = vtx_error.getX();
  float vtx_error_Y = vtx_error.getY();
  float vtx_error_Z = vtx_error.getZ();
  float fvtx_X = fvtx.getX();
  float fvtx_Y = fvtx.getY();
  float fvtx_Z = fvtx.getZ();
  float fvtx_error_X = fvtx_error.getX();
  float fvtx_error_Y = fvtx_error.getY();
  float fvtx_error_Z = fvtx_error.getZ();
  float data[18] = {f_ievt,
		    trueVTX_X,trueVTX_Y,trueVTX_Z,
		    bbcZ, 
		    vtx_X, vtx_Y, vtx_Z,
		    vtx_error_X, vtx_error_Y, vtx_error_Z,
		    fvtx_X,fvtx_Y,fvtx_Z,
		    fvtx_error_X,fvtx_error_Y,fvtx_error_Z,
		    -9999.9};

  _vtxntup->Fill(data);

  return EVENT_OK;
}

//______________________________________________________
int FvtxPrimVtxEval::End(PHCompositeNode* top_node)
{
  _vtxfile->cd();
  _vtxntup->Write();
  _vtxfile->Close();

  return 0;
}

//____________________________________________________________
bool FvtxPrimVtxEval::set_node_ptrs(PHCompositeNode* top_node) {
  
  vtxout = TMutNode<VtxOut>::find_io_node( top_node, "VtxOut");
  if (!vtxout) {
    cout << PHWHERE << " VtxOut not in Node Tree" << endl;
    return ABORTRUN;
  }

  // look for mutoo node
  PHNodeIterator node_iter(top_node);
  
  // fvtx node
  _fvtxoo_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "FVTXOO" ));
  if(!_fvtxoo_node) {
    throw logic_error( "FvtxPrimVtxEval::set_node_ptrs - could not find node FVTXOO. You shoud run the correct Unpacker to your macro." );
  }

  return true;
}
