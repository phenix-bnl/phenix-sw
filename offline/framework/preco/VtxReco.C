#include "VtxReco.h"

#include <ZdcOut.h>
#include <BbcOut.h>
#include <VtxOutv7.h>
#include <VtxOrdercodes.h>

#include <recoConsts.h>

#include <PHNodeIterator.h>
#include <PHCompositeNode.h>

#include <getClass.h>
#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;

// default vtx positions and errors if vertex component is not measured
static const float VTX_X = 0.;
static const float VTX_Y = 0.;

static const float VTXERR_X = 1.;
static const float VTXERR_Y = 1.;

static const float VTXERR_Z = 38.;
static const float VTX_Z = 0.;

//________________________________________________________
VtxReco::VtxReco(const string &name): SubsysReco(name)
{
  return ;
}

//________________________________________________________
int VtxReco::InitRun(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

//________________________________________________________
int VtxReco::CreateNodeTree(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  VtxOut *vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
  if (!vtxout)
  {
    vtxout = new VtxOutv7();
    PHObjectNode_t *VtxOutNode = new PHObjectNode_t(vtxout, "VtxOut", "PHObject");
    dstNode->addNode(VtxOutNode);
  }

  return 0;
}

//________________________________________________________
int VtxReco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;
  float vtx[3], vtxerr[3];

  /*
    check if the node containing the VtxOut object exists - note that here we don't
    need to create a VtxOutvX object (inheritance can be a pretty cool thing). 
  */

  VtxOut* vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
  if (!vtxout)
    {
      cout << PHWHERE << "VtxOut Object missing, returning" << endl;
      return -1;
    }

  //---------------------------------------------------------------------------
  // only good vertices should make it into the VtxOut class since it does not
  // do any checking on the validity of the input
  //---------------------------------------------------------------------------

  // dig into the bbc output and grab vertex if it is valid
  BbcOut *bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (bbcout)
  {
    if (bbcout->isValid())  // check if BBC fired
    {
      vtx[2] = bbcout->get_VertexPoint();
      if (vtx[2] > -999)
      {
        vtx[0] = VTX_X;
        vtx[1] = VTX_Y;
        vtxerr[0] = VTXERR_X;
        vtxerr[1] = VTXERR_Y;
        vtxerr[2] = bbcout->get_dVertexPoint();
        vtxout->AddVtx("BBC", vtx, vtxerr, VTX::BBCORDER);
      }
    }
  }


  // ask the zdc for its opinion about a reconstructed vertex
  ZdcOut *zdcout = findNode::getClass<ZdcOut>(topNode, "ZdcOut");
  if (zdcout)
    {
      if (zdcout->isValid()) // check if zdc fired
        {
          vtx[2] = zdcout->get_Zvertex();
          if (vtx[2] > -800)
            {
              vtx[0] = VTX_X;
              vtx[1] = VTX_Y;
              vtxerr[0] = VTXERR_X;
              vtxerr[1] = VTXERR_Y;
              vtxerr[2] = zdcout->get_ZvertexError();
              vtxout->AddVtx("ZDC", vtx, vtxerr, VTX::ZDCORDER);
            }
        }
    }

  vtx[0] = VTX_X;
  vtx[1] = VTX_Y;
  vtx[2] = VTX_Z;
  vtxerr[0] = VTXERR_X;
  vtxerr[1] = VTXERR_Y;
  vtxerr[2] = VTXERR_Z;
  vtxout->AddVtx("DEFAULT", vtx, vtxerr, VTX::DEFAULTORDER);
  if (verbosity > 0)
  {
    vtxout->identify();
  }
  return iret;
}

