
#include "Run15pAu200_DCA_BeamAngle_Recal.h"

#include <Fun4AllReturnCodes.h>
#include <VtxOut.h>
#include <Bbc.hh>
#include <BbcOut.h>
#include <PHGlobal.h>

#include <RunHeader.h>
#include <recoConsts.h>
#include <getClass.h>

#include <PHPoint.h>

#include <iostream>
#include <iomanip>
#include <exception>
#include <set>
#include <cmath>

#include <PHCentralTrack.h>

using namespace std;
using namespace findNode;

/////////////////////////////////////////////////
Run15pAu200_DCA_BeamAngle_Recal::Run15pAu200_DCA_BeamAngle_Recal(const std::string &name):
  Recalibrator("Run15pAu200_DCA_BeamAngle_Recal"),
  m_runNumber(-9999),
  m_ppflag(0),
  m_vtxflag(0),
  m_vtx_found(0),
  m_bbccharge(-9999.0),
  m_centrality(-9999.0)

{
  baseclasses.insert("VtxOut");  
  //baseclasses.insert("PHCentralTrack");
}

Run15pAu200_DCA_BeamAngle_Recal::~Run15pAu200_DCA_BeamAngle_Recal()
{
}

int Run15pAu200_DCA_BeamAngle_Recal::InitRun(PHCompositeNode *topNode)
{
  // Set up the node tree
  int i = CreateNodeTree(topNode);
  if (verbosity > 0) cout << "Run15pAu200_DCA_BeamAngle_Recal::InitRun-I: CreateNodeTree returned " << i << endl;
  if (!(i == EVENT_OK))
  {
    return EVENT_OK;
  }

  /////////////////////////////
  recoConsts *rc =  recoConsts::instance();

  if (rc->FlagExist("RUNNUMBER"))
  {
    m_runNumber = rc->get_IntFlag("RUNNUMBER");
  }

  return 0;
}

int Run15pAu200_DCA_BeamAngle_Recal::isValidRun(const int runno) const
{
  if(runno >= 432637 && runno <= 436647)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}


int Run15pAu200_DCA_BeamAngle_Recal::process_event(PHCompositeNode *topNode)
{
  if(isValidRun(m_runNumber) == 0)
    {
      return 0;
    }

  if (verbosity > 0) cout << "Run15pAu200_DCA_BeamAngle_Recal::process_event started" << endl;

  VtxOut *vtxout = getClass<VtxOut>(topNode, "VtxOut");
  if (vtxout == NULL)
  {
    cout << "No VtxOut in the NODE tree" << endl;
    /// \todo Really return EVENT_OK if no VtxOut node?
    return EVENT_OK;
  }

  /* 03/31/2021, Zhiyan: BbcOut causes error when DST_EVE file is not used (Jonathan Runchey only uses CNT DSTs.)
                         It's only used for bbcqn and bbcqs, which are now obtained from PHGlobal.
  BbcOut *bbcout = getClass<BbcOut>(topNode, "BbcOut");
  if (bbcout == NULL)
  {
    cout << "No BbcOut in the NODE tree" << endl;
    m_bbccharge = -9999.0;
    ///\todo Really ABORTEVENT only if no BbcOut node?
    return ABORTEVENT;
  }

  // check BbcChargeSum
  float bbcqs = bbcout->get_ChargeSum(Bbc::South);
  float bbcqn = bbcout->get_ChargeSum(Bbc::North);
  m_bbccharge = bbcqs + bbcqn;
  */

  PHGlobal *d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if (d_global == NULL)
  {
    cout << "No PHGlobal in the NODE tree" << endl;
    m_centrality = -9999.0;
    ///\todo Really ABORTEVENT only if no BbcOut node?
    return ABORTEVENT;
  }
  // check BbcChargeSum
  float bbcqn = d_global->getBbcChargeN();
  float bbcqs = d_global->getBbcChargeS();
  m_bbccharge = bbcqs + bbcqn;

  m_centrality = d_global->getCentrality();
   
  setPPFlag(1);

  // GetVertex
  float vtx[3];
  // find most precise available vertex position
  getPrimaryVtx(vtxout, &vtx[0], &vtx[1], &vtx[2]);


  //Correct for Beam Angle Rotation in Run15 pAu

  float vx_new = vtx[0]+(0.00307962*vtx[2] + 0.00691413); //All Runs from Train 16481, fit (-5,5) p0: 0.00691413, p1: 0.00307962, mean 0.009158

  //float vx_new = vtx[0]+(0.00307962*vtx[2] + 0.00691413 - 0.009158); //All Runs from Train 16481, fit (-5,5) p0: 0.00691413, p1: 0.00307962, mean 0.009158    

  //float vx_new = vtx[0]+(0.00313168*vtx[2] + 0.0064631); //All Runs from Train 16481, fit (-10,10) p0: 0.0064631, p1: 0.00313168, mean 0.009158  

  //float vx_new = vtx[0]+(0.00313168*vtx[2] + 0.0064631 - 0.009158); //All Runs from Train 16481, fit (-10,10) p0: 0.0064631, p1: 0.00313168, mean 0.009158  

  //cout << "New vtx[0] recalibrated" << endl;

  //float vx_new = vtx[0]+(0.00313037*vtx[2] + 0.00555435); //One Run DIFF, from RunMyMacro 900,000 events, fit (-5,5) p0: 0.00555435, p1: 0.00313037, mean 0.006177 

  //float vx_new = vtx[0]+(0.003035*vtx[2] + 0.2002 - 0.2024); //All run no DIFF, p0: 0.2002, p1: 0.003035, mean: 0.2024

  //float vx_new = vtx[0]+(0.2002*vtx[2] - 0.1994);

  // store to the output node

  const string     sVtxName[2] = {"BBC", "SVX"};
  const VTX::Order vtxOrder[2] = {VTX::BBCORDER, VTX::SVXORDER};

  for(int ivtx=0; ivtx<2; ivtx++)
  {
    float zvtx = vtxout->get_ZVertex( sVtxName[ivtx].c_str() );
    if(isnan(zvtx)) {
      //cout<<"Value is NAN "<<sVtxName[ivtx].c_str()<<endl;
      continue;// NAN check
    }

    PHPoint v  = vtxout->get_Vertex(      sVtxName[ivtx].c_str() );
    PHPoint ve = vtxout->get_VertexError( sVtxName[ivtx].c_str() );

    float vtx_new[3] = {(float)vx_new,    (float)v.getY(),  (float)v.getZ()};
    float vtxerr[3]  = {(float)ve.getX(), (float)ve.getY(), (float)ve.getZ()};
    vtxout->AddVtx(sVtxName[ivtx].c_str(), vtx_new, vtxerr, vtxOrder[ivtx]);
   
  }


  return 0;
}



int Run15pAu200_DCA_BeamAngle_Recal::End(PHCompositeNode *topNode)
{
  return 0;
}



void Run15pAu200_DCA_BeamAngle_Recal::getPrimaryVtx(VtxOut *vtxout, float *vx, float *vy, float *vz)
{
  if (vtxout == NULL)
  {
    *vx = -999.0;
    *vy = -999.0;
    *vz = -999.0;
    return;
  }

  string s_vtx = vtxout->which_Vtx();



  PHPoint vtxpos;
  if (m_vtxflag == 1) // simulation
  {
    vtxpos =  vtxout->get_Vertex("SIM");
    m_vtx_found = 1;
    if (verbosity > 2) cout << "Vtx SIM: " << vtxpos.getX() << " " << vtxpos.getY() << " " << vtxpos.getZ() << endl;
    cout << "m_vtxflag == 1, SIM" << endl;
  }
  else if (m_vtxflag == 2) // svxprc
  {
    vtxpos =  vtxout->get_Vertex("SVX_PRECISE");
    m_vtx_found = 1;
    cout << "m_vtxflag == 2, SVX_PRECISE" <<endl;
  }
  else if (m_vtxflag == 3)
  {
    if (verbosity > 0) cout << "Reconstructing SvxCentralTrack with respect to (BCx, BCy, SEEDz)" << endl;
    vtxpos =  vtxout->get_Vertex("SVX");

    PHPoint vtxposbbc =  vtxout->get_Vertex("BBC");
    m_vtx_found = (fabs(vtxpos.getZ() - vtxposbbc.getZ()) > 0.00001) ? 2 : 0; //if bbcz==zvtx, zvtx is bbcz
    //cout << "m_vtxflag == 3, SVX" <<endl;
  }
  else if (m_vtxflag == 4)
  {
    if (verbosity > 2) cout << "Vtx : " << s_vtx.c_str() << " " << vtxpos.getX() << " " << vtxpos.getY() << " " << vtxpos.getZ() << endl;

    // check if the vertex(SVX) is found
    if (s_vtx == "SVX_PRECISE")
    {
      m_vtx_found = 0;
      vtxpos =  vtxout->get_Vertex("SVX_COMBINED");
      cout << "m_vtxflag == 4, s_vtx == SVX_PRECISE, SVX_COMBINED" <<endl;

    }
    else if (s_vtx == "SVX")
    {
      vtxpos =  vtxout->get_Vertex("SVX");

      PHPoint vtxposbbc =  vtxout->get_Vertex("BBC");
      m_vtx_found = (fabs(vtxpos.getZ() - vtxposbbc.getZ()) > 0.00001) ? 2 : 0; //if bbcz==zvtx, zvtx is bbcz
      cout << "m_vtxflag == 4, s_vtx == SVX, SVX" <<endl;
    }  
  }
  else if (m_vtxflag == 5)
  {
    string s_vtxname = "";
    string s_write = "";
    ////////////////////////////////////////////
    bool isCentralityOK = m_centrality>-1;
    //bool isPeripheral   = isCentralityOK ? (m_centrality>=40) : (m_bbccharge<420);
    bool isPeripheral   = true; // DCA from BC for all centralities
 
    static int errCnt = 0;
    if( (!isCentralityOK) && errCnt < 10){
      cout<<"centrality is not available. BBC charge is used instead for Vertex Selection"<<endl;
      errCnt++;
    }

    if(isPeripheral) // seed for peripheral
    {
      vtxpos =  vtxout->get_Vertex("SVX");

      cout << "m_vtxflag == 5, Peripheral, SVX" <<endl;

      // if precise exist, replace zvertex by the precise-z
      if (s_vtx == "SVX_PRECISE")
      {
        PHPoint vtxpospri =  vtxout->get_Vertex();
        vtxpos.setZ(vtxpospri.getZ());
        s_vtxname = s_vtx;
        m_vtx_found = 1;
      }
      else  // seed or bbc
      {
        PHPoint vtxposbbc =  vtxout->get_Vertex("BBC");

        if(fabs(vtxpos.getZ() - vtxposbbc.getZ()) > 0.00001) {//if bbcz==zvtx, zvtx is bbcz
          m_vtx_found = 2;
          s_vtxname = "SVX";
        }
        else {
          m_vtx_found = 0;
          s_vtxname = "BBC";
        }
      }

      s_write = "peripheral";
    }
    else //  default vertex is used for central and mid-central
    {
      vtxpos =  vtxout->get_Vertex();
      s_vtxname = s_vtx;

      cout << "m_vtxflag == 5, central or mid-cenetral, default" <<endl;

      // check if the vertex(SVX) is found
      if (s_vtx == "SVX_PRECISE")
      {
        m_vtx_found = 1;
      }
      else if (s_vtx == "SVX")
      {
        PHPoint vtxposbbc =  vtxout->get_Vertex("BBC");
        if(fabs(vtxpos.getZ() - vtxposbbc.getZ()) > 0.00001) { //if bbcz==zvtx, zvtx is bbcz
          m_vtx_found = 2;
        }
        else {
          m_vtx_found = 0;
          s_vtxname = "BBC";
        }
      }
      else   // if s_vtx==BBC
      {
        m_vtx_found = 0;
      }

      s_write = "central";
    }

    if (verbosity > 2) {
      cout << "Vtx Centrality: " << s_vtxname << " " << s_write << " " << vtxpos << " "<< m_bbccharge <<" "<<m_vtx_found<<endl;
    }


  }
  else
  {
    vtxpos =  vtxout->get_Vertex();

    cout << "else m_vtxflag, default" <<endl;

    if (verbosity > 2) cout << "Vtx : " << s_vtx.c_str() << " " << vtxpos.getX() << " " << vtxpos.getY() << " " << vtxpos.getZ() << endl;

    // check if the vertex(SVX) is found
    if (s_vtx == "SVX_PRECISE")
    {
      m_vtx_found = 1;
    }
    else if (s_vtx == "SVX")
    {
      PHPoint vtxposbbc =  vtxout->get_Vertex("BBC");
      m_vtx_found = (fabs(vtxpos.getZ() - vtxposbbc.getZ()) > 0.00001) ? 2 : 0; //if bbcz==zvtx, zvtx is bbcz
    }
    else   // if s_vtx==BBC
    {
      m_vtx_found = 0;
    }
  }

  if (verbosity > 3)
  {
    PHPoint vtx_prim = vtxout->get_Vertex("SVX_PRECISE");
    PHPoint vtx_seed = vtxout->get_Vertex("SVX");
    PHPoint vtx_bbc   = vtxout->get_Vertex("BBC");
    cout << "VtxZ : ";
    cout << (m_vtx_found==1 ? "Precise" : ( m_vtx_found==2 ? "Seed" : "BBC") ) << " ";
    cout << vtxpos.getZ() << " (";
    cout << vtx_prim.getZ() << ", ";
    cout << vtx_seed.getZ() << ", ";
    cout << vtx_bbc.getZ() << ") ";
    cout << endl;
  }


  *vx = vtxpos.getX();
  *vy = vtxpos.getY();
  *vz = vtxpos.getZ();
}


int Run15pAu200_DCA_BeamAngle_Recal::CreateNodeTree(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);

  // Find DST node.
  PHCompositeNode *dstNode = static_cast<PHCompositeNode *>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
  {
    cerr << PHWHERE << "DST Node missing, doing nothing." << endl;
    return EVENT_OK;
  }

  if (verbosity > 0) cout << "Run15pAu200_DCA_BeamAngle_Recal::CreateNodeTree-I: Execution completed" << endl;

  return EVENT_OK;
}


void Run15pAu200_DCA_BeamAngle_Recal::setPPFlag(int flag)
{
  m_ppflag = flag;

  //If ppflag == true, set the vertex reconstruction flag to be the beam center
  //in (x,y) and the seed vertex in z
  if(m_ppflag == 1)
  {
    m_vtxflag = 3;
  }
}
