
#include "SvxCentralTrackReco.h"

#include <svxAddress.hh>
#include <Fun4AllReturnCodes.h>
#include <SvxCentralTrackListv9.h>
#include <SvxCentralTrackv9.h>
#include <SvxCluster.h>
#include <SvxClusterContainer.h>
#include <SvxTracker.h>
#include <SvxPixelHotDeadMapv2.h>
#include <SvxDeadMap.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <VtxOut.h>
#include <Bbc.hh>
#include <BbcOut.h>
#include <PHPoint.h>

#include <RunHeader.h>
#include <PHGlobal.h>

#include <svxDetectorGeo.hh>
#include <SvxComponentGeom.h>
#include <recoConsts.h>
#include <getClass.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankList.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>
#include <RunToTime.hh>

#include <cmath>
#include <TMath.h>
#include <TVector3.h>

#include <iostream>
#include <iomanip>
#include <exception>
#include <set>

using namespace std;
using namespace findNode;

/////////////////
// constant used in the class

/// \todo get B field from the database?
//static const float B = 0.92;
static const float B = 0.90;    // field strength (tesla)
static const float b = 0.003 * B; // magnetic pT kick(GeV/c) per cm. convert B field to GeV

// verbosity = 0 : no message
// verbosity = 1 : function name
// verbosity = 2 : result of the calculation and judgement
// verbosity = 3 : object identity

// 2012.03.16 change mag_bend calculation since sign of the mag_bend depends on the B-field direction.

// dphi search window
static const float DPHICUT_SUBLAYER[8] =
{0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4}; // cm charge 2012/4/28 (0.7 -> 0.4)

// bbccharge dependent dz search window
static const float DZCUT_SVX = 0.5;            // +-1cm if Prim or Seed vertex is found or if no SVX vertex and bbccharge>200.
static const float DZCUT_BBC_MIDCENTRAL =  6.0; // +-5cm if no Svx vertex and 50<bbccharge<200.
static const float DZCUT_BBC_PERIPHERAL = 10.0; // +-5cm if Prim or Seed vertex is found.

inline float SvxClsLinkNode:: get_zdiff()
{
  float sz = (found) ? cluster->get_xyz_global(2) : 0.0;
  return zproj - sz;
}

inline float SvxClsLinkNode:: get_pdiff()
{
  return dproj + mag_bend;
}

inline int SvxClsLinkNode:: get_layer()
{
  int layer = 0;
  if     (sublayer == 0)             layer = 0;
  else if (sublayer == 1)             layer = 1;
  else if (2 <= sublayer && sublayer < 5) layer = 2;
  else                             layer = 3; // sublayer==5,6,7
  return layer;
}

void SvxClsLink::addnode(SvxClsLinkNode &node)
{
  if (node.found)
  {
    m_previdx = m_nodelink.size();
    m_nhit++;
    if ( node.cluster != NULL &&
         (0 <= node.cluster->get_layer() && node.cluster->get_layer() < 4))
    {
      m_nhitlayer[node.cluster->get_layer()]++;
    }
  }
  m_nodelink.push_back(node); // arg. is ref variable since push_back copy the arg.
}

int SvxClsLink::getInnerMostLayer()
{
  int ilayer = 9999.;
  for (unsigned int inode = 0; inode < m_nodelink.size(); inode++)
  {
    SvxClsLinkNode &node = m_nodelink[inode];
    if (node.found)
    {
      if (0 <= node.get_layer() && node.get_layer() < 4)
      {
        if (ilayer > node.get_layer()) ilayer = node.get_layer();
      }
      else
      {
        cout << "getInnerMostLayer : error unknown layer : " << node.get_layer() << endl;
      }
    }
  }
  return ilayer;
}

void SvxClsLink::setLinkValue(ScgTrack &geoTrack)
{
  // Using the tracking modeler, encode a value that designates the
  // performance of this link.
  m_linkValue = 0;
  int clusFound[4] = {0}, hitFound[4] = {0}, hitIsBad[4] = {0};

  for (int i = 0; i < 4; ++i)
  {
    clusFound[i] = (getNHitLayer(i) > 0) ? 1 : 0;
  }
  for (int ihit = 0; ihit < geoTrack.nhits; ihit++)
  {
    ScgHit geoHit = geoTrack.GetHit(ihit);
    hitFound[geoHit.layer] = 1;
    if (geoHit.status == 0) // Status 0 means hit is in a bad area.
      hitIsBad[geoHit.layer] = 1;
  }

  for (int i = 0; i < 4; ++i)
  {
    m_linkValue |= clusFound[i] << i;     // bit 0-3: Cluster found in layer 0-3.
    m_linkValue |= hitFound[i]  << (i + 4); // bit 4-7: Proj. hit found in layer 0-3.
    m_linkValue |= hitIsBad[i]  << (i + 8); // bit 8-11: Found hit lies in bad area.
    // More slots available, if needed...
  }
  return;
}

void SvxClsLink::setLinkScore(ScgTrack &geoTrack)
{
  // Accounting variables
  int nclus = 0, npixl = 0;

  //the fraction of bad pixels in the tile for a
  vector<float> badFrac_missingLayers;
  vector<int> missingLayers;

  //find the number of clusters in this link
  for (int i = 0; i < 4; ++i)
  {
    if (getNHitLayer(i) > 0)   //cluster found in this layer
    {
      nclus++;
      if (i < 2)
        npixl++;
    }
    else   //no cluster, check for geohit
    {
      missingLayers.push_back(i); //record which layer is missing

      bool foundHit = false;
      for (int ihit = 0; ihit < geoTrack.nhits; ihit++)
      {
        ScgHit geoHit = geoTrack.GetHit(ihit);
        if (geoHit.layer == i)
        {
          badFrac_missingLayers.push_back(1 - geoHit.livefrac);
          foundHit = true;
        }
      }
      if (!foundHit)   //did not find a geohit
      {
        badFrac_missingLayers.push_back(1);
      }
    }

  }



  //calculate the link score
  switch (nclus)
  {

  case 4:
    // Best possible score.
    m_linkScore = 100.;

    break;

  case 3:
    // Favor links including both pixel layers
    m_linkScore = (npixl == 2) ? 70. : 40.;

    // Favor links with projected hits in a bad area or missed hit
    //m_linkScore += (linkval & 1 << (missingLayers.at(0)+8)) ? 20 : 0;
    m_linkScore += badFrac_missingLayers.at(0) * 20.;

    // Favor outermost missing layers
    m_linkScore += missingLayers.at(0);

    break;

  case 2:
    // Favor links including both pixel layers
    //    m_linkScore = (npixl==2) ? 20 : 15;
    m_linkScore = 10.*npixl;

    // Favor links with projected hits in bad areas
    m_linkScore += badFrac_missingLayers.at(0) * 5.;
    m_linkScore += badFrac_missingLayers.at(1) * 5.;

    // Favor outermost missing layers
    m_linkScore += missingLayers.at(0);
    m_linkScore += missingLayers.at(1);

    break;

  default:
    m_linkScore = 0;
    cout << PHWHERE << nclus
         << " clusters found. Should be 2,3,4 only." << endl;
  }

}


void SvxClsLink::print()
{
  for (unsigned int inode = 0; inode < m_nodelink.size(); inode++)
  {
    SvxClsLinkNode &node = m_nodelink[inode];
    if (node.found)
    {
      cout << "(";
      cout << node.clsid << " ";
      cout << setprecision(4);
      cout << setw(5) << node.get_pdiff() << " ";
      cout << setw(5) << node.get_zdiff() << "), ";
      cout << setprecision(6);
      cout << flush;

    }
    else
    {
      cout << "NoClus(" << node.sublayer << "), "  << flush;
    }
  }
  cout << " chi2/ndf=" << m_chi2 << "/" << m_ndf << flush;
  cout << " D2DCA=" << m_d2dca << flush;
  cout << " bestF=" << m_bestlink << flush;
  cout << " N=" << m_nhit << " ("  << flush;
  cout << m_nhitlayer[3] << " ";
  cout << m_nhitlayer[2] << " ";
  cout << m_nhitlayer[1] << " ";
  cout << m_nhitlayer[0] << " ";
  cout << ") "  << flush;
  cout << " Nlay=" << getNHitSum() << flush;
  cout << " id=" << m_id << flush;
  cout << " linkValue=" << m_linkValue << flush;
  cout << endl;
}

int SvxCentralClusterLink::m_verbosity = 0;

void SvxCentralClusterLink::print()
{
  cout << "trk : " << m_trkpart.phi0 << " " << m_trkpart.the0 << endl;
  for (unsigned int i = 0; i < m_vlink.size(); i++)
  {
    SvxClsLink &link = m_vlink[i];
    cout << "   " << i << " "  << flush;
    cout << " bID=" << m_bestLinkId << flush;
    link.print();

  }
}

void SvxCentralClusterLink::setBestLinkId(int bestId)
{
  if ( 0 <= bestId && bestId < (int)m_vlink.size() )
  {
    m_bestLinkId = bestId;
    m_vlink[bestId].m_bestlink = true;
  }
  else
  {
    if (m_verbosity > 1)
      cout << "ERROR bestID exceeds array Size  (" << bestId << " " << m_vlink.size() << ")" << endl;
  }

}

//////////////////////////////////////////////////
// SvxTrackParts
SvxTrackPart::SvxTrackPart(float Mom, int Chrg, float Phi0, float The0) :
  mom(Mom), charge(Chrg), phi0(Phi0), the0(The0)
{
  pt = mom * sin(the0);
  ux = cos(phi0);
  uy = sin(phi0);

  R  = pt / b;
};

//////////////////////////////////////////////////
/// utility function
/// calculate residual(dphi and dz) between a VTX cluster and a PHCentralTrack
void SvxCentralTrackReco::calculate_dphidz(SvxTrackPart &trkpart, // track info
    float svxx, float svxy, float svxz,        // cluster position
    float xvtx, float yvtx, float zvtx,        // prim. vertex position;
    float bDir,                                // direction of B-field
    float *dproj, float *magbend, float *zproj // "output"
                                          )
{
  int   &charge = trkpart.charge;
  float &the0   = trkpart.the0;
  float &pt     = trkpart.pt;

  float &ux     = trkpart.ux;
  float &uy     = trkpart.uy;


  float dx = svxx - xvtx;
  float dy = svxy - yvtx;
  float rhit_2 = dx * dx + dy * dy;
  float rhit = sqrt(rhit_2);

  // vector product (ux,uy) x (dx, dy)
  // this is signed difference between straight line projection
  // from beam spot (xoffset1,yoffset1) and a svx hit position.
  float u_cross_dx = ux * dy - uy * dx;
  float mag_bend   = -bDir * (0.00135 * charge * rhit_2 / pt); // 2012.03.16 dir of mag.bend depends on the field direction.
  // 2013.07.26 0.0013 -> 0.00135
  *dproj   = u_cross_dx;
  *magbend = mag_bend;
  *zproj   = (zvtx + rhit / tan(the0));
}

/////////////////////////////////////////////////
SvxCentralTrackReco::SvxCentralTrackReco(const std::string &name):
  SubsysReco(name),
  m_ppflag(0),
  m_vtxflag(0),
  m_rndmassocflag(0),
  m_windowflag(0),
  m_fieldScale(1.0),
  m_geometry(NULL),
  m_svxCompGeom(NULL),
  m_pixelMap(NULL),
  m_stripMap(NULL),
  m_address(NULL),
  m_printCsv(false),
  m_clearOtherLink(true),
  m_vtx_found(0),
  m_bbccharge(-9999.0),
  m_centrality(-9999.0),
  m_timer(PHTimeServer::get()->insert_new(name)),
  m_zfflag(false),
  m_shiftphcentral(true),
  m_readphcntrotdb(true),
  m_phcntrotdbname("svxphcntrotations"),
  m_enableLinkSplitting(true), 
  m_requireB2B3hit(false),
  m_isFitNoCNT(false),
  m_newdataset_for_run16(false)
{
  m_vtrklist.reserve(100000);

  // set default cut window
  for (int isublayer = 0; isublayer < 8; isublayer++)
  {
    m_cutDPHI[isublayer] = DPHICUT_SUBLAYER[isublayer];
  }

  m_cutDZ[0] = DZCUT_SVX;            // +-1cm if Prim or Seed vertex is found or if no SVX vertex and bbccharge>200.
  m_cutDZ[1] = DZCUT_BBC_MIDCENTRAL; // +-5cm if no Svx vertex and 50<bbccharge<200.
  m_cutDZ[2] = DZCUT_BBC_PERIPHERAL; // +-5cm if Prim or Seed vertex is found.

  m_windowFactor = 1.0; // default factor is 1

  m_rotAngle = 10.0; // originally 5 degree as a default

  for (int i = 0; i < 2; i++)
  {
    m_rotPHcntPhi[i] = 0;
    m_rotPHcntTheta[i] = 0;
  }

  // Change the default behavior of PHCentralTrack rotations in SIMULATIONS
  recoConsts *rc =  recoConsts::instance();
  if ( rc->FlagExist("SIMULATIONFLAG") )
  {
    if ( verbosity)
    {
      cout << "SvxCentralTrackReco::SvxCentralTrackReco --"
           << " Found SIMULATIONFLAG in recoConsts."
           << " Turning off PHCentralTrack rotations by default."
           << endl;
    }
    m_shiftphcentral = false;
  }


  // these vertex info is used for DCA calculation
  m_vtxPrecise[0]    = m_vtxPrecise[1]    = m_vtxPrecise[2]    = -9999.;
  m_vtxBeamCenter[0] = m_vtxBeamCenter[1] = m_vtxBeamCenter[2] = -9999.;

  //cout<<"SvxCentralTrackReco:: this is production version 05/27/2016"<<endl;
  cout<<"SvxCentralTrackReco:: this is production version 01/06/2020"<<endl;

}

SvxCentralTrackReco::~SvxCentralTrackReco()
{
  clearTrackList();
  if (m_svxCompGeom) delete m_svxCompGeom; // the objects is owned in this class
}

int SvxCentralTrackReco::InitRun(PHCompositeNode *topNode)
{
  // Set up the node tree
  int i = CreateNodeTree(topNode);
  if (verbosity > 0) cout << "SvxCentralTrackReco::InitRun-I: CreateNodeTree returned " << i << endl;
  if (!(i == EVENT_OK))
  {
    return EVENT_OK;
  }

  svxDetectorGeo *svxgeometry = findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if (svxgeometry == NULL)
  {
    cout << PHWHERE << "Can't find svxDetectorGeo. " << endl;
    return ABORTRUN;
  }
  m_geometry = svxgeometry;
  m_tracker.set_DetectorGeo(m_geometry);
  m_tracker.setFitNoCNT(m_isFitNoCNT);

  cout<<"SvxCentralTrack fitting : "<<(m_isFitNoCNT ? "with no CNT" : "with CNT")<<" "<<m_isFitNoCNT<<endl;

  //Set SvxTracker in Zero Field mode
  if (m_zfflag) m_tracker.set_ZeroFieldFlag(true);

  // check magnet current
  RunHeader *runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader == NULL)
  {
    cout << PHWHERE << "Can't find runheader. " << endl;
    return ABORTRUN;
  }
  if (runheader->get_currentCentral() > 0)
  {
    m_fieldScale = 1.0;
  }
  else
  {
    m_fieldScale = -1.0;
  }
  cout << "SvxCentralTrackReco::InitRun  fieldScale=" << m_fieldScale << endl;

  /////////////////////////////
  // write the cut status
  cout << "SvxCentralTrackReco::InitRun  setSearchWindowFlag=" << m_windowflag << endl;

  cout << "SvxCentralTrackReco::InitRun  DPHI cut window" << endl;
  cout << "  (";
  for (int ilay = 0; ilay < 8; ilay++)
  {
    cout << m_cutDPHI[ilay] << ", ";
  }
  cout << ")" << endl;

  cout << "SvxCentralTrackReco::InitRun  DPHI cut window" << endl;
  cout << "  (";
  for (int ilay = 0; ilay < 8; ilay++)
  {
    cout << m_cutDPHI[ilay] << ", ";
  }
  cout << ")" << endl;

  cout << "SvxCentralTrackReco::InitRun  DZ cut window" << endl;
  cout << "  (";
  for (int i = 0; i < 3; i++)
  {
    cout << m_cutDZ[i] << ", ";
  }
  cout << ")" << endl;

  cout<<"SvxCentralTrackReco::InitRun WindowFactor : "<<m_windowFactor<<endl;

  // Build model of active volumes for track pointing / deadmap integration
  if (!m_svxCompGeom)
    m_svxCompGeom = new SvxComponentGeom(m_geometry);

  m_svxCompGeom->SetVerbosity(0);

  if ( m_rndmassocflag != 0 )
  {
    cout << "SvxCentralTrackReco::InitRun  Rotation Angle for BG : "
         << m_rotAngle << " degree" << endl;
  }

  // Get deadmap information from SvxPixelHotDeadMapv2 and SvxDeadMap if they
  // exist on the node tree
  bool foundPixelMap = false;
  m_pixelMap = findNode::getClass<SvxPixelHotDeadMapv2>(topNode, "SvxPixelHotDeadMapv2");
  if (!m_pixelMap)
  {
    cout << PHWHERE << "No SvxPixelHotDeadMapv2 found on the node tree. "
         << "SvxComponentGeom will get pixel deadmap information from the database based on the runnumber."
         << endl;
  }
  else
  {
    foundPixelMap = true;
  }

  bool foundStripMap = false;
  m_stripMap = findNode::getClass<SvxDeadMap>(topNode, "SvxStripHotDeadMap");
  if (!m_stripMap)
  {
    cout << PHWHERE << "No SvxDeadMap found on the node tree. "
         << "SvxComponentGeom will get strip deadmap information from the database based on the runnumber."
         << endl;
  }
  else
  {
    foundStripMap = true;
  }


  // Read deadmap from database using run number from DST if it exists.
  recoConsts *rc =  recoConsts::instance();


  if (rc->FlagExist("RUNNUMBER"))
  {
    int runnumber = rc->get_IntFlag("RUNNUMBER");
    m_address = findNode::getClass<svxAddress>(topNode, "svxAddress");
    if (!m_address)
    {
      cout << PHWHERE
           << "No svxAddress object found on node tree. "
           << "New instance will be created in SvxComponentGeom::AssignMap()."
           << endl;
    }
    if (foundPixelMap && foundStripMap && m_address)
    {
      m_svxCompGeom->AssignMap(m_pixelMap, m_stripMap, m_address);
    }
    else
    {
      m_svxCompGeom->AssignMap(runnumber, m_address);
    }

    if (rc->FlagExist("SIMULATIONFLAG") && rc->get_IntFlag("SIMULATIONFLAG") >= 1)
    {
      m_tracker.SetFitParameters("SIM");
      cout<<"SvxCentralTrackReco:: SvxTracker is set as SIM"<<endl;
    }
    else
    {
      m_tracker.SetFitParameters("DATA");
      cout<<"SvxCentralTrackReco:: SvxTracker is set as DATA"<<endl;
    }

    //read PHCentralTrack rotation parameters from DB (if desired)
    if (m_shiftphcentral && m_readphcntrotdb)
      fetchDBPHCentralTrackDphiDtheta(runnumber);

    static const int RUN16_BEGIN = 443085;
    if(runnumber>=RUN16_BEGIN){
      m_newdataset_for_run16 = true;
      
      cout<<"SvxCentralTrackReco: runnumber="<<runnumber<<endl;
    }
  }

  // m_vtxflag
  cout<<"VtxFlag : "<<m_vtxflag;
  if     (m_vtxflag == 1 ) cout<<" SIM";
  else if(m_vtxflag == 2 ) cout<<" SVX_PRECISE";
  else if(m_vtxflag == 3 ) cout<<" SVX(seed)";
  else if(m_vtxflag == 4 ) cout<<" Combined";
  else if(m_vtxflag == 5 ) cout<<" SVX + SVX_PRECISE_Z";
  else if(m_vtxflag == 6 ) cout<<" Centrality";
  else                     cout<<" Default";
  cout<<" is used"<<endl;
  

  // cout
  cout << "SvxCentralTrackReco::InitRun LinkSplittingAlgorithm is "
       << (m_enableLinkSplitting ? "enabled" : "disabled" )<<endl;
  cout << "SvxCentralTrackReco::InitRun B2B3requirement is "
       << (m_requireB2B3hit ? "enabled" : "disabled" )<<endl;

  cout << endl;
  cout << "SvxCentralTrackReco::InitRun verbosity=" << verbosity << endl;


  if(m_newdataset_for_run16){
    cout << endl;
    cout << "------------------------------------"<< endl;
    cout << "  SvxCentralTrackReco::InitRun"<< endl;
    cout << "    newdataset flag is enabled for run16 production"<< endl;
    cout << "    new DCA variable is used to save DCA from vertex selected by the vertex flag."<< endl;
    cout << "    Original DCA variable saves DCA from the beam center, instead."<< endl;
    cout << "------------------------------------"<< endl;
  }
  else {
    cout << endl;
    cout << "------------------------------------"<< endl;
    cout << "  SvxCentralTrackReco::InitRun"<< endl;
    cout << "    newdataset flag is disabled"<< endl;
    cout << "    Original DCA variable saves DCA from the vertex selected by the vertex flag"<< endl;
    cout << "------------------------------------"<< endl;
  }

  return 0;
}

int SvxCentralTrackReco::process_event(PHCompositeNode *topNode)
{
  m_timer.get()->restart();
  static int ievt = 0;

  if (verbosity > 0) cout << "SvxCentralTrackReco::process_event started" << endl;

  PHCentralTrack *trk = getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if (trk == NULL)
  {
    cout << "No PHCentralTrack in the NODE tree" << endl;
    /// \todo Really return EVENT_OK if no PHCentralTrack node?
    return EVENT_OK;
  }

  VtxOut *vtxout = getClass<VtxOut>(topNode, "VtxOut");
  if (vtxout == NULL)
  {
    cout << "No VtxOut in the NODE tree" << endl;
    /// \todo Really return EVENT_OK if no VtxOut node?
    return EVENT_OK;
  }

  SvxClusterContainer *clscont = getClass<SvxClusterContainer>(topNode, "SvxClusterContainer");
  if (clscont == NULL)
  {
    cout << "No ClusterContainer in the NODE tree" << endl;
    /// \todo Really return EVENT_OK if no SvxClusterContainer?
    return EVENT_OK;
  }

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

  PHGlobal *pGlobal = getClass<PHGlobal>(topNode, "PHGlobal");
  if (pGlobal == NULL)
  {
    cout << "No PHGlobal in the NODE tree" << endl;
    m_centrality = -9999.0;
    ///\todo Really ABORTEVENT only if no BbcOut node?
    return ABORTEVENT;
  }
  // check BbcChargeSum
  m_centrality = pGlobal->getCentrality();


  // GetVertex
  float vtx[3];
  // find most precise available vertex position
  getPrimaryVtx(vtxout, &vtx[0], &vtx[1], &vtx[2]);

  // for DCA from precise vertex
  {
    if(vtxout->isVtx("SVX_PRECISE")){
      PHPoint vtxpos_precise =  vtxout->get_Vertex("SVX_PRECISE");
      m_vtxPrecise[0] = vtxpos_precise.getX();
      m_vtxPrecise[1] = vtxpos_precise.getY();
      m_vtxPrecise[2] = vtxpos_precise.getZ();
      //cout<<"is Precise OK"<<endl;
    } 
    else {
      m_vtxPrecise[0] = -999.;
      m_vtxPrecise[1] = -999.;
      m_vtxPrecise[2] = -999.;
    } 
    if(vtxout->isVtx("SVX")){
      PHPoint vtxpos_bc =  vtxout->get_Vertex("SVX");
      m_vtxBeamCenter[0] = vtxpos_bc.getX();
      m_vtxBeamCenter[1] = vtxpos_bc.getY();
      m_vtxBeamCenter[2] = vtxpos_bc.getZ();
      //cout<<"is BeamCenter OK"<<endl;
    } 
    else {
      m_vtxBeamCenter[0] = -999.;
      m_vtxBeamCenter[1] = -999.;
      m_vtxBeamCenter[2] = -999.;
    }

    //cout<<" precise = "
    //    <<m_vtxPrecise[0]<<" "
    //    <<m_vtxPrecise[1]<<" "
    //    <<m_vtxPrecise[2]<<endl;
    //cout<<" beamcenter = "
    //    <<m_vtxBeamCenter[0]<<" "
    //    <<m_vtxBeamCenter[1]<<" "
    //    <<m_vtxBeamCenter[2]<<endl;
  }



  // update the dz cut window value for this event
  // updated value is stored in getDZCut. See source of getDZCut() for details
  float dZcut = getDZCut(true); // update the dz value used based on bbccharge etc.

  if (verbosity > 1)
  {
    cout << "Event Based dZCut : " << dZcut << " +1 = " << dZcut + 1 << endl;
  }

  static int rotate_idx = 0; // 0,1,2,3

  vector<ScgTrack> geoTracks;

  try
  {
    clearTrackList();

    int ntrk = trk->get_npart();
    if (ntrk > 0)
    {
      for (int itrk = 0; itrk < ntrk; itrk++)
      {
        if (verbosity > 5) cout << "trkid=" << itrk << endl;

        PHSnglCentralTrack *sngltrk = trk->get_track(itrk);
        SvxCentralClusterLink *trkseed = new SvxCentralClusterLink(itrk, sngltrk);
        m_vtrklist.push_back(trkseed);// push back list of all possible cluster combinations which could be associated with this central track

        // search link for this track
        float mom    = trk->get_mom(itrk);
        int   charge = trk->get_charge(itrk);
        float the0   = trk->get_the0(itrk);
        float phi0   = trk->get_phi0(itrk);

        //if running zero field,
        //arbitratily set:
        // DC momentum to 100 GeV
        // DC charge to (-)1
        //This (temporarily) fixes calculation problems in
        //calculateChisquareByMultiCircle() and
        //SvxTracker::TrackFit_SvxCNT()
        if (m_zfflag)
        {
          mom = 100.;
          charge = (charge < 0) ? -1 : 1;
        }

        if ( m_rndmassocflag != 0 )
        {
          // sequential method 2012.11.21
          float pmthe = 0., pmphi = 0., tilt = m_rotAngle;
          if      (rotate_idx == 0) { pmphi =  tilt; pmthe =  tilt; }
          else if (rotate_idx == 1) { pmphi = -tilt; pmthe =  tilt; }
          else if (rotate_idx == 2) { pmphi =  tilt; pmthe = -tilt; }
          else                      { pmphi = -tilt; pmthe = -tilt; }

          the0 += pmthe * TMath::Pi() / 180.;
          phi0 += pmphi * TMath::Pi() / 180.;

          rotate_idx++;
          if (rotate_idx >= 4)
          {
            rotate_idx = 0;
          }
        }

        //shift the PHCentralTrack into vtx coordinate
        // system if desired.
        float sphi0 = phi0;
        float sthe0 = the0;
        if (m_shiftphcentral)
        {
          shiftPHCentralTrack( &sphi0, &sthe0);
        }

        // set track info (rotation angle)
        // trkseed->setTrackPart(mom, charge, phi0, the0);
        trkseed->setTrackPart(mom, charge, sphi0, sthe0);

        //-----------------------------------
        // generate Links
        try
        {
          SvxClsLink link;
          LinkClusters(trkseed->m_trkpart,      // track info
                       vtx[0], vtx[1], vtx[2],  // primary vtx
                       link,  clscont,          // link data
                       7, 0.2, dZcut + 1.);     // search condition (block, dphi(radians), dZ)
          // note that search window in Z is wider than dZcut value by 1cm
          // this is to take into account the granuality of find_clusters_block()
        }
        catch (const exception &e)
        {
          cout << "SvxCentralTrackReco::process_event : LinkCluster" << endl;
          cout << e.what() << endl;
          throw;
        }

        //-----------------------------------
        // calculate LinkScore
        ScgTrack geoTrack = m_svxCompGeom->FindHitsFromVertex(vtx[0], vtx[1], vtx[2],
                            mom, sphi0, sthe0,
                            charge, m_fieldScale * B);

        // Always add track, even if no hits, so that geoTracks.size() == ntrk.
        geoTracks.push_back(geoTrack);

        for (int ihit = 0; ihit < geoTrack.nhits; ihit++)
        {
          ScgHit hit = geoTrack.GetHit(ihit);
          if (0)
            Printf("   xyz: (%#6.2f, %#6.2f, %#6.2f) "
                   "address: %d %d %d %d \tstatus %d",
                   hit.x, hit.y, hit.z,
                   hit.layer, hit.ladder, hit.sensor, hit.component,
                   hit.status);


          // Store live fraction at hit location in each layer in
          // SvxCentralClusterLink object. Will be used to fill
          // SvxCentralTrack output.
          trkseed->m_live_percent[hit.layer] = hit.livefrac * 100.;
        }

        // Add geoHit info to links prior to optimalLink(). This
        // includes QA status of areas where clusters are expected,
        // but not found.
        vector< SvxClsLink > &nodelink  = trkseed->m_vlink;
        for (unsigned int ilink = 0; ilink < nodelink.size(); ilink++)
        {
          SvxClsLink *link_p = &(nodelink[ilink]);
          link_p->setLinkValue(geoTrack);
          link_p->setLinkScore(geoTrack);
        }

        //-----------------------------------
        // Calculate most reliable link for this track
        try
        {

          optimalLink(trkseed, vtx[0], vtx[1], vtx[2]);

          // Print link candidate info in CSV format.
          // Use setPrintLinkInfo(true) to set.
          if (m_printCsv)
          {
            PrintCsvLines(ievt, itrk, trkseed, geoTrack);
          }

          // Clear all other links
          if (m_clearOtherLink)
          {
            clearOtherLink(trkseed);
          }

        }
        catch (const exception &e)
        {
          cout << "SvxCentralTrackReco::process_event : optimalLink" << endl;
          cout << e.what() << endl;
          throw;
        }

        if (verbosity > 1)
        {
          trkseed->print();
        }
      }
    }
  }
  catch (const exception &e)
  {
    cout << "SvxCentralTrackReco::process_event : loop_over the track" << endl;
    cout << e.what() << endl;
    throw;
  }

  // store to the output node
  try
  {
    fillCentralTrack(topNode);
  }
  catch (const exception &e)
  {
    cout << "SvxCentralTrackReco::process_event : fillCentralTrack" << endl;
    cout << e.what() << endl;
    throw;
  }

  if (verbosity > 0) cout << "SvxCentralTrackReco::process_event completed" << endl;

  m_timer.get()->stop();

  ievt++;
  return 0;
}

void SvxCentralTrackReco::setPHCentralTrackDphiDtheta(int arm, float dphi, float dtheta)
{
  if (arm >= 0 && arm < 2)
  {
    m_rotPHcntPhi[arm] = dphi;
    m_rotPHcntTheta[arm] = dtheta;

    // cout << PHWHERE << " - set PHCentralTrack rotations for arm " << arm
    //      << " to dphi=" << dphi << " dtheta=" << dtheta << endl;
  }
  else
  {
    cout << PHWHERE << " - arm must be 0 (East) or 1 (West)! arm=" << arm << endl;
  }
}

bool SvxCentralTrackReco::updateDBPHCentralTrackDphiDtheta(const int beginrun,
    const int endrun,
    const char *desc)
{

  if (verbosity)
  {
    cout << "Starting SvxCentralTrackReco::updateDBPHCentralTrackDphiDtheta()..." << endl;
    cout << "         for run range: " << beginrun << " " << endrun << endl;
  }

  //check that the run ranges are valid
  if ( beginrun < 0 || endrun < 0 || endrun < beginrun)
  {
    cout << PHWHERE << " - run range is invalid!"
         << " beginrun=" << beginrun << " endrun=" << endrun << endl;
    return false;
  }

  RunToTime *runTime = RunToTime::instance();

  //begin time
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;

  //end time
  PHTimeStamp Tstop;
  ts = runTime->getEndTime(endrun);
  Tstop = *ts;
  delete ts;

  if (verbosity)
  {
    cout << "  TimeStamp range: ";
    Tstart.print(); cout << " - "; Tstop.print(); cout << endl;
  }


  //check that the description is appropriate
  string s_desc(desc);
  if (s_desc.length() == 0)
  {
    s_desc = string("PHCentralTrack phi/theta rotations for period : ") +
             Tstart.formatTimeString() + " - " + Tstop.formatTimeString();

    cout << PHWHERE
         << "WARNING - desc is empty, building default string based on TimeStamp : "
         << s_desc
         << endl;
  }
  else if (s_desc.length() > 256)
  {
    s_desc = s_desc.substr(0, 256);

    cout << PHWHERE
         << " WARNING - desc must be < 256 characters. Truncated to : "
         << s_desc
         << endl;
  }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbBankID bankID;

  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
  {
    cout << PHWHERE << " - DB not writable! Aborting." << endl;
    application->abort();
    return false;
  }

  bankID.setInternalValue(0);

  PdbCalBank *phcntrotBank = bankManager->createBank(
                               "PdbParameterBank",
                               bankID,
                               desc,
                               Tstart, Tstop,
                               m_phcntrotdbname);

  if (phcntrotBank)
  {
    int length = 2 * 2 + 1; // 2 rotations * 2 arms + 1 hdr
    phcntrotBank->setLength(length);

    PdbParameter *parameter;
    int index = 0;
    parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
    parameter->setParameter(1.0);
    parameter->setName("scheme");

    parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
    parameter->setParameter( m_rotPHcntPhi[0] );
    parameter->setName("PHCntDphiE");

    parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
    parameter->setParameter( m_rotPHcntPhi[1] );
    parameter->setName("PHCntDphiW");

    parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
    parameter->setParameter( m_rotPHcntTheta[0] );
    parameter->setName("PHCntDthetaE");

    parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
    parameter->setParameter( m_rotPHcntTheta[1] );
    parameter->setName("PHCntDthetaW");

    if (verbosity)
    {
      cout << PHWHERE
           << " Committing following parameters to DB="
           << m_phcntrotdbname << " : " << endl;
      cout << "   0(East) dphi=" << m_rotPHcntPhi[0]
           << " dtheta=" << m_rotPHcntTheta[0]
           << endl;
      cout << "   1(West) dphi=" << m_rotPHcntPhi[1]
           << " dtheta=" << m_rotPHcntTheta[1]
           << endl;
    }

    bool success = application->commit();

    delete phcntrotBank;

    return success;
  }
  else
  {
    cout << PHWHERE << " ERROR - Unable to create bank. Aborting" << endl;
    return false;
  }
}

bool SvxCentralTrackReco::fetchDBPHCentralTrackDphiDtheta(const int runnumber)
{

  //is fetch based off run number or time stamp?
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(runnumber));
  PHTimeStamp Trun = *ts;
  delete ts;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startRead())
  {
    cerr << PHWHERE << " DB not readable."
         << " Setting PHCentralTrack rotations to default values (0)." << endl;
    setPHCentralTrackDphiDtheta(0, 0, 0);
    setPHCentralTrackDphiDtheta(1, 0, 0);

    application->abort();
    return false;
  }

  PdbBankID bankID;
  bankID.setInternalValue(0);
  PdbParameter *parameter;
  int index = 0;

  PdbCalBank *phcntrotBank = bankManager->fetchBank(
                               "PdbParameterBank",
                               bankID,
                               m_phcntrotdbname,
                               Trun);
  if (!phcntrotBank)
  {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run "
         << runnumber << " from " << m_phcntrotdbname << endl;
    cout << "    Setting PHCentralTrack rotations to default values (0)." << endl;

    setPHCentralTrackDphiDtheta(0, 0, 0);
    setPHCentralTrackDphiDtheta(1, 0, 0);

    return false;
  }

  int length = 2 * 2 + 1; // 2 arms * 2 rotations  + 1 header value

  int truelength = phcntrotBank->getLength();
  if (length != truelength)
  {
    cout << PHWHERE;
    cout << " FATAL...wrong length DB read for PHCnt Rotations t: " << truelength << endl;
    cout << "                                    expected length: " << length << endl;
    cout << "    Setting PHCentralTrack rotations to default values (0)." << endl;

    setPHCentralTrackDphiDtheta(0, 0, 0);
    setPHCentralTrackDphiDtheta(1, 0, 0);

    return false;
  }

  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
  {
    cout << PHWHERE
         << " FATAL...unknown scheme DB read for PHCnt rotations:"
         << scheme << endl;
    cout << "    Setting PHCentralTrack rotations to default values (0)." << endl;

    setPHCentralTrackDphiDtheta(0, 0, 0);
    setPHCentralTrackDphiDtheta(1, 0, 0);

    return false;
  }

  // OK, ALL Checks passed...get the parameters...

  if (verbosity)
  {
    cout << PHWHERE
         << "READING PHCentralTrack rotations from DB ..."
         << endl;
  }

  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  float dphiE = parameter->getParameter();

  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  float dphiW = parameter->getParameter();

  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  float dtheE = parameter->getParameter();

  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  float dtheW = parameter->getParameter();

  if (verbosity)
  {
    cout << "    dphi   : " << dphiE << " " << dphiW << endl;
    cout << "    dtheta : " << dtheE << " " << dtheW << endl;
  }

  delete phcntrotBank;

  //set the values
  setPHCentralTrackDphiDtheta(0, dphiE, dtheE);
  setPHCentralTrackDphiDtheta(1, dphiW, dtheW);

  return true;

}

void SvxCentralTrackReco::shiftPHCentralTrack(float *phi0, float *the0)
{
  //D. McGlinchey - 11/20/2014
  //Temporary function to shift PHCentralTrack into VTX coordinate system
  //TODO
  //  1) use some sort of DB structure for offsets

  //get arm East=0, West=1
  // D. McGlinchey 12/23/2014 - Could use PHCentralTrack dcarm instead.
  int arm = *phi0 < (float)1.5 ? 1 : 0;

  //rotate phi0
  *phi0 = *phi0 + m_rotPHcntPhi[arm];

  //rotate the0
  *the0 = *the0 + m_rotPHcntTheta[arm];

  return;

}


void SvxCentralTrackReco::PrintCsvLines(int ievent,
                                        int itrack,
                                        SvxCentralClusterLink *trkseed,
                                        ScgTrack &geoTrack)
{
  // Print a comma-separated line to stdout for analysis. Parsing
  // these lines is a simple way to analyze the reconstruction without
  // making big changes to the class.

  // Absolute cluster and hit positions in layers 0-3
  float cx[4] = {0}, cy[4] = {0}, cz[4] = {0}, hx[4] = {0}, hy[4] = {0}, hz[4] = {0};

  //fraction of good pixels in hit tiles
  float fh[4];
  for (int i = 0; i < 4; i++)
    fh[i] = -1;

  for (int ihit = 0; ihit < geoTrack.nhits; ihit++)
  {
    ScgHit hit = geoTrack.GetHit(ihit);

    // Final condition selects inner hit in case of multiple hits/layer.
    if (hit.layer >= 0 && hit.layer <= 3 && hx[hit.layer] == 0)
    {
      hx[hit.layer] = hit.x;
      hy[hit.layer] = hit.y;
      hz[hit.layer] = hit.z;

      //get the fraction of good pixels for each geohit
      fh[hit.layer] = hit.livefrac;
      //printf("GoodFrac: %i %i %i %i %i %i %i %.2f %i\n",ievent,itrack,hit.layer,hit.ladder,hit.sensor,hit.component,hit.tile,fh[hit.layer],hit.status);
    }



    /*
    Printf(" event %d track %d ihit %d  xyz: (%#6.2f, %#6.2f, %#6.2f) "
       "address: %d %d %d %d \tstatus %d",
       ievent, itrack, ihit, hit.x, hit.y, hit.z,
       hit.layer, hit.ladder, hit.sensor, hit.component,
       hit.status);
    */
  }



  vector< SvxClsLink > &nodelink  = trkseed->m_vlink;
  unsigned int nlinks = nodelink.size();
  for (unsigned int ilink = 0; ilink < nlinks; ilink++)
  {
    SvxClsLink *link_p = &(nodelink[ilink]);
    std::vector<SvxClsLinkNode> linkedClusters = link_p->m_nodelink;
    int nlc = linkedClusters.size();

    // Get info on individual clusters
    for (int icl = 0; icl < nlc; ++icl)
    {
      SvxClsLinkNode clnode = linkedClusters.at(icl);
      SvxCluster *cl = clnode.cluster;
      if (cl)
      {
        int l = cl->get_layer();
        if (l >= 0 && l <= 3 && cx[l] == 0)   // Select inner hit if >1 clusters/layer.
        {
          getClusterPos(cl, &cx[l], &cy[l], &cz[l]);
        }
      }
    }

    // Print all info in one line
    printf("csvout %d,%d,%u,%u,"
           "%d,%d,%.1f,"
           "%.3f,%.3f,%d,%d,"
           "%.3f,"
           "%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,"
           "%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,"
           "%.3f,%.3f,%.3f,%.3f\n"
           ,
           ievent, itrack, nlinks, ilink,
           link_p->m_nhit, link_p->getLinkValue(), link_p->getLinkScore(),
           link_p->m_linkQuality, link_p->m_chi2, link_p->m_ndf, link_p->m_bestlink,
           trkseed->m_central->get_mom(),
           cx[0], cy[0], cz[0],
           cx[1], cy[1], cz[1],
           cx[2], cy[2], cz[2],
           cx[3], cy[3], cz[3],
           hx[0], hy[0], hz[0],
           hx[1], hy[1], hz[1],
           hx[2], hy[2], hz[2],
           hx[3], hy[3], hz[3],
           fh[0], fh[1], fh[2], fh[3]
          );


    //for each cluster, print a comparison of addresses
    for (int icl = 0; icl < nlc; ++icl)
    {
      SvxClsLinkNode clnode = linkedClusters.at(icl);
      SvxCluster *cl = clnode.cluster;
      if (cl)
      {
        //if (cl->get_layer() < 2) //only consider inner two layers for now
        {

          int hlayer = -1, hladder = -1, hsensor = -1, hcomp = -1, htile = -1;
          int clayer = cl->get_layer();
          int cladder = cl->get_ladder();
          int csensor = cl->get_sensor();
          int ccomp = m_svxCompGeom->GetComponent(clayer, cladder, csensor,
                                                  cl->get_xyz_local(0),
                                                  cl->get_xyz_local(1),
                                                  cl->get_xyz_local(2));
          int ctile = m_svxCompGeom->GetTile(clayer, cladder, csensor,
                                             cl->get_xyz_local(0),
                                             cl->get_xyz_local(1),
                                             cl->get_xyz_local(2));

          //find the corresponding geoHit
          for (int ihit = 0; ihit < geoTrack.nhits; ihit++)
          {
            ScgHit hit = geoTrack.GetHit(ihit);
            if (hit.layer == cl->get_layer())
            {
              hlayer = hit.layer;
              hladder = hit.ladder;
              hsensor = hit.sensor;
              hcomp = hit.component;
              htile = hit.tile;
            }
          }



          //print
          printf("tileOut: %.2f "
                 "%i %i %i %i %i "
                 "%i %i %i %i %i\n"
                 ,
                 trkseed->m_central->get_mom(),
                 clayer, cladder, csensor, ccomp, ctile,
                 hlayer, hladder, hsensor, hcomp, htile);

        }
      }
    }

  }

  /*
  // NTuple variable list:
  const char* vars =
    "event:track:nlinks:link:nclus:value:score:qual:chi2:ndf:best:mom:"
    "cx0:cy0:cz0:cx1:cy1:cz1:cx2:cy2:cz2:cx3:cy3:cz3:"
    "hx0:hy0:hz0:hx1:hy1:hz1:hx2:hy2:hz2:hx3:hy3:hz3";
  */
  return;
}

int SvxCentralTrackReco::End(PHCompositeNode *topNode)
{
  return 0;
}

void SvxCentralTrackReco::setPPFlag(int flag)
{
  m_ppflag = flag;

  //If ppflag == true, set the vertex reconstruction flag to be the beam center
  //in (x,y) and the seed vertex in z
  if(m_ppflag == 1)
  {
    m_vtxflag = 3;
  }
}

void SvxCentralTrackReco::LinkClusters(
  SvxTrackPart &trkpart, // track info
  float xvtx, float yvtx, float zvtx,            // primary vtx
  SvxClsLink          &link,                     // link data
  SvxClusterContainer *d_svxcls,                 // SvxClusterContainer
  int block, float dphi, float dz)            // search condition
{
  // For each layer, add a hit(if exist) or null(no hit), and go to the next layer(recursive call).
  // A null is inserted in the following condition
  //  1) There is no hit in the layer
  //  2) in B3 (block==7), a null is always inserted.
  //  3) in B2 (block==4), a null is inserted if B3 has a hit.
  //  in this way, we require that at least 1 hit in B3 or B2.


  if (d_svxcls == NULL)
  {
    cout << PHWHERE << "No SvxClusterContainer" << endl;
    return;
  }
  if (Rsub(0) == 0.0)
  {
    cout << "SvxCentralTrackReco::LinkCluster Rsub is not initialized yet : " << Rsub(0) << endl;
  }

  // prev hit
  int prevIdx = link.getPrevHitIdx();
  SvxClsLinkNode *prevhit  = (prevIdx == -1) ? NULL : &(link.m_nodelink[prevIdx]);

  bool found = false;
  float &phi0   = trkpart.phi0;
  float &the0   = trkpart.the0;
  float  z0     = Rsub(block) / tan(the0) + zvtx;

  // this should be changed to search more effectively.
  //    calculate projection point on this sublayer

  // phi0 should be tilted here
  float  L = Rsub(block);
  float &R = trkpart.R;
  int   &charge = trkpart.charge;
  float tilt_phi = m_fieldScale * charge * 0.5 * asin(L / R);
  float phi0_new = phi0 + tilt_phi;

  // get cluster by block
  vector<SvxCluster *> vclus;
  vector<int> vsublayer;
  float dphi_new = (block == 0) ? 0.4 : dphi; // this dphi is in radians!
  d_svxcls->find_clusters_block(vclus, vsublayer, block, phi0_new, dphi_new, z0, dz); // 2013.01.15 version
//--  cout<<"Ncluster : "<<vclus.size()<<" block: "<<block<<endl;


  // minimum hit is 2
  if (block <= 0 && link.m_nhit < 1) // stop condition of reccursive call
  {
    return;
  }

  // hit condition used 
  bool isB2B3hit = true;
  if(m_requireB2B3hit) { isB2B3hit = (link.getNHitLayer(2)>0 || link.getNHitLayer(3)>0);  }

  // cluster loop
  for (unsigned int icls = 0; icls < vclus.size(); icls++)
  {
    try
    {
      SvxCluster *clus0 = vclus[icls];

      float svxx, svxy, svxz;
      getClusterPos(clus0, &svxx, &svxy, &svxz);

      int sublayer = vsublayer[icls];

      int   id = clus0->get_hitID();

      // calculate dproj, magbend and zproj for dphi and dz cut
      float dproj, mag_bend, zproj;
      calculate_dphidz(trkpart,   // track info
                       svxx, svxy, svxz,         // cluster position
                       xvtx, yvtx, zvtx,         // prim. vertex position;
                       m_fieldScale,             // direction of B-field
                       &dproj, &mag_bend, &zproj // "output"
                      );


      // create new node
      SvxClsLinkNode newnode(clus0, dproj, mag_bend, zproj, sublayer, true, id);
      if ( (prevIdx == -1 && fabs(dproj + mag_bend) < m_cutDPHI[sublayer] && applyDZCut(&newnode)) || // if first hit, dphi cut is apply,
           (prevIdx >= 0  && applyCorrelationCut(&newnode, prevhit))     // if not, correlation cut is apply
         )
      {
        // acceptable hit is found (dphi window and/or correlation cut).
        // Make a new branch of tree to continue this link direction
        // Note that you must make new_link here. Otherwise, you have a problem
        // if there is more than one acceptable hit

        if (verbosity > 5)
        {
          cout << " block: " << block << "  found " << icls << "  " << dproj << " " << mag_bend << " : " << sublayer << " " << svxx << " " << svxy << endl;
        }

        SvxClsLink newlink = link; // split the link
        try
        {
          newlink.addnode(newnode);
        }
        catch (const exception &e)
        {
          cout << "SvxCentralTrackReco::LinkCluster : addnode newlink " << endl;
          cout << e.what() << endl;
          throw;
        }

        if (sublayer <= 0) // stop condition of reccursive call
        {

          if(!isB2B3hit) { return; } // require B2 or B3 hit for the link

          try
          {
            SvxCentralClusterLink *trklink = m_vtrklist.back();
            int linkid = trklink->m_vlink.size();
            newlink.m_id = linkid;

            trklink->m_vlink.push_back(newlink);
            if (verbosity > 5) cout << "Add New Link." << endl;

          }
          catch (const exception &e)
          {
            cout << "SvxCentralTrackReco::LinkCluster : addlink to trk" << endl;
            cout << e.what() << endl;
            throw;
          }
        }
        else
        {
          int next_block = sublayer - 1;
          //Search Next SubLayer
          LinkClusters(trkpart, // track info
                       xvtx, yvtx, zvtx,        // primary vtx
                       newlink,                 // link data
                       d_svxcls,                // SvxClusterContainer
                       next_block, dphi, dz);   // search condition
        }

        found = true;

      } // if hit is in the window
//--      else {
//--        cout<<"NG : ";
//--        if(prevIdx == -1){
//--          cout<<"outmost ";
//--          cout<<"dphi:"<<(fabs(dproj + mag_bend) < m_cutDPHI[sublayer] ? "OK": Form("NG %f < %f", fabs(dproj+mag_bend), m_cutDPHI[sublayer]));
//--          cout<<", dz:"<<(applyDZCut(&newnode)?"OK":"NG");
//--        }
//--        else if(prevIdx >= 0){
//--          cout<<"layer prev:"<<prevIdx;
//--          cout<<" corr_cut :"<<(applyCorrelationCut(&newnode, prevhit)?"OK":"NG");     // if not, correlation cut is apply
//--        }
//--        else{
//--          cout<<"unknown prev:"<<prevIdx;
//--        }
//--        cout<<endl;
//--      }

    }
    catch (const exception &e)
    {
      SvxCentralClusterLink *trklink = m_vtrklist.back();
      cout << "SvxCentralTrackReco::LinkCluster : loop_over the clusters : nlink : " << trklink->m_vlink.size() << endl;
      cout << e.what() << endl;
      throw;
    }
  } // for (int icls);

  ////////////////////////////////
  if (
    ( m_enableLinkSplitting &&
      (
        block == 7 ||                 // for B3, always put null cluster
       (prevIdx != -1 && block == 4)  // for B2, put NULL cluster only when B3 has a hit.
      )
    )
    || 
    (!found)                        // this means that particle passes through dead area. Search Next SubLayer
  )
  {
    if (verbosity > 5)  cout << " block: " << block << "  nofound " << endl;

    try
    {
      SvxClsLink newlink = link;

      if (block <= 0) // stop condition of reccursive call
      {

        if ( newlink.m_nhit < 2 || // minimum number of hit is 2.
            !isB2B3hit)            // require B2 or B3 hit if flag is ON
        {
          return;
        }


        SvxCentralClusterLink *trklink = m_vtrklist.back();
        int linkid = trklink->m_vlink.size();
        newlink.m_id = linkid;


        trklink->m_vlink.push_back(newlink); // save current link 

        if (verbosity > 5) cout << "not found. Stop LinkCluster" << endl;
        return;
      }

      if ((block == 0 || block == 1 || block == 4 || block == 7))
      {
        // add null cluster to the link
        SvxClsLinkNode newnode(NULL, -999., -999., -999., block, false, -1); // false
        newlink.addnode(newnode);
      }

      int next_block = 0;
      if      (5 <= block && block < 8) next_block = 4;
      else if (2 <= block && block < 5) next_block = 1;
      else                              next_block = block - 1;

      //Search Next SubLayer
      LinkClusters(trkpart,                 // track info
                   xvtx, yvtx, zvtx,        // primary vtx
                   newlink,                 // link data
                   d_svxcls,                // SvxClusterContainer
                   next_block, dphi, dz);   // search condition

    }
    catch (const exception &e)
    {
      SvxCentralClusterLink *trklink = m_vtrklist.back();
      cout << "SvxCentralTrackReco::LinkCluster : not found the clusters : nlink : " << trklink->m_vlink.size() << endl;
      cout << e.what() << endl;
      throw;
    }
  }

}

int SvxCentralTrackReco::optimalLink(SvxCentralClusterLink *trk,
                                     float vx, float vy, float vz)
{
  // Alternative to old chooseBestLink().
  // Find best link using combination of link score and chi squared:

  double bestLinkQual = 0;
  double d2dca = -999;     // d2dca not currently used.
  int best_link_id = -1;  // Index of best link. Purpose of fn. is to assign this.

  // Collection of all link candidates
  vector< SvxClsLink > &nodelink  = trk->m_vlink;

  // Compare quality of all link candidates
  for (unsigned int ilink = 0; ilink < nodelink.size(); ilink++)
  {

    SvxClsLink *link_p  = &(nodelink[ilink]);

    // See setLinkScore() for details. Scale by 1/max so range is [0,1].
    float score = 0.01 * link_p->getLinkScore();

    /* COMMENTED FOR TESTING -- UNCOMMENT WHEN FINISHED
    // Skip rubbish link candidates to save time
    if (score < 20./40)
    continue;
    */

    // Compute chi2 and assign to link_p
    calculateChisquareByMultiCircle(trk, link_p, vx, vy, vz, &d2dca);
    float chi2  = link_p->m_chi2 / link_p->m_ndf;  // Multi-circle (not DCA) chi^2

    /* COMMENTED FOR TESTING -- UNCOMMENT WHEN FINISHED
    // Skip NANs
    if (chi2 != chi2)
    continue;
    */

    //    link_p->m_linkQuality = TMath::Sqrt(1.0/chi2/chi2 + score*score);
    link_p->m_linkQuality = 1.0 / (chi2 + 2) + score;

    if (link_p->m_linkQuality > bestLinkQual)
    {
      bestLinkQual = link_p->m_linkQuality;
      best_link_id = link_p->m_id;
    }

    /*
    //--------------------------------------------------------------------------
    // EXTRA: Compute DCA chi squared value.
    // 1. Why isn't link.m_chi2_2 set within the function?
    // 2. chi2_2 not needed here. Do others need it?
    // 3. Does it cost significant time? It will be called a LOT.
    float chi2_dphi, chi2_dz, dca_d2, dca_z;
    int   ndf, ndf_dphi, ndf_dz;
    calculateChisquareByDCA(trk, link_p,
    &chi2_dphi, &ndf_dphi, &dca_d2,
    &chi2_dz,   &ndf_dz,   &dca_z,
    &chi2,      &ndf);
    link_p->m_chi2_2 = chi2;
    //--------------------------------------------------------------------------
    */
  }

  if (best_link_id != -1)
  {
    trk->setBestLinkId(best_link_id);
  }

  return best_link_id;
}



void SvxCentralTrackReco::calculateChisquareByMultiCircle(SvxCentralClusterLink *trk,
    SvxClsLink *link,
    double vx, double vy, double vz, // primary vertex
    double *d2dca)                 // return value
{
  double mom  = trk->m_trkpart.mom;
  int   chg  = trk->m_trkpart.charge;
  double phi0 = trk->m_trkpart.phi0;
  double the0 = trk->m_trkpart.the0;
  // cnt info (-Pi < phi0 < Pi)
  if (phi0 > TMath::Pi())
  {
    phi0 -= (2.0 * TMath::Pi());
  }
  double pt   = mom * sin(the0);
  double charge = (double)chg;

  // search inner most hit
  int ilast = (int)link->m_nodelink.size() - 1;
  SvxClsLinkNode *node = NULL;
  for (int i = ilast; i >= 0; i--) // search inner most hit
  {
    node = &link->m_nodelink[i];
    if (node->found)
    {
      break;
    }
  }
  if (node == NULL || node->cluster == NULL)
  {
    cerr << "SvxCentralTrackReco::calculateChisquareByMultiCircle == No Cluster Object is found" << endl;
    return;
  }

  double hx = (double)node->cluster->get_xyz_global(0);
  double hy = (double)node->cluster->get_xyz_global(1);

  // phi1 which is phi angle at innermost layer
  double r = sqrt((hx - vx) * (hx - vx) + (hy - vy) * (hy - vy));
  double phi1 = phi0 + charge * (m_fieldScale * b * r / pt); // dphi = b*r/pT

  double dcapos[3];
  calcDCAbyCircleProjection
  (pt, phi1, charge, hx, hy, // pt , phi, charge at inner most layer and its hit position
   vx, vy,                   // primary vertex
   & (dcapos[0]), &(dcapos[1]),  // DCA position
   d2dca);                       // return


  vector<SvxCluster *> vcluster;
  for (int i = ilast; i >= 0; i--) // search inner most hit
  {
    if ( link->m_nodelink[i].cluster )
    {
      vcluster.push_back(link->m_nodelink[i].cluster);
    }
  }
  bool helicity = (chg * m_fieldScale > 0) ? true : false;
  /// update alpha, phi0, pt
  double dphi0 = phi0 - trk->m_central->get_phi0();
  if ( dphi0 > TMath::Pi() * 1.5 ) dphi0 -= TMath::Pi() * 2.;
  if ( dphi0 < TMath::Pi() * (-1.5) ) dphi0 += TMath::Pi() * 2.;
  double alpha = trk->m_central->get_alpha();
  double phi = trk->m_central->get_phi() + dphi0;
  /// the second terms of alpha and phi are for 5-deg rotated fake tracks.
  double new_alpha = alpha + dcapos[0] * sin(phi) / 220. - dcapos[1] * cos(phi) / 220.;
  double new_pt = pt * fabs(alpha / new_alpha);
  double new_phi0 = phi0 + 2.0195 * (new_alpha - alpha);
  link->m_the0 = the0;
  double chisq;
  int ndf;
  double chi_phi;
  double chi_z;
  double chi_angle;


  m_tracker.TrackFit_SvxCNT(vcluster, helicity, new_pt, new_phi0, the0,
                            dcapos[0], dcapos[1], chisq, ndf, chi_phi, chi_z, chi_angle);


  /// calculate DCA again
  double px = m_tracker.get3Mom(0);
  double py = m_tracker.get3Mom(1);
  //float pz = m_tracker.get3Mom(2);
  float xexp;
  float yexp;
  float zexp;
  m_tracker.getExpectedPosition(vcluster[0]->get_layer(), xexp, yexp, zexp, 0);
  vector<float> pos_primary(3, 0);
  vector<float> mom_primary(3, 0);
  //--vector<double> vertex(3, 0);
  //--vertex.push_back(vx);
  //--vertex.push_back(vy);
  //--vertex.push_back(vz);
  double new_pt2 = sqrt(px * px + py * py);
  double new_phi02;
  calcDCAbyCircleProjection
  (new_pt2, atan2(py, px), the0, charge, xexp, yexp, zexp,
   vx, vy,
   &(dcapos[0]), &(dcapos[1]), &(dcapos[2]), &new_phi02, d2dca);
  link->m_d2dca = *d2dca;
  link->m_dca_z = dcapos[2] - vz;

  /*
    float new_alpha2 = alpha + dcapos[0]*sin(phi)/220. - dcapos[1]*cos(phi)/220.;
    new_pt2 = pt*alpha/new_alpha2;
    float new_phi02 = phi0 + 2.0195*(new_alpha2-alpha);
    link->m_pt = new_pt2;
    link->m_phi0 = new_phi02;
  */
  link->m_pt = new_pt2;
  link->m_phi0 = new_phi02;

  if (TMath::IsNaN(chisq) || TMath::IsNaN(ndf))
  {
    if (verbosity > 1)
      cout << PHWHERE << "chi2 is NaN, setting to 123456789. " << endl;
    chisq = 123456789;
    ndf  = 0;
  }

  /// fill in
  link->m_chi2 = chisq;
  link->m_ndf = ndf;
  link->m_chi2_dphi = chi_phi;
  link->m_chi2_dz = chi_z;
  ///

  // DCA value
  link->m_dcapos[0] = dcapos[0];
  link->m_dcapos[1] = dcapos[1];
  link->m_dcapos[2] = dcapos[2];

  // projection position
  int nodeIDlist[4][2] = {{ -1, -1}, { -1, -1}, { -1, -1}, { -1, -1}};
  int nhit_layer[4] = {0, 0, 0, 0};
  SvxClsLinkNode *node_tmp = NULL;
  for ( int i = ilast; i >= 0; i-- ) // search inner most hit
  {
    node_tmp = &link->m_nodelink[i];
    if ( node_tmp->found )
    {
      int layer = node_tmp->cluster->get_layer();
      nodeIDlist[layer][nhit_layer[layer]] = i;
      nhit_layer[layer]++;
    }
  }
  int ncls = 0;
  for ( int ilyr = 0; ilyr < 4; ilyr++ )
  {
    for ( int ihit = 0; ihit < nhit_layer[ilyr]; ihit++ )
    {
      link->m_nodelink[nodeIDlist[ilyr][ihit]].fit_dphi = m_tracker.getDPHI(ncls);
      link->m_nodelink[nodeIDlist[ilyr][ihit]].fit_dz = m_tracker.getDZ(ncls);
      link->m_nodelink[nodeIDlist[ilyr][ihit]].scatter = m_tracker.getScatter(ncls);
      link->m_nodelink[nodeIDlist[ilyr][ihit]].scatter_xy = m_tracker.getScatterXY(ncls);
      link->m_nodelink[nodeIDlist[ilyr][ihit]].scatter_rz = m_tracker.getScatterRZ(ncls);
      link->m_nodelink[nodeIDlist[ilyr][ihit]].sscatter = m_tracker.getSScatter(ncls);
      link->m_nodelink[nodeIDlist[ilyr][ihit]].pxout = m_tracker.getMomOut(ncls, 0);
      link->m_nodelink[nodeIDlist[ilyr][ihit]].pyout = m_tracker.getMomOut(ncls, 1);
      link->m_nodelink[nodeIDlist[ilyr][ihit]].pzout = m_tracker.getMomOut(ncls, 2);
      ncls++;
    }
  }

  // chi2 of svxcnt w/o mom vector
  link->m_chi2_nocnt = m_tracker.getChi2SvxCntNoCNT();
  link->m_ndf_nocnt  = m_tracker.getNDFSvxCntNoCNT();

  // chi2 comparison
  //
  //
  // calculate DCA from primary vertex

  double dcapos_precise[3], new_phi02_precise, d2dca_precise;
  calcDCAbyCircleProjection
  (new_pt2, atan2(py, px), the0, charge, xexp, yexp, zexp,
   m_vtxPrecise[0], m_vtxPrecise[1],
   &(dcapos_precise[0]), &(dcapos_precise[1]), &(dcapos_precise[2]), 
   &new_phi02_precise, &d2dca_precise);
  link->m_d2dca_precise = d2dca_precise;
  link->m_dca_z_precise = dcapos_precise[2] - m_vtxPrecise[2];
  
  // calculate DCA from beam center
  double dcapos_bc[3], new_phi02_bc, d2dca_bc;
  calcDCAbyCircleProjection
  (new_pt2, atan2(py, px), the0, charge, xexp, yexp, zexp,
   m_vtxBeamCenter[0], m_vtxBeamCenter[1],
   &(dcapos_bc[0]), &(dcapos_bc[1]), &(dcapos_bc[2]), 
   &new_phi02_bc, &d2dca_bc);
  link->m_d2dca_bc = d2dca_bc;
  link->m_dca_z_bc = dcapos_bc[2] - m_vtxBeamCenter[2];

//  cout<<"chi2 : "<<link->m_chi2<<" "<<link->m_chi2_nocnt<<" : "<<link->m_ndf<<" "<<link->m_ndf_nocnt<<endl;
}

void SvxCentralTrackReco::clearTrackList()
{
  vector<SvxCentralClusterLink *>::iterator itr;
  for (itr = m_vtrklist.begin(); itr != m_vtrklist.end(); ++itr)
  {
    SvxCentralClusterLink *trk = *itr;
    delete trk;
  }
  m_vtrklist.clear();
}

void SvxCentralTrackReco::getClusterPos(SvxCluster *cls, float *sx, float *sy, float *sz)
{
  // if nesessary, cluster position shoule be moved for alignment
  if (cls != NULL)
  {
    *sx  = cls->get_xyz_global(0);
    *sy  = cls->get_xyz_global(1);
    *sz  = cls->get_xyz_global(2);
  }
  else
  {
    *sx  = -999.0;
    *sy  = -999.0;
    *sz  = -999.0;
  }
}

void SvxCentralTrackReco::getPrimaryVtx(VtxOut *vtxout, float *vx, float *vy, float *vz)
{
  if (vtxout == NULL)
  {
    *vx = -999.0;
    *vy = -999.0;
    *vz = -999.0;
    m_vtx_found = 0;
    return;
  }


  string s_vtx = vtxout->which_Vtx();

  PHPoint vtxpos;
  if (m_vtxflag == 1) // simulation
  {
    vtxpos =  vtxout->get_Vertex("SIM");
    m_vtx_found = 1;
    if (verbosity > 2) cout << "Vtx SIM: " << vtxpos.getX() << " " << vtxpos.getY() << " " << vtxpos.getZ() << endl;
  }
  else if (m_vtxflag == 2) // svxprc
  {
    vtxpos =  vtxout->get_Vertex("SVX_PRECISE");
    m_vtx_found = 1;
  }
  else if (m_vtxflag == 3)
  {
    if (verbosity > 0) cout << "Reconstructing SvxCentralTrack with respect to (BCx, BCy, SEEDz)" << endl;
    vtxpos =  vtxout->get_Vertex("SVX");

    PHPoint vtxposbbc =  vtxout->get_Vertex("BBC");
    m_vtx_found = (fabs(vtxpos.getZ() - vtxposbbc.getZ()) > 0.00001) ? 2 : 0; //if bbcz==zvtx, zvtx is bbcz
  }
  else if (m_vtxflag == 4)
  {
    if (verbosity > 2) cout << "Vtx : " << s_vtx.c_str() << " " << vtxpos.getX() << " " << vtxpos.getY() << " " << vtxpos.getZ() << endl;

    // check if the vertex(SVX) is found
    if (s_vtx == "SVX_PRECISE")
    {
      m_vtx_found = 0;
      vtxpos =  vtxout->get_Vertex("SVX_COMBINED");

    }
    else if (s_vtx == "SVX")
    {
      vtxpos =  vtxout->get_Vertex("SVX");

      PHPoint vtxposbbc =  vtxout->get_Vertex("BBC");
      m_vtx_found = (fabs(vtxpos.getZ() - vtxposbbc.getZ()) > 0.00001) ? 2 : 0; //if bbcz==zvtx, zvtx is bbcz
    }  
  }
  else if (m_vtxflag == 5)
  {
    string s_vtxname = "";
    vtxpos =  vtxout->get_Vertex("SVX");

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


    if (verbosity > 2) {
      cout << "SVX + SVX_PRECISE_Z: " << s_vtxname << " " << m_vtx_found << " " << vtxpos << endl;
    }


  }
  else if (m_vtxflag == 6)
  {
    string s_vtxname = "";
    string s_write = "";
    ////////////////////////////////////////////
    bool isCentralityOK = m_centrality>-1;
    bool isPeripheral   = isCentralityOK ? (m_centrality>=40) : (m_bbccharge<420);
 
    static int errCnt = 0;
    if( (!isCentralityOK) && errCnt < 10){
      cout<<"centrality is not available. BBC charge is used instead for Vertex Selection"<<endl;
      errCnt++;
    }

    if(isPeripheral) // seed for peripheral
    {
      vtxpos =  vtxout->get_Vertex("SVX");

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

bool SvxCentralTrackReco::applyCorrelationCut(SvxClsLinkNode *hit, SvxClsLinkNode *prevhit)
{
  if (hit == NULL || prevhit == NULL)
  {
    if (hit == NULL)     cout << "hit is NULL" << endl;
    if (verbosity > 1)
    {
      if (prevhit == NULL) cout << "prevhit is NULL" << endl;
    }

    return false; // is prev hit is NULL, correlation cut is always true
  }

  // cut value;

  // index is combinations between the four layers as defined in getCorrelationIdx
  static const float dPhiCutCorr[6] = {0.10, 0.15, 0.30, 0.10, 0.15, 0.05}; // cm
  static const float dZCutCorr[6]   = {0.25, 0.25, 0.30, 0.25, 0.20, 0.07}; // cm
//  static const float dZCutCorr[6]   = {0.25, 0.25, 0.30, 0.10, 0.20, 0.07}; // cm

  ////////////////////
  int layer      = hit->get_layer();
  int layer_prev = prevhit->get_layer();

  int idx = getCorrelationIdx(layer, layer_prev);
  if (idx < 0)
  {
    return false;
  }

  float dpcut = m_windowFactor * dPhiCutCorr[idx];
  float dzcut = m_windowFactor * dZCutCorr[idx];


  ////////////////////
  float dp_slope = getCorrelationSlopeDPParallel(layer, layer_prev); // 2012.4.20 updated.
  float dz_slope = getCorrelationSlopeDZParallel(layer, layer_prev); // 2012.4.20 updated.

  if (dp_slope <= 0)
  {
    cout << "No IDX is identified : " << layer << "-" << layer_prev << endl;
    return false;
  }


  float dp      = hit->get_pdiff();
  float dp_prev = prevhit->get_pdiff();

  float dz      = hit->get_zdiff();
  float dz_prev = prevhit->get_zdiff();

//--  float dp_diff = fabs(dp_prev - (dp_slope * dp));
//--  float dz_diff = fabs(dz_prev - (dz_slope * dz));
//--
//--  bool isOK_dp = dp_diff < dpcut;
//--  bool isOK_dz = dz_diff < dzcut;
//--
//--  cout<<"  diff : "<<dp_diff<<"("<<dpcut<<"):"<<(isOK_dp?"OK":"NG")<<" ";
//--  cout             <<dz_diff<<"("<<dzcut<<"):"<<(isOK_dz?"OK":"NG")<<endl;
//--  if(!isOK_dp) cout<<" dpNG :"<<dp<<" "<<dp_prev<<endl;
//--  if(!isOK_dz) cout<<" dzNG :"<<dz<<" "<<dz_prev<<" : "<<hit->zproj<<" "<<prevhit->zproj<<" "<<endl;
  

  return ( fabs(dp_prev - (dp_slope * dp)) < dpcut) &&
         ( fabs(dz_prev - (dz_slope * dz)) < dzcut );
}

///
/// this function returns the actual dzcut value used in applyDZCut()
/// the dzcut value is changed event by event, based on the centrality or external flag.
/// this is because Zvertex position is poorly determined in peripheral event
///
/// In order to speed up this function, the dzcut value is stored in the function
/// as a static variable, which is updated when update flag is true. This is done
/// at the beginning of an event
///
/// Input: bool update    flag to update the dzcut value stored in the function
///    if update == true, the dz cut is re-calculated (updated),
///    if update == false (default), the dzcut which is stored in the function is returned.
///
/// return: float dzcut   dzcut value that is used in applyDCCut
///
float SvxCentralTrackReco::getDZCut(bool update)
{
  ///////////////////////////
  // choose dZcut parameter using vertex condition and bbccharge

  static float dZcut = m_cutDZ[0];  // dzcut value used in applyDZCut
  // m_cutDZ[0] is the default value (narrowest dZcut)

  if (update)   // update the value of dZcut
  {
    // m_windowflag=1, 2, 3 sets specific dZCut value. Used for debugging purpose
    if (     m_windowflag == 1)
    {
      dZcut = m_cutDZ[0]; /*DZCUT_SVX;           */
    }
    else if (m_windowflag == 2)
    {
      dZcut = m_cutDZ[1]; /*DZCUT_BBC_MIDCENTRAL;*/
    }
    else if (m_windowflag == 3)
    {
      dZcut = m_cutDZ[2]; /*DZCUT_BBC_PERIPHERAL;*/
    }
    else   // m_windowflagflag==0. This is the normal use of the function.
    {
      if ( m_vtx_found ==1 )    // if precision vertex is found, use the default (narrowest)
      {
        if (      0 <= m_bbccharge && m_bbccharge < 50) dZcut = m_cutDZ[0] * 4.; /*DZCUT_ZVX*4=2cm;*/
        else if (50 <= m_bbccharge && m_bbccharge < 200) dZcut = m_cutDZ[0] * 2.; /*DZCUT_ZVX*2=1cm;*/
        else                                       dZcut = m_cutDZ[0];    /*DZCUT_SVX  =0.5cm;*/
      }
      else if ( m_vtx_found ==2 )// if seed vertex is found, use the default (narrowest)
      {
        if (      0 <= m_bbccharge && m_bbccharge < 50)  dZcut = m_cutDZ[0] * 8.; /*DZCUT_ZVX*4=2cm;*/
        else if (50 <= m_bbccharge && m_bbccharge < 200) dZcut = m_cutDZ[0] * 4.; /*DZCUT_ZVX*2=1cm;*/
        else                                             dZcut = m_cutDZ[0] * 2.; /*DZCUT_SVX  =0.5cm;*/
      }
      else   // select dZCut value according to the BBCcharge. This is to expand dZCut for
      {
        // peripheral event where Zvertex position is poorly determined
        if (      0 <= m_bbccharge && m_bbccharge < 50) dZcut = m_cutDZ[2]; /*DZCUT_BBC_PERIPHERAL;*/
        else if (50 <= m_bbccharge && m_bbccharge < 200) dZcut = m_cutDZ[1]; /*DZCUT_BBC_MIDCENTRAL;*/
        else                                       dZcut = m_cutDZ[0]; /*DZCUT_SVX; bbccharge>200 or bbccharge<0 */
      }
    }

    if (verbosity > 2)
    {
      cout << "applyDZcut : " << dZcut << " ";
      cout << " (" << m_vtx_found << ", " << m_bbccharge << ")" << endl;
    }
  }
  return dZcut;  //zcut value used by applyDCZut
}

bool SvxCentralTrackReco::applyDZCut(SvxClsLinkNode *hit)
{
  // this is the function to choose the associated cluster at the most outer layer

  float dZcut = getDZCut();

  return ( fabs(hit->get_zdiff()) < dZcut );
}


int SvxCentralTrackReco::getCorrelationIdx(int layer, int layer_prev)
{

  int idx = 0;
  if     (layer == 2 && layer_prev == 3) idx = 0;
  else if (layer == 1 && layer_prev == 3) idx = 1;
  else if (layer == 0 && layer_prev == 3) idx = 2;
  else if (layer == 1 && layer_prev == 2) idx = 3;
  else if (layer == 0 && layer_prev == 2) idx = 4;
  else if (layer == 0 && layer_prev == 1) idx = 5;
  else                             idx = -1;

  if (verbosity > 5) cout << "layer-prev: " << layer << "-" << layer_prev << " = " << idx << endl;

  return idx;
}


float SvxCentralTrackReco::getCorrelationSlopeDPParallel(int layer, int layer_prev)
{
  //  return correlation slope,
  //  The slope are determined from single particle of B->e simulation with ideal geometry
  //  1:1 correlation is adopted

  // idx=    0,     1,     2,     3,     4,     5
  //     B2-B3, B1-B3, B0-B3, B1-B2, B0-B2, B0-B1
  static const float dp_slope[6] =
  {
    0.9323, 0.8412, 0.8105, 0.9081, 0.8736, 0.9656 // udpate 2012.04.20 from simulation
  };

  //  if layer==layer_prev, slope should be one since the radius of layer is same.
  if (layer == layer_prev) return 1.0;

  int idx = getCorrelationIdx(layer, layer_prev);
  if (idx < 0)
  {
    return -1;
  }

  return dp_slope[idx];
}

float SvxCentralTrackReco::getCorrelationSlopeDZParallel(int layer, int layer_prev)
{
  //  return correlation slope,
  //  The slope are determined from single particle of B->e simulation with ideal geometry
  //  1:1 correlation is adopted

  // idx=    0,     1,     2,     3,     4,     5
  //     B2-B3, B1-B3, B0-B3, B1-B2, B0-B2, B0-B1
  static const float dz_slope[6] =
  {
    0.9589, 0.9656, 0.9537, 0.9799, 0.9680, 0.9888 // udpate 2012.02.01
  };

  //  if layer==layer_prev, slope should be one since the radius of layer is same.
  if (layer == layer_prev) return 1.0;

  int idx = getCorrelationIdx(layer, layer_prev);
  if (idx < 0)
  {
    return -1;
  }

  return dz_slope[idx];
}




void SvxCentralTrackReco::calcDCAbyCircleProjection(
  double pt, double phi, int charge, // pt in xy-plane, phi, charge at inner most layer
  double hx, double hy,              // hit position at inner most layer
  double vx, double vy,              // primary vertex
  double *dx, double *dy,            // dca position in 2D
  double *d2dca)                    // return
{

  // vector to rotation center at hit1
  double R  = pt / b;

  // this is updated definition 20120317
  double b_sign = (m_fieldScale > 0) ? -1.0 : 1.0;
  double cx = hx + (b_sign) * charge * R * ( sin(phi));
  double cy = hy + (b_sign) * charge * R * (-cos(phi));

  // L is a distance btw the rotation center and primary vtx
  // phi is a angle of the vector starting from the rotation center to primary vertex
  double L = sqrt((vx - cx) * (vx - cx) + (vy - cy) * (vy - cy));
  double psi = atan2((vy - cy), (vx - cx));

  // DCA point
  *dx = cx + R * cos(psi);
  *dy = cy + R * sin(psi);

  // DCA value
  *d2dca = b_sign * (charge * (R - L));

  // *d2dca = charge*(R - L); // original definition 20120317
}

void SvxCentralTrackReco::calcDCAbyCircleProjection(
  double pt, double phi, double the,  // pt in xy-plane, phi, theta at inner most layer
  int charge,                         // charge of the track
  double hx, double hy, double hz,    // hit position at inner most layer
  double vx, double vy,               // primary vertex
  double *dx, double *dy, double *dz, // dca position
  double *phi0, double *d2dca)        // return
{

  // vector to rotation center at hit1
  double R  = pt / b;
  double pz = pt / tan(the);

  // this is updated definition 20120317
  double b_sign = (m_fieldScale > 0) ? -1.0 : 1.0;
  double dir = ( (b_sign) * charge > 0. ) ? -1.0 : 1.0;
  double cx = hx - dir * R * sin(phi);
  double cy = hy + dir * R * cos(phi);

  // L is a distance btw the rotation center and primary vtx
  // phi is a angle of the vector starting from the rotation center to primary vertex
  double L = sqrt((vx - cx) * (vx - cx) + (vy - cy) * (vy - cy));
  double psi = atan2((vy - cy), (vx - cx));

  // DCA point
  *dx = cx + R * cos(psi);
  *dy = cy + R * sin(psi);

  *phi0 = psi + dir * 0.5 * M_PI;
  double dphi = phi - (*phi0);
  if ( dphi > M_PI * 1.5 ) dphi -= M_PI * 2;
  else if ( dphi < -M_PI * 1.5 ) dphi += M_PI * 2;
  //  double dzdphi = dir*pz*R/pt;
  double dzdphi = pz * R / pt;
  *dz = hz - fabs(dphi) * dzdphi;

  // DCA value
  *d2dca = b_sign * (charge * (R - L));
}

int SvxCentralTrackReco::CreateNodeTree(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);

  // Find DST node.
  PHCompositeNode *dstNode = static_cast<PHCompositeNode *>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
  {
    cerr << PHWHERE << "DST Node missing, doing nothing." << endl;
    return EVENT_OK;
  }


  // Find SVX node.
  PHCompositeNode *svxNode = dynamic_cast<PHCompositeNode *> (iter.findFirst("PHCompositeNode", "SVX"));
  if (! svxNode)
  {
    cerr << PHWHERE << "SVX node missing, doing nothing." << endl;
    return EVENT_OK;
  }

  PHIODataNode<PHObject> *SvxCentralTrackListNode = NULL;

  PHString svxcntrklst = "SvxCentralTrack";
  if (m_rndmassocflag != 0)
  {
    svxcntrklst += "Back";
  }
  svxcntrklst += "List";

  SvxCentralTrackListNode = (PHIODataNode<PHObject> *)iter.findFirst("PHIODataNode", svxcntrklst);
  if (!SvxCentralTrackListNode)
  {
    cout << "svxcentraltracks node added" << endl;
    SvxCentralTrackList *svxcentraltracks = new SvxCentralTrackListv9(); // v2 is used from 2012/2/21
    // v3 is used from 2012/3/21
    // v4 is used from 2012/4/13
    // v5 is used from 2012/4/28
    // v6 is used from 2013/2/20
    // v8 is used from 2016/5/18
    // v9 is used from 2020/1/6
    SvxCentralTrackListNode = new PHIODataNode<PHObject>(svxcentraltracks, svxcntrklst, "PHObject");
    svxNode->addNode(SvxCentralTrackListNode);
    cout << "svx node added" << endl;
  }



  if (verbosity > 0) cout << "SvxCentralTrackReco::CreateNodeTree-I: Execution completed" << endl;

  return EVENT_OK;
}


int SvxCentralTrackReco::fillCentralTrack(PHCompositeNode *topNode)
{

  // GetNode
  string svxcntrklst = "SvxCentralTrack";
  if (m_rndmassocflag != 0)
  {
    svxcntrklst += "Back";
  }
  svxcntrklst += "List";
  SvxCentralTrackList *listObj = getClass<SvxCentralTrackList>(topNode, svxcntrklst);
  if (listObj == NULL)
  {
    cout << "No SvxCentralTrackList in the NODE tree" << endl;
    return EVENT_OK;
  }
  if (verbosity > 2) listObj->identify();

  // Filling output node
  int ntrk = m_vtrklist.size();

  if (ntrk > 0) // ntrk is # of DchTrack
  {
    for (int itrk = 0; itrk < ntrk; itrk++)
    {
      SvxCentralClusterLink *trklink = m_vtrklist[itrk];
      //PHSnglCentralTrack    *trk     = trklink->m_central;
      vector<SvxClsLink>    &vlink   = trklink->m_vlink;

      for (int ilink = 0; ilink < (int)vlink.size(); ++ilink)
      {
        SvxClsLink &link = vlink[ilink];

        if (link.m_bestlink)   // best associated link
        {
          SvxCentralTrack *newtrk = new SvxCentralTrackv9(); // v2 is used from 2012/2/21
          // v3 is used from 2012/3/21
          // v4 is used from 2012/4/15
          // v5 is used from 2012/4/28
          // v6 is used from 2013/2/20
          // v7 is used from 2013/7/29
          // v8 is used from 2016/5/18
          // v9 is used from 2020/1/6


          newtrk->setDchIndex(trklink->m_trkid);
          newtrk->setQuality(TMath::Prob(link.m_chi2, link.m_ndf));
          newtrk->setLinkQuality(link.m_linkQuality);
          newtrk->setLinkScore(link.getLinkScore());
          //newtrk->setDCA2D(link.m_d2dca);
          //newtrk->setDCAZ(link.m_dca_z);
          
          // swap dca variables 2021.1.6
          // original : 
          //    DCA from vertex selected by vertexflag 
          // new treatment : 
          //    DCA from vertex selected by vertexflag is set to DCA2Dprimary (new variable)
          //    and 
          //    DCA from beam center is always set to DCA2D(original variable)
          //
          // we decided to save 2 DCA variables after long discussion based on 
          // run14 analysis and  calibration of geometry alignment for run16.
          // One is DCA from the vertex selected by vertexflag(original), 
          // the other is from beam center.
          // For this purpose, new variables (DCA2Dprimary and DCAZprimary) are 
          // added to SvxCentralTrack and used to save the original DCA.
          // Instead, The DCA from beam center is set to the original DCA variable.
          // This treatment is to keep the same meaning of the DCA variable with run14.
          // For Run14 production, DCA from beamcenter is used. 
          // We keep this meaning of the varialbe for run16.
          // 
          //
          if(m_newdataset_for_run16){
            newtrk->setDCA2D(link.m_d2dca_bc);
            newtrk->setDCAZ(link.m_dca_z_bc);
            newtrk->setDCA2Dprimary(link.m_d2dca);
            newtrk->setDCAZprimary(link.m_dca_z);
          }
          else {
            newtrk->setDCA2D(link.m_d2dca);
            newtrk->setDCAZ( link.m_dca_z);
            newtrk->setDCA2Dprimary(link.m_d2dca_bc);
            newtrk->setDCAZprimary( link.m_dca_z_bc);
          }

          // d2dca0 : 1st quick calculation of DCA
          //-- removed from v8 newtrk->setD2DCA0(link.m_d2dca0);
          //cout<<"DCA : "<< link.m_d2dca<< " "<<link.m_d2dca_precise<< " "<<link.m_d2dca_bc<<endl;


          // set rotation angle
          newtrk->setRotatedAngle(0, trklink->m_trkpart.phi0);
          newtrk->setRotatedAngle(1, trklink->m_trkpart.the0);
          //cout<<"RotationAngle : "<<trklink->m_trkpart.phi0<<" "<<trklink->m_trkpart.the0<<endl;

          // DCA position
          newtrk->setClosestApproach(link.m_dcapos[0], link.m_dcapos[1], link.m_dcapos[2]);

          // isPrimary
          newtrk->setIsPrimary(true); // now always primary

          // refined momentum at DCA
          //newtrk->set3MomentumAtPrimaryVertex(trk->get_px(), trk->get_py(), trk->get_pz());
          /// modified by akimoto
          float px = link.m_pt * cos(link.m_phi0);
          float py = link.m_pt * sin(link.m_phi0);
          float pz = link.m_pt / tan(link.m_the0);
          newtrk->set3MomentumAtPrimaryVertex(px, py, pz);
          ///

          // Chi-Square same as quality
          newtrk->setChiSquare(link.m_chi2);
          newtrk->setNDF(link.m_ndf);

          newtrk->setChiSquare2(link.m_chi2_nocnt);
          newtrk->setChiSquareNoCNT(link.m_chi2_nocnt);
          newtrk->setNDFNoCNT(      link.m_ndf_nocnt);

          newtrk->setChiSquareDPHI(link.m_chi2_dphi);
          //-- removed from v8 newtrk->setNDFDPHI(link.m_ndf_dphi);
          newtrk->setChiSquareDZ(link.m_chi2_dz);
          //-- removed from v8 newtrk->setNDFDZ(link.m_ndf_dz);

          // cout of chi2
          if (verbosity > 1)
          {
            cout<<"itrk : "<<ilink<<", chi2: "<<link.m_chi2 <<"("<<link.m_ndf 
                           <<"), chi2(nocnt):"<<link.m_chi2_nocnt<<"("<<link.m_ndf_nocnt<<")"<<endl;
          }

          // Unique
          int unique = 0;
          for (int isub = 0; isub < 8; isub++)
          {
            int bit = (trklink->m_multihit[isub] == 1) ? 1 : 0;
            unique |= (bit << isub);
          }
          newtrk->setUnique(unique);
          if (verbosity > 1)
          {
            cout << "unique 0x" << hex << unique << dec << endl;
          }

          float adc[2] = {0.0, 0.0};


          // associated clusters
          vector<SvxClsLinkNode> &nodelink = link.m_nodelink;

          for (int inode = 0; inode < (int)nodelink.size(); inode++)
          {
            SvxClsLinkNode &node = nodelink[inode];
            bool found = node.found;
            if (found)
            {
              SvxCluster *cls = node.cluster;

              SvxClusterInfov5 cinfo;
              cinfo.setClusterId(node.clsid);
              cinfo.setLayer((char)cls->get_layer());
              cinfo.setLadder((char)cls->get_ladder());
              cinfo.setSensor((char)cls->get_sensor());
              cinfo.setPosition(cls->get_xyz_global(0), cls->get_xyz_global(1), cls->get_xyz_global(2));
              cinfo.setSize(cls->get_size());
              cinfo.setXZSize(cls->get_xz_size(0), cls->get_xz_size(1));
              cinfo.setEdgeFlag(cls->get_edgeflag());
              cinfo.setAdc(cls->get_adc(0), cls->get_adc(1));
              cinfo.setCircumference(cls->get_circumference());
              cinfo.setAmbiguous(cls->get_ambiguous());

              cinfo.setdproj(node.dproj);
              cinfo.setbend(node.mag_bend);
              cinfo.setzproj(node.zproj);

              cinfo.setfitdphi(node.fit_dphi);
              cinfo.setfitdz(  node.fit_dz);
              cinfo.setscatter(node.scatter);
              cinfo.setscatterXY(node.scatter_xy);
              cinfo.setscatterRZ(node.scatter_rz);
              cinfo.setsscatter(node.sscatter);

              if (cls->get_layer() == 2)
              {
                adc[0] += (cls->get_adc(0) + cls->get_adc(1));
              }
              else
              {
                adc[1] += (cls->get_adc(0) + cls->get_adc(1));
              }

              cinfo.setNcold(cls->get_Ncold());
              cinfo.setNhot(cls->get_Nhot());

              if (verbosity > 1)
              {
                cout << " layer : x:y:z : ";
                cout << cinfo.getClusterId() << " ";
                cout << (int)cinfo.getLayer() << " ";
                cout << cinfo.getPosition(0) << " ";
                cout << cinfo.getPosition(1) << " ";
                cout << cinfo.getPosition(2) << " " << endl;
              }


              newtrk->addClusterInfo(&cinfo);
            }
          }


          //get the live fraction in each layer
          //if we find a cluster, it's based off
          //the cluster position, else it's based
          //off the geoTrack
          for (int i = 0; i < 4; i++)
          {
            float livePerc = -1;

            //get the live fraction for the found cluster.
            //in the strip layers, find the live fraction of the
            //sublayer with the largest live fraction
            for (int inode = 0; inode < (int)nodelink.size(); inode++)
            {
              SvxClsLinkNode &node = nodelink[inode];
              bool found = node.found;
              if (found)
              {
                SvxCluster *cluster = node.cluster;
                if (cluster->get_layer() == i)
                {
                  //get the layer, ladder, sensor, comp indeces
                  int layer = cluster->get_layer();
                  int ladder = cluster->get_ladder();
                  int sensor = cluster->get_sensor();

                  //get the local coordinates
                  float lx = cluster->get_xyz_local(0);
                  float ly = cluster->get_xyz_local(1);
                  float lz = cluster->get_xyz_local(2);

                  //get the component (chip or half-module) index
                  int comp = m_svxCompGeom->GetComponent(layer, ladder, sensor, lx, ly, lz);

                  //get the tile index
                  int tile = m_svxCompGeom->GetTile(cluster->get_layer(), cluster->get_ladder(), cluster->get_sensor(), lx, ly, lz);

                  //get the tile live fraction
                  float liveperc = m_svxCompGeom->GetTileGoodFrac(layer, ladder, sensor, comp, tile) * 100.;

                  //get the highest live fraction
                  if (liveperc > livePerc)
                    livePerc = liveperc;
                }
              }
            }

            if (livePerc >= 0) //we found a cluster
              newtrk->setLivePercentage(i, livePerc);
            else //use the previously filled ghit
              newtrk->setLivePercentage(i, trklink->m_live_percent[i]);

          }

          // dE/dx
          newtrk->set_dEdX1(adc[0]);
          newtrk->set_dEdX2(adc[1]);

/*
          /////////////////////////////////////////
          // for test 
          // calculate chi2 using new chi2 option (no CNT angle)
          {
            VtxOut *vtxout = getClass<VtxOut>(topNode, "VtxOut");

            float vx,vy,vz;
            // find most precise available vertex position
            getPrimaryVtx(vtxout, &vx, &vy, &vz);


            SvxClsLink link_cp = link; // copy the best link object
            float d2dca_newchi2;
            
            //m_tracker.setFitNoCNT(true);
            cout<<"fill"<<endl;
            calculateChisquareByMultiCircle(trklink, &link_cp, vx, vy, vz, &d2dca_newchi2);
            
            //m_tracker.setFitNoCNT(m_isFitNoCNT);
          }
*/

          // object added to TCloneArray
          int ncentral = listObj->get_nCentralTracks();
          listObj->addCentralTrack(ncentral, *newtrk);

          delete newtrk;
        } // if (bestlink)
      } // for(ilink)

    } // for(itrk)
  } // if ntrk>0

  if (verbosity > 0) cout << "fillCentralTrack completed" << endl;
  return EVENT_OK;
}


double SvxCentralTrackReco::Rsub(const int isub)
{
  return (m_geometry != NULL) ? m_geometry->get_Rsublayer(isub) : 0.0;
}


void SvxCentralTrackReco::clearOtherLink(SvxCentralClusterLink *trk)
{
  if (0 <= trk->m_bestLinkId &&
      trk->m_bestLinkId < (int)trk->m_vlink.size() )   // best link exist
  {
    SvxClsLink link = trk->m_vlink[trk->m_bestLinkId];

    //////////////////////
    //  before
    //cout<<"before  "<<trk->m_vlink.size()<<" "<<trk->m_bestLinkId<<" ";
    //cout<<link.m_nhit<<" "<<link.m_chi2<<" "<<link.m_bestlink<<endl;

    // clear vlink
    trk->m_vlink.clear();

    // add best link
    link.m_id = 0;
    trk->m_vlink.push_back(link);
    trk->m_bestLinkId = 0;

    //////////////////////
    //  after
    //SvxClsLink alink = trk->m_vlink[0];
    //cout<<"after   "<<trk->m_vlink.size()<<" "<<trk->m_bestLinkId<<" ";
    //cout<<alink.m_nhit<<" "<<alink.m_chi2<<" "<<alink.m_bestlink<<endl;
  }
  else if (trk->m_bestLinkId >= (int)trk->m_vlink.size() )   // best link id exceeds the array length.
  {
    if (verbosity > 0)
    {
      cerr << "Error SvxCentralTrackReco::clearOtherLink";
      cerr << " bestID is invalid : " << trk->m_bestLinkId;
      cerr << " (" << trk->m_vlink.size() << ")" << endl;
    }
  }
  else if (trk->m_bestLinkId < 0)
  {
    if (verbosity > 3)
    {
      cout << "no best link. clear all link" << endl;
    }
    trk->m_vlink.clear();
  }
}
