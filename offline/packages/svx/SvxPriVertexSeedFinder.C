#include "SvxPriVertexSeedFinder.h"
#include "SvxClusterContainer.h"
#include "SvxCluster.h"
#include "svxDetectorGeo.hh"

#include <PHGlobal.h>
#include <BbcOut.h>
#include <VtxOut.h>
#include <VtxOutv7.h>

#include <getClass.h>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <TH1.h>

#include <cmath>
#include <cstring> // for memset

using namespace std;

static const int H_VTX_SEED_NBIN = 640;

SvxPriVertexSeedFinder::SvxPriVertexSeedFinder(const string& name, const int flag):
  SubsysReco(name),
  m_bbcz(0.),
  m_side(flag),
  m_use_SvxVtxOut(false),
  _timer(PHTimeServer::get()->insert_new(name))
{
  cout << "SvxPriVertexSeedFinder::SvxPriVertexSeedFinder()" << endl;
  /*!
   * this initializes the arrays to 0 independent of their size
   * if you initialize to 0 ints and floats work
   * any other value use std::fill
   */
  memset(m_nclscmb, 0, sizeof(m_nclscmb));
  memset(m_mean, 0, sizeof(m_mean));
  memset(m_nassestrk, 0, sizeof(m_nassestrk));
  //memset(m_nclstr_thrshld, 0, sizeof(m_nclstr_thrshld));
  /*!
   *--- ncluster threshold is changed as a function of BBC charge ---
   *
   * Kazuya Nagashima (see the slides at VTX software meeting, Mar. 2, 2016)
   */
  m_nclstr_thrshld[0] = 30; // ncluster threshold for normal event
  m_nclstr_thrshld[1] = 5;  // ncluster threshold for low multiplicity event
  m_h_vtxseed = new TH1F(TString::Format("m_h_vtxseed_%d", m_side), "zvtx", H_VTX_SEED_NBIN, -20, 20);
}


SvxPriVertexSeedFinder::~SvxPriVertexSeedFinder()
{
  delete m_h_vtxseed;
}


int SvxPriVertexSeedFinder::InitRun(PHCompositeNode* topNode)
{
  cout << "SvxPriVertexSeedFinder::InitRun()" << endl;
  if (m_use_SvxVtxOut) {
    int i = CreateNodeTree(topNode);
    if (verbosity > 0) {
      cout << "SvxPriVertexSeedFinder::InitRun-I: CreateNodeTree returned " << i << endl;
    }
    if (i != EVENT_OK) return EVENT_OK;
  }

  return EVENT_OK;
}


const int sublayermin[2][2] = {{0, 1}, {2, 5}};
const int sublayermax[2][2] = {{1, 2}, {5, 8}};
/// sublayermin[i][j] : minimum sublayer ID
/// sublayermax[i][j] : maximum sublayer ID + 1
/// i : pixel (i=0) or stripixel (i=1) layer
/// j : inner (j=0) or outer (j=1) layer
const float half_z[2] = {11.0, 19.5};


int SvxPriVertexSeedFinder::process_event(PHCompositeNode* topNode)
{
  if (verbosity > 0) 
  {
    cout << "SvxPriVertexSeedFinder::process_event()" << endl;
  }
  _timer.get()->restart();

  ////////////////////
  // Initialization //
  ////////////////////
  int nclust[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int   clstcmb = 0;
  //  float evec[3];
  float bbcq  = 0;
  float bbcqN = 0;
  float bbcqS = 0;
  vector<SvxCluster*> vclust[8];

  ////////////////////
  // Get BBC charge //
  ////////////////////
  PHGlobal* phglobal = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if (phglobal != NULL) {
    bbcqN = phglobal->getBbcChargeN();
    bbcqS = phglobal->getBbcChargeS();
    bbcq = bbcqN + bbcqS;
  } else {
    cout << "No phglobal found" << endl;
    bbcq = 0;
  }

  /////////////////////////////////////
  // Get index of ncluster threshold //
  // as a function of BBC charge     //
  /////////////////////////////////////
  int IndexThreshold = getIndexThreshold(bbcq); //get index of ncluster threshold
  //cout<< "IndexThreshold: " << IndexThreshold <<",  Ncluster threshold: "<< m_nclstr_thrshld[IndexThreshold]<<endl;

  BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (bbcout != NULL) {
    m_bbcz = bbcout->get_VertexPoint();
  } else {
    cout << "No bbcout found" << endl;
    m_bbcz = 0.;
  }

  SvxClusterContainer *vtxclusters =
    findNode::getClass<SvxClusterContainer>(topNode, "SvxClusterContainer");
  if (vtxclusters == NULL) {
    if (verbosity > 0) {
      cout << PHWHERE << "Can't find SvxClusterContainer. " << endl;
    }
    return ABORTRUN;
  }
  if (!vtxclusters->is_initialized()) {
    if (verbosity > 0) {
      cout << PHWHERE << "SvxClusterContainer is not initialized." << endl;
    }
    return ABORTRUN;
  }

  svxDetectorGeo* svxGeometry =
    findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if (svxGeometry == NULL) {
    if (verbosity > 0) {
      cout << PHWHERE << "Can't find svxDetectorGeo. " << endl;
    }
    return ABORTRUN;
  }

  /// get beam center position
  float beam_x;
  float beam_y;
  vtxclusters->get_beam_center(beam_x, beam_y);

  VtxOut* vtxout = NULL;

  if (!m_use_SvxVtxOut)
  {
    vtxout = findNode::getClass <VtxOut>(topNode, "VtxOut");
  }
  else
  {
    vtxout = findNode::getClass <VtxOut>(topNode, "SvxVtxOut");
  }
  if (!vtxout)
  {
    cout << PHWHERE << "ERROR: Can't find VtxOut." << endl;
    return EVENT_OK;
  }

  /// if can't find vertex seed, these values are stored.
  float tver[3] = {beam_x, beam_y, m_bbcz};

  for ( int ps = 0; ps < 2; ps++ ) {
    m_nclscmb[ps] = 0;
    m_mean[ps] = 888.;
    m_nassestrk[ps] = 0;
  }

  vector<double> zvtx_array[H_VTX_SEED_NBIN + 2];

  for ( int ps = 0; ps < 2; ps++ ) { // ps = 0 : pixel layer, ps = 1 : stripixel layer
    if ( 20 < abs(m_bbcz) ) continue;  // out of VTX acceptance
    m_h_vtxseed->Reset();
    //m_h_vtxseed->GetXaxis()->SetRange(1,H_VTX_SEED_NBIN);

    for ( int i = 0; i < H_VTX_SEED_NBIN + 2; i++ ) {
      if ( zvtx_array[i].size() != 0 ) {
        zvtx_array[i].clear();
      }
    }

    clstcmb = 0;
    for ( int sublayer0 = sublayermin[ps][0]; sublayer0 < sublayermax[ps][0]; sublayer0++ ) {

      /////////////////////
      // First Selection //
      /////////////////////
      // Number of cluster within z search window //
      if (m_side == 0) { // all
        nclust[sublayer0] = vtxclusters->find_clusters(vclust[sublayer0], sublayer0);
      }
      else if (m_side == 1) { // west only
        nclust[sublayer0] = vtxclusters->find_clusters(vclust[sublayer0], sublayer0, 0, M_PI / 2);
      }
      else { // east only
        nclust[sublayer0] = vtxclusters->find_clusters(vclust[sublayer0], sublayer0, M_PI, M_PI / 2);
      }

      if (verbosity > 0) {
        cout << " m_bbcz=" << m_bbcz;
        cout << " sublayer0=" << sublayer0;
        cout << " nclust = " << nclust[sublayer0] << endl;
      }

      if ( nclust[sublayer0] < m_nclstr_thrshld[IndexThreshold] ) {
        if (verbosity > 0) cout << "ps =" << ps << "  not enough hit in " << sublayer0 << ". Skip" << endl;
        continue;
      }
      for ( int cls0 = 0; cls0 < nclust[sublayer0]; cls0++ ) {
        float x_in = vclust[sublayer0][cls0]->get_xyz_global(0) - beam_x;
        float y_in = vclust[sublayer0][cls0]->get_xyz_global(1) - beam_y;
        float z_in = vclust[sublayer0][cls0]->get_xyz_global(2);

        for ( int sublayer1 = sublayermin[ps][1]; sublayer1 < sublayermax[ps][1]; sublayer1++ ) {
          //////////////////////
          // Second Selection //
          //////////////////////
          // Search window in z and phi direction on layer 1 or layer 3 //
          float phi0 = vtxclusters->calc_phi(x_in, y_in);
          float dphi = 2.*M_PI / 180.; // 2 degree

          // Number of cluster within z and phi search windows //
          nclust[sublayer1] = vtxclusters->find_clusters(vclust[sublayer1], sublayer1, phi0, dphi);

          for ( int cls1 = 0; cls1 < nclust[sublayer1]; cls1++ ) {
            float x_out = vclust[sublayer1][cls1]->get_xyz_global(0) - beam_x;
            float y_out = vclust[sublayer1][cls1]->get_xyz_global(1) - beam_y;
            float z_out = vclust[sublayer1][cls1]->get_xyz_global(2);

            // Get unit vector of line which made by combination of clusters //
            // float vec[3];
            // vec[0] = x_out - x_in;
            // vec[1] = y_out - y_in;
            // vec[2] = z_out - z_in;
            //            float vecl = sqrt(vec[0] * vec[0] + vec[1] * vec[1] + vec[2] * vec[2]);
            // evec[0] = vec[0] / vecl;
            // evec[1] = vec[1] / vecl;
            // evec[2] = vec[2] / vecl;

            //////////////////
            // Get z vertex //
            //////////////////

            /*!
             *-----------------------------------------------------------------------
             * Calculation of DCA based on formula for distance between two lines
             * 1. Parameterize each line in terms of a point and its direction vector
             * 2. There must be a point on each line such that the segment joining
             *    those points be perpendicular to the direction vector of each line.
             *    The length of this segment is the distance of closest approach.
             * 3. The parameters defining these points can be found by imposing the
             *    orthogonality condition with each line and solving the resulting
             *    2x2 linear system of equations, with the result below.
             * 4. For full documentation, see //INSERT LINK TO SLIDES
             *
             * Javier Orjuela-Koop (see the slides at VTX software meeting, Feb. 10, 2016)
             *
             * [modification]
             * Initial point is changed (beam_x,beam_y) to (0,0)
             *   because hit position is subtracted beam center offset.
             * by Kazuya Nagashima, Sep. 17, 2017
             *-----------------------------------------------------------------------
             */

            //Initial point for track
            float a[3];
            a[0] = x_in;
            a[1] = y_in;
            a[2] = z_in;

            //Direction vector for track
            float u_vec[3];
            u_vec[0] = x_out - x_in;
            u_vec[1] = y_out - y_in;
            u_vec[2] = z_out - z_in;

            //Initial point for "beam axis"
            float b[3];
            //b[0] = beam_x;
            //b[1] = beam_y;
            b[0] = 0.0;
            b[1] = 0.0;
            b[2] = 0.0;

            //Direction vector for horizontal line defined by beam center
            float v_vec[3];
            v_vec[0] = 0;
            v_vec[1] = 0;
            v_vec[2] = 1;

            //Auxiliary variables
            float u_dot_v = u_vec[0]*v_vec[0] + u_vec[1]*v_vec[1] + u_vec[2]*v_vec[2];
            float norm_u_sq = u_vec[0]*u_vec[0] + u_vec[1]*u_vec[1] + u_vec[2]*u_vec[2];
            float norm_v_sq = v_vec[0]*v_vec[0] + v_vec[1]*v_vec[1] + v_vec[2]*v_vec[2];

            float b_m_a[3];
            b_m_a[0] = b[0] - a[0];
            b_m_a[1] = b[1] - a[1];
            b_m_a[2] = b[2] - a[2];

            //Find the parameter the defines the point of closest approach on the track to the "beam axis"
            float t_Mod = ((u_dot_v*(b_m_a[0]*v_vec[0] + b_m_a[1]*v_vec[1] + b_m_a[2]*v_vec[2]))-(norm_v_sq*(b_m_a[0]*u_vec[0] + b_m_a[1]*u_vec[1] + b_m_a[2]*u_vec[2])))/(u_dot_v*u_dot_v - norm_v_sq*norm_u_sq);

            //The x,y,z vertex coordinates are simply the coordinates of the point on the track evaluated at t_Mod
            float vtxx = a[0] + t_Mod*u_vec[0];
            float vtxy = a[1] + t_Mod*u_vec[1];
            float vtxz = a[2] + t_Mod*u_vec[2];

            // Select events that vertex point is near the z axis //
            float r = sqrt(pow(vtxx - beam_x, 2) + pow(vtxy - beam_y, 2));

            ////////////////////
            // Fill Histogram //
            ////////////////////
            if ( r < 1. ) { /// (vtxx, vtxy) is within 1cm from beam position
              m_h_vtxseed->Fill(vtxz);
              int bin = m_h_vtxseed->FindBin(vtxz);
              zvtx_array[bin].push_back(vtxz);
              m_nassestrk[ps] += 1;
              clstcmb++;
              if (verbosity > 0) {
                cout << "clust0=" << cls0 << " clus1=" << cls1 << " r=" << r << " m_bbcz=" << m_bbcz << " vtxz=" << vtxz << endl;
              }
            }//if(r<1)
          }//for(cls1)
        }//for(sublayer1)
      }//for(cls0)
      if (verbosity > 0) cout << "sublayer0=" << sublayer0 << " GetMaximumBin()=" << m_h_vtxseed->GetMaximumBin() << endl;
    }//for(sublayer0)
    if (verbosity > 0) {
      cout << "clustcmb=" << clstcmb << " m_nassestrk=" << m_nassestrk[ps] << endl;
    }
    if ( clstcmb > 0 ) {
      m_nclscmb[ps] = clstcmb;
      if ( m_nassestrk[ps] > 0 ) {
        int maxbin = m_h_vtxseed->GetMaximumBin();
        int xmin = ( maxbin > 6 ) ? maxbin - 6 : 0;
        int xmax = ( maxbin < H_VTX_SEED_NBIN - 5 ) ? maxbin + 6 : H_VTX_SEED_NBIN + 1;
        double zvtx_sum = 0.;
        int zvtx_entry = 0;
        int max_entry = zvtx_array[maxbin].size();
        for ( int ibin = xmin; ibin <= xmax; ibin++ ) {
          if ( zvtx_array[ibin].size() < 0.5 * max_entry ) continue;
          for ( unsigned int ient = 0; ient < zvtx_array[ibin].size(); ient++ ) {
            zvtx_sum += zvtx_array[ibin].at(ient);
            zvtx_entry ++;
          }
        }
        m_mean[ps] = zvtx_sum / zvtx_entry;

        if (verbosity > 0) {
          cout << "maxbin=" << maxbin << " xmin=" << xmin << " xmax=" << xmax;
          cout << " lowedge(maxbin)=" << m_h_vtxseed->GetBinLowEdge(maxbin) << " m_mean=" << m_mean[ps] << endl;
          for (int ibin = xmin; ibin < xmax; ibin++) {
            cout << ibin << ":" << m_h_vtxseed->GetBinLowEdge(ibin) << ":" << m_h_vtxseed->GetBinContent(ibin) << endl;
          }
        }
      }
    }

    if ( ps == 0 ) { // primary vertex calculated from pixel layer
      if ( m_mean[ps] < 20. && m_mean[ps] > -20. ) {
        tver[2] = m_mean[ps];
        if ( verbosity > 0 ) {
          cout << "Primary Vertex Seed Finder (Pixel) : (" << tver[0] << ", " << tver[1] << ", " << tver[2] << ")" << endl;
        }
        break;
      }
    }
    else if ( ps == 1 ) { // primary vertex calculated from stripixel layer
      if ( m_mean[ps] < 20. && m_mean[ps] > -20. ) {
        tver[2] = m_mean[ps];
        if ( verbosity > 0 ) {
          cout << "Primary Vertex Seed Finder (Stripixel) : (" << tver[0] << ", " << tver[1] << ", " << tver[2] << ")" << endl;
        }
      }
    }
  }

  /////////////////////////////////
  // Record z-vertex into vtxout //
  /////////////////////////////////
  float vertexerror[3] = {0.01, 0.01, 0.007};
  if (m_side == 0)     { vtxout->AddVtx("SVX", tver, vertexerror, VTX::SVXORDER); }
  else if (m_side == 1) { vtxout->AddVtx("SVXW", tver, vertexerror, 11); }
  else              { vtxout->AddVtx("SVXE", tver, vertexerror, 12); }

  _timer.get()->stop();

  return 0;
}

void SvxPriVertexSeedFinder::setNclstrThrshld(int index, int nclus)
{
  m_nclstr_thrshld[index] = nclus;
}

/*!
 *-- Added by nagasy: Mar. 3 2016
 *   ncluster threshold is changed as a function of BBC charge (charge threshold == 200)
 *
 * Kazuya Nagashima (see the slides at VTX software meeting, Mar. 2, 2016)
 */
int SvxPriVertexSeedFinder::getIndexThreshold(float bbcq)
{
  if(bbcq>200) return 0;
  else return 1;
}

int SvxPriVertexSeedFinder::CreateNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);

  // Find SVX node.
  PHCompositeNode* svxNode
    = dynamic_cast<PHCompositeNode*> (iter.findFirst("PHCompositeNode", "SVX"));
  if (!svxNode) {
    cerr << PHWHERE << "SVX node missing, doing nothing." << endl;
    return EVENT_OK;
  }

  PHIODataNode<PHObject>* SvxVtxOutNode = NULL;
  SvxVtxOutNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxVtxOut");
  if (!SvxVtxOutNode) {
    VtxOut *svxvtx = new VtxOutv7();
    SvxVtxOutNode = new PHIODataNode<PHObject>(svxvtx, "SvxVtxOut", "PHObject");
    svxNode->addNode(SvxVtxOutNode);
  } else {
    cout << "SvxVtxOut should be made here." << endl;
  }

  return EVENT_OK;
}
