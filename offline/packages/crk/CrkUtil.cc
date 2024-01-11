
#include <TH1.h>
#include <TH2.h>
#include <TFile.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>

#include <fkin.h>
#include <fkinWrapper.h>

#include <crkghitWrapper.h>
#include <dCrkRawWrapper.h>
#include <dCrkHitWrapper.h>
#include <dCrkDCMWrapper.h>
#include <dCrkPidWrapper.h>

#include <dDchTracksWrapper.h>
#include <dDchHitWrapper.h>
#include <dPadClusterWrapper.h>
#include <dCglTrackWrapper.h>


#include <CrkUtil.h>
#include <getClass.h>

#include <string>
#include <iostream>
#include <cmath>
#include <algorithm>
using namespace std;

// external function called. (This is in TCrkModule)
void crk_print_pmt(int pmt);

//==== class Browser and its related helpers =====
// Browser::browse() is originally TCrkModule::browse().
// (Note 2/12/00)
// Having a copy of a function in a different file is not a good coding
// practice. Perhaps, I should move all browse related functions from
// TCrkModule to here.

template <class Tw, class T>
static void show_wrap(PHCompositeNode *, const char *);
static void browse_others(PHCompositeNode *top, const char *name);

void Browser::browse(const char *nodename)
{
  string ghit_name("crkghit");
  string raw_name("dCrkRaw");
  string dcm_name("dCrkDCM");
  string hit_name("dCrkHit");
  string pid_name("dCrkPid");
  string dchhit_name("dDchHit");
  string dchtracks_name("dDchTracks");
  string dpc1_name("dPc1Cluster");
  string dpc2_name("dPc2Cluster");
  string dpc3_name("dPc3Cluster");
  string dcgl_name("dCglTrack");
  string fkin_name("fkin");

  if ( raw_name == nodename)
    show_wrap<dCrkRawWrapper, DCRKRAW_ST>(ftop, nodename);
  else if (dcm_name == nodename)
    show_wrap<dCrkDCMWrapper, DCRKDCM_ST>(ftop, nodename);
  else if ( hit_name == nodename)
    show_wrap<dCrkHitWrapper, DCRKHIT_ST>(ftop, nodename);
  else if ( ghit_name == nodename)
    show_wrap<crkghitWrapper, CRKGHIT_ST>(ftop, nodename);
  else if ( pid_name == nodename)
    show_wrap<dCrkPidWrapper, DCRKPID_ST>(ftop, nodename);
  else if ( fkin_name == nodename)
    show_wrap<fkinWrapper, FKIN_ST>(ftop, nodename);
  else if (dchhit_name == nodename)
    show_wrap<dDchHitWrapper, DDCHHIT_ST>(ftop, nodename);
  else if ( dchtracks_name == nodename)
    show_wrap<dDchTracksWrapper, DDCHTRACKS_ST>(ftop, nodename);
  else if ( dpc1_name == nodename || dpc2_name == nodename ||
            dpc3_name == nodename)
    show_wrap<dPadClusterWrapper, DPADCLUSTER_ST>(ftop, nodename);
  else if ( dcgl_name == nodename)
    show_wrap<dCglTrackWrapper, DCGLTRACK_ST>(ftop, nodename);
  else
    browse_others(ftop, nodename);
}

template <class T>
struct print
{
  void operator()(const T&);
};

template <>
struct print<DCRKRAW_ST>
{
  int d_i;
  print(): d_i(0)
  {}
  void operator()(const DCRKRAW_ST &raw)
  {
    cout << d_i << ":";
    crk_print_pmt(raw.pmt);
    cout << " adc=" << raw.adc << ",tdc=" << raw.tdc << endl;
    ++d_i;
  }
};

template <>
struct print<DCRKDCM_ST>
{
  void operator()(const DCRKDCM_ST &dcm)
  {}
}
  ;

template <>
struct print<DCRKHIT_ST>
{
  int d_i;
  print(): d_i(0)
  {}
  void operator()(const DCRKHIT_ST &hit)
  {
    cout << d_i << ":";
    crk_print_pmt(hit.pmt);
    cout << " npe=" << hit.npe << ",time=" << hit.time << endl;
    ++d_i;
  }
};

template <>
struct print<DCRKPID_ST>
{
  int d_i;
  print(): d_i(0)
  {}
  void operator()(const DCRKPID_ST &pid)
  {
    cout << d_i << ":";
    cout << " id=" << pid.id << " proj_id=" << pid.proj_id;
    cout << " npmt=" << pid.npmt << " npe=" << pid.npe;
    cout << " timing=" << pid.timing << " chi2=" << pid.chi2 << endl;
    cout << "rdisp=" << pid.rdisp << " Lpath=" << pid.Lpath;
    cout << " xproj=(" << pid.xproj[0] << "," << pid.xproj[1] << ",";
    cout << pid.xproj[2] << ")" << endl;
    cout << "chi2b=" << pid.chi2b << " dt=" << pid.dt << endl;
    ++d_i;
  }
};

template <>
struct print<CRKGHIT_ST>
{
  void operator()(CRKGHIT_ST& ghit)
  {
    cout << "crkghit:";
    cout << "pmt=" << ghit.pmt << ", x=(" << ghit.x << "," << ghit.y << "," << ghit.z;
    cout << "), tof=" << ghit.tof << ",pid=" << ghit.pid << ",parent=";
    cout << ghit.parent << endl;
  }
};

template <>
struct print<FKIN_ST>
{
  void operator()(FKIN_ST& kin)
  {
    cout << "fkin:";
    cout << " true_track=" << kin.true_track;
    cout << " subevent  =" << kin.subevent;
    cout << " ntrack    =" << kin.ntrack << endl;
    cout << " ptot      =" << kin.ptot;
    cout << " pthet     =" << kin.pthet;
    cout << " pphi      =" << kin.pphi << endl;
    cout << " r_vtx     =" << kin.r_vertex;
    cout << " z_vtx     =" << kin.z_vertex;
    cout << " phi_vtx   =" << kin.ph_vertx << endl;
    cout << " pid       =" << kin.idpart;
    cout << " parent    =" << kin.itparent;
    cout << " parent_pid=" << kin.idparent << endl;
  }
};

template <>
struct print<DDCHHIT_ST>
{
  void operator()(DDCHHIT_ST &hit)
  {}
}
  ;

template <>
struct print<DDCHTRACKS_ST>
{
  void operator()(DDCHTRACKS_ST &t)
  {
    cout << "id  =" << t.trackid;
    cout << "arm =" << t.arm;
    cout << "side=" << t.side << endl;
    cout << "point=(" << t.point[0] << "," << t.point[1] << "," << t.point[2] << ")";
    cout << "direc=(" << t.direction[0] << "," << t.direction[1];
    cout << "," << t.direction[2] << ")" << endl;
  }
};

template <>
struct print<DPADCLUSTER_ST>
{
  void operator()(DPADCLUSTER_ST &p)
  {
    cout << "id =" << p.id;
    cout << " arm = " << p.arm;
    cout << " sect = " << p.sector;
    cout << " (" << p.xyz[0] << "," << p.xyz[1] << "," << p.xyz[2] << ")" << endl;
  }
};

template <>
struct print<DCGLTRACK_ST>
{
  void operator()(DCGLTRACK_ST &t)
  {
    cout << "id=" << t.id;
    cout << " arm=" << t.arm;
    cout << " dctrack=" << t.dctracksid;
    cout << " tec_id =" << t.tectrackid;
    cout << " dpc1id =" << t.pc1clusid;
    cout << " dpc2id =" << t.pc2clusid;
    cout << " dpc3id =" << t.pc3clusid;
    cout << " emc_id =" << t.emcclusid;
    cout << " quality=" << t.quality << endl;
  }
};

template <class Tw, class T>
static void show_wrap(PHCompositeNode *top, const char *name)
{
  Tw* wrap = findNode::getClass<Tw>(top, name);
  if (wrap)
    {
      wrap->Show();
      for_each(wrap->TableData(), wrap->TableData() + wrap->RowCount(), print<T>());
    }
}

static void browse_others(PHCompositeNode *top, const char *nodename)
{
  cout << nodename << " is not in browser dictionary." << endl;
}

//================= Class for Histogramming ===========

CrkHist::CrkHist()
{
  h1nhit = new TH1F("nhit", "hits in RICH", 200, 0., 200.);
  vh.push_back(h1nhit);
  h1npmt = new TH1F("npmt", "npmt", 20, -0.5, 19.5);
  vh.push_back(h1npmt);
  h1chi2 = new TH1F("chi2", "chi2", 20, 0.0, 5.0);
  vh.push_back(h1chi2);

  h1dc_zvtx = new TH1F("dc_zvtx", "zvtx by dc", 40, -20., 20.);
  vh.push_back(h1dc_zvtx);
  h1dc_r = new TH1F("dc_r", "r at dc", 100, 215., 225.);
  vh.push_back(h1dc_r);
  h1dc_z = new TH1F("dc_z", "z at dc", 100, -100., 100.);
  vh.push_back(h1dc_z);
  h1dc_ul = new TH1F("dc_ul", "ul of dc", 100, 0.99, 1.01);
  vh.push_back(h1dc_ul);

  /*
    h1cgv_zvtx = new TH1F("cgv_zvtx","zvtx by CglInVec",40.,-20.,20.);
    vh.push_back(h1cgv_zvtx);
    h1cgv_r    = new TH1F("cgv_r","r by CglInVec",100,215.,225.);
    vh.push_back(h1cgv_r);
    h1cgv_z    = new TH1F("cgv_z","z by CglInVec",100,-100.,100.);
    vh.push_back(h1cgv_z);
    h1cgv_ul   = new TH1F("cgv_ul","ul of CglInVec",100,0.99,1.01);
    vh.push_back(h1cgv_ul);

    h1cgp_zvtx = new TH1F("cgp_zvtx","zvtx by CglProj",40,-20.,20.);
    vh.push_back(h1cgp_zvtx);
    h1cgp_r    = new TH1F("cgp_r","r by CglProj",100,250.,270.);
    vh.push_back(h1cgp_r);
    h1cgp_z    = new TH1F("cgp_z","z by CglProj",100,-100.,100.);
    vh.push_back(h1cgp_z);
    h1cgp_ur   = new TH1F("cgp_ur","ur of CglProj",100,0.99,1.01);
    vh.push_back(h1cgp_ur);
  */

  h1pc1d = new TH1F("pc1d", "distance to pc1", 50, 0., 5.);
  vh.push_back(h1pc1d);
  h1pc2d = new TH1F("pc2d", "distance to pc2", 100, 0., 20.);
  vh.push_back(h1pc2d);
  h1pc3d = new TH1F("pc3d", "distance to pc3", 100, 0., 20.);
  vh.push_back(h1pc3d);

  h2pcxy = new TH2F("pcxy", "xy of PCs", 100, -500., 500., 100, -500., 500.);

  cout << "vh.size " << vh.size() << " vh.capacity" << vh.capacity() << endl;
}

CrkHist::~CrkHist()
{
  for (unsigned int i = 0; i < vh.size(); i++)
    delete vh[i];
}

static void get_dctrk(int itrk, dDchTracksWrapper *trk, float x[], float u[])
{
  if ( trk->get_trackid(itrk) != itrk)
    cout << "error! itrk is not itrk" << endl;
  for (int j = 0;j < 3;j++)
    {
      x[j] = trk->get_point(j, itrk);
      u[j] = trk->get_direction(j, itrk);
    }
}

static float line_distance(float x0[], float u[], float x[])
{
  // calculate distance between line (x0,u) and x
  // assume that u[] is a unit vector
  float dx = x[0] - x0[0];
  float dy = x[1] - x0[1];
  float dz = x[2] - x0[2];

  // calculate vector product
  float vx = u[1] * dz - u[2] * dy;
  float vy = u[2] * dx - u[0] * dz;
  float vz = u[0] * dy - u[1] * dx;

  // length of the vector product is the distance between line(x0,u) and x
  return sqrt(vx*vx + vy*vy + vz*vz);
}

static void fill_h1pc(TH1F *h, TH2F *h2, int pc, dPadClusterWrapper *dPc,
                      float x[], float u[])
{
  if (dPc)
    {
      float pcx[3];
      pcx[0] = dPc->get_xyz(0, pc);
      pcx[1] = dPc->get_xyz(1, pc);
      pcx[2] = dPc->get_xyz(2, pc);
      float d = line_distance(x, u, pcx);
      h->Fill(d);
      h2->Fill(pcx[0], pcx[1]);
    }
}


void CrkHist::fill(PHCompositeNode *top)
{
  dCrkHitWrapper *hit = findNode::getClass<dCrkHitWrapper>(top, "dCrkHit");
  if (hit)
    h1nhit->Fill((float)(hit->RowCount()));

  dCrkPidWrapper *pid = findNode::getClass<dCrkPidWrapper>(top, "dCrkPid");
  if (pid)
    {
      int nc = pid->RowCount();
      cout << "CrkHist::fill(): nc = " << nc << endl;
      if (nc > 0)
        {
          for (int i = 0; i < nc; i++)
            {
              cout << i << ":" << pid->get_npmt(i) << endl;
              h1npmt->Fill(pid->get_npmt(i));
              h1chi2->Fill(pid->get_chi2(i));
            }
        }
    }

  dDchTracksWrapper *trk = findNode::getClass<dDchTracksWrapper>(top, "dDchTracks");
  if (trk)
    {
      int ntrk = trk->RowCount();
      if (ntrk > 0)
        {
          for (int i = 0; i < ntrk; i++)
            {
              float x = trk->get_point(0, i);
              float y = trk->get_point(1, i);
              float z = trk->get_point(2, i);
              float ux = trk->get_direction(0, i);
              float uy = trk->get_direction(1, i);
              float uz = trk->get_direction(2, i);
              float ul = sqrt(ux * ux + uy * uy + uz * uz);

              float r = sqrt(x * x + y * y);
              float ur = sqrt(ux * ux + uy * uy);
              float zvtx = z - r * uz / ur;

              //	cout << "DchTrack: r="<<r<<" ur="<<ur;
              //	cout << " x="<<x<<" y="<<y<<" z="<<z;
              //	cout <<" ux="<<ux<<" uy="<<uy<<" uz="<<uz<<" zvtx="<<zvtx<<endl;

              h1dc_zvtx->Fill(zvtx);
              h1dc_r->Fill(r);
              h1dc_z->Fill(z);
              h1dc_ul->Fill(ul);
            }
        }
    }

  dPadClusterWrapper *dPc1 =
    findNode::getClass<dPadClusterWrapper>(top, "dPc1Cluster");
  dPadClusterWrapper *dPc2 =
    findNode::getClass<dPadClusterWrapper>(top, "dPc2Cluster");
  dPadClusterWrapper *dPc3 =
    findNode::getClass<dPadClusterWrapper>(top, "dPc3Cluster");

  dCglTrackWrapper *ctr =
    findNode::getClass<dCglTrackWrapper>(top, "dCglTrack");
  if (ctr)
    {
      int nctr = ctr->RowCount();
      for (int i = 0;i < nctr;i++)
        {
          int dc = ctr->get_dctracksid(i);
          int pc1 = ctr->get_pc1clusid(i);
          int pc2 = ctr->get_pc2clusid(i);
          int pc3 = ctr->get_pc3clusid(i);
          if (dc >= 0)
            {
              float x[3], u[3];
              get_dctrk(dc, trk, x, u);
              if (pc1 >= 0)
                fill_h1pc(h1pc1d, h2pcxy, pc1, dPc1, x, u);
              if (pc2 >= 0)
                fill_h1pc(h1pc2d, h2pcxy, pc2, dPc2, x, u);
              if (pc3 >= 0)
                fill_h1pc(h1pc3d, h2pcxy, pc3, dPc3, x, u);
            }
        }
    }
}

void CrkHist::write(const char *rootfile)
{
  TFile *hfile = new TFile(rootfile, "RECREATE", "RICH hist");
  cout << "write vh into file. size = " << vh.size() << endl;
  for (unsigned int i = 0; i < vh.size(); i++)
    {
      vh[i]->Write();
    }
  h2pcxy->Write();
  hfile->Close();
  delete hfile;
}
