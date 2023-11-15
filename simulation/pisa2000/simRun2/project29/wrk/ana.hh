#ifndef _ANA_HH_
#define _ANA_HH_
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stream.h>
#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <vector>
//#include <Rtypes.h>
#include <TObject.h>
#include <TROOT.h>
#include <TCanvas.h>
#include <TNamed.h>
#include <TH1.h>
#include <TTree.h>
#include <TF1.h>
#include <TH2.h>
#include <TH3.h>
#include <TObjArray.h>
#include <TClonesArray.h>
#include <TGraphErrors.h>

//==========================================================================
class CL_header : public TObject {
public:
   int run;
   int event;
   int multiplicity;
   float b;
   int a1;
   int z1;
   int a2;
   int z2;
   float sqrt_s;
   float bmin;
   float bmax;
   float t0femto;
   float vertex[3];
   int itime;
   int idate;
   int nrndm[2];
   int isqStart;
   int iSeconds;
   int maxTrueTrack;
  CL_header(){ };
};
//==========================================================================
class CL_pythia : public TObject {
public:
   int pyth_proc_id;
   float pyth_bjork[2];
   float pyth_parstu[3];
   float pyth_qsqr;
   float pyth_ptrans;
   int intr_part_id[4];
   float intr_part_p[4][4];
  CL_pythia(){ };
};
//==========================================================================
class CL_primary : public TObject {
public:
   int key;
   int event_track;
   int subevent_track;
   int true_track;
   int subevent;
   int idpart;
   int nfile;
   float px_momentum;
   float py_momentum;
   float pz_momentum;
  void Print(const char* opt="") const;
  CL_primary(){ };
};
//==========================================================================
class CL_fkin : public TObject {
public:
   int true_track;
   int subevent;
   int ntrack;
   float ptot;
   float pthet;
   float pphi;
   float r_vertex;
   float z_vertex;
   float th_vertx;
   float ph_vertx;
   int itparent;
   int idparent;
   int idpart;
   int nfile;
  void Print(const char* opt="") const;
  CL_fkin(){ };
};
//==========================================================================
class CL_dEmcGeaClusterTrack : public TObject {
public:
   int id;
   int clusid;
   int evno;
   int keycent;
   int input;
   int type;
   int arm;
   int sector;
   int trkno[3];
   int tracktwrhit[3];
   float edep_nom[3];
   float pid[3];
   float ptot[3];
   float vertex[3][3];
   float ancestry[3];
   float xyz[3][3];
   float edep[3];
   float efrac[3];
   float measxyz[3];
   float mease;
   float ecore;
   float tof;
   float etof;
   float tofmin;
   float etofmin;
   float tofmax;
   float etofmax;
   int twrhit;
   float disp[2];
   float padisp[2];
   float partesum[8];
   int charged;
   float pc3proj[3];
   float chi2_sh;
   float prob_photon_sh;
   float e_sh[2];
   float chglist[8];
  CL_dEmcGeaClusterTrack() { };
};
//==========================================================================
class CL_dEmcGeaTrackCluster : public TObject {
public:
   int id;
   int trkno;
   int track_ptr;
   int input;
   int clusid[3];
   float pid;
   float ptot;
   float nom_edep;
   float edep[3];
   float efrac[3];
  void Print(const char* opt="") const;
  CL_dEmcGeaTrackCluster() { };
};
//==========================================================================
class CL_dEmcGeaTrack : public TObject {
public:
   int id;
   int trkno;
   int input;
   int anclvl;
   int pid;
   float ekin;
   float xyz[3];
   float ptot;
   float pxyz[3];
   float impxyz[3];
   int itparent;
   int idparent;
   int parent_ptr;
   int twrhit;
   float edep;
  CL_dEmcGeaTrack(){ };
};
//==========================================================================
//
// Analysis Class
//
//==========================================================================
class CL_part : public TObject {
public:
  bool b_fkin;
  int i_sect,i_accept;
  float f_phi, f_eta,f_pt;
  CL_fkin fkin;
  bool b_dEmcGeaTrackCluster;
  CL_dEmcGeaTrackCluster dEmcGeaTrackCluster;
  TClonesArray* ar_dEmcGeaClusterTrack;
  TClonesArray* ar_part;
public:
  CL_part();
  ~CL_part();
  int put_fkin(CL_fkin* f);
  int put_dEmcGeaTrackCluster(CL_dEmcGeaTrackCluster* g);
  int put_dEmcGeaClusterTrack(CL_dEmcGeaClusterTrack* g);
  TObjArray getarray_dEmcGeaClusterTrack(int depth = 1000);
  TObjArray getarray_part_pid(int pid,int depth = 1000);
  void Print(const char* opt ="") const;
};
//==========================================================================
class CL_recpri : public TObject {
public:
  CL_primary primary;
  bool b_part;
  CL_part part;
public:
  CL_recpri(){ b_part = false; };
  void set_primary(CL_primary* p){ primary = *p;};
  bool put_fkin(CL_fkin* f);
  bool put_dEmcGeaTrackCluster(CL_dEmcGeaTrackCluster* g);
  bool put_dEmcGeaClusterTrack(CL_dEmcGeaClusterTrack* g);
  void Print(const char* opt="") const;

};
//==========================================================================
class CL_event : public TObject {
public:
  bool _debug;
  CL_header header;
  TClonesArray* ar_recpri;
  TClonesArray* ar_part;
  int nn_dEmcGeaTrackCluster_all;
  int nn_dEmcGeaTrackCluster_rej;
  int nn_dEmcGeaClusterTrack_all;
  int nn_dEmcGeaClusterTrack_rej;
public:
  CL_event();
  ~CL_event();
  void set_header(CL_header* h){ header = *h; };
  bool set_primary(TClonesArray* ar_primary);
  bool set_fkin(TClonesArray* ar_fkin);
  bool set_dEmcGeaTrackCluster(TClonesArray* ar_dEmcGeaTrackCluster);
  bool set_dEmcGeaClusterTrack(TClonesArray* ar_dEmcGeaClusterTrack);
  void Reset();
  bool set_all(TClonesArray* ar_primary,TClonesArray* ar_fkin,
	       TClonesArray* ar_dEmcGeaTrackCluster,TClonesArray* ar_dEmcGeaClusterTrack){
    bool b_stat = true;
    Reset();
    b_stat &= set_primary(ar_primary);
    b_stat &= set_fkin(ar_fkin);
    b_stat &= set_dEmcGeaTrackCluster(ar_dEmcGeaTrackCluster);
    b_stat &= set_dEmcGeaClusterTrack(ar_dEmcGeaClusterTrack);
    return b_stat;
  };
  void Print(const char* opt="") const;

};
//==========================================================================
//
#endif
//
