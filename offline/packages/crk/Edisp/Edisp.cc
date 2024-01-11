#include <cmath>
#include <string>

#include "gsl/gsl_math.h"

#include "TCanvas.h"
#include "TArc.h"
#include "TLine.h"
#include "TPolyLine.h"
#include "TMarker.h"
#include "TPolyMarker.h"

#include "dBbcOutWrapper.h"
#include "dDchTracks.h"
#include "dDchTracksWrapper.h"
#include "dDchHit.h"
#include "dDchHitWrapper.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "dPadClusterWrapper.h"
#include "dTofReconstructedWrapper.h"
#include "dCrkHitWrapper.h"
#include "dTecTrackWrapper.h"

#include "find_Wrapper.h"
#include "Edisp.h"

static const float degree = M_PI / 180.0;

inline float sind(float x) {return sin(x*degree);}
inline float cosd(float x) {return cos(x*degree);}

//static const float w_off_x = 40.0;
//static const float e_off_x = -40.0;
static const float w_off_x = 0.0;
static const float e_off_x = 0.0;


// static for side panel drawing
static const float y_side_height = 400.;
static const float x_rich_side_base  = 0.;
static const float x_rich_side_width = 130.;
static const float x_pc3_side_base   = 135.;
static const float x_pc3_side_width  = 130.;
static const float x_emc_side_base   = 270.;
static const float x_emc_side_width  = 130.;

DCframe::DCframe() {
  // all parameters are from pisa.par
  const float inradius  = 202.0;
  const float outradius = 246.0;
  const float phibotw   = -33.75;
  const float phitopw   =  56.25;
  const float phibote   = 213.75;
  const float phitope   = 123.75;

  d_W[0] = new TArc(w_off_x,0., inradius,  phibotw, phitopw);
  d_W[1] = new TArc(w_off_x,0., outradius, phibotw, phitopw);
  d_E[0] = new TArc(e_off_x,0., inradius,  phitope, phibote);
  d_E[1] = new TArc(e_off_x,0., outradius, phitope, phibote);
}

void DCframe::draw(void) {
  d_W[0]->Draw();
  d_W[1]->Draw();
  d_E[0]->Draw();
  d_E[1]->Draw();
}

Richframe::Richframe() {
  const float rin   = 258.77;
  const float rout  = 406.94;
  const float phibotw   = -33.75;
  const float phitopw   =  56.25;
  const float phibote   = 213.75;
  const float phitope   = 123.75;

  d_W[0] = new TArc(w_off_x,0., rin,  phibotw, phitopw);
  d_W[1] = new TArc(w_off_x,0., rout, phibotw, phitopw);
  d_E[0] = new TArc(e_off_x,0., rin,  phitope, phibote);
  d_E[1] = new TArc(e_off_x,0., rout, phitope, phibote);
}

void Richframe::draw(void) {
  d_W[0]->Draw();
  d_W[1]->Draw();
  d_E[0]->Draw();
  d_E[1]->Draw();
}

static void make_box(float xmin, float xmax, float ymin, float ymax,
		     float theta, float xdisp, TPolyLine **box) {
  float xl[4],yl[4]; // local coordinates of 4 corners of the box
  float x[5],y[5];   // 4 (+1) corners of the box after rotation
  float cosT = cosd(theta);
  float sinT = sind(theta);

  xl[0] = xmin;
  xl[1] = xmax;
  xl[2] = xl[1];
  xl[3] = xl[0];
  yl[0] = ymin;
  yl[1] = yl[0];
  yl[2] = ymax;
  yl[3] = yl[2];
  int j;
  for(j=0;j<4;j++) {
    x[j] =  cosT*xl[j] - sinT*yl[j];
    y[j] =  sinT*xl[j] + cosT*yl[j];
  }
  x[4] = x[0];
  y[4] = y[0];

  // displace the x coordinate by xdisp
  for(j=0;j<5;j++) {
    x[j] = x[j] + xdisp;
  }
  *box = new TPolyLine(5,x,y);
}

Emcframe::Emcframe() {
  // geometry data obtained from mEmcDefGeom.c
  // However, pbgl size I can not figure it out from the code.
  // I need check this...
  float phi_sector[8] = { 202.632, //W0
			  180.00,  //W1
			  157.368, //W2
			  134.738, //W3
			  45.264,  //E3
			  22.632,  //E2
			  0.00,    //E1 PbGl
			  -22.00   //E0 PbGl
                         };
  const float pbsc_rmin = 510.0;
  //  const float pbsc_w    = 38.934; 
  const float pbsc_w    = 40.0;  // PbSc length is 38.934, but use 40 
  const float pbsc_cell = 5.542;
  const int   pbsc_ncell  = 36;
  const float pbsc_dy = pbsc_cell*pbsc_ncell;

  const float pbgl_rmin = 542;
  const float pbgl_w    = 40.0; //? T. Awe wrote PbGl size is 40cm...
  const float pbgl_cell = 4.068;
  const int   pbgl_ncell  = 48;
  const float pbgl_dy = pbgl_cell*pbgl_ncell;

  int i;
  // well...180 degree convention difference....
  for(i=0;i<8;i++)
    phi_sector[i] = 180. - phi_sector[i];

  // PbSc
  for(i=0;i<6;i++) {
    float xdisp;
    if(i<4) xdisp = w_off_x;
    else xdisp = e_off_x;

    make_box(pbsc_rmin, pbsc_rmin + pbsc_w, -0.5*pbsc_dy,0.5*pbsc_dy,
	     phi_sector[i], xdisp, d_sector+i);
  }

  //PbGl
  for(i=6;i<8;i++) {
    make_box(pbgl_rmin, pbgl_rmin + pbgl_w, -0.5*pbgl_dy,0.5*pbgl_dy,
	     phi_sector[i],e_off_x, d_sector+i);
  }
}

void Emcframe::draw(void) {
  for(int i=0;i<8;i++) d_sector[i]->Draw();
}

Edisp::Edisp(PHCompositeNode *top):d_top(top) {
  d_nframes=0;
  d_ndchits0=0;
  d_ndchits1=0;
  d_ndctrks0=0;
  d_ndctrks1=0;
  d_nemclus0=0;
  d_nemclus1=0;
  d_npcclus0=0;
  d_npcclus1=0;
  d_ntofrec0=0;
  d_ntofrec1=0;
  d_ncrkhit0=0;
  d_ncrkhit1=0;
  d_nrich=0;
  for(int iside=0;iside<4;iside++) {
    d_nrich_side[iside] = 0;
    d_nemc_side[iside]  = 0;
    d_npc1_side[iside]  = 0;
    d_npc3_side[iside]  = 0;
  }
  d_frames[0] = new DCframe();
  d_frames[1] = new Richframe();
  d_frames[2] = new Emcframe();
  d_nframes = 3;
  make_d_rich_frame();
  make_sideframe();
}

void Edisp::draw_frames(void) {
  for(int i=0;i<d_nframes;i++) d_frames[i]->draw();
}

void Edisp::draw_rich_frames(void) {
  for(int i=0;i<4;i++) d_rich_frame[i]->Draw(); 
}

int Edisp::good_event(void) {
  dBbcOutWrapper *bbc_w
    = find_Wrapper<dBbcOutWrapper>(d_top,"dBbcOut");
  dBbcOut* bbc = bbc_w->TableData();
  if(bbc == NULL) return 0;
  float Zvertex = bbc->VertexPoint;
  if( -20 < Zvertex && Zvertex < 20) return 1;
  else return 0;
}

void Edisp::load_event(void) {
  load_dchits();cout <<"dchit load OK"<<endl;
  load_dctrks();cout <<"dctrk load OK"<<endl;
  load_emclus();cout <<"emc load OK"<<endl;
  load_pcclus();cout <<"pc load OK"<<endl;
  load_tofrec();cout <<"tof load OK"<<endl;
  load_crkhit();cout <<"crk load OK"<<endl;
  load_tec(); cout << "tec load OK"<<endl;
  load_rich();cout <<"rich load OK"<<endl;
}

void Edisp::draw_dchits(int side) {
  if(side == 0) {
    cout << "dchits0: size = "<<d_ndchits0<<endl;
    for(int i=0;i<d_ndchits0;i++) d_dchits0[i]->Draw();
  } else if(side == 1) {
    cout << "dchits1: size = "<<d_ndchits1<<endl;
    for(int i=0;i<d_ndchits1;i++) d_dchits1[i]->Draw();
  }
}

void Edisp::draw_dctrks(int side) {
  if(side == 0) {
    cout << "dctrks0: size="<<d_ndctrks0<<endl;
    for(int i=0;i<d_ndctrks0;i++) d_dctrks0[i]->Draw();
  } else if(side == 1) {
    cout << "dctrks1: size="<<d_ndctrks1<<endl;
    for(int i=0;i<d_ndctrks1;i++) d_dctrks1[i]->Draw();
  }
}

void Edisp::draw_tec(int side) {
  if(side == 0) {
    cout << "tec0: size="<<d_ntec0<<endl;
    for(int i=0;i<d_ntec0;i++) d_tec0[i]->Draw();
  } else if(side == 1) {
    cout << "tec1: size="<<d_ntec1<<endl;
    for(int i=0;i<d_ntec1;i++) d_tec1[i]->Draw();
  }
}

void Edisp::draw_emclus(int side) {
  if(side == 0) {
    cout << "emc0: size="<<d_nemclus0<<endl;
    for(int i=0;i<d_nemclus0;i++) d_emclus0[i]->Draw();
  } else if(side == 1) {
    cout << "emc1: size="<<d_nemclus1<<endl;
    for(int i=0;i<d_nemclus1;i++) d_emclus1[i]->Draw();
  }
}

void Edisp::draw_pcclus(int side) {
  if(side == 0) {
    cout << "pc0: size="<<d_npcclus0<<endl;
    for(int i=0;i<d_npcclus0;i++) d_pcclus0[i]->Draw();
  } else if(side == 1) {
    cout << "pc1: size="<<d_npcclus1<<endl;
    for(int i=0;i<d_npcclus1;i++) d_pcclus1[i]->Draw();
  }
}

void Edisp::draw_tofrec(int side) {
  if(side == 0) {
    cout << "tof0: size="<<d_ntofrec0<<endl;
    for(int i=0;i<d_ntofrec0;i++) d_tofrec0[i]->Draw();
  } else if(side == 1) {
    cout << "tof1: size="<<d_ntofrec1<<endl;
    for(int i=0;i<d_ntofrec1;i++) d_tofrec1[i]->Draw();
  }
}

void Edisp::draw_crkhit(int side) {
  if(side == 0) {
    cout << "crk0: size="<<d_ncrkhit0<<endl;
    for(int i=0;i<d_ncrkhit0;i++) d_crkhit0[i]->Draw();
  } else if(side == 1) {
    cout << "crk1: size="<<d_ncrkhit1<<endl;
    for(int i=0;i<d_ncrkhit1;i++) d_crkhit1[i]->Draw();
  }
}

void Edisp::draw_rich() {
  for(int i=0;i<d_nrich;i++) d_rich[i]->Draw();
}

void Edisp::draw_side(int iside) {
  //draw frames
  d_rich_sideframe->Draw();
  d_pc3_sideframe->Draw();
  d_emc_sideframe->Draw();

  for(int i=0;i<d_nrich_side[iside];i++)
    d_rich_side[iside][i]->Draw();
  for(int i=0;i<d_npc3_side[iside];i++)
    d_pc3_side[iside][i]->Draw();
  for(int i=0;i<d_nemc_side[iside];i++)
    d_emc_side[iside][i]->Draw();
}

void Edisp::load_dchits()
{
  cout <<"load_dchits"<<endl;

  // delete all previous hit markers....
  for(int i=0;i<d_ndchits0;i++)
    if(d_dchits0[i]) delete d_dchits0[i];
  for(int i=0;i<d_ndchits1;i++)
    if(d_dchits1[i]) delete d_dchits1[i];
  d_ndchits0=0;
  d_ndchits1=0;

  dDchHitWrapper *wrap = find_Wrapper<dDchHitWrapper>(d_top,"dDchHit");
  int nhits = wrap->RowCount();
  dDchHit *data = wrap->TableData();
  int i;

  // make new hit markers 
  for(int i=0;i<nhits;i++) {
    dDchHit *hit = data + i;
    int plane = hit->plane;
    if(plane < 12 || (19 < plane && plane < 32)) {
      float x = hit->xyz[0];
      float y = hit->xyz[1];
      if(-250 < x && x < 250 && -200 < y && y < 200) {
	TMarker *m = new TMarker(x,y,2);
	if(hit->side == 0) {
	  m->SetMarkerColor(2);
	  d_dchits0[d_ndchits0] = m;
	  d_ndchits0++;
	} else {
	  m->SetMarkerColor(4);
	  d_dchits1[d_ndchits1]=m;
	  d_ndchits1++;
	}
      }
    }
  }
}

void Edisp::load_dctrks() {
  dDchTracksWrapper *wrap = 
    find_Wrapper<dDchTracksWrapper>(d_top,"dDchTracks");
  int ntrks = wrap->RowCount();
  dDchTracks *data = wrap->TableData();
  int i;

  // delete all previous hit markers....
  for(i=0;i<d_ndctrks0;i++)
    if(d_dctrks0[i]) delete d_dctrks0[i];
  for(i=0;i<d_ndctrks1;i++)
    if(d_dctrks1[i]) delete d_dctrks1[i];
  d_ndctrks0=0;
  d_ndctrks1=0;

  // make new TLine of track segment
  float dL1 = 25.;
  float dL2 = 300.;
  for(int i=0;i<ntrks;i++) {
    dDchTracks *trk = data + i;
    float ux = trk->direction[0];
    float uy = trk->direction[1];
    float u  = sqrt(ux*ux + uy*uy);
    float x  = trk->point[0];
    float y  = trk->point[1];
    float dL1 = 25./u;
    float dL2 = 350./u;
    TLine *l = new TLine(x - dL1*ux, y - dL1*uy,
			 x + dL2*ux, y + dL2*uy);
    l->SetLineWidth(1); 
    if(trk->side == 0) {
      l->SetLineColor(2);
      d_dctrks0[d_ndctrks0]=l;
      d_ndctrks0++;
    } else {
      l->SetLineColor(4);
      d_dctrks1[d_ndctrks1]=l;
      d_ndctrks1++;
    }
  }
}

void Edisp::load_tec(void) {
  dTecTrackWrapper *wrap = 
    find_Wrapper<dTecTrackWrapper>(d_top,"dTecTrack");
  int ntec = wrap->RowCount();
  dTecTrack *data = wrap->TableData();
  int i;

  // delete all previous hit markers....
  for(i=0;i<d_ntec0;i++)
    if(d_tec0[i]) delete d_tec0[i];
  for(i=0;i<d_ntec1;i++)
    if(d_tec1[i]) delete d_tec1[i];
  d_ntec0=0;
  d_ntec1=0;

  // make new TLine of tec track
  for(int i=0;i<ntec;i++) {
    dTecTrack *trk = data + i;
    
    cout << "tec (";
    cout << trk->xyzin[0] <<",";
    cout << trk->xyzin[1] <<",";
    cout << trk->xyzin[2] <<")(";
    cout << trk->xyzout[0] <<", ";
    cout << trk->xyzout[1] <<",";
    cout << trk->xyzout[2] <<")"<<endl;

    TLine *l = new TLine(trk->xyzin[0],  trk->xyzin[1],
			 trk->xyzout[0], trk->xyzout[1]);
    l->SetLineWidth(2); 
    if(trk->xyzout[2] < 0.) { //Z<0 --> South --> side 0
      l->SetLineColor(6);
      d_tec0[d_ntec0]=l;
      d_ntec0++;
    } else {
      l->SetLineColor(3);
      d_tec1[d_ntec1]=l;
      d_ntec1++;
    }
  }
}

void Edisp::load_emclus(void) {
  dEmcClusterLocalExtWrapper *wrap = 
    find_Wrapper<dEmcClusterLocalExtWrapper>(d_top,"dEmcClusterLocalExt");
  int nclus = wrap->RowCount();
  dEmcClusterLocalExt *data = wrap->TableData();
  int i;

  // delete all previous hit markers....
  for(i=0;i<d_nemclus0;i++)
    if(d_emclus0[i]) delete d_emclus0[i];
  for(i=0;i<d_nemclus1;i++)
    if(d_emclus1[i]) delete d_emclus1[i];
  d_nemclus0=0;
  d_nemclus1=0;

  for(int iside=0;iside<4;iside++) {
    for(int j=0;j<d_nemc_side[iside];j++)
      if(d_emc_side[iside][j]) delete d_emc_side[iside][j];
    d_nemc_side[iside]=0;
  }

  // EMCAL cluster is reprented as a line. The line end point is the
  // cluster position, and its length shows the cluster energy. The
  // color of the line is used to show the Z coordinate
  for(int i=0;i<nclus;i++) {
    dEmcClusterLocalExt *cl = data + i;
    float x  = cl->xyz[0];
    float y  = cl->xyz[1];
    float r  = sqrt(x*x+y*y);
    float dl = 40*(cl->ecorr);
    cout << "emcal="<<cl->ecorr<<" ("<<x<<","<<y<<","<<cl->xyz[2]<<")"<<endl;

    if(cl->ecorr > 0.3) { // ignore all cluster less than 0.3 GeV
      TLine *l = new TLine(x, y, x + dl*x/r, y + dl*y/r);
      l->SetLineWidth(5);
      
      // There are only 4 usable colors...2(RED),6(MAGENTA),4(BLUE),3(GREEN)
      if(cl->xyz[2] < 0.) {
	l->SetLineColor(2);
	d_emclus0[d_nemclus0]=l;
	d_nemclus0++;
      } else {
	l->SetLineColor(4);
	d_emclus1[d_nemclus1]=l;
	d_nemclus1++;
    }

    //Fill side panles for EMC hits
      float zside = (cl->xyz[2])*126.0/200.;
      if(zside<0) zside = -zside;
      zside += x_emc_side_base;
      if(x<0) x = -x;
      float phi = atan(y/x) / degree + 33.75;
      float yphi = phi*y_side_height / 90.0;
      int sector;
      if(cl->xyz[0]>0) {
	if(cl->xyz[2]<0) sector = 0;
	else sector=1;
      }else{
	if(cl->xyz[2]<0) sector = 2;
	else sector=3;
      }
      TMarker *m = new TMarker(zside,yphi,8);
      if(cl->ecorr<0.1) m->SetMarkerColor(1);     //black == Noise.
      else if(cl->ecorr<0.4) m->SetMarkerColor(4); //Dark Blue
      else if(cl->ecorr<0.7) m->SetMarkerColor(7); //Light Blue
      else if(cl->ecorr<1.0) m->SetMarkerColor(3); //Green
      else if(cl->ecorr<1.3) m->SetMarkerColor(5); //Yellow
      else if(cl->ecorr<1.6) m->SetMarkerColor(6); //Magenta
      else m->SetMarkerColor(2.0);//Red
      
      d_emc_side[sector][d_nemc_side[sector]]=m;
      d_nemc_side[sector]++;
    }
  }
}

void Edisp::load_pcclus(void) {
  char *names[2] = {"dPc1Cluster",
		    "dPc3Cluster"};
  // delete all previous hit markers....
  int i;
  for(i=0;i<d_npcclus0;i++)
    if(d_pcclus0[i]) delete d_pcclus0[i];
  for(i=0;i<d_npcclus1;i++)
    if(d_pcclus1[i]) delete d_pcclus1[i];
  d_npcclus0=0;
  d_npcclus1=0;

  for(int iside=0;iside<4;iside++) {
    for(int j=0;j<d_npc3_side[iside];j++)
      if(d_pc3_side[iside][j]) delete d_pc3_side[iside][j];
    d_npc3_side[iside]=0;
  }

  // OK, fill new markers
  for(int id=0;id<2;id++) {
    dPadClusterWrapper *wrap = 
      find_Wrapper<dPadClusterWrapper>(d_top,names[id]);
    if(wrap == NULL) continue;
    int nclus = wrap->RowCount();
    dPadCluster *data = wrap->TableData();

    for(i=0;i<nclus;i++) {
      dPadCluster *clus = data + i;
      TMarker *m = new TMarker(clus->xyz[0],clus->xyz[1],3);

      if(clus->xyz[2] < 0.) {
	m->SetMarkerColor(2);
        d_pcclus0[d_npcclus0]=m;
	d_npcclus0++;
      } else {
	m->SetMarkerColor(4);
        d_pcclus1[d_npcclus1]=m;
	d_npcclus1++;
      }
      // Fill side panels for PC3 hits
      if(id == 1) { // PC3
	float zside = (clus->xyz[2])*126.0/200.;
	if(zside < 0) zside = -zside;
	zside += x_pc3_side_base;
	float x = clus->xyz[0];
	if(x<0) x = -x;
	float y = clus->xyz[1];
	float phi = atan(y/x) / degree + 33.75;
	float yphi = phi*y_side_height / 90.0;
	int sector;
	if(clus->xyz[0]>0) {
	  if(clus->xyz[2]<0) sector = 0;
	  else sector=1;
	}else{
	  if(clus->xyz[2]<0) sector = 2;
	  else sector=3;
	}
	TMarker *m = new TMarker(zside,yphi,2);
	d_pc3_side[sector][d_npc3_side[sector]]=m;
	d_npc3_side[sector]++;
      }
    }
  }
}

void Edisp::load_tofrec(void) {
  // delete all previous hit markers....
  int i;
  for(i=0;i<d_ntofrec0;i++)
    if(d_tofrec0[i]) delete d_tofrec0[i];
  for(i=0;i<d_ntofrec1;i++)
    if(d_tofrec1[i]) delete d_tofrec1[i];
  d_ntofrec0=0;
  d_ntofrec1=0;

  dTofReconstructedWrapper *wrap = 
      find_Wrapper<dTofReconstructedWrapper>(d_top,"dTofReconstructed");
  int ntof = wrap->RowCount();
  dTofReconstructed *data = wrap->TableData();

  for(i=0;i<ntof;i++) {
    dTofReconstructed *tof = data + i;
    TMarker *m = new TMarker(tof->xtof[0],tof->xtof[1],4);
    m->SetMarkerSize(1);

    if(tof->xtof[2] < 0.) {
      m->SetMarkerColor(6);
      d_tofrec0[d_ntofrec0]=m;
      d_ntofrec0++;
    } else {
      m->SetMarkerColor(3);
      d_tofrec1[d_ntofrec1]=m;
      d_ntofrec1++;
    }
  }
}

static void pmt_position(int pmt, float x[]);
static void pmt2cell(int, int*, int*, int*);

void Edisp::load_crkhit(void) {
  // delete all previous hit markers....
  int i;
  cout<<"delete crkhits0 "<<d_ncrkhit0<<endl;
  for(i=0;i<d_ncrkhit0;i++)
    if(d_crkhit0[i]) delete d_crkhit0[i];
  cout<<"delete crkhits1 "<<d_ncrkhit1<<endl;
  for(i=0;i<d_ncrkhit1;i++)
    if(d_crkhit1[i]) delete d_crkhit1[i];
  d_ncrkhit0=0;
  d_ncrkhit1=0;
  cout<<"done"<<endl;

  dCrkHitWrapper *wrap = 
      find_Wrapper<dCrkHitWrapper>(d_top,"dCrkHit");
  int nhit = wrap->RowCount();
  dCrkHit *data = wrap->TableData();

  cout << "number of RICH hits = "<<nhit<<endl;

  for(i=0;i<nhit;i++) {
    float x[3];
    dCrkHit *hit = data + i;

    pmt_position(hit->pmt,x);
    float u = sqrt(x[0]*x[0] + x[1]*x[1]);
    float ux = x[0]/u;
    float uy = x[1]/u;
    float dl = 10*(hit->npe);
    float xoff;
    if(x[0] > 0) xoff = w_off_x;
    else xoff = e_off_x;

    TLine *l = new TLine(x[0]+xoff, x[1], x[0]+xoff + dl*ux, x[1] + dl*uy);
    l->SetLineWidth(5);
    if(x[2] < 0.) {
      l->SetLineColor(5);
      d_crkhit0[d_ncrkhit0]=l;
      d_ncrkhit0++;
    } else {
      l->SetLineColor(5);
      d_crkhit1[d_ncrkhit1]=l;
      d_ncrkhit1++;
    }
  }
}

void Edisp::make_sideframe(void) {
  float x[5],y[5];
  // RICH
  x[0] = x_rich_side_base; y[0] = 0.0;
  x[1] = x_rich_side_base; y[1] = y_side_height;
  x[2] = x_rich_side_base + x_rich_side_width; y[2] = y_side_height;
  x[3] = x_rich_side_base + x_rich_side_width; y[3] = 0;
  x[4] = x_rich_side_base; y[4] = 0;
  d_rich_sideframe = new TPolyLine(5,x,y);
  // PC3
  x[0] = x_pc3_side_base;
  x[1] = x_pc3_side_base;
  x[2] = x_pc3_side_base + x_pc3_side_width;
  x[3] = x_pc3_side_base + x_pc3_side_width;
  x[4] = x_pc3_side_base;
  d_pc3_sideframe = new TPolyLine(5,x,y);
  // EMC
  x[0] = x_emc_side_base;
  x[1] = x_emc_side_base;
  x[2] = x_emc_side_base + x_emc_side_width;
  x[3] = x_emc_side_base + x_emc_side_width;
  x[4] = x_emc_side_base;
  d_emc_sideframe = new TPolyLine(5,x,y);
}
void Edisp::make_d_rich_frame(void) {
  for(int i=0;i<4;i++) {
    float x[5],y[5];
    float xbase = 130*i;
    float xw = 128.0;

    x[0] = xbase;    y[0] = -5.;
    x[1] = xbase;    y[1] = 405.;
    x[2] = xbase+xw; y[2] = 405.;
    x[3] = xbase+xw; y[3] = -5.;
    x[4] = xbase;    y[4] = -5.;

    d_rich_frame[i] = new TPolyLine(5,x,y);
  }
}

void Edisp::load_rich(void) {
  // delete all previous hit markers....
  int i;
  for(i=0;i<d_nrich;i++)
    if(d_rich[i]) delete d_rich[i];
  d_nrich=0;

  for(int iside=0;iside<4;iside++) {
    for(int j=0;j<d_nrich_side[iside];j++)
      if(d_rich_side[iside][j]) delete d_rich_side[iside][j];
    d_nrich_side[iside]=0;
  }

  // OK, now get data and fill markers
  dCrkHitWrapper *wrap = 
    find_Wrapper<dCrkHitWrapper>(d_top,"dCrkHit");
  int nhit = wrap->RowCount();
  dCrkHit *data = wrap->TableData();

  cout << "number of RICH hits = "<<nhit<<endl;

  for(i=0;i<nhit;i++) {
    float xyz[3];
    int sector, iphi, iz;
    dCrkHit *hit = data + i;

    //    cout << hit->pmt << " " << hit->npe << endl; 
    pmt2cell(hit->pmt,&sector,&iz,&iphi);
    pmt_position(hit->pmt,xyz);

    float x = xyz[2];
    if(x < 0) x = -x;
    x = x + sector*130 - 140.;
    float y = 5.0*iphi;
    
    TMarker *m = new TMarker(x, y, 2);
    if(hit->npe < 0.5) m->SetMarkerColor(1);
    else if(hit->npe < 1.5) m->SetMarkerColor(2);
    else if(hit->npe < 2.5) m->SetMarkerColor(3);
    else if(hit->npe < 3.5) m->SetMarkerColor(4);
    else m->SetMarkerColor(6);

    d_rich[d_nrich]=m;
    d_nrich++;

    //Fill side of RICH
    float zside = xyz[2];
    if(zside<0) zside = -zside;
    zside = zside - 140 + x_rich_side_base;
    float yside = (y_side_height/80)*(iphi+0.5);
    TMarker *mside = new TMarker(zside,yside,3);
    if(hit->npe < 0.5) mside->SetMarkerColor(1); //black
    else if(hit->npe < 1.5) mside->SetMarkerColor(4); //blue
    else if(hit->npe < 2.5) mside->SetMarkerColor(7); //light blue
    else if(hit->npe < 3.5) mside->SetMarkerColor(3); //green
    else if(hit->npe < 4.5) mside->SetMarkerColor(6); //magenta
    else mside->SetMarkerColor(2);//red

    d_rich_side[sector][d_nrich_side[sector]]= mside;
    d_nrich_side[sector]++;
  }
}

/************************************
 * RICH geometry section ---> should be moved out to an external class
 * copied from TCrkModule.cc
 ************************************/
#include "dCrkGeo.h"
// Fixed parameters for RICH geometry
#define N_CRK_PMT    5120
#define NCELL_SECT   1280
#define NPMT_FEM      320 /* # of ch in one FEM */
#define NPMT_FEM2     160 /* # of ch in 1/2 FEM...read-out unit */
#define NPMT_AMUCARD2  32 /* # of ch in 1/2 AMUADC card */
#define NCARD_FEM       5 /* # of AMUADC card in one FEM */
#define NPHI_FEM2      10 /* # of phi rows in 1/2 FEM */
#define NCPHI          80 /* # of rows in one sector */
#define NCPMT          32 /* # of PMTs in one SM */     
#define NCPMT_2        16 /* # of PMTs in one row */

/****************************************************************************
 * Defalut values of CrkGeom parameters.
 ****************************************************************************/
static dCrkGeo default_geo = {
  90.,      /* phi_cntr */
  78.75,    /* phi_open */
  100.,     /* dphi_carm */
  92.722,   /* dphi_cshe */
  -33.0365, /* pmt_phi_min */
  55.9365,  /* pmt_phi_max */
  1.11216,  /* pmt_dphi */
  2.5,      /* r_pmt_ent */
  {         /* dx_pmt[32] */
    2.424, 2.424, 2.424, 2.424, 2.424, 2.424, 2.424, 2.424, 2.424, 2.424,
    2.424, 2.424, 2.424, 2.424, 2.424, 2.424,
    -2.424,-2.424,-2.424,-2.424,-2.424,-2.424,-2.424,-2.424,-2.424,-2.424,
    -2.424,-2.424,-2.424,-2.424,-2.424,-2.424
  },
  {        /* r_pmt[32] */
    263.49, 263.49, 263.49, 263.49, 263.49,
    263.49, 263.49, 263.49, 263.49, 263.49,
    263.49, 263.49, 263.49, 263.49, 265.428, 269.301,
    263.49, 263.49, 263.49, 263.49, 263.49,
    263.49, 263.49, 263.49, 263.49, 263.49,
    263.49, 263.49, 263.49, 263.49, 267.363, 271.239
  },
  {        /* z_pmt[32] */
    141.227, 150.056, 158.885, 167.714, 176.543,
    185.372, 194.201, 203.03,  211.859, 220.688,
    229.532, 237.948, 246.055, 253.983, 260.134, 264.587,
    145.53,  154.359, 163.188, 172.017, 180.846,
    189.675, 198.504, 207.333, 216.162, 225.217,
    233.848, 242.047, 250.061, 257.907, 262.362, 266.815
  },
  {        /* theta_pmt[32] */
    35.487,35.487,35.487,35.487,35.487,
    35.487,35.487,35.487,35.487,35.487,
    37.847,40.347,43.012,45.802,48.992,48.992,
    35.487,35.487,35.487,35.487,35.487,
    35.487,35.487,35.487,35.487,
    37.847,40.347,43.012,45.802,48.992,48.992,48.992
  },
  403.0,   /* mir_rin */
  0.01,    /* mir_thck */
  96.41,   /* mir_theta1 */
  122.24,  /* mir_theta2 */
  98.99,   /* mir_thetacut */
  -44.1,   /* mir_phi1 */
  44.1,    /* mir_phi2 */
  215.,    /* mir_dz */
  258.77,  /* wi1_rin */
  0.013,   /* wi1_thck */
  115.7,   /* wi1_zend */
  406.939, /* wi2_rin */
  0.013,   /* wi2_thck */
  185.6    /* wi2_zend */
};

static void pmt2cell(int pmt, int *sector, int *iz, int *iphi) {
/*
 * convert PMT_Id to (sector,iz,iphi). Note that all of those indexes
 * starts from 0 (i.e. C-fashion), not from 1.
 *
 * PMT = NCELL_SECT*sector + NCPMT_2*iphi + iz;
 *
 */
  if( pmt >= 0 && pmt < N_CRK_PMT) {
    *sector =  pmt/NCELL_SECT;
    *iphi   = (pmt - (*sector)*NCELL_SECT)/NCPMT_2;
    *iz     =  pmt - (*sector)*NCELL_SECT - (*iphi)*NCPMT_2;
  } else
    cout << "TCrkModule::pmt2cell: strange cell ID = " << pmt <<endl;
}

//static void pmt_position(dCrkGeo *geo, int pmt, float x[]) {
static void pmt_position(int pmt, float x[]) {
  int sector,iz,iphi;
  float pmt_phi;
  dCrkGeo *geo = &default_geo;

  pmt2cell(pmt,&sector,&iz,&iphi);
  pmt_phi = (geo->pmt_phi_min + (iphi+0.5)*(geo->pmt_dphi))*degree;
  if(((sector == 1 || sector == 2) && (iphi%2 == 1)) ||
     ((sector == 0 || sector == 3) && (iphi%2 == 0))) {
    x[0] = geo->r_pmt[iz]*cos(pmt_phi);
    x[1] = geo->r_pmt[iz]*sin(pmt_phi);
    x[2] = geo->z_pmt[iz];
  } else {
    x[0] = geo->r_pmt[iz + NCPMT_2]*cos(pmt_phi);
    x[1] = geo->r_pmt[iz + NCPMT_2]*sin(pmt_phi);
    x[2] = geo->z_pmt[iz + NCPMT_2];
  }
  if( sector >= 2)   x[0] = -x[0];   /* X < 0 */
  if( sector%2 == 0) x[2] = -x[2];   /* Z < 0 */
}


