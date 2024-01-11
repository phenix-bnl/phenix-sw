#ifndef EDISP_H_INCLUDED
#define EDISP_H_INCLUDED

#include <vector>
//
// A simple event display program to aid data analysis
//
class TArc;
class TLine;
class TPolyLine;
class TMarker;

class PHCompositeNode;

class SSframe {
 public:
  virtual void draw()=0;
};

class DCframe: public SSframe {
 public:
  DCframe();
  void draw(void);
 private:
  TArc  *d_E[2];
  TArc  *d_W[2];
};

class Richframe: public SSframe {
 public:
  Richframe();
  void draw();
 private:
  TArc *d_E[2];
  TArc *d_W[2];
};


class Emcframe: public SSframe {
 public:
  Emcframe();
  void draw();
 private:
  TPolyLine *d_sector[8];
};

class Edisp {
public:
  Edisp(PHCompositeNode *top);

  int good_event(void);
  void load_event(void);

  void draw_frames(void);
  void draw_rich_frames(void);

  void draw_dchits(int);
  void draw_dctrks(int);
  void draw_emclus(int);
  void draw_pcclus(int);
  void draw_tofrec(int);
  void draw_crkhit(int);
  void draw_tec(int);
  void draw_rich(void);
  void draw_side(int);

private:
  void load_dchits(void);
  void load_dctrks(void);
  void load_emclus(void);
  void load_pcclus(void);
  void load_tofrec(void);
  void load_crkhit(void);
  void load_tec(void);
  void load_rich(void);

  void make_d_rich_frame(void);
  void make_sideframe(void);

  PHCompositeNode *d_top;
  SSframe *d_frames[10];
  int      d_nframes;
  TMarker *d_dchits0[10000];
  int      d_ndchits0;
  TMarker *d_dchits1[10000];
  int      d_ndchits1;
  TLine   *d_dctrks0[1000];
  int      d_ndctrks0;
  TLine   *d_dctrks1[1000];
  int      d_ndctrks1;
  TLine   *d_emclus0[1000];
  int      d_nemclus0;
  TLine   *d_emclus1[1000];
  int      d_nemclus1;
  TMarker *d_pcclus0[1000];
  int      d_npcclus0;
  TMarker *d_pcclus1[1000];
  int      d_npcclus1;
  TMarker *d_tofrec0[1000];
  int      d_ntofrec0;
  TMarker *d_tofrec1[1000];
  int      d_ntofrec1;
  TLine   *d_crkhit0[1000];
  int      d_ncrkhit0;
  TLine   *d_crkhit1[1000];
  int      d_ncrkhit1;
  TLine   *d_tec0[1000];
  int      d_ntec0;
  TLine   *d_tec1[1000];
  int      d_ntec1;
  TMarker *d_rich[2000];
  int      d_nrich;
  TMarker *d_rich_side[4][500];
  int      d_nrich_side[4];
  TMarker *d_emc_side[4][500];
  int      d_nemc_side[4];
  TMarker *d_pc1_side[4][500];
  int      d_npc1_side[4];
  TMarker *d_pc3_side[4][500];
  int      d_npc3_side[4];
  TPolyLine *d_rich_frame[4];
  TPolyLine *d_rich_sideframe;
  TPolyLine *d_pc3_sideframe;
  TPolyLine *d_emc_sideframe;
};


#endif
