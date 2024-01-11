#ifndef CRKPID_HH_INCLUDED
#define CRKPID_HH_INCLUDED

#include "phool.h"
class PHLine;
class PHCompositeNode;
class CrkGeometryObject;
class dCrkHitWrapper;
class dCrkHit;


struct CrkPIDout {
  static const int MAXPMTHIT = 15;
  //
  // RICH hit infomation in optimum assiciation window
  //
  CrkPIDout();  //default constructor

  // utiltiy to retrive PMT hit information
  void get_pmt_info(int i, int *p_pmtid, float *p_npe, float *p_time,
		    float *p_x, float *p_y, float *p_z) const;

  // data members
  bool  accepted; // The proejcted ray is in RICH acceptance;
  int   npmt0;//# of RICH PMT hits in the ring around the track
  float npe0; //# of photo-electrons detected in the ring
  float chi2; //"chi-square" value
  float disp; //displacement of the "center" of the ring relative to the track
  //
  // RICH hits in wider association window. 
  //
  int   npmt1;//# of RICH hits in wider association window 1
  float npe1; //# of photo-electrons in wider assiciation window 1
  int   npmt2;//# of RICH hits in wider association window 2
  float npe2; //# of photo-electrons in wider assiciation window 2
  //
  // more vars
  int npmt3; //# of RICH hits with rather tight cuts
  float npe3;//# of P.E. in the tighter cut
  float time;

  //
  // parameters of reflected track (reflected by RICH mirror)
  //
  int   arm;
  int   side;
  int   panel;
  float cross_mirror[3]; //crossing point of the track and RICH mirror
  float cross_array[3];  //crossing point of the reflected ray on the PMT array
  float cross_phi;       //crossing point of the reflected track and PMT array
  float cross_z;
  float cross_phi_cor;   //crossing point after correction
  float cross_z_cor;     
  float path;            // path length of the track before the mirror
  float center[3]; // npe weighted average of hit PMTs positions.
                   // after alignment correction


  // Hit information of RICH PMTs in the window
  // Up to MAXPMTHIT PMTs are stored here.
  // Note that only first npmt0 elements ([0...npmt0-1] contains
  // valid info.
  //
  dCrkHit *pmt[MAXPMTHIT];  //pointer to PMT hits
  float    rpmt[MAXPMTHIT]; //distance from the projected ray
  CrkGeometryObject *d_cgo; //RICH geometry data used for association.
};

class CrkPID {
public:
  CrkPID(char *alignment_file = NULL);
  ~CrkPID();
  void SetCrkHitFromTop(PHCompositeNode *top);
  void SetCrkHit(dCrkHitWrapper *crkw);
  void SetPidParameters(float R0, float Rmin, float Rmax, float R1,
			float R2);
  bool AssociateTrack(const PHLine &track, CrkPIDout *result);

private:
  //
  // pointers to the geometry data and RICH hit data
  //
  CrkGeometryObject *d_cgo;
  PHBoolean          d_align_status;
  dCrkHitWrapper    *d_crkw;
  //
  // PID analysis parameters
  float d_R0;    //nominal Cherenkov radius
  float d_Rmin;  //optimum R window is (d_Rmin, d_Rmax)
  float d_Rmax;  //npmt0 is # of RICH PMTs in (d_Rmin,d_Rmax) 
  float d_R1;    //Wider R window 1. npmt1 is # of RICH PMT in (0,d_R1)
  float d_R2;    //Wider R window 2. npmt2 is # of RICH PMT in (0,d_R2)
  //
  float d_MaxNpe; // Maximum npe values allowed in one PMT.
};
#endif


