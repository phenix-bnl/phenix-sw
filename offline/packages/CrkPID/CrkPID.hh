#ifndef CRKPID_HH_INCLUDED
#define CRKPID_HH_INCLUDED

#include <dCrkHit.h>
#include <PHPoint.h>

#include <gsl/gsl_rng.h>

class PHLine;
class PHCompositeNode;
class CrkGeometryObject;
class dCrkHitWrapper;
class CrkHitExt;

const int NCRKPMT = 5120;

struct CrkPIDout {

  // ADF 9/28/12, increased from 15 to 20
  static const int MAXPMTHIT = 20;

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
  int   npmt2;//# of RICH hits in association window2.(*see Note below)
  float npe2; //# of photo-electrons in wider assiciation window 2
  //
  // Note on npmt2: 5/22/2002 by Y.A
  // Orignally, npmt2 was the # of PMTs within 15 cm (for CO2) of radius.
  // This is the largest window. Since We never used this, I changed the
  // (default) meaning. It is now # of PMTs within Rmax radius. The difference
  // from npmt0 is that npmt2 includes PMT hits within Rmin. I think this
  // helps pion identification just above the RICH threshold.
  // I made the change in the constructor of CrkPID, where defalut values
  // of parameters are set.
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
  DCRKHIT_ST *pmt[MAXPMTHIT];  //pointer to PMT hits
  float    rpmt[MAXPMTHIT]; //distance from the projected ray
  CrkGeometryObject *d_cgo; //RICH geometry data used for association.
};

class CrkPID {
public:
  CrkPID(const char *alignment_file = NULL, int fUseSurvey=1);
  CrkPID(int RunNumber,int fUseSurvey=0);
  virtual ~CrkPID();
  void SetCrkHitFromTop(PHCompositeNode *top);
  void SetCrkHit(dCrkHitWrapper *crkw);
  void SetBbcT0(float timeZero) {bbct0 = timeZero;}
  void SetPidParameters(float R0, float Rmin, float Rmax, float R1,
			float R2);
  void GetPidParameters(float& R0, float& Rmin, float& Rmax, float& R1,
                        float& R2);
  bool AssociateTrack(const PHLine &track, CrkPIDout *result);
  bool Associate(const PHLine& track, const int npmt, DCRKHIT_ST *crk, CrkPIDout *result);
  
  // Newly extended function 
  bool TrackOnMirrorAndPMT(const PHLine& track, PHLine& reftrack, PHPoint& mcross, PHPoint& cross);
  void GetPMThit(CrkHitExt& crkhitext);

private:
  //
  // pointers to the geometry data and RICH hit data
  //
  CrkGeometryObject *d_cgo;
  PHBoolean          d_align_status;
  dCrkHitWrapper    *d_crkw;
  PHPoint pmt_pos1[NCRKPMT];
  //
  // PID analysis parameters
  float d_R0;    //nominal Cherenkov radius
  float d_Rmin;  //optimum R window is (d_Rmin, d_Rmax)
  float d_Rmax;  //npmt0 is # of RICH PMTs in (d_Rmin,d_Rmax) 
  float d_R1;    //Wider R window 1. npmt1 is # of RICH PMT in (0,d_R1)
  float d_R2;    //Wider R window 2. npmt2 is # of RICH PMT in (0,d_R2)
  //
  float d_MaxNpe; // Maximum npe values allowed in one PMT.

  float t0[NCRKPMT]; // T0 constants for the RICH
  PHBoolean FetchFromDatabase(int RunNumber);

  float bbct0;

  short int SimFlag_CrkPID;

  short int SimFlag_CrkPID_VTX;

  gsl_rng *rng;
};
#endif


