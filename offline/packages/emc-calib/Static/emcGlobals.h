#ifndef  __EMCGLOBALS_H__
#define  __EMCGLOBALS_H__

#include <fstream>
#include <iomanip>
#include <vector>
#include <iosfwd>
#include <Rtypes.h>
#include <TString.h>

class PHTimeStamp;
class Event;
class TH2;
class TLine;
class TString;


//*************************************************************************

/** Some utility functions for online stuff. **/

struct GF{

  /// This function is copied from PhRootHistogramFactory of M.Purschke
  static Int_t Save(const Text_t *file, Option_t *option="",
		    const Text_t *ftitle="", Int_t compress=1);

  static PHTimeStamp * getFileCreationDate(char * fName);
  static void getEventTimeStamp(Event * ev, char * fName, PHTimeStamp * & StartTime, PHTimeStamp *& when);

  /** Get timestamp from GLV1 packet of event.
      The returned pointer belongs to you, i.e. you must delete
      it when done with it. */
  static PHTimeStamp * getGLV1TimeStamp(Event * ev);

  static void zero(int   * i, int n){for (int k=0;k<n;k++) i[k]=0;}
  static void zero(float * f, int n){for (int k=0;k<n;k++) f[k]=0.;}
  static int  reducedMean(int items, float  * e, float   & av, float  & rms, float retain=0.7);
  static int  reducedMean(int items, double * e, double  & av, double & rms, float retain=0.7);
  static char * getNextFileName(std::ifstream & conffile, PHTimeStamp * & fTS);
  static void   drawGrid(TH2*, TLine &);
};

//*************************************************************************
/**  convenience structure to store data in vectors instead of using profile histograms with variable binning\\
this point structure may have multiple entries (measurements). If this capability is used - i will accumulate the sums and make an averaging whenever used wants to get its "param" value **/
struct  point{
  point(float XX, float YY, bool OK = true);
  point(float XX, float YY, float WW, bool OK = true);
  void addEntry(float XX, float YY, float WW = 1.);
  void update();
  void reset();
  void print();

  float X();
  float Y();
  bool  QA();
  void  setQA(bool fOK);
  void  setW(float WW);
  void  setDev(float d);
  float W();
  float D();
  float RMSX();
  float RMSY();

  float x;
  float y; 
  float rmsX, rmsY;
  double x1s, x2s, y1s, y2s, ws;  //  sums made over all entries
  float w; //  normally one, can be an amplitude or anything else
  float dev;    //  this point deviation from the straight line fitted through the set of points
  bool  ok;
  bool  updated;
  int   entries;
};

//*************************************************************************
/**  single set of data points. Can later be used for fitting anything sensible **/
struct single{
  single(int Item, int Towerid, TString Id, TString Name);
  void  push_back(point next);
  int   size();
  point & at(int i);
  std::vector<point> & getPoints();
  void  erase();
  void  erase(int i);
  void  fitLine();
  void  cleanAndFit();
  float slope();
  float crossing();
  void  print();

  std::vector<point> points;
  int item, towerid;
  TString id;
  TString name;
  float sl, cr;
  bool fitted;
};


//***************************************************************************
#endif // _EMC_GLOBALS_h 
