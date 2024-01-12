#ifndef TCENTRALITYRECALIBRATORBBC_H__
#define TCENTRALITYRECALIBRATORBBC_H__

#include <vector>

class TH1;
class TH2;

class TCentralityRecalibratorBBC
{
 public:

  TCentralityRecalibratorBBC();
  virtual ~TCentralityRecalibratorBBC() {}

  int   isValidRun(const int runno) const;
  void  help() const;

  void  SetEventParameters(const int, const float, const float, const float);
  float GetNpartNorth() const;
  float GetNpartSouth() const;
  float GetNcollisions() const;
  float GetBimpact() const;
  float GetOverlapArea() const;
  float GetEccentricity() const;
  float GetMBtriggerEfficiency() const;
  float GetCentrality() const;
  float runn_to_nfile(const int) const;

  // DB update
  void read_file(const char*);
  void update_RecalCentBBC(const int, const int, const int);
  void update_DataBase(const char*, const int, const int);
  // DB fetch
  void fetch_RecalCentBBC(const int);
  void fetch_DataBase(const char*,  const int);


 private:

  float  pn, ps;
  int    key;
  int    key2;

  static const int nbinx = 331;
  static const int nbiny = 331;
  static const int nzver = 30;
  static const int nbine = 80;  // for efficienty (cut the range above pn+ps > 15)
  static const int nbinc = 400; // for centrality
  static const int nkeys = 331 * 331 * 30;

  TH2*   PARTN[nzver];
  TH2*   PARTS[nzver];
  TH2*   NCOLL[nzver];
  TH2*   BIMP [nzver];
  TH2*   AREA [nzver];
  TH2*   ECCE [nzver];
  TH1*   EFFI [nzver];
  TH1*   CENT;

  std::vector<float> vNpartn;
  std::vector<float> vNparts;
  std::vector<float> vNcoll;
  std::vector<float> vBimp;
  std::vector<float> vArea;
  std::vector<float> vEcce;
  std::vector<float> vEffi;
  std::vector<float> vCent;
};

#endif /* TCENTRALITYRECALIBRATORBBC_H__ */
