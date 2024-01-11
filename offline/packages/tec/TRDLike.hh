#ifndef __TRDLIKE_H__
#define __TRDLIKE_H__

#include <string>
#include <vector>
#include "PdbTRDlike.hh"

namespace TRDvar
{
  enum TRDVAR {ERR, DE, TR, NHITS, NTR, WTB};
}

class TRDLike
{
 public:
  TRDLike() {}
  TRDLike(size_t var, size_t i);
  TRDLike(const std::string &namein, size_t ind);
  virtual ~TRDLike();
  int FetchFromFile(const std::string &filename);
  int FetchFromFile();
  void setBin(float min, float max, float pe, float pp);
  float prob_e(float val);
  float prob_p(float val);
  float likelihood(float val);
  std::string Name();
  int Update(int beginrun, int endrun);
  int Fetch(int runnumber);
  size_t get_index() {return index;};
  size_t get_nbins() {return nbins;};
  size_t get_trdvar() {return trdvar;};
  void set_index(int i) {index = i;};

 protected:

  size_t nbins;
  size_t trdvar;
  size_t index;
  std::vector<likerange> likebin;
};

#endif
