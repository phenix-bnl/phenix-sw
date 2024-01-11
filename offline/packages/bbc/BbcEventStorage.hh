#ifndef PHENIX_BBCEVENTSTORAGE_HH
#define PHENIX_BBCEVENTSTORAGE_HH

#include <iosfwd>

class BbcEvent;
class TFile;
class TNtuple;

class BbcEventStorage{
public:
  BbcEventStorage(void);
  BbcEventStorage(const char* const file);
  ~BbcEventStorage(void);
  void fill(const BbcEvent& bbc);
  void print(const BbcEvent& bbc, std::ostream& output, const int PrintLevel);

private:
  const int isStorage;
  TFile* hfile;
  TNtuple* bbcntuple0;
  TNtuple* bbcntuple1;
};

#endif /* PHENIX_BBCEVENTSTORAGE_HH */


