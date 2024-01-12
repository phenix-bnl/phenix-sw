#ifndef __DEATHTOMEMORYHOGS_H__
#define __DEATHTOMEMORYHOGS_H__

#include <SubsysReco.h>

#include <ctime>
#include <string>
#include <unistd.h> // pid_t definition

class DeathToMemoryHogs: public SubsysReco
{
 public:
  DeathToMemoryHogs(const std::string &name = "DEATHTOMEMORYHOGS");
  virtual ~DeathToMemoryHogs();


  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void memlimit(const unsigned int mmem = 1400) {memlim = mmem*1000;} // maximum allowed memory
  void check_frequency(const time_t secs = 300) {deltat = secs;}
  unsigned int getmemsize();
  void exitcode(const int iext = 20) {iexit = iext;}
  void Addcurrmem(unsigned int currmem);
  void SaveHisto(const int i=1) {savehisto = i;}
  void event_frequency(const int nevt = -1) {evtfreq = nevt;}

 protected:
  pid_t pid;
  time_t lastcheck;
  time_t deltat;
  unsigned int memlim;
  unsigned int maxmem;
  int iexit;
  std::string procfilename;
  int savehisto;
  int evtfreq;
  int evtcnt;
  unsigned int nChannels;
  unsigned int currchan;
  unsigned int *array;
};

#endif
