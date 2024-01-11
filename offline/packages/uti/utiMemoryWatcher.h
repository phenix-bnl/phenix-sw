#ifndef __utiMemoryWatcher_h__
#define __utiMemoryWatcher_h__

class TH2;
class TGraph;
class TStopwatch;

/** @name Basic Memory Leak utility.
    
    You can use this tiny class to *see* if your program is leaking.

    Usage:

    utiMemoryWatcher memwatcher;

    some program loop on events here {
      if ( nevents % x == 0 ) 
      {
      // take a sample every x events
        memwatcher.watch(nevents);
      }
    }

    TFile f("out.root","RECREATE");
    memwatcher.write();
    f.Close();

    In the output root file you'll get 3 graphs representing
    the evolution, as a function of the number of events, of :
    - VSIZE is the virtual size (in KBytes) of your program, that is sort of
    the total memory used
    - RSSIZE is the resident size (in KBytes), that is, the part of your 
    program which is really in physical memory.
    - TIME is an estimate of time per event (really it's the time elasped
    between two calls to watch method)

    WARNING: this is far from a bulletproof memory report (it's basically 
    using UNIX command ps -h -p [PID] -o vsize,rssize to do its job).
    It has only been tested on Linux so far.
    
    But by fitting the VSIZE by a pol1 under ROOT, you'll see right away
    by how much your program is leaking.

    @author aphecetc@in2p3.fr
*/

class utiMemoryWatcher
{
public:

  utiMemoryWatcher(unsigned int maxsize=10000);
  ~utiMemoryWatcher();

  void watch(int x);
  
  unsigned int size(void) const { return fSize; }

  int X(int n) const { return fX[n]; }
  int VSIZE(int n) const { return fVSIZE[n]; }
  int RSSIZE(int n) const { return fRSSIZE[n]; }
  double TIME(int n) const { return fTIME[n]; }

  TGraph* graphVSIZE(void);
  TGraph* graphRSSIZE(void);
  TGraph* graphTIME(void);

  TH2* frame(void);

  void write(void);

private:
  int fPID;
  char fCmd[1024];
  unsigned int fMAXSIZE;
  unsigned int fSize;
  int* fX;
  int* fVSIZE;
  int* fRSSIZE;
  double* fTIME;
  TStopwatch* fTimer;
  bool fDisabled;
} ;

#endif