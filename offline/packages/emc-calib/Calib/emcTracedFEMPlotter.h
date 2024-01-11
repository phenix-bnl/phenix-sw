#ifndef __EMCTRACEDFEMPLOTTER_H__
#define __EMCTRACEDFEMPLOTTER_H__

class emcTracedFEM;
class TGraph;

#include <ctime>
#include <vector>

/** A utility class to draw emcTracedFEM objects. */

class emcTracedFEMPlotter
{
public:

  /// ctor from a tracedFEM.
  emcTracedFEMPlotter(const emcTracedFEM& tf, time_t tics0);

  /** ctor from a tracedFEM with a plain int instead of time_t.
      (mainly for CINT).*/
  emcTracedFEMPlotter(const emcTracedFEM& tf, int tics0);

  ///
  emcTracedFEMPlotter(const emcTracedFEM& tf);

  /** Returns a copy graph for a given channel.
      The returned pointer is yours.
   */
  TGraph* CopyGraph(int ichannel);  

  /// dtor.
  ~emcTracedFEMPlotter();

  /// Draw a given channel of the tracedFEM.
  bool Draw(int ichannel, const char* option="");

  /** Write a given channel graphical representation into a ROOT file.
      A ROOT file must be opened.*/
  bool Write(int ichannel, const char* name);

private:
  /// Make it private so we can not call it.
  emcTracedFEMPlotter(const emcTracedFEMPlotter&);

  /// Create a TGraph for this channel.
  void MakeGraph(int ichannel);

  /// Test if ichannel is within bounds.
  bool ValidChannel(int ichannel) const; 

private:
  emcTracedFEM* fTracedFEM;
  std::vector<TGraph*> fGraphs;
  time_t fTics0;
};

#endif
