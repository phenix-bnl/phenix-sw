#ifndef __EMCCHANNELEVOLUTION_H__
#define __EMCCHANNELEVOLUTION_H__

#include "emcManageable.h"
#include <vector>
#include <string>

class TGraph;
class emcCalibrationDataHelper;
class emcTracedFEM;

/** Helper class to help visualizing channel gain evolution over time.
 *  Usage is a 2 step process :
 *
 *  1) emcChannelEvolution ce;
 *     ce.produce(run1,run2,opt)
 *     will get the (baseline-corrected) gains from run1 to run2  
 *     opt is used to determine in which way the baseline is computed
 *     (see emcCalibrationDataHelper::getBaseLine())
 *
 *     ce.save("/somedir/")
 *     will save the resulting gains into ASCII files
 *
 *  2) emcChannelEvolution c;
 *     c.read("/somedir")
 *     will read back gains from files.
 *     TGraph* g = c.graph(ifem,ichannel)
 *     Get a graph of the gain evolution for (ifem,ichannel) tower.
 *
 *  Note that produce(), read(), save() require that the relevant
 *  emcDataManager plugin libraries have been loaded.
 *  Currently those are (subject to change, though) :
 *
 *  read from Objy : libemcOM.so
 *  read from Pg   : libemcOMpg.so
 *  read/write from/to ASCII files : libemcOMascii.so
 *
 * @ingroup calibration
 * @ingroup helper
 */

class emcChannelEvolution
{
 public:
  emcChannelEvolution(emcManageable::EStorage datasource = 
		      emcManageable::kDB_Pg);

  ~emcChannelEvolution();

  /// Get a text output of gain evolution for one channel.
  void dump(int ifem, int channel);

  /// Get a graphical representation of gain evolution.
  TGraph* graph(int ifem, int channel);

  /// Same as above, but specifiying origin of times = tics0.
  TGraph* graph(int ifem, int channel, time_t tics0);

  /** Produce the gains for the period run1, run2, by
      reading them from the datasource and normalizing them
      by the baseline, computed using femdetails (see
      emcCalibrationDataHelper::getBaseLine()).
  */
  void produce(int run1, int run2, const char* femdetails);

  /// Read gains from a directory.
  bool read(const char* inputdir);

  /// Save gains to a directory.
  bool save(const char* outputdir);

 private:

  emcTracedFEM* read(int ifem);

  emcCalibrationDataHelper* fCDH;
  std::vector<emcTracedFEM*> fGains;
  emcManageable::EStorage fDataSource;
  std::string fSourceDir;
};

#endif
