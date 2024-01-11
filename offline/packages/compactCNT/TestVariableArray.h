#ifndef __TESTVARIABLEARRAY_H__
#define __TESTVARIABLEARRAY_H__

#include <SubsysReco.h>
#ifndef __CINT__
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#endif
#include <string>
#include <iosfwd>

class PHCompositeNode;

/** Base class for all reconstruction and analysis modules to be
 *  used under the Fun4All framework.
 *
 *  If you write a reconstruction/analysis module, you must derive
 *  from this base class and you have to implement this class methods.
 *  None of these are strictly required as far as C++ is concerned, but as
 *  far as your job is concerned, at least process_event(), to do the
 *  job, and InitRun(), to initialize, should be implemented.  
 *  
 */

class TestVariableArray: public SubsysReco
{
 public:


  TestVariableArray(const std::string &name = "TESTOUT");
  virtual ~TestVariableArray();

  /// Called at the end of all processing.
  int End(PHCompositeNode *topNode);

  /// Called at the end of each run.
  int EndRun(const int runnumber) {return 0;}

  /** Called during initialization.
      Typically this is where you can book histograms, and e.g.
      register them to Fun4AllServer (so they can be variable to file
      using Fun4AllServer::dumpHistos() method).
   */
  int Init(PHCompositeNode *topNode) {return 0;} 

  /** Called for first event when run number is known.
      Typically this is where you may want to fetch data from
      database, because you know the run number.
   */
  int InitRun(PHCompositeNode *topNode);

  /** Called for each event.
      This is where you do the real work.
  */
  int process_event(PHCompositeNode *topNode);

  /// Reset.
  int Reset(PHCompositeNode *topNode) {return 0;} 

  /// Clean up after each event.
  int ResetEvent(PHCompositeNode *topNode) {return 0;}

   void Print(const std::string &what = "ALL") const;

 protected:
#ifndef __CINT__
  const gsl_rng_type * T;
  gsl_rng * rand;
#endif
  std::ostream *fout;
};

#endif /* __TESTVARIABLEARRAY_H__ */

