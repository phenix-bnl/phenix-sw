#ifndef RECALIBRATOR_H__
#define RECALIBRATOR_H__

#include <SubsysReco.h>

#include <set>
#include <string>

class MasterRecalibrator;

/** Base class for all Recalibrator modules to be
 *  used under the Fun4All/Recalibrator framework.
 *
 *  If you write a recalibrator module, you must derive
 *  from this base class and you have to implement this class methods.
 *  None of these are strictly required as far as C++ is concerned, but as
 *  far as your job is concerned, at least process_event(), to do the
 *  job, and InitRun(), to initialize, should be implemented.  
 *  
 */

class Recalibrator : public SubsysReco
{
 public:

  /** ctor.
      @param name is usefull when doing a Print from e.g.
      the Fun4AllServer or getting a pointer to it on the cmd line
  */
  Recalibrator(const std::string  &name = "NONAME");

  /** dtor. 
      Does nothing as this is a base class only.
  */
  virtual ~Recalibrator() {}
  virtual int isValidRun(const int /*irun*/) const { return 0; }
  virtual void Print(const std::string &what = "ALL") const;

  const std::set<std::string> *BaseClasses() const {return &baseclasses;}
  void SetInputNode(const std::string &nodename) {inputnodename = nodename;}
  void SetBaseClass(const std::string &classname) {mybaseclass = classname;} 
  void Order(const int i) {order = i;}
  int Order() const {return order;}
  std::string ClassName() const {return myclassname;}
  void FillHistos(const int i=1) {fillhistos = i;}
  void SetMasterRecalibrator(MasterRecalibrator* theMasterRecal);
  MasterRecalibrator* GetMasterRecalibrator() {return masterRecal;}

 protected:
  std::set<std::string> baseclasses; // use <set> to avoid accidental duplicates in base class names
  std::string mybaseclass;
  std::string inputnodename;
  std::string myclassname; // unmodified classname
  int order;
  int fillhistos;
  MasterRecalibrator *masterRecal; //pointer to the masterRecalibrator that for a given topNode, object not the owner

};

#endif /* RECALIBRATOR_H__ */
