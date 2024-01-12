#ifndef __PHPYQUEN_H__
#define __PHPYQUEN_H__

#include <string>
#include <SubsysReco.h>
class PHCompositeNode;
class TPythia6;
class PHPythiaHeader;
class PHPythiaContainer;
class TFile;
class TTree;

class PHPyquen : public SubsysReco {

 public:
  PHPyquen(const std::string& name="PHPyquen");
  virtual ~PHPyquen();

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void SetConfigFile(const std::string& fileName){
    _configFile = fileName;
  }

  void SetSeed(unsigned int seed){
    _seed = seed;
  }

 private:
  void ReadConfig();
  std::string _configFile;

  TPythia6* _pythia;
  unsigned int _seed;
  std::string _frame;
  std::string _projectile;
  std::string _target;
  float _roots;
  unsigned int _eventcounter;

  //parameters for pyquen
  double _A;
  double _bimpact;
  int _ifb;
  double _T0;
  double _tau0;
  int _nf;
  int _ienglu;
  int _ianglu;
  int _noquen;

  int CreateNodeTree(PHCompositeNode *topNode);
  PHPythiaHeader* _phpythiaheader;
  PHPythiaContainer* _phpythia;

  TFile* _xsectfile;
  TTree* _tp;
  unsigned int _tp_isub, _tp_nevt;
  char* _tp_proc;
  double _tp_sigma, _tp_nevt_sigma, _tp_integlumi;

};

#endif /* __PHPYQUEN_H__ */
