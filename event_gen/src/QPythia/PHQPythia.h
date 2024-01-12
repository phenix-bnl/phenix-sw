#ifndef __PHQPYTHIA_H__
#define __PHQPYTHIA_H__

#include <SubsysReco.h>
#include <string>
class PHCompositeNode;
class PHPythiaHeader;
class PHPythiaContainer;
class TQPythia6;

class PHQPythia : public SubsysReco {

 public:
  //! constructor
  PHQPythia(unsigned int seed = 0, const std::string& configFile="phqpythia.cfg", const std::string& name="PHQPythia");

  //! destructor
  virtual ~PHQPythia();

  //! Initialize the module
  int Init(PHCompositeNode *topNode);
  
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! event reset
  int ResetEvent(PHCompositeNode *topNode);
  
  //! end of job
  int End(PHCompositeNode *topNode);

 private:
  //! node tree
  int CreateNodeTree(PHCompositeNode *topNode);

  //! Print the configuration
  void PrintConfig() const;

  //! Read the Config File
  void ReadConfig();

  //! the random number seed
  unsigned long int _seed;

  //! the name of the configuration file
  std::string _configFile;

  //! the qpythia generator
  TQPythia6* _pythia;

  //! the header information written out event-by-event
  PHPythiaHeader* _header;

  //! the particle container to be persistified
  PHPythiaContainer* _particleContainer;

};

#endif	/* __PHQPYTHIA_H__ */
