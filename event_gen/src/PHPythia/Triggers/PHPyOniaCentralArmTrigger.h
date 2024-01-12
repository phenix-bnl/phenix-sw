#ifndef PHPyOniaCentralArmTrigger_h
#define PHPyOniaCentralArmTrigger_h

#include "Riostream.h"
#include "TNtuple.h"
#include "TFile.h"

#include "PHPyTrigger.h"

//!  trigger module to select events that contain a Quarkonia in the central arm

class PHPyOniaCentralArmTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyOniaCentralArmTrigger( const std::string& name = "PHPyOniaCentralArmTrigger" );
  
  //! destructor 
  virtual ~PHPyOniaCentralArmTrigger( void ); 
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode);

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  //!@name Setters for Normalization output file
  //@{
  void SetNormalizationFileName(char * name)
    { fNormFileName= name; }

  void WriteNormalizationFileName(bool flag)
    { fWriteNormFile = flag; }
  //@}

  protected:
  
  //! true if quarkonia is found near the central arm acceptance
  bool OniaInCentralArm( PHPythiaContainer *phpylist );

 private:
  const char * fNormFileName;     //! normalization file name
  bool fWriteNormFile;      //! boolean to decide if save norm. file
  TFile * fNormFile;        //! normalization file
  TNtuple * fPairNtuple;    //! pairs ntuple
  TNtuple * fSingleNtuple;  //! single particles ntuple

};

#endif	// PHPyOniaCentralArmTrigger_h
