#ifndef __FCLDISPLAY_H__
#define __FCLDISPLAY_H__


class FclIndexer;
class PHCompositeNode;
class TCanvas;
class TFile;
class TH2;
class TPad;

class FclDisplay
{
 public:

  FclDisplay();                          
  virtual ~FclDisplay() {}

  int display(PHCompositeNode *topNode); // the work-horse routine
  void saveDisplay();

 protected:

  TFile *FclDisplayFile; 
  TH2 *Fclhs;
  TH2 *Fclhn;
  TCanvas *Fclc1;
  TPad *Fclc1_1;
  TPad *Fclc1_2;

  FclIndexer* indexer;

};

#endif
