#ifndef __UTICENTRALITYFCAL_H__
#define __UTICENTRALITYFCAL_H__

class PHCompositeNode;
class TH1;
class TF1;

class utiCentralityFcal
{
  static utiCentralityFcal* _instance;
  utiCentralityFcal();
  public:
  static utiCentralityFcal* instance();
  ~utiCentralityFcal();

  int getdAuCentralityPercentile(PHCompositeNode*);
  int getdAuCentralityPercentile(float fclEnergy, int runnumber);
  TH1* getdAuCentrality(PHCompositeNode *);
  TH1* getdAuCentrality(float fclEnergy, int runnumber);
  TH1* getdAuCentralityAlt(PHCompositeNode *);
  TH1* getdAuCentralityAlt(float fclEnergy, int runnumber);
  
  private:

  enum centralitybankids
    {
      percentileBankid,
      glauberBankid,
      trigeffBankid,


    };

  int Init(int runnumber);
  int _initialized;
  int _lastrunnumber;

  int _NcolMax;
  TF1* _PSimpleGlauberEfcalSumNcol;
  TF1* _PSimpleGlauberEfcalNcol;
  TF1* _PGlauberEfcalSumNcol;
  TF1* _PGlauberEfcalNcol;

  int _npercentiles;
  double* _percentiles;
  double* _energybinlowedge;

};

#endif //__UTICENTRALITYFCAL_H__
