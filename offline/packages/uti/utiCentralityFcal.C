#include"utiCentralityFcal.h"
#include "ncfclProbDist.h"
#include "getClass.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include "PdbDouble.hh"
#include "TF1.h"
#include "TH1.h"
#include "RunToTime.hh"
#include "PHGlobal.h"
#include "TrigLvl1.h"
#include "RunHeader.h"
//INCLUDECHECKER: Removed this line: #include "TFile.h"

#include<iostream>

using namespace std;

utiCentralityFcal* utiCentralityFcal::_instance = 0;
utiCentralityFcal* utiCentralityFcal::instance()
{
  if(!_instance)
    _instance = new utiCentralityFcal();
  return _instance;
}

utiCentralityFcal::utiCentralityFcal()
{
  _initialized= 0;
  _NcolMax = 45;
  _PSimpleGlauberEfcalSumNcol = 0;
  _PSimpleGlauberEfcalNcol = 0;
  _PGlauberEfcalSumNcol = 0;
  _PGlauberEfcalNcol = 0;
  _npercentiles = 0;
  _percentiles = 0;
  _energybinlowedge = 0;

  cout<<"utiCentralityFcal Created...\n";

}

int utiCentralityFcal::Init(int runnumber)
{
  if(_initialized < 0) return 0;

  if(!_initialized)
  {

    std::cout<<"Initializing getdAuCentralityDistFcl for run "<<runnumber<<std::endl;

    // Get PHTimeStamp for this run number...
    RunToTime *runTime = RunToTime::instance();
    PHTimeStamp *bRunTime = runTime->getBeginTime(runnumber);
    if(!bRunTime)
    {
      std::cout<<PHWHERE<<" No Timestamp for run "<<runnumber<<std::endl;
      _initialized = -1;
      return -1;
    }

    // Prepare to read centrality parameters from database
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication * application = bankManager->getApplication();


    if(application->startRead())
    {
      PdbBankID bIdNcoll;
      bIdNcoll.setInternalValue(glauberBankid);
      PdbBankID bIdTrigEff;
      bIdTrigEff.setInternalValue(trigeffBankid);
      PdbCalBank *ncolBank = bankManager->fetchBank("PdbDoubleBank",bIdNcoll,"calib.fcl.centrality",*bRunTime);
      PdbCalBank *trigEffBank = bankManager->fetchBank("PdbDoubleBank",bIdTrigEff,"calib.fcl.centrality",*bRunTime);

      if(!ncolBank||!trigEffBank)
      {
	std::cout<<PHWHERE<<" Fcal Centrality Glauber bank not found\n";
        _initialized = -1;
        return -1;

      }
      if(trigEffBank->getLength() < ncolBank->getLength())
      {
	std::cout<<PHWHERE<<" Fcal Centrality: Trigger Efficiency bank contains too few entries!\n";
        _initialized = -1;
        return -1;

      }

      ncolBank->printHeader();
      trigEffBank->printHeader();

      NcfclA::hGlauberDist = new TH1D("hNpartAu","hNpartAu",ncolBank->getLength(),-0.5,ncolBank->getLength() - 0.5);
      // This is apparently necessary to prevent the histogram from
      // being deleted when switch from one nanoDST to the next segment
      NcfclA::hGlauberDist->SetDirectory(0);
      std::cout<<"Glauber from DB: ";
      for(unsigned int ibin = 0; ibin < ncolBank->getLength(); ibin++)
      {
	if(ibin <= 10)
	  printf("%f(%f) ",
		 ((PdbDouble*)&ncolBank->getEntry(ibin))->getValue(),
		 ((PdbDouble*)&trigEffBank->getEntry(ibin))->getValue());
	NcfclA::hGlauberDist->SetBinContent(ibin + 1,
					    ((PdbDouble*)&ncolBank->getEntry(ibin))->getValue()
					    * ((PdbDouble*)&trigEffBank->getEntry(ibin))->getValue()
					    );
      }
      std::cout<<std::endl;
      delete ncolBank;
      delete trigEffBank;

    }

    //Renormalize
    NcfclA::hGlauberDist->Scale(1.0/(NcfclA::hGlauberDist->Integral()));

    std::cout<<" Glauber Distribution : ";
    for(int iNp = 1; iNp <= 10; iNp++)
      std::cout<<NcfclA::hGlauberDist->GetBinContent(iNp)<<" ";
    std::cout<<std::endl;

    //Npart Extraction functions
    if(_PSimpleGlauberEfcalSumNcol) delete _PSimpleGlauberEfcalSumNcol;

    _PSimpleGlauberEfcalSumNcol = new TF1("_PSimpleGlauberEfcalSumNcol",
					    NcfclA::fPSimpleGlauberEfcalSumNcol,
					    0,100, 5);
    _PSimpleGlauberEfcalSumNcol->SetTitle("PSimpleGlauberEfcalSumNcol");
    _PSimpleGlauberEfcalSumNcol->SetParNames("gamma_b","a0","a1","a2","a3");

    if(_PSimpleGlauberEfcalNcol) delete _PSimpleGlauberEfcalNcol;

    _PSimpleGlauberEfcalNcol = new TF1("_PSimpleGlauberEfcalNcol",
				       NcfclA::fPSimpleGlauberEfcalNcol,
				       0,100, 6);
    _PSimpleGlauberEfcalNcol->SetTitle("PSimpleGlauberEfcalNcol");
    _PSimpleGlauberEfcalNcol->SetParNames("gamma_b","a0","a1","a2","a3","Ncol");

//     //Fit to all run3 data
//     double gamma_b = 3.68376e-01;
//     double a0      = 1.67407e+00;
//     double a1      = 0.00000e+00;
//     double a2      = 7.59268e+00;
//     double a3      = 9.55864e+00;
//     //Fit to muon_rcp hadron data only
//     double gamma_b = 3.68418e-01;
//     double a0      = 1.74388e+00;
//     double a1      = 0.00000e+00;
//     double a2      = 8.23251e+00;
//     double a3      = 1.02227e+01;

//     //Fit to muon_rcp hadron data only
//     // with SS2 with Smearing
//     double gamma_b = 0.3053;
//     double a0      = 2.013;
//     double a1      = 0.9818;
//     double a2      = 5.617;
//     double a3      = 7.97;

//    //Fit to muon_rcp hadron data only
//    // with SS2 with Smearing to the Ntagged Distribution
//    // saturation asymptote varied
//    double gamma_b = 0.2243;
//    double a0      = 2.862;
//    double a1      = 0.8798;
//    double a2      = 2.422;
//    double a3      = 3.793;

//     //Fit to muon_rcp hadron data only
//     // with SS2 with Smearing to the Ntagged Distribution
//     // fixing a2 to minbias values
//     double gamma_b = 0.3406;
//     double a0      = 2.172;
//     double a1      = 0.9025;
//     double a2      = 5.617;
//     double a3      = 6.06;

    //Fit to muon_rcp hadron data only
    // with Poly2 with Smearing to MB distribution
    // fixing a2  
    double gamma_b = 0.4728;
    double a0      = 15.47;
    double a1      = 0.9417;
    double a2      = 37.96;
    double a3      = -0.0117627;

//    //Fit to muon_rcp hadron data only
//    // with Poly2 with Smearing to NT distribution
//    // fixing a2  
//    double gamma_b = 0.4122;
//    double a0      = 16.75;
//    double a1      = 0.8735;
//    double a2      = 37.96;
//    double a3      = -0.01252;
    
    _PSimpleGlauberEfcalNcol->SetParameter("gamma_b",gamma_b);
    _PSimpleGlauberEfcalNcol->SetParameter("a0",a0);
    _PSimpleGlauberEfcalNcol->SetParameter("a1",a1);
    _PSimpleGlauberEfcalNcol->SetParameter("a2",a2);
    _PSimpleGlauberEfcalNcol->SetParameter("a3",a3);
      
    _PSimpleGlauberEfcalSumNcol->SetParameter("gamma_b",gamma_b);
    _PSimpleGlauberEfcalSumNcol->SetParameter("a0",a0);
    _PSimpleGlauberEfcalSumNcol->SetParameter("a1",a1);
    _PSimpleGlauberEfcalSumNcol->SetParameter("a2",a2);
    _PSimpleGlauberEfcalSumNcol->SetParameter("a3",a3);

    //Alternate Npart Extraction Function
    if(_PGlauberEfcalSumNcol) delete _PGlauberEfcalSumNcol;

    _PGlauberEfcalSumNcol = new TF1("_PGlauberEfcalSumNcol",
					    NcfclA::fPEfcalSumNcol,
					    0,100, 4);
    _PGlauberEfcalSumNcol->SetTitle("PGlauberEfcalSumNcol");
    _PGlauberEfcalSumNcol->SetParNames("gamma_b","a0","a1","w0");

    if(_PGlauberEfcalNcol) delete _PGlauberEfcalNcol;

    _PGlauberEfcalNcol = new TF1("_PGlauberEfcalNcol",
				       NcfclA::fPGlauberEfcalNcol,
				       0,100, 5);
    _PGlauberEfcalNcol->SetTitle("PGlauberEfcalNcol");
    _PGlauberEfcalNcol->SetParNames("gamma_b","a0","a1","w0","Ncol");

    _PGlauberEfcalNcol->SetParameter("gamma_b",gamma_b);
    _PGlauberEfcalNcol->SetParameter("a0",a0);
    _PGlauberEfcalNcol->SetParameter("a1",a1);
      
    _PGlauberEfcalSumNcol->SetParameter("gamma_b",gamma_b);
    _PGlauberEfcalSumNcol->SetParameter("a0",a0);
    _PGlauberEfcalSumNcol->SetParameter("a1",a1);

    // Initialize the cumulants to report the Energy Percentile
    std::cout << " Initializing Fcal dAu Centrality ...\n";
    //Initialize from the database
    if(application->startRead())
    {
      PdbBankID bankID;
      bankID.setInternalValue(percentileBankid);
      PdbCalBank *fclCentBank;
      fclCentBank = bankManager->fetchBank("PdbDoubleBank",bankID,"calib.fcl.centrality",*bRunTime);
      if(!fclCentBank){
	std::cout<<PHWHERE<<" Fcl dAu Centrality bank not found\n";
	_initialized = -1;
	return -1;
      }
      if(fclCentBank->getLength()<=2){
	std::cout<<PHWHERE<<" Fcl dAu Centrality bank contains too few entries.\n";
	_initialized = -1;
	return -1;
      }
      fclCentBank->printHeader();

      _npercentiles = fclCentBank->getLength()-2;
      double lowedge = ((PdbDouble*)&fclCentBank->getEntry(0))->getValue();
      double binwidth = ((PdbDouble*)&fclCentBank->getEntry(1))->getValue();
      
      if(_percentiles) delete[] _percentiles;
      _percentiles = new double[_npercentiles];
      if(_energybinlowedge) delete[] _energybinlowedge;
      _energybinlowedge = new double[_npercentiles];
      int calentry;
      for(int ibin = 0; ibin < _npercentiles; ibin++)
	{
	  calentry = ibin + 2;
	  _percentiles[ibin] = ((PdbDouble*)&(fclCentBank->getEntry(calentry)))->getValue();
	  _energybinlowedge[ibin] = lowedge - ibin*binwidth;
	}
      delete fclCentBank;
      cout<<" Initializing Fcal dAu Centrality done. \n";
    }
    
    std::cout<<" ...done for run number "<<runnumber<<std::endl;
    _initialized = 1;
    _lastrunnumber = runnumber;
  }


  return 0;
}

TH1* utiCentralityFcal::getdAuCentrality(float fclEnergyS, int runnumber)
{

  if(!_initialized)
  {
    Init(runnumber);
  }

  if(_initialized < 0)
  {
    return 0;
  }

  TH1* hNcolTmp = new TH1F("hNpartTmp","P(N_{part,Au}|E_{FCAL})",_NcolMax,-0.5,_NcolMax-0.5);

  if(fclEnergyS < 0.01) fclEnergyS = 0.01;

  float pnorm = _PSimpleGlauberEfcalSumNcol->Eval(fclEnergyS);
  //  std::cout<<"utiCentralityFcal: Energy("<<fclEnergyS<<") ";
  for(int iNc = 1; iNc < _NcolMax; iNc++)
  {
    _PSimpleGlauberEfcalNcol->SetParameter("Ncol",iNc);
    float pval = _PSimpleGlauberEfcalNcol->Eval(fclEnergyS)/pnorm;
    //    printf("%f ",pval);
    if(pval!=pval)
    {
      std::cerr<<PHWHERE<<" Error in calculating FclCentrality:\n"
	       <<"\tEnergy "<<fclEnergyS
	       <<" Total Prob:"<<pnorm
	       <<" Ncol: "<<iNc
	       <<" Prob:"<<pval
	       <<std::endl;
    }
    hNcolTmp->SetBinContent(iNc+1,pval);
  }
  //  std::cout<<std::endl;
  return hNcolTmp;
}

TH1* utiCentralityFcal::getdAuCentrality(PHCompositeNode *topNode)
{
  PHGlobal* pGlobal = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  if(!pGlobal)
  {
    std::cerr<<"utiCentrality::getdAuCentrality(PHCompositeNode*): Error Global object not found!\n";
    return 0;
  }

  float fclEnergyS = pGlobal->get_FclGreyS();
  // Check that Fcl Sums were properly filled in PHGlobal
  if(fclEnergyS == -9999)
  {
    return 0;
  }

  TrigLvl1* pLvl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  if(!pLvl1)
  {
    static int firstComplaint = 1;
    if(firstComplaint)
    {
      std::cerr<<__PRETTY_FUNCTION__<<" TrigLvl1 Object not found: No check for fcal reset! Last warning!\n";
      firstComplaint = 0;
    }
  }else{
    // Check for run3 Fcal Reset which invalidates FCAL data
    // Resets were performed between clocks 11 and 35 inclusive
    if(pLvl1->get_lvl1_clock_cross()>10&&pLvl1->get_lvl1_clock_cross()<36)
      return 0;
  }
  RunHeader *run = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if(_lastrunnumber!=run->get_RunNumber())
  {
    _initialized=0;
  }
  
  return getdAuCentrality(fclEnergyS, run->get_RunNumber());
}

TH1* utiCentralityFcal::getdAuCentralityAlt(float fclEnergyS, int runnumber)
{

  if(!_initialized)
  {
    Init(runnumber);
  }

  if(_initialized < 0)
  {
    return 0;
  }

  TH1* hNcolTmp = new TH1F("hNcolTmp","hNcolTmp",_NcolMax,-0.5,_NcolMax-0.5);

  if(fclEnergyS < 0.01) fclEnergyS = 0.01;

  float pnorm = _PGlauberEfcalSumNcol->Eval(fclEnergyS);

  for(int iNc = 1; iNc < _NcolMax; iNc++)
  {
    _PGlauberEfcalNcol->SetParameter("Ncol",iNc);
    float pval = _PSimpleGlauberEfcalNcol->Eval(fclEnergyS)/pnorm;
    if(pval!=pval)
    {
      std::cerr<<PHWHERE<<" Error in calculating FclCentrality:\n"
	       <<"\tEnergy "<<fclEnergyS
	       <<" Total Prob:"<<pnorm
	       <<" Ncol: "<<iNc
	       <<" Prob:"<<pval
	       <<std::endl;
    }
    hNcolTmp->SetBinContent(iNc+1,pval);
  }
  return hNcolTmp;
}

TH1* utiCentralityFcal::getdAuCentralityAlt(PHCompositeNode *topNode)
{
  PHGlobal* pGlobal = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  if(!pGlobal)
  {
    std::cerr<<__PRETTY_FUNCTION__<<": Error Global object not found!\n";
    return 0;
  }

  float fclEnergyS = pGlobal->get_FclGreyS();
  // Check that Fcl Sums were properly filled in PHGlobal
  if(fclEnergyS == -9999)
  {
    return 0;
  }

  TrigLvl1* pLvl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  if(!pLvl1)
  {
    static int firstComplaint = 1;
    if(firstComplaint)
    {
      std::cerr<<__PRETTY_FUNCTION__<<" TrigLvl1 Object not found: No check for fcal reset! Last warning!\n";
      firstComplaint = 0;
    }
  }else{
    // Check for run3 Fcal Reset which invalidates FCAL data
    // Resets were performed between clocks 11 and 35 inclusive
    if(pLvl1->get_lvl1_clock_cross()>10&&pLvl1->get_lvl1_clock_cross()<36)
      return 0;
  }
  RunHeader *run = findNode::getClass<RunHeader>(topNode,"RunHeader");

  if(_lastrunnumber!=run->get_RunNumber())
  {
    _initialized=0;
  }
  
  return getdAuCentralityAlt(fclEnergyS, run->get_RunNumber());
}

int utiCentralityFcal::getdAuCentralityPercentile(float fclEnergy, int runnumber)
{
  if(!_initialized)
  {
    Init(runnumber);
  }

  if(_initialized < 0)
  {
    return -1;
  }

    for(int ibin = 0; ibin < _npercentiles; ibin++)
    {
      if(fclEnergy >= _energybinlowedge[ibin]) return (int) _percentiles[ibin];
    }
    return -1;
  
}

int utiCentralityFcal::getdAuCentralityPercentile(PHCompositeNode *topNode)
{
  PHGlobal* pGlobal = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  if(!pGlobal)
  {
    std::cerr<<__PRETTY_FUNCTION__<<": Error Global object not found!\n";
    return -1;
  }

  TrigLvl1* pLvl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  if(!pLvl1)
  {
    static int firstComplaint = 1;
    if(firstComplaint)
    {
      std::cerr<<__PRETTY_FUNCTION__<<" TrigLvl1 Object not found: No check for fcal reset! Last warning!\n";
      firstComplaint = 0;
    }
  }else{
    // Check for run3 Fcal Reset which invalidates FCAL data
    // Resets were performed between clocks 11 and 35 inclusive
    if(pLvl1->get_lvl1_clock_cross()>10&&pLvl1->get_lvl1_clock_cross()<36)
      return -1;
  }

  float fclEnergyS = pGlobal->get_FclGreyS();
  // Check that Fcl Sums were properly filled in PHGlobal
  if(fclEnergyS == -9999)
  {
    return -1;
  }
  RunHeader *run = findNode::getClass<RunHeader>(topNode,"RunHeader");

  if(_lastrunnumber!=run->get_RunNumber())
  {
    _initialized=0;
  }
  return getdAuCentralityPercentile(fclEnergyS,run->get_RunNumber());
}
