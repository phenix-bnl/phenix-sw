/*
 * histFcl.C
 *
 * Routines to book and fill ZDC QA histograms
 */

#include "histFcl.h"

// ROOT header files
#include <TH1.h>
#include <TH2.h>
#include <TProfile.h>
#include "FclConsts.h"

#include "TriggerHelper.h"
#include "FclRaw.h"
#include "getClass.h"

// Declare FCAL histograms

TH2F *fclADCNorth;
TH2F *fclADCSouth;
TH2F *fclTDCNorth;
TH2F *fclTDCSouth;
TProfile *fclADCNorthP;
TProfile *fclADCSouthP;
TProfile *fclTDCNorthP;
TProfile *fclTDCSouthP;


//-------------------------------------------------------------------------
/*
 *  Create the FCAL QA histograms
 */

int QAFcl::Init(PHCompositeNode *topNode)
{

  const float MAXCHAN = CHANTOT;
  const float MAXADC = ADCOVERFLOW - ADCZERO;
  const int MAXADCINT = ADCOVERFLOW - ADCZERO;

  fclADCNorth = new TH2F("fclADCNorth", "FCAL North ADC Distributions",
                         CHANTOT, -0.5, MAXCHAN - 0.5,
                         MAXADCINT, -0.5 + ADCZERO, MAXADC - 0.5);
  fclADCSouth = new TH2F("fclADCSouth", "FCAL South ADC Distributions",
                         CHANTOT, -0.5, MAXCHAN - 0.5,
                         MAXADCINT, -0.5 + ADCZERO, MAXADC - 0.5);
  fclTDCNorth = new TH2F("fclTDCNorth", "FCAL North TDC Distributions",
                         CHANTOT, -0.5, MAXCHAN - 0.5,
                         MAXADCINT + ADCZERO, -0.5, MAXADC - 0.5);
  fclTDCSouth = new TH2F("fclTDCSouth", "FCAL South TDC Distributions",
                         CHANTOT, -0.5, MAXCHAN - 0.5,
                         MAXADCINT + ADCZERO, -0.5, MAXADC - 0.5);


  fclADCNorthP = new TProfile("fclADCNorthP", "FCAL North ADC Distributions",
                              CHANTOT, -0.5, MAXCHAN - 0.5);
  fclADCSouthP = new TProfile("fclADCSouthP", "FCAL South ADC Distributions",
                              CHANTOT, -0.5, MAXCHAN - 0.5);
  fclTDCNorthP = new TProfile("fclTDCNorthP", "FCAL North TDC Distributions",
                              CHANTOT, -0.5, MAXCHAN - 0.5);
  fclTDCSouthP = new TProfile("fclTDCSouthP", "FCAL South TDC Distributions",
                              CHANTOT, -0.5, MAXCHAN - 0.5);
  return 0;

}

//-------------------------------------------------------------------------

int QAFcl::process_event(PHCompositeNode *topNode)
{
  TriggerHelper trig(topNode);
  if (!trig.IsEventMinBias())
    {
      return 0;
    }

  FclRaw* fclnorth = findNode::getClass<FclRaw>(topNode, "fclRawNorth");
  FclRaw* fclsouth = findNode::getClass<FclRaw>(topNode, "fclRawSouth");

  for (int i = 0; i < CHANTOT; i++)
    {

      fclADCNorth->Fill(i, fclnorth->getLowGain(i));
      fclADCSouth->Fill(i, fclsouth->getLowGain(i));
      fclTDCNorth->Fill(i, fclnorth->getTdc(i));
      fclTDCSouth->Fill(i, fclsouth->getTdc(i));

      fclADCNorthP->Fill(i, fclnorth->getLowGain(i));
      fclADCSouthP->Fill(i, fclsouth->getLowGain(i));
      fclTDCNorthP->Fill(i, fclnorth->getTdc(i));
      fclTDCSouthP->Fill(i, fclsouth->getTdc(i));

    }


  return 0;
}







