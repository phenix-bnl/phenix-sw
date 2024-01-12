#include <MomChangeRecal_dAu_Reco.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankList.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>
#include <RunToTime.hh>
#include <RunHeader.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <Fun4AllServer.h>

#include <TH1.h>
#include <TProfile.h>

#include <iostream>
#include <memory>

using namespace std;

MomChangeRecal_dAu_Reco::MomChangeRecal_dAu_Reco(const char* name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  verbosity = 0;
  ScaleFactor = 1.;

}

int
MomChangeRecal_dAu_Reco::isValidRun(const int runno) const
{
  if (runno < (int) BEGIN_OF_RUN3 || runno > (int) BEGIN_OF_RUN4 )
    {
      return 0;
    }

  return 1;
}

int 
MomChangeRecal_dAu_Reco::Init(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  string Histoname = Name();
  Histoname += "_dAumomorig";
  dAumomold = new TH1F(Histoname.c_str(),"original momentum",200,-10,10);
  se->registerHisto(dAumomold);

  Histoname = Name();
  Histoname += "_dAumomnew";
  dAumomnew = new TH1F(Histoname.c_str(),"modified momentum",200,-10,10);
  se->registerHisto(dAumomnew);

  Histoname = Name();
  Histoname += "_dAumomdiff";
  dAumomdiff = new TH1F(Histoname.c_str(),"delta mom",200,-10,10);
  se->registerHisto(dAumomdiff);

  Histoname = Name();
  Histoname += "_dAumomprof";
  dAumomprof = new TProfile(Histoname.c_str(),"delta mom/mom vs mom",200,0,10,-10,10);
  se->registerHisto(dAumomprof);

  // get calibrations
  refRadius = 220.0; //  dch reference radius (cm)
  dx[0]=-0.124221;   //  X offsets in cm,  index-->dcarm
  dx[1]= 0.469652;
  dy[0]=-0.006808;   //  Y offsets in cm,  index-->dcarm
  dy[1]=-0.025964;

  return 0;
}

int MomChangeRecal_dAu_Reco::InitRun(PHCompositeNode *topNode)
{
  RunHeader * d_runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");

  if (!d_runhdr)
    {
      cout << PHWHERE << "MomChangeRecal_dAu_Reco:: runhdr not in Node Tree" << endl;
      cout << PHWHERE << "You get zeroes for your Generic Recalibrations" << endl;
      return -1;
    }

  unsigned int runnumber = d_runhdr->get_RunNumber();
  // get calibrations
  ScaleFactor = 1.03196;
  if (runnumber<71500) ScaleFactor=1.03409;

  return 0;
}

int
MomChangeRecal_dAu_Reco::process_event(PHCompositeNode *topNode)
{
  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  
  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
        {
	  PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);

	  int quality = sngltrk->get_quality();
	  if (quality!=31 || quality!=63)
	    {
	      if (verbosity)
		{
		  cout << PHWHERE << " MomChangeRecal_dAu_Reco NOTE:  IMPORTANT!!!";
		  cout << " This routine *only* works well for Q=63||31 tracks.  Lower quality";
		  cout << "  tracks will not return acurate answers!!!" << endl;
		}
	      continue;
	    }

	  float mom   = sngltrk->get_mom();
          float alpha = sngltrk->get_alpha();
          float zed   = sngltrk->get_zed();
	  float the0  = sngltrk->get_the0();
          float phi   = sngltrk->get_phi();
          int   dcarm = sngltrk->get_dcarm();
	  int  charge = sngltrk->get_charge();

	  // for plots and couts
	  float oldmom = mom;
	  float newmom = ScaleFactor*mom; //from MomChangeRecalReco logic -- if bad alpha,the0 use ScaleFactor*mom
	  float newalpha = alpha;

	  if (alpha > -999) // if sane alpha, calibrate -- leave the crazies alone
	    {
	      newalpha = new_alpha(alpha, phi, dcarm);
	      float del_alpha = newalpha - alpha;
	      sngltrk->set_alpha(newalpha);

	      if (the0> -999) // if sane the0, calibrate momentum
		{
		  newmom = momentum_changer(mom, alpha, zed, the0, phi, dcarm);
		}

	      float phi0 = sngltrk->get_phi0 ();
	      if (phi0 > -999) //if sane phi0, make phi0 better
		{
		  float newphi0 = delta_phi0(del_alpha) + phi0;
		  sngltrk->set_phi0(newphi0);
		}
	      
	      //  Check whether the charge sign of the particle has changed...	  
	      if ( (alpha>0 && newalpha<0) || (alpha<0 && newalpha>0) )
		{
		  sngltrk->set_charge(-charge);
		}
	    }

	  //set new momentum
	  sngltrk->set_mom(newmom);

	  // fill histos and couts -- for the sane and the crazies
          dAumomold->Fill(oldmom);
          dAumomnew->Fill(newmom);
	  if (oldmom < 10)
	    {
	      dAumomdiff->Fill(oldmom-newmom);
              dAumomprof->Fill(oldmom,(oldmom-newmom)/oldmom);
	    }

	  if (newmom < 0)
 	    {
 	      cout << " new mom: " << newmom
 		   << " old mom: " << oldmom
 		   << " alpha: " << alpha
 		   << " new alpha: " << newalpha
 		   << " charge: " << charge
 		   << " zed: " << zed
 		   << " the0: " << the0
 		   << " quality: " << quality
 		   << endl;
 	    }
        }
    }


  return 0;
}

float //from mom_changer.C
MomChangeRecal_dAu_Reco::momentum_changer  (float mom, float alpha, float zed, float the0, float phi, int dcarm) 
{
  
  // Hello momentum fan.  Since Sean had discovered that the mean alpha vs phi in
  // both the east and west chambers was *not* zero, we needed a rapid fix.  
  // This routine intends to apply exactly that fix.  Here is the procedure:
  //
  //  1)  Write a routine that gives one the ability to predict the momentum of
  //      a track based upon its parameters.  Momentum_predictor is that very 
  //      routine and gets the answer right to about the 1% level.
  //
  //  2)  For each track calculate the correction to the actual alpha of the
  //      track using the parameterization from Sean:  
  //
  //        For east  (dcarm = 0)
  //         m=  0.000643 +- 0.000007
  //         b= -0.00258  +- 0.00002     ( negative)
  //        
  //        For West we have  (dcarm = 1)
  //         m=0.00158 +- 0.000006
  //         b=0.00043 +- 0.000003
  //
  //  3)  We calculate the change in a manner that places any inaccuracies of
  //      the method into second order.  We add to the *original* momentum the prediction
  //      from the model of the *change* that the momentum itself will receive.  This 
  //      is suscinctly written as:
  //           mom_new = mom + Pred(altered)-Pred(original);
  //
  //  4)  Return the resulting momentum to the user.
  //
  //  5)  Fit again using real calibrations.
  //
  //     RUN 65904:
  //        For east  (dcarm = 0)
  //         m=  0.0003164 +- 0.0000473
  //         b= -0.0011936 +- 0.0001406     ( negative)
  //        
  //        For West we have  (dcarm = 1)
  //         m=  0.002007 +- 0.000042
  //         b=  0.000201 +- 0.000021
  //
  //     RUN 79644:
  //        For east  (dcarm = 0)
  //         m=  0.0006588 +- 0.0000324
  //         b= -0.0020983 +- 0.0000985    ( negative)
  //        
  //        For West we have  (dcarm = 1)
  //         m=  0.001924 +- 0.0000317
  //         b=  0.000025 +- 0.0000164
  //
  //  6)  Fit to the sin(phi) function instead of a linear function.
  //      this is consistent with the assumption that there is
  //      a simple X-offset, which can explain the data.  Only use run
  //      79644 since the earlier one is somehow bad.
  //         fcn = Off*sin(phi);
  //         dx  = Off*(220 cm)
  //     Run 79644:
  //         For East:
  //          Off = -0.00069686  +/- 0.00003325   radians
  //           dx = -0.1533      +/- 0.0073       cm
  //
  //         For West:
  //          Off =  0.00213004  +/- 0.00003138   radians
  //           dx =  0.4686      +/- 0.0068       cm
  //
  //  7)  Use the ZDC to determine how the collision vertex varies inside of PHENIX.  
  //      It seems that the beam position does indeed move, especially in the 
  //      Y coordinate.  AT step 7, we are not yet sure that the motion of the vertex 
  //      in the X direction is well understood since the X coordinates might not
  //      obey PHENIX conventions.  So, thus version of the code will *only* 
  //
  //      NOTES:  Run 65904 (B=0) has bad north; run 74443 copied from 74428.
  //
  //  8)  Re-run the ZDC data so that the code used for the collision determinations
  //      is static and does not vary across the runs.  After this change, the data looks
  //      quite reasonable and indicates only anout +/- 2 mm shift over time.
  //
  //  9)  OK, comparison of the ZDC determinations with the (more accurate and stable)
  //      dch beam positions determinations was quite revealing.  This data showed that
  //      the ZDC determinations of the collision point motion were somehow magnified
  //      even though the trend was right.  The final solution was to use the average
  //      detector-vs-beam position measurements as a single correction factor that will
  //      be applied to all runs.  not tracking the exact beam motion costs us a little
  //      worsening of the momentum resolution, but that is the best we can presently do.
  //
  // 10)  Use the avergae of proton and antiproton masses to finalize the momentum scale.
  //     
  //                                        FINAL VERSION  5-4-2003
  //                                        THOMAS K HEMMICK
  //

  // Finally do the offset measured for the reference run...

  float new_alphaVal= new_alpha(alpha,phi,dcarm);
  float pred_orig = fabs(momentum_predictor(mom,alpha,zed,the0));
  float pred_altr = fabs(momentum_predictor(mom,new_alphaVal,zed,the0));

  return ScaleFactor*(mom + (pred_altr - pred_orig));

}

float   //contents from mom_changer.C
MomChangeRecal_dAu_Reco::new_alpha(float alpha, float phi, int dcarm)
{
  float delta_alpha = dx[dcarm]/refRadius*sin(phi) + dy[dcarm]/refRadius*cos(phi);
  if (phi>0.18&&phi<0.4) delta_alpha += 1.0723/1000.0;// Correction for "West Hole Previously-Dead Region"

  return alpha-delta_alpha;
}

float // from MomChangeRecalReco -- since alpha changes --> phi0 changes
MomChangeRecal_dAu_Reco::delta_phi0(float del_alpha)
{
  // number found from slope of phi0-phi vs alpha from run3dAu data
  return (del_alpha*1.6244);
}

float  //from mom_changer.C
MomChangeRecal_dAu_Reco::momentum_predictor(float p, float alpha, float zed, float the0) 
{  
  // First determine the momentum-dependent constants.
  float a1= 11.80 * exp(-(1/p-0.5)*(1/p-0.5)/(2*18.04*18.04));
  float a2= 0.00035 * (1.0 - exp(-8.9*p));
  float a3= exp(0.6789) * exp(-4.555*p) + 0.9956;

  // Now we use these constants to produce a momentum result:
  float z100 = zed/100.0;
  float pt = -1.0/(alpha*(a1 - a2*zed*zed + a3*z100*z100*z100*z100));
  float answer = pt/sin(the0);

  return answer;
}


