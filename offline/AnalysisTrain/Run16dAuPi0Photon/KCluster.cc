#include "KCluster.h"
#include "KTofCutter.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "PHPoint.h"
#include "PHLine.h"
#include <math.h>
#include "Diagnostic.h"


template<class T> T SQ(T x) { return x*x; }

ClassImp(KCluster);

int KCluster::inputFlag = 0;

void KCluster::setArmSecIyIz() {
  karm = arm();
  sec = sector();
  iy = iypos();
  iz = izpos();
  
  if (karm==1) {
      sec = 7 - sec;
    }
}  

void KCluster::setLocalPos(mEmcGeometryModule *EmcGeo) {
  lx = -1; ly = -1; lz = -1;
  EmcGeo->GlobalToLocal(x(),y(),z(),sec,lx,ly,lz);
}

void KCluster::setTrackVector(float theta) {
  float trkLength, px, py, pz, px_newe, py_newe, pz_newe; 
  
  const float pi = M_PI;

  trkLength = sqrt(x()*x() + y()*y() + (z()-vtxZ)*(z()-vtxZ));
  xyz_unit[0] = x()/trkLength;
  xyz_unit[1] = y()/trkLength;
  xyz_unit[2] = (z()-vtxZ)/trkLength;

  // For old ecore def.
  px = ecore() * xyz_unit[0];
  py = ecore() * xyz_unit[1];
  pz = ecore() * xyz_unit[2]; 

  pt = sqrt(px*px + py*py);

  // For new ecore def.
  px_newe = ecore_newe() * xyz_unit[0];
  py_newe = ecore_newe() * xyz_unit[1];
  pz_newe = ecore_newe() * xyz_unit[2]; 

  pt_newe = sqrt(px_newe*px_newe + py_newe*py_newe);

  if(pt/ecore()<0.910){
    std::cout << "Cosine Error: " << pt/ecore()<< std::endl;
    std::cout << "at Ecore: " << ecore()<< std::endl;
    std::cout << "at pt: " << pt<< std::endl;
    std::cout << "at VTX: " << vtxZ<< std::endl;
    std::cout << "at X: " << x()<< std::endl;
    std::cout << "at Y: " << y()<< std::endl;
    std::cout << "at Z: " << z()<< std::endl;
  }

  phi = atan2(py,px);
  phiPHI = atan2(sin(2*(phi-theta)),cos(2*(phi-theta)))/2.;
  phiDEG = phi*180./pi;
  phiPHIDEG = phiPHI*180./pi;
  
  rap = (1./2) * log( (ecore()+pz)/(ecore()-pz) );

}

/*
/// tof resolution, tof cut 3-sigma, 2-sigma 
/// 450 ps                1.35          0.9
/// 700 ps                2.1           1.4
/// 1.2 ns                3.6           2.4
void KCluster::passTOFCut()
{
  TOFCut = 0;
  if (sec < 6) { // PbSc
      if ( fabs(tofcorr()) < 3.6 ) { // in ns
	  TOFCut = 1;
	  if ( fabs(tofcorr()) < 2.4 ) {
	      TOFCut = 2;
	    }
	}
  }  else {// PbGl
      if ( fabs(tofcorr()) < 1.8 ) {
	  TOFCut = 1;
      }
  }
}*/

/*
void KCluster::passTOFCut()
{
  //KTofCutter* tofcutter = KTofCutter::getInstance();

  //TOFCut = 0;
  double tofdiff = fabs(tofcorr());//fabs(tofcutter->tofRelOffset(sec,pt,tofcorr()));
  if ( tofdiff < 5.)  { // in \sigma
    TOFCut = 1;
    if ( tofdiff < 2. ) {
      TOFCut = 2;
      if ( tofdiff < 1. ) {
        TOFCut = 3;
      }
    }
  }
  //std::cout << tofdiff << "  " << TOFCut << std::endl;
}

*/

/*
void KCluster::passTOFCut()
{
  TOFCut = -1;
  double tof = fabs(tofcorr());

  if ( tof < 1.2 )
    {
       TOFCut   = 0;
    }
  else if ( tof < 5 )
    {
       TOFCut   = 1;
    }
  else if ( tof > 5 && tof < 10 )
    {
       TOFCut   = 2;
    }
  else if ( tof > 10 )
    {
       TOFCut   = 3;
    }

//  std::cout << "--------------------------------" << tof << "  " << TOFCut << std::endl;
}
*/



void KCluster::passTOFCut(int sec)
{
  double sigma[8] = {0.835379,0.904834,0.964893,1.03263,0.841881,0.977165,0.979313,1.16456}; //17425; MB
 
  TOFCut = -1;
  double tof = fabs(tofcorr());

  if ( tof < 1.0*sigma[sec] )
    {
       TOFCut   = 0;
    }
  else if ( tof < 1.5*sigma[sec] )
    {
       TOFCut   = 1;
    }
  else if ( tof < 2.0*sigma[sec] )
    {
       TOFCut   = 2;
    }
  else if ( tof < 2.5*sigma[sec] )
    {
       TOFCut   = 3;
    }
  else if ( tof < 3.0*sigma[sec] )
    {
       TOFCut   = 4;
    }
  // else if ( tof < 5.0 )
  else if ( tof < 4.0*sigma[sec] )
    {
       TOFCut   = 5;
    }


//  std::cout << "--------------------------------" << tof << "  " << TOFCut << std::endl;
}




void KCluster::passCHI2Cut() {
  chi2Cut = 0;
  if(sec<6){
     if ( chi2() < 2.0 ) chi2Cut = 1;
     
     else   if ( chi2() < 3.0 ) chi2Cut = 2;
     else  if ( chi2() < 4.0 ) chi2Cut = 3;
  }
  else{

    // WGH: theta is apparently not in run7 CWG's, no cut for PbGl
    // ondrejch chi2Cut = 1;
    
    //      //
    //      // For PbGl, I will do a dispersion cut instead of chi2
    //      // ondrejch unmask
         const double p1 = 0.270;
         const double p2 = -0.0145;
          const double p3 = 0.00218;
    
    //      // This is the cut quantity
          double dispCut = p1+p2*theta()+p3*theta()*theta();
    
          double dispMax = corrdispz();
          if(dispMax < corrdispy()) dispMax = corrdispy();
    
    //      // Now apply cut
          if (dispMax<dispCut) chi2Cut=1; // ondrejch end unmask
  }
}      

void KCluster::passTwrhitCut() {
  twrhitCut = 0;
  if ((sec < 6 && ecore() < 1.0) || (sec > 5 && ecore() < 1.0))  {
      if ( multiplicity() < 10. ) {
	  twrhitCut = 1;
      }
  }  else    {
      if ( multiplicity() < 20. ) {
	  twrhitCut = 1;
      }
  }
}      

void KCluster::passStochasticCuts() { 
  for(int i=0;i<5;i++)stochCut[i]=0; 
  for(int i=0;i<5;i++)stochCut_newe[i]=0; 
  
  if ((0.3+4*exp(-ecore()/ecent()))* 
      (1.9-0.67*chi2())*(1.-exp(-8.*padispy()/padispz()))>1.4) 
    // && padispz()>0 && padispy()>0) 
    stochCut[0] = 1; 

  if ((0.3+4*exp(-ecore_newe()/ecent()))* 
      (1.9-0.67*chi2())*(1.-exp(-8.*padispy()/padispz()))>1.4) 
    // && padispz()>0 && padispy()>0) 
    stochCut_newe[0] = 1; 
  
  if ((0.3+4*exp(-ecore()/ecent()))* 
      (1.9-0.67*chi2())>1.4) 
    stochCut[1] = 1; 
  
  if ((0.3+4*exp(-ecore_newe()/ecent()))* 
      (1.9-0.67*chi2())>1.4) 
    stochCut_newe[1] = 1; 
  
  if (((0.3+4*exp(-ecore()/ecent()))* 
       (1.9-0.67*chi2())>1.4)&&(fabs(tofcorr())<1.2)) 
    stochCut[2] = 1; 
  
  if (((0.3+4*exp(-ecore_newe()/ecent()))* 
       (1.9-0.67*chi2())>1.4)&&(fabs(tofcorr())<1.2)) 
    stochCut_newe[2] = 1; 
  
  if ((0.3+4*exp(-ecore()/ecent()))* 
      (1.9-0.67*chi2()/twrhit())*(1.-exp(-8.*padispy()/padispz()))>1.4) 
    // && padispz()>0 && padispy()>0) 
    stochCut[3] = 1; 
  
  if ((0.3+4*exp(-ecore_newe()/ecent()))* 
      (1.9-0.67*chi2()/twrhit())*(1.-exp(-8.*padispy()/padispz()))>1.4) 
    // && padispz()>0 && padispy()>0) 
    stochCut_newe[3] = 1; 
  
  if ((0.3+4*exp(-ecore()/ecent()))* 
      (1.9-0.67*chi2())>2.0) 
    stochCut[4] = 1; 
  
  if ((0.3+4*exp(-ecore_newe()/ecent()))* 
      (1.9-0.67*chi2())>2.0) 
    stochCut_newe[4] = 1; 
  
} 

bool KCluster::passFiducialCuts() {
//  if ( lx < 8. || lx > 389. || ly < 2. || ly > 191. ) 
  if ( lx < 8. || lx > 389. || ly < 5. || ly > 188. )  // ondrejch
     {
       return false;
     }

  // Tight fiducial cut (~20 cm in x and ~25 cm in y)
//  if ( (lx > 20. && lx < 377.) && (ly > 25. && ly < 168.) )  
  if ( (lx > 20. && lx < 377.) && (ly > 15. && ly < 178.) ) // ondrejch
     {
       tightFiducial = 1;
     }

  return true;
}      

void KCluster::passTrackRejCut(PHCentralTrack *trackList) {

  // distance btw. cluster and track projection (in cm)
  float dist;

  // "swapped" distance (also in cm)
  float flipdist;

  float xtrk, ytrk, ztrk;
  float dx, dy, dz;

  float xftrk, yftrk, zftrk;  // for flipping
  float dxf, dyf, dzf; 

  // this is used for flipping because of x difference in PbSc and PbGl
  //  float hitdist = sqrt( (x() * x() ) +
  //			(y() * y()) +
  //			(z() - vtxZ) * (z() - vtxZ));  

  // remove z part till we understand its source...
  float hitdist = sqrt( (x() * x() ) +
			(y() * y()) );

  int numtracks = trackList->get_npart(); 
  _minDist = -1.;
  _minFlipDist = -1.;

  // cut only gets initialized for real and false associations
  chargeRejCut = 0;
  flipChargeRejCut=0;

  for (int itrk=0; itrk<numtracks; itrk++)     {
      //  PhCglSnglv2* trk;
      PHSnglCentralTrack* trk = trackList->get_track(itrk); // sufficient for both nDSTs and pDSTs
 
//       if (inputFlag == 0) // this is NOT the pDST
// 	{
// 	  trk = trackList->get_track(itrk);
// 	}
//       else // this is for pDSTs
// 	{
// 	  trk = (PhCglSnglv2*) trackList->get_track(itrk);
// 	}

      // Only look at good quality tracks
      // See the posting at: https://www.phenix.bnl.gov/phenix/WWW/p/lists/phenix-off-l/msg07182.html
      // 63 is the highest quality track: PC1 found/unique, UV found/unique (+both X1 & X2)
      // 31 is the next highest quality: PC1 found/ambiguous, UV found w/ one best choice (+both X1 & X2)
      // 
      // There is some debate over whether to use the VALUES of the bit pattern, or
      // a BITMASK.  Using the values (as we currently do), requires that the track quality be
      // exactly the highest and next highest as defined since Run2.
      //
      if (! (trk->get_quality()==31 || trk->get_quality()==63))
	continue;

      if (inputFlag == 0) // this is NOT the pDSTs....
	{
	  xtrk = trk->get_pemcx();
	  ytrk = trk->get_pemcy();
	  ztrk = trk->get_pemcz(); 
	  	  
	  xftrk = -xtrk;  //flip arm
	  yftrk = ytrk;
	  zftrk = ztrk;
	  
	  double epx = xtrk;
	  double epy = ytrk;
	  double epz = ztrk;
	  double epxf = xftrk;
	  double epzf = yftrk;
	  double epyf = zftrk;
	  //closestApproachPPP( xtrk, ytrk, ztrk, x(), y(), z(), epx, epy, epz); // this one
	  //closestApproachPPP( xftrk, yftrk, zftrk, x(), y(), z(), epxf, epyf, epzf); // and flipped one

	  dx = epx - x();
	  dy = epy - y();
	  dz = epz - z();
	  
	  dxf = epxf - x();
	  dyf = epyf - y();
	  dzf = epzf - z();
	  
	  dist = (510 / hitdist) * sqrt( dx*dx + dy*dy + dz*dz );  
	  flipdist =(510 / hitdist) *  sqrt( dxf*dxf + dyf*dyf + dzf*dzf );  

	  // only store if within the 50 cm histogram limit, otherwise keep -1

	  if (flipdist < _minFlipDist || (_minFlipDist<0. && flipdist <= 50.)) {
	      _minFlipDist = flipdist;
	    }
	  
	  if (dist < _minDist || (_minDist < 0. && dist <= 50.) ) {
	      _minDist = dist;
	      _pemcx = xtrk;
	      _pemcy = ytrk;
	      _pemcz = ztrk;
	    }
	 
	}
      else
	{
	  // emcid is useless in the pDST's, so compare ecore to get 
	  // the track corresponding to the current cluster
	  if (fabs(trk->get_ecore() - ecore()) < 0.0001) {
	      
	      _minFlipDist = -1.;  // no flipped distance for pDST's

	      // distance in cm
	      //
// 	      dist = sqrt ( SQ (trk->get_emcdz()) +
// 			    SQ (sqrt(SQ(x() + SQ(y()) * trk->get_emcdphi()))) );

	      // distance in sigma.  don't know if the dist in cm is even available in the pDST
	      //
	      dist = sqrt ( SQ(trk->get_emcsdz()) + SQ(trk->get_emcsdphi()) );
	      
	      if (dist < _minDist || (_minDist < 0. && dist <= 50.) ) {
		  _minDist = dist;
		}

	    }
	  
	}
    }

  // 2 sigma cuts 
  //
  if (_minDist > 10. || _minDist < 0.) chargeRejCut = 1;
  if (_minFlipDist > 10. || _minFlipDist < 0.) flipChargeRejCut = 1;
  
}

void 
KCluster::passPc3RejCut()
{

  pc3RejCut=0;
  double projDist = sqrt( SQ( emcpc3dz() ) 
			  + SQ( sqrt( SQ(x()) + SQ(y()) ) * emcpc3dphi() ) );

  if (projDist > 6.5)
    {
      // 1 sigma cut
      pc3RejCut=1;
      if (projDist > 13.)
 	{
 	  // 2 sigma cut
 	  pc3RejCut=2;
 	}
    }

  return;
}

void KCluster::fillHistRap( THmulf *gammarap, int centralityBin)
{
  int gammatofcut = 0;
  if (TOFCut >= 1) gammatofcut = 1;
  
  int det=0;
  if(sec<6) det=0;
  else det=1;
  if (chi2Cut >= 1 && pt>0.2)
  	gammarap->Fill(1.0, pt, centralityBin, rap, gammatofcut);

}
//void KCluster::fillHist( THmulf *gamma1, int centralityBin, int runn, TNtuple *gnt, int nclusters, int rpBin, int is_bookNt)
void KCluster::fillHist( THmulf *gamma1, int centralityBin, int runn, TNtuple *gnt, int is_bookNt, float inPtcut, int nclusters, int rpBin, int ERTb)
{
  int gammatofcut = 0;
//  if (TOFCut >= 1) gammatofcut = 1;
  gammatofcut = TOFCut;

  
  int det=0;
  if(sec<6) det=0;
  else det=1;

  gamma1->Fill(1.0,pt, centralityBin, sec, gammatofcut, 0.,rpBin,pc3RejCut, ERTb);

  if (chi2Cut == 1)
    gamma1->Fill(1.0,pt, centralityBin, sec, gammatofcut, 1.,rpBin,pc3RejCut, ERTb);

  if (chi2Cut == 2)
    gamma1->Fill(1.0,pt, centralityBin, sec, gammatofcut, 2.,rpBin,pc3RejCut,ERTb);

  if (chi2Cut == 3)
    gamma1->Fill(1.0,pt, centralityBin, sec, gammatofcut, 3., rpBin,pc3RejCut,ERTb);


/// fill gamma ntuple
#if(1)
#ifdef BOOK_GAMMA_NTUPLE
//  if(is_bookNt>0 && pt>5.0) {
//  if(is_bookNt>0 && pt>0.5) {
  if(is_bookNt>0 && pt>inPtcut) {

    KTofCutter *tofcutter = KTofCutter::getInstance();

    Diagamma dg;
//    dg.trig     = ntrig;
    dg.trig     = 1;
    dg.cent     = centralityBin;
    dg.vtxZ     = vtxZ;
    dg.nclust   = nclusters;

    dg.pt       = pt;
    dg.ptnewe   = pt_newe;
    dg.costheta = cos(theta());
    dg.phi      = phi;
  
    dg.sec      = sec;
    dg.iy       = iy;
    dg.iz       = iz;
    dg.ecore    = ecore();
    dg.ecorenew = ecore_newe();
    dg.ecent    = ecent();
    dg.tof      = tofcorr();
    dg.tofrd    = tofcutter->tofRelOffset(sec,pt,tofcorr());
//    dg.tofrd    = 0.0;
    dg.prob     = prob_photon();
    dg.disp     = getDispN();
    dg.chisq    = chi2();
    dg.twrhit   = twrhit();
    dg.padispy  = padispy();
    dg.padispz  = padispz();
    dg.stoch    = (0.3+4*exp(-ecore()/ecent()))*(1.9-0.67*chi2());

    dg.x      = x();
    dg.y      = y();
    dg.z      = z();
    dg.xp     = getPEmcX();
    dg.yp     = getPEmcY();
    dg.zp     = getPEmcZ(); 
    dg.pc3dphi = emcpc3dphi();
    dg.pc3dz   = emcpc3dz();
    dg.et00ecore = get_et_00_ecore();
    dg.et02ecore = get_et_02_ecore();
    dg.et00e = get_et_00_e();
    dg.et02e = get_et_02_e();
    dg.bbcqn = get_bbcqn();
    dg.bbcqs = get_bbcqs();
    dg.nevents = get_nevents();

    dg.runn   = runn;
  
    gnt->Fill((Float_t*)&dg);
  }
#endif
#endif


}
