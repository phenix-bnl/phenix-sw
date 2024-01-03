#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "TNtuple.h"

#include "Combination.h"
#include "Diagnostic.h"
#include "KTofCutter.h"

ClassImp(Combination)

	template <class T> T sqr(const T& x) {
		return x * x;
	}

const double Combination::pi0_asymmetry_cut = 0.99;
const double Combination::eta_asymmetry_cut = 0.8;

//
// Calculate combination of two tracks and fill the structure
//
	bool
Combination::calcCombination( const KCluster *c1, const KCluster *c2, float theta, int req_trig, int vorder,TH2F *pair_timing_sec[])
{

	_cluster_a = c1;
	_cluster_b = c2;

	_tofcorr_a = c1->tofcorr();
	_tofcorr_b = c2->tofcorr();
	
	//std::cout<<"\n in Combination TOF A "<<_tofcorr_a<<"\t"<<_tofcorr_b;

	float deltaR;

	goodFlag = 0;
	EselFlag = 0;       // ondrejch 5/6/2010, use only ecore (old)

	float      prob_a,     prob_b;     // nEmcClusterLocalExt's probability.
	float      chi2_a,     chi2_b;     // 

	//float      tofcorr_a, tofcorr_b;
	float      lx_a, lx_b, ly_a, ly_b;

	//
	// First require the on Ert
	//
	if(req_trig>=0 &&
			c1->getErtBit(req_trig)==0 &&
			c2->getErtBit(req_trig)==0) return false;


	//std::cout << "getting tightys" << std::endl;

	tighty_a = c1->getTightMapCut();
	tighty_b = c2->getTightMapCut();

	//std::cout << "getting arms" << std::endl;
	arm_a = c1->getArm();
	arm_b = c2->getArm();

	if ( arm_a != arm_b ) return false;

	sec_a = c1->getSec();
	sec_b = c2->getSec();

	if  ( ( sec_a == 6 || sec_a == 7 ) && ( sec_b < 6 ) ) return false;
	if  ( ( sec_b == 6 || sec_b == 7 ) && ( sec_a < 6 ) ) return false;

	iy_a = c1->getIy();
	iy_b = c2->getIy();

	iz_a = c1->getIz();
	iz_b = c2->getIz();

	if (sec_a < 6)
	{
		sm_a_iy = int(iy_a/12);
		sm_a_iz = int(iz_a/12);
	}
	else
	{
		sm_a_iy = int(iy_a/16);
		sm_a_iz = int(iz_a/16);
	}      

	if (sec_b < 6)
	{
		sm_b_iy = int(iy_b/12);
		sm_b_iz = int(iz_b/12);
	}
	else
	{
		sm_b_iy = int(iy_b/16);
		sm_b_iz = int(iz_b/16);
	}

	if(EselFlag==0){
		e1 = c1->ecore();
		e2 = c2->ecore();
	}
	else{
		e1 = c1->ecore_newe();
		e2 = c2->ecore_newe();
	}

	// ENERGY CUT
	e = e1 + e2;
	if( (e < 0.0) || (e > 40.0) ) return false;

	// ASYM CUT
	asym = fabs( e1 - e2 ) / e;
	if( asym < 0.0 || asym > 1.0) return false;
	//std::cout << "passed asymmetry cut" << std::endl;

	c1_xyz[0] = c1->x();
	c1_xyz[1] = c1->y();
	c1_xyz[2] = c1->z();

	c2_xyz[0] = c2->x();
	c2_xyz[1] = c2->y();
	c2_xyz[2] = c2->z();

	deltaR = sqrt( sqr(c1->x() - c2->x()) +
			sqr(c1->y() - c2->y()) +
			sqr(c1->z() - c2->z()) );

	if( deltaR < 8. ) return false;

	//std::cout << "passed deltaR" << std::endl;

	norm_tr1[0] = c1->getTrackVector(0);
	norm_tr1[1] = c1->getTrackVector(1);
	norm_tr1[2] = c1->getTrackVector(2);

	norm_tr2[0] = c2->getTrackVector(0);
	norm_tr2[1] = c2->getTrackVector(1);
	norm_tr2[2] = c2->getTrackVector(2);

	// Reconstruct invariant mass.

	// MOMETUM
	float px = e1 * (norm_tr1[0]) + e2 * (norm_tr2[0]);
	float py = e1 * (norm_tr1[1]) + e2 * (norm_tr2[1]);
	float pz = e1 * (norm_tr1[2]) + e2 * (norm_tr2[2]);

	pxyz[0] = px;
	pxyz[1] = py;
	pxyz[2] = pz;

	double trkLength = sqrt( px*px + py*py + pz*pz);

	if ( trkLength == 0) 
	{  
		TrackVector[0] = 0;
		TrackVector[1] = 0;
		TrackVector[2] = 0;
	}
	else
	{
		TrackVector[0] = px / trkLength;
		TrackVector[1] = py / trkLength;
		TrackVector[2] = pz / trkLength;

	}

	// TRANSVERSE MOMENTUM and CUT
	pt = sqrt( (px * px) + (py * py) );
	if( pt < 0.5 || pt > 200.0 ) return false;
	//std::cout << "passed pt cut" << std::endl;

	// OPENING ANGLE AND CUT
	cosine =
		norm_tr1[0] * norm_tr2[0] +
		norm_tr1[1] * norm_tr2[1] +
		norm_tr1[2] * norm_tr2[2];

	if( cosine < -1.0 || cosine > 1.0 ) return false;

	//float phi = atan2(py,px);
	const float pi = M_PI;
	_phi = atan2(py,px);
	_rap = (1./2) * log( (e+pz)/(e-pz) );
	double tang = atan2(abs(pz),(px*px+py*py) );
	_eta = log( tan (tang) );
	//std::cout << pz << "  " << pt << "  " << atan2(pz,pt) << "  " << tan2 (atan2(abs(pz),(px*px+py*py) ) ) << "  " << log( (double)tan(atan2(abs(pz),(px*px+py*py) ) ) ) << std::endl;
	//std::cout << _phi << std::endl;

	float dphi;

	if(vorder==1) dphi = atan2(sin((_phi-theta)),cos((_phi-theta)));
	else dphi = atan2(sin(2*(_phi-theta)),cos(2*(_phi-theta)))/2.;

	//
	// bin with respect to reaction plane
	//
	float DivUnit = pi/12./((float)vorder); // binwidth unit

	reac=11;
	for(int i=0;i<12;i++){
		if (fabs(dphi) >= ((float)i)*DivUnit
				&& fabs(dphi) < ((float)i+1)*DivUnit)
		{
			reac = i;
		}
	}

	//std::cout << "RP bin was calculated" << std::endl;
	// INVARIANT MASS
	//
	inv_mass = sqrt( 2 * e1 * e2 * ( 1.0 - cosine ) );
	if( inv_mass < 0.0 ||
			inv_mass > 500.0 ) return false;
	//      inv_mass > 1.0 ) return false;

	kcm = inv_mass/2.;
	//std::cout << "kcm = " << kcm << std::endl;

	prob_a    = c1->prob_photon();
	prob_b    = c2->prob_photon();

	chi2_a    = c1->chi2();
	chi2_b    = c2->chi2();



	CRTOFCut   = 0;
	if (fabs(_tofcorr_a - _tofcorr_b) < 1.2)
	{
		//std::cout << _tofcorr_a << "  " << _tofcorr_b << std::endl;
		CRTOFCut   = 1;
	}

//     Fill the t1/t2 2D Histogram 
       if(sec_a==sec_b&&pt>1.) pair_timing_sec[sec_a]->Fill(_tofcorr_a,_tofcorr_b);

// Sigmatized tof cut
	CTOFCut   = -1;
        double Sigma_cut[8] = {0.835379,0.904834,0.964893,1.03263,0.841881,0.977165,0.979313,1.16456}; //17425; MB
        if ( fabs(_tofcorr_a) < 1.0*Sigma_cut[sec_a] && fabs(_tofcorr_b) < 1.0*Sigma_cut[sec_b] )
        {
                CTOFCut   = 0;
        }
	else if ( fabs(_tofcorr_a) < 1.5*Sigma_cut[sec_a] && fabs(_tofcorr_b) < 1.5*Sigma_cut[sec_b] )
        {
                CTOFCut   = 1;
        }
        
        else if ( fabs(_tofcorr_a) < 2.0*Sigma_cut[sec_a] && fabs(_tofcorr_b) < 2.0*Sigma_cut[sec_b] )
        {
                CTOFCut   = 2;
        }
	else if ( fabs(_tofcorr_a) < 2.5*Sigma_cut[sec_a] && fabs(_tofcorr_b) < 2.5*Sigma_cut[sec_b] )
        {
                CTOFCut   = 3;
        }
	else if ( fabs(_tofcorr_a) < 3.0*Sigma_cut[sec_a] && fabs(_tofcorr_b) < 3.0*Sigma_cut[sec_b] )
        {
                CTOFCut   = 4;
        }
        else if ( fabs(_tofcorr_a) < 4.0*Sigma_cut[sec_a] && fabs(_tofcorr_b) < 4.0*Sigma_cut[sec_b] )
	// else if ( fabs(_tofcorr_a) < 5.0 && fabs(_tofcorr_b) < 5.0 )
        {
                CTOFCut   = 5;
        }


        BoxCut = -1;
        if(fabs(_tofcorr_a) < 3.&&fabs(_tofcorr_b) < 3.) BoxCut=0;
        else if(_tofcorr_a < 25. && _tofcorr_a > 8. && _tofcorr_b < 25. && _tofcorr_b > 8.) BoxCut=1; 
        else if(_tofcorr_a < 70. && _tofcorr_a > 25. && _tofcorr_b < 70. && _tofcorr_b > 25.) BoxCut=2;
        else if(_tofcorr_a < -8. && _tofcorr_a > -30. && _tofcorr_b < -8. && _tofcorr_b > -30.) BoxCut=3;
        else if(_tofcorr_a < -8. && _tofcorr_a > -30. && fabs(_tofcorr_b) < 8.)  BoxCut=4;
        else if(fabs(_tofcorr_a) < 8. && _tofcorr_b < -8. && _tofcorr_b > -30.)  BoxCut=5;
        else if(_tofcorr_a < 25. && _tofcorr_a > 8. && fabs(_tofcorr_b) < 8.)  BoxCut=6;
        else if(fabs(_tofcorr_a) < 8. && _tofcorr_b < 25. && _tofcorr_b > 8.)  BoxCut=7;
        else if(_tofcorr_a < -8. && _tofcorr_a > -30. && _tofcorr_b < 70. && _tofcorr_b > 25.) BoxCut=8;
        else if(_tofcorr_a < 70. && _tofcorr_a > 25. && _tofcorr_b < -8. && _tofcorr_b > -30.) BoxCut=9;
        else if(_tofcorr_a < 70. && _tofcorr_a > 25. && fabs(_tofcorr_b) < 8.)  BoxCut=10;
        else if(fabs(_tofcorr_a) < 8. && _tofcorr_b < 70. && _tofcorr_b > 25.)  BoxCut=11;

	
	CChi2Cut=0;
	if(c1->getChi2Cut()>0&&c2->getChi2Cut()>0)
	  {
	    if(c1->getChi2Cut()<=c2->getChi2Cut())
	      CChi2Cut=c2->getChi2Cut();
	    else if (c1->getChi2Cut()>c2->getChi2Cut())
	      CChi2Cut=c1->getChi2Cut();
	  }

	/*
	CChi2Cut = 0;
	if (c1->getChi2Cut() == 1 && c2->getChi2Cut() == 1)
	{
		CChi2Cut   = 1;
	}
	else if (c1->getChi2Cut() == 2 && c2->getChi2Cut() == 2)
	{
		CChi2Cut   = 2;
	}
	if (c1->getChi2Cut() == 3 && c2->getChi2Cut() == 3)
	{
		CChi2Cut   = 3;
		}
	*/



//printf("chi2 = %d\n",CChi2Cut);

	for(int i = 0; i < 5; i++) 
	{ 
		CStochCut[i] = 0; 
		if (c1->getStochCut(i,EselFlag) == 1 && c2->getStochCut(i,EselFlag) == 1) 
		{ 
			CStochCut[i] = 1; 
		} 
	}
/*
	CProbPhotCut = 0;
	if(prob_a > 0.02 && prob_b > 0.02)	CProbPhotCut = 1;
	//printf("CProbPhotCut=%d\n",CProbPhotCut);
*/
	CChargeRejCut = 0;
	if (c1->getChargeRejCut() == 1 || c2->getChargeRejCut() == 1)
	{
		CChargeRejCut   = 1;
	}

	CPc3RejCut = 0;
	if (c1->getPc3RejCut() >= 1 || c2->getPc3RejCut() >= 1)
	{
		CPc3RejCut   = 1;
	}

	// one photon should be very far from the edge
	CTightFiducial = 0;
	if (c1->getTightFiducial() == 1 || c2->getTightFiducial() == 1)
	{
		CTightFiducial = 1;
	} 

	ec_a = e1;
	ec_b = e2;

	lx_a = c1->getLx();
	lx_b = c2->getLx();
	ly_a = c1->getLy();
	ly_b = c2->getLy();

	if(sec_a<6){
		Ly = abs((c1->iypos()+c2->iypos())/24-1);
		Lz = abs((c1->izpos()+c2->izpos())/24-3);
	}
	else{
		Ly = abs((c1->iypos()+c2->iypos())/24-2);
		Lz = abs((c1->izpos()+c2->izpos())/24-4);
	}

	goodFlag = 1;

	return true;
}


double Combination::getTrackVector(const int i) const
{
	if ( i<0 || i>2) return 0;

	return TrackVector[i];
}

//     boxes cut -----------------

        void
Combination::fillBoxesHist( THmulf *gghboxes,int mixFlag, int cent )
{
        if ( pt < 1.0 ) return;
        int box = BoxCut;

        if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b)))

        {
                if ( sec_a == sec_b && CChi2Cut == 1 )
                {
                       //printf("mass=%.3lf,pt=%.2lf,sec=%d,box=%d\n",inv_mass,pt,sec_a,box);
                        gghboxes->Fill(1.0, inv_mass, pt,sec_a,box);
                }
        }
}

//-------------------------------------------
	void 
	Combination::fillAsymHist( THmulf *gghasym, int mixFlag, int cent, int ERTtrig)
{
	if ( pt < 0.5 ) return;
	int det = 0;
	if (sec_a > 5) det = 1;
	int tofcut = CTOFCut;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
		//   if ( inv_mass>0) 
	{
		//if ( sec_a < 6 && sec_a == sec_b && asym < 0.8 ) 
		if ( sec_a == sec_b && asym < 0.8 ) 
		  gghasym->Fill(1.0, inv_mass, pt, mixFlag, cent, CChi2Cut, sec_a,  tofcut, ERTtrig);
                
	}
}


void Combination::fill_sysErr_asym( THmulf *ggh_sysErr_Asym, int mixFlag, int cent, int ERTb)
{
	if ( pt < 0.5 ) return;
	int det = 0;
	if (sec_a > 5) det = 1;
	int tofcut = CTOFCut;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
	  {
	    if ( sec_a == sec_b && sec_a<6) 
	      ggh_sysErr_Asym->Fill(1.0, inv_mass, pt, mixFlag, cent, CChi2Cut, asym, tofcut, ERTb);
                
	}
}













	void 
Combination::fillRapHist( THmulf *gghrap, int cent)
{
	if ( pt < 0.5 ) return;
	int tofcut = CTOFCut;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
		//   if ( inv_mass>0) 
	{
		if ( sec_a == sec_b && CChi2Cut == 1 && asym < 0.8) 
		{
			gghrap->Fill(1.0, inv_mass, pt, _rap, cent, tofcut);
		}
	}
}

	void 
Combination::fillCheck( THmulf *gghcheck, int mixFlag, int spin, int xing)
{
	if ( pt < 1.0 ) return;
	if ( CTOFCut != 0 && CTOFCut != 1 ) return;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
		//   if ( inv_mass>0) 
	{
		if ( asym < 0.8 && CChi2Cut == 1 ) 
		{
			//"mass","pt","mix","rorl","chi2","sec","bbcz"
			gghcheck->Fill(1.0, inv_mass, mixFlag, arm_a, pt, spin, xing);
		}
	}
}


	void 
Combination::fillSpinHist( THmulf *gghspin, int cent, int spin)
{
	if ( pt < 0.5 ) return;
	int det = 0;
	if (sec_a > 5) det = 1;
	int tofcut = CTOFCut;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
		//   if ( inv_mass>0) 
	{
		if ( CChi2Cut == 1 && asym < 0.8) 
		{
			gghspin->Fill(1.0, inv_mass, pt, cent, sec_a, spin, tofcut);
		}
	}
}

	void 
Combination::fillSpinAsymHist( THmulf *gghspin, int cent, int spin)
{
	if ( pt < 0.5 ) return;
	int det = 0;
	if (sec_a > 5) det = 1;
	int tofcut = CTOFCut;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
		//   if ( inv_mass>0) 
	{
		if ( CChi2Cut == 1 && asym < 0.99) 
		{
			gghspin->Fill(1.0, inv_mass, pt, cent, sec_a, spin, tofcut);
		}
	}
}

	void 
Combination::fillPhiEta(TH2F *hAcc)
{
	if ( pt < 0.5 ) return;
	int tofcut = CTOFCut;
	if(_phi < -1) _phi = 2*3.14159 + _phi;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
		//   if ( inv_mass>0) 
	{
		if ( CChi2Cut == 1 && asym < 0.8 && (tofcut == 0 || tofcut == 1) && (inv_mass>0.11&&inv_mass<0.18)) 
		{
			hAcc->Fill(_phi, _rap);
		}
	}
}

	void 
Combination::fillSpinPhi( THmulf *gghspin, int cent, int spin)
{
	if ( pt < 0.5 ) return;
	int det = 0;
	if (sec_a > 5) det = 1;
	int tofcut = CTOFCut;
	if(_phi < -1) _phi = 2*3.14159 + _phi;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
		//   if ( inv_mass>0) 
	{
		if ( CChi2Cut == 1 && asym < 0.8 && abs(_rap) < 0.5) 
		{
			gghspin->Fill(1.0, inv_mass, pt, _phi, spin, tofcut);
		}
	}
}

	void 
Combination::fillShuffle( THmulf *gghshuffle, int bunch)
{
	if ( pt < 0.5 ) return;
	int side = -1;
	if (sec_a < 4 && sec_b < 4) side = 0;
	if (sec_a > 3 && sec_b > 3) side = 1;
	if ( side == -1 ) return;
	//std::cout << side << std::endl;
	if ( CTOFCut != 0 && CTOFCut != 1 ) return;

	if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b)) ) 
	{
		if ( CChi2Cut == 1 && asym < 0.8) 
		{
			gghshuffle->Fill(1.0, inv_mass, pt, side, bunch);
		}
	}
}

void Combination::fillRPHist( THmulf *gghrp, int mixed, int cent )
{

	if (pt >= 1.0 && asym < pi0_asymmetry_cut )
	{
		if ( (sec_a == sec_b) )
		{
			if ( cosine < (1. - (0.0425*0.0425)/(2.*ec_a*ec_b))) 
			{
				//// No-PID
				gghrp->Fill(1.0, inv_mass, pt, mixed, cent, 0, reac, sec_a, CPc3RejCut, asym);

				//// Chi2 < 3
				if( CChi2Cut > 0 ){
					gghrp->Fill(1.0, inv_mass, pt, mixed, cent, 1, reac, sec_a, CPc3RejCut, asym);
				}

				//// Stoch Cut 2
				if( CStochCut[1] >0 ){
					gghrp->Fill(1.0, inv_mass, pt, mixed, cent, 2, reac, sec_a, CPc3RejCut, asym);
				}
			}
		}
	}
}


void
Combination::fillNtuple(TNtuple* ntp, int inCent, float inPtCut, int inRunN) const
{
	//   std::cout << "*** DIAG NTUPLE FILLED ***"<<std::endl
	if ( ntp == 0 ) return;

	// Some cuts to reduce the output to the ntuple
	//
	if ( pt < inPtCut ) return;
	if ( asym > pi0_asymmetry_cut ) return;

	KTofCutter* tofcutter = KTofCutter::getInstance();

	Diagnostic diag;

	//  diag.trig = inTrig;
	diag.trig = 0;
	diag.cent = inCent;
	diag.vtxZ = _cluster_a->getVtxZ();
	diag.Epair = e;
	diag.pt = pt;
	diag.costheta = cosine;
	diag.phi = _phi;
	diag.mass = inv_mass;
	diag.asym = asym;

	diag.sec1 = sec_a; // has been translated to sector id = [0,7]
	diag.iy1 = iy_a;
	diag.iz1 = iz_a;
	diag.Ecore1 = ec_a;
	diag.E1 = _cluster_a->e();
	diag.E91 = _cluster_a->e9();
	diag.tof1 = _tofcorr_a;
	diag.tofrd1  = tofcutter->tofRelOffset(sec_a, _cluster_a->pt, _tofcorr_a);
	//  diag.tofrd1  = 0.0;
	diag.prob1 = _cluster_a->prob_photon();
	diag.disp1 = _cluster_a->getDispN();
	diag.chisq1 = _cluster_a->chi2();

	// Coords for the cluster
	diag.x1 = _cluster_a->x();
	diag.y1 = _cluster_a->y();
	diag.z1 = _cluster_a->z();
	diag.xp1 = _cluster_a->getPEmcX();
	diag.yp1 = _cluster_a->getPEmcY();
	diag.zp1 = _cluster_a->getPEmcZ(); 
	diag.stoch1 = (0.3+4*exp(-diag.Ecore1/_cluster_a->ecent()))*(1.9-0.67*diag.chisq1);
	diag.pc3dphi1 = _cluster_a->emcpc3dphi();
	diag.pc3dz1 = _cluster_a->emcpc3dz();


	//   // Coords for the swapping
	//   diag.sx1 = 0;
	//   diag.sy1 = 0;
	//   diag.sz1 = 0;
	//   diag.sxp1 = 0;
	//   diag.syp1 = 0;
	//   diag.szp1 = 0; 

	diag.sec2 = sec_b;
	diag.iy2 = iy_b;
	diag.iz2 = iz_b;
	diag.Ecore2 = ec_b;
	diag.E2 =  _cluster_b->e();
	diag.E92 = _cluster_b->e9();
	diag.tof2 = _tofcorr_b;
	diag.tofrd2  = tofcutter->tofRelOffset(sec_b, _cluster_b->pt, _tofcorr_b);
	//  diag.tofrd2  = 0.0;
	diag.prob2 = _cluster_b->prob_photon();
	diag.disp2 = _cluster_b->getDispN();
	diag.chisq2 = _cluster_b->chi2();

	diag.x2 = _cluster_b->x();
	diag.y2 = _cluster_b->y();
	diag.z2 = _cluster_b->z();
	diag.xp2 = _cluster_b->getPEmcX();
	diag.yp2 = _cluster_b->getPEmcY();
	diag.zp2 = _cluster_b->getPEmcZ(); 
	diag.stoch2 = (0.3+4*exp(-diag.Ecore2/_cluster_b->ecent()))*(1.9-0.67*diag.chisq2);
	diag.pc3dphi2 = _cluster_b->emcpc3dphi();
	diag.pc3dz2 = _cluster_b->emcpc3dz();
	diag.et00ecore = _cluster_b->get_et_00_ecore();
	diag.et02ecore = _cluster_b->get_et_02_ecore();
	diag.et00e = _cluster_b->get_et_00_e();
	diag.et02e = _cluster_b->get_et_02_e();
	diag.bbcqn = _cluster_b->get_bbcqn();
	diag.bbcqs = _cluster_b->get_bbcqs();
	diag.nevents = _cluster_b->get_nevents();


	/*  diag.sx2 = 0;
			diag.sy2 = 0;
			diag.sz2 = 0;
			diag.sxp2 = 0;
			diag.syp2 = 0;
			diag.szp2 = 0; */
	diag.runn = inRunN;

	ntp->Fill((Float_t*)&diag);

	return;
}


// EOF
