#include "Run8PC3MatchRecal.h"
#include "Fun4AllReturnCodes.h"

#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "getClass.h"
#include "RunHeader.h"

#include "PHGlobal.h"

#include "PHCompositeNode.h"
#include "recoConsts.h"

#include <iostream>

using namespace std;
using namespace findNode;

Run8PC3MatchRecal::Run8PC3MatchRecal(const char* name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
}

int Run8PC3MatchRecal::InitRun(PHCompositeNode *topNode)
{

  RunHeader* d_run = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(!d_run){
    cout << PHWHERE << " RunHeader not found" << endl;
    return 0;
  }

  runNumber = d_run->get_RunNumber();

  return 0;
}

int Run8PC3MatchRecal::isValidRun(const int runno) const
{

  // Run8 dAu 200 GeV
  if (246214 <= runno && runno <=253701)
    {
      return 1;
    }
  // Run8 pp 200 GeV
  else if (256450 <= runno && runno < 259575)
    {
      return 1;
    }

  return 0;
}

int Run8PC3MatchRecal::process_event(PHCompositeNode *topNode)
{
  d_cnt    = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if (!d_cnt || !d_global) return 0;

  if(246214 <= runNumber && runNumber <=253701)
    {
      Calibrate_Run8dAu();
    }
  else if(256450 <= runNumber && runNumber <=259575)
    {
      Calibrate_Run8pp();
    }

  return EVENT_OK;
}

int Run8PC3MatchRecal::Calibrate_Run8dAu()
{
  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {

      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();      
      if (
	  sngltrk->isImplemented(sngltrk->get_the0()) &&
	  sngltrk->isImplemented(sngltrk->get_mom()) &&
	  sngltrk->isImplemented(sngltrk->get_alpha()) &&
	  sngltrk->isImplemented(sngltrk->get_zed()) &&
	  sngltrk->isImplemented(sngltrk->get_phi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3dphi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3dz()) 
	  )
	{
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
	}
      else
	{
	  sngltrk->ShutUp(1);
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
	  return 0;
	}
      
      sngltrk->ShutUp(1); // enable virtual warnings again


      float the0 = sngltrk->get_the0();
      float mom  = sngltrk->get_mom();
      float pt   = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }
      float alpha  = sngltrk->get_alpha();
      float zed    = sngltrk->get_zed();
      float phi    = sngltrk->get_phi();


      // Matching variables
      float pc3dphi = sngltrk->get_pc3dphi();
      float pc3dz   = sngltrk->get_pc3dz();

      //Calculate the new variables
      // ------------------
      float pc3sdphi = calculate_pc3sdphi_dAu(alpha, zed, phi, pt, pc3dphi);
      float pc3sdz   = calculate_pc3sdz_dAu  (alpha, zed, phi, pt, pc3dz  );
      // ------------------

      //Set the new variables
      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_pc3sdz(pc3sdz);
    }

  return EVENT_OK;
}

int Run8PC3MatchRecal::Calibrate_Run8pp()
{
  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {

      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();      
      if (
	  sngltrk->isImplemented(sngltrk->get_the0()) &&
	  sngltrk->isImplemented(sngltrk->get_mom()) &&
	  sngltrk->isImplemented(sngltrk->get_charge()) &&
	  sngltrk->isImplemented(sngltrk->get_zed()) &&
	  sngltrk->isImplemented(sngltrk->get_phi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3dphi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3dz()) 
	  )
	{
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
	}
      else
	{
	  sngltrk->ShutUp(1);
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
	  return 0;
	}
      
      sngltrk->ShutUp(1); // enable virtual warnings again

      float the0 = sngltrk->get_the0();
      float mom  = sngltrk->get_mom();
      float pt   = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }
      short charge = sngltrk->get_charge();
      float zed    = sngltrk->get_zed();
      float phi    = sngltrk->get_phi();

      // Matching variables
      float pc3dphi = sngltrk->get_pc3dphi();
      float pc3dz   = sngltrk->get_pc3dz();

      //Calculate the new variables
      // ------------------
      float pc3sdphi = calculate_pc3sdphi_pp(charge, zed, phi, pt, pc3dphi);
      float pc3sdz   = calculate_pc3sdz_pp  (charge, zed, phi, pt, pc3dz  );
      // ------------------

      //Set the new variables
      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_pc3sdz(pc3sdz);
    }

  return EVENT_OK;
}


float Run8PC3MatchRecal::calculate_pc3sdphi_dAu(const float alpha, const float zed, const float phi, const float pt, const float dphi)
{

  if(dphi==-9999||pt<0){return -9999;}

  //The constants used to calculate the mean and sigma
  float phi_mp0[8]={-0.00025237,-0.000329851,-0.000246941,-0.000132332,0.000304688,0.000249956,0.000200367,0.000106257};
  float phi_mp1[8]={0.000774948,0.0288195,0.00117845,0.0156174,0.0010071,0.0195076,0.00110677,0.021657};
  float phi_mp2[8]={-0.000696589,-0.00964979,0.000548157,-0.00487407,0.000120627,-0.00664539,-6.85E-05,-0.00692233};
  float phi_mp3[8]={0.0012406,0.00107878,-0.000447106,0.000507322,0.000154599,0.00074573,0.000756525,0.000725232};
  float phi_mp4[8]={-0.000690983,-0.0275843,-0.000584903,-0.0155739,-0.00153414,-0.0182875,-0.00126092,-0.0216262};
  
  float phi_sp0[8]={0.000909115,0.000792189,0.000862728,0.000859338,0.000906201,0.000821783,0.00091144,0.000787245};
  float phi_sp1[8]={-2.58E-05,0.00179816,-0.000227111,-5.03E-03,3.97E-06,-0.000799559,-6.03E-05,-9.48E-03};
  float phi_sp2[8]={-0.000421448,-0.00072275,-0.000125406,0.00163635,-0.00042448,0.000153265,-1.70E-05,0.00319743};
  float phi_sp3[8]={0.000285591,9.41E-05,0.00021002,-0.000173629,0.000380861,-3.47E-06,-6.96E-05,-0.000350401};
  float phi_sp4[8]={0.00107968,-0.000521949,0.0010534,0.00593809,0.00103577,0.00203562,0.00102953,0.0100638};
  
  float phimean;
  float phisigma;
  
  float sdphi;
  
  if(alpha<0)//positive particles pre magnet flip upto run 250484
    {
      if(zed>0)//in the North
	{
	  if(phi<1.5)//in the West
	    {
	      phimean = phi_mp0[0]/pt + phi_mp1[0]*phi + phi_mp2[0]*phi*phi + phi_mp3[0]*phi*phi*phi + phi_mp4[0];
	      phisigma = phi_sp0[0]/pt + phi_sp1[0]*phi + phi_sp2[0]*phi*phi + phi_sp3[0]*phi*phi*phi + phi_sp4[0];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  else       //in the East
	    {
	      phimean = phi_mp0[1]/pt + phi_mp1[1]*phi + phi_mp2[1]*phi*phi + phi_mp3[1]*phi*phi*phi + phi_mp4[1];
	      phisigma = phi_sp0[1]/pt + phi_sp1[1]*phi + phi_sp2[1]*phi*phi + phi_sp3[1]*phi*phi*phi + phi_sp4[1]; 
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  
	}
      else    //in the South
	{
	  if(phi<1.5)//in the West
	    {
	      phimean = phi_mp0[2]/pt + phi_mp1[2]*phi + phi_mp2[2]*phi*phi + phi_mp3[2]*phi*phi*phi + phi_mp4[2];
	      phisigma = phi_sp0[2]/pt + phi_sp1[2]*phi + phi_sp2[2]*phi*phi + phi_sp3[2]*phi*phi*phi + phi_sp4[2];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  else       //in the East
	    {
	      phimean = phi_mp0[3]/pt + phi_mp1[3]*phi + phi_mp2[3]*phi*phi + phi_mp3[3]*phi*phi*phi + phi_mp4[3];
	      phisigma = phi_sp0[3]/pt + phi_sp1[3]*phi + phi_sp2[3]*phi*phi + phi_sp3[3]*phi*phi*phi + phi_sp4[3];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	}
    }
  else        //negative particles  pre magnet flip upto run 250484
    {
      if(zed>0)//in the North
	{
	  if(phi<1.5)//in the West
	    {
	      phimean = phi_mp0[4]/pt + phi_mp1[4]*phi + phi_mp2[4]*phi*phi + phi_mp3[4]*phi*phi*phi + phi_mp4[4];
	      phisigma = phi_sp0[4]/pt + phi_sp1[4]*phi + phi_sp2[4]*phi*phi + phi_sp3[4]*phi*phi*phi + phi_sp4[4];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  else       //in the East
	    {
	      phimean = phi_mp0[5]/pt + phi_mp1[5]*phi + phi_mp2[5]*phi*phi + phi_mp3[5]*phi*phi*phi + phi_mp4[5];
	      phisigma = phi_sp0[5]/pt + phi_sp1[5]*phi + phi_sp2[5]*phi*phi + phi_sp3[5]*phi*phi*phi + phi_sp4[5];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	}
      else    //in the South
	{
	  if(phi<1.5)//in the West
	    {
	      phimean = phi_mp0[6]/pt + phi_mp1[6]*phi + phi_mp2[6]*phi*phi + phi_mp3[6]*phi*phi*phi + phi_mp4[6];
	      phisigma = phi_sp0[6]/pt + phi_sp1[6]*phi + phi_sp2[6]*phi*phi + phi_sp3[6]*phi*phi*phi + phi_sp4[6];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  else       //in the East
	    {
	      phimean = phi_mp0[7]/pt + phi_mp1[7]*phi + phi_mp2[7]*phi*phi + phi_mp3[7]*phi*phi*phi + phi_mp4[7];
	      phisigma = phi_sp0[7]/pt + phi_sp1[7]*phi + phi_sp2[7]*phi*phi + phi_sp3[7]*phi*phi*phi + phi_sp4[7];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	}
    }
  
  return sdphi;

}


float Run8PC3MatchRecal::calculate_pc3sdphi_pp(const short charge, const float zed, const float phi, const float pt, const float dphi)
{

  if(dphi==-9999||pt<0){return -9999;}

  //The constants used to calculate the mean and sigma
  float phi_mp0[8]={0.00036017,0.000297892,0.000277998,0.000113439,-0.000282722,-0.000323062,-0.000263378,-0.000142725};
  float phi_mp1[8]={0.000978276,0.0171747,0.0010826,0.0250509,0.000791772,0.0296928,0.00133601,0.0232377};
  float phi_mp2[8]={-0.000211288,-0.00592102,-0.00013122,-0.00810098,-0.00116634,-0.00997607,0.000318575,-0.00755386};
  float phi_mp3[8]={0.000549219,0.000669355,0.000725855,0.000858483,0.00197091,0.00111766,-0.000629346,0.000816189};
  float phi_mp4[8]={-0.00157207,-0.0158222,-0.00132481,-0.0248448,-0.000674176,-0.028331,-0.00049548,-0.0226577};
  
  float phi_sp0[8]={0.000850896,0.000833366,0.000922612,0.000817494,0.000936163,0.000788314,0.000891493,0.000846608};
  float phi_sp1[8]={0.000147124,0.00165757,-0.00019127,-0.0189152,0.000003566,0.00497455,-0.000287824,-0.00688997};
  float phi_sp2[8]={-0.000784293,-0.000706685,0.000034200,0.006471910,-0.000891760,-0.001808190,0.000021453,0.002290870};
  float phi_sp3[8]={0.000427785,0.000094546,0.000148277,-0.000724934,0.000860818,0.000216029,0.000142302,-0.000246942};
  float phi_sp4[8]={0.00108428,-0.000226603,0.000992956,0.0190024,0.00108108,-0.0035669,0.00103955,0.00760288};
  
  float phimean;
  float phisigma;
  
  float sdphi;
  
  if(charge>0)//positive particles
    {
      if(zed>0)//in the North
	{
	  if(phi<1.5)//in the West
	    {
	      phimean = phi_mp0[0]/pt + phi_mp1[0]*phi + phi_mp2[0]*phi*phi + phi_mp3[0]*phi*phi*phi + phi_mp4[0];
	      phisigma = phi_sp0[0]/pt + phi_sp1[0]*phi + phi_sp2[0]*phi*phi + phi_sp3[0]*phi*phi*phi + phi_sp4[0];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  else       //in the East
	    {
	      phimean = phi_mp0[1]/pt + phi_mp1[1]*phi + phi_mp2[1]*phi*phi + phi_mp3[1]*phi*phi*phi + phi_mp4[1];
	      phisigma = phi_sp0[1]/pt + phi_sp1[1]*phi + phi_sp2[1]*phi*phi + phi_sp3[1]*phi*phi*phi + phi_sp4[1]; 
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  
	}
      else    //in the South
	{
	  if(phi<1.5)//in the West
	    {
	      phimean = phi_mp0[2]/pt + phi_mp1[2]*phi + phi_mp2[2]*phi*phi + phi_mp3[2]*phi*phi*phi + phi_mp4[2];
	      phisigma = phi_sp0[2]/pt + phi_sp1[2]*phi + phi_sp2[2]*phi*phi + phi_sp3[2]*phi*phi*phi + phi_sp4[2];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  else       //in the East
	    {
	      phimean = phi_mp0[3]/pt + phi_mp1[3]*phi + phi_mp2[3]*phi*phi + phi_mp3[3]*phi*phi*phi + phi_mp4[3];
	      phisigma = phi_sp0[3]/pt + phi_sp1[3]*phi + phi_sp2[3]*phi*phi + phi_sp3[3]*phi*phi*phi + phi_sp4[3];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	}
    }
  else        //negative particles
    {
      if(zed>0)//in the North
	{
	  if(phi<1.5)//in the West
	    {
	      phimean = phi_mp0[4]/pt + phi_mp1[4]*phi + phi_mp2[4]*phi*phi + phi_mp3[4]*phi*phi*phi + phi_mp4[4];
	      phisigma = phi_sp0[4]/pt + phi_sp1[4]*phi + phi_sp2[4]*phi*phi + phi_sp3[4]*phi*phi*phi + phi_sp4[4];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  else       //in the East
	    {
	      phimean = phi_mp0[5]/pt + phi_mp1[5]*phi + phi_mp2[5]*phi*phi + phi_mp3[5]*phi*phi*phi + phi_mp4[5];
	      phisigma = phi_sp0[5]/pt + phi_sp1[5]*phi + phi_sp2[5]*phi*phi + phi_sp3[5]*phi*phi*phi + phi_sp4[5];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	}
      else    //in the South
	{
	  if(phi<1.5)//in the West
	    {
	      phimean = phi_mp0[6]/pt + phi_mp1[6]*phi + phi_mp2[6]*phi*phi + phi_mp3[6]*phi*phi*phi + phi_mp4[6];
	      phisigma = phi_sp0[6]/pt + phi_sp1[6]*phi + phi_sp2[6]*phi*phi + phi_sp3[6]*phi*phi*phi + phi_sp4[6];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	  else       //in the East
	    {
	      phimean = phi_mp0[7]/pt + phi_mp1[7]*phi + phi_mp2[7]*phi*phi + phi_mp3[7]*phi*phi*phi + phi_mp4[7];
	      phisigma = phi_sp0[7]/pt + phi_sp1[7]*phi + phi_sp2[7]*phi*phi + phi_sp3[7]*phi*phi*phi + phi_sp4[7];
	      sdphi = (dphi-phimean)/phisigma;
	    }
	}
    }
  return sdphi;

}


float Run8PC3MatchRecal::calculate_pc3sdz_dAu(const float alpha, const float zed, const float phi, const float pt, const float dz)
{

  if(dz<=-999||pt<0){return -9999;}

  //The constants used to calculate the mean and sigma
 float mp0[8]={-0.280832,-0.218428,0.238238,0.107944,-0.131437,-0.284557,0.12967,0.248366};
 float mp1[8]={0.00442907,-0.00196563,0.00164693,0.00409277,0.00836442,-0.00585038,0.00251098,-0.00107678};
 float mp2[8]={-0.000317136,0.000112927,0.000384116,6.05E-05,-0.000356759,0.000157498,0.000278677,-3.28E-06};
 float mp3[8]={2.75E-06,-5.47E-07,3.95E-06,2.08E-07,3.11E-06,-9.03E-07,2.67E-06,-1.92E-07};
 float mp4[8]={0.259364,-0.593553,-0.239874,-0.871735,0.149264,-0.485105,-0.124938,-0.983201};

 float sp0[8]={0.327527,0.304063,0.354222,0.301687,0.341321,0.310641,0.329996,0.305715};
 float sp1[8]={0.00121941,0.00122216,-0.000296322,0.000379685,0.000740521,0.000649298,0.000102154,-0.000448991};
 float sp2[8]={1.13899,1.15146,1.10313,1.21735,1.07625,1.10292,1.10882,1.117};

 float zmean;
 float zsigma;

 float sdz;
 if(alpha<0)//positive particles  pre magnet flip upto run 250484   
   {
     if(zed>0)//in the North
       {
	 if(phi<1.5)//in the West
	   {
	     zmean = mp0[0]/pt + mp1[0]*zed + mp2[0]*zed*zed + mp3[0]*zed*zed*zed + mp4[0];
	     zsigma = sp0[0]/pt + sp1[0]*zed + sp2[0];
	     sdz = (dz-zmean)/zsigma;
	   }
	 else       //in the East
	   {
	     zmean = mp0[1]/pt + mp1[1]*zed + mp2[1]*zed*zed + mp3[1]*zed*zed*zed + mp4[1];
	     zsigma = sp0[1]/pt + sp1[1]*zed + sp2[1];
	     sdz = (dz-zmean)/zsigma; 
	   }
	      
       }
     else    //in the South
       {
	 if(phi<1.5)//in the West
	   {
	     zmean = mp0[2]/pt + mp1[2]*zed + mp2[2]*zed*zed + mp3[2]*zed*zed*zed + mp4[2];
	     zsigma = sp0[2]/pt + sp1[2]*zed + sp2[2];
	     sdz = (dz-zmean)/zsigma; 
	   }
	 else       //in the East
	   {
	     zmean = mp0[3]/pt + mp1[3]*zed + mp2[3]*zed*zed + mp3[3]*zed*zed*zed + mp4[3];
	     zsigma = sp0[3]/pt + sp1[3]*zed + sp2[3];
	     sdz = (dz-zmean)/zsigma; 
	   }
       }
   }
 else        //negative particles  pre magnet flip upto run 250484
   {
     if(zed>0)//in the North
       {
	 if(phi<1.5)//in the West
	   {
	     zmean = mp0[4]/pt + mp1[4]*zed + mp2[4]*zed*zed + mp3[4]*zed*zed*zed + mp4[4];
	     zsigma = sp0[4]/pt + sp1[4]*zed + sp2[4];
	     sdz = (dz-zmean)/zsigma; 
	   }
	 else       //in the East
	   {
	     zmean = mp0[5]/pt + mp1[5]*zed + mp2[5]*zed*zed + mp3[5]*zed*zed*zed + mp4[5];
	     zsigma = sp0[5]/pt + sp1[5]*zed + sp2[5];
	     sdz = (dz-zmean)/zsigma; 
	   }
       }
     else    //in the South
       {
	 if(phi<1.5)//in the West
	   {
	     zmean = mp0[6]/pt + mp1[6]*zed + mp2[6]*zed*zed + mp3[6]*zed*zed*zed + mp4[6];
	     zsigma = sp0[6]/pt + sp1[6]*zed + sp2[6];
	     sdz = (dz-zmean)/zsigma; 
	   }
	 else       //in the East
	   {
	     zmean = mp0[7]/pt + mp1[7]*zed + mp2[7]*zed*zed + mp3[7]*zed*zed*zed + mp4[7];
	     zsigma = sp0[7]/pt + sp1[7]*zed + sp2[7];
	     sdz = (dz-zmean)/zsigma; 
	   }
       }
   }

 return sdz;
 
}


float Run8PC3MatchRecal::calculate_pc3sdz_pp(const short charge, const float zed, const float phi, const float pt, const float dz)
{

  if(dz<=-999||pt<0){return -9999;}

  //The constants used to calculate the mean and sigma
 float mp0[8]={-0.118269,-0.276576,0.097878,0.207393,-0.297319,-0.202669,0.187848,0.0860964};
 float mp1[8]={0.0133789,-0.00657007,0.00897384,0.00315154,0.00507137,-0.00548064,0.00729165,0.00216807};
 float mp2[8]={-0.000463553,0.000133612,0.000447383,0.00012306,-0.000320612,0.000169533,0.000549196,-1.14E-05};
 float mp3[8]={3.88E-06,-4.99E-07,4.00E-06,8.18E-07,2.72E-06,-8.16E-07,5.16E-06,-5.82E-07};
 float mp4[8]={-0.0863641,-0.493546,-0.200308,-0.927097,0.0649243,-0.593257,-0.313627,-0.892512};

 float sp0[8]={0.298971,0.324044,0.357088,0.314834,0.357915,0.340632,0.394535,0.348548};
 float sp1[8]={0.000838377,0.000147365,-0.000571047,-0.000881162,-0.000122697,0.000191101,-0.000622198,-0.000197261};
 float sp2[8]={1.23659,1.2298,1.18127,1.22261,1.20931,1.20601,1.13131,1.19614};

 float finetune[2]={0.0785569, 1.15822};

 float zmean;
 float zsigma;

 float sdz;

 if(charge>0)//positive particles
   {
     if(zed>0)//in the North
       {
	 if(phi<1.5)//in the West
	   {
	     zmean = mp0[0]/pt + mp1[0]*zed + mp2[0]*zed*zed + mp3[0]*zed*zed*zed + mp4[0];
	     zsigma = sp0[0]/pt + sp1[0]*zed + sp2[0];
	     sdz = (dz-zmean)/zsigma;
	   }
	 else       //in the East
	   {
	     zmean = mp0[1]/pt + mp1[1]*zed + mp2[1]*zed*zed + mp3[1]*zed*zed*zed + mp4[1];
	     zsigma = sp0[1]/pt + sp1[1]*zed + sp2[1];
	     sdz = (dz-zmean)/zsigma; 
	   }
	      
       }
     else    //in the South
       {
	 if(phi<1.5)//in the West
	   {
	     zmean = mp0[2]/pt + mp1[2]*zed + mp2[2]*zed*zed + mp3[2]*zed*zed*zed + mp4[2];
	     zsigma = sp0[2]/pt + sp1[2]*zed + sp2[2];
	     sdz = (dz-zmean)/zsigma; 
	   }
	 else       //in the East
	   {
	     zmean = mp0[3]/pt + mp1[3]*zed + mp2[3]*zed*zed + mp3[3]*zed*zed*zed + mp4[3];
	     zsigma = sp0[3]/pt + sp1[3]*zed + sp2[3];
	     sdz = (dz-zmean)/zsigma; 
	   }
       }
   }
 else        //negative particles
   {
     if(zed>0)//in the North
       {
	 if(phi<1.5)//in the West
	   {
	     zmean = mp0[4]/pt + mp1[4]*zed + mp2[4]*zed*zed + mp3[4]*zed*zed*zed + mp4[4];
	     zsigma = sp0[4]/pt + sp1[4]*zed + sp2[4];
	     sdz = (dz-zmean)/zsigma; 
	   }
	 else       //in the East
	   {
	     zmean = mp0[5]/pt + mp1[5]*zed + mp2[5]*zed*zed + mp3[5]*zed*zed*zed + mp4[5];
	     zsigma = sp0[5]/pt + sp1[5]*zed + sp2[5];
	     sdz = (dz-zmean)/zsigma; 
	   }
       }
     else    //in the South
       {
	 if(phi<1.5)//in the West
	   {
	     zmean = mp0[6]/pt + mp1[6]*zed + mp2[6]*zed*zed + mp3[6]*zed*zed*zed + mp4[6];
	     zsigma = sp0[6]/pt + sp1[6]*zed + sp2[6];
	     sdz = (dz-zmean)/zsigma; 
	   }
	 else       //in the East
	   {
	     zmean = mp0[7]/pt + mp1[7]*zed + mp2[7]*zed*zed + mp3[7]*zed*zed*zed + mp4[7];
	     zsigma = sp0[7]/pt + sp1[7]*zed + sp2[7];
	     sdz = (dz-zmean)/zsigma; 
	   }
       }
   }

 //fine tune the sigmalized variable apparently the same across all categories
 sdz = sdz/(finetune[0]/pt + finetune[1]);

 return sdz;
 
}

