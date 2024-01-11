// $Id: mRxnpCalXang.cxx,v 1.16 2017/07/12 01:36:23 phnxbld Exp $
/*!
  \file    mRxnpCalXang.cxx
  \brief   unpack raw data hits. Generates interface class TRxnpRawScint_v1.
  \author  Chun Zhang
  \version $Revision: 1.16 $
  \date    $Date: 2017/07/12 01:36:23 $
*/

// std include
//
#include <iostream>
#include <string>
#include <sstream>
#include <fstream>

// PHENIX include
//
#include <Event.h>
#include <PHCompositeNode.h>
#include <PHException.h>

// MUTOO include
//
#include <TMutNode.h>
#include <MUTOO.h>
#include <PHTimer.h>

//Rxnp include
//
#include <TRxnpRawXangMap.h>
#include <TRxnpScintMap.h>
#include <RXNP.h>

#include "mRxnpCalXang.h"

// constructor
//
mRxnpCalXang::mRxnpCalXang():
  _timer(PHTimeServer::get()->insert_new("mRxnpCalXang"))
{
  MUTOO::TRACE("initializing module mRxnpCalXang");
  //  cout << "mRxnpCalXang::mRxnpCalXang constructor" << endl; 
}

// destructor
//
mRxnpCalXang::~mRxnpCalXang()
{
  //  cout << "mRxnpCalXang::~mRxnpCalXang destructor" << endl; 
  return;
}

void mRxnpCalXang::init_tables()
{
  //  cout << "mRxnpCalXang::init_tables" << endl; 
  clearArray();
  resetHisto();
  if (_mod_par->get_iteration(2) > 0 ) readOffst();
  if (_mod_par->get_iteration(2) > 1 ) readTable(); 
}


void mRxnpCalXang::dump_out()
{
  cout << "mRxnpCalXang::dump_out" << endl; 
  if (_mod_par->get_iteration(1) == 0 ) writeOffst();
  if (_mod_par->get_iteration(1) == 1 ) writeTable();
  //  closeHisto();
}

// Event method
//
PHBoolean mRxnpCalXang::event(PHCompositeNode* top_node)
{
  // start the timer
  //
  _timer.get()->restart();

  // event loop put into a try-catch block
  //
  // reset the interface ptrs
  // call unpack method to fill the TRxnpScintRawMap
  // call calibrate method to fill the TRxnpScintMap
  //
  try 
    {
      // reset IOC pointers
      //
      set_interface_ptrs(top_node);

      // 
      //
      if(_mod_par->state_change())
	{
	  dump_out();
	  init_tables();
	  _mod_par->sychroniz_iter_state();
	}

      // clear TRxnpRawScintMap and TrxnpScintMap
      //
      if(_mod_par->get_iteration()==0) 
	{
	  _xang_map->clear();
	  // call cal_raw_xang method
	  //
	  cal_raw_xang();
	}
      cal_global();
      int imul = (int)(nmul*getGlobal(2)/170000.0);
      int izps = (int)(nzps*(getGlobal(6)+100.0)/200.0);
      if (imul<0)      imul=0;
      if (imul>nmul-1) imul=nmul-1;
      if (izps<0)      izps=0;
      if (izps>nzps-1) izps=nzps-1;
      flatten_xang(imul,izps);
    } 
  catch ( std::exception& e) 
    {
      MUTOO::TRACE(e.what());
      return False;
    }
      
  // stop the timer
  //
  _timer.get()->stop();
    
  // if verbose dump the information
  //
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) 
    {
      _xang_map->print();
    }
  if(_mod_par->get_verbosity() >= MUTOO::SOME)
    _timer.get()->print();
  
  return True;
}

// set IOC pointers
//
void mRxnpCalXang::set_interface_ptrs(PHCompositeNode* top_node)
{
  
  // module runtime parameters
  _mod_par = TMutNode<mRxnpCalXangPar>::find_node(top_node, "mRxnpCalXangPar");
  
  // raw scint map pointer
  _xang_map = TMutNode<TRxnpRawXangMap>::find_node(top_node, "TRxnpRawXangMap");

  // scint map pointer
  _scint_map = TMutNode<TRxnpScintMap>::find_node(top_node, "TRxnpScintMap");
  
}

// calibrate raw hits
//
void mRxnpCalXang::cal_raw_xang()
{
  // need calibration data base infor.
  //
  // Loop through arm and ring
  //
  for(unsigned short iarm = 0; iarm < RXNP::NARM; iarm++)
    {
      for(unsigned short iring = 0; iring < RXNP::NRING; iring++)
	{
	  
	  TRxnpScintMap::const_iterator scint_iter = _scint_map->get(iarm,
								     iring);
	  double x1_high = 0;
	  double x2_high = 0;
	  double y1_high = 0;
	  double y2_high = 0;
	  double x1_low = 0;
	  double x2_low = 0;
	  double y1_low = 0;
	  double y2_low = 0;
	  
	  // calculate the coordinates first
	  //
	  while(TRxnpScintMap::const_pointer scint_ptr = scint_iter.next())
	    {
	      x1_high += scint_ptr->get()->get_high_e()*cos(scint_ptr->get()->get_phi());
	      y1_high += scint_ptr->get()->get_high_e()*sin(scint_ptr->get()->get_phi());
	      x2_high += scint_ptr->get()->get_high_e()*cos(2*scint_ptr->get()->get_phi());
	      y2_high += scint_ptr->get()->get_high_e()*sin(2*scint_ptr->get()->get_phi());

	      x1_low += scint_ptr->get()->get_low_e()*cos(scint_ptr->get()->get_phi());
	      y1_low += scint_ptr->get()->get_low_e()*sin(scint_ptr->get()->get_phi());
	      x2_low += scint_ptr->get()->get_low_e()*cos(2*scint_ptr->get()->get_phi());
	      y2_low += scint_ptr->get()->get_low_e()*sin(2*scint_ptr->get()->get_phi());

	    }
	  // inset new raw reaction plane angle object into map
	  //
	  TRxnpRawXangMap::iterator xang_iter = _xang_map->insert_new(iarm,
								      iring);
	  // fill the object
	  //
	  // dipole fields
	  //
	  xang_iter->get()->set_X1_high(x1_high);
	  xang_iter->get()->set_Y1_high(y1_high);
	  // reaction plane dipole angle, high gain
	  //
	  double phi1_high = atan2(y1_high,x1_high);
	  // make the angle from [0, 2*M_PI]
	  //
	  if(phi1_high < 0)
	    phi1_high +=2*M_PI;
	  xang_iter->get()->set_Xangv1_high(phi1_high);

	  xang_iter->get()->set_X1_low(x1_low);
	  xang_iter->get()->set_Y1_low(y1_low);
	  // reaction plane dipole angle, low gain
	  //
	  double phi1_low = atan2(y1_low,x1_low);
	  // make the angle from [0, 2*M_PI]
	  //
	  if(phi1_low < 0)
	    phi1_low +=2*M_PI;
	  xang_iter->get()->set_Xangv1_low(phi1_low);

	  // quadropole fields
	  //
	  xang_iter->get()->set_X2_high(x2_high);
	  xang_iter->get()->set_Y2_high(y2_high);
	  // reaction plane quadropole angle, high gain
	  //
	  double phi2_high = atan2(y2_high,x2_high)/2.0;
	  // make the angle from [0, M_PI]
	  //
	  if(phi2_high > - M_PI/2.0 && phi2_high < 0)
	    phi2_high += M_PI;
	  xang_iter->get()->set_Xangv2_high(phi2_high);
	  xang_iter->get()->set_X2_low(x2_low);
	  xang_iter->get()->set_Y2_low(y2_low);
	  // reaction plane quadropole angle, low gain
	  //
	  double phi2_low = atan2(y2_low,x2_low)/2.0;
	  // make the angle from [0, M_PI]
	  //
	  if(phi2_low > - M_PI/2.0 && phi2_low < 0)
	    phi2_low += M_PI;
	  xang_iter->get()->set_Xangv2_low(phi2_low);
	}
    }
}

// get global variables: charge/time/zvertex
//
void mRxnpCalXang::cal_global()
{
  double nseg[2];
  double charge[2]; 
  double time[2]; 
//double offT=85.0;   // offset timing difference
  double offT=0.0;   // offset timing difference
  double facT=1.0;   // position/time difference ratio
  double wg,tm;
  int count;
  int ii[2][2];
  for(unsigned short iarm = 0; iarm < RXNP::NARM; iarm++) {
    charge[iarm]=0;
    time[iarm]=0;
    count=0;
    for(unsigned short iring = 0; iring < RXNP::NRING; iring++) {
      ii[iarm][iring]=0;
      TRxnpScintMap::const_iterator scint_iter = _scint_map->get(iarm,iring);
      while(TRxnpScintMap::const_pointer scint_ptr = scint_iter.next()) {
        wg=scint_ptr->get()->get_low_e();
        tm=scint_ptr->get()->get_tof();
        if (tm>500 && tm<1800) {
          charge[iarm]+=wg;
          time[iarm]+=tm;
          count++;
        }
        ii[iarm][iring]++;
      }
    }
    if (count>0) time[iarm]/=(double)count;
    else         time[iarm]=-9999.9;
    nseg[iarm]=(double)count;
  }
  bool b1 = (ii[0][0]==12 && ii[0][1]==12 && ii[1][0]==12 && ii[1][1]==12);
  bool b2 = (ii[0][0]==0  && ii[0][1]==0  && ii[1][0]==0  && ii[1][1]==0 );
  if (!(b1 || b2)) {
    cout << "mRxnpCalXang::cal_global, error in segment# " <<
    ii[0][0] << " " << ii[0][1] << " " << ii[1][0] << " " << ii[1][1] << endl;
  }
  eventCounter++;
  if (b1) {
    glb[0]=charge[0];
    glb[1]=charge[1];
    glb[2]=charge[0]+charge[1];
    goodCounter++;
    glb[7]=nseg[0];
    glb[8]=nseg[1];
    glb[9]=nseg[0]+nseg[1];
  } else {
    glb[0]=-9999.9;
    glb[1]=-9999.9;
    glb[2]=-9999.9;
    glb[7]=-9999.9;
    glb[8]=-9999.9;
    glb[9]=-9999.9;
  }
//   if (eventCounter%1000==0) cout << "mRxnpCalXang::cal_global, good / total event : " 
//                                  << goodCounter << " / " << eventCounter << endl;
  if (time[0]>-9000.0) glb[3]=time[0];
  else             glb[3]=-9999.9;
  if (time[1]>-9000.0) glb[4]=time[1];
  else             glb[4]=-9999.9;
  if (time[0]>-9000.0 && time[1]>-9000.0) {
    glb[5]=(time[0]+time[1])*0.5;
    glb[6]=(time[0]-time[1]+offT)*facT;
  } else {
    glb[5]=-9999.9;
    glb[6]=-9999.9;
  }
}

double mRxnpCalXang::getGlobal(int opt)
{
  // opt = 0 for south charge
  // opt = 1 for north charge
  // opt = 2 for south+north charge
  // opt = 3 for south time
  // opt = 4 for north time
  // opt = 5 for south,north average time
  // opt = 6 for z-vertex
  // opt = 7 for south nsegment
  // opt = 8 for north nsegment
  // opt = 9 for south+north nsegment
  if (opt>-1 && opt<10) {
    return glb[opt];
  } else {
    cout << "out of range, mRxnpCalXang::getGlobal [0-9] " << opt << endl;
    return -9999.9;
  }
}

// flatten xang 
//
void mRxnpCalXang::flatten_xang(int imul, int izps)
{
  // need calibration data base infor.
  //
  // Loop through arm and ring
  //
  double tm,wt,ph,hm,pm;
  for(unsigned short iarm = 0; iarm < RXNP::NARM; iarm++) {
    for(unsigned short iring = 0; iring < RXNP::NRING; iring++) {
      unsigned short id=iarm*3+iring;           // si,so,sc,ni,no,nc,ci,co,cc
      for(unsigned short ih = 0; ih < 4; ih++) {  // harmonics 1-4
        for(unsigned short ig = 0; ig < 2; ig++) {  // high-low gain
          for(unsigned short ii = 0; ii < 3; ii++) {  // x,y,w
            sum[id][ih][ig][ii]=0;
          }
        }
      }
      TRxnpScintMap::const_iterator scint_iter = _scint_map->get(iarm,iring);
      while(TRxnpScintMap::const_pointer scint_ptr = scint_iter.next()) {
        for(unsigned short ig = 0; ig < 2; ig++) {  // high-low gain
          if (ig==0) wt=scint_ptr->get()->get_high_e();
          else       wt=scint_ptr->get()->get_low_e();
          ph=scint_ptr->get()->get_phi();
          tm=scint_ptr->get()->get_tof();
          if (tm<500 || tm>1800 || wt<0) wt=0;
          for(unsigned short ih = 0; ih < 4; ih++) {  // harmonics 1-4
            hm=1.0+(double)ih;
            sum[id][ih][ig][0] += wt*cos(hm*ph);
            sum[id][ih][ig][1] += wt*sin(hm*ph);
            sum[id][ih][ig][2] += wt;
          }
        }
      }
    }
  }
  for(unsigned short ih = 0; ih < 4; ih++) {  // harmonics 1-4
    for(unsigned short ig = 0; ig < 2; ig++) {  // high-low gain
      for(unsigned short ii = 0; ii < 3; ii++) {  // x,y,w
        if (ii<2 && ih%2==0) pm=-1.0;
        else                 pm=1.0;
        sum[2][ih][ig][ii]=sum[0][ih][ig][ii]+sum[1][ih][ig][ii];      // sc = si  +  so
        sum[5][ih][ig][ii]=sum[3][ih][ig][ii]+sum[4][ih][ig][ii];      // nc = ni  +  no
        sum[6][ih][ig][ii]=sum[0][ih][ig][ii]+pm*sum[3][ih][ig][ii];   // ci = si +/- ni
        sum[7][ih][ig][ii]=sum[1][ih][ig][ii]+pm*sum[4][ih][ig][ii];   // co = so +/- no
        sum[8][ih][ig][ii]=sum[2][ih][ig][ii]+pm*sum[5][ih][ig][ii];   // cc = sc +/- nc
      }
      for(unsigned short id = 0; id < 9; id++) {  // si,so,sc,ni,no,nc,ci,co,cc
        if (sum[id][ih][ig][2]>0) {
          sum[id][ih][ig][0]/=sum[id][ih][ig][2];
          sum[id][ih][ig][1]/=sum[id][ih][ig][2];
        }
        ang[id][ih][ig]=-9999.9;
      }
    }
  }
  if (imul>-1 && imul<nmul && izps>-1 && izps<nzps) {
  double sumx,sumy,avex,avey,sigx,sigy,xx,yy;
  double psi,dp,cc,ss,aa,bb;
  for(unsigned short id = 0; id < 9; id++) {  // si,so,sc,ni,no,nc,ci,co,cc
    for(unsigned short ih = 0; ih < 4; ih++) {  // harmonics 1-4
      for(unsigned short ig = 0; ig < 2; ig++) {  // high-low gain
        unsigned short idet = 4*2*id + 2*ih + ig;   // 0 - 72 (=9*4*2)
        if (sum[id][ih][ig][2]>0) {
          sumx=sum[id][ih][ig][0];
          sumy=sum[id][ih][ig][1];
          Hsum[idet][imul][izps]->Fill(1,sumx);
          Hsum[idet][imul][izps]->Fill(2,sumy);
          if (_mod_par->get_iteration()>0) {
            avex=ofsx[idet][imul][izps];
            avey=ofsy[idet][imul][izps];
            sigx=widx[idet][imul][izps];
            sigy=widy[idet][imul][izps];
            xx=(sumx-avex)/sigx;
            yy=(sumy-avey)/sigy;
            Hsum[idet][imul][izps]->Fill(3,xx);
            Hsum[idet][imul][izps]->Fill(4,yy);
            Hsmx[idet][imul][izps]->Fill(xx);
            Hsmy[idet][imul][izps]->Fill(yy);
            psi=atan2(yy,xx);
          } else { 
            psi=atan2(sumy,sumx);
          }
          Hphi[idet][imul][izps]->Fill(psi);
          dp=0;
          for (int ia=0; ia<nord; ia++) {
            hm=1.0+(double)ia;
            cc=cos(hm*psi);
            ss=sin(hm*psi);
            Hprc[idet][imul][izps]->Fill(hm,cc);
            Hprs[idet][imul][izps]->Fill(hm,ss);
            aa=aaa[idet][imul][izps][ia];   // mean cos
            bb=bbb[idet][imul][izps][ia];   // mean sin
            dp+=(aa*ss-bb*cc)*2.0/hm;
          }
          dp+=psi;
          if (_mod_par->get_iteration()>1) {
            psi=atan2(sin(dp),cos(dp));
            Hphj[idet][imul][izps]->Fill(psi);
          }
        } else {
          psi=-9999.9;
        }
        ang[id][ih][ig]=psi;
      }
    }
  }
  }
}

double mRxnpCalXang::getRxnPlane(int id, int ih, int ig)
{
  // id = [0,8] for si,so,sc,ni,no,nc,ci,co,cc   where s[south],n[north],c[combined]
  //                                                   i[inner],o[outer],c[combined]
  // ih = [0,3] for harmonics 1-4
  // ig = [0,1] for high-low gain
  if (id>-1 && id<9 && ih>-1 && ih<4 && ig>-1 && ig<2) {
    double val=ang[id][ih][ig];
    if (val<-9000.0) return -9999.9;
    else             return val/(1.0+(double)ih);
  } else {
    cout << "out of range, mRxnpCalXang::getRxnPlane [0-8,0-3,0-1] "
         << id << " " << ih << " " << ig << endl;
    return -9999.9;
  }
}

double mRxnpCalXang::getXYWsum(int id, int ih, int ig, int ii)
{
  // id = [0,8] for si,so,sc,ni,no,nc,ci,co,cc   where s[south],n[north],c[combined]
  //                                                   i[inner],o[outer],c[combined]
  // ih = [0,3] for harmonics 1-4
  // ig = [0,1] for high-low gain
  // ih = [0,2] for X_Sum, Y_Sum and Weight_Sum
  if (id>-1 && id<9 && ih>-1 && ih<4 && ig>-1 && ig<2 && ii>-1 && ii<3) {
    return sum[id][ih][ig][ii];
  } else {
    cout << "out of range, mRxnpCalXang::getXYWsum [0-8,0-3,0-1,0-2] "
         << id << " " << ih << " " << ig << " " << ii << endl;
    return -9999.9;
  }
}

void mRxnpCalXang::readTable()
{
  std::ifstream ifs;
  char name[40];

  sprintf(name,"RxnpCalXang-%06d.table",nrun);
  cout << "mRxnpCalXang::readTable reading file :" << name << endl; 
  ifs.open(name);
  for (int id=0; id<ndet; id++) {
    for (int im=0; im<nmul; im++) {
      for (int iz=0; iz<nzps; iz++) {
        for (int ia=0; ia<nord; ia++) {
          ifs >> aaa[id][im][iz][ia] >> bbb[id][im][iz][ia];
        }
      }
    }
  }
  ifs.close();
}

void mRxnpCalXang::writeTable()
{
  std::ofstream ofs;
  char name[40];
  sprintf(name,"RxnpCalXang-%06d.table",nrun);
  cout << "mRxnpCalXang::writeTable writing file :" << name << endl; 
  ofs.open(name);
  for (int id=0; id<ndet; id++) {
    for (int im=0; im<nmul; im++) {
      for (int iz=0; iz<nzps; iz++) {
        for (int ia=0; ia<nord; ia++) {
          aaa[id][im][iz][ia]=Hprc[id][im][iz]->GetBinContent(ia+1);
          bbb[id][im][iz][ia]=Hprs[id][im][iz]->GetBinContent(ia+1);
          ofs << aaa[id][im][iz][ia] << " "
              << bbb[id][im][iz][ia] << endl;
        }
      }
    }
  }
  ofs.close();
}

void mRxnpCalXang::readOffst()
{
  std::ifstream ifs;
  char name[40];
  sprintf(name,"RxnpCalXang-%06d.offst",nrun);
  cout << "mRxnpCalXang::readOffst reading file :" << name << endl; 
  ifs.open(name);
  for (int id=0; id<ndet; id++) {
    for (int im=0; im<nmul; im++) {
      for (int iz=0; iz<nzps; iz++) {
        ifs >> ofsx[id][im][iz] >> ofsy[id][im][iz]
            >> widx[id][im][iz] >> widy[id][im][iz];
        if (widx[id][im][iz]==0) widx[id][im][iz]=1;
        if (widy[id][im][iz]==0) widy[id][im][iz]=1;
      }
    }
  }
  ifs.close();
}

void mRxnpCalXang::writeOffst()
{
  std::ofstream ofs;
  char name[40];
  sprintf(name,"RxnpCalXang-%06d.offst",nrun);
  cout << "mRxnpCalXang::writeOffst writing file :" << name << endl; 
  ofs.open(name);
  for (int id=0; id<ndet; id++) {
    for (int im=0; im<nmul; im++) {
      for (int iz=0; iz<nzps; iz++) {
        ofsx[id][im][iz]=Hsum[id][im][iz]->GetBinContent(1);
        ofsy[id][im][iz]=Hsum[id][im][iz]->GetBinContent(2);
        widx[id][im][iz]=Hsum[id][im][iz]->GetBinError(1);
        widy[id][im][iz]=Hsum[id][im][iz]->GetBinError(2);
        ofs << ofsx[id][im][iz] << " " << ofsy[id][im][iz] << " "
            << widx[id][im][iz] << " " << widy[id][im][iz] << endl;
      }
    }
  }
  ofs.close();
}

void mRxnpCalXang::clearArray()
{
  cout << "mRxnpCalXang::clearArray" << endl;
  for (int id=0; id<ndet; id++) {
    for (int im=0; im<nmul; im++) {
      for (int iz=0; iz<nzps; iz++) {
        ofsx[id][im][iz]=0;
        ofsy[id][im][iz]=0;
        widx[id][im][iz]=1;
        widy[id][im][iz]=1;
        for (int ia=0; ia<nord; ia++) {
          aaa[id][im][iz][ia]=0;
          bbb[id][im][iz][ia]=0;
        }
      }
    }
  }
  goodCounter=0;
  eventCounter=0;
}

void mRxnpCalXang::bookHisto()
{
  char name[40];
  sprintf(name,"RxnpCalXang-%06d.root",nrun);
  cout << "mRxnpCalXang::bookHisto opening root file :" << name << endl; 
  hfile = new TFile(name,"recreate");
  hfile->cd();
  float pi=acos(-1.0);
  for (int id=0; id<ndet; id++) {
    for (int im=0; im<nmul; im++) { 
      for (int iz=0; iz<nzps; iz++) { 
        sprintf(name,"phi_%d_%d_%d",id,im,iz);
        Hphi[id][im][iz] = new TH1F(name,name, 50,-pi,pi);
        sprintf(name,"phj_%d_%d_%d",id,im,iz);
        Hphj[id][im][iz] = new TH1F(name,name, 50,-pi,pi);
        sprintf(name,"smx_%d_%d_%d",id,im,iz);
        Hsmx[id][im][iz] = new TH1F(name,name, 50,-5,5);
        sprintf(name,"smy_%d_%d_%d",id,im,iz);
        Hsmy[id][im][iz] = new TH1F(name,name, 50,-5,5);
        sprintf(name,"prc_%d_%d_%d",id,im,iz);
        Hprc[id][im][iz] = new TProfile(name,name,nord,0.5,nord+0.5,-1.1,1.1);
        sprintf(name,"prs_%d_%d_%d",id,im,iz);
        Hprs[id][im][iz] = new TProfile(name,name,nord,0.5,nord+0.5,-1.1,1.1);
        sprintf(name,"sum_%d_%d_%d",id,im,iz);
        Hsum[id][im][iz] = new TProfile(name,name,4,0.5,4.5,-1000.0,1000.0,"S");
      }
    }
  }
}

void mRxnpCalXang::resetHisto()
{
  cout << "mRxnpCalXang::clearHisto" << endl;
  hfile->cd();
  for (int id=0; id<ndet; id++) {
    for (int im=0; im<nmul; im++) { 
      for (int iz=0; iz<nzps; iz++) { 
        Hphi[id][im][iz]->Reset();
        Hphj[id][im][iz]->Reset();
        Hsmx[id][im][iz]->Reset();
        Hsmy[id][im][iz]->Reset();
        Hprc[id][im][iz]->Reset();
        Hprs[id][im][iz]->Reset();
        Hsum[id][im][iz]->Reset();
      }
    }
  }
}

void mRxnpCalXang::closeHisto()
{
  char name[40];
  if(hfile)
    {
      // extra check if the hfile is actually booked.
      //
      sprintf(name,"RxnpCalXang-%06d.root",nrun);
      cout << "mRxnpCalXang::closeHisto closing root file :" << name << endl; 
      hfile->Write();
      hfile->Close();
    }
  cout << "mRxnpCalXang::closeHisto, good / total event : "
       << goodCounter << " / " << eventCounter << endl;
}
