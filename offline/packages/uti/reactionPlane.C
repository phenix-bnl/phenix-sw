#include "reactionPlane.hh"

#include <cstdlib>
#include <iostream>

using namespace std;

reactionPlane::reactionPlane()
{
  cout << "info. reactionPlane::reactionPlane, created" << endl;
  readSetup();
  readGeoCal();
  if (flag>-2) readCorre();
  if (flag>-1) readOffst();
  if (flag==1) readTable();
  makeHisto();
}

reactionPlane::~reactionPlane()
{
  cout << "info. reactionPlane::~reactionPlane, deleted" << endl;
}

void reactionPlane::closePlane()
{
  cout << "info. reactionPlane::closePlane" << endl;
  if (flag==-2) writeCorre();
  if (flag==-1) writeOffst();
  if (flag== 0) writeTable();
  saveHisto();
}

void reactionPlane::readSetup()
{
  cout << "info. reactionPlane::readSetup" << endl;
  ifstream setFile;
  setFile.open("reactionPlane.setup");
  setFile >> nrun >> ndet >> nhar >> nmul >> nzps >> nord >> flag;
  setFile.close();
  cout << "info. reactionPlane::readSetup, nrun = " << nrun << endl;
  cout << "info. reactionPlane::readSetup, ndet = " << ndet << endl;
  cout << "info. reactionPlane::readSetup, nhar = " << nhar << endl;
  cout << "info. reactionPlane::readSetup, nmul = " << nmul << endl;
  cout << "info. reactionPlane::readSetup, nzps = " << nzps << endl;
  cout << "info. reactionPlane::readSetup, nord = " << nord << endl;
  cout << "info. reactionPlane::readSetup, flag = " << flag << endl;
  if (ndet<0 || ndet>nDet || nhar<0 || nhar>nHar || nmul<0 || nmul>nMul ||
      nzps<0 || nzps>nZps || nord<0 || nord>nOrd || flag<-2|| flag>1) {
    cout << "error reactionPlane::readSetup, exceed array" << endl;
    exit(-1);
  }
}

void reactionPlane::readGeoCal()
{
  cout << "info. reactionPlane::readGeoCal" << endl;
  ifstream calFile,geoFile;
  calFile.open("bbccal.dat");
  geoFile.open("bbcgeo.dat");
  int itmp,ioff,nrad[14];
  float rad;
  for (int i=0; i<14; i++) nrad[i]=0;
  for (int ipmt=0; ipmt<128; ipmt++) {
    calFile >> pede[ipmt] >> oflw[ipmt] >> slw1[ipmt] >> slw2[ipmt]
            >> gain[ipmt] >> tgai[ipmt] >> qgai[ipmt];
    geoFile >> itmp >> xpos[ipmt] >> ypos[ipmt] >> zpos[ipmt];
/*
    cout << itmp << " "
         << pede[ipmt] << " " << oflw[ipmt] << " "
         << slw1[ipmt] << " " << slw2[ipmt] << " "
         << gain[ipmt] << " " << tgai[ipmt] << " " << qgai[ipmt] << " "
         << xpos[ipmt] << " " << ypos[ipmt] << " " << zpos[ipmt] << endl;
*/
    ioff=0; 
    if (ipmt>63) ioff=7; 
    rad=sqrt(xpos[ipmt]*xpos[ipmt]+ypos[ipmt]*ypos[ipmt]);
    if (rad<80.0)       {irad[ipmt]=0+ioff; nrad[0+ioff]++;}
    else if (rad<90.0)  {irad[ipmt]=1+ioff; nrad[1+ioff]++;}
    else if (rad<100.0) {irad[ipmt]=2+ioff; nrad[2+ioff]++;}
    else if (rad<110.0) {irad[ipmt]=3+ioff; nrad[3+ioff]++;}
    else if (rad<120.0) {irad[ipmt]=4+ioff; nrad[4+ioff]++;}
    else if (rad<126.0) {irad[ipmt]=5+ioff; nrad[5+ioff]++;}
    else                {irad[ipmt]=6+ioff; nrad[6+ioff]++;}
  }
  calFile.close();
  geoFile.close();
  cout << "info. reactionPlane::readGeoCal, ";
  for (int i=0; i<14; i++) cout << nrad[i] << " ";
  cout << endl;
}

void reactionPlane::readTable()
{
  cout << "info. reactionPlane::readTable" << endl;
  ifstream ifs;
  ifs.open("reactionPlane.table");
  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<nmul; im++) {
      for (int ih=0; ih<nhar; ih++) {
        for (int ib=0; ib<ndet; ib++) {
          for (int ia=0; ia<nord; ia++) {
            ifs >> aaa[ib][ih][im][iz][ia] >> bbb[ib][ih][im][iz][ia];
          }
        }
      }
    }
  }
  ifs.close();
}

void reactionPlane::writeTable()
{
  cout << "info. reactionPlane::writeTable" << endl;
  ofstream ofs;
  ofs.open("reactionPlane.table");
  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<nmul; im++) {
      for (int ih=0; ih<nhar; ih++) {
        for (int ib=0; ib<ndet; ib++) {
          for (int ia=0; ia<nord; ia++) {
            aaa[ib][ih][im][iz][ia]=Hprc[ib][ih][im][iz]->GetBinContent(ia+1);
            bbb[ib][ih][im][iz][ia]=Hprs[ib][ih][im][iz]->GetBinContent(ia+1);
            ofs << aaa[ib][ih][im][iz][ia] << " "
                << bbb[ib][ih][im][iz][ia] << endl;
          }
        }
      }
    }
  }
  ofs.close();
}

void reactionPlane::readCorre()
{
  cout << "info. reactionPlane::readCorre" << endl;
  ifstream ifs;
  ifs.open("reactionPlane.corre");
  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<nmul; im++) {
      for (int ip=0; ip<128; ip++) {
        ifs >> fac[im][iz][ip];
//      if (irad[ip]%7==2) fac[im][iz][ip]=0.0;
      }
    }
  }
  ifs.close();
}

void reactionPlane::writeCorre()
{
  cout << "info. reactionPlane::writeCorre" << endl;
  ofstream ofs;
  ofs.open("reactionPlane.corre");
  float norm;
  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<nmul; im++) {
      for (int ip=0; ip<128; ip++) {
        fac[im][iz][ip]=Hpmt[im][iz]->GetBinContent(ip+1);
        norm           =Hrad[im][iz]->GetBinContent(irad[ip]+1);
        if (fac[im][iz][ip]!=0) fac[im][iz][ip]=norm/fac[im][iz][ip];
        else {
          cout << "error reactionPlane::writeCorre" << endl;
          fac[im][iz][ip]=1.0;
        }
        ofs << fac[im][iz][ip] << endl;
      }
    }
  }
  ofs.close();
}

void reactionPlane::readOffst()
{
  cout << "info. reactionPlane::readOffst" << endl;
  ifstream ifs;
  ifs.open("reactionPlane.offst");
  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<nmul; im++) {
      for (int ih=0; ih<nhar; ih++) {
        for (int ib=0; ib<ndet; ib++) {
          ifs >> ofsx[ib][ih][im][iz]
              >> ofsy[ib][ih][im][iz];
        }
      }
    }
  }
  ifs.close();
}

void reactionPlane::writeOffst()
{
  cout << "info. reactionPlane::writeOffst" << endl;
  ofstream ofs;
  ofs.open("reactionPlane.offst");
  for (int iz=0; iz<nzps; iz++) {
    for (int im=0; im<nmul; im++) {
      for (int ih=0; ih<nhar; ih++) {
        for (int ib=0; ib<ndet; ib++) {
            ofsx[ib][ih][im][iz]=Hsum[ib][ih][im][iz]->GetBinContent(1);
            ofsy[ib][ih][im][iz]=Hsum[ib][ih][im][iz]->GetBinContent(2);
            ofs << ofsx[ib][ih][im][iz] << " "
                << ofsy[ib][ih][im][iz] << endl;
        }
      }
    }
  }
  ofs.close();
}

void reactionPlane::makeHisto()
{
  cout << "info. reactionPlane::makeHisto" << endl;
  hfile = new TFile("reactionPlane.root","recreate");
  hfile->cd();
  char name[20];
  float pi=acos(-1.0);
  for (int iz=0; iz<nzps; iz++) { 
    for (int im=0; im<nmul; im++) { 
      for (int ih=0; ih<nhar; ih++) { 
        for (int ib=0; ib<ndet; ib++) {
          sprintf(name,"phi%d%d%d%d",ib,ih,im,iz);
          Hphi[ib][ih][im][iz] = new TH1F(name,name, 50,-pi,pi);
          sprintf(name,"phj%d%d%d%d",ib,ih,im,iz);
          Hphj[ib][ih][im][iz] = new TH1F(name,name, 50,-pi,pi);
          sprintf(name,"prc%d%d%d%d",ib,ih,im,iz);
          Hprc[ib][ih][im][iz] = 
          new TProfile(name,name,nord,0.5,nord+0.5,-1.1,1.1);
          sprintf(name,"prs%d%d%d%d",ib,ih,im,iz);
          Hprs[ib][ih][im][iz] = 
          new TProfile(name,name,nord,0.5,nord+0.5,-1.1,1.1);
          sprintf(name,"sum%d%d%d%d",ib,ih,im,iz);
          Hsum[ib][ih][im][iz] = 
          new TProfile(name,name,2,0.5,2.5,-4000.0,4000.0);
        }
      }
      sprintf(name,"pmt%d%d",im,iz);
      Hpmt[im][iz] = new TProfile(name,name,128,-0.5,127.5,0.0,100.0);
      sprintf(name,"rad%d%d",im,iz);
      Hrad[im][iz] = new TProfile(name,name,14,-0.5,13.5,0.0,100.0);
    }
  }
  int nbin=nmul*nzps;
  for (int ih=0; ih<23; ih++) { 
    for (int iz=0; iz<nzps; iz++) { 
      sprintf(name,"cor%d%d",iz,ih);
      Hcor[ih][iz] = new TH2F(name,name, 50,-pi,pi,50,-pi,pi);
    }
    sprintf(name,"rsp%d",ih);
    Hrsp[ih] = new TH2F    (name,name, nbin,-0.5,nbin-0.5,50,-pi,pi);
    sprintf(name,"rsc%d",ih);
    Hrsc[ih] = new TProfile(name,name, nbin,-0.5,nbin-0.5,-1.1,1.1);
    sprintf(name,"rss%d",ih);
    Hrss[ih] = new TProfile(name,name, nbin,-0.5,nbin-0.5,-1.1,1.1);
  }
}

void reactionPlane::saveHisto()
{
  cout << "info. reactionPlane::saveHisto" << endl;
  hfile->Write();
  hfile->Close();
}

void reactionPlane::clrPlane()
{
//cout << "info. reactionPlane::clrPlane" << endl;
  for (int ihar=0; ihar<nhar; ihar++) {
    for (int idet=0; idet<ndet; idet++) {
      sumx[idet][ihar]=0;
      sumy[idet][ihar]=0;
      sumw[idet][ihar]=0;
    }
  }
}

void reactionPlane::filHitPlane(int idet, float phi, float wgt, float pm)
{
//cout << "info. reactionPlane::filHitPlane" << endl;
  if (idet+3<0 || idet+3>ndet) {
    cout << "error reactionPlane::filHitPlane, exceed array" << endl;
    exit(-1);
  }
  for (int ihar=0; ihar<nhar; ihar++) {
    float wg;
    if (ihar%2==0) wg=wgt*pm;
    else           wg=wgt;
    sumx[idet+3][ihar]+=wg*cos((float)(ihar+1)*phi);
    sumy[idet+3][ihar]+=wg*sin((float)(ihar+1)*phi);
    sumw[idet+3][ihar]+=wg;
  }
}

void reactionPlane::calHitPlane(int imul, int izps, int idet)
{
//cout << "info. reactionPlane::calHitPlane" << endl;
//float off[4] = {0.0, 0.03108, 0.0, 0.03149};
  for (int ihar=0; ihar<nhar; ihar++) {
    if (sumw[idet][ihar]!=0) {
      Hsum[idet][ihar][imul][izps]->Fill(1,sumx[idet][ihar]);
      Hsum[idet][ihar][imul][izps]->Fill(2,sumy[idet][ihar]);
      if (flag>-1) {
        float corx=ofsx[idet][ihar][imul][izps];
        float cory=ofsy[idet][ihar][imul][izps];
        psi[idet][ihar]=atan2(sumy[idet][ihar]-cory,sumx[idet][ihar]-corx)
                       /(float)(ihar+1);
      } else { 
        psi[idet][ihar]=atan2(sumy[idet][ihar],sumx[idet][ihar])
                       /(float)(ihar+1);
      }
      float phk=psi[idet][ihar]*(float)(ihar+1);
//    if (idet<3 && ihar%2==1) {
//      float sinphk=sin(phk);
//      float cosphk=cos(phk)+off[ihar];
//      phk=atan2(sinphk,cosphk); 
//    }
      Hphi[idet][ihar][imul][izps]->Fill(phk);
      float dp=0;
      for (int ia=0; ia<nord; ia++) {
        float cc=cos(phk*(float)(ia+1));
        float ss=sin(phk*(float)(ia+1));
        Hprc[idet][ihar][imul][izps]->Fill(ia+1,cc);
        Hprs[idet][ihar][imul][izps]->Fill(ia+1,ss);
        float aa=aaa[idet][ihar][imul][izps][ia];   // mean cos
        float bb=bbb[idet][ihar][imul][izps][ia];   // mean sin
        dp+=(aa*ss-bb*cc)*2.0/(float)(ia+1);
      }
      dp+=phk;
      if (flag==1) {
        psi[idet][ihar]=atan2(sin(dp),cos(dp))/(float)(ihar+1);
        Hphj[idet][ihar][imul][izps]->Fill(psi[idet][ihar]*(float)(ihar+1));
      }
    } else {
      psi[idet][ihar]=-10000;
    }
  }
}

void reactionPlane::calPlane(int imul, int izps)
{
//cout << "info. reactionPlane::calPlane" << endl;
  if (imul<0 || imul>nmul || izps<0 || izps>nzps) {
    cout << "error reactionPlane::calPlane, exceed array" << endl;
    exit(-1);
  }
  for (int idet=3; idet<ndet; idet++) {
    calHitPlane(imul,izps,idet);
  }
}

void reactionPlane::calBbcPlane(int imul, int izps, 
                                short adc[128], short tdc[128])
{
//cout << "info. reactionPlane::calBbcPlane" << endl;
  if (imul<0 || imul>nmul || izps<0 || izps>nzps) {
    cout << "error reactionPlane::calBbcPlane, exceed array" << endl;
    exit(-1);
  }
  clrPlane();
  float charge;
  for (int ipmt=0; ipmt<128; ipmt++) {
    if (tdc[ipmt]<oflw[ipmt]-10) {
      charge=(adc[ipmt]-pede[ipmt])*qgai[ipmt]/gain[ipmt];
      if (flag>-2) charge=charge*fac[imul][izps][ipmt];
      Hpmt[imul][izps]->Fill(ipmt,charge); 
      Hrad[imul][izps]->Fill(irad[ipmt],charge); 
      float phk=atan2(ypos[ipmt],xpos[ipmt]);
      int ibbc;
      float pm;
      if (ipmt<64) {ibbc=0; pm=1.0;}
      else         {ibbc=1; pm=-1.0;}
      filHitPlane(ibbc-3,phk,charge,pm);
      filHitPlane(2-3,phk,charge,pm);
    }
  }
  for (int idet=0; idet<3; idet++) {
    calHitPlane(imul,izps,idet);
  }
}

float reactionPlane::getBbcPlane(int idet, int ihar)
{
//cout << "info. reactionPlane::getBbcPlane" << endl;
  if (idet<0 || idet>ndet || ihar<0 || ihar>nhar) {
    cout << "error reactionPlane::getBbcPlane, exceed array" << endl;
    exit(-1);
  }
  return psi[idet][ihar];
}

float reactionPlane::getHitPlane(int idet, int ihar)
{
//cout << "info. reactionPlane::getHitPlane" << endl;
  if (idet+3<0 || idet+3>ndet || ihar<0 || ihar>nhar) {
    cout << "error reactionPlane::getHitPlane, exceed array" << endl;
    exit(-1);
  }
  return psi[idet+3][ihar];
}

void reactionPlane::checkResolution(int imul, int izps)
{
  for (int ihar=0; ihar<nhar; ihar++) {
    filResolution(imul+nmul*izps,  ihar,     0,ihar,  1,ihar);
    filResolution(imul+nmul*izps,  ihar+4,   0,ihar,  3,ihar);
    filResolution(imul+nmul*izps,  ihar+8,   1,ihar,  3,ihar);
    filResolution(imul+nmul*izps,  ihar+12,  2,ihar,  3,ihar);
    filResolution(imul+nmul*izps,  ihar+16,  4,ihar,  6,ihar);
    filResolution(imul+nmul*izps,  ihar+16,  5,ihar,  7,ihar);
  }
  filResolution(imul+nmul*izps,  20,  0,0,  1,1);
  filResolution(imul+nmul*izps,  20,  0,1,  1,0);
  filResolution(imul+nmul*izps,  21,  0,0,  3,1);
  filResolution(imul+nmul*izps,  21,  1,0,  3,1);
  filResolution(imul+nmul*izps,  22,  2,0,  3,1);
}

void reactionPlane::filResolution(int imul, int hid, 
                                  int idet, int ihar,
                                  int jdet, int jhar)
{
  if (psi[idet][ihar]>-9999 && psi[jdet][jhar]>-9999) {
    int ifac=ihar;
    if (ihar<jhar)  ifac=jhar;
    float phk=(psi[idet][ihar]-psi[jdet][jhar])*(float)(ifac+1);
    phk=atan2(sin(phk),cos(phk));
    Hrsp[hid]->Fill(imul,phk);
    Hrsc[hid]->Fill(imul,cos(phk));
    Hrss[hid]->Fill(imul,sin(phk));
    int izps=imul/nmul;
    int jmul=imul%nmul;
    if (jmul>(int)(nmul*0.1) && jmul<(int)(nmul*0.7))
    Hcor[hid][izps]->Fill(psi[idet][ihar]*(float)(ihar+1),
                          psi[jdet][jhar]*(float)(jhar+1));
  }
}
