#include <string>
using namespace std;

void checkDetGeo(){
  gROOT->SetStyle("Plain");

  gSystem->Load("libsvx.so");

  svxDetectorGeo *geo = new svxDetectorGeo();
  geo->set_Verbose(2);
  geo->Read_svxPISApar();
  

/*
  PHTimeStamp TStamp = PHTimeStamp();
  TStamp.setToSystemTime();
  geo->Fetch_svxPISApar(&TStamp);
*/


  for(int ily=0; ily<4; ily++){
    int nLadder = geo->get_nBarLadder(ily);
    int nSensor = geo->get_nBarSensor(ily);
    cout<<ily<<" "<<nLadder<<" "<<nSensor<<endl;
    for(int ildr=0; ildr<nLadder; ildr++){
      for(int isen=0; isen<nSensor; isen++){
        SvxSensor *sens_p = geo->GetSensorPtr(ily, ildr, isen);

/*
        float xyz[3];
        //if(isen==0) 
          cout<<ily<<":"<<ildr<<":"<<isen<<"  ---  "<<flush;
        for(int i=0; i<3; i++){ 
          xyz[i] = sens_p->get_transVector(i);
          cout<<xyz[i]<<" ";
        }
        //if(isen==0)
          cout<<endl;

*/
        if(ily==0&&ildr==0&&isen==0){
          float rot[3][3];
          for(int i=0; i<3; i++){ 
            for(int j=0; j<3; j++){ 
              rot[i][j] = sens_p->get_rotMatrix(i, j);
              cout<<rot[i][j]<<" ";
            }
            cout<<endl;
          }
        }

        if(isen==0){
          float cosphi   = sens_p->get_rotMatrix(0, 0); 
          float sinphi   = sens_p->get_rotMatrix(0, 1); 
          //---float sinphi   = sens_p->get_rotMatrix(1, 0); 
          float phicos   = TMath::ACos(cosphi)/3.14*180.;
          float phisin   = TMath::ASin(sinphi)/3.14*180.;
         
          float phicos_mod;
          float phisin_mod;
          string s_txt;
          if(cosphi>0){
            if(sinphi>0){ s_txt = "1st quadrant"; phicos_mod= phicos; phisin_mod=phisin;}
            else        { s_txt = "4th quadrant"; phicos_mod=-phicos; phisin_mod=phisin;}
          }
          else{
            if(sinphi>0){ s_txt = "2nd quadrant"; phicos_mod= phicos; phisin_mod= 180.-phisin;}
            else        { s_txt = "3rd quadrant"; phicos_mod=-phicos; phisin_mod=-180.-phisin;}
          }
          float phiangle = TMath::ATan2(sinphi, cosphi); // (x,y)==(cosphi, sinphi)
          float phideg  = phiangle/TMath::Pi()*180.;
          //--cout<<ily<<" "<<ildr<<" "<<isen<<" : "<<cosphi<<" "<<sinphi<<" "<<sqrt(cosphi*cosphi+sinphi*sinphi)<<" ";
          //--cout<<phicos<<" "<<phisin<<" "<<phiangle<<" "<<phideg<<"  "<<s_txt.c_str()<<" "<<phicos_mod<<" "<<phisin_mod<<endl;
          float costheta = sens_p->get_rotMatrix(2, 2); 
          float sintheta = sens_p->get_rotMatrix(1, 2); 
          float thetaangle = TMath::ATan2(sintheta, costheta); // (x,y)==(costheta, sintheta)
          float thetadeg  = thetaangle/TMath::Pi()*180.;

          cout<<ily<<" "<<ildr<<" "<<isen<<" : phi="<<cosphi<<" ("<<phideg<<")  theta="<<costheta<<" ("<<thetadeg<<")"<<endl;

        }
      }
    }
  }


  TCanvas *c1 = new TCanvas("c1","c1", 800, 800);
  c1->Range(-23, -23, 22, 22);
//  c1->Range(-12, -12, 12, 12);
  c1->SetFillStyle(4000);
  TLine *lx = new TLine(-20, 0, 20, 0);
  lx->Draw();
  TLine *ly = new TLine(0, -20, 0, 20);
  ly->Draw();

  TGaxis *xaxis = new TGaxis(-20, -20, 20, -20, -20, 20, 510);
  xaxis->SetName(TString::Format("xaxis"));
  xaxis->SetTitle("x");
  xaxis->SetLabelSize(0.02);
  xaxis->SetTitleSize(0.02);
  xaxis->Draw();

  TGaxis *yaxis = new TGaxis(-20, -20, -20,  20, -20, 20, 510);
  yaxis->SetName(TString::Format("yaxis"));
  yaxis->SetTitle("y");
  yaxis->SetLabelSize(0.02);
  yaxis->SetTitleSize(0.02);
  yaxis->Draw();


  int isen=0;
  for(int ily=0; ily<4; ily++){
    int nLadder = geo->get_nBarLadder(ily);
    for(int ildr=0; ildr<nLadder; ildr++){
      SvxSensor *sens_p = geo->GetSensorPtr(ily, ildr, isen);
      float xyz[3];
      float xwid = sens_p->get_xhalfWidth();
      float ywid = 0.1 ; // 500um
      float zwid = 0;
      //for(int isec=0; isec<sens_p->get_nSection(); isec++){
      //   zwid += sens_p->get_zhalfWidth(isec, 0);
      //}

      for(int i=0; i<3; i++){ xyz[i] = sens_p->get_transVector(i); }

      float rot[3][3];
      for(int i=0; i<3; i++){ 
        for(int j=0; j<3; j++){ 
          rot[i][j] = sens_p->get_rotMatrix(j, i);
/*
          rot[i][j] = sens_p->get_rotMatrix(i, j);
          if( (i==0&&j==1) || (i==1&&j==0) || (i==1&&j==2) || (i==2&&j==1)) {
            rot[i][j] *= -1.0;
            //cout<<"invert "<<i<<" "<<j<<endl;
          }
*/
        }
      }

      float xyz_lorg[4][3]={{-xwid, -ywid, -zwid},
                            { xwid, -ywid, -zwid},
                            { xwid,  ywid, -zwid},
                            {-xwid,  ywid, -zwid}};

      //  ratation
      //         | (0,0), (0,1), (0,2) |   |cosphi, -sinphicostheta,  sinphisintheta|
      //  Mrot = | (1,0), (1,1), (1,2) | = |sinphi,  cosphicostheta, -cosphisintheta|
      //         | (2,0), (2,1), (2,2) |   |     0,        sintheta,        costheta|
      float xyz_lo[4][3];
      for(int ipos=0; ipos<4; ipos++){
        xyz_lo[ipos][0] =rot[0][0]*xyz_lorg[ipos][0] + rot[0][1]*xyz_lorg[ipos][1] + rot[0][2]*xyz_lorg[ipos][2];
        xyz_lo[ipos][1] =rot[1][0]*xyz_lorg[ipos][0] + rot[1][1]*xyz_lorg[ipos][1] + rot[1][2]*xyz_lorg[ipos][2];
        xyz_lo[ipos][2] =rot[2][0]*xyz_lorg[ipos][0] + rot[2][1]*xyz_lorg[ipos][1] + rot[2][2]*xyz_lorg[ipos][2];

        cout<<ily<<" "<<ildr<<" ipos ="<<ipos<<", "<<xyz_lo[ipos][0]<<" "<<xyz_lo[ipos][1]<<" "<<xyz_lo[ipos][2]<<" --- ";
        cout<<rot[0][0]<<" "<<xyz_lorg[ipos][0]<<" "<<rot[1][0]<<" "<<xyz_lorg[ipos][1]<<" "<<rot[2][0]<<" "<<xyz_lorg[ipos][2]<<endl;
      }

/*
      float cosphi   = sens_p->get_rotMatrix(0, 0); 
      float sinphi   = sens_p->get_rotMatrix(0, 1); 
      float costheta = sens_p->get_rotMatrix(2, 2); 
      float sintheta = sens_p->get_rotMatrix(1, 2); 

      for(int ipos=0; ipos<4; ipos++){
        xyz_lo[ipos][0] = cosphi*xyz_lorg[ipos][0] - sinphi*xyz_lorg[ipos][1];
        xyz_lo[ipos][1] = sinphi*xyz_lorg[ipos][0] + cosphi*xyz_lorg[ipos][1];
        xyz_lo[ipos][2] = xyz_lorg[ipos][0];
      }
*/

      float x_pol[5], y_pol[5], z_pol[5];
      for(int i=0; i<5; i++){
        if(i<4){
          x_pol[i] = xyz[0] + xyz_lo[i][0];
          y_pol[i] = xyz[1] + xyz_lo[i][1];
          z_pol[i] = xyz[2] + xyz_lo[i][2];
/*
          if(ily==0&&ildr==0){
            cout<<"ipos ="<<i<<", "<<x_pol[i]<<" "<<y_pol[i]<<" "<<z_pol[i]<<endl;
            cout<<"ipos ="<<i<<", "<<xyz_lo[i][0]<<" "<<xyz_lo[i][1]<<" "<<xyz_lo[i][2]<<endl;
            cout<<rot[0][0]<<" "<<xyz_lorg[i][0]<<" "<<rot[1][0]<<" "<<xyz_lorg[i][1]<<" "<<rot[2][0]<<" "<<xyz_lorg[i][2]<<endl;
            cout<<rot[0][1]<<" "<<xyz_lorg[i][0]<<" "<<rot[1][1]<<" "<<xyz_lorg[i][1]<<" "<<rot[2][1]<<" "<<xyz_lorg[i][2]<<endl;
            cout<<rot[0][2]<<" "<<xyz_lorg[i][0]<<" "<<rot[1][2]<<" "<<xyz_lorg[i][1]<<" "<<rot[2][2]<<" "<<xyz_lorg[i][2]<<endl;
          }
*/
        } 
        else {
          x_pol[i] = xyz[0] + xyz_lo[0][0];
          y_pol[i] = xyz[1] + xyz_lo[0][1];
          z_pol[i] = xyz[2] + xyz_lo[0][2];
        }
      }
 
      TPolyLine *pl = new TPolyLine(5, x_pol, y_pol);
      pl->SetFillStyle(1001);
      pl->SetFillColor(6);
      pl->Draw();

      TPolyLine *plse = new TPolyLine(2, &x_pol[2], &y_pol[2]);
      plse->SetLineColor(2);
      plse->SetLineWidth(1.5);
      plse->Draw();

      TPolyLine *plsex = new TPolyLine(2, &x_pol[1], &y_pol[1]);
      plsex->SetLineColor(4);
      plsex->SetLineWidth(3.0);
      plsex->Draw();
      
      TText *txt = new TText(xyz[0], xyz[1], TString::Format("%d", ildr));
      txt->SetTextSize(0.02);
      txt->Draw();
     
    }
  }
  c1->Print("checkDetGeo.png");

  
}

