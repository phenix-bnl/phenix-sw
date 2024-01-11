#include <string>
using namespace std;

void checkDetGeoSensor(){
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


  TCanvas *c1 = new TCanvas("c1","c1", 1000, 800);
  c1->Divide(4,2);

  int nlad[4]={10, 20, 16, 24};
  for(int ily=0; ily<4; ily++){
    int nLadder = geo->get_nBarLadder(ily);
    int nSensor = geo->get_nBarSensor(ily);
    for(int ildr=0; ildr<nLadder; ildr++){
      for(int isen=0; isen<nSensor; isen++){

        int ican = 2*ily + int(ildr/(nlad[ily]/2));
        //cout<<"ican : "<<ican<<endl;
        c1->cd(ican+1);
        TPad *pad = gPad;
        float x_rng = (ican<4)? 20 : 30;
        float y_rng = (ican<4)? 10 : 20;
        pad->Range(-x_rng, -y_rng, x_rng, y_rng); 

        TGaxis *zaxis = new TGaxis(-(x_rng-5), -(y_rng-2), (x_rng-5), -(y_rng-2), -(x_rng-5), (x_rng-5), 510);
        zaxis->SetName(TString::Format("zaxis_%d", ican));
        zaxis->SetTitle("z");
        zaxis->Draw();

        TGaxis *yaxis = new TGaxis( (x_rng-5), -(y_rng-2), (x_rng-5), (y_rng-2), -(y_rng-2), (y_rng-2), 510, "+L");
        yaxis->SetName(TString::Format("yaxis_%d", ican));
        yaxis->SetTitle("y");
        yaxis->Draw();


        SvxSensor *sens_p = geo->GetSensorPtr(ily, ildr, isen);

        float xyz[3];
        float xwid = sens_p->get_zhalfWidth(0, 0);
        float ywid = 0.01; // 5000um
        float zwid = 0.0;
        for(int isec=0; isec<sens_p->get_nSection(); isec++){
           zwid += sens_p->get_zhalfWidth(isec, 0);
        }
        //if(isen==0) cout<<"zwid : "<<zwid<<endl;
        for(int i=0; i<3; i++){ xyz[i] = sens_p->get_transVector(i); }

        float rot[3][3];
        for(int i=0; i<3; i++){  
          for(int j=0; j<3; j++){ // rot_element sub_xyz
            rot[i][j] = sens_p->get_rotMatrix(i, j); 
          }
        }

        float xyz_lo[4][3];
        float xyz_lorg[4][3]={{-xwid, ywid, -zwid},
                              {-xwid, ywid,  zwid},
                              {xwid,  ywid,  zwid},
                              {xwid,  ywid, -zwid}};

        //  ratation
        float cosphi   = sens_p->get_rotMatrix(0, 0); 
        float sinphi   = sens_p->get_rotMatrix(1, 0); 
        float costheta = sens_p->get_rotMatrix(2, 2); 
        float sintheta = sens_p->get_rotMatrix(1, 2); 

        for(int ipos=0; ipos<4; ipos++){

/*
          xyz_lo[ipos][2] = costheta*xyz_lorg[ipos][2] - sintheta*xyz_lorg[ipos][0];
          xyz_lo[ipos][1] = sintheta*xyz_lorg[ipos][2] + costheta*xyz_lorg[ipos][0];
*/

          for(int i=0; i<3; i++){  // xyz
            xyz_lo[ipos][i] = 0;
            for(int j=0; j<3; j++){ // rot_element sub_xyz
              xyz_lo[ipos][i] += rot[j][i]*(xyz_lorg[ipos][j]);
            }
          //xyz_lo[ipos][i] = rot[i][0]*(xyz_lorg[ipos][0]) + rot[i][1]*(xyz_lorg[ipos][1]) + rot[i][2]*(xyz_lorg[ipos][2]);
          }
        }

        float x_pol[5], y_pol[5];
        for(int i=0; i<5; i++){
          if(i<4){
            x_pol[i] = xyz[2] + xyz_lo[i][2];
            y_pol[i] = xyz[1] + xyz_lo[i][1];
          } 
          else {
            x_pol[i] = xyz[2] + xyz_lo[0][2];
            y_pol[i] = xyz[1] + xyz_lo[0][1];
          }
        }
 

        TPolyLine *pl = new TPolyLine(5, x_pol, y_pol);
        pl->SetFillStyle(1001);
        pl->SetFillColor(6);
        pl->Draw();

        if(isen==0){
          int ilabelpos = (ican<4)? -18 : -25;
          TText *tlad = new TText(ilabelpos, xyz[1], TString::Format("%d", ildr));
          tlad->Draw();
        }
        
        TText *txt = new TText(xyz[2], xyz[1], TString::Format("%d", isen));
        txt->Draw();
       
        // 
        TPolyLine *plx = new TPolyLine(2, &x_pol[2], &y_pol[2]);
        plx->SetLineColor(4);
        plx->SetLineWidth(3.0);
        plx->Draw();

        TPolyLine *plz = new TPolyLine(2, &x_pol[1], &y_pol[1]);
        plz->SetLineColor(2);
        plz->SetLineWidth(3.0);
        plz->Draw();
      }
    }
  }
  c1->Print("checkDetGeoSensor.png");

  
}

