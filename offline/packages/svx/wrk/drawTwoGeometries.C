{
// This macro draws VTX geometry (X-Y slice at Z=0) using
// information from svxGeometry.txt file.
// This file can be created by svxDetectorGeo::PutIntoFile() method,
// which writes out from memory VTX ladders represented as PHPanels
// see  macro 

  //FILE *svxgeofile = fopen("svxGeometry_survey0.txt", "r");
  FILE *svxgeofile = fopen("svxGeometry_new.txt", "r");
  //FILE *svxgeofile = fopen("svxGeometry_surveyKieran0.txt", "r");
  //FILE *svxgeofile = fopen("svxGeometry_externalAlignment.txt", "r");
  if ( !svxgeofile )                                    {std::cerr << "ERROR opening svxGeometry.txt file" << endl;}
  char cdummy[120];
  int svxNLadders[4];
  float x0[99],y0[99],z0[99];
  float x1[99],y1[99],z1[99];
  float x2[99],y2[99],z2[99];
  float x3[99],y3[99],z3[99];
  float xyz[999];
  int tmparm, tmplayer, tmpladder;

  TCanvas* c1 = new TCanvas("c1", "", 800, 800);
  //TCanvas* c1 = new TCanvas("c1", "", 600, 600);
  c1->Range(-20.,-20.,20.,20.);
  TLine l1[30];
  TLine l2[30];
  TLine l3[30];
  TLine l4[30];

  for ( unsigned int i = 0; i < 4; i++ ) { fscanf(svxgeofile,"%d",&svxNLadders[i]); }
  std::cout << "Number of ladders = " << svxNLadders[0] << " " << svxNLadders[1] << " " << svxNLadders[2] << " " << svxNLadders[3] << std::endl;

  for ( unsigned int k = 0; k < svxNLadders[0]*2; k++ ) { // all ladders in layer 0
    for ( unsigned int j = 0; j < 4; j++ ) {   // 4 points
      for ( unsigned int i = 0; i < 3; i++ ) { // x,y,z
        fscanf(svxgeofile,"%f",&xyz[i]); 
//        std::cout << xyz[i] << " ";
      }
//      std::cout << endl;
      if(j==0) {x0[k]=xyz[0];} 
      if(j==0) {y0[k]=xyz[1];} 
      if(j==1) {x1[k]=xyz[0];} 
      if(j==1) {y1[k]=xyz[1];} 
    }
    //std::cout << "drawing from (" << x0[k] << "," << y0[k] << ") to (" << x1[k] << "," << y1[k] << ")" << std::endl;
    l1[k] = new TLine(x0[k],y0[k],x1[k],y1[k]);
    (l1[k])->SetLineColor(kBlack);
    //(l1[k])->Draw();
  }

  for ( unsigned int k = 0; k < svxNLadders[1]*2; k++ ) { // all ladders in layer 1
    for ( unsigned int j = 0; j < 4; j++ ) {   // 4 points
      for ( unsigned int i = 0; i < 3; i++ ) { // x,y,z
        fscanf(svxgeofile,"%f",&xyz[i]);
//        std::cout << xyz[i] << " ";
      }
//      std::cout << endl;
      if(j==0) {x0[k]=xyz[0];}
      if(j==0) {y0[k]=xyz[1];}
      if(j==1) {x1[k]=xyz[0];}
      if(j==1) {y1[k]=xyz[1];}
    }
    //std::cout << "drawing from (" << x0[k] << "," << y0[k] << ") to (" << x1[k] << "," << y1[k] << ")" << std::endl;
    l2[k] = new TLine(x0[k],y0[k],x1[k],y1[k]);
    (l2[k])->SetLineColor(kRed);
    //(l2[k])->Draw();
  }

  for ( unsigned int k = 0; k < svxNLadders[2]*2; k++ ) { // all ladders in layer 2
    for ( unsigned int j = 0; j < 4; j++ ) {   // 4 points
      for ( unsigned int i = 0; i < 3; i++ ) { // x,y,z
        fscanf(svxgeofile,"%f",&xyz[i]);
//        std::cout << xyz[i] << " ";
      }
//      std::cout << endl;
      if(j==0) {x0[k]=xyz[0];}
      if(j==0) {y0[k]=xyz[1];}
      if(j==1) {x1[k]=xyz[0];}
      if(j==1) {y1[k]=xyz[1];}
    }
    //std::cout << "drawing from (" << x0[k] << "," << y0[k] << ") to (" << x1[k] << "," << y1[k] << ")" << std::endl;
    l3[k] = new TLine(x0[k],y0[k],x1[k],y1[k]);
    (l3[k])->SetLineWidth(3);
    (l3[k])->SetLineColor(kGreen+2);
    (l3[k])->Draw();
  }

  for ( unsigned int k = 0; k < svxNLadders[3]*2; k++ ) { // all ladders in layer 3
    for ( unsigned int j = 0; j < 4; j++ ) {   // 4 points
      for ( unsigned int i = 0; i < 3; i++ ) { // x,y,z
        fscanf(svxgeofile,"%f",&xyz[i]);
        //if(k==9) std::cout << xyz[i] << " ";
      }
        //if(k==9) std::cout << endl;
      if(j==0) {x0[k]=xyz[0];}
      if(j==0) {y0[k]=xyz[1];}
      if(j==1) {x1[k]=xyz[0];}
      if(j==1) {y1[k]=xyz[1];}
    }
    //std::cout << "drawing from (" << x0[k] << "," << y0[k] << ") to (" << x1[k] << "," << y1[k] << ")" << std::endl;
    l4[k] = new TLine(x0[k],y0[k],x1[k],y1[k]);
    (l4[k])->SetLineWidth(3);
    (l4[k])->SetLineColor(kBlue);
    (l4[k])->Draw();
  }


//------------------------------------
// now draw perfect geometry
//------------------------------------

  FILE *svxgeofile2 = fopen("svxGeometry.txt", "r");
  //FILE *svxgeofile2 = fopen("svxGeometry_externalAlignment.txt", "r");
  if ( !svxgeofile2 )                                    {std::cerr << "ERROR opening svxGeometry.txt file" << endl;}

  TLine pl1[30];
  TLine pl2[30];
  TLine pl3[30];
  TLine pl4[30];
  TLatex t3[30];

  for ( unsigned int i = 0; i < 4; i++ ) { fscanf(svxgeofile2,"%d",&svxNLadders[i]); }
  std::cout << "Number of ladders = " << svxNLadders[0] << " " << svxNLadders[1] << " " << svxNLadders[2] << " " << svxNLadders[3] << std::endl;

  for ( unsigned int k = 0; k < svxNLadders[0]*2; k++ ) { // all ladders in layer 0
    for ( unsigned int j = 0; j < 4; j++ ) {   // 4 points
      for ( unsigned int i = 0; i < 3; i++ ) { // x,y,z
        fscanf(svxgeofile2,"%f",&xyz[i]);
//        std::cout << xyz[i] << " ";
      }
//      std::cout << endl;
      if(j==0) {x0[k]=xyz[0];}
      if(j==0) {y0[k]=xyz[1];}
      if(j==1) {x1[k]=xyz[0];}
      if(j==1) {y1[k]=xyz[1];}
    }
    //std::cout << "drawing from (" << x0[k] << "," << y0[k] << ") to (" << x1[k] << "," << y1[k] << ")" << std::endl;
    pl1[k] = new TLine(x0[k],y0[k],x1[k],y1[k]);
    (pl1[k])->SetLineColor(kBlack);
    //(pl1[k])->Draw();
  }

  for ( unsigned int k = 0; k < svxNLadders[1]*2; k++ ) { // all ladders in layer 1
    for ( unsigned int j = 0; j < 4; j++ ) {   // 4 points
      for ( unsigned int i = 0; i < 3; i++ ) { // x,y,z
        fscanf(svxgeofile2,"%f",&xyz[i]);
//        std::cout << xyz[i] << " ";
      }
//      std::cout << endl;
      if(j==0) {x0[k]=xyz[0];}
      if(j==0) {y0[k]=xyz[1];}
      if(j==1) {x1[k]=xyz[0];}
      if(j==1) {y1[k]=xyz[1];}
    }
    //std::cout << "drawing from (" << x0[k] << "," << y0[k] << ") to (" << x1[k] << "," << y1[k] << ")" << std::endl;
    pl2[k] = new TLine(x0[k],y0[k],x1[k],y1[k]);
    (pl2[k])->SetLineColor(kRed);
    //(pl2[k])->Draw();
  }

  for ( unsigned int k = 0; k < svxNLadders[2]*2; k++ ) { // all ladders in layer 2
//  for ( unsigned int k = 0; k < 4; k++ ) { // all ladders in layer 2
    for ( unsigned int j = 0; j < 4; j++ ) {   // 4 points
      for ( unsigned int i = 0; i < 3; i++ ) { // x,y,z
        fscanf(svxgeofile2,"%f",&xyz[i]);
//        std::cout << xyz[i] << " ";
      }
//      std::cout << endl;
      if(j==0) {x0[k]=xyz[0];}
      if(j==0) {y0[k]=xyz[1];}
      if(j==1) {x1[k]=xyz[0];}
      if(j==1) {y1[k]=xyz[1];}
    }
    //std::cout << "drawing from (" << x0[k] << "," << y0[k] << ") to (" << x1[k] << "," << y1[k] << ")" << std::endl;
    pl3[k] = new TLine(x0[k],y0[k],x1[k],y1[k]);
    (pl3[k])->SetLineColor(kRed);
    (pl3[k])->SetLineWidth(2);
    (pl3[k])->Draw();
  }

  for ( unsigned int k = 0; k < svxNLadders[3]*2; k++ ) { // all ladders in layer 3
//  for ( unsigned int k = 0; k < 4; k++ ) { // all ladders in layer 2
    for ( unsigned int j = 0; j < 4; j++ ) {   // 4 points
      for ( unsigned int i = 0; i < 3; i++ ) { // x,y,z
        fscanf(svxgeofile2,"%f",&xyz[i]);
//        std::cout << xyz[i] << " ";
      }
//      std::cout << endl;
      if(j==0) {x0[k]=xyz[0];}
      if(j==0) {y0[k]=xyz[1];}
      if(j==1) {x1[k]=xyz[0];}
      if(j==1) {y1[k]=xyz[1];}
    }
    //std::cout << "drawing from (" << x0[k] << "," << y0[k] << ") to (" << x1[k] << "," << y1[k] << ")" << std::endl;
    pl4[k] = new TLine(x0[k],y0[k],x1[k],y1[k]);
    (pl4[k])->SetLineColor(kRed);
    (pl4[k])->SetLineWidth(2);
    (pl4[k])->Draw();
  }

//t1=new TLatex(1.0,9.0,"energy of one photon, GeV");
//t1->Draw();


TArrow* a1 = new TArrow(0.,0.,3.,0.,0.01);
a1->Draw();
TArrow* a2 = new TArrow(0.,0.,0.,3.,0.01);
a2->Draw();

}

