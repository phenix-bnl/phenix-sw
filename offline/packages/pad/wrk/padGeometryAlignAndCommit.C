{
/////////////////////////////////////////////////////////////////////
// macro written to commit to Objy DB alignment geometry changes
// for PC2 West, PC3 East and West
//   
// valid for run 2:  June 2001 through January 2002  
// author: Julia Velkovska
// e-mail: julia@bnl.gov 
//  

// This is the second attempt on alignment for year 2 data.
// 
// Starting from file :
// offline/packages/pad/wrk/padGeometry.adb.beforeTransformation
// 1) First we applied arm transformations that were specified in
// transformation.txt and were applied to West and East arm as a whole.
// 2) Second alignmnet was applied on a chamber-by-chamber basis, using
// residuals information from no-field runs.
// 
// Now (January 2002) the final alignment was applied using this macro.
///////////////////////////////////////////////////////////////////////





gSystem->Load("libphgeo.so");
gSystem->Load("libpad.so");


using namespace PHGeometry; 

mPadDetGeo = new PHpadDetectorGeo();

mPadDetGeo->FetchFromFileDBFormat("padGeometry.adb.beforeTransformation");

///   setup the arm transformations ////////
PHVector uE(0.999999,0.00138,0.0);
PHVector vE(-0.00138,0.999999,0.0);
PHPoint originE(-1.361,-1.132,-0.05);
PHFrame padEAST(originE,uE,vE);
mPadDetGeo->set_eastFrame(padEAST);
PHVector uW(0.999998,-0.00188,0.0);
PHVector vW(0.00188,0.999998,0.0);
PHPoint originW(-0.061,-1.012,0);
PHFrame padWEST(originW,uW,vW);
mPadDetGeo->set_westFrame(padWEST);

/// set-up chamber-by-chamber transformations
//
// pc3 East
PHFrame XYZ;
PHMatrix pc2M;
PHMatrix pc3M;
double anglePC3[2][4][2];
PHVector transPC3[2][4][2];  
double anglePC2[2][4][2];
PHVector transPC2[2][4][2];

anglePC3[0][0][1] = - 2.01293e-03;  // sector 0 , z>0
anglePC3[0][0][0] = - 2.58600e-03;  // sector 0 , z<0
anglePC3[0][1][1] = - 2.53414e-03;  // sector 1 , z>0
anglePC3[0][1][0] = - 2.39947e-03;  // sector 1 , z<0
anglePC3[0][2][1] = - 2.75711e-03;  // sector 2 , z>0
anglePC3[0][2][0] = - 2.47144e-03;  // sector 2 , z<0
anglePC3[0][3][1] = - 3.85405e-03;  // sector 3 , z>0
anglePC3[0][3][0] = - 3.57555e-03;  // sector 3 , z<0
//
transPC3[0][0][1].setZ(-4.29e-01);// sector 0 , z>0
transPC3[0][0][0].setZ(-5.25e-01);
transPC3[0][1][1].setZ(-4.24e-01);
transPC3[0][1][0].setZ(-5.63e-01);
transPC3[0][2][1].setZ(-3.48e-01);
transPC3[0][2][0].setZ(-4.84e-01);
transPC3[0][3][1].setZ(-1.85e-01);
transPC3[0][3][0].setZ(-3.72e-01);

//
// pc3West
anglePC3[1][0][1] =   2.84006e-03;   // sector 0 , z>0
anglePC3[1][0][0] =   2.71904e-03;   // sector 0 , z<0
anglePC3[1][1][1] =   1.96120e-03;   // sector 1 , z>0
anglePC3[1][1][0] =   1.50716e-03;   // sector 1 , z<0
anglePC3[1][2][1] =   7.40957e-04;   // sector 2 , z>0
anglePC3[1][2][0] =   4.82692e-05;   // sector 2 , z<0
anglePC3[1][3][1] =   5.49677e-04;   // sector 3 , z>0
anglePC3[1][3][0] = - 7.54216e-04;   // sector 3 , z<0

transPC3[1][0][1].setZ(-5.31074e-01);// sector 0 , z>0
transPC3[1][0][0].setZ(-4.31987e-01);
transPC3[1][1][1].setZ(-6.69281e-01);
transPC3[1][1][0].setZ(-4.47615e-01);
transPC3[1][2][1].setZ(-6.42880e-01);
transPC3[1][2][0].setZ(-2.14580e-01);
transPC3[1][3][1].setZ(-7.89998e-01);
transPC3[1][3][0].setZ(-1.46940e-01);



// pc2West
transPC2[1][0][1].setZ(- 3.72342e-01);// sector 0 , z>0
transPC2[1][0][0].setZ (- 2.91476e-01);// sector 0 , z<0
transPC2[1][1][1].setZ (- 4.80479e-01);// sector 1 , z>0
transPC2[1][1][0].setZ (- 4.05207e-01);// sector 1 , z<0
transPC2[1][2][1].setZ (- 5.47259e-01);// sector 2 , z>0
transPC2[1][2][0].setZ (- 1.34674e-01);// sector 2 , z<0
transPC2[1][3][1].setZ (- 5.55523e-01);// sector 3 , z>0
transPC2[1][3][0].setZ (- 1.57255e-01);// sector 3 , z<0

// pc2West
anglePC2[1][0][1] =   2.61140e-03;   // sector 0 , z>0
anglePC2[1][0][0] =   3.03330e-03;   // sector 0 , z<0
anglePC2[1][1][1] =   2.15693e-03;   // sector 1 , z>0
anglePC2[1][1][0] =   1.64966e-03;   // sector 1 , z<0
anglePC2[1][2][1] =   4.81966e-04;   // sector 2 , z>0
anglePC2[1][2][0] =   1.55256e-04;   // sector 2 , z<0
anglePC2[1][3][1] =   4.77410e-04;   // sector 3 , z>0
anglePC2[1][3][0] =  -6.92927e-04;   // sector 3 , z<0


PHVector axis(0,0,1);
PHFrame pc2F;
PHFrame pc3F;
// loop on and make frames
for(short arm = 0;arm<2;arm++){
  for(short sect=0;sect<4;sect++){
	  for(short side=0;side<2;side++){
	    pc2M = rotationMatrix(anglePC2[arm][sect][side],axis);
	    pc3M = rotationMatrix(anglePC3[arm][sect][side],axis);
	    pc2F = MatrixAndVector2frames(XYZ,pc2M,transPC2[arm][sect][side]);
	    pc3F = MatrixAndVector2frames(XYZ,pc3M,transPC3[arm][sect][side]);
	    mPadDetGeo->set_pc2Frame(pc2F,arm,sect,side);
	    mPadDetGeo->set_pc3Frame(pc3F,arm,sect,side);
	  }
  }
      }
//mPadDetGeo->PrintGeo(1,1);//before 
mPadDetGeo->rotateAndTranslate();
//mPadDetGeo->PrintGeo(1,1);// after

/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//  STOP if you are not sure !!                                //
//  uncomment the next line, if you want to commit the current //
//  geometry to the database                                   //
// mPadDetGeo->PutIntoGeoChamDatabase();                       //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
}





