//
// macro to draw Aerogel geometry
// 
//


{
  gROOT->Reset();

  TCanvas* c1 = new TCanvas("c1","c1");

  const int nbox = 80;        // number of box
  const float dx = 12.25/2.0; // half of box x length
  const float dy = 11.95/2.0; // half of box y length
  const float dz = 23.1/2.0;  // half of box z length
  const float bx = 200;
  const float by = 200;
  const float bz = 200;
  char name[100];
  char node[100];


  ifstream fin("accGeoCenterPosition.txt");
  if(!fin){
    cout << " can't open file " << endl;
    return;
  }

  int ibox;
  float x[nbox]; // box center position (x)
  float y[nbox]; // box center position (y)
  float z[nbox]; // box center position (z)

  TBRIK* aero[nbox];
  for(int i=0;i<nbox;i++){
    sprintf(name,"AERO%d",i);
//    aero[i] = new TBRIK(name,name,"void",dx,dy,dz);
    aero[i] = new TBRIK(name,name,"void",dx,dz,dy);
    aero[i]->SetFillColor(4);

    fin >> ibox >> x[i] >> y[i] >> z[i]; 
  }
  fin.close();

  TBRIK *brik = new TBRIK("BRIK","BRIK","void",bx,by,bz);
  TNode* node0 = new TNode("NODE0","NODE0","BRIK",0,0,0,"mat1");
  node0->cd();
  TNode* node1[nbox];
  for(int i=0;i<nbox;i++){
    sprintf(node,"NODE%d",i+1);
    sprintf(name,"AERO%d",i);
//    node1[i] = new TNode(node,node,name,x[i],y[i],z[i]);
    node1[i] = new TNode(node,node,name,x[i],z[i],y[i]);
  }

  node0->cd();
  node0->Draw();

  // Draw 3D axis
  TAxis3D rulers;
  rulers->SetXTitle("X [cm]");
  rulers->SetYTitle("Z [cm]");
  rulers->SetZTitle("Y [cm]");
  rulers->SetTitleOffset(1.4);
  rulers->Draw();
  
  c1->Update(); 
}

