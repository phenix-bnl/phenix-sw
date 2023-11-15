{
gSystem->Load("libphgeo.so");

using namespace PHGeometry;
// create a panel by giving 3 corner points
PHPoint p0(-1,0,0);
PHPoint p1(1,0,0);
PHPoint p2(-1,2,0);

PHPanel inP(p0,p1,p2);
cout<<" input Panel "<<endl;
inP.print();

PHVector axis (0,0,1);
cout<<"rotation AXIS "<<endl;
axis.print();
double angle = 3.141592654/6.;
cout<<"rotate the panel about the AXIS passing through its center to an ANGLE "<<angle<<endl; 
PHPanel outP = rotatePanelAboutCenter(3.141592654/6.,axis,inP);
cout<<" the resulting panel "<<endl;
outP.print();
cout<<" apply the inverse transformation "<<endl;
cout<<" get back this panel "<<endl;
PHPanel newOut = rotatePanelAboutCenter(-angle,axis,outP);
newOut.print();
}
