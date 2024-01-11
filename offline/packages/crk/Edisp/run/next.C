{
while(1) {
  //  mainIter.cd();
  mainIter.cd("DST");
  mainIter.forEach(reset);

  dstIn->read(dstNode);
  if(display->good_event()) break;
  cout << "skip one event"<<endl;
}
c_S->Clear();
c_N->Clear();
//c_R->Clear();
c_SW->Clear();
c_NW->Clear();
c_SE->Clear();
c_NE->Clear();

cout << "cleared"<<endl;

display->load_event();//do not display, yet
}
