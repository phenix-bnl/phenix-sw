{
//Draw North side
c_N->cd();
display->draw_frames();

display->draw_dchits(1);
cout << "dchits1"<<endl;

display->draw_dctrks(1);
cout << "dctrks1"<<endl;

display->draw_emclus(1);
cout << "emc1"<<endl;

display->draw_pcclus(1);
cout << "pc1"<<endl;

display->draw_tofrec(1);
cout << "tof1"<<endl;

display->draw_crkhit(1);
cout << "crk1"<<endl;

display->draw_tec(1);
cout << "tec1"<<endl;

}
