{
//Draw South Side
c_S->cd();
display->draw_frames();

display->draw_dchits(0);
cout << "dchits0"<<endl;

display->draw_dctrks(0);
cout << "dctrks0"<<endl;

display->draw_emclus(0);
cout << "emc0"<<endl;

display->draw_pcclus(0);
cout << "pc0"<<endl;

display->draw_tofrec(0);
cout << "tof0"<<endl;

display->draw_crkhit(0);
cout << "crk0"<<endl;

display->draw_tec(0);
cout << "tec0"<<endl;

}
