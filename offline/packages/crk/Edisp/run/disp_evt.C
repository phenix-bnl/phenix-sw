{
//Draw South Side
c_S->cd();
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


//Draw North side
c_N->cd();
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

c_R->cd();
display->draw_rich();
cout << "rich"<<endl;
}
