{
gStyle->SetPalette(1);
gSystem->Load("libemcEfficiency.so");
cout << "Hi ! My PID is " << gSystem->GetPid() << endl;
}
