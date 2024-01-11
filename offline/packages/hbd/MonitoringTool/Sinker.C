#include <iostream>

using namespace std;

Sinker()
{
gSystem->Load("bld/.libs/libhbd.so");

int packetdata[10000];
int ptr=0;
char data[40];

ifstream fin("Sample2.dat");

while(fin>>data){packetdata[ptr]=strtol(data, (char**)NULL, 16); cout << hex << packetdata[ptr] << endl; ptr++;}
cout << dec;
cout << "Number of words: " << ptr << endl;

HbdDcmRaw *dcm = new HbdDcmRaw();
dcm->SetNTotalSample(12);
dcm->setPacketManually(packetdata,0,ptr);
dcm->decode_packet(0);
dcm->showDecodedPacket(0);
}
