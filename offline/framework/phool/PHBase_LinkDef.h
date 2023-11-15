#ifdef __CINT__

#pragma link C++ typedef PHBoolean;
#pragma link C++ enum PHMessageType;
#pragma link C++ enum PHAccessType;
#pragma link C++ enum  PHTreeType;
#pragma link C++ class PHString-!;
#pragma link C++ function PHMessage(const PHString&, int, const PHString&);
#pragma link C++ function operator+(const PHString&, const PHString&);
#pragma link C++ function operator<<(ostream &, const PHString &);
#pragma link C++ class PHObject+ ;
#pragma link C++ class PHTimeServer-!;

#endif /* __CINT__ */
