
void chopthin_internal(std::vector<double>& w, unsigned int N, std::vector<double>& wres, std::vector<int>& ires, double eta=5.828427){
  // checks
  if (ires.size()!=N) ires.resize(N);
  if (wres.size()!=N) wres.resize(N);
  if (eta<4) chopthin_error("eta less than 4");
  if (N<=0) chopthin_error("N must be >0");
  
  int n=w.size();
  std::vector<double> vl= w;
  std::vector<double>::iterator vli, vui, j;
  
  std::vector<double> vu=vl;
  double sl=0,cm=0,su=0,cu=0;
  double a, b;
  double afinal=-1.;
  double h,sltmp,sutmp;
  int cutmp, cmtmp;
  while(vl.size()>0||vu.size()>0){
    if (vl.size()>=vu.size()){
      a=vl[intrand(vl.size())];
      b=a*eta/2.;
    }else{
      b=vu[intrand(vu.size())];
      a=2.*b/eta;      
    }
    cmtmp=0;
    sltmp=0.;
    for (vli=vl.begin(); vli!=vl.end(); ++vli){
      if (*vli<=a) sltmp+=*vli;
      else cmtmp++;
    }
    sutmp=0.;
    cutmp=0;
    for (vui=vu.begin(); vui!=vu.end(); ++vui){
      if (*vui>=b) {sutmp+=(*vui);cutmp++;};
    }
    if (a<=0){ //have I picked particle with non-positive weight?
      if (su+sutmp==0)
	chopthin_error("No positive weights");
      else
	h=N+1;
    }else{
      h=((cm+cmtmp)-(cu+cutmp))+(sl+sltmp)/a+(su+sutmp)/b;
    }
    if (h==N) {
      afinal=a;
      break;
    }
    if (h>N){
      sl+=sltmp;
      j=vl.begin();
      for (vli=vl.begin(); vli!=vl.end(); ++vli){
	if (*vli>a) {
	  *j=*vli;
	  ++j;
	}  
      }
      vl.resize(j-vl.begin());
      j=vu.begin();
      for (vui=vu.begin(); vui!=vu.end(); ++vui){
	if (*vui>b){
	  *j=*vui;
	  ++j;
	}
      }
      vu.resize(j-vu.begin());
    }else{
      j=vl.begin();
      for (vli=vl.begin(); vli!=vl.end(); ++vli){
	if (*vli<a) {
	  *j=*vli;
	  ++j;
	}else cm++;  
      }
      vl.resize(j-vl.begin());
      su+=sutmp;
      cu+=cutmp;
      j=vu.begin();
      for (vui=vu.begin(); vui!=vu.end(); ++vui){
	if (*vui<b){
	  *j=*vui;
	  ++j;
	}
      }
      vu.resize(j-vu.begin());
    }    
  }
  if (afinal<0){
    afinal=(sl+2.*su/eta)/(N-cm+cu);
  }
  a=afinal;
  b=a*eta/2;

  // prepare result
  std::vector<int>::iterator irespos=ires.begin();
  std::vector<double>::iterator wrespos=wres.begin();
  double u = myrunif();
  double wtot=0;
  for (int posv=0; posv<n; posv++){
    if (w[posv]<a)
      h=w[posv]/a;
    else
      if (w[posv]>b)
	h=w[posv]/b;
      else h=1.;
    u-=h;
    if (u<0){
      int ndes=ceil(-u);
      u+=(double)ndes;
      double wdes;
      if (w[posv]<a){
	wdes=a;
      }else{
	wdes=w[posv]/ndes;
      }
      for (int j=0; j<ndes; j++){
	*(irespos++)=posv+1;
	*(wrespos++)=wdes;
	wtot+=wdes;
      }	
    }
  }
  for (unsigned int count=0; count<N; count++){
    wres[count]*=((double)N)/wtot;
  }
}
