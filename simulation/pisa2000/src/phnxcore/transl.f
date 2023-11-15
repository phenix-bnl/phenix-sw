      subroutine transl(iref,itp,npid,kl2)                                      
c                                                                               
c       transl translates the particle identity code of rqmd into the           
c       lund code (iref=-1) or vice versa (iref= 1).                            
c                                                                               
c                                                                               
c input       iref=-1                                                           
c             input: itp,npid                                                   
c             output: kl2                                                       
c                                                                               
c input       iref= 1                                                           
c             input: kl2                                                        
c             output: itp,npid                                                  
c                                                                               
      if(iref.eq.-1) then                                                       
          if(itp.eq. 1 .or. itp.eq.10) then                                     
              kl2= 42                                                           
          else if(itp.eq. 2 .or. itp .eq.11) then                               
              kl2= 41                                                           
          else if(itp.eq. 3) then                                               
              kl2= 64                                                           
          else if(itp.eq. 4) then                                               
              kl2= 63                                                           
          else if(itp.eq. 5) then                                               
              kl2= 62                                                           
          else if(itp.eq. 6) then                                               
              kl2= 61                                                           
          else if(itp.eq. 7) then                                               
              if(npid.eq.0) then                                                
                kl2= -17                                                        
              else                                                              
                kl2= -27                                                        
              end if                                                            
          else if(itp.eq. 8) then                                               
              if(npid.eq.0) then                                                
                kl2=  23                                                        
              else                                                              
                kl2=  33                                                        
              end if                                                            
          else if(itp.eq. 9) then                                               
              if(npid.eq.0) then                                                
                kl2=  17                                                        
              else                                                              
                kl2=  27                                                        
              end if                                                            
          else if(itp.eq.12) then                                               
              kl2=  24                                                          
          else if(itp.eq.13) then                                               
              kl2=  57                                                          
          else if(itp.eq.14) then                                               
              if(iabs(npid).eq.2) then                                          
                kl2= isign(1,npid) * 18                                         
              else if(iabs(npid).eq.1) then                                     
                kl2= isign(1,npid) * 19                                         
              end if                                                            
          else if(itp.eq.15) then                                               
              if( npid.eq.1) then                                               
                 kl2= 43                                                        
              else if( npid.eq.0) then                                          
                 kl2= 44                                                        
              else if( npid.eq.-1) then                                         
                 kl2= 45                                                        
              end if                                                            
          else if(itp.eq. 98) then                                              
              kl2=npid                                                          
          else if(itp.eq. 99) then                                              
              kl2=npid                                                          
          else if(itp.eq.-9999) then                                            
              kl2=0                                                             
          else                                                                  
              write(8,*) 'itp=',itp,'npid=',npid,'iref=',iref                   
c             idiv=0                                                            
c             idiv=10/idiv                                                      
c             write(8,*) idiv                                                   
c             call errex('itp not known in transl')                             
          end if                                                                
      else if(iref.eq. 1) then                                                  
          if(kl2.eq.42) then                                                    
              itp=1                                                             
              npid=0                                                            
          else if(kl2.eq.41) then                                               
              itp=2                                                             
              npid=0                                                            
          else if(kl2.eq.64) then                                               
              itp=3                                                             
              npid=1                                                            
          else if(kl2.eq.63) then                                               
              itp=4                                                             
              npid=1                                                            
          else if(kl2.eq.62) then                                               
              itp=5                                                             
              npid=1                                                            
          else if(kl2.eq.61) then                                               
              itp=6                                                             
              npid=1                                                            
          else if(kl2.eq.17) then                                               
              itp=9                                                             
              npid=0                                                            
          else if(kl2.eq.-17) then                                              
              itp=7                                                             
              npid=0                                                            
          else if(iabs(kl2).eq.23) then                                         
              itp=8                                                             
              npid=0                                                            
          else if(kl2.eq.27) then                                               
              itp=9                                                             
              npid=2                                                            
          else if(kl2.eq.-27) then                                              
              itp=7                                                             
              npid=2                                                            
          else if(iabs(kl2).eq.33) then                                         
              itp=8                                                             
              npid=2                                                            
          else if(iabs(kl2).eq.24) then                                         
              itp=12                                                            
              npid=2                                                            
          else if(kl2.eq.57) then                                               
              itp=13                                                            
              npid=0                                                            
          else if(iabs(kl2).eq.18) then                                         
              itp=14                                                            
              npid= isign(1,kl2)* 2                                             
          else if(iabs(kl2).eq.19) then                                         
              itp=14                                                            
              npid= isign(1,kl2)* 1                                             
          else if(kl2.eq.43) then                                               
              itp=15                                                            
              npid=1                                                            
          else if(kl2.eq.44) then                                               
              itp=15                                                            
              npid=0                                                            
          else if(kl2.eq.45) then                                               
              itp=15                                                            
              npid=-1                                                           
          else if(kl2.eq.0) then                                                
              itp=-9999                                                         
              npid=0                                                            
          else                                                                  
              itp= 99                                                           
              npid= kl2                                                         
          end if                                                                
      end if                                                                    
c                                                                               
      return                                                                    
      end                                                                       
