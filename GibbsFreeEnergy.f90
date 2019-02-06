program Gibbs
implicit none
integer  ,  parameter                   ::      dp = selected_real_kind(15, 307)
integer                                 ::      alloc_err                                 !Deallocation status for allocatable arrays
integer                                 ::      i                                         !variable for reactants, products and TS loops     
integer                                 ::      k                                         !variable for Temperature loop
integer                                 ::      nTemp                                     !number of different Temperatures
character(20)                           ::      filename                                  !The variable for the name of input file
real(dp)                                ::      freq, qvibr,lnqvibr                       !frequencies, vibrational partition function of reactants, products
real(dp)                                ::      scfr                                      !SCF energy of reactants, products & TS
real(dp)                                ::      ZPCr,ZPEr                                 !zero point corrected energy/ zp energy of reactants.product & TS
real(dp)                                ::      h, kb, c,qvib,ZPE,scf,LT,HT,IT,Temp
real(dp) ,  allocatable, dimension (:)  ::      T                                         !Temperature array
real(dp) ,  allocatable, dimension (:)  ::      Gr                                        !Gibbs free energy for all materials

h  = 4.13566845e-15                                                                       !in eV/K
kb = 8.61733502e-5                                                                        !in eV/K
c  = 299792458                                                                            !light speed m/s                                                                              
!print the information about this code
call help()


!get Temperatures
write(*,*) 'Please type the temperature range you wish in the following order:'
write(*,*) 'Lower value of T. Higher value of T. Temperature interval'
read (*,*) LT, HT, IT
nTemp=((HT-LT)/IT)+1
allocate (T(nTemp))
do i=1,nTemp
   T(i)=LT+(i-1)*IT
end do


allocate(Gr(nTemp))


!loop over all calculations for different temperature
do k = 1,nTemp


!assign the temperature to variable Temp
        Temp = T(k)
        
                filename='SCF-Data'
                call q(filename,Temp,qvib,ZPE,scf)
                qvibr = qvib
                scfr  = scf
                ZPEr  = ZPE
                ZPCr  = scfr+ZPEr
                lnqvibr= log(qvibr)
        
Gr(k)             =  ZPCr-kb*T(k)*lnqvibr


end do
!end of the loop for Temparature


!Wrtie the data in file named 'Gibbs'
open    ( unit = 50000, file = 'Gibbs', status = 'new')
write    (50000, 40000)
40000 format ( 10x, "T             :     Temperature in Kelvin", &
             /,10x,"Gr             :     Gibbs Free Energy, eV ",///)

write   ( 50000, 50100)
50100 format ( 3x, "T(K)", 11x, "Gr")
write   (50000,50200)
50200 format (2x, "=============================")
do k=1,nTemp
write   (50000, 50300) T(k), Gr(k)
50300 format (2x, F6.2, 3x, ES20.13,3x)
end do
close(50000)

deallocate(Gr, stat = alloc_err)
deallocate(T, stat = alloc_err)

end program Gibbs



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Subroutine q!!!!!!!!!!!!!!!!!!!!!!
!This subroutine calculate vibrational partition function and read scf frome file
subroutine q(filename,Temp,qvib,ZPE,scf)

implicit none

integer  ,  parameter                   ::      dp = selected_real_kind(15, 307)
real     ,  parameter                   ::      h  = 4.13566845e-15                   !in eV/K
real     ,  parameter                   ::      kb = 8.61733502e-5                    !in eV/K
real     ,  parameter                   ::      c  = 299792458                        !light speed m/s




!local variable
real(dp) ,  allocatable, dimension (:)  ::      freq
real(dp)                                ::      freqtot
integer                                 ::      i,j,ierror,alloc_err,nfreq

!parameter types and definition
character(20)    ,  intent(in)                  ::      filename 
real(dp)         ,  intent(in)                  ::      Temp 
real(dp)         ,  intent(out)                 ::      scf
real(dp)         ,  intent(out)                 ::      qvib
real(dp)         ,  intent(out)                 ::      ZPE


!read the # of freq in freq.dat file
nfreq=-1       ! The first line is Escf
open(1, file=filename)
do
 read (1, *, end=10)
 nfreq=nfreq+1
end do
10 close(1)

allocate (freq(nfreq))
   qvib         =       1.0
   freqtot      =       0.0
   open (unit=99, file = filename, status='old', action = 'read', iostat = ierror)
   read(99,*,iostat = ierror) scf   
     
         do i=1,nfreq
                read(99,*, iostat = ierror) freq(i)
                if (ierror /=0) exit
                if (freq(i) == 0) exit
                if (freq(i) < 50) then
                        freq(i) = 50
                end if       
                freqtot=freqtot+freq(i)
                qvib=qvib*(1/(1-exp((-h*freq(i)*100*c)/(kb*Temp))))
        end do
        ZPE=0.5*h*100*c*freqtot
 close(99)
deallocate(freq, stat= alloc_err)
end subroutine q


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Subroutine Help!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine help()
integer         ::      status = 0
character(10)   ::      answer
write (*,950)
    950 format(//,1X,"This code is written  by Mehdi Zare in 5/20/2018 for calculating reaction properties of " ,&
                 " Surface Reactions",&
                //," You need to have one input files for this code:",&
                //," SCF-Data:The first line is SCF energy and others are frequencies",&
                //," HINT: DO NOT forget to remove imaginary frequency of transition state",//)
               
   
250  write (*,960)
        960 format (1X,"Do you still want to use this code? type yes or no",//)
        read (*,*) answer
        if (answer == 'no') then
          write(*,1010)
          1010 format(//,1X,"See You Later!",//)
          call exit (status)
          elseif (answer=='yes') then
          write (*,970)
          970 format (//,1X,"Great! Glad that you can use this code!",//)
          else
          write(*,980)
          980 format (//,1X,"Your answer is not recognized, please try again",//)
          go to 250
        end if                          
   End Subroutine help
