!  cfd_upwind.f90 
!
!  FUNCTIONS:
!  cfd_upwind - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: cfd_upwind
!  by mehrdad baba hoseinpour 
!  PURPOSE:  Entry point for the console application.
!  beyond sky!
!****************************************************************************

program wave_upwind
implicit none
integer::i,n
integer,parameter:: ix=201,nt=400
real::u(nt,ix),a,dt,dx,l,pi,u1(nt,ix)
pi=2*asin(1.d0)
a=0.01
dt=0.04
l=1
dx=l/(ix-1)
print*,'qourant number is = ',a*dt/dx

!exact_solution
do n=1,nt
 do i=1,ix
    u1(n,i)= sin(2*pi*(i-1)*(dx-a*dt)*5)  
 end do
end do

!initial condition
do i=1,ix
    u(1,i)=sin(2*pi*(i-1)*dx*5)   ! 1,nt  u(n,i) -a*n
end do
do n=1,nt-1
    do i=2,ix-1
        u(n+1,i)=u(n,i)-a*dt/dx*(u(n,i)-u(n,i-1))
    end do
    !periodic condition
    u(n+1,1)=u(n,1)-a*dt/dx*(u(n,1)-u(n,ix))
    u(n+1,ix)=u(n+1,1)
end do
open (1,file='analysis_result_upwind.plt')   !  result analysis
do n=1,nt
    write(1,*)'variables="x","u"'
    write(1,*)'zone'
    do i=1,ix
        write(1,*) dx*(i-1),u1(n,i)
    end do
end do
open (1,file='computational_result_upwind.plt')   !  result computational
do n=1,nt
    write(1,*)'variables="x","u"'
    write(1,*)'zone'
    do i=1,ix
        write(1,*) dx*(i-1),u(n,i)
    end do
end do
print*,">>> please Inter <<<"
pause
    end program wave_upwind


