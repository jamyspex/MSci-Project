module module_ifdata
 contains
      subroutine zero_arrays(&
      cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9, &
              dfu1, dfv1, dfw1 &
              ,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9 &
              ,nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9 &
              )
      implicit none
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov9
        real(kind=4), dimension(0:ip,jp,kp) , intent(Out) :: dfu1
        real(kind=4), dimension(ip,0:jp,kp) , intent(Out) :: dfv1
        real(kind=4), dimension(ip,jp,kp) , intent(Out) :: dfw1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu9
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou9
              cov1 = 0.0
              cov5 = 0.0
              cov2 = 0.0
              cov3 = 0.0
              cov4 = 0.0
              cov6 = 0.0
              cov7 = 0.0
              cov8 = 0.0
              cov9 = 0.0
              diu1 = 0.0
              diu5 = 0.0
              diu2 = 0.0
              diu3 = 0.0
              diu4 = 0.0
              diu6 = 0.0
              diu7 = 0.0
              diu8 = 0.0
              diu9 = 0.0
              nou1 = 0.0
              nou5 = 0.0
              nou2 = 0.0
              nou3 = 0.0
              nou4 = 0.0
              nou6 = 0.0
              nou7 = 0.0
              nou8 = 0.0
              nou9 = 0.0
              dfu1 = 0.0
              dfv1 = 0.0
              dfw1 = 0.0
      end subroutine
      subroutine ifdata( &
                fold,gold,hold,&
                 time, &
                n,u,v,w,p,usum,vsum,wsum,delx1,dx1,dy1,dzn,&
                diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,&
                sm,f,g,h,z2,dt,dxs,vn, &
                dfu1,dfv1,dfw1,&
                cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9,&
                dzs,&
                nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9,&
                amask1,bmask1,cmask1,dmask1,&
                alpha,beta,fx,fy,fz,zbm,ical,nif)
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: fold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: gold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: hold
        real(kind=4), intent(InOut) :: time
        integer, intent(In) :: ical
        real(kind=4), intent(In) :: alpha
        real(kind=4), intent(In) :: beta
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: amask1
        real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: bmask1
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: cmask1
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: dmask1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: cov9
        real(kind=4), dimension(kp) , intent(Out) :: delx1
        real(kind=4), dimension(0:ip,jp,kp) , intent(Out) :: dfu1
        real(kind=4), dimension(ip,0:jp,kp) , intent(Out) :: dfv1
        real(kind=4), dimension(ip,jp,kp) , intent(Out) :: dfw1
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: diu9
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fx
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fy
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fz
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
        integer, intent(InOut) :: n
        integer, intent(in) :: nif
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou9
        real(kind=4), dimension(0:1,0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: usum
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: vsum
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: wsum
        real(kind=4), intent(In) :: vn
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: sm
        real(kind=4), dimension(0:kp+2) , intent(In) :: z2
        real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
        real(kind=4),allocatable :: ua(:,:,:), ur(:,:,:)
        real(kind=4),allocatable :: va(:,:,:), vr(:,:,:)
        real(kind=4),allocatable :: wa(:,:,:), wr(:,:,:)
        real(kind=4),allocatable :: usuma(:,:,:)
        real(kind=4),allocatable :: vsuma(:,:,:)
        real(kind=4),allocatable :: wsuma(:,:,:)
        real(kind=4),allocatable :: pa(:,:,:),pr(:,:,:)
        real(kind=4),allocatable :: fa(:,:,:),fr(:,:,:)
        real(kind=4),allocatable :: ga(:,:,:),gr(:,:,:)
        real(kind=4),allocatable :: ha(:,:,:),hr(:,:,:)
        real(kind=4),allocatable :: folda(:,:,:)
        real(kind=4),allocatable :: golda(:,:,:)
        real(kind=4),allocatable :: holda(:,:,:)
    character(len=70) :: filename
    integer :: i,j,k
       if(ical == 1) then
        write(filename, '("../data/data30",i6.6, ".dat")') nif
        open(unit=30,file=filename,form='unformatted',status='unknown')
        read(30) n,time
        read(30) (((u(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(30) (((v(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(30) (((w(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(30) (((p(0,i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(30) (((usum(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(30) (((vsum(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(30) (((wsum(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        close(30)
        write(filename, '("../data/data31",i6.6, ".dat")') nif
        open(unit=31,file=filename,form='unformatted',status='unknown')
        read(31) (((f(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(31) (((g(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(31) (((h(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(31) (((fold(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(31) (((gold(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        read(31) (((hold(i,j,k),i=1,ipmax),j=1,jpmax),k=1,kp)
        close(31)
        call boundp2(p(0,:,:,:))
        call zero_arrays( &
      cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9, &
              dfu1, dfv1, dfw1 &
              ,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9 &
              ,nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9 &
              )
     end if
            call boundp1(p(0,:,:,:))
            call boundp2(p(0,:,:,:))
      end subroutine ifdata
end module module_ifdata
