module module_ifdata
      use module_bondv1
      use module_velFG
      use module_les
      use module_boundp
      use module_feedbfm
      use module_feedbf
 contains
      subroutine zero_arrays(cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9, &
              dfu1, dfv1, dfw1, &
              diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9, &
              nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9 &
              )
      use common_sn
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
          do k = 0,kp+1
          do j = 0,jp+2
          do i = -1,ip+2
              cov1(i,j,k) = 0.0
              cov5(i,j,k) = 0.0
              diu1(i,j,k) = 0.0
              diu5(i,j,k) = 0.0
              nou1(i,j,k) = 0.0
              nou5(i,j,k) = 0.0
          end do
          end do
          end do
          do k = 0,kp+1
          do j = 0,jp+2
          do i = 0,ip+2
              cov2(i,j,k) = 0.0
              cov3(i,j,k) = 0.0
              cov4(i,j,k) = 0.0
              cov6(i,j,k) = 0.0
              cov7(i,j,k) = 0.0
              cov8(i,j,k) = 0.0
              cov9(i,j,k) = 0.0
              diu2(i,j,k) = 0.0
              diu3(i,j,k) = 0.0
              diu4(i,j,k) = 0.0
              diu6(i,j,k) = 0.0
              diu7(i,j,k) = 0.0
              diu8(i,j,k) = 0.0
              diu9(i,j,k) = 0.0
              nou2(i,j,k) = 0.0
              nou3(i,j,k) = 0.0
              nou4(i,j,k) = 0.0
              nou6(i,j,k) = 0.0
              nou7(i,j,k) = 0.0
              nou8(i,j,k) = 0.0
              nou9(i,j,k) = 0.0
          end do
          end do
          end do
          do k = 1,kp
          do j = 1,jp
          do i = 0,ip
              dfu1(i,j,k) = 0.0
          end do
          end do
          end do
          do k = 1,kp
          do j = 0,jp
          do i = 1,ip
              dfv1(i,j,k) = 0.0
          end do
          end do
          end do
          do k = 1,kp
          do j = 1,jp
          do i = 1,ip
              dfw1(i,j,k) = 0.0
          end do
          end do
          end do
      end subroutine
      subroutine ifdata( &
      data30,data31, fold,gold,hold,fghold, time, &
      n,u,im,jm,km,v,w,p,usum,vsum,wsum, &
      delx1,dx1,dy1,dzn,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g,h,z2,dt, &
      dxs,cov1,cov2,cov3,dfu1,vn,cov4,cov5,cov6,dfv1,cov7,cov8,cov9,dfw1,dzs,nou1,nou5,nou9,nou2, &
      nou3,nou4,nou6,nou7,nou8,bmask1,cmask1,dmask1,alpha,beta,fx,fy,fz,amask1,zbm,ical)
      use common_sn 
        character(len=70), intent(In) :: data30
        character(len=70), intent(In) :: data31
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: fghold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: fold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: gold
        real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: hold
        real(kind=4), intent(InOut) :: time
        integer, intent(In) :: ical
        real(kind=4), intent(In) :: alpha
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: amask1
        real(kind=4), intent(In) :: beta
        real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: bmask1
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: cmask1
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
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: dmask1
        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: f
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fx
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fy
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: fz
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: g
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
        integer, intent(InOut) :: im
        integer, intent(InOut) :: jm
        integer, intent(InOut) :: km
        integer, intent(InOut) :: n
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou1
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou3
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou4
        real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou5
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou6
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou7
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou8
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) , intent(Out) :: nou9
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
        real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: sm
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: usum
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), intent(In) :: vn
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: vsum
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
        real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: wsum
        real(kind=4), dimension(0:kp+2) , intent(In) :: z2
        real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    real(kind=4),allocatable :: ua(:,:,:)
    real(kind=4),allocatable :: va(:,:,:)
    real(kind=4),allocatable :: wa(:,:,:)
    real(kind=4),allocatable :: usuma(:,:,:)
    real(kind=4),allocatable :: vsuma(:,:,:)
    real(kind=4),allocatable :: wsuma(:,:,:)
    real(kind=4),allocatable :: pa(:,:,:)
    real(kind=4),allocatable :: fa(:,:,:)
    real(kind=4),allocatable :: ga(:,:,:)
    real(kind=4),allocatable :: ha(:,:,:)
    real(kind=4),allocatable :: folda(:,:,:)
    real(kind=4),allocatable :: golda(:,:,:)
    real(kind=4),allocatable :: holda(:,:,:)
    if ((im/=ip) .or. (jm/=jp) .or. (km/=kp)) then
            print *, "im,km,km is different from ip,jp,kp, aborting
            call exit(-1)
    end if
       if(ical == 1) then
        if (isMaster()) then
        open(unit=30,file='data30048000.dat',form='unformatted',status='unknown')
        read(30) n,time
        end if
        allocate(ua(0:ipmax+1,-1:jpmax+1,0:kp+1))
        if (isMaster()) then
        read(30) (((ua(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeifu(ua, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         u(i,j,k)=ua(i,j,k)
        end do
        end do
        end do
        deallocate(ua)
        allocate(va(0:ipmax+1,-1:jpmax+1,0:kp+1))
        if (isMaster()) then
        read(30) (((va(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeifu(va, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         v(i,j,k)=va(i,j,k)
        end do
        end do
        end do
        deallocate(va)
        allocate(wa(0:ipmax+1,-1:jpmax+1,-1:kp+1))
        if (isMaster()) then
        read(30) (((wa(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeifw(wa, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         w(i,j,k)=wa(i,j,k)
        end do
        end do
        end do
        deallocate(wa)
        allocate(pa(0:ipmax+2,0:jpmax+2,0:kp+1))
        if (isMaster()) then
        read(30) (((pa(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeifp(pa, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         p(i,j,k)=pa(i,j,k)
        end do
        end do
        end do
        deallocate(pa)
        allocate(usuma(0:ipmax,0:jpmax,0:kp))
        if (isMaster()) then
        read(30) (((usuma(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeifusum(usuma, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         usum(i,j,k)=usuma(i,j,k)
        end do
        end do
        end do
        deallocate(usuma)
        allocate(vsuma(0:ipmax,0:jpmax,0:kp))
        if (isMaster()) then
        read(30) (((vsuma(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeifusum(vsuma, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         vsum(i,j,k)=vsuma(i,j,k)
        end do
        end do
        end do
        deallocate(vsuma)
        allocate(wsuma(0:ipmax,0:jpmax,0:kp))
        if (isMaster()) then
        read(30) (((wsuma(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeifusum(wsuma, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         wsum(i,j,k)=wsuma(i,j,k)
        end do
        end do
        end do
        deallocate(wsuma)
        if (isMaster()) then
        close(30)
        end if
        if (isMaster()) then
        open(unit=31,file='data31048000.dat',form='unformatted',status='unknown')
        end if
        allocate(fa(0:ipmax,0:jpmax,0:kp))
        if (isMaster()) then
        read(31) (((fa(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeiff(fa, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         f(i,j,k)=fa(i,j,k)
        end do
        end do
        end do
        deallocate(fa)
        allocate(ga(0:ipmax,0:jpmax,0:kp))
        if (isMaster()) then
        read(31) (((ga(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeiff(ga, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         g(i,j,k)=ga(i,j,k)
        end do
        end do
        end do
        deallocate(ga)
        allocate(ha(0:ipmax,0:jpmax,0:kp))
        if (isMaster()) then
        read(31) (((ha(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeiff(ha, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         h(i,j,k)=ha(i,j,k)
        end do
        end do
        end do
        deallocate(ha)
        allocate(folda(ipmax,jpmax,kp))
        if (isMaster()) then
        read(31) (((folda(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeiffold(folda, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         fold(i,j,k)=folda(i,j,k)
        end do
        end do
        end do
        deallocate(folda)
        allocate(golda(ipmax,jpmax,kp))
        if (isMaster()) then
        read(31) (((golda(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeiffold(golda, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         gold(i,j,k)=golda(i,j,k)
        end do
        end do
        end do
        deallocate(golda)
        allocate(holda(ipmax,jpmax,kp))
        if (isMaster()) then
        read(31) (((holda(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        call distributeiffold(holda, ip, jp, kp, ipmax, jpmax, procPerRow)
        do k=1,km
        do j=1,jm
        do i=1,im
         hold(i,j,k)=holda(i,j,k)
        end do
        end do
        end do
        deallocate(holda)
        if (isMaster()) then
        close(31)
        end if
        call zero_arrays( &
              cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9, &
              dfu1, dfv1, dfw1, &
              diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9, &
              nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9 &
         )
     end if
      end subroutine ifdata
end module module_ifdata
