program main
      implicit none
    real(kind=4) :: alpha
    integer :: ical
    integer :: im
    integer :: jm
    integer :: km
    integer :: n
    integer :: n0
    integer :: n1
    integer :: nif
    integer :: nmax
    real(kind=4) :: beta
    character(len=70) :: data10
    character(len=70) :: data11
    character(len=70) :: data12
    character(len=70) :: data13
    character(len=70) :: data14
    character(len=70) :: data15
    character(len=70) :: data20
    character(len=70) :: data21
    character(len=70) :: data22
    character(len=70) :: data23
    character(len=70) :: data24
    character(len=70) :: data25
    character(len=70) :: data26
    character(len=70) :: data27
    character(len=70) :: data30
    character(len=70) :: data31
    real(kind=4) :: dt
    real(kind=4) :: ro
    real(kind=4) :: time
    real(kind=4) :: vn
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) :: amask1
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) :: cmask1
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) :: dmask1
    real(kind=4), dimension(ip,jp,kp) :: cn1
    real(kind=4), dimension(ip) :: cn2l
    real(kind=4), dimension(ip) :: cn2s
    real(kind=4), dimension(jp) :: cn3l
    real(kind=4), dimension(jp) :: cn3s
    real(kind=4), dimension(kp) :: cn4l
    real(kind=4), dimension(kp) :: cn4s
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: cov1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: cov2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: cov3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: cov4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: cov5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: cov6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: cov7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: cov8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: cov9
    real(kind=4), dimension(kp) :: delx1
    real(kind=4), dimension(0:ip,jp,kp) :: dfu1
    real(kind=4), dimension(ip,0:jp,kp) :: dfv1
    real(kind=4), dimension(ip,jp,kp) :: dfw1
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: diu1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: diu5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: diu9
    real(kind=4), dimension(-1:ip+1) :: dx1
    real(kind=4), dimension(0:ip) :: dxl
    real(kind=4), dimension(0:ip) :: dxs
    real(kind=4), dimension(0:jp+1) :: dy1
    real(kind=4), dimension(0:jp) :: dyl
    real(kind=4), dimension(0:jp) :: dys
    real(kind=4), dimension(-1:kp+2) :: dzn
    real(kind=4), dimension(-1:kp+2) :: dzs
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: f
    real(kind=4), dimension(ip,jp,kp) :: fold
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: fx
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: fy
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: fz
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: g
    real(kind=4), dimension(ip,jp,kp) :: gold
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: h
    real(kind=4), dimension(ip,jp,kp) :: hold
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: nou1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2) :: nou5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2) :: nou9
    real(kind=4), dimension(0:1,0:ip+2,0:jp+2,0:kp+1) :: p
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) :: rhs
    real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) :: sm
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) :: u
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: usum
    real(kind=4), dimension(ip,jp,kp) :: uwfx
    real(kind=4), dimension(ip,kp) :: uwfxs
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) :: v
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: vsum
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) :: w
    real(kind=4), dimension(0:ip,0:jp,0:kp) :: wsum
    real(kind=4), dimension(0:kp+2) :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) :: zbm
    real(kind=4), dimension(0:ip+1,0:jp+1) :: uspd
    real(kind=4), dimension(0:ip+1,0:jp+1) :: vspd
    integer :: clock_rate
    integer (kind=4), dimension(0:9) :: timestamp
    integer (kind=4) :: i
    call set(data10,data11,data20,data21,data22,data23,data24,data25,data26,&
             data27,data30,data31,ical,nif,n0,n1,nmax,dt,ro,&
             vn,alpha,beta,data12,data13,data14,data15)
    call grid(dx1,dxl,dy1,dyl,z2,dzn,dzs,dxs,dys)
    call init(u,v,w,p,cn2s,dxs,cn2l,cn3s,dys,cn3l,dzs,cn4s,cn4l,cn1,&
              amask1,bmask1,cmask1,dmask1, &
              zbm,z2,dzn)
    call ifdata( &
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
    do n = n0,nmax
        time = float(n-n0)*dt
        call velnw(p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h) 
        call bondv1(u,z2,dzn,v,w,n,n0,dt,dxs) 
        call velfg(dx1,dy1,dzn,f,g,h,u,v,w, &
        dfu1,dfv1,dfw1,vn,dzs, &
        diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9, &
        cov1,cov2,cov3,cov4,cov5,cov6,cov7,cov8,cov9, &
        nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9, &
        uspd,vspd) 
        call feedbf(usum,u,bmask1,vsum,v,cmask1,wsum,w,dmask1,alpha, &
                    dt,beta,fx,fy,fz,f,g,h,n) 
        call les(delx1,dx1,dy1,dzn,diu1,diu2,diu3,diu4,diu5,diu6, &
                 diu7,diu8,diu9,sm,f,g,h,u,v,uspd,vspd,dxs,dys,n) 
        call adam(n,nmax,data21,fold,gold,hold,&
        f,g,h) 
        call press(u,v,w,p,rhs,f,g,h,dx1,dy1,dzn,dxs,dys,dzs,dt,n,nmax&
        )
     end do
end program
