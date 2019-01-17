module module_init
      use module_feedbfm 
 contains
      subroutine init(u,v,w,p,cn2s,dxs,cn2l,cn3s,dys,cn3l,dzs,cn4s,cn4l,cn1, &
      amask1,bmask1,cmask1,dmask1, &
      zbm,z2,dzn)
      implicit none
integer, parameter :: kp=80
    integer, parameter :: ip = 300
    integer, parameter :: jp = 300
    integer, parameter :: ipmax = ip
    integer, parameter :: jpmax = jp
    character(300) :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt'
    real, parameter :: dxgrid = 4.
    real, parameter :: dygrid = 4.
    real, parameter :: cs0 = 0.14
    integer, parameter :: i_anime=1
    integer, parameter :: avetime=2
    integer, parameter :: km_sl=80
    integer, parameter :: i_aveflow=0
    integer, parameter :: i_ifdata_out=0
    real, parameter :: dt_orig = 0.05 
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: amask1
        real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(Out) :: bmask1
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: cmask1
        real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: dmask1
        real(kind=4), dimension(ip,jp,kp) , intent(Out) :: cn1
        real(kind=4), dimension(ip) , intent(Out) :: cn2l
        real(kind=4), dimension(ip) , intent(Out) :: cn2s
        real(kind=4), dimension(jp) , intent(Out) :: cn3l
        real(kind=4), dimension(jp) , intent(Out) :: cn3s
        real(kind=4), dimension(kp) , intent(Out) :: cn4l
        real(kind=4), dimension(kp) , intent(Out) :: cn4s
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(0:jp) , intent(In) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        real(kind=4), dimension(0:kp+2) , intent(In) :: z2
        real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(Out) :: p
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(Out) :: w
        real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: i,j,k
      do k = 0,kp+1
      do j = 0,jp+2
      do i = 0,ip+2
          p(i,j,k) = 0.0
      end do
      end do
      end do
      do k = 0,kp+1
      do j = -1,jp+1
      do i = 0,ip+1
        u(i,j,k) = 0.0
        v(i,j,k) = 0.0
      end do
      end do
      end do
      do k = -1,kp+1
      do j = -1,jp+1
      do i = 0,ip+1
        w(i,j,k) = 0.0
      end do
      end do
      end do
      call feedbfm(amask1,bmask1,cmask1,dmask1,zbm,z2,dzn)
      return
      end subroutine init
end module module_init
