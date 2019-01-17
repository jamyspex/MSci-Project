module module_init
      use module_feedbfm ! add_module_decls() line 156
 contains
      subroutine init(u,v,w,p,cn2s,dxs,cn2l,cn3s,dys,cn3l,dzs,cn4s,cn4l,cn1, &
      amask1,bmask1,cmask1,dmask1, &
      zbm,z2,dzn)
    use params_common_sn
    implicit none
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
! WV: The original boundary was 0,kp;0,jp;0,ip. This does not init the boundary values,so I changed it to the dimensions of u,v,w,p
! do k = 0,kp
! do j = 0,jp
! do i = 0,ip
! u(i,j,k) = 0.0
! v(i,j,k) = 0.0
! w(i,j,k) = 0.0
! p(i,j,k) = 0.0
! end do
! end do
! end do
!print *, 'Zero arrays'
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
!print *, 'call feedbfm'
! --check
      call feedbfm(amask1,bmask1,cmask1,dmask1,zbm,z2,dzn)
! if(ifbf == 1) call feedbfm(km,jp,ip,amask1,bmask1,cmask1,dmask1,zbm,z2,dzn)
!print *, 'Parameter settings for solving Poisson equation'
! =====================================================
!
! Parameter settings for solving Poisson equation
!
! =====================================================
      return
      end subroutine init
end module module_init
