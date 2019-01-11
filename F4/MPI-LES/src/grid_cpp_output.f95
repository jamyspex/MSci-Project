module module_grid
 contains
      subroutine grid(dx1,dxl,dy1,dyl,z2,dzn,dzs,dxs,dys)
    use params_common_sn
        real(kind=4), dimension(-1:ip+1) , intent(Out) :: dx1
        real(kind=4), dimension(0:ip) , intent(Out) :: dxl
        real(kind=4), dimension(0:ip) , intent(Out) :: dxs
        real(kind=4), dimension(0:jp+1) , intent(Out) :: dy1
        real(kind=4), dimension(0:jp) , intent(Out) :: dyl
        real(kind=4), dimension(0:jp) , intent(Out) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(Out) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(Out) :: dzs
        real(kind=4), dimension(0:kp+2) , intent(Out) :: z2
!
! WV: I think the use of run-time im,jp,kp is dangerous, unless they are identical to ip,jp,kp
! WV: So I changed it to be that way
! --dx set; streamwise direction
! WV: so -1 and ip+1 are not set!!! I changed it analogous to dy1
! do i = 0,ip
        do i = -1,ip+1
            dx1(i) = dxgrid
        end do
      dxl(0) = 0.
      do i = 1,ip
       dxl(i) = dxl(i-1)+dx1(i)
      end do
! --dy set; spanwise direction
!WV: let's set the *whole* array to this value!
      !do j = 0,jp
      do j = 0,jp+1
            dy1(j) = dygrid
      end do
      dyl(0) = 0.
      do j = 1,jp
       dyl(j) = dyl(j-1)+dy1(j)
      end do
! --dz set; vertical direction
!WV: also define the first and last point in the array!
! do k = 0,1
! z2(k) = 2.5
! dzn(k) = 2.5
! end do
! do k = 2,15
! dzn(k) = dzn(k-1)*1.05
! end do
! do k = 16,44
! dzn(k) = 5.
! end do
! do k = 45,kp+1
! dzn(k) = dzn(k-1)*1.0459
! end do
! do k = 2,kp+2 ! WV: was kp+1
! z2(k) = z2(k-1)+dzn(k)
! end do
      ! so z2(kp+2) is not set, why?
!original
! do k=0,1
! z2(k)= 1.
! dzn(k)= 1.
! write(*,*) 'dzn=',dzn(k)
! end do
        z2(0)= 0.
        dzn(0)= 1.
        z2(1)= 1.
        dzn(1)= 1.
      do k=2,15
        dzn(k)=dzn(k-1)*1.1
        ! write(*,*) 'dzn=',dzn(k)
      end do
      do k=16,44
        dzn(k)=4.
      end do
      do k=45,58
        dzn(k)=dzn(k-1)*1.1
      end do
      do k=59,kp+1
        dzn(k)=16.
      enddo
      do k=2,kp+2
        z2(k)=z2(k-1)+dzn(k) !Height
      end do
      do k=1,kp
       ! write(*,*) 'z2grid=',z2(k)
      end do
! --gaiten deno haba
      dzn(kp+1) = dzn(kp)
      !WV
      dzn(kp+2) = dzn(kp+1)
      dzn(0) = dzn(1)
      !WV
      dzn(-1)=dzn(0)
! -------------------------------------
      do k = 0,kp
        dzs(k) = dzn(k+1)/2.+dzn(k)/2.
      end do
! GR: dxs is defined from 0:ip but ip+1 is written to
! GR: dx1 is defined from -1 to ip+1 but ip+2 is read from
      !do i = 0,ip+1
      do i=0, ip
        dxs(i) = dx1(i)/2.+dx1(i+1)/2.
      end do
! WV: so the access to the undefine dy1(jp+2) seems to be what causes corruption of dy1(0)
! WV: In fact, dys is only defined (0 .. jp) so most likely they run into one another
      !do j = 0,jp+1
      do j = 0,jp
        dys(j) = dy1(j)/2.+dy1(j+1)/2.
      end do
!
      dzs(kp+1) = dzs(kp)
      dzs(kp+2) = dzs(kp+1) !WV
      dzs(-1) = dzs(0) !WV
!
!
      return
      end subroutine grid
end module module_grid
