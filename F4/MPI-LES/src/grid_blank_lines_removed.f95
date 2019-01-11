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
        do i = -1,ip+1
            dx1(i) = dxgrid
        end do
      dxl(0) = 0.
      do i = 1,ip
       dxl(i) = dxl(i-1)+dx1(i)
      end do
      do j = 0,jp+1
            dy1(j) = dygrid
      end do
      dyl(0) = 0.
      do j = 1,jp
       dyl(j) = dyl(j-1)+dy1(j)
      end do
        z2(0)= 0.
        dzn(0)= 1.
        z2(1)= 1.
        dzn(1)= 1.
      do k=2,15
        dzn(k)=dzn(k-1)*1.1
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
        z2(k)=z2(k-1)+dzn(k) 
      end do
      do k=1,kp
      end do
      dzn(kp+1) = dzn(kp)
      dzn(kp+2) = dzn(kp+1)
      dzn(0) = dzn(1)
      dzn(-1)=dzn(0)
      do k = 0,kp
        dzs(k) = dzn(k+1)/2.+dzn(k)/2.
      end do
      do i=0, ip
        dxs(i) = dx1(i)/2.+dx1(i+1)/2.
      end do
      do j = 0,jp
        dys(j) = dy1(j)/2.+dy1(j+1)/2.
      end do
      dzs(kp+1) = dzs(kp)
      dzs(kp+2) = dzs(kp+1) 
      dzs(-1) = dzs(0) 
      return
      end subroutine grid
end module module_grid
