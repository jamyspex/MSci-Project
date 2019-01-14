module module_bondFG
 contains
subroutine bondfg(km,jm,f,im,g,h)
      implicit none
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
    integer, intent(In) :: im, jm, km
    integer :: i, j, k
        do k = 1,km
            do j = 1,jm
                f( 0,j,k) = f(1 ,j,k)
            end do
        end do
    do k = 1,km
        do i = 1,im
            g(i, 0,k) = g(i,jm ,k) 
        end do
    end do
    do j = 1,jm
        do i = 1,im
            h(i,j, 0) = 0.0
            h(i,j,km) = 0.0
        end do
    end do
end subroutine bondFG
end module module_bondFG
