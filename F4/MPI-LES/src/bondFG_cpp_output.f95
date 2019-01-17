module module_bondFG
 contains
subroutine bondfg(f,g,h)
    use params_common_sn
    implicit none
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
    integer :: i, j, k
!
! --inflow condition
        do k = 1,kp
            do j = 1,jp
                f( 0,j,k) = f(1 ,j,k)
            end do
        end do
! --sideflow condition
    do k = 1,kp
        do i = 1,ip
            g(i, 0,k) = g(i,jp ,k) ! WV only right->left because g(jp+1) does not exist
        end do
    end do
! --ground and top condition
    do j = 1,jp
        do i = 1,ip
            h(i,j, 0) = 0.0
            h(i,j,kp) = 0.0
        end do
    end do
end subroutine bondFG
end module module_bondFG
