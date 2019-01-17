module module_bondFG
#ifdef MPI
    use communication_helper_real
#endif
 contains 
subroutine bondfg(f,g,h)
#ifdef WV_NEW
    use params_common_sn
#else
    use common_sn ! create_new_include_statements() line 102
#endif
    implicit none
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
    integer :: i, j, k
!
! --inflow condition
#ifdef MPI
    if (isTopRow(procPerRow)) then
#endif
        do k = 1,kp
            do j = 1,jp
                f( 0,j,k) = f(1  ,j,k)
            end do
        end do
#ifdef MPI
    end if
#endif
! --sideflow condition
#if !defined(MPI) || (PROC_PER_ROW==1)
    do k = 1,kp
        do i = 1,ip
            g(i, 0,k) = g(i,jp  ,k) ! WV only right->left because g(jp+1) does not exist
        end do
    end do
#else
! g has 0:jp and this is about setting this 0
    call sideflowRightLeft(g, procPerRow, jp+1, 1, 1, 0, 1, 0)
#endif
! --ground and top condition
    do j = 1,jp
        do i = 1,ip
            h(i,j, 0) = 0.0
            h(i,j,kp) = 0.0
        end do
    end do
#ifdef MPI
! --halo exchanges
#ifdef NESTED_LES
   if (syncTicks == 0) then
#endif
    call exchangeRealHalos(f, procPerRow, neighbours, 1, 0, 1, 0)
    call exchangeRealHalos(g, procPerRow, neighbours, 1, 0, 1, 0)
    call exchangeRealHalos(h, procPerRow, neighbours, 1, 0, 1, 0)
#ifdef NESTED_LES
   end if
#endif
#endif
end subroutine bondFG
end module module_bondFG
