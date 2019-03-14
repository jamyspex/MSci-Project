program loopVarDerive
      integer :: loop
      integer :: i, j, k
      integer, dimension(0:4, 0:2, 0:1, 0:3) :: data

      do i = 0, 4
            do j = 0, 2
                  do k = 0, 1
                        do l = 0, 3
                              data(i, j, k, l) = l + 4*k + 8*j + 24*i
                        end do
                  end do
            end do
      end do

      ! do i = 0, 4
      !       do j = 0, 2
      !             do k = 0, 1
      !                   do l = 0, 3
      !                         print *, data(i, j, k, l)
      !                   end do
      !             end do
      !       end do
      ! end do
      ! do i = 0, 4
      !       do j = 0, 2
      !             do k = 0, 3
      !                   write (*, fmt="(1x,a,i0)", advance="no") " ", data(i, j, k)
      !             end do
      !             print *,""
      !       end do
      !       print *, "----------------"
      ! end do
      ! do loop = 0, 59
      !       i = loop / (4*3)
      !       j = MODULO(loop/4, 3)
      !       k = MODULO(loop, 4)
      !       if (loop /= data(i,j,k)) then
      !             print *, "loop = ", loop, " i = ", i, " j = ", j, &
      !             & " k = ", k, " data(i, j, k) = ", data(i,j,k)
      !       end if
      !       ! print *, loop
      ! end do

      print *, ""
      do loop = 0, 119
            i = loop / (3*2*4)
            j = MODULO(loop/(2*4), 3)
            k = MODULO(loop/4, 2)
            l = MODULO(loop, 4)
            if (loop /= data(i,j,k,l)) then
                print *, "loop = ", loop, " i = ", i, " j = ", j, &
                & " k = ", k, " l = ", l, " data(i, j, k) = ", data(i,j,k,l)
            end if
            ! print *, loop
      end do
end program loopVarDerive
