program reshapeTest
      integer, dimension(0:1,0:2,0:2) :: testArray
      integer, dimension(0:17) :: reshaped
      do i = 0, 2
            do j = 0, 2
                  do k = 0, 1
                        testArray(k, j, i) = 6*i + 2*j + k
                  end do
            end do
      end do

      reshaped = pack(testArray, .true. )


      do i = 0, 2
            do j = 0, 2
                  do k = 0, 1
                        write (*, fmt="(1x,a,i0)", advance="no") " ", testArray(k, j, i)
                  end do
            print *, ""
            end do
      print *, "-----------"
      end do

      print *, "Flattened"

      do i = 0, 17
            print *, reshaped(i)
      end do
end program reshapeTest
