program reshapeTest
      integer, dimension(0:1,0:2,0:2) :: testArray
      integer, dimension(0:17) :: reshaped
      do i = 0, 1
            do k = 0, 2
                  do j = 0, 2
                        testArray(i, k, j) = 9*i + 3*k + j
                  end do
            end do
      end do

      reshaped = reshape(testArray, (/ 18 /) , (/ 1, 2, 3 /))


      do i = 0, 1
            do k = 0, 2
                  do j = 0, 2
                        write (*, fmt="(1x,a,i0)", advance="no") " ", testArray(i, k, j)
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
